/**
 * d3d2glsl; generate GLSL programs from bytecode of compiled Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

// !!! FIXME: I keep changing coding styles for symbols and typedefs.

// Shader bytecode format is described at MSDN:
//  http://msdn2.microsoft.com/en-us/library/ms800307.aspx

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>

#include "d3d2glsl.h"


// You get all the profiles unless you go out of your way to disable them.

#ifndef SUPPORT_PROFILE_D3D
#define SUPPORT_PROFILE_D3D 1
#endif

#ifndef SUPPORT_PROFILE_GLSL
#define SUPPORT_PROFILE_GLSL 1
#endif


// Get basic wankery out of the way here...

typedef unsigned int uint;  // this is a printf() helper. don't use for code.
typedef uint8_t uint8;
typedef uint32_t uint32;

#ifdef __GNUC__
#define ISPRINTF(x,y) __attribute__((format (printf, x, y)))
#else
#define ISPRINTF(x,y)
#endif

#define STATICARRAYLEN(x) ( (sizeof ((x))) / (sizeof ((x)[0])) )


// Byteswap magic...

#if ((defined __GNUC__) && (defined __POWERPC__))
    static inline uint32 SWAP32(uint32 x)
    {
        __asm__ __volatile__("lwbrx %0,0,%1" : "=r" (x) : "r" (&x));
        return x;
    } // SWAP32
#elif defined(__POWERPC__)
    static inline uint32 SWAP32(uint32 x)
    {
        return ( (((x) >> 24) & 0x000000FF) | (((x) >>  8) & 0x0000FF00) |
                 (((x) <<  8) & 0x00FF0000) | (((x) << 24) & 0xFF000000) );
    } // SWAP32
#else
#   define SWAP32(x) (x)
#endif


// predeclare.
typedef struct Context Context;

// one emit function for each opcode in each profile.
typedef void (*emit_function)(Context *ctx);

// one emit function for comments in each profile.
typedef void (*emit_comment)(Context *ctx, const char *str);

// one emit function for starting output in each profile.
typedef void (*emit_start)(Context *ctx);

// one emit function for ending output in each profile.
typedef void (*emit_end)(Context *ctx);

// one state function for each opcode where we have state machine updates.
typedef int (*state_function)(Context *ctx);

typedef struct
{
    const char *name;
    emit_start start_emitter;
    emit_end end_emitter;
    emit_comment comment_emitter;
} D3D2GLSL_profile;


typedef enum
{
    SHADER_TYPE_UNKNOWN = -1,
    SHADER_TYPE_PIXEL,
    SHADER_TYPE_VERTEX,
    SHADER_TYPE_TOTAL
} D3D2GLSL_shaderType;


// A simple linked list of strings, so we can build the final output without
//  realloc()ing for each new line, and easily insert lines into the middle
//  of the output without much trouble.
typedef struct OutputList
{
    char *str;
    struct OutputList *next;
} OutputList;

#define D3D2GLSL_SCRATCH_BUFFER_SIZE 256
#define D3D2GLSL_SCRATCH_BUFFERS 10

// Context...this is state that changes as we parse through a shader...
struct Context
{
    D3D2GLSL_malloc malloc;
    D3D2GLSL_free free;
    const uint32 *tokens;
    uint32 tokencount;
    OutputList output;
    OutputList *output_tail;
    int output_len; // total strlen; prevents walking the list just to malloc.
    const char *endline;
    int endline_len;
    const char *failstr;
    char scratch[D3D2GLSL_SCRATCH_BUFFERS][D3D2GLSL_SCRATCH_BUFFER_SIZE];
    int scratchidx;  // current scratch buffer.
    int profileid;
    const D3D2GLSL_profile *profile;
    D3D2GLSL_shaderType shader_type;
    uint32 major_ver;
    uint32 minor_ver;
};


static inline char *get_scratch_buffer(Context *ctx)
{
    ctx->scratchidx = (ctx->scratchidx + 1) % D3D2GLSL_SCRATCH_BUFFERS;
    return ctx->scratch[ctx->scratchidx];
} // get_scratch_buffer


// Special-case return values from the parsing pipeline...
#define FAIL (-1)
#define END_OF_STREAM (-2)

static const char *out_of_mem_string = "Out of memory";
static inline int out_of_memory(Context *ctx)
{
    if (ctx->failstr == NULL)
        ctx->failstr = out_of_mem_string;  // fail() would call malloc().
    return FAIL;
} // out_of_memory


static int failf(Context *ctx, const char *fmt, ...) ISPRINTF(2,3);
static int failf(Context *ctx, const char *fmt, ...)
{
    if (ctx->failstr == NULL)  // don't change existing error.
    {
        char *scratch = get_scratch_buffer(ctx);
        va_list ap;
        va_start(ap, fmt);
        const int len = vsnprintf(scratch,D3D2GLSL_SCRATCH_BUFFER_SIZE,fmt,ap);
        va_end(ap);

        char *failstr = (char *) ctx->malloc(len + 1);
        if (failstr == NULL)
            out_of_memory(ctx);
        else
        {
            // see comments about scratch buffer overflow in output_line().
            if (len < D3D2GLSL_SCRATCH_BUFFER_SIZE)
                strcpy(failstr, scratch);  // copy it over.
            else
            {
                va_start(ap, fmt);
                vsnprintf(failstr, len + 1, fmt, ap);  // rebuild it.
                va_end(ap);
            } // else
            ctx->failstr = failstr;
        } // else
    } // if

    return FAIL;
} // failf


static inline int fail(Context *ctx, const char *reason)
{
    return failf(ctx, "%s", reason);
} // fail


static int output_line(Context *ctx, const char *fmt, ...) ISPRINTF(2,3);
static int output_line(Context *ctx, const char *fmt, ...)
{
    if (ctx->failstr != NULL)
        return FAIL;  // we failed previously, don't go on...

    OutputList *item = (OutputList *) ctx->malloc(sizeof (OutputList));
    if (item == NULL)
        return out_of_memory(ctx);

    char *scratch = get_scratch_buffer(ctx);
    va_list ap;
    va_start(ap, fmt);
    const int len = vsnprintf(scratch, D3D2GLSL_SCRATCH_BUFFER_SIZE, fmt, ap);
    va_end(ap);

    item->str = (char *) ctx->malloc(len + 1);
    if (item->str == NULL)
    {
        free(item);
        return out_of_memory(ctx);
    } // if

    // If we overflowed our scratch buffer, that's okay. We were going to
    //  allocate anyhow...the scratch buffer just lets us avoid a second
    //  run of vsnprintf().
    if (len < D3D2GLSL_SCRATCH_BUFFER_SIZE)
        strcpy(item->str, scratch);  // copy it over.
    else
    {
        va_start(ap, fmt);
        vsnprintf(item->str, len + 1, fmt, ap);  // rebuild it.
        va_end(ap);
    } // else

    item->next = NULL;
    ctx->output_tail->next = item;
    ctx->output_tail = item;
    ctx->output_len += len + ctx->endline_len;
    return 0;
} // output_line


typedef struct
{
    int regnum;
    int relative;
    int writemask;
    int result_mod;
    int result_shift;
    int regtype;
} DestTokenInfo;

static int parse_destination_token(Context *ctx, DestTokenInfo *info)
{
    if (ctx->failstr != NULL)
        return FAIL;  // already failed elsewhere.

    if (ctx->tokencount == 0)
        return fail(ctx, "Out of tokens in destination parameter");

    const uint32 token = SWAP32(*(ctx->tokens));
    const int reserved1 = (int) ((token >> 14) & 0x3); // bits 14 through 15
    const int reserved2 = (int) ((token >> 31) & 0x1); // bit 31

    ctx->tokens++;  // swallow token for now, for multiple calls in a row.
    ctx->tokencount--;  // swallow token for now, for multiple calls in a row.

    info->regnum = (int) (token & 0x7ff);  // bits 0 through 10
    info->relative = (int) ((token >> 13) & 0x1); // bit 13
    info->writemask = (int) ((token >> 16) & 0xF); // bits 16 through 19
    info->result_mod = (int) ((token >> 20) & 0xF); // bits 20 through 23
    info->result_shift = (int) ((token >> 24) & 0xF); // bits 24 through 27
    info->regtype = (int) ((token >> 28) & 0x7) | ((token >> 9) & 0x18);  // bits 28-30, 11-12

    if (reserved1 != 0x0)
        return fail(ctx, "Reserved bit #1 in destination token must be zero");

    if (reserved2 != 0x1)
        return fail(ctx, "Reserved bit #2 in destination token must be one");

    if (info->relative)
    {
        if (ctx->shader_type != SHADER_TYPE_VERTEX)
            return fail(ctx, "Relative addressing in non-vertex shader");
        else if (ctx->major_ver < 3)
            return fail(ctx, "Relative addressing in vertex shader version < 3.0");
        return fail(ctx, "Relative addressing is unsupported");  // !!! FIXME
    } // if

    if (info->result_shift != 0)
    {
        if (ctx->shader_type != SHADER_TYPE_PIXEL)
            return fail(ctx, "Result shift scale in non-pixel shader");
        else if (ctx->major_ver >= 2)
            return fail(ctx, "Result shift scale in pixel shader version >= 2.0");
    } // if

    if (info->result_mod & 0x1)  // Saturate (vertex shaders only)
    {
        if (ctx->shader_type != SHADER_TYPE_VERTEX)
            return fail(ctx, "Saturate result mod in non-vertex shader");
    } // if

    if (info->result_mod & 0x2)  // Partial precision (pixel shaders only)
    {
        if (ctx->shader_type != SHADER_TYPE_PIXEL)
            return fail(ctx, "Partial precision result mod in non-pixel shader");
    } // if

    if (info->result_mod & 0x4)  // Centroid (pixel shaders only)
    {
        if (ctx->shader_type != SHADER_TYPE_PIXEL)
            return fail(ctx, "Centroid result mod in non-pixel shader");
    } // if

    return 1;
} // parse_destination_token


typedef struct
{
    int regnum;
    int relative;
    int swizzle_x;
    int swizzle_y;
    int swizzle_z;
    int swizzle_w;
    int src_mod;
    int regtype;
} SrcTokenInfo;

static int parse_source_token(Context *ctx, SrcTokenInfo *info)
{
    if (ctx->failstr != NULL)
        return FAIL;  // already failed elsewhere.

    if (ctx->tokencount == 0)
        return fail(ctx, "Out of tokens in source parameter");

    const uint32 token = SWAP32(*(ctx->tokens));
    const int reserved1 = (int) ((token >> 14) & 0x3); // bits 14 through 15
    const int reserved2 = (int) ((token >> 31) & 0x1); // bit 31

    ctx->tokens++;  // swallow token for now, for multiple calls in a row.
    ctx->tokencount--;  // swallow token for now, for multiple calls in a row.

    info->regnum = (int) (token & 0x7ff);  // bits 0 through 10
    info->relative = (int) ((token >> 13) & 0x1); // bit 13
    info->swizzle_x = (int) ((token >> 16) & 0x3); // bits 16 through 17
    info->swizzle_y = (int) ((token >> 18) & 0x3); // bits 18 through 19
    info->swizzle_z = (int) ((token >> 20) & 0x3); // bits 20 through 21
    info->swizzle_w = (int) ((token >> 22) & 0x3); // bits 22 through 23
    info->src_mod = (int) ((token >> 24) & 0xF); // bits 24 through 27
    info->regtype = (int) ((token >> 28) & 0x7) | ((token >> 9) & 0x18);  // bits 28-30, 11-12

    if (reserved1 != 0x0)
        return fail(ctx, "Reserved bit #1 in source token must be zero");

    if (reserved2 != 0x1)
        return fail(ctx, "Reserved bit #2 in source token must be one");

    if (info->relative)
    {
        if ((ctx->shader_type == SHADER_TYPE_PIXEL) && (ctx->major_ver < 3))
            return fail(ctx, "Relative addressing in pixel shader version < 3.0");
        return fail(ctx, "Relative addressing is unsupported");  // !!! FIXME
    } // if

    if (info->src_mod >= 0xE)
        return fail(ctx, "Unknown source modifier");

    return 1;
} // parse_source_token


// if SUPPORT_PROFILE_* isn't defined, we assume an implicit desire to support.
#define AT_LEAST_ONE_PROFILE 0

#if !SUPPORT_PROFILE_D3D
#define PROFILE_EMITTER_D3D(op)
#else
#undef AT_LEAST_ONE_PROFILE
#define AT_LEAST_ONE_PROFILE 1
#define PROFILE_EMITTER_D3D(op) emit_D3D_##op,

static char *get_D3D_destination(Context *ctx)
{
    char *retval = NULL;
    DestTokenInfo info;
    const int eaten = parse_destination_token(ctx, &info);
    if ((eaten < 1) || (ctx->failstr != NULL))
        return "";

    retval = get_scratch_buffer(ctx);
    strcpy(retval, "DST");  // !!! FIXME

    return retval;
} // get_D3D_destination


static char *get_D3D_source(Context *ctx)
{
    char *retval = NULL;
    SrcTokenInfo info;
    const int eaten = parse_source_token(ctx, &info);
    if ((eaten < 1) || (ctx->failstr != NULL))
        return "";

    retval = get_scratch_buffer(ctx);
    strcpy(retval, "SRC");  // !!! FIXME

    return retval;
} // get_D3D_source


static void emit_D3D_start(Context *ctx)
{
    const uint major = (uint) ctx->major_ver;
    const uint minor = (uint) ctx->minor_ver;
    if (ctx->shader_type == SHADER_TYPE_PIXEL)
        output_line(ctx, "ps_%u_%u", major, minor);
    else if (ctx->shader_type == SHADER_TYPE_VERTEX)
        output_line(ctx, "vs_%u_%u", major, minor);
    else
    {
        failf(ctx, "Shader type %u unsupported in this profile.",
              (uint) ctx->shader_type);
    } // else
} // emit_D3D_start


static void emit_D3D_end(Context *ctx)
{
    output_line(ctx, "END");
} // emit_D3D_end


static void emit_D3D_comment(Context *ctx, const char *str)
{
    output_line(ctx, "; %s", str);
} // emit_D3D_comment


static void emit_D3D_RESERVED(Context *ctx)
{
    // do nothing; fails in the state machine.
} // emit_D3D_RESERVED


// Generic D3D opcode emitters. A list of macros generate all the entry points
//  that call into these...

static void emit_D3D_opcode_d(Context *ctx, const char *opcode)
{
    const char *dst1 = get_D3D_destination(ctx);
    output_line(ctx, "%s %s", opcode, dst1);
} // emit_D3D_opcode_d


static void emit_D3D_opcode_s(Context *ctx, const char *opcode)
{
    const char *src1 = get_D3D_destination(ctx);
    output_line(ctx, "%s %s", opcode, src1);
} // emit_D3D_opcode_s


static void emit_D3D_opcode_ss(Context *ctx, const char *opcode)
{
    const char *src1 = get_D3D_destination(ctx);
    const char *src2 = get_D3D_destination(ctx);
    output_line(ctx, "%s %s, %s", opcode, src1, src2);
} // emit_D3D_opcode_s


static void emit_D3D_opcode_ds(Context *ctx, const char *opcode)
{
    const char *dst1 = get_D3D_destination(ctx);
    const char *src1 = get_D3D_source(ctx);
    output_line(ctx, "%s %s, %s", opcode, dst1, src1);
} // emit_D3D_opcode_ds


static void emit_D3D_opcode_dss(Context *ctx, const char *opcode)
{
    const char *dst1 = get_D3D_destination(ctx);
    const char *src1 = get_D3D_source(ctx);
    const char *src2 = get_D3D_source(ctx);
    output_line(ctx, "%s %s, %s, %s", opcode, dst1, src1, src2);
} // emit_D3D_opcode_dss


static void emit_D3D_opcode_dsss(Context *ctx, const char *opcode)
{
    const char *dst1 = get_D3D_destination(ctx);
    const char *src1 = get_D3D_source(ctx);
    const char *src2 = get_D3D_source(ctx);
    const char *src3 = get_D3D_source(ctx);
    output_line(ctx, "%s %s, %s, %s, %s", opcode, dst1, src1, src2, src3);
} // emit_D3D_opcode_dsss


static void emit_D3D_opcode_dssss(Context *ctx, const char *opcode)
{
    const char *dst1 = get_D3D_destination(ctx);
    const char *src1 = get_D3D_source(ctx);
    const char *src2 = get_D3D_source(ctx);
    const char *src3 = get_D3D_source(ctx);
    const char *src4 = get_D3D_source(ctx);
    output_line(ctx,"%s %s, %s, %s, %s, %s",opcode,dst1,src1,src2,src3,src4);
} // emit_D3D_opcode_dssss


#define EMIT_D3D_OPCODE_FUNC(op) \
    static void emit_D3D_##op(Context *ctx) { \
        output_line(ctx, #op); \
    }
#define EMIT_D3D_OPCODE_D_FUNC(op) \
    static void emit_D3D_##op(Context *ctx) { \
        emit_D3D_opcode_d(ctx, #op); \
    }
#define EMIT_D3D_OPCODE_S_FUNC(op) \
    static void emit_D3D_##op(Context *ctx) { \
        emit_D3D_opcode_s(ctx, #op); \
    }
#define EMIT_D3D_OPCODE_SS_FUNC(op) \
    static void emit_D3D_##op(Context *ctx) { \
        emit_D3D_opcode_ss(ctx, #op); \
    }
#define EMIT_D3D_OPCODE_DS_FUNC(op) \
    static void emit_D3D_##op(Context *ctx) { \
        emit_D3D_opcode_ds(ctx, #op); \
    }
#define EMIT_D3D_OPCODE_DSS_FUNC(op) \
    static void emit_D3D_##op(Context *ctx) { \
        emit_D3D_opcode_dss(ctx, #op); \
    }
#define EMIT_D3D_OPCODE_DSSS_FUNC(op) \
    static void emit_D3D_##op(Context *ctx) { \
        emit_D3D_opcode_dsss(ctx, #op); \
    }
#define EMIT_D3D_OPCODE_DSSSS_FUNC(op) \
    static void emit_D3D_##op(Context *ctx) { \
        emit_D3D_opcode_dssss(ctx, #op); \
    }

EMIT_D3D_OPCODE_FUNC(NOP)
EMIT_D3D_OPCODE_DS_FUNC(MOV)
EMIT_D3D_OPCODE_DSS_FUNC(ADD)
EMIT_D3D_OPCODE_DSS_FUNC(SUB)
EMIT_D3D_OPCODE_DSSS_FUNC(MAD)
EMIT_D3D_OPCODE_DSS_FUNC(MUL)
EMIT_D3D_OPCODE_DS_FUNC(RCP)
EMIT_D3D_OPCODE_DS_FUNC(RSQ)
EMIT_D3D_OPCODE_DSS_FUNC(DP3)
EMIT_D3D_OPCODE_DSS_FUNC(DP4)
EMIT_D3D_OPCODE_DSS_FUNC(MIN)
EMIT_D3D_OPCODE_DSS_FUNC(MAX)
EMIT_D3D_OPCODE_DSS_FUNC(SLT)
EMIT_D3D_OPCODE_DSS_FUNC(SGE)
EMIT_D3D_OPCODE_DS_FUNC(EXP)
EMIT_D3D_OPCODE_DS_FUNC(LOG)
EMIT_D3D_OPCODE_DS_FUNC(LIT)
EMIT_D3D_OPCODE_DSS_FUNC(DST)
EMIT_D3D_OPCODE_DSSS_FUNC(LRP)
EMIT_D3D_OPCODE_DS_FUNC(FRC)
EMIT_D3D_OPCODE_DSS_FUNC(M4X4)
EMIT_D3D_OPCODE_DSS_FUNC(M4X3)
EMIT_D3D_OPCODE_DSS_FUNC(M3X4)
EMIT_D3D_OPCODE_DSS_FUNC(M3X3)
EMIT_D3D_OPCODE_DSS_FUNC(M3X2)
EMIT_D3D_OPCODE_S_FUNC(CALL)
EMIT_D3D_OPCODE_SS_FUNC(CALLNZ)
EMIT_D3D_OPCODE_S_FUNC(LOOP)
EMIT_D3D_OPCODE_FUNC(RET)
EMIT_D3D_OPCODE_FUNC(ENDLOOP)
EMIT_D3D_OPCODE_S_FUNC(LABEL)
EMIT_D3D_OPCODE_FUNC(DCL)  // !!! FIXME!
EMIT_D3D_OPCODE_DSS_FUNC(POW)
EMIT_D3D_OPCODE_DSS_FUNC(CRS)
EMIT_D3D_OPCODE_DSSS_FUNC(SGN)
EMIT_D3D_OPCODE_DS_FUNC(ABS)
EMIT_D3D_OPCODE_DS_FUNC(NRM)
EMIT_D3D_OPCODE_DS_FUNC(SINCOS)
EMIT_D3D_OPCODE_S_FUNC(REP)
EMIT_D3D_OPCODE_FUNC(ENDREP)
EMIT_D3D_OPCODE_S_FUNC(IF)
EMIT_D3D_OPCODE_SS_FUNC(IFC)
EMIT_D3D_OPCODE_FUNC(ELSE)
EMIT_D3D_OPCODE_FUNC(ENDIF)
EMIT_D3D_OPCODE_FUNC(BREAK)
EMIT_D3D_OPCODE_SS_FUNC(BREAKC)
EMIT_D3D_OPCODE_DS_FUNC(MOVA)
EMIT_D3D_OPCODE_FUNC(DEFB) // !!! FIXME!
EMIT_D3D_OPCODE_FUNC(DEFI) // !!! FIXME!
EMIT_D3D_OPCODE_FUNC(TEXCOORD) // !!! FIXME!
EMIT_D3D_OPCODE_D_FUNC(TEXKILL)
EMIT_D3D_OPCODE_FUNC(TEX) // !!! FIXME!
EMIT_D3D_OPCODE_DS_FUNC(TEXBEM)
EMIT_D3D_OPCODE_DS_FUNC(TEXBEML)
EMIT_D3D_OPCODE_DS_FUNC(TEXREG2AR)
EMIT_D3D_OPCODE_DS_FUNC(TEXREG2GB)
EMIT_D3D_OPCODE_DS_FUNC(TEXM3X2PAD)
EMIT_D3D_OPCODE_DS_FUNC(TEXM3X2TEX)
EMIT_D3D_OPCODE_DS_FUNC(TEXM3X3PAD)
EMIT_D3D_OPCODE_DS_FUNC(TEXM3X3TEX)
EMIT_D3D_OPCODE_DSS_FUNC(TEXM3X3SPEC)
EMIT_D3D_OPCODE_DS_FUNC(TEXM3X3VSPEC)
EMIT_D3D_OPCODE_DS_FUNC(EXPP)
EMIT_D3D_OPCODE_DS_FUNC(LOGP)
EMIT_D3D_OPCODE_DSSS_FUNC(CND)
EMIT_D3D_OPCODE_FUNC(DEF) // !!! FIXME!
EMIT_D3D_OPCODE_DS_FUNC(TEXREG2RGB)
EMIT_D3D_OPCODE_DS_FUNC(TEXDP3TEX)
EMIT_D3D_OPCODE_DS_FUNC(TEXM3X2DEPTH)
EMIT_D3D_OPCODE_DS_FUNC(TEXDP3)
EMIT_D3D_OPCODE_DS_FUNC(TEXM3X3)
EMIT_D3D_OPCODE_D_FUNC(TEXDEPTH)
EMIT_D3D_OPCODE_DSSS_FUNC(CMP)
EMIT_D3D_OPCODE_DSS_FUNC(BEM)
EMIT_D3D_OPCODE_DSSS_FUNC(DP2ADD)
EMIT_D3D_OPCODE_DS_FUNC(DSX)
EMIT_D3D_OPCODE_DS_FUNC(DSY)
EMIT_D3D_OPCODE_DSSSS_FUNC(TEXLDD)
EMIT_D3D_OPCODE_DSS_FUNC(SETP)
EMIT_D3D_OPCODE_DSS_FUNC(TEXLDL)
EMIT_D3D_OPCODE_DS_FUNC(BREAKP)

#undef EMIT_D3D_OPCODE_FUNC
#undef EMIT_D3D_OPCODE_D_FUNC
#undef EMIT_D3D_OPCODE_S_FUNC
#undef EMIT_D3D_OPCODE_SS_FUNC
#undef EMIT_D3D_OPCODE_DS_FUNC
#undef EMIT_D3D_OPCODE_DSS_FUNC
#undef EMIT_D3D_OPCODE_DSSS_FUNC
#undef EMIT_D3D_OPCODE_DSSSS_FUNC

#endif  // SUPPORT_PROFILE_D3D



#if !SUPPORT_PROFILE_GLSL
#define PROFILE_EMITTER_GLSL(op)
#else
#undef AT_LEAST_ONE_PROFILE
#define AT_LEAST_ONE_PROFILE 1
#define PROFILE_EMITTER_GLSL(op) emit_GLSL_##op,

static void emit_GLSL_start(Context *ctx)
{
    const uint major = (uint) ctx->major_ver;
    const uint minor = (uint) ctx->minor_ver;
    if (ctx->shader_type == SHADER_TYPE_PIXEL)
        output_line(ctx, "// Pixel shader, version %u.%u", major, minor);
    else if (ctx->shader_type == SHADER_TYPE_VERTEX)
        output_line(ctx, "// Vertex shader, version %u.%u", major, minor);
    else
    {
        failf(ctx, "Shader type %u unsupported in this profile.",
              (uint) ctx->shader_type);
    } // else

    output_line(ctx, "void main() {");
} // emit_GLSL_start

static void emit_GLSL_end(Context *ctx)
{
    output_line(ctx, "}");
} // emit_GLSL_end

static void emit_GLSL_comment(Context *ctx, const char *str)
{
    output_line(ctx, "// %s", str);
} // emit_GLSL_comment

static void emit_GLSL_NOP(Context *ctx)
{
    // no-op is a no-op.  :)
} // emit_GLSL_NOP

static void emit_GLSL_MOV(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_MOV

static void emit_GLSL_ADD(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_ADD

static void emit_GLSL_SUB(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_SUB

static void emit_GLSL_MAD(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_MAD

static void emit_GLSL_MUL(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_MUL

static void emit_GLSL_RCP(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_RCP

static void emit_GLSL_RSQ(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_RSQ

static void emit_GLSL_DP3(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DP3

static void emit_GLSL_DP4(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DP4

static void emit_GLSL_MIN(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_MIN

static void emit_GLSL_MAX(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_MAX

static void emit_GLSL_SLT(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_SLT

static void emit_GLSL_SGE(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_SGE

static void emit_GLSL_EXP(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_EXP

static void emit_GLSL_LOG(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_LOG

static void emit_GLSL_LIT(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_LIT

static void emit_GLSL_DST(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DST

static void emit_GLSL_LRP(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_LRP

static void emit_GLSL_FRC(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_FRC

static void emit_GLSL_M4X4(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_M4X4

static void emit_GLSL_M4X3(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_M4X3

static void emit_GLSL_M3X4(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_M3X4

static void emit_GLSL_M3X3(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_M3X3

static void emit_GLSL_M3X2(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_M3X2

static void emit_GLSL_CALL(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_CALL

static void emit_GLSL_CALLNZ(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_CALLNZ

static void emit_GLSL_LOOP(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_LOOP

static void emit_GLSL_RET(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_RET

static void emit_GLSL_ENDLOOP(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_ENDLOOP

static void emit_GLSL_LABEL(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_LABEL

static void emit_GLSL_DCL(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DCL

static void emit_GLSL_POW(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_POW

static void emit_GLSL_CRS(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_CRS

static void emit_GLSL_SGN(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_SGN

static void emit_GLSL_ABS(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_ABS

static void emit_GLSL_NRM(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_NRM

static void emit_GLSL_SINCOS(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_SINCOS

static void emit_GLSL_REP(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_REP

static void emit_GLSL_ENDREP(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_ENDREP

static void emit_GLSL_IF(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_IF

static void emit_GLSL_IFC(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_IFC

static void emit_GLSL_ELSE(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_ELSE

static void emit_GLSL_ENDIF(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_ENDIF

static void emit_GLSL_BREAK(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_BREAK

static void emit_GLSL_BREAKC(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_BREAKC

static void emit_GLSL_MOVA(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_MOVA

static void emit_GLSL_DEFB(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DEFB

static void emit_GLSL_DEFI(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DEFI

static void emit_GLSL_TEXCOORD(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXCOORD

static void emit_GLSL_TEXKILL(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXKILL

static void emit_GLSL_TEX(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEX

static void emit_GLSL_TEXBEM(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXBEM

static void emit_GLSL_TEXBEML(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXBEML

static void emit_GLSL_TEXREG2AR(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXREG2AR

static void emit_GLSL_TEXREG2GB(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXREG2GB

static void emit_GLSL_TEXM3X2PAD(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X2PAD

static void emit_GLSL_TEXM3X2TEX(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X2TEX

static void emit_GLSL_TEXM3X3PAD(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X3PAD

static void emit_GLSL_TEXM3X3TEX(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X3TEX

static void emit_GLSL_TEXM3X3SPEC(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X3SPEC

static void emit_GLSL_TEXM3X3VSPEC(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X3VSPEC

static void emit_GLSL_EXPP(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_EXPP

static void emit_GLSL_LOGP(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_LOGP

static void emit_GLSL_CND(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_CND

static void emit_GLSL_DEF(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DEF

static void emit_GLSL_TEXREG2RGB(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXREG2RGB

static void emit_GLSL_TEXDP3TEX(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXDP3TEX

static void emit_GLSL_TEXM3X2DEPTH(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X2DEPTH

static void emit_GLSL_TEXDP3(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXDP3

static void emit_GLSL_TEXM3X3(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X3

static void emit_GLSL_TEXDEPTH(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXDEPTH

static void emit_GLSL_CMP(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_CMP

static void emit_GLSL_BEM(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_BEM

static void emit_GLSL_DP2ADD(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DP2ADD

static void emit_GLSL_DSX(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DSX

static void emit_GLSL_DSY(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DSY

static void emit_GLSL_TEXLDD(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXLDD

static void emit_GLSL_SETP(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_SETP

static void emit_GLSL_TEXLDL(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXLDL

static void emit_GLSL_BREAKP(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_BREAKP

static void emit_GLSL_RESERVED(Context *ctx)
{
    // do nothing; fails in the state machine.
} // emit_GLSL_RESERVED

#endif  // SUPPORT_PROFILE_GLSL



#if !AT_LEAST_ONE_PROFILE
#error No profiles are supported. Fix your build.
#endif

static const D3D2GLSL_profile profiles[] =
{
#if SUPPORT_PROFILE_D3D
    { "d3d", emit_D3D_start, emit_D3D_end, emit_D3D_comment },
#endif
#if SUPPORT_PROFILE_GLSL
    { "glsl", emit_GLSL_start, emit_GLSL_end, emit_GLSL_comment },
#endif
};

// The PROFILE_EMITTER_* items MUST be in the same order as profiles[]!
#define PROFILE_EMITTERS(op) { \
     PROFILE_EMITTER_D3D(op) \
     PROFILE_EMITTER_GLSL(op) \
}



// State machine functions...

static int state_RESERVED(Context *ctx)
{
    return fail(ctx, "Tried to use RESERVED opcode.");
} // state_RESERVED



// Lookup table for instruction opcodes...

typedef struct
{
    const char *opcode_string;
    int arg_tokens;
    //uint32 shader_requirements;
    state_function state;
    emit_function emitter[STATICARRAYLEN(profiles)];
} Instruction;

// These have to be in the right order! This array is indexed by the value
//  of the instruction token.
static Instruction instructions[] = {
    // INSTRUCTION_STATE means this opcode has to update the state machine
    //  (we're entering an ELSE block, etc). INSTRUCTION means there's no
    //  state, just go straight to the emitters.
    #define INSTRUCTION_STATE(op, args) { #op, args, state_##op, PROFILE_EMITTERS(op) }
    #define INSTRUCTION(op, args) { #op, args, NULL, PROFILE_EMITTERS(op) }
    INSTRUCTION(NOP, 0),
    INSTRUCTION(MOV, 2),
    INSTRUCTION(ADD, 3),
    INSTRUCTION(SUB, 3),
    INSTRUCTION(MAD, 4),
    INSTRUCTION(MUL, 3),
    INSTRUCTION(RCP, 2),
    INSTRUCTION(RSQ, 2),
    INSTRUCTION(DP3, 3),
    INSTRUCTION(DP4, 3),
    INSTRUCTION(MIN, 3),
    INSTRUCTION(MAX, 3),
    INSTRUCTION(SLT, 3),
    INSTRUCTION(SGE, 3),
    INSTRUCTION(EXP, 2),
    INSTRUCTION(LOG, 2),
    INSTRUCTION(LIT, 2),
    INSTRUCTION(DST, 3),
    INSTRUCTION(LRP, 4),
    INSTRUCTION(FRC, 2),
    INSTRUCTION(M4X4, 3),
    INSTRUCTION(M4X3, 3),
    INSTRUCTION(M3X4, 3),
    INSTRUCTION(M3X3, 3),
    INSTRUCTION(M3X2, 3),
    INSTRUCTION(CALL, 1),
    INSTRUCTION(CALLNZ, 2),
    INSTRUCTION(LOOP, 2),
    INSTRUCTION(RET, 0),
    INSTRUCTION(ENDLOOP, 0),
    INSTRUCTION(LABEL, 1),
    INSTRUCTION(DCL, -1),
    INSTRUCTION(POW, 3),
    INSTRUCTION(CRS, 3),
    INSTRUCTION(SGN, 4),
    INSTRUCTION(ABS, 2),
    INSTRUCTION(NRM, 2),
    INSTRUCTION(SINCOS, 4),
    INSTRUCTION(REP, 1),
    INSTRUCTION(ENDREP, 0),
    INSTRUCTION(IF, 1),
    INSTRUCTION(IFC, 2),
    INSTRUCTION(ELSE, 0),
    INSTRUCTION(ENDIF, 0),
    INSTRUCTION(BREAK, 0),
    INSTRUCTION(BREAKC, 2),
    INSTRUCTION(MOVA, 2),
    INSTRUCTION(DEFB, 2),
    INSTRUCTION(DEFI, 5),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION(TEXCOORD, -1),
    INSTRUCTION(TEXKILL, 1),
    INSTRUCTION(TEX, -1),
    INSTRUCTION(TEXBEM, 2),
    INSTRUCTION(TEXBEML, 2),
    INSTRUCTION(TEXREG2AR, 2),
    INSTRUCTION(TEXREG2GB, 2),
    INSTRUCTION(TEXM3X2PAD, 2),
    INSTRUCTION(TEXM3X2TEX, 2),
    INSTRUCTION(TEXM3X3PAD, 2),
    INSTRUCTION(TEXM3X3TEX, 2),
    INSTRUCTION_STATE(RESERVED, 0),
    INSTRUCTION(TEXM3X3SPEC, 3),
    INSTRUCTION(TEXM3X3VSPEC, 2),
    INSTRUCTION(EXPP, 2),
    INSTRUCTION(LOGP, 2),
    INSTRUCTION(CND, 4),
    INSTRUCTION(DEF, 5),
    INSTRUCTION(TEXREG2RGB, 2),
    INSTRUCTION(TEXDP3TEX, 2),
    INSTRUCTION(TEXM3X2DEPTH, 2),
    INSTRUCTION(TEXDP3, 2),
    INSTRUCTION(TEXM3X3, 2),
    INSTRUCTION(TEXDEPTH, 1),
    INSTRUCTION(CMP, 4),
    INSTRUCTION(BEM, 3),
    INSTRUCTION(DP2ADD, 4),
    INSTRUCTION(DSX, 2),
    INSTRUCTION(DSY, 2),
    INSTRUCTION(TEXLDD, 5),
    INSTRUCTION(SETP, 3),
    INSTRUCTION(TEXLDL, 3),
    INSTRUCTION(BREAKP, 1),  // src
    #undef INSTRUCTION
    #undef INSTRUCTION_STATE
};


// parse various token types...

static int parse_instruction_token(Context *ctx)
{
    const uint32 *start_tokens = ctx->tokens;
    const uint32 start_tokencount = ctx->tokencount;
    const uint32 token = SWAP32(*(ctx->tokens));
    const uint32 opcode = (token & 0xFFFF);
    const uint32 controls = ((token >> 16) & 0xFF);
    const uint32 insttoks = ((token >> 24) & 0x0F);
    const int coissue = (token & 0x40000000) ? 1 : 0;
    const int predicated = (token & 0x10000000) ? 1 : 0;
    const Instruction *instruction = &instructions[opcode];
    const emit_function emitter = instruction->emitter[ctx->profileid];
    int retval = FAIL;

    if ( opcode >= (sizeof (instructions) / sizeof (instructions[0])) )
        return 0;  // not an instruction token, or just not handled here.

    if ((token & 0x80000000) != 0)
        return fail(ctx, "instruction token high bit must be zero.");  // so says msdn.

    if (coissue)  // !!! FIXME: I'm not sure what this means, yet.
        return fail(ctx, "coissue instructions unsupported");

    if (instruction->arg_tokens >= 0)
    {
        if (instruction->arg_tokens != insttoks)
        {
            return failf(ctx,
                    "unexpected number of tokens (%u) for instruction '%s'.",
                    (uint) insttoks, instruction->opcode_string);
        } // if
        else if (ctx->tokencount <= instruction->arg_tokens)
        {
            return failf(ctx,
                    "need more tokens (need %u, got %u) for instruction '%s'.",
                    (uint) instruction->arg_tokens, (uint) ctx->tokencount,
                    instruction->opcode_string);
        } // else if
    } // if

    if (instruction->state != NULL)  // update state machine
        retval = instruction->state(ctx);
    else
        retval = insttoks + 1;

    if (ctx->failstr == NULL)  // only do this if there wasn't a previous fail.
        emitter(ctx);  // call the profile's emitter.

    ctx->tokens = start_tokens;  // in case emitters ate anything.
    ctx->tokencount = start_tokencount;  // in case emitters ate anything.

    return retval;
} // parse_instruction_token


static int parse_version_token(Context *ctx)
{
    if (ctx->tokencount == 0)
        return fail(ctx, "Expected version token, got none at all.");

    const uint32 token = SWAP32(*(ctx->tokens));
    const uint32 shadertype = ((token >> 16) & 0xFFFF);

    ctx->major_ver = (uint32) ((token >> 8) & 0xFF);
    ctx->minor_ver = (uint32) (token & 0xFF);

    // 0xFFFF == pixel shader, 0xFFFE == vertex shader
    if (shadertype == 0xFFFF)
        ctx->shader_type = SHADER_TYPE_PIXEL;
    else if (shadertype == 0xFFFE)
        ctx->shader_type = SHADER_TYPE_VERTEX;
    else
        return fail(ctx, "Unsupported shader type or not a shader at all");

    ctx->profile->start_emitter(ctx);
    return 1;  // ate one token.
} // parse_version_token


static int parse_comment_token(Context *ctx)
{
    const uint32 token = SWAP32(*(ctx->tokens));
    if ((token & 0xFFFF) != 0xFFFE)
        return 0;  // not a comment token.
    else if ((token & 0x80000000) != 0)
        return fail(ctx, "comment token high bit must be zero.");  // so says msdn.
    else
    {
        const uint32 commenttoks = ((token >> 16) & 0xFFFF);
        const uint32 len = commenttoks * sizeof (uint32);
        const int needmalloc = (len >= D3D2GLSL_SCRATCH_BUFFER_SIZE);
        char *str = ((needmalloc) ? (char *) ctx->malloc(len + 1) :
                        get_scratch_buffer(ctx));
        memcpy(str, (const char *) (ctx->tokens+1), len);
        str[len] = '\0';
        ctx->profile->comment_emitter(ctx, str);
        if (needmalloc)
            ctx->free(str);

        return commenttoks + 1;  // comment data plus the initial token.
    } // else

    // shouldn't hit this.
    return failf(ctx, "Logic error at %s:%d", __FILE__, __LINE__);
} // parse_comment_token


static int parse_end_token(Context *ctx)
{
    if (SWAP32(*(ctx->tokens)) != 0x0000FFFF)   // end token always 0x0000FFFF.
        return 0;  // not us, eat no tokens.

    if (ctx->tokencount != 1)  // we _must_ be last. If not: fail.
        return fail(ctx, "end token before end of stream");

    ctx->profile->end_emitter(ctx);

    return END_OF_STREAM;
} // parse_end_token


static int parse_phase_token(Context *ctx)
{
    if (SWAP32(*(ctx->tokens)) != 0x0000FFFD) // phase token always 0x0000FFFD.
        return 0;  // not us, eat no tokens.
    return fail(ctx, "not sure what this thing is yet.");
} // parse_phase_token


static int parse_token(Context *ctx)
{
    int rc = 0;

    if (ctx->failstr != NULL)
        return FAIL;  // just in case...catch previously unhandled fails here.

    if (ctx->tokencount == 0)
        return fail(ctx, "unexpected end of shader.");

    if ((rc = parse_comment_token(ctx)) != 0)
        return rc;

    if ((rc = parse_end_token(ctx)) != 0)
        return rc;

    if ((rc = parse_phase_token(ctx)) != 0)
        return rc;

    if ((rc = parse_instruction_token(ctx)) != 0)
        return rc;

    return failf(ctx, "unknown token (%u)", (uint) *ctx->tokens);
} // parse_token


static void *internal_malloc(int bytes) { return malloc(bytes); }
static void internal_free(void *ptr) { free(ptr); }


static int find_profile_id(const char *profile)
{
    int i;
    for (i = 0; i < STATICARRAYLEN(profiles); i++)
    {
        const char *name = profiles[i].name;
        if (strcmp(name, profile) == 0)
            return i;
    } // for
    return -1;  // no match.
} // find_profile_id


static Context *build_context(const char *profile,
                                       const unsigned char *tokenbuf,
                                       const unsigned int bufsize,
                                       D3D2GLSL_malloc m, D3D2GLSL_free f)
{
    if (m == NULL) m = internal_malloc;
    if (f == NULL) f = internal_free;

    Context *ctx = m(sizeof (Context));
    if (ctx == NULL)
        return NULL;

    memset(ctx, '\0', sizeof (Context));
    ctx->malloc = m;
    ctx->free = f;
    ctx->tokens = (const uint32 *) tokenbuf;
    ctx->tokencount = bufsize / sizeof (uint32);
    ctx->endline = "\n";
    ctx->endline_len = 1;  // !!! FIXME: do "\r\n" on Windows?
    ctx->output.str = NULL;
    ctx->output.next = NULL;
    ctx->output_tail = &ctx->output;

    const int profileid = find_profile_id(profile);
    ctx->profileid = profileid;
    if (profileid >= 0)  // we'll fail later, but we still need the context!
        ctx->profile = &profiles[profileid];

    return ctx;
} // build_context


static void destroy_context(Context *ctx)
{
    OutputList *item = ctx->output.next;
    while (item != NULL)
    {
        OutputList *next = item->next;
        ctx->free(item->str);
        ctx->free(item);
        item = next;
    } // for

    if (ctx->failstr != out_of_mem_string)
        ctx->free((void *) ctx->failstr);
    ctx->free(ctx);
} // destroy_context


static char *build_output(Context *ctx)
{
    char *retval = (char *) ctx->malloc(ctx->output_len + 1);
    if (retval == NULL)
        out_of_memory(ctx);
    else
    {
        const char *endline = ctx->endline;
        const size_t endline_len = ctx->endline_len;
        char *wptr = retval;
        OutputList *item = ctx->output.next;
        while (item != NULL)
        {
            const size_t len = strlen(item->str);
            memcpy(wptr, item->str, len);
            wptr += len;
            memcpy(wptr, endline, endline_len);
            wptr += endline_len;
            item = item->next;
        } // while
        *wptr = '\0';
    } // else

    return retval;
} // build_output


// API entry point...

int D3D2GLSL_parse(const char *profile, const unsigned char *tokenbuf,
                   const unsigned int bufsize, D3D2GLSL_malloc m,
                   D3D2GLSL_free f)
{
    int rc = FAIL;

    Context *ctx = build_context(profile, tokenbuf, bufsize, m, f);
    if (ctx == NULL)
        return 0;  // !!! FIXME: error string?

    if (ctx->profile == NULL)
        failf(ctx, "Profile '%s' is unknown or unsupported", profile);

    if (ctx->failstr == NULL)  // only go on if there was no previous error...
    {
        // Version token always comes first.
        rc = parse_version_token(ctx);

        // parse out the rest of the tokens after the version token...
        while (rc > 0)
        {
            ctx->tokens += rc;
            ctx->tokencount -= rc;
            rc = parse_token(ctx);
        } // while
    } // if

    if (ctx->failstr == NULL)
    {
        char *str = build_output(ctx);
        if (str != NULL)
        {
            printf("OUTPUT:\n%s\n", str);  // !!! FIXME: report to caller.
            ctx->free(str);
        } // if
    } // if

    if (ctx->failstr != NULL)
        printf("FAIL: %s\n", ctx->failstr);

    destroy_context(ctx);

    return (rc == END_OF_STREAM);
} // D3D2GLSL_parse

// end of parse.c ...

