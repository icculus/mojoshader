/**
 * MojoShader; generate shader programs from bytecode of compiled
 *  Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

// !!! FIXME: I keep changing coding styles for symbols and typedefs.

// !!! FIXME: do DEF* opcodes have to come before instructions?
// !!! FIXME:  my reading of the msdn spec suggests no, but it sounds like
// !!! FIXME:  something they'd require. DCL_* _does_ have to be first.


// Shader bytecode format is described at MSDN:
//  http://msdn2.microsoft.com/en-us/library/ms800307.aspx

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <assert.h>

#include "mojoshader.h"


// This is the highest shader version we currently support.

#define MAX_SHADER_MAJOR 3
#define MAX_SHADER_MINOR 0


// If SUPPORT_PROFILE_* isn't defined, we assume an implicit desire to support.
//  You get all the profiles unless you go out of your way to disable them.

#ifndef SUPPORT_PROFILE_D3D
#define SUPPORT_PROFILE_D3D 1
#endif

#ifndef SUPPORT_PROFILE_PASSTHROUGH
#define SUPPORT_PROFILE_PASSTHROUGH 1
#endif

#ifndef SUPPORT_PROFILE_GLSL
#define SUPPORT_PROFILE_GLSL 1
#endif


// Get basic wankery out of the way here...

typedef unsigned int uint;  // this is a printf() helper. don't use for code.
typedef uint8_t uint8;
typedef uint32_t uint32;
typedef int32_t int32;

#ifdef __GNUC__
#define ISPRINTF(x,y) __attribute__((format (printf, x, y)))
#else
#define ISPRINTF(x,y)
#endif

#define STATICARRAYLEN(x) ( (sizeof ((x))) / (sizeof ((x)[0])) )

#ifdef _WINDOWS  // !!! FIXME: bleh
const char *endline_str = "\r\n";
#else
const char *endline_str = "\n";
#endif


// we need to reference this by explicit value occasionally.
#define OPCODE_RET 28


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

typedef enum
{
    REG_TYPE_TEMP = 0,
    REG_TYPE_INPUT = 1,
    REG_TYPE_CONST = 2,
    REG_TYPE_ADDRESS = 3,
    REG_TYPE_TEXTURE = 3,  // ALSO 3!
    REG_TYPE_RASTOUT = 4,
    REG_TYPE_ATTROUT = 5,
    REG_TYPE_TEXCRDOUT = 6,
    REG_TYPE_OUTPUT = 6,  // ALSO 6!
    REG_TYPE_CONSTINT = 7,
    REG_TYPE_COLOROUT = 8,
    REG_TYPE_DEPTHOUT = 9,
    REG_TYPE_SAMPLER = 10,
    REG_TYPE_CONST2 = 11,
    REG_TYPE_CONST3 = 12,
    REG_TYPE_CONST4 = 13,
    REG_TYPE_CONSTBOOL = 14,
    REG_TYPE_LOOP = 15,
    REG_TYPE_TEMPFLOAT16 = 16,
    REG_TYPE_MISCTYPE = 17,
    REG_TYPE_LABEL = 18,
    REG_TYPE_PREDICATE = 19,
    REG_TYPE_MAX = 19
} RegisterType;

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

// one emit function for finalizing output in each profile.
typedef void (*emit_finalize)(Context *ctx);

// one emit function for global definitions in each profile.
typedef void (*emit_global)(Context *ctx, RegisterType regtype, int regnum);

// one emit function for uniforms in each profile.
typedef void (*emit_uniform)(Context *ctx, RegisterType regtype, int regnum);

// one emit function for attributes in each profile.
typedef void (*emit_attribute)(Context *ctx, RegisterType regtype, int regnum,
                               MOJOSHADER_usage usage, int index, int wmask);

// one args function for each possible sequence of opcode arguments.
typedef int (*args_function)(Context *ctx);

// one state function for each opcode where we have state machine updates.
typedef void (*state_function)(Context *ctx);

typedef struct
{
    const char *name;
    emit_start start_emitter;
    emit_end end_emitter;
    emit_comment comment_emitter;
    emit_global global_emitter;
    emit_uniform uniform_emitter;
    emit_attribute attribute_emitter;
    emit_finalize finalize_emitter;
} Profile;

typedef enum
{
    RASTOUT_TYPE_POSITION = 0,
    RASTOUT_TYPE_FOG = 1,
    RASTOUT_TYPE_POINT_SIZE = 2,
    RASTOUT_TYPE_MAX = 2
} RastOutType;

typedef enum
{
    MISCTYPE_TYPE_POSITION = 0,
    MISCTYPE_TYPE_FACE = 1,
    MISCTYPE_TYPE_MAX = 1
} MiscTypeType;

typedef enum
{
    TEXTURE_TYPE_2D = 2,
    TEXTURE_TYPE_CUBE = 3,
    TEXTURE_TYPE_VOLUME = 4,
} TextureType;


// A simple linked list of strings, so we can build the final output without
//  realloc()ing for each new line, and easily insert lines into the middle
//  of the output without much trouble.
typedef struct OutputListNode
{
    char *str;
    struct OutputListNode *next;
} OutputListNode;

typedef struct OutputList
{
    OutputListNode head;
    OutputListNode *tail;
} OutputList;


typedef struct RegisterList
{
    RegisterType regtype;
    int regnum;
    MOJOSHADER_usage usage;
    int index;
    int writemask;
    struct RegisterList *next;
} RegisterList;


// result modifiers.
#define MOD_SATURATE 0x01
#define MOD_PP 0x02
#define MOD_CENTROID 0x04

// source modifiers.
typedef enum
{
    SRCMOD_NONE,
    SRCMOD_NEGATE,
    SRCMOD_BIAS,
    SRCMOD_BIASNEGATE,
    SRCMOD_SIGN,
    SRCMOD_SIGNNEGATE,
    SRCMOD_COMPLEMENT,
    SRCMOD_X2,
    SRCMOD_X2NEGATE,
    SRCMOD_DZ,
    SRCMOD_DW,
    SRCMOD_ABS,
    SRCMOD_ABSNEGATE,
    SRCMOD_NOT,
    SRCMOD_TOTAL
} SourceMod;


typedef struct
{
    const uint32 *token;   // this is the unmolested token in the stream.
    int regnum;
    int relative;
    int writemask;   // xyzw or rgba (all four, not split out).
    int writemask0;  // x or red
    int writemask1;  // y or green
    int writemask2;  // z or blue
    int writemask3;  // w or alpha
    int result_mod;
    int result_shift;
    RegisterType regtype;
} DestArgInfo;

typedef struct
{
    const uint32 *token;   // this is the unmolested token in the stream.
    int regnum;
    int relative;
    int swizzle;  // xyzw (all four, not split out).
    int swizzle_x;
    int swizzle_y;
    int swizzle_z;
    int swizzle_w;
    int src_mod;
    RegisterType regtype;
} SourceArgInfo;


typedef enum
{
    // Specific to GLSL profile...
    CTX_FLAGS_GLSL_LIT_OPCODE = (1 << 0),
    CTX_FLAGS_GLSL_DST_OPCODE = (1 << 1),
    CTX_FLAGS_GLSL_LRP_OPCODE = (1 << 2),
    CTX_FLAGS_MASK = 0xFFFFFFFF
} ContextFlags;


#define SCRATCH_BUFFER_SIZE 256
#define SCRATCH_BUFFERS 10

// !!! FIXME: the scratch buffers make Context pretty big.
// !!! FIXME:  might be worth having one set of static scratch buffers that
// !!! FIXME:  are mutex protected?

// Context...this is state that changes as we parse through a shader...
struct Context
{
    MOJOSHADER_malloc malloc;
    MOJOSHADER_free free;
    void *malloc_data;
    const uint32 *tokens;
    uint32 tokencount;
    OutputList *output;
    OutputList globals;
    OutputList helpers;
    OutputList subroutines;
    OutputList mainline;
    OutputList ignore;
    OutputList *output_stack[2];
    uint8 *output_bytes;  // can be used instead of the OutputLists.
    int indent_stack[2];
    int output_stack_len;
    int output_len; // total strlen; prevents walking the lists just to malloc.
    int indent;
    const char *endline;
    int endline_len;
    const char *failstr;
    char scratch[SCRATCH_BUFFERS][SCRATCH_BUFFER_SIZE];
    int scratchidx;  // current scratch buffer.
    int profileid;
    const Profile *profile;
    MOJOSHADER_shaderType shader_type;
    uint8 major_ver;
    uint8 minor_ver;
    DestArgInfo dest_args[1];
    SourceArgInfo source_args[5];
    uint32 dwords[4];
    int instruction_count;
    uint32 instruction_controls;
    uint32 previous_opcode;
    ContextFlags flags;
    int loops;
    int reps;
    int cmps;
    RegisterList used_registers;
    RegisterList defined_registers;
    int uniform_count;
    RegisterList uniforms;
    int attribute_count;
    RegisterList attributes;
};


// Convenience functions for allocators...

static inline void *Malloc(const Context *ctx, const int len)
{
    return ctx->malloc(len, ctx->malloc_data);
} // Malloc


static inline void Free(const Context *ctx, void *ptr)
{
    if (ptr != NULL)  // check for NULL in case of dumb free() impl.
        ctx->free(ptr, ctx->malloc_data);
} // Free


// jump between output sections in the context...

static inline void push_output(Context *ctx, OutputList *section)
{
    assert(ctx->output_stack_len < STATICARRAYLEN(ctx->output_stack));
    ctx->output_stack[ctx->output_stack_len] = ctx->output;
    ctx->indent_stack[ctx->output_stack_len] = ctx->indent;
    ctx->output_stack_len++;
    ctx->output = section;
    ctx->indent = 0;
} // push_output

static inline void pop_output(Context *ctx)
{
    assert(ctx->output_stack_len > 0);
    ctx->output_stack_len--;
    ctx->output = ctx->output_stack[ctx->output_stack_len];
    ctx->indent = ctx->indent_stack[ctx->output_stack_len];
} // pop_output



// Shader model version magic...

static inline uint32 ver_ui32(const uint8 major, const uint8 minor)
{
    return ( (((uint32) major) << 16) | (((minor) == 0xFF) ? 0 : (minor)) );
} // version_ui32

static int shader_version_supported(uint8 maj, uint8 min)
{
    return (ver_ui32(maj,min) <= ver_ui32(MAX_SHADER_MAJOR, MAX_SHADER_MINOR));
} // shader_version_supported

static int shader_version_atleast(const Context *ctx, uint8 maj, uint8 min)
{
    return (ver_ui32(ctx->major_ver, ctx->minor_ver) >= ver_ui32(maj, min));
} // shader_version_atleast


// Bit arrays (for storing large arrays of booleans...

static inline char *get_scratch_buffer(Context *ctx)
{
    ctx->scratchidx = (ctx->scratchidx + 1) % SCRATCH_BUFFERS;
    return ctx->scratch[ctx->scratchidx];
} // get_scratch_buffer


// Special-case return values from the parsing pipeline...
#define FAIL (-1)
#define NOFAIL (-2)
#define END_OF_STREAM (-3)

static inline int isfail(const Context *ctx)
{
    return (ctx->failstr != NULL);
} // isfail


static MOJOSHADER_parseData out_of_mem_data = {
    "Out of memory", 0, 0, 0, MOJOSHADER_TYPE_UNKNOWN, 0, 0, 0, 0
};

static const char *out_of_mem_str = "Out of memory";
static inline int out_of_memory(Context *ctx)
{
    if (ctx->failstr == NULL)
        ctx->failstr = out_of_mem_str;  // fail() would call malloc().
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
        const int len = vsnprintf(scratch,SCRATCH_BUFFER_SIZE,fmt,ap);
        va_end(ap);

        char *failstr = (char *) Malloc(ctx, len + 1);
        if (failstr == NULL)
            out_of_memory(ctx);
        else
        {
            // see comments about scratch buffer overflow in output_line().
            if (len < SCRATCH_BUFFER_SIZE)
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
    OutputListNode *item = NULL;

    if (isfail(ctx))
        return FAIL;  // we failed previously, don't go on...

    char *scratch = get_scratch_buffer(ctx);

    const int indent = ctx->indent;
    if (indent > 0)
        memset(scratch, '\t', indent);

    va_list ap;
    va_start(ap, fmt);
    const int len = vsnprintf(scratch+indent, SCRATCH_BUFFER_SIZE-indent, fmt, ap) + indent;
    va_end(ap);

    item = (OutputListNode *) Malloc(ctx, sizeof (OutputListNode));
    if (item == NULL)
        return out_of_memory(ctx);

    item->str = (char *) Malloc(ctx, len + 1);
    if (item->str == NULL)
    {
        Free(ctx, item);
        return out_of_memory(ctx);
    } // if

    // If we overflowed our scratch buffer, that's okay. We were going to
    //  allocate anyhow...the scratch buffer just lets us avoid a second
    //  run of vsnprintf().
    if (len < SCRATCH_BUFFER_SIZE)
        strcpy(item->str, scratch);  // copy it over.
    else
    {
        if (indent > 0)
            memset(item->str, '\t', indent);
        va_start(ap, fmt);
        vsnprintf(item->str+indent, len + 1, fmt, ap);  // rebuild it.
        va_end(ap);
    } // else

    item->next = NULL;

    ctx->output->tail->next = item;
    ctx->output->tail = item;
    ctx->output_len += len + ctx->endline_len;

    return 0;
} // output_line


// this is just to stop gcc whining.
static inline int output_blank_line(Context *ctx)
{
    return output_line(ctx, "%s", "");
} // output_blank_line


// !!! FIXME: this is sort of nasty.
static void floatstr(Context *ctx, char *buf, size_t bufsize, float f,
                     int leavedecimal)
{
    const size_t len = snprintf(buf, bufsize, "%f", f);
    if ((len+2) >= bufsize)
        fail(ctx, "BUG: internal buffer is too small");
    else
    {
        char *end = buf + len;
        char *ptr = strchr(buf, '.');
        if (ptr == NULL)
        {
            if (leavedecimal)
                strcat(buf, ".0");
            return;  // done.
        } // if

        while (--end != ptr)
        {
            if (*end != '0')
            {
                end++;
                break;
            } // if
        } // while
        if ((leavedecimal) && (end == ptr))
            end += 2;
        *end = '\0';  // chop extra '0' or all decimal places off.
    } // else
} // floatstr


// Deal with register lists...  !!! FIXME: I sort of hate this.

static void free_reglist(MOJOSHADER_free f, void *d, RegisterList *item)
{
    while (item != NULL)
    {
        RegisterList *next = item->next;
        f(item, d);
        item = next;
    } // while
} // free_reglist

static inline uint32 reg_to_ui32(const RegisterType regtype, const int regnum)
{
    return ( ((uint32) regtype) | (((uint32) regnum) << 16) );
} // reg_to_uint32

static RegisterList *reglist_insert(Context *ctx, RegisterList *prev,
                                    const RegisterType regtype,
                                    const int regnum)
{
    const uint32 newval = reg_to_ui32(regtype, regnum);
    RegisterList *item = prev->next;
    while (item != NULL)
    {
        const uint32 val = reg_to_ui32(item->regtype, item->regnum);
        if (newval == val)
            return item;  // already set, so we're done.
        else if (newval < val)  // insert it here.
            break;
        else // if (newval > val)
        {
            // keep going, we're not to the insertion point yet.
            prev = item;
            item = item->next;
        } // else
    } // while

    // we need to insert an entry after (prev).
    item = (RegisterList *) Malloc(ctx, sizeof (RegisterList));
    if (item == NULL)
        out_of_memory(ctx);
    else
    {
        item->regtype = regtype;
        item->regnum = regnum;
        item->usage = 0;
        item->index = 0;
        item->writemask = 0;
        item->next = prev->next;
        prev->next = item;
    } // else

    return item;
} // reglist_insert

static RegisterList *reglist_find(RegisterList *prev, const RegisterType rtype,
                                  const int regnum)
{
    const uint32 newval = reg_to_ui32(rtype, regnum);
    RegisterList *item = prev->next;
    while (item != NULL)
    {
        const uint32 val = reg_to_ui32(item->regtype, item->regnum);
        if (newval == val)
            return item;  // here it is.
        else if (newval < val)  // should have been here if it existed.
            return NULL;
        else // if (newval > val)
        {
            // keep going, we're not to the insertion point yet.
            prev = item;
            item = item->next;
        } // else
    } // while

    return NULL;  // wasn't in the list.
} // reglist_find

static inline const RegisterList *reglist_exists(RegisterList *prev,
                                                 const RegisterType regtype,
                                                 const int regnum)
{
    return (reglist_find(prev, regtype, regnum));
} // reglist_exists

static inline void set_used_register(Context *ctx, const RegisterType regtype,
                                     const int regnum)
{
    reglist_insert(ctx, &ctx->used_registers, regtype, regnum);
} // set_used_register

static inline int get_used_register(Context *ctx, const RegisterType regtype,
                                    const int regnum)
{
    return (reglist_exists(&ctx->used_registers, regtype, regnum) != NULL);
} // get_used_register

static inline void set_defined_register(Context *ctx, const RegisterType rtype,
                                        const int regnum)
{
    reglist_insert(ctx, &ctx->defined_registers, rtype, regnum);
} // set_defined_register

static inline int get_defined_register(Context *ctx, const RegisterType rtype,
                                       const int regnum)
{
    return (reglist_exists(&ctx->defined_registers, rtype, regnum) != NULL);
} // get_defined_register

static void add_attribute_register(Context *ctx, const RegisterType rtype,
                                const int regnum, const MOJOSHADER_usage usage,
                                const int index, const int writemask)
{
    RegisterList *item = reglist_insert(ctx, &ctx->attributes, rtype, regnum);
    item->usage = usage;
    item->index = index;
    item->writemask = writemask;
} // add_attribute_register


static inline int replicate_swizzle(const int swizzle)
{
    return ( (((swizzle >> 0) & 0x3) == ((swizzle >> 2) & 0x3)) &&
             (((swizzle >> 2) & 0x3) == ((swizzle >> 4) & 0x3)) &&
             (((swizzle >> 4) & 0x3) == ((swizzle >> 6) & 0x3)) );
} // replicate_swizzle



// D3D stuff that's used in more than just the d3d profile...

static const char *usagestrs[] = {
    "_position", "_blendweight", "_blendindices", "_normal", "_psize",
    "_texcoord", "_tangent", "_binormal", "_tessfactor", "_positiont",
    "_color", "_fog", "_depth", "_sample"
};

static const char *get_D3D_register_string(Context *ctx,
                                           RegisterType regtype,
                                           int regnum, char *regnum_str,
                                           size_t regnum_size)
{
    const char *retval = NULL;
    int has_number = 1;

    switch (regtype)
    {
        case REG_TYPE_TEMP:
            retval = "r";
            break;

        case REG_TYPE_INPUT:
            retval = "v";
            break;

        case REG_TYPE_CONST:
            retval = "c";
            break;

        case REG_TYPE_CONST2:
            retval = "c";
            regnum += 2048;
            break;

        case REG_TYPE_CONST3:
            retval = "c";
            regnum += 4096;
            break;

        case REG_TYPE_CONST4:
            retval = "c";
            regnum += 6144;
            break;

        case REG_TYPE_ADDRESS:  // (or REG_TYPE_TEXTURE, same value.)
            retval = (ctx->shader_type == MOJOSHADER_TYPE_VERTEX) ? "a" : "t";
            break;

        case REG_TYPE_RASTOUT:
            switch ((RastOutType) regnum)
            {
                case RASTOUT_TYPE_POSITION: retval = "oPos"; break;
                case RASTOUT_TYPE_FOG: retval = "oFog"; break;
                case RASTOUT_TYPE_POINT_SIZE: retval = "oPts"; break;
            } // switch
            has_number = 0;
            break;

        case REG_TYPE_ATTROUT:
            retval = "oD";
            break;

        case REG_TYPE_OUTPUT: // (or REG_TYPE_TEXCRDOUT, same value.)
            if ((ctx->shader_type==MOJOSHADER_TYPE_VERTEX) && (ctx->major_ver>=3))
                retval = "o";
            else
                retval = "oT";
            break;

        case REG_TYPE_CONSTINT:
            retval = "i";
            break;

        case REG_TYPE_COLOROUT:
            retval = "oC";
            break;

        case REG_TYPE_DEPTHOUT:
            retval = "oDepth";
            has_number = 0;
            break;

        case REG_TYPE_SAMPLER:
            retval = "s";
            break;

        case REG_TYPE_CONSTBOOL:
            retval = "b";
            break;

        case REG_TYPE_LOOP:
            retval = "aL";
            has_number = 0;
            break;

        // !!! FIXME: don't know what the asm string is for this..
        case REG_TYPE_TEMPFLOAT16:
            fail(ctx, "don't know the ASM for tempfloat16 register");
            retval = "???";
            has_number = 0;
            break;

        case REG_TYPE_MISCTYPE:
            switch ((MiscTypeType) regnum)
            {
                case MISCTYPE_TYPE_POSITION: retval = "vPos"; break;
                case MISCTYPE_TYPE_FACE: retval = "vFace"; break;
            } // switch
            has_number = 0;
            break;

        case REG_TYPE_LABEL:
            retval = "l";
            break;

        case REG_TYPE_PREDICATE:
            retval = "p";
            break;
    } // switch

    if (has_number)
        snprintf(regnum_str, regnum_size, "%u", (uint) regnum);
    else
        regnum_str[0] = '\0';

    return retval;
} // get_D3D_register_string



#define AT_LEAST_ONE_PROFILE 0

#if !SUPPORT_PROFILE_D3D
#define PROFILE_EMITTER_D3D(op)
#else
#undef AT_LEAST_ONE_PROFILE
#define AT_LEAST_ONE_PROFILE 1
#define PROFILE_EMITTER_D3D(op) emit_D3D_##op,

static const char *make_D3D_destarg_string(Context *ctx, const int idx)
{
    if (idx >= STATICARRAYLEN(ctx->dest_args))
    {
        fail(ctx, "Too many destination args");
        return "";
    } // if

    const DestArgInfo *arg = &ctx->dest_args[idx];

    const char *result_shift_str = "";
    switch (arg->result_shift)
    {
        case 0x1: result_shift_str = "_x2"; break;
        case 0x2: result_shift_str = "_x4"; break;
        case 0x3: result_shift_str = "_x8"; break;
        case 0xD: result_shift_str = "_d8"; break;
        case 0xE: result_shift_str = "_d4"; break;
        case 0xF: result_shift_str = "_d2"; break;
    } // switch

    const char *sat_str = (arg->result_mod & MOD_SATURATE) ? "_sat" : "";
    const char *pp_str = (arg->result_mod & MOD_PP) ? "_pp" : "";
    const char *cent_str = (arg->result_mod & MOD_CENTROID) ? "_centroid" : "";

    char regnum_str[16];
    const char *regtype_str = get_D3D_register_string(ctx, arg->regtype,
                                                      arg->regnum, regnum_str,
                                                      sizeof (regnum_str));
    if (regtype_str == NULL)
    {
        fail(ctx, "Unknown destination register type.");
        return "";
    } // if

    char writemask_str[6];
    int i = 0;
    if (arg->writemask != 0xF)  // 0xF == 1111. No explicit mask.
    {
        writemask_str[i++] = '.';
        if (arg->writemask0) writemask_str[i++] = 'x';
        if (arg->writemask1) writemask_str[i++] = 'y';
        if (arg->writemask2) writemask_str[i++] = 'z';
        if (arg->writemask3) writemask_str[i++] = 'w';
    } // if
    writemask_str[i] = '\0';
    assert(i < sizeof (writemask_str));

    // may turn out something like "_x2_sat_pp_centroid r0.xyzw" ...
    char *retval = get_scratch_buffer(ctx);
    snprintf(retval, SCRATCH_BUFFER_SIZE, "%s%s%s%s %s%s%s",
             result_shift_str, sat_str, pp_str, cent_str,
             regtype_str, regnum_str, writemask_str);
    // !!! FIXME: make sure the scratch buffer was large enough.
    return retval;
} // make_D3D_destarg_string


static const char *make_D3D_sourcearg_string(Context *ctx, const int idx)
{
    if (idx >= STATICARRAYLEN(ctx->source_args))
    {
        fail(ctx, "Too many source args");
        return "";
    } // if

    const SourceArgInfo *arg = &ctx->source_args[idx];

    const char *premod_str = "";
    const char *postmod_str = "";
    switch ((SourceMod) arg->src_mod)
    {
        case SRCMOD_NEGATE:
            premod_str = "-";
            break;

        case SRCMOD_BIASNEGATE:
            premod_str = "-";
            // fall through.
        case SRCMOD_BIAS:
            postmod_str = "_bias";
            break;

        case SRCMOD_SIGNNEGATE:
            premod_str = "-";
            // fall through.
        case SRCMOD_SIGN:
            postmod_str = "_bx2";
            break;

        case SRCMOD_COMPLEMENT:
            premod_str = "1-";
            break;

        case SRCMOD_X2NEGATE:
            premod_str = "-";
            // fall through.
        case SRCMOD_X2:
            postmod_str = "_x2";
            break;

        case SRCMOD_DZ:
            postmod_str = "_dz";
            break;

        case SRCMOD_DW:
            postmod_str = "_dw";
            break;

        case SRCMOD_ABSNEGATE:
            premod_str = "-";
            // fall through.
        case SRCMOD_ABS:
            postmod_str = "_abs";
            break;

        case SRCMOD_NOT:
            premod_str = "!";
            break;

        case SRCMOD_NONE:
        case SRCMOD_TOTAL:
             break;  // stop compiler whining.
    } // switch


    char regnum_str[16];
    const char *regtype_str = get_D3D_register_string(ctx, arg->regtype,
                                                      arg->regnum, regnum_str,
                                                      sizeof (regnum_str));

    if (regtype_str == NULL)
    {
        fail(ctx, "Unknown source register type.");
        return "";
    } // if

    char swizzle_str[6];
    int i = 0;
    if (arg->swizzle != 0xE4)  // 0xE4 == 11100100 ... 3 2 1 0. No swizzle.
    {
        static const char channel[] = { 'x', 'y', 'z', 'w' };
        swizzle_str[i++] = '.';
        swizzle_str[i++] = channel[arg->swizzle_x];
        swizzle_str[i++] = channel[arg->swizzle_y];
        swizzle_str[i++] = channel[arg->swizzle_z];
        swizzle_str[i++] = channel[arg->swizzle_w];

        // .xyzz is the same as .xyz, .z is the same as .zzzz, etc.
        while (swizzle_str[i-1] == swizzle_str[i-2])
            i--;
    } // if
    swizzle_str[i] = '\0';
    assert(i < sizeof (swizzle_str));

    char *retval = get_scratch_buffer(ctx);
    snprintf(retval, SCRATCH_BUFFER_SIZE, "%s%s%s%s%s",
             premod_str, regtype_str, regnum_str, postmod_str, swizzle_str);
    // !!! FIXME: make sure the scratch buffer was large enough.
    return retval;
} // make_D3D_sourcearg_string


static void emit_D3D_start(Context *ctx)
{
    const uint major = (uint) ctx->major_ver;
    const uint minor = (uint) ctx->minor_ver;
    const char *shadertype_str = NULL;
    char minor_str[16];

    if (minor == 0xFF)
        strcpy(minor_str, "sw");
    else if (minor == 0x1)  // apparently this is "vs_2_x". Weird.
        strcpy(minor_str, "x");
    else
        snprintf(minor_str, sizeof (minor_str), "%u", (uint) minor);

    if (ctx->shader_type == MOJOSHADER_TYPE_PIXEL)
        shadertype_str = "ps";
    else if (ctx->shader_type == MOJOSHADER_TYPE_VERTEX)
        shadertype_str = "vs";
    else
    {
        failf(ctx, "Shader type %u unsupported in this profile.",
              (uint) ctx->shader_type);
        return;
    } // else

    output_line(ctx, "%s_%u_%s", shadertype_str, major, minor_str);
} // emit_D3D_start


static void emit_D3D_end(Context *ctx)
{
    output_line(ctx, "end");
} // emit_D3D_end


static void emit_D3D_finalize(Context *ctx)
{
    // no-op.
} // emit_D3D_finalize


static void emit_D3D_global(Context *ctx, RegisterType regtype, int regnum)
{
    // no-op.
} // emit_D3D_global


static void emit_D3D_uniform(Context *ctx, RegisterType regtype, int regnum)
{
    // no-op.
} // emit_D3D_uniform


static void emit_D3D_attribute(Context *ctx, RegisterType regtype, int regnum,
                               MOJOSHADER_usage usage, int index, int wmask)
{
    // no-op.
} // emit_D3D_attribute


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

static char *lowercase(char *dst, const char *src)
{
    int i = 0;
    do
    {
        const char ch = src[i];
        dst[i] = (((ch >= 'A') && (ch <= 'Z')) ? (ch - ('A' - 'a')) : ch);
    } while (src[i++]);
    return dst;
} // lowercase


static void emit_D3D_opcode_d(Context *ctx, const char *opcode)
{
    const char *dst0 = make_D3D_destarg_string(ctx, 0);
    opcode = lowercase(get_scratch_buffer(ctx), opcode);
    output_line(ctx, "%s%s", opcode, dst0);
} // emit_D3D_opcode_d


static void emit_D3D_opcode_s(Context *ctx, const char *opcode)
{
    const char *src0 = make_D3D_sourcearg_string(ctx, 0);
    opcode = lowercase(get_scratch_buffer(ctx), opcode);
    output_line(ctx, "%s %s", opcode, src0);
} // emit_D3D_opcode_s


static void emit_D3D_opcode_ss(Context *ctx, const char *opcode)
{
    const char *src0 = make_D3D_sourcearg_string(ctx, 0);
    const char *src1 = make_D3D_sourcearg_string(ctx, 1);
    opcode = lowercase(get_scratch_buffer(ctx), opcode);
    output_line(ctx, "%s %s, %s", opcode, src0, src1);
} // emit_D3D_opcode_ss


static void emit_D3D_opcode_ds(Context *ctx, const char *opcode)
{
    const char *dst0 = make_D3D_destarg_string(ctx, 0);
    const char *src0 = make_D3D_sourcearg_string(ctx, 0);
    opcode = lowercase(get_scratch_buffer(ctx), opcode);
    output_line(ctx, "%s%s, %s", opcode, dst0, src0);
} // emit_D3D_opcode_ds


static void emit_D3D_opcode_dss(Context *ctx, const char *opcode)
{
    const char *dst0 = make_D3D_destarg_string(ctx, 0);
    const char *src0 = make_D3D_sourcearg_string(ctx, 0);
    const char *src1 = make_D3D_sourcearg_string(ctx, 1);
    opcode = lowercase(get_scratch_buffer(ctx), opcode);
    output_line(ctx, "%s%s, %s, %s", opcode, dst0, src0, src1);
} // emit_D3D_opcode_dss


static void emit_D3D_opcode_dsss(Context *ctx, const char *opcode)
{
    const char *dst0 = make_D3D_destarg_string(ctx, 0);
    const char *src0 = make_D3D_sourcearg_string(ctx, 0);
    const char *src1 = make_D3D_sourcearg_string(ctx, 1);
    const char *src2 = make_D3D_sourcearg_string(ctx, 2);
    opcode = lowercase(get_scratch_buffer(ctx), opcode);
    output_line(ctx, "%s%s, %s, %s, %s", opcode, dst0, src0, src1, src2);
} // emit_D3D_opcode_dsss


static void emit_D3D_opcode_dssss(Context *ctx, const char *opcode)
{
    const char *dst0 = make_D3D_destarg_string(ctx, 0);
    const char *src0 = make_D3D_sourcearg_string(ctx, 0);
    const char *src1 = make_D3D_sourcearg_string(ctx, 1);
    const char *src2 = make_D3D_sourcearg_string(ctx, 2);
    const char *src3 = make_D3D_sourcearg_string(ctx, 3);
    opcode = lowercase(get_scratch_buffer(ctx), opcode);
    output_line(ctx,"%s%s, %s, %s, %s, %s",opcode,dst0,src0,src1,src2,src3);
} // emit_D3D_opcode_dssss


static void emit_D3D_opcode(Context *ctx, const char *opcode)
{
    opcode = lowercase(get_scratch_buffer(ctx), opcode);
    output_line(ctx, "%s", opcode);
} // emit_D3D_opcode_dssss


#define EMIT_D3D_OPCODE_FUNC(op) \
    static void emit_D3D_##op(Context *ctx) { \
        emit_D3D_opcode(ctx, #op); \
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
EMIT_D3D_OPCODE_SS_FUNC(LOOP)
EMIT_D3D_OPCODE_FUNC(RET)
EMIT_D3D_OPCODE_FUNC(ENDLOOP)
EMIT_D3D_OPCODE_S_FUNC(LABEL)
EMIT_D3D_OPCODE_DSS_FUNC(POW)
EMIT_D3D_OPCODE_DSS_FUNC(CRS)
EMIT_D3D_OPCODE_DSSS_FUNC(SGN)
EMIT_D3D_OPCODE_DS_FUNC(ABS)
EMIT_D3D_OPCODE_DS_FUNC(NRM)
EMIT_D3D_OPCODE_S_FUNC(REP)
EMIT_D3D_OPCODE_FUNC(ENDREP)
EMIT_D3D_OPCODE_S_FUNC(IF)
EMIT_D3D_OPCODE_FUNC(ELSE)
EMIT_D3D_OPCODE_FUNC(ENDIF)
EMIT_D3D_OPCODE_FUNC(BREAK)
EMIT_D3D_OPCODE_DS_FUNC(MOVA)
EMIT_D3D_OPCODE_D_FUNC(TEXKILL)
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
EMIT_D3D_OPCODE_DSS_FUNC(TEXLDL)
EMIT_D3D_OPCODE_S_FUNC(BREAKP)

// special cases for comparison opcodes...
static const char *get_D3D_comparison_string(Context *ctx)
{
    static const char *comps[] = {
        "", "_gt", "_eq", "_ge", "_lt", "_ne", "_le"
    };

    if (ctx->instruction_controls >= STATICARRAYLEN(comps))
    {
        fail(ctx, "unknown comparison control");
        return "";
    } // if

    return comps[ctx->instruction_controls];
} // get_D3D_comparison_string

static void emit_D3D_BREAKC(Context *ctx)
{
    char op[16];
    snprintf(op, sizeof (op), "break%s", get_D3D_comparison_string(ctx));
    emit_D3D_opcode_ss(ctx, op);
} // emit_D3D_BREAKC

static void emit_D3D_IFC(Context *ctx)
{
    char op[16];
    snprintf(op, sizeof (op), "if%s", get_D3D_comparison_string(ctx));
    emit_D3D_opcode_ss(ctx, op);
} // emit_D3D_IFC

static void emit_D3D_SETP(Context *ctx)
{
    char op[16];
    snprintf(op, sizeof (op), "setp%s", get_D3D_comparison_string(ctx));
    emit_D3D_opcode_dss(ctx, op);
} // emit_D3D_SETP

static void emit_D3D_DEF(Context *ctx)
{
    const char *dst0 = make_D3D_destarg_string(ctx, 0);
    const float *val = (const float *) ctx->dwords; // !!! FIXME: could be int?
    char val0[32];
    char val1[32];
    char val2[32];
    char val3[32];
    floatstr(ctx, val0, sizeof (val0), val[0], 0);
    floatstr(ctx, val1, sizeof (val1), val[1], 0);
    floatstr(ctx, val2, sizeof (val2), val[2], 0);
    floatstr(ctx, val3, sizeof (val3), val[3], 0);
    output_line(ctx, "def%s, %s, %s, %s, %s", dst0, val0, val1, val2, val3);
} // emit_D3D_DEF

static void emit_D3D_DEFI(Context *ctx)
{
    const char *dst0 = make_D3D_destarg_string(ctx, 0);
    const int32 *x = (const int32 *) ctx->dwords;
    output_line(ctx, "defi%s, %d, %d, %d, %d", dst0,
                (int) x[0], (int) x[1], (int) x[2], (int) x[3]);
} // emit_D3D_DEFI

static void emit_D3D_DEFB(Context *ctx)
{
    const char *dst0 = make_D3D_destarg_string(ctx, 0);
    output_line(ctx, "defb%s, %s", dst0, ctx->dwords[0] ? "true" : "false");
} // emit_D3D_DEFB


static void emit_D3D_DCL(Context *ctx)
{
    const char *dst0 = make_D3D_destarg_string(ctx, 0);
    const DestArgInfo *arg = &ctx->dest_args[0];
    const char *usage_str = "";
    char index_str[16] = { '\0' };
    const uint32 usage = ctx->dwords[0];

    if (ctx->shader_type == MOJOSHADER_TYPE_VERTEX)
    {
        const uint32 index = ctx->dwords[1];
        usage_str = usagestrs[usage];
        if (index != 0)
            snprintf(index_str, sizeof (index_str), "%u", (uint) index);
    } // if

    else if (ctx->shader_type == MOJOSHADER_TYPE_PIXEL)
    {
        if (arg->regtype == REG_TYPE_SAMPLER)
        {
            switch ((const TextureType) usage)
            {
                case TEXTURE_TYPE_2D: usage_str = "_2d"; break;
                case TEXTURE_TYPE_CUBE: usage_str = "_cube"; break;
                case TEXTURE_TYPE_VOLUME: usage_str = "_volume"; break;
                default: fail(ctx, "unknown sampler texture type"); return;
            } // switch
        } // if
    } // else if

    output_line(ctx, "dcl%s%s%s", usage_str, index_str, dst0);
} // emit_D3D_DCL


static void emit_D3D_TEXCOORD(Context *ctx)
{
    // this opcode looks and acts differently depending on the shader model.
    if (shader_version_atleast(ctx, 1, 4))
        emit_D3D_opcode_ds(ctx, "texcrd");
    else
        emit_D3D_opcode_d(ctx, "texcoord");
} // emit_D3D_TEXCOORD

static void emit_D3D_TEX(Context *ctx)
{
    // this opcode looks and acts differently depending on the shader model.
    if (shader_version_atleast(ctx, 1, 4))
        emit_D3D_opcode_ds(ctx, "tex");
    else
        emit_D3D_opcode_d(ctx, "texld");
} // emit_D3D_TEX

static void emit_D3D_SINCOS(Context *ctx)
{
    // this opcode needs extra registers for sm2 and lower.
    if (!shader_version_atleast(ctx, 3, 0))
        emit_D3D_opcode_dsss(ctx, "sincos");
    else
        emit_D3D_opcode_ds(ctx, "sincos");
} // emit_D3D_SINCOS


#undef EMIT_D3D_OPCODE_FUNC
#undef EMIT_D3D_OPCODE_D_FUNC
#undef EMIT_D3D_OPCODE_S_FUNC
#undef EMIT_D3D_OPCODE_SS_FUNC
#undef EMIT_D3D_OPCODE_DS_FUNC
#undef EMIT_D3D_OPCODE_DSS_FUNC
#undef EMIT_D3D_OPCODE_DSSS_FUNC
#undef EMIT_D3D_OPCODE_DSSSS_FUNC

#endif  // SUPPORT_PROFILE_D3D


#if !SUPPORT_PROFILE_PASSTHROUGH
#define PROFILE_EMITTER_PASSTHROUGH(op)
#else
#undef AT_LEAST_ONE_PROFILE
#define AT_LEAST_ONE_PROFILE 1
#define PROFILE_EMITTER_PASSTHROUGH(op) emit_PASSTHROUGH_##op,

static void emit_PASSTHROUGH_start(Context *ctx)
{
    // just copy the whole token stream and make all other emitters no-ops.
    ctx->output_len = (ctx->tokencount * sizeof (uint32));
    ctx->output_bytes = Malloc(ctx, ctx->output_len);
    if (ctx->output_bytes == NULL)
        out_of_memory(ctx);
    else
        memcpy(ctx->output_bytes, ctx->tokens, ctx->output_len);
} // emit_PASSTHROUGH_start

static void emit_PASSTHROUGH_end(Context *ctx) {}
static void emit_PASSTHROUGH_finalize(Context *ctx) {}
static void emit_PASSTHROUGH_global(Context *ctx, RegisterType t, int n) {}
static void emit_PASSTHROUGH_uniform(Context *ctx, RegisterType t, int n) {}
static void emit_PASSTHROUGH_comment(Context *ctx, const char *str) {}
static void emit_PASSTHROUGH_attribute(Context *ctx, RegisterType t, int n,
                                       MOJOSHADER_usage u, int i, int w) {}

#define EMIT_PASSTHROUGH_OPCODE_FUNC(op) \
    static void emit_PASSTHROUGH_##op(Context *ctx) {}

EMIT_PASSTHROUGH_OPCODE_FUNC(RESERVED)
EMIT_PASSTHROUGH_OPCODE_FUNC(NOP)
EMIT_PASSTHROUGH_OPCODE_FUNC(MOV)
EMIT_PASSTHROUGH_OPCODE_FUNC(ADD)
EMIT_PASSTHROUGH_OPCODE_FUNC(SUB)
EMIT_PASSTHROUGH_OPCODE_FUNC(MAD)
EMIT_PASSTHROUGH_OPCODE_FUNC(MUL)
EMIT_PASSTHROUGH_OPCODE_FUNC(RCP)
EMIT_PASSTHROUGH_OPCODE_FUNC(RSQ)
EMIT_PASSTHROUGH_OPCODE_FUNC(DP3)
EMIT_PASSTHROUGH_OPCODE_FUNC(DP4)
EMIT_PASSTHROUGH_OPCODE_FUNC(MIN)
EMIT_PASSTHROUGH_OPCODE_FUNC(MAX)
EMIT_PASSTHROUGH_OPCODE_FUNC(SLT)
EMIT_PASSTHROUGH_OPCODE_FUNC(SGE)
EMIT_PASSTHROUGH_OPCODE_FUNC(EXP)
EMIT_PASSTHROUGH_OPCODE_FUNC(LOG)
EMIT_PASSTHROUGH_OPCODE_FUNC(LIT)
EMIT_PASSTHROUGH_OPCODE_FUNC(DST)
EMIT_PASSTHROUGH_OPCODE_FUNC(LRP)
EMIT_PASSTHROUGH_OPCODE_FUNC(FRC)
EMIT_PASSTHROUGH_OPCODE_FUNC(M4X4)
EMIT_PASSTHROUGH_OPCODE_FUNC(M4X3)
EMIT_PASSTHROUGH_OPCODE_FUNC(M3X4)
EMIT_PASSTHROUGH_OPCODE_FUNC(M3X3)
EMIT_PASSTHROUGH_OPCODE_FUNC(M3X2)
EMIT_PASSTHROUGH_OPCODE_FUNC(CALL)
EMIT_PASSTHROUGH_OPCODE_FUNC(CALLNZ)
EMIT_PASSTHROUGH_OPCODE_FUNC(LOOP)
EMIT_PASSTHROUGH_OPCODE_FUNC(RET)
EMIT_PASSTHROUGH_OPCODE_FUNC(ENDLOOP)
EMIT_PASSTHROUGH_OPCODE_FUNC(LABEL)
EMIT_PASSTHROUGH_OPCODE_FUNC(POW)
EMIT_PASSTHROUGH_OPCODE_FUNC(CRS)
EMIT_PASSTHROUGH_OPCODE_FUNC(SGN)
EMIT_PASSTHROUGH_OPCODE_FUNC(ABS)
EMIT_PASSTHROUGH_OPCODE_FUNC(NRM)
EMIT_PASSTHROUGH_OPCODE_FUNC(SINCOS)
EMIT_PASSTHROUGH_OPCODE_FUNC(REP)
EMIT_PASSTHROUGH_OPCODE_FUNC(ENDREP)
EMIT_PASSTHROUGH_OPCODE_FUNC(IF)
EMIT_PASSTHROUGH_OPCODE_FUNC(ELSE)
EMIT_PASSTHROUGH_OPCODE_FUNC(ENDIF)
EMIT_PASSTHROUGH_OPCODE_FUNC(BREAK)
EMIT_PASSTHROUGH_OPCODE_FUNC(MOVA)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXKILL)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXBEM)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXBEML)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXREG2AR)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXREG2GB)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXM3X2PAD)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXM3X2TEX)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXM3X3PAD)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXM3X3TEX)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXM3X3SPEC)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXM3X3VSPEC)
EMIT_PASSTHROUGH_OPCODE_FUNC(EXPP)
EMIT_PASSTHROUGH_OPCODE_FUNC(LOGP)
EMIT_PASSTHROUGH_OPCODE_FUNC(CND)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXREG2RGB)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXDP3TEX)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXM3X2DEPTH)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXDP3)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXM3X3)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXDEPTH)
EMIT_PASSTHROUGH_OPCODE_FUNC(CMP)
EMIT_PASSTHROUGH_OPCODE_FUNC(BEM)
EMIT_PASSTHROUGH_OPCODE_FUNC(DP2ADD)
EMIT_PASSTHROUGH_OPCODE_FUNC(DSX)
EMIT_PASSTHROUGH_OPCODE_FUNC(DSY)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXLDD)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXLDL)
EMIT_PASSTHROUGH_OPCODE_FUNC(BREAKP)
EMIT_PASSTHROUGH_OPCODE_FUNC(BREAKC)
EMIT_PASSTHROUGH_OPCODE_FUNC(IFC)
EMIT_PASSTHROUGH_OPCODE_FUNC(SETP)
EMIT_PASSTHROUGH_OPCODE_FUNC(DEF)
EMIT_PASSTHROUGH_OPCODE_FUNC(DEFI)
EMIT_PASSTHROUGH_OPCODE_FUNC(DEFB)
EMIT_PASSTHROUGH_OPCODE_FUNC(DCL)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEXCOORD)
EMIT_PASSTHROUGH_OPCODE_FUNC(TEX)

#undef EMIT_PASSTHROUGH_OPCODE_FUNC

#endif  // SUPPORT_PROFILE_PASSTHROUGH


#if !SUPPORT_PROFILE_GLSL
#define PROFILE_EMITTER_GLSL(op)
#else
#undef AT_LEAST_ONE_PROFILE
#define AT_LEAST_ONE_PROFILE 1
#define PROFILE_EMITTER_GLSL(op) emit_GLSL_##op,

const char *get_GLSL_register_string(Context *ctx, RegisterType regtype,
                                     int regnum, char *regnum_str, int len)
{
    const char *retval = get_D3D_register_string(ctx, regtype, regnum,
                                                 regnum_str, len);
    if (retval == NULL)
    {
        fail(ctx, "Unknown D3D register type.");
        return "";
    } // if

    return retval;
} // get_GLSL_register_string

static const char *get_GLSL_varname(Context *ctx, RegisterType rt, int regnum)
{
    char regnum_str[16];
    const char *regtype_str = get_GLSL_register_string(ctx, rt, regnum,
                                              regnum_str, sizeof (regnum_str));

    char *retval = get_scratch_buffer(ctx);
    snprintf(retval, SCRATCH_BUFFER_SIZE, "%s%s", regtype_str, regnum_str);
    return retval;
} // get_GLSL_varname

static const char *get_GLSL_destarg_varname(Context *ctx, int idx)
{
    if (idx >= STATICARRAYLEN(ctx->dest_args))
    {
        fail(ctx, "Too many destination args");
        return "";
    } // if

    const DestArgInfo *arg = &ctx->dest_args[idx];
    return get_GLSL_varname(ctx, arg->regtype, arg->regnum);
} // get_GLSL_destarg_varname

static const char *get_GLSL_sourcearg_varname(Context *ctx, int idx)
{
    if (idx >= STATICARRAYLEN(ctx->source_args))
    {
        fail(ctx, "Too many source args");
        return "";
    } // if

    const SourceArgInfo *arg = &ctx->source_args[idx];
    return get_GLSL_varname(ctx, arg->regtype, arg->regnum);
} // get_GLSL_sourcearg_varname


static const char *make_GLSL_destarg_assign(Context *, const int, const char *, ...) ISPRINTF(3,4);
static const char *make_GLSL_destarg_assign(Context *ctx, const int idx,
                                            const char *fmt, ...)
{
    if (idx >= STATICARRAYLEN(ctx->dest_args))
    {
        fail(ctx, "Too many destination args");
        return "";
    } // if

    int need_parens = 0;
    const DestArgInfo *arg = &ctx->dest_args[idx];
    char *operation = get_scratch_buffer(ctx);
    va_list ap;
    va_start(ap, fmt);
    const int len = vsnprintf(operation, SCRATCH_BUFFER_SIZE, fmt, ap);
    va_end(ap);
    if (len >= SCRATCH_BUFFER_SIZE)
    {
        fail(ctx, "operation string too large");  // I'm lazy.  :P
        return "";
    } // if

    const char *result_shift_str = "";
    switch (arg->result_shift)
    {
        case 0x1: result_shift_str = " * 2"; break;
        case 0x2: result_shift_str = " * 4"; break;
        case 0x3: result_shift_str = " * 8"; break;
        case 0xD: result_shift_str = " / 8"; break;
        case 0xE: result_shift_str = " / 4"; break;
        case 0xF: result_shift_str = " / 2"; break;
    } // switch
    need_parens |= (result_shift_str[0] != '\0');

// !!! FIXME
//    const char *sat_str = (arg->result_mod & MOD_SATURATE) ? "_sat" : "";
//    const char *pp_str = (arg->result_mod & MOD_PP) ? "_pp" : "";
//    const char *cent_str = (arg->result_mod & MOD_CENTROID) ? "_centroid" : "";

// !!! FIXME: use get_GLSL_destarg_varname() here?
    char regnum_str[16];
    const char *regtype_str = get_GLSL_register_string(ctx, arg->regtype,
                                                       arg->regnum, regnum_str,
                                                       sizeof (regnum_str));
    char writemask_str[6];
    int i = 0;
    if (arg->writemask != 0xF)  // 0xF == 1111. No explicit mask.
    {
        writemask_str[i++] = '.';
        if (arg->writemask0) writemask_str[i++] = 'x';
        if (arg->writemask1) writemask_str[i++] = 'y';
        if (arg->writemask2) writemask_str[i++] = 'z';
        if (arg->writemask3) writemask_str[i++] = 'w';
    } // if
    writemask_str[i] = '\0';
    assert(i < sizeof (writemask_str));

    const char *leftparen = (need_parens) ? "(" : "";
    const char *rightparen = (need_parens) ? ")" : "";

    char *retval = get_scratch_buffer(ctx);
    snprintf(retval, SCRATCH_BUFFER_SIZE, "%s%s%s = %s%s%s%s;",
             regtype_str, regnum_str, writemask_str,
             leftparen, operation, rightparen, result_shift_str);
    // !!! FIXME: make sure the scratch buffer was large enough.
    return retval;
} // make_GLSL_destarg_assign


static char *make_GLSL_sourcearg_string(Context *ctx, const int idx)
{
    if (idx >= STATICARRAYLEN(ctx->source_args))
    {
        fail(ctx, "Too many source args");
        return "";
    } // if

// !!! FIXME: not right.
    const SourceArgInfo *arg = &ctx->source_args[idx];

    const char *premod_str = "";
    const char *postmod_str = "";
    switch ((SourceMod) arg->src_mod)
    {
        case SRCMOD_NEGATE:
            premod_str = "-";
            break;

        case SRCMOD_BIASNEGATE:
            premod_str = "-";
            // fall through.
        case SRCMOD_BIAS:
            postmod_str = "_bias";
            break;

        case SRCMOD_SIGNNEGATE:
            premod_str = "-";
            // fall through.
        case SRCMOD_SIGN:
            postmod_str = "_bx2";
            break;

        case SRCMOD_COMPLEMENT:
            premod_str = "(1.0f - (";
            postmod_str = "))";
            break;

        case SRCMOD_X2NEGATE:
            premod_str = "-(";
            postmod_str = " * 2.0f)";
            break;

        case SRCMOD_X2:
            premod_str = "(";
            postmod_str = " * 2.0f)";
            break;

        case SRCMOD_DZ:
            postmod_str = "_dz";
            break;

        case SRCMOD_DW:
            postmod_str = "_dw";
            break;

        case SRCMOD_ABSNEGATE:
            premod_str = "-abs(";
            postmod_str = ")";
            break;

        case SRCMOD_ABS:
            premod_str = "abs(";
            postmod_str = ")";
            break;

        case SRCMOD_NOT:
            premod_str = "!";
            break;

        case SRCMOD_NONE:
        case SRCMOD_TOTAL:
             break;  // stop compiler whining.
    } // switch


    char regnum_str[16];
    const char *regtype_str = get_D3D_register_string(ctx, arg->regtype,
                                                      arg->regnum, regnum_str,
                                                      sizeof (regnum_str));

    if (regtype_str == NULL)
    {
        fail(ctx, "Unknown source register type.");
        return "";
    } // if

    char swizzle_str[6];
    int i = 0;
    if (arg->swizzle != 0xE4)  // 0xE4 == 11100100 ... 3 2 1 0. No swizzle.
    {
        static const char channel[] = { 'x', 'y', 'z', 'w' };
        swizzle_str[i++] = '.';
        swizzle_str[i++] = channel[arg->swizzle_x];
        swizzle_str[i++] = channel[arg->swizzle_y];
        swizzle_str[i++] = channel[arg->swizzle_z];
        swizzle_str[i++] = channel[arg->swizzle_w];

        // .xyzz is the same as .xyz, .z is the same as .zzzz, etc.
        while (swizzle_str[i-1] == swizzle_str[i-2])
            i--;
    } // if
    swizzle_str[i] = '\0';
    assert(i < sizeof (swizzle_str));

    char *retval = get_scratch_buffer(ctx);
    snprintf(retval, SCRATCH_BUFFER_SIZE, "%s%s%s%s%s",
             premod_str, regtype_str, regnum_str, postmod_str, swizzle_str);
    // !!! FIXME: make sure the scratch buffer was large enough.
    return retval;
} // make_GLSL_sourcearg_string


// special cases for comparison opcodes...

static const char *get_GLSL_comparison_string_scalar(Context *ctx)
{
    static const char *comps[] = { "", ">", "==", ">=", "<", "!=", "<=" };
    if (ctx->instruction_controls >= STATICARRAYLEN(comps))
    {
        fail(ctx, "unknown comparison control");
        return "";
    } // if

    return comps[ctx->instruction_controls];
} // get_GLSL_comparison_string_scalar

static const char *get_GLSL_comparison_string_vector(Context *ctx)
{
    static const char *comps[] = {
        "", "greaterThan", "equal", "greaterThanEqual", "lessThan",
        "notEqual", "lessThanEqual"
    };

    if (ctx->instruction_controls >= STATICARRAYLEN(comps))
    {
        fail(ctx, "unknown comparison control");
        return "";
    } // if

    return comps[ctx->instruction_controls];
} // get_GLSL_comparison_string_vector


static void emit_GLSL_start(Context *ctx)
{
    switch (ctx->shader_type)
    {
        case MOJOSHADER_TYPE_PIXEL:
        case MOJOSHADER_TYPE_VERTEX:
            break;  // supported.

        default:
            failf(ctx, "Shader type %u unsupported in this profile.",
                  (uint) ctx->shader_type);
            return;
    } // switch

    ctx->output = &ctx->mainline;
    output_line(ctx, "void main()");
    output_line(ctx, "{");
    ctx->indent++;
} // emit_GLSL_start

static void emit_GLSL_RET(Context *ctx);
static void emit_GLSL_end(Context *ctx)
{
    // force a RET opcode if we're at the end of the stream without one.
    if (ctx->previous_opcode != OPCODE_RET)
        emit_GLSL_RET(ctx);
} // emit_GLSL_end

static void emit_GLSL_finalize(Context *ctx)
{
    // throw some blank lines around to make source more readable.
    push_output(ctx, &ctx->globals);
    output_blank_line(ctx);
    pop_output(ctx);
} // emit_GLSL_finalize

static void emit_GLSL_global(Context *ctx, RegisterType regtype, int regnum)
{
    push_output(ctx, &ctx->globals);
    switch (regtype)
    {
        case REG_TYPE_ADDRESS:
            output_line(ctx, "ivec4 a%d;", regnum);
            break;
        case REG_TYPE_PREDICATE:
            output_line(ctx, "bvec4 p%d;", regnum);
            break;
        case REG_TYPE_TEMP:
            output_line(ctx, "vec4 r%d;", regnum);
            break;
        case REG_TYPE_LOOP:
            //output_line(ctx, "int aL;");
            break; // no-op. We declare these in for loops at the moment.
        case REG_TYPE_LABEL:
            break; // no-op. If we see it here, it means we optimized it out.
        default:
            fail(ctx, "BUG: we used a register we don't know how to define.");
            break;
    } // switch
    pop_output(ctx);
} // emit_GLSL_global

static void emit_GLSL_uniform(Context *ctx, RegisterType regtype, int regnum)
{
    push_output(ctx, &ctx->globals);
    if (regtype == REG_TYPE_CONST)
        output_line(ctx, "uniform vec4 c%d;", regnum);
    else if (regtype == REG_TYPE_CONST2)
        output_line(ctx, "uniform vec4 c%d;", regnum + 2048);
    else if (regtype == REG_TYPE_CONST3)
        output_line(ctx, "uniform vec4 c%d;", regnum + 4096);
    else if (regtype == REG_TYPE_CONST4)
        output_line(ctx, "uniform vec4 c%d;", regnum + 6144);
    else if (regtype == REG_TYPE_CONSTINT)
        output_line(ctx, "uniform ivec4 i%d;", regnum);
    else if (regtype == REG_TYPE_CONSTBOOL)
        output_line(ctx, "uniform bvec4 i%d;", regnum);
    else
        fail(ctx, "BUG: we used a uniform we don't know how to define.");
    pop_output(ctx);
} // emit_GLSL_uniform

static void emit_GLSL_attribute(Context *ctx, RegisterType regtype, int regnum,
                                MOJOSHADER_usage usage, int index, int wmask)
{
    // !!! FIXME: this function doesn't deal with write masks at all yet!
    const char *varname = get_GLSL_varname(ctx, regtype, regnum);
    const char *usage_str = NULL;
    char index_str[16] = { '\0' };

    if (index != 0)  // !!! FIXME: a lot of these MUST be zero.
        snprintf(index_str, sizeof (index_str), "%u", (uint) index);

    if (ctx->shader_type == MOJOSHADER_TYPE_VERTEX)
    {
        // pre-vs3 output registers.
        // these don't ever happen in DCL opcodes, I think. Map to vs_3_*
        //  output registers.
        if (!shader_version_atleast(ctx, 3, 0))
        {
            if (regtype == REG_TYPE_RASTOUT)
            {
                regtype = REG_TYPE_OUTPUT;
                switch ((const RastOutType) regnum)
                {
                    case RASTOUT_TYPE_POSITION:
                        usage = MOJOSHADER_USAGE_POSITION;
                        break;
                    case RASTOUT_TYPE_FOG:
                        usage = MOJOSHADER_USAGE_FOG;
                        break;
                    case RASTOUT_TYPE_POINT_SIZE:
                        usage = MOJOSHADER_USAGE_POINTSIZE;
                        break;
                } // switch
            } // if

            else if (regtype == REG_TYPE_ATTROUT)
            {
                regtype = REG_TYPE_OUTPUT;
                usage = MOJOSHADER_USAGE_COLOR;
            } // else if

            else if (regtype == REG_TYPE_TEXCRDOUT)
            {
                regtype = REG_TYPE_OUTPUT;
                usage = MOJOSHADER_USAGE_TEXCOORD;
            } // else if
        } // if

        // to avoid limitations of various GL entry points for input
        // attributes (glSecondaryColorPointer() can only take 3 component
        // items, glVertexPointer() can't do GL_UNSIGNED_BYTE, many other
        // issues), we set up all inputs as generic vertex attributes, so we
        // can pass data in just about any form, and ignore the built-in GLSL
        // attributes like gl_SecondaryColor. Output needs to use the the
        // built-ins, though, but we don't have to worry about the GL entry
        // point limitations there.

        if (regtype == REG_TYPE_INPUT)
        {
            // The GL will bind to attr_position_0 or whatever, but we'll
            //  refer to it in the shader by the original D3D register name.
            push_output(ctx, &ctx->globals);
            output_line(ctx, "#define %s attr%s_%d", varname,
                        usagestrs[(int) usage], index);
            output_line(ctx, "attribute vec4 %s;", varname);
            pop_output(ctx);
        } // if

        else if (regtype == REG_TYPE_OUTPUT)
        {
            const uint32 index = ctx->dwords[1];
            const char *arrayleft = "";
            const char *arrayright = "";

            switch (usage)
            {
                case MOJOSHADER_USAGE_POSITION:
                    usage_str = "gl_Position";
                    break;
                case MOJOSHADER_USAGE_POINTSIZE:
                    usage_str = "gl_PointSize";
                    break;
                case MOJOSHADER_USAGE_COLOR:
                    index_str[0] = '\0';  // no explicit number.
                    if (index == 0)
                        usage_str = "gl_FrontColor";
                    else if (index == 1)
                        usage_str = "gl_FrontSecondaryColor";
                    break;
                case MOJOSHADER_USAGE_FOG:
                    usage_str = "gl_FogFragCoord";
                    break;
                case MOJOSHADER_USAGE_TEXCOORD:
                    snprintf(index_str, sizeof (index_str), "%u", (uint) index);
                    usage_str = "gl_TexCoord";
                    arrayleft = "[";
                    arrayright = "]";
                    break;
                default:
                    // !!! FIXME: we need to deal with some more built-in varyings here.
                    break;
            } // switch

            // !!! FIXME: the #define is a little hacky, but it means we don't
            // !!! FIXME:  have to track these separately if this works.
            push_output(ctx, &ctx->globals);
            // no mapping to built-in var? Just make it a regular global, pray.
            if (usage_str == NULL)
                output_line(ctx, "vec %s;", varname);
            else
            {
                output_line(ctx, "#define %s %s%s%s%s", varname, usage_str,
                            arrayleft, index_str, arrayright);
            } // else
            pop_output(ctx);
        } // else if

        else
        {
            fail(ctx, "unknown attribute register");
        } // else
    } // if

#if 0 // !!! FIXME: write me.
    else if (ctx->shader_type == MOJOSHADER_TYPE_PIXEL)
    {
        if (regtype == REG_TYPE_SAMPLER)
        {
            switch ((const TextureType) usage)
            {
                case TEXTURE_TYPE_2D: usage_str = "_2d"; break;
                case TEXTURE_TYPE_CUBE: usage_str = "_cube"; break;
                case TEXTURE_TYPE_VOLUME: usage_str = "_volume"; break;
                default: fail(ctx, "unknown sampler texture type"); return;
            } // switch
        } // if

        else if (regtype == REG_TYPE_COLOROUT)
        {
            retval = "oC";
        } // else if

        else if (regtype == REG_TYPE_DEPTHOUT)
        {
            retval = "oDepth";
        } // else if
    } // else if
#endif

    else
    {
        fail(ctx, "Unknown shader type");  // state machine should catch this.
    } // else
} // emit_GLSL_attribute

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
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "%s", src0);
    output_line(ctx, "%s", code);
} // emit_GLSL_MOV

static void emit_GLSL_ADD(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "%s + %s", src0, src1);
    output_line(ctx, "%s", code);
} // emit_GLSL_ADD

static void emit_GLSL_SUB(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "%s - %s", src0, src1);
    output_line(ctx, "%s", code);
} // emit_GLSL_SUB

static void emit_GLSL_MAD(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    const char *src2 = make_GLSL_sourcearg_string(ctx, 2);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "(%s * %s) + %s", src0, src1, src2);
    output_line(ctx, "%s", code);
} // emit_GLSL_MAD

static void emit_GLSL_MUL(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "%s * %s", src0, src1);
    output_line(ctx, "%s", code);
} // emit_GLSL_MUL

static void emit_GLSL_RCP(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "1.0f / %s", src0);
    output_line(ctx, "%s", code);
} // emit_GLSL_RCP

static void emit_GLSL_RSQ(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "inversesqrt(%s)", src0);
    output_line(ctx, "%s", code);
} // emit_GLSL_RSQ

static void emit_GLSL_DP3(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "dot(vec3(%s), vec3(%s))", src0, src1);
    output_line(ctx, "%s", code);
} // emit_GLSL_DP3

static void emit_GLSL_DP4(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "dot(vec4(%s), vec4(%s))", src0, src1);
    output_line(ctx, "%s", code);
} // emit_GLSL_DP4

static void emit_GLSL_MIN(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "min(%s, %s)", src0, src1);
    output_line(ctx, "%s", code);
} // emit_GLSL_MIN

static void emit_GLSL_MAX(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "max(%s, %s)", src0, src1);
    output_line(ctx, "%s", code);
} // emit_GLSL_MAX

static void emit_GLSL_SLT(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    // !!! FIXME: need to cast from bvec to vec...
    const char *code = make_GLSL_destarg_assign(ctx, 0, "lessThan(%s, %s)", src0, src1);
    output_line(ctx, "%s", code);
} // emit_GLSL_SLT

static void emit_GLSL_SGE(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    // !!! FIXME: need to cast from bvec to vec...
    const char *code = make_GLSL_destarg_assign(ctx, 0, "greaterThanEqual(%s, %s)", src0, src1);
    output_line(ctx, "%s", code);
} // emit_GLSL_SGE

static void emit_GLSL_EXP(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "exp2(%s)", src0);
    output_line(ctx, "%s", code);
} // emit_GLSL_EXP

static void emit_GLSL_LOG(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "log2(%s)", src0);
    output_line(ctx, "%s", code);
} // emit_GLSL_LOG

static void emit_GLSL_LIT_helper(Context *ctx)
{
    const char *maxp = "127.9961f"; // value from the dx9 reference.

    if (ctx->flags & CTX_FLAGS_GLSL_LIT_OPCODE)
        return;

    ctx->flags |= CTX_FLAGS_GLSL_LIT_OPCODE;

    push_output(ctx, &ctx->helpers);
    output_line(ctx, "const vec4 LIT(const vec4 src)");
    output_line(ctx, "{"); ctx->indent++;
    output_line(ctx,   "const float power = clamp(src.w, -%s, %s);",maxp,maxp);
    output_line(ctx,   "vec4 retval(1.0f, 0.0f, 0.0f, 1.0f)");
    output_line(ctx,   "if (src.x > 0.0f) {"); ctx->indent++;
    output_line(ctx,     "retval.y = src.x;");
    output_line(ctx,     "if (src.y > 0.0f) {"); ctx->indent++;
    output_line(ctx,       "retval.z = pow(src.y, power);"); ctx->indent--;
    output_line(ctx,     "}"); ctx->indent--;
    output_line(ctx,   "}");
    output_line(ctx,   "return retval;"); ctx->indent--;
    output_line(ctx, "}");
    output_blank_line(ctx);
    pop_output(ctx);
} // emit_GLSL_LIT_helper

static void emit_GLSL_LIT(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "LIT(%s)", src0);
    output_line(ctx, "%s", code);
    emit_GLSL_LIT_helper(ctx);
} // emit_GLSL_LIT

static void emit_GLSL_DST_helper(Context *ctx)
{
    if (ctx->flags & CTX_FLAGS_GLSL_DST_OPCODE)
        return;

    ctx->flags |= CTX_FLAGS_GLSL_DST_OPCODE;

    push_output(ctx, &ctx->helpers);
    output_line(ctx, "const vec4 DST(const vec4 src0, const vec4 src1)");
    output_line(ctx, "{"); ctx->indent++;
    output_line(ctx,   "return vec4(1.0f, src0.y * src1.y, src0.z, src1.w);"); ctx->indent--;
    output_line(ctx, "}");
    output_blank_line(ctx);
    pop_output(ctx);
} // emit_GLSL_DST_helper

static void emit_GLSL_DST(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "DST(%s, %s)", src0, src1);
    output_line(ctx, "%s", code);
    emit_GLSL_DST_helper(ctx);
} // emit_GLSL_DST

static void emit_GLSL_LRP_helper(Context *ctx)
{
    if (ctx->flags & CTX_FLAGS_GLSL_LRP_OPCODE)
        return;

    ctx->flags |= CTX_FLAGS_GLSL_LRP_OPCODE;

    push_output(ctx, &ctx->helpers);
    output_line(ctx, "const vec4 LRP(const vec4 src0, const vec4 src1, const vec4 src2)");
    output_line(ctx, "{"); ctx->indent++;
    output_line(ctx,   "return vec4("); ctx->indent++;
    output_line(ctx,     "src0.x * (src1.x - src2.x) + src2.x,");
    output_line(ctx,     "src0.y * (src1.y - src2.y) + src2.y,");
    output_line(ctx,     "src0.z * (src1.z - src2.z) + src2.z,");
    output_line(ctx,     "src0.w * (src1.w - src2.w) + src2.w"); ctx->indent--;
    output_line(ctx,   ");"); ctx->indent--;
    output_line(ctx, "}");
    output_blank_line(ctx);
    pop_output(ctx);
} // emit_GLSL_LRP_helper

static void emit_GLSL_LRP(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    const char *src2 = make_GLSL_sourcearg_string(ctx, 2);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "LRP(%s, %s, %s)", src0, src1, src2);
    output_line(ctx, "%s", code);
    emit_GLSL_LRP_helper(ctx);
} // emit_GLSL_LRP

static void emit_GLSL_FRC(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "fract(%s)", src0);
    output_line(ctx, "%s", code);
} // emit_GLSL_FRC

static void emit_GLSL_M4X4(Context *ctx)
{
    // !!! FIXME: d3d is row-major, glsl is column-major, I think.
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *row0 = make_GLSL_sourcearg_string(ctx, 1);
    const char *row1 = make_GLSL_sourcearg_string(ctx, 2);
    const char *row2 = make_GLSL_sourcearg_string(ctx, 3);
    const char *row3 = make_GLSL_sourcearg_string(ctx, 4);
    const char *code = make_GLSL_destarg_assign(ctx, 0,
                    "vec4(dot(%s, %s), dot(%s, %s), dot(%s, %s), dot(%s, %s))",
                    src0, row0, src0, row1, src0, row2, src0, row3);
    output_line(ctx, "%s", code);
} // emit_GLSL_M4X4

static void emit_GLSL_M4X3(Context *ctx)
{
    // !!! FIXME: d3d is row-major, glsl is column-major, I think.
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *row0 = make_GLSL_sourcearg_string(ctx, 1);
    const char *row1 = make_GLSL_sourcearg_string(ctx, 2);
    const char *row2 = make_GLSL_sourcearg_string(ctx, 3);
    const char *code = make_GLSL_destarg_assign(ctx, 0,
                                "vec3(dot(%s, %s), dot(%s, %s), dot(%s, %s))",
                                src0, row0, src0, row1, src0, row2);
    output_line(ctx, "%s", code);
} // emit_GLSL_M4X3

static void emit_GLSL_M3X4(Context *ctx)
{
    // !!! FIXME: d3d is row-major, glsl is column-major, I think.
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *row0 = make_GLSL_sourcearg_string(ctx, 1);
    const char *row1 = make_GLSL_sourcearg_string(ctx, 2);
    const char *row2 = make_GLSL_sourcearg_string(ctx, 3);
    const char *row3 = make_GLSL_sourcearg_string(ctx, 4);

    const char *code = make_GLSL_destarg_assign(ctx, 0,
                                "vec4(dot(vec3(%s), vec3(%s)), "
                                     "dot(vec3(%s), vec3(%s)), "
                                     "dot(vec3(%s), vec3(%s)), "
                                     "dot(vec3(%s), vec3(%s)))",
                                src0, row0, src0, row1,
                                src0, row2, src0, row3);
    output_line(ctx, "%s", code);
} // emit_GLSL_M3X4

static void emit_GLSL_M3X3(Context *ctx)
{
    // !!! FIXME: d3d is row-major, glsl is column-major, I think.
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *row0 = make_GLSL_sourcearg_string(ctx, 1);
    const char *row1 = make_GLSL_sourcearg_string(ctx, 2);
    const char *row2 = make_GLSL_sourcearg_string(ctx, 3);
    const char *code = make_GLSL_destarg_assign(ctx, 0,
                                "vec3(dot(vec3(%s), vec3(%s)), "
                                     "dot(vec3(%s), vec3(%s)), "
                                     "dot(vec3(%s), vec3(%s)))",
                                src0, row0, src0, row1, src0, row2);
    output_line(ctx, "%s", code);
} // emit_GLSL_M3X3

static void emit_GLSL_M3X2(Context *ctx)
{
    // !!! FIXME: d3d is row-major, glsl is column-major, I think.
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *row0 = make_GLSL_sourcearg_string(ctx, 1);
    const char *row1 = make_GLSL_sourcearg_string(ctx, 2);

    const char *code = make_GLSL_destarg_assign(ctx, 0,
                                "vec3(dot(vec3(%s), vec3(%s)), "
                                     "dot(vec3(%s), vec3(%s)))",
                                src0, row0, src0, row1);
    output_line(ctx, "%s", code);
} // emit_GLSL_M3X2

static void emit_GLSL_CALL(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    if (ctx->loops > 0)
        output_line(ctx, "%s(aL);", src0);
    else
        output_line(ctx, "%s();", src0);
} // emit_GLSL_CALL

static void emit_GLSL_CALLNZ(Context *ctx)
{
    // !!! FIXME: if src1 is a constbool that's true, we can remove the
    // !!! FIXME:  if. If it's false, we can make this a no-op.
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);

    if (ctx->loops > 0)
        output_line(ctx, "if (%s) { %s(aL); }", src1, src0);
    else
        output_line(ctx, "if (%s) { %s(); }", src1, src0);
} // emit_GLSL_CALLNZ

static void emit_GLSL_LOOP(Context *ctx)
{
    const char *varname = get_GLSL_sourcearg_varname(ctx, 1);
    assert(ctx->source_args[0].regnum == 0);  // in case they add aL1 someday.
    output_line(ctx, "for (int aL = %s.y, const int aLend = %s.x+%s.y; aL < aLend; aL += %s.z) {",
                varname, varname, varname, varname);
    ctx->indent++;
} // emit_GLSL_LOOP

static void emit_GLSL_RET(Context *ctx)
{
    // thankfully, the MSDN specs say a RET _has_ to end a function...no
    //  early returns. So if you hit one, you know you can safely close
    //  a high-level function.
    ctx->indent--;
    output_line(ctx, "}");
    output_blank_line(ctx);
    ctx->output = &ctx->subroutines;
} // emit_GLSL_RET

static void emit_GLSL_ENDLOOP(Context *ctx)
{
    ctx->indent--;
    output_line(ctx, "}");
} // emit_GLSL_ENDLOOP

static void emit_GLSL_LABEL(Context *ctx)
{
    const char *labelstr = make_GLSL_sourcearg_string(ctx, 0);
    const int label = ctx->source_args[0].regnum;
    RegisterList *reg = reglist_find(&ctx->used_registers, REG_TYPE_LABEL, label);
    assert(ctx->output == &ctx->subroutines);  // not mainline, etc.
    assert(ctx->indent == 0);  // we shouldn't be in the middle of a function.

    // MSDN specs say CALL* has to come before the LABEL, so we know if we
    //  can ditch the entire function here as unused.
    if (reg == NULL)
        ctx->output = &ctx->ignore;  // Func not used. Parse, but don't output.

    // !!! FIXME: it would be nice if we could determine if a function is
    // !!! FIXME:  only called once and, if so, forcibly inline it.

    const char *uses_loopreg = ((reg) && (reg->usage == 1)) ? "int aL" : "";
    output_line(ctx, "void %s(%s)", labelstr, uses_loopreg);
    output_line(ctx, "{");
    ctx->indent++;
} // emit_GLSL_LABEL

static void emit_GLSL_DCL(Context *ctx)
{
    // no-op. We do our work at the end in emit_attribute() implementation.
} // emit_GLSL_DCL

static void emit_GLSL_POW(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "pow(abs(%s), %s))", src0, src1);
    output_line(ctx, "%s", code);
} // emit_GLSL_POW

static void emit_GLSL_CRS(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "cross(vec3(%s), vec3(%s))", src0, src1);
    output_line(ctx, "%s", code);
} // emit_GLSL_CRS

static void emit_GLSL_SGN(Context *ctx)
{
    // (we don't need the temporary registers specified for the D3D opcode.)
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "sign(%s)", src0);
    output_line(ctx, "%s", code);
} // emit_GLSL_SGN

static void emit_GLSL_ABS(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "abs(%s)", src0);
    output_line(ctx, "%s", code);
} // emit_GLSL_ABS

static void emit_GLSL_NRM(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "normalize(%s)", src0);
    output_line(ctx, "%s", code);
} // emit_GLSL_NRM

static void emit_GLSL_SINCOS(Context *ctx)
{
    // we don't care about the temp registers that <= sm2 demands; ignore them.
    //  sm2 also talks about what components are left untouched vs. undefined,
    //  but we just leave those all untouched with GLSL write masks (which
    //  would fulfill the "undefined" requirement, too).
    const int mask = ctx->dest_args[0].writemask;
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *code = NULL;

    if (mask == 0x1)  // .x
        code = make_GLSL_destarg_assign(ctx, 0, "cos(%s)", src0);
    else if (mask == 0x2)  // .y
        code = make_GLSL_destarg_assign(ctx, 0, "sin(%s)", src0);
    else if (mask == 0x3)  // .xy
        code = make_GLSL_destarg_assign(ctx, 0, "vec2(cos(%s), sin(%s))", src0, src0);
    output_line(ctx, "%s", code);
} // emit_GLSL_SINCOS

static void emit_GLSL_REP(Context *ctx)
{
    // !!! FIXME:
    // msdn docs say legal loop values are 0 to 255. We can check DEFI values
    //  at parse time, but if they are pulling a value from a uniform, do
    //  we clamp here?
    const char *varname = get_GLSL_sourcearg_varname(ctx, 0);
    const uint rep = (uint) ctx->reps;
    output_line(ctx, "for (int rep%u = 0; rep%u < %s.x; rep%u++) {",
                rep, rep, varname, rep);
    ctx->indent++;
} // emit_GLSL_REP

static void emit_GLSL_ENDREP(Context *ctx)
{
    ctx->indent--;
    output_line(ctx, "}");
} // emit_GLSL_ENDREP

static void emit_GLSL_IF(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    output_line(ctx, "if (%s) {", src0);
    ctx->indent++;
} // emit_GLSL_IF

static void emit_GLSL_IFC(Context *ctx)
{
    const char *comp = get_GLSL_comparison_string_scalar(ctx);
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    output_line(ctx, "if (%s %s %s) {", src0, comp, src1);
    ctx->indent++;
} // emit_GLSL_IFC

static void emit_GLSL_ELSE(Context *ctx)
{
    ctx->indent--;
    output_line(ctx, "} else {");
    ctx->indent++;
} // emit_GLSL_ELSE

static void emit_GLSL_ENDIF(Context *ctx)
{
    ctx->indent--;
    output_line(ctx, "}");
} // emit_GLSL_ENDIF

static void emit_GLSL_BREAK(Context *ctx)
{
    output_line(ctx, "break;");
} // emit_GLSL_BREAK

static void emit_GLSL_BREAKC(Context *ctx)
{
    const char *comp = get_GLSL_comparison_string_scalar(ctx);
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    output_line(ctx, "if (%s %s %s) { break; }", src0, comp, src1);
} // emit_GLSL_BREAKC

static void emit_GLSL_MOVA(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "ivec4(floor(abs(%s) + vec4(0.5f)) * sign(%s))", src0, src0);
    output_line(ctx, "%s", code);
} // emit_GLSL_MOVA

static void emit_GLSL_DEFB(Context *ctx)
{
    const char *varname = get_GLSL_destarg_varname(ctx, 0);
    push_output(ctx, &ctx->globals);
    output_line(ctx, "const bool %s = %s;",
                varname, ctx->dwords[0] ? "true" : "false");
    pop_output(ctx);
} // emit_GLSL_DEFB

static void emit_GLSL_DEFI(Context *ctx)
{
    const char *varname = get_GLSL_destarg_varname(ctx, 0);
    const int32 *x = (const int32 *) ctx->dwords;
    push_output(ctx, &ctx->globals);
    output_line(ctx, "const ivec4 %s(%d, %d, %d, %d);",
                varname, (int) x[0], (int) x[1], (int) x[2], (int) x[3]);
    pop_output(ctx);
} // emit_GLSL_DEFI

static void emit_GLSL_TEXCOORD(Context *ctx)
{
    // this opcode looks and acts differently depending on the shader model.
    //if (shader_version_atleast(ctx, 1, 4))
    //    emit_D3D_opcode_ds(ctx, "texcrd");
    //else
    //    emit_D3D_opcode_d(ctx, "texcoord");
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXCOORD

static void emit_GLSL_TEXKILL(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
// !!! FIXME
//    const char *dst0 = make_GLSL_destarg_string(ctx, 0);
//    output_line(ctx, "if (any(lessThan(vec3(%s), vec3(0.0)))) discard;", dst0);
} // emit_GLSL_TEXKILL

static void emit_GLSL_TEX(Context *ctx)
{
    // this opcode looks and acts differently depending on the shader model.
    //if (shader_version_atleast(ctx, 1, 4))
    //    emit_D3D_opcode_ds(ctx, "tex");
    //else
    //    emit_D3D_opcode_d(ctx, "texld");
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
    // !!! FIXME: msdn's asm docs don't list this opcode, I'll have to check the driver documentation.
    emit_GLSL_EXPP(ctx);  // I guess this is just partial precision EXP?
} // emit_GLSL_EXPP

static void emit_GLSL_LOGP(Context *ctx)
{
    // LOGP is just low-precision LOG, but we'll take the higher precision.
    emit_GLSL_LOG(ctx);
} // emit_GLSL_LOGP

// common code between CMP and CND.
static void emit_GLSL_comparison_operations(Context *ctx, const char *cmp)
{
    const DestArgInfo *dst = &ctx->dest_args[0];
    const char *src0 = get_GLSL_sourcearg_varname(ctx, 0);
    const char *src1 = get_GLSL_sourcearg_varname(ctx, 0);
    const char *src2 = get_GLSL_sourcearg_varname(ctx, 0);

    // !!! FIXME: for replicate swizzles, don't redo compares...

    if (dst->writemask0)
    {
        fail(ctx, "!!! FIXME: need to figure out source swizzle here");
        const char *code = make_GLSL_destarg_assign(ctx, 0, "((%s.x %s) ? %s.x : %s.x)", src0, cmp, src1, src2);
        output_line(ctx, "%s", code);
    } // if
    if (dst->writemask1)
    {
        fail(ctx, "!!! FIXME: need to figure out source swizzle here");
        const char *code = make_GLSL_destarg_assign(ctx, 0, "((%s.y %s) ? %s.y : %s.y)", src0, cmp, src1, src2);
        output_line(ctx, "%s", code);
    } // if
    if (dst->writemask2)
    {
        fail(ctx, "!!! FIXME: need to figure out source swizzle here");
        const char *code = make_GLSL_destarg_assign(ctx, 0, "((%s.z %s) ? %s.z : %s.z)", src0, cmp, src1, src2);
        output_line(ctx, "%s", code);
    } // if
    if (dst->writemask3)
    {
        fail(ctx, "!!! FIXME: need to figure out source swizzle here");
        const char *code = make_GLSL_destarg_assign(ctx, 0, "((%s.w %s) ? %s.w : %s.w)", src0, cmp, src1, src2);
        output_line(ctx, "%s", code);
    } // if
} // emit_GLSL_comparison_operations

static void emit_GLSL_CND(Context *ctx)
{
    emit_GLSL_comparison_operations(ctx, "> 0.5f");
} // emit_GLSL_CND

static void emit_GLSL_DEF(Context *ctx)
{
    const char *varname = get_GLSL_destarg_varname(ctx, 0);
    const float *val = (const float *) ctx->dwords; // !!! FIXME: could be int?
    char val0[32];
    char val1[32];
    char val2[32];
    char val3[32];
    floatstr(ctx, val0, sizeof (val0), val[0], 1);
    floatstr(ctx, val1, sizeof (val1), val[1], 1);
    floatstr(ctx, val2, sizeof (val2), val[2], 1);
    floatstr(ctx, val3, sizeof (val3), val[3], 1);

    push_output(ctx, &ctx->globals);
    output_line(ctx, "const vec4 %s(%sf, %sf, %sf, %sf);",
                varname, val0, val1, val2, val3);
    pop_output(ctx);
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
    emit_GLSL_comparison_operations(ctx, ">= 0.0f");
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
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "dFdx(%s)", src0);
    output_line(ctx, "%s", code);
} // emit_GLSL_DSX

static void emit_GLSL_DSY(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "dFdy(%s)", src0);
    output_line(ctx, "%s", code);
} // emit_GLSL_DSY

static void emit_GLSL_TEXLDD(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXLDD

static void emit_GLSL_SETP(Context *ctx)
{
    const char *comp = get_GLSL_comparison_string_vector(ctx);
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    const char *src1 = make_GLSL_sourcearg_string(ctx, 1);
    const char *code = make_GLSL_destarg_assign(ctx, 0, "%s(%s, %s)", comp, src0, src1);
    output_line(ctx, "%s", code);
} // emit_GLSL_SETP

static void emit_GLSL_TEXLDL(Context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXLDL

static void emit_GLSL_BREAKP(Context *ctx)
{
    const char *src0 = make_GLSL_sourcearg_string(ctx, 0);
    output_line(ctx, "if (%s) { break; }", src0);
} // emit_GLSL_BREAKP

static void emit_GLSL_RESERVED(Context *ctx)
{
    // do nothing; fails in the state machine.
} // emit_GLSL_RESERVED

#endif  // SUPPORT_PROFILE_GLSL



#if !AT_LEAST_ONE_PROFILE
#error No profiles are supported. Fix your build.
#endif

#define DEFINE_PROFILE(prof) { \
    MOJOSHADER_PROFILE_##prof, \
    emit_##prof##_start, \
    emit_##prof##_end, \
    emit_##prof##_comment, \
    emit_##prof##_global, \
    emit_##prof##_uniform, \
    emit_##prof##_attribute, \
    emit_##prof##_finalize, \
},

static const Profile profiles[] =
{
#if SUPPORT_PROFILE_D3D
    DEFINE_PROFILE(D3D)
#endif
#if SUPPORT_PROFILE_PASSTHROUGH
    DEFINE_PROFILE(PASSTHROUGH)
#endif
#if SUPPORT_PROFILE_GLSL
    DEFINE_PROFILE(GLSL)
#endif
};

#undef DEFINE_PROFILE

// The PROFILE_EMITTER_* items MUST be in the same order as profiles[]!
#define PROFILE_EMITTERS(op) { \
     PROFILE_EMITTER_D3D(op) \
     PROFILE_EMITTER_PASSTHROUGH(op) \
     PROFILE_EMITTER_GLSL(op) \
}

static int parse_destination_token(Context *ctx, DestArgInfo *info)
{
    // !!! FIXME: recheck against the spec for ranges (like RASTOUT values, etc).

    if (isfail(ctx))
        return FAIL;  // already failed elsewhere.

    if (ctx->tokencount == 0)
        return fail(ctx, "Out of tokens in destination parameter");

    const uint32 token = SWAP32(*(ctx->tokens));
    const int reserved1 = (int) ((token >> 14) & 0x3); // bits 14 through 15
    const int reserved2 = (int) ((token >> 31) & 0x1); // bit 31

    info->token = ctx->tokens;
    info->regnum = (int) (token & 0x7ff);  // bits 0 through 10
    info->relative = (int) ((token >> 13) & 0x1); // bit 13
    info->writemask = (int) ((token >> 16) & 0xF); // bits 16 through 19
    info->writemask0 = (int) ((token >> 16) & 0x1); // bit 16
    info->writemask1 = (int) ((token >> 17) & 0x1); // bit 17
    info->writemask2 = (int) ((token >> 18) & 0x1); // bit 18
    info->writemask3 = (int) ((token >> 19) & 0x1); // bit 19
    info->result_mod = (int) ((token >> 20) & 0xF); // bits 20 through 23
    info->result_shift = (int) ((token >> 24) & 0xF); // bits 24 through 27      abc
    info->regtype = (RegisterType) (((token >> 28) & 0x7) | ((token >> 8) & 0x18));  // bits 28-30, 11-12

    ctx->tokens++;  // swallow token for now, for multiple calls in a row.
    ctx->tokencount--;  // swallow token for now, for multiple calls in a row.

    if (reserved1 != 0x0)
        return fail(ctx, "Reserved bit #1 in destination token must be zero");

    if (reserved2 != 0x1)
        return fail(ctx, "Reserved bit #2 in destination token must be one");

    if (info->relative)
    {
        if (ctx->shader_type != MOJOSHADER_TYPE_VERTEX)
            return fail(ctx, "Relative addressing in non-vertex shader");
        else if (ctx->major_ver < 3)
            return fail(ctx, "Relative addressing in vertex shader version < 3.0");
        return fail(ctx, "Relative addressing is unsupported");  // !!! FIXME
    } // if

    const int s = info->result_shift;
    if (s != 0)
    {
        if (ctx->shader_type != MOJOSHADER_TYPE_PIXEL)
            return fail(ctx, "Result shift scale in non-pixel shader");
        else if (ctx->major_ver >= 2)
            return fail(ctx, "Result shift scale in pixel shader version >= 2.0");
        else if ( ! (((s >= 1) && (s <= 3)) || ((s >= 0xD) && (s <= 0xF))) )
            return fail(ctx, "Result shift scale isn't 1 to 3, or 13 to 15.");
    } // if

    if (info->result_mod & MOD_SATURATE)  // Saturate (vertex shaders only)
    {
        if (ctx->shader_type != MOJOSHADER_TYPE_VERTEX)
            return fail(ctx, "Saturate result mod in non-vertex shader");
    } // if

    if (info->result_mod & MOD_PP)  // Partial precision (pixel shaders only)
    {
        if (ctx->shader_type != MOJOSHADER_TYPE_PIXEL)
            return fail(ctx, "Partial precision result mod in non-pixel shader");
    } // if

    if (info->result_mod & MOD_CENTROID)  // Centroid (pixel shaders only)
    {
        if (ctx->shader_type != MOJOSHADER_TYPE_PIXEL)
            return fail(ctx, "Centroid result mod in non-pixel shader");
    } // if

    if ((info->regtype < 0) || (info->regtype > REG_TYPE_MAX))
        return fail(ctx, "Register type is out of range");

    set_used_register(ctx, info->regtype, info->regnum);
    return 1;
} // parse_destination_token


static int parse_source_token(Context *ctx, SourceArgInfo *info)
{
    if (isfail(ctx))
        return FAIL;  // already failed elsewhere.

    if (ctx->tokencount == 0)
        return fail(ctx, "Out of tokens in source parameter");

    const uint32 token = SWAP32(*(ctx->tokens));
    const int reserved1 = (int) ((token >> 14) & 0x3); // bits 14 through 15
    const int reserved2 = (int) ((token >> 31) & 0x1); // bit 31

    info->token = ctx->tokens;
    info->regnum = (int) (token & 0x7ff);  // bits 0 through 10
    info->relative = (int) ((token >> 13) & 0x1); // bit 13
    info->swizzle = (int) ((token >> 16) & 0xFF); // bits 16 through 23
    info->swizzle_x = (int) ((token >> 16) & 0x3); // bits 16 through 17
    info->swizzle_y = (int) ((token >> 18) & 0x3); // bits 18 through 19
    info->swizzle_z = (int) ((token >> 20) & 0x3); // bits 20 through 21
    info->swizzle_w = (int) ((token >> 22) & 0x3); // bits 22 through 23
    info->src_mod = (int) ((token >> 24) & 0xF); // bits 24 through 27
    info->regtype = (RegisterType) (((token >> 28) & 0x7) | ((token >> 8) & 0x18));  // bits 28-30, 11-12

    ctx->tokens++;  // swallow token for now, for multiple calls in a row.
    ctx->tokencount--;  // swallow token for now, for multiple calls in a row.

    if (reserved1 != 0x0)
        return fail(ctx, "Reserved bits #1 in source token must be zero");

    if (reserved2 != 0x1)
        return fail(ctx, "Reserved bit #2 in source token must be one");

    if (info->relative)
    {
        if ((ctx->shader_type == MOJOSHADER_TYPE_PIXEL) && (ctx->major_ver < 3))
            return fail(ctx, "Relative addressing in pixel shader version < 3.0");
        return fail(ctx, "Relative addressing is unsupported");  // !!! FIXME
    } // if

    if ( ((SourceMod) info->src_mod) >= SRCMOD_TOTAL )
        return fail(ctx, "Unknown source modifier");

    set_used_register(ctx, info->regtype, info->regnum);
    return 1;
} // parse_source_token


static int parse_args_NULL(Context *ctx)
{
    return (isfail(ctx) ? FAIL : 1);
} // parse_args_NULL


static int parse_args_DEF(Context *ctx)
{
    if (parse_destination_token(ctx, &ctx->dest_args[0]) == FAIL)
        return FAIL;

    ctx->dwords[0] = SWAP32(ctx->tokens[0]);
    ctx->dwords[1] = SWAP32(ctx->tokens[1]);
    ctx->dwords[2] = SWAP32(ctx->tokens[2]);
    ctx->dwords[3] = SWAP32(ctx->tokens[3]);

    return 6;
} // parse_args_DEF


static int parse_args_DEFB(Context *ctx)
{
    if (parse_destination_token(ctx, &ctx->dest_args[0]) == FAIL)
        return FAIL;

    ctx->dwords[0] = *(ctx->tokens) ? 1 : 0;

    return 3;
} // parse_args_DEFB


static int parse_args_DCL(Context *ctx)
{
    int unsupported = 0;
    const uint32 token = SWAP32(*(ctx->tokens));
    const int reserved1 = (int) ((token >> 31) & 0x1); // bit 31
    uint32 reserved_mask = 0x00000000;

    if (reserved1 != 0x1)
        return fail(ctx, "Bit #31 in DCL token must be one");

    ctx->tokens++;
    ctx->tokencount--;
    if (parse_destination_token(ctx, &ctx->dest_args[0]) == FAIL)
        return FAIL;

    const RegisterType regtype = ctx->dest_args[0].regtype;
    const int regnum = ctx->dest_args[0].regnum;
    if ((ctx->shader_type == MOJOSHADER_TYPE_PIXEL) && (ctx->major_ver >= 3))
    {
        if (regtype == REG_TYPE_INPUT)
            reserved_mask = 0x7FFFFFFF;

        else if (regtype == REG_TYPE_MISCTYPE)
        {
            const MiscTypeType mt = (MiscTypeType) regnum;
            if (mt == MISCTYPE_TYPE_POSITION)
                reserved_mask = 0x7FFFFFFF;
            else if (mt == MISCTYPE_TYPE_FACE)
            {
                reserved_mask = 0x7FFFFFFF;
                if (ctx->dest_args[0].writemask != 0xF)
                    return fail(ctx, "DCL face writemask must be full");
                else if (ctx->dest_args[0].result_mod != 0)
                    return fail(ctx, "DCL face result modifier must be zero");
                else if (ctx->dest_args[0].result_shift != 0)
                    return fail(ctx, "DCL face shift scale must be zero");
            } // else if
            else
            {
                unsupported = 1;
            } // else
        } // else if

        else if (regtype == REG_TYPE_TEXTURE)
        {
            const uint32 usage = (token & 0xF);
            const uint32 index = ((token >> 16) & 0xF);
            if (usage == MOJOSHADER_USAGE_TEXCOORD)
            {
                if (index > 7)
                    return fail(ctx, "DCL texcoord usage must have 0-7 index");
            } // if
            else if (usage == MOJOSHADER_USAGE_COLOR)
            {
                if (index != 0)
                    return fail(ctx, "DCL color usage must have 0 index");
            } // else if
            else
            {
                return fail(ctx, "Invalid DCL texture usage");
            } // else

            reserved_mask = 0x7FF0FFE0;
            ctx->dwords[0] = usage;
            ctx->dwords[1] = index;
        } // else if

        else if (regtype == REG_TYPE_SAMPLER)
        {
            reserved_mask = 0x7FFFFFF;
            ctx->dwords[0] = ((token >> 27) & 0xF);  // TextureType
        } // else if

        else
        {
            unsupported = 1;
        } // else
    } // if

    else if ((ctx->shader_type == MOJOSHADER_TYPE_PIXEL) && (ctx->major_ver >= 2))
    {
        if (regtype == REG_TYPE_INPUT)
            reserved_mask = 0x7FFFFFFF;
        else if (regtype == REG_TYPE_TEXTURE)
            reserved_mask = 0x7FFFFFFF;
        else if (regtype == REG_TYPE_SAMPLER)
        {
            reserved_mask = 0x7FFFFFF;
            ctx->dwords[0] = ((token >> 27) & 0xF);  // TextureType
        } // else if
        else
        {
            unsupported = 1;
        } // else
    } // if

    else if ((ctx->shader_type == MOJOSHADER_TYPE_VERTEX) && (ctx->major_ver >= 3))
    {
        if ((regtype == REG_TYPE_INPUT) || (regtype == REG_TYPE_OUTPUT))
        {
            const uint32 usage = (token & 0xF);
            const uint32 index = ((token >> 16) & 0xF);
            reserved_mask = 0x7FF0FFE0;
            ctx->dwords[0] = usage;
            ctx->dwords[1] = index;
        } // if
        else
        {
            unsupported = 1;
        } // else
    } // else if

    else if ((ctx->shader_type == MOJOSHADER_TYPE_VERTEX) && (ctx->major_ver >= 2))
    {
        if (regtype == REG_TYPE_INPUT)
        {
            const uint32 usage = (token & 0xF);
            const uint32 index = ((token >> 16) & 0xF);
            reserved_mask = 0x7FF0FFE0;
            ctx->dwords[0] = usage;
            ctx->dwords[1] = index;
        } // if
        else
        {
            unsupported = 1;
        } // else
    } // else if

    else
    {
        unsupported = 1;
    } // else

    if (unsupported)
        return fail(ctx, "invalid DCL register type for this shader model");

    if ((token & reserved_mask) != 0)
        return fail(ctx, "reserved bits in DCL dword aren't zero");

    return 3;
} // parse_args_DCL


static int parse_args_D(Context *ctx)
{
    if (parse_destination_token(ctx, &ctx->dest_args[0]) == FAIL) return FAIL;
    return 2;
} // parse_args_D


static int parse_args_S(Context *ctx)
{
    if (parse_source_token(ctx, &ctx->source_args[0]) == FAIL) return FAIL;
    return 2;
} // parse_args_S


static int parse_args_SS(Context *ctx)
{
    if (parse_source_token(ctx, &ctx->source_args[0]) == FAIL) return FAIL;
    if (parse_source_token(ctx, &ctx->source_args[1]) == FAIL) return FAIL;
    return 3;
} // parse_args_SS


static int parse_args_DS(Context *ctx)
{
    if (parse_destination_token(ctx, &ctx->dest_args[0]) == FAIL) return FAIL;
    if (parse_source_token(ctx, &ctx->source_args[0]) == FAIL) return FAIL;
    return 3;
} // parse_args_DS


static int parse_args_DSS(Context *ctx)
{
    if (parse_destination_token(ctx, &ctx->dest_args[0]) == FAIL) return FAIL;
    if (parse_source_token(ctx, &ctx->source_args[0]) == FAIL) return FAIL;
    if (parse_source_token(ctx, &ctx->source_args[1]) == FAIL) return FAIL;
    return 4;
} // parse_args_DSS


static int parse_args_DSSS(Context *ctx)
{
    if (parse_destination_token(ctx, &ctx->dest_args[0]) == FAIL) return FAIL;
    if (parse_source_token(ctx, &ctx->source_args[0]) == FAIL) return FAIL;
    if (parse_source_token(ctx, &ctx->source_args[1]) == FAIL) return FAIL;
    if (parse_source_token(ctx, &ctx->source_args[2]) == FAIL) return FAIL;
    return 5;
} // parse_args_DSSS


static int parse_args_DSSSS(Context *ctx)
{
    if (parse_destination_token(ctx, &ctx->dest_args[0]) == FAIL) return FAIL;
    if (parse_source_token(ctx, &ctx->source_args[0]) == FAIL) return FAIL;
    if (parse_source_token(ctx, &ctx->source_args[1]) == FAIL) return FAIL;
    if (parse_source_token(ctx, &ctx->source_args[2]) == FAIL) return FAIL;
    if (parse_source_token(ctx, &ctx->source_args[3]) == FAIL) return FAIL;
    return 6;
} // parse_args_DSSSS


static int parse_args_SINCOS(Context *ctx)
{
    // this opcode needs extra registers for sm2 and lower.
    if (!shader_version_atleast(ctx, 3, 0))
        return parse_args_DSSS(ctx);
    return parse_args_DS(ctx);
} // parse_args_SINCOS


static int parse_args_TEXCOORD(Context *ctx)
{
    // added extra register in ps_1_4.
    if (shader_version_atleast(ctx, 1, 4))
        return parse_args_DS(ctx);
    return parse_args_D(ctx);
} // parse_args_TEXCOORD



// State machine functions...

static void state_DEF(Context *ctx)
{
    const RegisterType regtype = ctx->dest_args[0].regtype;
    const int regnum = ctx->dest_args[0].regnum;
    switch (regtype)
    {
        case REG_TYPE_CONST:
        case REG_TYPE_CONST2:
        case REG_TYPE_CONST3:
        case REG_TYPE_CONST4:
            set_defined_register(ctx, regtype, regnum);
            break;

        default:
            fail(ctx, "DEF token using invalid register");
    } // switch
} // state_DEF

static void state_DEFI(Context *ctx)
{
    const RegisterType regtype = ctx->dest_args[0].regtype;
    const int regnum = ctx->dest_args[0].regnum;
    if (regtype != REG_TYPE_CONSTINT)
        fail(ctx, "DEFI token using invalid register");
    else
        set_defined_register(ctx, regtype, regnum);
} // state_DEFI

static void state_DEFB(Context *ctx)
{
    const RegisterType regtype = ctx->dest_args[0].regtype;
    const int regnum = ctx->dest_args[0].regnum;
    if (regtype != REG_TYPE_CONSTBOOL)
        fail(ctx, "DEFB token using invalid register");
    else
        set_defined_register(ctx, regtype, regnum);
} // state_DEFB

static void state_DCL(Context *ctx)
{
    const DestArgInfo *arg = &ctx->dest_args[0];
    const RegisterType regtype = arg->regtype;
    const int regnum = arg->regnum;
    const uint32 usage = ctx->dwords[0];
    int index = 0;

    // parse_args_DCL() does a lot of state checking before we get here.

    // !!! FIXME: fail if DCL opcode comes after real instructions.

    if (ctx->shader_type == MOJOSHADER_TYPE_VERTEX)
    {
        index = ctx->dwords[1];
        if (usage >= ((const uint32) MOJOSHADER_USAGE_TOTAL))
        {
            fail(ctx, "unknown DCL usage");
            return;
        } // if
    } // if

    else if (ctx->shader_type == MOJOSHADER_TYPE_PIXEL)
    {
        if (regtype == REG_TYPE_SAMPLER)
        {
            switch ((const TextureType) usage)
            {
                case TEXTURE_TYPE_2D:
                case TEXTURE_TYPE_CUBE:
                case TEXTURE_TYPE_VOLUME:
                    break;  // it's okay.
                default:
                    fail(ctx, "unknown sampler texture type");
                    return;
            } // switch
        } // if

    } // else if

    else
    {
        assert(0 && "Unsupported shader type."); // should be caught elsewhere.
    } // else

    set_defined_register(ctx, regtype, regnum);
    add_attribute_register(ctx, regtype, regnum, (MOJOSHADER_usage) usage,
                            index, arg->writemask);
} // state_DCL

static void state_FRC(Context *ctx)
{
    if (!shader_version_atleast(ctx, 2, 0))
    {
        const DestArgInfo *info = &ctx->dest_args[0];
        if ((info->writemask != 0x2) && (info->writemask != 0x3))
            fail(ctx, "FRC writemask must be .y or .xy for shader model 1.x");
    } // if
} // state_FRC


// replicate the matrix registers to source args. The D3D profile will
//  only use the one legitimate argument, but this saves other profiles
//  from having to build this.
static void sourcearg_matrix_replicate(Context *ctx, const int idx,
                                       const int rows)
{
    int i;
    SourceArgInfo *src = &ctx->source_args[idx];
    SourceArgInfo *dst = &ctx->source_args[idx+1];
    for (i = 0; i < (rows-1); i++, dst++)
    {
        memcpy(dst, src, sizeof (SourceArgInfo));
        dst->regnum += (i + 1);
        set_used_register(ctx, dst->regtype, dst->regnum);
    } // for
} // sourcearg_matrix_replicate

static void state_M4X4(Context *ctx)
{
    const DestArgInfo *info = &ctx->dest_args[0];
    if (info->writemask != 0xF)  // 0xF == 1111. No explicit mask.
        fail(ctx, "M4X4 writemask must be .xyzw");

// !!! FIXME: MSDN:
//The xyzw (default) mask is required for the destination register. Negate and swizzle modifiers are allowed for src0, but not for src1.
//Swizzle and negate modifiers are invalid for the src0 register. The dest and src0 registers cannot be the same.

    sourcearg_matrix_replicate(ctx, 1, 4);
} // state_M4X4

static void state_M4X3(Context *ctx)
{
    const DestArgInfo *info = &ctx->dest_args[0];
    if (info->writemask != 0x7)  // 0x7 == 0111. (that is: xyz)
        fail(ctx, "M4X3 writemask must be .xyz");

// !!! FIXME: MSDN stuff

    sourcearg_matrix_replicate(ctx, 1, 3);
} // state_M4X3

static void state_M3X4(Context *ctx)
{
    const DestArgInfo *info = &ctx->dest_args[0];
    if (info->writemask != 0xF)  // 0xF == 1111. No explicit mask.
        fail(ctx, "M3X4 writemask must be .xyzw");

// !!! FIXME: MSDN stuff

    sourcearg_matrix_replicate(ctx, 1, 4);
} // state_M3X4

static void state_M3X3(Context *ctx)
{
    const DestArgInfo *info = &ctx->dest_args[0];
    if (info->writemask != 0x7)  // 0x7 == 0111. (that is: xyz)
        fail(ctx, "M3X3 writemask must be .xyz");

// !!! FIXME: MSDN stuff

    sourcearg_matrix_replicate(ctx, 1, 3);
} // state_M3X3

static void state_M3X2(Context *ctx)
{
    const DestArgInfo *info = &ctx->dest_args[0];
    if (info->writemask != 0x3)  // 0x3 == 0011. (that is: xy)
        fail(ctx, "M3X2 writemask must be .xy");

// !!! FIXME: MSDN stuff

    sourcearg_matrix_replicate(ctx, 1, 2);
} // state_M3X2

static void state_RET(Context *ctx)
{
    // MSDN all but says that assembly shaders are more or less serialized
    //  HLSL functions, and a RET means you're at the end of one, unlike how
    //  most CPUs would behave. This is actually really helpful,
    //  since we can use high-level constructs and not a mess of GOTOs,
    //  which is a godsend for GLSL...this also means we can consider things
    //  like a LOOP without a matching ENDLOOP within a label's section as
    //  an error.
    if (ctx->loops > 0)
        fail(ctx, "LOOP without ENDLOOP");
    if (ctx->reps > 0)
        fail(ctx, "REP without ENDREP");
} // state_RET

static int check_label_register(Context *ctx, int arg, const char *opcode)
{
    const SourceArgInfo *info = &ctx->source_args[arg];
    const RegisterType regtype = info->regtype;
    const int regnum = info->regnum;

    if (regtype != REG_TYPE_LABEL)
        return failf(ctx, "%s with a non-label register specified", opcode);

    else if (!shader_version_atleast(ctx, 2, 0))
        return failf(ctx, "%s not supported in Shader Model 1", opcode);

    else if ((shader_version_atleast(ctx, 2, 255)) && (regnum > 2047))
        return failf(ctx, "label register number must be <= 2047");

    else if (regnum > 15)
        return failf(ctx, "label register number must be <= 15");

    return 0;
} // check_label_register

static void state_LABEL(Context *ctx)
{
    if (ctx->previous_opcode != OPCODE_RET)
        fail(ctx, "LABEL not followed by a RET");
    check_label_register(ctx, 0, "LABEL");
    set_defined_register(ctx, REG_TYPE_LABEL, ctx->source_args[0].regnum);
} // state_LABEL

static void check_call_loop_wrappage(Context *ctx, const int regnum)
{
    // msdn says subroutines inherit aL register if you're in a loop when
    //  you call, and further more _if you ever call this function in a loop,
    //  it must always be called in a loop_. So we'll just pass our loop
    //  variable as a function parameter in those cases.

    const int current_usage = (ctx->loops > 0) ? 1 : -1;
    RegisterList *reg = reglist_find(&ctx->used_registers, REG_TYPE_LABEL, regnum);
    assert(reg != NULL);

    if (reg->usage == 0)
        reg->usage = current_usage;
    else if (reg->usage != current_usage)
    {
        if (current_usage == 1)
            fail(ctx, "CALL to this label must be wrapped in LOOP/ENDLOOP");
        else
            fail(ctx, "CALL to this label must not be wrapped in LOOP/ENDLOOP");
    } // else if
} // check_call_loop_wrappage

static void state_CALL(Context *ctx)
{
    if (check_label_register(ctx, 0, "CALL") != FAIL)
        check_call_loop_wrappage(ctx, ctx->source_args[0].regnum);
} // state_CALL

static void state_CALLNZ(Context *ctx)
{
    const RegisterType regtype = ctx->source_args[1].regtype;
    if ((regtype != REG_TYPE_CONSTBOOL) && (regtype != REG_TYPE_PREDICATE))
        fail(ctx, "CALLNZ argument isn't constbool or predicate register");
    else if (check_label_register(ctx, 0, "CALLNZ") != FAIL)
        check_call_loop_wrappage(ctx, ctx->source_args[0].regnum);
} // state_CALLNZ

static void state_MOVA(Context *ctx)
{
    if (ctx->dest_args[0].regtype != REG_TYPE_ADDRESS)
        fail(ctx, "MOVA argument isn't address register");
} // state_MOVA

static void state_LOOP(Context *ctx)
{
    if (ctx->source_args[0].regtype != REG_TYPE_LOOP)
        fail(ctx, "LOOP argument isn't loop register");
    else if (ctx->source_args[1].regtype != REG_TYPE_CONSTINT)
        fail(ctx, "LOOP argument isn't constint register");
    else
        ctx->loops++;
} // state_LOOP

static void state_ENDLOOP(Context *ctx)
{
    // !!! FIXME: check that we aren't straddling an IF block.
    if (ctx->loops <= 0)
        fail(ctx, "ENDLOOP without LOOP");
    ctx->loops--;
} // state_ENDLOOP

static void state_BREAKP(Context *ctx)
{
    const RegisterType regtype = ctx->source_args[0].regtype;
    if (regtype != REG_TYPE_PREDICATE)
        fail(ctx, "BREAKP argument isn't predicate register");
    else if ((ctx->loops == 0) && (ctx->reps == 0))
        fail(ctx, "BREAKP outside LOOP/ENDLOOP or REP/ENDREP");
} // state_BREAKP

static void state_BREAK(Context *ctx)
{
    if ((ctx->loops == 0) && (ctx->reps == 0))
        fail(ctx, "BREAK outside LOOP/ENDLOOP or REP/ENDREP");
} // state_BREAKP

static void state_SETP(Context *ctx)
{
    const RegisterType regtype = ctx->dest_args[0].regtype;
    if (regtype != REG_TYPE_PREDICATE)
        fail(ctx, "SETP argument isn't predicate register");
} // state_SETP

static void state_REP(Context *ctx)
{
    const RegisterType regtype = ctx->source_args[0].regtype;
    if (regtype != REG_TYPE_CONSTINT)
        fail(ctx, "REP argument isn't constint register");
    ctx->reps++;
} // state_REP

static void state_ENDREP(Context *ctx)
{
    // !!! FIXME: check that we aren't straddling an IF block.
    if (ctx->reps <= 0)
        fail(ctx, "ENDREP without REP");
    ctx->reps--;
} // state_ENDREP

static void state_CMP(Context *ctx)
{
    ctx->cmps++;

    // extra limitations for ps <= 1.4 ...
    if (!shader_version_atleast(ctx, 1, 4))
    {
        int i;
        const DestArgInfo *dst = &ctx->dest_args[0];
        const RegisterType dregtype = dst->regtype;
        const int dregnum = dst->regnum;

        if (ctx->cmps > 3)
            fail(ctx, "only 3 CMP instructions allowed in this shader model");

        for (i = 0; i < 3; i++)
        {
            const SourceArgInfo *src = &ctx->source_args[i];
            const RegisterType sregtype = src->regtype;
            const int sregnum = src->regnum;
            if ((dregtype == sregtype) && (dregnum == sregnum))
                fail(ctx, "CMP dest can't match sources in this shader model");
        } // for
    } // if
} // state_CMP

static void state_CND(Context *ctx)
{
    // apparently it was removed...it's not in the docs past ps_1_4 ...
    if (shader_version_atleast(ctx, 2, 0))
        fail(ctx, "CND not allowed in this shader model");

    // extra limitations for ps <= 1.4 ...
    else if (!shader_version_atleast(ctx, 1, 4))
    {
        const SourceArgInfo *src = &ctx->source_args[0];
        if ((src->regtype != REG_TYPE_TEMP) || (src->regnum != 0) ||
            (src->swizzle != 0x0000))
        {
            fail(ctx, "CND src must be r0.a in this shader model");
        } // if
    } // if
} // state_CND

static void state_SINCOS(Context *ctx)
{
    const int mask = ctx->dest_args[0].writemask;
    if ((mask < 0x1) || (mask > 0x3))
    {
        fail(ctx, "SINCOS write mask must be .x or .y or .xy");
        return;
    } // if

    // this opcode needs extra registers, with extra limitations, for <= sm2.
    if (!shader_version_atleast(ctx, 3, 0))
    {
        int i;
        for (i = 1; i < 3; i++)
        {
            switch (ctx->source_args[i].regtype)
            {
                case REG_TYPE_CONST:
                case REG_TYPE_CONST2:
                case REG_TYPE_CONST3:
                case REG_TYPE_CONST4:
                    break;
                default:
                    failf(ctx, "SINCOS src%d must be constfloat", i);
                    return;
            } // switch
        } // for

        if (ctx->source_args[1].regnum == ctx->source_args[2].regnum)
            fail(ctx, "SINCOS src1 and src2 must be different registers");
    } // if
} // state_SINCOS

static void state_IFC(Context *ctx)
{
    if (!replicate_swizzle(ctx->source_args[0].swizzle))
        fail(ctx, "IFC src1 must have replicate swizzle");
    else if (!replicate_swizzle(ctx->source_args[1].swizzle))
        fail(ctx, "IFC src2 must have replicate swizzle");
} // state_IFC

static void state_BREAKC(Context *ctx)
{
    if (!replicate_swizzle(ctx->source_args[0].swizzle))
        fail(ctx, "BREAKC src1 must have replicate swizzle");
    else if (!replicate_swizzle(ctx->source_args[1].swizzle))
        fail(ctx, "BREAKC src2 must have replicate swizzle");
    else if ((ctx->loops == 0) && (ctx->reps == 0))
        fail(ctx, "BREAKC outside LOOP/ENDLOOP or REP/ENDREP");
} // state_BREAKC


// Lookup table for instruction opcodes...
typedef struct
{
    const char *opcode_string;
    MOJOSHADER_shaderType shader_types;  // mask of types that can use opcode.
    int arg_tokens;
    args_function parse_args;
    state_function state;
    emit_function emitter[STATICARRAYLEN(profiles)];
} Instruction;

// These have to be in the right order! This array is indexed by the value
//  of the instruction token.
static const Instruction instructions[] =
{
    // INSTRUCTION_STATE means this opcode has to update the state machine
    //  (we're entering an ELSE block, etc). INSTRUCTION means there's no
    //  state, just go straight to the emitters.
    #define INSTRUCTION_STATE(op, args, argsseq, t) { \
        #op, t, args, parse_args_##argsseq, state_##op, PROFILE_EMITTERS(op) \
    }
    #define INSTRUCTION(op, args, argsseq, t) { \
        #op, t, args, parse_args_##argsseq, 0, PROFILE_EMITTERS(op) \
    }

    // !!! FIXME: Some of these MOJOSHADER_TYPE_ANYs need to have their scope
    // !!! FIXME:  reduced to just PIXEL or VERTEX.

    INSTRUCTION(NOP, 0, NULL, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(MOV, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(ADD, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(SUB, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(MAD, 4, DSSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(MUL, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(RCP, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(RSQ, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(DP3, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(DP4, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(MIN, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(MAX, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(SLT, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(SGE, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(EXP, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(LOG, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(LIT, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(DST, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(LRP, 4, DSSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(FRC, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(M4X4, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(M4X3, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(M3X4, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(M3X3, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(M3X2, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(CALL, 1, S, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(CALLNZ, 2, SS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(LOOP, 2, SS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(RET, 0, NULL, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(ENDLOOP, 0, NULL, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(LABEL, 1, S, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(DCL, 2, DCL, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(POW, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(CRS, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(SGN, 4, DSSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(ABS, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(NRM, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(SINCOS, -1, SINCOS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(REP, 1, S, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(ENDREP, 0, NULL, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(IF, 1, S, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(IFC, 2, SS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(ELSE, 0, NULL, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(ENDIF, 0, NULL, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(BREAK, 0, NULL, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(BREAKC, 2, SS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(MOVA, 2, DS, MOJOSHADER_TYPE_VERTEX),
    INSTRUCTION_STATE(DEFB, 2, DEFB, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(DEFI, 5, DEF, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(TEXCOORD, -1, TEXCOORD, MOJOSHADER_TYPE_PIXEL),
    INSTRUCTION(TEXKILL, 1, D, MOJOSHADER_TYPE_PIXEL),
    INSTRUCTION(TEX, -1, TEXCOORD, MOJOSHADER_TYPE_PIXEL), // same parse_args logic as TEXCOORD
    INSTRUCTION(TEXBEM, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXBEML, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXREG2AR, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXREG2GB, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXM3X2PAD, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXM3X2TEX, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXM3X3PAD, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXM3X3TEX, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(RESERVED, 0, NULL, MOJOSHADER_TYPE_UNKNOWN),
    INSTRUCTION(TEXM3X3SPEC, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXM3X3VSPEC, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(EXPP, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(LOGP, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(CND, 4, DSSS, MOJOSHADER_TYPE_PIXEL),
    INSTRUCTION_STATE(DEF, 5, DEF, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXREG2RGB, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXDP3TEX, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXM3X2DEPTH, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXDP3, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXM3X3, 2, DS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXDEPTH, 1, D, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(CMP, 4, DSSS, MOJOSHADER_TYPE_PIXEL),
    INSTRUCTION(BEM, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(DP2ADD, 4, DSSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(DSX, 2, DS, MOJOSHADER_TYPE_PIXEL),
    INSTRUCTION(DSY, 2, DS, MOJOSHADER_TYPE_PIXEL),
    INSTRUCTION(TEXLDD, 5, DSSSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(SETP, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION(TEXLDL, 3, DSS, MOJOSHADER_TYPE_ANY),
    INSTRUCTION_STATE(BREAKP, 1, S, MOJOSHADER_TYPE_ANY),

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
    int retval = NOFAIL;

    if ( opcode >= (sizeof (instructions) / sizeof (instructions[0])) )
        return 0;  // not an instruction token, or just not handled here.

    if ((token & 0x80000000) != 0)
        return fail(ctx, "instruction token high bit must be zero.");  // so says msdn.

    if (coissue)  // !!! FIXME: I'm not sure what this means, yet.
        return fail(ctx, "coissue instructions unsupported");

    if (predicated)  // !!! FIXME: I'm not sure what this means, yet.
        return fail(ctx, "predicated instructions unsupported");

    if (ctx->major_ver < 2)
    {
        if (insttoks != 0)  // this is a reserved field in shaders < 2.0 ...
            return fail(ctx, "instruction token count must be zero");
    } // if

    else if (instruction->arg_tokens >= 0)
    {
        if (instruction->arg_tokens != insttoks)
        {
            return failf(ctx, "unexpected tokens count (%u) for opcode '%s'.",
                        (uint) insttoks, instruction->opcode_string);
        } // if
        else if (ctx->tokencount <= instruction->arg_tokens)
        {
            return failf(ctx,
                        "need more tokens (need %u, got %u) for opcode '%s'.",
                        (uint) instruction->arg_tokens, (uint) ctx->tokencount,
                        instruction->opcode_string);
        } // else if
    } // else if

    if ((ctx->shader_type & instruction->shader_types) == 0)
    {
        return failf(ctx, "opcode '%s' not available in this shader type.",
                     instruction->opcode_string);
    } // if

    ctx->instruction_count++;
    ctx->instruction_controls = controls;

    // Update the context with instruction's arguments.
    ctx->tokens++;
    ctx->tokencount--;
    retval = instruction->parse_args(ctx);
    assert((isfail(ctx)) || (retval >= 0));

    // parse_args() moves these forward for convenience...reset them.
    ctx->tokens = start_tokens;
    ctx->tokencount = start_tokencount;

    if (!isfail(ctx))
    {
        if (instruction->state != NULL)
            instruction->state(ctx);
    } // if

    if (isfail(ctx))
        retval = FAIL;
    else
        emitter(ctx);  // call the profile's emitter.

    ctx->previous_opcode = opcode;

    return retval;
} // parse_instruction_token


static int parse_version_token(Context *ctx)
{
    if (isfail(ctx))  // catch preexisting errors here.
        return FAIL;

    if (ctx->tokencount == 0)
        return fail(ctx, "Expected version token, got none at all.");

    const uint32 token = SWAP32(*(ctx->tokens));
    const uint32 shadertype = ((token >> 16) & 0xFFFF);
    const uint8 major = (uint8) ((token >> 8) & 0xFF);
    const uint8 minor = (uint8) (token & 0xFF);

    // 0xFFFF == pixel shader, 0xFFFE == vertex shader
    if (shadertype == 0xFFFF)
        ctx->shader_type = MOJOSHADER_TYPE_PIXEL;
    else if (shadertype == 0xFFFE)
        ctx->shader_type = MOJOSHADER_TYPE_VERTEX;
    else  // geometry shader? Bogus data?
        return fail(ctx, "Unsupported shader type or not a shader at all");

    ctx->major_ver = major;
    ctx->minor_ver = minor;

    if (!shader_version_supported(major, minor))
    {
        return failf(ctx, "Shader Model %u.%u is currently unsupported.",
                     (uint) major, (uint) minor);
    } // if

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
        const int needmalloc = (len >= SCRATCH_BUFFER_SIZE);
        char *str = NULL;

        if (!needmalloc)
            str = get_scratch_buffer(ctx);
        else
        {
            str = (char *) Malloc(ctx, len + 1);
            if (str == NULL)
                return out_of_memory(ctx);
        } // else

        memcpy(str, (const char *) (ctx->tokens+1), len);
        str[len] = '\0';
        ctx->profile->comment_emitter(ctx, str);

        if (needmalloc)
            Free(ctx, str);

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

    if (isfail(ctx))
        return FAIL;  // just in case...catch previously unhandled fails here.

    if (ctx->output_stack_len != 0)
        return fail(ctx, "BUG: output stack isn't empty on new token!");

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


static void *internal_malloc(int bytes, void *d) { return malloc(bytes); }
static void internal_free(void *ptr, void *d) { free(ptr); }


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
                              MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    if (m == NULL) m = internal_malloc;
    if (f == NULL) f = internal_free;

    Context *ctx = m(sizeof (Context), d);
    if (ctx == NULL)
        return NULL;

    memset(ctx, '\0', sizeof (Context));
    ctx->malloc = m;
    ctx->free = f;
    ctx->malloc_data = d;
    ctx->tokens = (const uint32 *) tokenbuf;
    ctx->tokencount = bufsize / sizeof (uint32);
    ctx->endline = endline_str;
    ctx->endline_len = strlen(ctx->endline);
    ctx->globals.tail = &ctx->globals.head;
    ctx->helpers.tail = &ctx->helpers.head;
    ctx->subroutines.tail = &ctx->subroutines.head;
    ctx->mainline.tail = &ctx->mainline.head;
    ctx->ignore.tail = &ctx->ignore.head;
    ctx->output = &ctx->mainline;

    const int profileid = find_profile_id(profile);
    ctx->profileid = profileid;
    if (profileid >= 0)
        ctx->profile = &profiles[profileid];
    else
        failf(ctx, "Profile '%s' is unknown or unsupported", profile);

    return ctx;
} // build_context


static void free_output_list(MOJOSHADER_free f, void *d, OutputListNode *item)
{
    while (item != NULL)
    {
        OutputListNode *next = item->next;
        if (item->str != NULL)
            f(item->str, d);
        f(item, d);
        item = next;
    } // while
} // free_output_list


static void destroy_context(Context *ctx)
{
    if (ctx != NULL)
    {
        MOJOSHADER_free f = ((ctx->free != NULL) ? ctx->free : internal_free);
        void *d = ctx->malloc_data;
        if (ctx->output_bytes != NULL)
            f(d, ctx->output_bytes);
        free_output_list(f, d, ctx->globals.head.next);
        free_output_list(f, d, ctx->helpers.head.next);
        free_output_list(f, d, ctx->subroutines.head.next);
        free_output_list(f, d, ctx->mainline.head.next);
        free_output_list(f, d, ctx->ignore.head.next);
        free_reglist(f, d, ctx->used_registers.next);
        free_reglist(f, d, ctx->defined_registers.next);
        free_reglist(f, d, ctx->uniforms.next);
        free_reglist(f, d, ctx->attributes.next);
        if ((ctx->failstr != NULL) && (ctx->failstr != out_of_mem_str))
            f((void *) ctx->failstr, d);
        f(ctx, d);
    } // if
} // destroy_context


static void append_list(char **_wptr, const char *endline,
                        const size_t endline_len, OutputListNode *item)
{
    char *wptr = *_wptr;
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
    *_wptr = wptr;
} // append_list


static char *build_output(Context *ctx)
{
    // add a byte for the null terminator if we're doing text output.
    const int plusbytes = (ctx->output_bytes == NULL) ? 1 : 0;
    char *retval = (char *) Malloc(ctx, ctx->output_len + plusbytes);
    if (retval == NULL)
        out_of_memory(ctx);
    else
    {
        const char *endl = ctx->endline;
        const size_t endllen = ctx->endline_len;
        char *wptr = retval;
        if (ctx->output_bytes != NULL)
            memcpy(retval, ctx->output_bytes, ctx->output_len);
        else
        {
            append_list(&wptr, endl, endllen, ctx->globals.head.next);
            append_list(&wptr, endl, endllen, ctx->helpers.head.next);
            append_list(&wptr, endl, endllen, ctx->subroutines.head.next);
            append_list(&wptr, endl, endllen, ctx->mainline.head.next);
            // don't append ctx->ignore ... that's why it's called "ignore"
        } // else
    } // else

    return retval;
} // build_output


static MOJOSHADER_uniform *build_uniforms(Context *ctx)
{
    MOJOSHADER_uniform *retval = (MOJOSHADER_uniform *)
                Malloc(ctx, sizeof (MOJOSHADER_uniform) * ctx->uniform_count);

    if (retval == NULL)
        out_of_memory(ctx);
    else
    {
        RegisterList *item = ctx->uniforms.next;
        MOJOSHADER_uniformType type = MOJOSHADER_UNIFORM_FLOAT;
        int index = 0;
        int i;

        for (i = 0; i < ctx->uniform_count; i++)
        {
            if (item == NULL)
            {
                fail(ctx, "BUG: mismatched uniform list and count");
                break;
            } // if

            index = item->regnum;
            switch (item->regtype)
            {
                case REG_TYPE_CONST:
                    type = MOJOSHADER_UNIFORM_FLOAT;
                    break;

                case REG_TYPE_CONST2:
                    index += 2048;
                    type = MOJOSHADER_UNIFORM_FLOAT;
                    break;

                case REG_TYPE_CONST3:
                    index += 4096;
                    type = MOJOSHADER_UNIFORM_FLOAT;
                    break;

                case REG_TYPE_CONST4:
                    index += 6144;
                    type = MOJOSHADER_UNIFORM_FLOAT;
                    break;

                case REG_TYPE_CONSTINT:
                    type = MOJOSHADER_UNIFORM_INT;
                    break;

                case REG_TYPE_CONSTBOOL:
                    type = MOJOSHADER_UNIFORM_BOOL;
                    break;

                default:
                    fail(ctx, "unknown uniform datatype");
                    break;
            } // switch

            retval[i].type = type;
            retval[i].index = index;
            item = item->next;
        } // while
    } // else

    return retval;
} // build_uniforms


static MOJOSHADER_attribute *build_attributes(Context *ctx)
{
    MOJOSHADER_attribute *retval = (MOJOSHADER_attribute *)
             Malloc(ctx, sizeof (MOJOSHADER_attribute) * ctx->attribute_count);

    if (retval == NULL)
        out_of_memory(ctx);
    else
    {
        RegisterList *item = ctx->attributes.next;
        int i;

        for (i = 0; i < ctx->attribute_count; i++)
        {
            if (item == NULL)
            {
                fail(ctx, "BUG: mismatched attribute list and count");
                break;
            } // if

            retval[i].usage = item->usage;
            retval[i].index = item->index;
            item = item->next;
        } // while
    } // else

    return retval;
} // build_attributes



static MOJOSHADER_parseData *build_parsedata(Context *ctx)
{
    char *output = NULL;
    MOJOSHADER_uniform *uniforms = NULL;
    MOJOSHADER_attribute *attributes = NULL;
    MOJOSHADER_parseData *retval;

    if ((retval = Malloc(ctx, sizeof (MOJOSHADER_parseData))) == NULL)
        return &out_of_mem_data;

    memset(retval, '\0', sizeof (MOJOSHADER_parseData));

    if (!isfail(ctx))
        output = build_output(ctx);

    if (!isfail(ctx))
        uniforms = build_uniforms(ctx);

    if (!isfail(ctx))
        attributes = build_attributes(ctx);

    // check again, in case build_output ran out of memory.
    if (isfail(ctx))
    {
        Free(ctx, output);
        Free(ctx, uniforms);
        Free(ctx, attributes);
        retval->error = ctx->failstr;  // we recycle.  :)
        ctx->failstr = NULL;  // don't let this get free()'d too soon.
    } // if
    else
    {
        retval->output = output;
        retval->output_len = ctx->output_len;
        retval->instruction_count = ctx->instruction_count;
        retval->shader_type = ctx->shader_type;
        retval->major_ver = (int) ctx->major_ver;
        retval->minor_ver = (int) ctx->minor_ver;
        retval->uniform_count = ctx->uniform_count;
        retval->uniforms = uniforms;
        retval->attribute_count = ctx->attribute_count;
        retval->attributes = attributes;
    } // else

    retval->malloc = (ctx->malloc == internal_malloc) ? NULL : ctx->malloc;
    retval->free = (ctx->free == internal_free) ? NULL : ctx->free;
    retval->malloc_data = ctx->malloc_data;

    return retval;
} // build_parsedata


static void process_definitions(Context *ctx)
{
    RegisterList *uitem = &ctx->uniforms;
    RegisterList *prev = &ctx->used_registers;
    RegisterList *item = prev->next;

    while (item != NULL)
    {
        RegisterList *next = item->next;
        const RegisterType regtype = item->regtype;
        const int regnum = item->regnum;

        if (!get_defined_register(ctx, regtype, regnum))
        {
            // haven't already dealt with this one.
            switch (regtype)
            {
                // !!! FIXME: I'm not entirely sure this is right...
                case REG_TYPE_RASTOUT:
                case REG_TYPE_ATTROUT:
                case REG_TYPE_TEXCRDOUT:
                case REG_TYPE_COLOROUT:
                case REG_TYPE_DEPTHOUT:
                    if (ctx->shader_type == MOJOSHADER_TYPE_VERTEX)
                    {
                        if (shader_version_atleast(ctx, 3, 0))
                        {
                            fail(ctx, "vs_3 can't use output registers"
                                      " without declaring them first.");
                            return;
                        } // if
                    } // if

                    // Apparently this is an attribute that wasn't DCL'd.
                    //  Add it to the attribute list; deal with it later.
                    add_attribute_register(ctx, item->regtype, item->regnum,
                                           0, 0, 0xF);
                    break;

                case REG_TYPE_ADDRESS:
                case REG_TYPE_PREDICATE:
                case REG_TYPE_TEMP:
                case REG_TYPE_LOOP:
                case REG_TYPE_LABEL:
                    ctx->profile->global_emitter(ctx, regtype, regnum);
                    break;

                case REG_TYPE_CONST:
                case REG_TYPE_CONST2:
                case REG_TYPE_CONST3:
                case REG_TYPE_CONST4:
                case REG_TYPE_CONSTINT:
                case REG_TYPE_CONSTBOOL:
                    // separate uniforms into a different list for now.
                    prev->next = item->next;
                    item->next = NULL;
                    uitem->next = item;
                    uitem = item;
                    item = prev;
                    break;

                default:
                    fail(ctx, "BUG: we used a register we don't know how to define.");
            } // switch
        } // if

        prev = item;
        item = next;
    } // while

    // okay, now deal with uniforms...
    for (item = ctx->uniforms.next; item != NULL; item = item->next)
    {
        ctx->uniform_count++;
        ctx->profile->uniform_emitter(ctx, item->regtype, item->regnum);
    } // for

    // ...and attributes...
    for (item = ctx->attributes.next; item != NULL; item = item->next)
    {
        ctx->attribute_count++;
        ctx->profile->attribute_emitter(ctx, item->regtype, item->regnum,
                                        item->usage, item->index,
                                        item->writemask);
    } // for
} // process_definitions


// API entry point...

const MOJOSHADER_parseData *MOJOSHADER_parse(const char *profile,
                                             const unsigned char *tokenbuf,
                                             const unsigned int bufsize,
                                             MOJOSHADER_malloc m,
                                             MOJOSHADER_free f, void *d)
{
    MOJOSHADER_parseData *retval = NULL;
    Context *ctx = NULL;
    int rc = FAIL;

    if ( ((m == NULL) && (f != NULL)) || ((m != NULL) && (f == NULL)) )
        return &out_of_mem_data;  // supply both or neither.

    if ((ctx = build_context(profile, tokenbuf, bufsize, m, f, d)) == NULL)
        return &out_of_mem_data;

    // Version token always comes first.
    rc = parse_version_token(ctx);

    // parse out the rest of the tokens after the version token...
    while ( (rc > 0) && (!isfail(ctx)) )
    {
        ctx->tokens += rc;
        ctx->tokencount -= rc;
        rc = parse_token(ctx);
    } // while

    if (!isfail(ctx))
        process_definitions(ctx);

    if (!isfail(ctx))
        ctx->profile->finalize_emitter(ctx);

    retval = build_parsedata(ctx);
    destroy_context(ctx);
    return retval;
} // MOJOSHADER_parse


void MOJOSHADER_freeParseData(const MOJOSHADER_parseData *_data)
{
    MOJOSHADER_parseData *data = (MOJOSHADER_parseData *) _data;
    if ((data == NULL) || (data == &out_of_mem_data))
        return;  // no-op.

    MOJOSHADER_free f = (data->free == NULL) ? internal_free : data->free;
    void *d = data->malloc_data;

    if (data->output != NULL)  // check for NULL in case of dumb free() impl.
        f((void *) data->output, d);

    if (data->uniforms != NULL)
        f((void *) data->uniforms, d);

    if (data->attributes != NULL)
        f((void *) data->attributes, d);

    if ((data->error != NULL) && (data->error != out_of_mem_str))
        f((void *) data->error, d);

    f(data, d);
} // MOJOSHADER_freeParseData


int MOJOSHADER_version(void)
{
    return MOJOSHADER_VERSION;
} // MOJOSHADER_version

// end of mojoshader.c ...

