/**
 * d3d2glsl; generate GLSL programs from bytecode of compiled Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

// Shader bytecode format is described at MSDN:
//  http://msdn2.microsoft.com/en-us/library/ms800307.aspx

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>

#include "d3d2glsl.h"

#ifndef SUPPORT_PROFILE_D3D
#define SUPPORT_PROFILE_D3D 1
#endif

#ifndef SUPPORT_PROFILE_GLSL
#define SUPPORT_PROFILE_GLSL 1
#endif

#ifdef __GNUC__
#define ISPRINTF(x,y) __attribute__((format (printf, x, y)))
#else
#define ISPRINTF(x,y)
#endif

#define STATICARRAYLEN(x) ( (sizeof ((x))) / (sizeof ((x)[0])) )

typedef unsigned int uint;  // this is a printf() helper. don't use for code.
typedef uint8_t uint8;
typedef uint32_t uint32;

// predeclare.
typedef struct D3D2GLSL_context D3D2GLSL_context;

// one emit function for each opcode in each profile.
typedef void (*emit_function)(D3D2GLSL_context *ctx);

// one emit function for comments in each profile.
typedef void (*emit_comment)(D3D2GLSL_context *ctx, const char *str);

// one emit function for starting output in each profile.
typedef void (*emit_start)(D3D2GLSL_context *ctx, uint32 shadertype,
                           uint32 major, uint32 minor);

// one emit function for ending output in each profile.
typedef void (*emit_end)(D3D2GLSL_context *ctx);

// one state function for each opcode where we have state machine updates.
typedef int (*state_function)(D3D2GLSL_context *ctx);

typedef struct
{
    const char *name;
    emit_start start_emitter;
    emit_end end_emitter;
    emit_comment comment_emitter;
} D3D2GLSL_profile;


#define D3D2GLSL_SCRATCH_BUFFER_SIZE 256
#define D3D2GLSL_SCRATCH_BUFFERS 5

// Context...this is state that changes as we parse through a shader...
struct D3D2GLSL_context
{
    const uint32 *tokens;
    uint32 tokencount;
    char *output;
    uint32 output_len;
    char *failstr;
    char buffers[D3D2GLSL_SCRATCH_BUFFERS][D3D2GLSL_SCRATCH_BUFFER_SIZE];
    int bufidx;  // current scratch buffer.
    int profileidx;
    const D3D2GLSL_profile *profile;
};



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


static inline char *get_scratch_buffer(D3D2GLSL_context *ctx)
{
    ctx->bufidx = (ctx->bufidx + 1) % D3D2GLSL_SCRATCH_BUFFERS;
    return ctx->buffers[ctx->bufidx];
} // get_scratch_buffer


// Special-case return values from the parsing pipeline...
#define FAIL (-1)
#define END_OF_STREAM (-2)

static int failf(D3D2GLSL_context *ctx, const char *fmt, ...) ISPRINTF(2,3);
static int failf(D3D2GLSL_context *ctx, const char *fmt, ...)
{
    if (ctx->failstr == NULL)  // don't change existing error.
    {
        char *buffer = get_scratch_buffer(ctx);
        va_list ap;
        va_start(ap, fmt);
        vsnprintf(buffer, D3D2GLSL_SCRATCH_BUFFER_SIZE, fmt, ap);
        va_end(ap);

        ctx->failstr = (char *) malloc(strlen(buffer) + 1);
        if (ctx->failstr != NULL)
            strcpy(ctx->failstr, buffer);
    } // if

    return FAIL;
} // failf


static inline int fail(D3D2GLSL_context *ctx, const char *reason)
{
    return failf(ctx, "%s", reason);
} // fail


static int output_line(D3D2GLSL_context *ctx, const char *fmt, ...) ISPRINTF(2,3);
static int output_line(D3D2GLSL_context *ctx, const char *fmt, ...)
{
    if (ctx->failstr != NULL)
        return FAIL;  // we failed previously, don't go on...

    char *buffer = get_scratch_buffer(ctx);
    va_list ap;
    va_start(ap, fmt);
    const int len = vsnprintf(buffer, D3D2GLSL_SCRATCH_BUFFER_SIZE, fmt, ap);
    va_end(ap);

    if (len >= D3D2GLSL_SCRATCH_BUFFER_SIZE)
    {
        return failf(ctx, "Internal bug--String is too big (%d vs %d bytes).",
                     (int) len, (int) D3D2GLSL_SCRATCH_BUFFER_SIZE);
    } // if

    const size_t newlen = ctx->output_len + len + 1;
    char *ptr = (char *) realloc(ctx->output, newlen+1);
    if (ptr == NULL)
        return fail(ctx, "Out of memory");

    memcpy(ptr + ctx->output_len, buffer, len);
    ptr[newlen-1] = '\n';
    ptr[newlen] = '\0';
    ctx->output = ptr;
    ctx->output_len = newlen;
    return 0;
} // output_line


static int parse_destination_token(D3D2GLSL_context *ctx)
{
    const uint32 token = SWAP32(*(ctx->tokens));
    const uint32 regnum = (token & 0x7ff);  // bits 0 through 10
} // parse_destination_token


#define AT_LEAST_ONE_PROFILE 0

#if !SUPPORT_PROFILE_D3D
#define PROFILE_EMITTER_D3D(op)
#else
#undef AT_LEAST_ONE_PROFILE
#define AT_LEAST_ONE_PROFILE 1
#define PROFILE_EMITTER_D3D(op) emit_D3D_##op,

static void emit_D3D_start(D3D2GLSL_context *ctx, uint32 shadertype,
                           uint32 major, uint32 minor)
{
    if (shadertype == 0xFFFF)  // pixel shader.
        output_line(ctx, "ps_%u_%u", (uint) major, (uint) minor);
    else if (shadertype == 0xFFFE)  // vertex shader.
        output_line(ctx, "vs_%u_%u", (uint) major, (uint) minor);
    else
        fail(ctx, "Unsupported in this profile.");
} // emit_D3D_start

static void emit_D3D_end(D3D2GLSL_context *ctx)
{
    output_line(ctx, "END");
} // emit_D3D_end

static void emit_D3D_comment(D3D2GLSL_context *ctx, const char *str)
{
    output_line(ctx, "; %s", str);
} // emit_D3D_comment

static void emit_D3D_NOP(D3D2GLSL_context *ctx)
{
    output_line(ctx, "NOP");
} // emit_D3D_NOP

static void emit_D3D_MOV(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_MOV

static void emit_D3D_ADD(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_ADD

static void emit_D3D_SUB(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_SUB

static void emit_D3D_MAD(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_MAD

static void emit_D3D_MUL(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_MUL

static void emit_D3D_RCP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_RCP

static void emit_D3D_RSQ(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_RSQ

static void emit_D3D_DP3(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_DP3

static void emit_D3D_DP4(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_DP4

static void emit_D3D_MIN(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_MIN

static void emit_D3D_MAX(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_MAX

static void emit_D3D_SLT(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_SLT

static void emit_D3D_SGE(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_SGE

static void emit_D3D_EXP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_EXP

static void emit_D3D_LOG(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_LOG

static void emit_D3D_LIT(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_LIT

static void emit_D3D_DST(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_DST

static void emit_D3D_LRP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_LRP

static void emit_D3D_FRC(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_FRC

static void emit_D3D_M4X4(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_M4X4

static void emit_D3D_M4X3(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_M4X3

static void emit_D3D_M3X4(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_M3X4

static void emit_D3D_M3X3(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_M3X3

static void emit_D3D_M3X2(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_M3X2

static void emit_D3D_CALL(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_CALL

static void emit_D3D_CALLNZ(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_CALLNZ

static void emit_D3D_LOOP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_LOOP

static void emit_D3D_RET(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_RET

static void emit_D3D_ENDLOOP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_ENDLOOP

static void emit_D3D_LABEL(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_LABEL

static void emit_D3D_DCL(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_DCL

static void emit_D3D_POW(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_POW

static void emit_D3D_CRS(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_CRS

static void emit_D3D_SGN(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_SGN

static void emit_D3D_ABS(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_ABS

static void emit_D3D_NRM(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_NRM

static void emit_D3D_SINCOS(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_SINCOS

static void emit_D3D_REP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_REP

static void emit_D3D_ENDREP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_ENDREP

static void emit_D3D_IF(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_IF

static void emit_D3D_IFC(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_IFC

static void emit_D3D_ELSE(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_ELSE

static void emit_D3D_ENDIF(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_ENDIF

static void emit_D3D_BREAK(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_BREAK

static void emit_D3D_BREAKC(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_BREAKC

static void emit_D3D_MOVA(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_MOVA

static void emit_D3D_DEFB(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_DEFB

static void emit_D3D_DEFI(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_DEFI

static void emit_D3D_TEXCOORD(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXCOORD

static void emit_D3D_TEXKILL(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXKILL

static void emit_D3D_TEX(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEX

static void emit_D3D_TEXBEM(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXBEM

static void emit_D3D_TEXBEML(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXBEML

static void emit_D3D_TEXREG2AR(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXREG2AR

static void emit_D3D_TEXREG2GB(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXREG2GB

static void emit_D3D_TEXM3X2PAD(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXM3X2PAD

static void emit_D3D_TEXM3X2TEX(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXM3X2TEX

static void emit_D3D_TEXM3X3PAD(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXM3X3PAD

static void emit_D3D_TEXM3X3TEX(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXM3X3TEX

static void emit_D3D_TEXM3X3SPEC(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXM3X3SPEC

static void emit_D3D_TEXM3X3VSPEC(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXM3X3VSPEC

static void emit_D3D_EXPP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_EXPP

static void emit_D3D_LOGP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_LOGP

static void emit_D3D_CND(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_CND

static void emit_D3D_DEF(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_DEF

static void emit_D3D_TEXREG2RGB(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXREG2RGB

static void emit_D3D_TEXDP3TEX(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXDP3TEX

static void emit_D3D_TEXM3X2DEPTH(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXM3X2DEPTH

static void emit_D3D_TEXDP3(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXDP3

static void emit_D3D_TEXM3X3(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXM3X3

static void emit_D3D_TEXDEPTH(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXDEPTH

static void emit_D3D_CMP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_CMP

static void emit_D3D_BEM(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_BEM

static void emit_D3D_DP2ADD(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_DP2ADD

static void emit_D3D_DSX(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_DSX

static void emit_D3D_DSY(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_DSY

static void emit_D3D_TEXLDD(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXLDD

static void emit_D3D_SETP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_SETP

static void emit_D3D_TEXLDL(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_TEXLDL

static void emit_D3D_BREAKP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_D3D_BREAKP

static void emit_D3D_RESERVED(D3D2GLSL_context *ctx)
{
    // do nothing; fails in the state machine.
} // emit_D3D_RESERVED

#endif  // SUPPORT_PROFILE_D3D



#if !SUPPORT_PROFILE_GLSL
#define PROFILE_EMITTER_GLSL(op)
#else
#undef AT_LEAST_ONE_PROFILE
#define AT_LEAST_ONE_PROFILE 1
#define PROFILE_EMITTER_GLSL(op) emit_GLSL_##op,

static void emit_GLSL_start(D3D2GLSL_context *ctx, uint32 shadertype,
                            uint32 major, uint32 minor)
{
    output_line(ctx, "// %s shader, version %u.%u",
                (shadertype == 0xFFFF) ? "Pixel" :
                (shadertype == 0xFFFE) ? "Vertex" :
                "Unknown", major, minor);
    output_line(ctx, "void main() {");
} // emit_GLSL_start

static void emit_GLSL_end(D3D2GLSL_context *ctx)
{
    output_line(ctx, "}");
} // emit_GLSL_end

static void emit_GLSL_comment(D3D2GLSL_context *ctx, const char *str)
{
    output_line(ctx, "// %s", str);
} // emit_GLSL_comment

static void emit_GLSL_NOP(D3D2GLSL_context *ctx)
{
    // no-op is a no-op.  :)
} // emit_GLSL_NOP

static void emit_GLSL_MOV(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_MOV

static void emit_GLSL_ADD(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_ADD

static void emit_GLSL_SUB(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_SUB

static void emit_GLSL_MAD(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_MAD

static void emit_GLSL_MUL(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_MUL

static void emit_GLSL_RCP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_RCP

static void emit_GLSL_RSQ(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_RSQ

static void emit_GLSL_DP3(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DP3

static void emit_GLSL_DP4(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DP4

static void emit_GLSL_MIN(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_MIN

static void emit_GLSL_MAX(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_MAX

static void emit_GLSL_SLT(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_SLT

static void emit_GLSL_SGE(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_SGE

static void emit_GLSL_EXP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_EXP

static void emit_GLSL_LOG(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_LOG

static void emit_GLSL_LIT(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_LIT

static void emit_GLSL_DST(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DST

static void emit_GLSL_LRP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_LRP

static void emit_GLSL_FRC(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_FRC

static void emit_GLSL_M4X4(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_M4X4

static void emit_GLSL_M4X3(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_M4X3

static void emit_GLSL_M3X4(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_M3X4

static void emit_GLSL_M3X3(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_M3X3

static void emit_GLSL_M3X2(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_M3X2

static void emit_GLSL_CALL(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_CALL

static void emit_GLSL_CALLNZ(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_CALLNZ

static void emit_GLSL_LOOP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_LOOP

static void emit_GLSL_RET(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_RET

static void emit_GLSL_ENDLOOP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_ENDLOOP

static void emit_GLSL_LABEL(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_LABEL

static void emit_GLSL_DCL(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DCL

static void emit_GLSL_POW(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_POW

static void emit_GLSL_CRS(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_CRS

static void emit_GLSL_SGN(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_SGN

static void emit_GLSL_ABS(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_ABS

static void emit_GLSL_NRM(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_NRM

static void emit_GLSL_SINCOS(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_SINCOS

static void emit_GLSL_REP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_REP

static void emit_GLSL_ENDREP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_ENDREP

static void emit_GLSL_IF(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_IF

static void emit_GLSL_IFC(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_IFC

static void emit_GLSL_ELSE(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_ELSE

static void emit_GLSL_ENDIF(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_ENDIF

static void emit_GLSL_BREAK(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_BREAK

static void emit_GLSL_BREAKC(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_BREAKC

static void emit_GLSL_MOVA(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_MOVA

static void emit_GLSL_DEFB(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DEFB

static void emit_GLSL_DEFI(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DEFI

static void emit_GLSL_TEXCOORD(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXCOORD

static void emit_GLSL_TEXKILL(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXKILL

static void emit_GLSL_TEX(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEX

static void emit_GLSL_TEXBEM(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXBEM

static void emit_GLSL_TEXBEML(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXBEML

static void emit_GLSL_TEXREG2AR(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXREG2AR

static void emit_GLSL_TEXREG2GB(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXREG2GB

static void emit_GLSL_TEXM3X2PAD(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X2PAD

static void emit_GLSL_TEXM3X2TEX(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X2TEX

static void emit_GLSL_TEXM3X3PAD(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X3PAD

static void emit_GLSL_TEXM3X3TEX(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X3TEX

static void emit_GLSL_TEXM3X3SPEC(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X3SPEC

static void emit_GLSL_TEXM3X3VSPEC(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X3VSPEC

static void emit_GLSL_EXPP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_EXPP

static void emit_GLSL_LOGP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_LOGP

static void emit_GLSL_CND(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_CND

static void emit_GLSL_DEF(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DEF

static void emit_GLSL_TEXREG2RGB(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXREG2RGB

static void emit_GLSL_TEXDP3TEX(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXDP3TEX

static void emit_GLSL_TEXM3X2DEPTH(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X2DEPTH

static void emit_GLSL_TEXDP3(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXDP3

static void emit_GLSL_TEXM3X3(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXM3X3

static void emit_GLSL_TEXDEPTH(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXDEPTH

static void emit_GLSL_CMP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_CMP

static void emit_GLSL_BEM(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_BEM

static void emit_GLSL_DP2ADD(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DP2ADD

static void emit_GLSL_DSX(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DSX

static void emit_GLSL_DSY(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_DSY

static void emit_GLSL_TEXLDD(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXLDD

static void emit_GLSL_SETP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_SETP

static void emit_GLSL_TEXLDL(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_TEXLDL

static void emit_GLSL_BREAKP(D3D2GLSL_context *ctx)
{
    fail(ctx, "unimplemented.");  // !!! FIXME
} // emit_GLSL_BREAKP

static void emit_GLSL_RESERVED(D3D2GLSL_context *ctx)
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

static int state_RESERVED(D3D2GLSL_context *ctx)
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

static int parse_instruction_token(D3D2GLSL_context *ctx)
{
    const uint32 token = SWAP32(*(ctx->tokens));
    const uint32 opcode = (token & 0xFFFF);
    const uint32 controls = ((token >> 16) & 0xFF);
    const uint32 insttoks = ((token >> 24) & 0x0F);
    const int coissue = (token & 0x40000000) ? 1 : 0;
    const int predicated = (token & 0x10000000) ? 1 : 0;
    const Instruction *instruction = &instructions[opcode];
    const emit_function emitter = instruction->emitter[ctx->profileidx];
    int retval = FAIL;

    if ( opcode >= (sizeof (instructions) / sizeof (instructions[0])) )
        return 0;  // not an instruction token, or just not handled here.

    if ((token & 0x80000000) != 0)
        return fail(ctx, "instruction token high bit must be zero.");  // so says msdn.

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

    return retval;
} // parse_instruction_token


static int parse_version_token(D3D2GLSL_context *ctx)
{
    if (ctx->tokencount == 0)
        return fail(ctx, "Expected version token, got none at all.");

    const uint32 token = SWAP32(*(ctx->tokens));
    const uint32 shadertype = ((token >> 16) & 0xFFFF);
    const uint32 major = ((token >> 8) & 0xFF);
    const uint32 minor = (token & 0xFF);

    // 0xFFFF == pixel shader, 0xFFFE == vertex shader
    if ((shadertype != 0xFFFF) && (shadertype != 0xFFFE))
        return fail(ctx, "geometry shader? Unsupported at the moment.");

    ctx->profile->start_emitter(ctx, shadertype, major, minor);
    return 1;  // ate one token.
} // parse_version_token


static int parse_comment_token(D3D2GLSL_context *ctx)
{
    const uint32 token = SWAP32(*(ctx->tokens));
    if ((token & 0xFFFF) != 0xFFFE)
        return 0;  // not a comment token.
    else if ((token & 0x80000000) != 0)
        return fail(ctx, "comment token high bit must be zero.");  // so says msdn.
    else
    {
        const uint32 commenttoks = ((token >> 16) & 0xFFFF);
        const uint32 commentlen = commenttoks * sizeof (uint32);

        // !!! FIXME: I'd rather not malloc() here.
        char *str = (char *) malloc(commentlen + 1);
        memcpy(str, (const char *) (ctx->tokens+1), commentlen);
        str[commentlen] = '\0';
        ctx->profile->comment_emitter(ctx, str);
        free(str);

        return commenttoks + 1;  // comment data plus the initial token.
    } // else

    // shouldn't hit this.
    return failf(ctx, "Logic error at %s:%d", __FILE__, __LINE__);
} // parse_comment_token


static int parse_end_token(D3D2GLSL_context *ctx)
{
    if (SWAP32(*(ctx->tokens)) != 0x0000FFFF)   // end token always 0x0000FFFF.
        return 0;  // not us, eat no tokens.

    if (ctx->tokencount != 1)  // we _must_ be last. If not: fail.
        return fail(ctx, "end token before end of stream");

    ctx->profile->end_emitter(ctx);

    return END_OF_STREAM;
} // parse_end_token


static int parse_phase_token(D3D2GLSL_context *ctx)
{
    if (SWAP32(*(ctx->tokens)) != 0x0000FFFD) // phase token always 0x0000FFFD.
        return 0;  // not us, eat no tokens.
    return fail(ctx, "not sure what this thing is yet.");
} // parse_phase_token


static int parse_token(D3D2GLSL_context *ctx)
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


// API entry point...

int D3D2GLSL_parse(const char *profile, const unsigned char *tokenbuf,
                   const unsigned int bufsize)
{
    D3D2GLSL_context ctx;
    int rc = 0;
    int i = 0;

    memset(&ctx, '\0', sizeof (D3D2GLSL_context));
    ctx.tokens = (const uint32 *) tokenbuf;
    ctx.tokencount = bufsize / sizeof (uint32);
    for (i = 0; i < STATICARRAYLEN(profiles); i++)
    {
        const char *name = profiles[i].name;
        if (strcmp(name, profile) == 0)
        {
            ctx.profile = &profiles[i];
            ctx.profileidx = i;
            break;
        } // if
    } // for

    if (ctx.profile == NULL)
        failf(&ctx, "Profile '%s' is unknown or unsupported", profile);
    else
    {
        rc = parse_version_token(&ctx);

        // parse out the rest of the tokens after the version token...
        while (rc > 0)
        {
            ctx.tokens += rc;
            ctx.tokencount -= rc;
            rc = parse_token(&ctx);
        } // while
    } // else

    // !!! FIXME: temp
    if (ctx.output != NULL)
        printf("%s\n", ctx.output);

    if (ctx.failstr != NULL)
        printf("FAIL: %s\n", ctx.failstr);

    free(ctx.failstr);
    free(ctx.output);

    return (rc == END_OF_STREAM);
} // D3D2GLSL_parse

// end of parse.c ...

