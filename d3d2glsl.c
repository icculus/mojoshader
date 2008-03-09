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

#include "d3d2glsl.h"

typedef unsigned int uint;  // this is a printf() helper. don't use for code.
typedef uint8_t uint8;
typedef uint32_t uint32;

// Context...this is state that changes as we parse through a shader...
typedef struct D3D2GLSL_context
{
    const uint32 *tokens;
    uint32 tokencount;
    char buffers[128][5];  // scratch buffers we cycle through.
    int bufidx;  // current scratch buffer.
} D3D2GLSL_context;


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


// Special-case return values from the parsing pipeline...

#define END_OF_STREAM (-2)
//#define FAIL(x) (-1)
static int FAIL(const char *reason)
{
    printf("%s FAIL.\n", reason);
    return -1;
} // FAIL


// one function for each opcode...

typedef int (*parse_instruction_function)(D3D2GLSL_context *ctx);

static int parse_NOP(D3D2GLSL_context *ctx)
{
    return 1;  // no-op is a no-op.   :)
} // parse_NOP

static int parse_MOV(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_MOV

static int parse_ADD(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_ADD

static int parse_SUB(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_SUB

static int parse_MAD(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_MAD

static int parse_MUL(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_MUL

static int parse_RCP(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_RCP

static int parse_RSQ(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_RSQ

static int parse_DP3(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DP3

static int parse_DP4(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DP4

static int parse_MIN(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_MIN

static int parse_MAX(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_MAX

static int parse_SLT(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_SLT

static int parse_SGE(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_SGE

static int parse_EXP(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_EXP

static int parse_LOG(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_LOG

static int parse_LIT(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_LIT

static int parse_DST(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DST

static int parse_LRP(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_LRP

static int parse_FRC(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_FRC

static int parse_M4X4(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_M4X4

static int parse_M4X3(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_M4X3

static int parse_M3X4(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_M3X4

static int parse_M3X3(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_M3X3

static int parse_M3X2(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_M3X2

static int parse_CALL(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_CALL

static int parse_CALLNZ(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_CALLNZ

static int parse_LOOP(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_LOOP

static int parse_RET(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_RET

static int parse_ENDLOOP(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_ENDLOOP

static int parse_LABEL(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_LABEL

static int parse_DCL(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DCL

static int parse_POW(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_POW

static int parse_CRS(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_CRS

static int parse_SGN(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_SGN

static int parse_ABS(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_ABS

static int parse_NRM(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_NRM

static int parse_SINCOS(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_SINCOS

static int parse_REP(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_REP

static int parse_ENDREP(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_ENDREP

static int parse_IF(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_IF

static int parse_IFC(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_IFC

static int parse_ELSE(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_ELSE

static int parse_ENDIF(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_ENDIF

static int parse_BREAK(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_BREAK

static int parse_BREAKC(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_BREAKC

static int parse_MOVA(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_MOVA

static int parse_DEFB(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DEFB

static int parse_DEFI(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DEFI

static int parse_TEXCOORD(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXCOORD

static int parse_TEXKILL(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXKILL

static int parse_TEX(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEX

static int parse_TEXBEM(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXBEM

static int parse_TEXBEML(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXBEML

static int parse_TEXREG2AR(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXREG2AR

static int parse_TEXREG2GB(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXREG2GB

static int parse_TEXM3X2PAD(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X2PAD

static int parse_TEXM3X2TEX(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X2TEX

static int parse_TEXM3X3PAD(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X3PAD

static int parse_TEXM3X3TEX(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X3TEX

static int parse_RESERVED0(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_RESERVED0

static int parse_TEXM3X3SPEC(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X3SPEC

static int parse_TEXM3X3VSPEC(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X3VSPEC

static int parse_EXPP(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_EXPP

static int parse_LOGP(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_LOGP

static int parse_CND(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_CND

static int parse_DEF(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DEF

static int parse_TEXREG2RGB(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXREG2RGB

static int parse_TEXDP3TEX(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXDP3TEX

static int parse_TEXM3X2DEPTH(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X2DEPTH

static int parse_TEXDP3(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXDP3

static int parse_TEXM3X3(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X3

static int parse_TEXDEPTH(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXDEPTH

static int parse_CMP(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_CMP

static int parse_BEM(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_BEM

static int parse_DP2ADD(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DP2ADD

static int parse_DSX(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DSX

static int parse_DSY(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DSY

static int parse_TEXLDD(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXLDD

static int parse_SETP(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_SETP

static int parse_TEXLDL(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXLDL

static int parse_BREAKP(D3D2GLSL_context *ctx)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_BREAKP

static int parse_RESERVED(D3D2GLSL_context *ctx)
{
    return FAIL("Tried to use RESERVED opcode.");
} // parse_BREAKP



// Lookup table for instruction opcodes...

typedef struct
{
    const char *opcode_string;
    int arg_tokens;
    //uint32 shader_requirements;
    parse_instruction_function parser;
} Instruction;

// These have to be in the right order! This array is indexed by the value
//  of the instruction token.
static Instruction instructions[] = {
    #define INSTRUCTION(op, args) { #op, args, parse_##op }
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
    INSTRUCTION(RESERVED, 0),
    INSTRUCTION(RESERVED, 0),
    INSTRUCTION(RESERVED, 0),
    INSTRUCTION(RESERVED, 0),
    INSTRUCTION(RESERVED, 0),
    INSTRUCTION(RESERVED, 0),
    INSTRUCTION(RESERVED, 0),
    INSTRUCTION(RESERVED, 0),
    INSTRUCTION(RESERVED, 0),
    INSTRUCTION(RESERVED, 0),
    INSTRUCTION(RESERVED, 0),
    INSTRUCTION(RESERVED, 0),
    INSTRUCTION(RESERVED, 0),
    INSTRUCTION(RESERVED, 0),
    INSTRUCTION(RESERVED, 0),
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
    INSTRUCTION(RESERVED, 0),
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

    if ( opcode >= (sizeof (instructions) / sizeof (instructions[0])) )
        return 0;  // not an instruction token, or just not handled here.

    if ((token & 0x80000000) != 0)
        return FAIL("instruction token high bit must be zero.");  // so says msdn.

    printf("%s\n", instruction->opcode_string);

    if (instruction->arg_tokens >= 0)
    {
        if (instruction->arg_tokens != insttoks)
            return FAIL("unexpected number of tokens for instruction.");
        else if (ctx->tokencount <= instruction->arg_tokens)
            return FAIL("not enough tokens left in shader for instruction.");
    } // if

return insttoks + 1;
//    return instruction->parser(ctx, tokens + 1);
} // parse_instruction_token


static int parse_version_token(D3D2GLSL_context *ctx)
{
    if (ctx->tokencount == 0)
        return FAIL("Expected version token, got none at all.");

    const uint32 token = SWAP32(*(ctx->tokens));
    const uint32 shadertype = ((token >> 16) & 0xFFFF);
    const uint32 major = ((token >> 8) & 0xFF);
    const uint32 minor = (token & 0xFF);

    if (shadertype == 0xFFFF)
        printf("Pixel shader\n");
    else if (shadertype == 0xFFFE)
        printf("Vertex shader\n");
    else
        return FAIL("geometry shader? Unsupported at the moment.");

    printf("Version %u.%u\n", (uint) major, (uint) minor);

    return 1;  // ate one token.
} // parse_version_token


static int parse_comment_token(D3D2GLSL_context *ctx)
{
    const uint32 token = SWAP32(*(ctx->tokens));
    if ((token & 0xFFFF) != 0xFFFE)
        return 0;  // not a comment token.
    else if ((token & 0x80000000) != 0)
        return FAIL("comment token high bit must be zero.");  // so says msdn.

    const uint32 commenttoks = ((token >> 16) & 0xFFFF);
    const uint32 commentlen = commenttoks * sizeof (uint32);
    printf("Comment (%u tokens, %u bytes): ",
            (uint) commenttoks, (uint) commentlen);

    uint32 i = 0;
    const char *comment = (const char *) (ctx->tokens+1);
    while (i < commentlen)
        fputc(comment[i++], stdout);

    printf("\n");

    return commenttoks + 1;  // comment data plus the initial token.
} // parse_comment_token


static int parse_end_token(D3D2GLSL_context *ctx)
{
    if (SWAP32(*(ctx->tokens)) != 0x0000FFFF)   // end token always 0x0000FFFF.
        return 0;  // not us, eat no tokens.

    printf("END\n");

    if (ctx->tokencount != 1)  // we _must_ be last. If not: FAIL.
        return FAIL("end token before end of stream");

    return END_OF_STREAM;
} // parse_end_token


static int parse_phase_token(D3D2GLSL_context *ctx)
{
    if (SWAP32(*(ctx->tokens)) != 0x0000FFFD) // phase token always 0x0000FFFD.
        return 0;  // not us, eat no tokens.
    return FAIL("not sure what this thing is yet.");
} // parse_phase_token


static int parse_token(D3D2GLSL_context *ctx)
{
    int rc = 0;

    if (ctx->tokencount == 0)
        return FAIL("unexpected end of shader.");

    if ((rc = parse_comment_token(ctx)) != 0)
        return rc;

    if ((rc = parse_end_token(ctx)) != 0)
        return rc;

    if ((rc = parse_phase_token(ctx)) != 0)
        return rc;

    if ((rc = parse_instruction_token(ctx)) != 0)
        return rc;

    return FAIL("unknown token");
} // parse_token


// API entry point...

int D3D2GLSL_parse(const unsigned char *tokenbuf, const unsigned int bufsize)
{
    D3D2GLSL_context ctx;
    int rc = 0;

    memset(&ctx, '\0', sizeof (D3D2GLSL_context));
    ctx.tokens = (const uint32 *) tokenbuf;
    ctx.tokencount = bufsize / sizeof (uint32);
    rc = parse_version_token(&ctx);

    // parse out the rest of the tokens after the version token...
    while (rc > 0)
    {
        ctx.tokens += rc;
        ctx.tokencount -= rc;
        rc = parse_token(&ctx);
    } // while

    return (rc == END_OF_STREAM);
} // D3D2GLSL_parse

// end of parse.c ...

