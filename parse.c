#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define END_OF_STREAM (-2)
//#define FAIL(x) (-1)
static int FAIL(const char *reason)
{
    printf("%s FAIL.\n", reason);
    return -1;
} // FAIL


typedef unsigned int uint;  // this is a printf() helper. don't use for code.
typedef uint8_t uint8;
typedef uint32_t uint32;

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

typedef int (*parse_instruction_function)(const uint32 *argtokens);

static int parse_NOP(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_NOP

static int parse_MOV(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_MOV

static int parse_ADD(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_ADD

static int parse_SUB(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_SUB

static int parse_MAD(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_MAD

static int parse_MUL(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_MUL

static int parse_RCP(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_RCP

static int parse_RSQ(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_RSQ

static int parse_DP3(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DP3

static int parse_DP4(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DP4

static int parse_MIN(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_MIN

static int parse_MAX(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_MAX

static int parse_SLT(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_SLT

static int parse_SGE(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_SGE

static int parse_EXP(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_EXP

static int parse_LOG(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_LOG

static int parse_LIT(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_LIT

static int parse_DST(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DST

static int parse_LRP(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_LRP

static int parse_FRC(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_FRC

static int parse_M4X4(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_M4X4

static int parse_M4X3(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_M4X3

static int parse_M3X4(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_M3X4

static int parse_M3X3(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_M3X3

static int parse_M3X2(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_M3X2

static int parse_CALL(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_CALL

static int parse_CALLNZ(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_CALLNZ

static int parse_LOOP(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_LOOP

static int parse_RET(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_RET

static int parse_ENDLOOP(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_ENDLOOP

static int parse_LABEL(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_LABEL

static int parse_DCL(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DCL

static int parse_POW(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_POW

static int parse_CRS(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_CRS

static int parse_SGN(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_SGN

static int parse_ABS(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_ABS

static int parse_NRM(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_NRM

static int parse_SINCOS(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_SINCOS

static int parse_REP(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_REP

static int parse_ENDREP(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_ENDREP

static int parse_IF(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_IF

static int parse_IFC(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_IFC

static int parse_ELSE(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_ELSE

static int parse_ENDIF(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_ENDIF

static int parse_BREAK(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_BREAK

static int parse_BREAKC(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_BREAKC

static int parse_MOVA(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_MOVA

static int parse_DEFB(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DEFB

static int parse_DEFI(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DEFI

static int parse_TEXCOORD(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXCOORD

static int parse_TEXKILL(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXKILL

static int parse_TEX(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEX

static int parse_TEXBEM(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXBEM

static int parse_TEXBEML(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXBEML

static int parse_TEXREG2AR(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXREG2AR

static int parse_TEXREG2GB(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXREG2GB

static int parse_TEXM3X2PAD(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X2PAD

static int parse_TEXM3X2TEX(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X2TEX

static int parse_TEXM3X3PAD(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X3PAD

static int parse_TEXM3X3TEX(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X3TEX

static int parse_RESERVED0(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_RESERVED0

static int parse_TEXM3X3SPEC(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X3SPEC

static int parse_TEXM3X3VSPEC(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X3VSPEC

static int parse_EXPP(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_EXPP

static int parse_LOGP(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_LOGP

static int parse_CND(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_CND

static int parse_DEF(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DEF

static int parse_TEXREG2RGB(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXREG2RGB

static int parse_TEXDP3TEX(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXDP3TEX

static int parse_TEXM3X2DEPTH(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X2DEPTH

static int parse_TEXDP3(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXDP3

static int parse_TEXM3X3(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXM3X3

static int parse_TEXDEPTH(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXDEPTH

static int parse_CMP(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_CMP

static int parse_BEM(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_BEM

static int parse_DP2ADD(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DP2ADD

static int parse_DSX(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DSX

static int parse_DSY(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_DSY

static int parse_TEXLDD(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXLDD

static int parse_SETP(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_SETP

static int parse_TEXLDL(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_TEXLDL

static int parse_BREAKP(const uint32 *argtokens)
{
    return FAIL("unimplemented.");  // !!! FIXME
} // parse_BREAKP

static int parse_RESERVED(const uint32 *argtokens)
{
    return FAIL("Tried to use RESERVED opcode.");
} // parse_BREAKP




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


static int parse_instruction_token(const uint32 *tokens, const uint32 tokencount)
{
    const uint32 token = SWAP32(*tokens);
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
        else if (tokencount <= instruction->arg_tokens)
            return FAIL("not enough tokens left in shader for instruction.");
    } // if

return insttoks + 1;
//    return instruction->parser(tokens + 1);
} // parse_instruction_token


static int parse_version_token(const uint32 *tokens, const uint32 tokencount)
{
    if (tokencount == 0)
        return FAIL("Expected version token, got none at all.");

    const uint32 token = SWAP32(*tokens);
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


static int parse_comment_token(const uint32 *tokens, const uint32 tokencount)
{
    const uint32 token = SWAP32(*tokens);
    if ((token & 0xFFFF) != 0xFFFE)
        return 0;  // not a comment token.
    else if ((token & 0x80000000) != 0)
        return FAIL("comment token high bit must be zero.");  // so says msdn.

    const uint32 commenttoks = ((token >> 16) & 0xFFFF);
    const uint32 commentlen = commenttoks * sizeof (uint32);
    printf("Comment (%u tokens, %u bytes): ",
            (uint) commenttoks, (uint) commentlen);

    uint32 i = 0;
    const char *comment = (const char *) (tokens+1);
    while (i < commentlen)
        fputc(comment[i++], stdout);

    printf("\n");

    return commenttoks + 1;  // comment data plus the initial token.
} // parse_comment_token


static int parse_end_token(const uint32 *tokens, const uint32 tokencount)
{
    if (SWAP32(*tokens) != 0x0000FFFF)      // end token is always 0x0000FFFF.
        return 0;  // not us, eat no tokens.

    printf("END\n");

    if (tokencount != 1)  // we _must_ be last. If not: FAIL.
        FAIL("end token before end of stream");

    return END_OF_STREAM;
} // parse_end_token


static int parse_phase_token(const uint32 *tokens, const uint32 tokencount)
{
    if (SWAP32(*tokens) != 0x0000FFFD)    // phase token is always 0x0000FFFD.
        return 0;  // not us, eat no tokens.
    return FAIL("not sure what this thing is yet.");
} // parse_phase_token


static int parse_token(const uint32 *tokens, const uint32 tokencount)
{
    int rc = 0;

    if (tokencount == 0)
        return FAIL("unexpected end of shader.");

    if ((rc = parse_comment_token(tokens, tokencount)) != 0)
        return rc;

    if ((rc = parse_end_token(tokens, tokencount)) != 0)
        return rc;

    if ((rc = parse_phase_token(tokens, tokencount)) != 0)
        return rc;

    if ((rc = parse_instruction_token(tokens, tokencount)) != 0)
        return rc;

    return FAIL("unknown token");
} // parse_token


int D3D2GLSL_parse(const uint8 *tokenbuf, const uint32 bufsize)
{
    const uint32 *tokens = (const uint32 *) tokenbuf;
    uint32 tokencount = bufsize / sizeof (uint32);
    int rc = parse_version_token(tokens, tokencount);

    // parse out the rest of the tokens after the version token...
    while (rc > 0)
    {
        tokens += rc;
        tokencount -= rc;
        rc = parse_token(tokens, tokencount);
    } // while

    return (rc == END_OF_STREAM);
} // D3D2GLSL_parse


int main(int argc, char **argv)
{
    if (argv[1] != NULL)
    {
        FILE *io = fopen(argv[1], "rb");
        if (io != NULL)
        {
            uint8 *buf = (uint8 *) malloc(1000000);
            int rc = fread(buf, 1, 1000000, io);
            fclose(io);
            D3D2GLSL_parse(buf, rc);
            free(buf);
        } // if
    } // if

    return 0;
} // main

// end of parse.c ...

