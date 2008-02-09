#include <stdio.h>
#include <stdlib.h>

#define SWAP32(x) (x)
typedef unsigned int uint;
typedef unsigned char uint8;
typedef unsigned int uint32;

// This enum complements of Filip Navara's public domain dev-cpp d3d9 header.
typedef enum
{
    OPCODE_NOP = 0,
    OPCODE_MOV = 1,
    OPCODE_ADD = 2,
    OPCODE_SUB = 3,
    OPCODE_MAD = 4,
    OPCODE_MUL = 5,
    OPCODE_RCP = 6,
    OPCODE_RSQ = 7,
    OPCODE_DP3 = 8,
    OPCODE_DP4 = 9,
    OPCODE_MIN = 10,
    OPCODE_MAX = 11,
    OPCODE_SLT = 12,
    OPCODE_SGE = 13,
    OPCODE_EXP = 14,
    OPCODE_LOG = 15,
    OPCODE_LIT = 16,
    OPCODE_DST = 17,
    OPCODE_LRP = 18,
    OPCODE_FRC = 19,
    OPCODE_M4x4 = 20,
    OPCODE_M4x3 = 21,
    OPCODE_M3x4 = 22,
    OPCODE_M3x3 = 23,
    OPCODE_M3x2 = 24,
    OPCODE_CALL = 25,
    OPCODE_CALLNZ = 26,
    OPCODE_LOOP = 27,
    OPCODE_RET = 28,
    OPCODE_ENDLOOP = 29,
    OPCODE_LABEL = 30,
    OPCODE_DCL = 31,
    OPCODE_POW = 32,
    OPCODE_CRS = 33,
    OPCODE_SGN = 34,
    OPCODE_ABS = 35,
    OPCODE_NRM = 36,
    OPCODE_SINCOS = 37,
    OPCODE_REP = 38,
    OPCODE_ENDREP = 39,
    OPCODE_IF = 40,
    OPCODE_IFC = 41,
    OPCODE_ELSE = 42,
    OPCODE_ENDIF = 43,
    OPCODE_BREAK = 44,
    OPCODE_BREAKC = 45,
    OPCODE_MOVA = 46,
    OPCODE_DEFB = 47,
    OPCODE_DEFI = 48,
    OPCODE_TEXCOORD = 64,
    OPCODE_TEXKILL = 65,
    OPCODE_TEX = 66,
    OPCODE_TEXBEM = 67,
    OPCODE_TEXBEML = 68,
    OPCODE_TEXREG2AR = 69,
    OPCODE_TEXREG2GB = 70,
    OPCODE_TEXM3x2PAD = 71,
    OPCODE_TEXM3x2TEX = 72,
    OPCODE_TEXM3x3PAD = 73,
    OPCODE_TEXM3x3TEX = 74,
    OPCODE_RESERVED0 = 75,
    OPCODE_TEXM3x3SPEC = 76,
    OPCODE_TEXM3x3VSPEC = 77,
    OPCODE_EXPP = 78,
    OPCODE_LOGP = 79,
    OPCODE_CND = 80,
    OPCODE_DEF = 81,
    OPCODE_TEXREG2RGB = 82,
    OPCODE_TEXDP3TEX = 83,
    OPCODE_TEXM3x2DEPTH = 84,
    OPCODE_TEXDP3 = 85,
    OPCODE_TEXM3x3 = 86,
    OPCODE_TEXDEPTH = 87,
    OPCODE_CMP = 88,
    OPCODE_BEM = 89,
    OPCODE_DP2ADD = 90,
    OPCODE_DSX = 91,
    OPCODE_DSY = 92,
    OPCODE_TEXLDD = 93,
    OPCODE_SETP = 94,
    OPCODE_TEXLDL = 95,
    OPCODE_BREAKP = 96,
    OPCODE_PHASE = 0xfffd,
} OpcodeVal;


static int parse_version_token(const uint32 *tokens, const uint32 tokencount)
{
    if (tokencount == 0)
        return -1;  // no tokens at all?

    const uint32 token = SWAP32(*tokens);
    const uint32 shadertype = ((token >> 16) & 0xFFFF);
    const uint32 major = ((token >> 8) & 0xFF);
    const uint32 minor = (token & 0xFF);

    if (shadertype == 0xFFFF)
        printf("Pixel shader\n");
    else if (shadertype == 0xFFFE)
        printf("Vertex shader\n");
    else
        return -1;  // geometry shader? Unsupported at the moment. FAIL.

    printf("Version %u.%u\n", (uint) major, (uint) minor);

    return 1;  // ate one token.
} // parse_version_token


static int parse_comment_token(const uint32 *tokens, const uint32 tokencount)
{
    const uint32 token = SWAP32(*tokens);
    if ((token & 0xFFFF) != 0xFFFE)
        return 0;  // not a comment token.
    else if ((token & 0x80000000) != 0)
        return -1;  // msdn docs say high bit must be zero. FAIL.

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

    // we _must_ be last. If so, eat the token. Otherwise: FAIL.
    return (tokencount == 1) ? 1 : -1;
} // parse_end_token


static int parse_instruction_token(const uint32 *tokens, const uint32 tokencount)
{
    const uint32 token = SWAP32(*tokens);
    const uint32 opcode = (token & 0xFFFF);
    const uint32 controls = ((token >> 16) & 0xFF);
    const uint32 insttoks = ((token >> 24) & 0x0F);
    const int coissue = (token & 0x40000000) ? 1 : 0;
    const int predicated = (token & 0x10000000) ? 1 : 0;

    if ((token & 0x80000000) != 0)
        return -1;  // msdn docs say high bit must be zero. FAIL.

    #define PARSE_OP(op) printf("OPCODE %s\n", #op);
    //case OPCODE_##op:
    //    parse_op_##op(tokens+1, opcode, controls, insttoks, coissue, predicated);
    //    break;

    PARSE_OP(NOP);
    PARSE_OP(MOV);
    PARSE_OP(ADD);
    PARSE_OP(SUB);
    PARSE_OP(MAD);
    PARSE_OP(MUL);
    PARSE_OP(RCP);
    PARSE_OP(RSQ);
    PARSE_OP(DP3);
    PARSE_OP(DP4);
    PARSE_OP(MIN);
    PARSE_OP(MAX);
    PARSE_OP(SLT);
    PARSE_OP(SGE);
    PARSE_OP(EXP);
    PARSE_OP(LOG);
    PARSE_OP(LIT);
    PARSE_OP(DST);
    PARSE_OP(LRP);
    PARSE_OP(FRC);
    PARSE_OP(M4x4);
    PARSE_OP(M4x3);
    PARSE_OP(M3x4);
    PARSE_OP(M3x3);
    PARSE_OP(M3x2);
    PARSE_OP(CALL);
    PARSE_OP(CALLNZ);
    PARSE_OP(LOOP);
    PARSE_OP(RET);
    PARSE_OP(ENDLOOP);
    PARSE_OP(LABEL);
    PARSE_OP(DCL);
    PARSE_OP(POW);
    PARSE_OP(CRS);
    PARSE_OP(SGN);
    PARSE_OP(ABS);
    PARSE_OP(NRM);
    PARSE_OP(SINCOS);
    PARSE_OP(REP);
    PARSE_OP(ENDREP);
    PARSE_OP(IF);
    PARSE_OP(IFC);
    PARSE_OP(ELSE);
    PARSE_OP(ENDIF);
    PARSE_OP(BREAK);
    PARSE_OP(BREAKC);
    PARSE_OP(MOVA);
    PARSE_OP(DEFB);
    PARSE_OP(DEFI);
    PARSE_OP(TEXCOORD);
    PARSE_OP(TEXKILL);
    PARSE_OP(TEX);
    PARSE_OP(TEXBEM);
    PARSE_OP(TEXBEML);
    PARSE_OP(TEXREG2AR);
    PARSE_OP(TEXREG2GB);
    PARSE_OP(TEXM3x2PAD);
    PARSE_OP(TEXM3x2TEX);
    PARSE_OP(TEXM3x3PAD);
    PARSE_OP(TEXM3x3TEX);
    PARSE_OP(RESERVED0);
    PARSE_OP(TEXM3x3SPEC);
    PARSE_OP(TEXM3x3VSPEC);
    PARSE_OP(EXPP);
    PARSE_OP(LOGP);
    PARSE_OP(CND);
    PARSE_OP(DEF);
    PARSE_OP(TEXREG2RGB);
    PARSE_OP(TEXDP3TEX);
    PARSE_OP(TEXM3x2DEPTH);
    PARSE_OP(TEXDP3);
    PARSE_OP(TEXM3x3);
    PARSE_OP(TEXDEPTH);
    PARSE_OP(CMP);
    PARSE_OP(BEM);
    PARSE_OP(DP2ADD);
    PARSE_OP(DSX);
    PARSE_OP(DSY);
    PARSE_OP(TEXLDD);
    PARSE_OP(SETP);
    PARSE_OP(TEXLDL);
    PARSE_OP(BREAKP);
    PARSE_OP(PHASE);

    #undef PARSE_OP
} // parse_instruction_token


static int parse_token(const uint32 *tokens, const uint32 tokencount)
{
    int retval = -1;
    int rc = 0;

    if (tokencount == 0)
        return -1;  // shouldn't happen, but just in case...

    if ((rc = parse_comment_token(tokens, tokencount)) != 0)
        return rc;

    if ((rc = parse_end_token(tokens, tokencount)) != 0)
        return rc;

    if ((rc = parse_instruction_token(tokens, tokencount)) != 0)
        return rc;

    return -1;  // nothing handled this? FAIL.
} // parse_token


int D3D2GLSL_parse(const uint8 *tokenbuf, const uint32 bufsize)
{
    const uint32 *tokens = (const uint32 *) tokenbuf;
    uint32 tokencount = bufsize / sizeof (uint32);
    int rc = parse_version_token(tokens, tokencount);

    // parse out the rest of the tokens after the version token...
    while ((rc > 0) && (tokencount > 0))
    {
        tokens += rc;
        tokencount -= rc;
        rc = parse_token(tokens, tokencount);
    } // while

    return ((rc <= 0) || (tokencount > 0)) ? 0 : 1;
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

