#ifndef _INCLUDE_MOJOSHADER_INTERNAL_H_
#define _INCLUDE_MOJOSHADER_INTERNAL_H_

#ifndef __MOJOSHADER_INTERNAL__
#error Do not include this header from your applications.
#endif

// Shader bytecode format is described at MSDN:
//  http://msdn2.microsoft.com/en-us/library/ms800307.aspx

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

#include "mojoshader.h"

#if (defined(__APPLE__) && defined(__MACH__))
#define PLATFORM_MACOSX 1
#endif

// This is the highest shader version we currently support.

#define MAX_SHADER_MAJOR 3
#define MAX_SHADER_MINOR 255  // vs_3_sw


// If SUPPORT_PROFILE_* isn't defined, we assume an implicit desire to support.
//  You get all the profiles unless you go out of your way to disable them.

#ifndef SUPPORT_PROFILE_D3D
#define SUPPORT_PROFILE_D3D 1
#endif

#ifndef SUPPORT_PROFILE_PASSTHROUGH
#define SUPPORT_PROFILE_BYTECODE 1
#endif

#ifndef SUPPORT_PROFILE_GLSL
#define SUPPORT_PROFILE_GLSL 1
#endif

#ifndef SUPPORT_PROFILE_ARB1
#define SUPPORT_PROFILE_ARB1 1
#endif


// Get basic wankery out of the way here...

#ifdef _WINDOWS
#define ENDLINE_STR "\r\n"
#else
#define ENDLINE_STR "\n";
#endif

typedef unsigned int uint;  // this is a printf() helper. don't use for code.

#ifdef _MSC_VER
#include <malloc.h>
#define snprintf _snprintf
#define strcasecmp stricmp
typedef unsigned __int8 uint8;
typedef unsigned __int16 uint16;
typedef unsigned __int32 uint32;
typedef __int32 int32;
// Warning Level 4 considered harmful.  :)
#pragma warning(disable: 4100)  // "unreferenced formal parameter"
#pragma warning(disable: 4389)  // "signed/unsigned mismatch"
#else
#include <stdint.h>
typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
typedef int32_t int32;
#endif

#ifdef __GNUC__
#define ISPRINTF(x,y) __attribute__((format (printf, x, y)))
#else
#define ISPRINTF(x,y)
#endif

#define STATICARRAYLEN(x) ( (sizeof ((x))) / (sizeof ((x)[0])) )

// Special-case return values from the parsing pipeline...
#define FAIL (-1)
#define NOFAIL (-2)
#define END_OF_STREAM (-3)


// Byteswap magic...

#if ((defined __GNUC__) && (defined __POWERPC__))
    static inline uint32 SWAP32(uint32 x)
    {
        __asm__ __volatile__("lwbrx %0,0,%1" : "=r" (x) : "r" (&x));
        return x;
    } // SWAP32
    static inline uint16 SWAP16(uint16 x)
    {
        __asm__ __volatile__("lhbrx %0,0,%1" : "=r" (x) : "r" (&x));
        return x;
    } // SWAP16
#elif defined(__POWERPC__)
    static inline uint32 SWAP32(uint32 x)
    {
        return ( (((x) >> 24) & 0x000000FF) | (((x) >>  8) & 0x0000FF00) |
                 (((x) <<  8) & 0x00FF0000) | (((x) << 24) & 0xFF000000) );
    } // SWAP32
    static inline uint16 SWAP16(uint16 x)
    {
        return ( (((x) >> 8) & 0x00FF) | (((x) << 8) & 0xFF00) );
    } // SWAP16
#else
#   define SWAP16(x) (x)
#   define SWAP32(x) (x)
#endif


// we need to reference these by explicit value occasionally...
#define OPCODE_RET 28
#define OPCODE_IFC 41
#define OPCODE_BREAKC 45

// TEXLD becomes a different instruction with these instruction controls.
#define CONTROL_TEXLD  0
#define CONTROL_TEXLDP 1
#define CONTROL_TEXLDB 2

// #define this to force app to supply an allocator, so there's no reference
//  to the C runtime's malloc() and free()...
#if MOJOSHADER_FORCE_ALLOCATOR
#define internal_malloc NULL
#define internal_free NULL
#else
static void *internal_malloc(int bytes, void *d) { return malloc(bytes); }
static void internal_free(void *ptr, void *d) { free(ptr); }
#endif


// result modifiers.
// !!! FIXME: why isn't this an enum?
#define MOD_SATURATE 0x01
#define MOD_PP 0x02
#define MOD_CENTROID 0x04

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

typedef enum
{
    TEXTURE_TYPE_2D = 2,
    TEXTURE_TYPE_CUBE = 3,
    TEXTURE_TYPE_VOLUME = 4,
} TextureType;

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
    int orig_writemask;   // writemask before mojoshader tweaks it.
    int result_mod;
    int result_shift;
    RegisterType regtype;
} DestArgInfo;


static inline int scalar_register(const MOJOSHADER_shaderType shader_type,
                                  const RegisterType regtype, const int regnum)
{
    switch (regtype)
    {
        case REG_TYPE_DEPTHOUT:
        case REG_TYPE_CONSTBOOL:
        case REG_TYPE_LOOP:
            return 1;

        case REG_TYPE_MISCTYPE:
            if ( ((const MiscTypeType) regnum) == MISCTYPE_TYPE_FACE )
                return 1;
            return 0;

        case REG_TYPE_PREDICATE:
            return (shader_type == MOJOSHADER_TYPE_PIXEL) ? 1 : 0;

        default: break;
    } // switch

    return 0;
} // scalar_register

typedef enum
{
    MOJOSHADER_PARSEPHASE_NOTSTARTED,
    MOJOSHADER_PARSEPHASE_WORKING,
    MOJOSHADER_PARSEPHASE_DONE,
} MOJOSHADER_parsePhase;

extern MOJOSHADER_parseData out_of_mem_data;
extern const char *out_of_mem_str;

#endif  // _INCLUDE_MOJOSHADER_INTERNAL_H_


#if MOJOSHADER_DO_INSTRUCTION_TABLE
// These have to be in the right order! Arrays are indexed by the value
//  of the instruction token.

// INSTRUCTION_STATE means this opcode has to update the state machine
//  (we're entering an ELSE block, etc). INSTRUCTION means there's no
//  state, just go straight to the emitters.

// !!! FIXME: Some of these MOJOSHADER_TYPE_ANYs need to have their scope
// !!! FIXME:  reduced to just PIXEL or VERTEX.

INSTRUCTION(NOP, "NOP", 1, NULL, MOJOSHADER_TYPE_ANY)
INSTRUCTION(MOV, "MOV", 1, DS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(ADD, "ADD", 1, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(SUB, "SUB", 1, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(MAD, "MAD", 1, DSSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(MUL, "MUL", 1, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(RCP, "RCP", 1, DS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(RSQ, "RSQ", 1, DS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(DP3, "DP3", 1, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(DP4, "DP4", 1, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(MIN, "MIN", 1, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(MAX, "MAX", 1, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(SLT, "SLT", 1, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(SGE, "SGE", 1, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(EXP, "EXP", 1, DS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(LOG, "LOG", 1, DS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(LIT, "LIT", 3, DS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(DST, "DST", 1, DSS, MOJOSHADER_TYPE_VERTEX)
INSTRUCTION(LRP, "LRP", 2, DSSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(FRC, "FRC", 1, DS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(M4X4, "M4X4", 4, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(M4X3, "M4X3", 3, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(M3X4, "M3X4", 4, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(M3X3, "M3X3", 3, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(M3X2, "M3X2", 2, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(CALL, "CALL", 2, S, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(CALLNZ, "CALLNZ", 3, SS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(LOOP, "LOOP", 3, SS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(RET, "RET", 1, NULL, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(ENDLOOP, "ENDLOOP", 2, NULL, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(LABEL, "LABEL", 0, S, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(DCL, "DCL", 0, DCL, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(POW, "POW", 3, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(CRS, "CRS", 2, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(SGN, "SGN", 3, DSSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(ABS, "ABS", 1, DS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(NRM, "NRM", 3, DS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(SINCOS, "SINCOS", 8, SINCOS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(REP, "REP", 3, S, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(ENDREP, "ENDREP", 2, NULL, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(IF, "IF", 3, S, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(IFC, "IF", 3, SS, MOJOSHADER_TYPE_ANY)
INSTRUCTION(ELSE, "ELSE", 1, NULL, MOJOSHADER_TYPE_ANY)  // !!! FIXME: state!
INSTRUCTION(ENDIF, "ENDIF", 1, NULL, MOJOSHADER_TYPE_ANY) // !!! FIXME: state!
INSTRUCTION_STATE(BREAK, "BREAK", 1, NULL, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(BREAKC, "BREAK", 3, SS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(MOVA, "MOVA", 1, DS, MOJOSHADER_TYPE_VERTEX)
INSTRUCTION_STATE(DEFB, "DEFB", 0, DEFB, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(DEFI, "DEFI", 0, DEFI, MOJOSHADER_TYPE_ANY)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION_STATE(TEXCRD, "TEXCRD", 1, TEXCRD, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION_STATE(TEXKILL, "TEXKILL", 2, D, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION_STATE(TEXLD, "TEXLD", 1, TEXLD, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXBEM, "TEXBEM", 1, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXBEML, "TEXBEML", 2, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXREG2AR, "TEXREG2AR", 1, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXREG2GB, "TEXREG2GB", 1, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXM3X2PAD, "TEXM3X2PAD", 1, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXM3X2TEX, "TEXM3X2TEX", 1, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXM3X3PAD, "TEXM3X3PAD", 1, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXM3X3TEX, "TEXM3X3TEX", 1, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(RESERVED, 0, 0, NULL, MOJOSHADER_TYPE_UNKNOWN)
INSTRUCTION(TEXM3X3SPEC, "TEXM3X3SPEC", 1, DSS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXM3X3VSPEC, "TEXM3X3VSPEC", 1, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(EXPP, "EXPP", 1, DS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(LOGP, "LOGP", 1, DS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(CND, "CND", 1, DSSS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION_STATE(DEF, "DEF", 0, DEF, MOJOSHADER_TYPE_ANY)
INSTRUCTION(TEXREG2RGB, "TEXREG2RGB", 1, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXDP3TEX, "TEXDP3TEX", 1, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXM3X2DEPTH, "TEXM3X2DEPTH", 1, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXDP3, "TEXDP3", 1, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXM3X3, "TEXM3X3", 1, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXDEPTH, "TEXDEPTH", 1, D, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION_STATE(CMP, "CMP", 1, DSSS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(BEM, "BEM", 2, DSS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION_STATE(DP2ADD, "DP2ADD", 2, DSSS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(DSX, "DSX", 2, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(DSY, "DSY", 2, DS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION(TEXLDD, "TEXLDD", 3, DSSSS, MOJOSHADER_TYPE_PIXEL)
INSTRUCTION_STATE(SETP, "SETP", 1, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(TEXLDL, "TEXLDL", 2, DSS, MOJOSHADER_TYPE_ANY)
INSTRUCTION_STATE(BREAKP, "BREAKP", 3, S, MOJOSHADER_TYPE_ANY)
#endif

// end of mojoshader_internal.h ...

