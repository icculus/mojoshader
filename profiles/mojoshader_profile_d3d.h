#ifndef MOJOSHADER_PROFILE_D3D_H
#define MOJOSHADER_PROFILE_D3D_H

#include "mojoshader_profile.h"

#if !SUPPORT_PROFILE_D3D
#define PROFILE_EMITTER_D3D(op)
#else
#undef AT_LEAST_ONE_PROFILE
#define AT_LEAST_ONE_PROFILE 1
#define PROFILE_EMITTER_D3D(op) emit_D3D_##op,

const char *make_D3D_srcarg_string_in_buf(Context *ctx,
                                                 const SourceArgInfo *arg,
                                                 char *buf, size_t buflen);
const char *make_D3D_destarg_string(Context *ctx, char *buf,
                                           const size_t buflen);
const char *make_D3D_srcarg_string(Context *ctx, const size_t idx,
                                          char *buf, size_t buflen);
const char *get_D3D_varname_in_buf(Context *ctx, RegisterType rt,
                                           int regnum, char *buf,
                                           const size_t len);


const char *get_D3D_varname(Context *ctx, RegisterType rt, int regnum);
const char *get_D3D_const_array_varname(Context *ctx, int base, int size);

void emit_D3D_start(Context *ctx, const char *profilestr);
void emit_D3D_end(Context *ctx);
void emit_D3D_phase(Context *ctx);
void emit_D3D_finalize(Context *ctx);
void emit_D3D_global(Context *ctx, RegisterType regtype, int regnum);
void emit_D3D_array(Context *ctx, VariableList *var);
void emit_D3D_const_array(Context *ctx, const ConstantsList *clist,
                                 int base, int size);
void emit_D3D_uniform(Context *ctx, RegisterType regtype, int regnum,
                             const VariableList *var);
void emit_D3D_sampler(Context *ctx, int s, TextureType ttype, int tb);
void emit_D3D_attribute(Context *ctx, RegisterType regtype, int regnum,
                               MOJOSHADER_usage usage, int index, int wmask,
                               int flags);
void emit_D3D_RESERVED(Context *ctx);


// Generic D3D opcode emitters. A list of macros generate all the entry points
//  that call into these...

void emit_D3D_opcode_d(Context *ctx, const char *opcode);
void emit_D3D_opcode_s(Context *ctx, const char *opcode);
void emit_D3D_opcode_ss(Context *ctx, const char *opcode);
void emit_D3D_opcode_ds(Context *ctx, const char *opcode);
void emit_D3D_opcode_dss(Context *ctx, const char *opcode);
void emit_D3D_opcode_dsss(Context *ctx, const char *opcode);
void emit_D3D_opcode_dssss(Context *ctx, const char *opcode);
void emit_D3D_opcode(Context *ctx, const char *opcode);

#define EMIT_D3D_OPCODE_FUNC(op) \
    void emit_D3D_##op(Context *ctx);

#define EMIT_D3D_OPCODE_D_FUNC(op) \
    void emit_D3D_##op(Context *ctx);

#define EMIT_D3D_OPCODE_S_FUNC(op) \
    void emit_D3D_##op(Context *ctx);

#define EMIT_D3D_OPCODE_SS_FUNC(op) \
    void emit_D3D_##op(Context *ctx);

#define EMIT_D3D_OPCODE_DS_FUNC(op) \
    void emit_D3D_##op(Context *ctx);

#define EMIT_D3D_OPCODE_DSS_FUNC(op) \
    void emit_D3D_##op(Context *ctx);

#define EMIT_D3D_OPCODE_DSSS_FUNC(op) \
    void emit_D3D_##op(Context *ctx);
	
#define EMIT_D3D_OPCODE_DSSSS_FUNC(op) \
    void emit_D3D_##op(Context *ctx);

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

const char *get_D3D_comparison_string(Context *ctx);

void emit_D3D_BREAKC(Context *ctx);
void emit_D3D_IFC(Context *ctx);
void emit_D3D_SETP(Context *ctx);
void emit_D3D_DEF(Context *ctx);
void emit_D3D_DEFI(Context *ctx);
void emit_D3D_DEFB(Context *ctx);
void emit_D3D_DCL(Context *ctx);
void emit_D3D_TEXCRD(Context *ctx);
void emit_D3D_TEXLD(Context *ctx);
void emit_D3D_SINCOS(Context *ctx);

#undef EMIT_D3D_OPCODE_FUNC
#undef EMIT_D3D_OPCODE_D_FUNC
#undef EMIT_D3D_OPCODE_S_FUNC
#undef EMIT_D3D_OPCODE_SS_FUNC
#undef EMIT_D3D_OPCODE_DS_FUNC
#undef EMIT_D3D_OPCODE_DSS_FUNC
#undef EMIT_D3D_OPCODE_DSSS_FUNC
#undef EMIT_D3D_OPCODE_DSSSS_FUNC

#endif // SUPPORT_PROFILE_D3D

#endif // MOJOSHADER_PROFILE_D3D_H