/**
 * MojoShader; generate shader programs from bytecode of compiled
 *  Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

#ifndef MOJOSHADER_PROFILE_ARB1_H
#define MOJOSHADER_PROFILE_ARB1_H

#include "mojoshader_profile.h"

#if !SUPPORT_PROFILE_ARB1
#define PROFILE_EMITTER_ARB1(op)
#else
#undef AT_LEAST_ONE_PROFILE
#define AT_LEAST_ONE_PROFILE 1
#define PROFILE_EMITTER_ARB1(op) emit_ARB1_##op,

inline static const char *get_ARB1_register_string(Context *ctx,
                        const RegisterType regtype, const int regnum,
                        char *regnum_str, const size_t regnum_size);

const char *allocate_ARB1_scratch_reg_name(Context *ctx, char *buf,
                                                  const size_t buflen);

inline static const char *get_ARB1_branch_label_name(Context *ctx, const int id,
                                                char *buf, const size_t buflen);

const char *get_ARB1_varname_in_buf(Context *ctx, const RegisterType rt,
                                           const int regnum, char *buf,
                                           const size_t buflen);

const char *get_ARB1_varname(Context *ctx, const RegisterType rt,
                                    const int regnum);

inline static const char *get_ARB1_const_array_varname_in_buf(Context *ctx,
                                                const int base, const int size,
                                                char *buf, const size_t buflen);

const char *get_ARB1_const_array_varname(Context *ctx, int base, int size);

const char *make_ARB1_srcarg_string_in_buf(Context *ctx,
                                                  const SourceArgInfo *arg,
                                                  char *buf, size_t buflen);

const char *get_ARB1_destarg_varname(Context *ctx, char *buf,
                                            const size_t buflen);

const char *get_ARB1_srcarg_varname(Context *ctx, const size_t idx,
                                           char *buf, const size_t buflen);


const char *make_ARB1_destarg_string(Context *ctx, char *buf,
                                            const size_t buflen);

void emit_ARB1_opcode_ds(Context *ctx, const char *opcode);
void emit_ARB1_opcode_dss(Context *ctx, const char *opcode);
void emit_ARB1_opcode_dsss(Context *ctx, const char *opcode);

#define EMIT_ARB1_OPCODE_FUNC(op) \
    void emit_ARB1_##op(Context *ctx);
#define EMIT_ARB1_OPCODE_D_FUNC(op) \
    void emit_ARB1_##op(Context *ctx);
#define EMIT_ARB1_OPCODE_S_FUNC(op) \
    void emit_ARB1_##op(Context *ctx);
#define EMIT_ARB1_OPCODE_SS_FUNC(op) \
    void emit_ARB1_##op(Context *ctx);
#define EMIT_ARB1_OPCODE_DS_FUNC(op) \
    void emit_ARB1_##op(Context *ctx);
#define EMIT_ARB1_OPCODE_DSS_FUNC(op) \
    void emit_ARB1_##op(Context *ctx);
#define EMIT_ARB1_OPCODE_DSSS_FUNC(op) \
    void emit_ARB1_##op(Context *ctx);
#define EMIT_ARB1_OPCODE_DSSSS_FUNC(op) \
    void emit_ARB1_##op(Context *ctx);
#define EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(op) \
    void emit_ARB1_##op(Context *ctx);


void emit_ARB1_start(Context *ctx, const char *profilestr);
void emit_ARB1_end(Context *ctx);
void emit_ARB1_phase(Context *ctx);
void emit_ARB1_finalize(Context *ctx);
void emit_ARB1_global(Context *ctx, RegisterType regtype, int regnum);
void emit_ARB1_array(Context *ctx, VariableList *var);
void emit_ARB1_const_array(Context *ctx, const ConstantsList *clist,
                                  int base, int size);
void emit_ARB1_uniform(Context *ctx, RegisterType regtype, int regnum,
                              const VariableList *var);
void emit_ARB1_sampler(Context *ctx,int stage,TextureType ttype,int tb);
void emit_ARB1_attribute(Context *ctx, RegisterType regtype, int regnum,
                                MOJOSHADER_usage usage, int index, int wmask,
                                int flags);
void emit_ARB1_RESERVED(Context *ctx);
void emit_ARB1_NOP(Context *ctx);
EMIT_ARB1_OPCODE_DS_FUNC(MOV)
EMIT_ARB1_OPCODE_DSS_FUNC(ADD)
EMIT_ARB1_OPCODE_DSS_FUNC(SUB)
EMIT_ARB1_OPCODE_DSSS_FUNC(MAD)
EMIT_ARB1_OPCODE_DSS_FUNC(MUL)
EMIT_ARB1_OPCODE_DS_FUNC(RCP)
void emit_ARB1_RSQ(Context *ctx);
EMIT_ARB1_OPCODE_DSS_FUNC(DP3)
EMIT_ARB1_OPCODE_DSS_FUNC(DP4)
EMIT_ARB1_OPCODE_DSS_FUNC(MIN)
EMIT_ARB1_OPCODE_DSS_FUNC(MAX)
EMIT_ARB1_OPCODE_DSS_FUNC(SLT)
EMIT_ARB1_OPCODE_DSS_FUNC(SGE)
void emit_ARB1_EXP(Context *ctx);
void emit_ARB1_LOG(Context *ctx);
EMIT_ARB1_OPCODE_DS_FUNC(LIT)
EMIT_ARB1_OPCODE_DSS_FUNC(DST)
void emit_ARB1_LRP(Context *ctx);
EMIT_ARB1_OPCODE_DS_FUNC(FRC)
void emit_ARB1_M4X4(Context *ctx);
void emit_ARB1_M4X3(Context *ctx);
void emit_ARB1_M3X4(Context *ctx);
void emit_ARB1_M3X3(Context *ctx);
void emit_ARB1_M3X2(Context *ctx);
void emit_ARB1_CALL(Context *ctx);
void emit_ARB1_CALLNZ(Context *ctx);
EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(LOOP)
void emit_ARB1_RET(Context *ctx);
EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(ENDLOOP)
void emit_ARB1_LABEL(Context *ctx);
void emit_ARB1_POW(Context *ctx);
void emit_ARB1_CRS(Context *ctx);
void emit_ARB1_SGN(Context *ctx);
EMIT_ARB1_OPCODE_DS_FUNC(ABS)
void emit_ARB1_NRM(Context *ctx);
void emit_ARB1_SINCOS(Context *ctx);
void emit_ARB1_REP(Context *ctx);
void emit_ARB1_ENDREP(Context *ctx);
void nv2_if(Context *ctx);
void emit_ARB1_IF(Context *ctx);
void emit_ARB1_ELSE(Context *ctx);
void emit_ARB1_ENDIF(Context *ctx);
void emit_ARB1_BREAK(Context *ctx);
void emit_ARB1_MOVA(Context *ctx);
void emit_ARB1_TEXKILL(Context *ctx);
void emit_ARB1_TEXBEM(Context *ctx);
void emit_ARB1_TEXBEML(Context *ctx);
EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(TEXREG2AR)
EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(TEXREG2GB)
void emit_ARB1_TEXM3X2PAD(Context *ctx);
void emit_ARB1_TEXM3X2TEX(Context *ctx);
void emit_ARB1_TEXM3X3PAD(Context *ctx);
void emit_ARB1_TEXM3X3TEX(Context *ctx);
void emit_ARB1_TEXM3X3SPEC(Context *ctx);
void emit_ARB1_TEXM3X3VSPEC(Context *ctx);
void emit_ARB1_EXPP(Context *ctx);
void emit_ARB1_LOGP(Context *ctx);
void emit_ARB1_CND(Context *ctx);
EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(TEXREG2RGB)
EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(TEXDP3TEX)
EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(TEXM3X2DEPTH)
EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(TEXDP3)
void emit_ARB1_TEXM3X3(Context *ctx);
EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(TEXDEPTH)
void emit_ARB1_CMP(Context *ctx);
EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(BEM)
void emit_ARB1_DP2ADD(Context *ctx);
void emit_ARB1_DSX(Context *ctx);
void emit_ARB1_DSY(Context *ctx);
void emit_ARB1_TEXLDD(Context *ctx);
void emit_ARB1_TEXLDL(Context *ctx);
EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(BREAKP)
EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(BREAKC)
void emit_ARB1_IFC(Context *ctx);
EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(SETP)
void emit_ARB1_DEF(Context *ctx);
void emit_ARB1_DEFI(Context *ctx);
void emit_ARB1_DEFB(Context *ctx);
void emit_ARB1_DCL(Context *ctx);
EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC(TEXCRD)
void emit_ARB1_TEXLD(Context *ctx);

#undef EMIT_ARB1_OPCODE_FUNC
#undef EMIT_ARB1_OPCODE_D_FUNC
#undef EMIT_ARB1_OPCODE_S_FUNC
#undef EMIT_ARB1_OPCODE_SS_FUNC
#undef EMIT_ARB1_OPCODE_DS_FUNC
#undef EMIT_ARB1_OPCODE_DSS_FUNC
#undef EMIT_ARB1_OPCODE_DSSS_FUNC
#undef EMIT_ARB1_OPCODE_DSSSS_FUNC
#undef EMIT_ARB1_OPCODE_UNIMPLEMENTED_FUNC

#endif  // SUPPORT_PROFILE_ARB1

#endif // MOJOSHADER_PROFILE_ARB1_H