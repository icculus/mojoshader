#ifndef MOJOSHADER_PROFILE_GLSL_H
#define MOJOSHADER_PROFILE_GLSL_H

#include "mojoshader_profile.h"

#if !SUPPORT_PROFILE_GLSL
#define PROFILE_EMITTER_GLSL(op)
#else
#undef AT_LEAST_ONE_PROFILE
#define AT_LEAST_ONE_PROFILE 1
#define PROFILE_EMITTER_GLSL(op) emit_GLSL_##op,

#define EMIT_GLSL_OPCODE_UNIMPLEMENTED_FUNC(op) \
    void emit_GLSL_##op(Context *ctx);

static inline const char *get_GLSL_register_string(Context *ctx,
                        const RegisterType regtype, const int regnum,
                        char *regnum_str, const size_t regnum_size);

const char *get_GLSL_uniform_type(Context *ctx, const RegisterType rtype);
const char *get_GLSL_varname_in_buf(Context *ctx, RegisterType rt,
                                           int regnum, char *buf,
                                           const size_t len);

const char *get_GLSL_varname(Context *ctx, RegisterType rt, int regnum);

static inline const char *get_GLSL_const_array_varname_in_buf(Context *ctx,
                                                const int base, const int size,
                                                char *buf, const size_t buflen);

const char *get_GLSL_const_array_varname(Context *ctx, int base, int size);

static inline const char *get_GLSL_input_array_varname(Context *ctx,
                                                char *buf, const size_t buflen);

const char *get_GLSL_uniform_array_varname(Context *ctx,
                                                  const RegisterType regtype,
                                                  char *buf, const size_t len);

const char *get_GLSL_destarg_varname(Context *ctx, char *buf, size_t len);

const char *get_GLSL_srcarg_varname(Context *ctx, const size_t idx,
                                           char *buf, size_t len);

const char *make_GLSL_destarg_assign(Context *, char *, const size_t,
                                            const char *, ...);

const char *make_GLSL_destarg_assign(Context *ctx, char *buf,
                                            const size_t buflen,
                                            const char *fmt, ...);

char *make_GLSL_swizzle_string(char *swiz_str, const size_t strsize,
                                      const int swizzle, const int writemask);

const char *make_GLSL_srcarg_string(Context *ctx, const size_t idx,
                                           const int writemask, char *buf,
                                           const size_t buflen);

// generate some convenience functions.
#define MAKE_GLSL_SRCARG_STRING_(mask, bitmask) \
    static inline const char *make_GLSL_srcarg_string_##mask(Context *ctx, \
                                                const size_t idx, char *buf, \
                                                const size_t buflen);
MAKE_GLSL_SRCARG_STRING_(x, (1 << 0))
MAKE_GLSL_SRCARG_STRING_(y, (1 << 1))
MAKE_GLSL_SRCARG_STRING_(z, (1 << 2))
MAKE_GLSL_SRCARG_STRING_(w, (1 << 3))
MAKE_GLSL_SRCARG_STRING_(scalar, (1 << 0))
MAKE_GLSL_SRCARG_STRING_(full, 0xF)
MAKE_GLSL_SRCARG_STRING_(masked, ctx->dest_arg.writemask)
MAKE_GLSL_SRCARG_STRING_(vec3, 0x7)
MAKE_GLSL_SRCARG_STRING_(vec2, 0x3)
#undef MAKE_GLSL_SRCARG_STRING_

// special cases for comparison opcodes...

const char *get_GLSL_comparison_string_scalar(Context *ctx);
const char *get_GLSL_comparison_string_vector(Context *ctx);

void emit_GLSL_start(Context *ctx, const char *profilestr);

void emit_GLSL_RET(Context *ctx);
void emit_GLSL_end(Context *ctx);

void emit_GLSL_phase(Context *ctx);

void output_GLSL_uniform_array(Context *ctx, const RegisterType regtype,
                                      const int size);

void emit_GLSL_finalize(Context *ctx);

void emit_GLSL_global(Context *ctx, RegisterType regtype, int regnum);

void emit_GLSL_array(Context *ctx, VariableList *var);

void emit_GLSL_const_array(Context *ctx, const ConstantsList *clist,
                                  int base, int size);

void emit_GLSL_uniform(Context *ctx, RegisterType regtype, int regnum,
                              const VariableList *var);

void emit_GLSL_sampler(Context *ctx,int stage,TextureType ttype,int tb);

void emit_GLSL_attribute(Context *ctx, RegisterType regtype, int regnum,
                                MOJOSHADER_usage usage, int index, int wmask,
                                int flags);

void emit_GLSL_NOP(Context *ctx);
void emit_GLSL_MOV(Context *ctx);
void emit_GLSL_ADD(Context *ctx);
void emit_GLSL_SUB(Context *ctx);
void emit_GLSL_MAD(Context *ctx);
void emit_GLSL_MUL(Context *ctx);
void emit_GLSL_RCP(Context *ctx);
void emit_GLSL_RSQ(Context *ctx);
void emit_GLSL_dotprod(Context *ctx, const char *src0, const char *src1,
                              const char *extra);
void emit_GLSL_DP3(Context *ctx);
void emit_GLSL_DP4(Context *ctx);
void emit_GLSL_MIN(Context *ctx);
void emit_GLSL_MAX(Context *ctx);
void emit_GLSL_SLT(Context *ctx);
void emit_GLSL_SGE(Context *ctx);
void emit_GLSL_EXP(Context *ctx);
void emit_GLSL_LOG(Context *ctx);
void emit_GLSL_LIT_helper(Context *ctx);
void emit_GLSL_LIT(Context *ctx);
void emit_GLSL_DST(Context *ctx);
void emit_GLSL_LRP(Context *ctx);
void emit_GLSL_FRC(Context *ctx);
void emit_GLSL_M4X4(Context *ctx);
void emit_GLSL_M4X3(Context *ctx);
void emit_GLSL_M3X4(Context *ctx);
void emit_GLSL_M3X3(Context *ctx);
void emit_GLSL_M3X2(Context *ctx);
void emit_GLSL_CALL(Context *ctx);
void emit_GLSL_CALLNZ(Context *ctx);
void emit_GLSL_LOOP(Context *ctx);
void emit_GLSL_RET(Context *ctx);
void emit_GLSL_ENDLOOP(Context *ctx);
void emit_GLSL_LABEL(Context *ctx);
void emit_GLSL_DCL(Context *ctx);
void emit_GLSL_POW(Context *ctx);
void emit_GLSL_CRS(Context *ctx);
void emit_GLSL_SGN(Context *ctx);
void emit_GLSL_ABS(Context *ctx);
void emit_GLSL_NRM(Context *ctx);
void emit_GLSL_SINCOS(Context *ctx);
void emit_GLSL_REP(Context *ctx);
void emit_GLSL_ENDREP(Context *ctx);
void emit_GLSL_IF(Context *ctx);
void emit_GLSL_IFC(Context *ctx);
void emit_GLSL_ELSE(Context *ctx);
void emit_GLSL_ENDIF(Context *ctx);
void emit_GLSL_BREAK(Context *ctx);
void emit_GLSL_BREAKC(Context *ctx);
void emit_GLSL_MOVA(Context *ctx);
void emit_GLSL_DEFB(Context *ctx);
void emit_GLSL_DEFI(Context *ctx);
EMIT_GLSL_OPCODE_UNIMPLEMENTED_FUNC(TEXCRD)
void emit_GLSL_TEXKILL(Context *ctx);
void emit_GLSL_TEXLD(Context *ctx);
void emit_GLSL_TEXBEM(Context *ctx);
void emit_GLSL_TEXBEML(Context *ctx);
EMIT_GLSL_OPCODE_UNIMPLEMENTED_FUNC(TEXREG2AR) // !!! FIXME
EMIT_GLSL_OPCODE_UNIMPLEMENTED_FUNC(TEXREG2GB) // !!! FIXME
void emit_GLSL_TEXM3X2PAD(Context *ctx);
void emit_GLSL_TEXM3X2TEX(Context *ctx);
void emit_GLSL_TEXM3X3PAD(Context *ctx);
void emit_GLSL_TEXM3X3TEX(Context *ctx);
void emit_GLSL_TEXM3X3SPEC_helper(Context *ctx);
void emit_GLSL_TEXM3X3SPEC(Context *ctx);
void emit_GLSL_TEXM3X3VSPEC(Context *ctx);
void emit_GLSL_EXPP(Context *ctx);
void emit_GLSL_LOGP(Context *ctx);

// common code between CMP and CND.
void emit_GLSL_comparison_operations(Context *ctx, const char *cmp);

void emit_GLSL_CND(Context *ctx);
void emit_GLSL_DEF(Context *ctx);
EMIT_GLSL_OPCODE_UNIMPLEMENTED_FUNC(TEXREG2RGB) // !!! FIXME
EMIT_GLSL_OPCODE_UNIMPLEMENTED_FUNC(TEXDP3TEX) // !!! FIXME
EMIT_GLSL_OPCODE_UNIMPLEMENTED_FUNC(TEXM3X2DEPTH) // !!! FIXME
EMIT_GLSL_OPCODE_UNIMPLEMENTED_FUNC(TEXDP3) // !!! FIXME
void emit_GLSL_TEXM3X3(Context *ctx);
EMIT_GLSL_OPCODE_UNIMPLEMENTED_FUNC(TEXDEPTH) // !!! FIXME
void emit_GLSL_CMP(Context *ctx);
EMIT_GLSL_OPCODE_UNIMPLEMENTED_FUNC(BEM) // !!! FIXME
void emit_GLSL_DP2ADD(Context *ctx);
void emit_GLSL_DSX(Context *ctx);
void emit_GLSL_DSY(Context *ctx);
void emit_GLSL_TEXLDD(Context *ctx);
void emit_GLSL_SETP(Context *ctx);
void emit_GLSL_TEXLDL(Context *ctx);
void emit_GLSL_BREAKP(Context *ctx);
void emit_GLSL_RESERVED(Context *ctx);

#undef EMIT_GLSL_OPCODE_UNIMPLEMENTED_FUNC

#endif  // SUPPORT_PROFILE_GLSL

#endif // MOJOSHADER_PROFILE_GLSL_H