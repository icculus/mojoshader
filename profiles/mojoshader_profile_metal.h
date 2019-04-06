#ifndef MOJOSHADER_PROFILE_METAL_H
#define MOJOSHADER_PROFILE_METAL_H

#include "mojoshader_profile.h"

#if !SUPPORT_PROFILE_METAL
#define PROFILE_EMITTER_METAL(op)
#else
#undef AT_LEAST_ONE_PROFILE
#define AT_LEAST_ONE_PROFILE 1
#define PROFILE_EMITTER_METAL(op) emit_METAL_##op,

#define EMIT_METAL_OPCODE_UNIMPLEMENTED_FUNC(op) \
    void emit_METAL_##op(Context *ctx);

static inline const char *get_METAL_register_string(Context *ctx,
                        const RegisterType regtype, const int regnum,
                        char *regnum_str, const size_t regnum_size);

const char *get_METAL_uniform_type(Context *ctx, const RegisterType rtype);

const char *get_METAL_varname_in_buf(Context *ctx, RegisterType rt,
                                           int regnum, char *buf,
                                           const size_t len);


const char *get_METAL_varname(Context *ctx, RegisterType rt, int regnum);


static inline const char *get_METAL_const_array_varname_in_buf(Context *ctx,
                                                const int base, const int size,
                                                char *buf, const size_t buflen);

const char *get_METAL_const_array_varname(Context *ctx, int base, int size);


static inline const char *get_METAL_input_array_varname(Context *ctx,
                                                char *buf, const size_t buflen);


const char *get_METAL_uniform_array_varname(Context *ctx,
                                                  const RegisterType regtype,
                                                  char *buf, const size_t len);

const char *get_METAL_destarg_varname(Context *ctx, char *buf, size_t len);

const char *get_METAL_srcarg_varname(Context *ctx, const size_t idx,
                                           char *buf, size_t len);


const char *make_METAL_destarg_assign(Context *, char *, const size_t,
                                            const char *, ...);

const char *make_METAL_destarg_assign(Context *ctx, char *buf,
                                            const size_t buflen,
                                            const char *fmt, ...);


char *make_METAL_swizzle_string(char *swiz_str, const size_t strsize,
                                      const int swizzle, const int writemask);


const char *make_METAL_srcarg_string(Context *ctx, const size_t idx,
                                           const int writemask, char *buf,
                                           const size_t buflen);

// generate some convenience functions.
#define MAKE_METAL_SRCARG_STRING_(mask, bitmask) \
    static inline const char *make_METAL_srcarg_string_##mask(Context *ctx, \
                                                const size_t idx, char *buf, \
                                                const size_t buflen);
MAKE_METAL_SRCARG_STRING_(x, (1 << 0))
MAKE_METAL_SRCARG_STRING_(y, (1 << 1))
MAKE_METAL_SRCARG_STRING_(z, (1 << 2))
MAKE_METAL_SRCARG_STRING_(w, (1 << 3))
MAKE_METAL_SRCARG_STRING_(scalar, (1 << 0))
MAKE_METAL_SRCARG_STRING_(full, 0xF)
MAKE_METAL_SRCARG_STRING_(masked, ctx->dest_arg.writemask)
MAKE_METAL_SRCARG_STRING_(vec3, 0x7)
MAKE_METAL_SRCARG_STRING_(vec2, 0x3)
#undef MAKE_METAL_SRCARG_STRING_

// special cases for comparison opcodes...

const char *get_METAL_comparison_string_scalar(Context *ctx);
const char *get_METAL_comparison_string_vector(Context *ctx);

void emit_METAL_start(Context *ctx, const char *profilestr);
void emit_METAL_RET(Context *ctx);
void emit_METAL_end(Context *ctx);
void emit_METAL_phase(Context *ctx);
void emit_METAL_finalize(Context *ctx);
void emit_METAL_global(Context *ctx, RegisterType regtype, int regnum);
void emit_METAL_array(Context *ctx, VariableList *var);
void emit_METAL_const_array(Context *ctx, const ConstantsList *clist,
                                   int base, int size);
void emit_METAL_uniform(Context *ctx, RegisterType regtype, int regnum,
                              const VariableList *var);
void emit_METAL_sampler(Context *ctx,int stage,TextureType ttype,int tb);
void emit_METAL_attribute(Context *ctx, RegisterType regtype, int regnum,
                                MOJOSHADER_usage usage, int index, int wmask,
                                int flags);

void emit_METAL_NOP(Context *ctx);
void emit_METAL_MOV(Context *ctx);
void emit_METAL_ADD(Context *ctx);
void emit_METAL_SUB(Context *ctx);
void emit_METAL_MAD(Context *ctx);
void emit_METAL_MUL(Context *ctx);
void emit_METAL_RCP(Context *ctx);
void emit_METAL_RSQ(Context *ctx);
void emit_METAL_dotprod(Context *ctx, const char *src0, const char *src1,
                              const char *extra);
void emit_METAL_DP3(Context *ctx);
void emit_METAL_DP4(Context *ctx);
void emit_METAL_MIN(Context *ctx);
void emit_METAL_MAX(Context *ctx);
void emit_METAL_SLT(Context *ctx);
void emit_METAL_SGE(Context *ctx);
void emit_METAL_EXP(Context *ctx);
void emit_METAL_LOG(Context *ctx);
void emit_METAL_LIT_helper(Context *ctx);
void emit_METAL_LIT(Context *ctx);
void emit_METAL_DST(Context *ctx);
void emit_METAL_LRP(Context *ctx);
void emit_METAL_FRC(Context *ctx);
void emit_METAL_M4X4(Context *ctx);
void emit_METAL_M4X3(Context *ctx);
void emit_METAL_M3X4(Context *ctx);
void emit_METAL_M3X3(Context *ctx);
void emit_METAL_M3X2(Context *ctx);
void emit_METAL_CALL(Context *ctx);
void emit_METAL_CALLNZ(Context *ctx);
void emit_METAL_LOOP(Context *ctx);
void emit_METAL_RET(Context *ctx);
void emit_METAL_ENDLOOP(Context *ctx);
void emit_METAL_LABEL(Context *ctx);
void emit_METAL_DCL(Context *ctx);
void emit_METAL_POW(Context *ctx);
void emit_METAL_CRS(Context *ctx);
void emit_METAL_SGN(Context *ctx);
void emit_METAL_ABS(Context *ctx);
void emit_METAL_NRM(Context *ctx);
void emit_METAL_SINCOS(Context *ctx);
void emit_METAL_REP(Context *ctx);
void emit_METAL_ENDREP(Context *ctx);
void emit_METAL_IF(Context *ctx);
void emit_METAL_IFC(Context *ctx);
void emit_METAL_ELSE(Context *ctx);
void emit_METAL_ENDIF(Context *ctx);
void emit_METAL_BREAK(Context *ctx);
void emit_METAL_BREAKC(Context *ctx);
void emit_METAL_MOVA(Context *ctx);
void emit_METAL_DEFB(Context *ctx);
void emit_METAL_DEFI(Context *ctx);
EMIT_METAL_OPCODE_UNIMPLEMENTED_FUNC(TEXCRD)
void emit_METAL_TEXKILL(Context *ctx);
void emit_METAL_TEXLD(Context *ctx);
void emit_METAL_TEXBEM(Context *ctx);
void emit_METAL_TEXBEML(Context *ctx);
EMIT_METAL_OPCODE_UNIMPLEMENTED_FUNC(TEXREG2AR) // !!! FIXME
EMIT_METAL_OPCODE_UNIMPLEMENTED_FUNC(TEXREG2GB) // !!! FIXME
void emit_METAL_TEXM3X2PAD(Context *ctx);
void emit_METAL_TEXM3X2TEX(Context *ctx);
void emit_METAL_TEXM3X3PAD(Context *ctx);
void emit_METAL_TEXM3X3TEX(Context *ctx);
void emit_METAL_TEXM3X3SPEC_helper(Context *ctx);
void emit_METAL_TEXM3X3SPEC(Context *ctx);
void emit_METAL_TEXM3X3VSPEC(Context *ctx);
void emit_METAL_EXPP(Context *ctx);
void emit_METAL_LOGP(Context *ctx);
void emit_METAL_comparison_operations(Context *ctx, const char *cmp);
void emit_METAL_CND(Context *ctx);
void emit_METAL_DEF(Context *ctx);
EMIT_METAL_OPCODE_UNIMPLEMENTED_FUNC(TEXREG2RGB) // !!! FIXME
EMIT_METAL_OPCODE_UNIMPLEMENTED_FUNC(TEXDP3TEX) // !!! FIXME
EMIT_METAL_OPCODE_UNIMPLEMENTED_FUNC(TEXM3X2DEPTH) // !!! FIXME
EMIT_METAL_OPCODE_UNIMPLEMENTED_FUNC(TEXDP3) // !!! FIXME
void emit_METAL_TEXM3X3(Context *ctx);
EMIT_METAL_OPCODE_UNIMPLEMENTED_FUNC(TEXDEPTH) // !!! FIXME
void emit_METAL_CMP(Context *ctx);
EMIT_METAL_OPCODE_UNIMPLEMENTED_FUNC(BEM) // !!! FIXME
void emit_METAL_DP2ADD(Context *ctx);
void emit_METAL_DSX(Context *ctx);
void emit_METAL_DSY(Context *ctx);
void emit_METAL_TEXLDD(Context *ctx);
void emit_METAL_SETP(Context *ctx);
void emit_METAL_TEXLDL(Context *ctx);
void emit_METAL_BREAKP(Context *ctx);
void emit_METAL_RESERVED(Context *ctx);

#undef EMIT_METAL_OPCODE_UNIMPLEMENTED_FUNC

#endif  // SUPPORT_PROFILE_METAL

#endif // MOJOSHADER_PROFILE_METAL_H