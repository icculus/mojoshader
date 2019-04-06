#ifndef MOJOSHADER_PROFILE_H
#define MOJOSHADER_PROFILE_H

#define __MOJOSHADER_INTERNAL__ 1
#include "../mojoshader_internal.h"

typedef struct ConstantsList
{
    MOJOSHADER_constant constant;
    struct ConstantsList *next;
} ConstantsList;

typedef struct VariableList
{
    MOJOSHADER_uniformType type;
    int index;
    int count;
    ConstantsList *constant;
    int used;
    int emit_position;  // used in some profiles.
    struct VariableList *next;
} VariableList;

typedef struct RegisterList
{
    RegisterType regtype;
    int regnum;
    MOJOSHADER_usage usage;
    unsigned int index;
    int writemask;
    int misc;
    int written;
    const VariableList *array;
    struct RegisterList *next;
} RegisterList;

typedef struct
{
    const uint32 *token;   // this is the unmolested token in the stream.
    int regnum;
    int swizzle;  // xyzw (all four, not split out).
    int swizzle_x;
    int swizzle_y;
    int swizzle_z;
    int swizzle_w;
    SourceMod src_mod;
    RegisterType regtype;
    int relative;
    RegisterType relative_regtype;
    int relative_regnum;
    int relative_component;
    const VariableList *relative_array;
} SourceArgInfo;

struct Profile;  // predeclare.

typedef struct CtabData
{
    int have_ctab;
    int symbol_count;
    MOJOSHADER_symbol *symbols;
} CtabData;

// Context...this is state that changes as we parse through a shader...
typedef struct Context
{
    int isfail;
    int out_of_memory;
    MOJOSHADER_malloc malloc;
    MOJOSHADER_free free;
    void *malloc_data;
    int current_position;
    const uint32 *orig_tokens;
    const uint32 *tokens;
    uint32 tokencount;
    int know_shader_size;
    const MOJOSHADER_swizzle *swizzles;
    unsigned int swizzles_count;
    const MOJOSHADER_samplerMap *samplermap;
    unsigned int samplermap_count;
    Buffer *output;
    Buffer *preflight;
    Buffer *globals;
    Buffer *inputs;
    Buffer *outputs;
    Buffer *helpers;
    Buffer *subroutines;
    Buffer *mainline_intro;
    Buffer *mainline_arguments;
    Buffer *mainline_top;
    Buffer *mainline;
    Buffer *postflight;
    Buffer *ignore;
    Buffer *output_stack[3];
    int indent_stack[3];
    int output_stack_len;
    int indent;
    const char *shader_type_str;
    const char *endline;
    const char *mainfn;
    int endline_len;
    int profileid;
    const struct Profile *profile;
    MOJOSHADER_shaderType shader_type;
    uint8 major_ver;
    uint8 minor_ver;
    DestArgInfo dest_arg;
    SourceArgInfo source_args[5];
    SourceArgInfo predicate_arg;  // for predicated instructions.
    uint32 dwords[4];
    uint32 version_token;
    int instruction_count;
    uint32 instruction_controls;
    uint32 previous_opcode;
    int coissue;
    int loops;
    int reps;
    int max_reps;
    int cmps;
    int scratch_registers;
    int max_scratch_registers;
    int branch_labels_stack_index;
    int branch_labels_stack[32];
    int assigned_branch_labels;
    int assigned_vertex_attributes;
    int last_address_reg_component;
    RegisterList used_registers;
    RegisterList defined_registers;
    ErrorList *errors;
    int constant_count;
    ConstantsList *constants;
    int uniform_count;
    int uniform_float4_count;
    int uniform_int4_count;
    int uniform_bool_count;
    RegisterList uniforms;
    int attribute_count;
    RegisterList attributes;
    int sampler_count;
    RegisterList samplers;
    VariableList *variables;  // variables to register mapping.
    int centroid_allowed;
    CtabData ctab;
    int have_relative_input_registers;
    int have_multi_color_outputs;
    int determined_constants_arrays;
    int predicated;
    int uses_pointsize;
    int uses_fog;

    // !!! FIXME: move these into SUPPORT_PROFILE sections.
    int glsl_generated_lit_helper;
    int glsl_generated_texldd_setup;
    int glsl_generated_texm3x3spec_helper;
    int arb1_wrote_position;
    // !!! FIXME: move these into SUPPORT_PROFILE sections.

    int have_preshader;
    int ignores_ctab;
    int reset_texmpad;
    int texm3x2pad_dst0;
    int texm3x2pad_src0;
    int texm3x3pad_dst0;
    int texm3x3pad_src0;
    int texm3x3pad_dst1;
    int texm3x3pad_src1;
    MOJOSHADER_preshader *preshader;

#if SUPPORT_PROFILE_ARB1_NV
    int profile_supports_nv2;
    int profile_supports_nv3;
    int profile_supports_nv4;
#endif
#if SUPPORT_PROFILE_GLSL120
    int profile_supports_glsl120;
#endif
#if SUPPORT_PROFILE_GLSLES
    int profile_supports_glsles;
#endif

#if SUPPORT_PROFILE_METAL
    int metal_need_header_common;
    int metal_need_header_math;
    int metal_need_header_relational;
    int metal_need_header_geometric;
    int metal_need_header_graphics;
    int metal_need_header_texture;
#endif
} Context;


// Use these macros so we can remove all bits of these profiles from the build.
#if SUPPORT_PROFILE_ARB1_NV
#define support_nv2(ctx) ((ctx)->profile_supports_nv2)
#define support_nv3(ctx) ((ctx)->profile_supports_nv3)
#define support_nv4(ctx) ((ctx)->profile_supports_nv4)
#else
#define support_nv2(ctx) (0)
#define support_nv3(ctx) (0)
#define support_nv4(ctx) (0)
#endif

#if SUPPORT_PROFILE_GLSL120
#define support_glsl120(ctx) ((ctx)->profile_supports_glsl120)
#else
#define support_glsl120(ctx) (0)
#endif

#if SUPPORT_PROFILE_GLSLES
#define support_glsles(ctx) ((ctx)->profile_supports_glsles)
#else
#define support_glsles(ctx) (0)
#endif


// Profile entry points...

// one emit function for each opcode in each profile.
typedef void (*emit_function)(Context *ctx);

// one emit function for starting output in each profile.
typedef void (*emit_start)(Context *ctx, const char *profilestr);

// one emit function for ending output in each profile.
typedef void (*emit_end)(Context *ctx);

// one emit function for phase opcode output in each profile.
typedef void (*emit_phase)(Context *ctx);

// one emit function for finalizing output in each profile.
typedef void (*emit_finalize)(Context *ctx);

// one emit function for global definitions in each profile.
typedef void (*emit_global)(Context *ctx, RegisterType regtype, int regnum);

// one emit function for relative uniform arrays in each profile.
typedef void (*emit_array)(Context *ctx, VariableList *var);

// one emit function for relative constants arrays in each profile.
typedef void (*emit_const_array)(Context *ctx,
                                 const struct ConstantsList *constslist,
                                 int base, int size);

// one emit function for uniforms in each profile.
typedef void (*emit_uniform)(Context *ctx, RegisterType regtype, int regnum,
                             const VariableList *var);

// one emit function for samplers in each profile.
typedef void (*emit_sampler)(Context *ctx, int stage, TextureType ttype,
                             int texbem);

// one emit function for attributes in each profile.
typedef void (*emit_attribute)(Context *ctx, RegisterType regtype, int regnum,
                               MOJOSHADER_usage usage, int index, int wmask,
                               int flags);

// one args function for each possible sequence of opcode arguments.
typedef int (*args_function)(Context *ctx);

// one state function for each opcode where we have state machine updates.
typedef void (*state_function)(Context *ctx);

// one function for varnames in each profile.
typedef const char *(*varname_function)(Context *c, RegisterType t, int num);

// one function for const var array in each profile.
typedef const char *(*const_array_varname_function)(Context *c, int base, int size);

typedef struct Profile
{
    const char *name;
    emit_start start_emitter;
    emit_end end_emitter;
    emit_phase phase_emitter;
    emit_global global_emitter;
    emit_array array_emitter;
    emit_const_array const_array_emitter;
    emit_uniform uniform_emitter;
    emit_sampler sampler_emitter;
    emit_attribute attribute_emitter;
    emit_finalize finalize_emitter;
    varname_function get_varname;
    const_array_varname_function get_const_array_varname;
} Profile;


// Common utilities...

void * MOJOSHADERCALL MallocBridge(int bytes, void *data);
void MOJOSHADERCALL FreeBridge(void *ptr, void *data);

static inline void out_of_memory(Context *ctx)
{
    ctx->isfail = ctx->out_of_memory = 1;
} // out_of_memory

static inline void *Malloc(Context *ctx, const size_t len)
{
    void *retval = ctx->malloc((int) len, ctx->malloc_data);
    if (retval == NULL)
        out_of_memory(ctx);
    return retval;
} // Malloc

static inline char *StrDup(Context *ctx, const char *str)
{
    char *retval = (char *) Malloc(ctx, strlen(str) + 1);
    if (retval != NULL)
        strcpy(retval, str);
    return retval;
} // StrDup

static inline void Free(Context *ctx, void *ptr)
{
    ctx->free(ptr, ctx->malloc_data);
} // Free

int set_output(Context *ctx, Buffer **section);
void push_output(Context *ctx, Buffer **section);

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
    return ( (((uint32) major) << 16) | (((minor) == 0xFF) ? 1 : (minor)) );
} // version_ui32

static inline int shader_version_supported(const uint8 maj, const uint8 min)
{
    return (ver_ui32(maj,min) <= ver_ui32(MAX_SHADER_MAJOR, MAX_SHADER_MINOR));
} // shader_version_supported

static inline int shader_version_atleast(const Context *ctx, const uint8 maj,
                                         const uint8 min)
{
    return (ver_ui32(ctx->major_ver, ctx->minor_ver) >= ver_ui32(maj, min));
} // shader_version_atleast

static inline int shader_version_exactly(const Context *ctx, const uint8 maj,
                                         const uint8 min)
{
    return ((ctx->major_ver == maj) && (ctx->minor_ver == min));
} // shader_version_exactly

static inline int shader_is_pixel(const Context *ctx)
{
    return (ctx->shader_type == MOJOSHADER_TYPE_PIXEL);
} // shader_is_pixel

static inline int shader_is_vertex(const Context *ctx)
{
    return (ctx->shader_type == MOJOSHADER_TYPE_VERTEX);
} // shader_is_vertex


static inline int isfail(const Context *ctx)
{
    return ctx->isfail;
} // isfail

void failf(Context *ctx, const char *fmt, ...);

static inline void fail(Context *ctx, const char *reason)
{
    failf(ctx, "%s", reason);
} // fail

void output_line(Context *ctx, const char *fmt, ...);

static inline void output_blank_line(Context *ctx)
{
    assert(ctx->output != NULL);
    if (!isfail(ctx))
        buffer_append(ctx->output, ctx->endline, ctx->endline_len);
} // output_blank_line

void floatstr(Context *ctx, char *buf, size_t bufsize, float f,
                     int leavedecimal);

RegisterList *reglist_insert(Context *ctx, RegisterList *prev,
                                    const RegisterType regtype,
                                    const int regnum);

RegisterList *reglist_find(const RegisterList *prev,
                                const RegisterType rtype, const int regnum);

static inline RegisterList *set_used_register(Context *ctx,
                                              const RegisterType regtype,
                                              const int regnum,
                                              const int written)
{
    RegisterList *reg = NULL;
    if ((regtype == REG_TYPE_COLOROUT) && (regnum > 0))
        ctx->have_multi_color_outputs = 1;

    reg = reglist_insert(ctx, &ctx->used_registers, regtype, regnum);
    if (reg && written)
        reg->written = 1;
    return reg;
} // set_used_register

static inline void set_defined_register(Context *ctx, const RegisterType rtype,
                                        const int regnum)
{
    reglist_insert(ctx, &ctx->defined_registers, rtype, regnum);
} // set_defined_register

static inline int writemask_xyzw(const int writemask)
{
    return (writemask == 0xF);  // 0xF == 1111. No explicit mask (full!).
} // writemask_xyzw


static inline int writemask_xyz(const int writemask)
{
    return (writemask == 0x7);  // 0x7 == 0111. (that is: xyz)
} // writemask_xyz


static inline int writemask_xy(const int writemask)
{
    return (writemask == 0x3);  // 0x3 == 0011. (that is: xy)
} // writemask_xy


static inline int writemask_x(const int writemask)
{
    return (writemask == 0x1);  // 0x1 == 0001. (that is: x)
} // writemask_x


static inline int writemask_y(const int writemask)
{
    return (writemask == 0x2);  // 0x1 == 0010. (that is: y)
} // writemask_y


static inline int replicate_swizzle(const int swizzle)
{
    return ( (((swizzle >> 0) & 0x3) == ((swizzle >> 2) & 0x3)) &&
             (((swizzle >> 2) & 0x3) == ((swizzle >> 4) & 0x3)) &&
             (((swizzle >> 4) & 0x3) == ((swizzle >> 6) & 0x3)) );
} // replicate_swizzle


static inline int no_swizzle(const int swizzle)
{
    return (swizzle == 0xE4);  // 0xE4 == 11100100 ... 0 1 2 3. No swizzle.
} // no_swizzle


static inline int vecsize_from_writemask(const int m)
{
    return (m & 1) + ((m >> 1) & 1) + ((m >> 2) & 1) + ((m >> 3) & 1);
} // vecsize_from_writemask


static inline void set_dstarg_writemask(DestArgInfo *dst, const int mask)
{
    dst->writemask = mask;
    dst->writemask0 = ((mask >> 0) & 1);
    dst->writemask1 = ((mask >> 1) & 1);
    dst->writemask2 = ((mask >> 2) & 1);
    dst->writemask3 = ((mask >> 3) & 1);
} // set_dstarg_writemask

int allocate_scratch_register(Context *ctx);
int allocate_branch_label(Context *ctx);

int isscalar(Context *ctx, const MOJOSHADER_shaderType shader_type,
                    const RegisterType rtype, const int rnum);

const char *get_D3D_register_string(Context *ctx,
                                           RegisterType regtype,
                                           int regnum, char *regnum_str,
                                           size_t regnum_size);

static const char swizzle_channels[] = { 'x', 'y', 'z', 'w' };

static const char *usagestrs[] = {
    "_position", "_blendweight", "_blendindices", "_normal", "_psize",
    "_texcoord", "_tangent", "_binormal", "_tessfactor", "_positiont",
    "_color", "_fog", "_depth", "_sample"
};

// d3d-specific parts that are used by arb1
const char *get_D3D_varname_in_buf(Context *ctx, RegisterType rt,
                                           int regnum, char *buf,
                                           const size_t len);
const char *get_D3D_varname(Context *ctx, RegisterType rt, int regnum);

#endif