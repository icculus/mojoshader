/**
 * MojoShader; generate shader programs from bytecode of compiled
 *  Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

#ifndef _INCL_MOJOSHADER_H_
#define _INCL_MOJOSHADER_H_

#ifdef __cplusplus
extern "C" {
#endif

/* You can define this if you aren't generating mojoshader_version.h */
#ifndef MOJOSHADER_NO_VERSION_INCLUDE
#include "mojoshader_version.h"
#endif

#ifndef MOJOSHADER_VERSION
#define MOJOSHADER_VERSION -1
#endif

#ifndef MOJOSHADER_CHANGESET
#define MOJOSHADER_CHANGESET "???"
#endif

#ifndef DECLSPEC
#ifdef _WIN32
#define DECLSPEC __declspec(dllexport)
#else
#define DECLSPEC
#endif
#endif

#ifndef MOJOSHADERCALL
#ifdef _WIN32
#define MOJOSHADERCALL __cdecl
#else
#define MOJOSHADERCALL
#endif
#endif

/* -Wpedantic nameless union/struct silencing */
#ifndef MOJOSHADERNAMELESS
#ifdef __GNUC__
#define MOJOSHADERNAMELESS __extension__
#else
#define MOJOSHADERNAMELESS
#endif /* __GNUC__ */
#endif /* MOJOSHADERNAMELESS */

/*
 * For determining the version of MojoShader you are using:
 *    const int compiled_against = MOJOSHADER_VERSION;
 *    const int linked_against = MOJOSHADER_version();
 *
 * The version is a single integer that increments, not a major/minor value.
 *
 * Please note that since moving to git, this function always returns -1;
 *  when hosted in Mercurial, this number meant something, but even
 *  there it wasn't reliable. With git, this number no longer exists at all.
 */
DECLSPEC int MOJOSHADER_version(void);

/*
 * For determining the revision control changeset of MojoShader you are using:
 *    const char *compiled_against = MOJOSHADER_CHANGESET;
 *    const char *linked_against = MOJOSHADER_changeset();
 *
 * The version is an arbitrary, null-terminated ASCII string. It is probably
 *  a hash that represents a revision control changeset, and can't be
 *  compared to any other string to determine chronology.
 *
 * Do not attempt to free this string; it's statically allocated.
 */
DECLSPEC const char *MOJOSHADER_changeset(void);

/*
 * These allocators work just like the C runtime's malloc() and free()
 *  (in fact, they probably use malloc() and free() internally if you don't
 *  specify your own allocator, but don't rely on that behaviour).
 * (data) is the pointer you supplied when specifying these allocator
 *  callbacks, in case you need instance-specific data...it is passed through
 *  to your allocator unmolested, and can be NULL if you like.
 */
typedef void *(MOJOSHADERCALL *MOJOSHADER_malloc)(int bytes, void *data);
typedef void (MOJOSHADERCALL *MOJOSHADER_free)(void *ptr, void *data);


/*
 * These are enum values, but they also can be used in bitmasks, so we can
 *  test if an opcode is acceptable: if (op->shader_types & ourtype) {} ...
 */
typedef enum
{
    MOJOSHADER_TYPE_UNKNOWN  = 0,
    MOJOSHADER_TYPE_PIXEL    = (1 << 0),
    MOJOSHADER_TYPE_VERTEX   = (1 << 1),
    MOJOSHADER_TYPE_GEOMETRY = (1 << 2),  /* (not supported yet.) */
    MOJOSHADER_TYPE_ANY = 0x7FFFFFFF   /* used for bitmasks */
} MOJOSHADER_shaderType;

/*
 * Data types for vertex attribute streams.
 */
typedef enum
{
    MOJOSHADER_ATTRIBUTE_UNKNOWN = -1,  /* housekeeping; not returned. */
    MOJOSHADER_ATTRIBUTE_BYTE,
    MOJOSHADER_ATTRIBUTE_UBYTE,
    MOJOSHADER_ATTRIBUTE_SHORT,
    MOJOSHADER_ATTRIBUTE_USHORT,
    MOJOSHADER_ATTRIBUTE_INT,
    MOJOSHADER_ATTRIBUTE_UINT,
    MOJOSHADER_ATTRIBUTE_FLOAT,
    MOJOSHADER_ATTRIBUTE_DOUBLE,
    MOJOSHADER_ATTRIBUTE_HALF_FLOAT  /* MAYBE available in your OpenGL! */
} MOJOSHADER_attributeType;

/*
 * Data types for uniforms. See MOJOSHADER_uniform for more information.
 */
typedef enum
{
    MOJOSHADER_UNIFORM_UNKNOWN = -1, /* housekeeping value; never returned. */
    MOJOSHADER_UNIFORM_FLOAT,
    MOJOSHADER_UNIFORM_INT,
    MOJOSHADER_UNIFORM_BOOL
} MOJOSHADER_uniformType;

/*
 * These are the uniforms to be set for a shader. "Uniforms" are what Direct3D
 *  calls "Constants" ... IDirect3DDevice::SetVertexShaderConstantF() would
 *  need this data, for example. These integers are register indexes. So if
 *  index==6 and type==MOJOSHADER_UNIFORM_FLOAT, that means we'd expect a
 *  4-float vector to be specified for what would be register "c6" in D3D
 *  assembly language, before drawing with the shader.
 * (array_count) means this is an array of uniforms...this happens in some
 *  profiles when we see a relative address ("c0[a0.x]", not the usual "c0").
 *  In those cases, the shader was built to set some range of constant
 *  registers as an array. You should set this array with (array_count)
 *  elements from the constant register file, starting at (index) instead of
 *  just a single uniform. To be extra difficult, you'll need to fill in the
 *  correct values from the MOJOSHADER_constant data into the appropriate
 *  parts of the array, overriding the constant register file. Fun!
 * (constant) says whether this is a constant array; these need to be loaded
 *  once at creation time, from the constant list and not ever updated from
 *  the constant register file. This is a workaround for limitations in some
 *  profiles.
 * (name) is a profile-specific variable name; it may be NULL if it isn't
 *  applicable to the requested profile.
 */
typedef struct MOJOSHADER_uniform
{
    MOJOSHADER_uniformType type;
    int index;
    int array_count;
    int constant;
    const char *name;
} MOJOSHADER_uniform;

/*
 * These are the constants defined in a shader. These are data values
 *  hardcoded in a shader (with the DEF, DEFI, DEFB instructions), which
 *  override your Uniforms. This data is largely for informational purposes,
 *  since they are compiled in and can't be changed, like Uniforms can be.
 * These integers are register indexes. So if index==6 and
 *  type==MOJOSHADER_UNIFORM_FLOAT, that means we'd expect a 4-float vector
 *  to be specified for what would be register "c6" in D3D assembly language,
 *  before drawing with the shader.
 * (value) is the value of the constant, unioned by type.
 */
typedef struct MOJOSHADER_constant
{
    MOJOSHADER_uniformType type;
    int index;
    union
    {
        float f[4];  /* if type==MOJOSHADER_UNIFORM_FLOAT */
        int i[4];    /* if type==MOJOSHADER_UNIFORM_INT */
        int b;       /* if type==MOJOSHADER_UNIFORM_BOOL */
    } value;
} MOJOSHADER_constant;

/*
 * Data types for samplers. See MOJOSHADER_sampler for more information.
 */
typedef enum
{
    MOJOSHADER_SAMPLER_UNKNOWN = -1, /* housekeeping value; never returned. */
    MOJOSHADER_SAMPLER_2D,
    MOJOSHADER_SAMPLER_CUBE,
    MOJOSHADER_SAMPLER_VOLUME
} MOJOSHADER_samplerType;

/*
 * These are the samplers to be set for a shader. ...
 *  IDirect3DDevice::SetTexture() would need this data, for example.
 * These integers are the sampler "stage". So if index==6 and
 *  type==MOJOSHADER_SAMPLER_2D, that means we'd expect a regular 2D texture
 *  to be specified for what would be register "s6" in D3D assembly language,
 *  before drawing with the shader.
 * (name) is a profile-specific variable name; it may be NULL if it isn't
 *  applicable to the requested profile.
 * (texbem) will be non-zero if a TEXBEM opcode references this sampler. This
 *  is only used in legacy shaders (ps_1_1 through ps_1_3), but it needs some
 *  special support to work, as we have to load a magic uniform behind the
 *  scenes to support it. Most code can ignore this field in general, and no
 *  one has to touch it unless they really know what they're doing.
 */
typedef struct MOJOSHADER_sampler
{
    MOJOSHADER_samplerType type;
    int index;
    const char *name;
    int texbem;
} MOJOSHADER_sampler;


/*
 * This struct is used if you have to force a sampler to a specific type.
 *  Generally, you can ignore this, but if you have, say, a ps_1_1
 *  shader, you might need to specify what the samplers are meant to be
 *  to get correct results, as Shader Model 1 samples textures according
 *  to what is bound to a sampler at the moment instead of what the shader
 *  is hardcoded to expect.
 */
typedef struct MOJOSHADER_samplerMap
{
    int index;
    MOJOSHADER_samplerType type;
} MOJOSHADER_samplerMap;

/*
 * Data types for attributes. See MOJOSHADER_attribute for more information.
 */
typedef enum
{
    MOJOSHADER_USAGE_UNKNOWN = -1,  /* housekeeping value; never returned. */
    MOJOSHADER_USAGE_POSITION,      /* 0-15 for Vertex, 1-15 for Pixel */
    MOJOSHADER_USAGE_BLENDWEIGHT,   /* 0-15 */
    MOJOSHADER_USAGE_BLENDINDICES,  /* 0-15 */
    MOJOSHADER_USAGE_NORMAL,        /* 0-15 */
    MOJOSHADER_USAGE_POINTSIZE,     /* 0-15 */
    MOJOSHADER_USAGE_TEXCOORD,      /* 0-15 */
    MOJOSHADER_USAGE_TANGENT,       /* 0-15 */
    MOJOSHADER_USAGE_BINORMAL,      /* 0-15 */
    MOJOSHADER_USAGE_TESSFACTOR,    /* 0 only */
    MOJOSHADER_USAGE_POSITIONT,     /* 0-15 for Vertex, 1-15 for Pixel */
    MOJOSHADER_USAGE_COLOR,         /* 0-15 but depends on MRT support */
    MOJOSHADER_USAGE_FOG,           /* 0-15 */
    MOJOSHADER_USAGE_DEPTH,         /* 0-15 */
    MOJOSHADER_USAGE_SAMPLE,
    MOJOSHADER_USAGE_TOTAL   /* housekeeping value; never returned. */
} MOJOSHADER_usage;

/*
 * These are the attributes to be set for a shader. "Attributes" are what
 *  Direct3D calls "Vertex Declarations Usages" ...
 *  IDirect3DDevice::CreateVertexDeclaration() would need this data, for
 *  example. Each attribute is associated with an array of data that uses one
 *  element per-vertex. So if usage==MOJOSHADER_USAGE_COLOR and index==1, that
 *  means we'd expect a secondary color array to be bound to this shader
 *  before drawing.
 * (name) is a profile-specific variable name; it may be NULL if it isn't
 *  applicable to the requested profile.
 */
typedef struct MOJOSHADER_attribute
{
    MOJOSHADER_usage usage;
    int index;
    const char *name;
} MOJOSHADER_attribute;

/*
 * Use this if you want to specify newly-parsed code to swizzle incoming
 *  data. This can be useful if you know, at parse time, that a shader
 *  will be processing data on COLOR0 that should be RGBA, but you'll
 *  be passing it a vertex array full of ARGB instead.
 */
typedef struct MOJOSHADER_swizzle
{
    MOJOSHADER_usage usage;
    unsigned int index;
    unsigned char swizzles[4];  /* {0,1,2,3} == .xyzw, {2,2,2,2} == .zzzz */
} MOJOSHADER_swizzle;


/*
 * MOJOSHADER_symbol data.
 *
 * These are used to expose high-level information in shader bytecode.
 *  They associate HLSL variables with registers. This data is used for both
 *  debugging and optimization.
 */

typedef enum
{
    MOJOSHADER_SYMREGSET_BOOL=0,
    MOJOSHADER_SYMREGSET_INT4,
    MOJOSHADER_SYMREGSET_FLOAT4,
    MOJOSHADER_SYMREGSET_SAMPLER,
    MOJOSHADER_SYMREGSET_TOTAL    /* housekeeping value; never returned. */
} MOJOSHADER_symbolRegisterSet;

typedef enum
{
    MOJOSHADER_SYMCLASS_SCALAR=0,
    MOJOSHADER_SYMCLASS_VECTOR,
    MOJOSHADER_SYMCLASS_MATRIX_ROWS,
    MOJOSHADER_SYMCLASS_MATRIX_COLUMNS,
    MOJOSHADER_SYMCLASS_OBJECT,
    MOJOSHADER_SYMCLASS_STRUCT,
    MOJOSHADER_SYMCLASS_TOTAL    /* housekeeping value; never returned. */
} MOJOSHADER_symbolClass;

typedef enum
{
    MOJOSHADER_SYMTYPE_VOID=0,
    MOJOSHADER_SYMTYPE_BOOL,
    MOJOSHADER_SYMTYPE_INT,
    MOJOSHADER_SYMTYPE_FLOAT,
    MOJOSHADER_SYMTYPE_STRING,
    MOJOSHADER_SYMTYPE_TEXTURE,
    MOJOSHADER_SYMTYPE_TEXTURE1D,
    MOJOSHADER_SYMTYPE_TEXTURE2D,
    MOJOSHADER_SYMTYPE_TEXTURE3D,
    MOJOSHADER_SYMTYPE_TEXTURECUBE,
    MOJOSHADER_SYMTYPE_SAMPLER,
    MOJOSHADER_SYMTYPE_SAMPLER1D,
    MOJOSHADER_SYMTYPE_SAMPLER2D,
    MOJOSHADER_SYMTYPE_SAMPLER3D,
    MOJOSHADER_SYMTYPE_SAMPLERCUBE,
    MOJOSHADER_SYMTYPE_PIXELSHADER,
    MOJOSHADER_SYMTYPE_VERTEXSHADER,
    MOJOSHADER_SYMTYPE_PIXELFRAGMENT,
    MOJOSHADER_SYMTYPE_VERTEXFRAGMENT,
    MOJOSHADER_SYMTYPE_UNSUPPORTED,
    MOJOSHADER_SYMTYPE_TOTAL    /* housekeeping value; never returned. */
} MOJOSHADER_symbolType;

typedef struct MOJOSHADER_symbolStructMember MOJOSHADER_symbolStructMember;

typedef struct MOJOSHADER_symbolTypeInfo
{
    MOJOSHADER_symbolClass parameter_class;
    MOJOSHADER_symbolType parameter_type;
    unsigned int rows;
    unsigned int columns;
    unsigned int elements;
    unsigned int member_count;
    MOJOSHADER_symbolStructMember *members;
} MOJOSHADER_symbolTypeInfo;

struct MOJOSHADER_symbolStructMember
{
    const char *name;
    MOJOSHADER_symbolTypeInfo info;
};

typedef struct MOJOSHADER_symbol
{
    const char *name;
    MOJOSHADER_symbolRegisterSet register_set;
    unsigned int register_index;
    unsigned int register_count;
    MOJOSHADER_symbolTypeInfo info;
} MOJOSHADER_symbol;


/*
 * These are used with MOJOSHADER_error as special case positions.
 */
#define MOJOSHADER_POSITION_NONE (-3)
#define MOJOSHADER_POSITION_BEFORE (-2)
#define MOJOSHADER_POSITION_AFTER (-1)

typedef struct MOJOSHADER_error
{
    /*
     * Human-readable error, if there is one. Will be NULL if there was no
     *  error. The string will be UTF-8 encoded, and English only. Most of
     *  these shouldn't be shown to the end-user anyhow.
     */
    const char *error;

    /*
     * Filename where error happened. This can be NULL if the information
     *  isn't available.
     */
    const char *filename;

    /*
     * Position of error, if there is one. Will be MOJOSHADER_POSITION_NONE if
     *  there was no error, MOJOSHADER_POSITION_BEFORE if there was an error
     *  before processing started, and MOJOSHADER_POSITION_AFTER if there was
     *  an error during final processing. If >= 0, MOJOSHADER_parse() sets
     *  this to the byte offset (starting at zero) into the bytecode you
     *  supplied, and MOJOSHADER_assemble(), MOJOSHADER_parseAst(), and
     *  MOJOSHADER_compile() sets this to a a line number in the source code
     *  you supplied (starting at one).
     */
    int error_position;
} MOJOSHADER_error;


/* !!! FIXME: document me. */
typedef enum MOJOSHADER_preshaderOpcode
{
    MOJOSHADER_PRESHADEROP_NOP,
    MOJOSHADER_PRESHADEROP_MOV,
    MOJOSHADER_PRESHADEROP_NEG,
    MOJOSHADER_PRESHADEROP_RCP,
    MOJOSHADER_PRESHADEROP_FRC,
    MOJOSHADER_PRESHADEROP_EXP,
    MOJOSHADER_PRESHADEROP_LOG,
    MOJOSHADER_PRESHADEROP_RSQ,
    MOJOSHADER_PRESHADEROP_SIN,
    MOJOSHADER_PRESHADEROP_COS,
    MOJOSHADER_PRESHADEROP_ASIN,
    MOJOSHADER_PRESHADEROP_ACOS,
    MOJOSHADER_PRESHADEROP_ATAN,
    MOJOSHADER_PRESHADEROP_MIN,
    MOJOSHADER_PRESHADEROP_MAX,
    MOJOSHADER_PRESHADEROP_LT,
    MOJOSHADER_PRESHADEROP_GE,
    MOJOSHADER_PRESHADEROP_ADD,
    MOJOSHADER_PRESHADEROP_MUL,
    MOJOSHADER_PRESHADEROP_ATAN2,
    MOJOSHADER_PRESHADEROP_DIV,
    MOJOSHADER_PRESHADEROP_CMP,
    MOJOSHADER_PRESHADEROP_MOVC,
    MOJOSHADER_PRESHADEROP_DOT,
    MOJOSHADER_PRESHADEROP_NOISE,
    MOJOSHADER_PRESHADEROP_SCALAR_OPS,
    MOJOSHADER_PRESHADEROP_MIN_SCALAR = MOJOSHADER_PRESHADEROP_SCALAR_OPS,
    MOJOSHADER_PRESHADEROP_MAX_SCALAR,
    MOJOSHADER_PRESHADEROP_LT_SCALAR,
    MOJOSHADER_PRESHADEROP_GE_SCALAR,
    MOJOSHADER_PRESHADEROP_ADD_SCALAR,
    MOJOSHADER_PRESHADEROP_MUL_SCALAR,
    MOJOSHADER_PRESHADEROP_ATAN2_SCALAR,
    MOJOSHADER_PRESHADEROP_DIV_SCALAR,
    MOJOSHADER_PRESHADEROP_DOT_SCALAR,
    MOJOSHADER_PRESHADEROP_NOISE_SCALAR
} MOJOSHADER_preshaderOpcode;

typedef enum MOJOSHADER_preshaderOperandType
{
    MOJOSHADER_PRESHADEROPERAND_INPUT,
    MOJOSHADER_PRESHADEROPERAND_OUTPUT,
    MOJOSHADER_PRESHADEROPERAND_LITERAL,
    MOJOSHADER_PRESHADEROPERAND_TEMP
} MOJOSHADER_preshaderOperandType;

typedef struct MOJOSHADER_preshaderOperand
{
    MOJOSHADER_preshaderOperandType type;
    unsigned int index;
    unsigned int array_register_count;
    unsigned int *array_registers;
} MOJOSHADER_preshaderOperand;

typedef struct MOJOSHADER_preshaderInstruction
{
    MOJOSHADER_preshaderOpcode opcode;
    unsigned int element_count;
    unsigned int operand_count;
    MOJOSHADER_preshaderOperand operands[4];
} MOJOSHADER_preshaderInstruction;

typedef struct MOJOSHADER_preshader
{
    unsigned int literal_count;
    double *literals;
    unsigned int temp_count;  /* scalar, not vector! */
    unsigned int symbol_count;
    MOJOSHADER_symbol *symbols;
    unsigned int instruction_count;
    MOJOSHADER_preshaderInstruction *instructions;
    unsigned int register_count;
    float *registers;
    MOJOSHADER_malloc malloc;
    MOJOSHADER_free free;
    void *malloc_data;
} MOJOSHADER_preshader;

/*
 * Structure used to return data from parsing of a shader...
 */
/* !!! FIXME: most of these ints should be unsigned. */
typedef struct MOJOSHADER_parseData
{
    /*
     * The number of elements pointed to by (errors).
     */
    int error_count;

    /*
     * (error_count) elements of data that specify errors that were generated
     *  by parsing this shader.
     * This can be NULL if there were no errors or if (error_count) is zero.
     */
    MOJOSHADER_error *errors;

    /*
     * The name of the profile used to parse the shader. Will be NULL on error.
     */
    const char *profile;

    /*
     * Bytes of output from parsing. Most profiles produce a string of source
     *  code, but profiles that do binary output may not be text at all.
     *  Will be NULL on error.
     */
    const char *output;

    /*
     * Byte count for output, not counting any null terminator. Most profiles
     *  produce an ASCII string of source code (which will be null-terminated
     *  even though that null char isn't included in output_len), but profiles
     *  that do binary output may not be text at all. Will be 0 on error.
     */
    int output_len;

    /*
     * Count of Direct3D instruction slots used. This is meaningless in terms
     *  of the actual output, as the profile will probably grow or reduce
     *  the count (or for high-level languages, not have that information at
     *  all). Also, as with Microsoft's own assembler, this value is just a
     *  rough estimate, as unpredicable real-world factors make the actual
     *  value vary at least a little from this count. Still, it can give you
     *  a rough idea of the size of your shader. Will be zero on error.
     */
    int instruction_count;

    /*
     * The type of shader we parsed. Will be MOJOSHADER_TYPE_UNKNOWN on error.
     */
    MOJOSHADER_shaderType shader_type;

    /*
     * The shader's major version. If this was a "vs_3_0", this would be 3.
     */
    int major_ver;

    /*
     * The shader's minor version. If this was a "ps_1_4", this would be 4.
     *  Two notes: for "vs_2_x", this is 1, and for "vs_3_sw", this is 255.
     */
    int minor_ver;

    /*
     * This is the main function name of the shader. This will be the
     *  caller-supplied string even if a given profile ignores it (GLSL,
     *  for example, always uses "main" in the shader output out of necessity,
     *  and Direct3D assembly has no concept of a "main function", etc).
     *  Otherwise, it'll be a default name chosen by the profile ("main") or
     *  whatnot.
     */
    const char *mainfn;

    /*
     * The number of elements pointed to by (uniforms).
     */
    int uniform_count;

    /*
     * (uniform_count) elements of data that specify Uniforms to be set for
     *  this shader. See discussion on MOJOSHADER_uniform for details.
     * This can be NULL on error or if (uniform_count) is zero.
     */
    MOJOSHADER_uniform *uniforms;

    /*
     * The number of elements pointed to by (constants).
     */
    int constant_count;

    /*
     * (constant_count) elements of data that specify constants used in
     *  this shader. See discussion on MOJOSHADER_constant for details.
     * This can be NULL on error or if (constant_count) is zero.
     *  This is largely informational: constants are hardcoded into a shader.
     *  The constants that you can set like parameters are in the "uniforms"
     *  list.
     */
    MOJOSHADER_constant *constants;

    /*
     * The number of elements pointed to by (samplers).
     */
    int sampler_count;

    /*
     * (sampler_count) elements of data that specify Samplers to be set for
     *  this shader. See discussion on MOJOSHADER_sampler for details.
     * This can be NULL on error or if (sampler_count) is zero.
     */
    MOJOSHADER_sampler *samplers;

    /* !!! FIXME: this should probably be "input" and not "attribute" */
    /*
     * The number of elements pointed to by (attributes).
     */
    int attribute_count;

    /* !!! FIXME: this should probably be "input" and not "attribute" */
    /*
     * (attribute_count) elements of data that specify Attributes to be set
     *  for this shader. See discussion on MOJOSHADER_attribute for details.
     * This can be NULL on error or if (attribute_count) is zero.
     */
    MOJOSHADER_attribute *attributes;

    /*
     * The number of elements pointed to by (outputs).
     */
    int output_count;

    /*
     * (output_count) elements of data that specify outputs this shader
     *  writes to. See discussion on MOJOSHADER_attribute for details.
     * This can be NULL on error or if (output_count) is zero.
     */
    MOJOSHADER_attribute *outputs;

    /*
     * The number of elements pointed to by (swizzles).
     */
    int swizzle_count;

    /* !!! FIXME: this should probably be "input" and not "attribute" */
    /*
     * (swizzle_count) elements of data that specify swizzles the shader will
     *  apply to incoming attributes. This is a copy of what was passed to
     *  MOJOSHADER_parseData().
     * This can be NULL on error or if (swizzle_count) is zero.
     */
    MOJOSHADER_swizzle *swizzles;

    /*
     * The number of elements pointed to by (symbols).
     */
    int symbol_count;

    /*
     * (symbol_count) elements of data that specify high-level symbol data
     *  for the shader. This will be parsed from the CTAB section
     *  in bytecode, and will be a copy of what you provide to
     *  MOJOSHADER_assemble(). This data is optional.
     * This can be NULL on error or if (symbol_count) is zero.
     */
    MOJOSHADER_symbol *symbols;

    /*
     * !!! FIXME: document me.
     * This can be NULL on error or if no preshader was available.
     */
    MOJOSHADER_preshader *preshader;

    /*
     * This is the malloc implementation you passed to MOJOSHADER_parse().
     */
    MOJOSHADER_malloc malloc;

    /*
     * This is the free implementation you passed to MOJOSHADER_parse().
     */
    MOJOSHADER_free free;

    /*
     * This is the pointer you passed as opaque data for your allocator.
     */
    void *malloc_data;
} MOJOSHADER_parseData;


/*
 * Profile string for Direct3D assembly language output.
 */
#define MOJOSHADER_PROFILE_D3D "d3d"

/*
 * Profile string for passthrough of the original bytecode, unchanged.
 */
#define MOJOSHADER_PROFILE_BYTECODE "bytecode"

/*
 * Profile string for HLSL Shader Model 4 output.
 */
#define MOJOSHADER_PROFILE_HLSL "hlsl"

/*
 * Profile string for GLSL: OpenGL high-level shader language output.
 */
#define MOJOSHADER_PROFILE_GLSL "glsl"

/*
 * Profile string for GLSL 1.20: minor improvements to base GLSL spec.
 */
#define MOJOSHADER_PROFILE_GLSL120 "glsl120"

/*
 * Profile string for GLSL ES: minor changes to GLSL output for ES compliance.
 */
#define MOJOSHADER_PROFILE_GLSLES "glsles"

/*
 * Profile string for GLSL ES: changes to GLSL output for ES 3.x compliance.
 */
#define MOJOSHADER_PROFILE_GLSLES3 "glsles3"

/*
 * Profile string for OpenGL ARB 1.0 shaders: GL_ARB_(vertex|fragment)_program.
 */
#define MOJOSHADER_PROFILE_ARB1 "arb1"

/*
 * Profile string for OpenGL ARB 1.0 shaders with Nvidia 2.0 extensions:
 *  GL_NV_vertex_program2_option and GL_NV_fragment_program2
 */
#define MOJOSHADER_PROFILE_NV2 "nv2"

/*
 * Profile string for OpenGL ARB 1.0 shaders with Nvidia 3.0 extensions:
 *  GL_NV_vertex_program3 and GL_NV_fragment_program2
 */
#define MOJOSHADER_PROFILE_NV3 "nv3"

/*
 * Profile string for OpenGL ARB 1.0 shaders with Nvidia 4.0 extensions:
 *  GL_NV_gpu_program4
 */
#define MOJOSHADER_PROFILE_NV4 "nv4"

/*
 * Profile string for Metal: Apple's lowlevel API's high-level shader language.
 */
#define MOJOSHADER_PROFILE_METAL "metal"

/*
 * Profile string for SPIR-V binary output
 */
#define MOJOSHADER_PROFILE_SPIRV "spirv"

/*
 * Profile string for ARB_gl_spirv-friendly SPIR-V binary output
 */
#define MOJOSHADER_PROFILE_GLSPIRV "glspirv"

/*
 * Determine the highest supported Shader Model for a profile.
 */
DECLSPEC int MOJOSHADER_maxShaderModel(const char *profile);


/*
 * Parse a compiled Direct3D shader's bytecode.
 *
 * This is your primary entry point into MojoShader. You need to pass it
 *  a compiled D3D shader and tell it which "profile" you want to use to
 *  convert it into useful data.
 *
 * The available profiles are the set of MOJOSHADER_PROFILE_* defines.
 *  Note that MojoShader may be built without support for all listed
 *  profiles (in which case using one here will return with an error).
 *
 * As parsing requires some memory to be allocated, you may provide a custom
 *  allocator to this function, which will be used to allocate/free memory.
 *  They function just like malloc() and free(). We do not use realloc().
 *  If you don't care, pass NULL in for the allocator functions. If your
 *  allocator needs instance-specific data, you may supply it with the
 *  (d) parameter. This pointer is passed as-is to your (m) and (f) functions.
 *
 * This function returns a MOJOSHADER_parseData.
 *
 * This function will never return NULL, even if the system is completely
 *  out of memory upon entry (in which case, this function returns a static
 *  MOJOSHADER_parseData object, which is still safe to pass to
 *  MOJOSHADER_freeParseData()).
 *
 * You can tell the generated program to swizzle certain inputs. If you know
 *  that COLOR0 should be RGBA but you're passing in ARGB, you can specify
 *  a swizzle of { MOJOSHADER_USAGE_COLOR, 0, {1,2,3,0} } to (swiz). If the
 *  input register in the code would produce reg.ywzx, that swizzle would
 *  change it to reg.wzxy ... (swiz) can be NULL.
 *
 * You can force the shader to expect samplers of certain types. Generally
 *  you don't need this, as Shader Model 2 and later always specify what they
 *  expect samplers to be (2D, cubemap, etc). Shader Model 1, however, just
 *  uses whatever is bound to a given sampler at draw time, but this doesn't
 *  work in OpenGL, etc. In these cases, MojoShader will default to
 *  2D texture sampling (or cubemap sampling, in cases where it makes sense,
 *  like the TEXM3X3TEX opcode), which works 75% of the time, but if you
 *  really needed something else, you'll need to specify it here. This can
 *  also be used, at your own risk, to override DCL opcodes in shaders: if
 *  the shader explicit says 2D, but you want Cubemap, for example, you can
 *  use this to override. If you aren't sure about any of this stuff, you can
 *  (and should) almost certainly ignore it: (smap) can be NULL.
 *
 * (bufsize) is the size in bytes of (tokenbuf). If (bufsize) is zero,
 *  MojoShader will attempt to figure out the size of the buffer, but you
 *  risk a buffer overflow if you have corrupt data, etc. Supply the value
 *  if you can.
 *
 * You should pass a name for your shader's main function in here, via the
 *  (mainfn) param. Some profiles need this name to be unique. Passing a NULL
 *  here will pick a reasonable default, and most profiles will ignore it
 *  anyhow. As the name of the shader's main function, etc, so make it a
 *  simple name that would match C's identifier rules. Keep it simple!
 *
 * This function is thread safe, so long as (m) and (f) are too, and that
 *  (tokenbuf) remains intact for the duration of the call. This allows you
 *  to parse several shaders on separate CPU cores at the same time.
 */
DECLSPEC const MOJOSHADER_parseData *MOJOSHADER_parse(const char *profile,
                                                      const char *mainfn,
                                                      const unsigned char *tokenbuf,
                                                      const unsigned int bufsize,
                                                      const MOJOSHADER_swizzle *swiz,
                                                      const unsigned int swizcount,
                                                      const MOJOSHADER_samplerMap *smap,
                                                      const unsigned int smapcount,
                                                      MOJOSHADER_malloc m,
                                                      MOJOSHADER_free f,
                                                      void *d);


/*
 * Call this to dispose of parsing results when you are done with them.
 *  This will call the MOJOSHADER_free function you provided to
 *  MOJOSHADER_parse multiple times, if you provided one.
 *  Passing a NULL here is a safe no-op.
 *
 * This function is thread safe, so long as any allocator you passed into
 *  MOJOSHADER_parse() is, too.
 */
DECLSPEC void MOJOSHADER_freeParseData(const MOJOSHADER_parseData *data);


/*
 * You almost certainly don't need this function, unless you absolutely know
 *  why you need it without hesitation. This is useful if you're doing
 *  extremely low-level shader work or building specialized tools.
 *
 * Parse a preshader structure. This expects a buffer of bytes that represents
 *  the preshader data starting with its magic number token and ending at
 *  the end of the comment tokens that contain this preshader. Note that it
 *  does _not_ start at the beginning of the comment tokens.
 *
 * On success, this will return a MOJOSHADER_preshader. This can be
 *  deallocated later by calling MOJOSHADER_freePreshader(). On failure,
 *  this will return NULL. Unlike other MojoShader APIs, this assumes you
 *  either have a complete and valid buffer of preshader tokens or you have
 *  incomplete/corrupted data, so there is no explicit error reporting. Please
 *  note that if the system runs out of memory, this function will also return
 *  NULL without distinction.
 *
 * This function is thread safe, so long as any allocator you passed into
 *  MOJOSHADER_parsePreshader() is, too.
 */
DECLSPEC const MOJOSHADER_preshader *MOJOSHADER_parsePreshader(const unsigned char *buf,
                                                               const unsigned int len,
                                                               MOJOSHADER_malloc m,
                                                               MOJOSHADER_free f,
                                                               void *d);


/*
 * You almost certainly don't need this function, unless you absolutely know
 *  why you need it without hesitation. This is useful if you're doing
 *  extremely low-level shader work or building specialized tools.
 *
 * Call this to dispose of preshader parsing results when you are done with
 *  them. This will call the MOJOSHADER_free function you provided to
 *  MOJOSHADER_parsePreshader() multiple times, if you provided one.
 *  Passing a NULL here is a safe no-op.
 *
 * You only need to call this function for results from a call to
 *  MOJOSHADER_parsePreshader(). Other MojoShader structures with a preshader
 *  field, such as MOJOSHADER_parseData(), should not use this function, as
 *  the preshader will be deallocated with everything else in
 *  MOJOSHADER_freeParseData(), etc.
 *
 * This function is thread safe, so long as any allocator you passed into
 *  MOJOSHADER_parsePreshader() is, too.
 */
DECLSPEC void MOJOSHADER_freePreshader(const MOJOSHADER_preshader *preshader);


/* SPIR-V interface... */

typedef enum
{
    MOJOSHADER_VERTEXELEMENTFORMAT_SINGLE,
    MOJOSHADER_VERTEXELEMENTFORMAT_VECTOR2,
    MOJOSHADER_VERTEXELEMENTFORMAT_VECTOR3,
    MOJOSHADER_VERTEXELEMENTFORMAT_VECTOR4,
    MOJOSHADER_VERTEXELEMENTFORMAT_COLOR,
    MOJOSHADER_VERTEXELEMENTFORMAT_BYTE4,
    MOJOSHADER_VERTEXELEMENTFORMAT_SHORT2,
    MOJOSHADER_VERTEXELEMENTFORMAT_SHORT4,
    MOJOSHADER_VERTEXELEMENTFORMAT_NORMALIZEDSHORT2,
    MOJOSHADER_VERTEXELEMENTFORMAT_NORMALIZEDSHORT4,
    MOJOSHADER_VERTEXELEMENTFORMAT_HALFVECTOR2,
    MOJOSHADER_VERTEXELEMENTFORMAT_HALFVECTOR4
} MOJOSHADER_vertexElementFormat;

typedef struct MOJOSHADER_vertexAttribute
{
    MOJOSHADER_usage usage;
    MOJOSHADER_vertexElementFormat vertexElementFormat;
    int usageIndex;
} MOJOSHADER_vertexAttribute;

/*
 * You almost certainly don't need this function, unless you absolutely know
 *  why you need it without hesitation. This is useful if you're doing
 *  extremely low-level shader work or building specialized tools.
 *
 * Call this to patch SPIR-V output returned from MOJOSHADER_parse to correctly
 *  interpret vertex input and link vertex shader output to pixel shader input.
 *
 * On success, this returns the size of the shaders' internal "patch table",
 *  which should be subtracted from the parseData's output_len when passing the
 *  final SPIR-V to your shader compiler.
 */
DECLSPEC int MOJOSHADER_linkSPIRVShaders(const MOJOSHADER_parseData *vertex_spirv,
                                         const MOJOSHADER_parseData *pixel_spirv,
                                         const MOJOSHADER_vertexAttribute *vertexAttributes,
                                         const int vertexAttributeCount);


/* OpenGL interface... */

/*
 * Signature for function lookup callbacks. MojoShader will call a function
 *  you provide to get OpenGL entry points (both standard functions and
 *  extensions). Through this, MojoShader never links directly to OpenGL,
 *  but relies on you to provide the implementation. This means you can
 *  swap in different drivers, or hook functions (log every GL call MojoShader
 *  makes, etc).
 *
 * (fnname) is the function name we want the address for ("glBegin" or
 *  whatever. (data) is a void pointer you provide, if this callback needs
 *  extra information. If you don't need it, you may specify NULL.
 *
 * Return the entry point on success, NULL if it couldn't be found.
 *  Note that this could ask for standard entry points like glEnable(), or
 *  extensions like glProgramLocalParameterI4ivNV(), so you might need
 *  to check two places to find the desired entry point, depending on your
 *  platform (Windows might need to look in OpenGL32.dll and use WGL, etc).
 */
typedef void *(MOJOSHADERCALL *MOJOSHADER_glGetProcAddress)(const char *fnname, void *data);


/*
 * "Contexts" map to OpenGL contexts...you need one per window, or whatever,
 *  and need to inform MojoShader when you make a new one current.
 *
 * "Shaders" refer to individual vertex or pixel programs, and are created
 *  by "compiling" Direct3D shader bytecode. A vertex and pixel shader are
 *  "linked" into a "Program" before you can use them to render.
 *
 * To the calling application, these are all opaque handles.
 */
typedef struct MOJOSHADER_glContext MOJOSHADER_glContext;
typedef struct MOJOSHADER_glShader MOJOSHADER_glShader;
typedef struct MOJOSHADER_glProgram MOJOSHADER_glProgram;


/*
 * Get a list of available profiles. This will fill in the array (profs)
 *  with up to (size) pointers of profiles that the current system can handle;
 *  that is, the profiles are built into MojoShader and the OpenGL extensions
 *  required for them exist at runtime. This function returns the number of
 *  available profiles, which may be more, less, or equal to (size).
 *
 * If there are more than (size) profiles, the (profs) buffer will not
 *  overflow. You can check the return value for the total number of
 *  available profiles, allocate more space, and try again if necessary.
 *  Calling this function with (size) == 0 is legal.
 *
 * You can only call this AFTER you have successfully built your GL context
 *  and made it current. This function will lookup the GL functions it needs
 *  through the callback you supply, via (lookup) and (lookup_d). The lookup
 *  function is neither stored nor used by MojoShader after this function
 *  returns, nor are the functions it might look up.
 *
 * As MojoShader requires some memory to be allocated, you may provide a
 *  custom allocator to this function, which will be used to allocate/free
 *  memory. They function just like malloc() and free(). We do not use
 *  realloc(). If you don't care, pass NULL in for the allocator functions.
 *  If your allocator needs instance-specific data, you may supply it with the
 *  (malloc_d) parameter. This pointer is passed as-is to your (m) and (f)
 *  functions.
 *
 * You should not free any strings returned from this function; they are
 *  pointers to internal, probably static, memory.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 */
DECLSPEC int MOJOSHADER_glAvailableProfiles(MOJOSHADER_glGetProcAddress lookup,
                                            void *lookup_d,
                                            const char **profs, const int size,
                                            MOJOSHADER_malloc m, MOJOSHADER_free f,
                                            void *malloc_d);


/*
 * Determine the best profile to use for the current system.
 *
 * You can only call this AFTER you have successfully built your GL context
 *  and made it current. This function will lookup the GL functions it needs
 *  through the callback you supply via (lookup) and (lookup_d). The lookup
 *  function is neither stored nor used by MojoShader after this function
 *  returns, nor are the functions it might look up.
 *
 * Returns the name of the "best" profile on success, NULL if none of the
 *  available profiles will work on this system. "Best" is a relative term,
 *  but it generally means the best trade off between feature set and
 *  performance. The selection algorithm may be arbitrary and complex.
 *
 * As MojoShader requires some memory to be allocated, you may provide a
 *  custom allocator to this function, which will be used to allocate/free
 *  memory. They function just like malloc() and free(). We do not use
 *  realloc(). If you don't care, pass NULL in for the allocator functions.
 *  If your allocator needs instance-specific data, you may supply it with the
 *  (malloc_d) parameter. This pointer is passed as-is to your (m) and (f)
 *  functions.
 *
 * The returned value is an internal static string, and should not be free()'d
 *  by the caller. If you get a NULL, calling MOJOSHADER_glGetError() might
 *  shed some light on why.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 */
DECLSPEC const char *MOJOSHADER_glBestProfile(MOJOSHADER_glGetProcAddress lookup,
                                   void *lookup_d,
                                   MOJOSHADER_malloc m, MOJOSHADER_free f,
                                   void *malloc_d);

/*
 * Prepare MojoShader to manage OpenGL shaders.
 *
 * You do not need to call this if all you want is MOJOSHADER_parse().
 *
 * You must call this once AFTER you have successfully built your GL context
 *  and made it current. This function will lookup the GL functions it needs
 *  through the callback you supply via (lookup) and (lookup_d), after which
 *  it may call them at any time up until you call
 *  MOJOSHADER_glDestroyContext(). The lookup function is neither stored nor
 *  used by MojoShader after this function returns.
 *
 * (profile) is an OpenGL-specific MojoShader profile, which decides how
 *  Direct3D bytecode shaders get turned into OpenGL programs, and how they
 *  are fed to the GL.
 *
 * (lookup) is a callback that is used to load GL entry points. This callback
 *  has to look up base GL functions and extension entry points. The pointer
 *  you supply in (lookup_d) is passed as-is to the callback.
 *
 * As MojoShader requires some memory to be allocated, you may provide a
 *  custom allocator to this function, which will be used to allocate/free
 *  memory. They function just like malloc() and free(). We do not use
 *  realloc(). If you don't care, pass NULL in for the allocator functions.
 *  If your allocator needs instance-specific data, you may supply it with the
 *  (malloc_d) parameter. This pointer is passed as-is to your (m) and (f)
 *  functions.
 *
 * Returns a new context on success, NULL on error. If you get a new context,
 *  you need to make it current before using it with
 *  MOJOSHADER_glMakeContextCurrent().
 *
 * This call is NOT thread safe! It must return success before you may call
 *  any other MOJOSHADER_gl* function. Also, as most OpenGL implementations
 *  are not thread safe, you should probably only call this from the same
 *  thread that created the GL context.
 */
DECLSPEC MOJOSHADER_glContext *MOJOSHADER_glCreateContext(const char *profile,
                                        MOJOSHADER_glGetProcAddress lookup,
                                        void *lookup_d,
                                        MOJOSHADER_malloc m, MOJOSHADER_free f,
                                        void *malloc_d);

/*
 * You must call this before using the context that you got from
 *  MOJOSHADER_glCreateContext(), and must use it when you switch to a new GL
 *  context.
 *
 * You can only have one MOJOSHADER_glContext per actual GL context, or
 *  undefined behaviour will result.
 *
 * It is legal to call this with a NULL pointer to make no context current,
 *  but you need a valid context to be current to use most of MojoShader.
 *
 * In current times, this allows one context to be current _per thread_,
 *  as the internal state inside MojoShader is marked thread local. Each
 *  new thread should call this function before using the context, even if
 *  other threads have previously set it current. You _also_ have to set
 *  the OpenGL context itself current for each thread (and have an OpenGL
 *  implementation that allows that in the first place).
 */
DECLSPEC void MOJOSHADER_glMakeContextCurrent(MOJOSHADER_glContext *ctx);

/*
 * Get any error state we might have picked up. MojoShader will NOT call
 *  glGetError() internally, but there are other errors we can pick up,
 *  such as failed shader compilation, etc.
 *
 * Returns a human-readable string. This string is for debugging purposes, and
 *  not guaranteed to be localized, coherent, or user-friendly in any way.
 *  It's for programmers!
 *
 * The latest error may remain between calls. New errors replace any existing
 *  error. Don't check this string for a sign that an error happened, check
 *  return codes instead and use this for explanation when debugging.
 *
 * Do not free the returned string: it's a pointer to a static internal
 *  buffer. Do not keep the pointer around, either, as it's likely to become
 *  invalid as soon as you call into MojoShader again.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call does NOT require a valid MOJOSHADER_glContext to have been made
 *  current. The error buffer is shared between contexts, so you can get
 *  error results from a failed MOJOSHADER_glCreateContext().
 */
DECLSPEC const char *MOJOSHADER_glGetError(void);

/*
 * Get the maximum uniforms a shader can support for the current GL context,
 *  MojoShader profile, and shader type. You can use this to make decisions
 *  about what shaders you want to use (for example, a less complicated
 *  shader may be swapped in for lower-end systems).
 *
 * Returns the number, or -1 on error.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 */
DECLSPEC int MOJOSHADER_glMaxUniforms(MOJOSHADER_shaderType shader_type);

/*
 * Compile a buffer of Direct3D shader bytecode into an OpenGL shader.
 *  You still need to link the shader before you may render with it.
 *
 *   (tokenbuf) is a buffer of Direct3D shader bytecode.
 *   (bufsize) is the size, in bytes, of the bytecode buffer.
 *   (swiz), (swizcount), (smap), and (smapcount) are passed to
 *   MOJOSHADER_parse() unmolested.
 *
 * Returns NULL on error, or a shader handle on success.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Compiled shaders from this function may not be shared between contexts.
 */
DECLSPEC MOJOSHADER_glShader *MOJOSHADER_glCompileShader(const unsigned char *tokenbuf,
                                                         const unsigned int bufsize,
                                                         const MOJOSHADER_swizzle *swiz,
                                                         const unsigned int swizcount,
                                                         const MOJOSHADER_samplerMap *smap,
                                                         const unsigned int smapcount);

/*
 * Increments a shader's internal refcount. To decrement the refcount, call
 *  MOJOSHADER_glDeleteShader().
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 */
DECLSPEC void MOJOSHADER_glShaderAddRef(MOJOSHADER_glShader *shader);

/*
 * Get the MOJOSHADER_parseData structure that was produced from the
 *  call to MOJOSHADER_glCompileShader().
 *
 * This data is read-only, and you should NOT attempt to free it. This
 *  pointer remains valid until the shader is deleted.
 */
DECLSPEC const MOJOSHADER_parseData *MOJOSHADER_glGetShaderParseData(
                                                MOJOSHADER_glShader *shader);
/*
 * Link a vertex and pixel shader into an OpenGL program.
 *  (vshader) or (pshader) can be NULL, to specify that the GL should use the
 *  fixed-function pipeline instead of the programmable pipeline for that
 *  portion of the work. You can reuse shaders in various combinations across
 *  multiple programs, by relinking different pairs.
 *
 * It is illegal to give a vertex shader for (pshader) or a pixel shader
 *  for (vshader).
 *
 * Once you have successfully linked a program, you may render with it.
 *
 * Returns NULL on error, or a program handle on success.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Linked programs from this function may not be shared between contexts.
 */
DECLSPEC MOJOSHADER_glProgram *MOJOSHADER_glLinkProgram(MOJOSHADER_glShader *vshader,
                                                        MOJOSHADER_glShader *pshader);

/*
 * This binds the program (using, for example, glUseProgramObjectARB()), and
 *  disables all the client-side arrays so we can reset them with new values
 *  if appropriate.
 *
 * Call with NULL to disable the programmable pipeline and all enabled
 *  client-side arrays.
 *
 * After binding a program, you should update any uniforms you care about
 *  with MOJOSHADER_glSetVertexShaderUniformF() (etc), set any vertex arrays
 *  you want to use with MOJOSHADER_glSetVertexAttribute(), and finally call
 *  MOJOSHADER_glProgramReady() to commit everything to the GL. Then you may
 *  begin drawing through standard GL entry points.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 */
DECLSPEC void MOJOSHADER_glBindProgram(MOJOSHADER_glProgram *program);

/*
 * This binds individual shaders as if you had linked them with
 *  MOJOSHADER_glLinkProgram(), and used MOJOSHADER_glBindProgram() on the
 *  linked result.
 *
 * MojoShader will handle linking behind the scenes, and keep a cache of
 *  programs linked here. Programs are removed from this cache when one of the
 *  invidual shaders in it is deleted, otherwise they remain cached so future
 *  calls to this function don't need to relink a previously-used shader
 *  grouping.
 *
 * This function is for convenience, as the API is closer to how Direct3D
 *  works, and retrofitting linking into your app can be difficult;
 *  frequently, you just end up building your own cache, anyhow.
 *
 * Calling with all shaders set to NULL is equivalent to calling
 *  MOJOSHADER_glBindProgram(NULL).
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 */
DECLSPEC void MOJOSHADER_glBindShaders(MOJOSHADER_glShader *vshader,
                                       MOJOSHADER_glShader *pshader);

/*
 * This queries for the shaders currently bound to the active context.
 *
 * This function is only for convenience, specifically for compatibility with
 * the effects API.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 */
DECLSPEC void MOJOSHADER_glGetBoundShaders(MOJOSHADER_glShader **vshader,
                                           MOJOSHADER_glShader **pshader);

/*
 * Set a floating-point uniform value (what Direct3D calls a "constant").
 *
 * There is a single array of 4-float "registers" shared by all vertex shaders.
 *  This is the "c" register file in Direct3D (c0, c1, c2, etc...)
 *  MojoShader will take care of synchronizing this internal array with the
 *  appropriate variables in the GL shaders.
 *
 * (idx) is the index into the internal array: 0 is the first four floats,
 *  1 is the next four, etc.
 * (data) is a pointer to (vec4count*4) floats.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Uniforms are not shared between contexts.
 */
DECLSPEC void MOJOSHADER_glSetVertexShaderUniformF(unsigned int idx, const float *data,
                                                   unsigned int vec4count);

/*
 * Retrieve a floating-point uniform value (what Direct3D calls a "constant").
 *
 * There is a single array of 4-float "registers" shared by all vertex shaders.
 *  This is the "c" register file in Direct3D (c0, c1, c2, etc...)
 *  MojoShader will take care of synchronizing this internal array with the
 *  appropriate variables in the GL shaders.
 *
 * (idx) is the index into the internal array: 0 is the first four floats,
 *  1 is the next four, etc.
 * (data) is a pointer to space for (vec4count*4) floats.
 *  (data) will be filled will current values in the register file. Results
 *  are undefined if you request data past the end of the register file or
 *  previously uninitialized registers.
 *
 * This is a "fast" call; we're just reading from internal memory. We do not
 *  query the GPU or the GL for this information.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Uniforms are not shared between contexts.
 */
DECLSPEC void MOJOSHADER_glGetVertexShaderUniformF(unsigned int idx, float *data,
                                                   unsigned int vec4count);


/*
 * Set an integer uniform value (what Direct3D calls a "constant").
 *
 * There is a single array of 4-int "registers" shared by all vertex shaders.
 *  This is the "i" register file in Direct3D (i0, i1, i2, etc...)
 *  MojoShader will take care of synchronizing this internal array with the
 *  appropriate variables in the GL shaders.
 *
 * (idx) is the index into the internal array: 0 is the first four ints,
 *  1 is the next four, etc.
 * (data) is a pointer to (ivec4count*4) ints.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Uniforms are not shared between contexts.
 */
DECLSPEC void MOJOSHADER_glSetVertexShaderUniformI(unsigned int idx, const int *data,
                                                   unsigned int ivec4count);

/*
 * Retrieve an integer uniform value (what Direct3D calls a "constant").
 *
 * There is a single array of 4-int "registers" shared by all vertex shaders.
 *  This is the "i" register file in Direct3D (i0, i1, i2, etc...)
 *  MojoShader will take care of synchronizing this internal array with the
 *  appropriate variables in the GL shaders.
 *
 * (idx) is the index into the internal array: 0 is the first four ints,
 *  1 is the next four, etc.
 * (data) is a pointer to space for (ivec4count*4) ints.
 *  (data) will be filled will current values in the register file. Results
 *  are undefined if you request data past the end of the register file or
 *  previously uninitialized registers.
 *
 * This is a "fast" call; we're just reading from internal memory. We do not
 *  query the GPU or the GL for this information.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Uniforms are not shared between contexts.
 */
DECLSPEC void MOJOSHADER_glGetVertexShaderUniformI(unsigned int idx, int *data,
                                                   unsigned int ivec4count);

/*
 * Set a boolean uniform value (what Direct3D calls a "constant").
 *
 * There is a single array of "registers" shared by all vertex shaders.
 *  This is the "b" register file in Direct3D (b0, b1, b2, etc...)
 *  MojoShader will take care of synchronizing this internal array with the
 *  appropriate variables in the GL shaders.
 *
 * Unlike the float and int counterparts, booleans are single values, not
 *  four-element vectors...so idx==1 is the second boolean in the internal
 *  array, not the fifth.
 *
 * Non-zero values are considered "true" and zero is considered "false".
 *
 * (idx) is the index into the internal array.
 * (data) is a pointer to (bcount) ints.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Uniforms are not shared between contexts.
 */
DECLSPEC void MOJOSHADER_glSetVertexShaderUniformB(unsigned int idx, const int *data,
                                                   unsigned int bcount);

/*
 * Retrieve a boolean uniform value (what Direct3D calls a "constant").
 *
 * There is a single array of "registers" shared by all vertex shaders.
 *  This is the "b" register file in Direct3D (b0, b1, b2, etc...)
 *  MojoShader will take care of synchronizing this internal array with the
 *  appropriate variables in the GL shaders.
 *
 * Unlike the float and int counterparts, booleans are single values, not
 *  four-element vectors...so idx==1 is the second boolean in the internal
 *  array, not the fifth.
 *
 * Non-zero values are considered "true" and zero is considered "false".
 *  This function will always return true values as 1, regardless of what
 *  non-zero integer you originally used to set the registers.
 *
 * (idx) is the index into the internal array.
 * (data) is a pointer to space for (bcount) ints.
 *  (data) will be filled will current values in the register file. Results
 *  are undefined if you request data past the end of the register file or
 *  previously uninitialized registers.
 *
 * This is a "fast" call; we're just reading from internal memory. We do not
 *  query the GPU or the GL for this information.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Uniforms are not shared between contexts.
 */
DECLSPEC void MOJOSHADER_glGetVertexShaderUniformB(unsigned int idx, int *data,
                                                   unsigned int bcount);

/*
 * The equivalent of MOJOSHADER_glSetVertexShaderUniformF() for pixel
 *  shaders. Other than using a different internal array that is specific
 *  to pixel shaders, this functions just like its vertex array equivalent.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Uniforms are not shared between contexts.
 */
DECLSPEC void MOJOSHADER_glSetPixelShaderUniformF(unsigned int idx, const float *data,
                                                  unsigned int vec4count);


/*
 * The equivalent of MOJOSHADER_glGetVertexShaderUniformF() for pixel
 *  shaders. Other than using a different internal array that is specific
 *  to pixel shaders, this functions just like its vertex array equivalent.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Uniforms are not shared between contexts.
 */
DECLSPEC void MOJOSHADER_glGetPixelShaderUniformF(unsigned int idx, float *data,
                                                  unsigned int vec4count);


/*
 * The equivalent of MOJOSHADER_glSetVertexShaderUniformI() for pixel
 *  shaders. Other than using a different internal array that is specific
 *  to pixel shaders, this functions just like its vertex array equivalent.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Uniforms are not shared between contexts.
 */
DECLSPEC void MOJOSHADER_glSetPixelShaderUniformI(unsigned int idx, const int *data,
                                                  unsigned int ivec4count);


/*
 * The equivalent of MOJOSHADER_glGetVertexShaderUniformI() for pixel
 *  shaders. Other than using a different internal array that is specific
 *  to pixel shaders, this functions just like its vertex array equivalent.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Uniforms are not shared between contexts.
 */
DECLSPEC void MOJOSHADER_glGetPixelShaderUniformI(unsigned int idx, int *data,
                                                  unsigned int ivec4count);

/*
 * The equivalent of MOJOSHADER_glSetVertexShaderUniformB() for pixel
 *  shaders. Other than using a different internal array that is specific
 *  to pixel shaders, this functions just like its vertex array equivalent.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Uniforms are not shared between contexts.
 */
DECLSPEC void MOJOSHADER_glSetPixelShaderUniformB(unsigned int idx, const int *data,
                                                  unsigned int bcount);

/*
 * The equivalent of MOJOSHADER_glGetVertexShaderUniformB() for pixel
 *  shaders. Other than using a different internal array that is specific
 *  to pixel shaders, this functions just like its vertex array equivalent.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Uniforms are not shared between contexts.
 */
DECLSPEC void MOJOSHADER_glGetPixelShaderUniformB(unsigned int idx, int *data,
                                                  unsigned int bcount);

/*
 * Fills register pointers with pointers that are directly used to push uniform
 *  data to the GL shader context.
 *
 * This function is really just for the effects API, you should NOT be using
 *  this unless you know every single line of MojoShader from memory.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 */
DECLSPEC void MOJOSHADER_glMapUniformBufferMemory(float **vsf, int **vsi, unsigned char **vsb,
                                                  float **psf, int **psi, unsigned char **psb);

/*
 * Tells the context that you are done with the memory mapped by
 *  MOJOSHADER_glMapUniformBufferMemory().
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 */
DECLSPEC void MOJOSHADER_glUnmapUniformBufferMemory();

/*
 * Set up the vector for the TEXBEM opcode. Most apps can ignore this API.
 *
 * Shader Model 1.1 through 1.3 had an instruction for "fake bump mapping"
 *  called TEXBEM. To use it, you had to set some sampler states,
 *  D3DTSS_BUMPENVMATxx, which would be referenced by the opcode.
 *
 * This functionality was removed from Shader Model 1.4 and later, because
 *  it was special-purpose and limited. The functionality could be built on
 *  more general opcodes, and the sampler state could be supplied in a more
 *  general uniform.
 *
 * However, to support this opcode, we supply a way to specify that sampler
 *  state, and the OpenGL glue code does the right thing to pass that
 *  information to the shader.
 *
 * This call maps to IDirect3DDevice::SetTextureStageState() with the
 *  D3DTSS_BUMPENVMAT00, D3DTSS_BUMPENVMAT01, D3DTSS_BUMPENVMAT10,
 *  D3DTSS_BUMPENVMAT11, D3DTSS_BUMPENVLSCALE, and D3DTSS_BUMPENVLOFFSET
 *  targets. This is only useful for Shader Model < 1.4 pixel shaders, if
 *  they use the TEXBEM or TEXBEML opcode. If you aren't sure, you don't need
 *  this function.
 *
 * Like the rest of your uniforms, you must call MOJOSHADER_glProgramReady()
 *  between setting new values and drawing with them.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * These values are not shared between contexts.
 */
DECLSPEC void MOJOSHADER_glSetLegacyBumpMapEnv(unsigned int sampler, float mat00,
                                               float mat01, float mat10, float mat11,
                                               float lscale, float loffset);

/*
 * Return the location of a vertex attribute for the currently-bound program.
 *
 * (usage) and (index) map to Direct3D vertex declaration values: COLOR1 would
 *  be MOJOSHADER_USAGE_COLOR and 1.
 *
 * The return value is the index of the attribute to be sent to
 *  glVertexAttribPointer, or -1 if the stream is not used.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 */
DECLSPEC int MOJOSHADER_glGetVertexAttribLocation(MOJOSHADER_usage usage, int index);

/*
 * Connect a client-side array to the currently-bound program.
 *
 * (usage) and (index) map to Direct3D vertex declaration values: COLOR1 would
 *  be MOJOSHADER_USAGE_COLOR and 1.
 *
 * The caller should bind VBOs before this call and treat (ptr) as an offset,
 *  if appropriate.
 *
 * MojoShader will figure out where to plug this stream into the
 *  currently-bound program, and enable the appropriate client-side array.
 *
 * (size), (type), (normalized), (stride), and (ptr) correspond to
 *  glVertexAttribPointer()'s parameters (in most cases, these get passed
 *  unmolested to that very entry point during this function).
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Vertex attributes are not shared between contexts.
 */
 /* !!! FIXME: this should probably be "input" and not "attribute" */
 /* !!! FIXME: or maybe "vertex array" or something. */
DECLSPEC void MOJOSHADER_glSetVertexAttribute(MOJOSHADER_usage usage,
                                              int index, unsigned int size,
                                              MOJOSHADER_attributeType type,
                                              int normalized, unsigned int stride,
                                              const void *ptr);

/*
 * Modify the rate at which this vertex attribute advances during instanced
 *  rendering.
 *
 * This should be called alongside glSetVertexAttribute, as this does not flag
 *  the vertex array as being in use. This just calls glVertexAttribDivisorARB.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 *
 * Vertex attributes are not shared between contexts.
 */
DECLSPEC void MOJOSHADER_glSetVertexAttribDivisor(MOJOSHADER_usage usage,
                                                  int index, unsigned int divisor);

/*
 * Inform MojoShader that it should commit any pending state to the GL. This
 *  must be called after you bind a program and update any inputs, right
 *  before you start drawing, so any outstanding changes made to the shared
 *  constants array (etc) can propagate to the shader during this call.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 */
DECLSPEC void MOJOSHADER_glProgramReady(void);

/*
 * Provide information about the current viewport to the prepared shader
 *  program.
 *
 * There are numerous components of OpenGL and Direct3D where the coordinate
 *  systems do not match, and so the vertex/pixel shaders have to be modified to
 *  compensate for these mismatches (for example, gl_FragCoord requires some
 *  additional math on the Y coordinate to match vPos when rendering to the
 *  backbuffer). Call this after MOJOSHADER_glProgramReady to apply all of the
 *  relevant coordinate fixups at once.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 */
DECLSPEC void MOJOSHADER_glProgramViewportInfo(int viewportW, int viewportH,
                                               int backbufferW, int backbufferH,
                                               int renderTargetBound);

/*
 * Free the resources of a linked program. This will delete the GL object
 *  and free memory.
 *
 * If the program is currently bound by MOJOSHADER_glBindProgram(), it will
 *  be deleted as soon as it becomes unbound.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 */
DECLSPEC void MOJOSHADER_glDeleteProgram(MOJOSHADER_glProgram *program);

/*
 * Free the resources of a compiled shader. This will delete the GL object
 *  and free memory.
 *
 * If the shader is currently referenced by a linked program (or is currently
 *  bound with MOJOSHADER_glBindShaders()), it will be deleted as soon as all
 *  referencing programs are deleted and it is no longer bound, too.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 */
DECLSPEC void MOJOSHADER_glDeleteShader(MOJOSHADER_glShader *shader);

/*
 * Deinitialize MojoShader's OpenGL shader management.
 *
 * You must call this once, while your GL context (not MojoShader context) is
 *  still current, if you previously had a successful call to
 *  MOJOSHADER_glCreateContext(). This should be the last MOJOSHADER_gl*
 *  function you call until you've prepared a context again.
 *
 * This will clean up resources previously allocated, and may call into the GL.
 *
 * This will not clean up shaders and programs you created! Please call
 *  MOJOSHADER_glDeleteShader() and MOJOSHADER_glDeleteProgram() to clean
 *  those up before calling this function!
 *
 * This function destroys the MOJOSHADER_glContext you pass it. If it's the
 *  current context, then no context will be current upon return.
 *
 * This call is NOT thread safe! There must not be any other MOJOSHADER_gl*
 *  functions running when this is called. Also, as most OpenGL implementations
 *  are not thread safe, you should probably only call this from the same
 *  thread that created the GL context.
 */
DECLSPEC void MOJOSHADER_glDestroyContext(MOJOSHADER_glContext *ctx);


/* D3D11 interface... */

typedef struct MOJOSHADER_d3d11Context MOJOSHADER_d3d11Context;
typedef struct MOJOSHADER_d3d11Shader MOJOSHADER_d3d11Shader;

/*
 * Prepare MojoShader to manage Direct3D 11 shaders.
 *
 * You do not need to call this if all you want is MOJOSHADER_parse().
 *
 * You must call this once AFTER you have successfully built your D3D11 context.
 *
 * As MojoShader requires some memory to be allocated, you may provide a
 *  custom allocator to this function, which will be used to allocate/free
 *  memory. They function just like malloc() and free(). We do not use
 *  realloc(). If you don't care, pass NULL in for the allocator functions.
 *  If your allocator needs instance-specific data, you may supply it with the
 *  (malloc_d) parameter. This pointer is passed as-is to your (m) and (f)
 *  functions.
 *
 * This call is only as thread safe as your D3D11 context! If you call your
 *  context from multiple threads, you must protect this call with whatever
 *  thread synchronization technique you have for your other D3D calls.
 */
DECLSPEC MOJOSHADER_d3d11Context *MOJOSHADER_d3d11CreateContext(void *device,
                                                                void *deviceContext,
                                                                MOJOSHADER_malloc m,
                                                                MOJOSHADER_free f,
                                                                void *malloc_d);

/*
 * Get any error state we might have picked up, such as failed shader
 *  compilation.
 *
 * Returns a human-readable string. This string is for debugging purposes, and
 *  not guaranteed to be localized, coherent, or user-friendly in any way.
 *  It's for programmers!
 *
 * The latest error may remain between calls. New errors replace any existing
 *  error. Don't check this string for a sign that an error happened, check
 *  return codes instead and use this for explanation when debugging.
 *
 * Do not free the returned string: it's a pointer to a static internal
 *  buffer. Do not keep the pointer around, either, as it's likely to become
 *  invalid as soon as you call into MojoShader again.
 */
DECLSPEC const char *MOJOSHADER_d3d11GetError(MOJOSHADER_d3d11Context *context);

/*
 * Compile a buffer of Direct3D 9 shader bytecode into a Direct3D 11 shader.
 *  You still need to link the shader before you may render with it.
 *
 *   (mainfn) is the name of the shader's main function.
 *   (tokenbuf) is a buffer of Direct3D shader bytecode.
 *   (bufsize) is the size, in bytes, of the bytecode buffer.
 *   (swiz), (swizcount), (smap), and (smapcount) are passed to
 *   MOJOSHADER_parse() unmolested.
 *
 * Returns NULL on error, or a shader handle on success.
 *
 * This call is only as thread safe as your D3D11 context! If you call your
 *  context from multiple threads, you must protect this call with whatever
 *  thread synchronization technique you have for your other D3D calls.
 */
DECLSPEC MOJOSHADER_d3d11Shader *MOJOSHADER_d3d11CompileShader(MOJOSHADER_d3d11Context *context,
                                                               const char *mainfn,
                                                               const unsigned char *tokenbuf,
                                                               const unsigned int bufsize,
                                                               const MOJOSHADER_swizzle *swiz,
                                                               const unsigned int swizcount,
                                                               const MOJOSHADER_samplerMap *smap,
                                                               const unsigned int smapcount);

/*
 * Increments a shader's internal refcount. To decrement the refcount, call
 *  MOJOSHADER_glDeleteShader().
 *
 * This call is only as thread safe as your D3D11 context! If you call your
 *  context from multiple threads, you must protect this call with whatever
 *  thread synchronization technique you have for your other D3D calls.
 */
DECLSPEC void MOJOSHADER_d3d11ShaderAddRef(MOJOSHADER_d3d11Shader *shader);

/*
 * Get the MOJOSHADER_parseData structure that was produced from the
 *  call to MOJOSHADER_d3d11CompileShader().
 *
 * This data is read-only, and you should NOT attempt to free it. This
 *  pointer remains valid until the shader is deleted.
 */
DECLSPEC const MOJOSHADER_parseData *MOJOSHADER_d3d11GetShaderParseData(
                                                MOJOSHADER_d3d11Shader *shader);

/*
 * This binds individual shaders together, to be linked into a single working
 *  program once MOJOSHADER_d3d11ProgramReady is called.
 *
 * This call is only as thread safe as your D3D11 context! If you call your
 *  context from multiple threads, you must protect this call with whatever
 *  thread synchronization technique you have for your other D3D calls.
 */
DECLSPEC void MOJOSHADER_d3d11BindShaders(MOJOSHADER_d3d11Context *context,
                                          MOJOSHADER_d3d11Shader *vshader,
                                          MOJOSHADER_d3d11Shader *pshader);

/*
 * This queries for the shaders currently bound to the active context.
 *
 * This function is only for convenience, specifically for compatibility with
 * the effects API.
 *
 * This call is only as thread safe as your D3D11 context! If you call your
 *  context from multiple threads, you must protect this call with whatever
 *  thread synchronization technique you have for your other D3D calls.
 */
DECLSPEC void MOJOSHADER_d3d11GetBoundShaders(MOJOSHADER_d3d11Context *context,
                                              MOJOSHADER_d3d11Shader **vshader,
                                              MOJOSHADER_d3d11Shader **pshader);

/*
 * Fills register pointers with pointers that are directly used to push uniform
 *  data to the D3D11 shader context.
 *
 * This function is really just for the effects API, you should NOT be using
 *  this unless you know every single line of MojoShader from memory.
 *
 * This call is only as thread safe as your D3D11 context! If you call your
 *  context from multiple threads, you must protect this call with whatever
 *  thread synchronization technique you have for your other D3D calls.
 */
DECLSPEC void MOJOSHADER_d3d11MapUniformBufferMemory(MOJOSHADER_d3d11Context *context,
                                                     float **vsf, int **vsi, unsigned char **vsb,
                                                     float **psf, int **psi, unsigned char **psb);

/*
 * Tells the context that you are done with the memory mapped by
 *  MOJOSHADER_d3d11MapUniformBufferMemory().
 *
 * This call is only as thread safe as your D3D11 context! If you call your
 *  context from multiple threads, you must protect this call with whatever
 *  thread synchronization technique you have for your other D3D calls.
 */
DECLSPEC void MOJOSHADER_d3d11UnmapUniformBufferMemory(MOJOSHADER_d3d11Context *context);

/*
 * Return the location of a vertex attribute for the given vertex shader.
 *
 * (usage) and (index) map to Direct3D vertex declaration values: COLOR1 would
 *  be MOJOSHADER_USAGE_COLOR and 1.
 *
 * The return value is the index of the attribute to be used when building the
 *  input layout object.
 */
DECLSPEC int MOJOSHADER_d3d11GetVertexAttribLocation(MOJOSHADER_d3d11Shader *vert,
                                                     MOJOSHADER_usage usage,
                                                     int index);

/*
 * Using the given input layout, compiles the vertex shader with input
 *  parameters that will be compatible with the incoming vertex data.
 *
 * (inputLayoutHash) is an application-defined value to differentiate unique
 *  vertex declarations that will be passed to the vertex shader.
 *  (elements) is an array of D3D11_INPUT_ELEMENT_DESCs, with (elementCount)
 *  entries. (bytecode) and (bytecodeLength) will be filled with the final
 *  compiled D3D11 vertex shader.
 *
 * Returns 0 on success, nonzero on error.
 *
 * This call is only as thread safe as your D3D11 context! If you call your
 *  context from multiple threads, you must protect this call with whatever
 *  thread synchronization technique you have for your other D3D calls.
 */
DECLSPEC int MOJOSHADER_d3d11CompileVertexShader(MOJOSHADER_d3d11Context *ctx,
                                                 unsigned long long inputLayoutHash,
                                                 void *elements, int elementCount,
                                                 void **bytecode, int *bytecodeLength);

/*
 * Inform MojoShader that it should commit any pending state and prepare the
 *  final shader program object, linking the input/output parameter data to
 *  be compatible with the more-strict Shader Model 4 rule set. This must be
 *  called after you bind shaders and update any inputs, right before you start
 *  drawing, so any outstanding changes made to the shared constants array (etc)
 *  can propagate to the shader during this call.
 *
 * Returns 0 on success, nonzero on error.
 *
 * This call is only as thread safe as your D3D11 context! If you call your
 *  context from multiple threads, you must protect this call with whatever
 *  thread synchronization technique you have for your other D3D calls.
 */
DECLSPEC int MOJOSHADER_d3d11ProgramReady(MOJOSHADER_d3d11Context *context,
                                          unsigned long long inputLayoutHash);

/*
 * Free the resources of a compiled shader. This will delete the shader object
 *  and free memory.
 *
 * This call is only as thread safe as your D3D11 context! If you call your
 *  context from multiple threads, you must protect this call with whatever
 *  thread synchronization technique you have for your other D3D calls.
 */
DECLSPEC void MOJOSHADER_d3d11DeleteShader(MOJOSHADER_d3d11Context *context,
                                           MOJOSHADER_d3d11Shader *shader);

/*
 * Deinitialize MojoShader's D3D11 shader management.
 *
 * This will clean up resources previously allocated for the active context.
 *
 * This will NOT clean up shaders you created! Please destroy all shaders
 *  before calling this function.
 */
DECLSPEC void MOJOSHADER_d3d11DestroyContext(MOJOSHADER_d3d11Context *context);


/* SDL_GPU interface... */

typedef struct MOJOSHADER_sdlContext MOJOSHADER_sdlContext;
typedef struct MOJOSHADER_sdlShaderData MOJOSHADER_sdlShaderData;
typedef struct MOJOSHADER_sdlProgram MOJOSHADER_sdlProgram;

#ifndef SDL_GPU_H
typedef struct SDL_GPUDevice SDL_GPUDevice;
typedef struct SDL_GPUShader SDL_GPUShader;
typedef struct SDL_GPUCommandBuffer SDL_GPUCommandBuffer;
#endif /* SDL_GPU_H */

/*
 * Call this function to get the 'formatFlags' parameter for
 *  SDL_CreateGPUDevice.
 *
 * Returns the SDL_GPUShaderFormatFlagBits for creating the SDL_GPUDevice.
 */
DECLSPEC unsigned int MOJOSHADER_sdlGetShaderFormats(void);

/*
 * Prepares a context to manage SDL_gpu shaders.
 *
 * You do not need to call this if all you want is MOJOSHADER_parse().
 *
 * (device) refers to the SDL_GPUDevice.
 *
 * You can only have one MOJOSHADER_sdlContext per actual SDL_gpu context, or
 *  undefined behaviour will result.
 *
 * As MojoShader requires some memory to be allocated, you may provide a
 *  custom allocator to this function, which will be used to allocate/free
 *  memory. They function just like malloc() and free(). We do not use
 *  realloc(). If you don't care, pass NULL in for the allocator functions.
 *  If your allocator needs instance-specific data, you may supply it with the
 *  (malloc_d) parameter. This pointer is passed as-is to your (m) and (f)
 *  functions.
 *
 * Returns a new context on success, NULL on error.
 */
DECLSPEC MOJOSHADER_sdlContext *MOJOSHADER_sdlCreateContext(SDL_GPUDevice *device,
                                                            MOJOSHADER_malloc m,
                                                            MOJOSHADER_free f,
                                                            void *malloc_d);

/*
 * Get any error state we might have picked up.
 *
 * Returns a human-readable string. This string is for debugging purposes, and
 *  not guaranteed to be localized, coherent, or user-friendly in any way.
 *  It's for programmers!
 *
 * The latest error may remain between calls. New errors replace any existing
 *  error. Don't check this string for a sign that an error happened, check
 *  return codes instead and use this for explanation when debugging.
 *
 * Do not free the returned string: it's a pointer to a static internal
 *  buffer. Do not keep the pointer around, either, as it's likely to become
 *  invalid as soon as you call into MojoShader again.
 *
 * This call does NOT require a valid MOJOSHADER_sdlContext to have been made
 *  current. The error buffer is shared between contexts, so you can get
 *  error results from a failed MOJOSHADER_sdlCreateContext().
 */
DECLSPEC const char *MOJOSHADER_sdlGetError(MOJOSHADER_sdlContext *ctx);

/*
 * Deinitialize MojoShader's SDL_gpu shader management.
 *
 * You must call this once, while your SDL_GPUDevice is still valid. This should
 * be the last MOJOSHADER_sdl* function you call until you've prepared a context
 * again.
 *
 * This will clean up resources previously allocated, and may call into SDL_gpu.
 *
 * This will not clean up shaders and programs you created! Please call
 *  MOJOSHADER_sdlDeleteShader() and MOJOSHADER_sdlDeleteProgram() to clean
 *  those up before calling this function!
 *
 * This function destroys the MOJOSHADER_sdlContext you pass it.
 */
DECLSPEC void MOJOSHADER_sdlDestroyContext(MOJOSHADER_sdlContext *ctx);

/*
 * Compile a buffer of Direct3D shader bytecode into an SDL_gpu shader module.
 *
 *   (tokenbuf) is a buffer of Direct3D shader bytecode.
 *   (bufsize) is the size, in bytes, of the bytecode buffer.
 *   (swiz), (swizcount), (smap), and (smapcount) are passed to
 *   MOJOSHADER_parse() unmolested.
 *
 * Returns NULL on error, or a shader handle on success.
 *
 * Compiled shaders from this function may not be shared between contexts.
 */
DECLSPEC MOJOSHADER_sdlShaderData *MOJOSHADER_sdlCompileShader(MOJOSHADER_sdlContext *ctx,
                                                           const char *mainfn,
                                                           const unsigned char *tokenbuf,
                                                           const unsigned int bufsize,
                                                           const MOJOSHADER_swizzle *swiz,
                                                           const unsigned int swizcount,
                                                           const MOJOSHADER_samplerMap *smap,
                                                           const unsigned int smapcount);

/*
 * Increments a shader's internal refcount.
 *
 * To decrement the refcount, call MOJOSHADER_sdlDeleteShader().
 */
DECLSPEC void MOJOSHADER_sdlShaderAddRef(MOJOSHADER_sdlShaderData *shader);

/*
 * Decrements a shader's internal refcount, and deletes if the refcount is zero.
 *
 * To increment the refcount, call MOJOSHADER_sdlShaderAddRef().
 */
DECLSPEC void MOJOSHADER_sdlDeleteShader(MOJOSHADER_sdlContext *ctx,
                                         MOJOSHADER_sdlShaderData *shader);

/*
 * Get the MOJOSHADER_parseData structure that was produced from the
 *  call to MOJOSHADER_sdlCompileShader().
 *
 * This data is read-only, and you should NOT attempt to free it. This
 *  pointer remains valid until the shader is deleted.
 */
DECLSPEC const MOJOSHADER_parseData *MOJOSHADER_sdlGetShaderParseData(
                                                  MOJOSHADER_sdlShaderData *shader);

/*
 * Link bound vertex and pixel shader into a working SDL_gpu shader program.
 *  (vshader) or (pshader) can NOT be NULL, unlike OpenGL.
 *
 * You can reuse shaders in various combinations across
 *  multiple programs, by relinking different pairs.
 *
 * Requires vertex element data for patches.
 *
 * It is illegal to give a vertex shader for (pshader) or a pixel shader
 *  for (vshader).
 *
 * Once you have successfully linked a program, you may render with it.
 *
 * Returns NULL on error, or a program handle on success.
 */
DECLSPEC MOJOSHADER_sdlProgram *MOJOSHADER_sdlLinkProgram(MOJOSHADER_sdlContext *context,
                                                          MOJOSHADER_vertexAttribute *vertexAttributes,
                                                          int vertexAttributeCount);

/*
 * This binds the program to the active context, and does nothing particularly
 * special until you start working with uniform buffers or shader modules.
 *
 * After binding a program, you should update any uniforms you care about
 *  with MOJOSHADER_sdlMapUniformBufferMemory() (etc), set any vertex arrays
 *  using MOJOSHADER_sdlGetVertexAttribLocation(), and finally call
 *  MOJOSHADER_sdlGetShaders() to get the final modules. Then you may
 *  begin building your pipeline state objects.
 */
DECLSPEC void MOJOSHADER_sdlBindProgram(MOJOSHADER_sdlContext *context,
                                        MOJOSHADER_sdlProgram *program);

/*
 * Free the resources of a linked program. This will delete the shader modules
 *  and free memory.
 *
 * If the program is currently bound by MOJOSHADER_sdlBindProgram(), it will
 *  be deleted as soon as it becomes unbound.
 */
DECLSPEC void MOJOSHADER_sdlDeleteProgram(MOJOSHADER_sdlContext *context,
                                          MOJOSHADER_sdlProgram *program);

/*
 * This "binds" individual shaders, which effectively means the context
 *  will store these shaders for later retrieval. No actual binding or
 *  pipeline creation is performed.
 *
 * This function is only for convenience, specifically for compatibility
 *  with the effects API.
 */
DECLSPEC void MOJOSHADER_sdlBindShaders(MOJOSHADER_sdlContext *ctx,
                                        MOJOSHADER_sdlShaderData *vshader,
                                        MOJOSHADER_sdlShaderData *pshader);

/*
 * This queries for the shaders currently bound to the active context.
 *
 * This function is only for convenience, specifically for compatibility
 *  with the effects API.
 */
DECLSPEC void MOJOSHADER_sdlGetBoundShaderData(MOJOSHADER_sdlContext *ctx,
                                            MOJOSHADER_sdlShaderData **vshader,
                                            MOJOSHADER_sdlShaderData **pshader);

/*
 * Fills register pointers with pointers that are directly used to push uniform
 *  data to the SDL3 shader context.
 *
 * This function is really just for the effects API, you should NOT be using
 *  this unless you know every single line of MojoShader from memory.
 */
DECLSPEC void MOJOSHADER_sdlMapUniformBufferMemory(MOJOSHADER_sdlContext *ctx,
                                                   float **vsf, int **vsi, unsigned char **vsb,
                                                   float **psf, int **psi, unsigned char **psb);

/*
 * Tells the context that you are done with the memory mapped by
 *  MOJOSHADER_sdlMapUniformBufferMemory().
 */
DECLSPEC void MOJOSHADER_sdlUnmapUniformBufferMemory(MOJOSHADER_sdlContext *ctx);

/*
 * Returns the minimum required size of the uniform buffer for this shader.
 *  You will need this to fill out the SDL_GPUGraphicsPipelineCreateInfo struct.
 */
DECLSPEC int MOJOSHADER_sdlGetUniformBufferSize(MOJOSHADER_sdlShaderData *shader);

/*
 * Pushes the uniform buffer updates for the currently bound program.
 *
 * This function will record calls to SDL_GPUPush*ShaderUniforms into the
 *  passed command buffer.
 */
DECLSPEC void MOJOSHADER_sdlUpdateUniformBuffers(MOJOSHADER_sdlContext *ctx,
                                                 SDL_GPUCommandBuffer *cb);

/*
 * Return the location of a vertex attribute for the given shader.
 *
 * (usage) and (index) map to Direct3D vertex declaration values: COLOR1 would
 *  be MOJOSHADER_USAGE_COLOR and 1.
 *
 * The return value is the index of the attribute to be used to create
 *  an SDL_GPUVertexAttribute, or -1 if the stream is not used.
 */
DECLSPEC int MOJOSHADER_sdlGetVertexAttribLocation(MOJOSHADER_sdlShaderData *vert,
                                                   MOJOSHADER_usage usage,
                                                   int index);

/*
 * Get the SDL_GPUShaderModules from the currently bound shader program.
 */
DECLSPEC void MOJOSHADER_sdlGetShaders(MOJOSHADER_sdlContext *ctx,
                                             SDL_GPUShader **vshader,
                                             SDL_GPUShader **pshader);

/*
 * Gets the number of sampler slots needed by a given shader module.
 */
DECLSPEC unsigned int MOJOSHADER_sdlGetSamplerSlots(MOJOSHADER_sdlShaderData *shader);


/* Effects interface... */
#include "mojoshader_effects.h"


#ifdef __cplusplus
}
#endif

#endif  /* include-once blocker. */

/* end of mojoshader.h ... */

