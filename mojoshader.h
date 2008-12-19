/**
 * MojoShader; generate shader programs from bytecode of compiled
 *  Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

#ifndef __INCL_MOJOSHADER_H_
#define __INCL_MOJOSHADER_H_

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

/*
 * For determining the version of MojoShader you are using:
 *    const int compiled_against = MOJOSHADER_VERSION;
 *    const int linked_against = MOJOSHADER_version();
 *
 * The version is a single integer that increments, not a major/minor value.
 */
int MOJOSHADER_version(void);

/*
 * For determining the revision control changeste of MojoShader you are using:
 *    const const *compiled_against = MOJOSHADER_CHANGESET;
 *    const char *linked_against = MOJOSHADER_changeset();
 *
 * The version is an arbitrary, null-terminated ASCII string. It is probably
 *  a hash that represents a revision control changeset, and can't be
 *  compared to any other string to determine chronology.
 *
 * Do not attempt to free this string; it's statically allocated.
 */
const char *MOJOSHADER_changeset(void);

/*
 * These allocators work just like the C runtime's malloc() and free()
 *  (in fact, they probably use malloc() and free() internally if you don't
 *  specify your own allocator, but don't rely on that behaviour).
 * (data) is the pointer you supplied when specifying these allocator
 *  callbacks, in case you need instance-specific data...it is passed through
 *  to your allocator unmolested, and can be NULL if you like.
 */
typedef void *(*MOJOSHADER_malloc)(int bytes, void *data);
typedef void (*MOJOSHADER_free)(void *ptr, void *data);


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
    MOJOSHADER_TYPE_ANY = 0xFFFFFFFF   /* used for bitmasks */
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
    MOJOSHADER_ATTRIBUTE_HALF_FLOAT,  /* MAYBE available in your OpenGL! */
} MOJOSHADER_attributeType;

/*
 * Data types for uniforms. See MOJOSHADER_uniform for more information.
 */
typedef enum
{
    MOJOSHADER_UNIFORM_UNKNOWN = -1, /* housekeeping value; never returned. */
    MOJOSHADER_UNIFORM_FLOAT,
    MOJOSHADER_UNIFORM_INT,
    MOJOSHADER_UNIFORM_BOOL,
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
    MOJOSHADER_SAMPLER_VOLUME,
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
 */
typedef struct MOJOSHADER_sampler
{
    MOJOSHADER_samplerType type;
    int index;
    const char *name;
} MOJOSHADER_sampler;

/*
 * Data types for attributes. See MOJOSHADER_attribute for more information.
 */
typedef enum
{
    MOJOSHADER_USAGE_UNKNOWN = -1,  /* housekeeping value; never returned. */
    MOJOSHADER_USAGE_POSITION,
    MOJOSHADER_USAGE_BLENDWEIGHT,
    MOJOSHADER_USAGE_BLENDINDICES,
    MOJOSHADER_USAGE_NORMAL,
    MOJOSHADER_USAGE_POINTSIZE,
    MOJOSHADER_USAGE_TEXCOORD,
    MOJOSHADER_USAGE_TANGENT,
    MOJOSHADER_USAGE_BINORMAL,
    MOJOSHADER_USAGE_TESSFACTOR,
    MOJOSHADER_USAGE_POSITIONT,
    MOJOSHADER_USAGE_COLOR,
    MOJOSHADER_USAGE_FOG,
    MOJOSHADER_USAGE_DEPTH,
    MOJOSHADER_USAGE_SAMPLE,
    MOJOSHADER_USAGE_TOTAL,  /* housekeeping value; never returned. */
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
 * Structure used to return data from parsing of a shader...
 */
typedef struct MOJOSHADER_parseData
{
    /*
     * Human-readable error, if there is one. Will be NULL if there was no
     *  error. The string will be UTF-8 encoded, and English only. Most of
     *  these shouldn't be shown to the end-user anyhow.
     */
    const char *error;

    /*
     * Position of error, if there is one. Will be -3 if there was no
     *  error, -2 if there was an error before processing started, and
     *  -1 if there was an error during final processing. If >= 0,
     *  MOJOSHADER_parse() sets this to the byte offset (starting at zero) into
     *  the bytecode you supplied, and MOJOSHADER_assemble() sets this to a
     *  a line number in the source code you supplied (starting at one).
     */
    int error_position;

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

    /*
     * The number of elements pointed to by (attributes).
     */
    int attribute_count;

    /*
     * (attribute_count) elements of data that specify Attributes to be set
     *  for this shader. See discussion on MOJOSHADER_attribute for details.
     * This can be NULL on error or if (attribute_count) is zero.
     */
    MOJOSHADER_attribute *attributes;

    /*
     * The number of elements pointed to by (swizzles).
     */
    int swizzle_count;

    /*
     * (swizzle_count) elements of data that specify swizzles the shader will
     *  apply to incoming attributes. This is a copy of what was passed to
     *  MOJOSHADER_parseData().
     * This can be NULL on error or if (swizzle_count) is zero.
     */
    MOJOSHADER_swizzle *swizzles;

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
 * Profile string for GLSL: OpenGL high-level shader language output.
 */
#define MOJOSHADER_PROFILE_GLSL "glsl"

/*
 * Profile string for GLSL 1.20: minor improvements to base GLSL spec.
 */
#define MOJOSHADER_PROFILE_GLSL120 "glsl120"

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
 * Determine the highest supported Shader Model for a profile.
 */
int MOJOSHADER_maxShaderModel(const char *profile);


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
 * This function is thread safe, so long as (m) and (f) are too, and that
 *  (tokenbuf) remains intact for the duration of the call. This allows you
 *  to parse several shaders on separate CPU cores at the same time.
 */
const MOJOSHADER_parseData *MOJOSHADER_parse(const char *profile,
                                             const unsigned char *tokenbuf,
                                             const unsigned int bufsize,
                                             const MOJOSHADER_swizzle *swiz,
                                             const unsigned int swizcount,
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
void MOJOSHADER_freeParseData(const MOJOSHADER_parseData *data);



/* Assembler interface... */

/*
 * This function is optional. Use this to convert Direct3D shader assembly
 *  language into bytecode, which can be handled by MOJOSHADER_parse().
 *
 * (source) is an ASCII, NULL-terminated string of valid Direct3D shader
 *  assembly source code.
 *
 * This will return a MOJOSHADER_parseData(), like MOJOSHADER_parse() would,
 *  except the profile will be MOJOSHADER_PROFILE_BYTECODE and the output
 *  will be the assembled bytecode instead of some other language. This output
 *  can be pushed back through MOJOSHADER_parseData() with a different profile.
 *
 * This function will never return NULL, even if the system is completely
 *  out of memory upon entry (in which case, this function returns a static
 *  MOJOSHADER_parseData object, which is still safe to pass to
 *  MOJOSHADER_freeParseData()).
 *
 * As assembling requires some memory to be allocated, you may provide a
 *  custom allocator to this function, which will be used to allocate/free
 *  memory. They function just like malloc() and free(). We do not use
 *  realloc(). If you don't care, pass NULL in for the allocator functions.
 *  If your allocator needs instance-specific data, you may supply it with the
 *  (d) parameter. This pointer is passed as-is to your (m) and (f) functions.
 *
 * This function is thread safe, so long as (m) and (f) are too, and that
 *  (source) remains intact for the duration of the call. This allows you
 *  to assemble several shaders on separate CPU cores at the same time.
 */
const MOJOSHADER_parseData *MOJOSHADER_assemble(const char *source,
                              MOJOSHADER_malloc m, MOJOSHADER_free f, void *d);



/* OpenGL interface... */

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
 *  through the callback you supply. The lookup function is neither stored nor
 *  used by MojoShader after this function returns, nor are the functions it
 *  might look up.
 *
 * You should not free any strings returned from this function; they are
 *  pointers to internal, probably static, memory.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 */
int MOJOSHADER_glAvailableProfiles(void *(*lookup)(const char *fnname),
                                   const char **profs, const int size);


/*
 * Determine the best profile to use for the current system.
 *
 * You can only call this AFTER you have successfully built your GL context
 *  and made it current. This function will lookup the GL functions it needs
 *  through the callback you supply. The lookup function is neither stored nor
 *  used by MojoShader after this function returns, nor are the functions it
 *  might look up.
 *
 * Returns the name of the "best" profile on success, NULL if none of the
 *  available profiles will work on this system. "Best" is a relative term,
 *  but it generally means the best trade off between feature set and
 *  performance. The selection algorithm may be arbitrary and complex.
 *
 * The returned value is an internal static string, and should not be free()'d
 *  by the caller. If you get a NULL, calling MOJOSHADER_glGetError() might
 *  shed some light on why.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 */
const char *MOJOSHADER_glBestProfile(void *(*lookup)(const char *fnname));


/*
 * Prepare MojoShader to manage OpenGL shaders.
 *
 * You do not need to call this if all you want is MOJOSHADER_parse().
 *
 * You must call this once AFTER you have successfully built your GL context
 *  and made it current. This function will lookup the GL functions it needs
 *  through the callback you supply, after which it may call them at any time
 *  up until you call MOJOSHADER_glDestroyContext(). The lookup function is
 *  neither stored nor used by MojoShader after this function returns.
 *
 * (profile) is an OpenGL-specific MojoShader profile, which decides how
 *  Direct3D bytecode shaders get turned into OpenGL programs, and how they
 *  are fed to the GL.
 *
 * (lookup) is a callback that is used to load GL entry points. This callback
 *  has to look up base GL functions and extension entry points.
 *
 * As MojoShader requires some memory to be allocated, you may provide a
 *  custom allocator to this function, which will be used to allocate/free
 *  memory. They function just like malloc() and free(). We do not use
 *  realloc(). If you don't care, pass NULL in for the allocator functions.
 *  If your allocator needs instance-specific data, you may supply it with the
 *  (d) parameter. This pointer is passed as-is to your (m) and (f) functions.
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
MOJOSHADER_glContext *MOJOSHADER_glCreateContext(const char *profile,
                                        void *(*lookup)(const char *fnname),
                                        MOJOSHADER_malloc m, MOJOSHADER_free f,
                                        void *d);

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
 */
void MOJOSHADER_glMakeContextCurrent(MOJOSHADER_glContext *ctx);

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
const char *MOJOSHADER_glGetError(void);

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
int MOJOSHADER_glMaxUniforms(MOJOSHADER_shaderType shader_type);

/*
 * Compile a buffer of Direct3D shader bytecode into an OpenGL shader.
 *  You still need to link the shader before you may render with it.
 *
 *   (tokenbuf) is a buffer of Direct3D shader bytecode.
 *   (bufsize) is the size, in bytes, of the bytecode buffer.
 *   (swiz) and (swizcount) are passed to MOJOSHADER_parse() unmolested.
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
MOJOSHADER_glShader *MOJOSHADER_glCompileShader(const unsigned char *tokenbuf,
                                                const unsigned int bufsize,
                                                const MOJOSHADER_swizzle *swiz,
                                                const unsigned int swizcount);


/*
 * Get the MOJOSHADER_parseData structure that was produced from the
 *  call to MOJOSHADER_glCompileShader().
 *
 * This data is read-only, and you should NOT attempt to free it. This
 *  pointer remains valid until the shader is deleted.
 */
const MOJOSHADER_parseData *MOJOSHADER_glGetShaderParseData(
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
MOJOSHADER_glProgram *MOJOSHADER_glLinkProgram(MOJOSHADER_glShader *vshader,
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
void MOJOSHADER_glBindProgram(MOJOSHADER_glProgram *program);


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
void MOJOSHADER_glSetVertexShaderUniformF(unsigned int idx, const float *data,
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
void MOJOSHADER_glSetVertexShaderUniformI(unsigned int idx, const int *data,
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
void MOJOSHADER_glSetVertexShaderUniformB(unsigned int idx, const int *data,
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
void MOJOSHADER_glSetPixelShaderUniformF(unsigned int idx, const float *data,
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
void MOJOSHADER_glSetPixelShaderUniformI(unsigned int idx, const int *data,
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
void MOJOSHADER_glSetPixelShaderUniformB(unsigned int idx, const int *data,
                                         unsigned int bcount);

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
void MOJOSHADER_glSetVertexAttribute(MOJOSHADER_usage usage,
                                     int index, unsigned int size,
                                     MOJOSHADER_attributeType type,
                                     int normalized, unsigned int stride,
                                     const void *ptr);

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
void MOJOSHADER_glProgramReady(void);

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
void MOJOSHADER_glDeleteProgram(MOJOSHADER_glProgram *program);

/*
 * Free the resources of a compiled shader. This will delete the GL object
 *  and free memory.
 *
 * If the shader is currently referenced by a linked program, it will
 *  be deleted as soon as all referencing programs are deleted, too.
 *
 * This call is NOT thread safe! As most OpenGL implementations are not thread
 *  safe, you should probably only call this from the same thread that created
 *  the GL context.
 *
 * This call requires a valid MOJOSHADER_glContext to have been made current,
 *  or it will crash your program. See MOJOSHADER_glMakeContextCurrent().
 */
void MOJOSHADER_glDeleteShader(MOJOSHADER_glShader *shader);

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
void MOJOSHADER_glDestroyContext(MOJOSHADER_glContext *ctx);

#ifdef __cplusplus
}
#endif

#endif  /* include-once blocker. */

/* end of mojoshader.h ... */

