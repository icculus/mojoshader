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

/*
 * For determining the version of MojoShader you are using:
 *    const int compiled_against = MOJOSHADER_VERSION;
 *    const int linked_against = MOJOSHADER_version();
 *
 * The version is a single integer that increments, not a major/minor value.
 */
#define MOJOSHADER_VERSION 1
int MOJOSHADER_version(void);

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
 * Data types for uniforms. See MOJOSHADER_uniform for more information.
 */
typedef enum
{
    MOJOSHADER_UNIFORM_FLOAT,
    MOJOSHADER_UNIFORM_INT,
    MOJOSHADER_UNIFORM_BOOL,
    MOJOSHADER_UNIFORM_SAMPLER_2D,
    MOJOSHADER_UNIFORM_SAMPLER_CUBE,
    MOJOSHADER_UNIFORM_SAMPLER_3D,
} MOJOSHADER_uniformType;

/*
 * These are the uniforms to be set for a shader. "Uniforms" are what Direct3D
 *  calls "Constants" ... IDirect3DDevice::SetVertexShaderConstantF() would
 *  need this data, for example. These integers are register indexes. So if
 *  index==6 and type==MOJOSHADER_UNIFORM_FLOAT, that means we'd expect a
 *  4-float vector to be specified for what would be register "c6" in D3D
 *  assembly language, before drawing with the shader.
 */
typedef struct
{
    MOJOSHADER_uniformType type;
    int index;
} MOJOSHADER_uniform;

/*
 * Data types for attributes. See MOJOSHADER_attribute for more information.
 */
typedef enum
{
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
    MOJOSHADER_USAGE_TOTAL,  /* housekeeping value; not ever returned. */
} MOJOSHADER_usage;

/*
 * These are the attributes to be set for a shader. "Attributes" are what
 *  Direct3D calls "Vertex Declarations Usages" ...
 *  IDirect3DDevice::CreateVertexDeclaration() would need this data, for
 *  example. Each attribute is associated with an array of data that uses one
 *  element per-vertex. So if usage==MOJOSHADER_USAGE_COLOR and index==1, that
 *  means we'd expect a secondary color array to be bound to this shader
 *  before drawing.
 */
typedef struct
{
    MOJOSHADER_usage usage;
    int index;
} MOJOSHADER_attribute;

/*
 * Structure used to return data from parsing of a shader...
 */
typedef struct
{
    /*
     * Human-readable error, if there is one. Will be NULL if there was no
     *  error. The string will be UTF-8 encoded, and English only. Most of
     *  these shouldn't be shown to the end-user anyhow.
     */
    const char *error;

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
     * Count of Direct3D instructions we parsed. This is meaningless in terms
     *  of the actual output, as the profile will probably grow or reduce
     *  the count (or for high-level languages, not have that information at
     *  all), but it can give you a rough idea of the size of your shader.
     *  Will be zero on error.
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
#define MOJOSHADER_PROFILE_PASSTHROUGH "passthrough"

/*
 * Profile string for GLSL: OpenGL high-level shader language output.
 */
#define MOJOSHADER_PROFILE_GLSL "glsl"


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
 * This function is thread safe, so long (m) and (f) are too, and that
 *  (tokenbuf) remains intact for the duration of the call. This allows you
 *  to parse several shaders on separate CPU cores at the same time.
 */
const MOJOSHADER_parseData *MOJOSHADER_parse(const char *profile,
                                             const unsigned char *tokenbuf,
                                             const unsigned int bufsize,
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

#ifdef __cplusplus
}
#endif

#endif  /* include-once blocker. */

/* end of mojoshader.h ... */

