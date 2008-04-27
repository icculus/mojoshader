
/* !!! FIXME: merge this in?
static GLenum opengl_attr_type(const MOJOSHADER_attributeType type)
{
    switch (type)
    {
        case MOJOSHADER_ATTRIBUTE_BYTE: return GL_BYTE;
        case MOJOSHADER_ATTRIBUTE_UBYTE: return GL_UNSIGNED_BYTE;
        case MOJOSHADER_ATTRIBUTE_SHORT: return GL_SHORT;
        case MOJOSHADER_ATTRIBUTE_USHORT: return GL_USHORT;
        case MOJOSHADER_ATTRIBUTE_INT: return GL_INT;
        case MOJOSHADER_ATTRIBUTE_UINT: return GL_UNSIGNED_INT;
        case MOJOSHADER_ATTRIBUTE_FLOAT: return GL_FLOAT;
        case MOJOSHADER_ATTRIBUTE_DOUBLE: return GL_DOUBLE;
    } // switch

    return GL_NONE;  // oh well. Raises a GL error later.
} // opengl_attr_type


static GLenum opengl_attr_index(const MOJOSHADER_usage usage, const int index)
{
    switch (usage)
    {
        case MOJOSHADER_USAGE_POSITION,
        case MOJOSHADER_USAGE_BLENDWEIGHT,
        case MOJOSHADER_USAGE_BLENDINDICES,
        case MOJOSHADER_USAGE_NORMAL,
        case MOJOSHADER_USAGE_POINTSIZE,
        case MOJOSHADER_USAGE_TEXCOORD,
        case MOJOSHADER_USAGE_TANGENT,
        case MOJOSHADER_USAGE_BINORMAL,
        case MOJOSHADER_USAGE_TESSFACTOR,
        case MOJOSHADER_USAGE_POSITIONT,
        case MOJOSHADER_USAGE_COLOR,
        case MOJOSHADER_USAGE_FOG,
        case MOJOSHADER_USAGE_DEPTH,
        case MOJOSHADER_USAGE_SAMPLE,
        case MOJOSHADER_USAGE_TOTAL: return GL_NONE;  // stop compiler whining.
    } // switch

    return GL_NONE;  // will fail later.
} // opengl_attr_index


void MOJOSHADER_glSetVertexDeclaration(MOJOSHADER_usage usage, int index,
                                       unsigned int size,
                                       MOJOSHADER_attributeType type,
                                       int normalized, unsigned int stride,
                                       const void *ptr)
{
    const GLuint gl_index = opengl_attr_index(usage, index);
    const GLenum gl_type = opengl_attr_type(type);
    const GLboolean gl_normalized = (normalized) ? GL_TRUE : GL_FALSE;
    pglVertexAttribPointer(gl_index, size, gl_type, gl_normalized,
                           stride, ptr);
} // MOJOSHADER_glSetVertexDeclaration
*/





#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <assert.h>

#include "mojoshader.h"

// Get basic wankery out of the way here...

typedef unsigned int uint;  // this is a printf() helper. don't use for code.
typedef uint8_t uint8;
typedef uint32_t uint32;
typedef int32_t int32;

struct MOJOSHADER_glShader
{
    const MOJOSHADER_parseData *parseData;
    GLuint handle;
    uint32 refcount;
};

struct MOJOSHADER_glProgram
{
    const MOJOSHADER_glShader *vertex;
    const MOJOSHADER_glShader *fragment;
    GLuint handle;
    uint32 refcount;
};

// Allocators...
static MOJOSHADER_malloc malloc_fn = NULL;
static MOJOSHADER_free free_fn = NULL;
static void *malloc_data = NULL;

// The constant register files...
// Man, it kills me how much memory this takes...
static float vs_register_file_f[8192 * 4];
static int vs_register_file_i[2047 * 4];
static uint8 vs_register_file_b[2047];
static float ps_register_file_f[8192 * 4];
static int ps_register_file_i[2047 * 4];
static uint8 ps_register_file_b[2047];

// GL stuff...
static MOJOSHADER_glProgram *bound_program = NULL;


// #define this to force app to supply an allocator, so there's no reference
//  to the C runtime's malloc() and free()...
#if MOJOSHADER_FORCE_ALLOCATOR
#define internal_malloc NULL
#define internal_free NULL
#else
static void *internal_malloc(int bytes, void *d) { return malloc(bytes); }
static void internal_free(void *ptr, void *d) { free(ptr); }
#endif

static inline void *Malloc(const size_t len)
{
    return malloc_fn(len, malloc_data);
} // Malloc

static inline void Free(void *ptr)
{
    return free_fn(ptr, malloc_data);
} // Free


int MOJOSHADER_glInit(const char *_profile,
                      void *(*lookup)(const char *fnname),
                      MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    if (strcmp(_profile, MOJOSHADER_PROFILE_GLSL) != 0)
        profile = MOJOSHADER_PROFILE_GLSL;
    else
        return 0;

    // !!! FIXME: lookup glGetString(), check extensions.

    malloc_fn = (m == NULL) ? internal_malloc : m;
    free_fn = (f == NULL) ? internal_free : f;
    malloc_data = d;

    // !!! FIXME: lookup other entry points.

    memset(vs_register_file_f, '\0', sizeof (vs_register_file_f));
    memset(vs_register_file_i, '\0', sizeof (vs_register_file_i));
    memset(vs_register_file_b, '\0', sizeof (vs_register_file_b));
    memset(ps_register_file_f, '\0', sizeof (ps_register_file_f));
    memset(ps_register_file_i, '\0', sizeof (ps_register_file_i));
    memset(ps_register_file_b, '\0', sizeof (ps_register_file_b));

    MOJOSHADER_glBindProgram(NULL);

    return 1;
} // MOJOSHADER_glInit


MOJOSHADER_glShader *MOJOSHADER_glCompileShader(const unsigned char *tokenbuf,
                                                const unsigned int bufsize)
{
    MOJOSHADER_glShader *retval = NULL;
    const MOJOSHADER_parseData *pd = MOJOSHADER_parse(profile, tokenbuf,
                                                      bufsize, malloc_fn,
                                                      free_fn, malloc_data);
    if (pd->error != NULL)
    {
        MOJOSHADER_freeParseData(pd);
        return NULL;
    } // if

    retval = (MOJOSHADER_glShader *) Malloc(sizeof (MOJOSHADER_glShader));
    if (retval == NULL)
    {
        MOJOSHADER_freeParseData(pd);
        return NULL;
    } // if
    
    GLint ok = 0;
    const GLenum shader_type = (pd->shader_type == MOJOSHADER_TYPE_PIXEL) ? GL_FRAGMENT_SHADER_ARB : GL_VERTEX_SHADER_ARB;
    GLint shaderlen = (GLint) pd->output_len;
    const GLhandleARB shader = pglCreateShaderObjectARB(shader_type);

    pglShaderSourceARB(shader, 1, (const GLcharARB **) &pd->output, &shaderlen);
    pglCompileShaderARB(shader);
    pglGetObjectParameterivARB(shader, GL_OBJECT_COMPILE_STATUS_ARB, &ok);

    if (!ok)
    {
        GLcharARB err[1024];
        GLsizei len = 0;
        //glGetInfoLogARB(shader, sizeof (err), &len, err);
        //printf("FAIL: %s glsl compile: %s\n", fname, err);
        pglDeleteObjectARB(shader);
        MOJOSHADER_freeParseData(pd);
        Free(retval);
        return NULL;
    } // if

    retval->parseData = pd;
    retval->handle = shader;
    retval->refcount = 1;
    return retval;
} // MOJOSHADER_glCompileShader


static void shader_unref(MOJOSHADER_glShader *shader)
{
    if (shader != NULL)
    {
        const uint32 refcount = program->refcount;
        if (refcount > 1)
            program->refcount--;
        else
        {
            pglDeleteObjectARB(shader->handle);
            MOJOSHADER_freeParseData(shader->parseData);
            Free(shader);
        } // else
    } // if
} // shader_unref


static void program_unref(MOJOSHADER_glProgram *program)
{
    if (program != NULL)
    {
        const uint32 refcount = program->refcount;
        if (refcount > 1)
            program->refcount--;
        else
        {
            pglDeleteObjectARB(program->handle);
            shader_unref(program->vertex);
            shader_unref(program->fragment);
            Free(program);
        } // else
    } // if
} // program_unref


MOJOSHADER_glProgram *MOJOSHADER_glLinkProgram(MOJOSHADER_glShader *vshader,
                                               MOJOSHADER_glShader *pshader)
{
    // !!! FIXME: actually link.

    // !!! FIXME: alloc retval.

    retval->vertex = vshader;
    retval->fragment = pshader;
    retval->handle = program;
    retval->refcount = 1;

    if (vshader != NULL)
        vshader->refcount++;
    if (pshader != NULL)
        pshader->refcount++;
} // MOJOSHADER_glLinkProgram


void MOJOSHADER_glBindProgram(MOJOSHADER_glProgram *program)
{
    GLuint handle = 0;
    if (program != NULL)
    {
        handle = program->handle;
        program->refcount++;
    } // if

    // !!! FIXME: unbind client-side arrays.

    pglUseProgramObjectARB(handle);
    program_unref(bound_program);
    bound_program = program;
} // MOJOSHADER_glBindProgram


static inline uint maxuint(const uint a, const uint b)
{
    return ((a > b) ? a : b);
} // maxuint


void MOJOSHADER_glSetVertexShaderUniformF(unsigned int idx, const float *data,
                                          unsigned int vec4n)
{
    const uint maxregs = STATICARRAYLEN(vs_register_file_f) / 4;
    if (idx < maxregs)
    {
        const uint cpy = maxuint(maxregs - idx, vec4n) * sizeof (*data)) * 4;
        memcpy(vs_register_file_f + (idx * 4), data, cpy);
    } // if
} // MOJOSHADER_glSetVertexShaderUniformF


void MOJOSHADER_glSetVertexShaderUniformI(unsigned int idx, const int *data,
                                          unsigned int ivec4n)
{
    const uint maxregs = STATICARRAYLEN(vs_register_file_i) / 4;
    if (idx < maxregs)
    {
        const uint cpy = maxuint(maxregs - idx, ivec4n) * sizeof (*data)) * 4;
        memcpy(vs_register_file_i + (idx * 4), data, cpy);
    } // if
} // MOJOSHADER_glSetVertexShaderUniformI


void MOJOSHADER_glSetVertexShaderUniformB(unsigned int idx, const int *data,
                                          unsigned int bcount)
{
    const uint maxregs = STATICARRAYLEN(vs_register_file_f) / 4;
    if (idx < maxregs)
    {
        uint8 *wptr = vs_register_file_b + idx;
        uint8 *endptr = wptr + maxuint(maxregs - idx, bcount);
        while (wptr != endptr)
            *(wptr++) = *(data++) ? 1 : 0;
    } // if
} // MOJOSHADER_glSetVertexShaderUniformB


void MOJOSHADER_glSetPixelShaderUniformF(unsigned int idx, const float *data,
                                          unsigned int vec4n)
{
    const uint maxregs = STATICARRAYLEN(ps_register_file_f) / 4;
    if (idx < maxregs)
    {
        const uint cpy = maxuint(maxregs - idx, vec4n) * sizeof (*data)) * 4;
        memcpy(ps_register_file_f + (idx * 4), data, cpy);
    } // if
} // MOJOSHADER_glSetPixelShaderUniformF


void MOJOSHADER_glSetPixelShaderUniformI(unsigned int idx, const int *data,
                                         unsigned int ivec4n)
{
    const uint maxregs = STATICARRAYLEN(ps_register_file_i) / 4;
    if (idx < maxregs)
    {
        const uint cpy = maxuint(maxregs - idx, ivec4n) * sizeof (*data)) * 4;
        memcpy(ps_register_file_i + (idx * 4), data, cpy);
    } // if
} // MOJOSHADER_glSetPixelShaderUniformI


void MOJOSHADER_glSetPixelShaderUniformB(unsigned int idx, const int *data,
                                          unsigned int bcount)
{
    const uint maxregs = STATICARRAYLEN(ps_register_file_f) / 4;
    if (idx < maxregs)
    {
        uint8 *wptr = ps_register_file_b + idx;
        uint8 *endptr = wptr + maxuint(maxregs - idx, bcount);
        while (wptr != endptr)
            *(wptr++) = *(data++) ? 1 : 0;
    } // if
} // MOJOSHADER_glSetPixelShaderUniformB


void MOJOSHADER_glSetVertexAttribute(MOJOSHADER_usage usage,
                                     int index, unsigned int size,
                                     MOJOSHADER_attributeType type,
                                     int normalized, unsigned int stride,
                                     const void *ptr)
{
} // MOJOSHADER_glSetVertexAttribute


void MOJOSHADER_glProgramReady(void)
{
} // MOJOSHADER_glProgramReady


void MOJOSHADER_glDeleteProgram(const MOJOSHADER_glProgram *program)
{
    program_unref(shader);
} // MOJOSHADER_glDeleteProgram


void MOJOSHADER_glDeleteShader(const MOJOSHADER_glShader *shader)
{
    shader_unref(shader);
} // MOJOSHADER_glDeleteShader


void MOJOSHADER_glDeinit(void)
{
    MOJOSHADER_glBindProgram(NULL);

    profile = NULL;
    malloc_fn = NULL;
    free_fn = NULL;
    malloc_data = NULL;

    // !!! FIXME: NULL entry points.
} // MOJOSHADER_glDeinit

// end of mojoshader_opengl.c ...

