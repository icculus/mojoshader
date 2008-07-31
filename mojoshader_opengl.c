/**
 * MojoShader; generate shader programs from bytecode of compiled
 *  Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>  // GL headers need this for WINGDIAPI definition.
#endif

#include "mojoshader.h"
#define GL_GLEXT_LEGACY 1
#include "GL/gl.h"
#include "GL/glext.h"

// Get basic wankery out of the way here...

typedef unsigned int uint;  // this is a printf() helper. don't use for code.

#ifdef _MSC_VER
#define snprintf _snprintf
typedef unsigned __int8 uint8;
typedef unsigned __int32 uint32;
typedef __int32 int32;
// Warning Level 4 considered harmful.  :)
#pragma warning(disable: 4100)  // "unreferenced formal parameter"
#pragma warning(disable: 4389)  // "signed/unsigned mismatch"
#else
#include <stdint.h>
typedef uint8_t uint8;
typedef uint32_t uint32;
typedef int32_t int32;
#endif

#define STATICARRAYLEN(x) ( (sizeof ((x))) / (sizeof ((x)[0])) )

#ifndef SUPPORT_PROFILE_GLSL
#define SUPPORT_PROFILE_GLSL 1
#endif

#ifndef SUPPORT_PROFILE_ARB1
#define SUPPORT_PROFILE_ARB1 1
#endif


struct MOJOSHADER_glShader
{
    const MOJOSHADER_parseData *parseData;
    GLuint handle;
    uint32 refcount;
};

typedef struct
{
    MOJOSHADER_shaderType shader_type;
    const MOJOSHADER_uniform *uniform;
    GLuint location;
    union {
        GLint b;
        GLint i[4];
        GLfloat f[4];
    } value;
    GLfloat *uniform_array_buffer;
} UniformMap;

typedef struct
{
    MOJOSHADER_shaderType shader_type;
    const MOJOSHADER_sampler *sampler;
    GLuint location;
    GLint value;
} SamplerMap;

typedef struct
{
    const MOJOSHADER_attribute *attribute;
    GLuint location;
} AttributeMap;

struct MOJOSHADER_glProgram
{
    MOJOSHADER_glShader *vertex;
    MOJOSHADER_glShader *fragment;
    GLuint handle;
    uint32 uniform_count;
    UniformMap *uniforms;
    uint32 sampler_count;
    SamplerMap *samplers;
    uint32 attribute_count;
    AttributeMap *attributes;
    uint32 refcount;
};

#ifndef WINGDIAPI
#define WINGDIAPI
#endif

// Entry points in base OpenGL that lack function pointer prototypes...
typedef WINGDIAPI void (APIENTRYP PFNGLGETINTEGERVPROC) (GLenum pname, GLint *params);
typedef WINGDIAPI const GLubyte * (APIENTRYP PFNGLGETSTRINGPROC) (GLenum name);
typedef WINGDIAPI GLenum (APIENTRYP PFNGLGETERRORPROC) (void);
typedef WINGDIAPI void (APIENTRYP PFNGLENABLEPROC) (GLenum cap);
typedef WINGDIAPI void (APIENTRYP PFNGLDISABLEPROC) (GLenum cap);

struct MOJOSHADER_glContext
{
    // Allocators...
    MOJOSHADER_malloc malloc_fn;
    MOJOSHADER_free free_fn;
    void *malloc_data;

    // The constant register files...
    // Man, it kills me how much memory this takes...
    GLfloat vs_reg_file_f[8192 * 4];
    GLint vs_reg_file_i[2047 * 4];
    GLint vs_reg_file_b[2047];
    GLfloat ps_reg_file_f[8192 * 4];
    GLint ps_reg_file_i[2047 * 4];
    GLint ps_reg_file_b[2047];
    GLuint sampler_reg_file[16];

    // GL stuff...
    int opengl_major;
    int opengl_minor;
    MOJOSHADER_glProgram *bound_program;
    char profile[16];

    // Extensions...
    int have_base_opengl:1;
    int have_GL_ARB_vertex_program:1;
    int have_GL_ARB_fragment_program:1;
    int have_GL_NV_vertex_program2_option:1;
    int have_GL_NV_fragment_program2:1;
    int have_GL_NV_vertex_program3:1;
    int have_GL_NV_gpu_program4:1;
    int have_GL_ARB_shader_objects:1;
    int have_GL_ARB_vertex_shader:1;
    int have_GL_ARB_fragment_shader:1;
    int have_GL_ARB_shading_language_100:1;
    int have_GL_NV_half_float:1;

    // Entry points...
    PFNGLGETSTRINGPROC glGetString;
    PFNGLGETERRORPROC glGetError;
    PFNGLGETINTEGERVPROC glGetIntegerv;
    PFNGLENABLEPROC glEnable;
    PFNGLDISABLEPROC glDisable;
    PFNGLDELETEOBJECTARBPROC glDeleteObject;
    PFNGLATTACHOBJECTARBPROC glAttachObject;
    PFNGLCOMPILESHADERARBPROC glCompileShader;
    PFNGLCREATEPROGRAMOBJECTARBPROC glCreateProgramObject;
    PFNGLCREATESHADEROBJECTARBPROC glCreateShaderObject;
    PFNGLDISABLEVERTEXATTRIBARRAYARBPROC glDisableVertexAttribArray;
    PFNGLENABLEVERTEXATTRIBARRAYARBPROC glEnableVertexAttribArray;
    PFNGLGETATTRIBLOCATIONARBPROC glGetAttribLocation;
    PFNGLGETINFOLOGARBPROC glGetInfoLog;
    PFNGLGETOBJECTPARAMETERIVARBPROC glGetObjectParameteriv;
    PFNGLGETUNIFORMLOCATIONARBPROC glGetUniformLocation;
    PFNGLLINKPROGRAMARBPROC glLinkProgram;
    PFNGLSHADERSOURCEARBPROC glShaderSource;
    PFNGLUNIFORM1IARBPROC glUniform1i;
    PFNGLUNIFORM4FVARBPROC glUniform4fv;
    PFNGLUNIFORM4IVARBPROC glUniform4iv;
    PFNGLUSEPROGRAMOBJECTARBPROC glUseProgramObject;
    PFNGLVERTEXATTRIBPOINTERARBPROC glVertexAttribPointer;
    PFNGLGETPROGRAMIVARBPROC glGetProgramivARB;
    PFNGLGETPROGRAMSTRINGARBPROC glGetProgramStringARB;
    PFNGLPROGRAMLOCALPARAMETER4FVARBPROC glProgramLocalParameter4fvARB;
    PFNGLPROGRAMLOCALPARAMETERI4IVNVPROC glProgramLocalParameterI4ivNV;
    PFNGLDELETEPROGRAMSARBPROC glDeleteProgramsARB;
    PFNGLGENPROGRAMSARBPROC glGenProgramsARB;
    PFNGLBINDPROGRAMARBPROC glBindProgramARB;
    PFNGLPROGRAMSTRINGARBPROC glProgramStringARB;

    // interface for profile-specific things.
    int (*profileMaxUniforms)(MOJOSHADER_shaderType shader_type);
    int (*profileCompileShader)(const MOJOSHADER_parseData *pd, GLuint *s);
    void (*profileDeleteShader)(const GLuint shader);
    void (*profileDeleteProgram)(const GLuint program);
    GLint (*profileGetAttribLocation)(MOJOSHADER_glProgram *program, int idx);
    GLint (*profileGetUniformLocation)(MOJOSHADER_glProgram *, MOJOSHADER_glShader *, int);
    GLint (*profileGetSamplerLocation)(MOJOSHADER_glProgram *, MOJOSHADER_glShader *, int);
    GLuint (*profileLinkProgram)(MOJOSHADER_glShader *, MOJOSHADER_glShader *);
    void (*profileUseProgramObject)(MOJOSHADER_glProgram *program);
    void (*profileUniform4fv)(const MOJOSHADER_parseData *, GLint, GLsizei, GLfloat *);
    void (*profileUniform4iv)(const MOJOSHADER_parseData *, GLint, GLsizei, GLint *);
    void (*profileUniform1i)(const MOJOSHADER_parseData *, GLint, GLint);
    void (*profileSetSampler)(GLint loc, GLuint sampler);
    int (*profileMustLoadConstantArrays)(void);
};


static MOJOSHADER_glContext *ctx = NULL;

// Error state...
static char error_buffer[1024] = { '\0' };

static void set_error(const char *str)
{
    snprintf(error_buffer, sizeof (error_buffer), "%s", str);
} // set_error


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
    void *retval = ctx->malloc_fn((int) len, ctx->malloc_data);
    if (retval == NULL)
        set_error("out of memory");
    return retval;
} // Malloc

static inline void Free(void *ptr)
{
    if (ptr != NULL)
        ctx->free_fn(ptr, ctx->malloc_data);
} // Free


static inline void toggle_gl_state(GLenum state, int val)
{
    if (val)
        ctx->glEnable(state);
    else
        ctx->glDisable(state);
} // toggle_gl_state


// profile-specific implementations...

#if SUPPORT_PROFILE_GLSL
static inline GLenum glsl_shader_type(const MOJOSHADER_shaderType t)
{
    if (t == MOJOSHADER_TYPE_VERTEX)
        return GL_VERTEX_SHADER;
    else if (t == MOJOSHADER_TYPE_PIXEL)
        return GL_FRAGMENT_SHADER;

    // !!! FIXME: geometry shaders?
    return GL_NONE;
} // glsl_shader_type


static int impl_GLSL_MustLoadConstantArrays(void) { return 1; }

static int impl_GLSL_MaxUniforms(MOJOSHADER_shaderType shader_type)
{
    GLenum pname = GL_NONE;
    GLint val = 0;
    if (shader_type == MOJOSHADER_TYPE_VERTEX)
        pname = GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB;
    else if (shader_type == MOJOSHADER_TYPE_PIXEL)
        pname = GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB;
    else
        return -1;

    ctx->glGetIntegerv(pname, &val);
    return (int) val;
} // impl_GLSL_MaxUniforms


static int impl_GLSL_CompileShader(const MOJOSHADER_parseData *pd, GLuint *s)
{
    GLint ok = 0;
    GLint shaderlen = (GLint) pd->output_len;
    const GLenum shader_type = glsl_shader_type(pd->shader_type);
    GLuint shader = ctx->glCreateShaderObject(shader_type);

    ctx->glShaderSource(shader, 1, (const GLchar **) &pd->output, &shaderlen);
    ctx->glCompileShader(shader);
    ctx->glGetObjectParameteriv(shader, GL_OBJECT_COMPILE_STATUS_ARB, &ok);

    if (!ok)
    {
        GLsizei len = 0;
        ctx->glGetInfoLog(shader, sizeof (error_buffer), &len,
                          (GLchar *) error_buffer);
        *s = 0;
        return 0;
    } // if

    *s = shader;
    return 1;
} // impl_GLSL_CompileShader


static void impl_GLSL_DeleteShader(const GLuint shader)
{
    ctx->glDeleteObject(shader);
} // impl_GLSL_DeleteShader


static void impl_GLSL_DeleteProgram(const GLuint program)
{
    ctx->glDeleteObject(program);
} // impl_GLSL_DeleteProgram


static GLint impl_GLSL_GetUniformLocation(MOJOSHADER_glProgram *program,
                                          MOJOSHADER_glShader *shader, int idx)
{
    return ctx->glGetUniformLocation(program->handle,
                                     shader->parseData->uniforms[idx].name);
} // impl_GLSL_GetUniformLocation


static GLint impl_GLSL_GetSamplerLocation(MOJOSHADER_glProgram *program,
                                          MOJOSHADER_glShader *shader, int idx)
{
    return ctx->glGetUniformLocation(program->handle,
                                     shader->parseData->samplers[idx].name);
} // impl_GLSL_GetSamplerLocation


static GLint impl_GLSL_GetAttribLocation(MOJOSHADER_glProgram *program, int idx)
{
    const MOJOSHADER_parseData *pd = program->vertex->parseData;
    const MOJOSHADER_attribute *a = pd->attributes;
    return ctx->glGetAttribLocation(program->handle, a[idx].name);
} // impl_GLSL_GetAttribLocation


static GLuint impl_GLSL_LinkProgram(MOJOSHADER_glShader *vshader,
                                    MOJOSHADER_glShader *pshader)
{
    const GLuint program = ctx->glCreateProgramObject();

    if (vshader != NULL) ctx->glAttachObject(program, vshader->handle);
    if (pshader != NULL) ctx->glAttachObject(program, pshader->handle);

    ctx->glLinkProgram(program);

    GLint ok = 0;
    ctx->glGetObjectParameteriv(program, GL_OBJECT_LINK_STATUS_ARB, &ok);
    if (!ok)
    {
        GLsizei len = 0;
        ctx->glGetInfoLog(program, sizeof (error_buffer), &len, (GLchar *) error_buffer);
        ctx->glDeleteObject(program);
        return 0;
    } // if

    return program;
} // impl_GLSL_LinkProgram


static void impl_GLSL_UseProgramObject(MOJOSHADER_glProgram *program)
{
    ctx->glUseProgramObject((program != NULL) ? program->handle : 0);
} // impl_GLSL_UseProgramObject


static void impl_GLSL_Uniform4fv(const MOJOSHADER_parseData *pd, GLint loc,
                                 GLsizei siz, GLfloat *v)
{
    ctx->glUniform4fv(loc, siz, v);
} // impl_GLSL_Uniform4fv


static void impl_GLSL_Uniform4iv(const MOJOSHADER_parseData *pd, GLint loc,
                                 GLsizei siz, GLint *v)
{
    ctx->glUniform4iv(loc, siz, v);
} // impl_GLSL_Uniform4iv


static void impl_GLSL_Uniform1i(const MOJOSHADER_parseData *pd, GLint loc,
                                GLint v)
{
    ctx->glUniform1i(loc, v);
} // impl_GLSL_Uniform1i


static void impl_GLSL_SetSampler(GLint loc, GLuint sampler)
{
    ctx->glUniform1i(loc, sampler);
} // impl_GLSL_SetSampler

#endif  // SUPPORT_PROFILE_GLSL


#if SUPPORT_PROFILE_ARB1
static inline GLenum arb1_shader_type(const MOJOSHADER_shaderType t)
{
    if (t == MOJOSHADER_TYPE_VERTEX)
        return GL_VERTEX_PROGRAM_ARB;
    else if (t == MOJOSHADER_TYPE_PIXEL)
        return GL_FRAGMENT_PROGRAM_ARB;

    // !!! FIXME: geometry shaders?
    return GL_NONE;
} // arb1_shader_type

static int impl_ARB1_MustLoadConstantArrays(void) { return 0; }

static int impl_ARB1_MaxUniforms(MOJOSHADER_shaderType shader_type)
{
    GLint retval = 0;
    const GLenum program_type = arb1_shader_type(shader_type);
    if (program_type == GL_NONE)
        return -1;

    ctx->glGetProgramivARB(program_type, GL_MAX_PROGRAM_PARAMETERS_ARB, &retval);
    return (int) retval;  // !!! FIXME: times four?
} // impl_ARB1_MaxUniforms


static int impl_ARB1_CompileShader(const MOJOSHADER_parseData *pd, GLuint *s)
{
    GLint shaderlen = (GLint) pd->output_len;
    const GLenum shader_type = arb1_shader_type(pd->shader_type);
    GLuint shader = 0;
    ctx->glGenProgramsARB(1, &shader);

    ctx->glGetError();  // flush any existing error state.
    ctx->glBindProgramARB(shader_type, shader);
    ctx->glProgramStringARB(shader_type, GL_PROGRAM_FORMAT_ASCII_ARB,
                                shaderlen, pd->output);

    if (ctx->glGetError() == GL_INVALID_OPERATION)
    { 
        GLint pos = 0;
        ctx->glGetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB, &pos);
        const GLubyte *errstr = ctx->glGetString(GL_PROGRAM_ERROR_STRING_ARB);
        snprintf(error_buffer, sizeof (error_buffer),
                  "ARB1 compile error at position %d: %s",
                  (int) pos, (const char *) errstr);
        ctx->glBindProgramARB(shader_type, 0);
        ctx->glDeleteProgramsARB(1, &shader);
        *s = 0;
        return 0;
    } // if

    *s = shader;
    return 1;
} // impl_ARB1_CompileShader


static void impl_ARB1_DeleteShader(const GLuint _shader)
{
    GLuint shader = _shader;  // const removal.
    ctx->glDeleteProgramsARB(1, &shader);
} // impl_ARB1_DeleteShader


static void impl_ARB1_DeleteProgram(const GLuint program)
{
    // no-op. ARB1 doesn't have real linked programs.
} // impl_GLSL_DeleteProgram


static GLint impl_ARB1_GetUniformLocation(MOJOSHADER_glProgram *program,
                                          MOJOSHADER_glShader *shader, int idx)
{
    assert(shader->parseData->uniforms[idx].type == MOJOSHADER_UNIFORM_FLOAT);
    return shader->parseData->uniforms[idx].index;  // !!! FIXME: doesn't work if there are int or bool uniforms!
} // impl_ARB1_GetUniformLocation


static GLint impl_ARB1_GetSamplerLocation(MOJOSHADER_glProgram *program,
                                          MOJOSHADER_glShader *shader, int idx)
{
    return shader->parseData->samplers[idx].index;
} // impl_ARB1_GetSamplerLocation


static GLint impl_ARB1_GetAttribLocation(MOJOSHADER_glProgram *program, int idx)
{
    return idx;  // map to vertex arrays in the same order as the parseData.
} // impl_ARB1_GetAttribLocation


static GLuint impl_ARB1_LinkProgram(MOJOSHADER_glShader *vshader,
                                    MOJOSHADER_glShader *pshader)
{
    // there is no formal linking in ARB1...just return a unique value.
    static GLuint retval = 1;
    return retval++;
} // impl_ARB1_LinkProgram


static void impl_ARB1_UseProgramObject(MOJOSHADER_glProgram *program)
{
    GLuint vhandle = 0;
    GLuint phandle = 0;
    if (program != NULL)
    {
        if (program->vertex != NULL)
            vhandle = program->vertex->handle;
        if (program->fragment != NULL)
            phandle = program->fragment->handle;
    } // if

    toggle_gl_state(GL_VERTEX_PROGRAM_ARB, vhandle != 0);
    toggle_gl_state(GL_FRAGMENT_PROGRAM_ARB, phandle != 0);

    ctx->glBindProgramARB(GL_VERTEX_PROGRAM_ARB, vhandle);
    ctx->glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, phandle);
} // impl_ARB1_UseProgramObject


static void impl_ARB1_Uniform4fv(const MOJOSHADER_parseData *pd, GLint loc,
                                 GLsizei siz, GLfloat *v)
{
    int i;
    const GLenum shader_type = arb1_shader_type(pd->shader_type);
    for (i = 0; i < siz; i++, v += 4)
        ctx->glProgramLocalParameter4fvARB(shader_type, loc + i, v);
} // impl_ARB1_Uniform4fv


static void impl_ARB1_Uniform4iv(const MOJOSHADER_parseData *pd, GLint loc,
                                 GLsizei siz, GLint *v)
{
    int i;
    const GLenum shader_type = arb1_shader_type(pd->shader_type);
    for (i = 0; i < siz; i++, v += 4)
    {
        GLfloat f[4] = {
            (GLfloat) v[0], (GLfloat) v[1], (GLfloat) v[2], (GLfloat) v[3]
        };
        ctx->glProgramLocalParameter4fvARB(shader_type, loc + i, f);
    } // for
} // impl_ARB1_Uniform4iv


static void impl_ARB1_Uniform1i(const MOJOSHADER_parseData *pd, GLint loc,
                                GLint _v)
{
    const GLenum shader_type = arb1_shader_type(pd->shader_type);
    const GLfloat v = (GLfloat) _v;
    GLfloat f[4] = { v, v, v, v };
    ctx->glProgramLocalParameter4fvARB(shader_type, loc, f);
} // impl_ARB1_Uniform1i


static void impl_NV4_Uniform4iv(const MOJOSHADER_parseData *pd, GLint loc,
                                 GLsizei siz, GLint *v)
{
    int i;
    const GLenum shader_type = arb1_shader_type(pd->shader_type);
    for (i = 0; i < siz; i++, v += 4)
        ctx->glProgramLocalParameterI4ivNV(shader_type, loc + i, v);
} // impl_NV4_Uniform4iv


static void impl_NV4_Uniform1i(const MOJOSHADER_parseData *pd, GLint loc,
                                GLint _v)
{
    const GLenum shader_type = arb1_shader_type(pd->shader_type);
    GLint v[4] = { _v, _v, _v, _v };
    ctx->glProgramLocalParameterI4ivNV(shader_type, loc, v);
} // impl_NV4_Uniform1i


static void impl_ARB1_SetSampler(GLint loc, GLuint sampler)
{
    // no-op in this profile...arb1 uses the texture units as-is.
    assert(loc == (GLint) sampler);
} // impl_ARB1_SetSampler

#endif  // SUPPORT_PROFILE_ARB1


const char *MOJOSHADER_glGetError(void)
{
    return error_buffer;
} // MOJOSHADER_glGetError


static void *loadsym(void *(*lookup)(const char *fn), const char *fn, int *ext)
{
    void *retval = NULL;
    if (lookup != NULL)
    {
        retval = lookup(fn);
        if (retval == NULL)
        {
            char arbfn[64];
            snprintf(arbfn, sizeof (arbfn), "%sARB", fn);
            retval = lookup(arbfn);
        } // if
    } // if

    if (retval == NULL)
        *ext = 0;

    return retval;
} // loadsym

static void lookup_entry_points(void *(*lookup)(const char *fnname))
{
    #define DO_LOOKUP(ext, typ, fn) { \
        int exist = ctx->have_##ext; \
        ctx->fn = (typ) loadsym(lookup, #fn, &exist); \
        ctx->have_##ext = exist; \
    }

    DO_LOOKUP(base_opengl, PFNGLGETSTRINGPROC, glGetString);
    DO_LOOKUP(base_opengl, PFNGLGETERRORPROC, glGetError);
    DO_LOOKUP(base_opengl, PFNGLGETINTEGERVPROC, glGetIntegerv);
    DO_LOOKUP(base_opengl, PFNGLENABLEPROC, glEnable);
    DO_LOOKUP(base_opengl, PFNGLDISABLEPROC, glDisable);
    DO_LOOKUP(GL_ARB_shader_objects, PFNGLDELETEOBJECTARBPROC, glDeleteObject);
    DO_LOOKUP(GL_ARB_shader_objects, PFNGLATTACHOBJECTARBPROC, glAttachObject);
    DO_LOOKUP(GL_ARB_shader_objects, PFNGLCOMPILESHADERARBPROC, glCompileShader);
    DO_LOOKUP(GL_ARB_shader_objects, PFNGLCREATEPROGRAMOBJECTARBPROC, glCreateProgramObject);
    DO_LOOKUP(GL_ARB_shader_objects, PFNGLCREATESHADEROBJECTARBPROC, glCreateShaderObject);
    DO_LOOKUP(GL_ARB_shader_objects, PFNGLGETINFOLOGARBPROC, glGetInfoLog);
    DO_LOOKUP(GL_ARB_shader_objects, PFNGLGETOBJECTPARAMETERIVARBPROC, glGetObjectParameteriv);
    DO_LOOKUP(GL_ARB_shader_objects, PFNGLGETUNIFORMLOCATIONARBPROC, glGetUniformLocation);
    DO_LOOKUP(GL_ARB_shader_objects, PFNGLLINKPROGRAMARBPROC, glLinkProgram);
    DO_LOOKUP(GL_ARB_shader_objects, PFNGLSHADERSOURCEARBPROC, glShaderSource);
    DO_LOOKUP(GL_ARB_shader_objects, PFNGLUNIFORM1IARBPROC, glUniform1i);
    DO_LOOKUP(GL_ARB_shader_objects, PFNGLUNIFORM4FVARBPROC, glUniform4fv);
    DO_LOOKUP(GL_ARB_shader_objects, PFNGLUNIFORM4IVARBPROC, glUniform4iv);
    DO_LOOKUP(GL_ARB_shader_objects, PFNGLUSEPROGRAMOBJECTARBPROC, glUseProgramObject);
    DO_LOOKUP(GL_ARB_vertex_shader, PFNGLDISABLEVERTEXATTRIBARRAYARBPROC, glDisableVertexAttribArray);
    DO_LOOKUP(GL_ARB_vertex_shader, PFNGLENABLEVERTEXATTRIBARRAYARBPROC, glEnableVertexAttribArray);
    DO_LOOKUP(GL_ARB_vertex_shader, PFNGLGETATTRIBLOCATIONARBPROC, glGetAttribLocation);
    DO_LOOKUP(GL_ARB_vertex_shader, PFNGLVERTEXATTRIBPOINTERARBPROC, glVertexAttribPointer);
    DO_LOOKUP(GL_ARB_vertex_program, PFNGLUSEPROGRAMOBJECTARBPROC, glUseProgramObject);
    DO_LOOKUP(GL_ARB_vertex_program, PFNGLVERTEXATTRIBPOINTERARBPROC, glVertexAttribPointer);
    DO_LOOKUP(GL_ARB_vertex_program, PFNGLGETPROGRAMIVARBPROC, glGetProgramivARB);
    DO_LOOKUP(GL_ARB_vertex_program, PFNGLGETPROGRAMSTRINGARBPROC, glGetProgramStringARB);
    DO_LOOKUP(GL_ARB_vertex_program, PFNGLPROGRAMLOCALPARAMETER4FVARBPROC, glProgramLocalParameter4fvARB);
    DO_LOOKUP(GL_ARB_vertex_program, PFNGLDELETEPROGRAMSARBPROC, glDeleteProgramsARB);
    DO_LOOKUP(GL_ARB_vertex_program, PFNGLGENPROGRAMSARBPROC, glGenProgramsARB);
    DO_LOOKUP(GL_ARB_vertex_program, PFNGLBINDPROGRAMARBPROC, glBindProgramARB);
    DO_LOOKUP(GL_ARB_vertex_program, PFNGLPROGRAMSTRINGARBPROC, glProgramStringARB);
    DO_LOOKUP(GL_NV_gpu_program4, PFNGLPROGRAMLOCALPARAMETERI4IVNVPROC, glProgramLocalParameterI4ivNV);

    #undef DO_LOOKUP
} // lookup_entry_points


static int verify_extension(const char *ext, int have, const char *extlist,
                            int major, int minor)
{
    if (have == 0)
        return 0;  // don't bother checking, we're missing an entry point.

    else if (!ctx->have_base_opengl)
        return 0;  // don't bother checking, we're missing basic functionality.

    // See if it's in the spec for this GL implementation's version.
    if (major >= 0)
    {
        if ( ((ctx->opengl_major << 16) | (ctx->opengl_minor & 0xFFFF)) >=
             ((major << 16) | (minor & 0xFFFF)) )
            return 1;
    } // if

    // Not available in the GL version, check the extension list.
    const char *ptr = strstr(extlist, ext);
    if (ptr == NULL)
        return 0;

    const char endchar = ptr[strlen(ext)];
    if ((endchar == '\0') || (endchar == ' '))
        return 1;  // extension is in the list.

    return 0;  // just not supported, fail.
} // verify_extension


static void parse_opengl_version_str(const char *verstr, int *maj, int *min)
{
    if (verstr == NULL)
        *maj = *min = 0;
    else
        sscanf(verstr, "%d.%d", maj, min);
} // parse_opengl_version_str


static inline void parse_opengl_version(const char *verstr)
{
    parse_opengl_version_str(verstr, &ctx->opengl_major, &ctx->opengl_minor);
} // parse_opengl_version


static int glsl_version_atleast(int maj, int min)
{
    int glslmin = 0;
    int glslmaj = 0;
    ctx->glGetError();  // flush any existing error state.
    const GLenum enumval = GL_SHADING_LANGUAGE_VERSION_ARB;
    const char *str = (const char *) ctx->glGetString(enumval);
    if (ctx->glGetError() == GL_INVALID_ENUM)
        return 0;  // this is a basic, 1.0-compliant implementation.
    parse_opengl_version_str(str, &glslmaj, &glslmin);
    return ( (glslmaj > maj) || ((glslmaj == maj) && (glslmin >= min)) );
} // glsl_version_atleast


static void load_extensions(void *(*lookup)(const char *fnname))
{
    const char *extlist = NULL;

    ctx->have_base_opengl = 1;
    ctx->have_GL_ARB_vertex_program = 1;
    ctx->have_GL_ARB_fragment_program = 1;
    ctx->have_GL_NV_vertex_program2_option = 1;
    ctx->have_GL_NV_fragment_program2 = 1;
    ctx->have_GL_NV_vertex_program3 = 1;
    ctx->have_GL_NV_gpu_program4 = 1;
    ctx->have_GL_ARB_shader_objects = 1;
    ctx->have_GL_ARB_vertex_shader = 1;
    ctx->have_GL_ARB_fragment_shader = 1;
    ctx->have_GL_ARB_shading_language_100 = 1;
    ctx->have_GL_NV_half_float = 1;

    lookup_entry_points(lookup);

    if (!ctx->have_base_opengl)
        set_error("missing basic OpenGL entry points");
    else
    {
        parse_opengl_version((const char *) ctx->glGetString(GL_VERSION));
        extlist = (const char *) ctx->glGetString(GL_EXTENSIONS);
    } // else

    if (extlist == NULL)
        extlist = "";  // just in case.

    #define VERIFY_EXT(ext, major, minor) \
        ctx->have_##ext = verify_extension(#ext, ctx->have_##ext, extlist, major, minor)

    VERIFY_EXT(GL_ARB_vertex_program, -1, -1);
    VERIFY_EXT(GL_ARB_fragment_program, -1, -1);
    VERIFY_EXT(GL_ARB_shader_objects, 2, 0);
    VERIFY_EXT(GL_ARB_vertex_shader, 2, 0);
    VERIFY_EXT(GL_ARB_fragment_shader, 2, 0);
    VERIFY_EXT(GL_ARB_shading_language_100, 2, 0);
    VERIFY_EXT(GL_NV_vertex_program2_option, -1, -1);
    VERIFY_EXT(GL_NV_fragment_program2, -1, -1);
    VERIFY_EXT(GL_NV_vertex_program3, -1, -1);
    VERIFY_EXT(GL_NV_half_float, -1, -1);

    #undef VERIFY_EXT
} // load_extensions


static int valid_profile(const char *profile)
{
    if (!ctx->have_base_opengl)
        return 0;

    #define MUST_HAVE(p, x) \
        if (!ctx->have_##x) { set_error(#p " profile needs " #x); return 0; }

    if (profile == NULL)
    {
        set_error("NULL profile");
        return 0;
    } // if

    #if SUPPORT_PROFILE_ARB1
    else if (strcmp(profile, MOJOSHADER_PROFILE_ARB1) == 0)
    {
        MUST_HAVE(MOJOSHADER_PROFILE_ARB1, GL_ARB_vertex_program);
        MUST_HAVE(MOJOSHADER_PROFILE_ARB1, GL_ARB_fragment_program);
    } // else if

    else if (strcmp(profile, MOJOSHADER_PROFILE_NV2) == 0)
    {
        MUST_HAVE(MOJOSHADER_PROFILE_NV2, GL_ARB_vertex_program);
        MUST_HAVE(MOJOSHADER_PROFILE_NV2, GL_ARB_fragment_program);
        MUST_HAVE(MOJOSHADER_PROFILE_NV2, GL_NV_vertex_program2_option);
        MUST_HAVE(MOJOSHADER_PROFILE_NV2, GL_NV_fragment_program2);
    } // else if

    else if (strcmp(profile, MOJOSHADER_PROFILE_NV3) == 0)
    {
        MUST_HAVE(MOJOSHADER_PROFILE_NV3, GL_ARB_vertex_program);
        MUST_HAVE(MOJOSHADER_PROFILE_NV3, GL_ARB_fragment_program);
        MUST_HAVE(MOJOSHADER_PROFILE_NV3, GL_NV_vertex_program3);
        MUST_HAVE(MOJOSHADER_PROFILE_NV3, GL_NV_fragment_program2);
    } // else if

    else if (strcmp(profile, MOJOSHADER_PROFILE_NV4) == 0)
    {
        MUST_HAVE(MOJOSHADER_PROFILE_NV4, GL_NV_gpu_program4);
    } // else if
    #endif

    #if SUPPORT_PROFILE_GLSL
    else if (strcmp(profile, MOJOSHADER_PROFILE_GLSL120) == 0)
    {
        MUST_HAVE(MOJOSHADER_PROFILE_GLSL, GL_ARB_shader_objects);
        MUST_HAVE(MOJOSHADER_PROFILE_GLSL, GL_ARB_vertex_shader);
        MUST_HAVE(MOJOSHADER_PROFILE_GLSL, GL_ARB_fragment_shader);
        MUST_HAVE(MOJOSHADER_PROFILE_GLSL, GL_ARB_shading_language_100);
        // if you got here, you have all the extensions.
        if (!glsl_version_atleast(1, 20))
            return 0;
    } // else if

    else if (strcmp(profile, MOJOSHADER_PROFILE_GLSL) == 0)
    {
        MUST_HAVE(MOJOSHADER_PROFILE_GLSL, GL_ARB_shader_objects);
        MUST_HAVE(MOJOSHADER_PROFILE_GLSL, GL_ARB_vertex_shader);
        MUST_HAVE(MOJOSHADER_PROFILE_GLSL, GL_ARB_fragment_shader);
        MUST_HAVE(MOJOSHADER_PROFILE_GLSL, GL_ARB_shading_language_100);
    } // else if
    #endif

    else
    {
        set_error("unknown or unsupported profile");
        return 0;
    } // else

    #undef MUST_HAVE

    return 1;
} // valid_profile


static const char *profile_priorities[] = {
#if SUPPORT_PROFILE_GLSL
    MOJOSHADER_PROFILE_GLSL120,
    MOJOSHADER_PROFILE_GLSL,
#endif
#if SUPPORT_PROFILE_ARB1
    MOJOSHADER_PROFILE_NV4,
    MOJOSHADER_PROFILE_NV3,
    MOJOSHADER_PROFILE_NV2,
    MOJOSHADER_PROFILE_ARB1,
#endif
};

int MOJOSHADER_glAvailableProfiles(void *(*lookup)(const char *fnname),
                                   const char **profs, const int size)
{
    int retval = 0;
    MOJOSHADER_glContext _ctx;
    MOJOSHADER_glContext *current_ctx = ctx;

    ctx = &_ctx;
    memset(ctx, '\0', sizeof (MOJOSHADER_glContext));
    load_extensions(lookup);

    if (ctx->have_base_opengl)
    {
        int i;
        for (i = 0; i < STATICARRAYLEN(profile_priorities); i++)
        {
            // !!! FIXME: if Mac OS X <= 10.4, don't ever pick GLSL, even if
            // !!! FIXME:  the system claims it is available.
            const char *profile = profile_priorities[i];
            if (valid_profile(profile))
            {
                if (retval < size)
                    profs[retval] = profile;
                retval++;
            } // if
        } // for
    } // if

    ctx = current_ctx;
    return retval;
} // MOJOSHADER_glAvailableProfiles


const char *MOJOSHADER_glBestProfile(void *(*lookup)(const char *fnname))
{
    const char *prof[STATICARRAYLEN(profile_priorities)];
    if (MOJOSHADER_glAvailableProfiles(lookup, prof, STATICARRAYLEN(prof)) <= 0)
    {
        set_error("no profiles available");
        return NULL;
    } // if

    return prof[0];  // profiles are sorted "best" to "worst."
} // MOJOSHADER_glBestProfile


MOJOSHADER_glContext *MOJOSHADER_glCreateContext(const char *profile,
                                        void *(*lookup)(const char *fnname),
                                        MOJOSHADER_malloc m, MOJOSHADER_free f,
                                        void *d)
{
    MOJOSHADER_glContext *retval = NULL;
    MOJOSHADER_glContext *current_ctx = ctx;
    ctx = NULL;

    if (m == NULL) m = internal_malloc;
    if (f == NULL) f = internal_free;

    ctx = (MOJOSHADER_glContext *) m(sizeof (MOJOSHADER_glContext), d);
    if (ctx == NULL)
    {
        set_error("out of memory");
        goto init_fail;
    } // if

    memset(ctx, '\0', sizeof (MOJOSHADER_glContext));
    ctx->malloc_fn = m;
    ctx->free_fn = f;
    ctx->malloc_data = d;
    snprintf(ctx->profile, sizeof (ctx->profile), "%s", profile);

    load_extensions(lookup);
    if (!valid_profile(profile))
        goto init_fail;

    MOJOSHADER_glBindProgram(NULL);

    // !!! FIXME: generalize this part.
    if (profile == NULL) {}

#if SUPPORT_PROFILE_GLSL
    else if ( (strcmp(profile, MOJOSHADER_PROFILE_GLSL) == 0) ||
              (strcmp(profile, MOJOSHADER_PROFILE_GLSL120) == 0) )
    {
        ctx->profileMaxUniforms = impl_GLSL_MaxUniforms;
        ctx->profileCompileShader = impl_GLSL_CompileShader;
        ctx->profileDeleteShader = impl_GLSL_DeleteShader;
        ctx->profileDeleteProgram = impl_GLSL_DeleteProgram;
        ctx->profileGetAttribLocation = impl_GLSL_GetAttribLocation;
        ctx->profileGetUniformLocation = impl_GLSL_GetUniformLocation;
        ctx->profileGetSamplerLocation = impl_GLSL_GetSamplerLocation;
        ctx->profileLinkProgram = impl_GLSL_LinkProgram;
        ctx->profileUseProgramObject = impl_GLSL_UseProgramObject;
        ctx->profileUniform4fv = impl_GLSL_Uniform4fv;
        ctx->profileUniform4iv = impl_GLSL_Uniform4iv;
        ctx->profileUniform1i = impl_GLSL_Uniform1i;
        ctx->profileSetSampler = impl_GLSL_SetSampler;
        ctx->profileMustLoadConstantArrays = impl_GLSL_MustLoadConstantArrays;
    } // if
#endif

#if SUPPORT_PROFILE_ARB1
    else if ( (strcmp(profile, MOJOSHADER_PROFILE_ARB1) == 0) ||
              (strcmp(profile, MOJOSHADER_PROFILE_NV2) == 0) ||
              (strcmp(profile, MOJOSHADER_PROFILE_NV3) == 0) ||
              (strcmp(profile, MOJOSHADER_PROFILE_NV4) == 0) )
    {
        ctx->profileMaxUniforms = impl_ARB1_MaxUniforms;
        ctx->profileCompileShader = impl_ARB1_CompileShader;
        ctx->profileDeleteShader = impl_ARB1_DeleteShader;
        ctx->profileDeleteProgram = impl_ARB1_DeleteProgram;
        ctx->profileGetAttribLocation = impl_ARB1_GetAttribLocation;
        ctx->profileGetUniformLocation = impl_ARB1_GetUniformLocation;
        ctx->profileGetSamplerLocation = impl_ARB1_GetSamplerLocation;
        ctx->profileLinkProgram = impl_ARB1_LinkProgram;
        ctx->profileUseProgramObject = impl_ARB1_UseProgramObject;
        ctx->profileUniform4fv = impl_ARB1_Uniform4fv;
        ctx->profileUniform4iv = impl_ARB1_Uniform4iv;
        ctx->profileUniform1i = impl_ARB1_Uniform1i;
        ctx->profileSetSampler = impl_ARB1_SetSampler;
        ctx->profileMustLoadConstantArrays = impl_ARB1_MustLoadConstantArrays;

        // GL_NV_gpu_program4 has integer uniform loading support.
        if (strcmp(profile, MOJOSHADER_PROFILE_NV4) == 0)
        {
            ctx->profileUniform4iv = impl_NV4_Uniform4iv;
            ctx->profileUniform1i = impl_NV4_Uniform1i;
        } // if
    } // if
#endif

    assert(ctx->profileMaxUniforms != NULL);
    assert(ctx->profileCompileShader != NULL);
    assert(ctx->profileDeleteShader != NULL);
    assert(ctx->profileDeleteProgram != NULL);
    assert(ctx->profileMaxUniforms != NULL);
    assert(ctx->profileGetAttribLocation != NULL);
    assert(ctx->profileGetUniformLocation != NULL);
    assert(ctx->profileGetSamplerLocation != NULL);
    assert(ctx->profileLinkProgram != NULL);
    assert(ctx->profileUseProgramObject != NULL);
    assert(ctx->profileUniform4fv != NULL);
    assert(ctx->profileUniform4iv != NULL);
    assert(ctx->profileUniform1i != NULL);
    assert(ctx->profileSetSampler != NULL);
    assert(ctx->profileMustLoadConstantArrays != NULL);

    retval = ctx;
    ctx = current_ctx;
    return retval;

init_fail:
    if (ctx != NULL)
        f(ctx, d);
    ctx = current_ctx;
    return NULL;
} // MOJOSHADER_glCreateContext


void MOJOSHADER_glMakeContextCurrent(MOJOSHADER_glContext *_ctx)
{
    ctx = _ctx;
} // MOJOSHADER_glMakeContextCurrent


int MOJOSHADER_glMaxUniforms(MOJOSHADER_shaderType shader_type)
{
    return ctx->profileMaxUniforms(shader_type);
} // MOJOSHADER_glMaxUniforms


MOJOSHADER_glShader *MOJOSHADER_glCompileShader(const unsigned char *tokenbuf,
                                                const unsigned int bufsize)
{
    MOJOSHADER_glShader *retval = NULL;
    GLuint shader = 0;
    const MOJOSHADER_parseData *pd = MOJOSHADER_parse(ctx->profile, tokenbuf,
                                                      bufsize, ctx->malloc_fn,
                                                      ctx->free_fn,
                                                      ctx->malloc_data);
    if (pd->error != NULL)
    {
        set_error(pd->error);
        goto compile_shader_fail;
    } // if

    retval = (MOJOSHADER_glShader *) Malloc(sizeof (MOJOSHADER_glShader));
    if (retval == NULL)
        goto compile_shader_fail;

    if (!ctx->profileCompileShader(pd, &shader))
        goto compile_shader_fail;

    retval->parseData = pd;
    retval->handle = shader;
    retval->refcount = 1;
    return retval;

compile_shader_fail:
    MOJOSHADER_freeParseData(pd);
    Free(retval);
    if (shader != 0)
        ctx->profileDeleteShader(shader);
    return NULL;
} // MOJOSHADER_glCompileShader


const MOJOSHADER_parseData *MOJOSHADER_glGetShaderParseData(
                                                MOJOSHADER_glShader *shader)
{
    return (shader != NULL) ? shader->parseData : NULL;
} // MOJOSHADER_glGetShaderParseData


static void shader_unref(MOJOSHADER_glShader *shader)
{
    if (shader != NULL)
    {
        const uint32 refcount = shader->refcount;
        if (refcount > 1)
            shader->refcount--;
        else
        {
            ctx->profileDeleteShader(shader->handle);
            MOJOSHADER_freeParseData(shader->parseData);
            Free(shader);
        } // else
    } // if
} // shader_unref


static void program_unref(MOJOSHADER_glProgram *program)
{
    if (program != NULL)
    {
        uint32 i;
        const uint32 refcount = program->refcount;
        if (refcount > 1)
            program->refcount--;
        else
        {
            ctx->profileDeleteProgram(program->handle);
            shader_unref(program->vertex);
            shader_unref(program->fragment);
            for (i = 0; i < program->uniform_count; i++)
                Free(program->uniforms[i].uniform_array_buffer);
            Free(program->samplers);
            Free(program->uniforms);
            Free(program->attributes);
            Free(program);
        } // else
    } // if
} // program_unref


static void fill_constant_array(GLfloat *f, const int base, const int size,
                                const MOJOSHADER_parseData *pd)
{
    int i;
    int filled = 0;
    for (i = 0; i < pd->constant_count; i++)
    {
        const MOJOSHADER_constant *c = &pd->constants[i];
        if (c->type != MOJOSHADER_UNIFORM_FLOAT)
            continue;
        else if (c->index < base)
            continue;
        else if (c->index >= (base+size))
            continue;
        memcpy(&f[(c->index-base) * 4], &c->value.f, sizeof (c->value.f));
        filled++;
    } // for

    assert(filled == size);
} // fill_constant_array


static void lookup_uniforms(MOJOSHADER_glProgram *program,
                            MOJOSHADER_glShader *shader)
{
    const MOJOSHADER_parseData *pd = shader->parseData;
    const MOJOSHADER_shaderType shader_type = pd->shader_type;
    int i;

    for (i = 0; i < pd->uniform_count; i++)
    {
        const MOJOSHADER_uniform *u = &pd->uniforms[i];
        const GLint loc = ctx->profileGetUniformLocation(program, shader, i);
        if (loc != -1)  // maybe the Uniform was optimized out?
        {
            // only do constants once, at link time. These aren't changed ever.
            if ( (u->constant) && (ctx->profileMustLoadConstantArrays()) )
            {
                const int base = u->index;
                const int size = u->array_count;
                GLfloat *f = (GLfloat *) alloca(sizeof (GLfloat) * size * 4);
                fill_constant_array(f, base, size, pd);
                ctx->profileUseProgramObject(program);
                ctx->profileUniform4fv(pd, loc, size, f);
            } // if
            else
            {
                UniformMap *map = &program->uniforms[program->uniform_count];
                map->shader_type = shader_type;
                map->uniform = u;
                map->location = (GLuint) loc;
                program->uniform_count++;
            } // else
        } // if
    } // for
} // lookup_uniforms


static void lookup_samplers(MOJOSHADER_glProgram *program,
                            MOJOSHADER_glShader *shader)
{
    int i;
    const MOJOSHADER_parseData *pd = shader->parseData;
    const MOJOSHADER_sampler *s = pd->samplers;
    const MOJOSHADER_shaderType shader_type = pd->shader_type;

    for (i = 0; i < pd->sampler_count; i++)
    {
        const GLint loc = ctx->profileGetSamplerLocation(program, shader, i);
        if (loc != -1)  // maybe the Sampler was optimized out?
        {
            SamplerMap *map = &program->samplers[program->sampler_count];
            map->shader_type = shader_type;
            map->sampler = &s[i];
            map->location = (GLuint) loc;
            map->value = -1;  // hopefully not a valid texture unit.
            program->sampler_count++;
        } // if
    } // for
} // lookup_samplers


static void lookup_attributes(MOJOSHADER_glProgram *program)
{
    int i;
    const MOJOSHADER_parseData *pd = program->vertex->parseData;
    const MOJOSHADER_attribute *a = pd->attributes;

    for (i = 0; i < pd->attribute_count; i++)
    {
        const GLint loc = ctx->profileGetAttribLocation(program, i);
        if (loc != -1)  // maybe the Attribute was optimized out?
        {
            AttributeMap *map = &program->attributes[program->attribute_count];
            map->attribute = &a[i];
            map->location = (GLuint) loc;
            program->attribute_count++;
        } // if
    } // for
} // lookup_attributes


// !!! FIXME: misnamed
// build a list of indexes that need to be overwritten with constant values
//  when pushing a uniform array to the GL.
static int build_constants_lists(MOJOSHADER_glProgram *program)
{
    int i;
    const int count = program->uniform_count;
    for (i = 0; i < count; i++)
    {
        UniformMap *map = &program->uniforms[i];
        const MOJOSHADER_uniform *u = map->uniform;
        const int size = u->array_count;

        assert(!u->constant);

        if (size == 0)
            continue;  // nothing to see here.

        // only use arrays for 'c' registers.
        const MOJOSHADER_uniformType type = u->type;
        assert(type == MOJOSHADER_UNIFORM_FLOAT);

        const size_t len = size * sizeof (GLfloat) * 4;
        map->uniform_array_buffer = (GLfloat *) Malloc(len);
        if (map->uniform_array_buffer == NULL)
            return 0;
        memset(map->uniform_array_buffer, 0xFF, len);
    } // for

    return 1;
} // build_constants_lists


MOJOSHADER_glProgram *MOJOSHADER_glLinkProgram(MOJOSHADER_glShader *vshader,
                                               MOJOSHADER_glShader *pshader)
{
    if ((vshader == NULL) && (pshader == NULL))
        return NULL;

    int numregs = 0;
    MOJOSHADER_glProgram *retval = NULL;
    const GLuint program = ctx->profileLinkProgram(vshader, pshader);
    if (program == 0)
        goto link_program_fail;

    retval = (MOJOSHADER_glProgram *) Malloc(sizeof (MOJOSHADER_glProgram));
    if (retval == NULL)
        goto link_program_fail;
    memset(retval, '\0', sizeof (MOJOSHADER_glProgram));

    numregs = 0;
    if (vshader != NULL) numregs += vshader->parseData->uniform_count;
    if (pshader != NULL) numregs += pshader->parseData->uniform_count;
    if (numregs > 0)
    {
        const size_t len = sizeof (UniformMap) * numregs;
        retval->uniforms = (UniformMap *) Malloc(len);
        if (retval->uniforms == NULL)
            goto link_program_fail;
        memset(retval->uniforms, '\0', len);
    } // if

    numregs = 0;
    if (vshader != NULL) numregs += vshader->parseData->sampler_count;
    if (pshader != NULL) numregs += pshader->parseData->sampler_count;
    if (numregs > 0)
    {
        const size_t len = sizeof (SamplerMap) * numregs;
        retval->samplers = (SamplerMap *) Malloc(len);
        if (retval->samplers == NULL)
            goto link_program_fail;
        memset(retval->samplers, '\0', len);
    } // if

    retval->handle = program;
    retval->vertex = vshader;
    retval->fragment = pshader;
    retval->refcount = 1;

    if (vshader != NULL)
    {
        if (vshader->parseData->attribute_count > 0)
        {
            const int count = vshader->parseData->attribute_count;
            const size_t len = sizeof (AttributeMap) * count;
            retval->attributes = (AttributeMap *) Malloc(len);
            if (retval->attributes == NULL)
                goto link_program_fail;

            memset(retval->attributes, '\0', len);
            lookup_attributes(retval);
        } // if

        lookup_uniforms(retval, vshader);
        lookup_samplers(retval, vshader);
        vshader->refcount++;
    } // if

    if (pshader != NULL)
    {
        lookup_uniforms(retval, pshader);
        lookup_samplers(retval, pshader);
        pshader->refcount++;
    } // if

    if (!build_constants_lists(retval))
        goto link_program_fail;

    return retval;

link_program_fail:
    if (retval != NULL)
    {
        uint32 i;
        for (i = 0; i < retval->uniform_count; i++)
            Free(retval->uniforms[i].uniform_array_buffer);
        Free(retval->samplers);
        Free(retval->uniforms);
        Free(retval->attributes);
        Free(retval);
    } // if

    if (program != 0)
        ctx->profileDeleteProgram(program);

    return NULL;
} // MOJOSHADER_glLinkProgram


void MOJOSHADER_glBindProgram(MOJOSHADER_glProgram *program)
{
    GLuint handle = 0;
    int i;

    if (program == ctx->bound_program)
        return;  // nothing to do.

    // Disable any client-side arrays the current program could have used.
    // !!! FIXME: don't disable yet...see which ones get reused, and disable
    // !!! FIXME:  only what we don't need in MOJOSHADER_glProgramReady().
    if (ctx->bound_program != NULL)
    {
        const int count = ctx->bound_program->attribute_count;
        for (i = 0; i < count; i++)
        {
            const AttributeMap *map = &ctx->bound_program->attributes[i];
            ctx->glDisableVertexAttribArray(map->location);
        } // if
    } // for

    if (program != NULL)
    {
        handle = program->handle;
        program->refcount++;
    } // if

    ctx->profileUseProgramObject(program);
    program_unref(ctx->bound_program);
    ctx->bound_program = program;
} // MOJOSHADER_glBindProgram


static inline uint minuint(const uint a, const uint b)
{
    return ((a < b) ? a : b);
} // minuint


void MOJOSHADER_glSetVertexShaderUniformF(unsigned int idx, const float *data,
                                          unsigned int vec4n)
{
    const uint maxregs = STATICARRAYLEN(ctx->vs_reg_file_f) / 4;
    if (idx < maxregs)
    {
        assert(sizeof (GLfloat) == sizeof (float));
        const uint cpy = (minuint(maxregs - idx, vec4n) * sizeof (*data)) * 4;
        memcpy(ctx->vs_reg_file_f + (idx * 4), data, cpy);
    } // if
} // MOJOSHADER_glSetVertexShaderUniformF


void MOJOSHADER_glSetVertexShaderUniformI(unsigned int idx, const int *data,
                                          unsigned int ivec4n)
{
    const uint maxregs = STATICARRAYLEN(ctx->vs_reg_file_i) / 4;
    if (idx < maxregs)
    {
        assert(sizeof (GLint) == sizeof (int));
        const uint cpy = (minuint(maxregs - idx, ivec4n) * sizeof (*data)) * 4;
        memcpy(ctx->vs_reg_file_i + (idx * 4), data, cpy);
    } // if
} // MOJOSHADER_glSetVertexShaderUniformI


void MOJOSHADER_glSetVertexShaderUniformB(unsigned int idx, const int *data,
                                          unsigned int bcount)
{
    const uint maxregs = STATICARRAYLEN(ctx->vs_reg_file_f) / 4;
    if (idx < maxregs)
    {
        GLint *wptr = ctx->vs_reg_file_b + idx;
        GLint *endptr = wptr + minuint(maxregs - idx, bcount);
        while (wptr != endptr)
            *(wptr++) = *(data++) ? 1 : 0;
    } // if
} // MOJOSHADER_glSetVertexShaderUniformB


void MOJOSHADER_glSetPixelShaderUniformF(unsigned int idx, const float *data,
                                         unsigned int vec4n)
{
    const uint maxregs = STATICARRAYLEN(ctx->ps_reg_file_f) / 4;
    if (idx < maxregs)
    {
        assert(sizeof (GLfloat) == sizeof (float));
        const uint cpy = (minuint(maxregs - idx, vec4n) * sizeof (*data)) * 4;
        memcpy(ctx->ps_reg_file_f + (idx * 4), data, cpy);
    } // if
} // MOJOSHADER_glSetPixelShaderUniformF


void MOJOSHADER_glSetPixelShaderUniformI(unsigned int idx, const int *data,
                                         unsigned int ivec4n)
{
    const uint maxregs = STATICARRAYLEN(ctx->ps_reg_file_i) / 4;
    if (idx < maxregs)
    {
        assert(sizeof (GLint) == sizeof (int));
        const uint cpy = (minuint(maxregs - idx, ivec4n) * sizeof (*data)) * 4;
        memcpy(ctx->ps_reg_file_i + (idx * 4), data, cpy);
    } // if
} // MOJOSHADER_glSetPixelShaderUniformI


void MOJOSHADER_glSetPixelShaderUniformB(unsigned int idx, const int *data,
                                         unsigned int bcount)
{
    const uint maxregs = STATICARRAYLEN(ctx->ps_reg_file_f) / 4;
    if (idx < maxregs)
    {
        GLint *wptr = ctx->ps_reg_file_b + idx;
        GLint *endptr = wptr + minuint(maxregs - idx, bcount);
        while (wptr != endptr)
            *(wptr++) = *(data++) ? 1 : 0;
    } // if
} // MOJOSHADER_glSetPixelShaderUniformB


static inline GLenum opengl_attr_type(const MOJOSHADER_attributeType type)
{
    switch (type)
    {
        case MOJOSHADER_ATTRIBUTE_UNKNOWN: return GL_NONE; // oh well.
        case MOJOSHADER_ATTRIBUTE_BYTE: return GL_BYTE;
        case MOJOSHADER_ATTRIBUTE_UBYTE: return GL_UNSIGNED_BYTE;
        case MOJOSHADER_ATTRIBUTE_SHORT: return GL_SHORT;
        case MOJOSHADER_ATTRIBUTE_USHORT: return GL_UNSIGNED_SHORT;
        case MOJOSHADER_ATTRIBUTE_INT: return GL_INT;
        case MOJOSHADER_ATTRIBUTE_UINT: return GL_UNSIGNED_INT;
        case MOJOSHADER_ATTRIBUTE_FLOAT: return GL_FLOAT;
        case MOJOSHADER_ATTRIBUTE_DOUBLE: return GL_DOUBLE;

        case MOJOSHADER_ATTRIBUTE_HALF_FLOAT:
            if (ctx->have_GL_NV_half_float)
                return GL_HALF_FLOAT_NV;
            break;
    } // switch

    return GL_NONE;  // oh well. Raises a GL error later.
} // opengl_attr_type


// !!! FIXME: shouldn't (index) be unsigned?
void MOJOSHADER_glSetVertexAttribute(MOJOSHADER_usage usage,
                                     int index, unsigned int size,
                                     MOJOSHADER_attributeType type,
                                     int normalized, unsigned int stride,
                                     const void *ptr)
{
    if ((ctx->bound_program == NULL) || (ctx->bound_program->vertex == NULL))
        return;

    const GLenum gl_type = opengl_attr_type(type);
    const GLboolean norm = (normalized) ? GL_TRUE : GL_FALSE;
    GLuint gl_index = 0;

    // We have to map POSITION0 to generic vertex attribute 0; the
    //  GL_ARB_vertex_shader spec says this is equivalent to using
    //  glVertexPointer(), but without the limitations of that entry point.

    if ((usage != MOJOSHADER_USAGE_POSITION) || (index != 0))
    {
        int i;
        const int count = ctx->bound_program->attribute_count;
        for (i = 0; i < count; i++)
        {
            const AttributeMap *map = &ctx->bound_program->attributes[i];
            const MOJOSHADER_attribute *a = map->attribute;

            // !!! FIXME: is this array guaranteed to be sorted by usage?
            // !!! FIXME:  if so, we can break if a->usage > usage.

            if ((a->usage == usage) && (a->index == index))
            {
                gl_index = map->location;
                break;
            } // if
        } // for

        if (gl_index == 0)
            return;  // nothing to do, this shader doesn't use this stream.
    } // if

    // these happen to work in both ARB1 and GLSL, but if something alien
    //  shows up, we'll have to split these into profile*() functions.
    ctx->glVertexAttribPointer(gl_index, size, gl_type, norm, stride, ptr);
    ctx->glEnableVertexAttribArray(gl_index);
} // MOJOSHADER_glSetVertexAttribute


void MOJOSHADER_glProgramReady(void)
{
    int i;
    int count;

    if (ctx->bound_program == NULL)
        return;  // nothing to do.

    // push Uniforms to the program from our register files...
    count = ctx->bound_program->uniform_count;
    for (i = 0; i < count; i++)
    {
        UniformMap *map = &ctx->bound_program->uniforms[i];
        const MOJOSHADER_uniform *u = map->uniform;
        const MOJOSHADER_uniformType type = u->type;
        const MOJOSHADER_shaderType shader_type = map->shader_type;
        const int index = u->index;
        const int size = u->array_count;
        const GLint location = map->location;
        const MOJOSHADER_parseData *pd;
        GLfloat *regf;
        GLint *regi;
        GLint *regb;

        assert(!u->constant);

        if (shader_type == MOJOSHADER_TYPE_VERTEX)
        {
            pd = ctx->bound_program->vertex->parseData;
            regf = ctx->vs_reg_file_f;
            regi = ctx->vs_reg_file_i;
            regb = ctx->vs_reg_file_b;
        } // if

        else if (shader_type == MOJOSHADER_TYPE_PIXEL)
        {
            pd = ctx->bound_program->fragment->parseData;
            regf = ctx->ps_reg_file_f;
            regi = ctx->ps_reg_file_i;
            regb = ctx->ps_reg_file_b;
        } // else if

        else
        {
            assert(0);  // !!! FIXME: geometry shaders?
        } // else


        if (size != 0)  // uniform array?
        {
            GLfloat *f = &regf[index * 4];
            const size_t len = size * sizeof (GLfloat) * 4;

            GLfloat *current = map->uniform_array_buffer;
            //if (memcmp(current, f, len) != 0)
            {
                // array has changed, upload it.
                ctx->profileUniform4fv(pd, location, size, f);
                memcpy(current, f, len);
            } // if
        } // if

        else
        {
            if (type == MOJOSHADER_UNIFORM_FLOAT)
            {
                GLfloat *f = &regf[index * 4];
                //if (memcmp(map->value.f, f, sizeof (map->value.f)) != 0)
                {
                    memcpy(map->value.f, f, sizeof (map->value.f));
                    ctx->profileUniform4fv(pd, location, 1, f);
                } // if
            } // if
            else if (type == MOJOSHADER_UNIFORM_INT)
            {
                GLint *i = &regi[index * 4];
                //if (memcmp(map->value.i, i, sizeof (map->value.i)) != 0)
                {
                    memcpy(map->value.i, i, sizeof (map->value.i));
                    ctx->profileUniform4iv(pd, location, 1, i);
                } // if
            } // else if
            else if (type == MOJOSHADER_UNIFORM_BOOL)
            {
                const GLint b = regb[index];
                //if (b != map->value.b)
                {
                    map->value.b = b;
                    ctx->profileUniform1i(pd, location, b);
                } // if
            } // else if
        } // if
    } // for

    // push Samplers to the program from our register files...
    count = ctx->bound_program->sampler_count;
    for (i = 0; i < count; i++)
    {
        SamplerMap *map = &ctx->bound_program->samplers[i];
        const MOJOSHADER_sampler *s = map->sampler;
        if (s->index != map->value)
        {
            map->value = s->index;
            ctx->profileSetSampler(map->location, s->index);
        } // if
    } // for
} // MOJOSHADER_glProgramReady


void MOJOSHADER_glDeleteProgram(MOJOSHADER_glProgram *program)
{
    program_unref(program);
} // MOJOSHADER_glDeleteProgram


void MOJOSHADER_glDeleteShader(MOJOSHADER_glShader *shader)
{
    shader_unref(shader);
} // MOJOSHADER_glDeleteShader


void MOJOSHADER_glDestroyContext(MOJOSHADER_glContext *_ctx)
{
    MOJOSHADER_glContext *current_ctx = ctx;
    ctx = _ctx;
    MOJOSHADER_glBindProgram(NULL);
    lookup_entry_points(NULL);
    Free(ctx);
    ctx = ((current_ctx == _ctx) ? NULL : current_ctx);
} // MOJOSHADER_glDestroyContext

// end of mojoshader_opengl.c ...

