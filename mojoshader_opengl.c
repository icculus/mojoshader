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
typedef unsigned __int32 int32;
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

struct MOJOSHADER_glShader
{
    const MOJOSHADER_parseData *parseData;
    GLhandleARB handle;
    uint32 refcount;
};

typedef struct
{
    MOJOSHADER_shaderType shader_type;
    const MOJOSHADER_uniform *uniform;
    GLuint location;
} UniformMap;

typedef struct
{
    MOJOSHADER_shaderType shader_type;
    const MOJOSHADER_sampler *sampler;
    GLuint location;
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
    GLhandleARB handle;
    uint32 constant_count;  // !!! FIXME: misnamed.
    GLfloat *constants;  // !!! FIXME: misnamed.
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
    int have_base_opengl;
    int have_GL_ARB_shader_objects;
    int have_GL_ARB_vertex_shader;
    int have_GL_ARB_fragment_shader;
    int have_GL_ARB_shading_language_100;
    int have_GL_NV_half_float;

    // Entry points...
    PFNGLGETSTRINGPROC glGetString;
    PFNGLGETINTEGERVPROC glGetIntegerv;
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
    void *retval = ctx->malloc_fn(len, ctx->malloc_data);
    if (retval == NULL)
        set_error("out of memory");
    return retval;
} // Malloc

static inline void Free(void *ptr)
{
    if (ptr != NULL)
        ctx->free_fn(ptr, ctx->malloc_data);
} // Free


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
    #define DO_LOOKUP(ext, typ, fn) ctx->fn = (typ) loadsym(lookup, #fn, &ctx->have_##ext)
    DO_LOOKUP(base_opengl, PFNGLGETSTRINGPROC, glGetString);
    DO_LOOKUP(base_opengl, PFNGLGETINTEGERVPROC, glGetIntegerv);
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


static void parse_opengl_version(const char *verstr)
{
    if (verstr == NULL)
        ctx->opengl_major = ctx->opengl_minor = 0;
    else
        sscanf(verstr, "%d.%d", &ctx->opengl_major, &ctx->opengl_minor);
} // parse_opengl_version


static void load_extensions(void *(*lookup)(const char *fnname))
{
    const char *extlist = NULL;

    ctx->have_base_opengl = 1;
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

    VERIFY_EXT(GL_ARB_shader_objects, 2, 0);
    VERIFY_EXT(GL_ARB_vertex_shader, 2, 0);
    VERIFY_EXT(GL_ARB_fragment_shader, 2, 0);
    VERIFY_EXT(GL_ARB_shading_language_100, 2, 0);
    VERIFY_EXT(GL_NV_half_float, -1, -1);

    #undef VERIFY_EXT
} // load_extensions


static int valid_profile(const char *profile)
{
    if (!ctx->have_base_opengl)
        return 0;

    #define MUST_HAVE(p, x) \
        if (!ctx->have_##x) { set_error(#p " profile needs " #x); return 0; }

    if (0) {}

    #if SUPPORT_PROFILE_GLSL
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


const char *MOJOSHADER_glBestProfile(void *(*lookup)(const char *fnname))
{
    const char *retval = NULL;
    MOJOSHADER_glContext _ctx;
    MOJOSHADER_glContext *current_ctx = ctx;

    ctx = &_ctx;
    memset(ctx, '\0', sizeof (MOJOSHADER_glContext));
    load_extensions(lookup);

    if (ctx->have_base_opengl)
    {
        static const char *priority[] = {
            MOJOSHADER_PROFILE_GLSL,
        };

        int i;
        for (i = 0; i < STATICARRAYLEN(priority); i++)
        {
            if (valid_profile(priority[i]))
            {
                retval = priority[i];
                break;
            } // if
        } // for

        if (retval == NULL)
            set_error("no profiles available");
    } // if

    ctx = current_ctx;
    return retval;
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
} // MOJOSHADER_glMaxUniforms


MOJOSHADER_glShader *MOJOSHADER_glCompileShader(const unsigned char *tokenbuf,
                                                const unsigned int bufsize)
{
    MOJOSHADER_glShader *retval = NULL;
    GLhandleARB shader = 0;
    const MOJOSHADER_parseData *pd = MOJOSHADER_parse(ctx->profile, tokenbuf,
                                                      bufsize, ctx->malloc_fn,
                                                      ctx->free_fn,
                                                      ctx->malloc_data);
    GLint ok = 0;
    const GLenum shader_type = (pd->shader_type == MOJOSHADER_TYPE_PIXEL) ? GL_FRAGMENT_SHADER : GL_VERTEX_SHADER;
    GLint shaderlen = (GLint) pd->output_len;

    if (pd->error != NULL)
    {
        set_error(pd->error);
        goto compile_shader_fail;
    } // if

    retval = (MOJOSHADER_glShader *) Malloc(sizeof (MOJOSHADER_glShader));
    if (retval == NULL)
        goto compile_shader_fail;

    shader = ctx->glCreateShaderObject(shader_type);

    ctx->glShaderSource(shader, 1, (const GLchar **) &pd->output, &shaderlen);
    ctx->glCompileShader(shader);
    ctx->glGetObjectParameteriv(shader, GL_OBJECT_COMPILE_STATUS_ARB, &ok);

    if (!ok)
    {
        GLsizei len = 0;
        ctx->glGetInfoLog(shader, sizeof (error_buffer), &len, (GLchar *) error_buffer);
        goto compile_shader_fail;
    } // if

    retval->parseData = pd;
    retval->handle = shader;
    retval->refcount = 1;
    return retval;

compile_shader_fail:
    MOJOSHADER_freeParseData(pd);
    Free(retval);
    if (shader != 0)
        ctx->glDeleteObject(shader);
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
            ctx->glDeleteObject(shader->handle);
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
            ctx->glDeleteObject(program->handle);
            shader_unref(program->vertex);
            shader_unref(program->fragment);
            Free(program->constants);
            Free(program->samplers);
            Free(program->uniforms);
            Free(program->attributes);
            Free(program);
        } // else
    } // if
} // program_unref


static void lookup_uniforms(MOJOSHADER_glProgram *program,
                            MOJOSHADER_glShader *shader)
{
    int i;
    const MOJOSHADER_parseData *pd = shader->parseData;
    const MOJOSHADER_uniform *u = pd->uniforms;
    const MOJOSHADER_shaderType shader_type = pd->shader_type;

    for (i = 0; i < pd->uniform_count; i++)
    {
        const GLint loc = ctx->glGetUniformLocation(program->handle, u[i].name);
        if (loc != -1)  // maybe the Uniform was optimized out?
        {
            UniformMap *map = &program->uniforms[program->uniform_count];
            map->shader_type = shader_type;
            map->uniform = &u[i];
            map->location = (GLuint) loc;
            program->uniform_count++;
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
        const GLint loc = ctx->glGetUniformLocation(program->handle, s[i].name);
        if (loc != -1)  // maybe the Sampler was optimized out?
        {
            SamplerMap *map = &program->samplers[program->sampler_count];
            map->shader_type = shader_type;
            map->sampler = &s[i];
            map->location = (GLuint) loc;
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
        const GLint loc = ctx->glGetAttribLocation(program->handle, a->name);
        if (loc != -1)  // maybe the Attribute was optimized out?
        {
            AttributeMap *map = &program->attributes[program->attribute_count];
            map->attribute = &a[i];
            map->location = (GLuint) loc;
            program->attribute_count++;
        } // if
    } // for
} // lookup_attributes


MOJOSHADER_glProgram *MOJOSHADER_glLinkProgram(MOJOSHADER_glShader *vshader,
                                               MOJOSHADER_glShader *pshader)
{
    if ((vshader == NULL) && (pshader == NULL))
        return NULL;

    MOJOSHADER_glProgram *retval = NULL;
    const GLhandleARB program = ctx->glCreateProgramObject();
    int numregs = 0;
    uint32 const_count = 0;

    if (vshader != NULL) ctx->glAttachObject(program, vshader->handle);
    if (pshader != NULL) ctx->glAttachObject(program, pshader->handle);

    ctx->glLinkProgram(program);

    GLint ok = 0;
    ctx->glGetObjectParameteriv(program, GL_OBJECT_LINK_STATUS_ARB, &ok);
    if (!ok)
    {
        GLsizei len = 0;
        ctx->glGetInfoLog(program, sizeof (error_buffer), &len, (GLchar *) error_buffer);
        goto link_program_fail;
    } // if

    retval = (MOJOSHADER_glProgram *) Malloc(sizeof (MOJOSHADER_glProgram));
    if (retval == NULL)
        goto link_program_fail;
    memset(retval, '\0', sizeof (MOJOSHADER_glProgram));

    numregs = 0;
    if (vshader != NULL) numregs += vshader->parseData->uniform_count;
    if (pshader != NULL) numregs += pshader->parseData->uniform_count;
    retval->uniforms = (UniformMap *) Malloc(sizeof (UniformMap) * numregs);
    if (retval->uniforms == NULL)
        goto link_program_fail;
    memset(retval->uniforms, '\0', sizeof (UniformMap) * numregs);

    numregs = 0;
    if (vshader != NULL) numregs += vshader->parseData->sampler_count;
    if (pshader != NULL) numregs += pshader->parseData->sampler_count;
    retval->samplers = (SamplerMap *) Malloc(sizeof (SamplerMap) * numregs);
    if (retval->samplers == NULL)
        goto link_program_fail;
    memset(retval->samplers, '\0', sizeof (SamplerMap) * numregs);

    retval->handle = program;
    retval->vertex = vshader;
    retval->fragment = pshader;
    retval->refcount = 1;

    if (vshader != NULL)
    {
        if (const_count < vshader->parseData->constant_count)
            const_count = vshader->parseData->constant_count;
        retval->attributes = (AttributeMap *) Malloc(sizeof (AttributeMap) *
                                        vshader->parseData->attribute_count);
        if (retval->attributes == NULL)
            goto link_program_fail;

        lookup_attributes(retval);
        lookup_uniforms(retval, vshader);
        lookup_samplers(retval, vshader);
        vshader->refcount++;
    } // if

    if (pshader != NULL)
    {
        if (const_count < pshader->parseData->constant_count)
            const_count = pshader->parseData->constant_count;

        lookup_uniforms(retval, pshader);
        lookup_samplers(retval, vshader);
        pshader->refcount++;
    } // if

    if (const_count > 0)    
    {
        retval->constants = (GLfloat *) Malloc(sizeof (GLfloat) * const_count);
        if (retval->constants == NULL)
            goto link_program_fail;
        retval->constant_count = const_count;
    } // if

    return retval;

link_program_fail:
    if (retval != NULL)
    {
        Free(retval->constants);
        Free(retval->samplers);
        Free(retval->uniforms);
        Free(retval->attributes);
        Free(retval);
    } // if

    ctx->glDeleteObject(program);
    return NULL;
} // MOJOSHADER_glLinkProgram


void MOJOSHADER_glBindProgram(MOJOSHADER_glProgram *program)
{
    GLhandleARB handle = 0;
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

    ctx->glUseProgramObject(handle);
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


void MOJOSHADER_glSetSampler(unsigned int idx, unsigned int unit)
{
    const uint maxregs = STATICARRAYLEN(ctx->sampler_reg_file);
    if (idx < maxregs)
        ctx->sampler_reg_file[idx] = (GLuint) unit;
} // MOJOSHADER_glSetSampler


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

    ctx->glVertexAttribPointer(gl_index, size, gl_type, norm, stride, ptr);
    ctx->glEnableVertexAttribArray(gl_index);
} // MOJOSHADER_glSetVertexAttribute


void MOJOSHADER_glProgramReady(void)
{
    int i;
    int count;

    if (ctx->bound_program == NULL)
        return;  // nothing to do.

    // !!! FIXME: don't push Uniforms/Samplers if they haven't changed.

    // push Uniforms to the program from our register files...
    count = ctx->bound_program->uniform_count;
    for (i = 0; i < count; i++)
    {
        const UniformMap *map = &ctx->bound_program->uniforms[i];
        const MOJOSHADER_uniform *u = map->uniform;
        const MOJOSHADER_uniformType type = u->type;
        const MOJOSHADER_shaderType shader_type = map->shader_type;
        const int index = u->index;
        const int size = u->array_count;
        const GLint location = map->location;

        // only use arrays for 'c' registers.
        assert((size == 0) || (type == MOJOSHADER_UNIFORM_FLOAT));

        if (size != 0)  // !!! FIXME: this code sucks.
        {
            // !!! FIXME: calculate this all at link time.
            if (shader_type == MOJOSHADER_TYPE_VERTEX)
            {
                const MOJOSHADER_constant *c = ctx->bound_program->vertex->parseData->constants;
                int hi = ctx->bound_program->vertex->parseData->constant_count;

                int j;
                GLfloat *ptr = ctx->bound_program->constants;

                for (j = 0; j < hi; j++)
                {
                    if (c[j].type != MOJOSHADER_UNIFORM_FLOAT)
                        continue;

                    const int idx = c[j].index;
                    if ( (idx >= index) && (idx < (index + size)) )
                    {
                        memcpy(ptr, &ctx->vs_reg_file_f[idx * 4], 16);  // !!! FIXME: 16
                        memcpy(&ctx->vs_reg_file_f[idx * 4], &c->value.f, 16);  // !!! FIXME: 16
                        ptr += 4;
                    } // if
                } // for

                ctx->glUniform4fv(location, size, &ctx->vs_reg_file_f[index * 4]);

                ptr = ctx->bound_program->constants;
                for (j = 0; j < hi; j++)
                {
                    if (c[j].type != MOJOSHADER_UNIFORM_FLOAT)
                        continue;

                    const int idx = c[j].index;
                    if ( (idx >= index) && (idx < (index + size)) )
                    {
                        memcpy(&ctx->vs_reg_file_f[idx * 4], ptr, 16);  // !!! FIXME: 16
                        ptr += 4;
                    } // if
                } // for
            } // if

            else if (shader_type == MOJOSHADER_TYPE_PIXEL)
            {
                const MOJOSHADER_constant *c = ctx->bound_program->fragment->parseData->constants;
                int hi = ctx->bound_program->fragment->parseData->constant_count;

                int j;
                GLfloat *ptr = ctx->bound_program->constants;

                for (j = 0; j < hi; j++)
                {
                    if (c[j].type != MOJOSHADER_UNIFORM_FLOAT)
                        continue;

                    const int idx = c[j].index;
                    if ( (idx >= index) && (idx < (index + size)) )
                    {
                        memcpy(ptr, &ctx->ps_reg_file_f[idx * 4], 16);  // !!! FIXME: 16
                        memcpy(&ctx->ps_reg_file_f[idx * 4], &c->value.f, 16);  // !!! FIXME: 16
                        ptr += 4;
                    } // if
                } // for

                ctx->glUniform4fv(location, size, &ctx->ps_reg_file_f[index * 4]);

                ptr = ctx->bound_program->constants;
                for (j = 0; j < hi; j++)
                {
                    if (c[j].type != MOJOSHADER_UNIFORM_FLOAT)
                        continue;

                    const int idx = c[j].index;
                    if ( (idx >= index) && (idx < (index + size)) )
                    {
                        memcpy(&ctx->ps_reg_file_f[idx * 4], ptr, 16);  // !!! FIXME: 16
                        ptr += 4;
                    } // if
                } // for
            } // else if
        } // if

        else if (shader_type == MOJOSHADER_TYPE_VERTEX)
        {
            if (type == MOJOSHADER_UNIFORM_FLOAT)
                ctx->glUniform4fv(location, 1, &ctx->vs_reg_file_f[index * 4]);
            else if (type == MOJOSHADER_UNIFORM_INT)
                ctx->glUniform4iv(location, 1, &ctx->vs_reg_file_i[index * 4]);
            else if (type == MOJOSHADER_UNIFORM_BOOL)
                ctx->glUniform1i(location, ctx->vs_reg_file_b[index]);
        } // if

        else if (shader_type == MOJOSHADER_TYPE_PIXEL)
        {
            if (type == MOJOSHADER_UNIFORM_FLOAT)
                ctx->glUniform4fv(location, 1, &ctx->ps_reg_file_f[index * 4]);
            else if (type == MOJOSHADER_UNIFORM_INT)
                ctx->glUniform4iv(location, 1, &ctx->ps_reg_file_i[index * 4]);
            else if (type == MOJOSHADER_UNIFORM_BOOL)
                ctx->glUniform1i(location, ctx->ps_reg_file_b[index]);
        } // else if
    } // for

    // push Samplers to the program from our register files...
    count = ctx->bound_program->sampler_count;
    for (i = 0; i < count; i++)
    {
        const SamplerMap *map = &ctx->bound_program->samplers[i];
        const MOJOSHADER_sampler *s = map->sampler;
        ctx->glUniform1i(map->location, ctx->sampler_reg_file[s->index]);
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

