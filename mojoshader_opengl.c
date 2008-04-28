#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <assert.h>

#include "mojoshader.h"
#define GL_GLEXT_LEGACY 1
#include "gl.h"
#include "glext.h"

// Get basic wankery out of the way here...

#define STATICARRAYLEN(x) ( (sizeof ((x))) / (sizeof ((x)[0])) )

typedef unsigned int uint;  // this is a printf() helper. don't use for code.
typedef uint8_t uint8;
typedef uint32_t uint32;
typedef int32_t int32;

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
    const MOJOSHADER_attribute *attribute;
    GLuint location;
} AttributeMap;

struct MOJOSHADER_glProgram
{
    MOJOSHADER_glShader *vertex;
    MOJOSHADER_glShader *fragment;
    GLhandleARB handle;
    uint32 uniform_count;
    UniformMap *uniforms;
    uint32 attribute_count;
    AttributeMap *attributes;
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
static int opengl_major = 0;
static int opengl_minor = 0;
static MOJOSHADER_glProgram *bound_program = NULL;
static const char *profile = NULL;

// Extensions...
static int have_base_opengl = 0;
static int have_GL_ARB_shader_objects = 0;
static int have_GL_ARB_vertex_shader = 0;
static int have_GL_ARB_fragment_shader = 0;
static int have_GL_ARB_shading_language_100 = 0;
static int have_GL_NV_half_float = 0;

// Entry points...
typedef WINGDIAPI const GLubyte * (APIENTRYP PFNGLGETSTRINGPROC) (GLenum name);
typedef WINGDIAPI void (APIENTRYP PFNGLDISABLECLIENTSTATEPROC) (GLenum array);
typedef WINGDIAPI void (APIENTRYP PFNGLENABLECLIENTSTATEPROC) (GLenum array);
typedef WINGDIAPI void (APIENTRYP PFNGLVERTEXPOINTERPROC) (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);

static PFNGLGETSTRINGPROC pglGetString = NULL;
static PFNGLDELETEOBJECTARBPROC pglDeleteObject = NULL;
static PFNGLATTACHOBJECTARBPROC pglAttachObject = NULL;
static PFNGLCOMPILESHADERARBPROC pglCompileShader = NULL;
static PFNGLCREATEPROGRAMOBJECTARBPROC pglCreateProgramObject = NULL;
static PFNGLCREATESHADEROBJECTARBPROC pglCreateShaderObject = NULL;
static PFNGLDISABLECLIENTSTATEPROC pglDisableClientState = NULL;
static PFNGLDISABLEVERTEXATTRIBARRAYARBPROC pglDisableVertexAttribArray = NULL;
static PFNGLENABLECLIENTSTATEPROC pglEnableClientState = NULL;
static PFNGLENABLEVERTEXATTRIBARRAYARBPROC pglEnableVertexAttribArray = NULL;
static PFNGLGETATTRIBLOCATIONARBPROC pglGetAttribLocation = NULL;
static PFNGLGETINFOLOGARBPROC pglGetInfoLog = NULL;
static PFNGLGETOBJECTPARAMETERIVARBPROC pglGetObjectParameteriv = NULL;
static PFNGLGETUNIFORMLOCATIONARBPROC pglGetUniformLocation = NULL;
static PFNGLLINKPROGRAMARBPROC pglLinkProgram = NULL;
static PFNGLSHADERSOURCEARBPROC pglShaderSource = NULL;
static PFNGLUNIFORM1IARBPROC pglUniform1i = NULL;
static PFNGLUNIFORM4FVARBPROC pglUniform4fv = NULL;
static PFNGLUNIFORM4IVARBPROC pglUniform4iv = NULL;
static PFNGLUSEPROGRAMOBJECTARBPROC pglUseProgramObject = NULL;
static PFNGLVERTEXATTRIBPOINTERARBPROC pglVertexAttribPointer = NULL;
static PFNGLVERTEXPOINTERPROC pglVertexPointer = NULL;


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
    void *retval = malloc_fn(len, malloc_data);
    if (retval == NULL)
        set_error("out of memory");
    return retval;
} // Malloc

static inline void Free(void *ptr)
{
    if (ptr != NULL)
        free_fn(ptr, malloc_data);
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
    #define DO_LOOKUP(ext, typ, fn) p##fn = (typ) loadsym(lookup, #fn, &have_##ext)
    DO_LOOKUP(base_opengl, PFNGLGETSTRINGPROC, glGetString);
    DO_LOOKUP(base_opengl, PFNGLDISABLECLIENTSTATEPROC, glDisableClientState);
    DO_LOOKUP(base_opengl, PFNGLENABLECLIENTSTATEPROC, glEnableClientState);
    DO_LOOKUP(base_opengl, PFNGLVERTEXPOINTERPROC, glVertexPointer);
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

    // See if it's in the spec for this GL implementation's version.
    if (major >= 0)
    {
        if ( ((opengl_major << 16) | (opengl_minor & 0xFFFF)) >=
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
        opengl_major = opengl_minor = 0;
    else
        sscanf(verstr, "%d.%d", &opengl_major, &opengl_minor);
} // parse_opengl_version


static int check_extensions(void *(*lookup)(const char *fnname))
{
    have_base_opengl = 1;
    have_GL_ARB_shader_objects = 1;
    have_GL_ARB_vertex_shader = 1;
    have_GL_ARB_fragment_shader = 1;
    have_GL_ARB_shading_language_100 = 1;
    have_GL_NV_half_float = 1;

    lookup_entry_points(lookup);

    if (!have_base_opengl)   // a function we should definitely have is MIA?
    {
        set_error("missing basic OpenGL entry points");
        return 0;
    } // if

    parse_opengl_version((const char *) pglGetString(GL_VERSION));

    const char *extlist = (const char *) pglGetString(GL_EXTENSIONS);
    if (extlist == NULL)
        extlist = "";  // just in case.

    #define VERIFY_EXT(ext, major, minor) \
        have_##ext = verify_extension(#ext, have_##ext, extlist, major, minor)

    VERIFY_EXT(GL_ARB_shader_objects, 2, 0);
    VERIFY_EXT(GL_ARB_vertex_shader, 2, 0);
    VERIFY_EXT(GL_ARB_fragment_shader, 2, 0);
    VERIFY_EXT(GL_ARB_shading_language_100, 2, 0);
    VERIFY_EXT(GL_NV_half_float, -1, -1);

    #undef VERIFY_EXT

    #define REQUIRE_GL_EXTENSION(x) \
        if (!have_##x) { set_error("This profile requires " #x); return 0; }

    if (strcmp(profile, MOJOSHADER_PROFILE_GLSL) == 0)
    {
        REQUIRE_GL_EXTENSION(GL_ARB_shader_objects);
        REQUIRE_GL_EXTENSION(GL_ARB_vertex_shader);
        REQUIRE_GL_EXTENSION(GL_ARB_fragment_shader);
        REQUIRE_GL_EXTENSION(GL_ARB_shading_language_100);
    } // if

    else
    {
        set_error("unknown profile");
        return 0;
    } // else

    #undef REQUIRE_GL_EXTENSION

    return 1;
} // check_extensions


int MOJOSHADER_glInit(const char *_profile,
                      void *(*lookup)(const char *fnname),
                      MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    error_buffer[0] = '\0';

    if (strcmp(_profile, MOJOSHADER_PROFILE_GLSL) == 0)
        profile = MOJOSHADER_PROFILE_GLSL;
    else
    {
        set_error("unknown profile");
        goto init_fail;
    } // else

    if (!check_extensions(lookup))
        goto init_fail;

    malloc_fn = (m == NULL) ? internal_malloc : m;
    free_fn = (f == NULL) ? internal_free : f;
    malloc_data = d;

    memset(vs_register_file_f, '\0', sizeof (vs_register_file_f));
    memset(vs_register_file_i, '\0', sizeof (vs_register_file_i));
    memset(vs_register_file_b, '\0', sizeof (vs_register_file_b));
    memset(ps_register_file_f, '\0', sizeof (ps_register_file_f));
    memset(ps_register_file_i, '\0', sizeof (ps_register_file_i));
    memset(ps_register_file_b, '\0', sizeof (ps_register_file_b));

    MOJOSHADER_glBindProgram(NULL);

    return 1;

init_fail:
    opengl_major = 0;
    opengl_minor = 0;
    profile = NULL;
    malloc_fn = NULL;
    free_fn = NULL;
    malloc_data = NULL;
    lookup_entry_points(NULL);
    return 0;
} // MOJOSHADER_glInit


MOJOSHADER_glShader *MOJOSHADER_glCompileShader(const unsigned char *tokenbuf,
                                                const unsigned int bufsize)
{
    MOJOSHADER_glShader *retval = NULL;
    GLhandleARB shader = 0;
    const MOJOSHADER_parseData *pd = MOJOSHADER_parse(profile, tokenbuf,
                                                      bufsize, malloc_fn,
                                                      free_fn, malloc_data);
    if (pd->error != NULL)
    {
        set_error(pd->error);
        goto compile_shader_fail;
    } // if

    retval = (MOJOSHADER_glShader *) Malloc(sizeof (MOJOSHADER_glShader));
    if (retval == NULL)
        goto compile_shader_fail;

    GLint ok = 0;
    const GLenum shader_type = (pd->shader_type == MOJOSHADER_TYPE_PIXEL) ? GL_FRAGMENT_SHADER : GL_VERTEX_SHADER;
    GLint shaderlen = (GLint) pd->output_len;
    shader = pglCreateShaderObject(shader_type);

    pglShaderSource(shader, 1, (const GLchar **) &pd->output, &shaderlen);
    pglCompileShader(shader);
    pglGetObjectParameteriv(shader, GL_OBJECT_COMPILE_STATUS_ARB, &ok);

    if (!ok)
    {
        GLsizei len = 0;
        pglGetInfoLog(shader, sizeof (error_buffer), &len, (GLchar *) error_buffer);
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
        pglDeleteObject(shader);
    return NULL;
} // MOJOSHADER_glCompileShader


static void shader_unref(MOJOSHADER_glShader *shader)
{
    if (shader != NULL)
    {
        const uint32 refcount = shader->refcount;
        if (refcount > 1)
            shader->refcount--;
        else
        {
            pglDeleteObject(shader->handle);
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
            pglDeleteObject(program->handle);
            shader_unref(program->vertex);
            shader_unref(program->fragment);
            Free(program->attributes);
            Free(program->uniforms);
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
        const GLint loc = pglGetUniformLocation(program->handle, u[i].name);
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


static void lookup_attributes(MOJOSHADER_glProgram *program)
{
    int i;
    const MOJOSHADER_parseData *pd = program->vertex->parseData;
    const MOJOSHADER_attribute *a = pd->attributes;

    for (i = 0; i < pd->attribute_count; i++)
    {
        const GLint loc = pglGetAttribLocation(program->handle, a->name);
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
    const GLhandleARB program = pglCreateProgramObject();

    if (vshader != NULL) pglAttachObject(program, vshader->handle);
    if (pshader != NULL) pglAttachObject(program, pshader->handle);

    pglLinkProgram(program);

    GLint ok = 0;
    pglGetObjectParameteriv(program, GL_OBJECT_LINK_STATUS_ARB, &ok);
    if (!ok)
    {
        GLsizei len = 0;
        pglGetInfoLog(program, sizeof (error_buffer), &len, (GLchar *) error_buffer);
        goto link_program_fail;
    } // if

    retval = (MOJOSHADER_glProgram *) Malloc(sizeof (MOJOSHADER_glProgram));
    if (retval == NULL)
        goto link_program_fail;
    memset(retval, '\0', sizeof (MOJOSHADER_glProgram));

    int numregs = 0;
    if (vshader != NULL) numregs += vshader->parseData->uniform_count;
    if (pshader != NULL) numregs += pshader->parseData->uniform_count;
    retval->uniforms = (UniformMap *) Malloc(sizeof (UniformMap) * numregs);
    if (retval->uniforms == NULL)
        goto link_program_fail;
    memset(retval->uniforms, '\0', sizeof (UniformMap) * numregs);

    retval->handle = program;
    retval->vertex = vshader;
    retval->fragment = pshader;
    retval->refcount = 1;

    if (vshader != NULL)
    {
        retval->attributes = (AttributeMap *) Malloc(sizeof (AttributeMap) *
                                        vshader->parseData->attribute_count);
        if (retval->attributes == NULL)
            goto link_program_fail;

        lookup_attributes(retval);
        lookup_uniforms(retval, vshader);
        vshader->refcount++;
    } // if

    if (pshader != NULL)
    {
        lookup_uniforms(retval, pshader);
        pshader->refcount++;
    } // if

    return retval;

link_program_fail:
    if (retval != NULL)
    {
        Free(retval->uniforms);
        Free(retval->attributes);
        Free(retval);
    } // if

    pglDeleteObject(program);
    return NULL;
} // MOJOSHADER_glLinkProgram


void MOJOSHADER_glBindProgram(MOJOSHADER_glProgram *program)
{
    GLhandleARB handle = 0;
    int i;

    if (program == bound_program)
        return;  // nothing to do.

    // Disable any client-side arrays the current program could have used.
    if (bound_program != NULL)
    {
        pglDisableClientState(GL_VERTEX_ARRAY);
        for (i = 0; i < bound_program->attribute_count; i++)
        {
            const AttributeMap *map = &bound_program->attributes[i];
            pglDisableVertexAttribArray(map->location);
        } // if
    } // for

    if (program != NULL)
    {
        handle = program->handle;
        program->refcount++;
    } // if

    pglUseProgramObject(handle);
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
        const uint cpy = (maxuint(maxregs - idx, vec4n) * sizeof (*data)) * 4;
        memcpy(vs_register_file_f + (idx * 4), data, cpy);
    } // if
} // MOJOSHADER_glSetVertexShaderUniformF


void MOJOSHADER_glSetVertexShaderUniformI(unsigned int idx, const int *data,
                                          unsigned int ivec4n)
{
    const uint maxregs = STATICARRAYLEN(vs_register_file_i) / 4;
    if (idx < maxregs)
    {
        const uint cpy = (maxuint(maxregs - idx, ivec4n) * sizeof (*data)) * 4;
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
        const uint cpy = (maxuint(maxregs - idx, vec4n) * sizeof (*data)) * 4;
        memcpy(ps_register_file_f + (idx * 4), data, cpy);
    } // if
} // MOJOSHADER_glSetPixelShaderUniformF


void MOJOSHADER_glSetPixelShaderUniformI(unsigned int idx, const int *data,
                                         unsigned int ivec4n)
{
    const uint maxregs = STATICARRAYLEN(ps_register_file_i) / 4;
    if (idx < maxregs)
    {
        const uint cpy = (maxuint(maxregs - idx, ivec4n) * sizeof (*data)) * 4;
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


static inline GLenum opengl_posattr_type(const MOJOSHADER_attributeType type)
{
    // we don't ever use the glVertexPointer() stream, so we just need to
    //  make the data types reasonably match, so the GL doesn't overrun
    //  the buffer when dereferencing it.

    switch (type)
    {
        case MOJOSHADER_ATTRIBUTE_BYTE: return GL_NONE;  // oh well.
        case MOJOSHADER_ATTRIBUTE_UBYTE: return GL_NONE;  // oh well.
        case MOJOSHADER_ATTRIBUTE_SHORT: return GL_SHORT;
        case MOJOSHADER_ATTRIBUTE_USHORT: return GL_SHORT;
        case MOJOSHADER_ATTRIBUTE_INT: return GL_INT;
        case MOJOSHADER_ATTRIBUTE_UINT: return GL_INT;
        case MOJOSHADER_ATTRIBUTE_FLOAT: return GL_FLOAT;
        case MOJOSHADER_ATTRIBUTE_DOUBLE: return GL_DOUBLE;
        case MOJOSHADER_ATTRIBUTE_HALF_FLOAT: return GL_SHORT;
    } // switch

    return GL_NONE;  // oh well. Raises a GL error later.
} // opengl_posattr_type


static inline GLenum opengl_attr_type(const MOJOSHADER_attributeType type)
{
    switch (type)
    {
        case MOJOSHADER_ATTRIBUTE_BYTE: return GL_BYTE;
        case MOJOSHADER_ATTRIBUTE_UBYTE: return GL_UNSIGNED_BYTE;
        case MOJOSHADER_ATTRIBUTE_SHORT: return GL_SHORT;
        case MOJOSHADER_ATTRIBUTE_USHORT: return GL_UNSIGNED_SHORT;
        case MOJOSHADER_ATTRIBUTE_INT: return GL_INT;
        case MOJOSHADER_ATTRIBUTE_UINT: return GL_UNSIGNED_INT;
        case MOJOSHADER_ATTRIBUTE_FLOAT: return GL_FLOAT;
        case MOJOSHADER_ATTRIBUTE_DOUBLE: return GL_DOUBLE;

        case MOJOSHADER_ATTRIBUTE_HALF_FLOAT:
            if (have_GL_NV_half_float)
                return GL_HALF_FLOAT_NV;
            break;
    } // switch

    return GL_NONE;  // oh well. Raises a GL error later.
} // opengl_attr_type


void MOJOSHADER_glSetVertexAttribute(MOJOSHADER_usage usage,
                                     int index, unsigned int size,
                                     MOJOSHADER_attributeType type,
                                     int normalized, unsigned int stride,
                                     const void *ptr)
{
    if ((bound_program == NULL) || (bound_program->vertex == NULL))
        return;

    // Since glVertexPointer() lacks the flexibility that we can get from
    //  glVertexAttribPointer(), we set POSITION0 to a generic vertex
    //  attribute, and the shaders we generate know to look there instead of
    //  GLSL's gl_Position global variable. But to keep the GL handy, we
    //  also set the best possible equivalent with glVertexPointer(). Messy.

    if ((usage == MOJOSHADER_USAGE_POSITION) && (index == 0))
    {
        // !!! FIXME: fails if size==1.
        pglVertexPointer(size, opengl_posattr_type(type), stride, ptr);
        pglEnableClientState(GL_VERTEX_ARRAY);
    } // if

    int i;
    GLuint gl_index = 0;
    for (i = 0; i < bound_program->attribute_count; i++)
    {
        const AttributeMap *map = &bound_program->attributes[i];
        const MOJOSHADER_attribute *a = map->attribute;

        // !!! FIXME: is this array guaranteed to be sorted by usage?
        // !!! FIXME:  if so, we can break out of the loop if a->usage > usage.

        if ((a->usage == usage) && (a->index == index))
        {
            gl_index = map->location;
            break;
        } // if
    } // for

    if (gl_index != 0)
    {
        const GLenum gl_type = opengl_attr_type(type);
        const GLboolean norm = (normalized) ? GL_TRUE : GL_FALSE;
        pglVertexAttribPointer(gl_index, size, gl_type, norm, stride, ptr);
        pglEnableVertexAttribArray(gl_index);
    } // if
} // MOJOSHADER_glSetVertexAttribute


void MOJOSHADER_glProgramReady(void)
{
    int i;

    if (bound_program == NULL)
        return;  // nothing to do.

    // !!! FIXME: don't push Uniforms if we know they haven't changed.

    // push Uniforms to the program from our register files...
    for (i = 0; i < bound_program->uniform_count; i++)
    {
        const UniformMap *map = &bound_program->uniforms[i];
        const MOJOSHADER_uniform *u = map->uniform;
        const MOJOSHADER_uniformType type = u->type;
        const MOJOSHADER_shaderType shader_type = map->shader_type;
        const int index = u->index;
        const GLint location = map->location;

        if (shader_type == MOJOSHADER_TYPE_VERTEX)
        {
            if (type == MOJOSHADER_UNIFORM_FLOAT)
                pglUniform4fv(location, 1, &vs_register_file_f[index * 4]);
            else if (type == MOJOSHADER_UNIFORM_INT)
                pglUniform4iv(location, 1, &vs_register_file_i[index * 4]);
            else if (type == MOJOSHADER_UNIFORM_BOOL)
                pglUniform1i(location, vs_register_file_b[index]);
        } // if

        else if (shader_type == MOJOSHADER_TYPE_PIXEL)
        {
            if (type == MOJOSHADER_UNIFORM_FLOAT)
                pglUniform4fv(location, 1, &ps_register_file_f[index * 4]);
            else if (type == MOJOSHADER_UNIFORM_INT)
                pglUniform4iv(location, 1, &ps_register_file_i[index * 4]);
            else if (type == MOJOSHADER_UNIFORM_BOOL)
                pglUniform1i(location, ps_register_file_b[index]);
        } // else if
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


void MOJOSHADER_glDeinit(void)
{
    MOJOSHADER_glBindProgram(NULL);
    lookup_entry_points(NULL);
    profile = NULL;
    malloc_fn = NULL;
    free_fn = NULL;
    malloc_data = NULL;
    error_buffer[0] = '\0';
    opengl_major = 0;
    opengl_minor = 0;
} // MOJOSHADER_glDeinit

// end of mojoshader_opengl.c ...

