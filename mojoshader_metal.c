/**
 * MojoShader; generate shader programs from bytecode of compiled
 *  Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

#if (defined(__APPLE__) && defined(__MACH__))
#define PLATFORM_APPLE 1
#include "TargetConditionals.h"
#include <objc/message.h>
#define msg     ((void* (*)(void*, void*))objc_msgSend)
#define msg_s   ((void* (*)(void*, void*, const char*))objc_msgSend)
#define msg_p   ((void* (*)(void*, void*, void*))objc_msgSend)
#define msg_ip  ((void* (*)(void*, void*, int, void*))objc_msgSend)
#define msg_ppp ((void* (*)(void*, void*, void*, void*, void*))objc_msgSend)
#endif /* (defined(__APPLE__) && defined(__MACH__)) */

#define __MOJOSHADER_INTERNAL__ 1
#include "mojoshader_internal.h"

typedef struct MOJOSHADER_mtlUniformBuffer MOJOSHADER_mtlUniformBuffer;
typedef struct MOJOSHADER_mtlShader
{
    const MOJOSHADER_parseData *parseData;
    MOJOSHADER_mtlUniformBuffer *ubo;
    uint32 refcount;
    void *library; // MTLLibrary*
} MOJOSHADER_mtlShader;

// Error state...
static char error_buffer[1024] = { '\0' };

static void set_error(const char *str)
{
    snprintf(error_buffer, sizeof (error_buffer), "%s", str);
} // set_error

static inline void out_of_memory(void)
{
    set_error("out of memory");
} // out_of_memory

// profile-specific implementations...

#if SUPPORT_PROFILE_METAL && PLATFORM_APPLE
#ifdef MOJOSHADER_EFFECT_SUPPORT

/* Structs */

typedef struct MOJOSHADER_mtlUniformBuffer
{
    int bufferSize;
    void **internalBuffers; // MTLBuffer*
    int internalBufferSize;
    int internalOffset;
    int currentFrame;
    int inUse;
} MOJOSHADER_mtlUniformBuffer;

// Max entries for each register file type...
#define MAX_REG_FILE_F 8192
#define MAX_REG_FILE_I 2047
#define MAX_REG_FILE_B 2047

typedef struct MOJOSHADER_mtlContext
{
    // Allocators...
    MOJOSHADER_malloc malloc_fn;
    MOJOSHADER_free free_fn;
    void *malloc_data;

    // The constant register files...
    // !!! FIXME: Man, it kills me how much memory this takes...
    // !!! FIXME:  ... make this dynamically allocated on demand.
    float vs_reg_file_f[MAX_REG_FILE_F * 4];
    int vs_reg_file_i[MAX_REG_FILE_I * 4];
    uint8 vs_reg_file_b[MAX_REG_FILE_B];
    float ps_reg_file_f[MAX_REG_FILE_F * 4];
    int ps_reg_file_i[MAX_REG_FILE_I * 4];
    uint8 ps_reg_file_b[MAX_REG_FILE_B];

    // Pointer to the active MTLDevice.
    void* device;

    // The maximum number of frames in flight.
    int framesInFlight;

    // Array of UBOs that are being used in the current frame.
    MOJOSHADER_mtlUniformBuffer **buffersInUse;

    // The current capacity of the uniform buffer array.
    int bufferArrayCapacity;

    // The actual number of UBOs used in the current frame.
    int numBuffersInUse;

    // The currently bound shaders.
    MOJOSHADER_mtlShader *vertexShader;
    MOJOSHADER_mtlShader *pixelShader;

    // Objective-C Selectors
    void* classNSString;
    void* selAlloc;
    void* selInitWithUTF8String;
    void* selUTF8String;
    void* selLength;
    void* selContents;
    void* selNewBufferWithLength;
    void* selRelease;
    void* selNewLibraryWithSource;
    void* selLocalizedDescription;
    void* selNewFunctionWithName;
    void* selRetain;
} MOJOSHADER_mtlContext;

static MOJOSHADER_mtlContext *ctx = NULL;

/* Uniform buffer utilities */

static inline int next_highest_alignment(int n)
{
    #if TARGET_OS_IOS || TARGET_OS_TV || TARGET_OS_SIMULATOR
    int align = 16;
    #else
    int align = 256;
    #endif

    return align * ((n + align - 1) / align);
} // next_highest_alignment

static void* create_ubo_backing_buffer(MOJOSHADER_mtlUniformBuffer *ubo,
                                                               int frame)
{
    void *oldBuffer = ubo->internalBuffers[frame];
    void *newBuffer = msg_ip(
        ctx->device,
        ctx->selNewBufferWithLength,
        ubo->internalBufferSize,
        NULL
    );
    if (oldBuffer != NULL)
    {
        // Copy over data from old buffer
        memcpy(
            msg(newBuffer, ctx->selContents),
            msg(oldBuffer, ctx->selContents),
            (int) msg(oldBuffer, ctx->selLength)
        );

        // Free the old buffer
        msg(oldBuffer, ctx->selRelease);
    } //if

    return newBuffer;
} // create_ubo_backing_buffer

static void predraw_ubo(MOJOSHADER_mtlUniformBuffer *ubo)
{
    if (!ubo->inUse)
    {
        ubo->inUse = 1;
        ctx->buffersInUse[ctx->numBuffersInUse++] = ubo;

        // Double the array size if we run out of room
        if (ctx->numBuffersInUse >= ctx->bufferArrayCapacity)
        {
            int oldlen = ctx->bufferArrayCapacity;
            ctx->bufferArrayCapacity *= 2;
            MOJOSHADER_mtlUniformBuffer **tmp;
            tmp = (MOJOSHADER_mtlUniformBuffer**) ctx->malloc_fn(
                ctx->bufferArrayCapacity * sizeof(MOJOSHADER_mtlUniformBuffer *),
                ctx->malloc_data
            );
            memcpy(tmp, ctx->buffersInUse, oldlen * sizeof(MOJOSHADER_mtlUniformBuffer *));
            ctx->free_fn(ctx->buffersInUse, ctx->malloc_data);
            ctx->buffersInUse = tmp;
        }
        return;
    } // if

    ubo->internalOffset += ubo->bufferSize;

    int buflen = (int) msg(
        ubo->internalBuffers[ubo->currentFrame],
        ctx->selLength
    );
    if (ubo->internalOffset >= buflen)
    {
        // Double capacity when we're out of room
        if (ubo->internalOffset >= ubo->internalBufferSize)
            ubo->internalBufferSize *= 2;

        ubo->internalBuffers[ubo->currentFrame] =
            create_ubo_backing_buffer(ubo, ubo->currentFrame);
    } //if
} // predraw_ubo

static MOJOSHADER_mtlUniformBuffer* create_ubo(MOJOSHADER_mtlShader *shader,
                                               MOJOSHADER_malloc m, void* d)
{
    int uniformCount = shader->parseData->uniform_count;
    if (uniformCount == 0)
        return NULL;

    // Calculate how big we need to make the buffer
    int buflen = 0;
    for (int i = 0; i < uniformCount; i += 1)
    {
        int arrayCount = shader->parseData->uniforms[i].array_count;
        int uniformSize = 16;
        if (shader->parseData->uniforms[i].type == MOJOSHADER_UNIFORM_BOOL)
            uniformSize = 1;
        buflen += (arrayCount ? arrayCount : 1) * uniformSize;
    } // for

    // Allocate the UBO
    MOJOSHADER_mtlUniformBuffer *retval;
    retval = (MOJOSHADER_mtlUniformBuffer *) m(sizeof(MOJOSHADER_mtlUniformBuffer), d);
    retval->bufferSize = next_highest_alignment(buflen);
    retval->internalBufferSize = retval->bufferSize * 16; // pre-allocate some extra room!
    retval->internalBuffers = m(ctx->framesInFlight * sizeof(void*), d);
    retval->internalOffset = 0;
    retval->inUse = 0;
    retval->currentFrame = 0;

    // Create the backing buffers
    for (int i = 0; i < ctx->framesInFlight; i++)
    {
        retval->internalBuffers[i] = NULL; // basically a memset('\0')
        retval->internalBuffers[i] = create_ubo_backing_buffer(retval, i);
    } // for

    return retval;
} // create_ubo

static void dealloc_ubo(MOJOSHADER_mtlShader *shader,
                        MOJOSHADER_free f,
                        void* d)
{
    if (shader->ubo == NULL)
        return;

    for (int i = 0; i < ctx->framesInFlight; i++)
    {
        msg(shader->ubo->internalBuffers[i], ctx->selRelease);
        shader->ubo->internalBuffers[i] = NULL;
    } // for

    f(shader->ubo->internalBuffers, d);
    f(shader->ubo, d);
} // dealloc_ubo

static void *get_uniform_buffer(MOJOSHADER_mtlShader *shader)
{
    if (shader == NULL || shader->ubo == NULL)
        return NULL;

    return shader->ubo->internalBuffers[shader->ubo->currentFrame];
} // get_uniform_buffer

static int get_uniform_offset(MOJOSHADER_mtlShader *shader)
{
    if (shader == NULL || shader->ubo == NULL)
        return 0;

    return shader->ubo->internalOffset;
} // get_uniform_offset

static void update_uniform_buffer(MOJOSHADER_mtlShader *shader)
{
    if (shader == NULL || shader->ubo == NULL)
        return;

    float *regF; int *regI; uint8 *regB;
    if (shader->parseData->shader_type == MOJOSHADER_TYPE_VERTEX)
    {
        regF = ctx->vs_reg_file_f;
        regI = ctx->vs_reg_file_i;
        regB = ctx->vs_reg_file_b;
    } // if
    else
    {
        regF = ctx->ps_reg_file_f;
        regI = ctx->ps_reg_file_i;
        regB = ctx->ps_reg_file_b;
    } // else

    predraw_ubo(shader->ubo);
    void *buf = shader->ubo->internalBuffers[shader->ubo->currentFrame];
    void *contents = msg(buf, ctx->selContents) + shader->ubo->internalOffset;

    int offset = 0;
    for (int i = 0; i < shader->parseData->uniform_count; i++)
    {
        int idx = shader->parseData->uniforms[i].index;
        int arrayCount = shader->parseData->uniforms[i].array_count;
        int size = arrayCount ? arrayCount : 1;

        switch (shader->parseData->uniforms[i].type)
        {
            case MOJOSHADER_UNIFORM_FLOAT:
                memcpy(
                    contents + (offset * 16),
                    &regF[4 * idx],
                    size * 16
                );
                break;

            case MOJOSHADER_UNIFORM_INT:
                // !!! FIXME: Need a test case
                memcpy(
                    contents + (offset * 16),
                    &regI[4 * idx],
                    size * 16
                );
                break;

            case MOJOSHADER_UNIFORM_BOOL:
                // !!! FIXME: Need a test case
                memcpy(
                    contents + offset,
                    &regB[idx],
                    size
                );
                break;

            default:
                assert(0); // This should never happen.
                break;
        } // switch

        offset += size;
    } // for
} // update_uniform_buffer

/* Public API */

int MOJOSHADER_mtlCreateContext(void* mtlDevice, int framesInFlight,
                                MOJOSHADER_malloc m, MOJOSHADER_free f,
                                void *malloc_d)
{
    assert(ctx == NULL);

    if (m == NULL) m = MOJOSHADER_internal_malloc;
    if (f == NULL) f = MOJOSHADER_internal_free;

    ctx = (MOJOSHADER_mtlContext *) m(sizeof(MOJOSHADER_mtlContext), malloc_d);
    if (ctx == NULL)
    {
        out_of_memory();
        goto init_fail;
    } // if

    memset(ctx, '\0', sizeof (MOJOSHADER_mtlContext));
    ctx->malloc_fn = m;
    ctx->free_fn = f;
    ctx->malloc_data = malloc_d;

    // Initialize the Metal state
    ctx->device = mtlDevice;
    ctx->framesInFlight = framesInFlight;

    // Allocate the uniform buffer object array
    ctx->bufferArrayCapacity = 32; // arbitrary!
    ctx->buffersInUse = ctx->malloc_fn(
        ctx->bufferArrayCapacity * sizeof(MOJOSHADER_mtlUniformBuffer *),
        ctx->malloc_data
    );

    // Grab references to Objective-C selectors
    ctx->classNSString = objc_getClass("NSString");
    ctx->selAlloc = sel_registerName("alloc");
    ctx->selInitWithUTF8String = sel_registerName("initWithUTF8String:");
    ctx->selUTF8String = sel_registerName("UTF8String");
    ctx->selLength = sel_registerName("length");
    ctx->selContents = sel_registerName("contents");
    ctx->selNewBufferWithLength = sel_registerName("newBufferWithLength:options:");
    ctx->selRelease = sel_registerName("release");
    ctx->selNewLibraryWithSource = sel_registerName("newLibraryWithSource:options:error:");
    ctx->selLocalizedDescription = sel_registerName("localizedDescription");
    ctx->selNewFunctionWithName = sel_registerName("newFunctionWithName:");
    ctx->selRetain = sel_registerName("retain");

    return 0;

init_fail:
    if (ctx != NULL)
        f(ctx, malloc_d);
    return -1;
} // MOJOSHADER_mtlCreateContext

void MOJOSHADER_mtlDestroyContext(void)
{
    ctx->free_fn(ctx->buffersInUse, ctx->malloc_data);
    ctx->free_fn(ctx, ctx->malloc_data);
} // MOJOSHADER_mtlDestroyContext

void *MOJOSHADER_mtlCompileLibrary(MOJOSHADER_effect *effect)
{
    MOJOSHADER_malloc m = ctx->malloc_fn;
    MOJOSHADER_free f = ctx->free_fn;
    void *d = ctx->malloc_data;

    int i, src_len, src_pos, output_len;
    char *shader_source, *ptr;
    const char *repl;
    MOJOSHADER_effectObject *object;
    MOJOSHADER_mtlShader *shader;
    void *retval, *compileError, *shader_source_ns;

    // Count the number of shaders before allocating
    src_len = 0;
    for (i = 0; i < effect->object_count; i++)
    {
        object = &effect->objects[i];
        if (object->type == MOJOSHADER_SYMTYPE_PIXELSHADER
         || object->type == MOJOSHADER_SYMTYPE_VERTEXSHADER)
        {
            if (!object->shader.is_preshader)
            {
                shader = (MOJOSHADER_mtlShader*) object->shader.shader;
                src_len += shader->parseData->output_len;
            } // if
        } // if
    } // for

    // Allocate shader source buffer
    shader_source = (char *) m(src_len + 1, d);
    memset(shader_source, '\0', src_len + 1);
    src_pos = 0;

    // Copy all the source text into the buffer
    for (i = 0; i < effect->object_count; i++)
    {
        object = &effect->objects[i];
        if (object->type == MOJOSHADER_SYMTYPE_PIXELSHADER
         || object->type == MOJOSHADER_SYMTYPE_VERTEXSHADER)
        {
            if (!object->shader.is_preshader)
            {
                shader = (MOJOSHADER_mtlShader*) object->shader.shader;
                memcpy(&shader_source[src_pos], shader->parseData->output,
                                                shader->parseData->output_len);
                src_pos += shader->parseData->output_len;
            } // if
        } // if
    } // for

    // Handle texcoord0 -> point_coord conversion
    if (strstr(shader_source, "[[point_size]]"))
    {
        // !!! FIXME: This assumes all texcoord0 attributes in the effect are
        // !!! FIXME:  actually point coords! It ain't necessarily so! -caleb
        repl = "[[  point_coord  ]]";
        while ((ptr = strstr(shader_source, "[[user(texcoord0)]]")))
        {
            memcpy(ptr, repl, strlen(repl));

            // "float4" -> "float2"
            int spaces = 0;
            while (spaces < 2)
                if (*(ptr--) == ' ')
                    spaces++;
            memcpy(ptr, "2", sizeof(char));
        } // while
    } // if

    // Compile the source into a library
    compileError = NULL;
    shader_source_ns = msg_s(
        msg(ctx->classNSString, ctx->selAlloc),
        ctx->selInitWithUTF8String,
        shader_source
    );
    retval = msg_ppp(ctx->device, ctx->selNewLibraryWithSource,
                        shader_source_ns, NULL, &compileError);
    f(shader_source, d);
    msg(shader_source_ns, ctx->selRelease);

    if (retval == NULL)
    {
        compileError = msg(compileError, ctx->selLocalizedDescription);
        set_error((char*) msg(compileError, ctx->selUTF8String));
        return NULL;
    } // if

    // Run through the shaders again, setting the library reference
    for (i = 0; i < effect->object_count; i++)
    {
        object = &effect->objects[i];
        if (object->type == MOJOSHADER_SYMTYPE_PIXELSHADER
         || object->type == MOJOSHADER_SYMTYPE_VERTEXSHADER)
        {
            if (object->shader.is_preshader)
                continue;

            ((MOJOSHADER_mtlShader*) object->shader.shader)->library = retval;
        } // if
    } // for

    return retval;
} // MOJOSHADER_mtlCompileLibrary

void MOJOSHADER_mtlDeleteLibrary(void *library)
{
    msg(library, ctx->selRelease);
} // MOJOSHADER_mtlDeleteLibrary

MOJOSHADER_mtlShader *MOJOSHADER_mtlCompileShader(const char *mainfn,
                                                  const unsigned char *tokenbuf,
                                                  const unsigned int bufsize,
                                                  const MOJOSHADER_swizzle *swiz,
                                                  const unsigned int swizcount,
                                                  const MOJOSHADER_samplerMap *smap,
                                                  const unsigned int smapcount)
{
    MOJOSHADER_malloc m = ctx->malloc_fn;
    MOJOSHADER_free f = ctx->free_fn;
    void *d = ctx->malloc_data;

    const MOJOSHADER_parseData *pd = MOJOSHADER_parse("metal", mainfn, tokenbuf,
                                                     bufsize, swiz, swizcount,
                                                     smap, smapcount, m, f, d);
    if (pd->error_count > 0)
    {
        // !!! FIXME: put multiple errors in the buffer? Don't use
        // !!! FIXME:  MOJOSHADER_mtlGetError() for this?
        set_error(pd->errors[0].error);
        goto compile_shader_fail;
    } // if

    MOJOSHADER_mtlShader *retval = (MOJOSHADER_mtlShader *) m(sizeof(MOJOSHADER_mtlShader), d);
    if (retval == NULL)
        goto compile_shader_fail;

    retval->parseData = pd;
    retval->refcount = 1;
    retval->ubo = create_ubo(retval, m, d);
    retval->library = NULL; // populated by MOJOSHADER_mtlCompileLibrary

    return retval;

compile_shader_fail:
    MOJOSHADER_freeParseData(retval->parseData);
    f(retval, d);
    return NULL;
} // MOJOSHADER_mtlCompileShader

void MOJOSHADER_mtlShaderAddRef(MOJOSHADER_mtlShader *shader)
{
    if (shader != NULL)
        shader->refcount++;
} // MOJOSHADER_mtlShaderAddRef

void MOJOSHADER_mtlDeleteShader(MOJOSHADER_mtlShader *shader)
{
    if (shader != NULL)
    {
        if (shader->refcount > 1)
            shader->refcount--;
        else
        {
            dealloc_ubo(shader, ctx->free_fn, ctx->malloc_data);
            MOJOSHADER_freeParseData(shader->parseData);
            ctx->free_fn(shader, ctx->malloc_data);
        } // else
    } // if
} // MOJOSHADER_mtlDeleteShader

const MOJOSHADER_parseData *MOJOSHADER_mtlGetShaderParseData(
                                                MOJOSHADER_mtlShader *shader)
{
    return (shader != NULL) ? shader->parseData : NULL;
} // MOJOSHADER_mtlGetParseData

void MOJOSHADER_mtlBindShaders(MOJOSHADER_mtlShader *vshader,
                               MOJOSHADER_mtlShader *pshader)
{
    // Use the last bound shaders in case of NULL
    if (vshader != NULL)
        ctx->vertexShader = vshader;

    if (pshader != NULL)
        ctx->pixelShader = pshader;
} // MOJOSHADER_mtlBindShaders

void MOJOSHADER_mtlGetBoundShaders(MOJOSHADER_mtlShader **vshader,
                                   MOJOSHADER_mtlShader **pshader)
{
    *vshader = ctx->vertexShader;
    *pshader = ctx->pixelShader;
} // MOJOSHADER_mtlGetBoundShaders

void MOJOSHADER_mtlMapUniformBufferMemory(float **vsf, int **vsi, unsigned char **vsb,
                                          float **psf, int **psi, unsigned char **psb)
{
    *vsf = ctx->vs_reg_file_f;
    *vsi = ctx->vs_reg_file_i;
    *vsb = ctx->vs_reg_file_b;
    *psf = ctx->ps_reg_file_f;
    *psi = ctx->ps_reg_file_i;
    *psb = ctx->ps_reg_file_b;
} // MOJOSHADER_mtlMapUniformBufferMemory

void MOJOSHADER_mtlUnmapUniformBufferMemory()
{
    /* This has nothing to do with unmapping memory
     * and everything to do with updating uniform
     * buffers with the latest parameter contents.
     */
    update_uniform_buffer(ctx->vertexShader);
    update_uniform_buffer(ctx->pixelShader);
} // MOJOSHADER_mtlUnmapUniformBufferMemory

void MOJOSHADER_mtlGetUniformBuffers(void **vbuf, int *voff,
                                     void **pbuf, int *poff)
{
    *vbuf = get_uniform_buffer(ctx->vertexShader);
    *voff = get_uniform_offset(ctx->vertexShader);
    *pbuf = get_uniform_buffer(ctx->pixelShader);
    *poff = get_uniform_offset(ctx->pixelShader);
} // MOJOSHADER_mtlGetUniformBuffers

void *MOJOSHADER_mtlGetFunctionHandle(MOJOSHADER_mtlShader *shader)
{
    if (shader == NULL)
        return NULL;

    void *fnname = msg_s(
        msg(ctx->classNSString, ctx->selAlloc),
        ctx->selInitWithUTF8String,
        shader->parseData->mainfn
    );
    void *ret = msg_p(
        shader->library,
        ctx->selNewFunctionWithName,
        fnname
    );
    msg(fnname, ctx->selRelease);
    msg(ret, ctx->selRetain);

    return ret;
} // MOJOSHADER_mtlGetFunctionHandle

void MOJOSHADER_mtlEndFrame()
{
    for (int i = 0; i < ctx->numBuffersInUse; i += 1)
    {
        MOJOSHADER_mtlUniformBuffer *buf = ctx->buffersInUse[i];
        buf->internalOffset = 0;
        buf->currentFrame = (buf->currentFrame + 1) % ctx->framesInFlight;
        buf->inUse = 0;
    } // for
    ctx->numBuffersInUse = 0;
} // MOJOSHADER_mtlEndFrame

int MOJOSHADER_mtlGetVertexAttribLocation(MOJOSHADER_mtlShader *vert,
                                          MOJOSHADER_usage usage, int index)
{
    if (vert == NULL)
        return -1;

    for (int i = 0; i < vert->parseData->attribute_count; i++)
    {
        if (vert->parseData->attributes[i].usage == usage &&
            vert->parseData->attributes[i].index == index)
        {
            return i;
        } // if
    } // for

    // failure, couldn't find requested attribute
    return -1;
} // MOJOSHADER_mtlGetVertexAttribLocation

const char *MOJOSHADER_mtlGetError(void)
{
    return error_buffer;
} // MOJOSHADER_mtlGetError

#endif /* MOJOSHADER_EFFECT_SUPPORT */
#endif /* SUPPORT_PROFILE_METAL && PLATFORM_APPLE */

// end of mojoshader_metal.c ...
