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
    void *library; // MTLLibrary*
    int numInternalBuffers;
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

typedef struct MOJOSHADER_mtlEffect
{
    MOJOSHADER_effect *effect;
    unsigned int num_shaders;
    MOJOSHADER_mtlShader *shaders;
    unsigned int *shader_indices;
    unsigned int num_preshaders;
    unsigned int *preshader_indices;
    MOJOSHADER_mtlShader *current_vert;
    MOJOSHADER_mtlShader *current_frag;
    MOJOSHADER_effectShader *current_vert_raw;
    MOJOSHADER_effectShader *current_frag_raw;
    MOJOSHADER_mtlShader *prev_vert;
    MOJOSHADER_mtlShader *prev_frag;
    void *library; // MTLLibrary*
} MOJOSHADER_mtlEffect;

typedef struct MOJOSHADER_mtlUniformBuffer
{
    void *device; // MTLDevice*
    int bufferSize;
    int numInternalBuffers;
    void **internalBuffers; // MTLBuffer*
    int internalBufferSize;
    int internalOffset;
    int currentFrame;
    int alreadyWritten;
} MOJOSHADER_mtlUniformBuffer;

/* Objective-C selector references */

static void *classNSString = NULL;
static void *selAlloc = NULL;
static void *selInitWithUTF8String = NULL;
static void *selUTF8String = NULL;
static void *selLength = NULL;
static void *selContents = NULL;
static void *selNewBufferWithLength = NULL;
static void *selRelease = NULL;
static void *selNewLibraryWithSource = NULL;
static void *selLocalizedDescription = NULL;
static void *selNewFunctionWithName = NULL;
static void *selRetain = NULL;

/* Helper functions */

static void initSelectors(void)
{
    classNSString = (void*) objc_getClass("NSString");
    selAlloc = sel_registerName("alloc");
    selInitWithUTF8String = sel_registerName("initWithUTF8String:");
    selUTF8String = sel_registerName("UTF8String");
    selLength = sel_registerName("length");
    selContents = sel_registerName("contents");
    selNewBufferWithLength = sel_registerName("newBufferWithLength:options:");
    selRelease = sel_registerName("release");
    selNewLibraryWithSource = sel_registerName("newLibraryWithSource:options:error:");
    selLocalizedDescription = sel_registerName("localizedDescription");
    selNewFunctionWithName = sel_registerName("newFunctionWithName:");
    selRetain = sel_registerName("retain");
} // initSelectors

static void *cstr_to_nsstr(const char *str)
{
    return msg_s(
        msg(classNSString, selAlloc),
        selInitWithUTF8String,
        str
    );
} // cstr_to_nsstr

static const char *nsstr_to_cstr(void *str)
{
    return (char *) msg(str, selUTF8String);
} // nssstr_to_cstr

/* Linked list */

typedef struct LLNODE {
    MOJOSHADER_mtlUniformBuffer *data;
    struct LLNODE *next;
} LLNODE;

static LLNODE *LL_append_node(LLNODE **baseNode,
                              MOJOSHADER_malloc m,
                              void *d)
{
    LLNODE *prev = NULL;
    LLNODE *node = *baseNode;

    /* Append a node to the linked list. */
    while (node != NULL)
    {
        prev = node;
        node = node->next;
    } // while
    node = m(sizeof(LLNODE), d);
    node->next = NULL;

    /* Connect the old to the new. */
    if (prev != NULL)
        prev->next = node;

    /* Special case for the first node. */
    if (*baseNode == NULL)
        *baseNode = node;

    return node;
} // LL_append_node

static void LL_remove_node(LLNODE **baseNode,
                           MOJOSHADER_mtlUniformBuffer *data,
                           MOJOSHADER_free f,
                           void *d)
{
    LLNODE *prev = NULL;
    LLNODE *node = *baseNode;

    /* Search for node with matching data pointer. */
    while (node != NULL && node->data != data)
    {
        prev = node;
        node = node->next;
    } // while

    if (node == NULL)
    {
        /* This should never happen. */
        assert(0);
    } // if

    /* Clear data pointer. The data must be freed separately. */
    node->data = NULL;

    /* Connect the old to the new. */
    if (prev != NULL)
        prev->next = node->next;

    /* Special cases where the first node is removed. */
    if (prev == NULL)
        *baseNode = (node->next != NULL) ? node->next : NULL;

    /* Free the node! */
    f(node, d);
} // LL_remove_node

/* Internal register utilities */

// Max entries for each register file type...
#define MAX_REG_FILE_F 8192
#define MAX_REG_FILE_I 2047
#define MAX_REG_FILE_B 2047

// The constant register files...
// !!! FIXME: Man, it kills me how much memory this takes...
// !!! FIXME:  ... make this dynamically allocated on demand.
float vs_reg_file_f[MAX_REG_FILE_F * 4];
int vs_reg_file_i[MAX_REG_FILE_I * 4];
uint8 vs_reg_file_b[MAX_REG_FILE_B];
float ps_reg_file_f[MAX_REG_FILE_F * 4];
int ps_reg_file_i[MAX_REG_FILE_I * 4];
uint8 ps_reg_file_b[MAX_REG_FILE_B];

static inline void copy_parameter_data(MOJOSHADER_effectParam *params,
                                       unsigned int *param_loc,
                                       MOJOSHADER_symbol *symbols,
                                       unsigned int symbol_count,
                                       float *regf, int *regi, uint8 *regb)
{
    int i, j, r, c;

    i = 0;
    for (i = 0; i < symbol_count; i++)
    {
        const MOJOSHADER_symbol *sym = &symbols[i];
        const MOJOSHADER_effectValue *param = &params[param_loc[i]].value;

        // float/int registers are vec4, so they have 4 elements each
        const uint32 start = sym->register_index << 2;

        if (param->type.parameter_type == MOJOSHADER_SYMTYPE_FLOAT)
            memcpy(regf + start, param->valuesF, sym->register_count << 4);
        else if (sym->register_set == MOJOSHADER_SYMREGSET_FLOAT4)
        {
            // Structs are a whole different world...
            if (param->type.parameter_class == MOJOSHADER_SYMCLASS_STRUCT)
                memcpy(regf + start, param->valuesF, sym->register_count << 4);
            else
            {
                // Sometimes int/bool parameters get thrown into float registers...
                j = 0;
                do
                {
                    c = 0;
                    do
                    {
                        regf[start + (j << 2) + c] = (float) param->valuesI[(j << 2) + c];
                    } while (++c < param->type.columns);
                } while (++j < sym->register_count);
            } // else
        } // else if
        else if (sym->register_set == MOJOSHADER_SYMREGSET_INT4)
            memcpy(regi + start, param->valuesI, sym->register_count << 4);
        else if (sym->register_set == MOJOSHADER_SYMREGSET_BOOL)
        {
            j = 0;
            r = 0;
            do
            {
                c = 0;
                do
                {
                    // regb is not a vec4, enjoy that 'start' bitshift! -flibit
                    regb[(start >> 2) + r + c] = param->valuesI[(j << 2) + c];
                    c++;
                } while (c < param->type.columns && ((r + c) < sym->register_count));
                r += c;
                j++;
            } while (r < sym->register_count);
        } // else if
    } // for
} // copy_parameter_data

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

static int UBO_buffer_length(void *buffer)
{
    return (int) msg(buffer, selLength);
} // UBO_buffer_length

static void *UBO_buffer_contents(void *buffer)
{
    return (void *) msg(buffer, selContents);
} // UBO_buffer_contents

static void *UBO_create_backing_buffer(MOJOSHADER_mtlUniformBuffer *ubo, int f)
{
    void *oldBuffer = ubo->internalBuffers[f];
    void *newBuffer = msg_ip(
        ubo->device,
        selNewBufferWithLength,
        ubo->internalBufferSize,
        NULL
    );
    if (oldBuffer != NULL)
    {
        // Copy over data from old buffer
        memcpy(
            UBO_buffer_contents(newBuffer),
            UBO_buffer_contents(oldBuffer),
            UBO_buffer_length(oldBuffer)
        );

        // Free the old buffer
        msg(oldBuffer, selRelease);
    } //if

    return newBuffer;
} // UBO_create_backing_buffer

static void UBO_predraw(MOJOSHADER_mtlUniformBuffer *ubo)
{
    if (!ubo->alreadyWritten)
    {
        ubo->alreadyWritten = 1;
        return;
    } // if

    ubo->internalOffset += ubo->bufferSize;

    int buflen = UBO_buffer_length(ubo->internalBuffers[ubo->currentFrame]);
    if (ubo->internalOffset >= buflen)
    {
        // Double capacity when we're out of room
        if (ubo->internalOffset >= ubo->internalBufferSize)
            ubo->internalBufferSize *= 2;

        ubo->internalBuffers[ubo->currentFrame] =
            UBO_create_backing_buffer(ubo, ubo->currentFrame);
    } //if
} // UBO_predraw

static void UBO_end_frame(MOJOSHADER_mtlUniformBuffer *ubo)
{
    ubo->internalOffset = 0;
    ubo->currentFrame = (ubo->currentFrame + 1) % ubo->numInternalBuffers;
    ubo->alreadyWritten = 0;
} // UBO_end_frame

LLNODE *ubos = NULL; /* global linked list of all active UBOs */

static MOJOSHADER_mtlUniformBuffer *create_ubo(MOJOSHADER_mtlShader *shader,
                                               void *mtlDevice,
                                               MOJOSHADER_malloc m,
                                               void *d)
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

    // Make the UBO
    MOJOSHADER_mtlUniformBuffer *ubo = (MOJOSHADER_mtlUniformBuffer *) m(sizeof(MOJOSHADER_mtlUniformBuffer), d);
    ubo->device = mtlDevice;
    ubo->alreadyWritten = 0;
    ubo->bufferSize = next_highest_alignment(buflen);
    ubo->currentFrame = 0;
    ubo->numInternalBuffers = shader->numInternalBuffers;
    ubo->internalBufferSize = ubo->bufferSize * 16; // pre-allocate some extra room!
    ubo->internalBuffers = m(ubo->numInternalBuffers * sizeof(void*), d);
    ubo->internalOffset = 0;
    for (int i = 0; i < ubo->numInternalBuffers; i++)
    {
        ubo->internalBuffers[i] = NULL;
        ubo->internalBuffers[i] = UBO_create_backing_buffer(ubo, i);
    } // for

    /* Add the UBO to the global list so it can be updated. */
    LLNODE *node = LL_append_node(&ubos, m, d);
    node->data = ubo;

    return ubo;
} // create_ubo

static void dealloc_ubo(MOJOSHADER_mtlShader *shader,
                        MOJOSHADER_free f,
                        void* d)
{
    if (shader->ubo == NULL)
        return;

    LL_remove_node(&ubos, shader->ubo, f, d);
    for (int i = 0; i < shader->ubo->numInternalBuffers; i++)
    {
        msg(shader->ubo->internalBuffers[i], selRelease);
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
        regF = vs_reg_file_f;
        regI = vs_reg_file_i;
        regB = vs_reg_file_b;
    } // if
    else
    {
        regF = ps_reg_file_f;
        regI = ps_reg_file_i;
        regB = ps_reg_file_b;
    } // else

    UBO_predraw(shader->ubo);
    void *buf = shader->ubo->internalBuffers[shader->ubo->currentFrame];
    void *contents = UBO_buffer_contents(buf) + shader->ubo->internalOffset;

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

MOJOSHADER_mtlEffect *MOJOSHADER_mtlCompileEffect(MOJOSHADER_effect *effect,
                                                  void *mtlDevice,
                                                  int numBackingBuffers)
{
    int i;
    MOJOSHADER_malloc m = effect->malloc;
    MOJOSHADER_free f = effect->free;
    void *d = effect->malloc_data;
    int current_shader = 0;
    int current_preshader = 0;
    int src_len = 0;

    // Make sure the Objective-C selectors have been initialized...
    if (selAlloc == NULL)
        initSelectors();

    MOJOSHADER_mtlEffect *retval = (MOJOSHADER_mtlEffect *) m(sizeof (MOJOSHADER_mtlEffect), d);
    if (retval == NULL)
    {
        out_of_memory();
        return NULL;
    } // if
    memset(retval, '\0', sizeof (MOJOSHADER_mtlEffect));

    // Count the number of shaders before allocating
    for (i = 0; i < effect->object_count; i++)
    {
        MOJOSHADER_effectObject *object = &effect->objects[i];
        if (object->type == MOJOSHADER_SYMTYPE_PIXELSHADER
         || object->type == MOJOSHADER_SYMTYPE_VERTEXSHADER)
        {
            if (object->shader.is_preshader)
                retval->num_preshaders++;
            else
            {
                retval->num_shaders++;
                src_len += object->shader.shader->output_len;
            } // else
        } // if
    } // for

    // Alloc shader source buffer
    char *shader_source = (char *) m(src_len + 1, d);
    memset(shader_source, '\0', src_len + 1);
    int src_pos = 0;

    // Copy all the source text into the buffer
    for (i = 0; i < effect->object_count; i++)
    {
        MOJOSHADER_effectObject *object = &effect->objects[i];
        if (object->type == MOJOSHADER_SYMTYPE_PIXELSHADER
         || object->type == MOJOSHADER_SYMTYPE_VERTEXSHADER)
        {
            if (!object->shader.is_preshader)
            {
                int output_len = object->shader.shader->output_len;
                memcpy(&shader_source[src_pos], object->shader.shader->output, output_len);
                src_pos += output_len;
            } // if
        } // if
    } // for

    // Handle texcoord0 -> point_coord conversion
    if (strstr(shader_source, "[[point_size]]"))
    {
        // !!! FIXME: This assumes all texcoord0 attributes in the effect are
        // !!! FIXME:  actually point coords! It ain't necessarily so! -caleb
        const char *repl = "[[  point_coord  ]]";
        char *ptr;
        while ((ptr = strstr(shader_source, "[[user(texcoord0)]]")))
        {
            memcpy(ptr, repl, strlen(repl));

            // float4 -> float2
            int spaces = 0;
            while (spaces < 2)
                if (*(ptr--) == ' ')
                    spaces++;
            memcpy(ptr, "2", sizeof(char));
        } // while
    } // if

    // Alloc shader information
    retval->shaders = (MOJOSHADER_mtlShader *) m(retval->num_shaders * sizeof (MOJOSHADER_mtlShader), d);
    if (retval->shaders == NULL)
    {
        f(retval, d);
        out_of_memory();
        return NULL;
    } // if
    memset(retval->shaders, '\0', retval->num_shaders * sizeof (MOJOSHADER_mtlShader));
    retval->shader_indices = (unsigned int *) m(retval->num_shaders * sizeof (unsigned int), d);
    if (retval->shader_indices == NULL)
    {
        f(retval->shaders, d);
        f(retval, d);
        out_of_memory();
        return NULL;
    } // if
    memset(retval->shader_indices, '\0', retval->num_shaders * sizeof (unsigned int));

    // Alloc preshader information
    if (retval->num_preshaders > 0)
    {
        retval->preshader_indices = (unsigned int *) m(retval->num_preshaders * sizeof (unsigned int), d);
        if (retval->preshader_indices == NULL)
        {
            f(retval->shaders, d);
            f(retval->shader_indices, d);
            f(retval, d);
            out_of_memory();
            return NULL;
        } // if
        memset(retval->preshader_indices, '\0', retval->num_preshaders * sizeof (unsigned int));
    } // if

    // Compile the source into a library
    void *compileError = NULL;
    void *shader_source_ns = cstr_to_nsstr(shader_source);
    void *library = msg_ppp(
        mtlDevice,
        selNewLibraryWithSource,
        shader_source_ns,
        NULL,
        &compileError
    );
    retval->library = library;
    f(shader_source, d);
    msg(shader_source_ns, selRelease);

    if (library == NULL)
    {
        // Set the error
        void *error_nsstr = msg(compileError, selLocalizedDescription);
        set_error(nsstr_to_cstr(error_nsstr));

        goto compile_shader_fail;
    } // if

    // Run through the shaders again, tracking the object indices
    for (i = 0; i < effect->object_count; i++)
    {
        MOJOSHADER_effectObject *object = &effect->objects[i];
        if (object->type == MOJOSHADER_SYMTYPE_PIXELSHADER
         || object->type == MOJOSHADER_SYMTYPE_VERTEXSHADER)
        {
            if (object->shader.is_preshader)
            {
                retval->preshader_indices[current_preshader++] = i;
                continue;
            } // if

            MOJOSHADER_mtlShader *curshader = &retval->shaders[current_shader];
            curshader->parseData = object->shader.shader;
            curshader->numInternalBuffers = numBackingBuffers;
            curshader->ubo = create_ubo(curshader, mtlDevice, m, d);
            curshader->library = library;

            retval->shader_indices[current_shader] = i;

            current_shader++;
        } // if
    } // for

    retval->effect = effect;
    return retval;

compile_shader_fail:
    f(retval->shader_indices, d);
    f(retval->shaders, d);
    f(retval, d);
    return NULL;
} // MOJOSHADER_mtlCompileEffect

void MOJOSHADER_mtlDeleteEffect(MOJOSHADER_mtlEffect *mtlEffect)
{
    MOJOSHADER_free f = mtlEffect->effect->free;
    void *d = mtlEffect->effect->malloc_data;

    int i;
    for (i = 0; i < mtlEffect->num_shaders; i++)
    {
        /* Release the uniform buffers */
        dealloc_ubo(&mtlEffect->shaders[i], f, d);
    } // for

    /* Release the library */
    msg(mtlEffect->library, selRelease);

    f(mtlEffect->shader_indices, d);
    f(mtlEffect->preshader_indices, d);
    f(mtlEffect, d);
} // MOJOSHADER_mtlDeleteEffect


void MOJOSHADER_mtlEffectBegin(MOJOSHADER_mtlEffect *mtlEffect,
                               unsigned int *numPasses,
                               int saveShaderState,
                               MOJOSHADER_effectStateChanges *stateChanges)
{
    *numPasses = mtlEffect->effect->current_technique->pass_count;
    mtlEffect->effect->restore_shader_state = saveShaderState;
    mtlEffect->effect->state_changes = stateChanges;

    if (mtlEffect->effect->restore_shader_state)
    {
        mtlEffect->prev_vert = mtlEffect->current_vert;
        mtlEffect->prev_frag = mtlEffect->current_frag;
    } // if
} // MOJOSHADER_mtlEffectBegin

// Predeclare
void MOJOSHADER_mtlEffectCommitChanges(MOJOSHADER_mtlEffect *mtlEffect,
                                       MOJOSHADER_mtlShaderState *shState);

void MOJOSHADER_mtlEffectBeginPass(MOJOSHADER_mtlEffect *mtlEffect,
                                   unsigned int pass,
                                   MOJOSHADER_mtlShaderState *shState)
{
    int i, j;
    MOJOSHADER_effectPass *curPass;
    MOJOSHADER_effectState *state;
    MOJOSHADER_effectShader *rawVert = mtlEffect->current_vert_raw;
    MOJOSHADER_effectShader *rawFrag = mtlEffect->current_frag_raw;
    int has_preshader = 0;

    assert(shState != NULL);
    assert(mtlEffect->effect->current_pass == -1);
    mtlEffect->effect->current_pass = pass;
    curPass = &mtlEffect->effect->current_technique->passes[pass];

    // !!! FIXME: I bet this could be stored at parse/compile time. -flibit
    for (i = 0; i < curPass->state_count; i++)
    {
        state = &curPass->states[i];
        #define ASSIGN_SHADER(stype, raw, mtls) \
            (state->type == stype) \
            { \
                j = 0; \
                do \
                { \
                    if (*state->value.valuesI == mtlEffect->shader_indices[j]) \
                    { \
                        raw = &mtlEffect->effect->objects[*state->value.valuesI].shader; \
                        mtlEffect->mtls = &mtlEffect->shaders[j]; \
                        break; \
                    } \
                    else if (mtlEffect->num_preshaders > 0 \
                          && *state->value.valuesI == mtlEffect->preshader_indices[j]) \
                    { \
                        raw = &mtlEffect->effect->objects[*state->value.valuesI].shader; \
                        has_preshader = 1; \
                        break; \
                    } \
                } while (++j < mtlEffect->num_shaders); \
            }
        if ASSIGN_SHADER(MOJOSHADER_RS_VERTEXSHADER, rawVert, current_vert)
        else if ASSIGN_SHADER(MOJOSHADER_RS_PIXELSHADER, rawFrag, current_frag)
        #undef ASSIGN_SHADER
    } // for

    mtlEffect->effect->state_changes->render_state_changes = curPass->states;
    mtlEffect->effect->state_changes->render_state_change_count = curPass->state_count;

    mtlEffect->current_vert_raw = rawVert;
    mtlEffect->current_frag_raw = rawFrag;

    /* If this effect pass has an array of shaders, we get to wait until
     * CommitChanges to actually bind the final shaders.
     * -flibit
     */
    if (!has_preshader)
    {
        if (mtlEffect->current_vert != NULL)
        {
            MOJOSHADER_mtlShader *vert = mtlEffect->current_vert;
            shState->vertexShader = vert;
            shState->vertexUniformBuffer = get_uniform_buffer(vert);
            shState->vertexUniformOffset = get_uniform_offset(vert);
        } // if

        if (mtlEffect->current_frag != NULL)
        {
            MOJOSHADER_mtlShader *frag = mtlEffect->current_frag;
            shState->fragmentShader = frag;
            shState->fragmentUniformBuffer = get_uniform_buffer(frag);
            shState->fragmentUniformOffset = get_uniform_offset(frag);
        } // if

        if (mtlEffect->current_vert_raw != NULL)
        {
            mtlEffect->effect->state_changes->vertex_sampler_state_changes = rawVert->samplers;
            mtlEffect->effect->state_changes->vertex_sampler_state_change_count = rawVert->sampler_count;
        } // if
        if (mtlEffect->current_frag_raw != NULL)
        {
            mtlEffect->effect->state_changes->sampler_state_changes = rawFrag->samplers;
            mtlEffect->effect->state_changes->sampler_state_change_count = rawFrag->sampler_count;
        } // if
    } // if

    MOJOSHADER_mtlEffectCommitChanges(mtlEffect, shState);
} // MOJOSHADER_mtlEffectBeginPass

void MOJOSHADER_mtlEffectCommitChanges(MOJOSHADER_mtlEffect *mtlEffect,
                                       MOJOSHADER_mtlShaderState *shState)
{
    MOJOSHADER_effectShader *rawVert = mtlEffect->current_vert_raw;
    MOJOSHADER_effectShader *rawFrag = mtlEffect->current_frag_raw;

    /* Used for shader selection from preshaders */
    int i, j;
    MOJOSHADER_effectValue *param;
    float selector;
    int shader_object;
    int selector_ran = 0;

    /* For effect passes with arrays of shaders, we have to run a preshader
     * that determines which shader to use, based on a parameter's value.
     * -flibit
     */
    // !!! FIXME: We're just running the preshaders every time. Blech. -flibit
    #define SELECT_SHADER_FROM_PRESHADER(raw, mtls) \
        if (raw != NULL && raw->is_preshader) \
        { \
            i = 0; \
            do \
            { \
                param = &mtlEffect->effect->params[raw->preshader_params[i]].value; \
                for (j = 0; j < (param->value_count >> 2); j++) \
                    memcpy(raw->preshader->registers + raw->preshader->symbols[i].register_index + j, \
                           param->valuesI + (j << 2), \
                           param->type.columns << 2); \
            } while (++i < raw->preshader->symbol_count); \
            MOJOSHADER_runPreshader(raw->preshader, &selector); \
            shader_object = mtlEffect->effect->params[raw->params[0]].value.valuesI[(int) selector]; \
            raw = &mtlEffect->effect->objects[shader_object].shader; \
            i = 0; \
            do \
            { \
                if (shader_object == mtlEffect->shader_indices[i]) \
                { \
                    mtls = &mtlEffect->shaders[i]; \
                    break; \
                } \
            } while (++i < mtlEffect->num_shaders); \
            selector_ran = 1; \
        }
    SELECT_SHADER_FROM_PRESHADER(rawVert, mtlEffect->current_vert)
    SELECT_SHADER_FROM_PRESHADER(rawFrag, mtlEffect->current_frag)
    #undef SELECT_SHADER_FROM_PRESHADER
    if (selector_ran)
    {
        if (mtlEffect->current_vert != NULL)
            shState->vertexShader = mtlEffect->current_vert;

        if (mtlEffect->current_frag != NULL)
            shState->fragmentShader = mtlEffect->current_frag;

        if (mtlEffect->current_vert_raw != NULL)
        {
            mtlEffect->effect->state_changes->vertex_sampler_state_changes = rawVert->samplers;
            mtlEffect->effect->state_changes->vertex_sampler_state_change_count = rawVert->sampler_count;
        } // if
        if (mtlEffect->current_frag_raw != NULL)
        {
            mtlEffect->effect->state_changes->sampler_state_changes = rawFrag->samplers;
            mtlEffect->effect->state_changes->sampler_state_change_count = rawFrag->sampler_count;
        } // if
    } // if

    /* This is where parameters are copied into the constant buffers.
     * If you're looking for where things slow down immensely, look at
     * the copy_parameter_data() and MOJOSHADER_runPreshader() functions.
     * -flibit
     */
    // !!! FIXME: We're just copying everything every time. Blech. -flibit
    // !!! FIXME: We're just running the preshaders every time. Blech. -flibit
    // !!! FIXME: Will the preshader ever want int/bool registers? -flibit
    #define COPY_PARAMETER_DATA(raw, stage) \
        if (raw != NULL) \
        { \
            copy_parameter_data(mtlEffect->effect->params, raw->params, \
                                raw->shader->symbols, \
                                raw->shader->symbol_count, \
                                stage##_reg_file_f, \
                                stage##_reg_file_i, \
                                stage##_reg_file_b); \
            if (raw->shader->preshader) \
            { \
                copy_parameter_data(mtlEffect->effect->params, raw->preshader_params, \
                                    raw->shader->preshader->symbols, \
                                    raw->shader->preshader->symbol_count, \
                                    raw->shader->preshader->registers, \
                                    NULL, \
                                    NULL); \
                MOJOSHADER_runPreshader(raw->shader->preshader, stage##_reg_file_f); \
            } \
        }
    COPY_PARAMETER_DATA(rawVert, vs)
    COPY_PARAMETER_DATA(rawFrag, ps)
    #undef COPY_PARAMETER_DATA

    update_uniform_buffer(shState->vertexShader);
    shState->vertexUniformBuffer = get_uniform_buffer(shState->vertexShader);
    shState->vertexUniformOffset = get_uniform_offset(shState->vertexShader);

    update_uniform_buffer(shState->fragmentShader);
    shState->fragmentUniformBuffer = get_uniform_buffer(shState->fragmentShader);
    shState->fragmentUniformOffset = get_uniform_offset(shState->fragmentShader);
} // MOJOSHADER_mtlEffectCommitChanges


void MOJOSHADER_mtlEffectEndPass(MOJOSHADER_mtlEffect *mtlEffect)
{
    assert(mtlEffect->effect->current_pass != -1);
    mtlEffect->effect->current_pass = -1;
} // MOJOSHADER_mtlEffectEndPass


void MOJOSHADER_mtlEffectEnd(MOJOSHADER_mtlEffect *mtlEffect,
                             MOJOSHADER_mtlShaderState *shState)
{
    if (mtlEffect->effect->restore_shader_state)
    {
        mtlEffect->effect->restore_shader_state = 0;
        shState->vertexShader = mtlEffect->prev_vert;
        shState->fragmentShader = mtlEffect->prev_frag;
        shState->vertexUniformBuffer = get_uniform_buffer(mtlEffect->prev_vert);
        shState->fragmentUniformBuffer = get_uniform_buffer(mtlEffect->prev_frag);
        shState->vertexUniformOffset = get_uniform_offset(mtlEffect->prev_vert);
        shState->fragmentUniformOffset = get_uniform_offset(mtlEffect->prev_frag);
    } // if

    mtlEffect->effect->state_changes = NULL;
} // MOJOSHADER_mtlEffectEnd

void *MOJOSHADER_mtlGetFunctionHandle(MOJOSHADER_mtlShader *shader)
{
    if (shader == NULL)
        return NULL;

    void *fnname = cstr_to_nsstr(shader->parseData->mainfn);
    void *ret = msg_p(
        shader->library,
        selNewFunctionWithName,
        fnname
    );
    msg(fnname, selRelease);
    msg(ret, selRetain);

    return ret;
} // MOJOSHADER_mtlGetFunctionHandle

void MOJOSHADER_mtlEndFrame()
{
    LLNODE *node = ubos;
    while (node != NULL)
    {
        UBO_end_frame((MOJOSHADER_mtlUniformBuffer *) node->data);
        node = node->next;
    } // while
} // MOJOSHADER_mtlEndFrame

int MOJOSHADER_mtlGetVertexAttribLocation(MOJOSHADER_mtlShader *vert,
                                          MOJOSHADER_usage usage, int index)
{
    if (vert == NULL)
        return -1;

    for (int i = 0; i < vert->parseData->attribute_count; i++)
    {
        if (vert->parseData->attributes[i].usage == usage
            && vert->parseData->attributes[i].index == index)
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
