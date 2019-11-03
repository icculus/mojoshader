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

#if (defined(__APPLE__) && defined(__MACH__))

#define PLATFORM_APPLE 1
#include "TargetConditionals.h"

#define OBJC_OLD_DISPATCH_PROTOTYPES 1
#include <objc/message.h>
#define objc_msgSend_STR ((void* (*)(void*, void*, const char*))objc_msgSend)
#define objc_msgSend_PTR ((void* (*)(void*, void*, void*))objc_msgSend)
#define objc_msgSend_INT_PTR ((void* (*)(void*, void*, int, void*))objc_msgSend)
#define objc_msgSend_PTR_PTR_PTR ((void* (*)(void*, void*, void*, void*, void*))objc_msgSend)

#endif /* (defined(__APPLE__) && defined(__MACH__)) */

#define __MOJOSHADER_INTERNAL__ 1
#include "mojoshader_internal.h"

typedef struct MOJOSHADER_mtlUniformBuffer MOJOSHADER_mtlUniformBuffer;
typedef struct MOJOSHADER_mtlShader
{
    const MOJOSHADER_parseData *parseData;
    void *handle; // MTLFunction*
    MOJOSHADER_mtlUniformBuffer *ubo;
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
    void *device; // MTLDevice
    int bufferSize;
    int numInternalBuffers;
    void **internalBuffers; // MTLBuffer*
    int internalBufferSize;
    int internalOffset;
    int currentFrame;
    int alreadyWritten;
} MOJOSHADER_mtlUniformBuffer;

/* Helper functions */

const char *MOJOSHADER_mtlGetError(void)
{
    return error_buffer;
}

static void *cstr_to_nsstr(const char *str)
{
    void *nsstr = objc_msgSend(
        (void*) objc_getClass("NSString"), sel_registerName("alloc")
    );
    return objc_msgSend_STR(
        nsstr, sel_registerName("initWithUTF8String:"), str
    );
}

static const char *nsstr_to_cstr(void *str)
{
    return (char *) objc_msgSend(str, sel_registerName("UTF8String"));
}

/* Linked list */

typedef struct LLNODE {
    void *data;
    struct LLNODE *next;
} LLNODE;

static LLNODE *LL_append_node(LLNODE **baseNode)
{
    LLNODE *prev = NULL;
    LLNODE *node = *baseNode;

    /* Append a new node to the end of the linked list. */
    while (node != NULL)
    {
        prev = node;
        node = node->next;
    }
    node = malloc(sizeof(LLNODE));
    node->next = NULL;

    /* Connect the old to the new. */
    if (prev != NULL)
        prev->next = node;

    /* Special case for the first node. */
    if (*baseNode == NULL)
        *baseNode = node;

    return node;
}

static void LL_remove_node(LLNODE **baseNode, void *data)
{
    LLNODE *prev = NULL;
    LLNODE *node = *baseNode;

    /* Search for node with matching data pointer. */
    while (node != NULL && node->data != data)
    {
        prev = node;
        node = node->next;
    }
    if (node == NULL)
    {
        set_error("No such node found!");
        return;
    }

    /* Clear data pointer. The data must be freed separately. */
    node->data = NULL;

    /* Connect the old to the new. */
    if (prev != NULL)
        prev->next = node->next;

    /* Special case where the first node is removed. */
    if (prev == NULL && node->next != NULL)
        *baseNode = node->next;

    free(node);
}

/* Uniform buffer utilities */

static inline int next_highest_alignment(int n)
{
    #if TARGET_OS_IOS || TARGET_OS_TV || TARGET_OS_SIMULATOR
    int align = 16;
    #else
    int align = 256;
    #endif

    return align * ((n + align - 1) / align);
}

static int UBO_buffer_length(void *buffer)
{
    return (int) objc_msgSend(buffer, sel_registerName("length"));
}

static void *UBO_buffer_contents(void *buffer)
{
    return (void *) objc_msgSend(buffer, sel_registerName("contents"));
}

static void *UBO_create_backing_buffer(MOJOSHADER_mtlUniformBuffer *ubo, int f)
{
    void *oldBuffer = ubo->internalBuffers[f];
    void *newBuffer = objc_msgSend_INT_PTR(
        ubo->device,
        sel_registerName("newBufferWithLength:options:"),
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
        objc_msgSend(oldBuffer, sel_registerName("release"));
    }
    return newBuffer;
}

static void UBO_predraw(MOJOSHADER_mtlUniformBuffer *ubo)
{
    if (!ubo->alreadyWritten)
    {
        ubo->alreadyWritten = 1;
        return;
    }

    ubo->internalOffset += ubo->bufferSize;

    int buflen = UBO_buffer_length(ubo->internalBuffers[ubo->currentFrame]);
    if (ubo->internalOffset >= buflen)
    {
        if (ubo->internalOffset >= ubo->internalBufferSize)
        {
            // Double capacity when we're out of room
            printf("UBO: We need more space! Doubling internal buffer size!\n");
            ubo->internalBufferSize *= 2;
        }
        ubo->internalBuffers[ubo->currentFrame] = UBO_create_backing_buffer(ubo, ubo->currentFrame);
    }
}

static void UBO_end_frame(MOJOSHADER_mtlUniformBuffer *ubo)
{
    ubo->internalOffset = 0;
    ubo->currentFrame = (ubo->currentFrame + 1) % ubo->numInternalBuffers;
    ubo->alreadyWritten = 0;
}

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

LLNODE *ubos = NULL; /* global linked list of all active UBOs */

static MOJOSHADER_mtlUniformBuffer *create_ubo(MOJOSHADER_mtlShader *shader,
                                               void *mtlDevice)
{
    if (shader->parseData->uniform_count == 0)
        return NULL;

    MOJOSHADER_mtlUniformBuffer *ubo = malloc(sizeof(MOJOSHADER_mtlUniformBuffer));
    ubo->device = mtlDevice;
    ubo->alreadyWritten = 0;
    ubo->bufferSize = next_highest_alignment(shader->parseData->uniform_count * 16);
    ubo->currentFrame = 0;
    ubo->numInternalBuffers = shader->numInternalBuffers;
    ubo->internalBufferSize = ubo->bufferSize * 16; /* Pre-allocate some extra room. */
    ubo->internalBuffers = malloc(ubo->numInternalBuffers * sizeof(void*));
    ubo->internalOffset = 0;

    for (int i = 0; i < ubo->numInternalBuffers; i++)
    {
        ubo->internalBuffers[i] = NULL;
        ubo->internalBuffers[i] = UBO_create_backing_buffer(ubo, i);
    }

    /* Add the UBO to the global list so it can be updated. */
    LLNODE *node = LL_append_node(&ubos);
    node->data = (void *) ubo;

    return ubo;
}

static void dealloc_ubo(MOJOSHADER_mtlShader *shader)
{
    if (shader->ubo == NULL)
        return;

    LL_remove_node(&ubos, shader->ubo);
    for (int i = 0; i < shader->ubo->numInternalBuffers; i++)
    {
        objc_msgSend(shader->ubo->internalBuffers[i], sel_registerName("release"));
        shader->ubo->internalBuffers[i] = NULL;
    }
    free(shader->ubo->internalBuffers);
    free(shader->ubo);
}

static void *get_uniform_buffer(MOJOSHADER_mtlShader *shader)
{
    if (shader == NULL || shader->ubo == NULL)
        return NULL;

    return shader->ubo->internalBuffers[shader->ubo->currentFrame];
}

static int get_uniform_offset(MOJOSHADER_mtlShader *shader)
{
    if (shader == NULL || shader->ubo == NULL)
        return 0;

    return shader->ubo->internalOffset;
}

static void update_uniform_buffer(MOJOSHADER_mtlShader *shader)
{
    if (shader == NULL || shader->ubo == NULL)
        return;

    int uniformCount = shader->parseData->uniform_count;
    int offset = 0;

    float *regF; int *regI; uint8 *regB;
    if (shader->parseData->shader_type == MOJOSHADER_TYPE_VERTEX)
    {
        regF = vs_reg_file_f;
        regI = vs_reg_file_i;
        regB = vs_reg_file_b;
    }
    else
    {
        regF = ps_reg_file_f;
        regI = ps_reg_file_i;
        regB = ps_reg_file_b;
    }

    UBO_predraw(shader->ubo);
    void *buf = shader->ubo->internalBuffers[shader->ubo->currentFrame];
    void *contents = UBO_buffer_contents(buf) + shader->ubo->internalOffset;

    for (int i = 0; i < uniformCount; i++)
    {
        switch (shader->parseData->uniforms[i].type)
        {
            case MOJOSHADER_UNIFORM_FLOAT:
                memcpy(
                    contents + offset,
                    &regF[4 * shader->parseData->uniforms[i].index],
                    16
                );
                break;

            case MOJOSHADER_UNIFORM_INT:
                // !!! FIXME: Need a test case
                memcpy(
                    contents + offset,
                    &regI[4 * shader->parseData->uniforms[i].index],
                    16
                );
                break;

            case MOJOSHADER_UNIFORM_BOOL:
                // !!! FIXME: Need a test case
                memcpy(
                    contents + offset,
                    &regB[4 * shader->parseData->uniforms[i].index],
                    16
                );
                break;

            default:
                assert(0); // This should never happen.
                break;
        }
        offset += 16;
    }
} // update_uniform_buffer

/* Public API */

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
        }
    }

    // failure, couldn't find requested attribute
    return -1;
} // MOJOSHADER_mtlGetVertexAttribLocation

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
            }
        } // if
    } // for

    // Alloc shader source buffer
    char *shader_source = malloc(src_len + 1);
    memset(shader_source, '\0', src_len + 1);
    int src_pos = 0;

    // Copy all the combined source text into the buffer
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
    void *library = objc_msgSend_PTR_PTR_PTR(
        mtlDevice,
        sel_registerName("newLibraryWithSource:options:error:"),
        shader_source_ns,
        NULL,
        &compileError
    );
    retval->library = library;
    free(shader_source);
    objc_msgSend(shader_source_ns, sel_registerName("release"));

    if (library == NULL)
    {
        // Set the error
        void *error_nsstr = objc_msgSend(compileError, sel_registerName("localizedDescription"));
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
            retval->shaders[current_shader].parseData = object->shader.shader;

            void *fnname = cstr_to_nsstr(object->shader.shader->mainfn);
            retval->shaders[current_shader].handle = objc_msgSend_PTR(
                library,
                sel_registerName("newFunctionWithName:"),
                fnname
            );
            objc_msgSend(fnname, sel_registerName("release"));

            retval->shaders[current_shader].numInternalBuffers = numBackingBuffers;
            retval->shaders[current_shader].ubo = create_ubo(
                &retval->shaders[current_shader],
                mtlDevice
            );
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
    void *selRelease = sel_registerName("release");

    int i;
    for (i = 0; i < mtlEffect->num_shaders; i++)
    {
        /* Release the uniform buffers */
        dealloc_ubo(&mtlEffect->shaders[i]);

        /* Delete the shader, but do NOT delete the parse data!
         * The parse data belongs to the parent effect.
         */
        objc_msgSend(mtlEffect->shaders[i].handle, selRelease);
    }

    /* Release the library */
    objc_msgSend(mtlEffect->library, selRelease);

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
    }
} // MOJOSHADER_mtlEffectBegin

// Predeclare
void MOJOSHADER_mtlEffectCommitChanges(MOJOSHADER_mtlEffect *mtlEffect,
                                       MOJOSHADER_mtlShader **newVert,
                                       MOJOSHADER_mtlShader **newFrag,
                                       void **newVertUniformBuffer,
                                       void **newFragUniformBuffer,
                                       int *newVertUniformOffset,
                                       int *newFragUniformOffset);

void MOJOSHADER_mtlEffectBeginPass(MOJOSHADER_mtlEffect *mtlEffect,
                                   unsigned int pass,
                                   MOJOSHADER_mtlShader **newVert,
                                   MOJOSHADER_mtlShader **newFrag,
                                   void **newVertUniformBuffer,
                                   void **newFragUniformBuffer,
                                   int *newVertUniformOffset,
                                   int *newFragUniformOffset)
{
    int i, j;
    MOJOSHADER_effectPass *curPass;
    MOJOSHADER_effectState *state;
    MOJOSHADER_effectShader *rawVert = mtlEffect->current_vert_raw;
    MOJOSHADER_effectShader *rawFrag = mtlEffect->current_frag_raw;
    int has_preshader = 0;

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
            *newVert = mtlEffect->current_vert;
            *newVertUniformBuffer = get_uniform_buffer(mtlEffect->current_vert);
            *newVertUniformOffset = get_uniform_offset(mtlEffect->current_vert);
        }

        if (mtlEffect->current_frag != NULL)
        {
            *newFrag = mtlEffect->current_frag;
            *newFragUniformBuffer = get_uniform_buffer(mtlEffect->current_frag);
            *newFragUniformOffset = get_uniform_offset(mtlEffect->current_frag);
        }

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

    MOJOSHADER_mtlEffectCommitChanges(
        mtlEffect,
        newVert,
        newFrag,
        newVertUniformBuffer,
        newFragUniformBuffer,
        newVertUniformOffset,
        newFragUniformOffset
    );

} // MOJOSHADER_mtlEffectBeginPass

void MOJOSHADER_mtlEffectCommitChanges(MOJOSHADER_mtlEffect *mtlEffect,
                                       MOJOSHADER_mtlShader **newVert,
                                       MOJOSHADER_mtlShader **newFrag,
                                       void **newVertUniformBuffer,
                                       void **newFragUniformBuffer,
                                       int *newVertUniformOffset,
                                       int *newFragUniformOffset)
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
            *newVert = mtlEffect->current_vert;

        if (mtlEffect->current_frag != NULL)
            *newFrag = mtlEffect->current_frag;

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

    update_uniform_buffer(*newVert);
    update_uniform_buffer(*newFrag);

    *newVertUniformBuffer = get_uniform_buffer(*newVert);
    *newFragUniformBuffer = get_uniform_buffer(*newFrag);
    *newVertUniformOffset = get_uniform_offset(*newVert);
    *newFragUniformOffset = get_uniform_offset(*newFrag);
} // MOJOSHADER_mtlEffectCommitChanges


void MOJOSHADER_mtlEffectEndPass(MOJOSHADER_mtlEffect *mtlEffect)
{
    assert(mtlEffect->effect->current_pass != -1);
    mtlEffect->effect->current_pass = -1;
} // MOJOSHADER_mtlEffectEndPass


void MOJOSHADER_mtlEffectEnd(MOJOSHADER_mtlEffect *mtlEffect,
                             MOJOSHADER_mtlShader **newVert,
                             MOJOSHADER_mtlShader **newFrag,
                             void **newVertUniformBuffer,
                             void **newFragUniformBuffer,
                             int *newVertUniformOffset,
                             int *newFragUniformOffset)
{
    if (mtlEffect->effect->restore_shader_state)
    {
        mtlEffect->effect->restore_shader_state = 0;
        *newVert = mtlEffect->prev_vert;
        *newFrag = mtlEffect->prev_frag;
        *newVertUniformBuffer = get_uniform_buffer(mtlEffect->prev_vert);
        *newFragUniformBuffer = get_uniform_buffer(mtlEffect->prev_frag);
        *newVertUniformOffset = get_uniform_offset(mtlEffect->prev_vert);
        *newFragUniformOffset = get_uniform_offset(mtlEffect->prev_frag);
    } // if

    mtlEffect->effect->state_changes = NULL;
} // MOJOSHADER_mtlEffectEnd

void *MOJOSHADER_mtlGetFunctionHandle(MOJOSHADER_mtlShader *shader)
{
    if (shader == NULL)
        return NULL;
    return shader->handle;
}

void MOJOSHADER_mtlEndFrame()
{
    LLNODE *node = ubos;
    while (node != NULL)
    {
        UBO_end_frame((MOJOSHADER_mtlUniformBuffer *) node->data);
        node = node->next;
    }
}

#endif /* MOJOSHADER_EFFECT_SUPPORT */
#endif /* SUPPORT_PROFILE_METAL && PLATFORM_APPLE */

// end of mojoshader_metal.c ...
