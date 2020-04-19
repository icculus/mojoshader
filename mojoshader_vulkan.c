/**
 * MojoShader; generate shader programs from bytecode of compiled
 *  Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

#define __MOJOSHADER_INTERNAL__ 1
#include "mojoshader_internal.h"

typedef struct MOJOSHADER_vkUniformBuffer MOJOSHADER_vkUniformBuffer;
typedef struct MOJOSHADER_vkShader
{
    const MOJOSHADER_parseData *parseData;
    MOJOSHADER_vkUniformBuffer *ubo;
    int numInternalBuffers;
} MOJOSHADER_vkShader;

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

#ifdef MOJOSHADER_EFFECT_SUPPORT

/* Structs */

typedef struct MOJOSHADER_vkEffect
{
    MOJOSHADER_effect *effect;
    unsigned int num_shaders;
    MOJOSHADER_vkShader *shaders;
    unsigned int *shader_indices;
    unsigned int num_preshaders;
    unsigned int *preshader_indices;
    MOJOSHADER_vkShader *current_vert;
    MOJOSHADER_vkShader *current_frag;
    MOJOSHADER_effectShader *current_vert_raw;
    MOJOSHADER_effectShader *current_frag_raw;
    MOJOSHADER_vkShader *prev_vert;
    MOJOSHADER_vkShader *prev_frag;
} MOJOSHADER_vkEffect;

MOJOSHADER_vkEffect *MOJOSHADER_vkCompileEffect(MOJOSHADER_effect *effect,
                                                  void *vkDevice,
                                                  int numBackingBuffers)
{
    int i;
    MOJOSHADER_malloc m = effect->malloc;
    MOJOSHADER_free f = effect->free;
    void *d = effect->malloc_data;
    int current_shader = 0;
    int current_preshader = 0;
    int src_len = 0;

    MOJOSHADER_vkEffect *retval = (MOJOSHADER_vkEffect *) m(sizeof (MOJOSHADER_vkEffect), d);
    if (retval == NULL)
    {
        out_of_memory();
        return NULL;
    } // if
    memset(retval, '\0', sizeof (MOJOSHADER_vkEffect));

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
    retval->shaders = (MOJOSHADER_vkShader *) m(retval->num_shaders * sizeof (MOJOSHADER_vkShader), d);
    if (retval->shaders == NULL)
    {
        f(retval, d);
        out_of_memory();
        return NULL;
    } // if
    memset(retval->shaders, '\0', retval->num_shaders * sizeof (MOJOSHADER_vkShader));
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

    /* TODO: create shader modules here */
    /* FIXME: THIS CODE IS NOT VALID */
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

            MOJOSHADER_vkShader *curshader = &retval->shaders[current_shader];
            curshader->parseData = object->shader.shader;
            curshader->numInternalBuffers = numBackingBuffers;
            curshader->ubo = create_ubo(curshader, vkDevice, m, d);

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

#endif /* MOJOSHADER_EFFECT_SUPPORT */
