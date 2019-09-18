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
#if !TARGET_OS_IPHONE && !TARGET_OS_TV
#define PLATFORM_MACOSX 1
#endif /* !TARGET_OS_IPHONE && !TARGET_OS_TV */
#endif /* (defined(__APPLE__) && defined(__MACH__)) */

#if PLATFORM_APPLE
#include <objc/message.h>
#endif

#define __MOJOSHADER_INTERNAL__ 1
#include "mojoshader_internal.h"

typedef struct MOJOSHADER_mtlShader
{
    const MOJOSHADER_parseData *parseData;
    void *handle; // MTLFunction*
    void *uniformBuffer; // MTLBuffer*
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

#if SUPPORT_PROFILE_METAL
#ifdef MOJOSHADER_EFFECT_SUPPORT

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

    void *library;              // MTLLibrary*
    void *prev_uniform_buffer;  // MTLBuffer*
} MOJOSHADER_mtlEffect;

const char *MOJOSHADER_mtlGetError(void)
{
	return error_buffer;
}

static void *cstr_to_nsstr(const char *str)
{
    void *nsstr = objc_msgSend((void *) objc_getClass("NSString"), sel_registerName("alloc"));
	return objc_msgSend(nsstr, sel_registerName("initWithUTF8String:"), str);
}

static const char *nsstr_to_cstr(void *str)
{
	return (char *) objc_msgSend(str, sel_registerName("UTF8String"));
}

static void alloc_uniform_buffers(MOJOSHADER_mtlEffect *mtlEffect, void *mtlDevice)
{
    for (int i = 0; i < mtlEffect->num_shaders; ++i)
    {
        int count = mtlEffect->shaders[i].parseData->uniform_count;
        if (count > 0)
        {
            mtlEffect->shaders[i].uniformBuffer = objc_msgSend(
                mtlDevice,
                sel_registerName("newBufferWithLength:options:"),
                16 * count, // all register types are 16 bytes
                NULL
            );
            assert(mtlEffect->shaders[i].uniformBuffer != NULL); // !!! FIXME: replace with something better
        }
        else
        {
            mtlEffect->shaders[i].uniformBuffer = NULL;
        }
    }
} // alloc_uniform_buffers

int MOJOSHADER_mtlGetVertexAttribLocation(MOJOSHADER_mtlShader *vert, MOJOSHADER_usage usage, int index)
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

MOJOSHADER_mtlEffect *MOJOSHADER_mtlCompileEffect(MOJOSHADER_effect *effect, void *mtlDevice)
{
    int i;
    MOJOSHADER_malloc m = effect->malloc;
    MOJOSHADER_free f = effect->free;
    void *d = effect->malloc_data;
    int current_shader = 0;
    int current_preshader = 0;
	void *library;
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
	void *selNewLibrary = sel_registerName("newLibraryWithSource:options:error:");
	library = objc_msgSend(
		mtlDevice,
		selNewLibrary,
		cstr_to_nsstr(shader_source),
		NULL,
		&compileError
	);
	retval->library = library;
    free(shader_source);

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
            retval->shaders[current_shader].handle = objc_msgSend(
				library,
				sel_registerName("newFunctionWithName:"),
				cstr_to_nsstr(object->shader.shader->mainfn)
			);

            //retval->shaders[current_shader].refcount = 1;
            retval->shader_indices[current_shader] = i;
            current_shader++;
        } // if
    } // for

    alloc_uniform_buffers(retval, mtlDevice);
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
    // !!! FIXME: TODO

    // int i;
    // MOJOSHADER_free f = glEffect->effect->free;
    // void *d = glEffect->effect->malloc_data;

    // for (i = 0; i < glEffect->num_shaders; i++)
    // {
    //     /* Arbitarily add a reference to the refcount.
    //      * We're going to be calling glDeleteShader so we can clean out the
    //      * program cache, but we can NOT let it free() the array elements!
    //      * We'll do that ourselves, as we malloc()'d in CompileEffect.
    //      * -flibit
    //      */
    //     glEffect->shaders[i].refcount++;
    //     MOJOSHADER_glDeleteShader(&glEffect->shaders[i]);

    //     /* Delete the shader, but do NOT delete the parse data!
    //      * The parse data belongs to the parent effect.
    //      * -flibit
    //      */
    //     ctx->profileDeleteShader(glEffect->shaders[i].handle);
    // } // for

    // f(glEffect->shader_indices, d);
    // f(glEffect->preshader_indices, d);
    // f(glEffect, d);
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
        mtlEffect->prev_uniform_buffer = mtlEffect->current_vert->uniformBuffer;
    }
} // MOJOSHADER_mtlEffectBegin

// Predeclare
void MOJOSHADER_mtlEffectCommitChanges(MOJOSHADER_mtlEffect *mtlEffect,
                                       MOJOSHADER_mtlShader **newVert,
                                       MOJOSHADER_mtlShader **newFrag,
                                       void **newUniformBuffer);

void MOJOSHADER_mtlEffectBeginPass(MOJOSHADER_mtlEffect *mtlEffect,
                                   unsigned int pass,
                                   MOJOSHADER_mtlShader **newVert,
                                   MOJOSHADER_mtlShader **newFrag,
                                   void **newUniformBuffer)
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
            *newUniformBuffer = mtlEffect->current_vert->uniformBuffer;
        }

        if (mtlEffect->current_frag != NULL)
        {
            *newFrag = mtlEffect->current_frag;
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
        newUniformBuffer
    );

} // MOJOSHADER_mtlEffectBeginPass

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

static void update_uniform_buffer(MOJOSHADER_mtlShader *shader)
{
    if (shader == NULL)
        return;

    int floatCount = 0;
    int intCount = 0;
    int boolCount = 0;
    int uniformCount = shader->parseData->uniform_count;
    int offset = 0;

    void *contents = objc_msgSend(
        shader->uniformBuffer,
        sel_registerName("contents")
    );

    for (int i = 0; i < uniformCount; i++)
    {
        switch (shader->parseData->uniforms[i].type)
        {
            case MOJOSHADER_UNIFORM_FLOAT:
                memcpy(
                    contents + offset,
                    &vs_reg_file_f[floatCount * 4],
                    16
                );
                floatCount++;
                break;

            case MOJOSHADER_UNIFORM_INT:
                intCount++;
                break;

            case MOJOSHADER_UNIFORM_BOOL:
                boolCount++;
                break;

            default:
                assert(0); // This should never happen.
                break;
        }
        offset += 16;
    }
} // update_uniform_buffer

void MOJOSHADER_mtlEffectCommitChanges(MOJOSHADER_mtlEffect *mtlEffect,
                                       MOJOSHADER_mtlShader **newVert,
                                       MOJOSHADER_mtlShader **newFrag,
                                       void **newUniformBuffer)
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
        {
            *newVert = mtlEffect->current_vert;
            *newUniformBuffer = mtlEffect->current_vert->uniformBuffer;
        }

        if (mtlEffect->current_frag != NULL)
        {
            *newFrag = mtlEffect->current_frag;
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

    update_uniform_buffer(mtlEffect->current_vert);
} // MOJOSHADER_mtlEffectCommitChanges


void MOJOSHADER_mtlEffectEndPass(MOJOSHADER_mtlEffect *mtlEffect)
{
    assert(mtlEffect->effect->current_pass != -1);
    mtlEffect->effect->current_pass = -1;
} // MOJOSHADER_mtlEffectEndPass


void MOJOSHADER_mtlEffectEnd(MOJOSHADER_mtlEffect *mtlEffect,
                             MOJOSHADER_mtlShader **newVert,
                             MOJOSHADER_mtlShader **newFrag,
                             void **newUniformBuffer)
{
    if (mtlEffect->effect->restore_shader_state)
    {
        mtlEffect->effect->restore_shader_state = 0;
        *newVert = mtlEffect->prev_vert;
        *newFrag = mtlEffect->prev_frag;
        *newUniformBuffer = &mtlEffect->prev_uniform_buffer;
    } // if

    mtlEffect->effect->state_changes = NULL;
} // MOJOSHADER_mtlEffectEnd

void *MOJOSHADER_mtlGetFunctionHandle(MOJOSHADER_mtlShader *shader)
{
    return shader->handle;
}

#endif // MOJOSHADER_EFFECT_SUPPORT
#endif // SUPPORT_PROFILE_METAL

// end of mojoshader_metal.c ...
