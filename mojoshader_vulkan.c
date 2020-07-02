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

#if SUPPORT_PROFILE_SPIRV

#include "vulkan/vulkan.h"

#define VULKAN_INSTANCE_FUNCTION(ret, func, params) \
    typedef ret (VKAPI_CALL *vkfntype_MOJOSHADER_##func) params;
#define VULKAN_DEVICE_FUNCTION(ret, func, params) \
	typedef ret (VKAPI_CALL *vkfntype_MOJOSHADER_##func) params;
#include "mojoshader_vulkan_vkfuncs.h"

#define UBO_BUFFER_COUNT 8
#define UBO_BUFFER_SIZE 1048576 /* ~1MB */

// Internal struct defs...

typedef struct MOJOSHADER_vkShader
{
    VkShaderModule shaderModule;
    const MOJOSHADER_parseData *parseData;
    uint32_t refcount;
} MOJOSHADER_vkShader;

typedef struct MOJOSHADER_vkUniformBuffer
{
    VkBuffer buffer;
    VkDeviceSize bufferSize;
    VkDeviceSize memoryOffset;
    VkDeviceSize dynamicOffset;
    VkDeviceSize currentBlockSize;
    int32_t full; // Records frame on which it became full, -1 if not full
} MOJOSHADER_vkUniformBuffer;

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

/* Max entries for each register file type */
#define MAX_REG_FILE_F 8192
#define MAX_REG_FILE_I 2047
#define MAX_REG_FILE_B 2047

typedef struct MOJOSHADER_vkContext
{
    VkInstance *instance;
    VkPhysicalDevice *physical_device;
    VkDevice *logical_device;
    PFN_vkGetInstanceProcAddr instance_proc_lookup;
    PFN_vkGetDeviceProcAddr device_proc_lookup;
    uint32_t graphics_queue_family_index;
    uint32_t maxUniformBufferRange;
    uint32_t minUniformBufferOffsetAlignment;

    int32_t frames_in_flight;

    MOJOSHADER_malloc malloc_fn;
    MOJOSHADER_free free_fn;
    void *malloc_data;

    // The constant register files...
    // !!! FIXME: Man, it kills me how much memory this takes...
    // !!! FIXME:  ... make this dynamically allocated on demand.
    float vs_reg_file_f[MAX_REG_FILE_F * 4];
    int32_t vs_reg_file_i[MAX_REG_FILE_I * 4];
    uint8_t vs_reg_file_b[MAX_REG_FILE_B * 4];
    float ps_reg_file_f[MAX_REG_FILE_F * 4];
    int32_t ps_reg_file_i[MAX_REG_FILE_I * 4];
    uint8_t ps_reg_file_b[MAX_REG_FILE_B * 4];

    VkDeviceMemory vertUboMemory;
    MOJOSHADER_vkUniformBuffer **vertUboBuffers;
    uint32_t vertUboCurrentIndex;

    VkDeviceMemory fragUboMemory;
    MOJOSHADER_vkUniformBuffer **fragUboBuffers;
    uint32_t fragUboCurrentIndex;

    uint32_t uboBufferCount;

    MOJOSHADER_vkShader *vertexShader;
    MOJOSHADER_vkShader *pixelShader;

    uint32_t currentFrame;

    #define VULKAN_INSTANCE_FUNCTION(ret, func, params) \
        vkfntype_MOJOSHADER_##func func;
    #define VULKAN_DEVICE_FUNCTION(ret, func, params) \
        vkfntype_MOJOSHADER_##func func;
    #include "mojoshader_vulkan_vkfuncs.h"
} MOJOSHADER_vkContext;

static MOJOSHADER_vkContext *ctx = NULL;

static uint8_t find_memory_type(
    MOJOSHADER_vkContext *ctx,
	uint32_t typeFilter,
	VkMemoryPropertyFlags properties,
	uint32_t *result
) {
	uint32_t i;
	VkPhysicalDeviceMemoryProperties memoryProperties;
	ctx->vkGetPhysicalDeviceMemoryProperties(*ctx->physical_device, &memoryProperties);

	for (i = 0; i < memoryProperties.memoryTypeCount; i++)
	{
		if ((typeFilter & (1 << i))
		 && (memoryProperties.memoryTypes[i].propertyFlags & properties) == properties)
		{
			*result = i;
			return 1;
		} // if
	} // for

	return 0;
} // find_memory_type

static uint32_t next_highest_offset_alignment(uint32_t offset)
{
    return (
        (offset + ctx->minUniformBufferOffsetAlignment - 1) /
        ctx->minUniformBufferOffsetAlignment *
        ctx->minUniformBufferOffsetAlignment
    );
} // next_highest_offset_alignment

static MOJOSHADER_vkUniformBuffer *create_ubo(MOJOSHADER_vkContext *ctx,
                                              MOJOSHADER_malloc m,
                                              void *d
) {
    MOJOSHADER_vkUniformBuffer *result = (MOJOSHADER_vkUniformBuffer *) m(
        sizeof(MOJOSHADER_vkUniformBuffer),
        d
    );
    VkBufferCreateInfo bufferCreateInfo =
    {
        VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
    };

    bufferCreateInfo.flags = 0;
    bufferCreateInfo.size = UBO_BUFFER_SIZE;
    bufferCreateInfo.usage = VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT;
    bufferCreateInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
    bufferCreateInfo.queueFamilyIndexCount = 1;
    bufferCreateInfo.pQueueFamilyIndices = &ctx->graphics_queue_family_index;

    ctx->vkCreateBuffer(
        *ctx->logical_device,
        &bufferCreateInfo,
        NULL,
        &result->buffer
    );

    result->bufferSize = UBO_BUFFER_SIZE;
    result->currentBlockSize = 0;
    result->dynamicOffset = 0;
    result->full = -1;

    return result;
} // create_ubo

static uint32_t uniform_data_size(MOJOSHADER_vkShader *shader)
{
    int32_t i;
    int32_t uniformSize;
    int32_t buflen = 0;
    for (i = 0; i < shader->parseData->uniform_count; i++)
    {
        const int32_t arrayCount = shader->parseData->uniforms[i].array_count;
        uniformSize = 16;
        if (shader->parseData->uniforms[i].type == MOJOSHADER_UNIFORM_BOOL)
            uniformSize = 1;
        buflen += (arrayCount ? arrayCount : 1) * uniformSize;
    } // for

    return buflen;
} // uniform_data_size

static VkBuffer get_uniform_buffer(MOJOSHADER_vkShader *shader)
{
    if (shader == NULL || shader->parseData->uniform_count == 0)
        return VK_NULL_HANDLE;

    if (shader->parseData->shader_type == MOJOSHADER_TYPE_VERTEX)
        return ctx->vertUboBuffers[ctx->vertUboCurrentIndex]->buffer;
    else
        return ctx->fragUboBuffers[ctx->fragUboCurrentIndex]->buffer;
} // get_uniform_buffer

static VkDeviceSize get_uniform_offset(MOJOSHADER_vkShader *shader)
{
    if (shader == NULL || shader->parseData->uniform_count == 0)
        return 0;

    if (shader->parseData->shader_type == MOJOSHADER_TYPE_VERTEX)
        return ctx->vertUboBuffers[ctx->vertUboCurrentIndex]->dynamicOffset;
    else
        return ctx->fragUboBuffers[ctx->fragUboCurrentIndex]->dynamicOffset;
} // get_uniform_offset

static VkDeviceSize get_uniform_size(MOJOSHADER_vkShader *shader)
{
    if (shader == NULL || shader->parseData->uniform_count == 0)
        return 0;

    if (shader->parseData->shader_type == MOJOSHADER_TYPE_VERTEX)
        return ctx->vertUboBuffers[ctx->vertUboCurrentIndex]->currentBlockSize;
    else
        return ctx->fragUboBuffers[ctx->fragUboCurrentIndex]->currentBlockSize;
} // get_uniform_size

static void update_uniform_buffer(MOJOSHADER_vkShader *shader)
{
    int32_t i;
    void *map;
    int32_t offset;
    uint8_t *contents;
    float *regF; int *regI; uint8_t *regB;
    MOJOSHADER_vkUniformBuffer *ubo;
    VkDeviceMemory uboMemory;

    if (shader == NULL || shader->parseData->uniform_count == 0)
        return;

    if (shader->parseData->shader_type == MOJOSHADER_TYPE_VERTEX)
    {
        regF = ctx->vs_reg_file_f;
        regI = ctx->vs_reg_file_i;
        regB = ctx->vs_reg_file_b;

        ubo = ctx->vertUboBuffers[ctx->vertUboCurrentIndex];
        uboMemory = ctx->vertUboMemory;
    } // if
    else
    {
        regF = ctx->ps_reg_file_f;
        regI = ctx->ps_reg_file_i;
        regB = ctx->ps_reg_file_b;

        ubo = ctx->fragUboBuffers[ctx->fragUboCurrentIndex];
        uboMemory = ctx->fragUboMemory;
    } // else

    ubo->dynamicOffset += ubo->currentBlockSize;

    ubo->currentBlockSize = next_highest_offset_alignment(uniform_data_size(shader));

    // Rotate buffer if it would overrun
    if (ubo->dynamicOffset + ubo->currentBlockSize >= ubo->bufferSize)
    {
        ubo->full = ctx->currentFrame;

        if (shader->parseData->shader_type == MOJOSHADER_TYPE_VERTEX)
        {
            for (i = 0; i < ctx->uboBufferCount; i++)
            {
                ctx->vertUboCurrentIndex = (ctx->vertUboCurrentIndex + 1) % ctx->uboBufferCount;
                if (ctx->vertUboBuffers[ctx->vertUboCurrentIndex]->full == -1)
                    break;
            } // for

            ubo = ctx->vertUboBuffers[ctx->vertUboCurrentIndex];
        }
        else
        {
            for (int i = 0; i < ctx->uboBufferCount; i++)
            {
                ctx->fragUboCurrentIndex = (ctx->fragUboCurrentIndex + 1) % ctx->uboBufferCount;
                if (ctx->fragUboBuffers[ctx->fragUboCurrentIndex]->full == -1)
                    break;
            } // for

            ubo = ctx->fragUboBuffers[ctx->fragUboCurrentIndex];
        } // else

        ubo->dynamicOffset = 0;
        ubo->currentBlockSize = next_highest_offset_alignment(uniform_data_size(shader));

        if (ubo->full >= 0)
            set_error("all UBO buffers are full");
    } // if

    ctx->vkMapMemory(
        *ctx->logical_device,
        uboMemory,
        ubo->memoryOffset,
        ubo->bufferSize,
        0,
        &map
    );

    contents = ((uint8_t *) map) + ubo->dynamicOffset;

    offset = 0;
    for (i = 0; i < shader->parseData->uniform_count; i++)
    {
        const int32_t index = shader->parseData->uniforms[i].index;
        const int32_t arrayCount = shader->parseData->uniforms[i].array_count;
        const int32_t size = arrayCount ? arrayCount : 1;

        switch (shader->parseData->uniforms[i].type)
        {
            case MOJOSHADER_UNIFORM_FLOAT:
                memcpy(
                    contents + (offset * 16),
                    &regF[4 * index],
                    size * 16
                );
                break;

            case MOJOSHADER_UNIFORM_INT:
                memcpy(
                    contents + (offset * 16),
                    &regI[4 * index],
                    size * 16
                );
                break;

            case MOJOSHADER_UNIFORM_BOOL:
                memcpy(
                    contents + offset,
                    &regB[index],
                    size
                );
                break;

            default:
                set_error(
                    "SOMETHING VERY WRONG HAPPENED WHEN UPDATING UNIFORMS"
                );
                assert(0);
                break;
        } // switch

        offset += size;
    } // for

    ctx->vkUnmapMemory(
        *ctx->logical_device,
        uboMemory
    );
} // update_uniform_buffer

static void lookup_entry_points(MOJOSHADER_vkContext *ctx)
{
    #define VULKAN_INSTANCE_FUNCTION(ret, func, params) \
        ctx->func = (vkfntype_MOJOSHADER_##func) ctx->instance_proc_lookup(*ctx->instance, #func);
    #define VULKAN_DEVICE_FUNCTION(ret, func, params) \
        ctx->func = (vkfntype_MOJOSHADER_##func) ctx->device_proc_lookup(*ctx->logical_device, #func);
    #include "mojoshader_vulkan_vkfuncs.h"
} // lookup_entry_points

static int shader_bytecode_len(MOJOSHADER_vkShader *shader)
{
    return shader->parseData->output_len - sizeof(SpirvPatchTable);
} // shader_bytecode_len

static void delete_shader(
    VkShaderModule shaderModule
) {
    ctx->vkDestroyShaderModule(
        *ctx->logical_device,
        shaderModule,
        NULL
    );
} // delete_shader

// Public API

MOJOSHADER_vkContext *MOJOSHADER_vkCreateContext(
    VkInstance *instance,
    VkPhysicalDevice *physical_device,
    VkDevice *logical_device,
    int frames_in_flight,
    PFN_MOJOSHADER_vkGetInstanceProcAddr instance_lookup,
    PFN_MOJOSHADER_vkGetDeviceProcAddr device_lookup,
    unsigned int graphics_queue_family_index,
    unsigned int max_uniform_buffer_range,
    unsigned int min_uniform_buffer_offset_alignment,
    MOJOSHADER_malloc m, MOJOSHADER_free f,
    void *malloc_d
) {
    int32_t i;
    int32_t uboMemoryOffset;
    VkMemoryAllocateInfo allocate_info =
    {
        VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
    };
    VkMemoryRequirements memoryRequirements;
    MOJOSHADER_vkContext* resultCtx;

    if (m == NULL) m = MOJOSHADER_internal_malloc;
    if (f == NULL) f = MOJOSHADER_internal_free;

    resultCtx = (MOJOSHADER_vkContext *) m(sizeof(MOJOSHADER_vkContext), malloc_d);
    if (resultCtx == NULL)
    {
        out_of_memory();
        goto init_fail;
    }

    memset(resultCtx, '\0', sizeof(MOJOSHADER_vkContext));
    resultCtx->malloc_fn = m;
    resultCtx->free_fn = f;
    resultCtx->malloc_data = malloc_d;

    resultCtx->instance = (VkInstance*) instance;
    resultCtx->physical_device = (VkPhysicalDevice*) physical_device;
    resultCtx->logical_device = (VkDevice*) logical_device;
    resultCtx->instance_proc_lookup = (PFN_vkGetInstanceProcAddr) instance_lookup;
    resultCtx->device_proc_lookup = (PFN_vkGetDeviceProcAddr) device_lookup;
    resultCtx->frames_in_flight = frames_in_flight;
    resultCtx->graphics_queue_family_index = graphics_queue_family_index;
    resultCtx->maxUniformBufferRange = max_uniform_buffer_range;
    resultCtx->minUniformBufferOffsetAlignment = min_uniform_buffer_offset_alignment;
    resultCtx->currentFrame = 0;

    lookup_entry_points(resultCtx);

    resultCtx->uboBufferCount = UBO_BUFFER_COUNT;

    // Allocate vert UBO

    resultCtx->vertUboCurrentIndex = 0;
    resultCtx->vertUboBuffers = (MOJOSHADER_vkUniformBuffer**) m(
        sizeof(MOJOSHADER_vkUniformBuffer*) * resultCtx->uboBufferCount,
        malloc_d
    );

    for (i = 0; i < resultCtx->uboBufferCount; i++)
        resultCtx->vertUboBuffers[i] = create_ubo(resultCtx, m, malloc_d);

    resultCtx->vkGetBufferMemoryRequirements(
        *resultCtx->logical_device,
        resultCtx->vertUboBuffers[0]->buffer,
        &memoryRequirements
    );

    allocate_info.allocationSize = UBO_BUFFER_SIZE * resultCtx->uboBufferCount;

    if (!find_memory_type(resultCtx,
                          memoryRequirements.memoryTypeBits,
                          VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
                          &allocate_info.memoryTypeIndex))
    {
        set_error("failed to find suitable memory type for UBO memory");
        return NULL;
    } // if

    resultCtx->vkAllocateMemory(
        *resultCtx->logical_device,
        &allocate_info,
        NULL,
        &resultCtx->vertUboMemory
    );

    uboMemoryOffset = 0;
    for (i = 0; i < resultCtx->uboBufferCount; i++)
    {
        resultCtx->vertUboBuffers[i]->memoryOffset = uboMemoryOffset;

        resultCtx->vkBindBufferMemory(
            *resultCtx->logical_device,
            resultCtx->vertUboBuffers[i]->buffer,
            resultCtx->vertUboMemory,
            uboMemoryOffset
        );

        uboMemoryOffset += UBO_BUFFER_SIZE;
    } // for

    // Allocate frag UBO

    resultCtx->fragUboCurrentIndex = 0;
    resultCtx->fragUboBuffers = (MOJOSHADER_vkUniformBuffer**) m(
        sizeof(MOJOSHADER_vkUniformBuffer*) * resultCtx->uboBufferCount,
        malloc_d
    );

    for (i = 0; i < resultCtx->uboBufferCount; i++)
        resultCtx->fragUboBuffers[i] = create_ubo(resultCtx, m, malloc_d);

    resultCtx->vkGetBufferMemoryRequirements(
        *resultCtx->logical_device,
        resultCtx->fragUboBuffers[0]->buffer,
        &memoryRequirements
    );

    allocate_info.allocationSize = UBO_BUFFER_SIZE * resultCtx->uboBufferCount;

    if (!find_memory_type(resultCtx,
                          memoryRequirements.memoryTypeBits,
                          VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
                          &allocate_info.memoryTypeIndex))
    {
        set_error("failed to find suitable memory type for UBO memory");
        return NULL;
    } // if

    resultCtx->vkAllocateMemory(
        *resultCtx->logical_device,
        &allocate_info,
        NULL,
        &resultCtx->fragUboMemory
    );

    uboMemoryOffset = 0;
    for (i = 0; i < resultCtx->uboBufferCount; i++)
    {
        resultCtx->fragUboBuffers[i]->memoryOffset = uboMemoryOffset;

        resultCtx->vkBindBufferMemory(
            *resultCtx->logical_device,
            resultCtx->fragUboBuffers[i]->buffer,
            resultCtx->fragUboMemory,
            uboMemoryOffset
        );

        uboMemoryOffset += UBO_BUFFER_SIZE;
    } // for

    return resultCtx;

init_fail:
    if (resultCtx != NULL)
        f(resultCtx, malloc_d);
    return NULL;
} // MOJOSHADER_vkCreateContext

void MOJOSHADER_vkMakeContextCurrent(MOJOSHADER_vkContext *_ctx)
{
    ctx = _ctx;
} // MOJOSHADER_vkMakeContextCurrent

void MOJOSHADER_vkDestroyContext()
{
    int32_t i;
    for (i = 0; i < ctx->uboBufferCount; i++)
    {
        ctx->vkDestroyBuffer(
            *ctx->logical_device,
            ctx->vertUboBuffers[i]->buffer,
            NULL
        );

        ctx->free_fn(ctx->vertUboBuffers[i], ctx->malloc_data);

        ctx->vkDestroyBuffer(
            *ctx->logical_device,
            ctx->fragUboBuffers[i]->buffer,
            NULL
        );

        ctx->free_fn(ctx->fragUboBuffers[i], ctx->malloc_data);
    } // for

    ctx->free_fn(ctx->vertUboBuffers, ctx->malloc_data);
    ctx->free_fn(ctx->fragUboBuffers, ctx->malloc_data);

    ctx->vkFreeMemory(
        *ctx->logical_device,
        ctx->vertUboMemory,
        NULL
    );

    ctx->vkFreeMemory(
        *ctx->logical_device,
        ctx->fragUboMemory,
        NULL
    );

    ctx->free_fn(ctx, ctx->malloc_data);
} // MOJOSHADER_vkDestroyContext

MOJOSHADER_vkShader *MOJOSHADER_vkCompileShader(
    const char *mainfn,
    const unsigned char *tokenbuf,
    const unsigned int bufsize,
    const MOJOSHADER_swizzle *swiz,
    const unsigned int swizcount,
    const MOJOSHADER_samplerMap *smap,
    const unsigned int smapcount
) {
    VkResult result;
    VkShaderModule shaderModule;
    VkShaderModuleCreateInfo shaderModuleCreateInfo =
    {
        VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
    };
    MOJOSHADER_vkShader *shader;

    const MOJOSHADER_parseData *pd = MOJOSHADER_parse(
        "spirv", mainfn,
        tokenbuf, bufsize,
        swiz, swizcount,
        smap, smapcount,
        ctx->malloc_fn,
        ctx->free_fn,
        ctx->malloc_data
    );

    if (pd->error_count > 0)
    {
        set_error(pd->errors[0].error);
        goto compile_shader_fail;
    } // if

    shader = (MOJOSHADER_vkShader *) ctx->malloc_fn(sizeof(MOJOSHADER_vkShader), ctx->malloc_data);
    if (shader == NULL)
    {
        out_of_memory();
        goto compile_shader_fail;
    } // if

    shader->parseData = pd;
    shader->refcount = 1;

    shaderModuleCreateInfo.flags = 0;
    shaderModuleCreateInfo.codeSize = shader_bytecode_len(shader);
    shaderModuleCreateInfo.pCode = (uint32_t*) pd->output;

    result = ctx->vkCreateShaderModule(
        *ctx->logical_device,
        &shaderModuleCreateInfo,
        NULL,
        &shader->shaderModule
    );

    if (result != VK_SUCCESS)
    {
        // FIXME: should display VK error code
        set_error("Error when creating VkShaderModule");
        goto compile_shader_fail;
    } // if

    return shader;

compile_shader_fail:
    MOJOSHADER_freeParseData(pd);
    if (shader != NULL)
    {
        delete_shader(shader->shaderModule);
        ctx->free_fn(shader, ctx->malloc_data);
    } // if
    return NULL;

} // MOJOSHADER_vkMakeContextCurrent

void MOJOSHADER_vkShaderAddRef(MOJOSHADER_vkShader *shader)
{
    if (shader != NULL)
        shader->refcount++;
} // MOJOShader_vkShaderAddRef

void MOJOSHADER_vkDeleteShader(MOJOSHADER_vkShader *shader)
{
    if (shader != NULL)
    {
        if (shader->refcount > 1)
            shader->refcount--;
        else
        {
            delete_shader(shader->shaderModule);
            MOJOSHADER_freeParseData(shader->parseData);
            ctx->free_fn(shader, ctx->malloc_data);
        } // else
    } // if
} // MOJOSHADER_vkDeleteShader

const MOJOSHADER_parseData *MOJOSHADER_vkGetShaderParseData(
    MOJOSHADER_vkShader *shader
) {
    return (shader != NULL) ? shader->parseData : NULL;
} // MOJOSHADER_vkGetShaderParseData

void MOJOSHADER_vkBindShaders(MOJOSHADER_vkShader *vshader,
                              MOJOSHADER_vkShader *pshader)
{
    /* NOOP if shader is null */

    if (vshader != NULL)
        ctx->vertexShader = vshader;
    if (pshader != NULL)
        ctx->pixelShader = pshader;
} // MOJOSHADER_vkBindShaders

void MOJOSHADER_vkGetBoundShaders(MOJOSHADER_vkShader **vshader,
                                  MOJOSHADER_vkShader **pshader)
{
    *vshader = ctx->vertexShader;
    *pshader = ctx->pixelShader;
} // MOJOSHADER_vkGetBoundShaders

void MOJOSHADER_vkMapUniformBufferMemory(float **vsf, int **vsi, unsigned char **vsb,
                                         float **psf, int **psi, unsigned char **psb)
{
    *vsf = ctx->vs_reg_file_f;
    *vsi = ctx->vs_reg_file_i;
    *vsb = ctx->vs_reg_file_b;
    *psf = ctx->ps_reg_file_f;
    *psi = ctx->ps_reg_file_i;
    *psb = ctx->ps_reg_file_b;
} // MOJOSHADER_vkMapUniformBufferMemory

void MOJOSHADER_vkUnmapUniformBufferMemory()
{
    /* Why is this function named unmap instead of update?
     * the world may never know...
     */

    update_uniform_buffer(ctx->vertexShader);
    update_uniform_buffer(ctx->pixelShader);
} // MOJOSHADER_vkUnmapUniformBufferMemory

void MOJOSHADER_vkGetUniformBuffers(VkBuffer *vbuf, unsigned long long *voff, unsigned long long *vsize,
                                    VkBuffer *pbuf, unsigned long long *poff, unsigned long long *psize)
{
    *vbuf = get_uniform_buffer(ctx->vertexShader);
    *voff = get_uniform_offset(ctx->vertexShader);
    *vsize = get_uniform_size(ctx->vertexShader);
    *pbuf = get_uniform_buffer(ctx->pixelShader);
    *poff = get_uniform_offset(ctx->pixelShader);
    *psize = get_uniform_size(ctx->pixelShader);
} // MOJOSHADER_vkGetUniformBuffers

void MOJOSHADER_vkEndFrame()
{
    int32_t i;
    ctx->currentFrame = (ctx->currentFrame + 1) % ctx->frames_in_flight;
    for (i = 0; i < ctx->uboBufferCount; i++)
    {
        if (ctx->vertUboBuffers[i]->full == ctx->currentFrame)
        {
            ctx->vertUboBuffers[i]->dynamicOffset = 0;
            ctx->vertUboBuffers[i]->currentBlockSize = 0;
            ctx->vertUboBuffers[i]->full = -1;
        } // if

        if (ctx->fragUboBuffers[i]->full == ctx->currentFrame)
        {
            ctx->fragUboBuffers[i]->dynamicOffset = 0;
            ctx->fragUboBuffers[i]->currentBlockSize = 0;
            ctx->fragUboBuffers[i]->full = -1;
        } // if
    } // for
} // MOJOSHADER_VkEndFrame

int MOJOSHADER_vkGetVertexAttribLocation(MOJOSHADER_vkShader *vert,
                                         MOJOSHADER_usage usage, int index)
{
    int32_t i;
    if (vert == NULL)
        return -1;

    for (i = 0; i < vert->parseData->attribute_count; i++)
    {
        if (vert->parseData->attributes[i].usage == usage &&
            vert->parseData->attributes[i].index == index)
        {
            return i;
        } // if
    } // for

    // failure
    return -1;
} //MOJOSHADER_vkGetVertexAttribLocation

unsigned long long MOJOSHADER_vkGetShaderModule(MOJOSHADER_vkShader *shader)
{
    if (shader == NULL)
        return 0;

    return (unsigned long long) shader->shaderModule;
} //MOJOSHADER_vkGetShaderModule

const char *MOJOSHADER_vkGetError(void)
{
    return error_buffer;
} // MOJOSHADER_vkGetError

#endif /* SUPPORT_PROFILE_SPIRV */

// end of mojoshader_vulkan.c ...
