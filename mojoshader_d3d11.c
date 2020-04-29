/**
 * MojoShader; generate shader programs from bytecode of compiled
 *  Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

#ifdef _WIN32
#define D3D11_NO_HELPERS
#define CINTERFACE
#define COBJMACROS
#include <d3d11.h>
#endif

#define __MOJOSHADER_INTERNAL__ 1
#include "mojoshader_internal.h"

typedef struct d3d11ShaderMap
{
    void *vshader; // ID3D11VertexShader*
    void *pshader; // ID3D11PixelShader*
} d3d11ShaderMap;

typedef struct MOJOSHADER_d3d11Shader
{
    const MOJOSHADER_parseData *parseData;
    void *ubo; // ID3DBuffer*
    uint32 refcount;
    union
    {
        struct
        {
            void *dataBlob; // ID3DBlob*
            void *shader; // ID3D11VertexShader*
        } vertex;

        struct
        {
            d3d11ShaderMap *shaderMaps;
            unsigned int mapCapacity;
            unsigned int numMaps;
        } pixel;
    };
} MOJOSHADER_d3d11Shader;

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

#if SUPPORT_PROFILE_HLSL
#ifdef MOJOSHADER_EFFECT_SUPPORT

typedef HRESULT(WINAPI *PFN_D3DCOMPILE)(
    LPCVOID pSrcData,
    SIZE_T SrcDataSize,
    LPCSTR pSourceName,
    const D3D_SHADER_MACRO *pDefines,
    ID3DInclude *pInclude,
    LPCSTR pEntrypoint,
    LPCSTR pTarget,
    UINT Flags1,
    UINT Flags2,
    ID3DBlob **ppCode,
    ID3DBlob **ppErrorMsgs
);

/* Structs */

// Max entries for each register file type...
#define MAX_REG_FILE_F 8192
#define MAX_REG_FILE_I 2047
#define MAX_REG_FILE_B 2047

typedef struct MOJOSHADER_d3d11Context
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

    // Pointer to the active ID3D11Device.
    ID3D11Device *device;

    // Pointer to the ID3D11DeviceContext.
    ID3D11DeviceContext *deviceContext;

    // Currently bound vertex and pixel shaders.
    MOJOSHADER_d3d11Shader *vertexShader;
    MOJOSHADER_d3d11Shader *pixelShader;

    // D3DCompile function pointer.
    PFN_D3DCOMPILE D3DCompileFunc;
} MOJOSHADER_d3d11Context;

static MOJOSHADER_d3d11Context *ctx = NULL;

/* Uniform buffer utilities */

static inline int next_highest_alignment(int n)
{
    const int align = 16;
    return align * ((n + align - 1) / align);
} // next_highest_alignment

static inline void *get_uniform_buffer(MOJOSHADER_d3d11Shader *shader)
{
    return (shader == NULL || shader->ubo == NULL) ? NULL : shader->ubo;
} // get_uniform_buffer

static void update_uniform_buffer(MOJOSHADER_d3d11Shader *shader)
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

    // Map the buffer
    D3D11_MAPPED_SUBRESOURCE res;
    ID3D11DeviceContext_Map(
        (ID3D11DeviceContext*) ctx->deviceContext,
        (ID3D11Resource*) shader->ubo,
        0,
        D3D11_MAP_WRITE_DISCARD,
        0,
        &res
    );

    // Update the buffer contents
    unsigned char *pData = (unsigned char*) res.pData;
    size_t offset = 0;
    for (int i = 0; i < shader->parseData->uniform_count; i++)
    {
        int idx = shader->parseData->uniforms[i].index;
        int arrayCount = shader->parseData->uniforms[i].array_count;
        size_t size = arrayCount ? arrayCount : 1;

        switch (shader->parseData->uniforms[i].type)
        {
            case MOJOSHADER_UNIFORM_FLOAT:
                memcpy(
                    pData + (offset * 16),
                    &regF[4 * idx],
                    size * 16
                );
                break;

            case MOJOSHADER_UNIFORM_INT:
                // !!! FIXME: Need a test case
                memcpy(
                    pData + (offset * 16),
                    &regI[4 * idx],
                    size * 16
                );
                break;

            case MOJOSHADER_UNIFORM_BOOL:
                // !!! FIXME: Need a test case
                memcpy(
                    pData + offset,
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

    // Unmap the buffer
    ID3D11DeviceContext_Unmap(
        (ID3D11DeviceContext*) ctx->deviceContext,
        (ID3D11Resource*) shader->ubo,
        0
    );
} // update_uniform_buffer

/* Pixel Shader Compilation Utility */

static ID3D11PixelShader *compilePixelShader(MOJOSHADER_d3d11Shader *vshader,
                                             MOJOSHADER_d3d11Shader *pshader)
{
    ID3D11PixelShader *retval = NULL;
    ID3DBlob *blob;
    const char *a, *b, *vout, *pstart, *pend, *loc;
    char *pfinal;
    size_t substr_len;
    HRESULT result;

    const char *vsrc = vshader->parseData->output;
    const char *psrc = pshader->parseData->output;

    #define MAKE_STRBUF(buf) \
        substr_len = b - a; \
        buf = (const char *) ctx->malloc_fn(substr_len + 1, ctx->malloc_data); \
        memset((void *) buf, '\0', substr_len + 1); \
        memcpy((void *) buf, a, substr_len);

    // Copy the contents of the vertex function's output struct into a buffer
    a = strstr(vsrc, "_Output\n{\n") + strlen("_Output\n{\n");
    b = a;
    while (*(b++) != '}') {}
    MAKE_STRBUF(vout)

    // Split up the pixel shader text...

    // (1) Everything up to the input contents
    a = psrc;
    b = strstr(psrc, "_Input\n{\n") + strlen("_Input\n{\n");
    MAKE_STRBUF(pstart)

    // (2) Everything after the input contents
    a = b;
    while (*(a++) != '}') {}
    a--;
    while (*(b++) != '\0') {}
    MAKE_STRBUF(pend)

    // Modify the copied vertex output so it works as pixel input
    while ((loc = strstr(vout, "m_oD")) != NULL)
        memcpy((void*) loc, " m_v", strlen(" m_v"));
    while ((loc = strstr(vout, "m_oT")) != NULL)
        memcpy((void*) loc, " m_t", strlen(" m_t"));
    // !!! FIXME: Others?

    // Concatenate the pieces together again
    pfinal = (char *) ctx->malloc_fn(substr_len + 1, ctx->malloc_data);
    memset((void *) pfinal, '\0', substr_len + 1);
    strcat(pfinal, pstart);
    strcat(pfinal, vout);
    strcat(pfinal, pend);

    // Compile into bytecode
    result = ctx->D3DCompileFunc(
        pfinal,
        strlen(pfinal),
        pshader->parseData->mainfn,
        NULL,
        NULL,
        pshader->parseData->mainfn,
        "ps_4_0",
        0,
        0,
        &blob,
        &blob
    );
    if (result < 0)
    {
        set_error((const char *) ID3D10Blob_GetBufferPointer(blob));
        ctx->free_fn((void *) vout, ctx->malloc_data);
        ctx->free_fn((void *) pstart, ctx->malloc_data);
        ctx->free_fn((void *) pend, ctx->malloc_data);
        ctx->free_fn((void *) pfinal, ctx->malloc_data);
        return NULL;
    } // if

    // Clean up
    ctx->free_fn((void *) vout, ctx->malloc_data);
    ctx->free_fn((void *) pstart, ctx->malloc_data);
    ctx->free_fn((void *) pend, ctx->malloc_data);
    ctx->free_fn((void *) pfinal, ctx->malloc_data);

    // Finally, create and return the shader
    ID3D11Device_CreatePixelShader(
        ctx->device,
        ID3D10Blob_GetBufferPointer(blob),
        ID3D10Blob_GetBufferSize(blob),
        NULL,
        &retval
    );
    return retval;
} // compilePixelShader

/* Public API */

int MOJOSHADER_d3d11CreateContext(void *device, void *deviceContext,
                                  MOJOSHADER_malloc m, MOJOSHADER_free f,
                                  void *malloc_d)
{
    assert(ctx == NULL);

    if (m == NULL) m = MOJOSHADER_internal_malloc;
    if (f == NULL) f = MOJOSHADER_internal_free;

    ctx = (MOJOSHADER_d3d11Context *) m(sizeof(MOJOSHADER_d3d11Context), malloc_d);
    if (ctx == NULL)
    {
        out_of_memory();
        goto init_fail;
    } // if

    memset(ctx, '\0', sizeof (MOJOSHADER_d3d11Context));
    ctx->malloc_fn = m;
    ctx->free_fn = f;
    ctx->malloc_data = malloc_d;

    // Store references to the D3D device and immediate context
    ctx->device = (ID3D11Device*) device;
    ctx->deviceContext = (ID3D11DeviceContext*) deviceContext;

    // Grab the D3DCompile function pointer
    HMODULE d3dCompilerModule = LoadLibrary("d3dcompiler_47.dll");
    assert(d3dCompilerModule != NULL);
    ctx->D3DCompileFunc = (PFN_D3DCOMPILE) GetProcAddress(d3dCompilerModule, "D3DCompile");

    return 0;

init_fail:
    if (ctx != NULL)
        f(ctx, malloc_d);
    return -1;
} // MOJOSHADER_d3d11CreateContext

void MOJOSHADER_d3d11DestroyContext(void)
{
    // !!! FIXME: What else?
    ctx->free_fn(ctx, ctx->malloc_data);
} // MOJOSHADER_d3d11DestroyContext

MOJOSHADER_d3d11Shader *MOJOSHADER_d3d11CompileShader(const char *mainfn,
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
    int i;

    const MOJOSHADER_parseData *pd = MOJOSHADER_parse("hlsl", mainfn, tokenbuf,
                                                     bufsize, swiz, swizcount,
                                                     smap, smapcount, m, f, d);
    if (pd->error_count > 0)
    {
        // !!! FIXME: put multiple errors in the buffer? Don't use
        // !!! FIXME:  MOJOSHADER_d3d11GetError() for this?
        set_error(pd->errors[0].error);
        goto compile_shader_fail;
    } // if

    MOJOSHADER_d3d11Shader *retval = (MOJOSHADER_d3d11Shader *) m(sizeof(MOJOSHADER_d3d11Shader), d);
    if (retval == NULL)
        goto compile_shader_fail;

    retval->parseData = pd;
    retval->ubo = NULL;
    retval->refcount = 1;

    int isvert = (pd->shader_type == MOJOSHADER_TYPE_VERTEX);
    if (isvert)
    {
        retval->vertex.dataBlob = NULL;
        retval->vertex.shader = NULL;
    } // if
    else
    {
        retval->pixel.shaderMaps = NULL;
        retval->pixel.mapCapacity = 0;
        retval->pixel.numMaps = 0;
    } // else

    /* Only vertex shaders and pixel shaders without input get compiled here.
     * The rest of the pixel shaders will be compiled at link time since they
     * depend on the vertex shader output layout.
     */
    if (isvert || pd->attribute_count == 0)
    {
        // Compile the shader
        ID3DBlob *blob;
        HRESULT result = ctx->D3DCompileFunc(
            pd->output,
            pd->output_len,
            pd->mainfn,
            NULL,
            NULL,
            pd->mainfn,
            isvert ? "vs_4_0" : "ps_4_0",
            0, /* Replace this with 1 if you want HLSL debug info! */
            0,
            (ID3DBlob **) &blob,
            &blob
        );
        if (result < 0)
        {
            set_error((const char *) ID3D10Blob_GetBufferPointer(blob));
            goto compile_shader_fail;
        } // if

        if (isvert)
        {
            retval->vertex.dataBlob = blob;
            ID3D11Device_CreateVertexShader(
                ctx->device,
                ID3D10Blob_GetBufferPointer((ID3DBlob *) retval->vertex.dataBlob),
                ID3D10Blob_GetBufferSize((ID3DBlob *) retval->vertex.dataBlob),
                NULL,
                (ID3D11VertexShader **) &retval->vertex.shader
            );
        } // if
        else
        {
            retval->pixel.shaderMaps = (d3d11ShaderMap *) m(sizeof(d3d11ShaderMap), d);
            if (retval->pixel.shaderMaps == NULL)
                goto compile_shader_fail;

            retval->pixel.mapCapacity = 1;
            retval->pixel.numMaps = 1;
            retval->pixel.shaderMaps[0].vshader = NULL;
            ID3D11Device_CreatePixelShader(
                ctx->device,
                ID3D10Blob_GetBufferPointer(blob),
                ID3D10Blob_GetBufferSize(blob),
                NULL,
                (ID3D11PixelShader **) &retval->pixel.shaderMaps[0].pshader
            );
            // !!! FIXME: Do we need to release the blob?
        } // else
    } // if
    else if (!isvert)
    {
        const int mapCount = 4; // arbitrary!
        retval->pixel.shaderMaps = (d3d11ShaderMap *) m(mapCount * sizeof(d3d11ShaderMap), d);
        if (retval->pixel.shaderMaps == NULL)
            goto compile_shader_fail;

        retval->pixel.mapCapacity = mapCount;
        for (i = 0; i < mapCount; i++)
        {
            retval->pixel.shaderMaps[i].vshader = NULL;
            retval->pixel.shaderMaps[i].pshader = NULL;
        } // for
    } // else if

    // Create the uniform buffer, if needed
    if (pd->uniform_count > 0)
    {
        // Calculate how big we need to make the buffer
        int buflen = 0;
        for (i = 0; i < pd->uniform_count; i++)
        {
            int arrayCount = pd->uniforms[i].array_count;
            int uniformSize = 16;
            if (pd->uniforms[i].type == MOJOSHADER_UNIFORM_BOOL)
                uniformSize = 1;
            buflen += (arrayCount ? arrayCount : 1) * uniformSize;
        } // for

        D3D11_BUFFER_DESC bdesc;
        bdesc.ByteWidth = next_highest_alignment(buflen);
        bdesc.Usage = D3D11_USAGE_DYNAMIC;
        bdesc.BindFlags = D3D11_BIND_CONSTANT_BUFFER;
        bdesc.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;
        bdesc.MiscFlags = 0;
        bdesc.StructureByteStride = 0;
        ID3D11Device_CreateBuffer(
            (ID3D11Device*) ctx->device,
            &bdesc,
            NULL,
            (ID3D11Buffer**) &retval->ubo
        );
    } // if

    return retval;

compile_shader_fail:
    MOJOSHADER_freeParseData(pd);
    f(retval, d);
    return NULL;
} // MOJOSHADER_d3d11CompileShader

void MOJOSHADER_d3d11ShaderAddRef(MOJOSHADER_d3d11Shader *shader)
{
    if (shader != NULL)
        shader->refcount++;
} // MOJOSHADER_d3d11ShaderAddRef

void MOJOSHADER_d3d11DeleteShader(MOJOSHADER_d3d11Shader *shader)
{
    if (shader != NULL)
    {
        if (shader->refcount > 1)
            shader->refcount--;
        else
        {
            if (shader->ubo != NULL)
                ID3D11Buffer_Release((ID3D11Buffer*) shader->ubo);

            if (shader->parseData->shader_type == MOJOSHADER_TYPE_VERTEX)
            {
                ID3D10Blob_Release((ID3DBlob *) shader->vertex.dataBlob);
                ID3D11VertexShader_Release((ID3D11VertexShader *) shader->vertex.shader);
            } // if
            else if (shader->parseData->shader_type == MOJOSHADER_TYPE_PIXEL)
            {
                for (int i = 0; i < shader->pixel.numMaps; i++)
                    ID3D11PixelShader_Release((ID3D11PixelShader *) shader->pixel.shaderMaps[i].pshader);

                // !!! FIXME: When do we need to free blobs?

                ctx->free_fn(shader->pixel.shaderMaps, ctx->malloc_data);
                shader->pixel.shaderMaps = NULL;
            } // else if

            MOJOSHADER_freeParseData(shader->parseData);
            ctx->free_fn(shader, ctx->malloc_data);
        } // else
    } // if
} // MOJOSHADER_d3d11DeleteShader

const MOJOSHADER_parseData *MOJOSHADER_d3d11GetShaderParseData(
                                                MOJOSHADER_d3d11Shader *shader)
{
    return (shader != NULL) ? shader->parseData : NULL;
} // MOJOSHADER_d3d11GetParseData

void MOJOSHADER_d3d11BindShaders(MOJOSHADER_d3d11Shader *vshader,
                                 MOJOSHADER_d3d11Shader *pshader)
{
    // Use the last bound shader in case of NULL

    if (pshader != NULL)
    {
        ctx->pixelShader = pshader;

        ID3D11PixelShader *realPS = NULL;
        if (vshader != ctx->vertexShader)
        {
            int noInputs = (pshader->pixel.numMaps == 1 &&
                pshader->pixel.shaderMaps[0].vshader == NULL);

            // Is there already a mapping for this vertex shader?
            for (int i = 0; i < pshader->pixel.numMaps; i++)
            {
                if (pshader->pixel.shaderMaps[i].vshader == vshader || noInputs)
                {
                    realPS = (ID3D11PixelShader*) pshader->pixel.shaderMaps[i].pshader;
                    break;
                } // if
            } // for

            if (realPS == NULL)
            {
                // We have to create a new vertex/pixel shader mapping...

                // Expand the mapping array if needed
                if (pshader->pixel.numMaps == pshader->pixel.mapCapacity)
                {
                    d3d11ShaderMap *newMap = (d3d11ShaderMap *) ctx->malloc_fn(
                        sizeof(d3d11ShaderMap) * pshader->pixel.mapCapacity * 2,
                        ctx->malloc_data);

                    memcpy(newMap, pshader->pixel.shaderMaps,
                        sizeof(d3d11ShaderMap) * pshader->pixel.mapCapacity);

                    pshader->pixel.mapCapacity *= 2;
                    ctx->free_fn(pshader->pixel.shaderMaps, ctx->malloc_data);
                    pshader->pixel.shaderMaps = newMap;
                    newMap = NULL;
                } // if

                // Add the new mapping
                pshader->pixel.numMaps++;
                pshader->pixel.shaderMaps[pshader->pixel.numMaps].vshader = vshader;
                realPS = compilePixelShader(vshader, pshader);
                pshader->pixel.shaderMaps[pshader->pixel.numMaps].pshader = realPS;
            } // if
        } // if

        ID3D11DeviceContext_PSSetShader(
            ctx->deviceContext,
            realPS,
            NULL,
            0
        );
        ID3D11DeviceContext_PSSetConstantBuffers(
            ctx->deviceContext,
            0,
            1,
            (ID3D11Buffer **) &pshader->ubo
        );
    } // if

    if (vshader != NULL)
    {
        ctx->vertexShader = vshader;
        ID3D11DeviceContext_VSSetShader(
            ctx->deviceContext,
            (ID3D11VertexShader*) vshader->vertex.shader,
            NULL,
            0
        );
        ID3D11DeviceContext_VSSetConstantBuffers(
            ctx->deviceContext,
            0,
            1,
            (ID3D11Buffer**) &vshader->ubo
        );
    } // if
} // MOJOSHADER_d3d11BindShaders

void MOJOSHADER_d3d11GetBoundShaders(MOJOSHADER_d3d11Shader **vshader,
                                     MOJOSHADER_d3d11Shader **pshader)
{
    *vshader = ctx->vertexShader;
    *pshader = ctx->pixelShader;
} // MOJOSHADER_d3d11GetBoundShaders

void MOJOSHADER_d3d11MapUniformBufferMemory(float **vsf, int **vsi, unsigned char **vsb,
                                            float **psf, int **psi, unsigned char **psb)
{
    *vsf = ctx->vs_reg_file_f;
    *vsi = ctx->vs_reg_file_i;
    *vsb = ctx->vs_reg_file_b;
    *psf = ctx->ps_reg_file_f;
    *psi = ctx->ps_reg_file_i;
    *psb = ctx->ps_reg_file_b;
} // MOJOSHADER_d3d11MapUniformBufferMemory

void MOJOSHADER_d3d11UnmapUniformBufferMemory()
{
    /* This has nothing to do with unmapping memory
     * and everything to do with updating uniform
     * buffers with the latest parameter contents.
     */
    MOJOSHADER_d3d11Shader *vs, *ps;
    MOJOSHADER_d3d11GetBoundShaders(&vs, &ps);
    update_uniform_buffer(vs);
    update_uniform_buffer(ps);
} // MOJOSHADER_d3d11UnmapUniformBufferMemory

int MOJOSHADER_d3d11GetVertexAttribLocation(MOJOSHADER_d3d11Shader *vert,
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
} // MOJOSHADER_d3d11GetVertexAttribLocation

const void *MOJOSHADER_d3d11GetBytecode(MOJOSHADER_d3d11Shader *vshader)
{
	return ID3D10Blob_GetBufferPointer((ID3DBlob*) vshader->vertex.dataBlob);
} // MOJOSHADER_d3d11GetShaderBytecode

int MOJOSHADER_d3d11GetBytecodeLength(MOJOSHADER_d3d11Shader *vshader)
{
	return ID3D10Blob_GetBufferSize((ID3DBlob*) vshader->vertex.dataBlob);
} // MOJOSHADER_d3d11GetShaderBytecodeLength

const char *MOJOSHADER_d3d11GetError(void)
{
    return error_buffer;
} // MOJOSHADER_d3d11GetError

#endif /* MOJOSHADER_EFFECT_SUPPORT */
#endif /* SUPPORT_PROFILE_HLSL */

// end of mojoshader_d3d11.c ...
