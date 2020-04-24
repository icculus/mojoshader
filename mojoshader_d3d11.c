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

typedef struct MOJOSHADER_d3d11Shader
{
    const MOJOSHADER_parseData *parseData;
    void *ubo; // ID3DBuffer*
    uint32 refcount;
    void *dataBlob; // ID3DBlob*
    void *shader; // ID3D11VertexShader* or ID3D11PixelShader*
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
        D3D11_MAP_WRITE,
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
    HMODULE d3dCompilerModule = LoadLibrary("d3dcompiler.dll");
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
    retval->dataBlob = NULL;
    retval->shader = NULL;

    // Vertex and pixel shaders are handled differently
    int isvert = (pd->shader_type == MOJOSHADER_TYPE_VERTEX);

    // Compile the shader
    ID3DBlob *errorBlob;
    HRESULT result = ctx->D3DCompileFunc(
        pd->output,
        pd->output_len,
        pd->mainfn,
        NULL,
        NULL,
        pd->mainfn,
        isvert ? "vs_4_0" : "ps_4_0",
        0,
        0,
        (ID3DBlob**) &retval->dataBlob,
        &errorBlob
    );
    if (result < 0)
    {
        set_error((const char*) ID3D10Blob_GetBufferPointer(errorBlob));
        goto compile_shader_fail;
    } // if

    if (isvert)
    {
        ID3D11Device_CreateVertexShader(
            ctx->device,
            ID3D10Blob_GetBufferPointer((ID3DBlob*) retval->dataBlob),
            ID3D10Blob_GetBufferSize((ID3DBlob*) retval->dataBlob),
            NULL,
            (ID3D11VertexShader**) &retval->shader
        );
    } // if
    else
    {
        ID3D11Device_CreatePixelShader(
            ctx->device,
            ID3D10Blob_GetBufferPointer((ID3DBlob*) retval->dataBlob),
            ID3D10Blob_GetBufferSize((ID3DBlob*) retval->dataBlob),
            NULL,
            (ID3D11PixelShader**) &retval->shader
        );
    } // else

    // Create the uniform buffer, if needed
    if (pd->uniform_count > 0)
    {
        // Calculate how big we need to make the buffer
        int buflen = 0;
        for (int i = 0; i < pd->uniform_count; i++)
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
            ID3D11Buffer_Release((ID3D11Buffer*) shader->ubo);
            ID3D10Blob_Release((ID3DBlob*) shader->dataBlob);
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
    // Use the last bound shaders in case of NULL
    if (vshader != NULL)
    {
        ctx->vertexShader = vshader;
        ID3D11DeviceContext_VSSetShader(
            ctx->deviceContext,
            (ID3D11VertexShader*) vshader->shader,
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
    if (pshader != NULL)
    {
        ctx->pixelShader = pshader;
        ID3D11DeviceContext_PSSetShader(
            ctx->deviceContext,
            (ID3D11PixelShader*) pshader->shader,
            NULL,
            0
        );
        ID3D11DeviceContext_PSSetConstantBuffers(
            ctx->deviceContext,
            0,
            1,
            (ID3D11Buffer**) &pshader->ubo
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

const char *MOJOSHADER_d3d11GetError(void)
{
    return error_buffer;
} // MOJOSHADER_d3d11GetError

#endif /* MOJOSHADER_EFFECT_SUPPORT */
#endif /* SUPPORT_PROFILE_HLSL */

// end of mojoshader_d3d11.c ...
