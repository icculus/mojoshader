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
    uint32 refcount;

    void *ubo; // ID3DBuffer*
    size_t buflen;
    unsigned char *constantData;

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

    // Update the buffer contents
    int needsUpdate = 0;
    size_t offset = 0;
    for (int i = 0; i < shader->parseData->uniform_count; i++)
    {
        int idx = shader->parseData->uniforms[i].index;
        int arrayCount = shader->parseData->uniforms[i].array_count;

        void *src, *dst;
        size_t size = arrayCount ? arrayCount : 1;

        switch (shader->parseData->uniforms[i].type)
        {
            case MOJOSHADER_UNIFORM_FLOAT:
                src = &regF[4 * idx];
                dst = shader->constantData + offset;
                size *= 16;
                break;

            case MOJOSHADER_UNIFORM_INT:
                // !!! FIXME: Need a test case
                src = &regI[4 * idx];
                dst = shader->constantData + offset;
                size *= 16;
                break;

            case MOJOSHADER_UNIFORM_BOOL:
                // !!! FIXME: Need a test case
                src = &regB[idx];
                dst = shader->constantData + offset;
                break;

            default:
                assert(0); // This should never happen.
                break;
        } // switch

        if (memcmp(dst, src, size) != 0)
        {
            memcpy(dst, src, size);
            needsUpdate = 1;
        } // if

        offset += size;
    } // for

    if (needsUpdate)
    {
        // Map the buffer
        D3D11_MAPPED_SUBRESOURCE res;
        ID3D11DeviceContext_Map((ID3D11DeviceContext*) ctx->deviceContext,
                                (ID3D11Resource*) shader->ubo, 0,
                                D3D11_MAP_WRITE_DISCARD, 0, &res);

        // Copy the contents
        memcpy(res.pData, shader->constantData, shader->buflen);

        // Unmap the buffer
        ID3D11DeviceContext_Unmap(
            (ID3D11DeviceContext*) ctx->deviceContext,
            (ID3D11Resource*) shader->ubo,
            0
        );
    } // if
} // update_uniform_buffer

/* Pixel Shader Compilation Utilities */

static void replaceVarname(const char *find, const char *replace,
                           const char **source)
{
    const char *srcbuf = *source;
    size_t find_len = strlen(find);
    size_t replace_len = strlen(replace);

    #define IS_PARTIAL_TOKEN(token) \
        (isalnum(*(token + find_len)) || isalnum(*(token-1)))

    // How many times does `find` occur in the source buffer?
    int count = 0;
    char *ptr = (char *) strstr(srcbuf, find);
    while (ptr != NULL)
    {
        if (!IS_PARTIAL_TOKEN(ptr))
            count++;
        ptr = strstr(ptr + find_len, find);
    } // while

    // How big should we make the new text buffer?
    size_t oldlen = strlen(srcbuf) + 1;
    size_t newlen = oldlen + (count * (replace_len - find_len));

    // Easy case; just find/replace in the original buffer
    if (newlen == oldlen)
    {
        ptr = (char *) strstr(srcbuf, find);
        while (ptr != NULL)
        {
            if (!IS_PARTIAL_TOKEN(ptr))
                memcpy(ptr, replace, replace_len);
            ptr = strstr(ptr + find_len, find);
        } // while
        return;
    } // if

    // Allocate a new buffer
    char *newbuf = (char *) ctx->malloc_fn(newlen, ctx->malloc_data);
    memset(newbuf, '\0', newlen);

    // Find + replace
    char *prev_ptr = (char *) srcbuf;
    char *curr_ptr = (char *) newbuf;
    ptr = (char*) strstr(srcbuf, find);
    while (ptr != NULL)
    {
        memcpy(curr_ptr, prev_ptr, ptr - prev_ptr);
        curr_ptr += ptr - prev_ptr;

        if (!IS_PARTIAL_TOKEN(ptr))
        {
            memcpy(curr_ptr, replace, replace_len);
            curr_ptr += replace_len;
        } // if
        else
        {
            // Don't accidentally eat partial tokens...
            memcpy(curr_ptr, find, find_len);
            curr_ptr += find_len;
        } // else

        prev_ptr = ptr + find_len;
        ptr = strstr(prev_ptr, find);
    } // while

    #undef IS_PARTIAL_TOKEN

    // Copy the remaining part of the source buffer
    memcpy(curr_ptr, prev_ptr, (srcbuf + oldlen) - prev_ptr);

    // Free the source buffer
    ctx->free_fn((void *) srcbuf, ctx->malloc_data);

    // Point the source parameter to the new buffer
    *source = newbuf;
} // replaceVarname

static char *rewritePixelShader(MOJOSHADER_d3d11Shader *vshader,
                                MOJOSHADER_d3d11Shader *pshader)
{
    const MOJOSHADER_parseData *vpd = vshader->parseData;
    const MOJOSHADER_parseData *ppd = pshader->parseData;
    const char *_Output = "_Output\n{\n";
    const char *_Input = "_Input\n{\n";
    const char *vsrc = vpd->output;
    const char *psrc = ppd->output;
    const char *a, *b, *vout, *pstart, *pend;
    size_t substr_len;
    char *pfinal;

    #define MAKE_STRBUF(buf) \
        substr_len = b - a; \
        buf = (const char *) ctx->malloc_fn(substr_len + 1, ctx->malloc_data); \
        memset((void *) buf, '\0', substr_len + 1); \
        memcpy((void *) buf, a, substr_len);

    // Copy the vertex function's output struct into a buffer
    a = strstr(vsrc, _Output) + strlen(_Output);
    b = a;
    while (*(b++) != '}') {}
    b--;
    MAKE_STRBUF(vout)

    // Split up the pixel shader text...

    // ...everything up to the input contents...
    a = psrc;
    b = strstr(psrc, _Input) + strlen(_Input);
    MAKE_STRBUF(pstart)

    // ...everything after the input contents.
    a = b;
    while (*(a++) != '}') {}
    a--;
    while (*(b++) != '\0') {}
    MAKE_STRBUF(pend)

    // Find matching semantics
    int i, j;
    const char *pvarname, *vvarname;
    for (i = 0; i < ppd->attribute_count; i++)
    {
        for (j = 0; j < vpd->output_count; j++)
        {
            if (ppd->attributes[i].usage == vpd->outputs[j].usage &&
                ppd->attributes[i].index == vpd->outputs[j].index)
            {
                pvarname = ppd->attributes[i].name;
                vvarname = vpd->outputs[j].name;
                if (strcmp(pvarname, vvarname) != 0)
                    replaceVarname(pvarname, vvarname, &pend);
            }
        }
    }

    // Concatenate the shader pieces together
    substr_len = strlen(pstart) + strlen(vout) + strlen(pend);
    pfinal = (char *) ctx->malloc_fn(substr_len + 1, ctx->malloc_data);
    memset((void *) pfinal, '\0', substr_len + 1);
    memcpy(pfinal, pstart, strlen(pstart));
    memcpy(pfinal + strlen(pstart), vout, strlen(vout));
    memcpy(pfinal + strlen(pstart) + strlen(vout), pend, strlen(pend));

    // Free the temporary buffers
    ctx->free_fn((void *) vout, ctx->malloc_data);
    ctx->free_fn((void *) pstart, ctx->malloc_data);
    ctx->free_fn((void *) pend, ctx->malloc_data);

    #undef MAKE_STRBUF

    return pfinal;
} // spliceVertexShaderInput

static ID3D11PixelShader *compilePixelShader(MOJOSHADER_d3d11Shader *vshader,
                                             MOJOSHADER_d3d11Shader *pshader)
{
    ID3D11PixelShader *retval = NULL;
    const char *source;
    ID3DBlob *blob;
    HRESULT result;

    if (pshader->parseData->attribute_count > 0)
        source = rewritePixelShader(vshader, pshader);
    else
        source = pshader->parseData->output;

    result = ctx->D3DCompileFunc(source, strlen(source),
                                 pshader->parseData->mainfn, NULL, NULL,
                                 pshader->parseData->mainfn, "ps_4_0", 0, 0,
                                 &blob, &blob);

    if (result < 0)
    {
        set_error((const char *) ID3D10Blob_GetBufferPointer(blob));
        ctx->free_fn((void *) source, ctx->malloc_data);
        return NULL;
    } // if

    ID3D11Device_CreatePixelShader(ctx->device,
                                   ID3D10Blob_GetBufferPointer(blob),
                                   ID3D10Blob_GetBufferSize(blob),
                                   NULL, &retval);

    ID3D10Blob_Release(blob);
    ctx->free_fn((void *) source, ctx->malloc_data);
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
    ctx->free_fn(ctx, ctx->malloc_data);
    ctx = NULL;
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
    retval->refcount = 1;
    retval->ubo = NULL;
    retval->constantData = NULL;
    retval->buflen = 0;

    if (pd->shader_type == MOJOSHADER_TYPE_VERTEX)
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

    /* Only vertex shaders get compiled here. Pixel shaders will be compiled
     * at link time since they depend on the vertex shader output layout.
     */
    if (pd->shader_type == MOJOSHADER_TYPE_VERTEX)
    {
        // Compile the shader
        ID3DBlob *blob;
        HRESULT result = ctx->D3DCompileFunc(pd->output, pd->output_len,
                                             pd->mainfn, NULL, NULL, pd->mainfn,
                                             "vs_4_0", 0, 0, &blob, &blob);

        if (result < 0)
        {
            set_error((const char *) ID3D10Blob_GetBufferPointer(blob));
            goto compile_shader_fail;
        } // if

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
        // Allocate the shader map array, this will be filled at bind time.
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
    } // else

    // Create the uniform buffer, if needed
    if (pd->uniform_count > 0)
    {
        // Calculate how big we need to make the buffer
        for (i = 0; i < pd->uniform_count; i++)
        {
            int arrayCount = pd->uniforms[i].array_count;
            int uniformSize = 16;
            if (pd->uniforms[i].type == MOJOSHADER_UNIFORM_BOOL)
                uniformSize = 1;
            retval->buflen += (arrayCount ? arrayCount : 1) * uniformSize;
        } // for

        D3D11_BUFFER_DESC bdesc;
        bdesc.ByteWidth = next_highest_alignment(retval->buflen);
        bdesc.Usage = D3D11_USAGE_DYNAMIC;
        bdesc.BindFlags = D3D11_BIND_CONSTANT_BUFFER;
        bdesc.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;
        bdesc.MiscFlags = 0;
        bdesc.StructureByteStride = 0;
        ID3D11Device_CreateBuffer((ID3D11Device*) ctx->device, &bdesc, NULL,
                                  (ID3D11Buffer**) &retval->ubo);

        // Additionally allocate a CPU-side staging buffer
        retval->constantData = (unsigned char *) m(retval->buflen, d);
        memset(retval->constantData, '\0', retval->buflen);
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
            {
                ID3D11Buffer_Release((ID3D11Buffer*) shader->ubo);
                ctx->free_fn(shader->constantData, ctx->malloc_data);
            } // if

            if (shader->parseData->shader_type == MOJOSHADER_TYPE_VERTEX)
            {
                ID3D10Blob_Release((ID3DBlob *) shader->vertex.dataBlob);
                ID3D11VertexShader_Release((ID3D11VertexShader *) shader->vertex.shader);
            } // if
            else if (shader->parseData->shader_type == MOJOSHADER_TYPE_PIXEL)
            {
                for (int i = 0; i < shader->pixel.numMaps; i++)
                    ID3D11PixelShader_Release((ID3D11PixelShader *) shader->pixel.shaderMaps[i].pshader);

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
    if (vshader != NULL)
    {
        ctx->vertexShader = vshader;
        ID3D11DeviceContext_VSSetShader(
            ctx->deviceContext,
            (ID3D11VertexShader*) vshader->vertex.shader,
            NULL, 0
        );
        ID3D11DeviceContext_VSSetConstantBuffers(
            ctx->deviceContext, 0, 1,
            (ID3D11Buffer**) &vshader->ubo
        );
    } // if

    if (pshader != NULL)
    {
        ctx->pixelShader = pshader;

        // Now we have to figure out which _real_ pixel shader to use...
        ID3D11PixelShader *realPS = NULL;

        // Is there already a mapping for this vertex shader?
        for (int i = 0; i < pshader->pixel.numMaps; i++)
        {
            if (pshader->pixel.shaderMaps[i].vshader == ctx->vertexShader)
            {
                realPS = (ID3D11PixelShader *) pshader->pixel.shaderMaps[i].pshader;
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
                    ctx->malloc_data
                );

                memcpy(newMap, pshader->pixel.shaderMaps,
                        sizeof(d3d11ShaderMap) * pshader->pixel.mapCapacity);

                pshader->pixel.mapCapacity *= 2;
                ctx->free_fn(pshader->pixel.shaderMaps, ctx->malloc_data);
                pshader->pixel.shaderMaps = newMap;
                newMap = NULL;
            } // if

            // Add the new mapping
            pshader->pixel.shaderMaps[pshader->pixel.numMaps].vshader = ctx->vertexShader;
            realPS = compilePixelShader(ctx->vertexShader, pshader);
            pshader->pixel.shaderMaps[pshader->pixel.numMaps].pshader = realPS;
            pshader->pixel.numMaps++;
            if (realPS == NULL)
            {
                // !!! FIXME: Just for debug purposes... <_<
                assert(0 && "OH CRAP");
            }
        } // if

        ID3D11DeviceContext_PSSetShader(ctx->deviceContext, realPS, NULL, 0);
        ID3D11DeviceContext_PSSetConstantBuffers(
            ctx->deviceContext, 0, 1,
            (ID3D11Buffer **) &pshader->ubo
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
