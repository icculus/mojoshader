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

#define DEBUG_TOKENIZER 1


typedef struct Context Context;

// Context...this is state that changes as we assemble a shader...
struct Context
{
    MOJOSHADER_malloc malloc;
    MOJOSHADER_free free;
    void *malloc_data;
    const char *failstr;
    const char *source;
    MOJOSHADER_shaderType shader_type;
    uint8 major_ver;
    uint8 minor_ver;
    unsigned int instruction_count;
    unsigned int linenum;
    char prevchar;
    char token[64];
    char pushedback;
    char pushback_token[64];
    uint32 tokenbuf[16];
    int tokenbufpos;
    int centroid_allowed;
    DestArgInfo dest_arg;
};


// Convenience functions for allocators...

static MOJOSHADER_assembleData out_of_mem_data = {
    "Out of memory", 0, 0, 0, MOJOSHADER_TYPE_UNKNOWN, 0, 0, 0, 0, 0
};


static const char *out_of_mem_str = "Out of memory";
static inline int out_of_memory(Context *ctx)
{
    if (ctx->failstr == NULL)
        ctx->failstr = out_of_mem_str;  // fail() would call malloc().
    return FAIL;
} // out_of_memory

static inline void *Malloc(Context *ctx, const size_t len)
{
    void *retval = ctx->malloc((int) len, ctx->malloc_data);
    if (retval == NULL)
        out_of_memory(ctx);
    return retval;
} // Malloc

static inline void Free(Context *ctx, void *ptr)
{
    if (ptr != NULL)  // check for NULL in case of dumb free() impl.
        ctx->free(ptr, ctx->malloc_data);
} // Free

static int failf(Context *ctx, const char *fmt, ...) ISPRINTF(2,3);
static int failf(Context *ctx, const char *fmt, ...)
{
    if (ctx->failstr == NULL)  // don't change existing error.
    {
        char scratch = 0;
        va_list ap;
        va_start(ap, fmt);
        const int len = vsnprintf(&scratch, sizeof (scratch), fmt, ap);
        va_end(ap);

        char *failstr = (char *) Malloc(ctx, len + 1);
        if (failstr != NULL)
        {
            va_start(ap, fmt);
            vsnprintf(failstr, len + 1, fmt, ap);  // rebuild it.
            va_end(ap);
            ctx->failstr = failstr;
        } // if
    } // if

    return FAIL;
} // failf

static inline int fail(Context *ctx, const char *reason)
{
    return failf(ctx, "%s", reason);
} // fail

static inline int isfail(const Context *ctx)
{
    return (ctx->failstr != NULL);
} // isfail


// Shader model version magic...

static inline uint32 ver_ui32(const uint8 major, const uint8 minor)
{
    return ( (((uint32) major) << 16) | (((minor) == 0xFF) ? 0 : (minor)) );
} // version_ui32

static inline int shader_version_supported(const uint8 maj, const uint8 min)
{
    return (ver_ui32(maj,min) <= ver_ui32(MAX_SHADER_MAJOR, MAX_SHADER_MINOR));
} // shader_version_supported

static inline int shader_version_atleast(const Context *ctx, const uint8 maj,
                                         const uint8 min)
{
    return (ver_ui32(ctx->major_ver, ctx->minor_ver) >= ver_ui32(maj, min));
} // shader_version_atleast

static inline int shader_version_exactly(const Context *ctx, const uint8 maj,
                                         const uint8 min)
{
    return ((ctx->major_ver == maj) && (ctx->minor_ver == min));
} // shader_version_exactly

static inline int shader_is_pixel(const Context *ctx)
{
    return (ctx->shader_type == MOJOSHADER_TYPE_PIXEL);
} // shader_is_pixel

static inline int shader_is_vertex(const Context *ctx)
{
    return (ctx->shader_type == MOJOSHADER_TYPE_VERTEX);
} // shader_is_vertex


extern void writeme(void);

static void output_token_noswap(Context *ctx, const uint32 token)
{
    if (isfail(ctx))
        return;

    writeme();
} // output_token_noswap


static inline void output_token(Context *ctx, const uint32 token)
{
    output_token_noswap(ctx, SWAP32(token));
} // output_token


static void output_comment_bytes(Context *ctx, const uint8 *buf, size_t len)
{
    if (len > (0xFFFF * 4))  // length is stored as token count, in 16 bits.
        fail(ctx, "Comment field is too big");
    else if (!isfail(ctx))
    {
        const uint32 tokencount = (len / 4) + ((len % 4) ? 1 : 0);
        output_token(ctx, 0xFFFE | (tokencount << 16));
        while (len >= 4)
        {
            output_token_noswap(ctx, *((const uint32 *) buf));
            len -= 4;
            buf += 4;
        } // while

        if (len > 0)  // handle spillover...
        {
            union { uint8 ui8[4]; uint32 ui32; } overflow;
            overflow.ui32 = 0;
            memcpy(overflow.ui8, buf, len);
            output_token_noswap(ctx, overflow.ui32);
        } // if
    } // else if
} // output_comment_bytes


static inline void output_comment_string(Context *ctx, const char *str)
{
    output_comment_bytes(ctx, (const uint8 *) str, strlen(str));
} // output_comment_string


static int _tokenize(Context *ctx)
{
    int idx = 0;

    if (isfail(ctx))
        return FAIL;

    if (ctx->pushedback)
    {
        ctx->pushedback = 0;
        return NOFAIL;
    } // if

    while (1)
    {
        if (idx >= sizeof (ctx->token))
            return fail(ctx, "buffer overflow");

        char ch = *ctx->source;
        if (ch == '\t')
            ch = ' ';  // collapse tabs into single spaces.
        else if (ch == '\r')
        {
            if (ctx->source[1] == '\n')
               continue;  // ignore '\r' if this is "\r\n" ...
            ch = '\n';
        } // else if

        if ((ch > '0') && (ch < '9'))
        {
            // starting a number, but rest of current token was not number.
            if ((idx > 0) && ((ctx->prevchar < '0') || (ctx->prevchar > '9')))
            {
                ctx->token[idx++] = '\0';
                return NOFAIL;
            } // if
        } // if
        else
        {
            // starting a non-number, but rest of current token was numbers.
            if ((idx > 0) && ((ctx->prevchar >= '0') || (ctx->prevchar <= '9')))
            {
                ctx->token[idx++] = '\0';
                return NOFAIL;
            } // if
        } // else

        switch (ch)
        {
            case '/':
            case ';':  // !!! FIXME: comment, right?
                if (idx != 0)  // finish off existing token.
                    ctx->token[idx] = '\0';
                else
                {
                    ctx->token[idx++] = ch;
                    ctx->source++;
                    if ((ch == '/') && (ctx->source[1] == '/'))
                    {
                        ctx->token[idx++] = '/';
                        ctx->source++;
                    } // if
                    ctx->token[idx++] = '\0';
                } // else
                return NOFAIL;

            case ' ':
                if (ctx->prevch == ' ')
                    break;   // multiple whitespace collapses into one.
                // intentional fall-through...

            case '_':
            case '[':
            case ']':
            case '(':
            case ')':
            case '!':
            case '+':
            case '-':
            case ',':
            case '.':
            case '\n':
                if (idx != 0)  // finish off existing token.
                    ctx->token[idx] = '\0';
                else  // this is a token in itself.
                {
                    if (ch == '\n')
                        ctx->linenum++;
                    ctx->source++;
                    ctx->token[idx++] = ch;
                    ctx->token[idx++] = '\0';
                } // else
                return NOFAIL;

            case '\0':
                ctx->token[idx] = '\0';
                if (idx != 0)  // had any chars? It's a token.
                    return NOFAIL;
                return END_OF_STREAM;

            default:
                ctx->source++;
                ctx->token[idx++] = ch;
                break;
        } // switch

        ctx->prevchar = ch;
    } // while

    return fail(ctx, "???");  // shouldn't hit this.
} // _tokenize


static inline int tokenize(Context *ctx)
{
    const int rc = _tokenize(ctx);
    #if DEBUG_TOKENIZER
    printf("TOKENIZE: %s '%s'\n",
           (rc == END_OF_STREAM) ? "END_OF_STREAM" :
           (rc == FAIL) ? "FAIL" :
           (rc == NOFAIL) ? "NOFAIL" : "???",
           ctx->token);
    #endif
    return rc;
} // tokenize


static inline void pushback(Context *ctx)
{
    #if DEBUG_TOKENIZER
    printf("PUSHBACK\n");
    #endif

    if (ctx->pushedback)
        fail(ctx, "BUG: Double pushback in parser");
    else
        ctx->pushedback = 1;
} // pushback


static int nexttoken(Context *ctx, const int ignoreeol,
                     const int ignorewhitespace, const int eolok,
                     const int eosok)
{
    int rc = NOFAIL;

    while ((rc = tokenize(ctx)) == NOFAIL)
    {
        if (strcmp(ctx->token, "\n") == 0)
        {
            if (ignoreeol)
                continue;
            else if (!eolok)
                return fail(ctx, "Unexpected EOL");
        } // if

        else if (strcmp(ctx->token, " ") == 0)
        {
            if (ignorewhitespace)
                continue;
        } // else if

        // skip comments...
        else if ((strcmp(ctx->token, "//") == 0) || (strcmp(ctx->token, ";") == 0))
        {
            while ((rc = tokenize(ctx)) == NOFAIL)
            {
                if (strcmp(ctx->token, "\n") == 0)
                    break;
            } // while
        } // if

        break;
    } // while

    #if DEBUG_TOKENIZER
    printf("NEXTTOKEN: %s '%s'\n",
           (rc == END_OF_STREAM) ? "END_OF_STREAM" :
           (rc == FAIL) ? "FAIL" :
           (rc == NOFAIL) ? "NOFAIL" : "???",
           ctx->token);
    #endif

    if ((rc == END_OF_STREAM) && (!eosok))
        return fail(ctx, "Unexpected EOF");

    return rc;
} // nexttoken


static int require_endline(Context *ctx)
{
    const int rc = nexttoken(ctx, 0, 1, 1, 1);
    if (rc == FAIL)
        return FAIL;
    else if (rc == END_OF_STREAM)
        return NOFAIL;  // we'll call this an EOL.
    else if (strcmp(ctx->token, "\n") != 0)
        return fail(ctx, "Endline expected");
    return NOFAIL;
} // require_endline



// !!! FIXME: merge parse_* into mojoshader.c to reduce cut-and-paste.
// !!! FIXME:  we need to merge Context, which is the nastiest part.

static int set_result_shift(Context *ctx, DestArgInfo *info, const int val)
{
    if (info->result_shift != 0)
        return fail(ctx, "Multiple result shift modifiers");
    info->result_shift = val;
    return NOFAIL;
} // set_result_shift


static int parse_register_name(Context *ctx, RegisterType *rtype, int *rnum)
{
    if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
        return FAIL;

    // !!! FIXME: some of these registers are only valid for some shader types.
    int neednum = 1;
    int regnum = 0;
    const char *t = ctx->token;
    RegisterType regtype = REG_TYPE_TEMP;
    if (strcasecmp(t, "r") == 0)
        regtype = REG_TYPE_TEMP;
    else if (strcasecmp(t, "v") == 0)
        regtype = REG_TYPE_INPUT;
    else if (strcasecmp(t, "c") == 0)
        regtype = REG_TYPE_CONST;
    else if (strcasecmp(t, "i") == 0)
        regtype = REG_TYPE_CONSTINT;
    else if (strcasecmp(t, "b") == 0)
        regtype = REG_TYPE_CONSTBOOL;
    else if (strcasecmp(t, "oC") == 0)
        regtype = REG_TYPE_COLOROUT;
    else if (strcasecmp(t, "oDepth") == 0)
        regtype = REG_TYPE_DEPTHOUT;
    else if (strcasecmp(t, "s") == 0)
        regtype = REG_TYPE_SAMPLER;
    else if (strcasecmp(t, "oD") == 0)
        regtype = REG_TYPE_ATTROUT;
    else if (strcasecmp(t, "l") == 0)
        regtype = REG_TYPE_LABEL;
    else if (strcasecmp(t, "p") == 0)
        regtype = REG_TYPE_PREDICATE;
    else if (strcasecmp(t, "aL") == 0)
    {
        regtype = REG_TYPE_LOOP;
        neednum = 0;
    } // else if
    else if (strcasecmp(t, "o") == 0)
    {
        if (!shader_is_vertex(ctx) || !shader_version_atleast(ctx, 3, 0))
            return fail(ctx, "Output register not valid in this shader type");
        regtype = REG_TYPE_OUTPUT;
    } // else if
    else if (strcasecmp(t, "oT") == 0)
    {
        if (shader_is_vertex(ctx) || shader_version_atleast(ctx, 3, 0))
            return fail(ctx, "Output register not valid in this shader type");
        regtype = REG_TYPE_OUTPUT;
    } // else if
    else if (strcasecmp(t, "a") == 0)
    {
        if (!shader_is_vertex(ctx))
            return fail(ctx, "Address register only valid in vertex shaders.");
        regtype = REG_TYPE_ADDRESS;
    } // else if
    else if (strcasecmp(t, "t") == 0)
    {
        if (!shader_is_pixel(ctx))
            return fail(ctx, "Address register only valid in pixel shaders.");
        regtype = REG_TYPE_ADDRESS;
    } // else if
    else if (strcasecmp(t, "vPos") == 0)
    {
        regtype = REG_TYPE_MISCTYPE;
        regnum = (int) MISCTYPE_TYPE_POSITION;
        neednum = 0;
    } // else if
    else if (strcasecmp(t, "vFace") == 0)
    {
        regtype = REG_TYPE_MISCTYPE;
        regnum = (int) MISCTYPE_TYPE_FACE;
        neednum = 0;
    } // else if
    else if (strcasecmp(t, "oPos") == 0)
    {
        regtype = REG_TYPE_RASTOUT;
        regnum = (int) RASTOUT_TYPE_POSITION;
        neednum = 0;
    } // else if
    else if (strcasecmp(t, "oFog") == 0)
    {
        regtype = REG_TYPE_RASTOUT;
        regnum = (int) RASTOUT_TYPE_FOG;
        neednum = 0;
    } // else if
    else if (strcasecmp(t, "oPts") == 0)
    {
        regtype = REG_TYPE_RASTOUT;
        regnum = (int) RASTOUT_TYPE_POINT_SIZE;
        neednum = 0;
    } // else if
        
    //case REG_TYPE_TEMPFLOAT16:  // !!! FIXME: don't know this asm string

    else
    {
        return fail(ctx, "expected register type");
    } // else

    if (neednum)
    {
        if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
            return FAIL;

        //minor = atoi(ctx->token);
        char *endptr = NULL;
        const long val = strtol(ctx->token, &endptr, 10);
        regnum = (int) val;
        if ((*ctx->token == '\0') || (*endptr != '\0'))
            return fail(ctx, "Invalid version string");
    } // if

    // split up REG_TYPE_CONST
    if (regtype == REG_TYPE_CONST)
    {
        if (regnum < 2048)
        {
            regtype = REG_TYPE_CONST;
            regnum -= 0;
        } // if
        else if (regnum < 4096)
        {
            regtype = REG_TYPE_CONST2;
            regnum -= 2048;
        } // if
        else if (regnum < 6144)
        {
            regtype = REG_TYPE_CONST3;
            regnum -= 4096;
        } // if
        else if (regnum < 8192)
        {
            regtype = REG_TYPE_CONST4;
            regnum -= 6144;
        } // if
        else
        {
            return fail(ctx, "Invalid const register index");
        } // else
    } // if

    *rtype = regtype;
    *rnum = regnum;

    return NOFAIL;
} // parse_register_name


static int parse_destination_token(Context *ctx, DestArgInfo *info)
{
    // !!! FIXME: recheck against the spec for ranges (like RASTOUT values, etc).

    memset(info, '\0', sizeof (info));
    info->token = token;

    // See if there are destination modifiers on the instruction itself...
    while (1)
    {
        if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
            return FAIL;
        else if (strcmp(ctx->token, " ") == 0)
            break;  // done with modifiers.
        else if (strcmp(ctx->token, "_") != 0)
            return fail(ctx, "Expected modifier or whitespace");
        else if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
            return FAIL;
        else if (strcasecmp(ctx->token, "x2") == 0)
            set_result_shift(ctx, info, 0x1);
        else if (strcasecmp(ctx->token, "x4") == 0)
            set_result_shift(ctx, info, 0x2);
        else if (strcasecmp(ctx->token, "x8") == 0)
            set_result_shift(ctx, info, 0x3);
        else if (strcasecmp(ctx->token, "d8") == 0)
            set_result_shift(ctx, info, 0xD);
        else if (strcasecmp(ctx->token, "d4") == 0)
            set_result_shift(ctx, info, 0xE);
        else if (strcasecmp(ctx->token, "d2") == 0)
            set_result_shift(ctx, info, 0xF);
        else if (strcasecmp(ctx->token, "sat") == 0)
            info->result_mod |= MOD_SATURATE;
        else if (strcasecmp(ctx->token, "pp") == 0)
            info->result_mod |= MOD_PP;
        else if (strcasecmp(ctx->token, "centroid") == 0)
            info->result_mod |= MOD_CENTROID;
        else
            return fail(ctx, "Expected modifier");
    } // while

    if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return FAIL;

    // !!! FIXME: predicates.
    if (strcmp(ctx->token, "(") == 0)
        return fail(ctx, "Predicates unsupported at this time");
    pushback(ctx);  // parse_register_name calls nexttoken().

    if (parse_register_name(ctx, &info->regtype, &info->regnum) == FAIL)
        return FAIL;

    if (nexttoken(ctx, 0, 0, 1, 1) == FAIL)
        return FAIL;

    // !!! FIXME: can dest registers do relative addressing?

    if (strcmp(ctx->token, ".") != 0)
    {
        info->writemask = 0xF;
        info->writemask0 = info->writemask1 = info->writemask2 = info->writemask3 = 1;
        pushback(ctx);  // no explicit writemask; do full mask.
    } // if
    else if (scalar_register(info->regtype, info->regnum))
        return fail(ctx, "Writemask specified for scalar register");
    else if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return FAIL;
    else if (ctx->token[0] == '\0')
        return fail(ctx, "Invalid writemask");
    else
    {
        char *ptr = ctx->token;
        info->writemask0 = info->writemask1 = info->writemask2 = info->writemask3 = 0;
        if (*ptr = 'x') { info->writemask0 = 1; ptr++; }
        if (*ptr = 'y') { info->writemask1 = 1; ptr++; }
        if (*ptr = 'z') { info->writemask2 = 1; ptr++; }
        if (*ptr = 'w') { info->writemask3 = 1; ptr++; }
        if ((ptr == ctx->token) && (is_pixel_shader(ctx))
        {
            if (*ptr = 'r') { info->writemask0 = 1; ptr++; }
            if (*ptr = 'g') { info->writemask1 = 1; ptr++; }
            if (*ptr = 'b') { info->writemask2 = 1; ptr++; }
            if (*ptr = 'a') { info->writemask3 = 1; ptr++; }
        } // if

        if (*ptr != '\0')
            return fail(ctx, "Invalid writemask");

        info->writemask = ( ((info->writemask0 & 0x1) << 0) |
                            ((info->writemask1 & 0x1) << 1) |
                            ((info->writemask2 & 0x1) << 2) |
                            ((info->writemask3 & 0x1) << 3) );
    } // else

    info->orig_writemask = info->writemask;

    // !!! FIXME: cut and paste from mojoshader.c ...
    if (info->relative)
    {
        if (!shader_is_vertex(ctx))
            return fail(ctx, "Relative addressing in non-vertex shader");
        else if (!shader_version_atleast(ctx, 3, 0))
            return fail(ctx, "Relative addressing in vertex shader version < 3.0");
        else if (!ctx->have_ctab)  // it's hard to do this efficiently without!
            return fail(ctx, "relative addressing unsupported without a CTAB");
        // !!! FIXME: I don't have a shader that has a relative dest currently.
        return fail(ctx, "Relative addressing of dest tokens is unsupported");
    } // if

    const int s = info->result_shift;
    if (s != 0)
    {
        if (!shader_is_pixel(ctx))
            return fail(ctx, "Result shift scale in non-pixel shader");
        else if (shader_version_atleast(ctx, 2, 0))
            return fail(ctx, "Result shift scale in pixel shader version >= 2.0");
        else if ( ! (((s >= 1) && (s <= 3)) || ((s >= 0xD) && (s <= 0xF))) )
            return fail(ctx, "Result shift scale isn't 1 to 3, or 13 to 15.");
    } // if

    if (info->result_mod & MOD_PP)  // Partial precision (pixel shaders only)
    {
        if (!shader_is_pixel(ctx))
            return fail(ctx, "Partial precision result mod in non-pixel shader");
    } // if

    if (info->result_mod & MOD_CENTROID)  // Centroid (pixel shaders only)
    {
        if (!shader_is_pixel(ctx))
            return fail(ctx, "Centroid result mod in non-pixel shader");
        else if (!ctx->centroid_allowed)  // only on DCL opcodes!
            return fail(ctx, "Centroid modifier not allowed here");
    } // if

    // !!! FIXME: from msdn:
    //  "_sat cannot be used with instructions writing to output o# registers."
    // !!! FIXME: actually, just go over this page:
    //  http://msdn.microsoft.com/archive/default.asp?url=/archive/en-us/directx9_c/directx/graphics/reference/shaders/ps_instructionmodifiers.asp

    if (ctx->tokenbufpos >= STATICARRAYLEN(ctx->tokenbuf))
        return fail(ctx, "Too many tokens");

    ctx->tokenbuf[ctx->tokenbufpos++] =
            ( ((((uint32) 0x80000000)) << 0) |
              ((((uint32) info->regnum) & 0x7ff) << 0) |
              ((((uint32) info->relative) & 0x1) << 13) |
              ((((uint32) info->result_mod) & 0xF) << 20) |
              ((((uint32) info->result_shift) & 0xF) << 24) |
              ((((uint32) info->regtype) & 0x7) << 28) |
              ((((uint32) info->regtype) & 0x18) << 8) );

    return 1;
} // parse_destination_token


static int parse_args_NULL(Context *ctx)
{
    return (isfail(ctx) ? FAIL : 1);
} // parse_args_NULL


static int parse_args_DEF(Context *ctx)
{
    if (parse_destination_token(ctx, &ctx->dest_arg) == FAIL)
        return FAIL;

    if (ctx->dest_arg.relative)  // I'm pretty sure this is illegal...?
        return fail(ctx, "relative addressing in DEFB");

sdfsdf
// !!! FIXME: parse out def.
    ctx->dwords[0] = SWAP32(ctx->tokens[0]);
    ctx->dwords[1] = SWAP32(ctx->tokens[1]);
    ctx->dwords[2] = SWAP32(ctx->tokens[2]);
    ctx->dwords[3] = SWAP32(ctx->tokens[3]);

    return 6;
} // parse_args_DEF


static int parse_args_DEFB(Context *ctx)
{
    if (parse_destination_token(ctx, &ctx->dest_arg) == FAIL)
        return FAIL;

    if (ctx->dest_arg.relative)  // I'm pretty sure this is illegal...?
        return fail(ctx, "relative addressing in DEFB");

    ctx->dwords[0] = *(ctx->tokens) ? 1 : 0;

    return 3;
} // parse_args_DEFB


static int valid_texture_type(const uint32 ttype)
{
    switch ((const TextureType) ttype)
    {
        case TEXTURE_TYPE_2D:
        case TEXTURE_TYPE_CUBE:
        case TEXTURE_TYPE_VOLUME:
            return 1;  // it's okay.
    } // switch

    return 0;
} // valid_texture_type


// !!! FIXME: this function is kind of a mess.
// !!! FIXME: cut-and-paste from mojoshader.c ...
static int parse_args_DCL(Context *ctx)
{
    int unsupported = 0;
    char usage[sizeof (ctx->token)];

static int nexttoken(Context *ctx, const int ignoreeol,
                     const int ignorewhitespace, const int eolok,
                     const int eosok)

    char usagestr[sizeof (ctx->token)];
    static const char *usagestrs[] = {
        "position", "blendweight", "blendindices", "normal", "psize",
        "texcoord", "tangent", "binormal", "tessfactor", "positiont",
        "color", "fog", "depth", "sample"
    };

    if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return FAIL;
    else if (strcmp(ctx->token, " ") == 0)
    {
        pushback(ctx);
        usagestr[0] = '\0';
    } // else if
    else if (strcmp(ctx->token, "_") != 0)
        return fail(ctx, "Expected register or usage");
    else if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return FAIL;
    else
        strcpy(usagestr, ctx->token);

    if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return FAIL;
    else if (strcmp(ctx->token, " ") == 0)
        return fail(ctx, "Expected whitespace");
    
    ctx->centroid_allowed = 1;
    ctx->tokenbufpos++;
    const int parse_dest_rc = parse_destination_token(ctx, &ctx->dest_arg);
    ctx->centroid_allowed = 0;
    if (parse_dest_rc == FAIL)
        return FAIL;

    if (ctx->dest_arg.result_shift != 0)  // I'm pretty sure this is illegal...?
        return fail(ctx, "shift scale in DCL");
    else if (ctx->dest_arg.relative)  // I'm pretty sure this is illegal...?
        return fail(ctx, "relative addressing in DCL");

    const RegisterType regtype = ctx->dest_arg.regtype;
    const int regnum = ctx->dest_arg.regnum;
    if ( (shader_is_pixel(ctx)) && (shader_version_atleast(ctx, 3, 0)) )
    {
        if (regtype == REG_TYPE_INPUT)
        {
            token |=
            const uint32 usage = (token & 0xF);
            const uint32 index = ((token >> 16) & 0xF);
            reserved_mask = 0x7FF0FFE0;
            ctx->dwords[0] = usage;
            ctx->dwords[1] = index;
        } // if

        else if (regtype == REG_TYPE_MISCTYPE)
        {
            const MiscTypeType mt = (MiscTypeType) regnum;
            if (mt == MISCTYPE_TYPE_POSITION)
                reserved_mask = 0x7FFFFFFF;
            else if (mt == MISCTYPE_TYPE_FACE)
            {
                reserved_mask = 0x7FFFFFFF;
                if (!writemask_xyzw(ctx->dest_arg.orig_writemask))
                    return fail(ctx, "DCL face writemask must be full");
                else if (ctx->dest_arg.result_mod != 0)
                    return fail(ctx, "DCL face result modifier must be zero");
                else if (ctx->dest_arg.result_shift != 0)
                    return fail(ctx, "DCL face shift scale must be zero");
            } // else if
            else
            {
                unsupported = 1;
            } // else

            ctx->dwords[0] = (uint32) MOJOSHADER_USAGE_UNKNOWN;
            ctx->dwords[1] = 0;
        } // else if

        else if (regtype == REG_TYPE_TEXTURE)
        {
            const uint32 usage = (token & 0xF);
            const uint32 index = ((token >> 16) & 0xF);
            if (usage == MOJOSHADER_USAGE_TEXCOORD)
            {
                if (index > 7)
                    return fail(ctx, "DCL texcoord usage must have 0-7 index");
            } // if
            else if (usage == MOJOSHADER_USAGE_COLOR)
            {
                if (index != 0)
                    return fail(ctx, "DCL color usage must have 0 index");
            } // else if
            else
            {
                return fail(ctx, "Invalid DCL texture usage");
            } // else

            reserved_mask = 0x7FF0FFE0;
            ctx->dwords[0] = usage;
            ctx->dwords[1] = index;
        } // else if

        else if (regtype == REG_TYPE_SAMPLER)
        {
            const uint32 ttype = ((token >> 27) & 0xF);
            if (!valid_texture_type(ttype))
                return fail(ctx, "unknown sampler texture type");
            reserved_mask = 0x7FFFFFF;
            ctx->dwords[0] = ttype;
        } // else if

        else
        {
            unsupported = 1;
        } // else
    } // if

    else if ( (shader_is_pixel(ctx)) && (shader_version_atleast(ctx, 2, 0)) )
    {
        if (regtype == REG_TYPE_INPUT)
        {
            ctx->dwords[0] = (uint32) MOJOSHADER_USAGE_COLOR;
            ctx->dwords[1] = regnum;
            reserved_mask = 0x7FFFFFFF;
        } // if
        else if (regtype == REG_TYPE_TEXTURE)
        {
            ctx->dwords[0] = (uint32) MOJOSHADER_USAGE_TEXCOORD;
            ctx->dwords[1] = regnum;
            reserved_mask = 0x7FFFFFFF;
        } // else if
        else if (regtype == REG_TYPE_SAMPLER)
        {
            const uint32 ttype = ((token >> 27) & 0xF);
            if (!valid_texture_type(ttype))
                return fail(ctx, "unknown sampler texture type");
            reserved_mask = 0x7FFFFFF;
            ctx->dwords[0] = ttype;
        } // else if
        else
        {
            unsupported = 1;
        } // else
    } // if

    else if ( (shader_is_vertex(ctx)) && (shader_version_atleast(ctx, 3, 0)) )
    {
        if ((regtype == REG_TYPE_INPUT) || (regtype == REG_TYPE_OUTPUT))
        {
            const uint32 usage = (token & 0xF);
            const uint32 index = ((token >> 16) & 0xF);
            reserved_mask = 0x7FF0FFE0;
            ctx->dwords[0] = usage;
            ctx->dwords[1] = index;
        } // if
        else
        {
            unsupported = 1;
        } // else
    } // else if

    else if ( (shader_is_vertex(ctx)) && (shader_version_atleast(ctx, 2, 0)) )
    {
        if (regtype == REG_TYPE_INPUT)
        {
            const uint32 usage = (token & 0xF);
            const uint32 index = ((token >> 16) & 0xF);
            reserved_mask = 0x7FF0FFE0;
            ctx->dwords[0] = usage;
            ctx->dwords[1] = index;
        } // if
        else
        {
            unsupported = 1;
        } // else
    } // else if

    else
    {
        unsupported = 1;
    } // else

    if (unsupported)
        return fail(ctx, "invalid DCL register type for this shader model");

    if ((token & reserved_mask) != 0)
        return fail(ctx, "reserved bits in DCL dword aren't zero");

    return 3;
} // parse_args_DCL


static int parse_args_D(Context *ctx)
{
    int retval = 1;
    retval += parse_destination_token(ctx, &ctx->dest_arg);
    return isfail(ctx) ? FAIL : retval;
} // parse_args_D


static int parse_args_S(Context *ctx)
{
    int retval = 1;
    retval += parse_source_token(ctx, &ctx->source_args[0]);
    return isfail(ctx) ? FAIL : retval;
} // parse_args_S


static int parse_args_SS(Context *ctx)
{
    int retval = 1;
    retval += parse_source_token(ctx, &ctx->source_args[0]);
    retval += parse_source_token(ctx, &ctx->source_args[1]);
    return isfail(ctx) ? FAIL : retval;
} // parse_args_SS


static int parse_args_DS(Context *ctx)
{
    int retval = 1;
    retval += parse_destination_token(ctx, &ctx->dest_arg);
    retval += parse_source_token(ctx, &ctx->source_args[0]);
    return isfail(ctx) ? FAIL : retval;
} // parse_args_DS


static int parse_args_DSS(Context *ctx)
{
    int retval = 1;
    retval += parse_destination_token(ctx, &ctx->dest_arg);
    retval += parse_source_token(ctx, &ctx->source_args[0]);
    retval += parse_source_token(ctx, &ctx->source_args[1]);
    return isfail(ctx) ? FAIL : retval;
} // parse_args_DSS


static int parse_args_DSSS(Context *ctx)
{
    int retval = 1;
    retval += parse_destination_token(ctx, &ctx->dest_arg);
    retval += parse_source_token(ctx, &ctx->source_args[0]);
    retval += parse_source_token(ctx, &ctx->source_args[1]);
    retval += parse_source_token(ctx, &ctx->source_args[2]);
    return isfail(ctx) ? FAIL : retval;
} // parse_args_DSSS


static int parse_args_DSSSS(Context *ctx)
{
    int retval = 1;
    retval += parse_destination_token(ctx, &ctx->dest_arg);
    retval += parse_source_token(ctx, &ctx->source_args[0]);
    retval += parse_source_token(ctx, &ctx->source_args[1]);
    retval += parse_source_token(ctx, &ctx->source_args[2]);
    retval += parse_source_token(ctx, &ctx->source_args[3]);
    return isfail(ctx) ? FAIL : retval;
} // parse_args_DSSSS


static int parse_args_SINCOS(Context *ctx)
{
    // this opcode needs extra registers for sm2 and lower.
    if (!shader_version_atleast(ctx, 3, 0))
        return parse_args_DSSS(ctx);
    return parse_args_DS(ctx);
} // parse_args_SINCOS


static int parse_args_TEXCRD(Context *ctx)
{
    // added extra register in ps_1_4.
    if (shader_version_atleast(ctx, 1, 4))
        return parse_args_DS(ctx);
    return parse_args_D(ctx);
} // parse_args_TEXCRD


static int parse_args_TEXLD(Context *ctx)
{
    // different registers in px_1_3, ps_1_4, and ps_2_0!
    if (shader_version_atleast(ctx, 2, 0))
        return parse_args_DSS(ctx);
    else if (shader_version_atleast(ctx, 1, 4))
        return parse_args_DS(ctx);
    return parse_args_D(ctx);
} // parse_args_TEXLD







// one args function for each possible sequence of opcode arguments.
typedef int (*args_function)(Context *ctx);

// one state function for each opcode where we have state machine updates.
typedef void (*state_function)(Context *ctx);

// Lookup table for instruction opcodes...
typedef struct
{
    const char *opcode_string;
    int slots;  // number of instruction slots this opcode eats.
    MOJOSHADER_shaderType shader_types;  // mask of types that can use opcode.
    args_function parse_args;
    state_function state;
} Instruction;


static const Instruction instructions[] =
{
/*  !!! FIXME: push this through mojoshader.c's state machine.
    #define INSTRUCTION_STATE(op, opstr, slots, a, t) { \
        opstr, slots, t, parse_args_##a, state_##op \
    },
*/
    #define INSTRUCTION_STATE(op, opstr, slots, a, t) { \
        opstr, slots, t, parse_args_##a, 0 \
    },

    #define INSTRUCTION(op, opstr, slots, a, t) { \
        opstr, slots, t, parse_args_##a, 0 \
    },

    #define MOJOSHADER_DO_INSTRUCTION_TABLE 1
    #include "mojoshader_internal.h"
    #undef MOJOSHADER_DO_INSTRUCTION_TABLE

    #undef INSTRUCTION
    #undef INSTRUCTION_STATE
};

static int parse_condition(Context *ctx, uint32 *controls)
{
    if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return 0;
    else if (strcmp(ctx->token, "_") != 0)
    {
        pushback(ctx);
        return 0;
    } // else if

    if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return 0;
    else
    {
        int i;
        static const char *comps[] = {"", "gt", "eq", "ge", "lt", "ne", "le"};
        for (i = 1; i < STATICARRAYLEN(comps); i++)
        {
            if (strcasecmp(ctx->token, comps[i]) == 0)
            {
                *controls = i;
                return 1;
            }
        } // for

        fail(ctx, "Expected comparison token");
        return 0;
    } // else if

    return 0;
} // parse_condition


static int parse_instruction_token(Context *ctx)
{
    int coissue = 0;
    int predicated = 0;

    if (strcmp(ctx->token, "+") == 0)
    {
        if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
            return FAIL;
        coissue = 1;
    } // if

    if (coissue)
    {
        if (!shader_is_pixel(ctx))
            return fail(ctx, "coissue instruction on non-pixel shader");
        else if (shader_version_atleast(ctx, 2, 0))
            return fail(ctx, "coissue instruction in Shader Model >= 2.0");
    } // if

    int i;
    int valid_opcode = 0;
    const Instruction *instruction = NULL;
    for (i = 0; i < STATICARRAYLEN(instructions); i++)
    {
        instruction = &instructions[i];
        if (instruction->opcode_string == NULL)
            continue;  // skip this.
        else if (strcasecmp(ctx->token, instruction->opcode_string) != 0)
            continue;  // not us.
        valid_opcode = 1;
        break;
    } // for

    uint32 opcode = (uint32) i;
    uint32 controls = 0;

    if (!valid_opcode)
        return failf(ctx, "Unknown instruction '%s'", ctx->token);

    // This might need to be IFC instead of IF.
    if (strcmp(instruction->opcode_string, "IF") == 0)
    {
        if (parse_condition(ctx, &controls))
            opcode = OPCODE_IFC;
    } // if

    // This might need to be BREAKC instead of BREAK.
    else if (strcmp(instruction->opcode_string, "BREAK") == 0)
    {
        if (parse_condition(ctx, &controls))
            opcode = OPCODE_BREAKC;
    } // else if

    // SETP has a conditional code, always.
    else if (strcmp(instruction->opcode_string, "SETP") == 0)
    {
        if (!parse_condition(ctx, &controls))
            return fail(ctx, "SETP requires a condition");
    } // else if

    instruction = &instructions[opcode];  // ...in case this changed.

    if ((ctx->shader_type & instruction->shader_types) == 0)
    {
        return failf(ctx, "opcode '%s' not available in this shader type.",
                     instruction->opcode_string);
    } // if

    // !!! FIXME: predicated instructions

    ctx->tokenbufpos = 0;

    const int tokcount = instruction->parse_args(ctx);
    if (isfail(ctx))
        return FAIL;

    if (instruction->state != NULL)
        instruction->state(ctx);

    if (isfail(ctx))
        return FAIL;

    ctx->instruction_count += instruction->slots;
    const uint32 insttoks = shader_version_atleast(ctx, 2, 0) ? tokcount : 0;

    // write out the instruction token.
    output_token(ctx, ((opcode & 0xFFFF) << 0) |
                      ((controls & 0xFF) << 16) |
                      ((insttoks & 0xF) << 24) |
                      ((coissue) ? 0x40000000 : 0x00000000) |
                      ((predicated) ? 0x10000000 : 0x00000000) );

    // write out the argument tokens.
    for (i = 0; i < (tokcount-1); i++)
        output_token(ctx, ctx->tokenbuf[i]);

    return NOFAIL;
} // parse_instruction_token


static int parse_version_token(Context *ctx)
{
    if (nexttoken(ctx, 1, 1, 0, 0) == FAIL)
        return FAIL;

    uint32 shader_type = 0;

    const char *t = ctx->token;
    switch (t[0])
    {
        case 'v':
            shader_type = 0xFFFE;
            ctx->shader_type = MOJOSHADER_TYPE_VERTEX;
            break;
        case 'p':
            shader_type = 0xFFFF;
            ctx->shader_type = MOJOSHADER_TYPE_PIXEL;
            break;
        // !!! FIXME: geometry shaders?
        default: return fail(ctx, "Expected version string");
    } // switch

    if ((t[1] != 's') || (t[2] != '_') || (t[4] != '_'))
        return fail(ctx, "Expected version string");

    ctx->major_ver = t[3] - '0';

    const char *minstr = &t[5];
    if (strcasecmp(minstr, "x") == 0)
        ctx->minor_ver = 1;
    else if (strcasecmp(minstr, "sw") == 0)
        ctx->minor_ver = 255;
    else if (strcmp(minstr, "0") == 0)
        ctx->minor_ver = 0;
    else
    {
        //minor = atoi(minstr);
        char *endptr = NULL;
        const long val = strtol(minstr, &endptr, 10);
        ctx->minor_ver = (uint8) val;
        if ((*minstr == '\0') || (*endptr != '\0') || (val < 0) || (val > 255))
            return fail(ctx, "Invalid version string");
    } // else

    // !!! FIXME: 1.x and 4.x?
    if ((ctx->major_ver < 2) || (ctx->major_ver > MAX_SHADER_MAJOR))
        return fail(ctx, "Unsupported shader model");

    if (require_endline(ctx) == FAIL)
        return FAIL;

    output_token(ctx, (((uint32) shader_type) << 16) |
                      (((uint32) ctx->major_ver) << 8) |
                      (((uint32) ctx->minor_ver) << 0) );
    return NOFAIL;
} // parse_version_token


static int parse_phase_token(Context *ctx)
{
    if (require_endline(ctx) == FAIL)
        return FAIL;

    // !!! FIXME: needs state; allow only one phase token per shader, I think?
    if ( (!shader_is_pixel(ctx)) || (!shader_version_exactly(ctx, 1, 4)) )
        return fail(ctx, "phase token only available in 1.4 pixel shaders");
    output_token(ctx, 0x0000FFFD); // phase token always 0x0000FFFD.
    return NOFAIL;
} // parse_phase_token


static int parse_end_token(Context *ctx)
{
    if (require_endline(ctx) == FAIL)
        return FAIL;

    // We don't emit the end token bits here, since it's valid for a shader
    //  to not specify an "end" string at all; it's implicit, in that case.
    // Instead, we make sure if we see "end" that it's the last thing we see.
    if (nexttoken(ctx, 1, 1, 0, 1) != END_OF_STREAM)
        return fail(ctx, "Content after END token");
    return NOFAIL;
} // parse_end_token


static int parse_token(Context *ctx)
{
    const char *t = ctx->token;
    if (strcasecmp(t, "end") == 0)
        return parse_end_token(ctx);
    else if (strcasecmp(t, "phase") == 0)
        return parse_phase_token(ctx);
    return parse_instruction_token(ctx);
} // parse_token


static Context *build_context(const char *source, MOJOSHADER_malloc m,
                              MOJOSHADER_free f, void *d)
{
    if (m == NULL) m = internal_malloc;
    if (f == NULL) f = internal_free;

    Context *ctx = (Context *) m(sizeof (Context), d);
    if (ctx == NULL)
        return NULL;

    memset(ctx, '\0', sizeof (Context));
    ctx->malloc = m;
    ctx->free = f;
    ctx->malloc_data = d;
    ctx->source = source;
    ctx->linenum = 1;

    return ctx;
} // build_context


static void destroy_context(Context *ctx)
{
    if (ctx != NULL)
    {
        MOJOSHADER_free f = ((ctx->free != NULL) ? ctx->free : internal_free);
        void *d = ctx->malloc_data;
        if ((ctx->failstr != NULL) && (ctx->failstr != out_of_mem_str))
            f((void *) ctx->failstr, d);
        f(ctx, d);
    } // if
} // destroy_context



// API entry point...

const MOJOSHADER_assembleData *MOJOSHADER_assemble(const char *source,
                            MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    MOJOSHADER_assembleData *retval = NULL;
    Context *ctx = NULL;

    if ( ((m == NULL) && (f != NULL)) || ((m != NULL) && (f == NULL)) )
        return &out_of_mem_data;  // supply both or neither.

    ctx = build_context(source, m, f, d);
    if (ctx == NULL)
        return &out_of_mem_data;

    // Version token always comes first.
    parse_version_token(ctx);

    const char *credit = "Generated by MojoShader assembler revision "
                         MOJOSHADER_CHANGESET
                         ", http://icculus.org/mojoshader/";
    output_comment_string(ctx, credit);

    // !!! FIXME: insert CTAB here.

    // parse out the rest of the tokens after the version token...
    while (nexttoken(ctx, 1, 1, 0, 1) == NOFAIL)
        parse_token(ctx);

    output_token(ctx, 0x0000FFFF);   // end token always 0x0000FFFF.

    retval = build_assembledata(ctx);
    destroy_context(ctx);
    return retval;
} // MOJOSHADER_assemble


void MOJOSHADER_freeAssembleData(const MOJOSHADER_assembleData *_data)
{
    MOJOSHADER_assembleData *data = (MOJOSHADER_assembleData *) _data;
    if ((data == NULL) || (data == &out_of_mem_data))
        return;  // no-op.

    MOJOSHADER_free f = (data->free == NULL) ? internal_free : data->free;
    void *d = data->malloc_data;

    if (data->output != NULL)  // check for NULL in case of dumb free() impl.
        f((void *) data->output, d);

    if ((data->error != NULL) && (data->error != out_of_mem_str))
        f((void *) data->error, d);

    f(data, d);
} // MOJOSHADER_freeParseData

// end of mojoshader_assembler.c ...

