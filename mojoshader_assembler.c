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

#define DEBUG_TOKENIZER 0

// !!! FIXME: no #define support yet.

typedef struct TokenizerContext
{
    const char *source;
    int on_endline;
    unsigned int linenum;
    char prevchar;
    char token[64];
    char pushedback;
} TokenizerContext;


typedef struct Context Context;

// Context...this is state that changes as we assemble a shader...
struct Context
{
    MOJOSHADER_malloc malloc;
    MOJOSHADER_free free;
    void *malloc_data;
    const char *failstr;
    TokenizerContext tctx;
    int started_parsing;
    MOJOSHADER_shaderType shader_type;
    uint8 major_ver;
    uint8 minor_ver;
    uint32 tokenbuf[16];
    int tokenbufpos;
    DestArgInfo dest_arg;
    uint32 *output;
    uint32 *token_to_line;
    size_t output_len;
    size_t output_allocation;
};


// Convenience functions for allocators...

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


static inline int tokeq(const TokenizerContext *tctx, const char *token)
{
    return (strcasecmp(tctx->token, token) == 0);
} // tokeq


// Shader model version magic...

static inline uint32 ver_ui32(const uint8 major, const uint8 minor)
{
    return ( (((uint32) major) << 16) | (((minor) == 0xFF) ? 0 : (minor)) );
} // version_ui32

static inline int shader_version_atleast(const Context *ctx, const uint8 maj,
                                         const uint8 min)
{
    return (ver_ui32(ctx->major_ver, ctx->minor_ver) >= ver_ui32(maj, min));
} // shader_version_atleast

static inline int shader_is_pixel(const Context *ctx)
{
    return (ctx->shader_type == MOJOSHADER_TYPE_PIXEL);
} // shader_is_pixel

static inline int shader_is_vertex(const Context *ctx)
{
    return (ctx->shader_type == MOJOSHADER_TYPE_VERTEX);
} // shader_is_vertex


static int ui32fromstr(const char *str, uint32 *ui32)
{
    //*ui32 = (uint32) atoi(minstr);
    char *endptr = NULL;
    const long val = strtol(str, &endptr, 10);
    *ui32 = (uint32) val;
    return ((val >= 0) && (*str != '\0') && (*endptr == '\0'));
} // ui32fromstr


static void output_token_noswap(Context *ctx, const uint32 token)
{
    if (isfail(ctx))
        return;

    if (ctx->output_len >= ctx->output_allocation)
    {
        const size_t output_alloc_bump = 1024;  // that's tokens, not bytes.
        const size_t newsize = ctx->output_allocation + output_alloc_bump;
        void *ptr;

        ptr = Malloc(ctx, newsize * sizeof (uint32));
        if (ptr == NULL)
            return;
        if (ctx->output_len > 0)
            memcpy(ptr, ctx->output, ctx->output_len * sizeof (uint32));
        Free(ctx, ctx->output);
        ctx->output = (uint32 *) ptr;

        ptr = Malloc(ctx, newsize * sizeof (uint32));
        if (ptr == NULL)
            return;
        if (ctx->output_len > 0)
            memcpy(ptr, ctx->token_to_line, ctx->output_len * sizeof (uint32));
        Free(ctx, ctx->token_to_line);
        ctx->token_to_line = (uint32 *) ptr;

        ctx->output_allocation = newsize;
    } // if

    ctx->output[ctx->output_len] = token;
    ctx->token_to_line[ctx->output_len] = ctx->tctx.linenum;
    ctx->output_len++;
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


static int tokenize_ctx(Context *ctx, TokenizerContext *tctx)
{
    int idx = 0;

    if (isfail(ctx))
        return FAIL;

    if (tctx->pushedback)
    {
        tctx->pushedback = 0;
        return NOFAIL;
    } // if

    if (tctx->on_endline)
    {
        tctx->on_endline = 0;
        tctx->linenum++;  // passed a newline, update.
    } // if

    while (1)
    {
        // !!! FIXME: carefully crafted (but legal) comments can trigger this.
        if (idx >= sizeof (tctx->token))
            return fail(ctx, "buffer overflow");

        char ch = *tctx->source;
        if (ch == '\t')
            ch = ' ';  // collapse tabs into single spaces.
        else if (ch == '\r')
        {
            if (tctx->source[1] == '\n')
               continue;  // ignore '\r' if this is "\r\n" ...
            ch = '\n';
        } // else if

        if ((ch >= '0') && (ch <= '9'))
        {
            // starting a number, but rest of current token was not number.
            if ((idx > 0) && ((tctx->prevchar < '0') || (tctx->prevchar > '9')))
            {
                tctx->token[idx++] = '\0';
                return NOFAIL;
            } // if
        } // if
        else
        {
            // starting a non-number, but rest of current token was numbers.
            if ((idx > 0) && ((tctx->prevchar >= '0') && (tctx->prevchar <= '9')))
            {
                tctx->token[idx++] = '\0';
                return NOFAIL;
            } // if
        } // else

        switch (ch)
        {
            case '/':
            case ';':  // !!! FIXME: comment, right?
                if (idx != 0)  // finish off existing token.
                    tctx->token[idx] = '\0';
                else
                {
                    tctx->token[idx++] = ch;
                    tctx->source++;
                    if ((ch == '/') && (*tctx->source == '/'))
                    {
                        tctx->token[idx++] = '/';
                        tctx->source++;
                    } // if
                    tctx->token[idx++] = '\0';
                } // else
                return NOFAIL;

            case ' ':
                if (tctx->prevchar == ' ')
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
                    tctx->token[idx] = '\0';
                else  // this is a token in itself.
                {
                    if (ch == '\n')
                        tctx->on_endline = 1;
                    tctx->source++;
                    tctx->token[idx++] = ch;
                    tctx->token[idx++] = '\0';
                } // else
                return NOFAIL;

            case '\0':
                tctx->token[idx] = '\0';
                if (idx != 0)  // had any chars? It's a token.
                    return NOFAIL;
                return END_OF_STREAM;

            default:
                tctx->source++;
                tctx->token[idx++] = ch;
                break;
        } // switch

        tctx->prevchar = ch;
    } // while

    return fail(ctx, "???");  // shouldn't hit this.
} // tokenize_ctx


static inline int tokenize(Context *ctx)
{
    const int rc = tokenize_ctx(ctx, &ctx->tctx);

    #if DEBUG_TOKENIZER
    printf("TOKENIZE: %s '%s'\n",
           (rc == END_OF_STREAM) ? "END_OF_STREAM" :
           (rc == FAIL) ? "FAIL" :
           (rc == NOFAIL) ? "NOFAIL" : "???",
           (ctx->tctx.token[0] == '\n') ? "\\n" : ctx->tctx.token);
    #endif

    return rc;
} // tokenize


static int pushback_ctx(Context *ctx, TokenizerContext *tctx)
{
    if (tctx->pushedback)
        return fail(ctx, "BUG: Double pushback in parser");
    else
        tctx->pushedback = 1;

    return NOFAIL;
}

static inline int pushback(Context *ctx)
{
    const int rc = pushback_ctx(ctx, &ctx->tctx);

    #if DEBUG_TOKENIZER
    printf("PUSHBACK: %s\n",
           (rc == END_OF_STREAM) ? "END_OF_STREAM" :
           (rc == FAIL) ? "FAIL" :
           (rc == NOFAIL) ? "NOFAIL" : "???");
    #endif

    return rc;
} // pushback


static int nexttoken_ctx(Context *ctx, TokenizerContext *tctx,
                     const int ignoreeol, const int ignorewhitespace,
                     const int eolok, const int eosok)
{
    int rc = NOFAIL;

    while ((rc = tokenize_ctx(ctx, tctx)) == NOFAIL)
    {
        if (tokeq(tctx, "\n"))
        {
            if (ignoreeol)
                continue;
            else if (!eolok)
                return fail(ctx, "Unexpected EOL");
        } // if

        else if (tokeq(tctx, " "))
        {
            if (ignorewhitespace)
                continue;
        } // else if

        // skip comments...
        else if (tokeq(tctx, "//") || tokeq(tctx, ";"))
        {
            while ((rc = tokenize_ctx(ctx, tctx)) == NOFAIL)
            {
                if (tokeq(tctx, "\n"))
                {
                    pushback_ctx(ctx, tctx);
                    break;
                } // if
            } // while
            continue;  // pick up from newline, go again.
        } // if

        break;
    } // while

    if ((rc == END_OF_STREAM) && (!eosok))
        return fail(ctx, "Unexpected EOF");

    return rc;
} // nexttoken_ctx


static inline int nexttoken(Context *ctx, const int ignoreeol,
                     const int ignorewhitespace, const int eolok,
                     const int eosok)
{
    const int rc = nexttoken_ctx(ctx, &ctx->tctx, ignoreeol,
                                 ignorewhitespace, eolok, eosok);

    #if DEBUG_TOKENIZER
    printf("NEXTTOKEN: %s '%s'\n",
           (rc == END_OF_STREAM) ? "END_OF_STREAM" :
           (rc == FAIL) ? "FAIL" :
           (rc == NOFAIL) ? "NOFAIL" : "???",
           (ctx->tctx.token[0] == '\n') ? "\\n" : ctx->tctx.token);
    #endif

    return rc;
} // nexttoken


static int require_endline(Context *ctx)
{
    TokenizerContext *tctx = &ctx->tctx;
    const int rc = nexttoken(ctx, 0, 1, 1, 1);
    if (rc == FAIL)
        return FAIL;
    else if (rc == END_OF_STREAM)
        return NOFAIL;  // we'll call this an EOL.
    else if (!tokeq(tctx, "\n"))
        return fail(ctx, "Endline expected");
    return NOFAIL;
} // require_endline


static int require_comma(Context *ctx)
{
    TokenizerContext *tctx = &ctx->tctx;
    const int rc = nexttoken(ctx, 0, 1, 0, 0);
    if (rc == FAIL)
        return FAIL;
    else if (!tokeq(tctx, ","))
        return fail(ctx, "Comma expected");
    return NOFAIL;
} // require_comma


static int parse_register_name(Context *ctx, RegisterType *rtype, int *rnum)
{
    TokenizerContext *tctx = &ctx->tctx;
    if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
        return FAIL;

    // !!! FIXME: some of these registers are only valid for some shader types.
    int neednum = 1;
    int regnum = 0;
    RegisterType regtype = REG_TYPE_TEMP;
    if (tokeq(tctx, "r"))
        regtype = REG_TYPE_TEMP;
    else if (tokeq(tctx, "v"))
        regtype = REG_TYPE_INPUT;
    else if (tokeq(tctx, "c"))
        regtype = REG_TYPE_CONST;
    else if (tokeq(tctx, "i"))
        regtype = REG_TYPE_CONSTINT;
    else if (tokeq(tctx, "b"))
        regtype = REG_TYPE_CONSTBOOL;
    else if (tokeq(tctx, "oC"))
        regtype = REG_TYPE_COLOROUT;
    else if (tokeq(tctx, "s"))
        regtype = REG_TYPE_SAMPLER;
    else if (tokeq(tctx, "oD"))
        regtype = REG_TYPE_ATTROUT;
    else if (tokeq(tctx, "l"))
        regtype = REG_TYPE_LABEL;
    else if (tokeq(tctx, "p"))
        regtype = REG_TYPE_PREDICATE;
    else if (tokeq(tctx, "oDepth"))
    {
        regtype = REG_TYPE_DEPTHOUT;
        neednum = 0;
    } // else if
    else if (tokeq(tctx, "aL"))
    {
        regtype = REG_TYPE_LOOP;
        neednum = 0;
    } // else if
    else if (tokeq(tctx, "o"))
    {
        if (!shader_is_vertex(ctx) || !shader_version_atleast(ctx, 3, 0))
            return fail(ctx, "Output register not valid in this shader type");
        regtype = REG_TYPE_OUTPUT;
    } // else if
    else if (tokeq(tctx, "oT"))
    {
        if (shader_is_vertex(ctx) && shader_version_atleast(ctx, 3, 0))
            return fail(ctx, "Output register not valid in this shader type");
        regtype = REG_TYPE_OUTPUT;
    } // else if
    else if (tokeq(tctx, "a"))
    {
        if (!shader_is_vertex(ctx))
            return fail(ctx, "Address register only valid in vertex shaders.");
        regtype = REG_TYPE_ADDRESS;
    } // else if
    else if (tokeq(tctx, "t"))
    {
        if (!shader_is_pixel(ctx))
            return fail(ctx, "Address register only valid in pixel shaders.");
        regtype = REG_TYPE_ADDRESS;
    } // else if
    else if (tokeq(tctx, "vPos"))
    {
        regtype = REG_TYPE_MISCTYPE;
        regnum = (int) MISCTYPE_TYPE_POSITION;
        neednum = 0;
    } // else if
    else if (tokeq(tctx, "vFace"))
    {
        regtype = REG_TYPE_MISCTYPE;
        regnum = (int) MISCTYPE_TYPE_FACE;
        neednum = 0;
    } // else if
    else if (tokeq(tctx, "oPos"))
    {
        regtype = REG_TYPE_RASTOUT;
        regnum = (int) RASTOUT_TYPE_POSITION;
        neednum = 0;
    } // else if
    else if (tokeq(tctx, "oFog"))
    {
        regtype = REG_TYPE_RASTOUT;
        regnum = (int) RASTOUT_TYPE_FOG;
        neednum = 0;
    } // else if
    else if (tokeq(tctx, "oPts"))
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
        // Make a temp TokenizerContext, since we need to skip whitespace here,
        //  but if the next non-whitespace token isn't '[', we'll want to get
        //  that whitespace back.
        TokenizerContext tmptctx;
        memcpy(&tmptctx, tctx, sizeof (TokenizerContext));
        if (nexttoken_ctx(ctx, &tmptctx, 0, 1, 1, 1) == FAIL)
            return FAIL;
        else if (tokeq(&ctx->tctx, "["))
            neednum = 0;
    } // if

    if (neednum)
    {
        if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
            return FAIL;

        uint32 ui32 = 0;
        if (!ui32fromstr(tctx->token, &ui32))
            return fail(ctx, "Invalid register index");
        regnum = (int) ui32;
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


static int set_result_shift(Context *ctx, DestArgInfo *info, const int val)
{
    if (info->result_shift != 0)
        return fail(ctx, "Multiple result shift modifiers");
    info->result_shift = val;
    return NOFAIL;
} // set_result_shift


static int parse_destination_token(Context *ctx)
{
    TokenizerContext *tctx = &ctx->tctx;

    DestArgInfo *info = &ctx->dest_arg;
    memset(info, '\0', sizeof (DestArgInfo));

    // See if there are destination modifiers on the instruction itself...
    while (1)
    {
        if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
            return FAIL;
        else if (tokeq(tctx, " "))
            break;  // done with modifiers.
        else if (!tokeq(tctx, "_"))
            return fail(ctx, "Expected modifier or whitespace");
        else if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
            return FAIL;
        // !!! FIXME: this can be cleaned up when tokenizer is fixed.
        else if (tokeq(tctx, "x"))
        {
            if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
                return FAIL;
            else if (tokeq(tctx, "2"))
                set_result_shift(ctx, info, 0x1);
            else if (tokeq(tctx, "4"))
                set_result_shift(ctx, info, 0x2);
            else if (tokeq(tctx, "8"))
                set_result_shift(ctx, info, 0x3);
            else
                return fail(ctx, "Expected modifier");
        } // else if
        // !!! FIXME: this can be cleaned up when tokenizer is fixed.
        else if (tokeq(tctx, "d"))
        {
            if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
                return FAIL;
            else if (tokeq(tctx, "8"))
                set_result_shift(ctx, info, 0xD);
            else if (tokeq(tctx, "4"))
                set_result_shift(ctx, info, 0xE);
            else if (tokeq(tctx, "2"))
                set_result_shift(ctx, info, 0xF);
            else
                return fail(ctx, "Expected modifier");
        } // else if
        else if (tokeq(tctx, "sat"))
            info->result_mod |= MOD_SATURATE;
        else if (tokeq(tctx, "pp"))
            info->result_mod |= MOD_PP;
        else if (tokeq(tctx, "centroid"))
            info->result_mod |= MOD_CENTROID;
        else
            return fail(ctx, "Expected modifier");
    } // while

    if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
        return FAIL;

    // !!! FIXME: predicates.
    if (tokeq(tctx, "("))
        return fail(ctx, "Predicates unsupported at this time");
    pushback(ctx);  // parse_register_name calls nexttoken().

    if (parse_register_name(ctx, &info->regtype, &info->regnum) == FAIL)
        return FAIL;

    if (nexttoken(ctx, 0, 1, 1, 1) == FAIL)
        return FAIL;

    // !!! FIXME: can dest registers do relative addressing?

    if (!tokeq(tctx, "."))
    {
        info->writemask = 0xF;
        info->writemask0 = info->writemask1 = info->writemask2 = info->writemask3 = 1;
        pushback(ctx);  // no explicit writemask; do full mask.
    } // if
    else if (scalar_register(ctx->shader_type, info->regtype, info->regnum))
        return fail(ctx, "Writemask specified for scalar register");
    else if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
        return FAIL;
    else if (tokeq(tctx, ""))
        return fail(ctx, "Invalid writemask");
    else
    {
        char *ptr = tctx->token;
        info->writemask0 = info->writemask1 = info->writemask2 = info->writemask3 = 0;
        if (*ptr == 'x') { info->writemask0 = 1; ptr++; }
        if (*ptr == 'y') { info->writemask1 = 1; ptr++; }
        if (*ptr == 'z') { info->writemask2 = 1; ptr++; }
        if (*ptr == 'w') { info->writemask3 = 1; ptr++; }
        if ((ptr == tctx->token) && (shader_is_pixel(ctx)))
        {
            if (*ptr == 'r') { info->writemask0 = 1; ptr++; }
            if (*ptr == 'g') { info->writemask1 = 1; ptr++; }
            if (*ptr == 'b') { info->writemask2 = 1; ptr++; }
            if (*ptr == 'a') { info->writemask3 = 1; ptr++; }
        } // if

        if (*ptr != '\0')
            return fail(ctx, "Invalid writemask");

        info->writemask = ( ((info->writemask0 & 0x1) << 0) |
                            ((info->writemask1 & 0x1) << 1) |
                            ((info->writemask2 & 0x1) << 2) |
                            ((info->writemask3 & 0x1) << 3) );
    } // else

    info->orig_writemask = info->writemask;

    if (ctx->tokenbufpos >= STATICARRAYLEN(ctx->tokenbuf))
        return fail(ctx, "Too many tokens");

    ctx->tokenbuf[ctx->tokenbufpos++] =
            ( ((((uint32) 1)) << 31) |
              ((((uint32) info->regnum) & 0x7ff) << 0) |
              ((((uint32) info->relative) & 0x1) << 13) |
              ((((uint32) info->result_mod) & 0xF) << 20) |
              ((((uint32) info->result_shift) & 0xF) << 24) |
              ((((uint32) info->writemask) & 0xF) << 16) |
              ((((uint32) info->regtype) & 0x7) << 28) |
              ((((uint32) info->regtype) & 0x18) << 8) );

    return 1;
} // parse_destination_token


static void set_source_mod(Context *ctx, const int negate,
                           const SourceMod norm, const SourceMod negated,
                           SourceMod *srcmod)
{
    if ( (*srcmod != SRCMOD_NONE) || (negate && (negated == SRCMOD_NONE)) )
        fail(ctx, "Incompatible source modifiers");
    else
        *srcmod = ((negate) ? negated : norm);
} // set_source_mod


static int parse_source_token_maybe_relative(Context *ctx, const int relok)
{
    TokenizerContext *tctx = &ctx->tctx;
    int retval = 1;

    if (ctx->tokenbufpos >= STATICARRAYLEN(ctx->tokenbuf))
        return fail(ctx, "Too many tokens");

    // mark this now, so optional relative addressing token is placed second.
    uint32 *token = &ctx->tokenbuf[ctx->tokenbufpos++];

    SourceMod srcmod = SRCMOD_NONE;
    int negate = 0;
    if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
        return FAIL;
    else if (tokeq(tctx, "1"))
    {
        if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
            return FAIL;
        else if (!tokeq(tctx, "-"))
            return fail(ctx, "Unexpected value");
        else
            srcmod = SRCMOD_COMPLEMENT;
    } // else
    else if (tokeq(tctx, "!"))
        srcmod = SRCMOD_NOT;
    else if (tokeq(tctx, "-"))
        negate = 1;
    else
        pushback(ctx);

    RegisterType regtype;
    int regnum;
    if (parse_register_name(ctx, &regtype, &regnum) == FAIL)
        return FAIL;
    else if (nexttoken(ctx, 0, 1, 1, 1) == FAIL)
        return FAIL;
    else if (!tokeq(tctx, "_"))
        pushback(ctx);
    else if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return FAIL;
    else if (tokeq(tctx, "bias"))
        set_source_mod(ctx, negate, SRCMOD_BIAS, SRCMOD_BIASNEGATE, &srcmod);
    else if (tokeq(tctx, "bx2"))
        set_source_mod(ctx, negate, SRCMOD_SIGN, SRCMOD_SIGNNEGATE, &srcmod);
    else if (tokeq(tctx, "x2"))
        set_source_mod(ctx, negate, SRCMOD_X2, SRCMOD_X2NEGATE, &srcmod);
    else if (tokeq(tctx, "dz"))
        set_source_mod(ctx, negate, SRCMOD_DZ, SRCMOD_NONE, &srcmod);
    else if (tokeq(tctx, "dw"))
        set_source_mod(ctx, negate, SRCMOD_DW, SRCMOD_NONE, &srcmod);
    else if (tokeq(tctx, "abs"))
        set_source_mod(ctx, negate, SRCMOD_ABS, SRCMOD_ABSNEGATE, &srcmod);
    else
        return fail(ctx, "Invalid source modifier");

    if (nexttoken(ctx, 0, 1, 1, 1) == FAIL)
        return FAIL;

    uint32 relative = 0;
    if (!tokeq(tctx, "["))
        pushback(ctx);  // not relative addressing?
    else if (!relok)
        return fail(ctx, "Relative addressing not permitted here.");
    else
    {
        const int rc = parse_source_token_maybe_relative(ctx, 0);
        if (rc == FAIL)
            return FAIL;
        retval += rc;
        relative = 1;
        if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
            return FAIL;
        else if (!tokeq(tctx, "+"))
            pushback(ctx);
        else if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
            return FAIL;
        else
        {
            if (regnum != 0)  // !!! FIXME: maybe c3[a0.x + 5] is legal and becomes c[a0.x + 8] ?
                fail(ctx, "Relative addressing with explicit register number.");
            uint32 ui32 = 0;
            if (!ui32fromstr(tctx->token, &ui32))
                return fail(ctx, "Invalid relative addressing offset");
            regnum += (int) ui32;
        } // else

        if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
            return FAIL;
        else if (!tokeq(tctx, "]"))
            return fail(ctx, "Expected ']'");
    } // else

    if (nexttoken(ctx, 0, 1, 1, 1) == FAIL)
        return FAIL;

    uint32 swizzle = 0;
    if (!tokeq(tctx, "."))
    {
        swizzle = 0xE4;  // 0xE4 == 11100100 ... 0 1 2 3. No swizzle.
        pushback(ctx);  // no explicit writemask; do full mask.
    } // if
    else if (scalar_register(ctx->shader_type, regtype, regnum))
        return fail(ctx, "Swizzle specified for scalar register");
    else if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
        return FAIL;
    else if (tokeq(tctx, ""))
        return fail(ctx, "Invalid swizzle");
    else
    {
        // deal with shortened form (.x = .xxxx, etc).
        if (tctx->token[1] == '\0')
            tctx->token[1] = tctx->token[2] = tctx->token[3] = tctx->token[0];
        else if (tctx->token[2] == '\0')
            tctx->token[2] = tctx->token[3] = tctx->token[1];
        else if (tctx->token[3] == '\0')
            tctx->token[3] = tctx->token[2];
        else if (tctx->token[4] != '\0')
            return fail(ctx, "Invalid swizzle");
        tctx->token[4] = '\0';

        uint32 val;
        int saw_xyzw = 0;
        int saw_rgba = 0;
        int i;
        for (i = 0; i < 4; i++)
        {
            const int component = (int) tctx->token[i];
            switch (component)
            {
                case 'x': val = 0; saw_xyzw = 1; break;
                case 'y': val = 1; saw_xyzw = 1; break;
                case 'z': val = 2; saw_xyzw = 1; break;
                case 'w': val = 3; saw_xyzw = 1; break;
                case 'r': val = 0; saw_rgba = 1; break;
                case 'g': val = 1; saw_rgba = 1; break;
                case 'b': val = 2; saw_rgba = 1; break;
                case 'a': val = 3; saw_rgba = 1; break;
                default: return fail(ctx, "Invalid swizzle");
            } // switch
            swizzle |= (val << (i * 2));
        } // for

        if (saw_xyzw && saw_rgba)
            return fail(ctx, "Invalid swizzle");
    } // else

    *token = ( ((((uint32) 1)) << 31) |
               ((((uint32) regnum) & 0x7ff) << 0) |
               ((((uint32) relative) & 0x1) << 13) |
               ((((uint32) swizzle) & 0xFF) << 16) |
               ((((uint32) srcmod) & 0xF) << 24) |
               ((((uint32) regtype) & 0x7) << 28) |
               ((((uint32) regtype) & 0x18) << 8) );

    return retval;
} // parse_source_token_maybe_relative


static inline int parse_source_token(Context *ctx)
{
    return parse_source_token_maybe_relative(ctx, 1);
} // parse_source_token


static int parse_args_NULL(Context *ctx)
{
    return (isfail(ctx) ? FAIL : 1);
} // parse_args_NULL


static int parse_num(Context *ctx, const int floatok, uint32 *token)
{
    TokenizerContext *tctx = &ctx->tctx;
    int32 negative = 1;
    union { float f; int32 si32; uint32 ui32; } cvt;
    cvt.si32 = 0;

    if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
        return FAIL;
    else if (tokeq(tctx, "-"))
        negative = -1;
    else
        pushback(ctx);

    uint32 val = 0;
    if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
        return FAIL;
    else if (!ui32fromstr(tctx->token, &val))
        return fail(ctx, "Expected number");

    uint32 fraction = 0;
    if (nexttoken(ctx, 0, 1, 1, 1) == FAIL)
        return FAIL;
    else if (!tokeq(tctx, "."))
        pushback(ctx);  // whole number
    else if (!floatok)
        return fail(ctx, "Expected whole number");
    else if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
        return FAIL;
    else if (!ui32fromstr(tctx->token, &fraction))
        return fail(ctx, "Expected number");

    uint32 exponent = 0;
    int negexp = 0;
    if (nexttoken(ctx, 0, 1, 1, 1) == FAIL)
        return FAIL;
    else if (!tokeq(tctx, "e"))
        pushback(ctx);
    else if (!floatok)
        return fail(ctx, "Exponent on whole number");  // !!! FIXME: illegal?
    else if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
        return FAIL;
    else
    {
        if (!tokeq(tctx, "-"))
            pushback(ctx);
        else
            negexp = 1;

        if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
            return FAIL;
        else if (!ui32fromstr(tctx->token, &exponent))
            return fail(ctx, "Expected exponent");
    } // else

    if (!floatok)
        cvt.si32 = ((int32) val) * negative;
    else
    {
        // !!! FIXME: this is lame.
        char buf[128];
        snprintf(buf, sizeof (buf), "%s%u.%u", (negative < 0) ? "-" : "",
                 (uint) val, (uint) fraction);
        sscanf(buf, "%f", &cvt.f);
        cvt.f *= (float) negative;

        if (exponent)
        {
            int i;
            if (negexp)
            {
                for (i = 0; i > exponent; i--)
                    cvt.f /= 10.0f;
            } // if
            else
            {
                for (i = 0; i < exponent; i++)
                    cvt.f *= 10.0f;
            } // else
        } // if
    } // else

    *token = cvt.ui32;
    return NOFAIL;
} // parse_num


static int parse_args_DEFx(Context *ctx, const int isflt)
{
    if (parse_destination_token(ctx) == FAIL)
        return FAIL;
    else if (require_comma(ctx) == FAIL)
        return FAIL;
    else if (parse_num(ctx, isflt, &ctx->tokenbuf[ctx->tokenbufpos++]) == FAIL)
        return FAIL;
    else if (require_comma(ctx) == FAIL)
        return FAIL;
    else if (parse_num(ctx, isflt, &ctx->tokenbuf[ctx->tokenbufpos++]) == FAIL)
        return FAIL;
    else if (require_comma(ctx) == FAIL)
        return FAIL;
    else if (parse_num(ctx, isflt, &ctx->tokenbuf[ctx->tokenbufpos++]) == FAIL)
        return FAIL;
    else if (require_comma(ctx) == FAIL)
        return FAIL;
    else if (parse_num(ctx, isflt, &ctx->tokenbuf[ctx->tokenbufpos++]) == FAIL)
        return FAIL;
    return 6;
} // parse_args_DEFx


static int parse_args_DEF(Context *ctx)
{
    return parse_args_DEFx(ctx, 1);
} // parse_args_DEF


static int parse_args_DEFI(Context *ctx)
{
    return parse_args_DEFx(ctx, 0);
} // parse_args_DEFI


static int parse_args_DEFB(Context *ctx)
{
    TokenizerContext *tctx = &ctx->tctx;
    if (parse_destination_token(ctx) == FAIL)
        return FAIL;
    else if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
        return FAIL;
    else if (!tokeq(tctx, ","))
        return fail(ctx, "Expected ','");
    else if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
        return FAIL;
    else if (tokeq(tctx, "true"))
        ctx->tokenbuf[ctx->tokenbufpos++] = 1;
    else if (tokeq(tctx, "false"))
        ctx->tokenbuf[ctx->tokenbufpos++] = 0;
    else
        return fail(ctx, "Expected 'true' or 'false'");
    return 3;
} // parse_args_DEFB


static int parse_dcl_usage(Context *ctx, uint32 *val, int *issampler)
{
    TokenizerContext *tctx = &ctx->tctx;
    int i;
    static const char *samplerusagestrs[] = { "2d", "cube", "volume" };
    static const char *usagestrs[] = {
        "position", "blendweight", "blendindices", "normal", "psize",
        "texcoord", "tangent", "binormal", "tessfactor", "positiont",
        "color", "fog", "depth", "sample"
    };
    static const char *ignorestrs[] = { "pp", "centroid", "saturate" };

    // !!! FIXME: we need to clean this out in the tokenizer.
    char token[sizeof (tctx->token)];
    strcpy(token, tctx->token);
    if (tokeq(tctx, "2"))  // "2d" is two tokens.
    {
        if (nexttoken(ctx, 0, 0, 1, 1) == FAIL)
            return FAIL;
        else if (!tokeq(tctx, "d") != 0)
            pushback(ctx);
        else
            strcpy(token, "2d");
    } // if

    for (i = 0; i < STATICARRAYLEN(usagestrs); i++)
    {
        if (strcasecmp(usagestrs[i], token) == 0)
        {
            *issampler = 0;
            *val = i;
            return NOFAIL;
        } // if
    } // for

    for (i = 0; i < STATICARRAYLEN(samplerusagestrs); i++)
    {
        if (strcasecmp(samplerusagestrs[i], token) == 0)
        {
            *issampler = 1;
            *val = i + 2;
            return NOFAIL;
        } // if
    } // for

    // !!! FIXME: this probably isn't the smartest way to handle this.
    *issampler = 0;
    *val = 0;
    for (i = 0; i < STATICARRAYLEN(ignorestrs); i++)
    {
        if (strcasecmp(ignorestrs[i], token) == 0)
        {
            tctx->source -= strlen(token);  // !!! FIXME: hack to move back
            strcpy(tctx->token, "_");  // !!! FIXME: hack to move back
            pushback(ctx);  // !!! FIXME: hack to move back
            return NOFAIL;  // if you have "dcl_pp", then "_pp" isn't a usage.
        } // if
    } // for

    return FAIL;
} // parse_dcl_usage


static int parse_args_DCL(Context *ctx)
{
    TokenizerContext *tctx = &ctx->tctx;
    int issampler = 0;
    uint32 usage = 0;
    uint32 index = 0;

    ctx->tokenbufpos++;  // save a spot for the usage/index token.

    if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return FAIL;
    else if (tokeq(tctx, " "))
        pushback(ctx);
    else if (!tokeq(tctx, "_"))
        return fail(ctx, "Expected register or usage");
    else if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return FAIL;
    else if (parse_dcl_usage(ctx, &usage, &issampler) == FAIL)
        return FAIL;

    if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return FAIL;
    else if (tokeq(tctx, " ") || tokeq(tctx, "_"))
        pushback(ctx);  // parse_destination_token() wants these.
    else if (!ui32fromstr(tctx->token, &index))
        return fail(ctx, "Expected usage index or register");

    if (parse_destination_token(ctx) == FAIL)
        return FAIL;

    const int samplerreg = (ctx->dest_arg.regtype == REG_TYPE_SAMPLER);
    if (issampler != samplerreg)
        return fail(ctx, "Invalid usage");
    else if (samplerreg)
        ctx->tokenbuf[0] = (usage << 27) | 0x80000000;
    else
        ctx->tokenbuf[0] = usage | (index << 16) | 0x80000000;

    return 3;
} // parse_args_DCL


static int parse_args_D(Context *ctx)
{
    int retval = 1;
    retval += parse_destination_token(ctx);
    return isfail(ctx) ? FAIL : retval;
} // parse_args_D


static int parse_args_S(Context *ctx)
{
    int retval = 1;
    retval += parse_source_token(ctx);
    return isfail(ctx) ? FAIL : retval;
} // parse_args_S


static int parse_args_SS(Context *ctx)
{
    int retval = 1;
    retval += parse_source_token(ctx);
    if (require_comma(ctx) == FAIL) return FAIL;
    retval += parse_source_token(ctx);
    return isfail(ctx) ? FAIL : retval;
} // parse_args_SS


static int parse_args_DS(Context *ctx)
{
    int retval = 1;
    retval += parse_destination_token(ctx);
    if (require_comma(ctx) == FAIL) return FAIL;
    retval += parse_source_token(ctx);
    return isfail(ctx) ? FAIL : retval;
} // parse_args_DS


static int parse_args_DSS(Context *ctx)
{
    int retval = 1;
    retval += parse_destination_token(ctx);
    if (require_comma(ctx) == FAIL) return FAIL;
    retval += parse_source_token(ctx);
    if (require_comma(ctx) == FAIL) return FAIL;
    retval += parse_source_token(ctx);
    return isfail(ctx) ? FAIL : retval;
} // parse_args_DSS


static int parse_args_DSSS(Context *ctx)
{
    int retval = 1;
    retval += parse_destination_token(ctx);
    if (require_comma(ctx) == FAIL) return FAIL;
    retval += parse_source_token(ctx);
    if (require_comma(ctx) == FAIL) return FAIL;
    retval += parse_source_token(ctx);
    if (require_comma(ctx) == FAIL) return FAIL;
    retval += parse_source_token(ctx);
    return isfail(ctx) ? FAIL : retval;
} // parse_args_DSSS


static int parse_args_DSSSS(Context *ctx)
{
    int retval = 1;
    retval += parse_destination_token(ctx);
    if (require_comma(ctx) == FAIL) return FAIL;
    retval += parse_source_token(ctx);
    if (require_comma(ctx) == FAIL) return FAIL;
    retval += parse_source_token(ctx);
    if (require_comma(ctx) == FAIL) return FAIL;
    retval += parse_source_token(ctx);
    if (require_comma(ctx) == FAIL) return FAIL;
    retval += parse_source_token(ctx);
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

// Lookup table for instruction opcodes...
typedef struct
{
    const char *opcode_string;
    args_function parse_args;
} Instruction;


static const Instruction instructions[] =
{
    #define INSTRUCTION_STATE(op, opstr, s, a, t) { opstr, parse_args_##a },
    #define INSTRUCTION(op, opstr, slots, a, t) { opstr, parse_args_##a },
    #define MOJOSHADER_DO_INSTRUCTION_TABLE 1
    #include "mojoshader_internal.h"
    #undef MOJOSHADER_DO_INSTRUCTION_TABLE
    #undef INSTRUCTION
    #undef INSTRUCTION_STATE
};

static int parse_condition(Context *ctx, uint32 *controls)
{
    TokenizerContext *tctx = &ctx->tctx;
    if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return 0;
    else if (!tokeq(tctx, "_"))
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
            if (tokeq(tctx, comps[i]))
            {
                *controls = i;
                return 1;
            } // if
        } // for

        fail(ctx, "Expected comparison token");
        return 0;
    } // else if

    return 0;
} // parse_condition


static inline int valid_instruction_char(const char ch)
{
    return ( ((ch >= 'A') && (ch <= 'Z')) ||
             ((ch >= 'a') && (ch <= 'z')) ||
             ((ch >= '0') && (ch <= '9')) );
} // valid_instruction_char


static int parse_instruction_token(Context *ctx)
{
    TokenizerContext *tctx = &ctx->tctx;
    int coissue = 0;
    int predicated = 0;
    char opstr[32];

    if (tokeq(tctx, "+"))
    {
        if (nexttoken(ctx, 0, 1, 0, 0) == FAIL)
            return FAIL;
        coissue = 1;
    } // if

    // All this tapdance is because some instructions mix letters and numbers,
    //  like "dp4" or "texm3x2depth" and the tokenizer splits words and digits
    //  into separate tokens, which makes parsing registers ("c31") easier.
    opstr[0] = '\0';
    while (1)
    {
        if ( (strlen(opstr) + strlen(tctx->token)) >= (sizeof (opstr)-1) )
            return fail(ctx, "Expected instruction");

        char *ptr;
        for (ptr = tctx->token; *ptr != '\0'; ptr++)
        {
            if (!valid_instruction_char(*ptr))
                break;
        } // for

        if ((ptr == tctx->token) || (*ptr != '\0'))
        {
            pushback(ctx);  // an invalid char or EOS in this token.
            break;
        } // if

        strcat(opstr, tctx->token);

        if (nexttoken(ctx, 0, 0, 1, 1) == FAIL)
            return FAIL;
    } // while

    int i;
    int valid_opcode = 0;
    const Instruction *instruction = NULL;
    for (i = 0; i < STATICARRAYLEN(instructions); i++)
    {
        instruction = &instructions[i];
        if (instruction->opcode_string == NULL)
            continue;  // skip this.
        else if (strcasecmp(opstr, instruction->opcode_string) != 0)
            continue;  // not us.
        valid_opcode = 1;
        break;
    } // for

    uint32 opcode = (uint32) i;
    uint32 controls = 0;

    if (!valid_opcode)
        return failf(ctx, "Unknown instruction '%s'", opstr);

    // This might need to be IFC instead of IF.
    // !!! FIXME: compare opcode, not string
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

    // !!! FIXME: predicated instructions

    ctx->tokenbufpos = 0;

    const int tokcount = instruction->parse_args(ctx);
    if (require_endline(ctx) == FAIL)
        return FAIL;

    // insttoks bits are reserved and should be zero if < SM2.
    const uint32 insttoks = shader_version_atleast(ctx, 2, 0) ? tokcount-1 : 0;

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
    TokenizerContext *tctx = &ctx->tctx;
    if (nexttoken(ctx, 1, 1, 0, 0) == FAIL)
        return FAIL;

    uint32 shader_type = 0;
    if (tokeq(tctx, "vs"))
    {
        ctx->shader_type = MOJOSHADER_TYPE_VERTEX;
        shader_type = 0xFFFE;
    } // if
    else if (tokeq(tctx, "ps"))
    {
        ctx->shader_type = MOJOSHADER_TYPE_PIXEL;
        shader_type = 0xFFFF;
    } // if
    else
    {
        // !!! FIXME: geometry shaders?
        return fail(ctx, "Expected version string");
    } // else

    uint32 major = 0;
    if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return FAIL;
    else if ( (!tokeq(tctx, "_")) && (!tokeq(tctx, ".")) )
        return fail(ctx, "Expected version string");
    else if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return FAIL;
    else if (!ui32fromstr(tctx->token, &major))
        return fail(ctx, "Expected version string");

    uint32 minor = 0;
    if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return FAIL;
    else if ( (!tokeq(tctx, "_")) && (!tokeq(tctx, ".")) )
        return fail(ctx, "Expected version string");
    else if (nexttoken(ctx, 0, 0, 0, 0) == FAIL)
        return FAIL;
    else if (tokeq(tctx, "x"))
        minor = 1;
    else if (tokeq(tctx, "sw"))
        minor = 255;
    else if (!ui32fromstr(tctx->token, &minor))
        return fail(ctx, "Expected version string");

    ctx->major_ver = major;
    ctx->minor_ver = minor;

    if (require_endline(ctx) == FAIL)
        return FAIL;

    output_token(ctx, (shader_type << 16) | (major << 8) | (minor << 0) );
    return NOFAIL;
} // parse_version_token


static int parse_phase_token(Context *ctx)
{
    if (require_endline(ctx) == FAIL)
        return FAIL;
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
        return fail(ctx, "Content after END");
    return NOFAIL;
} // parse_end_token


static int parse_token(Context *ctx)
{
    TokenizerContext *tctx = &ctx->tctx;
    if (tokeq(tctx, "end"))
        return parse_end_token(ctx);
    else if (tokeq(tctx, "phase"))
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
    ctx->tctx.source = source;
    ctx->tctx.linenum = 1;

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
        if (ctx->output != NULL)
            f(ctx->output, d);
        if (ctx->token_to_line != NULL)
            f(ctx->token_to_line, d);
        f(ctx, d);
    } // if
} // destroy_context


static const MOJOSHADER_parseData *build_failed_assembly(Context *ctx)
{
    MOJOSHADER_parseData *retval = NULL;
    if (!isfail(ctx))
        return NULL;

    retval = (MOJOSHADER_parseData*) Malloc(ctx, sizeof(MOJOSHADER_parseData));
    if (retval == NULL)
        return &out_of_mem_data;

    memset(retval, '\0', sizeof (MOJOSHADER_parseData));
    retval->malloc = (ctx->malloc == internal_malloc) ? NULL : ctx->malloc;
    retval->free = (ctx->free == internal_free) ? NULL : ctx->free;
    retval->malloc_data = ctx->malloc_data;
    retval->error = ctx->failstr;  // we recycle.  :)
    ctx->failstr = NULL;  // don't let this get free()'d too soon.

    if (ctx->started_parsing)
        retval->error_position = ctx->tctx.linenum;
    else
        retval->error_position = -1;

    return retval;
} // build_failed_assembly



// API entry point...

const MOJOSHADER_parseData *MOJOSHADER_assemble(const char *source,
                            MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    MOJOSHADER_parseData *retval = NULL;
    Context *ctx = NULL;

    if ( ((m == NULL) && (f != NULL)) || ((m != NULL) && (f == NULL)) )
        return &out_of_mem_data;  // supply both or neither.

    ctx = build_context(source, m, f, d);
    if (ctx == NULL)
        return &out_of_mem_data;

    // Version token always comes first.
    ctx->started_parsing = 1;
    parse_version_token(ctx);

    ctx->started_parsing = 0;  // make error messages sane if CTAB fails, etc.
    const char *credit = "Generated by MojoShader assembler revision "
                         MOJOSHADER_CHANGESET
                         ", http://icculus.org/mojoshader/";
    output_comment_string(ctx, credit);

    // !!! FIXME: insert CTAB here.

    ctx->started_parsing = 1;

    // parse out the rest of the tokens after the version token...
    while (nexttoken(ctx, 1, 1, 0, 1) == NOFAIL)
        parse_token(ctx);

    output_token(ctx, 0x0000FFFF);   // end token always 0x0000FFFF.

    if (isfail(ctx))
        retval = (MOJOSHADER_parseData *) build_failed_assembly(ctx);
    else
    {
        // This validates the shader; there are lots of things that are
        //  invalid, but will successfully parse in the assembler, generating
        //  bad bytecode; this will catch them without us having to
        //  duplicate most of the validation here.
        // It also saves us the trouble of duplicating all the other work,
        //  like setting up the uniforms list, etc.
        retval = (MOJOSHADER_parseData *)
                        MOJOSHADER_parse(MOJOSHADER_PROFILE_BYTECODE,
                                      (const unsigned char *) ctx->output,
                                      ctx->output_len * sizeof (uint32),
                                      NULL, 0, m, f, d);

        // on error, map the bytecode back to a line number.
        if (retval->error_position >= 0)
        {
            assert(retval != &out_of_mem_data);
            const int pos = retval->error_position / sizeof (uint32);
            if (pos < ctx->output_len)
                retval->error_position = ctx->token_to_line[pos];
            else
                retval->error_position = -1;  // oh well.
        } // if
    } // if

    destroy_context(ctx);
    return retval;
} // MOJOSHADER_assemble


// end of mojoshader_assembler.c ...

