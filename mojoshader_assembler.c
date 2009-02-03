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


typedef struct SourcePos
{
    const char *filename;
    uint32 line;
} SourcePos;


// Context...this is state that changes as we assemble a shader...
typedef struct Context
{
    int isfail;
    int out_of_memory;
    int eof;
    MOJOSHADER_malloc malloc;
    MOJOSHADER_free free;
    void *malloc_data;
    int error_count;
    ErrorList *errors;
    TokenizerContext tctx;
    MOJOSHADER_parsePhase parse_phase;
    MOJOSHADER_shaderType shader_type;
    uint8 major_ver;
    uint8 minor_ver;
    uint32 version_token;
    uint32 tokenbuf[16];
    int tokenbufpos;
    DestArgInfo dest_arg;
    uint32 *output;
    SourcePos *token_to_source;
    uint8 *ctab;
    uint32 ctab_len;
    uint32 ctab_allocation;
    size_t output_len;
    size_t output_allocation;
} Context;


// Convenience functions for allocators...

static inline void out_of_memory(Context *ctx)
{
    ctx->isfail = ctx->out_of_memory = 1;
} // out_of_memory

static inline void *Malloc(Context *ctx, const size_t len)
{
    void *retval = ctx->malloc((int) len, ctx->malloc_data);
    if (retval == NULL)
        out_of_memory(ctx);
    return retval;
} // Malloc

static inline char *StrDup(Context *ctx, const char *str)
{
    char *retval = (char *) Malloc(ctx, strlen(str) + 1);
    if (retval == NULL)
        out_of_memory(ctx);
    else
        strcpy(retval, str);
    return retval;
} // StrDup

static inline void Free(Context *ctx, void *ptr)
{
    if (ptr != NULL)  // check for NULL in case of dumb free() impl.
        ctx->free(ptr, ctx->malloc_data);
} // Free

static void failf(Context *ctx, const char *fmt, ...) ISPRINTF(2,3);
static void failf(Context *ctx, const char *fmt, ...)
{
    const char *fname = NULL;
    unsigned int linenum = 0;
    int error_position = 0;

    switch (ctx->parse_phase)
    {
        case MOJOSHADER_PARSEPHASE_NOTSTARTED:
            error_position = -2;
            break;
        case MOJOSHADER_PARSEPHASE_WORKING:
            // !!! FIXME: fname == base source file if output_pos == 0.
            if (ctx->output_len > 0)
            {
                const size_t idx = ctx->output_len - 1;
                linenum = ctx->token_to_source[idx].line;
                fname = ctx->token_to_source[idx].filename;
            } // if
            error_position = linenum;
            break;
        case MOJOSHADER_PARSEPHASE_DONE:
            error_position = -1;
            break;
        default:
            assert(0 && "Unexpected value");
            return;
    } // switch

    ErrorList *error = (ErrorList *) Malloc(ctx, sizeof (ErrorList));
    if (error == NULL)
        return;

    char scratch = 0;
    va_list ap;
    va_start(ap, fmt);
    const int len = vsnprintf(&scratch, sizeof (scratch), fmt, ap);
    va_end(ap);

    char *failstr = (char *) Malloc(ctx, len + 1);
    if (failstr == NULL)
        Free(ctx, error);
    else
    {
        va_start(ap, fmt);
        vsnprintf(failstr, len + 1, fmt, ap);  // rebuild it.
        va_end(ap);

        error->error.error = failstr;
        error->error.filename = fname ? StrDup(ctx, fname) : NULL;
        error->error.error_position = error_position;

        ErrorList *prev = NULL;
        error->next = ctx->errors;
        while (error->next != NULL)
        {
            prev = error->next;
            error->next = error->next->next;
        } // while

        if (prev != NULL)
            prev->next = error;
        else
            ctx->errors = error;

        ctx->error_count++;
    } // else
} // failf

static inline void fail(Context *ctx, const char *reason)
{
    failf(ctx, "%s", reason);
} // fail

static inline int isfail(const Context *ctx)
{
    return ctx->isfail;
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


static inline void add_token_sourcepos(Context *ctx, const size_t idx)
{
    ctx->token_to_source[idx].line = ctx->tctx.linenum;
    ctx->token_to_source[idx].filename = NULL;
} // add_token_sourcepos


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

        ptr = Malloc(ctx, newsize * sizeof (SourcePos));
        if (ptr == NULL)
            return;
        if (ctx->output_len > 0)
            memcpy(ptr, ctx->token_to_source, ctx->output_len * sizeof (SourcePos));
        Free(ctx, ctx->token_to_source);
        ctx->token_to_source = (SourcePos *) ptr;

        ctx->output_allocation = newsize;
    } // if

    ctx->output[ctx->output_len] = token;
    add_token_sourcepos(ctx, ctx->output_len);
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

    if (tctx->pushedback)
    {
        tctx->pushedback = 0;
        return 1;
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
        {
            fail(ctx, "buffer overflow");
            return 0;
        } // if

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
                return 1;
            } // if
        } // if
        else
        {
            // starting a non-number, but rest of current token was numbers.
            if ((idx > 0) && ((tctx->prevchar >= '0') && (tctx->prevchar <= '9')))
            {
                tctx->token[idx++] = '\0';
                return 1;
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
                return 1;

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
                return 1;

            case '\0':
                tctx->token[idx] = '\0';
                if (idx != 0)  // had any chars? It's a token.
                    return 1;
                ctx->eof = 1;
                return 0;

            default:
                tctx->source++;
                tctx->token[idx++] = ch;
                break;
        } // switch

        tctx->prevchar = ch;
    } // while

    assert(0 && "Shouldn't hit this code");
    return 0;
} // tokenize_ctx


static inline int tokenize(Context *ctx)
{
    const int rc = tokenize_ctx(ctx, &ctx->tctx);

    #if DEBUG_TOKENIZER
    printf("TOKENIZE: %d '%s'\n", rc,
           (ctx->tctx.token[0] == '\n') ? "\\n" : ctx->tctx.token);
    #endif

    return rc;
} // tokenize


static void pushback_ctx(Context *ctx, TokenizerContext *tctx)
{
    assert(!tctx->pushedback);
    tctx->pushedback = 1;
} // pushback_ctx


static inline void pushback(Context *ctx)
{
    pushback_ctx(ctx, &ctx->tctx);
    #if DEBUG_TOKENIZER
    printf("PUSHBACK\n");
    #endif
} // pushback


static int nexttoken_ctx(Context *ctx, TokenizerContext *tctx,
                     const int ignoreeol, const int ignorewhitespace,
                     const int eolok, const int eosok)
{
    while (tokenize_ctx(ctx, tctx))
    {
        if (tokeq(tctx, "\n"))
        {
            if (ignoreeol)
                continue;
            else if (!eolok)
            {
                fail(ctx, "Unexpected EOL");
                return 0;
            } // else if
        } // if

        else if (tokeq(tctx, " "))
        {
            if (ignorewhitespace)
                continue;
        } // else if

        // skip comments...
        else if (tokeq(tctx, "//") || tokeq(tctx, ";"))
        {
            while (tokenize_ctx(ctx, tctx))
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

    if ((ctx->eof) && (!eosok))
    {
        fail(ctx, "Unexpected EOF");
        return 0;
    } // if

    return 1;
} // nexttoken_ctx


static inline int nexttoken(Context *ctx, const int ignoreeol,
                     const int ignorewhitespace, const int eolok,
                     const int eosok)
{
    const int rc = nexttoken_ctx(ctx, &ctx->tctx, ignoreeol,
                                 ignorewhitespace, eolok, eosok);

    #if DEBUG_TOKENIZER
    printf("NEXTTOKEN: %d '%s'\n", rc,
           (ctx->tctx.token[0] == '\n') ? "\\n" : ctx->tctx.token);
    #endif

    return rc;
} // nexttoken


static void skip_line(Context *ctx)
{
    if (!tokeq(&ctx->tctx, "\n"))
    {
        while (nexttoken(ctx, 0, 1, 1, 1))
        {
            if (tokeq(&ctx->tctx, "\n"))
                break;
        } // while
    } // if
} // skip_line


static void require_endline(Context *ctx)
{
    TokenizerContext *tctx = &ctx->tctx;
    const int rc = nexttoken(ctx, 0, 1, 1, 1);
    if (ctx->eof)
        return;  // we'll call this an EOL.
    else if ((rc == 0) || (!tokeq(tctx, "\n")))
    {
        fail(ctx, "Endline expected");
        skip_line(ctx);
    } // else if
} // require_endline


static int require_comma(Context *ctx)
{
    TokenizerContext *tctx = &ctx->tctx;
    const int rc = nexttoken(ctx, 0, 1, 0, 0);
    if ((rc == 0) || (!tokeq(tctx, ",")))
    {
        fail(ctx, "Comma expected");
        return 0;
    } // if
    return 1;
} // require_comma


static int parse_register_name(Context *ctx, RegisterType *rtype, int *rnum)
{
    TokenizerContext *tctx = &ctx->tctx;
    if (!nexttoken(ctx, 0, 1, 0, 0))
        return 0;

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
            fail(ctx, "Output register not valid in this shader type");
        regtype = REG_TYPE_OUTPUT;
    } // else if
    else if (tokeq(tctx, "oT"))
    {
        if (shader_is_vertex(ctx) && shader_version_atleast(ctx, 3, 0))
            fail(ctx, "Output register not valid in this shader type");
        regtype = REG_TYPE_OUTPUT;
    } // else if
    else if (tokeq(tctx, "a"))
    {
        if (!shader_is_vertex(ctx))
            fail(ctx, "Address register only valid in vertex shaders.");
        regtype = REG_TYPE_ADDRESS;
    } // else if
    else if (tokeq(tctx, "t"))
    {
        if (!shader_is_pixel(ctx))
            fail(ctx, "Address register only valid in pixel shaders.");
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
        fail(ctx, "expected register type");
        regtype = REG_TYPE_CONST;
        regnum = 0;
        neednum = 0;
    } // else

    if (neednum)
    {
        // Make a temp TokenizerContext, since we need to skip whitespace here,
        //  but if the next non-whitespace token isn't '[', we'll want to get
        //  that whitespace back.
        TokenizerContext tmptctx;
        memcpy(&tmptctx, tctx, sizeof (TokenizerContext));
        if (!nexttoken_ctx(ctx, &tmptctx, 0, 1, 1, 1))
            return 0;
        else if (tokeq(&tmptctx, "["))
            neednum = 0;
    } // if

    if (neednum)
    {
        if (!nexttoken(ctx, 0, 0, 0, 0))
            return 0;

        uint32 ui32 = 0;
        if (!ui32fromstr(tctx->token, &ui32))
            fail(ctx, "Invalid register index");
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
            fail(ctx, "Invalid const register index");
        } // else
    } // if

    *rtype = regtype;
    *rnum = regnum;

    return 1;
} // parse_register_name


static void set_result_shift(Context *ctx, DestArgInfo *info, const int val)
{
    if (info->result_shift != 0)
        fail(ctx, "Multiple result shift modifiers");
    info->result_shift = val;
} // set_result_shift


static int parse_destination_token(Context *ctx)
{
    TokenizerContext *tctx = &ctx->tctx;

    DestArgInfo *info = &ctx->dest_arg;
    memset(info, '\0', sizeof (DestArgInfo));

    // See if there are destination modifiers on the instruction itself...
    while (1)
    {
        if (!nexttoken(ctx, 0, 0, 0, 0))
            return 1;
        else if (tokeq(tctx, " "))
            break;  // done with modifiers.
        else if (!tokeq(tctx, "_"))
            fail(ctx, "Expected modifier or whitespace");
        else if (!nexttoken(ctx, 0, 0, 0, 0))
            return 1;
        // !!! FIXME: this can be cleaned up when tokenizer is fixed.
        else if (tokeq(tctx, "x"))
        {
            if (!nexttoken(ctx, 0, 0, 0, 0))
                return 1;
            else if (tokeq(tctx, "2"))
                set_result_shift(ctx, info, 0x1);
            else if (tokeq(tctx, "4"))
                set_result_shift(ctx, info, 0x2);
            else if (tokeq(tctx, "8"))
                set_result_shift(ctx, info, 0x3);
            else
                fail(ctx, "Expected modifier");
        } // else if
        // !!! FIXME: this can be cleaned up when tokenizer is fixed.
        else if (tokeq(tctx, "d"))
        {
            if (!nexttoken(ctx, 0, 0, 0, 0))
                return 1;
            else if (tokeq(tctx, "8"))
                set_result_shift(ctx, info, 0xD);
            else if (tokeq(tctx, "4"))
                set_result_shift(ctx, info, 0xE);
            else if (tokeq(tctx, "2"))
                set_result_shift(ctx, info, 0xF);
            else
                fail(ctx, "Expected modifier");
        } // else if
        else if (tokeq(tctx, "sat"))
            info->result_mod |= MOD_SATURATE;
        else if (tokeq(tctx, "pp"))
            info->result_mod |= MOD_PP;
        else if (tokeq(tctx, "centroid"))
            info->result_mod |= MOD_CENTROID;
        else
            fail(ctx, "Expected modifier");
    } // while

    if (!nexttoken(ctx, 0, 1, 0, 0))
        return 1;

    // !!! FIXME: predicates.
    if (tokeq(tctx, "("))
        fail(ctx, "Predicates unsupported at this time");  // !!! FIXME: ...

    pushback(ctx);  // parse_register_name calls nexttoken().

    parse_register_name(ctx, &info->regtype, &info->regnum);

    if (!nexttoken(ctx, 0, 1, 1, 1))
        return 1;

    // !!! FIXME: can dest registers do relative addressing?

    int invalid_writemask = 0;
    int implicit_writemask = 0;
    if (!tokeq(tctx, "."))
    {
        implicit_writemask = 1;
        info->writemask = 0xF;
        info->writemask0 = info->writemask1 = info->writemask2 = info->writemask3 = 1;
        pushback(ctx);  // no explicit writemask; do full mask.
    } // if
    // !!! FIXME: Cg generates code with oDepth.z ... this is a bug, I think.
    //else if (scalar_register(ctx->shader_type, info->regtype, info->regnum))
    else if ( (scalar_register(ctx->shader_type, info->regtype, info->regnum)) && (info->regtype != REG_TYPE_DEPTHOUT) )
        fail(ctx, "Writemask specified for scalar register");
    else if (!nexttoken(ctx, 0, 1, 0, 0))
        return 1;
    else if (tokeq(tctx, ""))
        invalid_writemask = 1;
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
            invalid_writemask = 1;

        info->writemask = ( ((info->writemask0 & 0x1) << 0) |
                            ((info->writemask1 & 0x1) << 1) |
                            ((info->writemask2 & 0x1) << 2) |
                            ((info->writemask3 & 0x1) << 3) );
    } // else

    if (invalid_writemask)
        fail(ctx, "Invalid writemask");

    // !!! FIXME: Cg generates code with oDepth.z ... this is a bug, I think.
    if (info->regtype == REG_TYPE_DEPTHOUT)
    {
        if ( (!implicit_writemask) && ((info->writemask0 + info->writemask1 +
               info->writemask2 + info->writemask3) > 1) )
            fail(ctx, "Writemask specified for scalar register");
    } // if

    info->orig_writemask = info->writemask;

    if (ctx->tokenbufpos >= STATICARRAYLEN(ctx->tokenbuf))
    {
        fail(ctx, "Too many tokens");
        return 1;
    } // if

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
    {
        fail(ctx, "Too many tokens");
        return 0;
    } // if

    // mark this now, so optional relative addressing token is placed second.
    uint32 *token = &ctx->tokenbuf[ctx->tokenbufpos++];
    *token = 0;

    SourceMod srcmod = SRCMOD_NONE;
    int negate = 0;
    if (!nexttoken(ctx, 0, 1, 0, 0))
        return 1;
    else if (tokeq(tctx, "1"))
    {
        if (!nexttoken(ctx, 0, 1, 0, 0))
            return 1;
        else if (!tokeq(tctx, "-"))
            fail(ctx, "Unexpected value");
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
    parse_register_name(ctx, &regtype, &regnum);
    if (!nexttoken(ctx, 0, 1, 1, 1))
        return 1;
    else if (!tokeq(tctx, "_"))
        pushback(ctx);
    else if (!nexttoken(ctx, 0, 0, 0, 0))
        return 1;
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
        fail(ctx, "Invalid source modifier");

    if (!nexttoken(ctx, 0, 1, 1, 1))
        return 1;

    uint32 relative = 0;
    if (!tokeq(tctx, "["))
        pushback(ctx);  // not relative addressing?
    else
    {
        if (!relok)
            fail(ctx, "Relative addressing not permitted here.");
        else
            retval++;

        parse_source_token_maybe_relative(ctx, 0);
        relative = 1;
        if (!nexttoken(ctx, 0, 1, 0, 0))
            return retval;
        else if (!tokeq(tctx, "+"))
            pushback(ctx);
        else if (!nexttoken(ctx, 0, 1, 0, 0))
            return retval;
        else
        {
            if (regnum != 0)  // !!! FIXME: maybe c3[a0.x + 5] is legal and becomes c[a0.x + 8] ?
                fail(ctx, "Relative addressing with explicit register number.");
            uint32 ui32 = 0;
            if (!ui32fromstr(tctx->token, &ui32))
                fail(ctx, "Invalid relative addressing offset");
            regnum += (int) ui32;
        } // else

        if (!nexttoken(ctx, 0, 1, 0, 0))
            return retval;
        else if (!tokeq(tctx, "]"))
            fail(ctx, "Expected ']'");
    } // else

    if (!nexttoken(ctx, 0, 1, 1, 1))
        return retval;

    int invalid_swizzle = 0;
    uint32 swizzle = 0;
    if (!tokeq(tctx, "."))
    {
        swizzle = 0xE4;  // 0xE4 == 11100100 ... 0 1 2 3. No swizzle.
        pushback(ctx);  // no explicit writemask; do full mask.
    } // if
    else if (scalar_register(ctx->shader_type, regtype, regnum))
        fail(ctx, "Swizzle specified for scalar register");
    else if (!nexttoken(ctx, 0, 1, 0, 0))
        return retval;
    else if (tokeq(tctx, ""))
        invalid_swizzle = 1;
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
            invalid_swizzle = 1;
        tctx->token[4] = '\0';

        uint32 val = 0;
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
                default: invalid_swizzle = 1; break;
            } // switch
            swizzle |= (val << (i * 2));
        } // for

        if (saw_xyzw && saw_rgba)
            invalid_swizzle = 1;
    } // else

    if (invalid_swizzle)
        fail(ctx, "Invalid swizzle");

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
    return 1;
} // parse_args_NULL


static int parse_num(Context *ctx, const int floatok, uint32 *token)
{
    TokenizerContext *tctx = &ctx->tctx;
    int32 negative = 1;
    union { float f; int32 si32; uint32 ui32; } cvt;
    cvt.si32 = 0;

    *token = 0;

    if (!nexttoken(ctx, 0, 1, 0, 0))
        return 0;
    else if (tokeq(tctx, "-"))
        negative = -1;
    else
        pushback(ctx);

    uint32 val = 0;
    if (!nexttoken(ctx, 0, 1, 0, 0))
        return 0;
    else if (!ui32fromstr(tctx->token, &val))
        fail(ctx, "Expected number");

    uint32 fraction = 0;
    if (!nexttoken(ctx, 0, 1, 1, 1))
        return 0;
    else if (!tokeq(tctx, "."))
        pushback(ctx);  // whole number
    else if (!floatok)
        fail(ctx, "Expected whole number");
    else if (!nexttoken(ctx, 0, 1, 0, 0))
        return 0;
    else if (!ui32fromstr(tctx->token, &fraction))
        fail(ctx, "Expected number");

    uint32 exponent = 0;
    int negexp = 0;
    if (!nexttoken(ctx, 0, 1, 1, 1))
        return 0;
    else if (!tokeq(tctx, "e"))
        pushback(ctx);
    else if (!floatok)
        fail(ctx, "Exponent on whole number");  // !!! FIXME: illegal?
    else if (!nexttoken(ctx, 0, 1, 0, 0))
        return 0;
    else
    {
        if (!tokeq(tctx, "-"))
            pushback(ctx);
        else
            negexp = 1;

        if (!nexttoken(ctx, 0, 1, 0, 0))
            return 0;
        else if (!ui32fromstr(tctx->token, &exponent))
            fail(ctx, "Expected exponent");
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
    return 1;
} // parse_num


static int parse_args_DEFx(Context *ctx, const int isflt)
{
    parse_destination_token(ctx);
    require_comma(ctx);
    parse_num(ctx, isflt, &ctx->tokenbuf[ctx->tokenbufpos++]);
    require_comma(ctx);
    parse_num(ctx, isflt, &ctx->tokenbuf[ctx->tokenbufpos++]);
    require_comma(ctx);
    parse_num(ctx, isflt, &ctx->tokenbuf[ctx->tokenbufpos++]);
    require_comma(ctx);
    parse_num(ctx, isflt, &ctx->tokenbuf[ctx->tokenbufpos++]);
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
    parse_destination_token(ctx);
    require_comma(ctx);
    if (!nexttoken(ctx, 0, 1, 0, 0))
        return 1;
    else if (tokeq(tctx, "true"))
        ctx->tokenbuf[ctx->tokenbufpos++] = 1;
    else if (tokeq(tctx, "false"))
        ctx->tokenbuf[ctx->tokenbufpos++] = 0;
    else
        fail(ctx, "Expected 'true' or 'false'");
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
        if (!nexttoken(ctx, 0, 0, 1, 1))
            return 0;
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
            return 1;
        } // if
    } // for

    for (i = 0; i < STATICARRAYLEN(samplerusagestrs); i++)
    {
        if (strcasecmp(samplerusagestrs[i], token) == 0)
        {
            *issampler = 1;
            *val = i + 2;
            return 1;
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
            return 1;  // if you have "dcl_pp", then "_pp" isn't a usage.
        } // if
    } // for

    return 0;
} // parse_dcl_usage


static int parse_args_DCL(Context *ctx)
{
    TokenizerContext *tctx = &ctx->tctx;
    int issampler = 0;
    uint32 usage = 0;
    uint32 index = 0;

    ctx->tokenbufpos++;  // save a spot for the usage/index token.
    ctx->tokenbuf[0] = 0;

    if (!nexttoken(ctx, 0, 0, 0, 0))
        return 1;
    else if (tokeq(tctx, " "))
        pushback(ctx);
    else if (!tokeq(tctx, "_"))
        fail(ctx, "Expected register or usage");
    else if (!nexttoken(ctx, 0, 0, 0, 0))
        return 1;
    else
        parse_dcl_usage(ctx, &usage, &issampler);

    if (!nexttoken(ctx, 0, 0, 0, 0))
        return 1;
    else if (tokeq(tctx, " ") || tokeq(tctx, "_"))
        pushback(ctx);  // parse_destination_token() wants these.
    else if (!ui32fromstr(tctx->token, &index))
        fail(ctx, "Expected usage index or register");

    parse_destination_token(ctx);

    const int samplerreg = (ctx->dest_arg.regtype == REG_TYPE_SAMPLER);
    if (issampler != samplerreg)
        fail(ctx, "Invalid usage");
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
    return retval;
} // parse_args_D


static int parse_args_S(Context *ctx)
{
    int retval = 1;
    retval += parse_source_token(ctx);
    return retval;
} // parse_args_S


static int parse_args_SS(Context *ctx)
{
    int retval = 1;
    retval += parse_source_token(ctx);
    require_comma(ctx);
    retval += parse_source_token(ctx);
    return retval;
} // parse_args_SS


static int parse_args_DS(Context *ctx)
{
    int retval = 1;
    retval += parse_destination_token(ctx);
    require_comma(ctx);
    retval += parse_source_token(ctx);
    return retval;
} // parse_args_DS


static int parse_args_DSS(Context *ctx)
{
    int retval = 1;
    retval += parse_destination_token(ctx);
    require_comma(ctx);
    retval += parse_source_token(ctx);
    require_comma(ctx);
    retval += parse_source_token(ctx);
    return retval;
} // parse_args_DSS


static int parse_args_DSSS(Context *ctx)
{
    int retval = 1;
    retval += parse_destination_token(ctx);
    require_comma(ctx);
    retval += parse_source_token(ctx);
    require_comma(ctx);
    retval += parse_source_token(ctx);
    require_comma(ctx);
    retval += parse_source_token(ctx);
    return retval;
} // parse_args_DSSS


static int parse_args_DSSSS(Context *ctx)
{
    int retval = 1;
    retval += parse_destination_token(ctx);
    require_comma(ctx);
    retval += parse_source_token(ctx);
    require_comma(ctx);
    retval += parse_source_token(ctx);
    require_comma(ctx);
    retval += parse_source_token(ctx);
    require_comma(ctx);
    retval += parse_source_token(ctx);
    return retval;
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
    if (!nexttoken(ctx, 0, 0, 0, 0))
        return 0;
    else if (!tokeq(tctx, "_"))
    {
        pushback(ctx);
        return 0;
    } // else if

    if (!nexttoken(ctx, 0, 0, 0, 0))
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
        if (!nexttoken(ctx, 0, 1, 0, 0))
            return 0;
        coissue = 1;
    } // if

    // All this tapdance is because some instructions mix letters and numbers,
    //  like "dp4" or "texm3x2depth" and the tokenizer splits words and digits
    //  into separate tokens, which makes parsing registers ("c31") easier.
    opstr[0] = '\0';
    while (1)
    {
        if ( (strlen(opstr) + strlen(tctx->token)) >= (sizeof (opstr)-1) )
        {
            fail(ctx, "Expected instruction");
            return 0;
        } // if

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

        if (!nexttoken(ctx, 0, 0, 1, 1))
            return 0;
    } // while

    uint32 controls = 0;

    // This might need to be TEXLD instead of TEXLDP.
    if (strcasecmp(opstr, "TEXLDP") == 0)
    {
        controls = CONTROL_TEXLDP;
        strcpy(opstr, "TEXLD");
    } // if

    // This might need to be TEXLD instead of TEXLDB.
    if (strcasecmp(opstr, "TEXLDB") == 0)
    {
        controls = CONTROL_TEXLDB;
        strcpy(opstr, "TEXLD");
    } // else if

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

    if (!valid_opcode)
    {
        failf(ctx, "Unknown instruction '%s'", opstr);
        return 0;
    } // if

    // This might need to be IFC instead of IF.
    // !!! FIXME: compare opcode, not string
    if (strcmp(instruction->opcode_string, "IF") == 0)
    {
        if (parse_condition(ctx, &controls))
            opcode = OPCODE_IFC;
    } // if

    // This might need to be BREAKC instead of BREAK.
    // !!! FIXME: compare opcode, not string
    else if (strcmp(instruction->opcode_string, "BREAK") == 0)
    {
        if (parse_condition(ctx, &controls))
            opcode = OPCODE_BREAKC;
    } // else if

    // SETP has a conditional code, always.
    // !!! FIXME: compare opcode, not string
    else if (strcmp(instruction->opcode_string, "SETP") == 0)
    {
        if (!parse_condition(ctx, &controls))
            fail(ctx, "SETP requires a condition");
    } // else if

    instruction = &instructions[opcode];  // ...in case this changed.

    // !!! FIXME: predicated instructions

    ctx->tokenbufpos = 0;

    const int tokcount = instruction->parse_args(ctx);
    require_endline(ctx);

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

    return 1;
} // parse_instruction_token


static void parse_version_token(Context *ctx)
{
    TokenizerContext *tctx = &ctx->tctx;
    if (!nexttoken(ctx, 1, 1, 0, 0))
        return;

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
        fail(ctx, "Expected version string");
    } // else

    int bad_version = 0;

    uint32 major = 0;
    if (!nexttoken(ctx, 0, 0, 0, 0))
        return;
    else if ( (!tokeq(tctx, "_")) && (!tokeq(tctx, ".")) )
        bad_version = 1;
    else if (!nexttoken(ctx, 0, 0, 0, 0))
        return;
    else if (!ui32fromstr(tctx->token, &major))
        bad_version = 1;

    uint32 minor = 0;
    if (!nexttoken(ctx, 0, 0, 0, 0))
        return;
    else if ( (!tokeq(tctx, "_")) && (!tokeq(tctx, ".")) )
        bad_version = 1;
    else if (!nexttoken(ctx, 0, 0, 0, 0))
        return;
    else if (tokeq(tctx, "x"))
        minor = 1;
    else if (tokeq(tctx, "sw"))
        minor = 255;
    else if (!ui32fromstr(tctx->token, &minor))
        bad_version = 1;

    if (bad_version)
        fail(ctx, "Expected version string");
    else
        require_endline(ctx);

    ctx->major_ver = major;
    ctx->minor_ver = minor;

    ctx->version_token = (shader_type << 16) | (major << 8) | (minor << 0);
    output_token(ctx, ctx->version_token);
} // parse_version_token


static void parse_phase_token(Context *ctx)
{
    require_endline(ctx);
    output_token(ctx, 0x0000FFFD); // phase token always 0x0000FFFD.
} // parse_phase_token


static void parse_end_token(Context *ctx)
{
    require_endline(ctx);
    // We don't emit the end token bits here, since it's valid for a shader
    //  to not specify an "end" string at all; it's implicit, in that case.
    // Instead, we make sure if we see "end" that it's the last thing we see.
    nexttoken(ctx, 1, 1, 0, 1);
    if (!ctx->eof)
        fail(ctx, "Content after END");
} // parse_end_token


static void parse_token(Context *ctx)
{
    TokenizerContext *tctx = &ctx->tctx;
    if (tokeq(tctx, "end"))
        parse_end_token(ctx);
    else if (tokeq(tctx, "phase"))
        parse_phase_token(ctx);
    parse_instruction_token(ctx);
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
    ctx->parse_phase = MOJOSHADER_PARSEPHASE_NOTSTARTED;
    ctx->tctx.source = source;
    ctx->tctx.linenum = 1;

    return ctx;
} // build_context


static void free_error_list(MOJOSHADER_free f, void *d, ErrorList *item)
{
    while (item != NULL)
    {
        ErrorList *next = item->next;
        f((void *) item->error.error, d);
        f((void *) item->error.filename, d);
        f(item, d);
        item = next;
    } // while
} // free_error_list


static void destroy_context(Context *ctx)
{
    if (ctx != NULL)
    {
        MOJOSHADER_free f = ((ctx->free != NULL) ? ctx->free : internal_free);
        void *d = ctx->malloc_data;
        free_error_list(f, d, ctx->errors);
        if (ctx->output != NULL)
            f(ctx->output, d);
        if (ctx->token_to_source != NULL)
            f(ctx->token_to_source, d);
        if (ctx->ctab != NULL)
            f(ctx->ctab, d);
        f(ctx, d);
    } // if
} // destroy_context


static MOJOSHADER_error *build_errors(Context *ctx)
{
    int total = 0;
    MOJOSHADER_error *retval = (MOJOSHADER_error *)
            Malloc(ctx, sizeof (MOJOSHADER_error) * ctx->error_count);
    if (retval == NULL)
        return NULL;

    ErrorList *item = ctx->errors;
    while (item != NULL)
    {
        ErrorList *next = item->next;
        // reuse the string allocations
        memcpy(&retval[total], &item->error, sizeof (MOJOSHADER_error));
        Free(ctx, item);
        item = next;
        total++;
    } // while
    ctx->errors = NULL;

    assert(total == ctx->error_count);
    return retval;
} // build_errors


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

    retval->error_count = ctx->error_count;
    retval->errors = build_errors(ctx);
    if ((retval->errors == NULL) && (ctx->error_count > 0))
    {
        Free(ctx, retval);
        return &out_of_mem_data;
    } // if

    return retval;
} // build_failed_assembly


static uint32 add_ctab_bytes(Context *ctx, const uint8 *bytes, const size_t len)
{
    const size_t extra = CTAB_SIZE + sizeof (uint32);
    if (len <= (ctx->ctab_len - extra))
    {
        void *ptr = ctx->ctab + extra;
        if (len == 0)
            return ( (uint32) (((uint8 *) ptr) - ctx->ctab) ) - sizeof (uint32);
        else if ((len == 1) && ((ptr = memchr(ptr, bytes[0], ctx->ctab_len - len)) != NULL))
            return ( (uint32) (((uint8 *) ptr) - ctx->ctab) ) - sizeof (uint32);
        else  // search for the string of bytes...
        {
            while ((ptr = memchr(ptr, bytes[0], ctx->ctab_len - len)) != NULL)
            {
                if (memcmp(ptr, bytes, len) == 0)  // this is it?
                    return ( (uint32) (((uint8 *) ptr) - ctx->ctab) ) - sizeof (uint32);
                ptr++;
            } // while
        } // else
    } // if

    // add it to the byte pile...

    // verify allocation.
    const size_t newsize = (ctx->ctab_len + len);
    if (ctx->ctab_allocation < newsize)
    {
        const size_t additional = 4 * 1024;
        while (ctx->ctab_allocation < newsize)
            ctx->ctab_allocation += additional;
        void *ptr = Malloc(ctx, ctx->ctab_allocation);
        if (ptr == NULL)
            return 0;
        if (ctx->ctab != NULL)
        {
            memcpy(ptr, ctx->ctab, ctx->ctab_len);
            Free(ctx, ctx->ctab);
        } // if
        ctx->ctab = (uint8 *) ptr;
    } // if

    const uint32 retval = ctx->ctab_len - sizeof (uint32);
    memcpy(ctx->ctab + ctx->ctab_len, bytes, len);
    ctx->ctab_len += len;
    return retval;
} // add_ctab_bytes


static inline uint32 add_ctab_string(Context *ctx, const char *str)
{
    return add_ctab_bytes(ctx, (const uint8 *) str, strlen(str) + 1);
} // add_ctab_string


static uint32 add_ctab_typeinfo(Context *ctx, const MOJOSHADER_symbolTypeInfo *info);

static uint32 add_ctab_members(Context *ctx, const MOJOSHADER_symbolTypeInfo *info)
{
    unsigned int i;
    const size_t len = info->member_count * CMEMBERINFO_SIZE;
    uint8 *bytes = (uint8 *) Malloc(ctx, len);
    if (bytes == NULL)
        return 0;

    union { uint8 *ui8; uint16 *ui16; uint32 *ui32; } ptr;
    ptr.ui8 = bytes;
    for (i = 0; i < info->member_count; i++)
    {
        const MOJOSHADER_symbolStructMember *member = &info->members[i];
        *(ptr.ui32++) = SWAP32(add_ctab_string(ctx, member->name));
        *(ptr.ui32++) = SWAP32(add_ctab_typeinfo(ctx, &member->info));
    } // for

    const uint32 retval = add_ctab_bytes(ctx, bytes, len);
    Free(ctx, bytes);
    return retval;
} // add_ctab_members


static uint32 add_ctab_typeinfo(Context *ctx, const MOJOSHADER_symbolTypeInfo *info)
{
    uint8 bytes[CTYPEINFO_SIZE];
    union { uint8 *ui8; uint16 *ui16; uint32 *ui32; } ptr;
    ptr.ui8 = bytes;

    *(ptr.ui16++) = SWAP16((uint16) info->parameter_class);
    *(ptr.ui16++) = SWAP16((uint16) info->parameter_type);
    *(ptr.ui16++) = SWAP16((uint16) info->rows);
    *(ptr.ui16++) = SWAP16((uint16) info->columns);
    *(ptr.ui16++) = SWAP16((uint16) info->elements);
    *(ptr.ui16++) = SWAP16((uint16) info->member_count);
    *(ptr.ui32++) = SWAP32(add_ctab_members(ctx, info));

    return add_ctab_bytes(ctx, bytes, sizeof (bytes));
} // add_ctab_typeinfo


static uint32 add_ctab_info(Context *ctx, const MOJOSHADER_symbol *symbols,
                            const unsigned int symbol_count)
{
    unsigned int i;
    const size_t len = symbol_count * CINFO_SIZE;
    uint8 *bytes = (uint8 *) Malloc(ctx, len);
    if (bytes == NULL)
        return 0;

    union { uint8 *ui8; uint16 *ui16; uint32 *ui32; } ptr;
    ptr.ui8 = bytes;
    for (i = 0; i < symbol_count; i++)
    {
        const MOJOSHADER_symbol *sym = &symbols[i];
        *(ptr.ui32++) = SWAP32(add_ctab_string(ctx, sym->name));
        *(ptr.ui16++) = SWAP16((uint16) sym->register_set);
        *(ptr.ui16++) = SWAP16((uint16) sym->register_index);
        *(ptr.ui16++) = SWAP16((uint16) sym->register_count);
        *(ptr.ui16++) = SWAP16(0);  // reserved
        *(ptr.ui32++) = SWAP32(add_ctab_typeinfo(ctx, &sym->info));
        *(ptr.ui32++) = SWAP32(0);  // !!! FIXME: default value.
    } // for

    const uint32 retval = add_ctab_bytes(ctx, bytes, len);
    Free(ctx, bytes);
    return retval;
} // add_ctab_info


static void output_ctab(Context *ctx, const MOJOSHADER_symbol *symbols,
                        unsigned int symbol_count, const char *creator)
{
    ctx->ctab_len = CTAB_SIZE + sizeof (uint32);

    uint8 bytes[CTAB_SIZE + sizeof (uint32)];
    uint32 *table = (uint32 *) bytes;
    *(table++) = SWAP32(CTAB_ID);
    *(table++) = SWAP32(CTAB_SIZE);
    *(table++) = SWAP32(add_ctab_string(ctx, creator));
    *(table++) = SWAP32(ctx->version_token);
    *(table++) = SWAP32(((uint32) symbol_count));
    *(table++) = SWAP32(add_ctab_info(ctx, symbols, symbol_count));
    *(table++) = SWAP32(0);  // build flags.
    *(table++) = SWAP32(add_ctab_string(ctx, ""));  // !!! FIXME: target?
    memcpy(ctx->ctab, bytes, sizeof (bytes));
    output_comment_bytes(ctx, ctx->ctab, ctx->ctab_len);

    Free(ctx, ctx->ctab);
    ctx->ctab = NULL;
    ctx->ctab_len = 0;
    ctx->ctab_allocation = 0;
} // output_ctab


static void output_comments(Context *ctx, const char **comments,
                            unsigned int comment_count,
                            const MOJOSHADER_symbol *symbols,
                            unsigned int symbol_count)
{
    if (isfail(ctx))
        return;

    // make error messages sane if CTAB fails, etc.
    ctx->parse_phase = MOJOSHADER_PARSEPHASE_NOTSTARTED;

    const char *creator = "MojoShader revision " MOJOSHADER_CHANGESET;
    if (symbol_count > 0)
        output_ctab(ctx, symbols, symbol_count, creator);
    else
        output_comment_string(ctx, creator);

    int i;
    for (i = 0; i < comment_count; i++)
        output_comment_string(ctx, comments[i]);

    ctx->parse_phase = MOJOSHADER_PARSEPHASE_WORKING;
} // output_comments


// API entry point...

const MOJOSHADER_parseData *MOJOSHADER_assemble(const char *source,
                            const char **comments, unsigned int comment_count,
                            const MOJOSHADER_symbol *symbols,
                            unsigned int symbol_count,
                            MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    int failed = 0;
    MOJOSHADER_parseData *retval = NULL;
    Context *ctx = NULL;

    if ( ((m == NULL) && (f != NULL)) || ((m != NULL) && (f == NULL)) )
        return &out_of_mem_data;  // supply both or neither.

    ctx = build_context(source, m, f, d);
    if (ctx == NULL)
        return &out_of_mem_data;

    // Version token always comes first.
    ctx->parse_phase = MOJOSHADER_PARSEPHASE_WORKING;
    parse_version_token(ctx);
    output_comments(ctx, comments, comment_count, symbols, symbol_count);

    // parse out the rest of the tokens after the version token...
    while (nexttoken(ctx, 1, 1, 0, 1))
    {
        if (isfail(ctx))
        {
            failed = 1;
            ctx->isfail = 0;
            skip_line(ctx);  // start fresh on next line.
        } // if
        parse_token(ctx);
    } // while

    ctx->isfail = failed;

    output_token(ctx, 0x0000FFFF);   // end token always 0x0000FFFF.

    if (failed)
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
        int i;
        for (i = 0; i < retval->error_count; i++)
        {
            MOJOSHADER_error *error = &retval->errors[i];
            if (error->error_position >= 0)
            {
                assert(retval != &out_of_mem_data);
                const int pos = error->error_position / sizeof (uint32);
                if (pos >= ctx->output_len)
                    error->error_position = -1;  // oh well.
                else
                {
                    const SourcePos *srcpos = &ctx->token_to_source[pos];
                    Free(ctx, (void *) error->filename);
                    char *fname = (char *) Malloc(ctx,
                                        strlen(srcpos->filename) + 1);
                    if (fname != NULL)
                    {
                        strcpy(fname, srcpos->filename);
                        error->error_position = srcpos->line;
                    } // if
                    error->filename = fname;  // may be NULL, that's okay.
                } // else
            } // if
        } // for
    } // if

    destroy_context(ctx);
    return retval;
} // MOJOSHADER_assemble


// end of mojoshader_assembler.c ...

