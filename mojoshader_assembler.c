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

#if DEBUG_ASSEMBLY_PARSER
    #define print_debug_token(token, len, val) \
        MOJOSHADER_print_debug_token("ASSEMBLER", token, len, val)
#else
    #define print_debug_token(token, len, val)
#endif


// Simple linked list to cache source filenames, so we don't have to copy
//  the same string over and over for each opcode.
typedef struct FilenameCache
{
    char *filename;
    struct FilenameCache *next;
} FilenameCache;


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
    MOJOSHADER_malloc malloc;
    MOJOSHADER_free free;
    void *malloc_data;
    int error_count;
    ErrorList *errors;
    Preprocessor *preprocessor;
    MOJOSHADER_parsePhase parse_phase;
    MOJOSHADER_shaderType shader_type;
    uint8 major_ver;
    uint8 minor_ver;
    int pushedback;
    const char *token;      // assembler token!
    unsigned int tokenlen;  // assembler token!
    Token tokenval;         // assembler token!
    uint32 version_token;   // bytecode token!
    uint32 tokenbuf[16];    // bytecode tokens!
    int tokenbufpos;        // bytecode tokens!
    DestArgInfo dest_arg;
    uint32 *output;
    SourcePos *token_to_source;
    uint8 *ctab;
    uint32 ctab_len;
    uint32 ctab_allocation;
    size_t output_len;
    size_t output_allocation;
    FilenameCache *filename_cache;
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
    if (retval != NULL)
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

    ctx->isfail = 1;

    switch (ctx->parse_phase)
    {
        case MOJOSHADER_PARSEPHASE_NOTSTARTED:
            error_position = -2;
            break;
        case MOJOSHADER_PARSEPHASE_WORKING:
            fname = preprocessor_sourcepos(ctx->preprocessor, &linenum);
            error_position = (int) linenum;
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
        error->next = NULL;

        ErrorList *prev = NULL;
        ErrorList *item = ctx->errors;
        while (item != NULL)
        {
            prev = item;
            item = item->next;
        } // while

        if (prev == NULL)
            ctx->errors = error;
        else
            prev->next = error;

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

static inline void pushback(Context *ctx)
{
    #if DEBUG_ASSEMBLY_PARSER
    printf("ASSEMBLER PUSHBACK\n");
    #endif
    assert(!ctx->pushedback);
    ctx->pushedback = 1;
} // pushback

static Token _nexttoken(Context *ctx)
{
    while (1)
    {
        ctx->token = preprocessor_nexttoken(ctx->preprocessor, &ctx->tokenlen,
                                            &ctx->tokenval);

        if (preprocessor_outofmemory(ctx->preprocessor))
        {
            out_of_memory(ctx);
            ctx->tokenval = TOKEN_EOI;
            ctx->token = NULL;
            ctx->tokenlen = 0;
            break;
        } // if

        if (ctx->tokenval == TOKEN_PREPROCESSING_ERROR)
        {
            fail(ctx, ctx->token);
            continue;
        } // else

        break;
    } // while

    return ctx->tokenval;
} // _nexttoken


static Token nexttoken(Context *ctx)
{
    if (ctx->pushedback)
    {
        print_debug_token(ctx->token, ctx->tokenlen, ctx->tokenval);
        ctx->pushedback = 0;
        return ctx->tokenval;
    } // if

    Token token = _nexttoken(ctx);

    while (token == ((Token) '\n'))
        token = _nexttoken(ctx);  // skip endlines.

    if (token == ((Token) ';'))  // single line comment in assembler.
    {
        do
        {
            token = _nexttoken(ctx);
        } while ((token != ((Token) '\n')) && (token != TOKEN_EOI));

        while (token == ((Token) '\n'))
            token = _nexttoken(ctx);  // skip endlines.
    } // if

    print_debug_token(ctx->token, ctx->tokenlen, ctx->tokenval);
    return token;
} // nexttoken


static const char *cache_filename(Context *ctx, const char *fname)
{
    if (fname == NULL)
        return NULL;

    // !!! FIXME: this could be optimized into a hash table, but oh well.
    FilenameCache *item = ctx->filename_cache;
    while (item != NULL)
    {
        if (strcmp(item->filename, fname) == 0)
            return item->filename;
        item = item->next;
    } // while

    // new cache item.
    item = (FilenameCache *) Malloc(ctx, sizeof (FilenameCache));
    if (item == NULL)
        return NULL;

    item->filename = (char *) Malloc(ctx, strlen(fname) + 1);
    if (item->filename == NULL)
    {
        Free(ctx, item);
        return NULL;
    } // if

    strcpy(item->filename, fname);
    item->next = ctx->filename_cache;
    ctx->filename_cache = item;

    return item->filename;
} // cache_filename


static void free_filename_cache(Context *ctx)
{
    FilenameCache *item = ctx->filename_cache;
    while (item != NULL)
    {
        FilenameCache *next = item->next;
        Free(ctx, item->filename);
        Free(ctx, item);
        item = next;
    } // while
} // free_filename_cache


static inline void add_token_sourcepos(Context *ctx, const size_t idx)
{
    unsigned int pos = 0;
    const char *fname = preprocessor_sourcepos(ctx->preprocessor, &pos);
    ctx->token_to_source[idx].line = pos;
    ctx->token_to_source[idx].filename = cache_filename(ctx, fname);
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


static int require_comma(Context *ctx)
{
    const Token token = nexttoken(ctx);
    if (token != ((Token) ','))
    {
        fail(ctx, "Comma expected");
        return 0;
    } // if
    return 1;
} // require_comma


static int check_token_segment(Context *ctx, const char *str)
{
    // !!! FIXME: these are case-insensitive, right?
    const size_t len = strlen(str);
    if ( (ctx->tokenlen < len) || (strncasecmp(ctx->token, str, len) != 0) )
        return 0;
    ctx->token += len;
    ctx->tokenlen -= len;
    return 1;
} // check_token_segment


static int check_token(Context *ctx, const char *str)
{
    const size_t len = strlen(str);
    if ( (ctx->tokenlen != len) || (strncasecmp(ctx->token, str, len) != 0) )
        return 0;
    ctx->token += len;
    ctx->tokenlen = 0;
    return 1;
} // check_token


static int ui32fromtoken(Context *ctx, uint32 *_val)
{
    int i;
    for (i = 0; i < ctx->tokenlen; i++)
    {
        if ((ctx->token[i] < '0') || (ctx->token[i] > '9'))
            break;
    } // for

    if (i == 0)
    {
        *_val = 0;
        return 0;
    } // if

    const int len = i;
    uint32 val = 0;
    uint32 mult = 1;
    while (i--)
    {
        val += ((uint32) (ctx->token[i] - '0')) * mult;
        mult *= 10;
    } // while

    ctx->token += len;
    ctx->tokenlen -= len;

    *_val = val;
    return 1;
} // ui32fromtoken


static int parse_register_name(Context *ctx, RegisterType *rtype, int *rnum)
{
    if (nexttoken(ctx) != TOKEN_IDENTIFIER)
    {
        fail(ctx, "Expected register");
        return 0;
    } // if

    int neednum = 1;
    int regnum = 0;
    RegisterType regtype = REG_TYPE_TEMP;

    // Watch out for substrings! oDepth must be checked before oD, since
    //  the latter will match either case.
    if (check_token_segment(ctx, "oDepth"))
    {
        regtype = REG_TYPE_DEPTHOUT;
        neednum = 0;
    } // else if
    else if (check_token_segment(ctx, "vFace"))
    {
        regtype = REG_TYPE_MISCTYPE;
        regnum = (int) MISCTYPE_TYPE_FACE;
        neednum = 0;
    } // else if
    else if (check_token_segment(ctx, "vPos"))
    {
        regtype = REG_TYPE_MISCTYPE;
        regnum = (int) MISCTYPE_TYPE_POSITION;
        neednum = 0;
    } // else if
    else if (check_token_segment(ctx, "oPos"))
    {
        regtype = REG_TYPE_RASTOUT;
        regnum = (int) RASTOUT_TYPE_POSITION;
        neednum = 0;
    } // else if
    else if (check_token_segment(ctx, "oFog"))
    {
        regtype = REG_TYPE_RASTOUT;
        regnum = (int) RASTOUT_TYPE_FOG;
        neednum = 0;
    } // else if
    else if (check_token_segment(ctx, "oPts"))
    {
        regtype = REG_TYPE_RASTOUT;
        regnum = (int) RASTOUT_TYPE_POINT_SIZE;
        neednum = 0;
    } // else if
    else if (check_token_segment(ctx, "aL"))
    {
        regtype = REG_TYPE_LOOP;
        neednum = 0;
    } // else if
    else if (check_token_segment(ctx, "oC"))
        regtype = REG_TYPE_COLOROUT;
    else if (check_token_segment(ctx, "oT"))
        regtype = REG_TYPE_OUTPUT;
    else if (check_token_segment(ctx, "oD"))
        regtype = REG_TYPE_ATTROUT;
    else if (check_token_segment(ctx, "r"))
        regtype = REG_TYPE_TEMP;
    else if (check_token_segment(ctx, "v"))
        regtype = REG_TYPE_INPUT;
    else if (check_token_segment(ctx, "c"))
        regtype = REG_TYPE_CONST;
    else if (check_token_segment(ctx, "i"))
        regtype = REG_TYPE_CONSTINT;
    else if (check_token_segment(ctx, "b"))
        regtype = REG_TYPE_CONSTBOOL;
    else if (check_token_segment(ctx, "s"))
        regtype = REG_TYPE_SAMPLER;
    else if (check_token_segment(ctx, "l"))
        regtype = REG_TYPE_LABEL;
    else if (check_token_segment(ctx, "p"))
        regtype = REG_TYPE_PREDICATE;
    else if (check_token_segment(ctx, "o"))
        regtype = REG_TYPE_OUTPUT;
    else if (check_token_segment(ctx, "a"))
        regtype = REG_TYPE_ADDRESS;
    else if (check_token_segment(ctx, "t"))
        regtype = REG_TYPE_ADDRESS;
        
    //case REG_TYPE_TEMPFLOAT16:  // !!! FIXME: don't know this asm string

    else
    {
        fail(ctx, "expected register type");
        regtype = REG_TYPE_CONST;
        regnum = 0;
        neednum = 0;
    } // else

    // "c[5]" is the same as "c5", so if the token is done, see if next is '['.
    if ((neednum) && (ctx->tokenlen == 0))
    {
        if (nexttoken(ctx) == ((Token) '['))
            neednum = 0;  // don't need a number on register name itself.
        pushback(ctx);
    } // if

    if (neednum)
    {
        uint32 ui32 = 0;
        if (!ui32fromtoken(ctx, &ui32))
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
    DestArgInfo *info = &ctx->dest_arg;
    memset(info, '\0', sizeof (DestArgInfo));

    // parse_instruction_token() sets ctx->token to the end of the instruction
    //  so we can see if there are destination modifiers on the instruction
    //  itself...

    int invalid_modifier = 0;

    while ((ctx->tokenlen > 0) && (!invalid_modifier))
    {
        if (check_token_segment(ctx, "_x2"))
            set_result_shift(ctx, info, 0x1);
        else if (check_token_segment(ctx, "_x4"))
            set_result_shift(ctx, info, 0x2);
        else if (check_token_segment(ctx, "_x8"))
            set_result_shift(ctx, info, 0x3);
        else if (check_token_segment(ctx, "_d8"))
            set_result_shift(ctx, info, 0xD);
        else if (check_token_segment(ctx, "_d4"))
            set_result_shift(ctx, info, 0xE);
        else if (check_token_segment(ctx, "_d2"))
            set_result_shift(ctx, info, 0xF);
        else if (check_token_segment(ctx, "_sat"))
            info->result_mod |= MOD_SATURATE;
        else if (check_token_segment(ctx, "_pp"))
            info->result_mod |= MOD_PP;
        else if (check_token_segment(ctx, "_centroid"))
            info->result_mod |= MOD_CENTROID;
        else
            invalid_modifier = 1;
    } // while

    if (invalid_modifier)
        fail(ctx, "Invalid destination modifier");

    // !!! FIXME: predicates.
    if (nexttoken(ctx) == ((Token) '('))
        fail(ctx, "Predicates unsupported at this time");  // !!! FIXME: ...

    pushback(ctx);  // parse_register_name calls nexttoken().

    parse_register_name(ctx, &info->regtype, &info->regnum);
    // parse_register_name() can't check this: dest regs might have modifiers.
    if (ctx->tokenlen > 0)
        fail(ctx, "invalid register name");

    // !!! FIXME: can dest registers do relative addressing?

    int invalid_writemask = 0;
    int implicit_writemask = 0;
    if (nexttoken(ctx) != ((Token) '.'))
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
    else if (nexttoken(ctx) != TOKEN_IDENTIFIER)
        invalid_writemask = 1;
    else
    {
        // !!! FIXME: is out-of-order okay (yxzw instead of xyzw?)
        char tokenbytes[5] = { '\0', '\0', '\0', '\0', '\0' };
        const unsigned int tokenlen = ctx->tokenlen;
        memcpy(tokenbytes, ctx->token, ((tokenlen < 4) ? tokenlen : 4));
        char *ptr = tokenbytes;

        info->writemask0 = info->writemask1 = info->writemask2 = info->writemask3 = 0;
        if (*ptr == 'x') { info->writemask0 = 1; ptr++; }
        if (*ptr == 'y') { info->writemask1 = 1; ptr++; }
        if (*ptr == 'z') { info->writemask2 = 1; ptr++; }
        if (*ptr == 'w') { info->writemask3 = 1; ptr++; }
        if ((ptr == ctx->token) && (shader_is_pixel(ctx)))
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
    int retval = 1;

    if (ctx->tokenbufpos >= STATICARRAYLEN(ctx->tokenbuf))
    {
        fail(ctx, "Too many tokens");
        return 0;
    } // if

    // mark this now, so optional relative addressing token is placed second.
    uint32 *outtoken = &ctx->tokenbuf[ctx->tokenbufpos++];
    *outtoken = 0;

    SourceMod srcmod = SRCMOD_NONE;
    int negate = 0;
    Token token = nexttoken(ctx);

    if (token == ((Token) '!'))
        srcmod = SRCMOD_NOT;
    else if (token == ((Token) '-'))
        negate = 1;
    else if ( (token == TOKEN_INT_LITERAL) && (check_token(ctx, "1")) )
    {
        if (nexttoken(ctx) != ((Token) '-'))
            fail(ctx, "Unexpected token");
        else
            srcmod = SRCMOD_COMPLEMENT;
    } // else
    else
    {
        pushback(ctx);
    } // else

    RegisterType regtype;
    int regnum;
    parse_register_name(ctx, &regtype, &regnum);

    if (ctx->tokenlen > 0)
    {
        if (check_token_segment(ctx, "_bias"))
            set_source_mod(ctx, negate, SRCMOD_BIAS, SRCMOD_BIASNEGATE, &srcmod);
        else if (check_token_segment(ctx, "_bx2"))
            set_source_mod(ctx, negate, SRCMOD_SIGN, SRCMOD_SIGNNEGATE, &srcmod);
        else if (check_token_segment(ctx, "_x2"))
            set_source_mod(ctx, negate, SRCMOD_X2, SRCMOD_X2NEGATE, &srcmod);
        else if (check_token_segment(ctx, "_dz"))
            set_source_mod(ctx, negate, SRCMOD_DZ, SRCMOD_NONE, &srcmod);
        else if (check_token_segment(ctx, "_dw"))
            set_source_mod(ctx, negate, SRCMOD_DW, SRCMOD_NONE, &srcmod);
        else if (check_token_segment(ctx, "_abs"))
            set_source_mod(ctx, negate, SRCMOD_ABS, SRCMOD_ABSNEGATE, &srcmod);
        else
            fail(ctx, "Invalid source modifier");
    } // if

    uint32 relative = 0;
    if (nexttoken(ctx) != ((Token) '['))
        pushback(ctx);  // not relative addressing?
    else
    {
        if (!relok)
            fail(ctx, "Relative addressing not permitted here.");
        else
            retval++;

        parse_source_token_maybe_relative(ctx, 0);
        relative = 1;

        if (nexttoken(ctx) != ((Token) '+'))
            pushback(ctx);
        else
        {
            // !!! FIXME: maybe c3[a0.x + 5] is legal and becomes c[a0.x + 8] ?
            if (regnum != 0)
                fail(ctx, "Relative addressing with explicit register number.");

            uint32 ui32 = 0;
            if ( (nexttoken(ctx) != TOKEN_INT_LITERAL) ||
                 (!ui32fromtoken(ctx, &ui32)) ||
                 (ctx->tokenlen != 0) )
            {
                fail(ctx, "Invalid relative addressing offset");
            } // if
            regnum += (int) ui32;
        } // else

        if (nexttoken(ctx) != ((Token) ']'))
            fail(ctx, "Expected ']'");
    } // else

    int invalid_swizzle = 0;
    uint32 swizzle = 0;
    if (nexttoken(ctx) != ((Token) '.'))
    {
        swizzle = 0xE4;  // 0xE4 == 11100100 ... 0 1 2 3. No swizzle.
        pushback(ctx);  // no explicit writemask; do full mask.
    } // if
    else if (scalar_register(ctx->shader_type, regtype, regnum))
        fail(ctx, "Swizzle specified for scalar register");
    else if (nexttoken(ctx) != TOKEN_IDENTIFIER)
        invalid_swizzle = 1;
    else
    {
        char tokenbytes[5] = { '\0', '\0', '\0', '\0', '\0' };
        const unsigned int tokenlen = ctx->tokenlen;
        memcpy(tokenbytes, ctx->token, ((tokenlen < 4) ? tokenlen : 4));

        // deal with shortened form (.x = .xxxx, etc).
        if (tokenlen == 1)
            tokenbytes[1] = tokenbytes[2] = tokenbytes[3] = tokenbytes[0];
        else if (tokenlen == 2)
            tokenbytes[2] = tokenbytes[3] = tokenbytes[1];
        else if (tokenlen == 3)
            tokenbytes[3] = tokenbytes[2];
        else if (tokenlen != 4)
            invalid_swizzle = 1;
        tokenbytes[4] = '\0';

        uint32 val = 0;
        int saw_xyzw = 0;
        int saw_rgba = 0;
        int i;
        for (i = 0; i < 4; i++)
        {
            const int component = (int) tokenbytes[i];
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
        else if (saw_rgba && !shader_is_pixel(ctx))
            invalid_swizzle = 1;
    } // else

    if (invalid_swizzle)
        fail(ctx, "Invalid swizzle");

    *outtoken = ( ((((uint32) 1)) << 31) |
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


static int parse_num(Context *ctx, const int floatok, uint32 *value)
{
    union { float f; int32 si32; uint32 ui32; } cvt;
    int negative = 0;
    Token token = nexttoken(ctx);

    if (token == ((Token) '-'))
    {
        negative = 1;
        token = nexttoken(ctx);
    } // if

    if (token == TOKEN_INT_LITERAL)
    {
        int d = 0;
        sscanf(ctx->token, "%d", &d);
        if (floatok)
            cvt.f = (float) ((negative) ? -d : d);
        else
            cvt.si32 = (int32) ((negative) ? -d : d);
    } // if
    else if (token == TOKEN_FLOAT_LITERAL)
    {
        if (!floatok)
        {
            fail(ctx, "Expected whole number");
            *value = 0;
            return 0;
        } // if
        sscanf(ctx->token, "%f", &cvt.f);
        if (negative)
            cvt.f = -cvt.f;
    } // if
    else
    {
        fail(ctx, "Expected number");
        *value = 0;
        return 0;
    } // else

    *value = cvt.ui32;
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
    parse_destination_token(ctx);
    require_comma(ctx);

    // !!! FIXME: do a TOKEN_TRUE and TOKEN_FALSE? Is this case-sensitive?
    const Token token = nexttoken(ctx);

    int bad = 0;
    if (token != TOKEN_IDENTIFIER)
        bad = 1;
    else if (check_token_segment(ctx, "true"))
        ctx->tokenbuf[ctx->tokenbufpos++] = 1;
    else if (check_token_segment(ctx, "false"))
        ctx->tokenbuf[ctx->tokenbufpos++] = 0;
    else
        bad = 1;

    if (ctx->tokenlen != 0)
        bad = 1;

    if (bad)
        fail(ctx, "Expected 'true' or 'false'");

    return 3;
} // parse_args_DEFB


static int parse_dcl_usage(Context *ctx, uint32 *val, int *issampler)
{
    int i;
    static const char *samplerusagestrs[] = { "_2d", "_cube", "_volume" };
    static const char *usagestrs[] = {
        "_position", "_blendweight", "_blendindices", "_normal", "_psize",
        "_texcoord", "_tangent", "_binormal", "_tessfactor", "_positiont",
        "_color", "_fog", "_depth", "_sample"
    };

    for (i = 0; i < STATICARRAYLEN(usagestrs); i++)
    {
        if (check_token_segment(ctx, usagestrs[i]))
        {
            *issampler = 0;
            *val = i;
            return 1;
        } // if
    } // for

    for (i = 0; i < STATICARRAYLEN(samplerusagestrs); i++)
    {
        if (check_token_segment(ctx, samplerusagestrs[i]))
        {
            *issampler = 1;
            *val = i + 2;
            return 1;
        } // if
    } // for

    *issampler = 0;
    *val = 0;
    return 0;
} // parse_dcl_usage


static int parse_args_DCL(Context *ctx)
{
    int issampler = 0;
    uint32 usage = 0;
    uint32 index = 0;

    ctx->tokenbufpos++;  // save a spot for the usage/index token.
    ctx->tokenbuf[0] = 0;

    // parse_instruction_token() sets ctx->token to the end of the instruction
    //  so we can see if there are destination modifiers on the instruction
    //  itself...

    if (parse_dcl_usage(ctx, &usage, &issampler))
    {
        if ((ctx->tokenlen > 0) && (*ctx->token != '_'))
        {
            if (!ui32fromtoken(ctx, &index))
                fail(ctx, "Expected usage index");
        } // if
    } // if

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
    static const char *comps[] = { "_gt", "_eq", "_ge", "_lt", "_ne", "_le" };
    int i;

    if (ctx->tokenlen >= 3)
    {
        for (i = 1; i < STATICARRAYLEN(comps); i++)
        {
            if (check_token_segment(ctx, comps[i]))
            {
                *controls = i + 1;
                return 1;
            } // if
        } // for
    } // if

    return 0;
} // parse_condition

static inline int Min(const int a, const int b)
{
    return ((a < b) ? a : b);
} // Min

static int parse_instruction_token(Context *ctx, Token token)
{
    int coissue = 0;
    int predicated = 0;

    if (token == ((Token) '+'))
    {
        coissue = 1;
        token = nexttoken(ctx);
    } // if

    if (token != TOKEN_IDENTIFIER)
    {
        fail(ctx, "Expected instruction");
        return 0;
    } // if

    uint32 controls = 0;
    uint32 opcode = OPCODE_TEXLD;
    const char *origtoken = ctx->token;
    const unsigned int origtokenlen = ctx->tokenlen;

    // This might need to be TEXLD instead of TEXLDP.
    if (check_token_segment(ctx, "TEXLDP"))
        controls = CONTROL_TEXLDP;

    // This might need to be TEXLD instead of TEXLDB.
    else if (check_token_segment(ctx, "TEXLDB"))
        controls = CONTROL_TEXLDB;

    else  // find the instruction.
    {
        int i;
        for (i = 0; i < STATICARRAYLEN(instructions); i++)
        {
            const char *opcode_string = instructions[i].opcode_string;
            if (opcode_string == NULL)
                continue;  // skip this.
            else if (!check_token_segment(ctx, opcode_string))
                continue;  // not us.
            else if ((ctx->tokenlen > 0) && (*ctx->token != '_'))
            {
                ctx->token = origtoken;
                ctx->tokenlen = origtokenlen;
                continue;  // not the match: TEXLD when we wanted TEXLDL, etc.
            } // if

            break;  // found it!
        } // for

        opcode = (uint32) i;

        // This might need to be IFC instead of IF.
        if (opcode == OPCODE_IF)
        {
            if (parse_condition(ctx, &controls))
                opcode = OPCODE_IFC;
        } // if

        // This might need to be BREAKC instead of BREAK.
        else if (opcode == OPCODE_BREAK)
        {
            if (parse_condition(ctx, &controls))
                opcode = OPCODE_BREAKC;
        } // else if

        // SETP has a conditional code, always.
        else if (opcode == OPCODE_SETP)
        {
            if (!parse_condition(ctx, &controls))
                fail(ctx, "SETP requires a condition");
        } // else if
    } // else

    if ( (opcode == STATICARRAYLEN(instructions)) ||
         ((ctx->tokenlen > 0) && (ctx->token[0] != '_')) )
    {
        char opstr[32];
        const int len = Min(sizeof (opstr) - 1, origtokenlen);
        memcpy(opstr, origtoken, len);
        opstr[len] = '\0';
        failf(ctx, "Unknown instruction '%s'", opstr);
        return 0;
    } // if

    const Instruction *instruction = &instructions[opcode];

    // !!! FIXME: predicated instructions

    ctx->tokenbufpos = 0;

    const int tokcount = instruction->parse_args(ctx);

    // insttoks bits are reserved and should be zero if < SM2.
    const uint32 insttoks = shader_version_atleast(ctx, 2, 0) ? tokcount-1 : 0;

    // write out the instruction token.
    output_token(ctx, ((opcode & 0xFFFF) << 0) |
                      ((controls & 0xFF) << 16) |
                      ((insttoks & 0xF) << 24) |
                      ((coissue) ? 0x40000000 : 0x00000000) |
                      ((predicated) ? 0x10000000 : 0x00000000) );

    // write out the argument tokens.
    int i;
    for (i = 0; i < (tokcount-1); i++)
        output_token(ctx, ctx->tokenbuf[i]);

    return 1;
} // parse_instruction_token


static void parse_version_token(Context *ctx)
{
    int bad = 0;
    uint32 shader_type = 0;
    if (nexttoken(ctx) != TOKEN_IDENTIFIER)
        bad = 1;
    else if (check_token_segment(ctx, "vs"))
    {
        ctx->shader_type = MOJOSHADER_TYPE_VERTEX;
        shader_type = 0xFFFE;
    } // if
    else if (check_token_segment(ctx, "ps"))
    {
        ctx->shader_type = MOJOSHADER_TYPE_PIXEL;
        shader_type = 0xFFFF;
    } // if
    else
    {
        // !!! FIXME: geometry shaders?
        bad = 1;
    } // else

    uint32 major = 0;
    uint32 minor = 0;

    if ( (!check_token_segment(ctx, ".")) && (!check_token_segment(ctx, "_")) )
        bad = 1;
    else if (check_token_segment(ctx, "1"))
        major = 1;
    else if (check_token_segment(ctx, "2"))
        major = 2;
    else if (check_token_segment(ctx, "3"))
        major = 3;
    else
        bad = 1;

    if ( (!check_token_segment(ctx, ".")) && (!check_token_segment(ctx, "_")) )
        bad = 1;
    else if (check_token_segment(ctx, "0"))
        minor = 0;
    else if (check_token_segment(ctx, "x"))
        minor = 1;
    else if (check_token_segment(ctx, "sw"))
        minor = 255;
    else
        bad = 1;

    if (ctx->tokenlen != 0)
        bad = 1;

    if (bad)
        fail(ctx, "Expected valid version string");

    ctx->major_ver = major;
    ctx->minor_ver = minor;

    ctx->version_token = (shader_type << 16) | (major << 8) | (minor << 0);
    output_token(ctx, ctx->version_token);
} // parse_version_token


static void parse_phase_token(Context *ctx)
{
    output_token(ctx, 0x0000FFFD); // phase token always 0x0000FFFD.
} // parse_phase_token


static void parse_end_token(Context *ctx)
{
    // We don't emit the end token bits here, since it's valid for a shader
    //  to not specify an "end" string at all; it's implicit, in that case.
    // Instead, we make sure if we see "end" that it's the last thing we see.
    if (nexttoken(ctx) != TOKEN_EOI)
        fail(ctx, "Content after END");
} // parse_end_token


static void parse_token(Context *ctx, const Token token)
{
    if (token != TOKEN_IDENTIFIER)
        parse_instruction_token(ctx, token);  // might be a coissue '+', etc.
    else
    {
        if (check_token(ctx, "end"))
            parse_end_token(ctx);
        else if (check_token(ctx, "phase"))
            parse_phase_token(ctx);
        else
            parse_instruction_token(ctx, token);
    } // if
} // parse_token


static Context *build_context(const char *filename,
                              const char *source, unsigned int sourcelen,
                              const MOJOSHADER_preprocessorDefine **defines,
                              unsigned int define_count,
                              MOJOSHADER_includeOpen include_open,
                              MOJOSHADER_includeClose include_close,
                              MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    if (m == NULL) m = MOJOSHADER_internal_malloc;
    if (f == NULL) f = MOJOSHADER_internal_free;

    // !!! FIXME
    if (include_open == NULL) include_open = (MOJOSHADER_includeOpen) 0x1;
    if (include_close == NULL) include_close = (MOJOSHADER_includeClose) 0x1;

    Context *ctx = (Context *) m(sizeof (Context), d);
    if (ctx == NULL)
        return NULL;

    memset(ctx, '\0', sizeof (Context));
    ctx->malloc = m;
    ctx->free = f;
    ctx->malloc_data = d;
    ctx->parse_phase = MOJOSHADER_PARSEPHASE_NOTSTARTED;
    ctx->preprocessor = preprocessor_start(filename, source, sourcelen,
                                           include_open, include_close,
                                           defines, define_count, m, f, d);

    if (ctx->preprocessor == NULL)
    {
        f(ctx, d);
        return NULL;
    } // if

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
        MOJOSHADER_free f = ((ctx->free != NULL) ? ctx->free : MOJOSHADER_internal_free);
        void *d = ctx->malloc_data;
        free_error_list(f, d, ctx->errors);
        free_filename_cache(ctx);
        if (ctx->preprocessor != NULL)
            preprocessor_end(ctx->preprocessor);
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
    assert(isfail(ctx));

    if (ctx->out_of_memory)
        return &MOJOSHADER_out_of_mem_data;
        
    MOJOSHADER_parseData *retval = NULL;
    retval = (MOJOSHADER_parseData*) Malloc(ctx, sizeof(MOJOSHADER_parseData));
    if (retval == NULL)
        return &MOJOSHADER_out_of_mem_data;

    memset(retval, '\0', sizeof (MOJOSHADER_parseData));
    retval->malloc = (ctx->malloc == MOJOSHADER_internal_malloc) ? NULL : ctx->malloc;
    retval->free = (ctx->free == MOJOSHADER_internal_free) ? NULL : ctx->free;
    retval->malloc_data = ctx->malloc_data;

    retval->error_count = ctx->error_count;
    retval->errors = build_errors(ctx);
    if ((retval->errors == NULL) && (ctx->error_count > 0))
    {
        Free(ctx, retval);
        return &MOJOSHADER_out_of_mem_data;
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

const MOJOSHADER_parseData *MOJOSHADER_assemble(const char *filename,
                             const char *source, unsigned int sourcelen,
                             const char **comments, unsigned int comment_count,
                             const MOJOSHADER_symbol *symbols,
                             unsigned int symbol_count,
                             const MOJOSHADER_preprocessorDefine **defines,
                             unsigned int define_count,
                             MOJOSHADER_includeOpen include_open,
                             MOJOSHADER_includeClose include_close,
                             MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    MOJOSHADER_parseData *retval = NULL;
    Context *ctx = NULL;

    if ( ((m == NULL) && (f != NULL)) || ((m != NULL) && (f == NULL)) )
        return &MOJOSHADER_out_of_mem_data;  // supply both or neither.

    ctx = build_context(filename, source, sourcelen, defines, define_count,
                        include_open, include_close, m, f, d);
    if (ctx == NULL)
        return &MOJOSHADER_out_of_mem_data;

    // Version token always comes first.
    ctx->parse_phase = MOJOSHADER_PARSEPHASE_WORKING;
    parse_version_token(ctx);
    output_comments(ctx, comments, comment_count, symbols, symbol_count);

    // parse out the rest of the tokens after the version token...
    Token token;
    while ((token = nexttoken(ctx)) != TOKEN_EOI)
        parse_token(ctx, token);

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
        int i;
        for (i = 0; i < retval->error_count; i++)
        {
            MOJOSHADER_error *error = &retval->errors[i];
            if (error->error_position >= 0)
            {
                assert(retval != &MOJOSHADER_out_of_mem_data);
                assert((error->error_position % sizeof (uint32)) == 0);

                const int pos = error->error_position / sizeof (uint32);
                if (pos >= ctx->output_len)
                    error->error_position = -1;  // oh well.
                else
                {
                    const SourcePos *srcpos = &ctx->token_to_source[pos];
                    Free(ctx, (void *) error->filename);
                    char *fname = NULL;
                    if (srcpos->filename != NULL)
                        fname = StrDup(ctx, srcpos->filename);
                    error->error_position = srcpos->line;
                    error->filename = fname;  // may be NULL, that's okay.
                } // else
            } // if
        } // for
    } // if

    destroy_context(ctx);
    return retval;
} // MOJOSHADER_assemble

// end of mojoshader_assembler.c ...

