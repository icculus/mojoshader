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

#if DEBUG_PREPROCESSOR
    #define print_debug_token(token, len, val) \
        MOJOSHADER_print_debug_token("PREPROCESSOR", token, len, val)
#else
    #define print_debug_token(token, len, val)
#endif

typedef struct DefineHash
{
    MOJOSHADER_preprocessorDefine define;
    struct DefineHash *next;
} DefineHash;

typedef struct Context
{
    int isfail;
    int out_of_memory;
    char failstr[128];
    IncludeState *include_stack;
    DefineHash *define_hashtable[256];
    int pushedback;
    const char *token;
    unsigned int tokenlen;
    MOJOSHADER_includeOpen open_callback;
    MOJOSHADER_includeClose close_callback;
    MOJOSHADER_malloc malloc;
    MOJOSHADER_free free;
    void *malloc_data;
} Context;


// Convenience functions for allocators...

static inline void out_of_memory(Context *ctx)
{
    ctx->out_of_memory = 1;
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

static inline char *StrDup(Context *ctx, const char *str)
{
    char *retval = (char *) Malloc(ctx, strlen(str) + 1);
    if (retval != NULL)
        strcpy(retval, str);
    return retval;
} // StrDup

static void failf(Context *ctx, const char *fmt, ...) ISPRINTF(2,3);
static void failf(Context *ctx, const char *fmt, ...)
{
    ctx->isfail = 1;
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(ctx->failstr, sizeof (ctx->failstr), fmt, ap);
    va_end(ap);
} // failf

static inline void fail(Context *ctx, const char *reason)
{
    failf(ctx, "%s", reason);
} // fail


#if DEBUG_TOKENIZER
void MOJOSHADER_print_debug_token(const char *subsystem, const char *token,
                                  const unsigned int tokenlen,
                                  const Token tokenval)
{
    printf("%s TOKEN: \"", subsystem);
    unsigned int i;
    for (i = 0; i < tokenlen; i++)
    {
        if (token[i] == '\n')
            printf("\\n");
        else
            printf("%c", token[i]);
    } // for
    printf("\" (");
    switch (tokenval)
    {
        #define TOKENCASE(x) case x: printf("%s", #x); break
        TOKENCASE(TOKEN_UNKNOWN);
        TOKENCASE(TOKEN_IDENTIFIER);
        TOKENCASE(TOKEN_INT_LITERAL);
        TOKENCASE(TOKEN_FLOAT_LITERAL);
        TOKENCASE(TOKEN_STRING_LITERAL);
        TOKENCASE(TOKEN_ADDASSIGN);
        TOKENCASE(TOKEN_SUBASSIGN);
        TOKENCASE(TOKEN_MULTASSIGN);
        TOKENCASE(TOKEN_DIVASSIGN);
        TOKENCASE(TOKEN_MODASSIGN);
        TOKENCASE(TOKEN_XORASSIGN);
        TOKENCASE(TOKEN_ANDASSIGN);
        TOKENCASE(TOKEN_ORASSIGN);
        TOKENCASE(TOKEN_INCREMENT);
        TOKENCASE(TOKEN_DECREMENT);
        TOKENCASE(TOKEN_RSHIFT);
        TOKENCASE(TOKEN_LSHIFT);
        TOKENCASE(TOKEN_ANDAND);
        TOKENCASE(TOKEN_OROR);
        TOKENCASE(TOKEN_LEQ);
        TOKENCASE(TOKEN_GEQ);
        TOKENCASE(TOKEN_EQL);
        TOKENCASE(TOKEN_NEQ);
        TOKENCASE(TOKEN_HASHHASH);
        TOKENCASE(TOKEN_PP_INCLUDE);
        TOKENCASE(TOKEN_PP_LINE);
        TOKENCASE(TOKEN_PP_DEFINE);
        TOKENCASE(TOKEN_PP_UNDEF);
        TOKENCASE(TOKEN_PP_IF);
        TOKENCASE(TOKEN_PP_IFDEF);
        TOKENCASE(TOKEN_PP_IFNDEF);
        TOKENCASE(TOKEN_PP_ELSE);
        TOKENCASE(TOKEN_PP_ELIF);
        TOKENCASE(TOKEN_PP_ENDIF);
        TOKENCASE(TOKEN_PP_ERROR);
        TOKENCASE(TOKEN_INCOMPLETE_COMMENT);
        TOKENCASE(TOKEN_BAD_CHARS);
        TOKENCASE(TOKEN_EOI);
        TOKENCASE(TOKEN_PREPROCESSING_ERROR);
        #undef TOKENCASE

        case ((Token) '\n'):
            printf("'\\n'");
            break;

        default:
            assert(((int)tokenval) < 256);
            printf("'%c'", (char) tokenval);
            break;
    } // switch
    printf(")\n");
} // MOJOSHADER_print_debug_token
#endif


// Preprocessor define hashtable stuff...

static unsigned char hash_define(const char *sym)
{
    unsigned char retval = 0;
    while (sym)
        retval += *(sym++);
    return retval;
} // hash_define


static int add_define(Context *ctx, const char *sym, const char *val)
{
    char *identifier = NULL;
    char *definition = NULL;
    const unsigned char hash = hash_define(sym);
    DefineHash *bucket = ctx->define_hashtable[hash];
    while (bucket)
    {
        if (strcmp(bucket->define.identifier, sym) == 0)
        {
            failf(ctx, "'%s' already defined", sym);
            return 0;
        } // if
        bucket = bucket->next;
    } // while

    bucket = (DefineHash *) Malloc(ctx, sizeof (DefineHash));
    if (bucket == NULL)
        return 0;

    identifier = (char *) Malloc(ctx, strlen(sym) + 1);
    definition = (char *) Malloc(ctx, strlen(val) + 1);
    if ((identifier == NULL) || (definition == NULL))
    {
        Free(ctx, identifier);
        Free(ctx, definition);
        Free(ctx, bucket);
        return 0;
    } // if

    strcpy(identifier, sym);
    strcpy(definition, val);
    bucket->define.identifier = identifier;
    bucket->define.definition = definition;
    bucket->next = ctx->define_hashtable[hash];
    ctx->define_hashtable[hash] = bucket;
    return 1;
} // add_define


static int remove_define(Context *ctx, const char *sym)
{
    const unsigned char hash = hash_define(sym);
    DefineHash *bucket = ctx->define_hashtable[hash];
    DefineHash *prev = NULL;
    while (bucket)
    {
        if (strcmp(bucket->define.identifier, sym) == 0)
        {
            if (prev == NULL)
                ctx->define_hashtable[hash] = bucket->next;
            else
                prev->next = bucket->next;
            Free(ctx, (void *) bucket->define.identifier);
            Free(ctx, (void *) bucket->define.definition);
            Free(ctx, bucket);
            return 1;
        } // if
        prev = bucket;
        bucket = bucket->next;
    } // while

    failf(ctx, "'%s' not defined", sym);
    return 0;
} // remove_define


static const char *find_define(Context *ctx, const char *sym)
{
    const unsigned char hash = hash_define(sym);
    DefineHash *bucket = ctx->define_hashtable[hash];
    while (bucket)
    {
        if (strcmp(bucket->define.identifier, sym) == 0)
            return bucket->define.definition;
        bucket = bucket->next;
    } // while
    return NULL;
} // find_define


static void free_all_defines(Context *ctx)
{
    int i;
    for (i = 0; i < STATICARRAYLEN(ctx->define_hashtable); i++)
    {
        DefineHash *bucket = ctx->define_hashtable[i];
        ctx->define_hashtable[i] = NULL;
        while (bucket)
        {
            DefineHash *next = bucket->next;
            Free(ctx, (void *) bucket->define.identifier);
            Free(ctx, (void *) bucket->define.definition);
            Free(ctx, bucket);
            bucket = next;
        } // while
    } // for
} // find_define


static int push_source(Context *ctx, const char *fname, const char *source,
                       unsigned int srclen, int included)
{
    IncludeState *state = (IncludeState *) Malloc(ctx, sizeof (IncludeState));
    if (state == NULL)
        return 0;
    memset(state, '\0', sizeof (IncludeState));

    if (fname != NULL)
    {
        state->filename = StrDup(ctx, fname);
        if (state->filename == NULL)
        {
            Free(ctx, state);
            return 0;
        } // if
    } // if

    state->included = included;
    state->source_base = source;
    state->source = source;
    state->token = source;
    state->bytes_left = srclen;
    state->line = 1;
    state->next = ctx->include_stack;

    ctx->include_stack = state;

    return 1;
} // push_source


static void pop_source(Context *ctx)
{
    IncludeState *state = ctx->include_stack;
    if (state == NULL)
        return;

    if (state->included)
    {
        ctx->close_callback(state->source_base, ctx->malloc,
                            ctx->free, ctx->malloc_data);
    } // if

    ctx->include_stack = state->next;
    Free(ctx, state->filename);
    Free(ctx, state);
} // pop_source


Preprocessor *preprocessor_start(const char *fname, const char *source,
                            unsigned int sourcelen,
                            MOJOSHADER_includeOpen open_callback,
                            MOJOSHADER_includeClose close_callback,
                            const MOJOSHADER_preprocessorDefine **defines,
                            unsigned int define_count,
                            MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    int okay = 1;
    int i = 0;

    // the preprocessor is internal-only, so we verify all these are != NULL.
    assert(m != NULL);
    assert(f != NULL);
    assert(open_callback != NULL);
    assert(close_callback != NULL);

    Context *ctx = (Context *) m(sizeof (Context), d);
    if (ctx == NULL)
        return 0;

    memset(ctx, '\0', sizeof (Context));
    ctx->malloc = m;
    ctx->free = f;
    ctx->malloc_data = d;
    ctx->open_callback = open_callback;
    ctx->close_callback = close_callback;

    for (i = 0; i < define_count; i++)
    {
        if (!add_define(ctx, defines[i]->identifier, defines[i]->definition))
        {
            okay = 0;
            break;
        } // if
    } // for

    if ((okay) && (!push_source(ctx, fname, source, sourcelen, 0)))
        okay = 0;

    if (!okay)
    {
        preprocessor_end((Preprocessor *) ctx);
        return NULL;
    } // if

    return (Preprocessor *) ctx;
} // preprocessor_start


void preprocessor_end(Preprocessor *_ctx)
{
    Context *ctx = (Context *) _ctx;
    if (ctx == NULL)
        return;

    while (ctx->include_stack != NULL)
        pop_source(ctx);

    free_all_defines(ctx);

    Free(ctx, ctx);
} // preprocessor_end


int preprocessor_outofmemory(Preprocessor *_ctx)
{
    Context *ctx = (Context *) _ctx;
    return ctx->out_of_memory;
} // preprocessor_outofmemory


static inline const char *_preprocessor_nexttoken(Preprocessor *_ctx,
                                             unsigned int *_len, Token *_token)
{
    Context *ctx = (Context *) _ctx;

    while (1)
    {
        if (ctx->isfail)
        {
            ctx->isfail = 0;
            *_token = TOKEN_PREPROCESSING_ERROR;
            *_len = strlen(ctx->failstr);
            return ctx->failstr;
        } // if

        IncludeState *state = ctx->include_stack;
        if (state == NULL)
        {
            *_token = TOKEN_EOI;
            *_len = 0;
            return NULL;  // we're done!
        } // if

        Token token = preprocessor_internal_lexer(state);
        if (token == TOKEN_EOI)
        {
            assert(state->bytes_left == 0);
            pop_source(ctx);
            continue;  // pick up again after parent's #include line.
        } // if

        else if (token == TOKEN_INCOMPLETE_COMMENT)
        {
            fail(ctx, "Incomplete multiline comment");
            continue;  // will return at top of loop.
        } // else if

        *_token = token;
        *_len = (unsigned int) (state->source - state->token);
        return state->token;
    } // while

    assert(0 && "shouldn't hit this code");
    *_token = TOKEN_UNKNOWN;
    *_len = 0;
    return NULL;
} // _preprocessor_nexttoken


const char *preprocessor_nexttoken(Preprocessor *ctx, unsigned int *len,
                                   Token *token)
{
    const char *retval = _preprocessor_nexttoken(ctx, len, token);
    print_debug_token(retval, *len, *token);
    return retval;
} // preprocessor_nexttoken


const char *preprocessor_sourcepos(Preprocessor *_ctx, unsigned int *pos)
{
    Context *ctx = (Context *) _ctx;
    if (ctx->include_stack == NULL)
    {
        *pos = 0;
        return NULL;
    } // if

    *pos = ctx->include_stack->line;
    return ctx->include_stack->filename;
} // preprocessor_sourcepos


// public API...

static const MOJOSHADER_preprocessData out_of_mem_data_preprocessor = {
    1, &MOJOSHADER_out_of_mem_error, 0, 0, 0, 0, 0
};

#define BUFFER_LEN (64 * 1024)
typedef struct BufferList
{
    char buffer[BUFFER_LEN];
    size_t bytes;
    struct BufferList *next;
} BufferList;

typedef struct Buffer
{
    size_t total_bytes;
    BufferList head;
    BufferList *tail;
} Buffer;

static void buffer_init(Buffer *buffer)
{
    buffer->total_bytes = 0;
    buffer->head.bytes = 0;
    buffer->head.next = NULL;
    buffer->tail = &buffer->head;
} // buffer_init


static int add_to_buffer(Buffer *buffer, const char *data,
                         size_t len, MOJOSHADER_malloc m, void *d)
{
    buffer->total_bytes += len;
    while (len > 0)
    {
        const size_t avail = BUFFER_LEN - buffer->tail->bytes;
        const size_t cpy = (avail > len) ? len : avail;
        memcpy(buffer->tail->buffer + buffer->tail->bytes, data, cpy);
        len -= cpy;
        data += cpy;
        buffer->tail->bytes += cpy;
        assert(buffer->tail->bytes <= BUFFER_LEN);
        if (buffer->tail->bytes == BUFFER_LEN)
        {
            BufferList *item = (BufferList *) m(sizeof (BufferList), d);
            if (item == NULL)
                return 0;
            item->bytes = 0;
            item->next = NULL;
            buffer->tail->next = item;
            buffer->tail = item;
        } // if
    } // while

    return 1;
} // add_to_buffer


static int indent_buffer(Buffer *buffer, int n, int newline,
                         MOJOSHADER_malloc m, void *d)
{
    static char spaces[4] = { ' ', ' ', ' ', ' ' };
    if (newline)
    {
        while (n--)
        {
            if (!add_to_buffer(buffer, spaces, sizeof (spaces), m, d))
                return 0;
        } // while
    } // if
    else
    {
        if (!add_to_buffer(buffer, spaces, 1, m, d))
            return 0;
    } // else
    return 1;
} // indent_buffer


static char *flatten_buffer(Buffer *buffer, MOJOSHADER_malloc m, void *d)
{
    char *retval = m(buffer->total_bytes + 1, d);
    if (retval == NULL)
        return NULL;
    BufferList *item = &buffer->head;
    char *ptr = retval;
    while (item != NULL)
    {
        BufferList *next = item->next;
        memcpy(ptr, item->buffer, item->bytes);
        ptr += item->bytes;
        item = next;
    } // while
    *ptr = '\0';

    assert(ptr == (retval + buffer->total_bytes));
    return retval;
} // flatten_buffer


static void free_buffer(Buffer *buffer, MOJOSHADER_free f, void *d)
{
    // head is statically allocated, so start with head.next...
    BufferList *item = buffer->head.next;
    while (item != NULL)
    {
        BufferList *next = item->next;
        f(item, d);
        item = next;
    } // while
    buffer_init(buffer);
} // free_buffer


// !!! FIXME: cut and paste.
static void free_error_list(ErrorList *item, MOJOSHADER_free f, void *d)
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


// !!! FIXME: cut and paste.
static MOJOSHADER_error *build_errors(ErrorList **errors, const int count,
                         MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    int total = 0;
    MOJOSHADER_error *retval = (MOJOSHADER_error *)
                                m(sizeof (MOJOSHADER_error) * count, d);
    if (retval == NULL)
        return NULL;

    ErrorList *item = *errors;
    while (item != NULL)
    {
        ErrorList *next = item->next;
        // reuse the string allocations
        memcpy(&retval[total], &item->error, sizeof (MOJOSHADER_error));
        f(item, d);
        item = next;
        total++;
    } // while
    *errors = NULL;

    assert(total == count);
    return retval;
} // build_errors


const MOJOSHADER_preprocessData *MOJOSHADER_preprocess(const char *filename,
                             const char *source, unsigned int sourcelen,
                             const MOJOSHADER_preprocessorDefine **defines,
                             unsigned int define_count,
                             MOJOSHADER_includeOpen include_open,
                             MOJOSHADER_includeClose include_close,
                             MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    #ifdef _WINDOWS
    static const char endline[] = { '\r', '\n' };
    #else
    static const char endline[] = { '\n' };
    #endif

    ErrorList *errors = NULL;
    int error_count = 0;

    if (m == NULL) m = MOJOSHADER_internal_malloc;
    if (f == NULL) f = MOJOSHADER_internal_free;

// !!! FIXME
include_open = (MOJOSHADER_includeOpen) 0x1;
include_close = (MOJOSHADER_includeClose) 0x1;

    Preprocessor *pp = preprocessor_start(filename, source, sourcelen,
                                          include_open, include_close,
                                          defines, define_count, m, f, d);

    if (pp == NULL)
        return &out_of_mem_data_preprocessor;

    Token token = TOKEN_UNKNOWN;
    const char *tokstr = NULL;

    Buffer buffer;
    buffer_init(&buffer);

    int nl = 1;
    int indent = 0;
    unsigned int len = 0;
    int out_of_memory = 0;
    while ((tokstr = preprocessor_nexttoken(pp, &len, &token)) != NULL)
    {
        int isnewline = 0;

        assert(token != TOKEN_EOI);

        if (!out_of_memory)
            out_of_memory = preprocessor_outofmemory(pp);

        // Microsoft's preprocessor is weird.
        // It ignores newlines, and then inserts its own around certain
        //  tokens. For example, after a semicolon. This allows HLSL code to
        //  be mostly readable, instead of a stream of tokens.
        if (token == ((Token) '\n'))
            isnewline = nl;  // this doesn't actually care about '\n' ...

        else if ( (token == ((Token) '}')) || (token == ((Token) ';')) )
        {
            if (!out_of_memory)
            {
                if ( (token == ((Token) '}')) && (indent > 0) )
                    indent--;

                out_of_memory =
                    (!indent_buffer(&buffer, indent, nl, m, d)) ||
                    (!add_to_buffer(&buffer, tokstr, len, m, d)) ||
                    (!add_to_buffer(&buffer, endline, sizeof (endline), m, d));

                isnewline = 1;
            } // if
        } // if

        else if (token == ((Token) '{'))
        {
            if (!out_of_memory)
            {
                out_of_memory =
                    (!add_to_buffer(&buffer,endline,sizeof (endline),m,d)) ||
                    (!indent_buffer(&buffer, indent, 1, m, d)) ||
                    (!add_to_buffer(&buffer, "{", 1, m, d)) ||
                    (!add_to_buffer(&buffer,endline,sizeof (endline),m,d));
                indent++;
                isnewline = 1;
            } // if
        } // else if

        else if (token == TOKEN_PREPROCESSING_ERROR)
        {
            if (!out_of_memory)
            {
                ErrorList *error = (ErrorList *) m(sizeof (ErrorList), d);
                unsigned int pos = 0;
                char *fname = NULL;
                const char *str = preprocessor_sourcepos(pp, &pos);
                if (str != NULL)
                {
                    fname = (char *) m(strlen(str) + 1, d);
                    if (fname != NULL)
                        strcpy(fname, str);
                } // if

                // !!! FIXME: cut and paste with other error handlers.
                char *errstr = (char *) m(len + 1, d);
                if (errstr != NULL)
                    strcpy(errstr, tokstr);

                out_of_memory = ((!error) || ((!fname) && (str)) || (!errstr));
                if (out_of_memory)
                {
                    if (errstr) f(errstr, d);
                    if (fname) f(fname, d);
                    if (error) f(error, d);
                } // if
                else
                {
                    error->error.error = errstr;
                    error->error.filename = fname;
                    error->error.error_position = pos;
                    error->next = NULL;

                    ErrorList *prev = NULL;
                    ErrorList *item = errors;
                    while (item != NULL)
                    {
                        prev = item;
                        item = item->next;
                    } // while

                    if (prev == NULL)
                        errors = error;
                    else
                        prev->next = error;

                    error_count++;
                } // else
            } // if
        } // else if

        else
        {
            if (!out_of_memory)
            {
                out_of_memory = (!indent_buffer(&buffer, indent, nl, m, d)) ||
                                (!add_to_buffer(&buffer, tokstr, len, m, d));

            } // if
        } // else

        nl = isnewline;

        if (out_of_memory)
        {
            preprocessor_end(pp);
            free_buffer(&buffer, f, d);
            free_error_list(errors, f, d);
            return &out_of_mem_data_preprocessor;
        } // if
    } // while
    
    assert((token == TOKEN_EOI) || (out_of_memory));

    preprocessor_end(pp);

    const size_t total_bytes = buffer.total_bytes;
    char *output = flatten_buffer(&buffer, m, d);
    free_buffer(&buffer, f, d);
    if (output == NULL)
    {
        free_error_list(errors, f, d);
        return &out_of_mem_data_preprocessor;
    } // if

    MOJOSHADER_preprocessData *retval = (MOJOSHADER_preprocessData *)
                                    m(sizeof (MOJOSHADER_preprocessData), d);
    if (retval == NULL)
    {
        free_error_list(errors, f, d);
        f(output, d);
        return &out_of_mem_data_preprocessor;
    } // if

    retval->errors = build_errors(&errors, error_count, m, f, d);
    if (retval->errors == NULL)
    {
        free_error_list(errors, f, d);
        f(retval, d);
        f(output, d);
        return &out_of_mem_data_preprocessor;
    } // if

    retval->error_count = error_count;
    retval->output = output;
    retval->output_len = total_bytes;
    retval->malloc = m;
    retval->free = f;
    retval->malloc_data = d;
    return retval;
} // MOJOSHADER_preprocess


void MOJOSHADER_freePreprocessData(const MOJOSHADER_preprocessData *_data)
{
    MOJOSHADER_preprocessData *data = (MOJOSHADER_preprocessData *) _data;
    if ((data == NULL) || (data == &out_of_mem_data_preprocessor))
        return;

    MOJOSHADER_free f = (data->free == NULL) ? MOJOSHADER_internal_free : data->free;
    void *d = data->malloc_data;
    int i;

    if (data->output != NULL)
        f((void *) data->output, d);

    if (data->errors != NULL)
    {
        for (i = 0; i < data->error_count; i++)
        {
            if (data->errors[i].error != NULL)
                f((void *) data->errors[i].error, d);
            if (data->errors[i].filename != NULL)
                f((void *) data->errors[i].filename, d);
        } // for
        f(data->errors, d);
    } // if

    f(data, d);
} // MOJOSHADER_freePreprocessData


// end of mojoshader_preprocessor.c ...

