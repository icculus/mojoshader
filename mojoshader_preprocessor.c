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
    ctx->isfail = ctx->out_of_memory = 1;
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
    vsnprintf(ctx->failstr, sizeof (ctx->failstr), fmt, ap);  // rebuild it.
    va_end(ap);
} // failf

static inline void fail(Context *ctx, const char *reason)
{
    failf(ctx, "%s", reason);
} // fail


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
    state->insert_token = TOKEN_UNKNOWN;
    state->insert_token2 = TOKEN_UNKNOWN;
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


const char *preprocessor_error(Preprocessor *_ctx)
{
    Context *ctx = (Context *) _ctx;
    if (ctx->isfail)
    {
        ctx->isfail = 0;
        return ctx->failstr;
    } // if

    return NULL;
} // preprocessor_error


int preprocessor_outofmemory(Preprocessor *_ctx)
{
    Context *ctx = (Context *) _ctx;
    return ctx->out_of_memory;
} // preprocessor_outofmemory


const char *preprocessor_nexttoken(Preprocessor *_ctx, unsigned int *_len,
                                    Token *_token)
{
    Context *ctx = (Context *) _ctx;

    while (1)
    {
        IncludeState *state = ctx->include_stack;
        if (state == NULL)
        {
            *_token = TOKEN_EOI;
            *_len = 0;
            return NULL;  // we're done!
        } // if

        if (state->insert_token != TOKEN_UNKNOWN)
        {
            state->insert_tokchar = (char) state->insert_token;
            *_token = state->insert_token;
            *_len = 1;
            state->insert_token = TOKEN_UNKNOWN;
            return &state->insert_tokchar;
        } // if

        else if (state->insert_token2 != TOKEN_UNKNOWN)
        {
            state->insert_tokchar = (char) state->insert_token2;
            *_token = state->insert_token2;
            *_len = 1;
            state->insert_token2 = TOKEN_UNKNOWN;
            return &state->insert_tokchar;
        } // if

        Token token = preprocessor_internal_lexer(state);
        if (token == TOKEN_EOI)
        {
            assert(state->bytes_left == 0);
            pop_source(ctx);
            continue;  // pick up again after parent's #include line.
        } // if

        // Microsoft's preprocessor is weird.
        // It ignores newlines, and then inserts its own around certain
        //  tokens. For example, after a semicolon. This allows HLSL code to
        //  be mostly readable, and lets the ';' work as a single line comment
        //  in the assembler.
        if ( (token == ((Token) ';')) || (token == ((Token) '}')) )
            state->insert_token = (Token) '\n';
        else if (token == ((Token) '{'))
        {
            state->insert_token = (Token) '{';
            state->insert_token2 = (Token) '\n';
            state->insert_tokchar = '\n';
            *_token = (Token) '\n';
            *_len = 1;
            return &state->insert_tokchar;
        } // else if

        *_token = token;
        *_len = (unsigned int) (state->source - state->token);
        return state->token;
    } // while
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


const MOJOSHADER_preprocessData *MOJOSHADER_preprocess(const char *source,
                             unsigned int sourcelen,
                             const MOJOSHADER_preprocessorDefine **defines,
                             unsigned int define_count,
                             MOJOSHADER_includeOpen include_open,
                             MOJOSHADER_includeClose include_close,
                             MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    ErrorList *errors = NULL;
    int error_count = 0;

    if (m == NULL) m = MOJOSHADER_internal_malloc;
    if (f == NULL) f = MOJOSHADER_internal_free;

include_open = (MOJOSHADER_includeOpen) 0x1;
include_close = (MOJOSHADER_includeClose) 0x1;

    const char *fname = "*";  // !!! FIXME
    Preprocessor *pp = preprocessor_start(fname, source, sourcelen,
                                          include_open, include_close,
                                          defines, define_count, m, f, d);

    if (pp == NULL)
        return &out_of_mem_data_preprocessor;

    Token token = TOKEN_UNKNOWN;
    const char *tokstr = NULL;
    const char *err = NULL;

    Buffer buffer;
    buffer_init(&buffer);

    int nl = 1;
    int indent = 0;
    unsigned int len = 0;
    while ((tokstr = preprocessor_nexttoken(pp, &len, &token)) != NULL)
    {
        #ifdef _WINDOWS
        static const char endline[] = { '\r', '\n' };
        #else
        static const char endline[] = { '\n' };
        #endif

        const int isnewline = (token == ((Token) '\n'));
        if (isnewline)
        {
            tokstr = endline;  // convert to platform-specific.
            len = sizeof (endline);
        } // if

        if ((token == ((Token) '}')) && (indent > 0))
            indent--;

        int out_of_memory = preprocessor_outofmemory(pp);

        if ((!out_of_memory) && (!isnewline))
            out_of_memory = !indent_buffer(&buffer, indent, nl, m, d);

        if (!out_of_memory)
            out_of_memory = !add_to_buffer(&buffer, tokstr, len, m, d);

        if (token == ((Token) '{'))
            indent++;

        nl = isnewline;

        if ((!out_of_memory) && ((err = preprocessor_error(pp)) != NULL))
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
            char *errstr = (char *) m(strlen(err) + 1, d);
            if (errstr != NULL)
                strcpy(errstr, err);

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

            continue;
        } // if

        if (out_of_memory)
        {
            preprocessor_end(pp);
            free_buffer(&buffer, f, d);
            free_error_list(errors, f, d);
            return &out_of_mem_data_preprocessor;
        } // if
    } // while
    
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

