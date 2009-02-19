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

#if DEBUG_LEXER
static Token debug_preprocessor_lexer(IncludeState *s)
{
    const Token retval = preprocessor_lexer(s);
    MOJOSHADER_print_debug_token("LEXER", s->token, s->tokenlen, retval);
    return retval;
} // debug_preprocessor_lexer
#define preprocessor_lexer(s) debug_preprocessor_lexer(s)
#endif

typedef struct DefineHash
{
    const char *identifier;
    const char *definition;
    struct DefineHash *next;
} DefineHash;


// Simple linked list to cache source filenames, so we don't have to copy
//  the same string over and over for each opcode.
typedef struct FilenameCache
{
    char *filename;
    struct FilenameCache *next;
} FilenameCache;

typedef struct Context
{
    int isfail;
    int out_of_memory;
    char failstr[256];
    Conditional *conditional_pool;
    IncludeState *include_stack;
    IncludeState *include_pool;
    DefineHash *define_hashtable[256];
    DefineHash *define_pool;
    FilenameCache *filename_cache;
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
        else if (token[i] == '\\')
            printf("\\\\");
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

        case ((Token) '\\'):
            printf("'\\\\'");
            break;

        default:
            assert(((int)tokenval) < 256);
            printf("'%c'", (char) tokenval);
            break;
    } // switch
    printf(")\n");
} // MOJOSHADER_print_debug_token
#endif



#if !MOJOSHADER_FORCE_INCLUDE_CALLBACKS

// !!! FIXME: most of these _MSC_VER should probably be _WINDOWS?
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>  // GL headers need this for WINGDIAPI definition.
#else
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#endif

int MOJOSHADER_internal_include_open(MOJOSHADER_includeType inctype,
                                     const char *fname, const char *parent,
                                     const char **outdata,
                                     unsigned int *outbytes,
                                     MOJOSHADER_malloc m, MOJOSHADER_free f,
                                     void *d)
{
#ifdef _MSC_VER
#error Write me.
#else
    struct stat statbuf;
    if (stat(fname, &statbuf) == -1)
        return 0;
    char *data = (char *) m(statbuf.st_size, d);
    if (data == NULL)
        return 0;
    const int fd = open(fname, O_RDONLY);
    if (fd == -1)
    {
        f(data, d);
        return 0;
    } // if
    if (read(fd, data, statbuf.st_size) != statbuf.st_size)
    {
        f(data, d);
        close(fd);
        return 0;
    } // if
    close(fd);
    *outdata = data;
    *outbytes = (unsigned int) statbuf.st_size;
    return 1;
#endif
} // MOJOSHADER_internal_include_open


void MOJOSHADER_internal_include_close(const char *data, MOJOSHADER_malloc m,
                                       MOJOSHADER_free f, void *d)
{
    f((void *) data, d);
} // MOJOSHADER_internal_include_close
#endif  // !MOJOSHADER_FORCE_INCLUDE_CALLBACKS


// data buffer stuff...

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

static void init_buffer(Buffer *buffer)
{
    buffer->total_bytes = 0;
    buffer->head.bytes = 0;
    buffer->head.next = NULL;
    buffer->tail = &buffer->head;
} // init_buffer


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
    init_buffer(buffer);
} // free_buffer


// !!! FIXME: maybe use these pool magic elsewhere?

// Pool stuff...
// ugh, I hate this macro salsa.
#define FREE_POOL(type, poolname) \
    static void free_##poolname##_pool(Context *ctx) { \
        type *item = ctx->poolname##_pool; \
        while (item != NULL) { \
            type *next = item->next; \
            Free(ctx, item); \
            item = next; \
        } \
    }

#define GET_POOL(type, poolname) \
    static type *get_##poolname(Context *ctx) { \
        type *retval = ctx->poolname##_pool; \
        if (retval != NULL) \
            ctx->poolname##_pool = retval->next; \
        else \
            retval = (type *) Malloc(ctx, sizeof (type)); \
        if (retval != NULL) \
            memset(retval, '\0', sizeof (type)); \
        return retval; \
    }

#define PUT_POOL(type, poolname) \
    static void put_##poolname(Context *ctx, type *item) { \
        item->next = ctx->poolname##_pool; \
    }

#define IMPLEMENT_POOL(type, poolname) \
    FREE_POOL(type, poolname) \
    GET_POOL(type, poolname) \
    PUT_POOL(type, poolname)

IMPLEMENT_POOL(Conditional, conditional)
IMPLEMENT_POOL(IncludeState, include)
IMPLEMENT_POOL(DefineHash, define)


// Preprocessor define hashtable stuff...

static unsigned char hash_define(const char *sym)
{
    unsigned char retval = 0;
    while (*sym)
        retval += *(sym++);
    return retval;
} // hash_define


static int add_define(Context *ctx, const char *sym, const char *val, int copy)
{
    char *identifier = NULL;
    char *definition = NULL;
    const unsigned char hash = hash_define(sym);
    DefineHash *bucket = ctx->define_hashtable[hash];
    while (bucket)
    {
        if (strcmp(bucket->identifier, sym) == 0)
        {
            failf(ctx, "'%s' already defined", sym);
            return 0;
        } // if
        bucket = bucket->next;
    } // while

    bucket = get_define(ctx);
    if (bucket == NULL)
        return 0;

    identifier = StrDup(ctx, sym);
    if (identifier == NULL)
    {
        put_define(ctx, bucket);
        return 0;
    } // if

    if (!copy)
        bucket->definition = val;
    else
    {
        definition = StrDup(ctx, val);
        if (definition == NULL)
        {
            Free(ctx, identifier);
            put_define(ctx, bucket);
            return 0;
        } // if
        bucket->definition = definition;
    } // if

    bucket->identifier = identifier;
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
        if (strcmp(bucket->identifier, sym) == 0)
        {
            if (prev == NULL)
                ctx->define_hashtable[hash] = bucket->next;
            else
                prev->next = bucket->next;
            Free(ctx, (void *) bucket->identifier);
            Free(ctx, (void *) bucket->definition);
            put_define(ctx, bucket);
            return 1;
        } // if
        prev = bucket;
        bucket = bucket->next;
    } // while

    return 0;
} // remove_define


static const char *find_define(Context *ctx, const char *sym)
{
    const unsigned char hash = hash_define(sym);
    DefineHash *bucket = ctx->define_hashtable[hash];
    while (bucket)
    {
        if (strcmp(bucket->identifier, sym) == 0)
            return bucket->definition;
        bucket = bucket->next;
    } // while
    return NULL;
} // find_define


static void put_all_defines(Context *ctx)
{
    int i;
    for (i = 0; i < STATICARRAYLEN(ctx->define_hashtable); i++)
    {
        DefineHash *bucket = ctx->define_hashtable[i];
        ctx->define_hashtable[i] = NULL;
        while (bucket)
        {
            DefineHash *next = bucket->next;
            Free(ctx, (void *) bucket->identifier);
            Free(ctx, (void *) bucket->definition);
            put_define(ctx, bucket);
            bucket = next;
        } // while
    } // for
} // find_define


// filename cache stuff...

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

    item->filename = StrDup(ctx, fname);
    if (item->filename == NULL)
    {
        Free(ctx, item);
        return NULL;
    } // if

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


static int push_source(Context *ctx, const char *fname, const char *source,
                       unsigned int srclen, unsigned int linenum,
                       MOJOSHADER_includeClose close_callback)
{
    IncludeState *state = get_include(ctx);
    if (state == NULL)
        return 0;

    if (fname != NULL)
    {
        state->filename = cache_filename(ctx, fname);
        if (state->filename == NULL)
        {
            put_include(ctx, state);
            return 0;
        } // if
    } // if

    state->close_callback = close_callback;
    state->source_base = source;
    state->source = source;
    state->token = source;
    state->orig_length = srclen;
    state->bytes_left = srclen;
    state->line = linenum;
    state->next = ctx->include_stack;

    ctx->include_stack = state;

    return 1;
} // push_source


static void pop_source(Context *ctx)
{
    IncludeState *state = ctx->include_stack;
    if (state == NULL)
        return;

    if (state->close_callback)
    {
        state->close_callback(state->source_base, ctx->malloc,
                              ctx->free, ctx->malloc_data);
    } // if

    // state->filename is a pointer to the filename cache; don't free it here!

    while (state->conditional_stack)
        put_conditional(ctx, state->conditional_stack);

    ctx->include_stack = state->next;
    put_include(ctx, state);
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
        if (!add_define(ctx, defines[i]->identifier, defines[i]->definition, 1))
        {
            okay = 0;
            break;
        } // if
    } // for

    if ((okay) && (!push_source(ctx, fname, source, sourcelen, 1, NULL)))
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

    put_all_defines(ctx);

    free_filename_cache(ctx);
    free_define_pool(ctx);
    free_conditional_pool(ctx);
    free_include_pool(ctx);

    Free(ctx, ctx);
} // preprocessor_end


int preprocessor_outofmemory(Preprocessor *_ctx)
{
    Context *ctx = (Context *) _ctx;
    return ctx->out_of_memory;
} // preprocessor_outofmemory


static inline void pushback(IncludeState *state)
{
    #if DEBUG_PREPROCESSOR
    printf("PREPROCESSOR PUSHBACK\n");
    #endif
    assert(!state->pushedback);
    state->pushedback = 1;
} // pushback


static Token lexer(IncludeState *state)
{
    if (!state->pushedback)
        return preprocessor_lexer(state);
    state->pushedback = 0;
    return state->tokenval;
} // lexer


// !!! FIXME: parsing fails on preprocessor directives should skip rest of line.
static int require_newline(IncludeState *state)
{
    const Token token = lexer(state);
    pushback(state);  // rewind no matter what.
    return ( (token == TOKEN_INCOMPLETE_COMMENT) || // call it an eol.
             (token == ((Token) '\n')) || (token == TOKEN_EOI) );
} // require_newline


static void handle_pp_include(Context *ctx)
{
    IncludeState *state = ctx->include_stack;
    Token token = lexer(state);
    MOJOSHADER_includeType incltype;
    char *filename = NULL;
    int bogus = 0;

    if (token == TOKEN_STRING_LITERAL)
        incltype = MOJOSHADER_INCLUDETYPE_LOCAL;
    else if (token == ((Token) '<'))
    {
        incltype = MOJOSHADER_INCLUDETYPE_SYSTEM;
        // can't use lexer, since every byte between the < > pair is
        //  considered part of the filename.  :/
        while (!bogus)
        {
            if ( !(bogus = (state->bytes_left == 0)) )
            {
                const char ch = *state->source;
                if ( !(bogus = ((ch == '\r') || (ch == '\n'))) )
                {
                    state->source++;
                    state->bytes_left--;

                    if (ch == '>')
                        break;
                } // if
            } // if
        } // while
    } // else if
    else
    {
        bogus = 1;
    } // else

    if (!bogus)
    {
        state->token++;  // skip '<' or '\"'...
        const unsigned int len = ((unsigned int) (state->source-state->token));
        filename = (char *) alloca(len);
        memcpy(filename, state->token, len-1);
        filename[len-1] = '\0';
        bogus = !require_newline(state);
    } // if

    if (bogus)
    {
        fail(ctx, "Invalid #include directive");
        return;
    } // else

    const char *newdata = NULL;
    unsigned int newbytes = 0;
    if (!ctx->open_callback(incltype, filename, state->source_base,
                            &newdata, &newbytes, ctx->malloc,
                            ctx->free, ctx->malloc_data))
    {
        fail(ctx, "Include callback failed");  // !!! FIXME: better error
        return;
    } // if

    if (!push_source(ctx, filename, newdata, newbytes, 1, ctx->close_callback))
    {
        assert(ctx->out_of_memory);
        ctx->close_callback(newdata, ctx->malloc, ctx->free, ctx->malloc_data);
    } // if
} // handle_pp_include


static void handle_pp_line(Context *ctx)
{
    IncludeState *state = ctx->include_stack;
    char *filename = NULL;
    int linenum = 0;
    int bogus = 0;

    if (lexer(state) != TOKEN_INT_LITERAL)
        bogus = 1;
    else
    {
        char *buf = (char *) alloca(state->tokenlen+1);
        memcpy(buf, state->token, state->tokenlen);
        buf[state->tokenlen] = '\0';
        linenum = atoi(buf);
    } // else

    if (!bogus)
        bogus = (lexer(state) != TOKEN_STRING_LITERAL);

    if (!bogus)
    {
        state->token++;  // skip '\"'...
        filename = (char *) alloca(state->tokenlen);
        memcpy(filename, state->token, state->tokenlen-1);
        filename[state->tokenlen-1] = '\0';
        bogus = !require_newline(state);
    } // if

    if (bogus)
    {
        fail(ctx, "Invalid #line directive");
        return;
    } // if

    const char *cached = cache_filename(ctx, filename);
    assert((cached != NULL) || (ctx->out_of_memory));
    state->filename = cached;
    state->line = linenum;
} // handle_pp_line


static void handle_pp_error(Context *ctx)
{
    IncludeState *state = ctx->include_stack;
    char *ptr = ctx->failstr;
    int avail = sizeof (ctx->failstr) - 1;
    int cpy = 0;
    int done = 0;

    const char *prefix = "#error";
    const size_t prefixlen = strlen(prefix);
    strcpy(ctx->failstr, prefix);
    avail -= prefixlen;
    ptr += prefixlen;

    state->report_whitespace = 1;
    while (!done)
    {
        const Token token = lexer(state);
        switch (token)
        {
            case ((Token) '\n'):
                state->line--;  // make sure error is on the right line.
                // fall through!
            case TOKEN_INCOMPLETE_COMMENT:
            case TOKEN_EOI:
                pushback(state);  // move back so we catch this later.
                done = 1;
                break;

            case ' ':
                if (!avail)
                    break;
                *(ptr++) = ' ';
                avail--;
                break;

            default:
                cpy = Min(avail, (int) state->tokenlen);
                if (cpy)
                    memcpy(ptr, state->token, cpy);
                ptr += cpy;
                avail -= cpy;
                break;
        } // switch
    } // while

    *ptr = '\0';
    state->report_whitespace = 0;
    ctx->isfail = 1;
} // handle_pp_error


static void handle_pp_define(Context *ctx)
{
    IncludeState *state = ctx->include_stack;

    if (lexer(state) != TOKEN_IDENTIFIER)
    {
        fail(ctx, "Macro names must be indentifiers");
        return;
    } // if

    MOJOSHADER_malloc m = ctx->malloc;
    void *d = ctx->malloc_data;
    const char space = ' ';
    char *sym = (char *) alloca(state->tokenlen+1);
    memcpy(sym, state->token, state->tokenlen);
    sym[state->tokenlen] = '\0';

    Buffer buffer;
    init_buffer(&buffer);

    int done = 0;
    state->report_whitespace = 1;
    while ((!done) && (!ctx->out_of_memory))
    {
        const Token token = lexer(state);
        switch (token)
        {
            case TOKEN_INCOMPLETE_COMMENT:
            case TOKEN_EOI:
                pushback(state);  // move back so we catch this later.
                // fall through!
            case ((Token) '\n'):
                done = 1;
                break;

            case ((Token) ' '):  // may not actually point to ' '.
                if (buffer.total_bytes > 0)
                {
                    if (!add_to_buffer(&buffer, &space, 1, m, d))
                        ctx->out_of_memory = 1;
                } // if
                break;

            default:
                if (!add_to_buffer(&buffer,state->token,state->tokenlen,m,d))
                    ctx->out_of_memory = 1;
                break;
        } // switch
    } // while
    state->report_whitespace = 0;

    char *definition = NULL;
    if (!ctx->out_of_memory)
    {
        definition = flatten_buffer(&buffer, m, d);
        ctx->out_of_memory = (definition == NULL);
    } // if

    free_buffer(&buffer, ctx->free, d);

    if (ctx->out_of_memory)
        return;

    assert(done);
    add_define(ctx, sym, definition, 0);
} // handle_pp_define


static void handle_pp_undef(Context *ctx)
{
    IncludeState *state = ctx->include_stack;

    if (lexer(state) != TOKEN_IDENTIFIER)
    {
        fail(ctx, "Macro names must be indentifiers");
        return;
    } // if

    char *sym = (char *) alloca(state->tokenlen+1);
    memcpy(sym, state->token, state->tokenlen);
    sym[state->tokenlen] = '\0';

    if (!require_newline(state))
    {
        fail(ctx, "Invalid #undef directive");
        return;
    } // if

    remove_define(ctx, sym);
} // handle_pp_undef


static Conditional *_handle_pp_ifdef(Context *ctx, const Token type)
{
    IncludeState *state = ctx->include_stack;

    assert((type == TOKEN_PP_IFDEF) || (type == TOKEN_PP_IFNDEF));

    if (lexer(state) != TOKEN_IDENTIFIER)
    {
        fail(ctx, "Macro names must be indentifiers");
        return NULL;
    } // if

    char *sym = (char *) alloca(state->tokenlen+1);
    memcpy(sym, state->token, state->tokenlen);
    sym[state->tokenlen] = '\0';

    if (!require_newline(state))
    {
        if (type == TOKEN_PP_IFDEF)
            fail(ctx, "Invalid #ifdef directive");
        else
            fail(ctx, "Invalid #ifndef directive");
        return NULL;
    } // if

    Conditional *conditional = get_conditional(ctx);
    assert((conditional != NULL) || (ctx->out_of_memory));
    if (conditional == NULL)
        return NULL;

    Conditional *prev = state->conditional_stack;
    int skipping = ((prev != NULL) && (prev->skipping));
    if (!skipping)
    {
        const int found = (find_define(ctx, sym) != NULL);
        if (type == TOKEN_PP_IFDEF)
            skipping = !found;
        else
            skipping = found;
    } // if

    conditional->type = type;
    conditional->linenum = state->line - 1;
    conditional->skipping = skipping;
    conditional->chosen = !skipping;
    conditional->next = prev;
    state->conditional_stack = conditional;
    return conditional;
} // _handle_pp_ifdef


static inline void handle_pp_ifdef(Context *ctx)
{
    _handle_pp_ifdef(ctx, TOKEN_PP_IFDEF);
} // handle_pp_ifdef


static inline void handle_pp_ifndef(Context *ctx)
{
    _handle_pp_ifdef(ctx, TOKEN_PP_IFNDEF);
} // handle_pp_ifndef


static inline void handle_pp_else(Context *ctx)
{
    IncludeState *state = ctx->include_stack;
    Conditional *cond = state->conditional_stack;

    if (!require_newline(state))
        fail(ctx, "Invalid #else directive");
    else if (cond == NULL)
        fail(ctx, "#else without #if");
    else if (cond->type == TOKEN_PP_ELSE)
        fail(ctx, "#else after #else");
    else
    {
        cond->type = TOKEN_PP_ELSE;
        cond->skipping = cond->chosen;
        if (!cond->chosen)
            cond->chosen = 1;
    } // else
} // handle_pp_else


static void handle_pp_endif(Context *ctx)
{
    IncludeState *state = ctx->include_stack;
    Conditional *cond = state->conditional_stack;

    if (!require_newline(state))
        fail(ctx, "Invalid #endif directive");
    else if (cond == NULL)
        fail(ctx, "Unmatched #endif");
    else
    {
        state->conditional_stack = cond->next;  // pop it.
        put_conditional(ctx, cond);
    } // else
} // handle_pp_endif


static int handle_pp_identifier(Context *ctx)
{
    IncludeState *state = ctx->include_stack;
    char *sym = (char *) alloca(state->tokenlen+1);
    memcpy(sym, state->token, state->tokenlen);
    sym[state->tokenlen] = '\0';

    const char *def = find_define(ctx, sym);
    if (def == NULL)
        return 0;   // just send the token through unchanged.

    if (!push_source(ctx, state->filename, def, strlen(def), state->line, 0))
    {
        assert(ctx->out_of_memory);
        return 0;
    } // if

    return 1;
} // handle_pp_identifier


static void unterminated_pp_condition(Context *ctx)
{
    IncludeState *state = ctx->include_stack;
    Conditional *cond = state->conditional_stack;

    // !!! FIXME: report the line number where the #if is, not the EOI.
    switch (cond->type)
    {
        case TOKEN_PP_IF: fail(ctx, "Unterminated #if"); break;
        case TOKEN_PP_IFDEF: fail(ctx, "Unterminated #ifdef"); break;
        case TOKEN_PP_IFNDEF: fail(ctx, "Unterminated #ifndef"); break;
        case TOKEN_PP_ELSE: fail(ctx, "Unterminated #else"); break;
        case TOKEN_PP_ELIF: fail(ctx, "Unterminated #elif"); break;
        default: assert(0 && "Shouldn't hit this case"); break;
    } // switch

    // pop this conditional, we'll report the next error next time...

    state->conditional_stack = cond->next;  // pop it.
    put_conditional(ctx, cond);
} // unterminated_pp_condition


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

        // !!! FIXME: todo.
        // TOKEN_PP_IF,
        // TOKEN_PP_ELIF,

        const Conditional *cond = state->conditional_stack;
        const int skipping = ((cond != NULL) && (cond->skipping));

        Token token = lexer(state);
        if (token == TOKEN_EOI)
        {
            assert(state->bytes_left == 0);
            if (state->conditional_stack != NULL)
            {
                unterminated_pp_condition(ctx);
                continue;  // returns an error.
            } // if

            pop_source(ctx);
            continue;  // pick up again after parent's #include line.
        } // if

        else if (token == TOKEN_INCOMPLETE_COMMENT)
        {
            fail(ctx, "Incomplete multiline comment");
            continue;  // will return at top of loop.
        } // else if

        else if (token == TOKEN_PP_IFDEF)
        {
            handle_pp_ifdef(ctx);
            continue;  // get the next thing.
        } // else if

        else if (token == TOKEN_PP_IFNDEF)
        {
            handle_pp_ifndef(ctx);
            continue;  // get the next thing.
        } // else if

        else if (token == TOKEN_PP_ENDIF)
        {
            handle_pp_endif(ctx);
            continue;  // get the next thing.
        } // else if

        else if (token == TOKEN_PP_ELSE)
        {
            handle_pp_else(ctx);
            continue;  // get the next thing.
        } // else if

        // NOTE: Conditionals must be above (skipping) test.
        else if (skipping)
            continue;  // just keep dumping tokens until we get end of block.

        else if (token == TOKEN_PP_INCLUDE)
        {
            handle_pp_include(ctx);
            continue;  // will return error or use new top of include_stack.
        } // else if

        else if (token == TOKEN_PP_LINE)
        {
            handle_pp_line(ctx);
            continue;  // get the next thing.
        } // else if

        else if (token == TOKEN_PP_ERROR)
        {
            handle_pp_error(ctx);
            continue;  // will return at top of loop.
        } // else if

        else if (token == TOKEN_PP_DEFINE)
        {
            handle_pp_define(ctx);
            continue;  // will return at top of loop.
        } // else if

        else if (token == TOKEN_PP_UNDEF)
        {
            handle_pp_undef(ctx);
            continue;  // will return at top of loop.
        } // else if

        else if (token == TOKEN_IDENTIFIER)
        {
            if (handle_pp_identifier(ctx))
                continue;  // pushed the include_stack.
        } // else if

        assert(!skipping);
        *_token = token;
        *_len = state->tokenlen;
        return state->token;

        // !!! FIXME: check for ((Token) '\n'), so we know if a preprocessor
        // !!! FIXME:  directive started a line.
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


static const MOJOSHADER_preprocessData out_of_mem_data_preprocessor = {
    1, &MOJOSHADER_out_of_mem_error, 0, 0, 0, 0, 0
};


// public API...

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

    if (!m) m = MOJOSHADER_internal_malloc;
    if (!f) f = MOJOSHADER_internal_free;
    if (!include_open) include_open = MOJOSHADER_internal_include_open;
    if (!include_close) include_close = MOJOSHADER_internal_include_close;

    Preprocessor *pp = preprocessor_start(filename, source, sourcelen,
                                          include_open, include_close,
                                          defines, define_count, m, f, d);

    if (pp == NULL)
        return &out_of_mem_data_preprocessor;

    Token token = TOKEN_UNKNOWN;
    const char *tokstr = NULL;

    Buffer buffer;
    init_buffer(&buffer);

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

