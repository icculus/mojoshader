#define __MOJOSHADER_INTERNAL__ 1
#include "mojoshader_internal.h"

typedef struct HashItem
{
    const void *key;
    const void *value;
    struct HashItem *next;
} HashItem;

struct HashTable
{
    HashItem **table;
    uint32 table_len;
    int stackable;
    void *data;
    HashTable_HashFn hash;
    HashTable_KeyMatchFn keymatch;
    HashTable_NukeFn nuke;
    MOJOSHADER_malloc m;
    MOJOSHADER_free f;
    void *d;
};

static inline uint32 calc_hash(const HashTable *table, const void *key)
{
    return table->hash(key, table->data) & (table->table_len-1);
} // calc_hash

int hash_find(const HashTable *table, const void *key, const void **_value)
{
    HashItem *i;
    void *data = table->data;
    const uint32 hash = calc_hash(table, key);
    HashItem *prev = NULL;
    for (i = table->table[hash]; i != NULL; i = i->next)
    {
        if (table->keymatch(key, i->key, data))
        {
            if (_value != NULL)
                *_value = i->value;

            // Matched! Move to the front of list for faster lookup next time.
            //  (stackable tables have to remain in the same order, though!)
            if ((!table->stackable) && (prev != NULL))
            {
                assert(prev->next == i);
                prev->next = i->next;
                i->next = table->table[hash];
                table->table[hash] = i;
            } // if

            return 1;
        } // if

        prev = i;
    } // for

    return 0;
} // hash_find

int hash_insert(HashTable *table, const void *key, const void *value)
{
    HashItem *item = NULL;
    const uint32 hash = calc_hash(table, key);
    if ( (!table->stackable) && (hash_find(table, key, NULL)) )
        return 0;

    // !!! FIXME: grow and rehash table if it gets too saturated.
    item = (HashItem *) table->m(sizeof (HashItem), table->d);
    if (item == NULL)
        return -1;

    item->key = key;
    item->value = value;
    item->next = table->table[hash];
    table->table[hash] = item;

    return 1;
} // hash_insert

HashTable *hash_create(void *data, const HashTable_HashFn hashfn,
              const HashTable_KeyMatchFn keymatchfn,
              const HashTable_NukeFn nukefn,
              const int stackable,
              MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    const uint32 initial_table_size = 256;
    const uint32 alloc_len = sizeof (HashItem *) * initial_table_size;
    HashTable *table = (HashTable *) m(sizeof (HashTable), d);
    if (table == NULL)
        return NULL;
    memset(table, '\0', sizeof (HashTable));

    table->table = (HashItem **) m(alloc_len, d);
    if (table->table == NULL)
    {
        f(table, d);
        return NULL;
    } // if

    memset(table->table, '\0', alloc_len);
    table->table_len = initial_table_size;
    table->stackable = stackable;
    table->data = data;
    table->hash = hashfn;
    table->keymatch = keymatchfn;
    table->nuke = nukefn;
    table->m = m;
    table->f = f;
    table->d = d;
    return table;
} // hash_create

void hash_destroy(HashTable *table)
{
    uint32 i;
    void *data = table->data;
    MOJOSHADER_free f = table->f;
    void *d = table->d;
    for (i = 0; i < table->table_len; i++)
    {
        HashItem *item = table->table[i];
        while (item != NULL)
        {
            HashItem *next = item->next;
            table->nuke(item->key, item->value, data);
            f(item, d);
            item = next;
        } // while
    } // for

    f(table->table, d);
    f(table, d);
} // hash_destroy

int hash_remove(HashTable *table, const void *key)
{
    HashItem *item = NULL;
    HashItem *prev = NULL;
    void *data = table->data;
    const uint32 hash = calc_hash(table, key);
    for (item = table->table[hash]; item != NULL; item = item->next)
    {
        if (table->keymatch(key, item->key, data))
        {
            if (prev != NULL)
                prev->next = item->next;
            else
                table->table[hash] = item->next;

            table->nuke(item->key, item->value, data);
            table->f(item, table->d);
            return 1;
        } // if

        prev = item;
    } // for

    return 0;
} // hash_remove


// this is djb's xor hashing function.
static inline uint32 hash_string_djbxor(const char *str, size_t len)
{
    register uint32 hash = 5381;
    while (len--)
        hash = ((hash << 5) + hash) ^ *(str++);
    return hash;
} // hash_string_djbxor

static inline uint32 hash_string(const char *str, size_t len)
{
    return hash_string_djbxor(str, len);
} // hash_string

uint32 hash_hash_string(const void *sym, void *data)
{
    (void) data;
    return hash_string((const char*) sym, strlen((const char *) sym));
} // hash_hash_string

int hash_keymatch_string(const void *a, const void *b, void *data)
{
    (void) data;
    return (strcmp((const char *) a, (const char *) b) == 0);
} // hash_keymatch_string


// string -> string map...

static void stringmap_nuke_noop(const void *key, const void *val, void *d) {}

static void stringmap_nuke(const void *key, const void *val, void *d)
{
    StringMap *smap = (StringMap *) d;
    smap->f((void *) key, smap->d);
    smap->f((void *) val, smap->d);
} // stringmap_nuke

StringMap *stringmap_create(const int copy, MOJOSHADER_malloc m,
                            MOJOSHADER_free f, void *d)
{
    HashTable_NukeFn nuke = copy ? stringmap_nuke : stringmap_nuke_noop;
    StringMap *smap;
    smap = hash_create(0,hash_hash_string,hash_keymatch_string,nuke,0,m,f,d);
    smap->data = smap;
    return smap;
} // stringmap_create

void stringmap_destroy(StringMap *smap)
{
    return hash_destroy(smap);
} // stringmap_destroy

int stringmap_insert(StringMap *smap, const char *key, const char *value)
{
    assert(key != NULL);
    if (smap->nuke == stringmap_nuke_noop)  // no copy?
        return hash_insert(smap, key, value);

    int rc = -1;
    char *k = (char *) smap->m(strlen(key) + 1, smap->d);
    char *v = (char *) (value ? smap->m(strlen(value) + 1, smap->d) : NULL);
    if ( (!k) || ((!v) && (value)) || ((rc = hash_insert(smap, k, v)) <= 0) )
    {
        smap->f(k, smap->d);
        smap->f(v, smap->d);
    } // if

    return rc;
} // stringmap_insert

int stringmap_remove(StringMap *smap, const char *key)
{
    return hash_remove(smap, key);
} // stringmap_remove

int stringmap_find(const StringMap *smap, const char *key, const char **_value)
{
    const void *value = NULL;
    const int retval = hash_find(smap, key, &value);
    *_value = (const char *) value;
    return retval;
} // stringmap_find


// The string cache...   !!! FIXME: use StringMap internally for this.

typedef struct StringBucket
{
    char *string;
    struct StringBucket *next;
} StringBucket;

struct StringCache
{
    StringBucket **hashtable;
    uint32 table_size;
    MOJOSHADER_malloc m;
    MOJOSHADER_free f;
    void *d;
};

const char *stringcache(StringCache *cache, const char *str)
{
    return stringcache_len(cache, str, strlen(str));
} // stringcache

const char *stringcache_len(StringCache *cache, const char *str,
                             const unsigned int len)
{
    const uint8 hash = hash_string(str, len) & (cache->table_size-1);
    StringBucket *bucket = cache->hashtable[hash];
    StringBucket *prev = NULL;
    while (bucket)
    {
        const char *bstr = bucket->string;
        if ((strncmp(bstr, str, len) == 0) && (bstr[len] == 0))
        {
            // Matched! Move this to the front of the list.
            if (prev != NULL)
            {
                assert(prev->next == bucket);
                prev->next = bucket->next;
                bucket->next = cache->hashtable[hash];
                cache->hashtable[hash] = bucket;
            } // if
            return bstr; // already cached
        } // if
        prev = bucket;
        bucket = bucket->next;
    } // while

    // no match, add to the table.
    bucket = (StringBucket *) cache->m(sizeof (StringBucket), cache->d);
    if (bucket == NULL)
        return NULL;
    bucket->string = (char *) cache->m(len + 1, cache->d);
    if (bucket->string == NULL)
    {
        cache->f(bucket, cache->d);
        return NULL;
    } // if
    memcpy(bucket->string, str, len);
    bucket->string[len] = '\0';
    bucket->next = cache->hashtable[hash];
    cache->hashtable[hash] = bucket;
    return bucket->string;
} // stringcache_len

const char *stringcache_fmt(StringCache *cache, const char *fmt, ...)
{
    char buf[128];  // use the stack if reasonable.
    char *ptr = NULL;
    int len = 0;  // number of chars, NOT counting null-terminator!
    va_list ap;

    va_start(ap, fmt);
    len = vsnprintf(buf, sizeof (buf), fmt, ap);
    va_end(ap);

    if (len > sizeof (buf))
    {
        ptr = (char *) cache->m(len, cache->d);
        if (ptr == NULL)
            return NULL;

        va_start(ap, fmt);
        vsnprintf(ptr, len, fmt, ap);
        va_end(ap);
    } // if

    const char *retval = stringcache_len(cache, ptr ? ptr : buf, len);
    if (ptr != NULL)
        cache->f(ptr, cache->d);

    return retval;
} // stringcache_fmt

StringCache *stringcache_create(MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    const uint32 initial_table_size = 256;
    const size_t tablelen = sizeof (StringBucket *) * initial_table_size;
    StringCache *cache = (StringCache *) m(sizeof (StringCache), d);
    if (!cache)
        return NULL;
    memset(cache, '\0', sizeof (StringCache));

    cache->hashtable = (StringBucket **) m(tablelen, d);
    if (!cache->hashtable)
    {
        f(cache, d);
        return NULL;
    } // if
    memset(cache->hashtable, '\0', tablelen);

    cache->table_size = initial_table_size;
    cache->m = m;
    cache->f = f;
    cache->d = d;
    return cache;
} // stringcache_create

void stringcache_destroy(StringCache *cache)
{
    MOJOSHADER_free f = cache->f;
    void *d = cache->d;
    size_t i;

    for (i = 0; i < cache->table_size; i++)
    {
        StringBucket *bucket = cache->hashtable[i];
        cache->hashtable[i] = NULL;
        while (bucket)
        {
            StringBucket *next = bucket->next;
            f(bucket->string, d);
            f(bucket, d);
            bucket = next;
        } // while
    } // for

    f(cache->hashtable, d);
    f(cache, d);
} // stringcache_destroy

// end of mojoshader_common.c ...

