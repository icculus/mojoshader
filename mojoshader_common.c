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
    HashTable_HashFn hash;
    HashTable_KeyMatchFn keymatch;
    HashTable_NukeFn nuke;
    MOJOSHADER_malloc malloc;
    MOJOSHADER_free free;
    void *malloc_data;
};

int hash_find(const HashTable *table, const void *key, const void **_value)
{
    HashItem *i;
    const uint32 hash = table->hash(key) & table->table_len;
    for (i = table->table[hash]; i != NULL; i = i->next)
    {
        if (table->keymatch(key, i->key))
        {
            if (_value != NULL)
                *_value = i->value;
            return 1;
        } // if
    } // for

    return 0;
} // hash_find

int hash_insert(HashTable *table, const void *key, const void *value)
{
    HashItem *item = NULL;
    const uint32 hash = table->hash(key) & table->table_len;
    if ( (!table->stackable) && (hash_find(table, key, NULL)) )
        return 0;

    // !!! FIXME: grow and rehash table if it gets too saturated.
    item = (HashItem *) table->malloc(sizeof (HashItem), table->malloc_data);
    if (item == NULL)
        return -1;

    item->key = key;
    item->value = value;
    item->next = table->table[hash];
    table->table[hash] = item;

    return 1;
} // hash_insert

HashTable *hash_create(const uint32 initial_table_size,
              const HashTable_HashFn hashfn,
              const HashTable_KeyMatchFn keymatchfn,
              const HashTable_NukeFn nukefn,
              const int stackable,
              MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
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
    table->hash = hashfn;
    table->keymatch = keymatchfn;
    table->nuke = nukefn;
    table->malloc = m;
    table->free = f;
    table->malloc_data = d;
    return table;
} // hash_create

void hash_destroy(HashTable *table)
{
    uint32 i;
    for (i = 0; i < table->table_len; i++)
    {
        HashItem *item = table->table[i];
        while (item != NULL)
        {
            HashItem *next = item->next;
            table->nuke(item->key, item->value);
            table->free(item, table->malloc_data);
            item = next;
        } // while
    } // for

    table->free(table->table, table->malloc_data);
    table->free(table, table->malloc_data);
} // hash_destroy

int hash_remove(HashTable *table, const void *key)
{
    HashItem *item = NULL;
    HashItem *prev = NULL;
    const uint32 hash = table->hash(key) & table->table_len;
    for (item = table->table[hash]; item != NULL; item = item->next)
    {
        if (table->keymatch(key, item->key))
        {
            if (prev != NULL)
                prev->next = item->next;
            else
                table->table[hash] = item->next;

            table->nuke(item->key, item->value);
            table->free(item, table->malloc_data);
            return 1;
        } // if

        prev = item;
    } // for

    return 0;
} // hash_remove


// this is djb's xor hashing function.
uint32 hash_hash_string(const void *_sym)
{
    register const char *sym = (const char *) _sym;
    register uint32 hash = 5381;
    while (*sym)
        hash = ((hash << 5) + hash) ^ *(sym++);
    return hash;
} // hash_hash_string

int hash_keymatch_string(const void *a, const void *b)
{
    return (strcmp((const char *) a, (const char *) b) == 0);
} // hash_keymatch_string

// end of mojoshader_common.c ...

