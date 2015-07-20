/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#ifndef NECRONOMICON_HASH_TABLE_H_INCLUDED
#define NECRONOMICON_HASH_TABLE_H_INCLUDED

#include "../Endian.h"
#include "../Util.h"
#include <stdio.h>

// Fast and generic fixed memory hash table using open addressing with linear probing
// insertion and removal never alter the internal hash table size because the size is fixed.
// This is not thread safe.
// Only works with uint32_t and char* keys
// Open addressing causes a sharp decrease in performance as the load approaches the interal size limit
// use a max size that is reasonably larger than your predicted load requirement
// note: the max size will be increased to the next power of two

typedef union
{
    uint8_t bytes[4];
    uint32_t word;
    float f;
} four_bytes;

typedef union
{
    uint8_t bytes[8];
    double d;
    uint64_t ul;
    struct
    {
#if BYTE_ORDER == BIG_ENDIAN
        uint32_t high, low;
#else
        uint32_t low, high;
#endif
    } l;
} eight_bytes;

// FNV1-a hash function
extern const uint64_t PRIME;
extern const uint64_t SEED;

#define FNV1A(byte, hash) ((byte ^ hash) * PRIME)
#define HASH_KEY_PRIV(key) (FNV1A(key.bytes[3], FNV1A(key.bytes[2], FNV1A(key.bytes[1], FNV1A(key.bytes[0], SEED)))))
#define HASH_KEY(key) ((uint32_t) HASH_KEY_PRIV(((four_bytes) key)))

// expects a null terminated char* string
static inline uint32_t hash_string(const char* string)
{
    uint64_t hash = SEED;
    int32_t i = 0;
    char c = string[i];
    while (c)
    {
        hash = FNV1A(c, hash);
        c = string[++i];
    }

    return hash;
}

typedef enum
{
    string_key_type,
    uint_key_type
} hash_table_key_t;

typedef struct
{
    void* item;
    hash_table_key_t key_type;
    uint32_t uint_key;
    char* string_key;
    uint32_t hash;
} hash_table_node;

typedef struct
{
    hash_table_node** table;
    uint32_t size;
    uint32_t size_mask;
} hash_table;

hash_table hash_table_new(uint32_t max_items); // max_items will be increased up to the next power of two

// frees the internal hash table. If free_items_in_table == true, then each item in the table will also be freed.
void hash_table_free(hash_table htable, bool free_items_in_table);

// Use hash_table_free_with_callback if you have a specific call back you want to use to free each item in order to do more specific cleanup
typedef void (*free_item_callback)(void* item);
void hash_table_free_with_callback(hash_table htable, free_item_callback free_callback);

// attempts to insert item in the hash table, returns true if succesful, or false if not. *does not modify the size of the table*
bool hash_table_insert_uint_key(hash_table htable, void* item, uint32_t key);
bool hash_table_insert_string_key(hash_table htable, void* item, const char* key);

// Will remove the item from the table, but will not free the item itself.
// Use free on the item itself after you have called remove if you want to release their memory.
bool hash_table_remove_uint_key(hash_table htable, void* item, uint32_t key); // returns true if the item was found or false if not found in the table
// This expects a null terminated string. A copy of the key will be made internally and will be managed in the hash table.
bool hash_table_remove_string_key(hash_table htable, void* item, const char* key); // returns true if the item was found or false if not found in the table

void* hash_table_lookup_uint_key(hash_table htable, uint32_t key); // will return null if the item was not found in the table
void* hash_table_lookup_string_key(hash_table htable, const char* key); // will return null if the item was not found in the table

#endif // NECRONOMICON_HASH_TABLE_H_INCLUDED
