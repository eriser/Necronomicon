/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdint.h>

#include "HashTable.h"

const uint64_t PRIME = 0x01000193; // 16777619
const uint64_t SEED = 0x811C9DC5; // 2166136261
const uint32_t HASH_TABLE_NODE_SIZE = sizeof(hash_table_node);

hash_table hash_table_new(uint32_t max_items)
{
    uint32_t size = next_power_of_two(max_items);
    uint64_t size_mask = size - 1;
    hash_table_node** table = (hash_table_node**) calloc(size, sizeof(hash_table_node**));
    assert(table);

    hash_table htable = { table, size, size_mask };
    return htable;
}

void hash_table_free(hash_table htable, bool free_items_in_table)
{
    hash_table_node** table = htable.table;
    uint32_t size = htable.size;
    uint32_t i;

    for (i = 0; i < size; ++i)
    {
        hash_table_node* node = table[i];
        if (node != NULL)
        {
            if (free_items_in_table == true)
                free(node->item);

            free(node);
        }
    }

    free(table);
}

void hash_table_free_with_callback(hash_table htable, free_item_callback free_callback)
{
    hash_table_node** table = htable.table;
    uint32_t size = htable.size;
    uint32_t i;

    for (i = 0; i < size; ++i)
    {
        hash_table_node* node = table[i];
        if (node != NULL)
        {
            free_callback(node->item);
            free(node);
        }
    }

    free(table);
}

bool hash_table_insert_uint_key(hash_table htable, void* item, uint32_t key)
{
    hash_table_node** table = htable.table;
    uint32_t size = htable.size;
    uint32_t size_mask = htable.size_mask;
    uint32_t hash = HASH_KEY(key);
    uint32_t slot = hash & size_mask;
    uint32_t i;
    bool is_insertion_successful = false;

    for (i = 0; i < size; ++i)
    {
        if (table[slot] == NULL)
            break;

        slot = (slot + 1) & size_mask;
    }

    if (i < size)
    {
        hash_table_node* node_ptr = malloc(HASH_TABLE_NODE_SIZE);
        hash_table_node node = { item, uint_key_type, key, 0, hash };
        *node_ptr = node;
        table[slot] = node_ptr;
        is_insertion_successful = true;
    }

    return is_insertion_successful;
}

bool hash_table_insert_string_key(hash_table htable, void* item, const char* key)
{
    hash_table_node** table = htable.table;
    uint32_t size = htable.size;
    uint32_t size_mask = htable.size_mask;
    uint32_t hash = hash_string(key);
    uint32_t slot = hash & size_mask;
    uint32_t i;
    bool is_insertion_successful = false;

    for (i = 0; i < size; ++i)
    {
        if (table[slot] == NULL)
            break;

        slot = (slot + 1) & size_mask;
    }

    if (i < size)
    {
        hash_table_node* node_ptr = malloc(HASH_TABLE_NODE_SIZE);
        hash_table_node node = { item, string_key_type, 0, key, hash };
        *node_ptr = node;
        table[slot] = node_ptr;
        is_insertion_successful = true;
    }

    return is_insertion_successful;
}

bool hash_table_remove_uint_key(hash_table htable, void* item, uint32_t key)
{
    hash_table_node** table = htable.table;
    uint32_t size = htable.size;
    uint32_t size_mask = htable.size_mask;
    uint32_t hash = HASH_KEY(key);
    uint32_t index = hash & size_mask;
    bool is_removal_successful = false;

    hash_table_node* found_node = table[index];
    if (found_node != NULL && found_node->uint_key == key)
    {
        free(found_node);
        table[index] = NULL;
        is_removal_successful = true;
    }

    return is_removal_successful;
}

bool hash_table_remove_string_key(hash_table htable, void* item, const char* key)
{
    hash_table_node** table = htable.table;
    uint32_t size = htable.size;
    uint32_t size_mask = htable.size_mask;
    uint32_t hash = hash_string(key);
    uint32_t index = hash & size_mask;
    bool is_removal_successful = false;

    hash_table_node* found_node = table[index];
    if (found_node != NULL && found_node->string_key == key)
    {
        free(found_node);
        table[index] = NULL;
        is_removal_successful = true;
    }

    return is_removal_successful;
}

void* hash_table_lookup_uint_key(hash_table htable, uint32_t key)
{
    hash_table_node** table = htable.table;
    uint32_t size = htable.size;
    uint32_t size_mask = htable.size_mask;
    uint32_t hash = HASH_KEY(key);
    uint32_t slot = hash & size_mask;
    uint32_t i = 0;
    void* item_ptr = NULL;

    while (i < size)
    {
        hash_table_node* node = table[slot];
        if (node != NULL && node->uint_key == key)
        {
            item_ptr = table[slot];
            break;
        }

        ++i;
        slot = (slot + 1) & size_mask;
    }

    return item_ptr;
}

void* hash_table_lookup_string_key(hash_table htable, const char* key)
{
    hash_table_node** table = htable.table;
    uint32_t size = htable.size;
    uint32_t size_mask = htable.size_mask;
    uint32_t hash = hash_string(key);
    uint32_t slot = hash & size_mask;
    uint32_t i = 0;
    void* item_ptr = NULL;

    while (i < size)
    {
        hash_table_node* node = table[slot];
        if (node != NULL && node->string_key == key)
        {
            item_ptr = table[slot];
            break;
        }

        ++i;
        slot = (slot + 1) & size_mask;
    }

    return item_ptr;
}
