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
    hash_table_clear(htable, free_items_in_table);
    free(htable.table);
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

            if (node->key_type == string_key_type)
                free(node->string_key);

            free(node);
        }
    }

    free(table);
}

void print_hash_table_node(hash_table_node* node)
{
    if (node != NULL)
    {
        printf("hash_table_node %p { item: %p, key_type: %i, uint_key: %u, string_key: %s, hash: %u }\n",
            node,
            node->item,
            node->key_type,
            node->uint_key,
            node->string_key,
            node->hash
        );
    }

    else
    {
        puts("hash_table_node = NULL");
    }
}

bool hash_table_insert_uint_key(hash_table htable, void* item, uint32_t key)
{
    hash_table_node** table = htable.table;
    uint32_t size = htable.size;
    uint32_t size_mask = htable.size_mask;
    uint32_t hash = HASH_KEY(key);
    uint32_t slot = hash & size_mask;
    uint32_t i;
    hash_table_node* node_ptr = NULL;
    bool update_current_node = false;
    bool is_insertion_successful = false;

    for (i = 0; i < size; ++i)
    {
        node_ptr = table[slot];
        if (node_ptr == NULL)
        {
            break;
        }

        else if (node_ptr->uint_key == key)
        {
            update_current_node = true;
            break;
        }

        slot = (slot + 1) & size_mask;
    }

    if (i < size)
    {
        if (update_current_node == true)
        {
            node_ptr->item = item;
        }

        else
        {
            node_ptr = malloc(HASH_TABLE_NODE_SIZE);
            hash_table_node node = { item, uint_key_type, key, 0, hash };
            *node_ptr = node;
            table[slot] = node_ptr;
        }

        is_insertion_successful = true;
    }

    return is_insertion_successful;
}

// expects a null terminated string
char* copy_string(const char* string)
{
    char* copy = NULL;
    if (string != NULL)
    {
        const uint32_t null_terminator_length = 1;
        uint32_t length = strlen(string) + null_terminator_length;
        copy = malloc(length);
        strcpy(copy, string);
    }

    return copy;
}

// copies the string key internally
bool hash_table_insert_string_key(hash_table htable, void* item, const char* key)
{
    bool is_insertion_successful = false;
    if (key == NULL)
    {
        puts("hash_table_insert_string_key error: key is null.");
    }

    else if (htable.table == NULL)
    {
        puts("hash_table_insert_string_key error: htable.table is null.");
    }

    else
    {
        hash_table_node** table = htable.table;
        uint32_t size = htable.size;
        uint32_t size_mask = htable.size_mask;
        uint32_t hash = hash_string(key);
        uint32_t slot = hash & size_mask;
        uint32_t i;
        hash_table_node* node_ptr = NULL;
        bool update_current_node = false;

        for (i = 0; i < size; ++i)
        {
            node_ptr = table[slot];
            if (node_ptr == NULL)
            {
                break;
            }

            else if (node_ptr->hash == hash && strcmp(node_ptr->string_key, key) == 0)
            {
                update_current_node = true;
                break;
            }

            slot = (slot + 1) & size_mask;
        }

        if (i < size)
        {
            if (update_current_node == true)
            {
                node_ptr->item = item;
            }

            else
            {
                node_ptr = malloc(HASH_TABLE_NODE_SIZE);
                char* key_copy = copy_string(key);
                hash_table_node node = { item, string_key_type, 0, key_copy, hash };
                *node_ptr = node;
                table[slot] = node_ptr;
            }

            is_insertion_successful = true;
        }
    }

    return is_insertion_successful;
}

bool hash_table_remove_uint_key(hash_table htable, uint32_t key)
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

bool hash_table_remove_string_key(hash_table htable, const char* key)
{
    hash_table_node** table = htable.table;
    uint32_t size = htable.size;
    uint32_t size_mask = htable.size_mask;
    uint32_t hash = hash_string(key);
    uint32_t index = hash & size_mask;
    bool is_removal_successful = false;

    hash_table_node* node = table[index];
    if (node != NULL && node->hash == hash && strcmp(node->string_key, key) == 0)
    {
        free(node->string_key);
        free(node);
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
            item_ptr = node->item;
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
        if (node != NULL && node->hash == hash && strcmp(node->string_key, key) == 0)
        {
            item_ptr = node->item;
            break;
        }

        ++i;
        slot = (slot + 1) & size_mask;
    }

    return item_ptr;
}

const char* hash_table_get_string_key(hash_table htable, const char* key)
{
    hash_table_node** table = htable.table;
    uint32_t size = htable.size;
    uint32_t size_mask = htable.size_mask;
    uint32_t hash = hash_string(key);
    uint32_t slot = hash & size_mask;
    uint32_t i = 0;
    const char* string_key_ptr = NULL;

    while (i < size)
    {
        hash_table_node* node = table[slot];
        if (node != NULL && node->hash == hash && strcmp(node->string_key, key) == 0)
        {
            string_key_ptr = node->string_key;
            break;
        }

        ++i;
        slot = (slot + 1) & size_mask;
    }

    return string_key_ptr;
}

void hash_table_clear(hash_table htable, bool free_items_in_table)
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

            if (node->key_type == string_key_type)
                free(node->string_key);

            free(node);
        }
    }
}
