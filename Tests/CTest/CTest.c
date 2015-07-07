/*
    Necronomicon
      Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "Necronomicon.h"

//// Test FIFO

synth_node* new_test_synth(uint32_t time)
{
    ugen test_ugen = { &sin_a_calc, &sin_constructor, &sin_deconstructor, NULL, NULL, NULL };
    test_ugen.constructor(&test_ugen);

    synth_node* test_synth = malloc(NODE_SIZE);
    test_synth->ugen_graph = malloc(UGEN_SIZE);
    test_synth->ugen_wires = malloc(DOUBLE_SIZE);
    test_synth->previous = NULL;
    test_synth->next = NULL;
    test_synth->key = 0;
    test_synth->hash = 0;
    test_synth->table_index = 0;
    test_synth->num_ugens = 1;
    test_synth->num_wires = 1;
    test_synth->time = time;

    test_synth->ugen_graph[0] = test_ugen;
    test_synth->ugen_wires[0] = 0;
    return test_synth;
}

void print_list(node_list list)
{
    printf("scheduled_list_read_index: %i, scheduled_list_write_index: %i\n", scheduled_list_read_index, scheduled_list_write_index);
    uint32_t i = scheduled_list_read_index & FIFO_SIZE_MASK;
    scheduled_list_write_index = scheduled_list_write_index & FIFO_SIZE_MASK;

    for (; i != scheduled_list_write_index; i = (i + 1) & FIFO_SIZE_MASK)
    {
        print_node(list[i]);
        printf("\n");
    }
}

void randomize_and_print_list(node_list list)
{
    puts("\n//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////");
    puts("// RANDOMIZE LIST");
    puts("//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////\n");

    uint32_t i;
    for (i = 0; i < 1000; ++i)
    {
        uint32_t num_pop = random() / (double) RAND_MAX * 100;
        while ((num_pop > 0) && ((scheduled_list_read_index & FIFO_SIZE_MASK) != ((scheduled_list_write_index - 1) & FIFO_SIZE_MASK)))
        {
            SCHEDULED_LIST_POP();
            --num_pop;
        }

        uint32_t num_push = random() / (double) RAND_MAX * 100;
        while ((num_push > 0) && ((scheduled_list_read_index & FIFO_SIZE_MASK) != (scheduled_list_write_index & FIFO_SIZE_MASK)))
        {
            synth_node* node = new_test_synth((random() / (double) RAND_MAX) * 10000.0);
            SCHEDULED_LIST_PUSH(node);
            --num_push;
        }
    }

    scheduled_list_read_index = scheduled_list_read_index & FIFO_SIZE_MASK;
    scheduled_list_write_index = scheduled_list_write_index & FIFO_SIZE_MASK;
    print_list(list);
}

void sort_and_print_list(node_list list)
{
    puts("\n//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////");
    puts("// SORT LIST");
    puts("//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////\n");

    scheduled_list_sort();
    print_list(list);
}

void test_list()
{
    scheduled_node_list = new_node_list();
    synth_table = hash_table_new();

    while (scheduled_list_write_index < (MAX_FIFO_MESSAGES * 0.75))
    {
        synth_node* node = new_test_synth((random() / (double) RAND_MAX) * 10000.0);
        SCHEDULED_LIST_PUSH(node);
    }

    print_list(scheduled_node_list);

    uint32_t i = 0;
    for (; i < 100; ++i)
    {
        sort_and_print_list(scheduled_node_list);
        randomize_and_print_list(scheduled_node_list);
    }

    sort_and_print_list(scheduled_node_list);
    puts("scheduled_list_free()");
    scheduled_list_free();
    hash_table_free(synth_table);
    synth_table = NULL;
}

//// Test Hash Table

void print_hash_table(hash_table table)
{
    puts("\n\n//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////");
    puts("// Hash Table");
    puts("//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////\n");

    printf("hash_table [");

    uint32_t i;
    for (i = 0; i < MAX_SYNTHS; ++i)
    {
        // print_node(table[i]);
        if (i < (MAX_SYNTHS - 1))
            printf(", ");
    }

    printf("]\n\n");
}

uint32_t num_values = 5000;
uint32_t times[5000];

void test_hash_table()
{
    uint32_t i = 0;
    for (i = 0; i < 1000; ++i)
    {
        printf("key: %u, hash: %u, slot %u\n", i, HASH_KEY(i), HASH_KEY(i) & HASH_TABLE_SIZE_MASK);
    }

    synth_table = hash_table_new();

    for (i = 0; i < num_values; ++i)
    {
        times[i] = (random() / (double) RAND_MAX) * 10000.0;
        synth_node* node = new_test_synth(times[i]);
        node->key = i;
        node->hash = HASH_KEY(i);
        node->table_index = i;
        hash_table_insert(synth_table, node);
        assert(node == hash_table_lookup(synth_table, i));
    }

    print_hash_table(synth_table);
    puts("Asserting table values...\n\n");

    for (i = 0; i < num_values; ++i)
    {
        synth_node* node = hash_table_lookup(synth_table, i);
        assert(node);
        assert(node->time == times[i]);
        assert(node->key == i);
        assert(node->hash == HASH_KEY(i));
    }

    puts("Removing table values...\n\n");

    for (i = 0; i < num_values; ++i)
    {
        synth_node* node = hash_table_lookup(synth_table, i);
        assert(node);
        hash_table_remove(synth_table, node);
        free_synth(node);
    }

    puts("Asserting NULL values...\n\n");

    for (i = 0; i < MAX_SYNTHS; ++i)
    {
        assert(hash_table_lookup(synth_table, i) == NULL);
    }

    print_hash_table(synth_table);
    puts("Freeing table...\n\n");
    hash_table_free(synth_table);
}

//// Test Doubly Linked List

void doubly_linked_list_print(doubly_linked_list list)
{
    synth_node* node = list;
    while (node)
    {
        print_node(node);
        node = node->next;
    }
}

typedef bool node_filter_func(synth_node* node);
doubly_linked_list doubly_linked_list_filter(doubly_linked_list list, node_filter_func func)
{
    synth_node* node = list;
    while (node)
    {
        synth_node* next = node->next;

        if (!func(node))
        {
            puts("\nFiltered out node:");
            print_node(node);
            list = doubly_linked_list_remove(list, node);
            free_synth(node);
        }

        node = next;
    }

    return list;
}

bool is_odd(synth_node* node)
{
    if (node == NULL)
        return false;

    return (int32_t) node->time % 2 == 1;
}

void test_doubly_linked_list()
{
    int32_t i; // Don't make this unsigned, we'll go infinite!
    for (i = 50; i >= 0; --i)
    {
        synth_node* node = new_test_synth(i);
        synth_list = doubly_linked_list_push(synth_list, node);
    }

    puts("\n\n//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////");
    puts("// LIST");
    puts("//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////\n");

    doubly_linked_list_print(synth_list);

    synth_node* node = synth_list;
    while (node)
    {
        synth_node* next = node->next;

        if (next)
        {
            printf("(next: %llu) - (node: %llu) = %llu\n", next->time, node->time, next->time - node->time);
            assert((next->time - node->time) == 1);
        }

        node = next;
    }

    synth_list = doubly_linked_list_filter(synth_list, is_odd);

    puts("\n\n//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////");
    puts("// LIST");
    puts("//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////\n");

    doubly_linked_list_print(synth_list);

    node = synth_list;
    while (node)
    {
        synth_node* next = node->next;

        if (next)
        {
            printf("(next: %llu) - (node: %llu) = %llu\n", next->time, node->time, next->time - node->time);
            assert((next->time - node->time) == 2);
        }

        node = next;
    }

    doubly_linked_list_free(synth_list);
}
