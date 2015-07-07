/*
  Necronomicon - Deterministic Audio Engine
  Copyright 2014 - Chad McKinney and Curtis McKinney
 */

#ifndef NECRONOMICON_H_INCLUDED
#define NECRONOMICON_H_INCLUDED

#include <stdint.h>
#include <jack/jack.h>

#include "Necronomicon/Endian.h"

typedef enum { false, true } bool;

/////////////////////
// UGen
/////////////////////

typedef enum
{
    ControlRate = 0,
    AudioRate = 1,
} CalcRate;

struct ugen;
typedef struct ugen ugen;

struct ugen
{
    void (*calc)(ugen u);
    void (*constructor)(ugen* u);
    void (*deconstructor)(ugen* u);
    void* data; // ugen defined data structure
    double* constructor_args; // arguments passed in for use during construction
    uint32_t* inputs; // indexes to the parent synth's ugen wire buffer
    uint32_t* outputs; // indexes to the parent synth's ugen wire buffer
    CalcRate calc_rate;
    uint32_t __padding; // Pad struct size to 8 * 8 == 64 bytes
};

extern const uint32_t UGEN_SIZE;
extern const uint32_t UGEN_POINTER_SIZE;

typedef void (*ugen_constructor)(ugen* u);
typedef void (*ugen_deconstructor)(ugen* u);
typedef void (*calc_func)(ugen* u);

struct ugen_graph_pool_node;
typedef struct ugen_graph_pool_node ugen_graph_pool_node;
struct ugen_graph_pool_node
{
    ugen* ugen_graph;
    ugen_graph_pool_node* next_ugen_graph_pool_node;
    uint32_t pool_index;
};

extern const uint32_t UGEN_GRAPH_POOL_NODE_SIZE;
extern ugen_graph_pool_node** ugen_graph_pools;

struct ugen_wires_pool_node;
typedef struct ugen_wires_pool_node ugen_wires_pool_node;
struct ugen_wires_pool_node
{
    double* ugen_wires;
    ugen_wires_pool_node* next_ugen_wires_pool_node;
    uint32_t pool_index;
};

extern const uint32_t UGEN_WIRE_POOL_NODE_SIZE;
extern ugen_wires_pool_node** ugen_wires_pools;

////////////////////////
// SynthDef/Synth Node
////////////////////////

typedef enum
{
    NODE_DEAD, // Node is not playing
    NODE_SPAWNING, // Scheduled for playing
    NODE_ALIVE, // Actively playing
    NODE_SCHEDULED_FOR_REMOVAL, // Scheduled for removal from removal_fifo, first step in removal of synths
    NODE_SCHEDULED_FOR_FREE, // Scheduled for memory free, second and last step in removal of synths
} node_alive_status;

struct synth_node;
typedef struct synth_node synth_node;

struct synth_node
{
    ugen* ugen_graph; // UGen Graph
    ugen_graph_pool_node* ugen_graph_node; // ugen graph pool node, used to release the ugen graph memory during deconstruction
    double* ugen_wires; // UGen output wire buffers
    ugen_wires_pool_node* ugen_wires_node; // ugen wires pool node, used to release the ugen wire memory during reconstruction
    synth_node* previous; // Previous node, used in synth_list for the scheduler
    synth_node* next; // Next node, used in the synth_list for the scheduler
    jack_time_t time; // scheduled time, in microseconds
    uint32_t key; // Node ID, used to look up synths in the synth hash table
    uint32_t hash; // Cached hash of the node id for the synth hash table
    uint32_t table_index; // Cached hash table index
    uint32_t num_ugens;
    uint32_t num_wires;
    node_alive_status previous_alive_status; // Flag representing whether the previous status of the synth
    node_alive_status alive_status; // Flag representing the alive status of the synth
};

extern const uint32_t NODE_SIZE;
extern const uint32_t NODE_POINTER_SIZE;
extern const uint32_t MAX_SYNTHS;
extern const uint32_t HASH_TABLE_SIZE_MASK;

void free_synth(synth_node* synth);

/////////////////
// Message FIFO
/////////////////

extern const uint32_t MAX_FIFO_MESSAGES;
extern const uint32_t FIFO_SIZE_MASK;

extern uint32_t nrt_fifo_read_index;
extern uint32_t nrt_fifo_write_index;

extern uint32_t rt_fifo_read_index;
extern uint32_t rt_fifo_write_index;

// Increment fifo_write_index and fifo_read_index after assignment to maintain intended ordering (using a memory barrier): Assignment/Lookup -> Increment
#define FIFO_PUSH(fifo, write_index, node, size_mask) fifo[write_index & size_mask] = node; __sync_synchronize(); write_index++;
#define FIFO_POP(fifo, read_index, size_mask) fifo[read_index & size_mask]; __sync_synchronize(); read_index++;

// Non-realtime thread FIFO push/pop
#define NRT_FIFO_PUSH(node) FIFO_PUSH(nrt_fifo, nrt_fifo_write_index, node, FIFO_SIZE_MASK)
#define NRT_FIFO_POP() FIFO_POP(nrt_fifo, nrt_fifo_read_index, FIFO_SIZE_MASK)

// Realtime thread FIFO push/pop
#define RT_FIFO_PUSH(node) FIFO_PUSH(rt_fifo, rt_fifo_write_index, node, FIFO_SIZE_MASK)
#define RT_FIFO_POP() FIFO_POP(rt_fifo, rt_fifo_read_index, FIFO_SIZE_MASK)

///////////////////////
// Scheduled Node List
///////////////////////

// An ordered list of ugen nodes
typedef synth_node** node_list;
extern node_list scheduled_node_list;

node_list new_node_list();
void scheduled_list_free();
void scheduled_list_sort();

extern uint32_t scheduled_list_read_index;
extern uint32_t scheduled_list_write_index;

#define SCHEDULED_LIST_PUSH(node) FIFO_PUSH(scheduled_node_list, scheduled_list_write_index, node, FIFO_SIZE_MASK)
#define SCHEDULED_LIST_POP() FIFO_POP(scheduled_node_list, scheduled_list_read_index, FIFO_SIZE_MASK)
#define SCHEDULED_LIST_PEEK() (scheduled_node_list[scheduled_list_read_index & FIFO_SIZE_MASK])
#define SCHEDULED_LIST_PEEK_TIME() ((scheduled_node_list[scheduled_list_read_index & FIFO_SIZE_MASK])->time)

///////////////////////
// Removal FIFO
///////////////////////

// List used by ugens to queue for removal during RT runtime

extern const uint32_t MAX_REMOVAL_IDS; // Max number of ids able to be scheduled for removal *per sample frame*
extern const uint32_t REMOVAL_FIFO_SIZE_MASK;
typedef synth_node** node_fifo;

///////////////////////////
// Hash Table
///////////////////////////

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

// Fixed memory hash table using open Addressing with linear probing
// This is not thread safe.
typedef synth_node** hash_table;

// Synth hash table
extern hash_table synth_table;

hash_table hash_table_new();
void hash_table_free(hash_table table);
void hash_table_insert(hash_table table, synth_node* node);
bool hash_table_remove(hash_table table, synth_node* node);
synth_node* hash_table_lookup(hash_table table, uint32_t key);

///////////////////////////
// Doubly Linked List
///////////////////////////

typedef synth_node* doubly_linked_list;
extern doubly_linked_list synth_list;

doubly_linked_list doubly_linked_list_push(doubly_linked_list list, synth_node* node);
doubly_linked_list doubly_linked_list_remove(doubly_linked_list list, synth_node* node);
void doubly_linked_list_free(doubly_linked_list list);

///////////////////////////
// Misc
///////////////////////////

// default type sizes

extern const uint32_t DOUBLE_SIZE;
extern const uint32_t UINT_SIZE;

// Helper functions

void print_node(synth_node* node);
void print_synth_list();

// Forward declared ugen functions, used during testing, as well as norma ugen usage

void sin_constructor(ugen* u);
void sin_deconstructor(ugen* u);
void sin_a_calc(ugen u);

#endif // NECRONOMICON_H_INCLUDED
