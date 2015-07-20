/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#ifndef NECRONOMICON_H_INCLUDED
#define NECRONOMICON_H_INCLUDED

#include <stdint.h>
#include <jack/jack.h>

#include "Containers/HashTable.h"

/////////////////////
// Constants
/////////////////////

extern const uint32_t DOUBLE_SIZE;
extern const uint32_t UINT_SIZE;

#ifndef M_PI
#define M_PI 3.1415926535897932384626433832795028841971693993751058209749445923078164062L
#endif

#define LOG_001 -6.907755278982137

extern const double TWO_PI;
extern const double RECIP_TWO_PI;
extern const double HALF_PI;
extern const double QUARTER_PI;

#define TABLE_SIZE 65536
#define TABLE_SIZE_MASK 65535
#define DOUBLE_TABLE_SIZE 65536.0
extern double RECIP_TABLE_SIZE;
extern uint32_t HALF_TABLE_SIZE;
extern uint32_t QUATER_TABLE_SIZE;
extern double sine_table[TABLE_SIZE];
extern double cosn_table[TABLE_SIZE];
extern double sinh_table[TABLE_SIZE];
extern double atan_table[TABLE_SIZE];
extern double tanh_table[TABLE_SIZE];

#define PAN_TABLE_SIZE 4096
#define PAN_TABLE_SIZE_MASK 4095
#define DOUBLE_PAN_TABLE_SIZE 4096.0
extern double PAN_RECIP_TABLE_SIZE;
extern uint32_t PAN_HALF_TABLE_SIZE;
extern uint32_t PAN_QUATER_TABLE_SIZE;
extern double pan_table[PAN_TABLE_SIZE];

extern double SAMPLE_RATE;
extern double RECIP_SAMPLE_RATE;
extern double TABLE_MUL_RECIP_SAMPLE_RATE;
extern double TWO_PI_TIMES_RECIP_SAMPLE_RATE;
extern uint32_t BLOCK_SIZE;

/////////////////////
// Global Mutables
/////////////////////

extern double* _necronomicon_buses;
extern uint32_t num_audio_buses;
extern uint32_t last_audio_bus_index;
extern uint32_t num_audio_buses_bytes;
extern int32_t num_synths;

extern jack_nframes_t current_cycle_frames;
extern jack_time_t current_cycle_usecs;
extern jack_time_t next_cycle_usecs;
extern float period_usecs;
extern const jack_time_t USECS_PER_SECOND;
extern double usecs_per_frame;
extern long double recip_usecs_per_frame;
extern jack_time_t BLOCK_SIZE_USECS;

extern float out_bus_buffers[16][512];
extern uint32_t  out_bus_buffer_index;

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
    void* constructor_args; // arguments passed in for use during construction
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
extern const uint32_t SYNTH_HASH_TABLE_SIZE_MASK;
extern synth_node* _necronomicon_current_node; // pointer to current node being processed in the audio loop
extern synth_node* _necronomicon_current_node_underconstruction; // pointer to node being constructed in the NRT thread

void free_synth(synth_node* synth);

/////////////////////
// Sample Buffer
/////////////////////

struct sample_buffer;
typedef struct sample_buffer sample_buffer;

struct sample_buffer
{
    double* samples;
    sample_buffer* next_sample_buffer;
    uint32_t pool_index;
    uint32_t num_samples;
    uint32_t num_samples_mask; // used with power of 2 sized buffers
    uint32_t num_channels;
};

extern const uint32_t SAMPLE_BUFFER_SIZE;
extern const uint32_t SAMPLE_BUFFER_POINTER_SIZE;

sample_buffer* acquire_sample_buffer(uint32_t num_samples);
void release_sample_buffer(sample_buffer* buffer);
void print_sample_buffer(sample_buffer* buffer);

/////////////////
// Message FIFO
/////////////////

typedef union
{
    synth_node* node;
    uint32_t node_id;
    const int8_t* string;
    double number;
} message_arg;

typedef enum
{
    IGNORE,
    START_SYNTH,
    STOP_SYNTH,
    FREE_SYNTH, // Free synth memory
    SHUTDOWN,
    PRINT,
    PRINT_NUMBER
} message_type;

extern const int8_t* message_map[];

typedef struct
{
    message_arg arg;
    message_type type;
} message;

extern const uint32_t MESSAGE_SIZE;
extern const uint32_t MAX_FIFO_MESSAGES;
extern const uint32_t FIFO_SIZE_MASK;

// Lock Free FIFO Queue (Ring Buffer)
typedef message* message_fifo;

extern message_fifo nrt_fifo;
extern uint32_t nrt_fifo_read_index;
extern uint32_t nrt_fifo_write_index;

extern message_fifo rt_fifo;
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

extern node_fifo removal_fifo;
extern uint32_t removal_fifo_read_index;
extern uint32_t removal_fifo_write_index;
extern int32_t removal_fifo_size;

#define REMOVAL_FIFO_PUSH(id) FIFO_PUSH(removal_fifo, removal_fifo_write_index, id, REMOVAL_FIFO_SIZE_MASK)
#define REMOVAL_FIFO_POP() FIFO_POP(removal_fifo, removal_fifo_read_index, REMOVAL_FIFO_SIZE_MASK)

inline void try_schedule_current_synth_for_removal()
{
    if (_necronomicon_current_node && (removal_fifo_size < REMOVAL_FIFO_SIZE_MASK) && _necronomicon_current_node->alive_status == NODE_ALIVE)
    {
        _necronomicon_current_node->previous_alive_status = _necronomicon_current_node->alive_status;
        _necronomicon_current_node->alive_status = NODE_SCHEDULED_FOR_REMOVAL;
        removal_fifo_size = (removal_fifo_size + 1) & REMOVAL_FIFO_SIZE_MASK;
        REMOVAL_FIFO_PUSH(_necronomicon_current_node);
    }
}

///////////////////////////
// Synth Hash Table
///////////////////////////

// Fixed memory hash table using open Addressing with linear probing
// This is not thread safe.

typedef synth_node** synth_hash_table;
extern synth_hash_table synth_table;

synth_hash_table synth_hash_table_new();
void synth_hash_table_free(synth_hash_table table);
void synth_hash_table_insert(synth_hash_table table, synth_node* node);
bool synth_hash_table_remove(synth_hash_table table, synth_node* node);
synth_node* synth_hash_table_lookup(synth_hash_table table, uint32_t key);

///////////////////////////
// Doubly Linked List
///////////////////////////

typedef synth_node* doubly_linked_list;
extern doubly_linked_list synth_list;

doubly_linked_list doubly_linked_list_push(doubly_linked_list list, synth_node* node);
doubly_linked_list doubly_linked_list_remove(doubly_linked_list list, synth_node* node);
void doubly_linked_list_free(doubly_linked_list list);

///////////////////////////
// Sample Registry
///////////////////////////

sample_buffer* retrieve_sample_buffer(const char* file_path);
void load_and_register_sample(const char* file_path);
void load_and_register_samples(const char** file_paths, uint32_t num_files);

///////////////////////////
// Misc
///////////////////////////

// Helper functions and defines

extern uint32_t _block_frame; // Current block frame number
extern synth_node _necronomicon_current_node_object; // Current node being processed

#define UGEN_INPUT_BUFFER(ugen, index) (_necronomicon_current_node_object.ugen_wires + (ugen.inputs[index] * BLOCK_SIZE))
#define UGEN_OUTPUT_BUFFER(ugen, index) (_necronomicon_current_node_object.ugen_wires + (ugen.outputs[index] * BLOCK_SIZE))

#define AUDIO_LOOP(func)                            \
for (; _block_frame < BLOCK_SIZE; ++_block_frame)   \
{                                                   \
    func                                            \
}

#define UGEN_IN(wire_frame_buffer) wire_frame_buffer[_block_frame]
#define UGEN_OUT(wire_frame_buffer, out_value) wire_frame_buffer[_block_frame] = out_value

void null_deconstructor(ugen* u); // Does nothing
void null_constructor(ugen* u);

void print_node(synth_node* node);
void print_synth_list();

extern const int8_t* RESOUCES_PATH;

// Forward declared ugen functions

void sin_constructor(ugen* u);
void sin_deconstructor(ugen* u);
void sin_a_calc(ugen u);

bool minBLEP_Init();

#endif // NECRONOMICON_H_INCLUDED
