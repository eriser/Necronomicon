/*
    Necronomicon
      Copyright 2014-2015 Chad McKinney and Curtis McKinney

    To Do:

    Fix pluck (it's very broken...)
    zapgremlins in filters to prevent blow ups
    break up code into several files
    remove calc rate from ugen struct, it's not being used, nor do I predict that it will be
    random seeding?
    Consider changing the free_synths stack to be a queue with a minimum size.

    Various Noise ugens
        Gray
        Black
        Blue
        Violet
        green noise
        clip
        burst noise
        Diamond square (plasma)

    better line and xline implementations (with range arguments)
    Other env ugens like adr, adsr, etc...
    glitchy oscillators, with table range and maybe phase multiplier? Is this doable with just an osc ugen?
    Decay
    Pitch Shift
    Freq Shift
    Compressor/Expander
    Chorus/Flanger/Phaser
    Trigger UGens
    Sample Playback / Buffer UGens
    Correct Groups implementation
    Wave Shaper
    Wave Tables
    Pitch Detection
    Zero Crossing
    YiG Gens
    Wave Terrain (3D Wave terrain? 1D Wave Terrain? 4D? ND?)
    basic filters (One pole, two pole, zero, HZPF, integrator)
    demand style ugens (seq, etc...)

    Fix small block size, may require a more optimized clear_necronomicon_buses()
    MIDI support
    Granular synthesis
    Concatenative Synthesis
    Scanned synthesis
    Wavelets
    Fractal UGens
    Cellular Automata
    Flocking -- grains?
    FFT
    Formant
    Drum Modelling

    abstract away the audio backend to provide support beyond jack.
    HRTF
    Ambisonics?
*/

#ifndef NECRONOMICON_H_INCLUDED
#define NECRONOMICON_H_INCLUDED

#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdint.h>

#ifndef WIN32
#include <unistd.h>
#endif
#include <jack/jack.h>
#include <time.h>
#include <limits.h>
#include <sndfile.h>

#include "Necronomicon.h"
#include "Necronomicon/Endian.h"

static inline uint32_t next_power_of_two(uint32_t v)
{
    --v;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    return ++v;
}

#define LERP(A,B,D) (A+D*(B-A))
#define likely(x)   __builtin_expect((x),1)
#define unlikely(x) __builtin_expect((x),0)

/////////////////////
// Constants
/////////////////////

typedef enum { false, true } bool;

#ifndef M_PI
#define M_PI 3.1415926535897932384626433832795028841971693993751058209749445923078164062L
#endif

const double TWO_PI = M_PI * 2;
const double RECIP_TWO_PI =  1.0 / (M_PI * 2);
const double HALF_PI = M_PI * 0.5;
const double QUARTER_PI = M_PI * 0.25;
uint32_t DOUBLE_SIZE = sizeof(double);
uint32_t UINT_SIZE = sizeof(uint32_t);

#define TABLE_SIZE 65536
#define TABLE_SIZE_MASK 65535
#define DOUBLE_TABLE_SIZE 65536.0
double RECIP_TABLE_SIZE = 1.0 / (double) TABLE_SIZE;
uint32_t HALF_TABLE_SIZE = TABLE_SIZE / 2;
uint32_t QUATER_TABLE_SIZE = TABLE_SIZE / 4;
double sine_table[TABLE_SIZE];
double cosn_table[TABLE_SIZE];
double sinh_table[TABLE_SIZE];
double atan_table[TABLE_SIZE];
double tanh_table[TABLE_SIZE];

#define PAN_TABLE_SIZE 4096
#define PAN_TABLE_SIZE_MASK 4095
double PAN_RECIP_TABLE_SIZE = 1.0 / (double) TABLE_SIZE;
uint32_t PAN_HALF_TABLE_SIZE = PAN_TABLE_SIZE / 2;
uint32_t PAN_QUATER_TABLE_SIZE = PAN_TABLE_SIZE / 4;
double pan_table[PAN_TABLE_SIZE];

double SAMPLE_RATE = 44100;
double RECIP_SAMPLE_RATE = 1.0 / 44100.0;
double TABLE_MUL_RECIP_SAMPLE_RATE = TABLE_SIZE * (1.0 / 44100.0);
double TWO_PI_TIMES_RECIP_SAMPLE_RATE;
uint32_t BLOCK_SIZE = 64;
#define LOG_001 -6.907755278982137

/////////////////////
// Hashing
/////////////////////

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
const uint64_t PRIME = 0x01000193; // 16777619
const uint64_t SEED = 0x811C9DC5; // 2166136261

#define FNV1A(byte, hash) ((byte ^ hash) * PRIME)
#define HASH_KEY_PRIV(key) (FNV1A(key.bytes[3], FNV1A(key.bytes[2], FNV1A(key.bytes[1], FNV1A(key.bytes[0], SEED)))))
#define HASH_KEY(key) ((uint32_t) HASH_KEY_PRIV(((four_bytes) key)))

/////////////////////
// Global Mutables
/////////////////////

double* _necronomicon_buses = NULL;
uint32_t num_audio_buses = 256;
uint32_t last_audio_bus_index = 255;
uint32_t num_audio_buses_bytes;
int32_t num_synths = 0;
FILE* devurandom = NULL;

// Time
jack_nframes_t current_cycle_frames = 0;
jack_time_t current_cycle_usecs = 0;
jack_time_t next_cycle_usecs = 0;
float period_usecs = 0;
const jack_time_t USECS_PER_SECOND = 1000000;
double usecs_per_frame = 1000000 / 44100;
long double recip_usecs_per_frame = ((long double ) 1.0) / ((long double) 44100);
jack_time_t BLOCK_SIZE_USECS = 0;

float out_bus_buffers[16][512];
uint32_t  out_bus_buffer_index = 0;

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

const uint32_t UGEN_SIZE = sizeof(ugen);
const uint32_t UGEN_POINTER_SIZE = sizeof(ugen*);

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

const uint32_t UGEN_GRAPH_POOL_NODE_SIZE = sizeof(ugen_graph_pool_node);
const uint32_t NUM_UGEN_GRAPH_POOLS = 32;
ugen_graph_pool_node** ugen_graph_pools = NULL;

void print_ugen_graph_pool_node(ugen_graph_pool_node* ugen_graph)
{
    if (ugen_graph != NULL)
    {
        printf(
            "ugen_graph_pool_node %p { ugen_graph = %p, next_ugen_graph_pool_node = %p, pool_index = %u }\n",
            ugen_graph, ugen_graph->ugen_graph, ugen_graph->next_ugen_graph_pool_node, ugen_graph->pool_index);
    }

    else
    {
        printf("ugen_graph_pool_node = NULL\n");
    }
}

ugen_graph_pool_node* acquire_ugen_graph(uint32_t num_ugens)
{
    uint32_t pow_two_num_bytes = next_power_of_two(num_ugens * UGEN_SIZE);
    uint32_t pool_index = __builtin_ctz(pow_two_num_bytes);
    ugen_graph_pool_node* ugen_graph = ugen_graph_pools[pool_index];

    if (ugen_graph != NULL)
    {
        ugen_graph_pools[pool_index] = ugen_graph->next_ugen_graph_pool_node;
    }

    else
    {
        ugen_graph = malloc(UGEN_GRAPH_POOL_NODE_SIZE);
        ugen_graph->ugen_graph = malloc(pow_two_num_bytes);
        ugen_graph->pool_index = pool_index;
    }

    ugen_graph->next_ugen_graph_pool_node = NULL;
    return ugen_graph;
}

void release_ugen_graph(ugen_graph_pool_node* ugen_graph)
{
    if (ugen_graph != NULL)
    {
        uint32_t pool_index = ugen_graph->pool_index;
        ugen_graph->next_ugen_graph_pool_node = ugen_graph_pools[pool_index];
        ugen_graph_pools[pool_index] = ugen_graph;
    }
}

struct ugen_wires_pool_node;
typedef struct ugen_wires_pool_node ugen_wires_pool_node;
struct ugen_wires_pool_node
{
    double* ugen_wires;
    ugen_wires_pool_node* next_ugen_wires_pool_node;
    uint32_t pool_index;
};

const uint32_t UGEN_WIRE_POOL_NODE_SIZE = sizeof(ugen_wires_pool_node);
const uint32_t NUM_UGEN_WIRES_POOLS = 32;
ugen_wires_pool_node** ugen_wires_pools = NULL;

void print_ugen_wires_pool_node(ugen_wires_pool_node* ugen_wires)
{
    if (ugen_wires != NULL)
    {
        printf(
            "ugen_wires_pool_node %p { ugen_wires = %p, next_ugen_wires_pool_node = %p, pool_index = %u }\n",
            ugen_wires, ugen_wires->ugen_wires, ugen_wires->next_ugen_wires_pool_node, ugen_wires->pool_index);
    }

    else
    {
        printf("ugen_wires_pool_node = NULL\n");
    }
}

ugen_wires_pool_node* acquire_ugen_wires(uint32_t num_wires)
{
    uint32_t pow_two_num_bytes = next_power_of_two(num_wires * BLOCK_SIZE * DOUBLE_SIZE);
    uint32_t pool_index = __builtin_ctz(pow_two_num_bytes);
    ugen_wires_pool_node* ugen_wires = ugen_wires_pools[pool_index];

    if (ugen_wires != NULL)
    {
        ugen_wires_pools[pool_index] = ugen_wires->next_ugen_wires_pool_node;
    }

    else
    {
        ugen_wires = malloc(UGEN_WIRE_POOL_NODE_SIZE);
        ugen_wires->ugen_wires = malloc(pow_two_num_bytes);
        ugen_wires->pool_index = pool_index;
    }

    ugen_wires->next_ugen_wires_pool_node = NULL;
    return ugen_wires;
}

void release_ugen_wires(ugen_wires_pool_node* ugen_wires)
{
    if (ugen_wires != NULL)
    {
        uint32_t pool_index = ugen_wires->pool_index;
        ugen_wires->next_ugen_wires_pool_node = ugen_wires_pools[pool_index];
        ugen_wires_pools[pool_index] = ugen_wires;
    }
}

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
};

const uint32_t SAMPLE_BUFFER_SIZE = sizeof(sample_buffer);
const uint32_t SAMPLE_BUFFER_POINTER_SIZE = sizeof(sample_buffer*);

const uint32_t NUM_SAMPLE_BUFFER_POOLS = 32;
sample_buffer** sample_buffer_pools;

void print_sample_buffer(sample_buffer* buffer)
{
    if (buffer != NULL)
    {
        printf(
            "sample_buffer %p { samples = %p, next_sample_buffer = %p, pool_index = %u, num_samples = %u, num_samples_mask = %u }\n",
            buffer, buffer->samples, buffer->next_sample_buffer, buffer->pool_index, buffer->num_samples, buffer->num_samples_mask);
    }

    else
    {
        printf("sample_buffer = NULL\n");
    }
}

// Note: this will return a sample buffer with a number of samples equal to the next power of two higher than the requested amount
sample_buffer* acquire_sample_buffer(uint32_t num_samples)
{
    uint32_t pow_two_num_samples = next_power_of_two(num_samples);
    uint32_t pool_index = __builtin_ctz(pow_two_num_samples);
    sample_buffer* buffer = sample_buffer_pools[pool_index];

    if (buffer != NULL)
    {
        sample_buffer_pools[pool_index] = buffer->next_sample_buffer;
        memset(buffer->samples, 0, pow_two_num_samples * DOUBLE_SIZE);
    }

    else
    {
        buffer = malloc(SAMPLE_BUFFER_SIZE);
        buffer->samples = calloc(pow_two_num_samples, DOUBLE_SIZE);
        buffer->pool_index = pool_index;
        buffer->num_samples = pow_two_num_samples;
        buffer->num_samples_mask = pow_two_num_samples - 1;
    }

    buffer->next_sample_buffer = NULL;
    return buffer;
}

void release_sample_buffer(sample_buffer* buffer)
{
    if (buffer != NULL)
    {
        uint32_t pool_index = buffer->pool_index;
        buffer->next_sample_buffer = sample_buffer_pools[pool_index];
        sample_buffer_pools[pool_index] = buffer;
    }
}

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

const int8_t* node_alive_status_strings[] = { "NODE_DEAD", "NODE_SPAWNING", "NODE_ALIVE", "NODE_SCHEDULED_FOR_REMOVAL", "NODE_SCHEDULED_FOR_FREE" };

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

synth_node* _necronomicon_current_node = NULL;
synth_node* _necronomicon_current_node_underconstruction = NULL;
const uint32_t NODE_SIZE = sizeof(synth_node);
const uint32_t NODE_POINTER_SIZE = sizeof(synth_node*);
synth_node* free_synths = NULL;
int32_t num_free_synths = 0;
const uint32_t max_free_synths = 128;

// Synth hash table
const uint32_t MAX_SYNTHS = 8192;
const uint32_t HASH_TABLE_SIZE_MASK = 8191;
typedef synth_node** hash_table;
hash_table synth_table = NULL;
bool hash_table_remove(hash_table table, synth_node* node);
synth_node* hash_table_lookup(hash_table table, uint32_t key);

void print_node(synth_node* node); // Forward declaration
void print_node_alive_status(synth_node* node)
{
    if (node != NULL)
    {
        printf("node_previous_status: %s, node_status: %s\n", node_alive_status_strings[node->previous_alive_status], node_alive_status_strings[node->alive_status]);
    }

    else
    {
        puts("node_status: NULL\n");
    }
}

synth_node* malloc_synth()
{
    if (free_synths != NULL)
    {
        // Pop a synth off the synth free list
        synth_node* synth = free_synths;
        free_synths = synth->next;
        synth->next = NULL;

        --num_free_synths;
        return synth;
    }

    return malloc(NODE_SIZE);
}

void deconstruct_synth(synth_node* synth)
{
    ugen* ugen_graph = synth->ugen_graph;
    uint32_t num_ugens = synth->num_ugens;
    uint32_t i;
    for (i = 0; i < num_ugens; ++i)
    {
        ugen* graph_node = &ugen_graph[i];
        graph_node->deconstructor(graph_node);
    }

    release_ugen_graph(synth->ugen_graph_node);
    release_ugen_wires(synth->ugen_wires_node);
    synth->ugen_graph_node = NULL;
    synth->ugen_graph = NULL;
    synth->ugen_wires_node = NULL;
    synth->ugen_wires = NULL;
}

void free_synth(synth_node* synth)
{
    if (synth != NULL)
    {
        bool found = hash_table_remove(synth_table, synth);
        --num_synths;

        if (found == true && synth->alive_status == NODE_SCHEDULED_FOR_FREE)
        {
            synth->previous_alive_status = synth->alive_status;
            synth->alive_status = NODE_DEAD;
            deconstruct_synth(synth);

            // Push the synth on to the free_synth stack
            synth->next = free_synths;
            free_synths = synth;
            synth->previous = NULL;
            ++num_free_synths;
        }

        else
        {
            printf("free_synth warning: Potential double free of synth with nodeID %u.\n", synth->key);
        }
    }

    else
    {
        puts("free_synth() passed a NULL pointer.");
    }
}

void free_synth_definition(synth_node* synth_definition)
{
    uint32_t i;
    uint32_t num_ugens = synth_definition->num_ugens;
    for (i = 0; i < num_ugens; ++i)
    {
        ugen* u = &synth_definition->ugen_graph[i];
        free(u->inputs);
        free(u->outputs);

        if (u->constructor_args != NULL)
            free(u->constructor_args);
    }

    free(synth_definition->ugen_graph);
    free(synth_definition);
}

synth_node* new_synth(synth_node* synth_definition, double* arguments, uint32_t num_arguments, uint32_t node_id, jack_time_t time)
{
    uint32_t i;

    synth_node* synth = malloc_synth();
    synth->previous = NULL;
    synth->next = NULL;
    synth->key = node_id;
    synth->hash = HASH_KEY(node_id);
    synth->table_index = 0;
    synth->num_ugens = synth_definition->num_ugens;
    synth->num_wires = synth_definition->num_wires;
    synth->time = time;
    synth->previous_alive_status = NODE_DEAD;
    synth->alive_status = NODE_SPAWNING;
    _necronomicon_current_node_underconstruction = synth;

    // Wires
    uint32_t num_wires = synth->num_wires;
    uint32_t size_wires = num_wires * BLOCK_SIZE * DOUBLE_SIZE;
    synth->ugen_wires_node = acquire_ugen_wires(num_wires);
    synth->ugen_wires = synth->ugen_wires_node->ugen_wires;
    memcpy(synth->ugen_wires, synth_definition->ugen_wires, size_wires);

    double* ugen_wires = synth->ugen_wires;
    for (i = 0; i < num_arguments; ++i)
    {
        double* wire_buffer = ugen_wires + (i * BLOCK_SIZE);
        unsigned j;
        *wire_buffer = arguments[i];
    }

    // UGens
    uint32_t num_ugens = synth_definition->num_ugens;
    uint32_t size_ugens = num_ugens * UGEN_SIZE;
    synth->ugen_graph_node = acquire_ugen_graph(num_ugens);
    synth->ugen_graph = synth->ugen_graph_node->ugen_graph;
    ugen* ugen_graph = synth->ugen_graph;
    memcpy(ugen_graph, synth_definition->ugen_graph, size_ugens);

    for (i = 0; i < num_ugens; ++i)
    {
        ugen* graph_node = &ugen_graph[i];
        graph_node->constructor(graph_node);
    }

    _necronomicon_current_node_underconstruction = NULL;
    return synth;
}

uint32_t _block_frame = 0;
synth_node _necronomicon_current_node_object;
static inline void process_synth(synth_node* synth)
{
    _necronomicon_current_node_object = *synth;
    ugen* ugen_graph = _necronomicon_current_node_object.ugen_graph;
    uint32_t num_ugens = _necronomicon_current_node_object.num_ugens;
    uint32_t _start_frame;

    if (synth->time > 0) // Initial schedule for synth, begin playing part way through block according to scheduled time
    {
        // find the current frame. jack_time_t is platform dependant and sometimes signed or unsigned.
        // So let's convert to a type larger enough to contain all positive values from a uint_64_t but also supports negatives.
        long long time_dif = (long long) synth->time - (long long) current_cycle_usecs;
        _start_frame = time_dif <= 0 ? 0 : ((long double) time_dif * recip_usecs_per_frame); // Convert from microseconds to frames
        synth->time = 0;
    }

    else // Normal playback, full block
    {
        _start_frame = 0;
    }

    uint32_t i;
    ugen graph_node;
    for (i = 0; i < num_ugens; ++i)
    {
        _block_frame = _start_frame;
        graph_node = ugen_graph[i];
        graph_node.calc(graph_node);
    }
}

#define UGEN_INPUT_BUFFER(ugen, index) (_necronomicon_current_node_object.ugen_wires + (ugen.inputs[index] * BLOCK_SIZE))
#define UGEN_OUTPUT_BUFFER(ugen, index) (_necronomicon_current_node_object.ugen_wires + (ugen.outputs[index] * BLOCK_SIZE))

#define AUDIO_LOOP(func)                            \
for (; _block_frame < BLOCK_SIZE; ++_block_frame)   \
{                                                   \
    func                                            \
}                                                   \

#define UGEN_IN(wire_frame_buffer) wire_frame_buffer[_block_frame]
#define UGEN_OUT(wire_frame_buffer, out_value) wire_frame_buffer[_block_frame] = out_value

void null_deconstructor(ugen* u) {} // Does nothing
void null_constructor(ugen* u) { u->data = NULL; }
static inline void try_schedule_current_synth_for_removal(); // Forward declaration
bool minBLEP_Init(); //Curtis: MinBlep initialization Forward declaration

void initialize_wave_tables()
{
    uint32_t i;
    for (i = 0; i < TABLE_SIZE; ++i)
    {
        sine_table[i] =  sin((long double) TWO_PI * (((long double) i) / ((long double) TABLE_SIZE)));
        cosn_table[i] =  cos((long double) TWO_PI * (((long double) i) / ((long double) TABLE_SIZE)));
        sinh_table[i] = sinh((long double) TWO_PI * (((long double) i) / ((long double) TABLE_SIZE)));
        atan_table[i] = atan((long double) TWO_PI * (((long double) i) / ((long double) TABLE_SIZE)));
        tanh_table[i] = tanh((long double) TWO_PI * (((long double) i) / ((long double) TABLE_SIZE)));
    }

    for (i = 0; i < PAN_TABLE_SIZE; ++i)
    {
        pan_table[i] = (cos(TWO_PI * (((double) i) / ((double) PAN_TABLE_SIZE))) + 1) * 0.5;
    }

    // Needed to initialize minblep table.
    bool minblepInitialized = minBLEP_Init();
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Scheduler
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

const uint32_t MAX_FIFO_MESSAGES = 2048;
const uint32_t FIFO_SIZE_MASK = 2047;

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

const int8_t* message_map[] = { "IGNORE", "START_SYNTH", "STOP_SYNTH", "FREE_SYNTH", "SHUTDOWN", "PRINT", "PRINT_NUMBER" };

typedef struct
{
    message_arg arg;
    message_type type;
} message;

const uint32_t MESSAGE_SIZE = sizeof(message);

void print_fifo_message(message m)
{
    puts(message_map[m.type]);
};

// Lock Free FIFO Queue (Ring Buffer)
typedef message* message_fifo;

message_fifo nrt_fifo = NULL;
uint32_t nrt_fifo_read_index = 0;
uint32_t nrt_fifo_write_index = 0;

message_fifo rt_fifo = NULL;
uint32_t rt_fifo_read_index = 0;
uint32_t rt_fifo_write_index = 0;

// Increment fifo_write_index and fifo_read_index after assignment to maintain intended ordering (using a memory barrier): Assignment/Lookup -> Increment
#define FIFO_PUSH(fifo, write_index, node, size_mask) fifo[write_index & size_mask] = node; __sync_synchronize(); write_index++;
#define FIFO_POP(fifo, read_index, size_mask) fifo[read_index & size_mask]; __sync_synchronize(); read_index++;

// Non-realtime thread FIFO push/pop
#define NRT_FIFO_PUSH(node) FIFO_PUSH(nrt_fifo, nrt_fifo_write_index, node, FIFO_SIZE_MASK)
#define NRT_FIFO_POP() FIFO_POP(nrt_fifo, nrt_fifo_read_index, FIFO_SIZE_MASK)

// Realtime thread FIFO push/pop
#define RT_FIFO_PUSH(node) FIFO_PUSH(rt_fifo, rt_fifo_write_index, node, FIFO_SIZE_MASK)
#define RT_FIFO_POP() FIFO_POP(rt_fifo, rt_fifo_read_index, FIFO_SIZE_MASK)

// Allocate and null initialize a node list to be used as a node_fifo or node_list
message_fifo new_message_fifo()
{
    uint32_t byte_size = MESSAGE_SIZE * MAX_FIFO_MESSAGES;
    message_fifo fifo = (message_fifo) malloc(byte_size);
    assert(fifo);
    memset(fifo, 0, byte_size);

    return fifo;
}

void free_message_contents(message msg)
{
    switch (msg.type)
    {
    case START_SYNTH:
        free_synth(msg.arg.node);
        break;
    default:
        break;
    }
}

// Free all remaining nodes in the nrt_fifo and then free the nrt_fifo itself
void nrt_fifo_free()
{
    while (nrt_fifo_read_index != nrt_fifo_write_index)
    {
        message msg = NRT_FIFO_POP();
        free_message_contents(msg);
    }

    free(nrt_fifo);
    nrt_fifo = NULL;
    nrt_fifo_read_index = 0;
    nrt_fifo_write_index = 0;
}

// Free all remaining nodes in the nrt_fifo and then free the nrt_fifo itself
void rt_fifo_free()
{
    while (rt_fifo_read_index != rt_fifo_write_index)
    {
        message msg = RT_FIFO_POP();
        free_message_contents(msg);
    }

    free(rt_fifo);
    rt_fifo = NULL;
    rt_fifo_read_index = 0;
    rt_fifo_write_index = 0;
}

///////////////////////
// Scheduled Node List
///////////////////////

// An ordered list of ugen nodes
typedef synth_node** node_list;

node_list scheduled_node_list = NULL;
uint32_t scheduled_list_read_index = 0;
uint32_t scheduled_list_write_index = 0;

#define SCHEDULED_LIST_PUSH(node) FIFO_PUSH(scheduled_node_list, scheduled_list_write_index, node, FIFO_SIZE_MASK)
#define SCHEDULED_LIST_POP() FIFO_POP(scheduled_node_list, scheduled_list_read_index, FIFO_SIZE_MASK)
#define SCHEDULED_LIST_PEEK() (scheduled_node_list[scheduled_list_read_index & FIFO_SIZE_MASK])
#define SCHEDULED_LIST_PEEK_TIME() ((scheduled_node_list[scheduled_list_read_index & FIFO_SIZE_MASK])->time)

// Allocate and null initialize a node list to be used as a node_list
node_list new_node_list()
{
    uint32_t byte_size = NODE_POINTER_SIZE * MAX_FIFO_MESSAGES;
    node_list list = (node_list) malloc(byte_size);
    assert(list);
    memset(list, 0, byte_size);

    return list;
}

// Free all remaining nodes in the list and then free the list itself
void scheduled_list_free()
{
    while (scheduled_list_read_index != scheduled_list_write_index)
    {
        synth_node* node = SCHEDULED_LIST_POP();
        if (node != NULL)
            free_synth(node);
    }

    free(scheduled_node_list);
    scheduled_node_list = NULL;
    scheduled_list_read_index = 0;
    scheduled_list_write_index = 0;
}

// Simple insertion sort. Accounts for ring buffer array wrapping using bit masking and integer overflow
// To Do: Add a condition switch for schedule lists with large contents. Insertion sort won't perform well on anything but small lists.
void scheduled_list_sort()
{
    // Make sure our indexes are within bounds
    scheduled_list_read_index = scheduled_list_read_index & FIFO_SIZE_MASK;
    scheduled_list_write_index = scheduled_list_write_index & FIFO_SIZE_MASK;

    if (scheduled_list_read_index == scheduled_list_write_index)
        return;

    uint32_t i, j, k;
    synth_node* x;
    jack_time_t xTime, yTime;

    for (i = (scheduled_list_read_index + 1) & FIFO_SIZE_MASK; i != scheduled_list_write_index; i = (i + 1) & FIFO_SIZE_MASK)
    {
        x = scheduled_node_list[i];
        xTime = x->time;
        j = i;

        while (j != scheduled_list_read_index)
        {
            k = (j - 1) & FIFO_SIZE_MASK;
            yTime = scheduled_node_list[k]->time;
            if (yTime < xTime)
                break;

            scheduled_node_list[j] = scheduled_node_list[k];
            j = (j - 1) & FIFO_SIZE_MASK;
        }

        scheduled_node_list[j] = x;
    }
}

///////////////////////
// Removal FIFO
///////////////////////

// List used by ugens to queue for removal during RT runtime

const uint32_t MAX_REMOVAL_IDS = 512; // Max number of ids able to be scheduled for removal *per sample frame*
const uint32_t REMOVAL_FIFO_SIZE_MASK = 511;

typedef synth_node** node_fifo;

node_fifo removal_fifo = NULL;
uint32_t removal_fifo_read_index = 0;
uint32_t removal_fifo_write_index = 0;
int32_t removal_fifo_size = 0;

#define REMOVAL_FIFO_PUSH(id) FIFO_PUSH(removal_fifo, removal_fifo_write_index, id, REMOVAL_FIFO_SIZE_MASK)
#define REMOVAL_FIFO_POP() FIFO_POP(removal_fifo, removal_fifo_read_index, REMOVAL_FIFO_SIZE_MASK)

// Allocate and null initialize a node list to be used as a node_list
node_fifo new_removal_fifo()
{
    uint32_t byte_size = sizeof(synth_node*) * MAX_REMOVAL_IDS;
    node_fifo fifo = (node_fifo) malloc(byte_size);
    assert(fifo);
    memset(fifo, 0, byte_size);
    removal_fifo_size = 0;

    return fifo;
}

void removal_fifo_free()
{
    assert(removal_fifo);
    free(removal_fifo);
    removal_fifo = NULL;
    removal_fifo_size = 0;

    removal_fifo_read_index = 0;
    removal_fifo_write_index = 0;
}

///////////////////////////
// Hash Table
///////////////////////////

// Fixed memory hash table using open Addressing with linear probing
// This is not thread safe.

hash_table hash_table_new()
{
    uint32_t byte_size = NODE_POINTER_SIZE * MAX_SYNTHS;
    hash_table table = (hash_table) malloc(byte_size);
    assert(table);
    memset(table, 0, byte_size);

    return table;
}

void hash_table_free(hash_table table)
{
    uint32_t i;
    for (i = 0; i < MAX_SYNTHS; ++i)
    {
        synth_node* node = table[i];
        if (node != NULL)
            free_synth(node);
    }

    free(table);
}

void hash_table_insert(hash_table table, synth_node* node)
{
    uint32_t slot = node->hash & HASH_TABLE_SIZE_MASK;

    while (table[slot] != NULL)
        slot = (slot + 1) & HASH_TABLE_SIZE_MASK;

    table[slot] = node;
    node->table_index = slot;
}

bool hash_table_remove(hash_table table, synth_node* node)
{
    uint32_t index = node->table_index;
    synth_node* found_node = table[index];
    if (found_node == node)
    {
        table[index] = NULL;
        return true;
    }

    else
    {
        printf("hash_table_remove: found_node %p != node %p", found_node, node);
        return false;
    }
}

synth_node* hash_table_lookup(hash_table table, uint32_t key)
{
    uint32_t hash = HASH_KEY(key);
    uint32_t slot = hash & HASH_TABLE_SIZE_MASK;
    uint32_t i = 0;

    while (i < MAX_SYNTHS)
    {
        if (table[slot] != NULL)
        {
            if (table[slot]->key == key)
                return table[slot];
            else
                printf("Found synth node in hash_table_lookup, but not the one we're after. Looking up node ID %u but found %u.", key, table[slot]->key);
        }

        ++i;
        slot = (slot + 1) & HASH_TABLE_SIZE_MASK;
    }

    return NULL;
}

///////////////////////////
// Doubly Linked List
///////////////////////////

typedef synth_node* doubly_linked_list;
doubly_linked_list synth_list = NULL;

// Pushes nodes to the front of the list, returning the new list head
doubly_linked_list doubly_linked_list_push(doubly_linked_list list, synth_node* node)
{
    if (node == NULL)
        return list;

    node->previous = NULL;
    node->next = list;

    if (list)
        list->previous = node;

    return node;
}

// removes nodes from the doubly linked list, returning the new list head
doubly_linked_list doubly_linked_list_remove(doubly_linked_list list, synth_node* node)
{
    if (node == NULL)
        return list;

    synth_node* previous = node->previous;
    synth_node* next = node->next;

    if (previous != NULL)
        previous->next = next;

    if (next != NULL)
        next->previous = previous;

    node->previous = NULL;
    node->next = NULL;

    if (node == list) // If the node was the head of the list, return the next node as the head of the list
        return next;
    else
        return list; // Otherwise just return the current head of the list
}

void doubly_linked_list_free(doubly_linked_list list)
{
    while (list != NULL)
    {
        synth_node* next = list->next;
        free_synth(list);
        list = next;
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// RT thread Synth Node Handling
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

bool necronomicon_running = false;

static inline void clear_necronomicon_buses()
{
    memset(_necronomicon_buses, 0, num_audio_buses_bytes);
}

int32_t get_running()
{
    return (necronomicon_running == true);
}

uint32_t get_block_size()
{
    return BLOCK_SIZE;
}

void print_synth_list()
{
    uint32_t i = 0;
    puts("{{{print_synth_list");
    synth_node* current_node = synth_list;
    while(current_node != NULL)
    {
        print_node(current_node);
        current_node = current_node->next;
        ++i;
    }

    printf("print_synth_list i = %u, num_synths = %u }}}\n", i, num_synths);
}

static inline void add_synth(synth_node* node)
{
    if (node != NULL)
    {
        node->previous_alive_status = node->alive_status;
        if (node->alive_status == NODE_SPAWNING)
        {
            node->alive_status = NODE_ALIVE;
            synth_list = doubly_linked_list_push(synth_list, node);
        }
        // Scheduled to be freed before add message was handled
        else if (node->alive_status == NODE_SCHEDULED_FOR_REMOVAL)
        {
            node->alive_status = NODE_SCHEDULED_FOR_FREE;
            message msg;
            msg.arg.node = node;
            msg.type = FREE_SYNTH;
            NRT_FIFO_PUSH(msg); // Send ugen to NRT thread for freeign
        }

        else if (node->alive_status == NODE_ALIVE)
        {
            // print_node_alive_status(node);
            message msg;
            msg.arg.string = "Warning: add_synth() was given a node that is already playing.";
            msg.type = PRINT;
            NRT_FIFO_PUSH(msg);
        } // node->alive_status == NODE_SCHEDULED_FOR_FREE || NODE_DEAD means remove_scheduled_synths() handled it before add_synth() could
    }
}

static inline void remove_synth(synth_node* node)
{
    if ((node != NULL) && (node->alive_status == NODE_SCHEDULED_FOR_REMOVAL))
    {
        if (node->previous_alive_status == NODE_ALIVE)
        {
            synth_list = doubly_linked_list_remove(synth_list, node);
        }

        node->previous_alive_status = node->alive_status;
        node->alive_status = NODE_SCHEDULED_FOR_FREE;

        message msg;
        msg.arg.node = node;
        msg.type = FREE_SYNTH;
        NRT_FIFO_PUSH(msg); // Send ugen to NRT thread for freeing
    }

    else
    {
        print_node_alive_status(node);
        message msg;
        msg.arg.string = "warning: remove_synth() was given a node that is null or alive_status is not NODE_SCHEDULED_FOR_REMOVAL";
        msg.type = PRINT;
        NRT_FIFO_PUSH(msg);
    }
}

// Iterate over the scheduled list and add synths if they are ready. Stop as soon as we find a synth that isn't ready.
static inline void add_scheduled_synths()
{
    scheduled_list_read_index = scheduled_list_read_index & FIFO_SIZE_MASK;
    scheduled_list_write_index = scheduled_list_write_index & FIFO_SIZE_MASK;
    jack_time_t lookahead_usecs = current_cycle_usecs + BLOCK_SIZE_USECS;
    while (scheduled_list_read_index != scheduled_list_write_index)
    {
        synth_node* node = SCHEDULED_LIST_PEEK();

        if (node != NULL && node->time > lookahead_usecs)
            return;

        SCHEDULED_LIST_POP(); // Commit pop off the scheduled node list;
        add_synth(node);
        scheduled_list_read_index = scheduled_list_read_index & FIFO_SIZE_MASK;
        scheduled_list_write_index = scheduled_list_write_index & FIFO_SIZE_MASK;
    }
}

// Iterate over the removal fifo and remove all synths in it.
static inline void remove_scheduled_synths()
{
    removal_fifo_read_index = removal_fifo_read_index & REMOVAL_FIFO_SIZE_MASK;
    removal_fifo_write_index = removal_fifo_write_index & REMOVAL_FIFO_SIZE_MASK;

    while (removal_fifo_read_index != removal_fifo_write_index)
    {
        synth_node* node = REMOVAL_FIFO_POP();
        --removal_fifo_size;
        remove_synth(node);
        removal_fifo_read_index = removal_fifo_read_index & REMOVAL_FIFO_SIZE_MASK;
        removal_fifo_write_index = removal_fifo_write_index & REMOVAL_FIFO_SIZE_MASK;
    }
}

static inline void try_schedule_current_synth_for_removal()
{
    if (_necronomicon_current_node && (removal_fifo_size < REMOVAL_FIFO_SIZE_MASK) && _necronomicon_current_node->alive_status == NODE_ALIVE)
    {
        _necronomicon_current_node->previous_alive_status = _necronomicon_current_node->alive_status;
        _necronomicon_current_node->alive_status = NODE_SCHEDULED_FOR_REMOVAL;
        removal_fifo_size = (removal_fifo_size + 1) & REMOVAL_FIFO_SIZE_MASK;
        REMOVAL_FIFO_PUSH(_necronomicon_current_node);
    }
}

void shutdown_rt_runtime(); // Forward declaration

static inline void handle_rt_message(message msg)
{
    switch (msg.type)
    {
    case START_SYNTH:
        SCHEDULED_LIST_PUSH(msg.arg.node);
        break;
    case STOP_SYNTH:
        remove_synth(msg.arg.node);
        break;
    case SHUTDOWN:
        shutdown_rt_runtime();
        break;
    default:
        break;
    }
}

// Copy nodes from the rt_node_fifo into the scheduled_node_list
static inline void handle_messages_in_rt_fifo()
{
    rt_fifo_read_index = rt_fifo_read_index & FIFO_SIZE_MASK;
    rt_fifo_write_index = rt_fifo_write_index & FIFO_SIZE_MASK;
    if (rt_fifo_read_index == rt_fifo_write_index)
        return;

    while (rt_fifo_read_index != rt_fifo_write_index)
    {
        message msg = RT_FIFO_POP();
        handle_rt_message(msg);
        rt_fifo_read_index = rt_fifo_read_index & FIFO_SIZE_MASK;
        rt_fifo_write_index = rt_fifo_write_index & FIFO_SIZE_MASK;
    }

    scheduled_list_sort();
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// NRT thread Synth Node Handling
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void handle_nrt_message(message msg)
{
    switch (msg.type)
    {
    case FREE_SYNTH:
        free_synth(msg.arg.node);
        break;
    case PRINT:
        puts(msg.arg.string);
        break;
    case PRINT_NUMBER:
        printf("poll: %f\n", msg.arg.number);
        break;
    default:
        break;
    }
}

// Handle messages in the NRT fifo queue, including freeing memory and printing messages originating from the RT thread.
void handle_messages_in_nrt_fifo()
{
    nrt_fifo_read_index = nrt_fifo_read_index & FIFO_SIZE_MASK;
    nrt_fifo_write_index = nrt_fifo_write_index & FIFO_SIZE_MASK;
    while (nrt_fifo_read_index != nrt_fifo_write_index)
    {
        message msg = NRT_FIFO_POP();
        handle_nrt_message(msg);
        nrt_fifo_read_index = nrt_fifo_read_index & FIFO_SIZE_MASK;
        nrt_fifo_write_index = nrt_fifo_write_index & FIFO_SIZE_MASK;
    }
}

void play_synth(synth_node* synth_definition, double* arguments, uint32_t num_arguments, uint32_t node_id, jack_time_t time)
{
    if (num_synths < MAX_SYNTHS)
    {
        synth_node* synth = new_synth(synth_definition, arguments, num_arguments, node_id, time);
        ++num_synths;
        hash_table_insert(synth_table, synth);
        message msg;
        msg.arg.node = synth;
        msg.type = START_SYNTH;
        RT_FIFO_PUSH(msg);
    }

    else
    {
        printf("Unable to play synth because the maximum number of synths (%u) are already playing.\n", MAX_SYNTHS);
    }
}

void stop_synth(uint32_t id)
{
    synth_node* node = hash_table_lookup(synth_table, id);
    if ((node != NULL) && (node->alive_status == NODE_SPAWNING || node->alive_status == NODE_ALIVE))
    {
        node->previous_alive_status = node->alive_status;
        node->alive_status = NODE_SCHEDULED_FOR_REMOVAL;
        message msg;
        msg.arg.node = node;
        msg.type = STOP_SYNTH;
        RT_FIFO_PUSH(msg);
    }

    else
    {
        printf("stopSynth: Node ID %u not found. ", id);
        print_node_alive_status(node);
    }
}

// How to handle sample accurate setting? Use FIFO messages?
void send_set_synth_arg(uint32_t id, double argument, uint32_t arg_index)
{
    synth_node* synth = hash_table_lookup(synth_table, id);
    if ((synth != NULL) && (synth->alive_status == NODE_SPAWNING || synth->alive_status == NODE_ALIVE))
    {
        double* wire_buffer = synth->ugen_wires + (arg_index * BLOCK_SIZE);
        *wire_buffer = argument;
    }

    else
    {
        printf("setSynthArg: Node ID %u not found. ", id);
        print_node_alive_status(synth);
    }
}

void send_set_synth_args(uint32_t id, double* arguments, uint32_t num_arguments)
{
    synth_node* synth = hash_table_lookup(synth_table, id);
    if ((synth != NULL) && (synth->alive_status == NODE_SPAWNING || synth->alive_status == NODE_ALIVE))
    {
        double* ugen_wires = synth->ugen_wires;
        uint32_t i;
        for (i = 0; i < num_arguments; ++i)
        {
            double* wire_buffer = ugen_wires + (i * BLOCK_SIZE);
            *wire_buffer = arguments[i];
        }
    }

    else
    {
        printf("setSynthArgs: Node ID %u not found. ", id);
        print_node_alive_status(synth);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// RT Runtime
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

jack_port_t* output_port1;
jack_port_t* output_port2;
jack_client_t* client;

void init_rt_thread()
{
    assert(client);
    assert(necronomicon_running == false);
    assert(synth_table == NULL);
    assert(rt_fifo == NULL);
    assert(scheduled_node_list == NULL);
    assert(synth_list == NULL);
    assert(removal_fifo == NULL);
    assert(_necronomicon_buses == NULL);
    assert(sample_buffer_pools == NULL);
    assert(ugen_graph_pools == NULL);
    assert(ugen_wires_pools == NULL);

    SAMPLE_RATE = jack_get_sample_rate(client);
    RECIP_SAMPLE_RATE = 1.0 / SAMPLE_RATE;
    TABLE_MUL_RECIP_SAMPLE_RATE = (double) TABLE_SIZE / (double) SAMPLE_RATE;
    usecs_per_frame = USECS_PER_SECOND / SAMPLE_RATE;
    recip_usecs_per_frame = ((long double ) 1.0) / ((long double) usecs_per_frame);
    BLOCK_SIZE_USECS = usecs_per_frame * (double) BLOCK_SIZE;
    TWO_PI_TIMES_RECIP_SAMPLE_RATE = TWO_PI * RECIP_SAMPLE_RATE;

    synth_table = hash_table_new();
    rt_fifo = new_message_fifo();
    scheduled_node_list = new_node_list();
    removal_fifo = new_removal_fifo();

    last_audio_bus_index = num_audio_buses - 1;
    num_audio_buses_bytes = num_audio_buses * BLOCK_SIZE * DOUBLE_SIZE;
    _necronomicon_buses = malloc(num_audio_buses_bytes);
    clear_necronomicon_buses();

    sample_buffer_pools = (sample_buffer**) calloc(sizeof(sample_buffer*), NUM_SAMPLE_BUFFER_POOLS);
    ugen_graph_pools = (ugen_graph_pool_node**) calloc(sizeof(ugen_graph_pool_node*), NUM_UGEN_GRAPH_POOLS);
    ugen_wires_pools = (ugen_wires_pool_node**) calloc(sizeof(ugen_wires_pool_node*), NUM_UGEN_WIRES_POOLS);

    initialize_wave_tables();
    // load_audio_files(); To Do: Add this functionality

    out_bus_buffer_index = 0;
    _necronomicon_current_node = NULL;
    assert(nrt_fifo == NULL);
    nrt_fifo = new_message_fifo();
    necronomicon_running = true;
}

void clear_synth_list()
{
    while (synth_list)
    {
        synth_node* node = synth_list;
        synth_list = node->next;
        deconstruct_synth(node);
        free(node);
    }
}

void shutdown_rt_thread()
{
    assert(nrt_fifo != NULL);
    clear_synth_list();
    handle_messages_in_rt_fifo();
    handle_messages_in_nrt_fifo();

    nrt_fifo_free();
    nrt_fifo = NULL;

    assert(synth_table != NULL);
    assert(rt_fifo != NULL);
    assert(scheduled_node_list != NULL);
    assert(removal_fifo != NULL);
    assert(_necronomicon_buses != NULL);
    assert(sample_buffer_pools != NULL);
    assert(ugen_graph_pools != NULL);
    assert(ugen_wires_pools != NULL);

    hash_table_free(synth_table);
    rt_fifo_free();
    scheduled_list_free();
    doubly_linked_list_free(synth_list);
    removal_fifo_free();
    free(_necronomicon_buses);

    uint32_t i;
    for (i = 0; i < NUM_SAMPLE_BUFFER_POOLS; ++i)
    {
        sample_buffer* pooled_buffer = sample_buffer_pools[i];
        while(pooled_buffer != NULL)
        {
            sample_buffer* next_buffer = pooled_buffer->next_sample_buffer;
            free(pooled_buffer->samples);
            free(pooled_buffer);
            pooled_buffer = next_buffer;
        }
    }

    free(sample_buffer_pools);

    for (i = 0; i < NUM_UGEN_GRAPH_POOLS; ++i)
    {
        ugen_graph_pool_node* ugen_graph = ugen_graph_pools[i];
        while(ugen_graph != NULL)
        {
            ugen_graph_pool_node* next_ugen_graph_pool_node = ugen_graph->next_ugen_graph_pool_node;
            free(ugen_graph->ugen_graph);
            free(ugen_graph);
            ugen_graph = next_ugen_graph_pool_node;
        }
    }

    free(ugen_graph_pools);

    for (i = 0; i < NUM_UGEN_WIRES_POOLS; ++i)
    {
        ugen_wires_pool_node* ugen_wires = ugen_wires_pools[i];
        while(ugen_wires != NULL)
        {
            ugen_wires_pool_node* next_ugen_wires_pool_node = ugen_wires->next_ugen_wires_pool_node;
            free(ugen_wires->ugen_wires);
            free(ugen_wires);
            ugen_wires = next_ugen_wires_pool_node;
        }
    }

    free(ugen_wires_pools);

    while (free_synths)
    {
        synth_node* synth = free_synths;
        free_synths = synth->next;
        free(synth);
    }

    synth_table = NULL;
    rt_fifo = NULL;
    _necronomicon_current_node = NULL;
    scheduled_node_list = NULL;
    synth_list = NULL;
    removal_fifo = NULL;
    _necronomicon_buses = NULL;
    necronomicon_running = false;
}

void shutdown_rt_runtime()
{
    puts("Necronomicon audio engine shutting down...");
    puts("...............................................................................................................................................................");
    puts("...............................................................................................................................................................");
    puts("...............................................................................................................................................................");
    puts("...............................................................................................................................................................");
    shutdown_rt_thread();
    jack_client_close(client);
    puts("Necronomicon audio engine shut down.");
}

static void signal_handler(int32_t sig)
{
    shutdown_rt_runtime();
    fprintf(stderr, "signal received, exiting ...\n");
    exit(0);
}

/*
 * JACK calls this shutdown_callback if the server ever shuts down or
 * decides to disconnect the client.
 */
void jack_shutdown(void *arg)
{
    shutdown_rt_runtime();
    exit(1);
}

// Main audio process callback function
int32_t process(jack_nframes_t nframes, void* arg)
{
    jack_get_cycle_times(client, &current_cycle_frames, &current_cycle_usecs, &next_cycle_usecs, &period_usecs); // Update time variables for the current cycle
    // Cache the current_cycle_usecs so we can recalculate current_cycle_usecs without rounding errors from iteration
    jack_time_t cached_cycle_usecs = current_cycle_usecs;

    jack_default_audio_sample_t* out0 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port1, nframes);
    jack_default_audio_sample_t* out1 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port2, nframes);

    handle_messages_in_rt_fifo(); // Handles messages including moving uge_nodes from the RT FIFO queue into the scheduled_synth_list
    clear_necronomicon_buses(); // Zero out the audio buses
    add_scheduled_synths(); // Add any synths that need to start this frame into the current synth_list

    // Iterate through the synth_list, processing each synth
    _necronomicon_current_node = synth_list;


    while (_necronomicon_current_node)
    {
        process_synth(_necronomicon_current_node);
        _necronomicon_current_node = _necronomicon_current_node->next;
    }

    double* _necronomicon_buses_out0 = _necronomicon_buses;
    double* _necronomicon_buses_out1 = _necronomicon_buses + BLOCK_SIZE;

    double* _necronomicon_buses_out2 = _necronomicon_buses + BLOCK_SIZE * 2;
    double* _necronomicon_buses_out3 = _necronomicon_buses + BLOCK_SIZE * 3;

    double* _necronomicon_buses_out4 = _necronomicon_buses + BLOCK_SIZE * 4;
    double* _necronomicon_buses_out5 = _necronomicon_buses + BLOCK_SIZE * 5;

    double* _necronomicon_buses_out6 = _necronomicon_buses + BLOCK_SIZE * 6;
    double* _necronomicon_buses_out7 = _necronomicon_buses + BLOCK_SIZE * 7;

    float* _out_bus_0 = out_bus_buffers[0];
    float* _out_bus_1 = out_bus_buffers[1];
    float* _out_bus_2 = out_bus_buffers[2];
    float* _out_bus_3 = out_bus_buffers[3];
    float* _out_bus_4 = out_bus_buffers[4];
    float* _out_bus_5 = out_bus_buffers[5];
    float* _out_bus_6 = out_bus_buffers[6];
    float* _out_bus_7 = out_bus_buffers[7];

    uint32_t i;
    for (i = 0; i < nframes; ++i)
    {
        out0[i] = _necronomicon_buses_out0[i]; // Write the output of buses 0/1 to jack buffers 0/1, which can be routed or sent directly to the main outs
        out1[i] = _necronomicon_buses_out1[i];

        //Hand un-rolled this loop. The non-unrolled version was causing 13% cpu overhead on my machine, this doesn't make a blip...
        _out_bus_0[out_bus_buffer_index] = _necronomicon_buses_out0[i];
        _out_bus_1[out_bus_buffer_index] = _necronomicon_buses_out1[i];
        _out_bus_2[out_bus_buffer_index] = _necronomicon_buses_out2[i];
        _out_bus_3[out_bus_buffer_index] = _necronomicon_buses_out3[i];
        _out_bus_4[out_bus_buffer_index] = _necronomicon_buses_out4[i];
        _out_bus_5[out_bus_buffer_index] = _necronomicon_buses_out5[i];
        _out_bus_6[out_bus_buffer_index] = _necronomicon_buses_out6[i];
        _out_bus_7[out_bus_buffer_index] = _necronomicon_buses_out7[i];
        out_bus_buffer_index = (out_bus_buffer_index + 1) & 511;
    }

    remove_scheduled_synths(); // Remove any synths that are scheduled for removal and send them to the NRT thread FIFO queue for freeing.
    current_cycle_usecs = cached_cycle_usecs + (jack_time_t) ((double) nframes * usecs_per_frame); // update current usecs time every sample
    return 0;
}

const int8_t* RESOUCES_PATH;
void start_rt_runtime(const int8_t* resources_path)
{
    puts("Necronomicon audio engine booting");
    RESOUCES_PATH = resources_path;

    const char** ports;
    const int8_t* client_name = "Necronomicon";
    const int8_t* server_name = NULL;
    jack_options_t options = JackNullOption;
    jack_status_t status;

    /* open a client connection to the JACK server */

    client = jack_client_open(client_name, options, &status, server_name);
    if (client == NULL)
    {
        fprintf(stderr, "jack_client_open() failed, status = 0x%2.0x\n", status);

        if (status & JackServerFailed)
            fprintf(stderr, "Unable to connect to JACK server\n");

        exit (1);
    }

    BLOCK_SIZE = jack_get_buffer_size(client); // Maximum buffer size
    init_rt_thread();

    if (status & JackServerStarted)
        fprintf (stderr, "JACK server started\n");

    if (status & JackNameNotUnique)
    {
        client_name = jack_get_client_name(client);
        fprintf(stderr, "unique name `%s' assigned\n", client_name);
    }

    /* tell the JACK server to call `process()' whenever
       there is work to be done.
    */

    jack_set_process_callback(client, process, 0);

    /* tell the JACK server to call `jack_shutdown()' if
       it ever shuts down, either entirely, or if it
       just decides to stop calling us.
    */

    jack_on_shutdown(client, jack_shutdown, 0);

    /* create two ports */

    output_port1 = jack_port_register(client, "output1", JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
    output_port2 = jack_port_register(client, "output2", JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);

    if ((output_port1 == NULL) || (output_port2 == NULL))
    {
        fprintf(stderr, "no more JACK ports available\n");
        exit (1);
    }

    /* Tell the JACK server that we are ready to roll.  Our
     * process() callback will start running now. */

    if (jack_activate(client))
    {
        fprintf(stderr, "cannot activate client");
        exit (1);
    }

    /* Connect the ports.  You can't do this before the client is
     * activated, because we can't make connections to clients
     * that aren't running.  Note the confusing (but necessary)
     * orientation of the driver backend ports: playback ports are
     * "input" to the backend, and capture ports are "output" from
     * it.
     */

    ports = jack_get_ports(client, NULL, NULL, JackPortIsPhysical | JackPortIsInput);
    if (ports == NULL)
    {
        fprintf(stderr, "no physical playback ports\n");
        exit (1);
    }

    if (jack_connect(client, jack_port_name(output_port1), ports[0]))
        fprintf (stderr, "cannot connect output ports\n");

    if (jack_connect(client, jack_port_name(output_port2), ports[1]))
        fprintf (stderr, "cannot connect output ports\n");

    jack_free(ports);
    puts("Necronomicon audio engine booted.");
}

void shutdown_necronomicon()
{
    message msg = { NULL, SHUTDOWN };
    RT_FIFO_PUSH(msg);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// UGens
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define MAX( a, b ) ( ( a > b) ? a : b )
#define MIN( a, b ) ( ( a < b) ? a : b )

#define LINEAR_INTERP(A, B, DELTA) (A + DELTA * (B - A))

#define fast_pow(U,BASE,EXPONENT)                                   \
U.d = BASE;                                                         \
U.x[1] = (int32_t)(EXPONENT * (U.x[1] - 1072632447) + 1072632447);  \
U.x[0] = 0;                                                         \
U.d;                                                                \

#define AMP2DB(amp) (log10(amp * 20.0))
#define DB2AMP(db) (pow(10, db / 20.0))

#define LAGRANGE_2ND_ORDER(x0, x1, y0, y1, x) ((x - x1) / (x0 - x1) * y0 + (x - x0) / (x1 - x0) * y1)

/*
#define CUBIC_INTERP(A, B, C, D, DELTA)                 \
({                                                      \
    const double delta2 = DELTA * DELTA;                \
    const double a0     = D - C - A + B;                \
    const double a1     = A - B - a0;                   \
    const double a2     = C - A;                        \
    a0 * DELTA * delta2 + a1 * delta2 + a2 * DELTA + B; \
})*/


#define CUBIC_INTERP(y0, y1, y2, y3, x)                     \
({                                                          \
    const double c0 = y1;                                   \
    const double c1 = 0.5 * (y2 - y0);                      \
    const double c2 = y0 - 2.5 * y1 + 2.0 * y2 - 0.5 * y3;  \
    const double c3 = 0.5 * (y3 - y0) + 1.5 * (y1 - y2);    \
    ((c3 * x + c2) * x + c1) * x + c0;                      \
})

#define BIN_OP_CALC(OP, CONTROL_ARGS, AUDIO_ARGS)   \
double* in0 = UGEN_INPUT_BUFFER(u, 0);              \
double* in1 = UGEN_INPUT_BUFFER(u, 1);              \
double* out = UGEN_OUTPUT_BUFFER(u, 0);             \
double a,b;                                         \
CONTROL_ARGS                                        \
AUDIO_LOOP(                                         \
    AUDIO_ARGS                                      \
    UGEN_OUT(out, a OP b);                          \
);

#define BIN_FUNC_CALC(FUNC, CONTROL_ARGS, AUDIO_ARGS)   \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                  \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                  \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                 \
double a,b;                                             \
CONTROL_ARGS                                            \
AUDIO_LOOP(                                             \
    AUDIO_ARGS                                          \
    UGEN_OUT(out, FUNC(a, b));                          \
);

void add_aa_calc(ugen u)
{
    BIN_OP_CALC(
        +,
        /* no control args */,
        a = UGEN_IN(in0); // Audio args
        b = UGEN_IN(in1);
    );
}

void add_ak_calc(ugen u)
{
    BIN_OP_CALC(
        +,
        b = in1[0];, // Control arg
        a = UGEN_IN(in0); // Audio arg
    );
}

void add_ka_calc(ugen u)
{
    BIN_OP_CALC(
        +,
        a = in0[0];, // Control arg
        b = UGEN_IN(in1); // Audio arg
    );
}

void add_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double a = in0[0];
    double b = in1[0];
    *out = a + b;
}

void minus_aa_calc(ugen u)
{
    BIN_OP_CALC(
        -,
        /* no control args */,
        a = UGEN_IN(in0); // Audio args
        b = UGEN_IN(in1);
    );
}

void minus_ak_calc(ugen u)
{
    BIN_OP_CALC(
        -,
        b = in1[0];, // Control arg
        a = UGEN_IN(in0); // Audio arg
    );
}

void minus_ka_calc(ugen u)
{
    BIN_OP_CALC(
        -,
        a = in0[0];, // Control arg
        b = UGEN_IN(in1); // Audio arg
    );
}

void minus_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double a = in0[0];
    double b = in1[0];
    *out = a - b;
}

void mul_aa_calc(ugen u)
{
    BIN_OP_CALC(
        *,
        /* no control args */,
        a = UGEN_IN(in0); // Audio args
        b = UGEN_IN(in1);
    );
}

void mul_ak_calc(ugen u)
{
    BIN_OP_CALC(
        *,
        b = in1[0];, // Control arg
        a = UGEN_IN(in0); // Audio arg
    );
}

void mul_ka_calc(ugen u)
{
    BIN_OP_CALC(
        *,
        a = in0[0];, // Control arg
        b = UGEN_IN(in1); // Audio arg
    );
}

void mul_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double a = in0[0];
    double b = in1[0];
    *out = a * b;
}

#define DIV_CALC(CONTROL_ARGS, AUDIO_ARGS)  \
double* in0 = UGEN_INPUT_BUFFER(u, 0);      \
double* in1 = UGEN_INPUT_BUFFER(u, 1);      \
double* out = UGEN_OUTPUT_BUFFER(u, 0);     \
double a,b;                                 \
CONTROL_ARGS                                \
AUDIO_LOOP(                                 \
    AUDIO_ARGS                              \
    if (b == 0)                             \
        UGEN_OUT(out, 0);                   \
    else                                    \
        UGEN_OUT(out, a / b);               \
);                                          \

void div_aa_calc(ugen u)
{
    DIV_CALC(
        /* no control args */,
        a = UGEN_IN(in0); // Audio args
        b = UGEN_IN(in1);
    );
}

void div_ka_calc(ugen u)
{
    DIV_CALC(
        a = in0[0];, // Control args
        b = UGEN_IN(in1); // Audio args
    );
}

void div_ak_calc(ugen u)
{
    DIV_CALC(
        b = in1[0];, // Control args
        a = UGEN_IN(in0); // Audio args
    );
}

void div_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double a = in0[0];
    double b = in1[0];
    *out = b != 0 ? (a / b) : 0;
}

void abs_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, fabs(UGEN_IN(in)));
    );
}

void abs_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = fabs(in[0]);
}

#define SIGNUM_CALC()   \
if (value > 0)          \
    value = 1;          \
else if (value < 0)     \
    value = -1;         \
else                    \
    value = 0;          \

void signum_a_calc(ugen u)
{
    double* in  = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double value;
    AUDIO_LOOP(
        value = UGEN_IN(in);
        SIGNUM_CALC();
        UGEN_OUT(out, value);
    );
}

void signum_k_calc(ugen u)
{
    double* in  = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double value = in[0];
    SIGNUM_CALC();
    *out = value;
}

void negate_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, -(UGEN_IN(in)));
    );
}

void negate_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = -in[0];
}

void pow_aa_calc(ugen u)
{
    BIN_FUNC_CALC(
        pow,
        /* no control args */,
        a = UGEN_IN(in0);
        b = UGEN_IN(in1); // Audio args
    );
}

void pow_ak_calc(ugen u)
{
    BIN_FUNC_CALC(
        pow,
        b = in1[0];, // Control args
        a = UGEN_IN(in0); // Audio args
    );
}

void pow_ka_calc(ugen u)
{
    BIN_FUNC_CALC(
        pow,
        a = in0[0];, // Control args
        b = UGEN_IN(in1); // Audio args
    );
}

void pow_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = pow(in0[0], in1[0]);
}

void exp_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, exp(UGEN_IN(in)));
    );
}

void exp_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = exp(in[0]);
}

void log_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, log(UGEN_IN(in)));
    );
}

void log_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = log(in[0]);
}

void cos_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, cos(UGEN_IN(in)));
    );
}

void cos_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = cos(in[0]);
}

void asin_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, asin(UGEN_IN(in)));
    );
}

void asin_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = asin(in[0]);
}

void acos_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, acos(UGEN_IN(in)));
    );
}

void acos_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = acos(in[0]);
}

void atan_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, atan(UGEN_IN(in)));
    );
}

void atan_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = atan(in[0]);
}

void logbase_aa_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, log(UGEN_IN(in0)) / log(UGEN_IN(in1)));
    );
}

void logbase_ka_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    const double a = log(in0[0]);
    AUDIO_LOOP(
        UGEN_OUT(out, a / log(UGEN_IN(in1)));
    );
}

void logbase_ak_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    const double b = log(in1[0]);
    AUDIO_LOOP(
        UGEN_OUT(out, log(UGEN_IN(in0)) / b);
    );
}

void logbase_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    const double a = log(in0[0]);
    const double b = log(in1[0]);
    *out = a / b;
}

void sqrt_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, sqrt(UGEN_IN(in)));
    );
}

void sqrt_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = sqrt(in[0]);
}

void tan_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, tan(UGEN_IN(in)));
    );
}

void tan_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = tan(in[0]);
}

void sinh_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, sinh(UGEN_IN(in)));
    );
}

void sinh_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = sinh(in[0]);
}

void cosh_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, cosh(UGEN_IN(in)));
    );
}

void cosh_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = cosh(in[0]);
}

void tanh_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, tanh(UGEN_IN(in)));
    );
}

void tanh_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = tanh(in[0]);
}

void asinh_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, asinh(UGEN_IN(in)));
    );
}

void asinh_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = asinh(in[0]);
}

void atanh_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, atanh(UGEN_IN(in)));
    );
}

void atanh_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = atanh(in[0]);
}

void acosh_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, acosh(UGEN_IN(in)));
    );
}

void acosh_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = acosh(in[0]);
}


// max

#define UMAX_CALC(CONTROL_ARGS, AUDIO_ARGS)     \
double* in0 = UGEN_INPUT_BUFFER(u, 0);          \
double* in1 = UGEN_INPUT_BUFFER(u, 1);          \
double* out = UGEN_OUTPUT_BUFFER(u, 0);         \
double a;                                       \
double b;                                       \
CONTROL_ARGS                                    \
AUDIO_LOOP(                                     \
    AUDIO_ARGS                                  \
    UGEN_OUT(out, fmax(a, b));                  \
);


#define UMAX_AK a = *in0;
#define UMAX_AA a = UGEN_IN(in0);
#define UMAX_BK b = *in1;
#define UMAX_BA b = UGEN_IN(in1);


// 0
__attribute__((flatten)) void umax_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = fmax(*in0, *in1);
}

// 1
__attribute__((flatten)) void umax_ak_calc(ugen u)
{
    UMAX_CALC(
        // Control Arguments
        UMAX_BK               /* 1 */,
        // Audio Arguments
        UMAX_AA               /* 0 */
    )
}

// 2
__attribute__((flatten)) void umax_ka_calc(ugen u)
{
    UMAX_CALC(
        // Control Arguments
        UMAX_AK               /* 0 */,
        // Audio Arguments
        UMAX_BA               /* 1 */
    )
}

// 3
__attribute__((flatten)) void umax_aa_calc(ugen u)
{
    UMAX_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        UMAX_AA               /* 0 */
        UMAX_BA               /* 1 */
    )
}

// min

#define UMIN_CALC(CONTROL_ARGS, AUDIO_ARGS)     \
double* in0 = UGEN_INPUT_BUFFER(u, 0);          \
double* in1 = UGEN_INPUT_BUFFER(u, 1);          \
double* out = UGEN_OUTPUT_BUFFER(u, 0);         \
double a;                                       \
double b;                                       \
CONTROL_ARGS                                    \
AUDIO_LOOP(                                     \
    AUDIO_ARGS                                  \
    UGEN_OUT(out, fmin(a, b));                  \
);


#define UMIN_AK a = *in0;
#define UMIN_AA a = UGEN_IN(in0);
#define UMIN_BK b = *in1;
#define UMIN_BA b = UGEN_IN(in1);


// 0
__attribute__((flatten)) void umin_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = fmin(*in0, *in1);
}

// 1
__attribute__((flatten)) void umin_ak_calc(ugen u)
{
    UMIN_CALC(
        // Control Arguments
        UMIN_BK               /* 1 */,
        // Audio Arguments
        UMIN_AA               /* 0 */
    )
}

// 2
__attribute__((flatten)) void umin_ka_calc(ugen u)
{
    UMIN_CALC(
        // Control Arguments
        UMIN_AK               /* 0 */,
        // Audio Arguments
        UMIN_BA               /* 1 */
    )
}

// 3
__attribute__((flatten)) void umin_aa_calc(ugen u)
{
    UMIN_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        UMIN_AA               /* 0 */
        UMIN_BA               /* 1 */
    )
}

// line

void line_constructor(ugen* u)
{
    u->data = malloc(UINT_SIZE); // Line time
    *((uint32_t*) u->data) = 0;
}

void line_deconstructor(ugen* u)
{
    free(u->data);
}

#define LINE_CALC(CONTROL_ARGS, AUDIO_ARGS)             \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                  \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                 \
uint32_t line_time = *((uint32_t*) u.data);             \
double length;                                          \
double y;                                               \
bool scheduled_for_removal = false;                     \
CONTROL_ARGS                                            \
AUDIO_LOOP(                                             \
    AUDIO_ARGS                                          \
    if (line_time >= length)                            \
    {                                                   \
        y = 0;                                          \
        scheduled_for_removal =  true;                  \
    }                                                   \
    else                                                \
    {                                                   \
        y = fmax(0, 1 - (line_time / length));          \
        ++line_time;                                    \
    };                                                  \
    UGEN_OUT(out, y);                                   \
);                                                      \
if (scheduled_for_removal)                              \
    try_schedule_current_synth_for_removal();           \
*((uint32_t*) u.data) = line_time;

void line_k_calc(ugen u)
{
    LINE_CALC(
        length = in0[0] * SAMPLE_RATE;,
        /* no audio args */
    );
}

void line_a_calc(ugen u)
{
    LINE_CALC(
        /* no control args */,
        length = UGEN_IN(in0) * SAMPLE_RATE;
    );
}

#define OFF_INDEX(OFFSET,INDEX,ARRAY)

typedef struct
{
    double time;
    double curTotalDuration;
    double recipDuration;
    double curve;
    double nextValue;
    double currentValue;
    double nextTotalDuration;
    int32_t index;
    int32_t numValues;
    int32_t    maxIndex;
    int32_t numDurations;

} env_struct;

void env_constructor(ugen* u)
{
    env_struct* data        = malloc(sizeof(env_struct));
    data->time              = 0;
    data->index             = -1;
    data->numValues         = u->constructor_args[0];
    data->maxIndex          = fmax(0, data->numValues - 1);
    data->numDurations      = u->constructor_args[1];
    data->curTotalDuration  = -1;
    data->nextTotalDuration = -1;
    data->recipDuration     = 1;
    data->currentValue      = 0;
    data->nextValue         = 0;
    data->curve             = 0;
    u->data                 = data;
}

void env_deconstructor(ugen* u)
{
    free(u->data);
}

#define ENV_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                                          \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                                                                              \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                                                             \
env_struct   data  = *((env_struct*) u.data);                                                                       \
double x;                                                                                                           \
double curve = UGEN_INPUT_BUFFER(u, 0)[0];                                                                          \
const int32_t maxIndex = data.maxIndex;                                                                             \
bool scheduled_for_removal = false;                                                                                 \
union {                                                                                                             \
    double d;                                                                                                       \
    int32_t x[2];                                                                                                   \
} ud = { 0 };                                                                                                       \
CONTROL_ARGS                                                                                                        \
AUDIO_LOOP(                                                                                                         \
    AUDIO_ARGS                                                                                                      \
    if (data.time >= data.nextTotalDuration)                                                                        \
    {                                                                                                               \
        data.index = data.index + 1;                                                                                \
        if (data.index >= maxIndex)                                                                                 \
        {                                                                                                           \
            scheduled_for_removal = true;                                                                           \
            UGEN_OUT(out,data.nextValue);                                                                           \
            continue;                                                                                               \
        }                                                                                                           \
        else if (data.index < maxIndex)                                                                             \
        {                                                                                                           \
            int32_t dursOffset = 2 + data.numValues;                                                                \
            double nextDuration = UGEN_IN(UGEN_INPUT_BUFFER(u, (data.index % data.numValues) + dursOffset));        \
                                                                                                                    \
            if (data.nextTotalDuration < 0)                                                                         \
                data.curTotalDuration = 0;                                                                          \
            else                                                                                                    \
                data.curTotalDuration = data.nextTotalDuration;                                                     \
                                                                                                                    \
            data.nextTotalDuration = data.curTotalDuration + nextDuration;                                          \
            data.currentValue      = UGEN_IN(UGEN_INPUT_BUFFER(u, MIN(data.index, maxIndex) + 2));                  \
            data.nextValue         = UGEN_IN(UGEN_INPUT_BUFFER(u, MIN(data.index + 1, maxIndex) + 2));              \
                                                                                                                    \
            if (nextDuration == 0.0)                                                                                \
                data.recipDuration = 0.0;                                                                           \
            else                                                                                                    \
                data.recipDuration = 1.0 / nextDuration;                                                            \
                                                                                                                    \
            if (curve < 0)                                                                                          \
                data.curve = 1 / ((curve * -1) + 1);                                                                \
            else                                                                                                    \
                data.curve = curve + 1;                                                                             \
        }                                                                                                           \
    }                                                                                                               \
                                                                                                                    \
    double delta = fast_pow(ud, (data.time - data.curTotalDuration) * data.recipDuration, data.curve);              \
    UGEN_OUT(out,LERP(data.currentValue, data.nextValue, delta) * x);                                               \
    data.time   += RECIP_SAMPLE_RATE;                                                                               \
);                                                                                                                  \
if (scheduled_for_removal == true)                                                                                  \
    try_schedule_current_synth_for_removal();                                                                       \
*((env_struct*) u.data) = data;

void env_k_calc(ugen u)
{
    ENV_CALC(
        x = in1[0];,
        /* no audio args */
    )
}

void env_a_calc(ugen u)
{
    ENV_CALC(
        /* no control args */,
        x = UGEN_IN(in1);
    )
}

#define ENV2_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                                         \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                                                                              \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                                                             \
double x;                                                                                                           \
env_struct data  = *((env_struct*) u.data);                                                                         \
double curve = UGEN_INPUT_BUFFER(u, 0)[0];                                                                          \
const int32_t maxIndex = data.maxIndex;                                                                             \
union {                                                                                                             \
    double d;                                                                                                       \
    int32_t x[2];                                                                                                   \
} ud = { 0 };                                                                                                       \
CONTROL_ARGS                                                                                                        \
AUDIO_LOOP(                                                                                                         \
    AUDIO_ARGS                                                                                                      \
    if (data.time >= data.nextTotalDuration)                                                                        \
    {                                                                                                               \
        data.index = data.index + 1;                                                                                \
        if (data.index >= maxIndex)                                                                                 \
        {                                                                                                           \
            UGEN_OUT(out,data.nextValue);                                                                           \
            continue;                                                                                               \
        }                                                                                                           \
        else if (data.index < maxIndex)                                                                             \
        {                                                                                                           \
            int32_t dursOffset = 2 + data.numValues;                                                                \
            double nextDuration = *UGEN_INPUT_BUFFER(u, (data.index % data.numValues) + dursOffset);                \
                                                                                                                    \
            if (data.nextTotalDuration < 0)                                                                         \
                data.curTotalDuration = 0;                                                                          \
            else                                                                                                    \
                data.curTotalDuration = data.nextTotalDuration;                                                     \
                                                                                                                    \
            data.nextTotalDuration = data.curTotalDuration + nextDuration;                                          \
            data.currentValue      = *UGEN_INPUT_BUFFER(u, MIN(data.index, maxIndex) + 2);                          \
            data.nextValue         = *UGEN_INPUT_BUFFER(u, MIN(data.index + 1, maxIndex) + 2);                      \
                                                                                                                    \
            if (nextDuration == 0.0)                                                                                \
                data.recipDuration = 0.0;                                                                           \
            else                                                                                                    \
                data.recipDuration = 1.0 / nextDuration;                                                            \
                                                                                                                    \
            if (curve < 0)                                                                                          \
                data.curve = 1 / ((curve * -1) + 1);                                                                \
            else                                                                                                    \
                data.curve = curve + 1;                                                                             \
        }                                                                                                           \
    }                                                                                                               \
                                                                                                                    \
    double delta = fast_pow(ud, (data.time - data.curTotalDuration) * data.recipDuration, data.curve);              \
    UGEN_OUT(out,LERP(data.currentValue, data.nextValue, delta) * x);                                               \
    data.time   += RECIP_SAMPLE_RATE;                                                                               \
);                                                                                                                  \
*((env_struct*) u.data) = data;                                                                                     \

void env2_k_calc(ugen u)
{
    ENV2_CALC(
        x = in1[0];,
        /* no audio args */
    )
}

void env2_a_calc(ugen u)
{
    ENV2_CALC(
        /* no control args */,
        x = UGEN_IN(in1);
    )
}

void sin_constructor(ugen* u)
{
    u->data = malloc(DOUBLE_SIZE); // Phase accumulator
    *((double*) u->data) = 0;
}

void sin_deconstructor(ugen* u)
{
    free(u->data);
}

#define SIN_CALC(CONTROL_ARGS, AUDIO_ARGS)                              \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                  \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                 \
                                                                        \
double phase = *((double*) u.data);                                     \
double freq;                                                            \
uint16_t index1;                                                        \
uint16_t index2;                                                        \
double v1;                                                              \
double v2;                                                              \
double delta, phase_increment;                                          \
                                                                        \
CONTROL_ARGS                                                            \
                                                                        \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
    index1 = phase;                                                     \
    index2 = index1 + 1;                                                \
    v1     = sine_table[index1];                                        \
    v2     = sine_table[index2];                                        \
    delta  = phase - ((int64_t) phase);                                 \
    UGEN_OUT(out, LERP(v1,v2,delta));                                   \
    phase += phase_increment;                                           \
    if (phase > DOUBLE_TABLE_SIZE)                                      \
        phase = phase - DOUBLE_TABLE_SIZE;                              \
);                                                                      \
*((double*) u.data) = phase;

#define mmsin_a0  1.0
#define mmsin_a1 -1.666666666640169148537065260055e-1
#define mmsin_a2  8.333333316490113523036717102793e-3
#define mmsin_a3 -1.984126600659171392655484413285e-4
#define mmsin_a4  2.755690114917374804474016589137e-6
#define mmsin_a5 -2.502845227292692953118686710787e-8
#define mmsin_a6  1.538730635926417598443354215485e-10

#define MINIMAXSIN(X)                                                                                                       \
x2 = X * X;                                                                                                                 \
X * (mmsin_a0 + x2 * (mmsin_a1 + x2 * (mmsin_a2 + x2 * (mmsin_a3 + x2 * (mmsin_a4 + x2 * (mmsin_a5 + x2 * mmsin_a6))))));   \

#define SIN_PHASE_INCREMENT phase_increment = TABLE_MUL_RECIP_SAMPLE_RATE * freq;

void sin_a_calc(ugen u)
{
    SIN_CALC(
        /*no control args*/, // Control Args
        freq = UGEN_IN(in0); SIN_PHASE_INCREMENT // Audio Args
    );
}

void sin_k_calc(ugen u)
{
    SIN_CALC(
        freq = in0[0]; SIN_PHASE_INCREMENT,    // Control Args
        /*no audio args*/ // Audio Args
    );
}

void local_out_k_calc(ugen u)
{
    double in = *UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, in);
    );
}

void local_out_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, (UGEN_IN(in)));
    );
}

#define OUT_CALC(CONTROL_CODE, AUDIO_CODE)                  \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                      \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                      \
double x;                                                   \
uint8_t bus_index; /* constrains bus range */               \
uint32_t bus_frame;                                         \
CONTROL_CODE                                                \
AUDIO_LOOP(                                                 \
    AUDIO_CODE                                              \
    _necronomicon_buses[bus_frame + _block_frame] += x;     \
);

void out_aa_calc(ugen u)
{
    OUT_CALC(
        /* NO CONTROL CODE */,
        bus_index = UGEN_IN(in0); // Audio rate code
        bus_frame = bus_index * BLOCK_SIZE;
        x = UGEN_IN(in1);
    )
}

void out_ak_calc(ugen u)
{
    OUT_CALC(
        x = in1[0];, // Control rate code
        bus_index = UGEN_IN(in0); // Audio rate code
        bus_frame = bus_index * BLOCK_SIZE;
    )
}

void out_ka_calc(ugen u)
{
    OUT_CALC(
        bus_index = in0[0]; // Control rate code
        bus_frame = bus_index * BLOCK_SIZE;,
        x = UGEN_IN(in1); // Audio rate code
    )
}

void out_kk_calc(ugen u)
{
    OUT_CALC(
        bus_index = in0[0]; // Control rate code
        bus_frame = bus_index * BLOCK_SIZE;
        x = in1[0];,
        /* no audio rate code */
    )
}

#define IN_CALC(CONTROL_CODE, AUDIO_CODE)                                   \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                      \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                     \
uint8_t bus_index; /* constrains bus range */                               \
uint32_t bus_frame;                                                         \
CONTROL_CODE                                                                \
AUDIO_LOOP(                                                                 \
    AUDIO_CODE                                                              \
    UGEN_OUT(out, _necronomicon_buses[bus_frame + _block_frame]);           \
);                                                                          \

void in_a_calc(ugen u)
{
    IN_CALC(
        /* no control rate code */,
        bus_index = UGEN_IN(in0);
        bus_frame = bus_index * BLOCK_SIZE;
    );
}

void in_k_calc(ugen u)
{
    IN_CALC(
        bus_index = in0[0];
        bus_frame = bus_index * BLOCK_SIZE;,
        /* no audio rate code */
    );
}

void poll_constructor(ugen* u)
{
    uint32_t* count_buffer = (uint32_t*) malloc(sizeof(uint32_t));
    *count_buffer = 0;
    u->data = count_buffer;
}

void poll_calc(ugen u)
{
    uint32_t* count_buffer = (uint32_t*) u.data;
    uint32_t count = *count_buffer;
    message msg;
    msg.arg.number = 0;
    msg.type = PRINT_NUMBER;

    if (count >= ((double) SAMPLE_RATE * 0.25))
    {
        msg.arg.number = *UGEN_INPUT_BUFFER(u, 0); // Reads ugens as control rate
        NRT_FIFO_PUSH(msg);
        count = 0;
    }

    else
    {
        count += BLOCK_SIZE;
    }

    *count_buffer = count;
}

void poll_deconstructor(ugen* u)
{
    free(u->data);
}

typedef struct
{
    sample_buffer* buffer;
    double max_delay_time;
    int64_t write_index;
} delay_data;

const uint32_t DELAY_DATA_SIZE = sizeof(delay_data);

void delayN_constructor(ugen* u)
{
    u->data = malloc(DELAY_DATA_SIZE);
    double max_delay_time = fmax(1, u->constructor_args[0] * SAMPLE_RATE);
    delay_data data = { acquire_sample_buffer(max_delay_time), max_delay_time, 0 };
    *((delay_data*) u->data) = data;
}

const uint32_t MAX_DELAYL_OFFSET = 1;

void delayL_constructor(ugen* u)
{
    u->data = malloc(DELAY_DATA_SIZE);
    double max_delay_time = fmax(2, u->constructor_args[0] * SAMPLE_RATE);
    delay_data data = { acquire_sample_buffer(max_delay_time + MAX_DELAYL_OFFSET), fmax(0, max_delay_time), 0 };
    *((delay_data*) u->data) = data;
}

const uint32_t MAX_DELAYC_OFFSET = 2;

void delayC_constructor(ugen* u)
{
    u->data = malloc(DELAY_DATA_SIZE);
    double max_delay_time = fmax(3, u->constructor_args[0] * SAMPLE_RATE);
    delay_data data = { acquire_sample_buffer(max_delay_time + MAX_DELAYC_OFFSET), fmax(0, max_delay_time), 0 };
    *((delay_data*) u->data) = data;
}

void delay_deconstructor(ugen* u)
{
    release_sample_buffer(((delay_data*) u->data)->buffer);
    free(u->data);
}

#define INIT_DELAY(u)                                           \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                          \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                          \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                         \
delay_data data = *((delay_data*) u.data);                      \
sample_buffer buffer = *data.buffer;                            \
int64_t write_index = data.write_index;                         \
uint32_t num_samples_mask = buffer.num_samples_mask;            \
double delay_time;                                              \
double x;                                                       \
double y;                                                       \

#define FINISH_DELAY()                                          \
data.write_index = write_index;                                 \
*((delay_data*) u.data) = data;                                 \

#define DELAYN_CALC(CONTROL_ARGS, AUDIO_ARGS)                                       \
INIT_DELAY(u);                                                                      \
int64_t iread_index;                                                                \
CONTROL_ARGS                                                                        \
AUDIO_LOOP(                                                                         \
    AUDIO_ARGS                                                                      \
    iread_index = write_index - (int64_t) delay_time;                               \
    y = iread_index < 0 ? 0 : buffer.samples[iread_index & num_samples_mask];       \
    buffer.samples[write_index & num_samples_mask] = x;                             \
    ++write_index;                                                                  \
    UGEN_OUT(out, y);                                                               \
);                                                                                  \
FINISH_DELAY();                                                                     \

#define DELAYN_DELAY_TIMEK delay_time = fmin(data.max_delay_time, fmax(1, (*in0) * SAMPLE_RATE));
#define DELAYN_DELAY_TIMEA delay_time = fmin(data.max_delay_time, fmax(1, UGEN_IN(in0) * SAMPLE_RATE));
#define DELAYN_XK x = *in1;
#define DELAYN_XA x = UGEN_IN(in1);

// 0
void delayN_kk_calc(ugen u)
{
    DELAYN_CALC(
        // Control Arguments
        DELAYN_DELAY_TIMEK    /* 0 */
        DELAYN_XK             /* 1 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void delayN_ak_calc(ugen u)
{
    DELAYN_CALC(
        // Control Arguments
        DELAYN_XK             /* 1 */,
        // Audio Arguments
        DELAYN_DELAY_TIMEA    /* 0 */
    )
}

// 2
void delayN_ka_calc(ugen u)
{
    DELAYN_CALC(
        // Control Arguments
        DELAYN_DELAY_TIMEK    /* 0 */,
        // Audio Arguments
        DELAYN_XA             /* 1 */
    )
}

// 3
void delayN_aa_calc(ugen u)
{
    DELAYN_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        DELAYN_DELAY_TIMEA    /* 0 */
        DELAYN_XA             /* 1 */
    )
}

inline double delayL(int64_t write_index, int64_t idelay_time, double delta, uint32_t num_samples_mask, double* samples)
{
    const int64_t iread_index0 = write_index - idelay_time;
    double y;

    if (iread_index0 < 0)
    {
        y = 0;
    }

    else
    {
        int64_t iread_index1;
        double y0, y1;
        iread_index1 = iread_index0 - 1;
        y0 = samples[iread_index0 & num_samples_mask];
        y1 = iread_index1 < 0 ? 0 : samples[iread_index1 & num_samples_mask];
        y  = LINEAR_INTERP(y0, y1, delta);
    }

    return y;
}

#define DELAYL_CALC(CONTROL_ARGS, AUDIO_ARGS)                                               \
INIT_DELAY(u);                                                                              \
double delta;                                                                               \
int64_t idelay_time;                                                                        \
CONTROL_ARGS                                                                                \
AUDIO_LOOP(                                                                                 \
    AUDIO_ARGS                                                                              \
    idelay_time = delay_time;                                                               \
    delta = delay_time - (double) idelay_time;                                              \
    y = delayL(write_index, idelay_time, delta, num_samples_mask, buffer.samples);          \
    buffer.samples[write_index & num_samples_mask] = x;                                     \
    ++write_index;                                                                          \
    UGEN_OUT(out, y);                                                                       \
);                                                                                          \
FINISH_DELAY();                                                                             \


#define DELAYL_DELAY_TIMEK delay_time = fmin(data.max_delay_time, fmax(1, (*in0) * SAMPLE_RATE));
#define DELAYL_DELAY_TIMEA delay_time = fmin(data.max_delay_time, fmax(1, UGEN_IN(in0) * SAMPLE_RATE));
#define DELAYL_XK x = *in1;
#define DELAYL_XA x = UGEN_IN(in1);

// 0
void delayL_kk_calc(ugen u)
{
    DELAYL_CALC(
        // Control Arguments
        DELAYL_DELAY_TIMEK    /* 0 */
        DELAYL_XK             /* 1 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void delayL_ak_calc(ugen u)
{
    DELAYL_CALC(
        // Control Arguments
        DELAYL_XK             /* 1 */,
        // Audio Arguments
        DELAYL_DELAY_TIMEA    /* 0 */
    )
}

// 2
void delayL_ka_calc(ugen u)
{
    DELAYL_CALC(
        // Control Arguments
        DELAYL_DELAY_TIMEK    /* 0 */,
        // Audio Arguments
        DELAYL_XA             /* 1 */
    )
}

// 3
void delayL_aa_calc(ugen u)
{
    DELAYL_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        DELAYL_DELAY_TIMEA    /* 0 */
        DELAYL_XA             /* 1 */
    )
}

inline double delayC(int64_t write_index, int64_t idelay_time, double delta, uint32_t num_samples_mask, double* samples)
{
    const int64_t iread_index1 = write_index - idelay_time;
    const int64_t iread_index0 = iread_index1 + 1;
    double y;

    if (iread_index0 < 0)
    {
        y = 0;
    }

    else
    {
        const int64_t iread_index2 = iread_index1 - 1;
        const int64_t iread_index3 = iread_index1 - 2;
        double y0, y1, y2, y3;

        if (iread_index1 < 0)
        {
            y0 = samples[iread_index0 & num_samples_mask];
            y1 = y2 = y3 = 0;
        }

        else if (iread_index2 < 0)
        {
            y0 = samples[iread_index0 & num_samples_mask];
            y1 = samples[iread_index1 & num_samples_mask];
            y2 = y3 = 0;
        }

        else if (iread_index3 < 0)
        {
            y0 = samples[iread_index0 & num_samples_mask];
            y1 = samples[iread_index1 & num_samples_mask];
            y2 = samples[iread_index1 & num_samples_mask];
            y3 = 0;
        }

        else
        {
            y0 = samples[iread_index0 & num_samples_mask];
            y1 = samples[iread_index1 & num_samples_mask];
            y2 = samples[iread_index2 & num_samples_mask];
            y3 = samples[iread_index3 & num_samples_mask];
        }

        y = CUBIC_INTERP(y0, y1, y2, y3, delta);
    }

    return y;
}

#define DELAYC_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                                       \
INIT_DELAY(u);                                                                                                      \
double* samples = buffer.samples;                                                                                   \
double delta;                                                                                                       \
double read_index;                                                                                                  \
int64_t idelay_time;                                                                                                \
CONTROL_ARGS                                                                                                        \
AUDIO_LOOP(                                                                                                         \
    AUDIO_ARGS                                                                                                      \
    idelay_time = delay_time;                                                                                       \
    delta = delay_time - (double) idelay_time;                                                                      \
    y = delayC(write_index, idelay_time, delta, num_samples_mask, samples);                                         \
    buffer.samples[write_index & num_samples_mask] = x;                                                             \
    ++write_index;                                                                                                  \
    UGEN_OUT(out, y);                                                                                               \
);                                                                                                                  \
FINISH_DELAY();

// Clamp delay at 1 to prevent the + 1 iread_index3 from reading on the wrong side of the write head
#define DELAYC_DELAY_TIMEK delay_time = fmin(data.max_delay_time, fmax(1.0, (*in0) * SAMPLE_RATE));
#define DELAYC_DELAY_TIMEA delay_time = fmin(data.max_delay_time, fmax(1.0, UGEN_IN(in0) * SAMPLE_RATE));
#define DELAYC_XK x = *in1;
#define DELAYC_XA x = UGEN_IN(in1);

// 0
void delayC_kk_calc(ugen u)
{
    DELAYC_CALC(
        // Control Arguments
        DELAYC_DELAY_TIMEK    /* 0 */
        DELAYC_XK             /* 1 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void delayC_ak_calc(ugen u)
{
    DELAYC_CALC(
        // Control Arguments
        DELAYC_XK             /* 1 */,
        // Audio Arguments
        DELAYC_DELAY_TIMEA    /* 0 */
    )
}

// 2
void delayC_ka_calc(ugen u)
{
    DELAYC_CALC(
        // Control Arguments
        DELAYC_DELAY_TIMEK    /* 0 */,
        // Audio Arguments
        DELAYC_XA             /* 1 */
    )
}

// 3
void delayC_aa_calc(ugen u)
{
    DELAYC_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        DELAYC_DELAY_TIMEA    /* 0 */
        DELAYC_XA             /* 1 */
    )
}

#define INIT_COMB(u)                   \
double decay_time;                       \
double feedback;                       \
double* in2 = UGEN_INPUT_BUFFER(u, 2); \

static inline double CALC_FEEDBACK(double delay_time, double decay_time)
{
    if (delay_time == 0.0 || decay_time == 0.0)
        return 0.0;

    double absret = exp(LOG_001 * delay_time / fabs(decay_time));
    return copysign(absret, decay_time);
}

#define COMBN_CALC(CONTROL_ARGS, AUDIO_ARGS)                                        \
INIT_DELAY(u);                                                                      \
INIT_COMB(u);                                                                       \
int64_t iread_index;                                                                \
CONTROL_ARGS                                                                        \
AUDIO_LOOP(                                                                         \
    AUDIO_ARGS                                                                      \
    iread_index = write_index - (int64_t) delay_time;                               \
    y = iread_index < 0 ? 0 : buffer.samples[iread_index & num_samples_mask];       \
    feedback = CALC_FEEDBACK(delay_time, decay_time);                               \
    buffer.samples[write_index & num_samples_mask] = x + (feedback * y);            \
    ++write_index;                                                                  \
    UGEN_OUT(out, y);                                                               \
);                                                                                  \
FINISH_DELAY();                                                                     \

#define COMBN_DELAY_TIMEK delay_time = fmin(data.max_delay_time, fmax(1, (*in0) * SAMPLE_RATE));
#define COMBN_DELAY_TIMEA delay_time = fmin(data.max_delay_time, fmax(1, UGEN_IN(in0) * SAMPLE_RATE));
#define COMBN_FEEDBACKK decay_time = (*in1) * SAMPLE_RATE;
#define COMBN_FEEDBACKA decay_time = UGEN_IN(in1) * SAMPLE_RATE;
#define COMBN_XK x = *in2;
#define COMBN_XA x = UGEN_IN(in2);

// 0
void combN_kkk_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        COMBN_DELAY_TIMEK     /* 0 */
        COMBN_FEEDBACKK       /* 1 */
        COMBN_XK              /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void combN_akk_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        COMBN_FEEDBACKK       /* 1 */
        COMBN_XK              /* 2 */,
        // Audio Arguments
        COMBN_DELAY_TIMEA     /* 0 */
    )
}

// 2
void combN_kak_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        COMBN_DELAY_TIMEK     /* 0 */
        COMBN_XK              /* 2 */,
        // Audio Arguments
        COMBN_FEEDBACKA       /* 1 */
    )
}

// 3
void combN_aak_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        COMBN_XK              /* 2 */,
        // Audio Arguments
        COMBN_DELAY_TIMEA     /* 0 */
        COMBN_FEEDBACKA       /* 1 */
    )
}

// 4
void combN_kka_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        COMBN_DELAY_TIMEK     /* 0 */
        COMBN_FEEDBACKK       /* 1 */,
        // Audio Arguments
        COMBN_XA              /* 2 */
    )
}

// 5
void combN_aka_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        COMBN_FEEDBACKK       /* 1 */,
        // Audio Arguments
        COMBN_DELAY_TIMEA     /* 0 */
        COMBN_XA              /* 2 */
    )
}

// 6
void combN_kaa_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        COMBN_DELAY_TIMEK     /* 0 */,
        // Audio Arguments
        COMBN_FEEDBACKA       /* 1 */
        COMBN_XA              /* 2 */
    )
}

// 7
void combN_aaa_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        COMBN_DELAY_TIMEA     /* 0 */
        COMBN_FEEDBACKA       /* 1 */
        COMBN_XA              /* 2 */
    )
}

#define COMBL_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                \
INIT_DELAY(u);                                                                              \
INIT_COMB(u);                                                                               \
double delta;                                                                               \
int64_t idelay_time;                                                                        \
CONTROL_ARGS                                                                                \
AUDIO_LOOP(                                                                                 \
    AUDIO_ARGS                                                                              \
    idelay_time = delay_time;                                                               \
    delta = delay_time - (double) idelay_time;                                              \
    y = delayL(write_index, idelay_time, delta, num_samples_mask, buffer.samples);          \
    feedback = CALC_FEEDBACK(delay_time, decay_time);                                       \
    buffer.samples[write_index & num_samples_mask] = x + (feedback * y);                    \
    ++write_index;                                                                          \
    UGEN_OUT(out, y);                                                                       \
);                                                                                          \
FINISH_DELAY();                                                                             \

#define COMBL_DELAY_TIMEK delay_time = fmin(data.max_delay_time, fmax(1, (*in0) * SAMPLE_RATE));
#define COMBL_DELAY_TIMEA delay_time = fmin(data.max_delay_time, fmax(1, UGEN_IN(in0) * SAMPLE_RATE));
#define COMBL_FEEDBACKK decay_time = (*in1) * SAMPLE_RATE;
#define COMBL_FEEDBACKA decay_time = UGEN_IN(in1) * SAMPLE_RATE;
#define COMBL_XK x = *in2;
#define COMBL_XA x = UGEN_IN(in2);

// 0
void combL_kkk_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        COMBL_DELAY_TIMEK     /* 0 */
        COMBL_FEEDBACKK       /* 1 */
        COMBL_XK              /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void combL_akk_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        COMBL_FEEDBACKK       /* 1 */
        COMBL_XK              /* 2 */,
        // Audio Arguments
        COMBL_DELAY_TIMEA     /* 0 */
    )
}

// 2
void combL_kak_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        COMBL_DELAY_TIMEK     /* 0 */
        COMBL_XK              /* 2 */,
        // Audio Arguments
        COMBL_FEEDBACKA       /* 1 */
    )
}

// 3
void combL_aak_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        COMBL_XK              /* 2 */,
        // Audio Arguments
        COMBL_DELAY_TIMEA     /* 0 */
        COMBL_FEEDBACKA       /* 1 */
    )
}

// 4
void combL_kka_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        COMBL_DELAY_TIMEK     /* 0 */
        COMBL_FEEDBACKK       /* 1 */,
        // Audio Arguments
        COMBL_XA              /* 2 */
    )
}

// 5
void combL_aka_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        COMBL_FEEDBACKK       /* 1 */,
        // Audio Arguments
        COMBL_DELAY_TIMEA     /* 0 */
        COMBL_XA              /* 2 */
    )
}

// 6
void combL_kaa_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        COMBL_DELAY_TIMEK     /* 0 */,
        // Audio Arguments
        COMBL_FEEDBACKA       /* 1 */
        COMBL_XA              /* 2 */
    )
}

// 7
void combL_aaa_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        COMBL_DELAY_TIMEA     /* 0 */
        COMBL_FEEDBACKA       /* 1 */
        COMBL_XA              /* 2 */
    )
}

#define COMBC_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                                        \
INIT_DELAY(u);                                                                                                      \
INIT_COMB(u);                                                                                                       \
double* samples = buffer.samples;                                                                                   \
double delta;                                                                                                       \
int64_t idelay_time;                                                                                                \
CONTROL_ARGS                                                                                                        \
AUDIO_LOOP(                                                                                                         \
    AUDIO_ARGS                                                                                                      \
    idelay_time = delay_time;                                                                                       \
    delta = delay_time - (double) idelay_time;                                                                      \
    y = delayC(write_index, idelay_time, delta, num_samples_mask, samples);                                         \
    feedback = CALC_FEEDBACK(delay_time, decay_time);                                                               \
    samples[write_index & num_samples_mask] = x + (feedback * y);                                                   \
    ++write_index;                                                                                                  \
    UGEN_OUT(out, y);                                                                                               \
);                                                                                                                  \
FINISH_DELAY();

#define COMBC_DELAY_TIMEK delay_time = fmin(data.max_delay_time, fmax(1.0, (*in0) * SAMPLE_RATE));
#define COMBC_DELAY_TIMEA delay_time = fmin(data.max_delay_time, fmax(1.0, UGEN_IN(in0) * SAMPLE_RATE));
#define COMBC_FEEDBACKK decay_time = (*in1) * SAMPLE_RATE;
#define COMBC_FEEDBACKA decay_time = UGEN_IN(in1) * SAMPLE_RATE;
#define COMBC_XK x = *in2;
#define COMBC_XA x = UGEN_IN(in2);

// 0
void combC_kkk_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        COMBC_DELAY_TIMEK     /* 0 */
        COMBC_FEEDBACKK       /* 1 */
        COMBC_XK              /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void combC_akk_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        COMBC_FEEDBACKK       /* 1 */
        COMBC_XK              /* 2 */,
        // Audio Arguments
        COMBC_DELAY_TIMEA     /* 0 */
    )
}

// 2
void combC_kak_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        COMBC_DELAY_TIMEK     /* 0 */
        COMBC_XK              /* 2 */,
        // Audio Arguments
        COMBC_FEEDBACKA       /* 1 */
    )
}

// 3
void combC_aak_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        COMBC_XK              /* 2 */,
        // Audio Arguments
        COMBC_DELAY_TIMEA     /* 0 */
        COMBC_FEEDBACKA       /* 1 */
    )
}

// 4
void combC_kka_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        COMBC_DELAY_TIMEK     /* 0 */
        COMBC_FEEDBACKK       /* 1 */,
        // Audio Arguments
        COMBC_XA              /* 2 */
    )
}

// 5
void combC_aka_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        COMBC_FEEDBACKK       /* 1 */,
        // Audio Arguments
        COMBC_DELAY_TIMEA     /* 0 */
        COMBC_XA              /* 2 */
    )
}

// 6
void combC_kaa_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        COMBC_DELAY_TIMEK     /* 0 */,
        // Audio Arguments
        COMBC_FEEDBACKA       /* 1 */
        COMBC_XA              /* 2 */
    )
}

// 7
void combC_aaa_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        COMBC_DELAY_TIMEA     /* 0 */
        COMBC_FEEDBACKA       /* 1 */
        COMBC_XA              /* 2 */
    )
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

void print_node(synth_node* node)
{
    if (node != NULL)
        printf("synth_node %p { ugen_graph: %p, ugen_graph_node: %p, ugen_wires: %p, ugen_wires_node: %p, previous: %p, next: %p, time: %llu, "
               "key %u, hash: %u, table_index: %u, num_ugens: %u, num_wires: %u, previous_alive_status: %u, alive_status %u }\n",
               node,
               node->ugen_graph,
               node->ugen_graph_node,
               node->ugen_wires,
               node->ugen_wires_node,
               node->previous,
               node->next,
               node->time,
               node->key,
               node->hash,
               node->table_index,
               node->num_ugens,
               node->num_wires,
               node->previous_alive_status,
               node->alive_status);
    else
        printf("NULL");
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

    hash_table table = hash_table_new();

    for (i = 0; i < num_values; ++i)
    {
        times[i] = (random() / (double) RAND_MAX) * 10000.0;
        synth_node* node = new_test_synth(times[i]);
        node->key = i;
        node->hash = HASH_KEY(i);
        node->table_index = i;
        hash_table_insert(table, node);
        assert(node == hash_table_lookup(table, i));
    }

    print_hash_table(table);
    puts("Asserting table values...\n\n");

    for (i = 0; i < num_values; ++i)
    {
        synth_node* node = hash_table_lookup(table, i);
        assert(node);
        assert(node->time == times[i]);
        assert(node->key == i);
        assert(node->hash == HASH_KEY(i));
    }

    puts("Removing table values...\n\n");

    for (i = 0; i < num_values; ++i)
    {
        synth_node* node = hash_table_lookup(table, i);
        assert(node);
        hash_table_remove(table, node);
        free_synth(node);
    }

    puts("Asserting NULL values...\n\n");

    for (i = 0; i < MAX_SYNTHS; ++i)
    {
        assert(hash_table_lookup(table, i) == NULL);
    }

    print_hash_table(table);
    puts("Freeing table...\n\n");
    hash_table_free(table);
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

#define CLAMP(V,MIN,MAX)                    \
({                                          \
    double result = V < MIN ? MIN : V;      \
    result        = V > MAX ? MAX : V;      \
    result;                                 \
})

#define SOFT_CLIP(XIN,AMOUNT)                                               \
({                                                                          \
    double X     = XIN * AMOUNT * 0.5;                                      \
    double v1    = atan_table[((uint16_t) (X * TABLE_SIZE))];               \
    double v2    = atan_table[((uint16_t) (X * TABLE_SIZE + 1))];           \
    double delta = X - ((int64_t) X);                                       \
    v1 + delta * (v2 - v1);                                                 \
})

#define TANH_DIST(XIN,AMOUNT)                                               \
({                                                                          \
    double X     = XIN * AMOUNT * 0.5;                                      \
    double v1    = tanh_table[((uint16_t) (X * TABLE_SIZE))];               \
    double v2    = tanh_table[((uint16_t) (X * TABLE_SIZE + 1))];           \
    double delta = X - ((int64_t) X);                                       \
    v1 + delta * (v2 - v1);                                                 \
})


#define SIN_DIST(XIN,AMOUNT)                                                \
({                                                                          \
    double X     = XIN * AMOUNT * 0.5;                                      \
    double v1    = sine_table[((uint16_t) (X * TABLE_SIZE))];               \
    double v2    = sine_table[((uint16_t) (X * TABLE_SIZE + 1))];           \
    double delta = X - ((int64_t) X);                                       \
    v1 + delta * (v2 - v1);                                                 \
})

#define HARD_CLIP(X,AMOUNT) (CLAMP(X*AMOUNT,-1.0,1.0))

#define POLY3_DIST(X,AMOUNT) (1.5 * X - 0.5 * pow(X,3))

#define WRAP(X,AMOUNT)              \
({                                  \
    double x   = X * AMOUNT;        \
    double ret = x;                 \
    if (x >= 1)                     \
        ret = x - 2;                \
    else if (x < -1)                \
        ret = x + 2;                \
    ret;                            \
})

void accumulator_constructor(ugen* u)
{
    u->data = malloc(DOUBLE_SIZE); // Phase accumulator
    *((double*) u->data) = 0.0f;
}

void accumulator_deconstructor(ugen* u)
{
    free(u->data);
}

// LFOs

#define LFSAW_CALC(CONTROL_ARGS, AUDIO_ARGS)                \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                      \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                      \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                     \
double phase = *((double*) u.data);                         \
double freq;                                                \
/* double phaseArg; */                                      \
double amp1;                                                \
double amp2;                                                \
double delta;                                               \
double y;                                                   \
CONTROL_ARGS                                                \
AUDIO_LOOP(                                                 \
    AUDIO_ARGS                                              \
    /* Branchless and table-less saw */                     \
    amp1   = (uint16_t) phase;                              \
    amp2   = amp1 + 1;                                      \
    delta  = phase - ((int64_t) phase);                     \
    y      = LERP(amp1, amp2, delta) * RECIP_TABLE_SIZE;    \
    phase += TABLE_MUL_RECIP_SAMPLE_RATE * freq;            \
    UGEN_OUT(out, y);                                       \
);                                                          \
*((double*) u.data) = phase;                                \

void lfsaw_aa_calc(ugen u)
{
    LFSAW_CALC(
        /* no control args */,
        freq = UGEN_IN(in0); // Audio args
        /* phaseArg = UGEN_IN(in1); */
    );
}

void lfsaw_ak_calc(ugen u)
{
    LFSAW_CALC(
        /*phaseArg = in1[0];*/, // Control arg
        freq = UGEN_IN(in0); // Audio arg
    );
}

void lfsaw_ka_calc(ugen u)
{
    LFSAW_CALC(
        freq = in0[0];, // Control arg
        /*phaseArg = UGEN_IN(in1);*/ // Audio arg
    );
}

void lfsaw_kk_calc(ugen u)
{
    LFSAW_CALC(
        freq = in0[0]; // Control args
        /*phaseArg = in1[0]*/,
        /* no audio args */
    );
}

#define SIXTEEN_BITS 16
#define MASK_OFFSET 1
const int16_t lfpulse_phase_shift = sizeof(int16_t) * SIXTEEN_BITS - MASK_OFFSET;

#define LFPULSE_CALC(CONTROL_ARGS, AUDIO_ARGS)                          \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                  \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                                  \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                 \
double phase = *((double*) u.data);                                     \
double freq;                                                            \
/*double phaseArg;*/                                                    \
double y;                                                               \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
    /*Branchless and table-less square*/                                \
    y = 1 | (((int16_t) phase) >> lfpulse_phase_shift);                 \
    phase += TABLE_MUL_RECIP_SAMPLE_RATE * freq;                        \
    /* y * 0.5 + 0.5 to make range 0 to 1 */                            \
    UGEN_OUT(out, y * 0.5 + 0.5);                                       \
);                                                                      \
*((double*) u.data) = phase;                                            \

void lfpulse_aa_calc(ugen u)
{
    LFPULSE_CALC(
        /* no control args */,
        freq = UGEN_IN(in0); // Audio args
        /* phaseArg = UGEN_IN(in1); */
    );
}

void lfpulse_ak_calc(ugen u)
{
    LFPULSE_CALC(
        /*phaseArg = in1[0];*/, // Control arg
        freq = UGEN_IN(in0); // Audio arg
    );
}

void lfpulse_ka_calc(ugen u)
{
    LFPULSE_CALC(
        freq = in0[0];, // Control arg
        /*phaseArg = UGEN_IN(in1);*/ // Audio arg
    );
}

void lfpulse_kk_calc(ugen u)
{
    LFPULSE_CALC(
        freq = in0[0]; // Control args
        /*phaseArg = in1[0]*/,
        /* no audio args */
    );
}

//---------------------------------------------
//MinBlep Bandwidth-limited Saw and Square
//---------------------------------------------

#define KTABLE 64 // BLEP table oversampling factor

typedef struct
{
    double *lpTable;
    int32_t    c;
} minbleptable_t;

typedef struct
{
    double  output;
    double  phase;
    double  masterPhase;
    double *buffer;      // circular output buffer
    int32_t    cBuffer;     // buffer size
    int32_t    iBuffer;     // current buffer position
    int32_t    nInit;         // amount of initialized entries
    double  prevSyncAmp; //For hardsync
} minblep;

minbleptable_t gMinBLEP;

bool minBLEP_Init()
{
    // load table
    const int8_t* blep_table = "/misc/minblep.mat";
    int8_t path_to_blep_table[strlen(RESOUCES_PATH) + strlen(blep_table)];
    strcat(path_to_blep_table,RESOUCES_PATH);
    strcat(path_to_blep_table,blep_table);
    FILE *fp=fopen(path_to_blep_table,"rb");
    uint32_t iSize;

    if (!fp) return false;

    fseek(fp,0x134,SEEK_SET);

    fread(&iSize,sizeof(int32_t),1,fp);
    gMinBLEP.c=iSize/sizeof(double);

    gMinBLEP.lpTable=(double*)malloc(iSize);
    if (!gMinBLEP.lpTable) return false;

    fread(gMinBLEP.lpTable,iSize,1,fp);

    fclose(fp);

    return true;
}

void minBLEP_Free()
{
    free(gMinBLEP.lpTable);
}

void minblep_constructor(ugen* u)
{
    minblep* mb     = malloc(sizeof(minblep));
    mb->output      = 0.0;
    mb->phase       = 0.0;
    mb->masterPhase = 0.0;
    mb->cBuffer     = gMinBLEP.c/KTABLE;
    mb->buffer      = (double*)malloc(sizeof(double) * mb->cBuffer);
    mb->iBuffer     = 0;
    mb->nInit       = 0;
    mb->prevSyncAmp = 0;
    u->data         = mb;
}

void minblep_deconstructor(ugen* u)
{
    minblep* mb = (minblep*) u->data;
    // free(mb->buffer);
    free(u->data);
}

// add impulse into buffer
inline void add_blep(minblep* mb, double offset, double amp)
{
    int32_t   i;
    double *out       = mb->buffer  + mb->iBuffer;
    double *in        = gMinBLEP.lpTable + (int32_t) (KTABLE*offset);
    double frac       = fmod(KTABLE*offset,1.0);
    int32_t   cBLEP      = (gMinBLEP.c / KTABLE) - 1;
    double *bufferEnd = mb->buffer  + mb->cBuffer;
    double f;

    // add
    for(i=0; i < mb->nInit; i++, in += KTABLE, out++)
    {
        if (out >= bufferEnd)
            out = mb->buffer;

        f = LERP(in[0],in[1],frac);
        *out += amp * (1-f);
    }

    // copy
    for(; i < cBLEP; i++, in += KTABLE, out++)
    {
        if (out >= bufferEnd)
            out = mb->buffer;

        f = LERP(in[0],in[1],frac);
        *out = amp*(1-f);
    }

    mb->nInit = cBLEP;
}

#define SAW_CALC(CONTROL_ARGS, AUDIO_ARGS)      \
double* in0 = UGEN_INPUT_BUFFER(u, 0);          \
double* out = UGEN_OUTPUT_BUFFER(u, 0);         \
minblep mb  = *((minblep*) u.data);             \
double freq;                                    \
double y;                                       \
CONTROL_ARGS                                    \
AUDIO_LOOP(                                     \
    AUDIO_ARGS                                  \
    /* create waveform */                       \
    mb.phase += freq;                           \
    /* add BLEP at end of waveform */           \
    if (mb.phase >= 1)                          \
    {                                           \
        mb.phase  = mb.phase - 1.0;             \
        mb.output = 0.0;                        \
        add_blep(&mb, mb.phase/freq,1.0);       \
    }                                           \
    y = mb.phase;                               \
    /* add BLEP buffer contents */              \
    if (mb.nInit)                               \
    {                                           \
        y += mb.buffer[mb.iBuffer];             \
        mb.nInit--;                             \
        if (++mb.iBuffer >= mb.cBuffer)         \
            mb.iBuffer=0;                       \
    }                                           \
    UGEN_OUT(out, y);                           \
);                                              \
*((minblep*) u.data) = mb;                      \

void saw_k_calc(ugen u)
{
    SAW_CALC(
        freq  = in0[0] * RECIP_SAMPLE_RATE;,
        /* no audio args */
    )
}

void saw_a_calc(ugen u)
{
    SAW_CALC(
        /* no control args */,
        freq  = UGEN_IN(in0) * RECIP_SAMPLE_RATE;
    )
}

#define SQUARE_CALC(CONTROL_ARGS, AUDIO_ARGS)               \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                      \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                      \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                     \
minblep mb  = *((minblep*) u.data);                         \
double freq;                                                \
double pwm;                                                 \
double y;                                                   \
CONTROL_ARGS                                                \
AUDIO_LOOP(                                                 \
    AUDIO_ARGS                                              \
    /* create waveform */                                   \
    mb.phase += freq;                                       \
    /* add BLEP at end of waveform */                       \
    if (mb.phase >= 1)                                      \
    {                                                       \
        mb.phase  = mb.phase - 1.0;                         \
        mb.output = 0.0;                                    \
        add_blep(&mb, mb.phase/freq,1.0);                   \
    }                                                       \
    /* add BLEP in middle of wavefor for squarewave */      \
    if (!mb.output && mb.phase > pwm)                       \
    {                                                       \
        mb.output = 1.0;                                    \
        add_blep(&mb, (mb.phase - pwm) / freq,-1.0);        \
    }                                                       \
    y = mb.output;                                          \
    /* add BLEP buffer contents */                          \
    if (mb.nInit)                                           \
    {                                                       \
        y += mb.buffer[mb.iBuffer];                         \
        mb.nInit--;                                         \
        if (++mb.iBuffer >= mb.cBuffer)                     \
            mb.iBuffer=0;                                   \
    }                                                       \
    UGEN_OUT(out, y);                                       \
);                                                          \
*((minblep*) u.data) = mb;                                  \

void square_aa_calc(ugen u)
{
    SQUARE_CALC(
        /* no control args */,
        freq = UGEN_IN(in0) * RECIP_SAMPLE_RATE;
        pwm  = CLAMP(UGEN_IN(in1),0,1) * 0.5;
    )
}

void square_ka_calc(ugen u)
{
    SQUARE_CALC(
        freq = in0[0] * RECIP_SAMPLE_RATE;,
        pwm  = CLAMP(UGEN_IN(in1),0,1) * 0.5;
    )
}

void square_ak_calc(ugen u)
{
    SQUARE_CALC(
        pwm  = CLAMP(in1[0],0,1) * 0.5;,
        freq = UGEN_IN(in0) * RECIP_SAMPLE_RATE;
    )
}

void square_kk_calc(ugen u)
{
    SQUARE_CALC(
        freq = in0[0] * RECIP_SAMPLE_RATE;
        pwm  = CLAMP(in1[0],0,1) * 0.5;,
        /* no audio args */
    )
}

#define SYNCSAW_CALC(CONTROL_ARGS, AUDIO_ARGS)      \
double* in0 = UGEN_INPUT_BUFFER(u, 0);              \
double* in1 = UGEN_INPUT_BUFFER(u, 1);              \
double* out = UGEN_OUTPUT_BUFFER(u, 0);             \
minblep mb  = *((minblep*) u.data);                 \
double freq;                                        \
double sync;                                        \
double y;                                           \
CONTROL_ARGS                                        \
AUDIO_LOOP(                                         \
    AUDIO_ARGS                                      \
    /* create waveform */                           \
    mb.phase += freq;                               \
    /* add BLEP at end of waveform */               \
    if (mb.phase >= 1)                              \
    {                                               \
        mb.phase  = mb.phase - 1.0;                 \
        mb.output = 0.0;                            \
        add_blep(&mb, mb.phase/freq,1.0);           \
    }                                               \
    else if (mb.prevSyncAmp < 0 && sync > 0)        \
    {                                               \
        mb.phase  = 0.0;                            \
        mb.output = 0.0;                            \
        add_blep(&mb, mb.phase/freq,1.0);           \
    }                                               \
    y = mb.phase;                                   \
    /* add BLEP buffer contents */                  \
    if (mb.nInit)                                   \
    {                                               \
        y += mb.buffer[mb.iBuffer];                 \
        mb.nInit--;                                 \
        if (++mb.iBuffer >= mb.cBuffer)             \
            mb.iBuffer=0;                           \
    }                                               \
    mb.prevSyncAmp = sync;                          \
    UGEN_OUT(out, y);                               \
);                                                  \
*((minblep*) u.data) = mb;                          \

void syncsaw_aa_calc(ugen u)
{
    SYNCSAW_CALC(
        /* no control args */,
        freq  = UGEN_IN(in0) * RECIP_SAMPLE_RATE;
        sync  = UGEN_IN(in1);
    )
}

void syncsaw_ka_calc(ugen u)
{
    SYNCSAW_CALC(
        freq  = in0[0] * RECIP_SAMPLE_RATE;,
        sync  = UGEN_IN(in1);
    )
}

void syncsaw_ak_calc(ugen u)
{
    SYNCSAW_CALC(
        sync  = in1[0];,
        freq  = UGEN_IN(in0) * RECIP_SAMPLE_RATE;
    )
}

void syncsaw_kk_calc(ugen u)
{
    SYNCSAW_CALC(
        freq  = in0[0] * RECIP_SAMPLE_RATE;
        sync  = in1[0];,
        /* no audio args */
    )
}

#define SYNCSQUARE_CALC(CONTROL_ARGS, AUDIO_ARGS)           \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                      \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                      \
double* in2 = UGEN_INPUT_BUFFER(u, 2);                      \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                     \
minblep mb  = *((minblep*) u.data);                         \
double freq;                                                \
double pwm;                                                 \
double sync;                                                \
double y;                                                   \
CONTROL_ARGS                                                \
AUDIO_LOOP(                                                 \
    AUDIO_ARGS                                              \
    /* create waveform */                                   \
    mb.phase += freq;                                       \
    /* add BLEP at end of waveform */                       \
    if (mb.phase >= 1)                                      \
    {                                                       \
        mb.phase  = mb.phase - 1.0;                         \
        mb.output = -1.0;                                   \
        add_blep(&mb, mb.phase/freq,1.0);                   \
    }                                                       \
    /* add BLEP in middle of wavefor for squarewave */      \
    if (!mb.output && mb.phase > pwm)                       \
    {                                                       \
        mb.output = 1.0;                                    \
        add_blep(&mb, (mb.phase - pwm) / freq,-1.0);        \
    }                                                       \
    if (mb.prevSyncAmp < 0 && sync > 0)                     \
    {                                                       \
        mb.phase  = 0.0;                                    \
        mb.output = 1.0;                                    \
        add_blep(&mb, mb.phase/freq,1.0);                   \
    }                                                       \
    y = mb.output;                                          \
    /* add BLEP buffer contents */                          \
    if (mb.nInit)                                           \
    {                                                       \
        y += mb.buffer[mb.iBuffer];                         \
        mb.nInit--;                                         \
        if (++mb.iBuffer >= mb.cBuffer)                     \
            mb.iBuffer=0;                                   \
    }                                                       \
    mb.prevSyncAmp = sync;                                  \
    UGEN_OUT(out, y);                                       \
);                                                          \
*((minblep*) u.data) = mb;                                  \

#define SYNCSQUARE_FREQ_K freq = in0[0] * RECIP_SAMPLE_RATE;
#define SYNCSQUARE_PWM_K pwm  = CLAMP(in1[0],0,1) * 0.5;
#define SYNCSQUARE_SYNC_K sync = in2[0];

#define SYNCSQUARE_FREQ_A freq = UGEN_IN(in0) * RECIP_SAMPLE_RATE;
#define SYNCSQUARE_PWM_A pwm  = CLAMP(UGEN_IN(in1),0,1) * 0.5;
#define SYNCSQUARE_SYNC_A sync = UGEN_IN(in2);

void syncsquare_kkk_calc(ugen u)
{
    SYNCSQUARE_CALC(
        SYNCSQUARE_FREQ_K
        SYNCSQUARE_PWM_K
        SYNCSQUARE_SYNC_K,
        /* no audio args */
    )
}

void syncsquare_akk_calc(ugen u)
{
    SYNCSQUARE_CALC(
        SYNCSQUARE_PWM_K
        SYNCSQUARE_SYNC_K,
        SYNCSQUARE_FREQ_A
    )
}

void syncsquare_kak_calc(ugen u)
{
    SYNCSQUARE_CALC(
        SYNCSQUARE_FREQ_K
        SYNCSQUARE_SYNC_K,
        SYNCSQUARE_PWM_A
    )
}

void syncsquare_aak_calc(ugen u)
{
    SYNCSQUARE_CALC(
        SYNCSQUARE_SYNC_K,
        SYNCSQUARE_FREQ_A
        SYNCSQUARE_PWM_A
    )
}

void syncsquare_kka_calc(ugen u)
{
    SYNCSQUARE_CALC(
        SYNCSQUARE_FREQ_K
        SYNCSQUARE_PWM_K,
        SYNCSQUARE_SYNC_A
    )
}

void syncsquare_aka_calc(ugen u)
{
    SYNCSQUARE_CALC(
        SYNCSQUARE_PWM_K,
        SYNCSQUARE_FREQ_A
        SYNCSQUARE_SYNC_A
    )
}

void syncsquare_kaa_calc(ugen u)
{
    SYNCSQUARE_CALC(
        SYNCSQUARE_FREQ_K,
        SYNCSQUARE_PWM_A
        SYNCSQUARE_SYNC_A
    )
}

void syncsquare_aaa_calc(ugen u)
{
    SYNCSQUARE_CALC(
        /* no control args */,
        SYNCSQUARE_FREQ_A
        SYNCSQUARE_PWM_A
        SYNCSQUARE_SYNC_A
    )
}

#define SYNCOSC_CALC(CONTROL_ARGS, AUDIO_ARGS)                                              \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                                      \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                                                      \
double* in2 = UGEN_INPUT_BUFFER(u, 2);                                                      \
double* in3 = UGEN_INPUT_BUFFER(u, 3);                                                      \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                                     \
minblep mb  = *((minblep*) u.data);                                                         \
double slaveFreq;                                                                           \
double slaveWave;                                                                           \
double pwm;                                                                                 \
double masterFreq;                                                                          \
double y;                                                                                   \
double freqN;                                                                               \
CONTROL_ARGS                                                                                \
AUDIO_LOOP(                                                                                 \
    AUDIO_ARGS                                                                              \
                                                                                            \
    /* create waveform */                                                                   \
    mb.phase       = mb.phase + freqN;                                                      \
    mb.masterPhase = WRAP(mb.masterPhase + (masterFreq * RECIP_SAMPLE_RATE) * 1.0,1.0);     \
                                                                                            \
    /* add BLEP at end of waveform */                                                       \
    if (mb.phase >= 1)                                                                      \
    {                                                                                       \
        mb.phase  = mb.phase - 1.0;                                                         \
        mb.output = -1.0;                                                                   \
        add_blep(&mb, mb.phase/freqN,1.0);                                                  \
    }                                                                                       \
                                                                                            \
    /* add BLEP in middle of wavefor for squarewave */                                      \
    else if (slaveWave && !mb.output && mb.phase > pwm)                                     \
    {                                                                                       \
        mb.output = 1.0;                                                                    \
        add_blep(&mb, (mb.phase - pwm) / freqN,-1.0);                                       \
    }                                                                                       \
                                                                                            \
    else if (mb.prevSyncAmp <= 0 && mb.masterPhase > 0)                                     \
    {                                                                                       \
        mb.phase  = mb.masterPhase * (slaveFreq / masterFreq);                              \
        if (!slaveWave)                                                                     \
            mb.output = mb.masterPhase * (slaveFreq / masterFreq);                          \
        else                                                                                \
            mb.output = -1.0;                                                               \
        add_blep(&mb, mb.phase/freqN,1.0);                                                  \
    }                                                                                       \
                                                                                            \
    if (!slaveWave)                                                                         \
        y = mb.phase;                                                                       \
    else                                                                                    \
        y = mb.output;                                                                      \
                                                                                            \
    /* add BLEP buffer contents */                                                          \
    if (mb.nInit)                                                                           \
    {                                                                                       \
        y += mb.buffer[mb.iBuffer];                                                         \
        mb.nInit--;                                                                         \
        if (++mb.iBuffer >= mb.cBuffer)                                                     \
            mb.iBuffer=0;                                                                   \
    }                                                                                       \
    mb.prevSyncAmp = mb.masterPhase;                                                        \
    UGEN_OUT(out, y);                                                                       \
);                                                                                          \
*((minblep*) u.data) = mb;                                                                  \

#define SYNCOSC_0K slaveFreq  = in0[0]; freqN = slaveFreq * RECIP_SAMPLE_RATE;
#define SYNCOSC_1K slaveWave  = (int32_t)in1[0];
#define SYNCOSC_2K pwm        = CLAMP(in2[0],0,1) * 0.5;
#define SYNCOSC_3K masterFreq = in3[0];

#define SYNCOSC_0A slaveFreq  = UGEN_IN(in0); freqN = slaveFreq * RECIP_SAMPLE_RATE;
#define SYNCOSC_1A slaveWave  = (int32_t)UGEN_IN(in1);
#define SYNCOSC_2A pwm        = CLAMP(UGEN_IN(in2),0,1) * 0.5;
#define SYNCOSC_3A masterFreq = UGEN_IN(in3);

// syncosc 0
void syncosc_kkkk_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K
        SYNCOSC_1K
        SYNCOSC_2K
        SYNCOSC_3K,
        /* no audio args */
    )
}

// syncosc 1
void syncosc_akkk_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_1K
        SYNCOSC_2K
        SYNCOSC_3K,
        SYNCOSC_0A
    )
}

// syncosc 2
void syncosc_kakk_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K
        SYNCOSC_2K
        SYNCOSC_3K,
        SYNCOSC_1A
    )
}

// syncosc 3
void syncosc_aakk_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_2K
        SYNCOSC_3K,
        SYNCOSC_0A
        SYNCOSC_1A
    )
}

// syncosc 4
void syncosc_kkak_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K
        SYNCOSC_1K
        SYNCOSC_3K,
        SYNCOSC_2A
    )
}

// syncosc 5
void syncosc_akak_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_1K
        SYNCOSC_3K,
        SYNCOSC_0A
        SYNCOSC_2A
    )
}

// syncosc 6
void syncosc_kaak_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K
        SYNCOSC_3K,
        SYNCOSC_1A
        SYNCOSC_2A
    )
}

// syncosc 7
void syncosc_aaak_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_3K,
        SYNCOSC_0A
        SYNCOSC_1A
        SYNCOSC_2A
    )
}

// syncosc 8
void syncosc_kkka_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K
        SYNCOSC_1K
        SYNCOSC_2K,
        SYNCOSC_3A
    )
}

// syncosc 9
void syncosc_akka_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_1K
        SYNCOSC_2K,
        SYNCOSC_0A
        SYNCOSC_3A
    )
}

// syncosc 10
void syncosc_kaka_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K
        SYNCOSC_2K,
        SYNCOSC_1A
        SYNCOSC_3A
    )
}

// syncosc 11
void syncosc_aaka_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_2K,
        SYNCOSC_0A
        SYNCOSC_1A
        SYNCOSC_3A
    )
}

// syncosc 12
void syncosc_kkaa_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K
        SYNCOSC_1K,
        SYNCOSC_2A
        SYNCOSC_3A
    )
}

// syncosc 13
void syncosc_akaa_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_1K,
        SYNCOSC_0A
        SYNCOSC_2A
        SYNCOSC_3A
    )
}

// syncosc 14
void syncosc_kaaa_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K,
        SYNCOSC_1A
        SYNCOSC_2A
        SYNCOSC_3A
    )
}

// syncosc 15
void syncosc_aaaa_calc(ugen u)
{
    SYNCOSC_CALC(
        /* no control args */,
        SYNCOSC_0A
        SYNCOSC_1A
        SYNCOSC_2A
        SYNCOSC_3A
    )
}

//==========================================
// Randomness
//==========================================

static inline uint32_t xorshift128()
{
    static uint32_t xor_x = 1243598713U;
    static uint32_t xor_y = 3093459404U;
    static uint32_t xor_z = 1821928721U;
    static uint32_t xor_w = 1791912391U;

    uint32_t t = xor_x ^ (xor_x << 11);
    xor_x = xor_y; xor_y = xor_z; xor_z = xor_w;
    return xor_w = xor_w ^ (xor_w >> 19) ^ t ^ (t >> 8);
}

static inline uint64_t xorshift128plus()
{
    static uint64_t sx = 18446744073709551557UL;
    static uint64_t sy = 12764787846358441471UL;
    uint64_t x = sx;
    uint64_t const y = sy;
    sx = y;
    x ^= x << 23; // a
    x ^= x >> 17; // b
    x ^= y ^ (y >> 26); // c
    sy = x;
    return x + y;
}


static inline uint32_t trand()
{
    static uint32_t s1 = 1243598713U;
    static uint32_t s2 = 3093459404U;
    static uint32_t s3 = 1821928721U;
    // static uint32_t seed = 1791912391U;

    s1 = ((s1 & (uint32_t) - 2)  << 12) ^ (((s1 << 13) ^  s1) >> 19);
    s2 = ((s2 & (uint32_t) - 8)  <<  4) ^ (((s2 <<  2) ^  s2) >> 25);
    s3 = ((s3 & (uint32_t) - 16) << 17) ^ (((s3 <<  3) ^  s3) >> 11);
    return s1 ^ s2 ^ s3;
}

const long double RECIP_ULONG_MAX = (long double) 1.0 / (long double) ULONG_MAX;
const long double TWO_RECIP_ULONG_MAX = (long double) 2.0 / (long double) ULONG_MAX;
const long double QUARTER_RECIP_ULONG_MAX = (long double) 0.25 / (long double) ULONG_MAX;

#define RAND_RANGE(MIN,MAX) (((double) (((long double) xorshift128plus()) * RECIP_ULONG_MAX)) * (MAX - MIN) + MIN)
#define RAND_ONE() ((double) (((long double) xorshift128plus()) * RECIP_ULONG_MAX))
#define RAND_TWO() ((double) (((long double) xorshift128plus()) * TWO_RECIP_ULONG_MAX))
#define RAND_SIG() (((double) (((long double) xorshift128plus()) * TWO_RECIP_ULONG_MAX)) - 1.0)
#define RAND_SIG_EIGHTH() (((double) (((long double) xorshift128plus()) * QUARTER_RECIP_ULONG_MAX)) - 0.125)

typedef struct
{
    double phase;
    double value0;
    double value1;
    double value2;
    double value3;
} rand_t;

void rand_constructor(ugen* u)
{
    rand_t* rand = malloc(sizeof(rand_t));
    rand->value0 = RAND_SIG();
    rand->value1 = 0;
    rand->value2 = 0;
    rand->value3 = 0;
    rand->phase  = 0;
    u->data      = rand;
}

void rand_deconstructor(ugen* u)
{
    free(u->data);
}

void rand_range_constructor(ugen* u)
{
    double   seed  = u->constructor_args[0];
    double   min   = u->constructor_args[1];
    double   max   = u->constructor_args[2];
    double   range = max - min;
    double   in    = RAND_ONE();
    double   value = (in * range) + min;
    double*  out  = (_necronomicon_current_node_underconstruction->ugen_wires + (u->outputs[0] * BLOCK_SIZE));

    uint32_t i;
    for (i = 0; i < BLOCK_SIZE; ++i)
    {
        out[i] = value;
    }
}

void rand_range_deconstructor(ugen* u) { }
void rand_calc(ugen u) { }

#define NOISEN_CALC(CONTROL_ARGS, AUDIO_ARGS)                               \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                    \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                                   \
rand_t   rand = *((rand_t*) u.data);                                        \
double freq;                                                                \
CONTROL_ARGS                                                                \
AUDIO_LOOP(                                                                 \
    AUDIO_ARGS                                                              \
    if (rand.phase + RECIP_SAMPLE_RATE * freq >= 1.0)                       \
    {                                                                       \
        rand.phase  = fmod(rand.phase + RECIP_SAMPLE_RATE * freq,1.0);      \
        rand.value0 = RAND_SIG();                                           \
    }                                                                       \
    else                                                                    \
    {                                                                       \
        rand.phase = rand.phase + RECIP_SAMPLE_RATE * freq;                 \
    }                                                                       \
    UGEN_OUT(out, rand.value0);                                             \
);                                                                          \
*((rand_t*) u.data) = rand;                                                 \

void lfnoiseN_k_calc(ugen u)
{
    NOISEN_CALC(
        freq  = in0[0];,
        /* no audio args */
    )
}

void lfnoiseN_a_calc(ugen u)
{
    NOISEN_CALC(
        /* no control args */,
        freq  = UGEN_IN(in0);
    )
}

#define LFNOISEL_CALC(CONTROL_ARGS, AUDIO_ARGS)                             \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                    \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                                   \
rand_t   rand = *((rand_t*) u.data);                                        \
double freq;                                                                \
CONTROL_ARGS                                                                \
AUDIO_LOOP(                                                                 \
    AUDIO_ARGS                                                              \
    if (rand.phase + RECIP_SAMPLE_RATE * freq >= 1.0)                       \
    {                                                                       \
        rand.phase  = fmod(rand.phase + RECIP_SAMPLE_RATE * freq,1.0);      \
        rand.value1 = rand.value0;                                          \
        rand.value0 = RAND_SIG();                                           \
    }                                                                       \
    else                                                                    \
    {                                                                       \
        rand.phase = rand.phase + RECIP_SAMPLE_RATE * freq;                 \
    }                                                                       \
    UGEN_OUT(out, LERP(rand.value1,rand.value0,rand.phase));                \
);                                                                          \
*((rand_t*) u.data) = rand;                                                 \

void lfnoiseL_k_calc(ugen u)
{
    LFNOISEL_CALC(
        freq  = in0[0];,
        /* no audio args */
    );
}

void lfnoiseL_a_calc(ugen u)
{
    LFNOISEL_CALC(
        /* no control args */,
        freq  = UGEN_IN(in0);
    )
}

#define LFNOISEC_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                     \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                                            \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                                                           \
rand_t   rand = *((rand_t*) u.data);                                                                \
double freq;                                                                                        \
CONTROL_ARGS                                                                                        \
AUDIO_LOOP(                                                                                         \
    AUDIO_ARGS                                                                                      \
    if (rand.phase + RECIP_SAMPLE_RATE * freq >= 1.0)                                               \
    {                                                                                               \
        rand.phase  = fmod(rand.phase + RECIP_SAMPLE_RATE * freq,1.0);                              \
        rand.value3 = rand.value2;                                                                  \
        rand.value2 = rand.value1;                                                                  \
        rand.value1 = rand.value0;                                                                  \
        rand.value0 = RAND_SIG();                                                                   \
    }                                                                                               \
    else                                                                                            \
    {                                                                                               \
        rand.phase = rand.phase + RECIP_SAMPLE_RATE * freq;                                         \
    }                                                                                               \
    UGEN_OUT(out, CUBIC_INTERP(rand.value3,rand.value2,rand.value1,rand.value0,rand.phase));        \
);                                                                                                  \
*((rand_t*) u.data) = rand;                                                                         \

void lfnoiseC_k_calc(ugen u)
{
    LFNOISEC_CALC(
        freq  = in0[0];,
        /* no audio args */
    );
}

void lfnoiseC_a_calc(ugen u)
{
    LFNOISEC_CALC(
        /* no control args */,
        freq  = UGEN_IN(in0);
    )
}

#define RANGE_CALC(CONTROL_ARGS, AUDIO_ARGS)        \
double* in0  = UGEN_INPUT_BUFFER(u, 0);             \
double* in1  = UGEN_INPUT_BUFFER(u, 1);             \
double* in2  = UGEN_INPUT_BUFFER(u, 2);             \
double* out  = UGEN_OUTPUT_BUFFER(u, 0);            \
double  min, max;                                   \
double  mul, add;                                   \
double  x;                                          \
CONTROL_ARGS                                        \
AUDIO_LOOP(                                         \
    AUDIO_ARGS                                      \
    mul = (max - min) * 0.5;                        \
    add = mul + min;                                \
    UGEN_OUT(out, (x * mul) + add);                 \
);                                                  \


#define RANGE_MINK min = in0[0];
#define RANGE_MAXK max = in1[0];
#define RANGE_XK x = CLAMP(in2[0], -1, 1);

#define RANGE_MINA min = UGEN_IN(in0);
#define RANGE_MAXA max = UGEN_IN(in1);
#define RANGE_XA x = CLAMP(UGEN_IN(in2), -1, 1);

// 0
void range_kkk_calc(ugen u)
{
    double* in0  = UGEN_INPUT_BUFFER(u, 0);
    double* in1  = UGEN_INPUT_BUFFER(u, 1);
    double* in2  = UGEN_INPUT_BUFFER(u, 2);
    double* out  = UGEN_OUTPUT_BUFFER(u, 0);
    double  RANGE_MINK
    double  RANGE_MAXK
    double  RANGE_XK
    double  mul  = (max - min) * 0.5;
    double  add  = mul + min;
    *out         = (x * mul) + add;
}

// 1
void range_akk_calc(ugen u)
{
    RANGE_CALC(
        RANGE_MAXK // Control Args
        RANGE_XK,
        RANGE_MINA
    )
}

// 2
void range_kak_calc(ugen u)
{
    RANGE_CALC(
        RANGE_MINK // Control Args
        RANGE_XK, // Audio Args
        RANGE_MAXA
    )
}

// 3
void range_aak_calc(ugen u)
{
    RANGE_CALC(
        RANGE_XK, // Control Args
        RANGE_MINA // Audio Args
        RANGE_MAXA
    )
}

// 4
void range_kka_calc(ugen u)
{
    RANGE_CALC(
        RANGE_MINK // Control Args
        RANGE_MAXK, // Audio Args
        RANGE_XA
    )
}

// 5
void range_aka_calc(ugen u)
{
    RANGE_CALC(
        RANGE_MAXK, // Control Args
        RANGE_MINA // Audio Args
        RANGE_XA
    )
}

// 6
void range_kaa_calc(ugen u)
{
    RANGE_CALC(
        RANGE_MINK, // Control Args
        RANGE_MAXA // Audio Args
        RANGE_XA
    )
}

// 7
void range_aaa_calc(ugen u)
{
    RANGE_CALC(
        /* no control args */,
        RANGE_MINA // Audio Args
        RANGE_MAXA
        RANGE_XA
    )
}

#define EXPRANGE_CALC(CONTROL_ARGS, AUDIO_ARGS)                 \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                          \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                          \
double* in2 = UGEN_INPUT_BUFFER(u, 2);                          \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                         \
double  min, max;                                               \
double     x;                                                   \
CONTROL_ARGS                                                    \
AUDIO_LOOP(                                                     \
    AUDIO_ARGS                                                  \
    UGEN_OUT(out, pow(max / min, x * 0.5 + 0.5) * min);         \
);                                                              \

#define EXPRANGE_MINK min = in0[0];
#define EXPRANGE_MAXK max = in1[0];
#define EXPRANGE_XK x = CLAMP(in2[0], -1, 1);

#define EXPRANGE_MINA min = UGEN_IN(in0);
#define EXPRANGE_MAXA max = UGEN_IN(in1);
#define EXPRANGE_XA x = CLAMP(UGEN_IN(in2), -1, 1);

// 0
void exprange_kkk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* in2 = UGEN_INPUT_BUFFER(u, 2);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double  EXPRANGE_MINK;
    double  EXPRANGE_MAXK;
    double     EXPRANGE_XK;
    *out = pow(max / min, x * 0.5 + 0.5) * min;
}

// 1
void exprange_akk_calc(ugen u)
{
    EXPRANGE_CALC(
        EXPRANGE_MAXK
        EXPRANGE_XK,
        EXPRANGE_MINA
    )
}

// 2
void exprange_kak_calc(ugen u)
{
    EXPRANGE_CALC(
        EXPRANGE_MINK
        EXPRANGE_XK,
        EXPRANGE_MAXA
    )
}

// 3
void exprange_aak_calc(ugen u)
{
    EXPRANGE_CALC(
        EXPRANGE_XK,
        EXPRANGE_MINA
        EXPRANGE_MAXA
    )
}

// 4
void exprange_kka_calc(ugen u)
{
    EXPRANGE_CALC(
        EXPRANGE_MINK
        EXPRANGE_MAXK,
        EXPRANGE_XA
    )
}

// 5
void exprange_aka_calc(ugen u)
{
    EXPRANGE_CALC(
        EXPRANGE_MAXK,
        EXPRANGE_MINA
        EXPRANGE_XA
    )
}

// 6
void exprange_kaa_calc(ugen u)
{
    EXPRANGE_CALC(
        EXPRANGE_MINK,
        EXPRANGE_MAXA
        EXPRANGE_XA
    )
}

// 7
void exprange_aaa_calc(ugen u)
{
    EXPRANGE_CALC(
        /* no control args */,
        EXPRANGE_MINA
        EXPRANGE_MAXA
        EXPRANGE_XA
    )
}

#define IMPULSE_CALC(CONTROL_ARGS, AUDIO_ARGS)      \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);            \
/* double*  in1  = UGEN_INPUT_BUFFER(u, 1); */      \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);           \
double  phase = (*(double*)u.data);                 \
double freq;                                        \
/* Offset is unused at the moment... */             \
/* double offset; */                                \
CONTROL_ARGS                                        \
AUDIO_LOOP(                                         \
    AUDIO_ARGS                                      \
    phase += freq * RECIP_SAMPLE_RATE;              \
    if (phase >= 1)                                 \
        UGEN_OUT(out,1);                            \
    else                                            \
        UGEN_OUT(out,0);                            \
    phase = fmod(phase,1);                          \
);                                                  \
(*(double*)u.data) = phase;                         \

void impulse_aa_calc(ugen u)
{
    IMPULSE_CALC(
        /* no control args */,
        freq = UGEN_IN(in0); // Audio args
        /* offset = UGEN_IN(in1); */
    );
}

void impulse_ak_calc(ugen u)
{
    IMPULSE_CALC(
        /*offset = in1[0];*/, // Control arg
        freq = UGEN_IN(in0); // Audio arg
    );
}

void impulse_ka_calc(ugen u)
{
    IMPULSE_CALC(
        freq = in0[0];, // Control arg
        /*offset = UGEN_IN(in1);*/ // Audio arg
    );
}

void impulse_kk_calc(ugen u)
{
    IMPULSE_CALC(
        freq = in0[0]; // Control args
        /*offset = in1[0]*/,
        /* no audio args */
    );
}

typedef struct
{
    double phase;
    double period;
} dust_t;

void dust_constructor(ugen* u)
{
    dust_t* dust = malloc(sizeof(dust_t));
    dust->phase  = 0;
    dust->period = -1;
    u->data = dust;
}

void dust_deconstructor(ugen* u)
{
    free(u->data);
}

#define DUST_CALC(CONTROL_ARGS, AUDIO_ARGS)                             \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);                                 \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);                                \
dust_t  dust = *((dust_t*)u.data);                                      \
if (dust.period == -1)                                                  \
    dust.period = RAND_TWO();                                           \
double density;                                                         \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
    if (dust.phase + density * RECIP_SAMPLE_RATE >= dust.period)        \
    {                                                                   \
        dust.phase  = 0;                                                \
        dust.period = RAND_TWO();                                       \
        UGEN_OUT(out,RAND_SIG());                                       \
    }                                                                   \
    else                                                                \
    {                                                                   \
        dust.phase = dust.phase + density * RECIP_SAMPLE_RATE;          \
        UGEN_OUT(out,0);                                                \
    }                                                                   \
);                                                                      \
*((dust_t*)u.data) = dust;                                              \

void dust_k_calc(ugen u)
{
    DUST_CALC(
        density = *in0;,
        /* no audio args */
    )
}

void dust_a_calc(ugen u)
{
    DUST_CALC(
        /* no control args */,
        density = UGEN_IN(in0);
    )
}

#define DUST2_CALC(CONTROL_ARGS, AUDIO_ARGS)                            \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);                                 \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);                                \
dust_t  dust = *((dust_t*)u.data);                                      \
if (dust.period == -1)                                                  \
    dust.period = RAND_TWO();                                           \
double density;                                                         \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
    if (dust.phase + density * RECIP_SAMPLE_RATE >= dust.period)        \
    {                                                                   \
        dust.phase  = 0;                                                \
        dust.period = RAND_TWO();                                       \
        UGEN_OUT(out,RAND_ONE());                                       \
    }                                                                   \
    else                                                                \
    {                                                                   \
        dust.phase = dust.phase + density * RECIP_SAMPLE_RATE;          \
        UGEN_OUT(out,0);                                                \
    }                                                                   \
);                                                                      \
*((dust_t*)u.data) = dust;                                              \

void dust2_k_calc(ugen u)
{
    DUST2_CALC(
        density = *in0;,
        /* no audio args */
    )
}

void dust2_a_calc(ugen u)
{
    DUST2_CALC(
        /* no control args */,
        density = UGEN_IN(in0);
    )
}

//===================================
// RBJ Filters, Audio EQ Cookbook
//===================================

static inline double zapgremlins(double x)
{
    double absx = fabs(x);
    // very small numbers fail the first test, eliminating denormalized numbers
    //    (zero also fails the first test, but that is OK since it returns zero.)
    // very large numbers fail the second test, eliminating infinities
    // Not-a-Numbers fail both tests and are eliminated.
    return (absx > 1e-63 && absx < 1e63) ? x : 0.0;
}

typedef struct
{
    double x1;
    double x2;
    double y1;
    double y2;

    double prevF;
    double prevQ;
    double cs;
    double alpha;

} biquad_t;

void biquad_constructor(ugen* u)
{
    biquad_t* biquad = malloc(sizeof(biquad_t));
    biquad->x1       = 0;
    biquad->x2       = 0;
    biquad->y1       = 0;
    biquad->y2       = 0;
    biquad->prevF    = 0;
    biquad->prevQ    = 0;
    biquad->cs       = 0;
    biquad->alpha    = 0;
    u->data          = biquad;
}

void biquad_deconstructor(ugen* u)
{
    free(u->data);
}

#define BIQUAD(B0,B1,B2,A0,A1,A2,X,X1,X2,Y1,Y2) ( (B0/A0)*X + (B1/A0)*X1 + (B2/A0)*X2 - (A1/A0)*Y1 - (A2/A0)*Y2 )

#define TABLE_LOOKUP(VAL, TABLE)                                    \
({                                                                  \
    const double v1 = TABLE[((uint16_t) (VAL * TABLE_SIZE))];       \
    const double v2 = TABLE[((uint16_t) (VAL * TABLE_SIZE + 1))];   \
    const double delta  = VAL - ((uint16_t) VAL);                   \
    v1 + delta * (v2 - v1);                                         \
})

#define TABLE_SIN(x) (TABLE_LOOKUP(x, sine_table))
#define TABLE_COS(x) (TABLE_LOOKUP(x, cosn_table))
#define TABLE_SINH(x) (TABLE_LOOKUP(x, sinh_table))
#define TABLE_ATAN(x) (TABLE_LOOKUP(x, atan_table))
#define TABLE_TANH(x) (TABLE_LOOKUP(x, tanh_table))

#define LPF_CALC(CONTROL_ARGS, AUDIO_ARGS)                              \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double q;                                                               \
double in;                                                              \
                                                                        \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double alpha;                                                           \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
double snhi;                                                            \
                                                                        \
CONTROL_ARGS                                                            \
                                                                        \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
                                                                        \
    if (freq != bi.prevF || q != bi.prevQ)                              \
    {                                                                   \
        /* branchless max? */                                           \
        q     = MAX(q,0.00000001);                                      \
        bi.prevF = freq;                                                \
        bi.prevQ = q;                                                   \
        /* Don't recalc if unnecessary */                               \
        omega  = freq * RECIP_SAMPLE_RATE;                              \
        bi.cs  = TABLE_COS(omega);                                      \
        sn     = TABLE_SIN(omega);                                      \
        snhi   = (1 / (2 * q));                                         \
        snhi   = snhi * RECIP_TWO_PI;                                   \
        bi.alpha = TABLE_SINH(snhi);                                    \
        bi.alpha *= sn;                                                 \
    }                                                                   \
    cs    = bi.cs;                                                      \
    alpha = bi.alpha;                                                   \
                                                                        \
    b0    = (1 - cs) * 0.5;                                             \
    b1    =  1 - cs;                                                    \
    b2    = (1 - cs) * 0.5;                                             \
    a0    =  1 + alpha;                                                 \
    a1    = -2 * cs;                                                    \
    a2    =  1 - alpha;                                                 \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define LPF_FREQK freq = *in0;
#define LPF_FREQA freq = UGEN_IN(in0);
#define LPF_QK q = *in1;
#define LPF_QA q = UGEN_IN(in1);
#define LPF_INK in = *in2;
#define LPF_INA in = UGEN_IN(in2);


// 0
void lpf_kkk_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        LPF_FREQK             /* 0 */
        LPF_QK                /* 1 */
        LPF_INK               /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void lpf_akk_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        LPF_QK                /* 1 */
        LPF_INK               /* 2 */,
        // Audio Arguments
        LPF_FREQA             /* 0 */
    )
}

// 2
void lpf_kak_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        LPF_FREQK             /* 0 */
        LPF_INK               /* 2 */,
        // Audio Arguments
        LPF_QA                /* 1 */
    )
}

// 3
void lpf_aak_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        LPF_INK               /* 2 */,
        // Audio Arguments
        LPF_FREQA             /* 0 */
        LPF_QA                /* 1 */
    )
}

// 4
void lpf_kka_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        LPF_FREQK             /* 0 */
        LPF_QK                /* 1 */,
        // Audio Arguments
        LPF_INA               /* 2 */
    )
}

// 5
void lpf_aka_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        LPF_QK                /* 1 */,
        // Audio Arguments
        LPF_FREQA             /* 0 */
        LPF_INA               /* 2 */
    )
}

// 6
void lpf_kaa_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        LPF_FREQK             /* 0 */,
        // Audio Arguments
        LPF_QA                /* 1 */
        LPF_INA               /* 2 */
    )
}

// 7
void lpf_aaa_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        LPF_FREQA             /* 0 */
        LPF_QA                /* 1 */
        LPF_INA               /* 2 */
    )
}

#define HPF_CALC(CONTROL_ARGS, AUDIO_ARGS)                              \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double q;                                                               \
double in;                                                              \
                                                                        \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double alpha;                                                           \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
double sinh_i;                                                          \
                                                                        \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
                                                                        \
    omega = freq * TWO_PI_TIMES_RECIP_SAMPLE_RATE;                      \
    cs    = TABLE_COS(omega);                                           \
    sn    = TABLE_SIN(omega);                                           \
    alpha = sn * TABLE_SINH(1 / (2 * q));                               \
                                                                        \
    b0    = (1 + cs) * 0.5;                                             \
    b1    = -1 - cs;                                                    \
    b2    = (1 + cs) * 0.5;                                             \
    a0    =  1 + alpha;                                                 \
    a1    = -2*cs;                                                      \
    a2    =  1 - alpha;                                                 \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define HPF_FREQK freq = *in0;
#define HPF_FREQA freq = UGEN_IN(in0);
#define HPF_QK q = *in1;
#define HPF_QA q = UGEN_IN(in1);
#define HPF_INK in = *in2;
#define HPF_INA in = UGEN_IN(in2);


// 0
void hpf_kkk_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        HPF_FREQK             /* 0 */
        HPF_QK                /* 1 */
        HPF_INK               /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void hpf_akk_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        HPF_QK                /* 1 */
        HPF_INK               /* 2 */,
        // Audio Arguments
        HPF_FREQA             /* 0 */
    )
}

// 2
void hpf_kak_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        HPF_FREQK             /* 0 */
        HPF_INK               /* 2 */,
        // Audio Arguments
        HPF_QA                /* 1 */
    )
}

// 3
void hpf_aak_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        HPF_INK               /* 2 */,
        // Audio Arguments
        HPF_FREQA             /* 0 */
        HPF_QA                /* 1 */
    )
}

// 4
void hpf_kka_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        HPF_FREQK             /* 0 */
        HPF_QK                /* 1 */,
        // Audio Arguments
        HPF_INA               /* 2 */
    )
}

// 5
void hpf_aka_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        HPF_QK                /* 1 */,
        // Audio Arguments
        HPF_FREQA             /* 0 */
        HPF_INA               /* 2 */
    )
}

// 6
void hpf_kaa_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        HPF_FREQK             /* 0 */,
        // Audio Arguments
        HPF_QA                /* 1 */
        HPF_INA               /* 2 */
    )
}

// 7
void hpf_aaa_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        HPF_FREQA             /* 0 */
        HPF_QA                /* 1 */
        HPF_INA               /* 2 */
    )
}

#define BPF_CALC(CONTROL_ARGS, AUDIO_ARGS)                              \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double q;                                                               \
double in;                                                              \
                                                                        \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double alpha;                                                           \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
                                                                        \
    omega = freq * TWO_PI_TIMES_RECIP_SAMPLE_RATE;                      \
    cs    = TABLE_COS(omega);                                           \
    sn    = TABLE_SIN(omega);                                           \
    alpha = sn * sinh(1 / (2 * q));                                     \
                                                                        \
    b0    =  alpha;                                                     \
    b1    =  0;                                                         \
    b2    = -alpha;                                                     \
    a0    =  1 + alpha;                                                 \
    a1    = -2*cs;                                                      \
    a2    =  1 - alpha;                                                 \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define BPF_FREQK freq = *in0;
#define BPF_FREQA freq = UGEN_IN(in0);
#define BPF_QK q = *in1;
#define BPF_QA q = UGEN_IN(in1);
#define BPF_INK in = *in2;
#define BPF_INA in = UGEN_IN(in2);

// 0
void bpf_kkk_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        BPF_FREQK             /* 0 */
        BPF_QK                /* 1 */
        BPF_INK               /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void bpf_akk_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        BPF_QK                /* 1 */
        BPF_INK               /* 2 */,
        // Audio Arguments
        BPF_FREQA             /* 0 */
    )
}

// 2
void bpf_kak_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        BPF_FREQK             /* 0 */
        BPF_INK               /* 2 */,
        // Audio Arguments
        BPF_QA                /* 1 */
    )
}

// 3
void bpf_aak_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        BPF_INK               /* 2 */,
        // Audio Arguments
        BPF_FREQA             /* 0 */
        BPF_QA                /* 1 */
    )
}

// 4
void bpf_kka_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        BPF_FREQK             /* 0 */
        BPF_QK                /* 1 */,
        // Audio Arguments
        BPF_INA               /* 2 */
    )
}

// 5
void bpf_aka_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        BPF_QK                /* 1 */,
        // Audio Arguments
        BPF_FREQA             /* 0 */
        BPF_INA               /* 2 */
    )
}

// 6
void bpf_kaa_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        BPF_FREQK             /* 0 */,
        // Audio Arguments
        BPF_QA                /* 1 */
        BPF_INA               /* 2 */
    )
}

// 7
void bpf_aaa_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        BPF_FREQA             /* 0 */
        BPF_QA                /* 1 */
        BPF_INA               /* 2 */
    )
}

#define NOTCH_CALC(CONTROL_ARGS, AUDIO_ARGS)                            \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  in3  = UGEN_INPUT_BUFFER(u, 3);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double gain;                                                            \
double q;                                                               \
double in;                                                              \
                                                                        \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double alpha;                                                           \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
                                                                        \
    omega = freq * TWO_PI_TIMES_RECIP_SAMPLE_RATE;                      \
    cs    = TABLE_COS(omega);                                           \
    sn    = TABLE_SIN(omega);                                           \
    alpha = sn * sinh(1 / (2 * q));                                     \
                                                                        \
    b0    =  1;                                                         \
    b1    = -2*cs;                                                      \
    b2    =  1;                                                         \
    a0    =  1 + alpha;                                                 \
    a1    = -2*cs;                                                      \
    a2    =  1 - alpha;                                                 \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define NOTCH_FREQK freq = *in0;
#define NOTCH_FREQA freq = UGEN_IN(in0);
#define NOTCH_GAINK gain = *in1;
#define NOTCH_GAINA gain = UGEN_IN(in1);
#define NOTCH_QK q = *in2;
#define NOTCH_QA q = UGEN_IN(in2);
#define NOTCH_INK in = *in3;
#define NOTCH_INA in = UGEN_IN(in3);

// 0
void notch_kkkk_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */
        NOTCH_GAINK           /* 1 */
        NOTCH_QK              /* 2 */
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void notch_akkk_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_GAINK           /* 1 */
        NOTCH_QK              /* 2 */
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
    )
}

// 2
void notch_kakk_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */
        NOTCH_QK              /* 2 */
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        NOTCH_GAINA           /* 1 */
    )
}

// 3
void notch_aakk_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_QK              /* 2 */
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
        NOTCH_GAINA           /* 1 */
    )
}

// 4
void notch_kkak_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */
        NOTCH_GAINK           /* 1 */
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        NOTCH_QA              /* 2 */
    )
}

// 5
void notch_akak_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_GAINK           /* 1 */
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
        NOTCH_QA              /* 2 */
    )
}

// 6
void notch_kaak_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        NOTCH_GAINA           /* 1 */
        NOTCH_QA              /* 2 */
    )
}

// 7
void notch_aaak_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
        NOTCH_GAINA           /* 1 */
        NOTCH_QA              /* 2 */
    )
}

// 8
void notch_kkka_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */
        NOTCH_GAINK           /* 1 */
        NOTCH_QK              /* 2 */,
        // Audio Arguments
        NOTCH_INA             /* 3 */
    )
}

// 9
void notch_akka_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_GAINK           /* 1 */
        NOTCH_QK              /* 2 */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
        NOTCH_INA             /* 3 */
    )
}

// 10
void notch_kaka_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */
        NOTCH_QK              /* 2 */,
        // Audio Arguments
        NOTCH_GAINA           /* 1 */
        NOTCH_INA             /* 3 */
    )
}

// 11
void notch_aaka_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_QK              /* 2 */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
        NOTCH_GAINA           /* 1 */
        NOTCH_INA             /* 3 */
    )
}

// 12
void notch_kkaa_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */
        NOTCH_GAINK           /* 1 */,
        // Audio Arguments
        NOTCH_QA              /* 2 */
        NOTCH_INA             /* 3 */
    )
}

// 13
void notch_akaa_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_GAINK           /* 1 */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
        NOTCH_QA              /* 2 */
        NOTCH_INA             /* 3 */
    )
}

// 14
void notch_kaaa_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */,
        // Audio Arguments
        NOTCH_GAINA           /* 1 */
        NOTCH_QA              /* 2 */
        NOTCH_INA             /* 3 */
    )
}

// 15
void notch_aaaa_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
        NOTCH_GAINA           /* 1 */
        NOTCH_QA              /* 2 */
        NOTCH_INA             /* 3 */
    )
}

#define ALLPASS_CALC(CONTROL_ARGS, AUDIO_ARGS)                          \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double q;                                                               \
double in;                                                              \
                                                                        \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double alpha;                                                           \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
                                                                        \
    omega = freq * TWO_PI_TIMES_RECIP_SAMPLE_RATE;                      \
    cs    = TABLE_COS(omega);                                           \
    sn    = TABLE_SIN(omega);                                           \
    alpha = sn * sinh(1 / (2 * q));                                     \
                                                                        \
    b0    =   1 - alpha;                                                \
    b1    =  -2 * cs;                                                   \
    b2    =   1 + alpha;                                                \
    a0    =   1 + alpha;                                                \
    a1    =  -2 * cs;                                                   \
    a2    =   1 - alpha;                                                \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define ALLPASS_FREQK freq = *in0;
#define ALLPASS_FREQA freq = UGEN_IN(in0);
#define ALLPASS_QK q = *in1;
#define ALLPASS_QA q = UGEN_IN(in1);
#define ALLPASS_INK in = *in2;
#define ALLPASS_INA in = UGEN_IN(in2);

// 0
void allpass_kkk_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        ALLPASS_FREQK         /* 0 */
        ALLPASS_QK            /* 1 */
        ALLPASS_INK           /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void allpass_akk_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        ALLPASS_QK            /* 1 */
        ALLPASS_INK           /* 2 */,
        // Audio Arguments
        ALLPASS_FREQA         /* 0 */
    )
}

// 2
void allpass_kak_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        ALLPASS_FREQK         /* 0 */
        ALLPASS_INK           /* 2 */,
        // Audio Arguments
        ALLPASS_QA            /* 1 */
    )
}

// 3
void allpass_aak_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        ALLPASS_INK           /* 2 */,
        // Audio Arguments
        ALLPASS_FREQA         /* 0 */
        ALLPASS_QA            /* 1 */
    )
}

// 4
void allpass_kka_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        ALLPASS_FREQK         /* 0 */
        ALLPASS_QK            /* 1 */,
        // Audio Arguments
        ALLPASS_INA           /* 2 */
    )
}

// 5
void allpass_aka_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        ALLPASS_QK            /* 1 */,
        // Audio Arguments
        ALLPASS_FREQA         /* 0 */
        ALLPASS_INA           /* 2 */
    )
}

// 6
void allpass_kaa_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        ALLPASS_FREQK         /* 0 */,
        // Audio Arguments
        ALLPASS_QA            /* 1 */
        ALLPASS_INA           /* 2 */
    )
}

// 7
void allpass_aaa_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        ALLPASS_FREQA         /* 0 */
        ALLPASS_QA            /* 1 */
        ALLPASS_INA           /* 2 */
    )
}


#define PEAKEQ_CALC(CONTROL_ARGS, AUDIO_ARGS)                           \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  in3  = UGEN_INPUT_BUFFER(u, 3);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double gain;                                                            \
double q;                                                               \
double in;                                                              \
                                                                        \
double a;                                                               \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double alpha;                                                           \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
                                                                        \
    a     = pow(10,(gain/40));                                          \
    omega = freq * TWO_PI_TIMES_RECIP_SAMPLE_RATE;                      \
    cs    = TABLE_COS(omega);                                           \
    sn    = TABLE_SIN(omega);                                           \
    alpha = sn * sinh(1 / (2 * q));                                     \
                                                                        \
    b0    =  1 + alpha*a;                                               \
    b1    = -2*cs;                                                      \
    b2    =  1 - alpha*a;                                               \
    a0    =  1 + alpha/a;                                               \
    a1    = -2*cs;                                                      \
    a2    =  1 - alpha/a;                                               \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define PEAKEQ_FREQK freq = *in0;
#define PEAKEQ_FREQA freq = UGEN_IN(in0);
#define PEAKEQ_GAINK gain = *in1;
#define PEAKEQ_GAINA gain = UGEN_IN(in1);
#define PEAKEQ_QK q = *in2;
#define PEAKEQ_QA q = UGEN_IN(in2);
#define PEAKEQ_INK in = *in3;
#define PEAKEQ_INA in = UGEN_IN(in3);


// 0
void peakEQ_kkkk_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */
        PEAKEQ_GAINK          /* 1 */
        PEAKEQ_QK             /* 2 */
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void peakEQ_akkk_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_GAINK          /* 1 */
        PEAKEQ_QK             /* 2 */
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
    )
}

// 2
void peakEQ_kakk_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */
        PEAKEQ_QK             /* 2 */
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        PEAKEQ_GAINA          /* 1 */
    )
}

// 3
void peakEQ_aakk_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_QK             /* 2 */
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
        PEAKEQ_GAINA          /* 1 */
    )
}

// 4
void peakEQ_kkak_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */
        PEAKEQ_GAINK          /* 1 */
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        PEAKEQ_QA             /* 2 */
    )
}

// 5
void peakEQ_akak_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_GAINK          /* 1 */
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
        PEAKEQ_QA             /* 2 */
    )
}

// 6
void peakEQ_kaak_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        PEAKEQ_GAINA          /* 1 */
        PEAKEQ_QA             /* 2 */
    )
}

// 7
void peakEQ_aaak_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
        PEAKEQ_GAINA          /* 1 */
        PEAKEQ_QA             /* 2 */
    )
}

// 8
void peakEQ_kkka_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */
        PEAKEQ_GAINK          /* 1 */
        PEAKEQ_QK             /* 2 */,
        // Audio Arguments
        PEAKEQ_INA            /* 3 */
    )
}

// 9
void peakEQ_akka_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_GAINK          /* 1 */
        PEAKEQ_QK             /* 2 */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
        PEAKEQ_INA            /* 3 */
    )
}

// 10
void peakEQ_kaka_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */
        PEAKEQ_QK             /* 2 */,
        // Audio Arguments
        PEAKEQ_GAINA          /* 1 */
        PEAKEQ_INA            /* 3 */
    )
}

// 11
void peakEQ_aaka_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_QK             /* 2 */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
        PEAKEQ_GAINA          /* 1 */
        PEAKEQ_INA            /* 3 */
    )
}

// 12
void peakEQ_kkaa_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */
        PEAKEQ_GAINK          /* 1 */,
        // Audio Arguments
        PEAKEQ_QA             /* 2 */
        PEAKEQ_INA            /* 3 */
    )
}

// 13
void peakEQ_akaa_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_GAINK          /* 1 */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
        PEAKEQ_QA             /* 2 */
        PEAKEQ_INA            /* 3 */
    )
}

// 14
void peakEQ_kaaa_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */,
        // Audio Arguments
        PEAKEQ_GAINA          /* 1 */
        PEAKEQ_QA             /* 2 */
        PEAKEQ_INA            /* 3 */
    )
}

// 15
void peakEQ_aaaa_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
        PEAKEQ_GAINA          /* 1 */
        PEAKEQ_QA             /* 2 */
        PEAKEQ_INA            /* 3 */
    )
}

#define LOWSHELF_CALC(CONTROL_ARGS, AUDIO_ARGS)                         \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  in3  = UGEN_INPUT_BUFFER(u, 3);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double gain;                                                            \
double slope;                                                           \
double in;                                                              \
                                                                        \
double a;                                                               \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double beta;                                                            \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
                                                                        \
AUDIO_LOOP(                                                             \
    freq  = UGEN_IN(in0);                                               \
    gain  = UGEN_IN(in1);                                               \
    slope = UGEN_IN(in2);                                               \
    in    = UGEN_IN(in3);                                               \
                                                                        \
    a     = pow(10,(gain/40));                                          \
    omega = freq * TWO_PI_TIMES_RECIP_SAMPLE_RATE;                      \
    cs    = TABLE_COS(omega);                                           \
    sn    = TABLE_SIN(omega);                                           \
    beta  = sqrt( (pow(a,2) + 1) / slope - pow((a-1),2) );              \
                                                                        \
    b0    =    a*( (a+1) - (a-1)*cs + beta*sn );                        \
    b1    =  2*a*( (a-1) - (a+1)*cs           );                        \
    b2    =    a*( (a+1) - (a-1)*cs - beta*sn );                        \
    a0    =        (a+1) + (a-1)*cs + beta*sn;                          \
    a1    =   -2*( (a-1) + (a+1)*cs           );                        \
    a2    =        (a+1) + (a-1)*cs - beta*sn;                          \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define LOWSHELF_FREQK freq = *in0;
#define LOWSHELF_FREQA freq = UGEN_IN(in0);
#define LOWSHELF_GAINK gain = *in1;
#define LOWSHELF_GAINA gain = UGEN_IN(in1);
#define LOWSHELF_SLOPEK slope = *in2;
#define LOWSHELF_SLOPEA slope = UGEN_IN(in2);
#define LOWSHELF_INK in = *in3;
#define LOWSHELF_INA in = UGEN_IN(in3);

// 0
void lowshelf_kkkk_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */
        LOWSHELF_GAINK        /* 1 */
        LOWSHELF_SLOPEK       /* 2 */
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void lowshelf_akkk_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_GAINK        /* 1 */
        LOWSHELF_SLOPEK       /* 2 */
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
    )
}

// 2
void lowshelf_kakk_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */
        LOWSHELF_SLOPEK       /* 2 */
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        LOWSHELF_GAINA        /* 1 */
    )
}

// 3
void lowshelf_aakk_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_SLOPEK       /* 2 */
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
        LOWSHELF_GAINA        /* 1 */
    )
}

// 4
void lowshelf_kkak_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */
        LOWSHELF_GAINK        /* 1 */
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        LOWSHELF_SLOPEA       /* 2 */
    )
}

// 5
void lowshelf_akak_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_GAINK        /* 1 */
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
        LOWSHELF_SLOPEA       /* 2 */
    )
}

// 6
void lowshelf_kaak_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        LOWSHELF_GAINA        /* 1 */
        LOWSHELF_SLOPEA       /* 2 */
    )
}

// 7
void lowshelf_aaak_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
        LOWSHELF_GAINA        /* 1 */
        LOWSHELF_SLOPEA       /* 2 */
    )
}

// 8
void lowshelf_kkka_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */
        LOWSHELF_GAINK        /* 1 */
        LOWSHELF_SLOPEK       /* 2 */,
        // Audio Arguments
        LOWSHELF_INA          /* 3 */
    )
}

// 9
void lowshelf_akka_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_GAINK        /* 1 */
        LOWSHELF_SLOPEK       /* 2 */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
        LOWSHELF_INA          /* 3 */
    )
}

// 10
void lowshelf_kaka_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */
        LOWSHELF_SLOPEK       /* 2 */,
        // Audio Arguments
        LOWSHELF_GAINA        /* 1 */
        LOWSHELF_INA          /* 3 */
    )
}

// 11
void lowshelf_aaka_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_SLOPEK       /* 2 */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
        LOWSHELF_GAINA        /* 1 */
        LOWSHELF_INA          /* 3 */
    )
}

// 12
void lowshelf_kkaa_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */
        LOWSHELF_GAINK        /* 1 */,
        // Audio Arguments
        LOWSHELF_SLOPEA       /* 2 */
        LOWSHELF_INA          /* 3 */
    )
}

// 13
void lowshelf_akaa_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_GAINK        /* 1 */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
        LOWSHELF_SLOPEA       /* 2 */
        LOWSHELF_INA          /* 3 */
    )
}

// 14
void lowshelf_kaaa_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */,
        // Audio Arguments
        LOWSHELF_GAINA        /* 1 */
        LOWSHELF_SLOPEA       /* 2 */
        LOWSHELF_INA          /* 3 */
    )
}

// 15
void lowshelf_aaaa_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
        LOWSHELF_GAINA        /* 1 */
        LOWSHELF_SLOPEA       /* 2 */
        LOWSHELF_INA          /* 3 */
    )
}

#define HIGHSHELF_CALC(CONTROL_ARGS, AUDIO_ARGS)                        \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  in3  = UGEN_INPUT_BUFFER(u, 3);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double gain;                                                            \
double slope;                                                           \
double in;                                                              \
                                                                        \
double a;                                                               \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double beta;                                                            \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
                                                                        \
    a     = pow(10,(gain/40));                                          \
    omega = freq * TWO_PI_TIMES_RECIP_SAMPLE_RATE;                      \
    cs    = TABLE_COS(omega);                                           \
    sn    = TABLE_SIN(omega);                                           \
    beta  = sqrt( (pow(a,2) + 1) / slope - pow((a-1),2) );              \
                                                                        \
    b0    =    a*( (a+1) + (a-1)*cs + beta*sn );                        \
    b1    = -2*a*( (a-1) + (a+1)*cs           );                        \
    b2    =    a*( (a+1) + (a-1)*cs - beta*sn );                        \
    a0    =        (a+1) - (a-1)*cs + beta*sn;                          \
    a1    =    2*( (a-1) - (a+1)*cs           );                        \
    a2    =        (a+1) - (a-1)*cs - beta*sn;                          \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define HIGHSHELF_FREQK freq = *in0;
#define HIGHSHELF_FREQA freq = UGEN_IN(in0);
#define HIGHSHELF_GAINK gain = *in1;
#define HIGHSHELF_GAINA gain = UGEN_IN(in1);
#define HIGHSHELF_SLOPEK slope = *in2;
#define HIGHSHELF_SLOPEA slope = UGEN_IN(in2);
#define HIGHSHELF_INK in = *in3;
#define HIGHSHELF_INA in = UGEN_IN(in3);

// 0
void highshelf_kkkk_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */
        HIGHSHELF_GAINK       /* 1 */
        HIGHSHELF_SLOPEK      /* 2 */
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void highshelf_akkk_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_GAINK       /* 1 */
        HIGHSHELF_SLOPEK      /* 2 */
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
    )
}

// 2
void highshelf_kakk_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */
        HIGHSHELF_SLOPEK      /* 2 */
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        HIGHSHELF_GAINA       /* 1 */
    )
}

// 3
void highshelf_aakk_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_SLOPEK      /* 2 */
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
        HIGHSHELF_GAINA       /* 1 */
    )
}

// 4
void highshelf_kkak_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */
        HIGHSHELF_GAINK       /* 1 */
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        HIGHSHELF_SLOPEA      /* 2 */
    )
}

// 5
void highshelf_akak_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_GAINK       /* 1 */
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
        HIGHSHELF_SLOPEA      /* 2 */
    )
}

// 6
void highshelf_kaak_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        HIGHSHELF_GAINA       /* 1 */
        HIGHSHELF_SLOPEA      /* 2 */
    )
}

// 7
void highshelf_aaak_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
        HIGHSHELF_GAINA       /* 1 */
        HIGHSHELF_SLOPEA      /* 2 */
    )
}

// 8
void highshelf_kkka_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */
        HIGHSHELF_GAINK       /* 1 */
        HIGHSHELF_SLOPEK      /* 2 */,
        // Audio Arguments
        HIGHSHELF_INA         /* 3 */
    )
}

// 9
void highshelf_akka_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_GAINK       /* 1 */
        HIGHSHELF_SLOPEK      /* 2 */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
        HIGHSHELF_INA         /* 3 */
    )
}

// 10
void highshelf_kaka_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */
        HIGHSHELF_SLOPEK      /* 2 */,
        // Audio Arguments
        HIGHSHELF_GAINA       /* 1 */
        HIGHSHELF_INA         /* 3 */
    )
}

// 11
void highshelf_aaka_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_SLOPEK      /* 2 */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
        HIGHSHELF_GAINA       /* 1 */
        HIGHSHELF_INA         /* 3 */
    )
}

// 12
void highshelf_kkaa_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */
        HIGHSHELF_GAINK       /* 1 */,
        // Audio Arguments
        HIGHSHELF_SLOPEA      /* 2 */
        HIGHSHELF_INA         /* 3 */
    )
}

// 13
void highshelf_akaa_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_GAINK       /* 1 */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
        HIGHSHELF_SLOPEA      /* 2 */
        HIGHSHELF_INA         /* 3 */
    )
}

// 14
void highshelf_kaaa_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */,
        // Audio Arguments
        HIGHSHELF_GAINA       /* 1 */
        HIGHSHELF_SLOPEA      /* 2 */
        HIGHSHELF_INA         /* 3 */
    )
}

// 15
void highshelf_aaaa_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
        HIGHSHELF_GAINA       /* 1 */
        HIGHSHELF_SLOPEA      /* 2 */
        HIGHSHELF_INA         /* 3 */
    )
}

#define LAG_CALC(CONTROL_ARGS, AUDIO_ARGS)                      \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);                         \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);                         \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);                        \
double   z   = *((double*) u.data);                             \
                                                                \
double lagTime;                                                 \
double input;                                                   \
double a;                                                       \
double b;                                                       \
CONTROL_ARGS                                                    \
AUDIO_LOOP(                                                     \
    AUDIO_ARGS                                                  \
    a       = exp((-2 * M_PI) / (lagTime * SAMPLE_RATE));       \
    b       = 1.0f - a;                                         \
    z       = (input * b) + (z * a);                            \
                                                                \
    UGEN_OUT(out,z);                                            \
);                                                              \
                                                                \
*((double*) u.data) = z;                                        \

#define LAG_LAGTIMEK lagTime = *in0;
#define LAG_LAGTIMEA lagTime = UGEN_IN(in0);
#define LAG_INPUTK input = *in1;
#define LAG_INPUTA input = UGEN_IN(in1);

// 0
void lag_kk_calc(ugen u)
{
    LAG_CALC(
        // Control Arguments
        LAG_LAGTIMEK          /* 0 */
        LAG_INPUTK            /* 1 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void lag_ak_calc(ugen u)
{
    LAG_CALC(
        // Control Arguments
        LAG_INPUTK            /* 1 */,
        // Audio Arguments
        LAG_LAGTIMEA          /* 0 */
    )
}

// 2
void lag_ka_calc(ugen u)
{
    LAG_CALC(
        // Control Arguments
        LAG_LAGTIMEK          /* 0 */,
        // Audio Arguments
        LAG_INPUTA            /* 1 */
    )
}

// 3
void lag_aa_calc(ugen u)
{
    LAG_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        LAG_LAGTIMEA          /* 0 */
        LAG_INPUTA            /* 1 */
    )
}

//REMOVING THESE FOR NOW AS THEY ARE NON-FUNCTIONAL AT THE MOMENT ANYWAYS
/*
//=====================
// Zero delay filters
//=====================

typedef struct
{
    double* ss;//Delayed Samples for Filtering
} zeroDelayFilter_t;

void zeroDelayFilter_constructor(ugen* u)
{
    zeroDelayFilter_t* zerodft = malloc(sizeof(zeroDelayFilter_t));
    zerodft->ss                = malloc(sizeof(double) * 3);
    zerodft->ss[0]             = 0;
    zerodft->ss[1]             = 0;
    zerodft->ss[2]             = 0;
    u->data                    = zerodft;
}

void zeroDelayFilter_deconstructor(ugen* u)
{
    zeroDelayFilter_t* zerodft = (zeroDelayFilter_t*) u->data;
    free(zerodft->ss);
    free(zerodft);
}

#define PREWARP(F,SI) ((2 / SI) * tan((F * SI) / 2))

#define ZERO_DELAY(X,G,S)  (X * G + (X * G + S))

inline double zERO_DELAY_ONE_POLE(double X,double G,double* SS,int32_tI)
{
    double Y   = X * G + SS[I];
    double XmY = X - Y;
    SS[I]      = ZERO_DELAY(XmY,G,SS[I]);
    return Y;
}

//Consider different shaping functions

void zeroDelayOnePole_calc(ugen u)
{
    double freq                = UGEN_IN(u,0);
    double x                   = UGEN_IN(u,1);
    zeroDelayFilter_t* zerodft = (zeroDelayFilter_t*)u.data;

    double warped              = PREWARP(freq,RECIP_SAMPLE_RATE);
    double g                   = warped / (warped + 1);
    double y                   = zERO_DELAY_ONE_POLE(x,g,zerodft->ss,0);

    UGEN_OUT(u,0,y);
}

//Add wave shaper once this is confirmed to work.
//Consider adding a base level amount of noise (a small amount).
void zeroDelayLPMS20_calc(ugen u)
{
    double freq                = UGEN_IN(u,0);
    double resonance           = UGEN_IN(u,1);
    double distortion          = UGEN_IN(u,2);
    double x                   = UGEN_IN(u,3);
    zeroDelayFilter_t* zerodft = (zeroDelayFilter_t*)u.data;

    double warped              = PREWARP(freq,RECIP_SAMPLE_RATE);
    double g                   = warped / (warped + 1);
    double k                   = 2 * resonance;
    double s1                  = zerodft->ss[0];
    double s2                  = zerodft->ss[1];

    double gSqr                = g*g;
    double y                   = (((gSqr * x) + (g * s1)) + s2) * ((gSqr * k) - (g * k) + 1);
    // double ky                  = SOFT_CLIP(k * y,distortion);
    double ky                  = k * y;

    double y1                  = zERO_DELAY_ONE_POLE(x  - ky,g,zerodft->ss,0);
    double y2                  = zERO_DELAY_ONE_POLE(y1 + ky,g,zerodft->ss,1);

    UGEN_OUT(u,0,y);
}
*/

//============================================
// Distortion
//============================================

#define CLIP_CALC(CONTROL_ARGS, AUDIO_ARGS)     \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);         \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);         \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);        \
double amount, input;                           \
CONTROL_ARGS                                    \
AUDIO_LOOP(                                     \
    AUDIO_ARGS                                  \
    UGEN_OUT(out,HARD_CLIP(input, amount));     \
);                                              \

#define CLIP_AMOUNTK amount = *in0;
#define CLIP_AMOUNTA amount = UGEN_IN(in0);
#define CLIP_INPUTK input = *in1;
#define CLIP_INPUTA input = UGEN_IN(in1);

// 0
void clip_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double  CLIP_AMOUNTK
    double  CLIP_INPUTK
    *out = HARD_CLIP(input, amount);
}

// 1
void clip_ak_calc(ugen u)
{
    CLIP_CALC(
        // Control Arguments
        CLIP_INPUTK           /* 1 */,
        // Audio Arguments
        CLIP_AMOUNTA          /* 0 */
    )
}

// 2
void clip_ka_calc(ugen u)
{
    CLIP_CALC(
        // Control Arguments
        CLIP_AMOUNTK          /* 0 */,
        // Audio Arguments
        CLIP_INPUTA           /* 1 */
    )
}

// 3
void clip_aa_calc(ugen u)
{
    CLIP_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        CLIP_AMOUNTA          /* 0 */
        CLIP_INPUTA           /* 1 */
    )
}

#define SOFTCLIP_CALC(CONTROL_ARGS, AUDIO_ARGS)     \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);             \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);             \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);            \
double amount, input;                               \
CONTROL_ARGS                                        \
AUDIO_LOOP(                                         \
    AUDIO_ARGS                                      \
    UGEN_OUT(out, SOFT_CLIP(input, amount));        \
);                                                  \

#define SOFTCLIP_AMOUNTK amount = *in0;
#define SOFTCLIP_AMOUNTA amount = UGEN_IN(in0);
#define SOFTCLIP_INPUTK input = *in1;
#define SOFTCLIP_INPUTA input = UGEN_IN(in1);

// 0
void softclip_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double  SOFTCLIP_AMOUNTK
    double  SOFTCLIP_INPUTK
    *out = SOFT_CLIP(input, amount);
}

// 1
void softclip_ak_calc(ugen u)
{
    SOFTCLIP_CALC(
        // Control Arguments
        SOFTCLIP_INPUTK       /* 1 */,
        // Audio Arguments
        SOFTCLIP_AMOUNTA      /* 0 */
    )
}

// 2
void softclip_ka_calc(ugen u)
{
    SOFTCLIP_CALC(
        // Control Arguments
        SOFTCLIP_AMOUNTK      /* 0 */,
        // Audio Arguments
        SOFTCLIP_INPUTA       /* 1 */
    )
}

// 3
void softclip_aa_calc(ugen u)
{
    SOFTCLIP_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        SOFTCLIP_AMOUNTA      /* 0 */
        SOFTCLIP_INPUTA       /* 1 */
    )
}

void poly3_calc(ugen u)
{
    double*  in0 = UGEN_INPUT_BUFFER(u, 0);
    double*  in1 = UGEN_INPUT_BUFFER(u, 1);
    double*  out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out,POLY3_DIST(UGEN_IN(in0),UGEN_IN(in1)));
    );
}

#define POLY3_CALC(CONTROL_ARGS, AUDIO_ARGS)            \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);                 \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);                 \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);                \
double amount, input;                                   \
CONTROL_ARGS                                            \
AUDIO_LOOP(                                             \
    AUDIO_ARGS                                          \
    UGEN_OUT(out, POLY3_DIST(input, amount));           \
);                                                      \

#define POLY3_AMOUNTK amount = *in0;
#define POLY3_AMOUNTA amount = UGEN_IN(in0);
#define POLY3_INPUTK input = *in1;
#define POLY3_INPUTA input = UGEN_IN(in1);

// 0
void poly3_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double  POLY3_AMOUNTK
    double  POLY3_INPUTK
    *out = POLY3_DIST(input, amount);
}

// 1
void poly3_ak_calc(ugen u)
{
    POLY3_CALC(
        // Control Arguments
        POLY3_INPUTK          /* 1 */,
        // Audio Arguments
        POLY3_AMOUNTA         /* 0 */
    )
}

// 2
void poly3_ka_calc(ugen u)
{
    POLY3_CALC(
        // Control Arguments
        POLY3_AMOUNTK         /* 0 */,
        // Audio Arguments
        POLY3_INPUTA          /* 1 */
    )
}

// 3
void poly3_aa_calc(ugen u)
{
    POLY3_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        POLY3_AMOUNTA         /* 0 */
        POLY3_INPUTA          /* 1 */
    )
}

void tanhdist_calc(ugen u)
{
    double*  in0 = UGEN_INPUT_BUFFER(u, 0);
    double*  in1 = UGEN_INPUT_BUFFER(u, 1);
    double*  out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out,TANH_DIST(UGEN_IN(in0),UGEN_IN(in1)));
    );

}

#define TANHDIST_CALC(CONTROL_ARGS, AUDIO_ARGS)         \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);                 \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);                 \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);                \
double amount, input;                                   \
CONTROL_ARGS                                            \
AUDIO_LOOP(                                             \
    AUDIO_ARGS                                          \
    UGEN_OUT(out, TANH_DIST(input, amount));            \
);                                                      \

#define TANHDIST_AMOUNTK amount = *in0;
#define TANHDIST_AMOUNTA amount = UGEN_IN(in0);
#define TANHDIST_INPUTK input = *in1;
#define TANHDIST_INPUTA input = UGEN_IN(in1);

// 0
void tanhDist_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double  TANHDIST_AMOUNTK
    double  TANHDIST_INPUTK
    *out = TANH_DIST(input, amount);
}

// 1
void tanhDist_ak_calc(ugen u)
{
    TANHDIST_CALC(
        // Control Arguments
        TANHDIST_INPUTK       /* 1 */,
        // Audio Arguments
        TANHDIST_AMOUNTA      /* 0 */
    )
}

// 2
void tanhDist_ka_calc(ugen u)
{
    TANHDIST_CALC(
        // Control Arguments
        TANHDIST_AMOUNTK      /* 0 */,
        // Audio Arguments
        TANHDIST_INPUTA       /* 1 */
    )
}

// 3
void tanhDist_aa_calc(ugen u)
{
    TANHDIST_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        TANHDIST_AMOUNTA      /* 0 */
        TANHDIST_INPUTA       /* 1 */
    )
}

void sinDist_calc(ugen u)
{
    double*  in0 = UGEN_INPUT_BUFFER(u, 0);
    double*  in1 = UGEN_INPUT_BUFFER(u, 1);
    double*  out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out,SIN_DIST(UGEN_IN(in0),UGEN_IN(in1)));
    );
}

#define SINDIST_CALC(CONTROL_ARGS, AUDIO_ARGS)          \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);                 \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);                 \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);                \
double amount, input;                                   \
CONTROL_ARGS                                            \
AUDIO_LOOP(                                             \
    AUDIO_ARGS                                          \
    UGEN_OUT(out, SIN_DIST(input, amount));             \
);                                                      \


#define SINDIST_AMOUNTK amount = *in0;
#define SINDIST_AMOUNTA amount = UGEN_IN(in0);
#define SINDIST_INPUTK input = *in1;
#define SINDIST_INPUTA input = UGEN_IN(in1);

// 0
void sinDist_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double  SINDIST_AMOUNTK
    double  SINDIST_INPUTK
    *out = SIN_DIST(input, amount);
}

// 1
void sinDist_ak_calc(ugen u)
{
    SINDIST_CALC(
        // Control Arguments
        SINDIST_INPUTK        /* 1 */,
        // Audio Arguments
        SINDIST_AMOUNTA       /* 0 */
    )
}

// 2
void sinDist_ka_calc(ugen u)
{
    SINDIST_CALC(
        // Control Arguments
        SINDIST_AMOUNTK       /* 0 */,
        // Audio Arguments
        SINDIST_INPUTA        /* 1 */
    )
}

// 3
void sinDist_aa_calc(ugen u)
{
    SINDIST_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        SINDIST_AMOUNTA       /* 0 */
        SINDIST_INPUTA        /* 1 */
    )
}

#define WRAP_CALC(CONTROL_ARGS, AUDIO_ARGS)     \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);         \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);         \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);        \
double amount, input;                           \
CONTROL_ARGS                                    \
AUDIO_LOOP(                                     \
    AUDIO_ARGS                                  \
    UGEN_OUT(out, WRAP(input, amount));         \
);                                              \


#define WRAP_AMOUNTK amount = *in0;
#define WRAP_AMOUNTA amount = UGEN_IN(in0);
#define WRAP_INPUTK input = *in1;
#define WRAP_INPUTA input = UGEN_IN(in1);

// 0
void wrap_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double  WRAP_AMOUNTK
    double  WRAP_INPUTK
    *out = WRAP(input, amount);
}

// 1
void wrap_ak_calc(ugen u)
{
    WRAP_CALC(
        // Control Arguments
        WRAP_INPUTK           /* 1 */,
        // Audio Arguments
        WRAP_AMOUNTA          /* 0 */
    )
}

// 2
void wrap_ka_calc(ugen u)
{
    WRAP_CALC(
        // Control Arguments
        WRAP_AMOUNTK          /* 0 */,
        // Audio Arguments
        WRAP_INPUTA           /* 1 */
    )
}

// 3
void wrap_aa_calc(ugen u)
{
    WRAP_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        WRAP_AMOUNTA          /* 0 */
        WRAP_INPUTA           /* 1 */
    )
}

#define ROUND(f) ((float)((f > 0.0) ? floor(f + 0.5) : ceil(f - 0.5)))

#define CRUSH_CALC(CONTROL_ARGS, AUDIO_ARGS)                    \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);                         \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);                         \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);                        \
int32_t max;                                                    \
double x;                                                       \
union {                                                         \
    double d;                                                   \
    int32_t x[2];                                               \
} ud = { 0 };                                                   \
CONTROL_ARGS                                                    \
AUDIO_LOOP(                                                     \
    AUDIO_ARGS                                                  \
    UGEN_OUT(out, floor((x + 1.0) * max) / max - 1.0);          \
);                                                              \

#define CRUSH_DEPTHK max = fast_pow(ud, 2, *in0) - 1;
#define CRUSH_DEPTHA max = fast_pow(ud, 2, UGEN_IN(in0)) - 1;
#define CRUSH_XK x = *in1;
#define CRUSH_XA x = UGEN_IN(in1);

// 0
void crush_kk_calc(ugen u)
{
    CRUSH_CALC(
        // Control Arguments
        CRUSH_DEPTHK          /* 0 */
        CRUSH_XK              /* 1 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void crush_ak_calc(ugen u)
{
    CRUSH_CALC(
        // Control Arguments
        CRUSH_XK              /* 1 */,
        // Audio Arguments
        CRUSH_DEPTHA          /* 0 */
    )
}

// 2
void crush_ka_calc(ugen u)
{
    CRUSH_CALC(
        // Control Arguments
        CRUSH_DEPTHK          /* 0 */,
        // Audio Arguments
        CRUSH_XA              /* 1 */
    )
}

// 3
void crush_aa_calc(ugen u)
{
    CRUSH_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        CRUSH_DEPTHA          /* 0 */
        CRUSH_XA              /* 1 */
    )
}

typedef struct
{
    double samples;
    double prev;
} decimate_t;

void decimate_constructor(ugen* u)
{
    decimate_t* decimate = malloc(sizeof(decimate_t));
    decimate->samples    = 0;
    decimate->prev       = 0;
    u->data              = decimate;
}

void decimate_deconstructor(ugen* u)
{
    free(u->data);
}

#define DECIMATE_CALC(CONTROL_ARGS, AUDIO_ARGS)                     \
double*    in0      = UGEN_INPUT_BUFFER(u, 0);                      \
double*    in1      = UGEN_INPUT_BUFFER(u, 1);                      \
double*    out      = UGEN_OUTPUT_BUFFER(u, 0);                     \
decimate_t decimate = *((decimate_t*) u.data);                      \
double     rate;                                                    \
double     x;                                                       \
double     y;                                                       \
CONTROL_ARGS                                                        \
AUDIO_LOOP(                                                         \
    AUDIO_ARGS                                                      \
    y = 0;                                                          \
                                                                    \
    if (decimate.samples + rate >= 1)                               \
    {                                                               \
        decimate.samples = fmod(decimate.samples + rate,1.0);       \
        decimate.prev    = x;                                       \
        y                = x;                                       \
    }                                                               \
    else                                                            \
    {                                                               \
        decimate.samples += rate;                                   \
        y = decimate.prev;                                          \
    }                                                               \
                                                                    \
    UGEN_OUT(out,y);                                                \
);                                                                  \
                                                                    \
*((decimate_t*) u.data) = decimate;                                 \

#define DECIMATE_RATEK rate = (*in0) * RECIP_SAMPLE_RATE;
#define DECIMATE_RATEA rate = UGEN_IN(in0) * RECIP_SAMPLE_RATE;
#define DECIMATE_XK x = *in1;
#define DECIMATE_XA x = UGEN_IN(in1);

// 0
void decimate_kk_calc(ugen u)
{
    DECIMATE_CALC(
        // Control Arguments
        DECIMATE_RATEK        /* 0 */
        DECIMATE_XK           /* 1 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void decimate_ak_calc(ugen u)
{
    DECIMATE_CALC(
        // Control Arguments
        DECIMATE_XK           /* 1 */,
        // Audio Arguments
        DECIMATE_RATEA        /* 0 */
    )
}

// 2
void decimate_ka_calc(ugen u)
{
    DECIMATE_CALC(
        // Control Arguments
        DECIMATE_RATEK        /* 0 */,
        // Audio Arguments
        DECIMATE_XA           /* 1 */
    )
}

// 3
void decimate_aa_calc(ugen u)
{
    DECIMATE_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        DECIMATE_RATEA        /* 0 */
        DECIMATE_XA           /* 1 */
    )
}

//======================================
// Reverberation
//======================================

typedef struct
{
    delay_data* combFilterDelays;
    double*     combz1s;
    delay_data* allpassDelays;
} freeverb_data;

void freeverb_constructor(ugen* u)
{
    freeverb_data* freeverb       = calloc(sizeof(freeverb_data), 1);
    freeverb->combFilterDelays    = calloc(sizeof(delay_data), 8);
    freeverb->combz1s             = calloc(sizeof(double), 8);
    freeverb->allpassDelays       = calloc(sizeof(delay_data), 4);

    delay_data data0 = { acquire_sample_buffer(1557), 1556, 0 };
    freeverb->combFilterDelays[0] = data0;
    delay_data data1 = { acquire_sample_buffer(1617), 1616, 0 };
    freeverb->combFilterDelays[1] = data1;
    delay_data data2 = { acquire_sample_buffer(1491), 1490, 0 };
    freeverb->combFilterDelays[2] = data2;
    delay_data data3 = { acquire_sample_buffer(1422), 1421, 0 };
    freeverb->combFilterDelays[3] = data3;
    delay_data data4 = { acquire_sample_buffer(1277), 1276, 0 };
    freeverb->combFilterDelays[4] = data4;
    delay_data data5 = { acquire_sample_buffer(1356), 1355, 0 };
    freeverb->combFilterDelays[5] = data5;
    delay_data data6 = { acquire_sample_buffer(1188), 1187, 0 };
    freeverb->combFilterDelays[6] = data6;
    delay_data data7 = { acquire_sample_buffer(1116), 1115, 0 };
    freeverb->combFilterDelays[7] = data7;

    delay_data data8 = { acquire_sample_buffer(225), 225, 0 };
    freeverb->allpassDelays[0]    = data8;
    delay_data data9 = { acquire_sample_buffer(556), 556, 0 };
    freeverb->allpassDelays[1]    = data9;
    delay_data data10 = { acquire_sample_buffer(441), 441, 0 };
    freeverb->allpassDelays[2]    = data10;
    delay_data data11 = { acquire_sample_buffer(341), 341, 0 };
    freeverb->allpassDelays[3]    = data11;

    u->data                       = freeverb;
}

void freeverb_deconstructor(ugen* u)
{
    freeverb_data* freeverb = (freeverb_data*) u->data;
    int32_t i;
    for(i=0;i<8;++i)
    {
        release_sample_buffer(freeverb->combFilterDelays[i].buffer);
        // free(freeverb->combz1s);
    }
    for(i=0;i<4;++i)
        release_sample_buffer(freeverb->allpassDelays[i].buffer);
    free(u->data);
}

const double fixed_gain = 0.015;

// Should the delayed signal in this be x or y or x + y???
#define COMB_FILTER(X,R,D,N,DATA,ZS,I)                                                                          \
({                                                                                                              \
    int64_t        write_index              = DATA[I].write_index;                                              \
    sample_buffer* buffer                   = DATA[I].buffer;                                                   \
    uint32_t          num_samples_mask         = buffer->num_samples_mask;                                      \
    double*        samples                  = buffer->samples;                                                  \
    double         damp1                    = D * 0.4;                                                          \
    double         damp2                    = 1 - damp1;                                                        \
    double         feedback                 = R * 0.28 + 0.7;                                                   \
    int64_t        read_index               = write_index - N;                                                  \
    double         y                        = read_index < 0 ? 0 : samples[read_index & num_samples_mask];      \
    ZS[I]                                   = y * damp2 + ZS[I] * damp1;                                        \
    samples[write_index & num_samples_mask] = (X * fixed_gain) + ZS[I] * feedback;                              \
    DATA[I].write_index                     = write_index + 1;                                                  \
    y;                                                                                                          \
})

#define ALLPASS_FEEDBACK(X,F,N,DATA,I)                                                                          \
({                                                                                                              \
    int64_t        write_index              = DATA[I].write_index;                                              \
    int64_t        read_index               = write_index - N;                                                  \
    sample_buffer* buffer                   = DATA[I].buffer;                                                   \
    double*        samples                  = buffer->samples;                                                  \
    uint32_t          num_samples_mask         = buffer->num_samples_mask;                                      \
    double         bufout                   = read_index < 0 ? 0 : samples[read_index & num_samples_mask];      \
    double         y                        = -X + bufout;                                                      \
    samples[write_index & num_samples_mask] = X + bufout * F;                                                   \
    DATA[I].write_index                     = write_index + 1;                                                  \
    y;                                                                                                          \
})


// NOTE: Optimize COMB_FILTER and ALLPASS_FEEDBACK by pulling declarations and ugen data access outside of audio loop
#define FREEVERB_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                     \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                                              \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                                                              \
double* in2 = UGEN_INPUT_BUFFER(u, 2);                                                              \
double* in3 = UGEN_INPUT_BUFFER(u, 3);                                                              \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                                             \
freeverb_data vdata = *((freeverb_data*) u.data);                                                   \
double mix;                                                                                         \
double roomSize;                                                                                    \
double damp;                                                                                        \
double x, y;                                                                                        \
double cf0, cf1, cf2, cf3, cf4, cf5, cf6, cf7, cfy;                                                 \
double ap0, ap1, ap2, ap3;                                                                          \
CONTROL_ARGS                                                                                        \
AUDIO_LOOP(                                                                                         \
    AUDIO_ARGS                                                                                      \
    cf0      = COMB_FILTER(x, roomSize, damp, 1557, vdata.combFilterDelays, vdata.combz1s, 0);      \
    cf1      = COMB_FILTER(x, roomSize, damp, 1617, vdata.combFilterDelays, vdata.combz1s, 1);      \
    cf2      = COMB_FILTER(x, roomSize, damp, 1491, vdata.combFilterDelays, vdata.combz1s, 2);      \
    cf3      = COMB_FILTER(x, roomSize, damp, 1422, vdata.combFilterDelays, vdata.combz1s, 3);      \
    cf4      = COMB_FILTER(x, roomSize, damp, 1277, vdata.combFilterDelays, vdata.combz1s, 4);      \
    cf5      = COMB_FILTER(x, roomSize, damp, 1356, vdata.combFilterDelays, vdata.combz1s, 5);      \
    cf6      = COMB_FILTER(x, roomSize, damp, 1188, vdata.combFilterDelays, vdata.combz1s, 6);      \
    cf7      = COMB_FILTER(x, roomSize, damp, 1116, vdata.combFilterDelays, vdata.combz1s, 7);      \
    cfy      = cf0 + cf1 + cf2 + cf3 + cf3 + cf5 + cf6 + cf7;                                       \
                                                                                                    \
    ap0      = ALLPASS_FEEDBACK(cfy, 0.5, 225, vdata.allpassDelays, 0);                             \
    ap1      = ALLPASS_FEEDBACK(ap0, 0.5, 556, vdata.allpassDelays, 1);                             \
    ap2      = ALLPASS_FEEDBACK(ap1, 0.5, 441, vdata.allpassDelays, 2);                             \
    ap3      = ALLPASS_FEEDBACK(ap2, 0.5, 341, vdata.allpassDelays, 3);                             \
    y        = (x * (1 - mix)) + (ap3 * mix * 1.0);                                                 \
    UGEN_OUT(out, y);                                                                               \
);                                                                                                  \
*((freeverb_data*) u.data) = vdata;                                                                 \

#define FREEVERB_MIXK mix = CLAMP(*in0, 0, 1);
#define FREEVERB_MIXA mix = CLAMP(UGEN_IN(in0), 0, 1);
#define FREEVERB_ROOMSIZEK roomSize = CLAMP(*in1, 0, 1);
#define FREEVERB_ROOMSIZEA roomSize = CLAMP(UGEN_IN(in1), 0, 1);
#define FREEVERB_DAMPK damp = CLAMP(*in2, 0, 1);
#define FREEVERB_DAMPA damp = CLAMP(UGEN_IN(in2), 0, 1);
#define FREEVERB_XK x = *in3;
#define FREEVERB_XA x = UGEN_IN(in3);


// 0
void freeverb_kkkk_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */
        FREEVERB_ROOMSIZEK    /* 1 */
        FREEVERB_DAMPK        /* 2 */
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void freeverb_akkk_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_ROOMSIZEK    /* 1 */
        FREEVERB_DAMPK        /* 2 */
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
    )
}

// 2
void freeverb_kakk_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */
        FREEVERB_DAMPK        /* 2 */
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        FREEVERB_ROOMSIZEA    /* 1 */
    )
}

// 3
void freeverb_aakk_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_DAMPK        /* 2 */
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
        FREEVERB_ROOMSIZEA    /* 1 */
    )
}

// 4
void freeverb_kkak_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */
        FREEVERB_ROOMSIZEK    /* 1 */
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        FREEVERB_DAMPA        /* 2 */
    )
}

// 5
void freeverb_akak_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_ROOMSIZEK    /* 1 */
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
        FREEVERB_DAMPA        /* 2 */
    )
}

// 6
void freeverb_kaak_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        FREEVERB_ROOMSIZEA    /* 1 */
        FREEVERB_DAMPA        /* 2 */
    )
}

// 7
void freeverb_aaak_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
        FREEVERB_ROOMSIZEA    /* 1 */
        FREEVERB_DAMPA        /* 2 */
    )
}

// 8
void freeverb_kkka_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */
        FREEVERB_ROOMSIZEK    /* 1 */
        FREEVERB_DAMPK        /* 2 */,
        // Audio Arguments
        FREEVERB_XA           /* 3 */
    )
}

// 9
void freeverb_akka_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_ROOMSIZEK    /* 1 */
        FREEVERB_DAMPK        /* 2 */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
        FREEVERB_XA           /* 3 */
    )
}

// 10
void freeverb_kaka_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */
        FREEVERB_DAMPK        /* 2 */,
        // Audio Arguments
        FREEVERB_ROOMSIZEA    /* 1 */
        FREEVERB_XA           /* 3 */
    )
}

// 11
void freeverb_aaka_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_DAMPK        /* 2 */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
        FREEVERB_ROOMSIZEA    /* 1 */
        FREEVERB_XA           /* 3 */
    )
}

// 12
void freeverb_kkaa_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */
        FREEVERB_ROOMSIZEK    /* 1 */,
        // Audio Arguments
        FREEVERB_DAMPA        /* 2 */
        FREEVERB_XA           /* 3 */
    )
}

// 13
void freeverb_akaa_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_ROOMSIZEK    /* 1 */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
        FREEVERB_DAMPA        /* 2 */
        FREEVERB_XA           /* 3 */
    )
}

// 14
void freeverb_kaaa_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */,
        // Audio Arguments
        FREEVERB_ROOMSIZEA    /* 1 */
        FREEVERB_DAMPA        /* 2 */
        FREEVERB_XA           /* 3 */
    )
}

// 15
void freeverb_aaaa_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
        FREEVERB_ROOMSIZEA    /* 1 */
        FREEVERB_DAMPA        /* 2 */
        FREEVERB_XA           /* 3 */
    )
}

// Limiter

typedef struct
{
    sample_buffer* buffer;
    double lookahead;
    double envelope_out;
    int64_t write_index;
} limiter_data;

void limiter_constructor(ugen* u)
{
    u->data = (limiter_data*) malloc(sizeof(limiter_data));
    double lookahead = fmax(1, u->constructor_args[0] * SAMPLE_RATE);
    limiter_data data = { acquire_sample_buffer(lookahead * 2), lookahead, 0, 0 };
    *((limiter_data*) u->data) = data;
}

void limiter_deconstructor(ugen* u)
{
    release_sample_buffer(((limiter_data*) u->data)->buffer);
    free(u->data);
}

// 2nd order lagrange factoring out the left side for y0 = 0 and the y1 multiplication as y1 == ratio == 1 in a limter.
// ((x - x1) / (x0 - x1) * y0{{0}} + (x - x0) / (x1 - x0) * y1{{scale:1}})
#define LIMITER_LAGRANGE(x0, x1, x) ((x - x0) / (x1 - x0))

__attribute__((always_inline)) static inline double limiterInlineCalc(
    sample_buffer buffer, double* samples, uint32_t num_samples_mask, limiter_data* data, double lookahead, double attack_gain,
    double release_gain, double threshold, double knee_width, double lower_knee_bound, double upper_knee_bound, double x, double envelope_in)
{
    //  Envelope follower
    double envelope_out = data->envelope_out;
    if (envelope_out < envelope_in)
        envelope_out = envelope_in + attack_gain * (envelope_out - envelope_in);
    else
        envelope_out = envelope_in + release_gain * (envelope_out - envelope_in);

    data->envelope_out = envelope_out;

    // Delay
    int64_t write_index = data->write_index;
    const int64_t read_index = write_index - lookahead;
    double y = read_index < 0 ? 0 : samples[read_index & num_samples_mask];
    samples[write_index & num_samples_mask] = x;
    data->write_index = write_index + 1;

    // Calculate gain based on soft/hard knee. Threshold, knee_width, lower_knee_bound, and upper_knee_bound are in decibels.
    const double envelope_out_db = AMP2DB(envelope_out);
    double gain = 1;

    if (knee_width > 0.0 && envelope_out_db > lower_knee_bound && envelope_out_db < upper_knee_bound)
    {
        gain = LIMITER_LAGRANGE(lower_knee_bound, upper_knee_bound, envelope_out_db);
    }

    gain = DB2AMP(fmin(0, gain * (threshold - envelope_out_db)));
    return y * gain;
}

#define LIMITER_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                                          \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                                                                  \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                                                                                  \
double* in2 = UGEN_INPUT_BUFFER(u, 2);                                                                                  \
double* in3 = UGEN_INPUT_BUFFER(u, 3);                                                                                  \
double* in4 = UGEN_INPUT_BUFFER(u, 4);                                                                                  \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                                                                 \
limiter_data* data = (limiter_data*) u.data;                                                                            \
sample_buffer buffer = *data->buffer;                                                                                   \
double* samples = buffer.samples;                                                                                       \
uint32_t num_samples = buffer.num_samples;                                                                              \
uint32_t num_samples_mask = buffer.num_samples_mask;                                                                    \
double lookahead = data->lookahead;                                                                                     \
double attack, attack_gain;                                                                                             \
double release, release_gain;                                                                                           \
double threshold;                                                                                                       \
double knee, knee_width, lower_knee_bound, upper_knee_bound;                                                            \
double input, envelope_in;                                                                                              \
CONTROL_ARGS                                                                                                            \
AUDIO_LOOP(                                                                                                             \
    AUDIO_ARGS                                                                                                          \
    UGEN_OUT(out, limiterInlineCalc(buffer, samples, num_samples_mask, data, lookahead, attack_gain, release_gain,      \
                                    threshold, knee, lower_knee_bound, upper_knee_bound, input, envelope_in));          \
);                                                                                                                      \

#define LIMITER_ATTACK_GAIN attack_gain = exp(-1.0 / SAMPLE_RATE * attack); // LUT for exp?
#define LIMITER_RELEASE_GAIN release_gain = exp(-1.0 / SAMPLE_RATE * release);
#define LIMITER_ENVELOPE_IN envelope_in = fabs(input);
#define LIMITER_KNEE_WIDTH knee_width = fmin(0, threshold * knee * -1.0); lower_knee_bound = threshold - (knee_width / 2.0); upper_knee_bound = fmin(0, threshold + (knee_width / 2.0));

#define LIMITER_ATTACKK attack = fabs(*in0); LIMITER_ATTACK_GAIN
#define LIMITER_ATTACKA attack = fabs(UGEN_IN(in0)); LIMITER_ATTACK_GAIN
#define LIMITER_RELEASEK release = fabs(*in1); LIMITER_RELEASE_GAIN
#define LIMITER_RELEASEA release = fabs(UGEN_IN(in1)); LIMITER_RELEASE_GAIN
#define LIMITER_THRESHOLDK threshold = *in2;
#define LIMITER_THRESHOLDA threshold = UGEN_IN(in2);
#define LIMITER_KNEEK knee = fmin(1, fmax(0, fabs(*in3))); LIMITER_KNEE_WIDTH
#define LIMITER_KNEEA knee = fmin(1, fmax(0, fabs(UGEN_IN(in3)))); LIMITER_KNEE_WIDTH
#define LIMITER_INPUTK input = *in4; LIMITER_ENVELOPE_IN
#define LIMITER_INPUTA input = UGEN_IN(in4); LIMITER_ENVELOPE_IN

// 0
__attribute__((flatten)) void limiter_kkkkk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
__attribute__((flatten)) void limiter_akkkk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
    )
}

// 2
__attribute__((flatten)) void limiter_kakkk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
    )
}

// 3
__attribute__((flatten)) void limiter_aakkk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
    )
}

// 4
__attribute__((flatten)) void limiter_kkakk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_THRESHOLDA    /* 2 */
    )
}

// 5
__attribute__((flatten)) void limiter_akakk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_THRESHOLDA    /* 2 */
    )
}

// 6
__attribute__((flatten)) void limiter_kaakk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
    )
}

// 7
__attribute__((flatten)) void limiter_aaakk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
    )
}

// 8
__attribute__((flatten)) void limiter_kkkak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_KNEEA         /* 3 */
    )
}

// 9
__attribute__((flatten)) void limiter_akkak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_KNEEA         /* 3 */
    )
}

// 10
__attribute__((flatten)) void limiter_kakak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
        LIMITER_KNEEA         /* 3 */
    )
}

// 11
__attribute__((flatten)) void limiter_aakak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
        LIMITER_KNEEA         /* 3 */
    )
}

// 12
__attribute__((flatten)) void limiter_kkaak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
    )
}

// 13
__attribute__((flatten)) void limiter_akaak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
    )
}

// 14
__attribute__((flatten)) void limiter_kaaak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
    )
}

// 15
__attribute__((flatten)) void limiter_aaaak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
    )
}

// 16
__attribute__((flatten)) void limiter_kkkka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_INPUTA        /* 4 */
    )
}

// 17
__attribute__((flatten)) void limiter_akkka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 18
__attribute__((flatten)) void limiter_kakka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 19
__attribute__((flatten)) void limiter_aakka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 20
__attribute__((flatten)) void limiter_kkaka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 21
__attribute__((flatten)) void limiter_akaka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 22
__attribute__((flatten)) void limiter_kaaka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 23
__attribute__((flatten)) void limiter_aaaka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 24
__attribute__((flatten)) void limiter_kkkaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */,
        // Audio Arguments
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 25
__attribute__((flatten)) void limiter_akkaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 26
__attribute__((flatten)) void limiter_kakaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_THRESHOLDK    /* 2 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 27
__attribute__((flatten)) void limiter_aakaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_THRESHOLDK    /* 2 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 28
__attribute__((flatten)) void limiter_kkaaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */,
        // Audio Arguments
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 29
__attribute__((flatten)) void limiter_akaaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 30
__attribute__((flatten)) void limiter_kaaaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 31
__attribute__((flatten)) void limiter_aaaaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

#define PAN_CALC(CONTROL_ARGS, AUDIO_ARGS)          \
double* in0 = UGEN_INPUT_BUFFER(u, 0);              \
double* in1 = UGEN_INPUT_BUFFER(u, 1);              \
double* out0 = UGEN_OUTPUT_BUFFER(u, 0);            \
double* out1 = UGEN_OUTPUT_BUFFER(u, 1);            \
double x, delta, pos, ampL, ampR;                   \
uint32_t index1, index2;                            \
double cos1, cos2, cos3, cos4;                      \
CONTROL_ARGS                                        \
AUDIO_LOOP(                                         \
    AUDIO_ARGS                                      \
    UGEN_OUT(out0, x * ampL);                       \
    UGEN_OUT(out1, x * ampR);                       \
);

/* range is assumed to be -1 to 1 */
#define PAN_CALC_LR_AMPS                            \
pos = (pos + 1) * (double) PAN_HALF_TABLE_SIZE;     \
index1 = pos;                                       \
index2 = pos + PAN_HALF_TABLE_SIZE;                 \
ampL = pan_table[index1 & PAN_TABLE_SIZE_MASK];     \
ampR = pan_table[index2 & PAN_TABLE_SIZE_MASK];

#define PAN_POSK pos = *in0; PAN_CALC_LR_AMPS
#define PAN_POSA pos = UGEN_IN(in0); PAN_CALC_LR_AMPS
#define PAN_XK x = *in1;
#define PAN_XA x = UGEN_IN(in1);

// 0
void pan_kk_calc(ugen u)
{
    PAN_CALC(
        // Control Arguments
        PAN_POSK              /* 0 */
        PAN_XK                /* 1 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void pan_ak_calc(ugen u)
{
    PAN_CALC(
        // Control Arguments
        PAN_XK                /* 1 */,
        // Audio Arguments
        PAN_POSA              /* 0 */
    )
}

// 2
void pan_ka_calc(ugen u)
{
    PAN_CALC(
        // Control Arguments
        PAN_POSK              /* 0 */,
        // Audio Arguments
        PAN_XA                /* 1 */
    )
}

// 3
void pan_aa_calc(ugen u)
{
    PAN_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        PAN_POSA              /* 0 */
        PAN_XA                /* 1 */
    )
}

#define E 2.7182818284590452353602874713527

typedef struct
{
    sample_buffer* buffer;
    uint32_t write_index;
    uint32_t noiseSamples;
    uint32_t minFreq;
} pluck_data;

void pluck_constructor(ugen* u)
{
    u->data = malloc(sizeof(pluck_data));
    pluck_data data = { acquire_sample_buffer(SAMPLE_RATE / u->constructor_args[0]),0,0,u->constructor_args[0]};
    *((pluck_data*) u->data) = data;
}

void pluck_deconstructor(ugen* u)
{
    release_sample_buffer(((pluck_data*) u->data)->buffer);
    free(u->data);
}

// Jaffe and Smith "Extensions of the Karplus-Strong Plucked-String* Algorithm"
#define PLUCK_CALC(CONTROL_ARGS, AUDIO_ARGS)                                    \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                          \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                                          \
double* in2 = UGEN_INPUT_BUFFER(u, 2);                                          \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                         \
pluck_data  data             = *((pluck_data*) u.data);                         \
double*     samples          = data.buffer->samples;                            \
uint32_t    write_index      = data.write_index;                                \
uint32_t    num_samples_mask = data.buffer->num_samples_mask;                   \
double      clamped;                                                            \
double      freq;                                                               \
uint32_t    n;                                                                  \
uint32_t    index2;                                                             \
double      decay;                                                              \
double      duration;                                                           \
double      x;                                                                  \
double      y;                                                                  \
CONTROL_ARGS                                                                    \
AUDIO_LOOP(                                                                     \
    AUDIO_ARGS                                                                  \
    index2 = (write_index + 1) % n;                                             \
    decay = pow(E, (-(n + 0.5) * 6.908) / duration) / TABLE_COS(M_PI * freq);   \
    y = 0;                                                                      \
                                                                                \
    if (data.noiseSamples < n)                                                  \
    {                                                                           \
        y = x;                                                                  \
        data.noiseSamples++;                                                    \
    }                                                                           \
                                                                                \
    else                                                                        \
    {                                                                           \
        y = decay * (samples[write_index] + samples[index2]) / 2;               \
    }                                                                           \
                                                                                \
    samples[write_index] = y;                                                   \
    write_index = index2;                                                       \
    UGEN_OUT(out, y);                                                           \
);                                                                              \
data.write_index = index2;                                                      \
*((pluck_data*) u.data) = data;                                                 \

#define PLUCK_FREQK clamped = MAX(*in0, data.minFreq); freq = clamped * RECIP_SAMPLE_RATE; n = SAMPLE_RATE / clamped;
#define PLUCK_FREQA clamped = MAX(UGEN_IN(in0), data.minFreq); freq = clamped * RECIP_SAMPLE_RATE; n = SAMPLE_RATE / clamped;
#define PLUCK_DURATIONK duration = (*in1) * SAMPLE_RATE;
#define PLUCK_DURATIONA duration = UGEN_IN(in1) * SAMPLE_RATE;
#define PLUCK_XK x = *in2;
#define PLUCK_XA x = UGEN_IN(in2);

// 0
void pluck_kkk_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_DURATIONK       /* 1 */
        PLUCK_XK              /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void pluck_akk_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_DURATIONK       /* 1 */
        PLUCK_XK              /* 2 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
    )
}

// 2
void pluck_kak_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_XK              /* 2 */,
        // Audio Arguments
        PLUCK_DURATIONA       /* 1 */
    )
}

// 3
void pluck_aak_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_XK              /* 2 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_DURATIONA       /* 1 */
    )
}

// 4
void pluck_kka_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_DURATIONK       /* 1 */,
        // Audio Arguments
        PLUCK_XA              /* 2 */
    )
}

// 5
void pluck_aka_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_DURATIONK       /* 1 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_XA              /* 2 */
    )
}

// 6
void pluck_kaa_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */,
        // Audio Arguments
        PLUCK_DURATIONA       /* 1 */
        PLUCK_XA              /* 2 */
    )
}

// 7
void pluck_aaa_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_DURATIONA       /* 1 */
        PLUCK_XA              /* 2 */
    )
}

// White noise

void white_calc(ugen u)
{
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    AUDIO_LOOP(
        UGEN_OUT(out, RAND_SIG());
    );
}

// Pink Noise

#define DICE_SIZE 16
#define DICE_MASK 15
#define DICE_SHIFT 13

typedef struct
{
    uint32_t* dice;
    int32_t total;
} pink_data;

void pink_constructor(ugen* u)
{
    u->data = malloc(sizeof(pink_data));
    uint32_t* dice = malloc(DICE_SIZE * sizeof(uint32_t));
    int32_t total = 0;

    uint32_t i;
    for (i = 0; i < DICE_SIZE; ++i)
    {
        uint32_t newrand = trand() >> DICE_SHIFT;
        total += newrand;
        dice[i] = newrand;
    }

    pink_data data = { dice, total };
    *((pink_data*) u->data) = data;
}

void pink_deconstructor(ugen* u)
{
    free(((pink_data*) u->data)->dice);
    free(u->data);
}

// Voss/McCartney algorithm
// http://www.firstpr.com.au/dsp/pink-noise/
void pink_calc(ugen u)
{
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    pink_data* data = (pink_data*) u.data;
    uint32_t* dice = data->dice;
    int32_t total = data->total;

    AUDIO_LOOP(
        uint32_t counter = trand();
        uint32_t k = __builtin_ctz(counter) & DICE_MASK;
        uint32_t newrand = counter >> DICE_SHIFT;
        uint32_t prevrand = dice[k];
        dice[k] = newrand;
        total += (newrand - prevrand);
        newrand = trand() >> DICE_SHIFT;
        four_bytes y;
        y.word = (total + newrand) | 0x40000000;
        UGEN_OUT(out, y.f - 3.0);
    );

    data->total = total;
}

// brown noise

typedef struct
{
    double level;
} brown_data;

void brownNoise_constructor(ugen* u)
{
    u->data = malloc(sizeof(brown_data));
    brown_data data = { 0 };
    *((brown_data*) u->data) = data;
}

void brownNoise_deconstructor(ugen* u)
{
    free(u->data);
}

void brownNoise_calc(ugen u)
{
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    brown_data* data = (brown_data*) u.data;
    double y = data->level;

    AUDIO_LOOP(
        y += RAND_SIG_EIGHTH();
        if (y > 1.0) y = 2.0 - y;
        else if(y < -1.0) y = -2.0 - y;
        UGEN_OUT(out, y);
    );

    data->level = y;
}

// Simplex

// Many thanks to Stefan Gustavason for the nice Simplex implementation
// http://staffwww.itn.liu.se/~stegu/aqsis/aqsis-newnoise/

const uint8_t simplex_noise_perm[] = {
    151,160,137,91,90,15,131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,
    142,8,99,37,240,21,10,23,190,6,148,247,120,234,75,0,26,197,62,94,252,219,
    203,117,35,11,32,57,177,33,88,237,149,56,87,174,20,125,136,171,168,68,175,
    74,165,71,134,139,48,27,166,77,146,158,231,83,111,229,122,60,211,133,230,
    220,105,92,41,55,46,245,40,244,102,143,54,65,25,63,161,1,216,80,73,209,76,
    132,187,208,89,18,169,200,196,135,130,116,188,159,86,164,100,109,198,173,
    186,3,64,52,217,226,250,124,123,5,202,38,147,118,126,255,82,85,212,207,206,
    59,227,47,16,58,17,182,189,28,42,223,183,170,213,119,248,152,2,44,154,163,
    70,221,153,101,155,167,43,172,9,129,22,39,253,19,98,108,110,79,113,224,232,
    178,185,112,104,218,246,97,228,251,34,242,193,238,210,144,12,191,179,162,
    241,81,51,145,235,249,14,239,107,49,192,214,31,181,199,106,157,184,84,204,
    176,115,121,50,45,127,4,150,254,138,236,205,93,222,114,67,29,24,72,243,141,
    128,195,78,66,215,61,156,180,151,160,137,91,90,15,131,13,201,95,96,53,194,
    233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,190,6,148,247,120,234,
    75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,88,237,149,56,87,174,
    20,125,136,171,168,68,175,74,165,71,134,139,48,27,166,77,146,158,231,83,111,
    229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,102,143,54,65,25,63,
    161,1,216,80,73,209,76,132,187,208,89,18,169,200,196,135,130,116,188,159,
    86,164,100,109,198,173,186,3,64,52,217,226,250,124,123,5,202,38,147,118,126,
    255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,223,183,170,213,119,
    248,152,2,44,154,163,70,221,153,101,155,167,43,172,9,129,22,39,253,19,98,
    108,110,79,113,224,232,178,185,112,104,218,246,97,228,251,34,242,193,238,
    210,144,12,191,179,162,241,81,51,145,235,249,14,239,107,49,192,214,31,181,
    199,106,157,184,84,204,176,115,121,50,45,127,4,150,254,138,236,205,93,222,
    114,67,29,24,72,243,141,128,195,78,66,215,61,156,180
};


void simplex_constructor(ugen* u)
{
    u->data = malloc(DOUBLE_SIZE);
    *((double*) u->data) = 0;
}

void simplex_deconstructor(ugen* u)
{
    free(u->data);
}


static inline double simplex_grad1(uint8_t hash, double phase)
{
    uint8_t h = hash & 15;
    int32_t grad = 1 + (h & 7);
    if (h & 8) grad = -grad;
    return (double) grad * phase;
}

static inline double simplex_inline_calc(double phase_increment, double* phase_data)
{
    double x = (*phase_data) + phase_increment;
    int64_t i0 = x;
    int64_t i1 = i0 + 1;
    double x0 = x - (double) i0;
    double x1 = x0 - 1.0;
    double t0 = 1.0 - x0 * x0;
    t0 *= t0;
    double n0 = t0 * t0 * simplex_grad1(simplex_noise_perm[i0 & 0xff], x0);
    double t1 = 1.0 - x1 * x1;
    t1 *= t1;
    double n1 = t1 * t1 * simplex_grad1(simplex_noise_perm[i1 & 0xff], x1);
    double y = 0.395 * (n0 + n1); // put into -1 to 1 range
    *phase_data = x;
    return y;
}

#define SIMPLEX_CALC(CONTROL_ARGS, AUDIO_ARGS)                          \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                  \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                 \
double freq, phase_increment;                                           \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
    UGEN_OUT(out, simplex_inline_calc(phase_increment, u.data));        \
);

#define SIMPLEX_PHASE_INCREMENT phase_increment = freq * RECIP_SAMPLE_RATE;
#define SIMPLEX_FREQK freq = *in0; SIMPLEX_PHASE_INCREMENT
#define SIMPLEX_FREQA freq = UGEN_IN(in0); SIMPLEX_PHASE_INCREMENT

// 0
void simplex_k_calc(ugen u)
{
    SIMPLEX_CALC(
        // Control Arguments
        SIMPLEX_FREQK         /* 0 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void simplex_a_calc(ugen u)
{
    SIMPLEX_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        SIMPLEX_FREQA         /* 0 */
    )
}

// timeSecs

void time_micros_calc(ugen u)
{
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    AUDIO_LOOP(
        UGEN_OUT(out, current_cycle_usecs + (jack_time_t) ((double) _block_frame * usecs_per_frame));
    );
}

const double MICROS_PER_SECOND = 1000000;
void time_secs_calc(ugen u)
{
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    AUDIO_LOOP(
        UGEN_OUT(out, ((double) current_cycle_usecs + ((double) _block_frame * usecs_per_frame)) / MICROS_PER_SECOND);
    );
}

float out_bus_rms(int32_t bus)
{
    if (bus < 0 || bus > 7)
        return 0.0;

    float   squareSum    = 0;
    float*  outBusBuffer = out_bus_buffers[bus];
    uint32_t    i        = 0;

    for(i<512; ++i;)
    {
        squareSum += outBusBuffer[i] * outBusBuffer[i];
    }

    return sqrt(squareSum / 512);
}

#endif // NECRONOMICON_H_INCLUDED
