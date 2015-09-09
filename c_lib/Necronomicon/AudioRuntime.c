/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney

    To Do:

    zapgremlins in filters to prevent blow ups
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

    various cool pattern generators (fractal, chaos, generative, etc..., etc...)
    chord/scale monad
    meta-patterns, pattern selectors, pattern filters
    bit twiddling ugens (support for Data.Bits type class)
    beat based ugens (bseq, blace, brand, etc...)
    better line and xline implementations (with range arguments)
    Other env ugens like adr, adsr, etc...
    glitchy oscillators, with table range and maybe phase multiplier? Is this doable with just an osc ugen?
    Decay
    Pitch Shift
    Freq Shift
    Compressor/Expander
    Chorus/Flanger/Phaser
    Trigger UGens
    Sample Playback / Record / Buffer UGens
    Correct Groups implementation
    Wave Shaper
    Wave Tables
    Pitch Detection
    Zero Crossing
    Yig Gens
    Wave Terrain (3D Wave terrain? 1D Wave Terrain? 4D? ND?)
    basic filters (One pole, two pole, zero, HZPF, integrator)
    demand style ugens (dseq, drand, etc...)
    random seeding

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

#include "AudioRuntime.h"
#include "Necronomicon/Endian.h"
#include "Necronomicon/Util.h"

/////////////////////
// Constants
/////////////////////

const uint32_t DOUBLE_SIZE = sizeof(double);
const uint32_t UINT_SIZE = sizeof(uint32_t);

const double TWO_PI = M_PI * 2;
const double RECIP_TWO_PI =  1.0 / (M_PI * 2);
const double HALF_PI = M_PI * 0.5;
const double QUARTER_PI = M_PI * 0.25;

double RECIP_TABLE_SIZE = 1.0 / DOUBLE_TABLE_SIZE;
uint32_t HALF_TABLE_SIZE = TABLE_SIZE / 2;
uint32_t QUATER_TABLE_SIZE = TABLE_SIZE / 4;
double sine_table[TABLE_SIZE];
double cosn_table[TABLE_SIZE];
double sinh_table[TABLE_SIZE];
double atan_table[TABLE_SIZE];
double tanh_table[TABLE_SIZE];

double PAN_RECIP_TABLE_SIZE = 1.0 / DOUBLE_PAN_TABLE_SIZE;
uint32_t PAN_HALF_TABLE_SIZE = PAN_TABLE_SIZE / 2;
uint32_t PAN_QUATER_TABLE_SIZE = PAN_TABLE_SIZE / 4;
double pan_table[PAN_TABLE_SIZE];

double SAMPLE_RATE = 44100;
double RECIP_SAMPLE_RATE = 1.0 / 44100.0;
double TABLE_MUL_RECIP_SAMPLE_RATE = TABLE_SIZE * (1.0 / 44100.0);
double TWO_PI_TIMES_RECIP_SAMPLE_RATE;
uint32_t BLOCK_SIZE = 0;

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

const uint32_t UGEN_SIZE = sizeof(ugen);
const uint32_t UGEN_POINTER_SIZE = sizeof(ugen*);
const uint32_t UGEN_GRAPH_POOL_NODE_SIZE = sizeof(ugen_graph_pool_node);
const uint32_t UGEN_WIRE_POOL_NODE_SIZE = sizeof(ugen_wires_pool_node);

void print_ugen(ugen* u)
{
    if (u != NULL)
    {
        printf(
            "ugen %p { calc = %p, constructor = %p, deconstructor = %p, data = %p, constructor_args = %p, inputs = %p, outputs = %p, calc_rate = %i }\n",
            u,
            u->calc,
            u->constructor,
            u->deconstructor,
            u->data,
            u->constructor_args,
            u->inputs,
            u->outputs,
            u->calc_rate
        );
    }

    else
    {
        puts("ugen { NULL }");
    }
}

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

const uint32_t NUM_UGEN_GRAPH_POOLS = 32;
ugen_graph_pool_node** ugen_graph_pools = NULL;

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

const uint32_t SAMPLE_BUFFER_SIZE = sizeof(sample_buffer);
const uint32_t SAMPLE_BUFFER_POINTER_SIZE = sizeof(sample_buffer*);

const uint32_t NUM_SAMPLE_BUFFER_POOLS = 32;
sample_buffer** sample_buffer_pools;

void print_sample_buffer(sample_buffer* buffer)
{
    if (buffer != NULL)
    {
        printf(
            "sample_buffer %p { samples = %p, next_sample_buffer = %p, pool_index = %u, num_samples = %u, num_samples_mask = %u, num_channels: %u }\n",
            buffer, buffer->samples, buffer->next_sample_buffer, buffer->pool_index, buffer->num_samples, buffer->num_samples_mask, buffer->num_channels);
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
        buffer->num_channels = 1;
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

///////////////////////////
// Sample Registry
///////////////////////////

const int32_t SAMPLE_HASH_TABLE_SIZE = 2048;

hash_table sample_hash_table;

void create_sample_hash_table()
{
    sample_hash_table = hash_table_new(SAMPLE_HASH_TABLE_SIZE);
}

void free_sample_buffer(void* void_sample_buffer)
{
    if (void_sample_buffer != NULL)
    {
        sample_buffer* buffer = (sample_buffer*) void_sample_buffer;
        if (buffer->samples != NULL)
            free(buffer->samples);

        free(buffer);
    }
}

void free_sample_hash_table()
{
    hash_table_free_with_callback(sample_hash_table, free_sample_buffer);
}

// expects a null terminated string
void register_sample_buffer(const char* file_path, sample_buffer* buffer)
{
    hash_table_insert_string_key(sample_hash_table, (void*) buffer, file_path);
}

sample_buffer* retrieve_sample_buffer(const char* file_path)
{
    sample_buffer* buffer = NULL;
    if (file_path != NULL)
    {
        buffer = hash_table_lookup_string_key(sample_hash_table, file_path);
    }
    else
    {
        puts("retrieve_sample_buffer: file_path = NULL");
    }
    return buffer;
}

const char* retrieve_sample_buffer_name_string(const char* file_path)
{
    return hash_table_get_string_key(sample_hash_table, file_path);
}

void print_sfinfo(SF_INFO sfinfo)
{
    printf("SF_INFO { frames: %u, samplerate: %i, channels: %i, format: %i, sections: %i, seekable: %i }\n",
        (uint32_t) sfinfo.frames,
        (int32_t)  sfinfo.samplerate,
        (int32_t)  sfinfo.channels,
        (int32_t)  sfinfo.format,
        (int32_t)  sfinfo.sections,
        (int32_t)  sfinfo.seekable
    );
}

sample_buffer* load_sample_into_buffer(const char* file_path)
{
    const int32_t one_buffer = 1;
    sample_buffer* buffer = calloc(one_buffer, SAMPLE_BUFFER_SIZE);

    SF_INFO sfinfo;
    memset(&sfinfo, 0, sizeof(sfinfo)); // zero initialize

    SNDFILE* sndfile = sf_open(file_path, SFM_READ, &sfinfo);
    if (sndfile)
    {
        printf("loaded sound file %s\n", file_path);
        const sf_count_t items = sfinfo.frames * (sf_count_t) sfinfo.channels;
        buffer->samples = calloc(items, DOUBLE_SIZE);
        buffer->next_sample_buffer = NULL;
        buffer->pool_index = 0;
        buffer->num_samples = items;
        buffer->num_samples_mask = 0;
        buffer->num_channels = sfinfo.channels;

        sf_read_double(sndfile, buffer->samples, items);
        if (sf_close(sndfile) != 0)
            printf("Error closing sound file %s\n", file_path);
    }

    else
    {
        printf("Error opening sound file %s\n", file_path);
        printf("sndfile_error -> %s\n", sf_strerror(sndfile));
    }

    return buffer;
}

void load_and_register_sample(const char* file_path)
{
    sample_buffer* buffer = load_sample_into_buffer(file_path);
    register_sample_buffer(file_path, buffer);
}

void load_and_register_samples(const char** file_paths, uint32_t num_files)
{
    uint32_t i;
    for(i = 0; i < num_files; ++i)
    {
        load_and_register_sample(file_paths[i]);
    }
}

////////////////////////
// SynthDef/Synth Node
////////////////////////

const uint32_t NODE_SIZE = sizeof(synth_node);
const uint32_t NODE_POINTER_SIZE = sizeof(synth_node*);
const uint32_t MAX_SYNTHS = 8192;
const uint32_t SYNTH_HASH_TABLE_SIZE_MASK = 8191;

const int8_t* node_alive_status_strings[] = { "NODE_DEAD", "NODE_SPAWNING", "NODE_ALIVE", "NODE_SCHEDULED_FOR_REMOVAL", "NODE_SCHEDULED_FOR_FREE" };

synth_node* _necronomicon_current_node = NULL;
synth_node* _necronomicon_current_node_underconstruction = NULL;
synth_node* free_synths = NULL;
int32_t num_free_synths = 0;
const uint32_t max_free_synths = 128;

// Synth hash table
synth_hash_table synth_table = NULL;

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
        bool found = synth_hash_table_remove(synth_table, synth);
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

    // printf("synth_definition : wires [");
    // for (i = 0; i < num_wires; ++i)
    // {
    //     printf("%f", synth_definition->ugen_wires[i * BLOCK_SIZE]);
    //     if (i < (synth->num_wires - 1))
    //         printf(", ");
    // }
    // printf("]\n");

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
        // print_ugen(graph_node);
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
        __int128_t time_dif = (__int128_t) synth->time - (__int128_t) current_cycle_usecs;
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

void null_deconstructor(ugen* u) {} // Does nothing
void null_constructor(ugen* u) { u->data = NULL; }

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

/////////////////
// Message FIFO
/////////////////

const int8_t* message_map[] = { "IGNORE", "START_SYNTH", "STOP_SYNTH", "FREE_SYNTH", "SHUTDOWN", "PRINT", "PRINT_NUMBER" };

void print_fifo_message(message m)
{
    puts(message_map[m.type]);
};

const uint32_t MESSAGE_SIZE = sizeof(message);
const uint32_t MAX_FIFO_MESSAGES = 2048;
const uint32_t FIFO_SIZE_MASK = 2047;

// Lock Free FIFO Queue (Ring Buffer)

message_fifo nrt_fifo = NULL;
uint32_t nrt_fifo_read_index = 0;
uint32_t nrt_fifo_write_index = 0;

message_fifo rt_fifo = NULL;
uint32_t rt_fifo_read_index = 0;
uint32_t rt_fifo_write_index = 0;

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

node_list scheduled_node_list = NULL;
uint32_t scheduled_list_read_index = 0;
uint32_t scheduled_list_write_index = 0;

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

node_fifo removal_fifo = NULL;
uint32_t removal_fifo_read_index = 0;
uint32_t removal_fifo_write_index = 0;
int32_t removal_fifo_size = 0;

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

synth_hash_table synth_hash_table_new()
{
    synth_hash_table table = (synth_hash_table) calloc(MAX_SYNTHS, NODE_POINTER_SIZE);
    assert(table);
    return table;
}

void synth_hash_table_free(synth_hash_table table)
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

void synth_hash_table_insert(synth_hash_table table, synth_node* node)
{
    uint32_t slot = node->hash & SYNTH_HASH_TABLE_SIZE_MASK;

    while (table[slot] != NULL)
        slot = (slot + 1) & SYNTH_HASH_TABLE_SIZE_MASK;

    table[slot] = node;
    node->table_index = slot;
}

bool synth_hash_table_remove(synth_hash_table table, synth_node* node)
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
        printf("synth_hash_table_remove: found_node %p != node %p\n", found_node, node);
        return false;
    }
}

synth_node* synth_hash_table_lookup(synth_hash_table table, uint32_t key)
{
    uint32_t hash = HASH_KEY(key);
    uint32_t slot = hash & SYNTH_HASH_TABLE_SIZE_MASK;
    uint32_t i = 0;

    while (i < MAX_SYNTHS)
    {
        if (table[slot] != NULL)
        {
            if (table[slot]->key == key)
                return table[slot];
            else
                printf("Found synth node in synth_hash_table_lookup, but not the one we're after. Looking up node ID %u but found %u.\n", key, table[slot]->key);
        }

        ++i;
        slot = (slot + 1) & SYNTH_HASH_TABLE_SIZE_MASK;
    }

    return NULL;
}

///////////////////////////
// Doubly Linked List
///////////////////////////

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

void assert_block_size()
{
    assert(BLOCK_SIZE != 0);
}

uint32_t get_block_size()
{
    return BLOCK_SIZE;
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
               (unsigned long long) node->time,
               node->key,
               node->hash,
               node->table_index,
               node->num_ugens,
               node->num_wires,
               node->previous_alive_status,
               node->alive_status);
    else
        printf("NULL\n");
}

void print_synth_wires(synth_node* synth)
{
    if (synth != NULL)
    {
        printf("synth_node : wires [");

        int32_t i;
        for (i = 0; i < synth->num_wires; ++i)
        {
            printf("%f", synth->ugen_wires[i * BLOCK_SIZE]);
            if (i < (synth->num_wires - 1))
                printf(", ");
        }

        printf("]\n");
    }
    else
    {
        puts("synth { null }");
    }
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
        synth_hash_table_insert(synth_table, synth);
        message msg;
        msg.arg.node = synth;
        msg.type = START_SYNTH;
        RT_FIFO_PUSH(msg);
        // print_node(synth);
        // print_synth_wires(synth);
    }

    else
    {
        printf("Unable to play synth because the maximum number of synths (%u) are already playing.\n", MAX_SYNTHS);
    }
}

void stop_synth(uint32_t id)
{
    synth_node* node = synth_hash_table_lookup(synth_table, id);
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
        printf("stopSynth: Node ID %u not found.\n", id);
        print_node_alive_status(node);
    }
}

// How to handle sample accurate setting? Use FIFO messages?
void send_set_synth_arg(uint32_t id, double argument, uint32_t arg_index)
{
    synth_node* synth = synth_hash_table_lookup(synth_table, id);
    if ((synth != NULL) && (synth->alive_status == NODE_SPAWNING || synth->alive_status == NODE_ALIVE))
    {
        double* wire_buffer = synth->ugen_wires + (arg_index * BLOCK_SIZE);
        *wire_buffer = argument;
    }

    else
    {
        printf("setSynthArg: Node ID %u not found.\n", id);
        print_node_alive_status(synth);
    }
}

void send_set_synth_args(uint32_t id, double* arguments, uint32_t num_arguments)
{
    synth_node* synth = synth_hash_table_lookup(synth_table, id);
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
        printf("setSynthArgs: Node ID %u not found.\n", id);
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

    synth_table = synth_hash_table_new();
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
    create_sample_hash_table();

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

    synth_hash_table_free(synth_table);
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
    free_sample_hash_table();
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
        _out_bus_0[out_bus_buffer_index] = (_necronomicon_buses_out0[i] * 0.5) + 0.5;
        _out_bus_1[out_bus_buffer_index] = (_necronomicon_buses_out1[i] * 0.5) + 0.5;
        _out_bus_2[out_bus_buffer_index] = (_necronomicon_buses_out2[i] * 0.5) + 0.5;
        _out_bus_3[out_bus_buffer_index] = (_necronomicon_buses_out3[i] * 0.5) + 0.5;
        _out_bus_4[out_bus_buffer_index] = (_necronomicon_buses_out4[i] * 0.5) + 0.5;
        _out_bus_5[out_bus_buffer_index] = (_necronomicon_buses_out5[i] * 0.5) + 0.5;
        _out_bus_6[out_bus_buffer_index] = (_necronomicon_buses_out6[i] * 0.5) + 0.5;
        _out_bus_7[out_bus_buffer_index] = (_necronomicon_buses_out7[i] * 0.5) + 0.5;
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
        fprintf(stderr, "cannot activate client\n");
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
