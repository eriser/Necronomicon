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
#ifndef WIN32
#include <unistd.h>
#endif
#include <jack/jack.h>
#include <time.h>
#include <limits.h>
#include <sndfile.h>

#include "Necronomicon.h"

/////////////////////
// Constants
/////////////////////

typedef enum { false, true } bool;

#ifndef M_PI
#define M_PI 3.1415926535897932384626433832795028841971693993751058209749445923078164062L
#endif

const double TWO_PI = M_PI * 2;
const double RECIP_TWO_PI =  1.0 / (M_PI * 2);
unsigned int DOUBLE_SIZE = sizeof(double);
unsigned int UINT_SIZE = sizeof(unsigned int);

#define TABLE_SIZE 256
double RECIP_TABLE_SIZE = 1.0 / (double) TABLE_SIZE;
double sine_table[TABLE_SIZE];

double SAMPLE_RATE = 44100;
double RECIP_SAMPLE_RATE = 1.0 / 44100.0;
double TABLE_MUL_RECIP_SAMPLE_RATE = TABLE_SIZE * (1.0 / 44100.0);

/////////////////////
// Hashing
/////////////////////

typedef union
{
	unsigned char bytes[4];
	unsigned int word;
} four_bytes;

// FNV1-a hash function
const unsigned long PRIME = 0x01000193; // 16777619
const unsigned long SEED = 0x811C9DC5; // 2166136261

#define FNV1A(byte, hash) ((byte ^ hash) * PRIME)
#define HASH_KEY_PRIV(key) (FNV1A(key.bytes[3], FNV1A(key.bytes[2], FNV1A(key.bytes[1], FNV1A(key.bytes[0], SEED)))))
#define HASH_KEY(key) ((unsigned int) HASH_KEY_PRIV(((four_bytes) key)))

/////////////////////
// Global Mutables
/////////////////////

double* _necronomicon_buses = NULL;
unsigned int num_audio_buses = 256;
unsigned int last_audio_bus_index = 255;
unsigned int num_audio_buses_bytes;
int num_synths = 0;
FILE* devurandom = NULL;

// Time
jack_nframes_t current_cycle_frames = 0;
jack_time_t current_cycle_usecs = 0;
jack_time_t next_cycle_usecs = 0;
float period_usecs = 0;
const jack_time_t USECS_PER_SECOND = 1000000;
double usecs_per_frame = 1000000 / 44100;

double out_bus_buffers[16][512];
unsigned int  out_bus_buffer_index = 0;

/////////////////////
// UGen
/////////////////////

struct ugen;
typedef struct ugen ugen;

struct ugen
{
	void (*calc)(ugen* u);
	void (*constructor)(ugen* u);
	void (*deconstructor)(ugen* u);
	void* data; // ugen defined data structure
	double* constructor_args; // arguments passed in for use during construction
	unsigned int* inputs; // indexes to the parent synth's ugen wire buffer
	unsigned int* outputs; // indexes to the parent synth's ugen wire buffer
};

const unsigned int UGEN_SIZE = sizeof(ugen);
const unsigned int UGEN_POINTER_SIZE = sizeof(ugen*);

typedef void (*ugen_constructor)(ugen* u);
typedef void (*ugen_deconstructor)(ugen* u);
typedef void (*calc_func)(ugen* u);

/////////////////////
// Sample Buffer
/////////////////////

struct sample_buffer;
typedef struct sample_buffer sample_buffer;

struct sample_buffer
{
	double* samples;
	sample_buffer* next_sample_buffer;
	unsigned int pool_index;
	unsigned int num_samples;
	unsigned int num_samples_mask; // used with power of 2 sized buffers
};

const unsigned int SAMPLE_BUFFER_SIZE = sizeof(sample_buffer);
const unsigned int SAMPLE_BUFFER_POINTER_SIZE = sizeof(sample_buffer*);

const unsigned int NUM_SAMPLE_BUFFER_POOLS = 32;
sample_buffer** sample_buffer_pools;

unsigned int next_power_of_two(unsigned int v)
{
	--v;
	v |= v >> 1;
	v |= v >> 2;
	v |= v >> 4;
	v |= v >> 8;
	v |= v >> 16;
	return ++v;
}

void print_sample_buffer(sample_buffer* buffer)
{
	if (buffer != NULL)
	{
		printf(
			"sample_buffer { samples = %p, next_sample_buffer = %p, pool_index = %u, num_samples = %u, num_samples_mask = %u }\n",
			buffer->samples, buffer->next_sample_buffer, buffer->pool_index, buffer->num_samples, buffer->num_samples_mask);   
	}

	else
	{
		printf("sample_buffer = NULL\n");
	}
}

// Note: this will return a sample buffer with a number of samples equal to the next power of two higher than the requested amount
sample_buffer* acquire_sample_buffer(unsigned int num_samples)
{
	unsigned int pow_two_num_samples = next_power_of_two(num_samples);
	unsigned int pool_index = __builtin_ctz(pow_two_num_samples);
	sample_buffer* buffer = sample_buffer_pools[pool_index];

	assert(num_samples <= pow_two_num_samples);
	assert(pool_index < (NUM_SAMPLE_BUFFER_POOLS - 1));
	
	// printf("acquire_sample_buffer(num_samples = %u, pool_index = %u, pooled_buffer = %p.\n", num_samples, pool_index, buffer);
	
	if (buffer != NULL)
	{
		// printf("Reuse buffer :: ");
		sample_buffer_pools[pool_index] = buffer->next_sample_buffer;
	}

	else
	{
		// printf("New buffer :: ");
		buffer = malloc(SAMPLE_BUFFER_SIZE);
		buffer->samples = malloc(pow_two_num_samples * DOUBLE_SIZE);
		buffer->pool_index = pool_index;
		buffer->num_samples = pow_two_num_samples;
		buffer->num_samples_mask = pow_two_num_samples - 1;
	}

	buffer->next_sample_buffer = NULL;
	memset(buffer->samples, 0, pow_two_num_samples * DOUBLE_SIZE);
	// print_sample_buffer(buffer);
	return buffer;
}

void release_sample_buffer(sample_buffer* buffer)
{
	// printf("release_sample_buffer: ");
	// print_sample_buffer(buffer);
	
	if (buffer != NULL)
	{
		unsigned int pool_index = buffer->pool_index;
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

const char* node_alive_status_strings[] = { "NODE_DEAD", "NODE_SPAWNING", "NODE_ALIVE", "NODE_SCHEDULED_FOR_REMOVAL", "NODE_SCHEDULED_FOR_FREE" };

struct synth_node;
typedef struct synth_node synth_node;

struct synth_node
{
	ugen* ugen_graph; // UGen Graph
	double* ugen_wires; // UGen output wire buffers
	synth_node* previous; // Previous node, used in synth_list for the scheduler
	synth_node* next; // Next node, used in the synth_list for the scheduler
	jack_time_t time; // scheduled time, in microseconds
	unsigned int key; // Node ID, used to look up synths in the synth hash table
	unsigned int hash; // Cached hash of the node id for the synth hash table
	unsigned int table_index; // Cached hash table index
	unsigned int num_ugens;
	unsigned int num_wires;
	node_alive_status previous_alive_status; // Flag representing whether the previous status of the synth
	node_alive_status alive_status; // Flag representing the alive status of the synth
};

synth_node* _necronomicon_current_node = NULL;
const unsigned int NODE_SIZE = sizeof(synth_node);
const unsigned int NODE_POINTER_SIZE = sizeof(synth_node*);
synth_node* free_synths = NULL;
int num_free_synths = 0;
const unsigned int max_free_synths = 128;

// Synth hash table
const unsigned int MAX_SYNTHS = 8192;
const unsigned int HASH_TABLE_SIZE_MASK = 8191;
typedef synth_node** hash_table;
hash_table synth_table = NULL;
bool hash_table_remove(hash_table table, synth_node* node);
synth_node* hash_table_lookup(hash_table table, unsigned int key);

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
	unsigned int num_ugens = synth->num_ugens;
	unsigned int i;
	for (i = 0; i < num_ugens; ++i)
	{
		ugen* graph_node = &ugen_graph[i];
		graph_node->deconstructor(graph_node);
	}

	free(ugen_graph);
	free(synth->ugen_wires);
	synth->ugen_graph = NULL;
	synth->ugen_wires = NULL;
}

void free_synth(synth_node* synth)
{
	// printf("free_synth -> ");
	// print_node_alive_status(synth);
	
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

		/*
		  ++num_free_synths;
		if (num_free_synths < max_free_synths)
		{
		}

		else
		{
			free(synth);
			}*/
	}

	else
	{
		puts("free_synth() passed a NULL pointer.");
	}
}

void free_synth_definition(synth_node* synth_definition)
{
	unsigned int i;
	unsigned int num_ugens = synth_definition->num_ugens;
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

synth_node* new_synth(synth_node* synth_definition, double* arguments, unsigned int num_arguments, unsigned int node_id, jack_time_t time)
{
	unsigned int i;

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
	
	// UGens
	unsigned int num_ugens = synth_definition->num_ugens;
	unsigned int size_ugens = synth_definition->num_ugens * UGEN_SIZE;
	synth->ugen_graph = malloc(size_ugens);
	ugen* ugen_graph = synth->ugen_graph;
	memcpy(ugen_graph, synth_definition->ugen_graph, size_ugens);


	for (i = 0; i < num_ugens; ++i)
	{
		ugen* graph_node = &ugen_graph[i];
		graph_node->constructor(graph_node);
	}

	// Wires
	unsigned int size_wires = synth->num_wires * DOUBLE_SIZE;
	double* ugen_wires = malloc(size_wires);
	synth->ugen_wires = ugen_wires;
	memcpy(ugen_wires, synth_definition->ugen_wires, size_wires);

	for (i = 0; i < num_arguments; ++i)
	{
		ugen_wires[i] = arguments[i];
	}

	return synth;
}

void process_synth(synth_node* synth)
{
	ugen* ugen_graph = synth->ugen_graph;
	unsigned int num_ugens = synth->num_ugens;
	unsigned int i;
	for (i = 0; i < num_ugens; ++i)
	{
		ugen* graph_node = &ugen_graph[i];
		graph_node->calc(graph_node);
	}
}

#define UGEN_IN(ugen, index) _necronomicon_current_node->ugen_wires[ugen->inputs[index]]
#define UGEN_OUT(ugen, index, out_value) _necronomicon_current_node->ugen_wires[ugen->outputs[index]] = out_value

void null_deconstructor(ugen* u) {} // Does nothing
void null_constructor(ugen* u) { u->data = NULL; }
void try_schedule_current_synth_for_removal(); // Forward declaration
bool minBLEP_Init(); //Curtis: MinBlep initialization Forward declaration

void initialize_wave_tables()
{
	unsigned int i;
	for (i = 0; i < TABLE_SIZE; ++i)
	{
		sine_table[i] = sin(TWO_PI * (((double) i) / ((double) TABLE_SIZE)));
	}

	// Needed to initialize minblep table.
	bool minblepInitialized = minBLEP_Init();
	devurandom = fopen ("/dev/urandom","r");
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Scheduler
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

const unsigned int MAX_FIFO_MESSAGES = 2048;
const unsigned int FIFO_SIZE_MASK = 2047;

/////////////////
// Message FIFO
/////////////////

typedef union
{
	synth_node* node;
	unsigned int node_id;
	const char* string;
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

const char* message_map[] = { "IGNORE", "START_SYNTH", "STOP_SYNTH", "FREE_SYNTH", "SHUTDOWN", "PRINT", "PRINT_NUMBER" };

typedef struct
{
	message_arg arg;
	message_type type;
} message;

const unsigned int MESSAGE_SIZE = sizeof(message);

void print_fifo_message(message m)
{
	puts(message_map[m.type]);
};

// Lock Free FIFO Queue (Ring Buffer)
typedef message* message_fifo;

message_fifo nrt_fifo = NULL;
unsigned int nrt_fifo_read_index = 0;
unsigned int nrt_fifo_write_index = 0;

message_fifo rt_fifo = NULL;
unsigned int rt_fifo_read_index = 0;
unsigned int rt_fifo_write_index = 0;

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
	unsigned int byte_size = MESSAGE_SIZE * MAX_FIFO_MESSAGES;
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
unsigned int scheduled_list_read_index = 0;
unsigned int scheduled_list_write_index = 0;

#define SCHEDULED_LIST_PUSH(node) FIFO_PUSH(scheduled_node_list, scheduled_list_write_index, node, FIFO_SIZE_MASK)
#define SCHEDULED_LIST_POP() FIFO_POP(scheduled_node_list, scheduled_list_read_index, FIFO_SIZE_MASK)
#define SCHEDULED_LIST_PEEK() (scheduled_node_list[scheduled_list_read_index & FIFO_SIZE_MASK])
#define SCHEDULED_LIST_PEEK_TIME() ((scheduled_node_list[scheduled_list_read_index & FIFO_SIZE_MASK])->time)

// Allocate and null initialize a node list to be used as a node_list
node_list new_node_list()
{
	unsigned int byte_size = NODE_POINTER_SIZE * MAX_FIFO_MESSAGES;
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

	unsigned int i, j, k;
	synth_node* x;
	jack_time_t xTime, yTime;

	// printf("scheduled_list size: %i\n", scheduled_list_write_index - scheduled_list_read_index); 
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

const unsigned int MAX_REMOVAL_IDS = 512; // Max number of ids able to be scheduled for removal *per sample frame*
const unsigned int REMOVAL_FIFO_SIZE_MASK = 511;

typedef synth_node** node_fifo;

node_fifo removal_fifo = NULL;
unsigned int removal_fifo_read_index = 0;
unsigned int removal_fifo_write_index = 0;
int removal_fifo_size = 0;

#define REMOVAL_FIFO_PUSH(id) FIFO_PUSH(removal_fifo, removal_fifo_write_index, id, REMOVAL_FIFO_SIZE_MASK)
#define REMOVAL_FIFO_POP() FIFO_POP(removal_fifo, removal_fifo_read_index, REMOVAL_FIFO_SIZE_MASK)

// Allocate and null initialize a node list to be used as a node_list
node_fifo new_removal_fifo()
{
	unsigned int byte_size = sizeof(synth_node*) * MAX_REMOVAL_IDS;
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
	unsigned int byte_size = NODE_POINTER_SIZE * MAX_SYNTHS;
	hash_table table = (hash_table) malloc(byte_size);
	assert(table);
	memset(table, 0, byte_size);

	return table;
}

void hash_table_free(hash_table table)
{
	unsigned int i;
	for (i = 0; i < MAX_SYNTHS; ++i)
	{
		synth_node* node = table[i];
		free_synth(node);
	}

	free(table);
}

void hash_table_insert(hash_table table, synth_node* node)
{
	unsigned int slot = node->hash & HASH_TABLE_SIZE_MASK;

	while (table[slot] != NULL)
		slot = (slot + 1) & HASH_TABLE_SIZE_MASK;

	table[slot] = node;
	node->table_index = slot;
}

bool hash_table_remove(hash_table table, synth_node* node)
{
	unsigned int index = node->table_index;
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

void print_node(synth_node* node);
synth_node* hash_table_lookup(hash_table table, unsigned int key)
{
	unsigned int hash = HASH_KEY(key);
	unsigned int slot = hash & HASH_TABLE_SIZE_MASK;
	unsigned int i = 0;

	while (i < MAX_SYNTHS)
	{
		printf("table[%u] = ", slot, table[slot]);
		print_node(table[slot]);
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

void clear_necronomicon_buses()
{
	memset(_necronomicon_buses, 0, num_audio_buses_bytes);
}

int get_running()
{
	return (necronomicon_running == true);
}


void add_synth(synth_node* node)
{
	// printf("add_synth -> ");
	// print_node_alive_status(node);
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

		else
		{
			message msg;
			msg.arg.string = "warning: add_synth() was given a node with alive_status that is not NODE_SPAWNING or NODE_SCHEDULED_FOR_REMOVAL";
			msg.type = PRINT;
			NRT_FIFO_PUSH(msg);
		}
	}
}

void remove_synth(synth_node* node)
{
	// printf("remove_synth -> ");
	// print_node_alive_status(node);
	if ((node != NULL) && (node->alive_status == NODE_SCHEDULED_FOR_REMOVAL))
	{
		if (node->previous_alive_status == NODE_ALIVE)
			synth_list = doubly_linked_list_remove(synth_list, node);

		node->previous_alive_status = node->alive_status;
		node->alive_status = NODE_SCHEDULED_FOR_FREE;
		
		message msg;
		msg.arg.node = node;
		msg.type = FREE_SYNTH;
		NRT_FIFO_PUSH(msg); // Send ugen to NRT thread for freeing
	}

	else
	{
		message msg;
		msg.arg.string = "warning: remove_synth() was given node is null or alive_status is not NODE_SCHEDULED_FOR_REMOVAL";
		msg.type = PRINT;
		NRT_FIFO_PUSH(msg);
	}
}

// Iterate over the scheduled list and add synths if they are ready. Stop as soon as we find a synth that isn't ready.
void add_scheduled_synths()
{
	scheduled_list_read_index = scheduled_list_read_index & FIFO_SIZE_MASK;
	scheduled_list_write_index = scheduled_list_write_index & FIFO_SIZE_MASK;
	while (scheduled_list_read_index != scheduled_list_write_index)
	{
		synth_node* node = SCHEDULED_LIST_POP();
		// Uncomment to print timing information for synths
		// printf("add_synth: ugen_graph: %p, time: %llu, current_cycle_usecs: %llu, current_cycle_usecs - time: %llu\n", node->ugen_graph, node->time, current_cycle_usecs, current_cycle_usecs - node->time);
		add_synth(node);
		scheduled_list_read_index = scheduled_list_read_index & FIFO_SIZE_MASK;
		scheduled_list_write_index = scheduled_list_write_index & FIFO_SIZE_MASK;
	}
}

// Iterate over the removal fifo and remove all synths in it.
void remove_scheduled_synths()
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

void try_schedule_current_synth_for_removal()
{
	// puts("try_schedule_current_synth_for_removal()");
	if (_necronomicon_current_node && (removal_fifo_size < REMOVAL_FIFO_SIZE_MASK) && _necronomicon_current_node->alive_status == NODE_ALIVE)
	{
		_necronomicon_current_node->previous_alive_status = _necronomicon_current_node->alive_status;
		_necronomicon_current_node->alive_status = NODE_SCHEDULED_FOR_REMOVAL;
		removal_fifo_size = (removal_fifo_size + 1) & REMOVAL_FIFO_SIZE_MASK;
		REMOVAL_FIFO_PUSH(_necronomicon_current_node);
	}
}

void shutdown_rt_runtime(); // Forward declaration

void handle_rt_message(message msg)
{
	// print_fifo_message(msg);
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
void handle_messages_in_rt_fifo()
{
	rt_fifo_read_index = rt_fifo_read_index & FIFO_SIZE_MASK;
	rt_fifo_write_index = rt_fifo_write_index & FIFO_SIZE_MASK;
	if (rt_fifo_read_index == rt_fifo_write_index)
	{
		return;
	}

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
	// print_fifo_message(msg);
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

void play_synth(synth_node* synth_definition, double* arguments, unsigned int num_arguments, unsigned int node_id, jack_time_t time)
{
	if (num_synths < MAX_SYNTHS)
	{
		synth_node* synth = new_synth(synth_definition, arguments, num_arguments, node_id, time);
		++num_synths;
		hash_table_insert(synth_table, synth);
        // printf("play_synth node id: %u.\n", synth->key);
		message msg;
		msg.arg.node = synth;
		msg.type = START_SYNTH;
		RT_FIFO_PUSH(msg);
	}

	else
	{
		printf("Unable to play synth because the maximum number of synths (%u) are already playing.", MAX_SYNTHS);
	}
}

void stop_synth(unsigned int id)
{
	synth_node* node = hash_table_lookup(synth_table, id);
	if ((node != NULL) && (node->alive_status == NODE_SPAWNING || node->alive_status == NODE_ALIVE))
	{
		// printf("stop_synth node id: %u. ", node->key);
		// print_node_alive_status(node);
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
void send_set_synth_arg(unsigned int id, double argument, unsigned int arg_index)
{
	synth_node* synth = hash_table_lookup(synth_table, id);
	if ((synth != NULL) && (synth->alive_status == NODE_SPAWNING || synth->alive_status == NODE_ALIVE))
	{
		synth->ugen_wires[arg_index] = argument;
	}

	else
	{
		printf("setSynthArg: Node ID %u not found. ", id);
		print_node_alive_status(synth);
	}
}

void send_set_synth_args(unsigned int id, double* arguments, unsigned int num_arguments)
{
	synth_node* synth = hash_table_lookup(synth_table, id);
	if ((synth != NULL) && (synth->alive_status == NODE_SPAWNING || synth->alive_status == NODE_ALIVE))
	{
		memcpy(synth->ugen_wires, arguments, num_arguments * DOUBLE_SIZE);
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
	
	SAMPLE_RATE = jack_get_sample_rate(client);
	RECIP_SAMPLE_RATE = 1.0 / SAMPLE_RATE;
	TABLE_MUL_RECIP_SAMPLE_RATE = TABLE_SIZE * RECIP_SAMPLE_RATE;
	usecs_per_frame = USECS_PER_SECOND / SAMPLE_RATE;

	synth_table = hash_table_new();
	rt_fifo = new_message_fifo();
	scheduled_node_list = new_node_list();
	removal_fifo = new_removal_fifo();

	last_audio_bus_index = num_audio_buses - 1;
	num_audio_buses_bytes = num_audio_buses * DOUBLE_SIZE;
	_necronomicon_buses = malloc(num_audio_buses_bytes);
	clear_necronomicon_buses();

	sample_buffer_pools = (sample_buffer**) malloc(sizeof(sample_buffer*) * NUM_SAMPLE_BUFFER_POOLS);
	memset(sample_buffer_pools, 0, sizeof(sample_buffer*) * NUM_SAMPLE_BUFFER_POOLS);
	
	initialize_wave_tables();
	// load_audio_files();

	memset(out_bus_buffers, 0, DOUBLE_SIZE * 16 * 512);
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
	
	hash_table_free(synth_table);
	rt_fifo_free();
	scheduled_list_free();
	doubly_linked_list_free(synth_list);
	removal_fifo_free();
	free(_necronomicon_buses);

	unsigned int i;
	for (i = 0; i < NUM_SAMPLE_BUFFER_POOLS; ++i)
	{
		sample_buffer* pooled_buffer = sample_buffer_pools[i];
		while(pooled_buffer != NULL)
		{
			sample_buffer* next_buffer = pooled_buffer->next_sample_buffer;
			free(pooled_buffer);
			pooled_buffer = next_buffer;
		}
	}
	
	free(sample_buffer_pools);
	
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
	shutdown_rt_thread();
	jack_client_close(client);
	puts("Necronomicon audio engine shut down.");
}

static void signal_handler(int sig)
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
int process(jack_nframes_t nframes, void* arg)
{
	jack_get_cycle_times(client, &current_cycle_frames, &current_cycle_usecs, &next_cycle_usecs, &period_usecs); // Update time variables for the current cycle
	// Cache the current_cycle_usecs so we can recalculate current_cycle_usecs without rounding errors from iteration
	jack_time_t cached_cycle_usecs = current_cycle_usecs;

	jack_default_audio_sample_t* out0 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port1, nframes);
	jack_default_audio_sample_t* out1 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port2, nframes);
	handle_messages_in_rt_fifo(); // Handles messages including moving uge_nodes from the RT FIFO queue into the scheduled_synth_list

	unsigned int i;
	for (i = 0; i < nframes; ++i)
    {
		clear_necronomicon_buses(); // Zero out the audio buses
		add_scheduled_synths(); // Add any synths that need to start this frame into the current synth_list

		// Iterate through the synth_list, processing each synth
		_necronomicon_current_node = synth_list;
		
		while (_necronomicon_current_node)
		{
			process_synth(_necronomicon_current_node);
			_necronomicon_current_node = _necronomicon_current_node->next;
		}

		out0[i] = _necronomicon_buses[0]; // Write the output of buses 0/1 to jack buffers 0/1, which can be routed or sent directly to the main outs
		out1[i] = _necronomicon_buses[1];

		//Hand un-rolled this loop. The non-unrolled version was causing 13% cpu overhead on my machine, this doesn't make a blip...
		out_bus_buffers[0][out_bus_buffer_index]  = _necronomicon_buses[0];
		out_bus_buffers[1][out_bus_buffer_index]  = _necronomicon_buses[1];
		out_bus_buffers[2][out_bus_buffer_index]  = _necronomicon_buses[2];
		out_bus_buffers[3][out_bus_buffer_index]  = _necronomicon_buses[3];
		out_bus_buffers[4][out_bus_buffer_index]  = _necronomicon_buses[4];
		out_bus_buffers[5][out_bus_buffer_index]  = _necronomicon_buses[5];
		out_bus_buffers[6][out_bus_buffer_index]  = _necronomicon_buses[6];
		out_bus_buffers[7][out_bus_buffer_index]  = _necronomicon_buses[7];
		out_bus_buffers[8][out_bus_buffer_index]  = _necronomicon_buses[8];
		out_bus_buffers[9][out_bus_buffer_index]  = _necronomicon_buses[9];
		out_bus_buffers[10][out_bus_buffer_index] = _necronomicon_buses[10];
		out_bus_buffers[11][out_bus_buffer_index] = _necronomicon_buses[11];
		out_bus_buffers[12][out_bus_buffer_index] = _necronomicon_buses[12];
		out_bus_buffers[13][out_bus_buffer_index] = _necronomicon_buses[13];
		out_bus_buffers[14][out_bus_buffer_index] = _necronomicon_buses[14];
		out_bus_buffers[15][out_bus_buffer_index] = _necronomicon_buses[15];
		out_bus_buffer_index = (out_bus_buffer_index + 1) & 511;

		remove_scheduled_synths(); // Remove any synths that are scheduled for removal and send them to the NRT thread FIFO queue for freeing.
		current_cycle_usecs = cached_cycle_usecs + (jack_time_t) ((double) i * usecs_per_frame); // update current usecs time every sample
    }

	return 0;
}

const char* RESOUCES_PATH;
void start_rt_runtime(const char* resources_path)
{
	puts("Necronomicon audio engine booting");
	RESOUCES_PATH = resources_path;

	const char** ports;
	const char* client_name = "Necronomicon";
	const char* server_name = NULL;
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

#define LINEAR_INTERP(A, B, DELTA) (A + DELTA * (B - A))

#define CUBIC_INTERP(A,B,C,D,DELTA) \
({                                  \
	double delta2 = DELTA * DELTA;  \
	double a0     = D - C - A + B;  \
	double a1     = A - B - a0;     \
	double a2     = C - A;          \
	a0 * DELTA * delta2 + a1 * delta2 + a2 * DELTA + B; \
})

void add_calc(ugen* u)
{
	UGEN_OUT(u, 0, UGEN_IN(u, 0) + UGEN_IN(u, 1));
}

void minus_calc(ugen* u)
{
	UGEN_OUT(u, 0, UGEN_IN(u, 0) - UGEN_IN(u, 1));
}

void mul_calc(ugen* u)
{
	UGEN_OUT(u, 0, UGEN_IN(u, 0) * UGEN_IN(u, 1));
}

void div_calc(ugen* u)
{
	UGEN_OUT(u, 0, UGEN_IN(u, 0) / UGEN_IN(u, 1));
}

void abs_calc(ugen* u)
{
	UGEN_OUT(u, 0, abs(UGEN_IN(u, 0)));
}

void signum_calc(ugen* u)
{
	double value = UGEN_IN(u, 0);

	if (value > 0)
	{
		value = 1;
	}

	else if (value < 0)
	{
		value = -1;
	}

	UGEN_OUT(u, 0, value);
}

void negate_calc(ugen* u)
{
	UGEN_OUT(u, 0, -(UGEN_IN(u, 0)));
}

void pow_calc(ugen* u)
{
	UGEN_OUT(u, 0, pow(UGEN_IN(u, 0), UGEN_IN(u, 1)));
}

void exp_calc(ugen* u)
{
	UGEN_OUT(u, 0, exp(UGEN_IN(u, 0)));
}

void log_calc(ugen* u)
{
	UGEN_OUT(u, 0, log(UGEN_IN(u, 0)));
}

void cos_calc(ugen* u)
{
	UGEN_OUT(u, 0, cos(UGEN_IN(u, 0)));
}

void asin_calc(ugen* u)
{
	UGEN_OUT(u, 0, asin(UGEN_IN(u, 0)));
}

void acos_calc(ugen* u)
{
	UGEN_OUT(u, 0, acos(UGEN_IN(u, 0)));
}

void atan_calc(ugen* u)
{
	UGEN_OUT(u, 0, atan(UGEN_IN(u, 0)));
}

void logbase_calc(ugen* u)
{
	UGEN_OUT(u, 0, log(UGEN_IN(u, 0)) / log(UGEN_IN(u, 1)));
}

void line_constructor(ugen* u)
{
	u->data = malloc(UINT_SIZE); // Line time
	*((unsigned int*) u->data) = 0;
}

void sqrt_calc(ugen* u)
{
	UGEN_OUT(u, 0, sqrt(UGEN_IN(u, 0)));
}

void tan_calc(ugen* u)
{
	UGEN_OUT(u, 0, tan(UGEN_IN(u, 0)));
}

void sinh_calc(ugen* u)
{
	UGEN_OUT(u, 0, sinh(UGEN_IN(u, 0)));
}

void cosh_calc(ugen* u)
{
	UGEN_OUT(u, 0, cosh(UGEN_IN(u, 0)));
}

void tanh_calc(ugen* u)
{
	UGEN_OUT(u, 0, tanh(UGEN_IN(u, 0)));
}

void asinh_calc(ugen* u)
{
	UGEN_OUT(u, 0, asinh(UGEN_IN(u, 0)));
}

void atanh_calc(ugen* u)
{
	UGEN_OUT(u, 0, atanh(UGEN_IN(u, 0)));
}

void acosh_calc(ugen* u)
{
	UGEN_OUT(u, 0, acosh(UGEN_IN(u, 0)));
}

void line_deconstructor(ugen* u)
{
	free(u->data);
}

// To do: Give this range parameters
void line_calc(ugen* u)
{
	double length = UGEN_IN(u, 0) * SAMPLE_RATE;
	unsigned int line_time = *((unsigned int*) u->data);
	double output = 0;

	if (line_time >= length)
	{
		try_schedule_current_synth_for_removal();
	}

	else
	{
		output = fmax(0, 1 - (line_time / length));
		*((unsigned int*) u->data) = line_time + 1;
	}

	UGEN_OUT(u, 0, output);
}

void perc_calc(ugen* u)
{
	double       length    = UGEN_IN(u, 0) * SAMPLE_RATE;
	double       peak      = UGEN_IN(u, 1);
	double       curve     = UGEN_IN(u, 2);
	double       x         = UGEN_IN(u, 3);
	unsigned int line_time = *((unsigned int*) u->data);
	double       y         = 0;

	//Wtf is with negative values?
	if(curve < 0)
	{
		curve = 1 / ((curve * -1) + 1);
	}
	else
	{
		curve = curve + 1;
	}

	if(line_time >= length)
	{
		try_schedule_current_synth_for_removal();
	}
	else
	{
		y = pow(fmax(0, 1 - (line_time / length)),curve) * x * peak;
		*((unsigned int*) u->data) = line_time + 1;
	}

	UGEN_OUT(u, 0, y);
}

#define OFF_INDEX(OFFSET,INDEX,ARRAY)

void env_calc(ugen* u)
{
	double       curve      = UGEN_IN(u, 0);
	double       x          = UGEN_IN(u, 1);
	// double       length     = UGEN_IN(u, 2) * SAMPLE_RATE;
	int          valsLength = (int) UGEN_IN(u, 2);
	int          dursLength = (int) UGEN_IN(u, 3);
	int          valsOffset = 4;
	int          dursOffset = 4 + valsLength;
	unsigned int line_time  = *((unsigned int*) u->data);
	double       line_timed = (double) line_time * RECIP_SAMPLE_RATE;
	double       y          = 0;

	if(valsLength == 0 || dursLength == 0)
	{
		if(valsLength == 0)
			printf("Vals length 0.\n");

		if(dursLength == 0)
			printf("Durs length 0.\n");

		UGEN_OUT(u,0,0);
	}
	else
	{
		double currentDuration   = 0;
		double nextDuration      = 0;

		double curTotalDuration  = 0;
		double nextTotalDuration = 0;
		double totalDuration     = 0;

		double currentValue      = 0;
		double nextValue         = 0;

		int i;
	    for(i=0;i<valsLength - 1;++i)
	    {
	    	currentDuration   = nextDuration;
			nextDuration      = UGEN_IN(u,(i % dursLength)+dursOffset);

	    	curTotalDuration += currentDuration;
	    	nextTotalDuration = curTotalDuration + nextDuration;
			totalDuration     = nextTotalDuration;

			currentValue      = UGEN_IN(u,i     + valsOffset);
			nextValue         = UGEN_IN(u,i + 1 + valsOffset);

	    	if(nextTotalDuration > line_timed)
	    		break;
	    }

		for(;i<valsLength - 1; ++i)
		{
			totalDuration += UGEN_IN(u,(i % dursLength)+dursOffset);
		}

	    // if(line_time >= length)
		if(line_timed >= totalDuration)
		{
			try_schedule_current_synth_for_removal();
		}
	    else
	    {
			if(curve < 0)
		    	curve = 1 / ((curve * -1) + 1);
		    else
		    	curve = curve + 1;

			double delta               = pow((line_timed - curTotalDuration) / (nextTotalDuration - curTotalDuration), curve);
			y                          = ((1-delta) * currentValue + delta * nextValue) * x;
	    	*((unsigned int*) u->data) = line_time + 1;
	    }

		UGEN_OUT(u, 0, y);
	}
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

void sin_calc(ugen* u)
{
	double freq = UGEN_IN(u, 0);
	double phase = *((double*) u->data);

	unsigned char index1 = phase;
	unsigned char index2 = index1 + 1;
	double amp1 = sine_table[index1];
	double amp2 = sine_table[index2];
	double delta = phase - ((long) phase);
	double amplitude = amp1 + delta * (amp2 - amp1);

	*((double*) u->data) = phase + TABLE_MUL_RECIP_SAMPLE_RATE * freq;
	UGEN_OUT(u, 0, amplitude);
}

void local_out_calc(ugen* u)
{
	UGEN_OUT(u, 0, UGEN_IN(u, 0));
}

void out_calc(ugen* u)
{
	// Constrain bus index to the correct range
	unsigned char bus_index = UGEN_IN(u, 0);
	_necronomicon_buses[bus_index] += UGEN_IN(u, 1);
}

void in_calc(ugen* u)
{
	// Constrain bus index to the correct range
	unsigned char bus_index = UGEN_IN(u, 0);
	UGEN_OUT(u, 0, _necronomicon_buses[bus_index]);
}

void poll_constructor(ugen* u)
{
	unsigned int* count_buffer = (unsigned int*) malloc(sizeof(unsigned int));
	*count_buffer = 0;
	u->data = count_buffer;
}

void poll_calc(ugen* u)
{
	unsigned int* count_buffer = (unsigned int*) u->data;
	unsigned int count = *count_buffer;

	if (count >= SAMPLE_RATE)
	{
		double input = UGEN_IN(u, 0);
		message msg;
		msg.arg.number = input;
		msg.type = PRINT_NUMBER;
		NRT_FIFO_PUSH(msg);
		count = 0;
	}

	else
	{
		count += 10;
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
	long write_index;
} delay_data;

const unsigned int DELAY_DATA_SIZE = sizeof(delay_data);

void delayN_constructor(ugen* u)
{
	u->data = malloc(DELAY_DATA_SIZE);
	double max_delay_time = fmax(1, u->constructor_args[0] * SAMPLE_RATE);
	delay_data data = { acquire_sample_buffer(max_delay_time), max_delay_time, 0 };
	*((delay_data*) u->data) = data;
}

void delayL_constructor(ugen* u)
{
	u->data = malloc(DELAY_DATA_SIZE);
	double max_delay_time = fmax(2, u->constructor_args[0] * SAMPLE_RATE);
	delay_data data = { acquire_sample_buffer(max_delay_time), fmax(0, max_delay_time - 1), 0 };
	*((delay_data*) u->data) = data;
}

void delayC_constructor(ugen* u)
{
	u->data = malloc(DELAY_DATA_SIZE);
	double max_delay_time = fmax(3, u->constructor_args[0] * SAMPLE_RATE);
	delay_data data = { acquire_sample_buffer(max_delay_time), fmax(0, max_delay_time - 2), 0 };
	*((delay_data*) u->data) = data;
}

void delay_deconstructor(ugen* u)
{
	release_sample_buffer(((delay_data*) u->data)->buffer);
	free(u->data);
}

#define INIT_DELAY(u)									  \
delay_data data = *((delay_data*) u->data);				  \
sample_buffer buffer = *data.buffer;					  \
long write_index = data.write_index;					  \
unsigned int num_samples_mask = buffer.num_samples_mask;  \
double delay_time = UGEN_IN(u, 0) * SAMPLE_RATE;		  \
double x = UGEN_IN(u, 1);

#define FINISH_DELAY(u, y)								  \
buffer.samples[write_index & num_samples_mask] = x;	      \
data.write_index = write_index + 1;						  \
*((delay_data*) u->data) = data;						  \
UGEN_OUT(u, 0, y);

void delayN_calc(ugen* u)
{
	INIT_DELAY(u);
	delay_time = fmin(data.max_delay_time, fmax(1, delay_time));
	long iread_index = write_index - (long) delay_time;
	double y = iread_index < 0 ? 0 : buffer.samples[iread_index & num_samples_mask];
	FINISH_DELAY(u, y);
}

void delayL_calc(ugen* u)
{
	INIT_DELAY(u);
	delay_time = fmin(data.max_delay_time, fmax(1, delay_time));
	double y;
	double read_index = (double) write_index - delay_time;
	long iread_index0 = (long) read_index;

	if (iread_index0 < 0)
	{
		y = 0;
	}

	else
	{
		long iread_index1 = iread_index0 - 1;
		double delta = read_index - iread_index0;
		double y0 = buffer.samples[iread_index0 & num_samples_mask];
		double y1 = iread_index1 < 0 ? 0 : buffer.samples[iread_index1 & num_samples_mask];
		y  = LINEAR_INTERP(y0, y1, delta);
	}

	FINISH_DELAY(u, y);
}

void delayC_calc(ugen* u)
{
	INIT_DELAY(u);
	// Clamp delay at 1 to prevent the + 1 iread_index3 from reading on the wrong side of the write head
	delay_time = fmin(data.max_delay_time, fmax(2, delay_time));
	double read_index  = (double) write_index - delay_time;
	long iread_index0 = (long) read_index;
	long iread_index1 = iread_index0 - 1;
	long iread_index2 = iread_index0 - 2;
	long iread_index3 = iread_index0 + 1;
	double delta = read_index - iread_index0;
	double y0 = iread_index0 < 0 ? 0 : buffer.samples[iread_index0 & num_samples_mask];
	double y1 = iread_index1 < 0 ? 0 : buffer.samples[iread_index1 & num_samples_mask];
	double y2 = iread_index2 < 0 ? 0 : buffer.samples[iread_index2 & num_samples_mask];
	double y3 = iread_index3 < 0 ? 0 : buffer.samples[iread_index3 & num_samples_mask];
	double y  = CUBIC_INTERP(y0, y1, y2, y3, delta);
	FINISH_DELAY(u, y);
}

void print_ugen(ugen* ugen)
{
	puts("(UGen");
	printf(" (Calc %p)\n", ugen->calc);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//// Test FIFO

synth_node* new_test_synth(unsigned int time)
{
	ugen test_ugen = { &sin_calc, &sin_constructor, &sin_deconstructor, NULL, NULL, NULL };
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
		printf("synth_node { ugen_graph: %p, ugen_wires: %p, previous: %p, next: %p, time: %llu, key %u, hash: %u, table_index: %u, num_ugens: %u, num_wires: %u, previous_alive_status: %u, alive_status %u }\n",
			   node->ugen_graph,
			   node->ugen_wires,
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
	unsigned int i = scheduled_list_read_index & FIFO_SIZE_MASK;
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

	unsigned int i;
	for (i = 0; i < 1000; ++i)
	{
		unsigned int num_pop = random() / (double) RAND_MAX * 100;
	    while ((num_pop > 0) && ((scheduled_list_read_index & FIFO_SIZE_MASK) != ((scheduled_list_write_index - 1) & FIFO_SIZE_MASK)))
		{
			SCHEDULED_LIST_POP();
			--num_pop;
		}

		unsigned int num_push = random() / (double) RAND_MAX * 100;
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

	unsigned int i = 0;
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

	unsigned int i;
	for (i = 0; i < MAX_SYNTHS; ++i)
	{
		// print_node(table[i]);
		if (i < (MAX_SYNTHS - 1))
			printf(", ");
	}

	printf("]\n\n");
}

unsigned int num_values = 5000;
unsigned int times[5000];

void test_hash_table()
{
	unsigned int i = 0;
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

	return (int) node->time % 2 == 1;
}

void test_doubly_linked_list()
{
	int i; // Don't make this unsigned, we'll go infinite!
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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Curtis: New UGens
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define LERP(A,B,D) (A+D*(B-A))
#define CLAMP(V,MIN,MAX) \
({\
	double result = V < MIN ? MIN : V; \
	result        = V > MAX ? MAX : V; \
	result; \
})


#define HARD_CLIP(X,AMOUNT) (CLAMP(X*AMOUNT,-1.0,1.0))
#define POLY3_DIST(X,AMOUNT) (1.5 * X - 0.5 * pow(X,3))
#define TANH_DIST(X,AMOUNT) (tanh(X*AMOUNT))
#define SIN_DIST(X,AMOUNT) (sin(X*AMOUNT)/M_PI)
#define WRAP(X,AMOUNT)       \
({                           \
	double x   = X * AMOUNT; \
	double ret = x;          \
	if(x >= 1)               \
		ret = x - 2;         \
	else if(x < -1)          \
		ret = x + 2;         \
	ret;                     \
})

#define WAVE_TABLE_AMPLITUDE(phase,table) \
({ \
	double amp1  = table[(unsigned char)phase]; \
	double amp2  = table[(unsigned char)(phase+1)]; \
	double delta = phase - ((long) phase); \
	amp1 + delta * (amp2 - amp1); \
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

//LFOS
double RECIP_CHAR_RANGE = 1.0 / 255.0;
void lfsaw_calc(ugen* u)
{
	double freq     = UGEN_IN(u, 0);
	double phaseArg = UGEN_IN(u, 1);
	double phase    = *((double*) u->data);

	//Branchless and table-less saw
	double        amp1      = ((double)((char)phase));
	double        amp2      = ((double)(((char)phase)+1));
	double        delta     = phase - ((long) phase);
	double        amplitude = LERP(amp1,amp2,delta) * RECIP_CHAR_RANGE;

	*((double*) u->data) = phase + phaseArg + TABLE_MUL_RECIP_SAMPLE_RATE * freq;
	UGEN_OUT(u, 0, amplitude);
}

void lfpulse_calc(ugen* u)
{
	double freq     = UGEN_IN(u, 0);
	double phaseArg = UGEN_IN(u, 1);
	double phase    = *((double*) u->data);

	//Branchless and table-less square
	double amplitude = 1 | (((char)phase) >> (sizeof(char) * CHAR_BIT - 1));

	*((double*) u->data) = phase + phaseArg + TABLE_MUL_RECIP_SAMPLE_RATE * freq;
	UGEN_OUT(u, 0, amplitude);
}

//---------------------------------------------
//MinBlep Bandwidth-limited Saw and Square
//---------------------------------------------

#define KTABLE 64 // BLEP table oversampling factor

typedef struct
{
	double *lpTable;
	int     c;
} minbleptable_t;

typedef struct
{
	double  output;
	double  phase;
	double  masterPhase;
	double *buffer;      // circular output buffer
	int     cBuffer;	 // buffer size
	int     iBuffer;	 // current buffer position
	int     nInit;		 // amount of initialized entries
	double  prevSyncAmp; //For hardsync
} minblep;

minbleptable_t gMinBLEP;

bool minBLEP_Init()
{
	// load table
	const char* blep_table = "/misc/minblep.mat";
	char path_to_blep_table[strlen(RESOUCES_PATH) + strlen(blep_table)];
	strcat(path_to_blep_table,RESOUCES_PATH);
	strcat(path_to_blep_table,blep_table);
	FILE *fp=fopen(path_to_blep_table,"rb");
	unsigned int iSize;

	if (!fp) return false;

	fseek(fp,0x134,SEEK_SET);

	fread(&iSize,sizeof(int),1,fp);
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
	int    i;
	double *out       = mb->buffer  + mb->iBuffer;
	double *in        = gMinBLEP.lpTable + (int) (KTABLE*offset);
	double frac       = fmod(KTABLE*offset,1.0);
	int    cBLEP      = (gMinBLEP.c / KTABLE) - 1;
	double *bufferEnd = mb->buffer  + mb->cBuffer;
	double f;

	// add
	for(i=0; i < mb->nInit; i++, in += KTABLE, out++)
	{
		if(out >= bufferEnd)
			out = mb->buffer;

		f = LERP(in[0],in[1],frac);
		*out += amp * (1-f);
	}

	// copy
	for(; i < cBLEP; i++, in += KTABLE, out++)
	{
		if(out >= bufferEnd)
			out = mb->buffer;

		f = LERP(in[0],in[1],frac);
		*out = amp*(1-f);
	}

	mb->nInit = cBLEP;
}

void saw_calc(ugen* u)
{
	double   freq      = UGEN_IN(u, 0) * RECIP_SAMPLE_RATE;
	// double   pwm       = UGEN_IN(u, 1);
	minblep* mb        = (minblep*) u->data;
	double   amplitude = 0.0;

	// create waveform
	mb->phase = mb->phase + freq;

	// add BLEP at end of waveform
	if (mb->phase >= 1)
	{
		mb->phase  = mb->phase - 1.0;
		mb->output = 0.0;
		add_blep(mb, mb->phase/freq,1.0);
	}

	// add BLEP in middle of wavefor for squarewave
	// if (!mb->output && mb->phase > pwm && mb->type==OT_SQUARE)
	// {
		// mb->v=1.0;
		// osc_AddBLEP(mb, (mb->p-mb->fPWM)/fs,-1.0);
	// }

	// sample value
	// if (mb->type==OT_SAW)
		// v=mb->p;
	// else
		// v=mb->v;

	amplitude = mb->phase;

	// add BLEP buffer contents
	if(mb->nInit)
	{
		amplitude += mb->buffer[mb->iBuffer];
		mb->nInit--;
		if(++mb->iBuffer >= mb->cBuffer)
			mb->iBuffer=0;
	}

	UGEN_OUT(u, 0, amplitude);
}

void square_calc(ugen* u)
{
	double   freq      = UGEN_IN(u, 0) * RECIP_SAMPLE_RATE;
	double   pwm       = CLAMP(UGEN_IN(u, 1),0,1) * 0.5;
	minblep* mb        = (minblep*) u->data;
	double   amplitude = 0.0;

	// create waveform
	mb->phase = mb->phase + freq;

	// add BLEP at end of waveform
	if (mb->phase >= 1)
	{
		mb->phase  = mb->phase - 1.0;
		mb->output = 0.0;
		add_blep(mb, mb->phase/freq,1.0);
	}

	// add BLEP in middle of wavefor for squarewave
	if(!mb->output && mb->phase > pwm)
	{
		mb->output = 1.0;
		add_blep(mb, (mb->phase - pwm) / freq,-1.0);
	}

	amplitude = mb->output;

	// add BLEP buffer contents
	if(mb->nInit)
	{
		amplitude += mb->buffer[mb->iBuffer];
		mb->nInit--;
		if(++mb->iBuffer >= mb->cBuffer)
			mb->iBuffer=0;
	}

	UGEN_OUT(u, 0, amplitude);
}

void syncsaw_calc(ugen* u)
{
	double   freq      = UGEN_IN(u, 0) * RECIP_SAMPLE_RATE;
	double   sync      = UGEN_IN(u, 1);
	minblep* mb        = (minblep*) u->data;
	double   amplitude = 0.0;

	// create waveform
	mb->phase = mb->phase + freq;

	// add BLEP at end of waveform
	if (mb->phase >= 1)
	{
		mb->phase  = mb->phase - 1.0;
		mb->output = 0.0;
		add_blep(mb, mb->phase/freq,1.0);
	}
	else if(mb->prevSyncAmp < 0 && sync > 0)
	{
		mb->phase  = 0.0;
		mb->output = 0.0;
		add_blep(mb, mb->phase/freq,1.0);
	}

	amplitude = mb->phase;

	// add BLEP buffer contents
	if(mb->nInit)
	{
		amplitude += mb->buffer[mb->iBuffer];
		mb->nInit--;
		if(++mb->iBuffer >= mb->cBuffer)
			mb->iBuffer=0;
	}

	mb->prevSyncAmp = sync;
	UGEN_OUT(u, 0, amplitude);
}

void syncsquare_calc(ugen* u)
{
	double   freq      = UGEN_IN(u, 0) * RECIP_SAMPLE_RATE;
	double   pwm       = CLAMP(UGEN_IN(u, 1),0,1) * 0.5;
	double   sync      = UGEN_IN(u, 2);
	minblep* mb        = (minblep*) u->data;
	double   amplitude = 0.0;

	// create waveform
	mb->phase = mb->phase + freq;

	// add BLEP at end of waveform
	if (mb->phase >= 1)
	{
		mb->phase  = mb->phase - 1.0;
		mb->output = -1.0;
		add_blep(mb, mb->phase/freq,1.0);
	}

	// add BLEP in middle of wavefor for squarewave
	if(!mb->output && mb->phase > pwm)
	{
		mb->output = 1.0;
		add_blep(mb, (mb->phase - pwm) / freq,-1.0);
	}

	if(mb->prevSyncAmp < 0 && sync > 0)
	{
		mb->phase  = 0.0;
		mb->output = 1.0;
		add_blep(mb, mb->phase/freq,1.0);
	}

	amplitude = mb->output;

	// add BLEP buffer contents
	if(mb->nInit)
	{
		amplitude += mb->buffer[mb->iBuffer];
		mb->nInit--;
		if(++mb->iBuffer >= mb->cBuffer)
			mb->iBuffer=0;
	}

	mb->prevSyncAmp = sync;
	UGEN_OUT(u, 0, amplitude);
}

void syncosc_calc(ugen* u)
{
	double   slaveFreq  = UGEN_IN(u,0);
	int      slaveWave  = (int)UGEN_IN(u,1);
	double   pwm        = CLAMP(UGEN_IN(u, 2),0,1) * 0.5;
	double   masterFreq = UGEN_IN(u,3);
	minblep* mb         = (minblep*) u->data;
	double   y          = 0.0;
	double   freqN      = slaveFreq * RECIP_SAMPLE_RATE;

	// create waveform
	mb->phase       = mb->phase + freqN;
	mb->masterPhase = WRAP(mb->masterPhase + (masterFreq * RECIP_SAMPLE_RATE) * 1.0,1.0);

	// add BLEP at end of waveform
	if(mb->phase >= 1)
	{
		mb->phase  = mb->phase - 1.0;
		mb->output = -1.0;
		add_blep(mb, mb->phase/freqN,1.0);
	}

	// add BLEP in middle of wavefor for squarewave
	else if(slaveWave && !mb->output && mb->phase > pwm)
	{
		mb->output = 1.0;
		add_blep(mb, (mb->phase - pwm) / freqN,-1.0);
	}

	else if(mb->prevSyncAmp <= 0 && mb->masterPhase > 0)
	{
		mb->phase  = mb->masterPhase * (slaveFreq / masterFreq);
		if(!slaveWave)
			mb->output = mb->masterPhase * (slaveFreq / masterFreq);
		else
			mb->output = -1.0;
		add_blep(mb, mb->phase/freqN,1.0);
	}

	if(!slaveWave)
		y = mb->phase;
	else
		y = mb->output;

	// add BLEP buffer contents
	if(mb->nInit)
	{
		y += mb->buffer[mb->iBuffer];
		mb->nInit--;
		if(++mb->iBuffer >= mb->cBuffer)
			mb->iBuffer=0;
	}

	mb->prevSyncAmp = mb->masterPhase;
	UGEN_OUT(u, 0, y);
}

#define CUBIC_INTERP(A,B,C,D,DELTA) \
({                                  \
	double delta2 = DELTA * DELTA;  \
	double a0     = D - C - A + B;  \
	double a1     = A - B - a0;     \
	double a2     = C - A;          \
	a0 * DELTA * delta2 + a1 * delta2 + a2 * DELTA + B; \
})

//==========================================
// Randomness
//==========================================

#define RAND_RANGE(MIN,MAX) ( ((double)random() / (double) RAND_MAX) * (MAX - MIN) + MIN )

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
	rand->value0 = RAND_RANGE(0,1);
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

void rand_calc(ugen* u)
{
	rand_t* rand  = (rand_t*) u->data;
	double  amp   = rand->value0;// * range + min;

	UGEN_OUT(u,0,amp);
}

void lfnoiseN_calc(ugen* u)
{
	double  freq  = UGEN_IN(u,0);
	rand_t* rand  = (rand_t*) u->data;

	if(rand->phase + RECIP_SAMPLE_RATE * freq >= 1.0)
	{
		rand->phase  = fmod(rand->phase + RECIP_SAMPLE_RATE * freq,1.0);
		rand->value0 = RAND_RANGE(0,1);
	}
	else
	{
		rand->phase = rand->phase + RECIP_SAMPLE_RATE * freq;
	}

	double  amp = rand->value0;// * range + min;

	UGEN_OUT(u, 0, amp);
}

void lfnoiseL_calc(ugen* u)
{
	double  freq  = UGEN_IN(u,0);
	rand_t* rand  = (rand_t*) u->data;

	if(rand->phase + RECIP_SAMPLE_RATE * freq >= 1.0)
	{
		rand->phase  = fmod(rand->phase + RECIP_SAMPLE_RATE * freq,1.0);
		rand->value1 = rand->value0;
		rand->value0 = RAND_RANGE(0,1);
	}
	else
	{
		rand->phase = rand->phase + RECIP_SAMPLE_RATE * freq;
	}

	double amp = LERP(rand->value1,rand->value0,rand->phase);// * range + min;

	UGEN_OUT(u, 0, amp);
}

void lfnoiseC_calc(ugen* u)
{
	double  freq  = UGEN_IN(u,0);
	rand_t* rand  = (rand_t*) u->data;

	if(rand->phase + RECIP_SAMPLE_RATE * freq >= 1.0)
	{
		rand->phase  = fmod(rand->phase + RECIP_SAMPLE_RATE * freq,1.0);
		rand->value3 = rand->value2;
		rand->value2 = rand->value1;
		rand->value1 = rand->value0;
		rand->value0 = RAND_RANGE(0,1);
	}
	else
	{
		rand->phase = rand->phase + RECIP_SAMPLE_RATE * freq;
	}

	double amp = CUBIC_INTERP(rand->value3,rand->value2,rand->value1,rand->value0,rand->phase);// * range + min;

	UGEN_OUT(u, 0, amp);
}

void range_calc(ugen* u)
{
	double min   = UGEN_IN(u,0);
	double range = UGEN_IN(u,1) - min;
	double in    = CLAMP(UGEN_IN(u,2),-1,1) * 0.5 + 0.5;
	double amp   = (in * range) + min;

	UGEN_OUT(u, 0, amp);
}

void exprange_calc(ugen* u)
{
	double min   = UGEN_IN(u,0);
	double range = UGEN_IN(u,1) - min;
	double in    = CLAMP(UGEN_IN(u,2),-1,1) * 0.5 + 0.5;
	double amp   = (pow(range,in)) + min;

	UGEN_OUT(u, 0, amp);
}

//  /dev/urandom

typedef union {
  double d;
  char   bytes[8];
} rand_double_t;

typedef struct
{
	double        phase;
	rand_double_t value0;
	rand_double_t value1;
	rand_double_t value2;
	rand_double_t value3;
	// FILE*         file;
} urand_t;


void urand_constructor(ugen* u)
{
	urand_t* rand  = malloc(sizeof(urand_t));

	fgets(rand->value0.bytes,8,devurandom);
	rand->value0.d = fmod(rand->value0.d,2) - 1;

	printf("urandom: %f",rand->value0.d);

	rand->value1.d = 0;
	rand->value2.d = 0;
	rand->value3.d = 0;
	rand->phase    = 0;
	u->data        = rand;
}

void urand_deconstructor(ugen* u)
{
	urand_t* rand = (urand_t*)u->data;
	// fclose(rand->file);
	free(u->data);
}

void urand_calc(ugen* u)
{
	rand_t* rand = (rand_t*) u->data;
	UGEN_OUT(u,0,rand->value0);
}

void impulse_calc(ugen* u)
{
	double freqN  = UGEN_IN(u,0) * RECIP_SAMPLE_RATE;
	double offset = UGEN_IN(u,1);
	double phase  = *((double*) u->data);
	double y      = 0;

	if(phase + freqN >= 1)
	{
		y = 1;
	}

	(*(double*)u->data) = fmod(phase + freqN,1);

	UGEN_OUT(u,0,y);
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

void dust_calc(ugen* u)
{
	dust_t* dust   = (dust_t*)u->data;
	double density = UGEN_IN(u,0);
	double y       = 0;

	if(dust->period == -1)
		dust->period = RAND_RANGE(0,2);

	if(dust->phase + density * RECIP_SAMPLE_RATE >= dust->period)
	{
		y            = RAND_RANGE(-1,1);
		dust->phase  = 0;
		dust->period = RAND_RANGE(0,2);
	}
	else
	{
		dust->phase = dust->phase + density  * RECIP_SAMPLE_RATE;
	}

	UGEN_OUT(u,0,y);
}

void dust2_calc(ugen* u)
{
	dust_t* dust   = (dust_t*)u->data;
	double density = UGEN_IN(u,0);
	double y       = 0;

	if(dust->period == -1)
		dust->period = RAND_RANGE(0,2);

	if(dust->phase + density * RECIP_SAMPLE_RATE >= dust->period)
	{
		y            = RAND_RANGE(0,1);
		dust->phase  = 0;
		dust->period = RAND_RANGE(0,2);
	}
	else
	{
		dust->phase = dust->phase + RECIP_SAMPLE_RATE;
	}

	UGEN_OUT(u,0,y);
}

//===================================
// RBJ Filters, Audio EQ Cookbook
//===================================

typedef struct
{
	double x1;
	double x2;
	double y1;
	double y2;
} biquad_t;

void biquad_constructor(ugen* u)
{
	biquad_t* biquad = malloc(sizeof(biquad_t));
	biquad->x1       = 0;
	biquad->x2       = 0;
	biquad->y1       = 0;
	biquad->y2       = 0;
	u->data          = biquad;
}

void biquad_deconstructor(ugen* u)
{
	free(u->data);
}

#define BIQUAD(B0,B1,B2,A0,A1,A2,X,X1,X2,Y1,Y2) ( (B0/A0)*X + (B1/A0)*X1 + (B2/A0)*X2 - (A1/A0)*Y1 - (A2/A0)*Y2 )

void lpf_calc(ugen* u)
{
	double freq  = UGEN_IN(u,0);
	double q     = UGEN_IN(u,1);
	double in    = UGEN_IN(u,2);

	biquad_t* bi = (biquad_t*) u->data;

	double omega = 2 * M_PI * freq * RECIP_SAMPLE_RATE;
	double cs    = cos(omega);
    double sn    = sin(omega);
	double alpha = sn * sinh(1 / (2 * q));

    double b0    = (1 - cs)/2;
    double b1    =  1 - cs;
    double b2    = (1 - cs)/2;
    double a0    =  1 + alpha;
    double a1    = -2*cs;
    double a2    =  1 - alpha;

	double out   = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi->x1,bi->x2,bi->y1,bi->y2);

	bi->y2 = bi->y1;
	bi->y1 = out;
	bi->x2 = bi->x1;
	bi->x1 = in;

	UGEN_OUT(u,0,out);
}

void hpf_calc(ugen* u)
{
	double freq  = UGEN_IN(u,0);
	double q     = UGEN_IN(u,1);
	double in    = UGEN_IN(u,2);

	biquad_t* bi = (biquad_t*) u->data;

	double omega = 2 * M_PI * freq * RECIP_SAMPLE_RATE;
	double cs    = cos(omega);
    double sn    = sin(omega);
	double alpha = sn * sinh(1 / (2 * q));

	double b0    = (1 + cs)/2;
    double b1    = -1 - cs;
    double b2    = (1 + cs)/2;
    double a0    =  1 + alpha;
    double a1    = -2*cs;
    double a2    =  1 - alpha;

	double out   = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi->x1,bi->x2,bi->y1,bi->y2);

	bi->y2 = bi->y1;
	bi->y1 = out;
	bi->x2 = bi->x1;
	bi->x1 = in;

	UGEN_OUT(u,0,out);
}

void bpf_calc(ugen* u)
{
	double freq  = UGEN_IN(u,0);
	double q     = UGEN_IN(u,1);
	double in    = UGEN_IN(u,2);

	biquad_t* bi = (biquad_t*) u->data;

	double omega = 2 * M_PI * freq * RECIP_SAMPLE_RATE;
	double cs    = cos(omega);
    double sn    = sin(omega);
	double alpha = sn * sinh(1 / (2 * q));

	double b0    =  alpha;
    double b1    =  0;
    double b2    = -alpha;
    double a0    =  1 + alpha;
    double a1    = -2*cs;
    double a2    =  1 - alpha;

	double out   = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi->x1,bi->x2,bi->y1,bi->y2);

	bi->y2 = bi->y1;
	bi->y1 = out;
	bi->x2 = bi->x1;
	bi->x1 = in;

	UGEN_OUT(u,0,out);
}

void notch_calc(ugen* u)
{
	double freq  = UGEN_IN(u,0);
	double gain  = UGEN_IN(u,1);
	double q     = UGEN_IN(u,2);
	double in    = UGEN_IN(u,3);

	biquad_t* bi = (biquad_t*) u->data;

	double omega = 2 * M_PI * freq * RECIP_SAMPLE_RATE;
	double cs    = cos(omega);
    double sn    = sin(omega);
	double alpha = sn * sinh(1 / (2 * q));

	double b0    =  1;
    double b1    = -2*cs;
    double b2    =  1;
    double a0    =  1 + alpha;
    double a1    = -2*cs;
    double a2    =  1 - alpha;

	double out   = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi->x1,bi->x2,bi->y1,bi->y2);

	bi->y2 = bi->y1;
	bi->y1 = out;
	bi->x2 = bi->x1;
	bi->x1 = in;

	UGEN_OUT(u,0,out);
}

void allpass_calc(ugen* u)
{
	double freq  = UGEN_IN(u,0);
	double q     = UGEN_IN(u,1);
	double in    = UGEN_IN(u,2);

	biquad_t* bi = (biquad_t*) u->data;

	double omega = 2 * M_PI * freq * RECIP_SAMPLE_RATE;
	double cs    = cos(omega);
    double sn    = sin(omega);
	double alpha = sn * sinh(1 / (2 * q));

	double b0    =   1 - alpha;
    double b1    =  -2*cs;
    double b2    =   1 + alpha;
    double a0    =   1 + alpha;
    double a1    =  -2*cs;
    double a2    =   1 - alpha;

	double out   = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi->x1,bi->x2,bi->y1,bi->y2);

	bi->y2 = bi->y1;
	bi->y1 = out;
	bi->x2 = bi->x1;
	bi->x1 = in;

	UGEN_OUT(u,0,out);
}

void peakEQ_calc(ugen* u)
{
	double freq  = UGEN_IN(u,0);
	double gain  = UGEN_IN(u,1);
	double q     = UGEN_IN(u,2);
	double in    = UGEN_IN(u,3);

	biquad_t* bi = (biquad_t*) u->data;

	double a     = pow(10,(gain/40));
	double omega = 2 * M_PI * freq * RECIP_SAMPLE_RATE;
	double cs    = cos(omega);
    double sn    = sin(omega);
	double alpha = sn * sinh(1 / (2 * q));

	double b0    =  1 + alpha*a;
    double b1    = -2*cs;
    double b2    =  1 - alpha*a;
    double a0    =  1 + alpha/a;
    double a1    = -2*cs;
    double a2    =  1 - alpha/a;

	double out   = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi->x1,bi->x2,bi->y1,bi->y2);

	bi->y2 = bi->y1;
	bi->y1 = out;
	bi->x2 = bi->x1;
	bi->x1 = in;

	UGEN_OUT(u,0,out);
}

void lowshelf_calc(ugen* u)
{
	double freq  = UGEN_IN(u,0);
	double gain  = UGEN_IN(u,1);
	double slope = UGEN_IN(u,2);
	double in    = UGEN_IN(u,3);

	biquad_t* bi = (biquad_t*) u->data;

	double a     = pow(10,(gain/40));
	double omega = 2 * M_PI * freq * RECIP_SAMPLE_RATE;
	double cs    = cos(omega);
    double sn    = sin(omega);
	double beta  = sqrt( (pow(a,2) + 1) / slope - pow((a-1),2) );

	double b0    =    a*( (a+1) - (a-1)*cs + beta*sn );
    double b1    =  2*a*( (a-1) - (a+1)*cs           );
    double b2    =    a*( (a+1) - (a-1)*cs - beta*sn );
    double a0    =        (a+1) + (a-1)*cs + beta*sn;
    double a1    =   -2*( (a-1) + (a+1)*cs           );
    double a2    =        (a+1) + (a-1)*cs - beta*sn;

	double out   = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi->x1,bi->x2,bi->y1,bi->y2);

	bi->y2 = bi->y1;
	bi->y1 = out;
	bi->x2 = bi->x1;
	bi->x1 = in;

	UGEN_OUT(u,0,out);
}

void highshelf_calc(ugen* u)
{
	double freq  = UGEN_IN(u,0);
	double gain  = UGEN_IN(u,1);
	double slope = UGEN_IN(u,2);
	double in    = UGEN_IN(u,3);

	biquad_t* bi = (biquad_t*) u->data;

	double a     = pow(10,(gain/40));
	double omega = 2 * M_PI * freq * RECIP_SAMPLE_RATE;
	double cs    = cos(omega);
    double sn    = sin(omega);
	double beta  = sqrt( (pow(a,2) + 1) / slope - pow((a-1),2) );

	double b0    =    a*( (a+1) + (a-1)*cs + beta*sn );
    double b1    = -2*a*( (a-1) + (a+1)*cs           );
    double b2    =    a*( (a+1) + (a-1)*cs - beta*sn );
    double a0    =        (a+1) - (a-1)*cs + beta*sn;
    double a1    =    2*( (a-1) - (a+1)*cs           );
    double a2    =        (a+1) - (a-1)*cs - beta*sn;

	double out   = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi->x1,bi->x2,bi->y1,bi->y2);

	bi->y2 = bi->y1;
	bi->y1 = out;
	bi->x2 = bi->x1;
	bi->x1 = in;

	UGEN_OUT(u,0,out);
}

void lag_calc(ugen* u)
{
	double lagTime = UGEN_IN(u,0);
	double input   = UGEN_IN(u,1);
	double z       = *((double*) u->data);
    double a       = exp((-2 * M_PI) / (lagTime * SAMPLE_RATE));
    double b       = 1.0f - a;
	z              = (input * b) + (z * a);
	*((double*) u->data) = z;
	UGEN_OUT(u,0,z);
}

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

inline double zERO_DELAY_ONE_POLE(double X,double G,double* SS,int I)
{
	double Y   = X * G + SS[I];
	double XmY = X - Y;
	SS[I]      = ZERO_DELAY(XmY,G,SS[I]);
	return Y;
}

//Consider different shaping functions
#define SOFT_CLIP(X,AMOUNT) (atan(X*AMOUNT)/M_PI)

void zeroDelayOnePole_calc(ugen* u)
{
	double freq                = UGEN_IN(u,0);
	double x                   = UGEN_IN(u,1);
	zeroDelayFilter_t* zerodft = (zeroDelayFilter_t*)u->data;

	double warped              = PREWARP(freq,RECIP_SAMPLE_RATE);
	double g                   = warped / (warped + 1);
	double y                   = zERO_DELAY_ONE_POLE(x,g,zerodft->ss,0);

	UGEN_OUT(u,0,y);
}

//Add wave shaper once this is confirmed to work.
//Consider adding a base level amount of noise (a small amount).
void zeroDelayLPMS20_calc(ugen* u)
{
	double freq                = UGEN_IN(u,0);
	double resonance           = UGEN_IN(u,1);
	double distortion          = UGEN_IN(u,2);
	double x                   = UGEN_IN(u,3);
	zeroDelayFilter_t* zerodft = (zeroDelayFilter_t*)u->data;

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

//============================================
// Distortion
//============================================

void clip_calc(ugen* u)
{
	double amount = UGEN_IN(u,0);
	double x      = UGEN_IN(u,1);
	double y      = HARD_CLIP(x,amount);
	UGEN_OUT(u,0,y);
}

void softclip_calc(ugen* u)
{
	double amount = UGEN_IN(u,0);
	double x      = UGEN_IN(u,1);
	double y      = SOFT_CLIP(x,amount);
	UGEN_OUT(u,0,y);
}

void poly3_calc(ugen* u)
{
	double amount = UGEN_IN(u,0);
	double x      = UGEN_IN(u,1);
	double y      = POLY3_DIST(x,amount);
	UGEN_OUT(u,0,y);
}

void tanhdist_calc(ugen* u)
{
	double amount = UGEN_IN(u,0);
	double x      = UGEN_IN(u,1);
	double y      = TANH_DIST(x,amount);
	UGEN_OUT(u,0,y);
}

void sinDist_calc(ugen* u)
{
	double amount = UGEN_IN(u,0);
	double x      = UGEN_IN(u,1);
	double y      = SIN_DIST(x,amount);
	UGEN_OUT(u,0,y);
}

void wrap_calc(ugen* u)
{
	double amount = UGEN_IN(u,0);
	double x      = UGEN_IN(u,1);
	double y      = WRAP(x,amount);
	UGEN_OUT(u,0,y);
}

#define ROUND(f) ((float)((f > 0.0) ? floor(f + 0.5) : ceil(f - 0.5)))

void crush_calc(ugen* u)
{
    int    bitDepth = UGEN_IN(u,0);
    double x        = UGEN_IN(u,1);
    int    max      = pow(2, bitDepth) - 1;
    double y        = ROUND((x + 1.0) * max) / max - 1.0;

    UGEN_OUT(u,0,y);
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

void decimate_calc(ugen* u)
{
    decimate_t* decimate = (decimate_t*) u->data;
    double      rate     = UGEN_IN(u,0) * RECIP_SAMPLE_RATE;
    double      x        = UGEN_IN(u,1);
    double      y        = 0;

    if(decimate->samples + rate >= 1)
    {
        decimate->samples = fmod(decimate->samples + rate,1.0);
        decimate->prev    = x;
        y                 = x;
    }
    else
    {
        decimate->samples += rate;
        y = decimate->prev;
    }

    UGEN_OUT(u,0,y);
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
	freeverb_data* freeverb       = malloc(sizeof(freeverb_data));
	freeverb->combFilterDelays    = malloc(sizeof(delay_data) * 8);
	freeverb->combz1s             = malloc(sizeof(double) * 8);
	freeverb->allpassDelays       = malloc(sizeof(delay_data) * 4);

	delay_data data0 = { acquire_sample_buffer(1557), 1557, 0 };
	freeverb->combFilterDelays[0] = data0;
	delay_data data1 = { acquire_sample_buffer(1617), 1617, 0 };
	freeverb->combFilterDelays[1] = data1;
	delay_data data2 = { acquire_sample_buffer(1491), 1491, 0 };
	freeverb->combFilterDelays[2] = data2;
	delay_data data3 = { acquire_sample_buffer(1422), 1422, 0 };
	freeverb->combFilterDelays[3] = data3;
	delay_data data4 = { acquire_sample_buffer(1277), 1277, 0 };
	freeverb->combFilterDelays[4] = data4;
	delay_data data5 = { acquire_sample_buffer(1356), 1356, 0 };
	freeverb->combFilterDelays[5] = data5;
	delay_data data6 = { acquire_sample_buffer(1188), 1188, 0 };
	freeverb->combFilterDelays[6] = data6;
	delay_data data7 = { acquire_sample_buffer(1116), 1116, 0 };
	freeverb->combFilterDelays[7] = data7;

	freeverb->combz1s[0] = 0;
	freeverb->combz1s[1] = 0;
	freeverb->combz1s[2] = 0;
	freeverb->combz1s[3] = 0;
	freeverb->combz1s[4] = 0;
	freeverb->combz1s[5] = 0;
	freeverb->combz1s[6] = 0;
	freeverb->combz1s[7] = 0;

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
	int i;
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

//Should the delayed signal in this be x or y or x + y???
#define COMB_FILTER(X,R,D,N,DATA,ZS,I)                                                       \
({                                                                                           \
	sample_buffer* buffer           = DATA[I].buffer;                                        \
	unsigned int   write_index      = DATA[I].write_index;                                   \
	unsigned int   num_samples_mask = buffer->num_samples_mask;                              \
	double         damp1            = D * 0.4;                                               \
	double         damp2            = 1 - damp1;                                             \
	double         feedback         = R * 0.28 + 0.7;                                        \
	double         y                = buffer->samples[write_index];                          \
	ZS[I]                           = y * damp2 + ZS[I] * damp1;                             \
	buffer->samples[write_index]    = (X * fixed_gain) + ZS[I] * feedback;                   \
	DATA[I].write_index             = (write_index + 1) & num_samples_mask;                  \
	y;                                                                                       \
})

#define ALLPASS_FEEDBACK(X,F,N,DATA,I)                                                       \
({                                                                                           \
    sample_buffer* buffer           = DATA[I].buffer;                                        \
    unsigned int   write_index      = DATA[I].write_index;                                   \
    unsigned int   num_samples_mask = buffer->num_samples_mask;                              \
    double         bufout           = buffer->samples[write_index];                          \
	double         y                = -X + bufout;                                           \
    buffer->samples[write_index]    = X + bufout * F;                                        \
	DATA[I].write_index             = (write_index + 1) & num_samples_mask;                  \
	y;                                                                                       \
})


void freeverb_calc(ugen* u)
{
	freeverb_data* data     = ((freeverb_data*) u->data);
	double         mix      = CLAMP(UGEN_IN(u, 0),0,1);
	double         roomSize = CLAMP(UGEN_IN(u, 1),0,1);
	double         damp     = CLAMP(UGEN_IN(u, 2),0,1);
	double         x        = CLAMP(UGEN_IN(u, 3),0,1);

	double         cf0      = COMB_FILTER(x,roomSize,damp,1557,data->combFilterDelays,data->combz1s,0);
	double         cf1      = COMB_FILTER(x,roomSize,damp,1617,data->combFilterDelays,data->combz1s,1);
	double         cf2      = COMB_FILTER(x,roomSize,damp,1491,data->combFilterDelays,data->combz1s,2);
	double         cf3      = COMB_FILTER(x,roomSize,damp,1422,data->combFilterDelays,data->combz1s,3);
	double         cf4      = COMB_FILTER(x,roomSize,damp,1277,data->combFilterDelays,data->combz1s,4);
	double         cf5      = COMB_FILTER(x,roomSize,damp,1356,data->combFilterDelays,data->combz1s,5);
	double         cf6      = COMB_FILTER(x,roomSize,damp,1188,data->combFilterDelays,data->combz1s,6);
	double         cf7      = COMB_FILTER(x,roomSize,damp,1116,data->combFilterDelays,data->combz1s,7);

	double         cfy      = cf0 + cf1 + cf2 + cf3 + cf3 + cf5 + cf6 + cf7;

    double         ap0      = ALLPASS_FEEDBACK(cfy,0.5,225,data->allpassDelays,0);
	double         ap1      = ALLPASS_FEEDBACK(ap0,0.5,556,data->allpassDelays,1);
	double         ap2      = ALLPASS_FEEDBACK(ap1,0.5,441,data->allpassDelays,2);
	double         ap3      = ALLPASS_FEEDBACK(ap2,0.5,341,data->allpassDelays,3);

    double         y        = (x * (1 - mix)) + (ap3 * mix * 1.0);

	UGEN_OUT(u,0,y);
}

#define E 2.7182818284590452353602874713527
typedef unsigned int uint;

typedef struct
{
	sample_buffer* buffer;
	uint write_index;
	uint noiseSamples;
	uint minFreq;
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

#define MAX( a, b ) ( ( a > b) ? a : b )

//Jaffe and Smith "Extensions of the Karplus-Strong Plucked-String* Algorithm"
void pluck_calc(ugen* u)
{
	pluck_data* data             = ((pluck_data*) u->data);
	uint        write_index      = data->write_index;
	uint        num_samples_mask = data->buffer->num_samples_mask;
	double      freq             = MAX(UGEN_IN(u, 0),data->minFreq) * RECIP_SAMPLE_RATE;
	uint        n                = SAMPLE_RATE / MAX(UGEN_IN(u, 0),data->minFreq);
	double      duration         = UGEN_IN(u, 1) * SAMPLE_RATE;
	double      x                = UGEN_IN(u, 2);
	uint        index2           = (write_index + 1) % n;
	double      decay            = pow(E, (-(n + 0.5) * 6.908) / duration) / cos(M_PI * freq) ;
	double      y                = 0;

	if(data->noiseSamples < n)
	{
		y = x;
		data->noiseSamples++;
	}
	else
	{
		y = decay * (data->buffer->samples[write_index] + data->buffer->samples[index2]) / 2;
	}

	data->buffer->samples[write_index] = y;
	data->write_index                  = index2;
	UGEN_OUT(u, 0, y);
}

void white_calc(ugen* u)
{
	UGEN_OUT(u, 0, RAND_RANGE(-1,1));
}
