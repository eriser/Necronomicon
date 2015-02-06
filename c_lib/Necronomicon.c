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
unsigned int num_synths = 0;

/////////////////////
// Time
/////////////////////

typedef struct
{
	time_t tv_sec;  /* seconds */
    long   tv_nsec; /* nanoseconds */
} timespec;

timespec current_time;

unsigned int current_sample_time = 0;

/////////////////////
// SynthDef
/////////////////////

struct ugen;
typedef struct ugen ugen;

struct ugen
{
	void (*calc)(ugen* u);
	void (*constructor)(ugen* u);
	void (*deconstructor)(ugen* u);
	void* data; // ugen defined data structure
	unsigned int* inputs; // indexes to the parent synth's ugen wire buffer
	unsigned int* outputs; // indexes to the parent synth's ugen wire buffer
};

unsigned int UGEN_SIZE = sizeof(ugen);

typedef void (*ugen_constructor)(ugen* u);
typedef void (*ugen_deconstructor)(ugen* u);
typedef void (*calc_func)(ugen* u);

struct synth_node;
typedef struct synth_node synth_node;

struct synth_node
{
	ugen* ugen_graph; // UGen Graph
	double* ugen_wires; // UGen output wire buffers
	double* local_buses; // local buses used for feedback
	synth_node* previous; // Previous node, used in synth_list for the scheduler
	synth_node* next; // Next node, used in the synth_list for the scheduler
	unsigned int key; // Node ID, used to look up synths in the synth hash table
	unsigned int hash; // Cached hash of the node id for the synth hash table
	unsigned int num_ugens;
	unsigned int num_wires;
	unsigned int num_buses; // number of local buses, used for feedback
	unsigned int time; // scheduled time, in samples
};

synth_node* _necronomicon_current_node = NULL;
const unsigned int NODE_SIZE = sizeof(synth_node);
const unsigned int NODE_POINTER_SIZE = sizeof(synth_node*);

void free_synth_definition(synth_node* synth_definition)
{
	unsigned int i;
	unsigned int num_ugens = synth_definition->num_ugens;
	for (i = 0; i < num_ugens; ++i)
	{
		ugen* u = &synth_definition->ugen_graph[i];
		free(u->inputs);
		free(u->outputs);
	}

	free(synth_definition->ugen_graph);
	free(synth_definition);
}

synth_node* new_synth(synth_node* synth_definition, double* arguments, unsigned int num_arguments, unsigned int node_id, unsigned int time)
{
	unsigned int i;

	/*
	printf("synth_definition->ugen_wires: [");
	for (i = 0; i < synth_definition->num_wires; ++i)
	{
		printf("%f,", synth_definition->ugen_wires[i]);
	}

	printf("]\n");

	puts("Building synth");*/

	synth_node* synth = malloc(NODE_SIZE);
	synth->previous = NULL;
	synth->next = NULL;
	synth->key = node_id;
	synth->hash = HASH_KEY(node_id);
	synth->num_ugens = synth_definition->num_ugens;
	synth->num_wires = synth_definition->num_wires;
	synth->time = time;

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

	// Local buses
	unsigned int num_buses = synth_definition->num_buses;
	if (num_buses > 0)
	{
		synth->num_buses = num_buses;
		unsigned int size_buses = num_buses * DOUBLE_SIZE;
		double* local_buses = malloc(size_buses);
		synth->local_buses = local_buses;
		memset(local_buses, 0, size_buses);
	}

	else
	{
		synth->local_buses = NULL;
		synth->num_buses = 0;
	}

	/*
	printf("synth->ugen_wires: [");
	for (i = 0; i < synth->num_wires; ++i)
	{
		printf("%f,", synth->ugen_wires[i]);
	}

	printf("]\n");*/

	return synth;
}

void free_synth(synth_node* synth)
{
	if (synth)
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

		if (synth->local_buses)
			free(synth->local_buses);

		free(synth);
	}
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

	//Curtis: Needed to initialize minblep table.
	bool minblepInitialized = minBLEP_Init();
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
	double xTime, yTime;

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

const unsigned int MAX_REMOVAL_IDS = 256; // Max number of ids able to be scheduled for removal *per sample frame*
const unsigned int REMOVAL_FIFO_SIZE_MASK = 255;

typedef unsigned int* node_id_fifo;

node_id_fifo removal_fifo = NULL;
unsigned int removal_fifo_read_index = 0;
unsigned int removal_fifo_write_index = 0;
int removal_fifo_size = 0;

#define REMOVAL_FIFO_PUSH(id) FIFO_PUSH(removal_fifo, removal_fifo_write_index, id, REMOVAL_FIFO_SIZE_MASK)
#define REMOVAL_FIFO_POP() FIFO_POP(removal_fifo, removal_fifo_read_index, REMOVAL_FIFO_SIZE_MASK)

// Allocate and null initialize a node list to be used as a node_list
node_id_fifo new_removal_fifo()
{
	unsigned int byte_size = sizeof(unsigned int) * MAX_REMOVAL_IDS;
	node_id_fifo fifo = (node_id_fifo) malloc(byte_size);
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

const unsigned int MAX_SYNTHS = 8192;
const unsigned int HASH_TABLE_SIZE_MASK = 8191;

typedef synth_node** hash_table;
hash_table synth_table = NULL;

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
	assert(num_synths < MAX_SYNTHS);
	unsigned int slot = node->hash & HASH_TABLE_SIZE_MASK;

	while (table[slot])
		slot = (slot + 1) & HASH_TABLE_SIZE_MASK;

	table[slot] = node;
	++num_synths;
}

synth_node* hash_table_remove(hash_table table, unsigned int key)
{
	unsigned int hash = HASH_KEY(key);
	unsigned int slot = hash & HASH_TABLE_SIZE_MASK;
	unsigned int i = 0;

	while (i < MAX_SYNTHS)
	{
		if (table[slot])
		{
			if (table[slot]->key == key)
			{
				synth_node* node = table[slot];
				table[slot] = NULL;
				--num_synths;
				return node;
			}
		}

		++i;
		slot = (slot + 1) & HASH_TABLE_SIZE_MASK;
	}

	return NULL;
}

synth_node* hash_table_lookup(hash_table table, unsigned int key)
{
	unsigned int hash = HASH_KEY(key);
	unsigned int slot = hash & HASH_TABLE_SIZE_MASK;
	unsigned int i = 0;

	while (i < MAX_SYNTHS)
	{
		if (table[slot])
		{
			if (table[slot]->key == key)
				return table[slot];
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

	if (previous)
		previous->next = next;

	if (next)
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
	while (list)
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
	hash_table_insert(synth_table, node);
	synth_list = doubly_linked_list_push(synth_list, node);
}

void remove_synth(synth_node* node)
{
	if (node)
	{
		synth_list = doubly_linked_list_remove(synth_list, node);

		message msg;
		msg.arg.node = node;
		msg.type = FREE_SYNTH;

		NRT_FIFO_PUSH(msg); // Send ugen to NRT thread for freeing
	}
}

void remove_synth_by_id(unsigned int id)
{
	synth_node* node = hash_table_remove(synth_table, id);
	remove_synth(node);
}

// Iterate over the scheduled list and add synths if they are ready. Stop as soon as we find a synth that isn't ready.
void add_scheduled_synths()
{

	while (scheduled_list_read_index != scheduled_list_write_index)
	{
		if (SCHEDULED_LIST_PEEK_TIME() <= current_sample_time)
		{
			synth_node* node = SCHEDULED_LIST_POP();
			add_synth(node);
		}

		else
		{
			return;
		}
	}
}

// Iterate over the removal fifo and remove all synths in it.
void remove_scheduled_synths()
{
	removal_fifo_read_index = removal_fifo_read_index & REMOVAL_FIFO_SIZE_MASK;
	removal_fifo_write_index = removal_fifo_write_index & REMOVAL_FIFO_SIZE_MASK;

	while (removal_fifo_read_index != removal_fifo_write_index)
	{
		unsigned int id = REMOVAL_FIFO_POP();
		--removal_fifo_size;
		synth_node* node = hash_table_remove(synth_table, id);
		remove_synth(node);
	}
}

void try_schedule_current_synth_for_removal()
{
	if (_necronomicon_current_node && (removal_fifo_size < REMOVAL_FIFO_SIZE_MASK))
	{
		++removal_fifo_size;
		REMOVAL_FIFO_PUSH(_necronomicon_current_node->key);
	}
}

void shutdown_rt_runtime(); // Forward declaration

void handle_rt_message(message msg)
{
	switch (msg.type)
	{
	case START_SYNTH:
		SCHEDULED_LIST_PUSH(msg.arg.node);
		break;
	case STOP_SYNTH:
		remove_synth_by_id(msg.arg.node_id);
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
	if (rt_fifo_read_index == rt_fifo_write_index)
	{
		return;
	}

	while (rt_fifo_read_index != rt_fifo_write_index)
	{
		message msg = RT_FIFO_POP();
		handle_rt_message(msg);
	}

	scheduled_list_sort();
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// NRT thread Synth Node Handling
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void handle_nrt_message(message msg)
{
	switch(msg.type)
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
	while (nrt_fifo_read_index != nrt_fifo_write_index)
	{
		message msg = NRT_FIFO_POP();
		handle_nrt_message(msg);
	}
}

void play_synth(synth_node* synth_definition, double* arguments, unsigned int num_arguments, unsigned int node_id, double time)
{
	time = time * SAMPLE_RATE;
	synth_node* synth = new_synth(synth_definition, arguments, num_arguments, node_id, time);

	message msg;
	msg.arg.node = synth;
	msg.type = START_SYNTH;
	RT_FIFO_PUSH(msg);
}

void stop_synth(unsigned int id)
{
	message msg;
	msg.arg.node_id = id;
	msg.type = STOP_SYNTH;
	RT_FIFO_PUSH(msg);
}

void send_set_synth_arg(unsigned int id, double argument, unsigned int arg_index)
{
	synth_node* synth = hash_table_lookup(synth_table, id);
	if (synth)
	{
		synth->ugen_wires[arg_index] = argument;
	}

	else
	{
		printf("Node ID not found: %u\n", id);
	}
}

void send_set_synth_args(unsigned int id, double* arguments, unsigned int num_arguments)
{
	synth_node* synth = hash_table_lookup(synth_table, id);
	if (synth)
	{
		memcpy(synth->ugen_wires, arguments, num_arguments * DOUBLE_SIZE);
	}

	else
	{
		printf("Node ID not found: %u\n", id);
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

	SAMPLE_RATE = jack_get_sample_rate(client);
	RECIP_SAMPLE_RATE = 1.0 / SAMPLE_RATE;
	TABLE_MUL_RECIP_SAMPLE_RATE = TABLE_SIZE * RECIP_SAMPLE_RATE;

	synth_table = hash_table_new();
	rt_fifo = new_message_fifo();
	scheduled_node_list = new_node_list();
	removal_fifo = new_removal_fifo();

	last_audio_bus_index = num_audio_buses - 1;
	num_audio_buses_bytes = num_audio_buses * DOUBLE_SIZE;
	_necronomicon_buses = malloc(num_audio_buses_bytes);
	clear_necronomicon_buses();

	initialize_wave_tables();
	_necronomicon_current_node = NULL;

	assert(nrt_fifo == NULL);
	nrt_fifo = new_message_fifo();

	current_sample_time = 0;
	necronomicon_running = true;
}

void clear_synth_list()
{
	while (synth_list)
	{
		remove_synth_by_id(synth_list->key);
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

	hash_table_free(synth_table);
	rt_fifo_free();
	scheduled_list_free();
	doubly_linked_list_free(synth_list);
	removal_fifo_free();
	free(_necronomicon_buses);

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

int process(jack_nframes_t nframes, void* arg)
{
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

		out0[i] = _necronomicon_buses[0];
		out1[i] = _necronomicon_buses[1];

		remove_scheduled_synths(); // Remove any synths that are scheduled for removal and send them to the NRT thread FIFO queue for freeing.
        current_sample_time += 1;
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

void line_constructor(ugen* u)
{
	u->data = malloc(UINT_SIZE); // Line time
	*((unsigned int*) u->data) = 0;
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
	test_synth->local_buses = malloc(DOUBLE_SIZE);
	test_synth->previous = NULL;
	test_synth->next = NULL;
	test_synth->key = 0;
	test_synth->hash = 0;
	test_synth->num_ugens = 1;
	test_synth->num_wires = 1;
	test_synth->num_buses = 1;
	test_synth->time = time;

	test_synth->ugen_graph[0] = test_ugen;
	test_synth->ugen_wires[0] = 0;
	return test_synth;
}

void print_node(synth_node* node)
{
	if (node)
		printf("synth_node { graph: %p, wires: %p, prev: %p, next: %p, key %i, hash: %i, num_ugens: %u, num_wires: %u, time: %u }",
			   node->ugen_graph,
			   node->ugen_wires,
			   node->previous,
			   node->next,
			   node->key,
			   node->hash,
			   node->num_ugens,
			   node->num_wires,
			   node->time);
	else
		printf("0");
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
		print_node(table[i]);
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
		synth_node* node = hash_table_remove(table, i);
		assert(node);
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
			printf("(next: %u) - (node: %u) = %u\n", next->time, node->time, next->time - node->time);
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
			printf("(next: %u) - (node: %u) = %u\n", next->time, node->time, next->time - node->time);
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
	if (mb->phase >= 1 || (mb->prevSyncAmp <= 0 && sync > 0))
	{
		mb->phase  = mb->phase - 1.0;
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
		mb->output = 0.0;
		add_blep(mb, mb->phase/freq,1.0);
	}

	// add BLEP in middle of wavefor for squarewave
	if(!mb->output && mb->phase > pwm)
	{
		mb->output = 1.0;
		add_blep(mb, (mb->phase - pwm) / freq,-1.0);
	}

	// add BLEP at hard sync
	//Have to detect this better and determine the incoming frequence!
	if(mb->prevSyncAmp <= 0 && sync > 0)
	{
		mb->phase  = mb->phase - 1.0;
		mb->output = 0.0;
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

#define CUBIC_INTERP(A,B,C,D,DELTA) \
({                                  \
	double delta2 = DELTA * DELTA;  \
	double a0     = D - C - A + B;  \
	double a1     = A - B - a0;     \
	double a2     = C - A;          \
	a0 * DELTA * delta2 + a1 * delta2 + a2 * DELTA + B; \
})
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
	// double  min   = UGEN_IN(u,0);
	// double  range = UGEN_IN(u,1) - min;
	rand_t* rand  = (rand_t*) u->data;
	double  amp   = rand->value0;// * range + min;

	UGEN_OUT(u,0,amp);
}

void lfnoiseN_calc(ugen* u)
{
	double  freq  = UGEN_IN(u,0);
	// double  min   = UGEN_IN(u,1);
	// double  range = UGEN_IN(u,2) - min;
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
	// double  min   = UGEN_IN(u,1);
	// double  range = UGEN_IN(u,2) - min;
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
	// double  min   = UGEN_IN(u,1);
	// double  range = UGEN_IN(u,2) - min;
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
	double in    = UGEN_IN(u,2);
	double amp   = (in * range) + min;

	UGEN_OUT(u, 0, amp);
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
	double gain  = UGEN_IN(u,1);
	double q     = UGEN_IN(u,2);
	double in    = UGEN_IN(u,3);

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
	double gain  = UGEN_IN(u,1);
	double q     = UGEN_IN(u,2);
	double in    = UGEN_IN(u,3);

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
	double gain  = UGEN_IN(u,1);
	double q     = UGEN_IN(u,2);
	double in    = UGEN_IN(u,3);

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
