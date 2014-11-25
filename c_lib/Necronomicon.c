/*
  Necronomicon - Deterministic Audio Engine
  Copyright 2014 - Chad McKinney and Curtis McKinney
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

#include "Necronomicon.h"

//////////////
// Constants
//////////////

#ifndef M_PI
#define M_PI 3.1415926535897932384626433832795028841971693993751058209749445923078164062L
#endif

const double TWO_PI = M_PI * 2;
const double RECIP_TWO_PI =  1.0 / (M_PI * 2);
const double SAMPLE_RATE = 44100;
const double RECIP_SAMPLE_RATE = 1.0 / 44100.0;

#define TABLE_SIZE   (256)
const double RECIP_TABLE_SIZE = 1.0 / (double) TABLE_SIZE;
double sine_table[TABLE_SIZE];

const double TABLE_MUL_RECIP_SAMPLE_RATE = TABLE_SIZE * (1.0 / 44100.0);
const double TABLE_SIZE_MUL_RECIP_TWO_PI = TABLE_SIZE * (1.0 / (M_PI*2));

typedef struct
{
	double amplitude;
	double offset;
} Signal;

int signalSize = sizeof(Signal);
int signalAlignment = __alignof__(Signal);

typedef Signal Calc(void* args, double time);

typedef struct
{
	Calc* calc;
	void* args;
	unsigned int numArgs;
} UGen;

int ugenSize = sizeof(UGen);
int ugenAlignment = __alignof__(UGen);

Signal numberCalc(void* args, double time)
{
	return ((Signal*) args)[0];
}

void pr_free_ugen(UGen* ugen)
{
	if (ugen == NULL)
		return;

	if (ugen->args == NULL)
		return;
	
	if (ugen->calc != numberCalc)
	{
		unsigned int i;
		for (i = 0; i < ugen->numArgs; ++i)
		{
			pr_free_ugen(&(((UGen*) ugen->args)[i]));
		}
	}

	free(ugen->args);
	ugen->args = NULL;
}

void free_ugen(UGen* ugen)
{
	puts("FREE UGEN!");
	if (ugen == NULL)
		return;
	
	pr_free_ugen(ugen);
	free(ugen);	
}

void initialize_wave_tables()
{
	unsigned int i;
	for (i = 0; i < TABLE_SIZE; ++i)
	{
		sine_table[i] = sin(TWO_PI * (((double) i) / ((double) TABLE_SIZE)));
	}
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// UGens
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

inline double sigsum(Signal signal)
{
	return signal.amplitude + signal.offset;
}

Signal delayCalc(void* args, double time)
{
	UGen delayUGen = ((UGen*) args)[0];
	UGen input = ((UGen*) args)[1];
	Signal delayTimeSig = (delayUGen.calc(delayUGen.args, time));
	int delayedTime = delayTimeSig.offset * (double) SAMPLE_RATE + time + (delayTimeSig.amplitude * RECIP_TWO_PI * (double) SAMPLE_RATE);
	return input.calc(input.args, delayedTime);
}

Signal timeWarpCalc(void* args, double time)
{
	UGen timeUGen = ((UGen*) args)[0];
	UGen input = ((UGen*) args)[1];
	Signal modTimeSig = (timeUGen.calc(timeUGen.args, time));
	int modedTime = modTimeSig.offset * time + (modTimeSig.amplitude * (double) SAMPLE_RATE);
	return input.calc(input.args, modedTime);
}

Signal sinCalc(void* args, double time)
{
	UGen freqUGen = ((UGen*) args)[0];
	Signal freq = freqUGen.calc(freqUGen.args, time);

    //sin function version
	/* double amplitude = sin(freq.offset * TWO_PI * time * RECIP_SAMPLE_RATE + freq.amplitude); */

	//look up table version
	double rawIndex = freq.offset * TABLE_MUL_RECIP_SAMPLE_RATE * time + (freq.amplitude * TABLE_SIZE_MUL_RECIP_TWO_PI);
	unsigned char index1 = rawIndex;
	unsigned char index2 = index1+1;
	double amp1 = sine_table[index1];
	double amp2 = sine_table[index2];
	double delta = rawIndex - ((long) rawIndex);
	double amplitude = amp1 + delta * (amp2 - amp1);
	
	Signal signal = { amplitude, 0 };
	return signal;
}

Signal addCalc(void* args, double time)
{
	UGen a = ((UGen*) args)[0];
	UGen b = ((UGen*) args)[1];
	Signal as = a.calc(a.args, time);
	Signal bs = b.calc(b.args, time);
	Signal signal = { as.amplitude + bs.amplitude, as.offset + bs.offset };
	return signal;
}

Signal minusCalc(void* args, double time)
{
	UGen a = ((UGen*) args)[0];
	UGen b = ((UGen*) args)[1];
	Signal as = a.calc(a.args, time);
	Signal bs = b.calc(b.args, time);
	Signal signal = { as.amplitude - bs.amplitude, as.offset - bs.offset };
	return signal;
}

Signal mulCalc(void* args, double time)
{
	UGen a = ((UGen*) args)[0];
	UGen b = ((UGen*) args)[1];
	Signal as = a.calc(a.args, time);
	Signal bs = b.calc(b.args, time);
	Signal signal;

	if (as.amplitude != 0)
	{
		signal.amplitude = (bs.amplitude + bs.offset) * as.amplitude;
		signal.offset    = (bs.amplitude + bs.offset) * as.offset;
	}

	else
	{
		signal.amplitude = (as.amplitude + as.offset) * bs.amplitude;
		signal.offset    = (as.amplitude + as.offset) * bs.offset;
	}

	return signal;
}

Signal udivCalc(void* args, double time)
{
	UGen a = ((UGen*) args)[0];
	UGen b = ((UGen*) args)[1];
	Signal as = a.calc(a.args, time);
	Signal bs = b.calc(b.args, time);
	Signal signal;

	if (as.amplitude != 0)
	{
		signal.amplitude = (bs.amplitude + bs.offset) / as.amplitude;
		signal.offset    = (bs.amplitude + bs.offset) / as.offset;
	}

	else
	{
		signal.amplitude = (as.amplitude + as.offset) / bs.amplitude;
		signal.offset    = (as.amplitude + as.offset) / bs.offset;
	}

	return signal;
}

Signal uabsCalc(void* args, double time)
{
	UGen input = *((UGen*) args);
	Signal signal = input.calc(input.args, time);
	signal.amplitude = abs(signal.amplitude);
	signal.offset = abs(signal.offset);
	return signal;
}

Signal signumCalc(void* args, double time)
{
	UGen input = *((UGen*) args);
	double signal = sigsum(input.calc(input.args, time));
	Signal result = { 0, 0 };

	if (signal > 0)
	{
		result.offset = 1;
	}
	
	else if (signal < 0)
	{
		result.offset = -1;
	}
	
	return result;
}

Signal negateCalc(void* args, double time)
{
	UGen a = *((UGen*) args);
	Signal as = a.calc(a.args, time);
	Signal signal;

	if (as.amplitude != 0)
	{
		signal.amplitude = -1 * as.amplitude;
		signal.offset    = -1 * as.offset;
	}

	else
	{
		signal.amplitude = 0;
		signal.offset    = (as.amplitude+as.offset) * -1;
	}

	return signal;
}

void printTabs(unsigned int depth)
{
	unsigned int i;
	for (i = 0; i < depth; ++i)
	{
		printf(" ");
	}
}

void printUGen(UGen* ugen, unsigned int depth)
{
	printTabs(depth);
	puts("(UGen");
	printTabs(depth + 1);
	printf("(Calc %p)\n", ugen->calc);
	printTabs(depth + 1);
	printf("(Args\n");

	if (ugen->calc != numberCalc)
	{
		unsigned int i;
		for (i = 0; i < ugen->numArgs; ++i)
		{
			printUGen(&((UGen*) ugen->args)[i], depth + 2);
		}
	}

	else
	{
		printTabs(depth + 2);
		Signal signal = ((Signal*) ugen->args)[0];
		printf("(Signal %f %f))\n", signal.amplitude, signal.offset);
	}	

	printTabs(depth + 1);
	printf("(NumArgs %i))\n", ugen->numArgs);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Scheduler
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef enum { false, true } bool;
const unsigned int max_fifo_messages = 2048;
const unsigned int fifo_size_mask = 2047;

typedef struct synth_node
{
	double time;
	UGen* ugen;
	struct synth_node* previous;
	struct synth_node* next;
	unsigned int key;
	unsigned int hash;
} synth_node;

const unsigned int node_size = sizeof(synth_node);
const unsigned int node_pointer_size = sizeof(synth_node*);

synth_node* new_synth_node(double time, UGen* ugen)
{
	synth_node* node = (synth_node*) malloc(node_size);
	assert(node);
	
	node->time = time;
	node->ugen = ugen;
	node->previous = NULL;
	node->next = NULL;
	node->key = 0;
	node->hash = 0;
	return node;
}

void node_free(synth_node* node)
{
	if (node)
		free(node);
}

/////////////////
// Message FIFO
/////////////////

typedef union
{
	synth_node* node;
	unsigned int node_id;
	const char* string;
} message_arg;

typedef enum
{
	IGNORE,
	START_SYNTH,
	STOP_SYNTH,
	SET_SYNTH,
	FREE_SYNTH, // Free synth memory
	SHUTDOWN,
	PRINT
} message_type;

typedef struct 
{
	message_arg arg;
	message_type type;
} message;

unsigned int message_size = sizeof(message);

// Lock Free FIFO Queue (Ring Buffer)
typedef message* message_fifo;

message_fifo nrt_fifo = NULL;
unsigned int nrt_fifo_read_index = 0;
unsigned int nrt_fifo_write_index = 0;

message_fifo rt_fifo = NULL;
unsigned int rt_fifo_read_index = 0;
unsigned int rt_fifo_write_index = 0;

// Increment fifo_write_index and fifo_read_index after assignment to maintain intended ordering (using a memory barrier): Assignment/Lookup -> Increment
#define FIFO_PUSH(fifo, write_index, node) fifo[write_index & fifo_size_mask] = node; __sync_synchronize(); write_index++;
#define FIFO_POP(fifo, read_index) fifo[read_index & fifo_size_mask]; __sync_synchronize(); read_index++;

// Non-realtime thread FIFO push/pop
#define NRT_FIFO_PUSH(node) FIFO_PUSH(nrt_fifo, nrt_fifo_write_index, node)
#define NRT_FIFO_POP() FIFO_POP(nrt_fifo, nrt_fifo_read_index)

// Realtime thread FIFO push/pop
#define RT_FIFO_PUSH(node) FIFO_PUSH(rt_fifo, rt_fifo_write_index, node)
#define RT_FIFO_POP() FIFO_POP(rt_fifo, rt_fifo_read_index)

// Allocate and null initialize a node list to be used as a node_fifo or node_list
message_fifo new_message_fifo()
{
	unsigned int byte_size = message_size * max_fifo_messages;
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
		node_free(msg.arg.node);
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

#define SCHEDULED_LIST_PUSH(node) FIFO_PUSH(scheduled_node_list, scheduled_list_write_index, node)
#define SCHEDULED_LIST_POP() FIFO_POP(scheduled_node_list, scheduled_list_read_index)
#define SCHEDULED_LIST_PEEK() (scheduled_node_list[scheduled_list_read_index & fifo_size_mask])
#define SCHEDULED_LIST_PEEK_TIME() ((scheduled_node_list[scheduled_list_read_index & fifo_size_mask])->time)

// Allocate and null initialize a node list to be used as a node_list
node_list new_node_list()
{
	unsigned int byte_size = node_pointer_size * max_fifo_messages;
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
		node_free(node);		
	}
	
	free(scheduled_node_list);
	scheduled_node_list = NULL;
	scheduled_list_read_index = 0;
	scheduled_list_write_index = 0;
}

// Simple insertion sort. Accounts for ring buffer array wrapping using bit masking and integer overflow
void scheduled_list_sort()
{
	// Make sure our indexes are within bounds
	scheduled_list_read_index = scheduled_list_read_index & fifo_size_mask; 
	scheduled_list_write_index = scheduled_list_write_index & fifo_size_mask;

	if (scheduled_list_read_index == scheduled_list_write_index)
		return;
		
	unsigned int i, j, k;
	synth_node* x;
	double xTime, yTime;
	
	for (i = (scheduled_list_read_index + 1) & fifo_size_mask; i != scheduled_list_write_index; i = (i + 1) & fifo_size_mask)
	{
		x = scheduled_node_list[i];
		xTime = x->time;
		j = i;
		
		while (j != scheduled_list_read_index)
		{
			k = (j - 1) & fifo_size_mask;
			yTime = scheduled_node_list[k]->time;
			if (yTime < xTime)
				break;

			scheduled_node_list[j] = scheduled_node_list[k];
			j = (j - 1) & fifo_size_mask;
		}

		scheduled_node_list[j] = x;
	}
}

///////////////////////////
// Hash Table
///////////////////////////

// Fixed memory hash table using open Addressing with linear probing
// This is not thread safe.

const unsigned int max_synths = 8192;
const unsigned int hash_table_size_mask = 8191;
unsigned int num_synths = 0;

typedef union
{
	unsigned char bytes[4];
	unsigned int word;
} four_bytes;

typedef synth_node** hash_table;
hash_table synth_table = NULL;

hash_table hash_table_new()
{
	unsigned int byte_size = node_pointer_size * max_synths;
	hash_table table = (hash_table) malloc(byte_size);
	assert(table);
	memset(table, 0, byte_size);
	
	return table;
}

void hash_table_free(hash_table table)
{
	unsigned int i;
	for (i = 0; i < max_synths; ++i)
	{
		synth_node* node = table[i];
		node_free(node);
	}

	free(table);
}

// FNV1-a hash function
const unsigned int prime = 0x01000193; // 16777619
const unsigned int seed = 0x811C9DC5; // 2166136261

#define FNV1A(byte, hash) ((byte ^ hash) * prime)
#define HASH_KEY_PRIV(key) (FNV1A(key.bytes[3], FNV1A(key.bytes[2], FNV1A(key.bytes[1], FNV1A(key.bytes[0], seed)))))
#define HASH_KEY(key) ((unsigned int) HASH_KEY_PRIV(((four_bytes) key)))

void hash_table_insert(hash_table table, synth_node* node)
{
	assert(num_synths < max_synths);
	node->hash = HASH_KEY(node->key);
	unsigned int slot = node->hash & hash_table_size_mask;

	while (table[slot])
		slot = (slot + 1) & hash_table_size_mask;
	
	table[slot] = node;
	++num_synths;
}

synth_node* hash_table_remove(hash_table table, unsigned int key)
{
	assert(num_synths > 0);
	unsigned int hash = HASH_KEY(key);
	unsigned int slot = hash & hash_table_size_mask;
	unsigned int i = 0;
	
	while (i < max_synths)
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
		slot = (slot + 1) & hash_table_size_mask;
	}

	return NULL;
}

synth_node* hash_table_lookup(hash_table table, unsigned int key)
{
	unsigned int hash = HASH_KEY(key);
	unsigned int slot = hash & hash_table_size_mask;
	unsigned int i = 0;
	
	while (i < max_synths)
	{
		if (table[slot])
		{
			if (table[slot]->key == key)
				return table[slot];
		}

		++i;
		slot = (slot + 1) & hash_table_size_mask;
	}

	return NULL;
}

///////////////////////////
// Doubly Linked List
///////////////////////////

typedef synth_node* doubly_linked_list;
doubly_linked_list synth_list = NULL;
doubly_linked_list synth_removal_list = NULL;

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

	if (node == list) // If the node was the head of the list, return the next node as the head of the list
	{
		return next;
	}
	
	if (next)
		next->previous = previous;

	return list; // Otherwise just return the current head of the list
}

void doubly_linked_list_free(doubly_linked_list list)
{
	while (list)
	{
		synth_node* next = list->next;
		node_free(list);
		list = next;
	}
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// RT thread Synth Node Handling
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

unsigned int absolute_time = 0;
bool necronomicon_running = false;

void init_rt_thread()
{
	puts("Initializing real-time thread...");
	assert(necronomicon_running == false);
	assert(synth_table == NULL);
	assert(rt_fifo == NULL);
	assert(scheduled_node_list == NULL);
	assert(synth_list == NULL);
	assert(synth_removal_list == NULL);
	
	synth_table = hash_table_new();
	rt_fifo = new_message_fifo();
	scheduled_node_list = new_node_list();
	absolute_time = 0;

	initialize_wave_tables();
	necronomicon_running = true;
}

void shutdown_rt_thread()
{
	puts("Shutting down real-time thread...");
	assert(synth_table != NULL);
	assert(rt_fifo != NULL);
	assert(scheduled_node_list != NULL);

	hash_table_free(synth_table);
	rt_fifo_free();
	scheduled_list_free();
	doubly_linked_list_free(synth_list);
	doubly_linked_list_free(synth_removal_list);
	
	synth_table = NULL;
	rt_fifo = NULL;
	scheduled_node_list = NULL;
	synth_list = NULL;
	synth_removal_list = NULL;
	necronomicon_running = false;
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
		message msg = { node, FREE_SYNTH };
		NRT_FIFO_PUSH(msg); // Send ugen to NRT thread for free/reuse
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
		if (SCHEDULED_LIST_PEEK_TIME() <= absolute_time)
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

// Iterate over the removal list and remove all synths in it.
void remove_scheduled_synths()
{
	synth_node* node = synth_removal_list;
	while (node)
	{
		hash_table_remove(synth_table, node->key);
		synth_node* next = node->next;
		remove_synth(node);
		node = next;
	}

	synth_removal_list = NULL;
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
	case SET_SYNTH:
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

doubly_linked_list nrt_free_node_list = NULL;
const unsigned int max_node_pool_count = 500;
const unsigned int initial_node_count = 250;
unsigned int node_pool_count = 0;
synth_node default_node = { 0, NULL, NULL, NULL, 0, 0 };
unsigned int next_node_id = 1000;

void init_nrt_thread()
{
	puts("Initializing non-real-time thread...");
	assert(nrt_fifo == NULL);
	nrt_fifo = new_message_fifo();
	node_pool_count = 0;
	next_node_id = 1000;
	
	// Pre-allocate some ugen nodes for runtime
	while (node_pool_count < initial_node_count)
	{
		nrt_free_node_list = doubly_linked_list_push(nrt_free_node_list, new_synth_node(0, NULL));
		++node_pool_count;
	}
}

void shutdown_nrt_thread()
{
	puts("shutting down non-real-time thread...");
	assert(nrt_fifo != NULL);

	nrt_fifo_free();
	doubly_linked_list_free(nrt_free_node_list);

	nrt_fifo = NULL;
	nrt_free_node_list = NULL;
	node_pool_count = 0;
}

void nrt_free_node(synth_node* node)
{
	if (node)
	{
		if (node_pool_count < max_node_pool_count)
		{
			memcpy(node, &default_node, node_size);
			++node_pool_count;
			nrt_free_node_list = doubly_linked_list_push(nrt_free_node_list, node);
		}

		else
		{
			free(node);
		}
	}
}

void handle_nrt_message(message msg)
{
	switch(msg.type)
	{
	case FREE_SYNTH:
		nrt_free_node(msg.arg.node);
		break;
	case PRINT:
		puts(msg.arg.string);
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

synth_node* nrt_alloc_node(UGen* ugen, double time, unsigned int node_id)
{
	synth_node* node = NULL;
	if (node_pool_count)
	{
		node = nrt_free_node_list;

		if (node)
		{
			nrt_free_node_list = node->next;
			node->time = time;
			node->ugen = ugen;
			node->previous = NULL;
			node->next = NULL;
			node->key = node_id;
			
			--node_pool_count;
			return  node;
		}
	}

	node = new_synth_node(time, ugen);
	node->key = node_id;
	return node;
}

void play_synth(UGen* synth, double time, unsigned int node_id)
{
	message msg = { nrt_alloc_node(synth, time, node_id), START_SYNTH }; 
	RT_FIFO_PUSH(msg);
}

void stop_synth(unsigned int id)
{
	message msg = { NULL, STOP_SYNTH };
	msg.arg.node_id = id;
	RT_FIFO_PUSH(msg);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// RT Runtime
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

jack_port_t* output_port1;
jack_port_t* output_port2;
jack_client_t* client;

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
	jack_default_audio_sample_t* out1 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port1, nframes);
	jack_default_audio_sample_t* out2 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port2, nframes);
	handle_messages_in_rt_fifo(); // Handles messages including moving uge_nodes from the RT FIFO queue into the scheduled_synth_list
	
	unsigned int i;
	for (i = 0; i < nframes; ++i)
    {
		out1[i] = 0; // Clear out from last process callback values
		out2[i] = 0;
		add_scheduled_synths(); // Add any synths that need to start this frame into the current synth_list

		// Iterate through the synth_list calling calc on each synth and mixing in the result to the out buffers.
		synth_node* node = synth_list;
		while (node)
		{
			Signal signal = node->ugen->calc(node->ugen->args, absolute_time);
			double sample = signal.amplitude + signal.offset;
			out1[i] += sample;
			out2[i] += sample;

			node = node->next;
		}

		remove_scheduled_synths(); // Remove any synths that are scheduled for removal and send them to the NRT thread FIFO queue for freeing.
        ++absolute_time; // Increment absolute time. We need to consider adjusting this for xruns
    }

	return 0;
}

void start_rt_runtime()
{
	puts("Necronomicon audio engine booting...");
	init_rt_thread();
	
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

	// Wait for RT thread to finish.
	while(necronomicon_running == true)
	{	
		sleep(100);
	}
	
	shutdown_nrt_thread();
	puts("Necronomicon shut down.");
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//// Test FIFO

void print_node(synth_node* node)
{	
	if (node)
		printf("synth_node { time: %f, ugen: %p, previous: %p, next: %p, hash: %i, key %i }", node->time, node->ugen, node->previous, node->next, node->hash, node->key);
	else
		printf("0");
}

void print_list(node_list list)
{
	printf("scheduled_list_read_index: %i, scheduled_list_write_index: %i\n", scheduled_list_read_index, scheduled_list_write_index);
	unsigned int i = scheduled_list_read_index & fifo_size_mask;
	scheduled_list_write_index = scheduled_list_write_index & fifo_size_mask;

	for (; i != scheduled_list_write_index; i = (i + 1) & fifo_size_mask)
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
	    while ((num_pop > 0) && ((scheduled_list_read_index & fifo_size_mask) != ((scheduled_list_write_index - 1) & fifo_size_mask)))
		{
			SCHEDULED_LIST_POP();
			--num_pop;
		}
		
		unsigned int num_push = random() / (double) RAND_MAX * 100;
		while ((num_push > 0) && ((scheduled_list_read_index & fifo_size_mask) != (scheduled_list_write_index & fifo_size_mask)))
		{
			synth_node* node = new_synth_node((random() / (double) RAND_MAX) * 10000.0, NULL);
			SCHEDULED_LIST_PUSH(node);
			--num_push;
		}
	}

	scheduled_list_read_index = scheduled_list_read_index & fifo_size_mask;
	scheduled_list_write_index = scheduled_list_write_index & fifo_size_mask;
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
	while (scheduled_list_write_index < (max_fifo_messages * 0.75))
	{
		synth_node* node = new_synth_node((random() / (double) RAND_MAX) * 10000.0, NULL);
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
	for (i = 0; i < max_synths; ++i)
	{
		print_node(table[i]);
		if (i < (max_synths - 1))
			printf(", ");
	}

	printf("]\n\n");
}

unsigned int num_values = 5000;
double times[5000];

void test_hash_table()
{
	unsigned int i = 0;
	for (i = 0; i < 1000; ++i)
	{
		printf("key: %u, hash: %u, slot %u\n", i, HASH_KEY(i), HASH_KEY(i) & hash_table_size_mask);
	}

    hash_table table = hash_table_new();

	for (i = 0; i < num_values; ++i)
	{
		times[i] = (random() / (double) RAND_MAX) * 10000.0;
		synth_node* node = new_synth_node(times[i], NULL);
		node->key = i;
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
	}

	puts("Removing table values...\n\n");
	
	for (i = 0; i < num_values; ++i)
	{
		synth_node* node = hash_table_remove(table, i);
		assert(node);
		node_free(node);
	}

	puts("Asserting NULL values...\n\n");

	for (i = 0; i < max_synths; ++i)
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
			puts("Filtered out node:");
			print_node(node);
			list = doubly_linked_list_remove(list, node);
			node_free(node);
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
		synth_node* node = new_synth_node(i, NULL);
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
			printf("(next: %f) - (node: %f) = %f\n", next->time, node->time, next->time - node->time);
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
			printf("(next: %f) - (node: %f) = %f\n", next->time, node->time, next->time - node->time);
			assert((next->time - node->time) == 2);
		}

		node = next;
	}

	doubly_linked_list_free(synth_list);
}
