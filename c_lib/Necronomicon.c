/*
  Necronomicon - Deterministic Audio Engine
  Copyright 2014 - Chad McKinney and Curtis McKinney
*/

#include <stdio.h>
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
#define M_PI 3.1415926535897932384626433832795028841971693993751058209749445923078164062
#endif

const double TWO_PI = M_PI * 2;
const double RECIP_TWO_PI =  1.0 / (M_PI * 2);
const double SAMPLE_RATE = 44100;
const double RECIP_SAMPLE_RATE = 1.0 / 44100.0;

#define TABLE_SIZE   (256)
const double RECIP_TABLE_SIZE = 1.0 / (double)TABLE_SIZE;
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

////////////////////////////////////////////////////////////////////////////////////
// UGens
////////////////////////////////////////////////////////////////////////////////////

inline double sigsum(Signal signal)
{
	return signal.amplitude + signal.offset;
}

Signal delayCalc(void* args, double time)
{
	UGen delayUGen = ((UGen*) args)[0];
	UGen input = ((UGen*) args)[1];
	Signal delayTimeSig = (delayUGen.calc(delayUGen.args, time));
	double delayedTime = delayTimeSig.offset * SAMPLE_RATE + time + (delayTimeSig.amplitude * RECIP_TWO_PI * SAMPLE_RATE);
	return input.calc(input.args, delayedTime);
}

Signal timeWarpCalc(void* args, double time)
{
	UGen timeUGen = ((UGen*) args)[0];
	UGen input = ((UGen*) args)[1];
	Signal modTimeSig = (timeUGen.calc(timeUGen.args, time));
	double modedTime = modTimeSig.offset * time + (modTimeSig.amplitude * SAMPLE_RATE);
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
	double delta = rawIndex - ((long)rawIndex);
	double amplitude = amp1 + delta * (amp2 - amp1);
	
	Signal signal = { amplitude, 0 };
	return signal;
}

Signal numberCalc(void* args, double time)
{
	return ((Signal*) args)[0];
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
	if(as.amplitude != 0)
	{
		signal.amplitude = (bs.amplitude+bs.offset)*as.amplitude;
		signal.offset    = (bs.amplitude+bs.offset)*as.offset;
	}
	else
	{
		signal.amplitude = (as.amplitude+as.offset)*bs.amplitude;
		signal.offset    = (as.amplitude+as.offset)*bs.offset;
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
	if(as.amplitude != 0)
	{
		signal.amplitude = (bs.amplitude+bs.offset)/as.amplitude;
		signal.offset    = (bs.amplitude+bs.offset)/as.offset;
	}
	else
	{
		signal.amplitude = (as.amplitude+as.offset)/bs.amplitude;
		signal.offset    = (as.amplitude+as.offset)/bs.offset;
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

	if(signal > 0)
	{
		result.offset = 1;
	}
	
	else if(signal < 0)
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

	if(as.amplitude != 0)
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

//////////////
// Runtime
//////////////

unsigned int absolute_time = 0;
jack_port_t* output_port1;
jack_port_t* output_port2;
jack_client_t* client;

static void signal_handler(int sig)
{
	jack_client_close(client);
	fprintf(stderr, "signal received, exiting ...\n");
	exit(0);
}

/*
 * JACK calls this shutdown_callback if the server ever shuts down or
 * decides to disconnect the client.
 */
void jack_shutdown (void *arg)
{
	exit (1);
}

int process(jack_nframes_t nframes, void *arg)
{
	jack_default_audio_sample_t *out1, *out2;
	UGen* ugen = (UGen*) arg;
	
	int i;

	out1 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port1, nframes);
	out2 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port2, nframes);

	for(i = 0; i < nframes; i++)
    {
		Signal signal = ugen->calc(ugen->args, absolute_time);
		double sample = signal.amplitude + signal.offset;
        out1[i] = sample;
        out2[i] = sample;
        absolute_time++;
    }
    
	return 0;      
}

void printTabs(unsigned int depth)
{
	unsigned int i;
	for(i = 0; i < depth; ++i)
	{
		printf("\t");
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

	if(ugen->calc != numberCalc)
	{
		unsigned int i;
		for(i = 0; i < ugen->numArgs; ++i)
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

void startRuntime(UGen* ugen)
{
	/* printf("UGen size: %i\n", ugenSize); */
	/* printf("UGen alignment: %i\n", ugenAlignment); */
	/* printf("Signal size: %i\n", signalSize); */
	/* printf("Signal alignment: %i\n", signalAlignment); */
	
	/* printf("Delay calc: %p\n", delayCalc); */
	/* printf("Number calc: %p\n", numberCalc); */
	
	puts("Starting Necronomicon");
	/* printUGen(ugen, 0); */
	/* puts("\n"); */
	
	int si;
	for(si = 0; si < TABLE_SIZE; si++)
	{
		sine_table[si] = sin(TWO_PI * (((float)si) / ((float)TABLE_SIZE)));
	}
	
	const char** ports;
	const char* client_name = "Necronomicon";
	const char* server_name = NULL;
	jack_options_t options = JackNullOption;
	jack_status_t status;

	int i;

	/* open a client connection to the JACK server */

	client = jack_client_open (client_name, options, &status, server_name);
	if (client == NULL) {
		fprintf (stderr, "jack_client_open() failed, "
			 "status = 0x%2.0x\n", status);
		if (status & JackServerFailed) {
			fprintf (stderr, "Unable to connect to JACK server\n");
		}
		exit (1);
	}
	if (status & JackServerStarted) {
		fprintf (stderr, "JACK server started\n");
	}
	if (status & JackNameNotUnique) {
		client_name = jack_get_client_name(client);
		fprintf (stderr, "unique name `%s' assigned\n", client_name);
	}

	/* tell the JACK server to call `process()' whenever
	   there is work to be done.
	*/

	jack_set_process_callback (client, process, ugen);

	/* tell the JACK server to call `jack_shutdown()' if
	   it ever shuts down, either entirely, or if it
	   just decides to stop calling us.
	*/

	jack_on_shutdown (client, jack_shutdown, 0);

	/* create two ports */

	output_port1 = jack_port_register (client, "output1",
					  JACK_DEFAULT_AUDIO_TYPE,
					  JackPortIsOutput, 0);

	output_port2 = jack_port_register (client, "output2",
					  JACK_DEFAULT_AUDIO_TYPE,
					  JackPortIsOutput, 0);

	if ((output_port1 == NULL) || (output_port2 == NULL)) {
		fprintf(stderr, "no more JACK ports available\n");
		exit (1);
	}

	/* Tell the JACK server that we are ready to roll.  Our
	 * process() callback will start running now. */

	if (jack_activate (client)) {
		fprintf (stderr, "cannot activate client");
		exit (1);
	}

	/* Connect the ports.  You can't do this before the client is
	 * activated, because we can't make connections to clients
	 * that aren't running.  Note the confusing (but necessary)
	 * orientation of the driver backend ports: playback ports are
	 * "input" to the backend, and capture ports are "output" from
	 * it.
	 */
 	
	ports = jack_get_ports (client, NULL, NULL, JackPortIsPhysical | JackPortIsInput);
	if (ports == NULL) {
		fprintf(stderr, "no physical playback ports\n");
		exit (1);
	}

	if (jack_connect (client, jack_port_name (output_port1), ports[0])) {
		fprintf (stderr, "cannot connect output ports\n");
	}

	if (jack_connect (client, jack_port_name (output_port2), ports[1])) {
		fprintf (stderr, "cannot connect output ports\n");
	}

	jack_free (ports);
    
    /* install a signal handler to properly quits jack client */
#ifdef WIN32
	signal(SIGINT, signal_handler);
    signal(SIGABRT, signal_handler);
	signal(SIGTERM, signal_handler);
#else
	signal(SIGQUIT, signal_handler);
	signal(SIGTERM, signal_handler);
	signal(SIGHUP, signal_handler);
	signal(SIGINT, signal_handler);
#endif

	/* keep running until the Ctrl+C */

	while (1) {
	#ifdef WIN32 
		Sleep(1000);
	#else
		/* sleep (1); */
		sleep (10);
	#endif
	}

	jack_client_close (client);

	puts("Necronomicon shutting down...");
}


////////////////////////////////////////////////////////////////////////////////////
// Scheduler
////////////////////////////////////////////////////////////////////////////////////

typedef enum { false, true } bool;
unsigned int max_nodes = 2048;
unsigned int size_mask = 2047;

typedef struct
{
	double time;
	UGen* ugen;
} ugen_node;

void node_free(ugen_node node)
{
	if(node.ugen)
		free(node.ugen);
}

//////////////
// Node FIFO
//////////////

// Lock Free FIFO Queue (Ring Buffer)
typedef ugen_node* node_fifo;
unsigned int node_size = sizeof(ugen_node);
unsigned int fifo_read_index = 0;
unsigned int fifo_write_index = 0;

#define FIFO_PUSH(fifo, node) fifo[fifo_write_index & size_mask] = node; fifo_write_index++;
#define FIFO_POP(fifo) fifo[fifo_read_index++ & size_mask]

// Allocate and null initialize a node list to be used as a node_fifo or node_list
ugen_node* new_node_list()
{
	ugen_node* list = (ugen_node*) malloc(node_size * max_nodes);
	memset(list, 0, node_size * max_nodes);
	return list;
}

// Free all remaining nodes in the fifo and then free the fifo itself
void fifo_free(node_fifo fifo)
{
	bool freeing = true;
	if(fifo_read_index == fifo_write_index)
	{
		freeing = false;
	}

	while(freeing)
	{
		if(fifo_read_index != fifo_write_index)
		{
			ugen_node node = FIFO_POP(fifo);
			node_free(node);
		}

		else
		{
			freeing = false;
		}		
	}
	
	free(fifo);
	fifo_read_index = 0;
	fifo_write_index = 0;
}

//////////////
// Node List
//////////////

// An ordered list of ugen nodes
typedef ugen_node* node_list;
unsigned int list_read_index = 0;
unsigned int list_write_index = 0;

// Increment list_write_index after assignment to maintain intended ordering: Assignment -> Increment
#define LIST_PUSH(list, node) list[list_write_index & size_mask] = node; list_write_index++;
#define LIST_POP(list) list[list_read_index++ & size_mask]
#define LIST_PEEK(list) list[list_read_index & size_mask]
#define LIST_PEEK_TIME(list) list[list_read_index & size_mask].time

// Free all remaining nodes in the list and then free the list itself
void list_free(node_list list)
{
	bool freeing = true;
	if(list_read_index == list_write_index)
	{
		freeing = false;
	}

	while(freeing)
	{
		if(list_read_index != list_write_index)
		{
			ugen_node node = LIST_POP(list);
			node_free(node);
		}

		else
		{
			freeing = false;
		}		
	}
	
	free(list);
	list_read_index = 0;
	list_write_index = 0;
}

// Simple insertion sort. Accounts for ring buffer array wrapping using bit masking and integer overflow
void list_sort(node_list list)
{
	// Make sure our indexes are within bounds
	list_read_index = list_read_index & size_mask; 
	list_write_index = list_write_index & size_mask;
	unsigned int i, j, k;
	ugen_node x;
	double xTime, yTime;
	
	for(i = (list_read_index + 1) & size_mask; i != list_write_index; i = (++i) & size_mask)
	{
		x = list[i];
		xTime = x.time;
		j = i;
		
		while(j != list_read_index)
		{
			k = (j - 1) & size_mask;
			yTime = list[k].time;
			if(yTime < xTime)
				break;

			list[j] = list[k];
			j = (j - 1) & size_mask;
		}

		list[j] = x;
	}
}

// Copy nodes from the node_fifo into the internal node_list using atomic read/write operations.
void copy_nodes_from_fifo(node_fifo fifo, node_list list)
{
	bool copying = true;
	if(fifo_read_index == fifo_write_index)
	{
		copying = false;
	}

	while(copying)
	{
		if(fifo_read_index != fifo_write_index)
		{
			LIST_PUSH(list, FIFO_POP(fifo));
		}

		else
		{
			list_sort(list);
			copying = false;
		}
	}
}

void print_node(ugen_node node)
{
	printf("ugen_node { %f, %p }\n", node.time, node.ugen);
}

void print_list(node_list list)
{
	printf("list_read_index: %i, list_write_index: %i\n", list_read_index, list_write_index);
	unsigned int i = list_read_index & size_mask;
	list_write_index = list_write_index & size_mask;
	for(; i != list_write_index; i = (++i) & size_mask)
	{
		print_node(list[i]);
	}
}

void randomize_and_print_list(node_list list)
{
	puts("\n//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////");
	puts("// RANDOMIZE LIST");
	puts("//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////\n");
	
	unsigned int i = 0;
	for(; i < 1000; ++i)
	{
		unsigned int num_pop = random() / (double) RAND_MAX * 100;
	    while((num_pop > 0) && ((list_read_index & size_mask) != ((list_write_index - 1) & size_mask)))
		{
			LIST_POP(list);
			--num_pop;
		}
		
		unsigned int num_push = random() / (double) RAND_MAX * 100;
		while((num_push > 0) && ((list_read_index & size_mask) != (list_write_index & size_mask)))
		{
			ugen_node node = { (random() / (double) RAND_MAX) * 1000.0, 0 };
			LIST_PUSH(list, node);
			--num_push;
		}
	}

	list_read_index = list_read_index & size_mask;
	list_write_index = list_write_index & size_mask;
	print_list(list);
}

void sort_and_print_list(node_list list)
{
	puts("\n//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////");
	puts("// SORT LIST");
	puts("//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////\n");

	list_sort(list);
	print_list(list);
}

void test_list()
{
	node_list list = new_node_list();
	while(list_write_index < (max_nodes * 0.75))
	{
		ugen_node node = { (random() / (double) RAND_MAX) * 1000.0, 0 };
		LIST_PUSH(list, node);
	}

	print_list(list);

	unsigned int i = 0;
	for(; i < 100; ++i)
	{
		sort_and_print_list(list);
		randomize_and_print_list(list);
	}

	sort_and_print_list(list);
}
