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
// Scheduler
//////////////

typedef void SchedulerCallback(double time);

typedef struct SchedulerNode
{
	struct Scheduler* next;
	SchedulerCallback* callback;
} SchedulerNode;

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
	printf("UGen size: %i\n", ugenSize);
	printf("UGen alignment: %i\n", ugenAlignment);
	printf("Signal size: %i\n", signalSize);
	printf("Signal alignment: %i\n", signalAlignment);
	
	printf("Delay calc: %p\n", delayCalc);
	printf("Number calc: %p\n", numberCalc);
	
	puts("Starting Necronomicon");
	printUGen(ugen, 0);
	puts("\n");
	
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
