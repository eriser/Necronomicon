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
const double SAMPLE_RATE = 44100;
const double RECIP_SAMPLE_RATE = 1.0 / 44100.0;

#define TABLE_SIZE   (200)

double constants[7] = { 0, 1, 2, 3, 4, 5, 6 };

//////////////
// UGens
//////////////

typedef struct
{
	double amplitude;
	double offset;
} Signal;

typedef Signal Calc(void* args, double time);

typedef struct
{
	void* args;
	unsigned int numArgs;
	Calc* calc;
} UGen;

Signal sinCalc(void* args, double time)
{
	UGen* freqUGen = &((UGen*) args)[0];
	
	Signal freq = freqUGen->calc(freqUGen->args, time);
	double amplitude = sin(freq.offset * TWO_PI * time * RECIP_SAMPLE_RATE + freq.amplitude);
	Signal signal = { amplitude, 0 };
	return signal;
}

UGen sinOsc(UGen freq)
{
	void* args = malloc(sizeof(UGen));
	((UGen*) args)[0] = freq;
	UGen ugen;
	ugen.args = args;
	ugen.calc = sinCalc;
	ugen.numArgs = 1;
	return ugen;
}

Signal numberCalc(void* args, double time)
{
	return ((Signal*) args)[0];
}

UGen number(double number)
{
	Signal signal = { 0, number };
	void* args = malloc(sizeof(Signal));
	((Signal*) args)[0] = signal;
	UGen ugen;
	ugen.args = args;
	ugen.calc = numberCalc;
	ugen.numArgs = 1;
	return ugen;
}

Signal addCalc(void* args, double time)
{
	UGen* a = &((UGen*) args)[0];
	UGen* b = &((UGen*) args)[1];
	Signal as = a->calc(a->args, time);
	Signal bs = b->calc(b->args, time);
	Signal signal = { as.amplitude + bs.amplitude, as.offset + bs.offset };
	return signal;
}

UGen add(UGen a, UGen b)
{
	void* args = malloc(sizeof(UGen) * 2);
	((UGen*) args)[0] = a;
	((UGen*) args)[1] = b;
	UGen ugen;
	ugen.args = args;
	ugen.calc = addCalc;
	ugen.numArgs = 2;
	return ugen;
}

Signal mulCalc(void* args, double time)
{
	UGen* a = &((UGen*) args)[0];
	UGen* b = &((UGen*) args)[1];
	Signal as = a->calc(a->args, time);
	Signal bs = b->calc(b->args, time);
	Signal signal = { (as.amplitude + as.offset) * bs.amplitude,  bs.offset };
	return signal;
}

UGen mul(UGen a, UGen b)
{
	void* args = malloc(sizeof(UGen) * 2);
	((UGen*) args)[0] = a;
	((UGen*) args)[1] = b;
	UGen ugen;
	ugen.args = args;
	ugen.calc = mulCalc;
	ugen.numArgs = 2;
	return ugen;
}

double myCoolSynth(double time)
{
	//UGen synthUGen = sinOsc(mul(add(mul(sinOsc(number(0.3)), number(0.5)), number(0.5)), number(440.0)));
	UGen synthUGen = sinOsc(number(440.0));
	Signal signal = synthUGen.calc(synthUGen.args, time);
	return signal.amplitude + signal.offset; // ?????????????
}

//////////////
// Runtime
//////////////

unsigned int absolute_time = 0;
jack_port_t* output_port1;
jack_port_t* output_port2;
jack_client_t* client;

typedef struct
{
    UGen synth;
} SynthData;

typedef double NodeCallback(double time);

typedef struct
{
	NodeCallback* calc;
	// NodeList* next;
} NodeList;

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
void
jack_shutdown (void *arg)
{
	exit (1);
}

int process(jack_nframes_t nframes, void *arg)
{
	jack_default_audio_sample_t *out1, *out2;
	SynthData* data = (SynthData*) arg;
	
	int i;

	out1 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port1, nframes);
	out2 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port2, nframes);

	for(i = 0; i < nframes; i++)
    {
		Signal signal = data->synth.calc(data->synth.args, absolute_time);
		double sample = signal.amplitude + signal.offset;
        out1[i] = sample;
        out2[i] = sample;
        absolute_time++;
    }
    
	return 0;      
}

void startRuntime(double sampleRate)
{
	puts("Starting Necronomicon");

	const char** ports;
	const char* client_name = "Necronomicon";
	const char* server_name = NULL;
	jack_options_t options = JackNullOption;
	jack_status_t status;
	SynthData data = { sinOsc(mul(sinOsc(number(0.3)), number(440.0))) };
	/* SynthData data = { sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(mul(sinOsc(number(0.3)), number(440.0)))))))))))))))))))))) }; */

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

	jack_set_process_callback (client, process, &data);

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
	//exit (0);
	//printf("SampleRate: %f\n", SAMPLE_RATE);
	
}
