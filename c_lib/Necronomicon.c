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

#define TABLE_SIZE   (65536)
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
	UGen input = ((UGen*) args)[0];
	UGen delayUGen = ((UGen*) args)[1];
	Signal delayTimeSig = (delayUGen.calc(delayUGen.args, time));
	double delayedTime = delayTimeSig.offset * SAMPLE_RATE + time + (delayTimeSig.amplitude * RECIP_TWO_PI * SAMPLE_RATE);
	return input.calc(input.args, delayedTime);
}

UGen delay(UGen input, UGen amount)
{
	void* args = malloc(sizeof(UGen) * 2);
	((UGen*) args)[0] = input;
	((UGen*) args)[1] = amount;
	UGen ugen = { delayCalc, args, 2 };
	return ugen;
}

Signal timeWarpCalc(void* args, double time)
{
	UGen input = ((UGen*) args)[0];
	UGen timeUGen = ((UGen*) args)[1];
	Signal modTimeSig = (timeUGen.calc(timeUGen.args, time));
	double modedTime = modTimeSig.offset * time + (modTimeSig.amplitude * SAMPLE_RATE);
	return input.calc(input.args, modedTime);
}

UGen timeWarp(UGen input, UGen amount)
{
	void* args = malloc(sizeof(UGen) * 2);
	((UGen*) args)[0] = input;
	((UGen*) args)[1] = amount;
	UGen ugen = { timeWarpCalc, args, 2 };
	return ugen;
}

Signal sinCalc(void* args, double time)
{
	UGen freqUGen = ((UGen*) args)[0];
	Signal freq = freqUGen.calc(freqUGen.args, time);

    //sin function version
	double amplitude = sin(freq.offset * TWO_PI * time * RECIP_SAMPLE_RATE + freq.amplitude);
	
	//look up table version
	/* unsigned short index = freq.offset * TABLE_MUL_RECIP_SAMPLE_RATE * time + (freq.amplitude * TABLE_SIZE_MUL_RECIP_TWO_PI); */
	/* double amplitude = sine_table[index]; */
	
	Signal signal = { amplitude, 0 };
	return signal;
}

UGen sinOsc(UGen freq)
{
	void* args = malloc(sizeof(UGen));
	((UGen*) args)[0] = freq;
	UGen ugen = { sinCalc, args, 1 };
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
	UGen ugen = { numberCalc, args, 1 };
	return ugen;
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

UGen add(UGen a, UGen b)
{
	void* args = malloc(sizeof(UGen) * 2);
	((UGen*) args)[0] = a;
	((UGen*) args)[1] = b;
	UGen ugen = { addCalc, args, 2 };
	return ugen;
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

UGen mul(UGen a, UGen b)
{
	void* args = malloc(sizeof(UGen) * 2);
	((UGen*) args)[0] = a;
	((UGen*) args)[1] = b;
	UGen ugen = { mulCalc, args, 2 };
	return ugen;
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

UGen udiv(UGen a, UGen b)
{
	void* args = malloc(sizeof(UGen) * 2);
	((UGen*) args)[0] = a;
	((UGen*) args)[1] = b;
	UGen ugen = { udivCalc, args, 2 };
	return ugen;
}

Signal uabsCalc(void* args, double time)
{
	UGen input = *((UGen*) args);
	Signal signal = input.calc(input.args, time);
	signal.amplitude = abs(signal.amplitude);
	signal.offset = abs(signal.offset);
	return signal;
}

UGen uabs(UGen input)
{
	void* args = malloc(sizeof(UGen));
	*((UGen*) args) = input;
	UGen ugen = { uabsCalc, args, 1 };
	return ugen;
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

UGen signum(UGen input)
{
	void* args = malloc(sizeof(UGen));
	*((UGen*) args) = input;
	UGen ugen = { signumCalc, args, 1 };
	return ugen;
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

UGen negate(UGen input)
{
	void* args = malloc(sizeof(UGen));
	*((UGen*) args) = input;
	UGen ugen = { negateCalc, args, 1 };
	return ugen;
}


Signal gainCalc(void* args, double time)
{
	UGen a = ((UGen*) args)[0];
	UGen b = ((UGen*) args)[1];
	Signal as = a.calc(a.args, time);
	Signal bs = b.calc(b.args, time);
	Signal signal = { (as.amplitude + as.offset) * bs.amplitude, bs.offset };
	return signal;
}

UGen gain(UGen a, UGen b)
{
	void* args = malloc(sizeof(UGen) * 2);
	((UGen*) args)[0] = a;
	((UGen*) args)[1] = b;
	UGen ugen = { gainCalc, args, 2 };
	return ugen;
}

double myCoolSynth(double time)
{
	/* UGen synthUGen = sinOsc(mul(add(mul(sinOsc(number(0.3)), number(0.5)), number(0.5)), number(440.0))); */
	UGen synthUGen = sinOsc(number(440.0));
	Signal signal = synthUGen.calc(synthUGen.args, time);
	return signal.amplitude + signal.offset; // ?????????????
}

UGen number2(void* args)
{
	UGen ugen = { numberCalc, args, 0 };
}

UGen add2(void* args)
{
	UGen ugen = { addCalc, args, 2 };
	return ugen;
}

typedef UGen (*UGenFunc) (void* args);

UGenFunc ugens[] = { number2, add2 };

// void compileHaskellUGen( 

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
		sine_table[si] = sin(M_PI * (si - (TABLE_SIZE / 2)) / (TABLE_SIZE / 2));
	}
	
	const char** ports;
	const char* client_name = "Necronomicon";
	const char* server_name = NULL;
	jack_options_t options = JackNullOption;
	jack_status_t status;


	SynthData data = { *ugen };
	
	// UGen sinUGen = sinOsc(add(mul(sinOsc(number(1.5)), number(100.0)),number(440.0)));
	// UGen delayTime = add(number(0.5),mul(number(0.5),sinOsc(mul(number(333), sinOsc(number(0.1))))));
	// SynthData data = { mul(number(0.75), delay(sinUGen, delayTime)) };

	// UGen sinUGen = sinOsc(add(mul(sinOsc(number(1.5)), number(100.0)),number(440.0)));
	// UGen delayTime = add(number(1),mul(number(1),sinOsc(number(0.25))));
	/* SynthData data = { mul(number(0.75),delay(sinUGen, delayTime))}; */

	// SynthData data = { mul(number(0.75),timeWarp(sinUGen,delayTime))};

	
	//gain vs mul test 
	/* SynthData data = { sinOsc(add(number(1000.0),mul(sinOsc(number(0.3)),number(400.0)))) }; */

	//pure sin test sound test for LUT
	/* SynthData data = { sinOsc(number(400.0)) }; */
	
	//gain vs mul test 
	/* SynthData data = { sinOsc(add(number(1000.0),mul(sinOsc(number(0.3)),number(400.0)))) }; */

    //20 sins test
	// SynthData data = { sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(sinOsc(mul(sinOsc(number(0.3)), number(440.0)))))))))))))))))))))) };

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
