/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <sndfile.h>

#include "../AudioRuntime.h"
#include "Containers/HashTable.h"
#include "Util.h"

// Use haskell threads to do actual loading
// Have a single thread that reads a tchan for load sample messages?
// Have it send a message to the nrt thread with the name/double* pair to for storing in the sample_hash_table
// The RT thread never allocates/deallocates or accesses the sample_hash_table

// playSample

typedef struct
{
    sample_buffer* buffer;
    long double read_index;
} playSample_data;

void playSample_constructor(ugen* u)
{
    u->data = malloc(sizeof(playSample_data));
    const char* sample_file_path = (const char*) u->constructor_args;
    sample_buffer* buffer = retrieve_sample_buffer(sample_file_path);
    if (buffer == NULL)
        printf("Unable to find sound file: %s\n", sample_file_path);

    playSample_data data = { buffer, 0 };
    *((playSample_data*) u->data) = data;
}

void playSample_deconstructor(ugen* u)
{
    free(u->data);
}

static inline double playSample_inline_calc(playSample_data* data, double* samples, uint32_t num_samples, double rate)
{
    long double read_index = data->read_index;
    double y = 0;

    if (read_index >= 0 && read_index < num_samples)
    {
        y = samples[(uint64_t) read_index];
        data->read_index = read_index + (long double) rate;
    }

    else
    {
        y = 0;
    }

    return y;
}

#define PLAY_SAMPLE_CALC(CONTROL_ARGS, AUDIO_ARGS)                                  \
playSample_data* data = (playSample_data*) u.data;                                  \
if (data->buffer != NULL)                                                           \
{                                                                                   \
    double* in0 = UGEN_INPUT_BUFFER(u, 0);                                          \
    double* out = UGEN_OUTPUT_BUFFER(u, 0);                                         \
    double* samples = data->buffer->samples;                                        \
    uint32_t num_samples = data->buffer->num_samples;                               \
    double rate;                                                                    \
    CONTROL_ARGS                                                                    \
    AUDIO_LOOP(                                                                     \
        AUDIO_ARGS                                                                  \
        UGEN_OUT(out, playSample_inline_calc(data, samples, num_samples, rate));    \
    );                                                                              \
}                                                                                   \

#define PLAYSAMPLE_RATEK rate = *in0;
#define PLAYSAMPLE_RATEA rate = UGEN_IN(in0);


// 0
void playSample_k_calc(ugen u)
{
    PLAY_SAMPLE_CALC(
        // Control Arguments
        PLAYSAMPLE_RATEK      /* 0 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void playSample_a_calc(ugen u)
{
    PLAY_SAMPLE_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        PLAYSAMPLE_RATEA      /* 0 */
    )
}
