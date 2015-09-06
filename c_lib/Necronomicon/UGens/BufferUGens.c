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

// playSample

typedef struct
{
    char* sample_file_path;
    int32_t num_channels;
} playSample_constructor_args;

typedef struct
{
    sample_buffer* buffer;
    long double read_index;
    int32_t num_channels;
} playSample_data;

void playSample_constructor(ugen* u)
{
    u->data = malloc(sizeof(playSample_data));
    memset(u->data, 0, sizeof(playSample_data));

    playSample_constructor_args* constructor_args = (playSample_constructor_args*) u->constructor_args;
    if (constructor_args == NULL)
    {
        puts("playSample_constructor :: constructor_args = NULL");
        return;
    }

    const char* sample_file_path = constructor_args->sample_file_path;

    if (sample_file_path == NULL)
    {
        puts("playSample_constructor :: sample_file_path = NULL");
        return;
    }

    int32_t num_channels = constructor_args->num_channels;
    sample_buffer* buffer = retrieve_sample_buffer(sample_file_path);

    if (buffer == NULL)
    {
        printf("Unable to find sound file: %s\n", sample_file_path);
    }
    else if(buffer->num_channels < num_channels)
    {
        printf("the sample %s has %i channels, but %i were requested. Changing to %i channels\n", sample_file_path, buffer->num_channels, num_channels, buffer->num_channels);
        num_channels = buffer->num_channels;
    }

    playSample_data data = { buffer, 0, num_channels };
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
}

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

// playMonoSampleL

static inline double playSampleL_inline_calc(playSample_data* data, double* samples, uint32_t num_samples, double rate)
{
    double y = 0;
    long double read_index = data->read_index;
    uint64_t lread_index = read_index;
    if (lread_index >= 0 && lread_index < num_samples)
    {
        y = samples[lread_index];
        data->read_index = read_index + (long double) rate;
    }

    double y2 = 0;
    uint64_t lread_index2 = lread_index + 1;
    if (lread_index2 >= 0 && lread_index2 < num_samples)
    {
        y = samples[lread_index2];
    }

    const double linear_interp_scale = 0.5;
    return (y + y2) * linear_interp_scale;
}


#define PLAY_MONO_SAMPLE_L_CALC(CONTROL_ARGS, AUDIO_ARGS)                           \
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
        UGEN_OUT(out, playSampleL_inline_calc(data, samples, num_samples, rate));   \
    );                                                                              \
}

#define PLAYMONOSAMPLEL_RATEK rate = *in0;
#define PLAYMONOSAMPLEL_RATEA rate = UGEN_IN(in0);

// 0
void playMonoSampleL_k_calc(ugen u)
{
    PLAY_MONO_SAMPLE_L_CALC(
        // Control Arguments
        PLAYMONOSAMPLEL_RATEK /* 0 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void playMonoSampleL_a_calc(ugen u)
{
    PLAY_MONO_SAMPLE_L_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        PLAYMONOSAMPLEL_RATEA /* 0 */
    )
}

// playMonoSampleC

static inline double playSampleC_inline_calc(playSample_data* data, double* samples, uint32_t num_samples, double rate)
{
    long double read_index = data->read_index;
    data->read_index = read_index + (long double) rate;
    uint64_t iread_index1 = read_index;
    const int64_t iread_index0 = iread_index1 + 1;
    double y = 0;

    if (iread_index0 < 0)
    {
        y = 0;
    }

    else
    {
        double delta = read_index - (long double) iread_index1;
        const int64_t iread_index2 = iread_index1 - 1;
        const int64_t iread_index3 = iread_index1 - 2;
        double y0, y1, y2, y3;

        if (iread_index1 < 0)
        {
            y0 = iread_index0 < num_samples ? samples[iread_index0] : 0;
            y1 = y2 = y3 = 0;
        }

        else if (iread_index2 < 0)
        {
            y0 = iread_index0 < num_samples ? samples[iread_index0] : 0;
            y1 = iread_index1 < num_samples ? samples[iread_index1] : 0;
            y2 = y3 = 0;
        }

        else if (iread_index3 < 0)
        {
            y0 = iread_index0 < num_samples ? samples[iread_index0] : 0;
            y1 = iread_index1 < num_samples ? samples[iread_index1] : 0;
            y2 = iread_index2 < num_samples ? samples[iread_index1] : 0;
            y3 = 0;
        }

        else
        {
            y0 = iread_index0 < num_samples ? samples[iread_index0] : 0;
            y1 = iread_index1 < num_samples ? samples[iread_index1] : 0;
            y2 = iread_index2 < num_samples ? samples[iread_index2] : 0;
            y3 = iread_index3 < num_samples ? samples[iread_index3] : 0;
        }

        y = CUBIC_INTERP(y0, y1, y2, y3, delta);
    }

    return y;
}

#define PLAY_MONO_SAMPLE_C_CALC(CONTROL_ARGS, AUDIO_ARGS)                           \
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
        UGEN_OUT(out, playSampleC_inline_calc(data, samples, num_samples, rate));   \
    );                                                                              \
}

#define PLAYMONOSAMPLEC_RATEK rate = *in0;
#define PLAYMONOSAMPLEC_RATEA rate = UGEN_IN(in0);

// 0
void playMonoSampleC_k_calc(ugen u)
{
    PLAY_MONO_SAMPLE_C_CALC(
        // Control Arguments
        PLAYMONOSAMPLEC_RATEK /* 0 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void playMonoSampleC_a_calc(ugen u)
{
    PLAY_MONO_SAMPLE_C_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        PLAYMONOSAMPLEC_RATEA /* 0 */
    )
}

// playStereoSample

typedef struct
{
    double l_channel;
    double r_channel;
} playSample_stereo_out;

static inline playSample_stereo_out playSample_stereo_inline_calc(playSample_data* data, double* samples, uint32_t num_samples, double rate)
{
    long double read_index = data->read_index;
    playSample_stereo_out y = { 0, 0 };

    const double two_channels_stride = 2.0;
    const uint64_t lread_index = (uint64_t) (read_index * two_channels_stride); // interleaved indexes
    const uint64_t one_channel_offset = 1;
    const uint64_t rread_index = lread_index + one_channel_offset;
    if (read_index >= 0 && rread_index < num_samples)
    {
        y.l_channel = samples[lread_index];
        y.r_channel = samples[rread_index];
        data->read_index = read_index + (long double) rate;
    }

    return y;
}

#define PLAY_SAMPLE_STEREO_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                   \
playSample_data* data = (playSample_data*) u.data;                                                          \
if (data->buffer != NULL)                                                                                   \
{                                                                                                           \
    double* in0 = UGEN_INPUT_BUFFER(u, 0);                                                                  \
    double* out0 = UGEN_OUTPUT_BUFFER(u, 0);                                                                \
    double* out1 = UGEN_OUTPUT_BUFFER(u, 1);                                                                \
    double* samples = data->buffer->samples;                                                                \
    uint32_t num_samples = data->buffer->num_samples;                                                       \
    double rate;                                                                                            \
    CONTROL_ARGS                                                                                            \
    AUDIO_LOOP(                                                                                             \
        AUDIO_ARGS                                                                                          \
        playSample_stereo_out stereo_out = playSample_stereo_inline_calc(data, samples, num_samples, rate); \
        UGEN_OUT(out0, stereo_out.l_channel);                                                               \
        UGEN_OUT(out1, stereo_out.r_channel);                                                               \
    );                                                                                                      \
}

// 0
void playSample_stereo_k_calc(ugen u)
{
    PLAY_SAMPLE_STEREO_CALC(
        // Control Arguments
        PLAYSAMPLE_RATEK      /* 0 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void playSample_stereo_a_calc(ugen u)
{
    PLAY_SAMPLE_STEREO_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        PLAYSAMPLE_RATEA      /* 0 */
    )
}
