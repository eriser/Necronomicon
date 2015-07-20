/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <stdlib.h>
#include <math.h>

#include "../AudioRuntime.h"
#include "Util.h"

// line

#define LINE_CALC(CONTROL_ARGS, AUDIO_ARGS)             \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                  \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                 \
uint32_t line_time = *((uint32_t*) u.data);             \
double length;                                          \
double y;                                               \
bool scheduled_for_removal = false;                     \
CONTROL_ARGS                                            \
AUDIO_LOOP(                                             \
    AUDIO_ARGS                                          \
    if (line_time >= length)                            \
    {                                                   \
        y = 0;                                          \
        scheduled_for_removal =  true;                  \
    }                                                   \
    else                                                \
    {                                                   \
        y = fmax(0, 1 - (line_time / length));          \
        ++line_time;                                    \
    };                                                  \
    UGEN_OUT(out, y);                                   \
);                                                      \
if (scheduled_for_removal)                              \
    try_schedule_current_synth_for_removal();           \
*((uint32_t*) u.data) = line_time;

void line_k_calc(ugen u)
{
    LINE_CALC(
        length = in0[0] * SAMPLE_RATE;,
        /* no audio args */
    );
}

void line_a_calc(ugen u)
{
    LINE_CALC(
        /* no control args */,
        length = UGEN_IN(in0) * SAMPLE_RATE;
    );
}

// env

#define OFF_INDEX(OFFSET,INDEX,ARRAY)

typedef struct
{
    double time;
    double curTotalDuration;
    double recipDuration;
    double curve;
    double nextValue;
    double currentValue;
    double nextTotalDuration;
    int32_t index;
    int32_t numValues;
    int32_t    maxIndex;
    int32_t numDurations;

} env_struct;

void env_constructor(ugen* u)
{
    env_struct* data        = malloc(sizeof(env_struct));
    data->time              = 0;
    data->index             = -1;
    data->numValues         = ((double*) u->constructor_args)[0];
    data->maxIndex          = fmax(0, data->numValues - 1);
    data->numDurations      = ((double*) u->constructor_args)[1];
    data->curTotalDuration  = -1;
    data->nextTotalDuration = -1;
    data->recipDuration     = 1;
    data->currentValue      = 0;
    data->nextValue         = 0;
    data->curve             = 0;
    u->data                 = data;
}

void env_deconstructor(ugen* u)
{
    free(u->data);
}

#define ENV_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                                          \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                                                                              \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                                                             \
env_struct   data  = *((env_struct*) u.data);                                                                       \
double x;                                                                                                           \
double curve = UGEN_INPUT_BUFFER(u, 0)[0];                                                                          \
const int32_t maxIndex = data.maxIndex;                                                                             \
bool scheduled_for_removal = false;                                                                                 \
union {                                                                                                             \
    double d;                                                                                                       \
    int32_t x[2];                                                                                                   \
} ud = { 0 };                                                                                                       \
CONTROL_ARGS                                                                                                        \
AUDIO_LOOP(                                                                                                         \
    AUDIO_ARGS                                                                                                      \
    if (data.time >= data.nextTotalDuration)                                                                        \
    {                                                                                                               \
        data.index = data.index + 1;                                                                                \
        if (data.index >= maxIndex)                                                                                 \
        {                                                                                                           \
            scheduled_for_removal = true;                                                                           \
            UGEN_OUT(out,data.nextValue);                                                                           \
            continue;                                                                                               \
        }                                                                                                           \
        else if (data.index < maxIndex)                                                                             \
        {                                                                                                           \
            int32_t dursOffset = 2 + data.numValues;                                                                \
            double nextDuration = UGEN_IN(UGEN_INPUT_BUFFER(u, (data.index % data.numValues) + dursOffset));        \
                                                                                                                    \
            if (data.nextTotalDuration < 0)                                                                         \
                data.curTotalDuration = 0;                                                                          \
            else                                                                                                    \
                data.curTotalDuration = data.nextTotalDuration;                                                     \
                                                                                                                    \
            data.nextTotalDuration = data.curTotalDuration + nextDuration;                                          \
            data.currentValue      = UGEN_IN(UGEN_INPUT_BUFFER(u, MIN(data.index, maxIndex) + 2));                  \
            data.nextValue         = UGEN_IN(UGEN_INPUT_BUFFER(u, MIN(data.index + 1, maxIndex) + 2));              \
                                                                                                                    \
            if (nextDuration == 0.0)                                                                                \
                data.recipDuration = 0.0;                                                                           \
            else                                                                                                    \
                data.recipDuration = 1.0 / nextDuration;                                                            \
                                                                                                                    \
            if (curve < 0)                                                                                          \
                data.curve = 1 / ((curve * -1) + 1);                                                                \
            else                                                                                                    \
                data.curve = curve + 1;                                                                             \
        }                                                                                                           \
    }                                                                                                               \
                                                                                                                    \
    double delta = fast_pow(ud, (data.time - data.curTotalDuration) * data.recipDuration, data.curve);              \
    UGEN_OUT(out,LERP(data.currentValue, data.nextValue, delta) * x);                                               \
    data.time   += RECIP_SAMPLE_RATE;                                                                               \
);                                                                                                                  \
if (scheduled_for_removal == true)                                                                                  \
    try_schedule_current_synth_for_removal();                                                                       \
*((env_struct*) u.data) = data;

void env_k_calc(ugen u)
{
    ENV_CALC(
        x = in1[0];,
        /* no audio args */
    )
}

void env_a_calc(ugen u)
{
    ENV_CALC(
        /* no control args */,
        x = UGEN_IN(in1);
    )
}

// env2

#define ENV2_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                                         \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                                                                              \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                                                             \
double x;                                                                                                           \
env_struct data  = *((env_struct*) u.data);                                                                         \
double curve = UGEN_INPUT_BUFFER(u, 0)[0];                                                                          \
const int32_t maxIndex = data.maxIndex;                                                                             \
union {                                                                                                             \
    double d;                                                                                                       \
    int32_t x[2];                                                                                                   \
} ud = { 0 };                                                                                                       \
CONTROL_ARGS                                                                                                        \
AUDIO_LOOP(                                                                                                         \
    AUDIO_ARGS                                                                                                      \
    if (data.time >= data.nextTotalDuration)                                                                        \
    {                                                                                                               \
        data.index = data.index + 1;                                                                                \
        if (data.index >= maxIndex)                                                                                 \
        {                                                                                                           \
            UGEN_OUT(out,data.nextValue);                                                                           \
            continue;                                                                                               \
        }                                                                                                           \
        else if (data.index < maxIndex)                                                                             \
        {                                                                                                           \
            int32_t dursOffset = 2 + data.numValues;                                                                \
            double nextDuration = *UGEN_INPUT_BUFFER(u, (data.index % data.numValues) + dursOffset);                \
                                                                                                                    \
            if (data.nextTotalDuration < 0)                                                                         \
                data.curTotalDuration = 0;                                                                          \
            else                                                                                                    \
                data.curTotalDuration = data.nextTotalDuration;                                                     \
                                                                                                                    \
            data.nextTotalDuration = data.curTotalDuration + nextDuration;                                          \
            data.currentValue      = *UGEN_INPUT_BUFFER(u, MIN(data.index, maxIndex) + 2);                          \
            data.nextValue         = *UGEN_INPUT_BUFFER(u, MIN(data.index + 1, maxIndex) + 2);                      \
                                                                                                                    \
            if (nextDuration == 0.0)                                                                                \
                data.recipDuration = 0.0;                                                                           \
            else                                                                                                    \
                data.recipDuration = 1.0 / nextDuration;                                                            \
                                                                                                                    \
            if (curve < 0)                                                                                          \
                data.curve = 1 / ((curve * -1) + 1);                                                                \
            else                                                                                                    \
                data.curve = curve + 1;                                                                             \
        }                                                                                                           \
    }                                                                                                               \
                                                                                                                    \
    double delta = fast_pow(ud, (data.time - data.curTotalDuration) * data.recipDuration, data.curve);              \
    UGEN_OUT(out,LERP(data.currentValue, data.nextValue, delta) * x);                                               \
    data.time   += RECIP_SAMPLE_RATE;                                                                               \
);                                                                                                                  \
*((env_struct*) u.data) = data;                                                                                     \

void env2_k_calc(ugen u)
{
    ENV2_CALC(
        x = in1[0];,
        /* no audio args */
    )
}

void env2_a_calc(ugen u)
{
    ENV2_CALC(
        /* no control args */,
        x = UGEN_IN(in1);
    )
}

// lfsaw

#define LFSAW_CALC(CONTROL_ARGS, AUDIO_ARGS)                \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                      \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                      \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                     \
double phase = *((double*) u.data);                         \
double freq;                                                \
/* double phaseArg; */                                      \
double amp1;                                                \
double amp2;                                                \
double delta;                                               \
double y;                                                   \
CONTROL_ARGS                                                \
AUDIO_LOOP(                                                 \
    AUDIO_ARGS                                              \
    /* Branchless and table-less saw */                     \
    amp1   = (uint16_t) phase;                              \
    amp2   = amp1 + 1;                                      \
    delta  = phase - ((int64_t) phase);                     \
    y      = LERP(amp1, amp2, delta) * RECIP_TABLE_SIZE;    \
    phase += TABLE_MUL_RECIP_SAMPLE_RATE * freq;            \
    UGEN_OUT(out, y);                                       \
);                                                          \
*((double*) u.data) = phase;                                \

void lfsaw_aa_calc(ugen u)
{
    LFSAW_CALC(
        /* no control args */,
        freq = UGEN_IN(in0); // Audio args
        /* phaseArg = UGEN_IN(in1); */
    );
}

void lfsaw_ak_calc(ugen u)
{
    LFSAW_CALC(
        /*phaseArg = in1[0];*/, // Control arg
        freq = UGEN_IN(in0); // Audio arg
    );
}

void lfsaw_ka_calc(ugen u)
{
    LFSAW_CALC(
        freq = in0[0];, // Control arg
        /*phaseArg = UGEN_IN(in1);*/ // Audio arg
    );
}

void lfsaw_kk_calc(ugen u)
{
    LFSAW_CALC(
        freq = in0[0]; // Control args
        /*phaseArg = in1[0]*/,
        /* no audio args */
    );
}

// lfpulse

#define SIXTEEN_BITS 16
#define MASK_OFFSET 1
const int16_t lfpulse_phase_shift = sizeof(int16_t) * SIXTEEN_BITS - MASK_OFFSET;

#define LFPULSE_CALC(CONTROL_ARGS, AUDIO_ARGS)                          \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                  \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                                  \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                 \
double phase = *((double*) u.data);                                     \
double freq;                                                            \
/*double phaseArg;*/                                                    \
double y;                                                               \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
    /*Branchless and table-less square*/                                \
    y = 1 | (((int16_t) phase) >> lfpulse_phase_shift);                 \
    phase += TABLE_MUL_RECIP_SAMPLE_RATE * freq;                        \
    /* y * 0.5 + 0.5 to make range 0 to 1 */                            \
    UGEN_OUT(out, y * 0.5 + 0.5);                                       \
);                                                                      \
*((double*) u.data) = phase;                                            \

void lfpulse_aa_calc(ugen u)
{
    LFPULSE_CALC(
        /* no control args */,
        freq = UGEN_IN(in0); // Audio args
        /* phaseArg = UGEN_IN(in1); */
    );
}

void lfpulse_ak_calc(ugen u)
{
    LFPULSE_CALC(
        /*phaseArg = in1[0];*/, // Control arg
        freq = UGEN_IN(in0); // Audio arg
    );
}

void lfpulse_ka_calc(ugen u)
{
    LFPULSE_CALC(
        freq = in0[0];, // Control arg
        /*phaseArg = UGEN_IN(in1);*/ // Audio arg
    );
}

void lfpulse_kk_calc(ugen u)
{
    LFPULSE_CALC(
        freq = in0[0]; // Control args
        /*phaseArg = in1[0]*/,
        /* no audio args */
    );
}

// impulse

#define IMPULSE_CALC(CONTROL_ARGS, AUDIO_ARGS)      \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);            \
/* double*  in1  = UGEN_INPUT_BUFFER(u, 1); */      \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);           \
double  phase = (*(double*)u.data);                 \
double freq;                                        \
/* Offset is unused at the moment... */             \
/* double offset; */                                \
CONTROL_ARGS                                        \
AUDIO_LOOP(                                         \
    AUDIO_ARGS                                      \
    phase += freq * RECIP_SAMPLE_RATE;              \
    if (phase >= 1)                                 \
        UGEN_OUT(out,1);                            \
    else                                            \
        UGEN_OUT(out,0);                            \
    phase = fmod(phase,1);                          \
);                                                  \
(*(double*)u.data) = phase;                         \

void impulse_aa_calc(ugen u)
{
    IMPULSE_CALC(
        /* no control args */,
        freq = UGEN_IN(in0); // Audio args
        /* offset = UGEN_IN(in1); */
    );
}

void impulse_ak_calc(ugen u)
{
    IMPULSE_CALC(
        /*offset = in1[0];*/, // Control arg
        freq = UGEN_IN(in0); // Audio arg
    );
}

void impulse_ka_calc(ugen u)
{
    IMPULSE_CALC(
        freq = in0[0];, // Control arg
        /*offset = UGEN_IN(in1);*/ // Audio arg
    );
}

void impulse_kk_calc(ugen u)
{
    IMPULSE_CALC(
        freq = in0[0]; // Control args
        /*offset = in1[0]*/,
        /* no audio args */
    );
}
