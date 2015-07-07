/*
  Necronomicon - Deterministic Audio Engine
  Copyright 2014 - Chad McKinney and Curtis McKinney
*/

#include <stdlib.h>
#include <math.h>

#include "../Necronomicon.h"
#include "UGenUtil.h"
#include "Random.h"

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
    rand->value0 = RAND_SIG();
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

void rand_range_constructor(ugen* u)
{
    double   seed  = u->constructor_args[0];
    double   min   = u->constructor_args[1];
    double   max   = u->constructor_args[2];
    double   range = max - min;
    double   in    = RAND_ONE();
    double   value = (in * range) + min;
    double*  out  = (_necronomicon_current_node_underconstruction->ugen_wires + (u->outputs[0] * BLOCK_SIZE));

    uint32_t i;
    for (i = 0; i < BLOCK_SIZE; ++i)
    {
        out[i] = value;
    }
}

void rand_range_deconstructor(ugen* u) { }
void rand_calc(ugen u) { }

#define NOISEN_CALC(CONTROL_ARGS, AUDIO_ARGS)                               \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                    \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                                   \
rand_t   rand = *((rand_t*) u.data);                                        \
double freq;                                                                \
CONTROL_ARGS                                                                \
AUDIO_LOOP(                                                                 \
    AUDIO_ARGS                                                              \
    if (rand.phase + RECIP_SAMPLE_RATE * freq >= 1.0)                       \
    {                                                                       \
        rand.phase  = fmod(rand.phase + RECIP_SAMPLE_RATE * freq,1.0);      \
        rand.value0 = RAND_SIG();                                           \
    }                                                                       \
    else                                                                    \
    {                                                                       \
        rand.phase = rand.phase + RECIP_SAMPLE_RATE * freq;                 \
    }                                                                       \
    UGEN_OUT(out, rand.value0);                                             \
);                                                                          \
*((rand_t*) u.data) = rand;                                                 \

void lfnoiseN_k_calc(ugen u)
{
    NOISEN_CALC(
        freq  = in0[0];,
        /* no audio args */
    )
}

void lfnoiseN_a_calc(ugen u)
{
    NOISEN_CALC(
        /* no control args */,
        freq  = UGEN_IN(in0);
    )
}

#define LFNOISEL_CALC(CONTROL_ARGS, AUDIO_ARGS)                             \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                    \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                                   \
rand_t   rand = *((rand_t*) u.data);                                        \
double freq;                                                                \
CONTROL_ARGS                                                                \
AUDIO_LOOP(                                                                 \
    AUDIO_ARGS                                                              \
    if (rand.phase + RECIP_SAMPLE_RATE * freq >= 1.0)                       \
    {                                                                       \
        rand.phase  = fmod(rand.phase + RECIP_SAMPLE_RATE * freq,1.0);      \
        rand.value1 = rand.value0;                                          \
        rand.value0 = RAND_SIG();                                           \
    }                                                                       \
    else                                                                    \
    {                                                                       \
        rand.phase = rand.phase + RECIP_SAMPLE_RATE * freq;                 \
    }                                                                       \
    UGEN_OUT(out, LERP(rand.value1,rand.value0,rand.phase));                \
);                                                                          \
*((rand_t*) u.data) = rand;                                                 \

void lfnoiseL_k_calc(ugen u)
{
    LFNOISEL_CALC(
        freq  = in0[0];,
        /* no audio args */
    );
}

void lfnoiseL_a_calc(ugen u)
{
    LFNOISEL_CALC(
        /* no control args */,
        freq  = UGEN_IN(in0);
    )
}

#define LFNOISEC_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                     \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                                            \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                                                           \
rand_t   rand = *((rand_t*) u.data);                                                                \
double freq;                                                                                        \
CONTROL_ARGS                                                                                        \
AUDIO_LOOP(                                                                                         \
    AUDIO_ARGS                                                                                      \
    if (rand.phase + RECIP_SAMPLE_RATE * freq >= 1.0)                                               \
    {                                                                                               \
        rand.phase  = fmod(rand.phase + RECIP_SAMPLE_RATE * freq,1.0);                              \
        rand.value3 = rand.value2;                                                                  \
        rand.value2 = rand.value1;                                                                  \
        rand.value1 = rand.value0;                                                                  \
        rand.value0 = RAND_SIG();                                                                   \
    }                                                                                               \
    else                                                                                            \
    {                                                                                               \
        rand.phase = rand.phase + RECIP_SAMPLE_RATE * freq;                                         \
    }                                                                                               \
    UGEN_OUT(out, CUBIC_INTERP(rand.value3,rand.value2,rand.value1,rand.value0,rand.phase));        \
);                                                                                                  \
*((rand_t*) u.data) = rand;                                                                         \

void lfnoiseC_k_calc(ugen u)
{
    LFNOISEC_CALC(
        freq  = in0[0];,
        /* no audio args */
    );
}

void lfnoiseC_a_calc(ugen u)
{
    LFNOISEC_CALC(
        /* no control args */,
        freq  = UGEN_IN(in0);
    )
}
