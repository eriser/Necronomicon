/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
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

// dust

typedef struct
{
    double phase;
    double period;
} dust_t;

void dust_constructor(ugen* u)
{
    dust_t* dust = malloc(sizeof(dust_t));
    dust->phase  = 0;
    dust->period = -1;
    u->data = dust;
}

void dust_deconstructor(ugen* u)
{
    free(u->data);
}

#define DUST_CALC(CONTROL_ARGS, AUDIO_ARGS)                             \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);                                 \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);                                \
dust_t  dust = *((dust_t*)u.data);                                      \
if (dust.period == -1)                                                  \
    dust.period = RAND_TWO();                                           \
double density;                                                         \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
    if (dust.phase + density * RECIP_SAMPLE_RATE >= dust.period)        \
    {                                                                   \
        dust.phase  = 0;                                                \
        dust.period = RAND_TWO();                                       \
        UGEN_OUT(out,RAND_SIG());                                       \
    }                                                                   \
    else                                                                \
    {                                                                   \
        dust.phase = dust.phase + density * RECIP_SAMPLE_RATE;          \
        UGEN_OUT(out,0);                                                \
    }                                                                   \
);                                                                      \
*((dust_t*)u.data) = dust;                                              \

void dust_k_calc(ugen u)
{
    DUST_CALC(
        density = *in0;,
        /* no audio args */
    )
}

void dust_a_calc(ugen u)
{
    DUST_CALC(
        /* no control args */,
        density = UGEN_IN(in0);
    )
}

// dust2

#define DUST2_CALC(CONTROL_ARGS, AUDIO_ARGS)                            \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);                                 \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);                                \
dust_t  dust = *((dust_t*)u.data);                                      \
if (dust.period == -1)                                                  \
    dust.period = RAND_TWO();                                           \
double density;                                                         \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
    if (dust.phase + density * RECIP_SAMPLE_RATE >= dust.period)        \
    {                                                                   \
        dust.phase  = 0;                                                \
        dust.period = RAND_TWO();                                       \
        UGEN_OUT(out,RAND_ONE());                                       \
    }                                                                   \
    else                                                                \
    {                                                                   \
        dust.phase = dust.phase + density * RECIP_SAMPLE_RATE;          \
        UGEN_OUT(out,0);                                                \
    }                                                                   \
);                                                                      \
*((dust_t*)u.data) = dust;                                              \

void dust2_k_calc(ugen u)
{
    DUST2_CALC(
        density = *in0;,
        /* no audio args */
    )
}

void dust2_a_calc(ugen u)
{
    DUST2_CALC(
        /* no control args */,
        density = UGEN_IN(in0);
    )
}

// White noise

void white_calc(ugen u)
{
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    AUDIO_LOOP(
        UGEN_OUT(out, RAND_SIG());
    );
}

// Pink Noise

#define DICE_SIZE 16
#define DICE_MASK 15
#define DICE_SHIFT 13

typedef struct
{
    uint32_t* dice;
    int32_t total;
} pink_data;

void pink_constructor(ugen* u)
{
    u->data = malloc(sizeof(pink_data));
    uint32_t* dice = malloc(DICE_SIZE * sizeof(uint32_t));
    int32_t total = 0;

    uint32_t i;
    for (i = 0; i < DICE_SIZE; ++i)
    {
        uint32_t newrand = trand() >> DICE_SHIFT;
        total += newrand;
        dice[i] = newrand;
    }

    pink_data data = { dice, total };
    *((pink_data*) u->data) = data;
}

void pink_deconstructor(ugen* u)
{
    free(((pink_data*) u->data)->dice);
    free(u->data);
}

// Voss/McCartney algorithm
// http://www.firstpr.com.au/dsp/pink-noise/
void pink_calc(ugen u)
{
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    pink_data* data = (pink_data*) u.data;
    uint32_t* dice = data->dice;
    int32_t total = data->total;

    AUDIO_LOOP(
        uint32_t counter = trand();
        uint32_t k = __builtin_ctz(counter) & DICE_MASK;
        uint32_t newrand = counter >> DICE_SHIFT;
        uint32_t prevrand = dice[k];
        dice[k] = newrand;
        total += (newrand - prevrand);
        newrand = trand() >> DICE_SHIFT;
        four_bytes y;
        y.word = (total + newrand) | 0x40000000;
        UGEN_OUT(out, y.f - 3.0);
    );

    data->total = total;
}

// brown noise

typedef struct
{
    double level;
} brown_data;

void brownNoise_constructor(ugen* u)
{
    u->data = malloc(sizeof(brown_data));
    brown_data data = { 0 };
    *((brown_data*) u->data) = data;
}

void brownNoise_deconstructor(ugen* u)
{
    free(u->data);
}

void brownNoise_calc(ugen u)
{
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    brown_data* data = (brown_data*) u.data;
    double y = data->level;

    AUDIO_LOOP(
        y += RAND_SIG_EIGHTH();
        if (y > 1.0) y = 2.0 - y;
        else if(y < -1.0) y = -2.0 - y;
        UGEN_OUT(out, y);
    );

    data->level = y;
}

// Simplex

// Many thanks to Stefan Gustavason for the nice Simplex implementation
// http://staffwww.itn.liu.se/~stegu/aqsis/aqsis-newnoise/

const uint8_t simplex_noise_perm[] = {
    151,160,137,91,90,15,131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,
    142,8,99,37,240,21,10,23,190,6,148,247,120,234,75,0,26,197,62,94,252,219,
    203,117,35,11,32,57,177,33,88,237,149,56,87,174,20,125,136,171,168,68,175,
    74,165,71,134,139,48,27,166,77,146,158,231,83,111,229,122,60,211,133,230,
    220,105,92,41,55,46,245,40,244,102,143,54,65,25,63,161,1,216,80,73,209,76,
    132,187,208,89,18,169,200,196,135,130,116,188,159,86,164,100,109,198,173,
    186,3,64,52,217,226,250,124,123,5,202,38,147,118,126,255,82,85,212,207,206,
    59,227,47,16,58,17,182,189,28,42,223,183,170,213,119,248,152,2,44,154,163,
    70,221,153,101,155,167,43,172,9,129,22,39,253,19,98,108,110,79,113,224,232,
    178,185,112,104,218,246,97,228,251,34,242,193,238,210,144,12,191,179,162,
    241,81,51,145,235,249,14,239,107,49,192,214,31,181,199,106,157,184,84,204,
    176,115,121,50,45,127,4,150,254,138,236,205,93,222,114,67,29,24,72,243,141,
    128,195,78,66,215,61,156,180,151,160,137,91,90,15,131,13,201,95,96,53,194,
    233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,190,6,148,247,120,234,
    75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,88,237,149,56,87,174,
    20,125,136,171,168,68,175,74,165,71,134,139,48,27,166,77,146,158,231,83,111,
    229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,102,143,54,65,25,63,
    161,1,216,80,73,209,76,132,187,208,89,18,169,200,196,135,130,116,188,159,
    86,164,100,109,198,173,186,3,64,52,217,226,250,124,123,5,202,38,147,118,126,
    255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,223,183,170,213,119,
    248,152,2,44,154,163,70,221,153,101,155,167,43,172,9,129,22,39,253,19,98,
    108,110,79,113,224,232,178,185,112,104,218,246,97,228,251,34,242,193,238,
    210,144,12,191,179,162,241,81,51,145,235,249,14,239,107,49,192,214,31,181,
    199,106,157,184,84,204,176,115,121,50,45,127,4,150,254,138,236,205,93,222,
    114,67,29,24,72,243,141,128,195,78,66,215,61,156,180
};


void simplex_constructor(ugen* u)
{
    u->data = malloc(DOUBLE_SIZE);
    *((double*) u->data) = 0;
}

void simplex_deconstructor(ugen* u)
{
    free(u->data);
}


static inline double simplex_grad1(uint8_t hash, double phase)
{
    uint8_t h = hash & 15;
    int32_t grad = 1 + (h & 7);
    if (h & 8) grad = -grad;
    return (double) grad * phase;
}

static inline double simplex_inline_calc(double phase_increment, double* phase_data)
{
    double x = (*phase_data) + phase_increment;
    int64_t i0 = x;
    int64_t i1 = i0 + 1;
    double x0 = x - (double) i0;
    double x1 = x0 - 1.0;
    double t0 = 1.0 - x0 * x0;
    t0 *= t0;
    double n0 = t0 * t0 * simplex_grad1(simplex_noise_perm[i0 & 0xff], x0);
    double t1 = 1.0 - x1 * x1;
    t1 *= t1;
    double n1 = t1 * t1 * simplex_grad1(simplex_noise_perm[i1 & 0xff], x1);
    double y = 0.395 * (n0 + n1); // put into -1 to 1 range
    *phase_data = x;
    return y;
}

#define SIMPLEX_CALC(CONTROL_ARGS, AUDIO_ARGS)                          \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                  \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                 \
double freq, phase_increment;                                           \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
    UGEN_OUT(out, simplex_inline_calc(phase_increment, u.data));        \
);

#define SIMPLEX_PHASE_INCREMENT phase_increment = freq * RECIP_SAMPLE_RATE;
#define SIMPLEX_FREQK freq = *in0; SIMPLEX_PHASE_INCREMENT
#define SIMPLEX_FREQA freq = UGEN_IN(in0); SIMPLEX_PHASE_INCREMENT

// 0
void simplex_k_calc(ugen u)
{
    SIMPLEX_CALC(
        // Control Arguments
        SIMPLEX_FREQK         /* 0 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void simplex_a_calc(ugen u)
{
    SIMPLEX_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        SIMPLEX_FREQA         /* 0 */
    )
}
