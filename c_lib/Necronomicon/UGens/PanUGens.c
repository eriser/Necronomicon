/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <stdlib.h>
#include <math.h>

#include "../AudioRuntime.h"
#include "Util.h"

// pan (stereo)

#define PAN_CALC(CONTROL_ARGS, AUDIO_ARGS)          \
double* in0 = UGEN_INPUT_BUFFER(u, 0);              \
double* in1 = UGEN_INPUT_BUFFER(u, 1);              \
double* out0 = UGEN_OUTPUT_BUFFER(u, 0);            \
double* out1 = UGEN_OUTPUT_BUFFER(u, 1);            \
double x, delta, pos, ampL, ampR;                   \
uint32_t index1, index2;                            \
CONTROL_ARGS                                        \
AUDIO_LOOP(                                         \
    AUDIO_ARGS                                      \
    UGEN_OUT(out0, x * ampL);                       \
    UGEN_OUT(out1, x * ampR);                       \
);

/* range is assumed to be -1 to 1 */
#define PAN_CALC_LR_AMPS                            \
pos = (pos + 1) * (double) PAN_QUATER_TABLE_SIZE;   \
index1 = pos;                                       \
index2 = pos + PAN_HALF_TABLE_SIZE;                 \
ampL = pan_table[index1 & PAN_TABLE_SIZE_MASK];     \
ampR = pan_table[index2 & PAN_TABLE_SIZE_MASK];

#define PAN_POSK pos = *in0; PAN_CALC_LR_AMPS
#define PAN_POSA pos = UGEN_IN(in0); PAN_CALC_LR_AMPS
#define PAN_XK x = *in1;
#define PAN_XA x = UGEN_IN(in1);

// 0
void pan_kk_calc(ugen u)
{
    PAN_CALC(
        // Control Arguments
        PAN_POSK              /* 0 */
        PAN_XK                /* 1 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void pan_ak_calc(ugen u)
{
    PAN_CALC(
        // Control Arguments
        PAN_XK                /* 1 */,
        // Audio Arguments
        PAN_POSA              /* 0 */
    )
}

// 2
void pan_ka_calc(ugen u)
{
    PAN_CALC(
        // Control Arguments
        PAN_POSK              /* 0 */,
        // Audio Arguments
        PAN_XA                /* 1 */
    )
}

// 3
void pan_aa_calc(ugen u)
{
    PAN_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        PAN_POSA              /* 0 */
        PAN_XA                /* 1 */
    )
}
