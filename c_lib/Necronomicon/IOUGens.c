/*
  Necronomicon - Deterministic Audio Engine
  Copyright 2014 - Chad McKinney and Curtis McKinney
*/

#include <stdlib.h>
#include <math.h>

#include "../Necronomicon.h"
#include "UGenUtil.h"

// localOut

void local_out_k_calc(ugen u)
{
    double in = *UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, in);
    );
}

void local_out_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, (UGEN_IN(in)));
    );
}

// out

#define OUT_CALC(CONTROL_CODE, AUDIO_CODE)                  \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                      \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                      \
double x;                                                   \
uint8_t bus_index; /* constrains bus range */               \
uint32_t bus_frame;                                         \
CONTROL_CODE                                                \
AUDIO_LOOP(                                                 \
    AUDIO_CODE                                              \
    _necronomicon_buses[bus_frame + _block_frame] += x;     \
);

void out_aa_calc(ugen u)
{
    OUT_CALC(
        /* NO CONTROL CODE */,
        bus_index = UGEN_IN(in0); // Audio rate code
        bus_frame = bus_index * BLOCK_SIZE;
        x = UGEN_IN(in1);
    )
}

void out_ak_calc(ugen u)
{
    OUT_CALC(
        x = in1[0];, // Control rate code
        bus_index = UGEN_IN(in0); // Audio rate code
        bus_frame = bus_index * BLOCK_SIZE;
    )
}

void out_ka_calc(ugen u)
{
    OUT_CALC(
        bus_index = in0[0]; // Control rate code
        bus_frame = bus_index * BLOCK_SIZE;,
        x = UGEN_IN(in1); // Audio rate code
    )
}

void out_kk_calc(ugen u)
{
    OUT_CALC(
        bus_index = in0[0]; // Control rate code
        bus_frame = bus_index * BLOCK_SIZE;
        x = in1[0];,
        /* no audio rate code */
    )
}

// in

#define IN_CALC(CONTROL_CODE, AUDIO_CODE)                                   \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                      \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                     \
uint8_t bus_index; /* constrains bus range */                               \
uint32_t bus_frame;                                                         \
CONTROL_CODE                                                                \
AUDIO_LOOP(                                                                 \
    AUDIO_CODE                                                              \
    UGEN_OUT(out, _necronomicon_buses[bus_frame + _block_frame]);           \
);                                                                          \

void in_a_calc(ugen u)
{
    IN_CALC(
        /* no control rate code */,
        bus_index = UGEN_IN(in0);
        bus_frame = bus_index * BLOCK_SIZE;
    );
}

void in_k_calc(ugen u)
{
    IN_CALC(
        bus_index = in0[0];
        bus_frame = bus_index * BLOCK_SIZE;,
        /* no audio rate code */
    );
}

// poll

void poll_constructor(ugen* u)
{
    uint32_t* count_buffer = (uint32_t*) malloc(sizeof(uint32_t));
    *count_buffer = 0;
    u->data = count_buffer;
}

void poll_calc(ugen u)
{
    uint32_t* count_buffer = (uint32_t*) u.data;
    uint32_t count = *count_buffer;
    message msg;
    msg.arg.number = 0;
    msg.type = PRINT_NUMBER;

    if (count >= ((double) SAMPLE_RATE * 0.25))
    {
        msg.arg.number = *UGEN_INPUT_BUFFER(u, 0); // Reads ugens as control rate
        NRT_FIFO_PUSH(msg);
        count = 0;
    }

    else
    {
        count += BLOCK_SIZE;
    }

    *count_buffer = count;
}

void poll_deconstructor(ugen* u)
{
    free(u->data);
}
