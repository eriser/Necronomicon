/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <stdlib.h>
#include <math.h>

#include "../Necronomicon.h"
#include "UGenUtil.h"

// timeMicros

void time_micros_calc(ugen u)
{
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    AUDIO_LOOP(
        UGEN_OUT(out, current_cycle_usecs + (jack_time_t) ((double) _block_frame * usecs_per_frame));
    );
}

// timeSecs

const double MICROS_PER_SECOND = 1000000;
void time_secs_calc(ugen u)
{
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    AUDIO_LOOP(
        UGEN_OUT(out, ((double) current_cycle_usecs + ((double) _block_frame * usecs_per_frame)) / MICROS_PER_SECOND);
    );
}
