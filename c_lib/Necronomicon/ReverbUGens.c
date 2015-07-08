/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <math.h>
#include <stdlib.h>

#include "../Necronomicon.h"
#include "DelayUGens.h"
#include "UGenUtil.h"

typedef struct
{
    delay_data* combFilterDelays;
    double*     combz1s;
    delay_data* allpassDelays;
} freeverb_data;

void freeverb_constructor(ugen* u)
{
    freeverb_data* freeverb       = calloc(sizeof(freeverb_data), 1);
    freeverb->combFilterDelays    = calloc(sizeof(delay_data), 8);
    freeverb->combz1s             = calloc(sizeof(double), 8);
    freeverb->allpassDelays       = calloc(sizeof(delay_data), 4);

    delay_data data0 = { acquire_sample_buffer(1557), 1556, 0 };
    freeverb->combFilterDelays[0] = data0;
    delay_data data1 = { acquire_sample_buffer(1617), 1616, 0 };
    freeverb->combFilterDelays[1] = data1;
    delay_data data2 = { acquire_sample_buffer(1491), 1490, 0 };
    freeverb->combFilterDelays[2] = data2;
    delay_data data3 = { acquire_sample_buffer(1422), 1421, 0 };
    freeverb->combFilterDelays[3] = data3;
    delay_data data4 = { acquire_sample_buffer(1277), 1276, 0 };
    freeverb->combFilterDelays[4] = data4;
    delay_data data5 = { acquire_sample_buffer(1356), 1355, 0 };
    freeverb->combFilterDelays[5] = data5;
    delay_data data6 = { acquire_sample_buffer(1188), 1187, 0 };
    freeverb->combFilterDelays[6] = data6;
    delay_data data7 = { acquire_sample_buffer(1116), 1115, 0 };
    freeverb->combFilterDelays[7] = data7;

    delay_data data8 = { acquire_sample_buffer(225), 225, 0 };
    freeverb->allpassDelays[0]    = data8;
    delay_data data9 = { acquire_sample_buffer(556), 556, 0 };
    freeverb->allpassDelays[1]    = data9;
    delay_data data10 = { acquire_sample_buffer(441), 441, 0 };
    freeverb->allpassDelays[2]    = data10;
    delay_data data11 = { acquire_sample_buffer(341), 341, 0 };
    freeverb->allpassDelays[3]    = data11;

    u->data                       = freeverb;
}

void freeverb_deconstructor(ugen* u)
{
    freeverb_data* freeverb = (freeverb_data*) u->data;
    int32_t i;
    for(i=0;i<8;++i)
    {
        release_sample_buffer(freeverb->combFilterDelays[i].buffer);
        // free(freeverb->combz1s);
    }
    for(i=0;i<4;++i)
        release_sample_buffer(freeverb->allpassDelays[i].buffer);
    free(u->data);
}

const double fixed_gain = 0.015;

// Should the delayed signal in this be x or y or x + y???
#define COMB_FILTER(X,R,D,N,DATA,ZS,I)                                                                          \
({                                                                                                              \
    int64_t        write_index              = DATA[I].write_index;                                              \
    sample_buffer* buffer                   = DATA[I].buffer;                                                   \
    uint32_t          num_samples_mask         = buffer->num_samples_mask;                                      \
    double*        samples                  = buffer->samples;                                                  \
    double         damp1                    = D * 0.4;                                                          \
    double         damp2                    = 1 - damp1;                                                        \
    double         feedback                 = R * 0.28 + 0.7;                                                   \
    int64_t        read_index               = write_index - N;                                                  \
    double         y                        = read_index < 0 ? 0 : samples[read_index & num_samples_mask];      \
    ZS[I]                                   = y * damp2 + ZS[I] * damp1;                                        \
    samples[write_index & num_samples_mask] = (X * fixed_gain) + ZS[I] * feedback;                              \
    DATA[I].write_index                     = write_index + 1;                                                  \
    y;                                                                                                          \
})

#define ALLPASS_FEEDBACK(X,F,N,DATA,I)                                                                          \
({                                                                                                              \
    int64_t        write_index              = DATA[I].write_index;                                              \
    int64_t        read_index               = write_index - N;                                                  \
    sample_buffer* buffer                   = DATA[I].buffer;                                                   \
    double*        samples                  = buffer->samples;                                                  \
    uint32_t          num_samples_mask         = buffer->num_samples_mask;                                      \
    double         bufout                   = read_index < 0 ? 0 : samples[read_index & num_samples_mask];      \
    double         y                        = -X + bufout;                                                      \
    samples[write_index & num_samples_mask] = X + bufout * F;                                                   \
    DATA[I].write_index                     = write_index + 1;                                                  \
    y;                                                                                                          \
})


// NOTE: Optimize COMB_FILTER and ALLPASS_FEEDBACK by pulling declarations and ugen data access outside of audio loop
#define FREEVERB_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                     \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                                              \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                                                              \
double* in2 = UGEN_INPUT_BUFFER(u, 2);                                                              \
double* in3 = UGEN_INPUT_BUFFER(u, 3);                                                              \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                                             \
freeverb_data vdata = *((freeverb_data*) u.data);                                                   \
double mix;                                                                                         \
double roomSize;                                                                                    \
double damp;                                                                                        \
double x, y;                                                                                        \
double cf0, cf1, cf2, cf3, cf4, cf5, cf6, cf7, cfy;                                                 \
double ap0, ap1, ap2, ap3;                                                                          \
CONTROL_ARGS                                                                                        \
AUDIO_LOOP(                                                                                         \
    AUDIO_ARGS                                                                                      \
    cf0      = COMB_FILTER(x, roomSize, damp, 1557, vdata.combFilterDelays, vdata.combz1s, 0);      \
    cf1      = COMB_FILTER(x, roomSize, damp, 1617, vdata.combFilterDelays, vdata.combz1s, 1);      \
    cf2      = COMB_FILTER(x, roomSize, damp, 1491, vdata.combFilterDelays, vdata.combz1s, 2);      \
    cf3      = COMB_FILTER(x, roomSize, damp, 1422, vdata.combFilterDelays, vdata.combz1s, 3);      \
    cf4      = COMB_FILTER(x, roomSize, damp, 1277, vdata.combFilterDelays, vdata.combz1s, 4);      \
    cf5      = COMB_FILTER(x, roomSize, damp, 1356, vdata.combFilterDelays, vdata.combz1s, 5);      \
    cf6      = COMB_FILTER(x, roomSize, damp, 1188, vdata.combFilterDelays, vdata.combz1s, 6);      \
    cf7      = COMB_FILTER(x, roomSize, damp, 1116, vdata.combFilterDelays, vdata.combz1s, 7);      \
    cfy      = cf0 + cf1 + cf2 + cf3 + cf3 + cf5 + cf6 + cf7;                                       \
                                                                                                    \
    ap0      = ALLPASS_FEEDBACK(cfy, 0.5, 225, vdata.allpassDelays, 0);                             \
    ap1      = ALLPASS_FEEDBACK(ap0, 0.5, 556, vdata.allpassDelays, 1);                             \
    ap2      = ALLPASS_FEEDBACK(ap1, 0.5, 441, vdata.allpassDelays, 2);                             \
    ap3      = ALLPASS_FEEDBACK(ap2, 0.5, 341, vdata.allpassDelays, 3);                             \
    y        = (x * (1 - mix)) + (ap3 * mix * 1.0);                                                 \
    UGEN_OUT(out, y);                                                                               \
);                                                                                                  \
*((freeverb_data*) u.data) = vdata;                                                                 \

#define FREEVERB_MIXK mix = CLAMP(*in0, 0, 1);
#define FREEVERB_MIXA mix = CLAMP(UGEN_IN(in0), 0, 1);
#define FREEVERB_ROOMSIZEK roomSize = CLAMP(*in1, 0, 1);
#define FREEVERB_ROOMSIZEA roomSize = CLAMP(UGEN_IN(in1), 0, 1);
#define FREEVERB_DAMPK damp = CLAMP(*in2, 0, 1);
#define FREEVERB_DAMPA damp = CLAMP(UGEN_IN(in2), 0, 1);
#define FREEVERB_XK x = *in3;
#define FREEVERB_XA x = UGEN_IN(in3);


// 0
void freeverb_kkkk_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */
        FREEVERB_ROOMSIZEK    /* 1 */
        FREEVERB_DAMPK        /* 2 */
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void freeverb_akkk_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_ROOMSIZEK    /* 1 */
        FREEVERB_DAMPK        /* 2 */
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
    )
}

// 2
void freeverb_kakk_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */
        FREEVERB_DAMPK        /* 2 */
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        FREEVERB_ROOMSIZEA    /* 1 */
    )
}

// 3
void freeverb_aakk_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_DAMPK        /* 2 */
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
        FREEVERB_ROOMSIZEA    /* 1 */
    )
}

// 4
void freeverb_kkak_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */
        FREEVERB_ROOMSIZEK    /* 1 */
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        FREEVERB_DAMPA        /* 2 */
    )
}

// 5
void freeverb_akak_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_ROOMSIZEK    /* 1 */
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
        FREEVERB_DAMPA        /* 2 */
    )
}

// 6
void freeverb_kaak_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        FREEVERB_ROOMSIZEA    /* 1 */
        FREEVERB_DAMPA        /* 2 */
    )
}

// 7
void freeverb_aaak_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_XK           /* 3 */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
        FREEVERB_ROOMSIZEA    /* 1 */
        FREEVERB_DAMPA        /* 2 */
    )
}

// 8
void freeverb_kkka_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */
        FREEVERB_ROOMSIZEK    /* 1 */
        FREEVERB_DAMPK        /* 2 */,
        // Audio Arguments
        FREEVERB_XA           /* 3 */
    )
}

// 9
void freeverb_akka_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_ROOMSIZEK    /* 1 */
        FREEVERB_DAMPK        /* 2 */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
        FREEVERB_XA           /* 3 */
    )
}

// 10
void freeverb_kaka_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */
        FREEVERB_DAMPK        /* 2 */,
        // Audio Arguments
        FREEVERB_ROOMSIZEA    /* 1 */
        FREEVERB_XA           /* 3 */
    )
}

// 11
void freeverb_aaka_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_DAMPK        /* 2 */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
        FREEVERB_ROOMSIZEA    /* 1 */
        FREEVERB_XA           /* 3 */
    )
}

// 12
void freeverb_kkaa_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */
        FREEVERB_ROOMSIZEK    /* 1 */,
        // Audio Arguments
        FREEVERB_DAMPA        /* 2 */
        FREEVERB_XA           /* 3 */
    )
}

// 13
void freeverb_akaa_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_ROOMSIZEK    /* 1 */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
        FREEVERB_DAMPA        /* 2 */
        FREEVERB_XA           /* 3 */
    )
}

// 14
void freeverb_kaaa_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        FREEVERB_MIXK         /* 0 */,
        // Audio Arguments
        FREEVERB_ROOMSIZEA    /* 1 */
        FREEVERB_DAMPA        /* 2 */
        FREEVERB_XA           /* 3 */
    )
}

// 15
void freeverb_aaaa_calc(ugen u)
{
    FREEVERB_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        FREEVERB_MIXA         /* 0 */
        FREEVERB_ROOMSIZEA    /* 1 */
        FREEVERB_DAMPA        /* 2 */
        FREEVERB_XA           /* 3 */
    )
}
