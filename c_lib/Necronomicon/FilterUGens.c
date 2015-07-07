/*
  Necronomicon
  Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <stdlib.h>
#include <math.h>

#include "../Necronomicon.h"
#include "UGenUtil.h"

//===================================
// RBJ Filters, Audio EQ Cookbook
//===================================

typedef struct
{
    double x1;
    double x2;
    double y1;
    double y2;

    double prevF;
    double prevQ;
    double cs;
    double alpha;

} biquad_t;

void biquad_constructor(ugen* u)
{
    biquad_t* biquad = malloc(sizeof(biquad_t));
    biquad->x1       = 0;
    biquad->x2       = 0;
    biquad->y1       = 0;
    biquad->y2       = 0;
    biquad->prevF    = 0;
    biquad->prevQ    = 0;
    biquad->cs       = 0;
    biquad->alpha    = 0;
    u->data          = biquad;
}

void biquad_deconstructor(ugen* u)
{
    free(u->data);
}

#define BIQUAD(B0,B1,B2,A0,A1,A2,X,X1,X2,Y1,Y2) ( (B0/A0)*X + (B1/A0)*X1 + (B2/A0)*X2 - (A1/A0)*Y1 - (A2/A0)*Y2 )

#define TABLE_LOOKUP(VAL, TABLE)                                    \
({                                                                  \
    const double v1 = TABLE[((uint16_t) (VAL * TABLE_SIZE))];       \
    const double v2 = TABLE[((uint16_t) (VAL * TABLE_SIZE + 1))];   \
    const double delta  = VAL - ((uint16_t) VAL);                   \
    v1 + delta * (v2 - v1);                                         \
})

#define TABLE_SIN(x) (TABLE_LOOKUP(x, sine_table))
#define TABLE_COS(x) (TABLE_LOOKUP(x, cosn_table))
#define TABLE_SINH(x) (TABLE_LOOKUP(x, sinh_table))
#define TABLE_ATAN(x) (TABLE_LOOKUP(x, atan_table))
#define TABLE_TANH(x) (TABLE_LOOKUP(x, tanh_table))

#define LPF_CALC(CONTROL_ARGS, AUDIO_ARGS)                              \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double q;                                                               \
double in;                                                              \
                                                                        \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double alpha;                                                           \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
double snhi;                                                            \
                                                                        \
CONTROL_ARGS                                                            \
                                                                        \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
                                                                        \
    if (freq != bi.prevF || q != bi.prevQ)                              \
    {                                                                   \
        /* branchless max? */                                           \
        q     = MAX(q,0.00000001);                                      \
        bi.prevF = freq;                                                \
        bi.prevQ = q;                                                   \
        /* Don't recalc if unnecessary */                               \
        omega  = freq * RECIP_SAMPLE_RATE;                              \
        bi.cs  = TABLE_COS(omega);                                      \
        sn     = TABLE_SIN(omega);                                      \
        snhi   = (1 / (2 * q));                                         \
        snhi   = snhi * RECIP_TWO_PI;                                   \
        bi.alpha = TABLE_SINH(snhi);                                    \
        bi.alpha *= sn;                                                 \
    }                                                                   \
    cs    = bi.cs;                                                      \
    alpha = bi.alpha;                                                   \
                                                                        \
    b0    = (1 - cs) * 0.5;                                             \
    b1    =  1 - cs;                                                    \
    b2    = (1 - cs) * 0.5;                                             \
    a0    =  1 + alpha;                                                 \
    a1    = -2 * cs;                                                    \
    a2    =  1 - alpha;                                                 \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define LPF_FREQK freq = *in0;
#define LPF_FREQA freq = UGEN_IN(in0);
#define LPF_QK q = *in1;
#define LPF_QA q = UGEN_IN(in1);
#define LPF_INK in = *in2;
#define LPF_INA in = UGEN_IN(in2);


// 0
void lpf_kkk_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        LPF_FREQK             /* 0 */
        LPF_QK                /* 1 */
        LPF_INK               /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void lpf_akk_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        LPF_QK                /* 1 */
        LPF_INK               /* 2 */,
        // Audio Arguments
        LPF_FREQA             /* 0 */
    )
}

// 2
void lpf_kak_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        LPF_FREQK             /* 0 */
        LPF_INK               /* 2 */,
        // Audio Arguments
        LPF_QA                /* 1 */
    )
}

// 3
void lpf_aak_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        LPF_INK               /* 2 */,
        // Audio Arguments
        LPF_FREQA             /* 0 */
        LPF_QA                /* 1 */
    )
}

// 4
void lpf_kka_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        LPF_FREQK             /* 0 */
        LPF_QK                /* 1 */,
        // Audio Arguments
        LPF_INA               /* 2 */
    )
}

// 5
void lpf_aka_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        LPF_QK                /* 1 */,
        // Audio Arguments
        LPF_FREQA             /* 0 */
        LPF_INA               /* 2 */
    )
}

// 6
void lpf_kaa_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        LPF_FREQK             /* 0 */,
        // Audio Arguments
        LPF_QA                /* 1 */
        LPF_INA               /* 2 */
    )
}

// 7
void lpf_aaa_calc(ugen u)
{
    LPF_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        LPF_FREQA             /* 0 */
        LPF_QA                /* 1 */
        LPF_INA               /* 2 */
    )
}

#define HPF_CALC(CONTROL_ARGS, AUDIO_ARGS)                              \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double q;                                                               \
double in;                                                              \
                                                                        \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double alpha;                                                           \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
double sinh_i;                                                          \
                                                                        \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
                                                                        \
    omega = freq * TWO_PI_TIMES_RECIP_SAMPLE_RATE;                      \
    cs    = TABLE_COS(omega);                                           \
    sn    = TABLE_SIN(omega);                                           \
    alpha = sn * TABLE_SINH(1 / (2 * q));                               \
                                                                        \
    b0    = (1 + cs) * 0.5;                                             \
    b1    = -1 - cs;                                                    \
    b2    = (1 + cs) * 0.5;                                             \
    a0    =  1 + alpha;                                                 \
    a1    = -2*cs;                                                      \
    a2    =  1 - alpha;                                                 \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define HPF_FREQK freq = *in0;
#define HPF_FREQA freq = UGEN_IN(in0);
#define HPF_QK q = *in1;
#define HPF_QA q = UGEN_IN(in1);
#define HPF_INK in = *in2;
#define HPF_INA in = UGEN_IN(in2);


// 0
void hpf_kkk_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        HPF_FREQK             /* 0 */
        HPF_QK                /* 1 */
        HPF_INK               /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void hpf_akk_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        HPF_QK                /* 1 */
        HPF_INK               /* 2 */,
        // Audio Arguments
        HPF_FREQA             /* 0 */
    )
}

// 2
void hpf_kak_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        HPF_FREQK             /* 0 */
        HPF_INK               /* 2 */,
        // Audio Arguments
        HPF_QA                /* 1 */
    )
}

// 3
void hpf_aak_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        HPF_INK               /* 2 */,
        // Audio Arguments
        HPF_FREQA             /* 0 */
        HPF_QA                /* 1 */
    )
}

// 4
void hpf_kka_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        HPF_FREQK             /* 0 */
        HPF_QK                /* 1 */,
        // Audio Arguments
        HPF_INA               /* 2 */
    )
}

// 5
void hpf_aka_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        HPF_QK                /* 1 */,
        // Audio Arguments
        HPF_FREQA             /* 0 */
        HPF_INA               /* 2 */
    )
}

// 6
void hpf_kaa_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        HPF_FREQK             /* 0 */,
        // Audio Arguments
        HPF_QA                /* 1 */
        HPF_INA               /* 2 */
    )
}

// 7
void hpf_aaa_calc(ugen u)
{
    HPF_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        HPF_FREQA             /* 0 */
        HPF_QA                /* 1 */
        HPF_INA               /* 2 */
    )
}

#define BPF_CALC(CONTROL_ARGS, AUDIO_ARGS)                              \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double q;                                                               \
double in;                                                              \
                                                                        \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double alpha;                                                           \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
                                                                        \
    omega = freq * TWO_PI_TIMES_RECIP_SAMPLE_RATE;                      \
    cs    = TABLE_COS(omega);                                           \
    sn    = TABLE_SIN(omega);                                           \
    alpha = sn * sinh(1 / (2 * q));                                     \
                                                                        \
    b0    =  alpha;                                                     \
    b1    =  0;                                                         \
    b2    = -alpha;                                                     \
    a0    =  1 + alpha;                                                 \
    a1    = -2*cs;                                                      \
    a2    =  1 - alpha;                                                 \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define BPF_FREQK freq = *in0;
#define BPF_FREQA freq = UGEN_IN(in0);
#define BPF_QK q = *in1;
#define BPF_QA q = UGEN_IN(in1);
#define BPF_INK in = *in2;
#define BPF_INA in = UGEN_IN(in2);

// 0
void bpf_kkk_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        BPF_FREQK             /* 0 */
        BPF_QK                /* 1 */
        BPF_INK               /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void bpf_akk_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        BPF_QK                /* 1 */
        BPF_INK               /* 2 */,
        // Audio Arguments
        BPF_FREQA             /* 0 */
    )
}

// 2
void bpf_kak_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        BPF_FREQK             /* 0 */
        BPF_INK               /* 2 */,
        // Audio Arguments
        BPF_QA                /* 1 */
    )
}

// 3
void bpf_aak_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        BPF_INK               /* 2 */,
        // Audio Arguments
        BPF_FREQA             /* 0 */
        BPF_QA                /* 1 */
    )
}

// 4
void bpf_kka_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        BPF_FREQK             /* 0 */
        BPF_QK                /* 1 */,
        // Audio Arguments
        BPF_INA               /* 2 */
    )
}

// 5
void bpf_aka_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        BPF_QK                /* 1 */,
        // Audio Arguments
        BPF_FREQA             /* 0 */
        BPF_INA               /* 2 */
    )
}

// 6
void bpf_kaa_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        BPF_FREQK             /* 0 */,
        // Audio Arguments
        BPF_QA                /* 1 */
        BPF_INA               /* 2 */
    )
}

// 7
void bpf_aaa_calc(ugen u)
{
    BPF_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        BPF_FREQA             /* 0 */
        BPF_QA                /* 1 */
        BPF_INA               /* 2 */
    )
}

#define NOTCH_CALC(CONTROL_ARGS, AUDIO_ARGS)                            \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  in3  = UGEN_INPUT_BUFFER(u, 3);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double gain;                                                            \
double q;                                                               \
double in;                                                              \
                                                                        \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double alpha;                                                           \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
                                                                        \
    omega = freq * TWO_PI_TIMES_RECIP_SAMPLE_RATE;                      \
    cs    = TABLE_COS(omega);                                           \
    sn    = TABLE_SIN(omega);                                           \
    alpha = sn * sinh(1 / (2 * q));                                     \
                                                                        \
    b0    =  1;                                                         \
    b1    = -2*cs;                                                      \
    b2    =  1;                                                         \
    a0    =  1 + alpha;                                                 \
    a1    = -2*cs;                                                      \
    a2    =  1 - alpha;                                                 \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define NOTCH_FREQK freq = *in0;
#define NOTCH_FREQA freq = UGEN_IN(in0);
#define NOTCH_GAINK gain = *in1;
#define NOTCH_GAINA gain = UGEN_IN(in1);
#define NOTCH_QK q = *in2;
#define NOTCH_QA q = UGEN_IN(in2);
#define NOTCH_INK in = *in3;
#define NOTCH_INA in = UGEN_IN(in3);

// 0
void notch_kkkk_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */
        NOTCH_GAINK           /* 1 */
        NOTCH_QK              /* 2 */
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void notch_akkk_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_GAINK           /* 1 */
        NOTCH_QK              /* 2 */
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
    )
}

// 2
void notch_kakk_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */
        NOTCH_QK              /* 2 */
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        NOTCH_GAINA           /* 1 */
    )
}

// 3
void notch_aakk_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_QK              /* 2 */
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
        NOTCH_GAINA           /* 1 */
    )
}

// 4
void notch_kkak_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */
        NOTCH_GAINK           /* 1 */
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        NOTCH_QA              /* 2 */
    )
}

// 5
void notch_akak_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_GAINK           /* 1 */
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
        NOTCH_QA              /* 2 */
    )
}

// 6
void notch_kaak_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        NOTCH_GAINA           /* 1 */
        NOTCH_QA              /* 2 */
    )
}

// 7
void notch_aaak_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_INK             /* 3 */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
        NOTCH_GAINA           /* 1 */
        NOTCH_QA              /* 2 */
    )
}

// 8
void notch_kkka_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */
        NOTCH_GAINK           /* 1 */
        NOTCH_QK              /* 2 */,
        // Audio Arguments
        NOTCH_INA             /* 3 */
    )
}

// 9
void notch_akka_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_GAINK           /* 1 */
        NOTCH_QK              /* 2 */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
        NOTCH_INA             /* 3 */
    )
}

// 10
void notch_kaka_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */
        NOTCH_QK              /* 2 */,
        // Audio Arguments
        NOTCH_GAINA           /* 1 */
        NOTCH_INA             /* 3 */
    )
}

// 11
void notch_aaka_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_QK              /* 2 */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
        NOTCH_GAINA           /* 1 */
        NOTCH_INA             /* 3 */
    )
}

// 12
void notch_kkaa_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */
        NOTCH_GAINK           /* 1 */,
        // Audio Arguments
        NOTCH_QA              /* 2 */
        NOTCH_INA             /* 3 */
    )
}

// 13
void notch_akaa_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_GAINK           /* 1 */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
        NOTCH_QA              /* 2 */
        NOTCH_INA             /* 3 */
    )
}

// 14
void notch_kaaa_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        NOTCH_FREQK           /* 0 */,
        // Audio Arguments
        NOTCH_GAINA           /* 1 */
        NOTCH_QA              /* 2 */
        NOTCH_INA             /* 3 */
    )
}

// 15
void notch_aaaa_calc(ugen u)
{
    NOTCH_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        NOTCH_FREQA           /* 0 */
        NOTCH_GAINA           /* 1 */
        NOTCH_QA              /* 2 */
        NOTCH_INA             /* 3 */
    )
}

#define ALLPASS_CALC(CONTROL_ARGS, AUDIO_ARGS)                          \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double q;                                                               \
double in;                                                              \
                                                                        \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double alpha;                                                           \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
                                                                        \
    omega = freq * TWO_PI_TIMES_RECIP_SAMPLE_RATE;                      \
    cs    = TABLE_COS(omega);                                           \
    sn    = TABLE_SIN(omega);                                           \
    alpha = sn * sinh(1 / (2 * q));                                     \
                                                                        \
    b0    =   1 - alpha;                                                \
    b1    =  -2 * cs;                                                   \
    b2    =   1 + alpha;                                                \
    a0    =   1 + alpha;                                                \
    a1    =  -2 * cs;                                                   \
    a2    =   1 - alpha;                                                \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define ALLPASS_FREQK freq = *in0;
#define ALLPASS_FREQA freq = UGEN_IN(in0);
#define ALLPASS_QK q = *in1;
#define ALLPASS_QA q = UGEN_IN(in1);
#define ALLPASS_INK in = *in2;
#define ALLPASS_INA in = UGEN_IN(in2);

// 0
void allpass_kkk_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        ALLPASS_FREQK         /* 0 */
        ALLPASS_QK            /* 1 */
        ALLPASS_INK           /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void allpass_akk_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        ALLPASS_QK            /* 1 */
        ALLPASS_INK           /* 2 */,
        // Audio Arguments
        ALLPASS_FREQA         /* 0 */
    )
}

// 2
void allpass_kak_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        ALLPASS_FREQK         /* 0 */
        ALLPASS_INK           /* 2 */,
        // Audio Arguments
        ALLPASS_QA            /* 1 */
    )
}

// 3
void allpass_aak_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        ALLPASS_INK           /* 2 */,
        // Audio Arguments
        ALLPASS_FREQA         /* 0 */
        ALLPASS_QA            /* 1 */
    )
}

// 4
void allpass_kka_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        ALLPASS_FREQK         /* 0 */
        ALLPASS_QK            /* 1 */,
        // Audio Arguments
        ALLPASS_INA           /* 2 */
    )
}

// 5
void allpass_aka_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        ALLPASS_QK            /* 1 */,
        // Audio Arguments
        ALLPASS_FREQA         /* 0 */
        ALLPASS_INA           /* 2 */
    )
}

// 6
void allpass_kaa_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        ALLPASS_FREQK         /* 0 */,
        // Audio Arguments
        ALLPASS_QA            /* 1 */
        ALLPASS_INA           /* 2 */
    )
}

// 7
void allpass_aaa_calc(ugen u)
{
    ALLPASS_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        ALLPASS_FREQA         /* 0 */
        ALLPASS_QA            /* 1 */
        ALLPASS_INA           /* 2 */
    )
}


#define PEAKEQ_CALC(CONTROL_ARGS, AUDIO_ARGS)                           \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  in3  = UGEN_INPUT_BUFFER(u, 3);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double gain;                                                            \
double q;                                                               \
double in;                                                              \
                                                                        \
double a;                                                               \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double alpha;                                                           \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
                                                                        \
    a     = pow(10,(gain/40));                                          \
    omega = freq * TWO_PI_TIMES_RECIP_SAMPLE_RATE;                      \
    cs    = TABLE_COS(omega);                                           \
    sn    = TABLE_SIN(omega);                                           \
    alpha = sn * sinh(1 / (2 * q));                                     \
                                                                        \
    b0    =  1 + alpha*a;                                               \
    b1    = -2*cs;                                                      \
    b2    =  1 - alpha*a;                                               \
    a0    =  1 + alpha/a;                                               \
    a1    = -2*cs;                                                      \
    a2    =  1 - alpha/a;                                               \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define PEAKEQ_FREQK freq = *in0;
#define PEAKEQ_FREQA freq = UGEN_IN(in0);
#define PEAKEQ_GAINK gain = *in1;
#define PEAKEQ_GAINA gain = UGEN_IN(in1);
#define PEAKEQ_QK q = *in2;
#define PEAKEQ_QA q = UGEN_IN(in2);
#define PEAKEQ_INK in = *in3;
#define PEAKEQ_INA in = UGEN_IN(in3);


// 0
void peakEQ_kkkk_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */
        PEAKEQ_GAINK          /* 1 */
        PEAKEQ_QK             /* 2 */
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void peakEQ_akkk_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_GAINK          /* 1 */
        PEAKEQ_QK             /* 2 */
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
    )
}

// 2
void peakEQ_kakk_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */
        PEAKEQ_QK             /* 2 */
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        PEAKEQ_GAINA          /* 1 */
    )
}

// 3
void peakEQ_aakk_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_QK             /* 2 */
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
        PEAKEQ_GAINA          /* 1 */
    )
}

// 4
void peakEQ_kkak_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */
        PEAKEQ_GAINK          /* 1 */
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        PEAKEQ_QA             /* 2 */
    )
}

// 5
void peakEQ_akak_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_GAINK          /* 1 */
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
        PEAKEQ_QA             /* 2 */
    )
}

// 6
void peakEQ_kaak_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        PEAKEQ_GAINA          /* 1 */
        PEAKEQ_QA             /* 2 */
    )
}

// 7
void peakEQ_aaak_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_INK            /* 3 */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
        PEAKEQ_GAINA          /* 1 */
        PEAKEQ_QA             /* 2 */
    )
}

// 8
void peakEQ_kkka_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */
        PEAKEQ_GAINK          /* 1 */
        PEAKEQ_QK             /* 2 */,
        // Audio Arguments
        PEAKEQ_INA            /* 3 */
    )
}

// 9
void peakEQ_akka_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_GAINK          /* 1 */
        PEAKEQ_QK             /* 2 */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
        PEAKEQ_INA            /* 3 */
    )
}

// 10
void peakEQ_kaka_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */
        PEAKEQ_QK             /* 2 */,
        // Audio Arguments
        PEAKEQ_GAINA          /* 1 */
        PEAKEQ_INA            /* 3 */
    )
}

// 11
void peakEQ_aaka_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_QK             /* 2 */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
        PEAKEQ_GAINA          /* 1 */
        PEAKEQ_INA            /* 3 */
    )
}

// 12
void peakEQ_kkaa_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */
        PEAKEQ_GAINK          /* 1 */,
        // Audio Arguments
        PEAKEQ_QA             /* 2 */
        PEAKEQ_INA            /* 3 */
    )
}

// 13
void peakEQ_akaa_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_GAINK          /* 1 */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
        PEAKEQ_QA             /* 2 */
        PEAKEQ_INA            /* 3 */
    )
}

// 14
void peakEQ_kaaa_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        PEAKEQ_FREQK          /* 0 */,
        // Audio Arguments
        PEAKEQ_GAINA          /* 1 */
        PEAKEQ_QA             /* 2 */
        PEAKEQ_INA            /* 3 */
    )
}

// 15
void peakEQ_aaaa_calc(ugen u)
{
    PEAKEQ_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        PEAKEQ_FREQA          /* 0 */
        PEAKEQ_GAINA          /* 1 */
        PEAKEQ_QA             /* 2 */
        PEAKEQ_INA            /* 3 */
    )
}

#define LOWSHELF_CALC(CONTROL_ARGS, AUDIO_ARGS)                         \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  in3  = UGEN_INPUT_BUFFER(u, 3);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double gain;                                                            \
double slope;                                                           \
double in;                                                              \
                                                                        \
double a;                                                               \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double beta;                                                            \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
                                                                        \
AUDIO_LOOP(                                                             \
    freq  = UGEN_IN(in0);                                               \
    gain  = UGEN_IN(in1);                                               \
    slope = UGEN_IN(in2);                                               \
    in    = UGEN_IN(in3);                                               \
                                                                        \
    a     = pow(10,(gain/40));                                          \
    omega = freq * TWO_PI_TIMES_RECIP_SAMPLE_RATE;                      \
    cs    = TABLE_COS(omega);                                           \
    sn    = TABLE_SIN(omega);                                           \
    beta  = sqrt( (pow(a,2) + 1) / slope - pow((a-1),2) );              \
                                                                        \
    b0    =    a*( (a+1) - (a-1)*cs + beta*sn );                        \
    b1    =  2*a*( (a-1) - (a+1)*cs           );                        \
    b2    =    a*( (a+1) - (a-1)*cs - beta*sn );                        \
    a0    =        (a+1) + (a-1)*cs + beta*sn;                          \
    a1    =   -2*( (a-1) + (a+1)*cs           );                        \
    a2    =        (a+1) + (a-1)*cs - beta*sn;                          \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define LOWSHELF_FREQK freq = *in0;
#define LOWSHELF_FREQA freq = UGEN_IN(in0);
#define LOWSHELF_GAINK gain = *in1;
#define LOWSHELF_GAINA gain = UGEN_IN(in1);
#define LOWSHELF_SLOPEK slope = *in2;
#define LOWSHELF_SLOPEA slope = UGEN_IN(in2);
#define LOWSHELF_INK in = *in3;
#define LOWSHELF_INA in = UGEN_IN(in3);

// 0
void lowshelf_kkkk_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */
        LOWSHELF_GAINK        /* 1 */
        LOWSHELF_SLOPEK       /* 2 */
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void lowshelf_akkk_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_GAINK        /* 1 */
        LOWSHELF_SLOPEK       /* 2 */
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
    )
}

// 2
void lowshelf_kakk_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */
        LOWSHELF_SLOPEK       /* 2 */
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        LOWSHELF_GAINA        /* 1 */
    )
}

// 3
void lowshelf_aakk_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_SLOPEK       /* 2 */
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
        LOWSHELF_GAINA        /* 1 */
    )
}

// 4
void lowshelf_kkak_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */
        LOWSHELF_GAINK        /* 1 */
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        LOWSHELF_SLOPEA       /* 2 */
    )
}

// 5
void lowshelf_akak_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_GAINK        /* 1 */
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
        LOWSHELF_SLOPEA       /* 2 */
    )
}

// 6
void lowshelf_kaak_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        LOWSHELF_GAINA        /* 1 */
        LOWSHELF_SLOPEA       /* 2 */
    )
}

// 7
void lowshelf_aaak_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_INK          /* 3 */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
        LOWSHELF_GAINA        /* 1 */
        LOWSHELF_SLOPEA       /* 2 */
    )
}

// 8
void lowshelf_kkka_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */
        LOWSHELF_GAINK        /* 1 */
        LOWSHELF_SLOPEK       /* 2 */,
        // Audio Arguments
        LOWSHELF_INA          /* 3 */
    )
}

// 9
void lowshelf_akka_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_GAINK        /* 1 */
        LOWSHELF_SLOPEK       /* 2 */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
        LOWSHELF_INA          /* 3 */
    )
}

// 10
void lowshelf_kaka_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */
        LOWSHELF_SLOPEK       /* 2 */,
        // Audio Arguments
        LOWSHELF_GAINA        /* 1 */
        LOWSHELF_INA          /* 3 */
    )
}

// 11
void lowshelf_aaka_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_SLOPEK       /* 2 */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
        LOWSHELF_GAINA        /* 1 */
        LOWSHELF_INA          /* 3 */
    )
}

// 12
void lowshelf_kkaa_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */
        LOWSHELF_GAINK        /* 1 */,
        // Audio Arguments
        LOWSHELF_SLOPEA       /* 2 */
        LOWSHELF_INA          /* 3 */
    )
}

// 13
void lowshelf_akaa_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_GAINK        /* 1 */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
        LOWSHELF_SLOPEA       /* 2 */
        LOWSHELF_INA          /* 3 */
    )
}

// 14
void lowshelf_kaaa_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        LOWSHELF_FREQK        /* 0 */,
        // Audio Arguments
        LOWSHELF_GAINA        /* 1 */
        LOWSHELF_SLOPEA       /* 2 */
        LOWSHELF_INA          /* 3 */
    )
}

// 15
void lowshelf_aaaa_calc(ugen u)
{
    LOWSHELF_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        LOWSHELF_FREQA        /* 0 */
        LOWSHELF_GAINA        /* 1 */
        LOWSHELF_SLOPEA       /* 2 */
        LOWSHELF_INA          /* 3 */
    )
}

#define HIGHSHELF_CALC(CONTROL_ARGS, AUDIO_ARGS)                        \
double*  in0  = UGEN_INPUT_BUFFER(u, 0);                                \
double*  in1  = UGEN_INPUT_BUFFER(u, 1);                                \
double*  in2  = UGEN_INPUT_BUFFER(u, 2);                                \
double*  in3  = UGEN_INPUT_BUFFER(u, 3);                                \
double*  out  = UGEN_OUTPUT_BUFFER(u, 0);                               \
biquad_t bi   = *((biquad_t*) u.data);                                  \
                                                                        \
double freq;                                                            \
double gain;                                                            \
double slope;                                                           \
double in;                                                              \
                                                                        \
double a;                                                               \
double omega;                                                           \
double cs;                                                              \
double sn;                                                              \
double beta;                                                            \
                                                                        \
double b0;                                                              \
double b1;                                                              \
double b2;                                                              \
double a0;                                                              \
double a1;                                                              \
double a2;                                                              \
                                                                        \
double y;                                                               \
CONTROL_ARGS                                                            \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
                                                                        \
    a     = pow(10,(gain/40));                                          \
    omega = freq * TWO_PI_TIMES_RECIP_SAMPLE_RATE;                      \
    cs    = TABLE_COS(omega);                                           \
    sn    = TABLE_SIN(omega);                                           \
    beta  = sqrt( (pow(a,2) + 1) / slope - pow((a-1),2) );              \
                                                                        \
    b0    =    a*( (a+1) + (a-1)*cs + beta*sn );                        \
    b1    = -2*a*( (a-1) + (a+1)*cs           );                        \
    b2    =    a*( (a+1) + (a-1)*cs - beta*sn );                        \
    a0    =        (a+1) - (a-1)*cs + beta*sn;                          \
    a1    =    2*( (a-1) - (a+1)*cs           );                        \
    a2    =        (a+1) - (a-1)*cs - beta*sn;                          \
                                                                        \
    y     = BIQUAD(b0,b1,b2,a0,a1,a2,in,bi.x1,bi.x2,bi.y1,bi.y2);       \
                                                                        \
    bi.y2 = bi.y1;                                                      \
    bi.y1 = y;                                                          \
    bi.x2 = bi.x1;                                                      \
    bi.x1 = in;                                                         \
                                                                        \
    UGEN_OUT(out,y);                                                    \
);                                                                      \
                                                                        \
*((biquad_t*) u.data) = bi;                                             \

#define HIGHSHELF_FREQK freq = *in0;
#define HIGHSHELF_FREQA freq = UGEN_IN(in0);
#define HIGHSHELF_GAINK gain = *in1;
#define HIGHSHELF_GAINA gain = UGEN_IN(in1);
#define HIGHSHELF_SLOPEK slope = *in2;
#define HIGHSHELF_SLOPEA slope = UGEN_IN(in2);
#define HIGHSHELF_INK in = *in3;
#define HIGHSHELF_INA in = UGEN_IN(in3);

// 0
void highshelf_kkkk_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */
        HIGHSHELF_GAINK       /* 1 */
        HIGHSHELF_SLOPEK      /* 2 */
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void highshelf_akkk_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_GAINK       /* 1 */
        HIGHSHELF_SLOPEK      /* 2 */
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
    )
}

// 2
void highshelf_kakk_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */
        HIGHSHELF_SLOPEK      /* 2 */
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        HIGHSHELF_GAINA       /* 1 */
    )
}

// 3
void highshelf_aakk_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_SLOPEK      /* 2 */
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
        HIGHSHELF_GAINA       /* 1 */
    )
}

// 4
void highshelf_kkak_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */
        HIGHSHELF_GAINK       /* 1 */
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        HIGHSHELF_SLOPEA      /* 2 */
    )
}

// 5
void highshelf_akak_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_GAINK       /* 1 */
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
        HIGHSHELF_SLOPEA      /* 2 */
    )
}

// 6
void highshelf_kaak_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        HIGHSHELF_GAINA       /* 1 */
        HIGHSHELF_SLOPEA      /* 2 */
    )
}

// 7
void highshelf_aaak_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_INK         /* 3 */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
        HIGHSHELF_GAINA       /* 1 */
        HIGHSHELF_SLOPEA      /* 2 */
    )
}

// 8
void highshelf_kkka_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */
        HIGHSHELF_GAINK       /* 1 */
        HIGHSHELF_SLOPEK      /* 2 */,
        // Audio Arguments
        HIGHSHELF_INA         /* 3 */
    )
}

// 9
void highshelf_akka_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_GAINK       /* 1 */
        HIGHSHELF_SLOPEK      /* 2 */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
        HIGHSHELF_INA         /* 3 */
    )
}

// 10
void highshelf_kaka_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */
        HIGHSHELF_SLOPEK      /* 2 */,
        // Audio Arguments
        HIGHSHELF_GAINA       /* 1 */
        HIGHSHELF_INA         /* 3 */
    )
}

// 11
void highshelf_aaka_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_SLOPEK      /* 2 */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
        HIGHSHELF_GAINA       /* 1 */
        HIGHSHELF_INA         /* 3 */
    )
}

// 12
void highshelf_kkaa_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */
        HIGHSHELF_GAINK       /* 1 */,
        // Audio Arguments
        HIGHSHELF_SLOPEA      /* 2 */
        HIGHSHELF_INA         /* 3 */
    )
}

// 13
void highshelf_akaa_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_GAINK       /* 1 */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
        HIGHSHELF_SLOPEA      /* 2 */
        HIGHSHELF_INA         /* 3 */
    )
}

// 14
void highshelf_kaaa_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        HIGHSHELF_FREQK       /* 0 */,
        // Audio Arguments
        HIGHSHELF_GAINA       /* 1 */
        HIGHSHELF_SLOPEA      /* 2 */
        HIGHSHELF_INA         /* 3 */
    )
}

// 15
void highshelf_aaaa_calc(ugen u)
{
    HIGHSHELF_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        HIGHSHELF_FREQA       /* 0 */
        HIGHSHELF_GAINA       /* 1 */
        HIGHSHELF_SLOPEA      /* 2 */
        HIGHSHELF_INA         /* 3 */
    )
}

#define LAG_CALC(CONTROL_ARGS, AUDIO_ARGS)                      \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);                         \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);                         \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);                        \
double   z   = *((double*) u.data);                             \
                                                                \
double lagTime;                                                 \
double input;                                                   \
double a;                                                       \
double b;                                                       \
CONTROL_ARGS                                                    \
AUDIO_LOOP(                                                     \
    AUDIO_ARGS                                                  \
    a       = exp((-2 * M_PI) / (lagTime * SAMPLE_RATE));       \
    b       = 1.0f - a;                                         \
    z       = (input * b) + (z * a);                            \
                                                                \
    UGEN_OUT(out,z);                                            \
);                                                              \
                                                                \
*((double*) u.data) = z;                                        \

#define LAG_LAGTIMEK lagTime = *in0;
#define LAG_LAGTIMEA lagTime = UGEN_IN(in0);
#define LAG_INPUTK input = *in1;
#define LAG_INPUTA input = UGEN_IN(in1);

// 0
void lag_kk_calc(ugen u)
{
    LAG_CALC(
        // Control Arguments
        LAG_LAGTIMEK          /* 0 */
        LAG_INPUTK            /* 1 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void lag_ak_calc(ugen u)
{
    LAG_CALC(
        // Control Arguments
        LAG_INPUTK            /* 1 */,
        // Audio Arguments
        LAG_LAGTIMEA          /* 0 */
    )
}

// 2
void lag_ka_calc(ugen u)
{
    LAG_CALC(
        // Control Arguments
        LAG_LAGTIMEK          /* 0 */,
        // Audio Arguments
        LAG_INPUTA            /* 1 */
    )
}

// 3
void lag_aa_calc(ugen u)
{
    LAG_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        LAG_LAGTIMEA          /* 0 */
        LAG_INPUTA            /* 1 */
    )
}

//REMOVING THESE FOR NOW AS THEY ARE NON-FUNCTIONAL AT THE MOMENT ANYWAYS
/*
//=====================
// Zero delay filters
//=====================

typedef struct
{
    double* ss;//Delayed Samples for Filtering
} zeroDelayFilter_t;

void zeroDelayFilter_constructor(ugen* u)
{
    zeroDelayFilter_t* zerodft = malloc(sizeof(zeroDelayFilter_t));
    zerodft->ss                = malloc(sizeof(double) * 3);
    zerodft->ss[0]             = 0;
    zerodft->ss[1]             = 0;
    zerodft->ss[2]             = 0;
    u->data                    = zerodft;
}

void zeroDelayFilter_deconstructor(ugen* u)
{
    zeroDelayFilter_t* zerodft = (zeroDelayFilter_t*) u->data;
    free(zerodft->ss);
    free(zerodft);
}

#define PREWARP(F,SI) ((2 / SI) * tan((F * SI) / 2))

#define ZERO_DELAY(X,G,S)  (X * G + (X * G + S))

inline double zERO_DELAY_ONE_POLE(double X,double G,double* SS,int32_tI)
{
    double Y   = X * G + SS[I];
    double XmY = X - Y;
    SS[I]      = ZERO_DELAY(XmY,G,SS[I]);
    return Y;
}

//Consider different shaping functions

void zeroDelayOnePole_calc(ugen u)
{
    double freq                = UGEN_IN(u,0);
    double x                   = UGEN_IN(u,1);
    zeroDelayFilter_t* zerodft = (zeroDelayFilter_t*)u.data;

    double warped              = PREWARP(freq,RECIP_SAMPLE_RATE);
    double g                   = warped / (warped + 1);
    double y                   = zERO_DELAY_ONE_POLE(x,g,zerodft->ss,0);

    UGEN_OUT(u,0,y);
}

//Add wave shaper once this is confirmed to work.
//Consider adding a base level amount of noise (a small amount).
void zeroDelayLPMS20_calc(ugen u)
{
    double freq                = UGEN_IN(u,0);
    double resonance           = UGEN_IN(u,1);
    double distortion          = UGEN_IN(u,2);
    double x                   = UGEN_IN(u,3);
    zeroDelayFilter_t* zerodft = (zeroDelayFilter_t*)u.data;

    double warped              = PREWARP(freq,RECIP_SAMPLE_RATE);
    double g                   = warped / (warped + 1);
    double k                   = 2 * resonance;
    double s1                  = zerodft->ss[0];
    double s2                  = zerodft->ss[1];

    double gSqr                = g*g;
    double y                   = (((gSqr * x) + (g * s1)) + s2) * ((gSqr * k) - (g * k) + 1);
    // double ky                  = SOFT_CLIP(k * y,distortion);
    double ky                  = k * y;

    double y1                  = zERO_DELAY_ONE_POLE(x  - ky,g,zerodft->ss,0);
    double y2                  = zERO_DELAY_ONE_POLE(y1 + ky,g,zerodft->ss,1);

    UGEN_OUT(u,0,y);
}
*/
