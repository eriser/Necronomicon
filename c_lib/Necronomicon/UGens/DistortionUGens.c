/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <stdlib.h>
#include <math.h>

#include "../AudioRuntime.h"
#include "Util.h"

#define SOFT_CLIP(XIN,AMOUNT)                                               \
({                                                                          \
    double X     = XIN * AMOUNT * 0.5;                                      \
    double v1    = atan_table[((uint16_t) (X * TABLE_SIZE))];               \
    double v2    = atan_table[((uint16_t) (X * TABLE_SIZE + 1))];           \
    double delta = X - ((int64_t) X);                                       \
    v1 + delta * (v2 - v1);                                                 \
})

#define TANH_DIST(XIN,AMOUNT)                                               \
({                                                                          \
    double X     = XIN * AMOUNT * 0.5;                                      \
    double v1    = tanh_table[((uint16_t) (X * TABLE_SIZE))];               \
    double v2    = tanh_table[((uint16_t) (X * TABLE_SIZE + 1))];           \
    double delta = X - ((int64_t) X);                                       \
    v1 + delta * (v2 - v1);                                                 \
})


#define SIN_DIST(XIN,AMOUNT)                                                \
({                                                                          \
    double X     = XIN * AMOUNT * 0.5;                                      \
    double v1    = sine_table[((uint16_t) (X * TABLE_SIZE))];               \
    double v2    = sine_table[((uint16_t) (X * TABLE_SIZE + 1))];           \
    double delta = X - ((int64_t) X);                                       \
    v1 + delta * (v2 - v1);                                                 \
})

#define HARD_CLIP(X,AMOUNT) (CLAMP(X*AMOUNT,-1.0,1.0))

#define POLY3_DIST(X,AMOUNT) (1.5 * X - 0.5 * pow(X,3))

#define CLIP_CALC(CONTROL_ARGS, AUDIO_ARGS)     \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);         \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);         \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);        \
double amount, input;                           \
CONTROL_ARGS                                    \
AUDIO_LOOP(                                     \
    AUDIO_ARGS                                  \
    UGEN_OUT(out,HARD_CLIP(input, amount));     \
);                                              \

#define CLIP_AMOUNTK amount = *in0;
#define CLIP_AMOUNTA amount = UGEN_IN(in0);
#define CLIP_INPUTK input = *in1;
#define CLIP_INPUTA input = UGEN_IN(in1);

// 0
void clip_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double  CLIP_AMOUNTK
    double  CLIP_INPUTK
    *out = HARD_CLIP(input, amount);
}

// 1
void clip_ak_calc(ugen u)
{
    CLIP_CALC(
        // Control Arguments
        CLIP_INPUTK           /* 1 */,
        // Audio Arguments
        CLIP_AMOUNTA          /* 0 */
    )
}

// 2
void clip_ka_calc(ugen u)
{
    CLIP_CALC(
        // Control Arguments
        CLIP_AMOUNTK          /* 0 */,
        // Audio Arguments
        CLIP_INPUTA           /* 1 */
    )
}

// 3
void clip_aa_calc(ugen u)
{
    CLIP_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        CLIP_AMOUNTA          /* 0 */
        CLIP_INPUTA           /* 1 */
    )
}

#define SOFTCLIP_CALC(CONTROL_ARGS, AUDIO_ARGS)     \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);             \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);             \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);            \
double amount, input;                               \
CONTROL_ARGS                                        \
AUDIO_LOOP(                                         \
    AUDIO_ARGS                                      \
    UGEN_OUT(out, SOFT_CLIP(input, amount));        \
);                                                  \

#define SOFTCLIP_AMOUNTK amount = *in0;
#define SOFTCLIP_AMOUNTA amount = UGEN_IN(in0);
#define SOFTCLIP_INPUTK input = *in1;
#define SOFTCLIP_INPUTA input = UGEN_IN(in1);

// 0
void softclip_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double  SOFTCLIP_AMOUNTK
    double  SOFTCLIP_INPUTK
    *out = SOFT_CLIP(input, amount);
}

// 1
void softclip_ak_calc(ugen u)
{
    SOFTCLIP_CALC(
        // Control Arguments
        SOFTCLIP_INPUTK       /* 1 */,
        // Audio Arguments
        SOFTCLIP_AMOUNTA      /* 0 */
    )
}

// 2
void softclip_ka_calc(ugen u)
{
    SOFTCLIP_CALC(
        // Control Arguments
        SOFTCLIP_AMOUNTK      /* 0 */,
        // Audio Arguments
        SOFTCLIP_INPUTA       /* 1 */
    )
}

// 3
void softclip_aa_calc(ugen u)
{
    SOFTCLIP_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        SOFTCLIP_AMOUNTA      /* 0 */
        SOFTCLIP_INPUTA       /* 1 */
    )
}

void poly3_calc(ugen u)
{
    double*  in0 = UGEN_INPUT_BUFFER(u, 0);
    double*  in1 = UGEN_INPUT_BUFFER(u, 1);
    double*  out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out,POLY3_DIST(UGEN_IN(in0),UGEN_IN(in1)));
    );
}

#define POLY3_CALC(CONTROL_ARGS, AUDIO_ARGS)            \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);                 \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);                 \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);                \
double amount, input;                                   \
CONTROL_ARGS                                            \
AUDIO_LOOP(                                             \
    AUDIO_ARGS                                          \
    UGEN_OUT(out, POLY3_DIST(input, amount));           \
);                                                      \

#define POLY3_AMOUNTK amount = *in0;
#define POLY3_AMOUNTA amount = UGEN_IN(in0);
#define POLY3_INPUTK input = *in1;
#define POLY3_INPUTA input = UGEN_IN(in1);

// 0
void poly3_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double  POLY3_AMOUNTK
    double  POLY3_INPUTK
    *out = POLY3_DIST(input, amount);
}

// 1
void poly3_ak_calc(ugen u)
{
    POLY3_CALC(
        // Control Arguments
        POLY3_INPUTK          /* 1 */,
        // Audio Arguments
        POLY3_AMOUNTA         /* 0 */
    )
}

// 2
void poly3_ka_calc(ugen u)
{
    POLY3_CALC(
        // Control Arguments
        POLY3_AMOUNTK         /* 0 */,
        // Audio Arguments
        POLY3_INPUTA          /* 1 */
    )
}

// 3
void poly3_aa_calc(ugen u)
{
    POLY3_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        POLY3_AMOUNTA         /* 0 */
        POLY3_INPUTA          /* 1 */
    )
}

void tanhdist_calc(ugen u)
{
    double*  in0 = UGEN_INPUT_BUFFER(u, 0);
    double*  in1 = UGEN_INPUT_BUFFER(u, 1);
    double*  out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out,TANH_DIST(UGEN_IN(in0),UGEN_IN(in1)));
    );

}

#define TANHDIST_CALC(CONTROL_ARGS, AUDIO_ARGS)         \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);                 \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);                 \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);                \
double amount, input;                                   \
CONTROL_ARGS                                            \
AUDIO_LOOP(                                             \
    AUDIO_ARGS                                          \
    UGEN_OUT(out, TANH_DIST(input, amount));            \
);                                                      \

#define TANHDIST_AMOUNTK amount = *in0;
#define TANHDIST_AMOUNTA amount = UGEN_IN(in0);
#define TANHDIST_INPUTK input = *in1;
#define TANHDIST_INPUTA input = UGEN_IN(in1);

// 0
void tanhDist_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double  TANHDIST_AMOUNTK
    double  TANHDIST_INPUTK
    *out = TANH_DIST(input, amount);
}

// 1
void tanhDist_ak_calc(ugen u)
{
    TANHDIST_CALC(
        // Control Arguments
        TANHDIST_INPUTK       /* 1 */,
        // Audio Arguments
        TANHDIST_AMOUNTA      /* 0 */
    )
}

// 2
void tanhDist_ka_calc(ugen u)
{
    TANHDIST_CALC(
        // Control Arguments
        TANHDIST_AMOUNTK      /* 0 */,
        // Audio Arguments
        TANHDIST_INPUTA       /* 1 */
    )
}

// 3
void tanhDist_aa_calc(ugen u)
{
    TANHDIST_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        TANHDIST_AMOUNTA      /* 0 */
        TANHDIST_INPUTA       /* 1 */
    )
}

void sinDist_calc(ugen u)
{
    double*  in0 = UGEN_INPUT_BUFFER(u, 0);
    double*  in1 = UGEN_INPUT_BUFFER(u, 1);
    double*  out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out,SIN_DIST(UGEN_IN(in0),UGEN_IN(in1)));
    );
}

#define SINDIST_CALC(CONTROL_ARGS, AUDIO_ARGS)          \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);                 \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);                 \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);                \
double amount, input;                                   \
CONTROL_ARGS                                            \
AUDIO_LOOP(                                             \
    AUDIO_ARGS                                          \
    UGEN_OUT(out, SIN_DIST(input, amount));             \
);                                                      \


#define SINDIST_AMOUNTK amount = *in0;
#define SINDIST_AMOUNTA amount = UGEN_IN(in0);
#define SINDIST_INPUTK input = *in1;
#define SINDIST_INPUTA input = UGEN_IN(in1);

// 0
void sinDist_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double  SINDIST_AMOUNTK
    double  SINDIST_INPUTK
    *out = SIN_DIST(input, amount);
}

// 1
void sinDist_ak_calc(ugen u)
{
    SINDIST_CALC(
        // Control Arguments
        SINDIST_INPUTK        /* 1 */,
        // Audio Arguments
        SINDIST_AMOUNTA       /* 0 */
    )
}

// 2
void sinDist_ka_calc(ugen u)
{
    SINDIST_CALC(
        // Control Arguments
        SINDIST_AMOUNTK       /* 0 */,
        // Audio Arguments
        SINDIST_INPUTA        /* 1 */
    )
}

// 3
void sinDist_aa_calc(ugen u)
{
    SINDIST_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        SINDIST_AMOUNTA       /* 0 */
        SINDIST_INPUTA        /* 1 */
    )
}

// wrap

#define WRAP_CALC(CONTROL_ARGS, AUDIO_ARGS)     \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);         \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);         \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);        \
double amount, input;                           \
CONTROL_ARGS                                    \
AUDIO_LOOP(                                     \
    AUDIO_ARGS                                  \
    UGEN_OUT(out, WRAP(input, amount));         \
);                                              \


#define WRAP_AMOUNTK amount = *in0;
#define WRAP_AMOUNTA amount = UGEN_IN(in0);
#define WRAP_INPUTK input = *in1;
#define WRAP_INPUTA input = UGEN_IN(in1);

// 0
void wrap_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double  WRAP_AMOUNTK
    double  WRAP_INPUTK
    *out = WRAP(input, amount);
}

// 1
void wrap_ak_calc(ugen u)
{
    WRAP_CALC(
        // Control Arguments
        WRAP_INPUTK           /* 1 */,
        // Audio Arguments
        WRAP_AMOUNTA          /* 0 */
    )
}

// 2
void wrap_ka_calc(ugen u)
{
    WRAP_CALC(
        // Control Arguments
        WRAP_AMOUNTK          /* 0 */,
        // Audio Arguments
        WRAP_INPUTA           /* 1 */
    )
}

// 3
void wrap_aa_calc(ugen u)
{
    WRAP_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        WRAP_AMOUNTA          /* 0 */
        WRAP_INPUTA           /* 1 */
    )
}

// crush

#define CRUSH_CALC(CONTROL_ARGS, AUDIO_ARGS)                    \
double*  in0 = UGEN_INPUT_BUFFER(u, 0);                         \
double*  in1 = UGEN_INPUT_BUFFER(u, 1);                         \
double*  out = UGEN_OUTPUT_BUFFER(u, 0);                        \
int32_t max;                                                    \
double x;                                                       \
union {                                                         \
    double d;                                                   \
    int32_t x[2];                                               \
} ud = { 0 };                                                   \
CONTROL_ARGS                                                    \
AUDIO_LOOP(                                                     \
    AUDIO_ARGS                                                  \
    UGEN_OUT(out, floor((x + 1.0) * max) / max - 1.0);          \
);                                                              \

#define CRUSH_DEPTHK max = fast_pow(ud, 2, *in0) - 1;
#define CRUSH_DEPTHA max = fast_pow(ud, 2, UGEN_IN(in0)) - 1;
#define CRUSH_XK x = *in1;
#define CRUSH_XA x = UGEN_IN(in1);

// 0
void crush_kk_calc(ugen u)
{
    CRUSH_CALC(
        // Control Arguments
        CRUSH_DEPTHK          /* 0 */
        CRUSH_XK              /* 1 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void crush_ak_calc(ugen u)
{
    CRUSH_CALC(
        // Control Arguments
        CRUSH_XK              /* 1 */,
        // Audio Arguments
        CRUSH_DEPTHA          /* 0 */
    )
}

// 2
void crush_ka_calc(ugen u)
{
    CRUSH_CALC(
        // Control Arguments
        CRUSH_DEPTHK          /* 0 */,
        // Audio Arguments
        CRUSH_XA              /* 1 */
    )
}

// 3
void crush_aa_calc(ugen u)
{
    CRUSH_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        CRUSH_DEPTHA          /* 0 */
        CRUSH_XA              /* 1 */
    )
}

// decimate

typedef struct
{
    double samples;
    double prev;
} decimate_t;

void decimate_constructor(ugen* u)
{
    decimate_t* decimate = malloc(sizeof(decimate_t));
    decimate->samples    = 0;
    decimate->prev       = 0;
    u->data              = decimate;
}

void decimate_deconstructor(ugen* u)
{
    free(u->data);
}

#define DECIMATE_CALC(CONTROL_ARGS, AUDIO_ARGS)                     \
double*    in0      = UGEN_INPUT_BUFFER(u, 0);                      \
double*    in1      = UGEN_INPUT_BUFFER(u, 1);                      \
double*    out      = UGEN_OUTPUT_BUFFER(u, 0);                     \
decimate_t decimate = *((decimate_t*) u.data);                      \
double     rate;                                                    \
double     x;                                                       \
double     y;                                                       \
CONTROL_ARGS                                                        \
AUDIO_LOOP(                                                         \
    AUDIO_ARGS                                                      \
    y = 0;                                                          \
                                                                    \
    if (decimate.samples + rate >= 1)                               \
    {                                                               \
        decimate.samples = fmod(decimate.samples + rate,1.0);       \
        decimate.prev    = x;                                       \
        y                = x;                                       \
    }                                                               \
    else                                                            \
    {                                                               \
        decimate.samples += rate;                                   \
        y = decimate.prev;                                          \
    }                                                               \
                                                                    \
    UGEN_OUT(out,y);                                                \
);                                                                  \
                                                                    \
*((decimate_t*) u.data) = decimate;                                 \

#define DECIMATE_RATEK rate = (*in0) * RECIP_SAMPLE_RATE;
#define DECIMATE_RATEA rate = UGEN_IN(in0) * RECIP_SAMPLE_RATE;
#define DECIMATE_XK x = *in1;
#define DECIMATE_XA x = UGEN_IN(in1);

// 0
void decimate_kk_calc(ugen u)
{
    DECIMATE_CALC(
        // Control Arguments
        DECIMATE_RATEK        /* 0 */
        DECIMATE_XK           /* 1 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void decimate_ak_calc(ugen u)
{
    DECIMATE_CALC(
        // Control Arguments
        DECIMATE_XK           /* 1 */,
        // Audio Arguments
        DECIMATE_RATEA        /* 0 */
    )
}

// 2
void decimate_ka_calc(ugen u)
{
    DECIMATE_CALC(
        // Control Arguments
        DECIMATE_RATEK        /* 0 */,
        // Audio Arguments
        DECIMATE_XA           /* 1 */
    )
}

// 3
void decimate_aa_calc(ugen u)
{
    DECIMATE_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        DECIMATE_RATEA        /* 0 */
        DECIMATE_XA           /* 1 */
    )
}
