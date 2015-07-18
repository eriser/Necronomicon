/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <stdlib.h>
#include <math.h>

#include "../AudioRuntime.h"
#include "Util.h"

#define RANGE_CALC(CONTROL_ARGS, AUDIO_ARGS)        \
double* in0  = UGEN_INPUT_BUFFER(u, 0);             \
double* in1  = UGEN_INPUT_BUFFER(u, 1);             \
double* in2  = UGEN_INPUT_BUFFER(u, 2);             \
double* out  = UGEN_OUTPUT_BUFFER(u, 0);            \
double  min, max;                                   \
double  mul, add;                                   \
double  x;                                          \
CONTROL_ARGS                                        \
AUDIO_LOOP(                                         \
    AUDIO_ARGS                                      \
    mul = (max - min) * 0.5;                        \
    add = mul + min;                                \
    UGEN_OUT(out, (x * mul) + add);                 \
);                                                  \


#define RANGE_MINK min = in0[0];
#define RANGE_MAXK max = in1[0];
#define RANGE_XK x = CLAMP(in2[0], -1, 1);

#define RANGE_MINA min = UGEN_IN(in0);
#define RANGE_MAXA max = UGEN_IN(in1);
#define RANGE_XA x = CLAMP(UGEN_IN(in2), -1, 1);

// 0
void range_kkk_calc(ugen u)
{
    double* in0  = UGEN_INPUT_BUFFER(u, 0);
    double* in1  = UGEN_INPUT_BUFFER(u, 1);
    double* in2  = UGEN_INPUT_BUFFER(u, 2);
    double* out  = UGEN_OUTPUT_BUFFER(u, 0);
    double  RANGE_MINK
    double  RANGE_MAXK
    double  RANGE_XK
    double  mul  = (max - min) * 0.5;
    double  add  = mul + min;
    *out         = (x * mul) + add;
}

// 1
void range_akk_calc(ugen u)
{
    RANGE_CALC(
        RANGE_MAXK // Control Args
        RANGE_XK,
        RANGE_MINA
    )
}

// 2
void range_kak_calc(ugen u)
{
    RANGE_CALC(
        RANGE_MINK // Control Args
        RANGE_XK, // Audio Args
        RANGE_MAXA
    )
}

// 3
void range_aak_calc(ugen u)
{
    RANGE_CALC(
        RANGE_XK, // Control Args
        RANGE_MINA // Audio Args
        RANGE_MAXA
    )
}

// 4
void range_kka_calc(ugen u)
{
    RANGE_CALC(
        RANGE_MINK // Control Args
        RANGE_MAXK, // Audio Args
        RANGE_XA
    )
}

// 5
void range_aka_calc(ugen u)
{
    RANGE_CALC(
        RANGE_MAXK, // Control Args
        RANGE_MINA // Audio Args
        RANGE_XA
    )
}

// 6
void range_kaa_calc(ugen u)
{
    RANGE_CALC(
        RANGE_MINK, // Control Args
        RANGE_MAXA // Audio Args
        RANGE_XA
    )
}

// 7
void range_aaa_calc(ugen u)
{
    RANGE_CALC(
        /* no control args */,
        RANGE_MINA // Audio Args
        RANGE_MAXA
        RANGE_XA
    )
}

#define EXPRANGE_CALC(CONTROL_ARGS, AUDIO_ARGS)                 \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                          \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                          \
double* in2 = UGEN_INPUT_BUFFER(u, 2);                          \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                         \
double  min, max;                                               \
double     x;                                                   \
CONTROL_ARGS                                                    \
AUDIO_LOOP(                                                     \
    AUDIO_ARGS                                                  \
    UGEN_OUT(out, pow(max / min, x * 0.5 + 0.5) * min);         \
);                                                              \

#define EXPRANGE_MINK min = in0[0];
#define EXPRANGE_MAXK max = in1[0];
#define EXPRANGE_XK x = CLAMP(in2[0], -1, 1);

#define EXPRANGE_MINA min = UGEN_IN(in0);
#define EXPRANGE_MAXA max = UGEN_IN(in1);
#define EXPRANGE_XA x = CLAMP(UGEN_IN(in2), -1, 1);

// 0
void exprange_kkk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* in2 = UGEN_INPUT_BUFFER(u, 2);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double  EXPRANGE_MINK;
    double  EXPRANGE_MAXK;
    double     EXPRANGE_XK;
    *out = pow(max / min, x * 0.5 + 0.5) * min;
}

// 1
void exprange_akk_calc(ugen u)
{
    EXPRANGE_CALC(
        EXPRANGE_MAXK
        EXPRANGE_XK,
        EXPRANGE_MINA
    )
}

// 2
void exprange_kak_calc(ugen u)
{
    EXPRANGE_CALC(
        EXPRANGE_MINK
        EXPRANGE_XK,
        EXPRANGE_MAXA
    )
}

// 3
void exprange_aak_calc(ugen u)
{
    EXPRANGE_CALC(
        EXPRANGE_XK,
        EXPRANGE_MINA
        EXPRANGE_MAXA
    )
}

// 4
void exprange_kka_calc(ugen u)
{
    EXPRANGE_CALC(
        EXPRANGE_MINK
        EXPRANGE_MAXK,
        EXPRANGE_XA
    )
}

// 5
void exprange_aka_calc(ugen u)
{
    EXPRANGE_CALC(
        EXPRANGE_MAXK,
        EXPRANGE_MINA
        EXPRANGE_XA
    )
}

// 6
void exprange_kaa_calc(ugen u)
{
    EXPRANGE_CALC(
        EXPRANGE_MINK,
        EXPRANGE_MAXA
        EXPRANGE_XA
    )
}

// 7
void exprange_aaa_calc(ugen u)
{
    EXPRANGE_CALC(
        /* no control args */,
        EXPRANGE_MINA
        EXPRANGE_MAXA
        EXPRANGE_XA
    )
}
