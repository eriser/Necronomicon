/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <stdlib.h>
#include <math.h>

#include "../Necronomicon.h"
#include "UGenUtil.h"

#define BIN_OP_CALC(OP, CONTROL_ARGS, AUDIO_ARGS)   \
double* in0 = UGEN_INPUT_BUFFER(u, 0);              \
double* in1 = UGEN_INPUT_BUFFER(u, 1);              \
double* out = UGEN_OUTPUT_BUFFER(u, 0);             \
double a,b;                                         \
CONTROL_ARGS                                        \
AUDIO_LOOP(                                         \
    AUDIO_ARGS                                      \
    UGEN_OUT(out, a OP b);                          \
);

#define BIN_FUNC_CALC(FUNC, CONTROL_ARGS, AUDIO_ARGS)   \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                  \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                  \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                 \
double a,b;                                             \
CONTROL_ARGS                                            \
AUDIO_LOOP(                                             \
    AUDIO_ARGS                                          \
    UGEN_OUT(out, FUNC(a, b));                          \
);

void add_aa_calc(ugen u)
{
    BIN_OP_CALC(
        +,
        /* no control args */,
        a = UGEN_IN(in0); // Audio args
        b = UGEN_IN(in1);
    );
}

void add_ak_calc(ugen u)
{
    BIN_OP_CALC(
        +,
        b = in1[0];, // Control arg
        a = UGEN_IN(in0); // Audio arg
    );
}

void add_ka_calc(ugen u)
{
    BIN_OP_CALC(
        +,
        a = in0[0];, // Control arg
        b = UGEN_IN(in1); // Audio arg
    );
}

void add_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double a = in0[0];
    double b = in1[0];
    *out = a + b;
}

void minus_aa_calc(ugen u)
{
    BIN_OP_CALC(
        -,
        /* no control args */,
        a = UGEN_IN(in0); // Audio args
        b = UGEN_IN(in1);
    );
}

void minus_ak_calc(ugen u)
{
    BIN_OP_CALC(
        -,
        b = in1[0];, // Control arg
        a = UGEN_IN(in0); // Audio arg
    );
}

void minus_ka_calc(ugen u)
{
    BIN_OP_CALC(
        -,
        a = in0[0];, // Control arg
        b = UGEN_IN(in1); // Audio arg
    );
}

void minus_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double a = in0[0];
    double b = in1[0];
    *out = a - b;
}

void mul_aa_calc(ugen u)
{
    BIN_OP_CALC(
        *,
        /* no control args */,
        a = UGEN_IN(in0); // Audio args
        b = UGEN_IN(in1);
    );
}

void mul_ak_calc(ugen u)
{
    BIN_OP_CALC(
        *,
        b = in1[0];, // Control arg
        a = UGEN_IN(in0); // Audio arg
    );
}

void mul_ka_calc(ugen u)
{
    BIN_OP_CALC(
        *,
        a = in0[0];, // Control arg
        b = UGEN_IN(in1); // Audio arg
    );
}

void mul_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double a = in0[0];
    double b = in1[0];
    *out = a * b;
}

#define DIV_CALC(CONTROL_ARGS, AUDIO_ARGS)  \
double* in0 = UGEN_INPUT_BUFFER(u, 0);      \
double* in1 = UGEN_INPUT_BUFFER(u, 1);      \
double* out = UGEN_OUTPUT_BUFFER(u, 0);     \
double a,b;                                 \
CONTROL_ARGS                                \
AUDIO_LOOP(                                 \
    AUDIO_ARGS                              \
    if (b == 0)                             \
        UGEN_OUT(out, 0);                   \
    else                                    \
        UGEN_OUT(out, a / b);               \
);                                          \

void div_aa_calc(ugen u)
{
    DIV_CALC(
        /* no control args */,
        a = UGEN_IN(in0); // Audio args
        b = UGEN_IN(in1);
    );
}

void div_ka_calc(ugen u)
{
    DIV_CALC(
        a = in0[0];, // Control args
        b = UGEN_IN(in1); // Audio args
    );
}

void div_ak_calc(ugen u)
{
    DIV_CALC(
        b = in1[0];, // Control args
        a = UGEN_IN(in0); // Audio args
    );
}

void div_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double a = in0[0];
    double b = in1[0];
    *out = b != 0 ? (a / b) : 0;
}

void abs_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, fabs(UGEN_IN(in)));
    );
}

void abs_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = fabs(in[0]);
}

#define SIGNUM_CALC()   \
if (value > 0)          \
    value = 1;          \
else if (value < 0)     \
    value = -1;         \
else                    \
    value = 0;          \

void signum_a_calc(ugen u)
{
    double* in  = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double value;
    AUDIO_LOOP(
        value = UGEN_IN(in);
        SIGNUM_CALC();
        UGEN_OUT(out, value);
    );
}

void signum_k_calc(ugen u)
{
    double* in  = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    double value = in[0];
    SIGNUM_CALC();
    *out = value;
}

void negate_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, -(UGEN_IN(in)));
    );
}

void negate_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = -in[0];
}

void pow_aa_calc(ugen u)
{
    BIN_FUNC_CALC(
        pow,
        /* no control args */,
        a = UGEN_IN(in0);
        b = UGEN_IN(in1); // Audio args
    );
}

void pow_ak_calc(ugen u)
{
    BIN_FUNC_CALC(
        pow,
        b = in1[0];, // Control args
        a = UGEN_IN(in0); // Audio args
    );
}

void pow_ka_calc(ugen u)
{
    BIN_FUNC_CALC(
        pow,
        a = in0[0];, // Control args
        b = UGEN_IN(in1); // Audio args
    );
}

void pow_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = pow(in0[0], in1[0]);
}

void exp_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, exp(UGEN_IN(in)));
    );
}

void exp_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = exp(in[0]);
}

void log_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, log(UGEN_IN(in)));
    );
}

void log_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = log(in[0]);
}

void cos_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, cos(UGEN_IN(in)));
    );
}

void cos_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = cos(in[0]);
}

void asin_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, asin(UGEN_IN(in)));
    );
}

void asin_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = asin(in[0]);
}

void acos_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, acos(UGEN_IN(in)));
    );
}

void acos_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = acos(in[0]);
}

void atan_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, atan(UGEN_IN(in)));
    );
}

void atan_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = atan(in[0]);
}

void logbase_aa_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, log(UGEN_IN(in0)) / log(UGEN_IN(in1)));
    );
}

void logbase_ka_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    const double a = log(in0[0]);
    AUDIO_LOOP(
        UGEN_OUT(out, a / log(UGEN_IN(in1)));
    );
}

void logbase_ak_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    const double b = log(in1[0]);
    AUDIO_LOOP(
        UGEN_OUT(out, log(UGEN_IN(in0)) / b);
    );
}

void logbase_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    const double a = log(in0[0]);
    const double b = log(in1[0]);
    *out = a / b;
}

void sqrt_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, sqrt(UGEN_IN(in)));
    );
}

void sqrt_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = sqrt(in[0]);
}

void tan_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, tan(UGEN_IN(in)));
    );
}

void tan_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = tan(in[0]);
}

void sinh_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, sinh(UGEN_IN(in)));
    );
}

void sinh_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = sinh(in[0]);
}

void cosh_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, cosh(UGEN_IN(in)));
    );
}

void cosh_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = cosh(in[0]);
}

void tanh_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, tanh(UGEN_IN(in)));
    );
}

void tanh_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = tanh(in[0]);
}

void asinh_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, asinh(UGEN_IN(in)));
    );
}

void asinh_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = asinh(in[0]);
}

void atanh_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, atanh(UGEN_IN(in)));
    );
}

void atanh_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = atanh(in[0]);
}

void acosh_a_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);

    AUDIO_LOOP(
        UGEN_OUT(out, acosh(UGEN_IN(in)));
    );
}

void acosh_k_calc(ugen u)
{
    double* in = UGEN_INPUT_BUFFER(u, 0);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = acosh(in[0]);
}

// max

#define UMAX_CALC(CONTROL_ARGS, AUDIO_ARGS)     \
double* in0 = UGEN_INPUT_BUFFER(u, 0);          \
double* in1 = UGEN_INPUT_BUFFER(u, 1);          \
double* out = UGEN_OUTPUT_BUFFER(u, 0);         \
double a;                                       \
double b;                                       \
CONTROL_ARGS                                    \
AUDIO_LOOP(                                     \
    AUDIO_ARGS                                  \
    UGEN_OUT(out, fmax(a, b));                  \
);

#define UMAX_AK a = *in0;
#define UMAX_AA a = UGEN_IN(in0);
#define UMAX_BK b = *in1;
#define UMAX_BA b = UGEN_IN(in1);


// 0
__attribute__((flatten)) void umax_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = fmax(*in0, *in1);
}

// 1
__attribute__((flatten)) void umax_ak_calc(ugen u)
{
    UMAX_CALC(
        // Control Arguments
        UMAX_BK               /* 1 */,
        // Audio Arguments
        UMAX_AA               /* 0 */
    )
}

// 2
__attribute__((flatten)) void umax_ka_calc(ugen u)
{
    UMAX_CALC(
        // Control Arguments
        UMAX_AK               /* 0 */,
        // Audio Arguments
        UMAX_BA               /* 1 */
    )
}

// 3
__attribute__((flatten)) void umax_aa_calc(ugen u)
{
    UMAX_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        UMAX_AA               /* 0 */
        UMAX_BA               /* 1 */
    )
}

// min

#define UMIN_CALC(CONTROL_ARGS, AUDIO_ARGS)     \
double* in0 = UGEN_INPUT_BUFFER(u, 0);          \
double* in1 = UGEN_INPUT_BUFFER(u, 1);          \
double* out = UGEN_OUTPUT_BUFFER(u, 0);         \
double a;                                       \
double b;                                       \
CONTROL_ARGS                                    \
AUDIO_LOOP(                                     \
    AUDIO_ARGS                                  \
    UGEN_OUT(out, fmin(a, b));                  \
);


#define UMIN_AK a = *in0;
#define UMIN_AA a = UGEN_IN(in0);
#define UMIN_BK b = *in1;
#define UMIN_BA b = UGEN_IN(in1);


// 0
__attribute__((flatten)) void umin_kk_calc(ugen u)
{
    double* in0 = UGEN_INPUT_BUFFER(u, 0);
    double* in1 = UGEN_INPUT_BUFFER(u, 1);
    double* out = UGEN_OUTPUT_BUFFER(u, 0);
    *out = fmin(*in0, *in1);
}

// 1
__attribute__((flatten)) void umin_ak_calc(ugen u)
{
    UMIN_CALC(
        // Control Arguments
        UMIN_BK               /* 1 */,
        // Audio Arguments
        UMIN_AA               /* 0 */
    )
}

// 2
__attribute__((flatten)) void umin_ka_calc(ugen u)
{
    UMIN_CALC(
        // Control Arguments
        UMIN_AK               /* 0 */,
        // Audio Arguments
        UMIN_BA               /* 1 */
    )
}

// 3
__attribute__((flatten)) void umin_aa_calc(ugen u)
{
    UMIN_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        UMIN_AA               /* 0 */
        UMIN_BA               /* 1 */
    )
}

// line

void line_constructor(ugen* u)
{
    u->data = malloc(UINT_SIZE); // Line time
    *((uint32_t*) u->data) = 0;
}

void line_deconstructor(ugen* u)
{
    free(u->data);
}
