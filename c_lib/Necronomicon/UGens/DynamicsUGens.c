/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <stdlib.h>
#include <math.h>

#include "../AudioRuntime.h"
#include "Util.h"

// Limiter

typedef struct
{
    sample_buffer* buffer;
    double lookahead;
    double envelope_out;
    int64_t write_index;
} limiter_data;

void limiter_constructor(ugen* u)
{
    u->data = (limiter_data*) malloc(sizeof(limiter_data));
    double lookahead = fmax(1, u->constructor_args[0] * SAMPLE_RATE);
    limiter_data data = { acquire_sample_buffer(lookahead * 2), lookahead, 0, 0 };
    *((limiter_data*) u->data) = data;
}

void limiter_deconstructor(ugen* u)
{
    release_sample_buffer(((limiter_data*) u->data)->buffer);
    free(u->data);
}

// 2nd order lagrange factoring out the left side for y0 = 0 and the y1 multiplication as y1 == ratio == 1 in a limter.
// ((x - x1) / (x0 - x1) * y0{{0}} + (x - x0) / (x1 - x0) * y1{{scale:1}})
#define LIMITER_LAGRANGE(x0, x1, x) ((x - x0) / (x1 - x0))

__attribute__((always_inline)) static inline double limiterInlineCalc(
    sample_buffer buffer, double* samples, uint32_t num_samples_mask, limiter_data* data, double lookahead, double attack_gain,
    double release_gain, double threshold, double knee_width, double lower_knee_bound, double upper_knee_bound, double x, double envelope_in)
{
    //  Envelope follower
    double envelope_out = data->envelope_out;
    if (envelope_out < envelope_in)
        envelope_out = envelope_in + attack_gain * (envelope_out - envelope_in);
    else
        envelope_out = envelope_in + release_gain * (envelope_out - envelope_in);

    data->envelope_out = envelope_out;

    // Delay
    int64_t write_index = data->write_index;
    const int64_t read_index = write_index - lookahead;
    double y = read_index < 0 ? 0 : samples[read_index & num_samples_mask];
    samples[write_index & num_samples_mask] = x;
    data->write_index = write_index + 1;

    // Calculate gain based on soft/hard knee. Threshold, knee_width, lower_knee_bound, and upper_knee_bound are in decibels.
    const double envelope_out_db = AMP2DB(envelope_out);
    double gain = 1;

    if (knee_width > 0.0 && envelope_out_db > lower_knee_bound && envelope_out_db < upper_knee_bound)
    {
        gain = LIMITER_LAGRANGE(lower_knee_bound, upper_knee_bound, envelope_out_db);
    }

    gain = DB2AMP(fmin(0, gain * (threshold - envelope_out_db)));
    return y * gain;
}

#define LIMITER_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                                          \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                                                                  \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                                                                                  \
double* in2 = UGEN_INPUT_BUFFER(u, 2);                                                                                  \
double* in3 = UGEN_INPUT_BUFFER(u, 3);                                                                                  \
double* in4 = UGEN_INPUT_BUFFER(u, 4);                                                                                  \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                                                                 \
limiter_data* data = (limiter_data*) u.data;                                                                            \
sample_buffer buffer = *data->buffer;                                                                                   \
double* samples = buffer.samples;                                                                                       \
uint32_t num_samples = buffer.num_samples;                                                                              \
uint32_t num_samples_mask = buffer.num_samples_mask;                                                                    \
double lookahead = data->lookahead;                                                                                     \
double attack, attack_gain;                                                                                             \
double release, release_gain;                                                                                           \
double threshold;                                                                                                       \
double knee, knee_width, lower_knee_bound, upper_knee_bound;                                                            \
double input, envelope_in;                                                                                              \
CONTROL_ARGS                                                                                                            \
AUDIO_LOOP(                                                                                                             \
    AUDIO_ARGS                                                                                                          \
    UGEN_OUT(out, limiterInlineCalc(buffer, samples, num_samples_mask, data, lookahead, attack_gain, release_gain,      \
                                    threshold, knee, lower_knee_bound, upper_knee_bound, input, envelope_in));          \
);                                                                                                                      \

#define LIMITER_ATTACK_GAIN attack_gain = exp(-1.0 / SAMPLE_RATE * attack); // LUT for exp?
#define LIMITER_RELEASE_GAIN release_gain = exp(-1.0 / SAMPLE_RATE * release);
#define LIMITER_ENVELOPE_IN envelope_in = fabs(input);
#define LIMITER_KNEE_WIDTH knee_width = fmin(0, threshold * knee * -1.0); lower_knee_bound = threshold - (knee_width / 2.0); upper_knee_bound = fmin(0, threshold + (knee_width / 2.0));

#define LIMITER_ATTACKK attack = fabs(*in0); LIMITER_ATTACK_GAIN
#define LIMITER_ATTACKA attack = fabs(UGEN_IN(in0)); LIMITER_ATTACK_GAIN
#define LIMITER_RELEASEK release = fabs(*in1); LIMITER_RELEASE_GAIN
#define LIMITER_RELEASEA release = fabs(UGEN_IN(in1)); LIMITER_RELEASE_GAIN
#define LIMITER_THRESHOLDK threshold = *in2;
#define LIMITER_THRESHOLDA threshold = UGEN_IN(in2);
#define LIMITER_KNEEK knee = fmin(1, fmax(0, fabs(*in3))); LIMITER_KNEE_WIDTH
#define LIMITER_KNEEA knee = fmin(1, fmax(0, fabs(UGEN_IN(in3)))); LIMITER_KNEE_WIDTH
#define LIMITER_INPUTK input = *in4; LIMITER_ENVELOPE_IN
#define LIMITER_INPUTA input = UGEN_IN(in4); LIMITER_ENVELOPE_IN

// 0
__attribute__((flatten)) void limiter_kkkkk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
__attribute__((flatten)) void limiter_akkkk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
    )
}

// 2
__attribute__((flatten)) void limiter_kakkk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
    )
}

// 3
__attribute__((flatten)) void limiter_aakkk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
    )
}

// 4
__attribute__((flatten)) void limiter_kkakk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_THRESHOLDA    /* 2 */
    )
}

// 5
__attribute__((flatten)) void limiter_akakk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_THRESHOLDA    /* 2 */
    )
}

// 6
__attribute__((flatten)) void limiter_kaakk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
    )
}

// 7
__attribute__((flatten)) void limiter_aaakk_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_KNEEK         /* 3 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
    )
}

// 8
__attribute__((flatten)) void limiter_kkkak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_KNEEA         /* 3 */
    )
}

// 9
__attribute__((flatten)) void limiter_akkak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_KNEEA         /* 3 */
    )
}

// 10
__attribute__((flatten)) void limiter_kakak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
        LIMITER_KNEEA         /* 3 */
    )
}

// 11
__attribute__((flatten)) void limiter_aakak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
        LIMITER_KNEEA         /* 3 */
    )
}

// 12
__attribute__((flatten)) void limiter_kkaak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
    )
}

// 13
__attribute__((flatten)) void limiter_akaak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
    )
}

// 14
__attribute__((flatten)) void limiter_kaaak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
    )
}

// 15
__attribute__((flatten)) void limiter_aaaak_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_INPUTK        /* 4 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
    )
}

// 16
__attribute__((flatten)) void limiter_kkkka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_INPUTA        /* 4 */
    )
}

// 17
__attribute__((flatten)) void limiter_akkka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 18
__attribute__((flatten)) void limiter_kakka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 19
__attribute__((flatten)) void limiter_aakka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_THRESHOLDK    /* 2 */
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 20
__attribute__((flatten)) void limiter_kkaka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 21
__attribute__((flatten)) void limiter_akaka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 22
__attribute__((flatten)) void limiter_kaaka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 23
__attribute__((flatten)) void limiter_aaaka_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_KNEEK         /* 3 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 24
__attribute__((flatten)) void limiter_kkkaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */,
        // Audio Arguments
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 25
__attribute__((flatten)) void limiter_akkaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */
        LIMITER_THRESHOLDK    /* 2 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 26
__attribute__((flatten)) void limiter_kakaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_THRESHOLDK    /* 2 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 27
__attribute__((flatten)) void limiter_aakaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_THRESHOLDK    /* 2 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 28
__attribute__((flatten)) void limiter_kkaaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */
        LIMITER_RELEASEK      /* 1 */,
        // Audio Arguments
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 29
__attribute__((flatten)) void limiter_akaaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_RELEASEK      /* 1 */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 30
__attribute__((flatten)) void limiter_kaaaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        LIMITER_ATTACKK       /* 0 */,
        // Audio Arguments
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}

// 31
__attribute__((flatten)) void limiter_aaaaa_calc(ugen u)
{
    LIMITER_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        LIMITER_ATTACKA       /* 0 */
        LIMITER_RELEASEA      /* 1 */
        LIMITER_THRESHOLDA    /* 2 */
        LIMITER_KNEEA         /* 3 */
        LIMITER_INPUTA        /* 4 */
    )
}
