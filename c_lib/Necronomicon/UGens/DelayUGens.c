/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <math.h>
#include <stdlib.h>

#include "../AudioRuntime.h"
#include "DelayUGens.h"
#include "Util.h"

const uint32_t DELAY_DATA_SIZE = sizeof(delay_data);

void delayN_constructor(ugen* u)
{
    u->data = malloc(DELAY_DATA_SIZE);
    double max_delay_seconds = *((double*) u->constructor_args);
    double max_delay_time = fmax(1, max_delay_seconds * SAMPLE_RATE);
    delay_data data = { acquire_sample_buffer(max_delay_time), max_delay_time, 0 };
    *((delay_data*) u->data) = data;
}

const uint32_t MAX_DELAYL_OFFSET = 1;

void delayL_constructor(ugen* u)
{
    u->data = malloc(DELAY_DATA_SIZE);
    double max_delay_seconds = *((double*) u->constructor_args);
    double max_delay_time = fmax(2, max_delay_seconds * SAMPLE_RATE);
    delay_data data = { acquire_sample_buffer(max_delay_time + MAX_DELAYL_OFFSET), fmax(0, max_delay_time), 0 };
    *((delay_data*) u->data) = data;
}

const uint32_t MAX_DELAYC_OFFSET = 2;

void delayC_constructor(ugen* u)
{
    u->data = malloc(DELAY_DATA_SIZE);
    double max_delay_seconds = *((double*) u->constructor_args);
    double max_delay_time = fmax(3, max_delay_seconds * SAMPLE_RATE);
    delay_data data = { acquire_sample_buffer(max_delay_time + MAX_DELAYC_OFFSET), fmax(0, max_delay_time), 0 };
    *((delay_data*) u->data) = data;
}

void delay_deconstructor(ugen* u)
{
    puts("delay_deconstructor");
    puts("about to print delay_deconstructor ugen");
    printf("%p\n", u);
    puts("about to print delay_deconstructor ugen->data");
    printf("%p\n", u->data);
    puts("about to print delay_deconstructor ugen->data->buffer");
    printf("%p\n", ((delay_data*) u->data)->buffer);
    release_sample_buffer(((delay_data*) u->data)->buffer);
    free(u->data);
}

#define INIT_DELAY(u)                                           \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                          \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                          \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                         \
delay_data data = *((delay_data*) u.data);                      \
sample_buffer buffer = *data.buffer;                            \
int64_t write_index = data.write_index;                         \
uint32_t num_samples_mask = buffer.num_samples_mask;            \
double delay_time;                                              \
double x;                                                       \
double y;                                                       \

#define FINISH_DELAY()                                          \
data.write_index = write_index;                                 \
*((delay_data*) u.data) = data;                                 \

#define DELAYN_CALC(CONTROL_ARGS, AUDIO_ARGS)                                       \
INIT_DELAY(u);                                                                      \
int64_t iread_index;                                                                \
CONTROL_ARGS                                                                        \
AUDIO_LOOP(                                                                         \
    AUDIO_ARGS                                                                      \
    iread_index = write_index - (int64_t) delay_time;                               \
    y = iread_index < 0 ? 0 : buffer.samples[iread_index & num_samples_mask];       \
    buffer.samples[write_index & num_samples_mask] = x;                             \
    ++write_index;                                                                  \
    UGEN_OUT(out, y);                                                               \
);                                                                                  \
FINISH_DELAY();                                                                     \

#define DELAYN_DELAY_TIMEK delay_time = fmin(data.max_delay_time, fmax(1, (*in0) * SAMPLE_RATE));
#define DELAYN_DELAY_TIMEA delay_time = fmin(data.max_delay_time, fmax(1, UGEN_IN(in0) * SAMPLE_RATE));
#define DELAYN_XK x = *in1;
#define DELAYN_XA x = UGEN_IN(in1);

// 0
void delayN_kk_calc(ugen u)
{
    DELAYN_CALC(
        // Control Arguments
        DELAYN_DELAY_TIMEK    /* 0 */
        DELAYN_XK             /* 1 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void delayN_ak_calc(ugen u)
{
    DELAYN_CALC(
        // Control Arguments
        DELAYN_XK             /* 1 */,
        // Audio Arguments
        DELAYN_DELAY_TIMEA    /* 0 */
    )
}

// 2
void delayN_ka_calc(ugen u)
{
    DELAYN_CALC(
        // Control Arguments
        DELAYN_DELAY_TIMEK    /* 0 */,
        // Audio Arguments
        DELAYN_XA             /* 1 */
    )
}

// 3
void delayN_aa_calc(ugen u)
{
    DELAYN_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        DELAYN_DELAY_TIMEA    /* 0 */
        DELAYN_XA             /* 1 */
    )
}

inline double delayL(int64_t write_index, int64_t idelay_time, double delta, uint32_t num_samples_mask, double* samples)
{
    const int64_t iread_index0 = write_index - idelay_time;
    double y;

    if (iread_index0 < 0)
    {
        y = 0;
    }

    else
    {
        int64_t iread_index1;
        double y0, y1;
        iread_index1 = iread_index0 - 1;
        y0 = samples[iread_index0 & num_samples_mask];
        y1 = iread_index1 < 0 ? 0 : samples[iread_index1 & num_samples_mask];
        y  = LINEAR_INTERP(y0, y1, delta);
    }

    return y;
}

#define DELAYL_CALC(CONTROL_ARGS, AUDIO_ARGS)                                               \
INIT_DELAY(u);                                                                              \
double delta;                                                                               \
int64_t idelay_time;                                                                        \
CONTROL_ARGS                                                                                \
AUDIO_LOOP(                                                                                 \
    AUDIO_ARGS                                                                              \
    idelay_time = delay_time;                                                               \
    delta = delay_time - (double) idelay_time;                                              \
    y = delayL(write_index, idelay_time, delta, num_samples_mask, buffer.samples);          \
    buffer.samples[write_index & num_samples_mask] = x;                                     \
    ++write_index;                                                                          \
    UGEN_OUT(out, y);                                                                       \
);                                                                                          \
FINISH_DELAY();                                                                             \


#define DELAYL_DELAY_TIMEK delay_time = fmin(data.max_delay_time, fmax(1, (*in0) * SAMPLE_RATE));
#define DELAYL_DELAY_TIMEA delay_time = fmin(data.max_delay_time, fmax(1, UGEN_IN(in0) * SAMPLE_RATE));
#define DELAYL_XK x = *in1;
#define DELAYL_XA x = UGEN_IN(in1);

// 0
void delayL_kk_calc(ugen u)
{
    DELAYL_CALC(
        // Control Arguments
        DELAYL_DELAY_TIMEK    /* 0 */
        DELAYL_XK             /* 1 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void delayL_ak_calc(ugen u)
{
    DELAYL_CALC(
        // Control Arguments
        DELAYL_XK             /* 1 */,
        // Audio Arguments
        DELAYL_DELAY_TIMEA    /* 0 */
    )
}

// 2
void delayL_ka_calc(ugen u)
{
    DELAYL_CALC(
        // Control Arguments
        DELAYL_DELAY_TIMEK    /* 0 */,
        // Audio Arguments
        DELAYL_XA             /* 1 */
    )
}

// 3
void delayL_aa_calc(ugen u)
{
    DELAYL_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        DELAYL_DELAY_TIMEA    /* 0 */
        DELAYL_XA             /* 1 */
    )
}

inline double delayC(int64_t write_index, int64_t idelay_time, double delta, uint32_t num_samples_mask, double* samples)
{
    const int64_t iread_index1 = write_index - idelay_time;
    const int64_t iread_index0 = iread_index1 + 1;
    double y;

    if (iread_index0 < 0)
    {
        y = 0;
    }

    else
    {
        const int64_t iread_index2 = iread_index1 - 1;
        const int64_t iread_index3 = iread_index1 - 2;
        double y0, y1, y2, y3;

        if (iread_index1 < 0)
        {
            y0 = samples[iread_index0 & num_samples_mask];
            y1 = y2 = y3 = 0;
        }

        else if (iread_index2 < 0)
        {
            y0 = samples[iread_index0 & num_samples_mask];
            y1 = samples[iread_index1 & num_samples_mask];
            y2 = y3 = 0;
        }

        else if (iread_index3 < 0)
        {
            y0 = samples[iread_index0 & num_samples_mask];
            y1 = samples[iread_index1 & num_samples_mask];
            y2 = samples[iread_index1 & num_samples_mask];
            y3 = 0;
        }

        else
        {
            y0 = samples[iread_index0 & num_samples_mask];
            y1 = samples[iread_index1 & num_samples_mask];
            y2 = samples[iread_index2 & num_samples_mask];
            y3 = samples[iread_index3 & num_samples_mask];
        }

        y = CUBIC_INTERP(y0, y1, y2, y3, delta);
    }

    return y;
}

#define DELAYC_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                                       \
INIT_DELAY(u);                                                                                                      \
double* samples = buffer.samples;                                                                                   \
double delta;                                                                                                       \
double read_index;                                                                                                  \
int64_t idelay_time;                                                                                                \
CONTROL_ARGS                                                                                                        \
AUDIO_LOOP(                                                                                                         \
    AUDIO_ARGS                                                                                                      \
    idelay_time = delay_time;                                                                                       \
    delta = delay_time - (double) idelay_time;                                                                      \
    y = delayC(write_index, idelay_time, delta, num_samples_mask, samples);                                         \
    buffer.samples[write_index & num_samples_mask] = x;                                                             \
    ++write_index;                                                                                                  \
    UGEN_OUT(out, y);                                                                                               \
);                                                                                                                  \
FINISH_DELAY();

// Clamp delay at 1 to prevent the + 1 iread_index3 from reading on the wrong side of the write head
#define DELAYC_DELAY_TIMEK delay_time = fmin(data.max_delay_time, fmax(1.0, (*in0) * SAMPLE_RATE));
#define DELAYC_DELAY_TIMEA delay_time = fmin(data.max_delay_time, fmax(1.0, UGEN_IN(in0) * SAMPLE_RATE));
#define DELAYC_XK x = *in1;
#define DELAYC_XA x = UGEN_IN(in1);

// 0
void delayC_kk_calc(ugen u)
{
    DELAYC_CALC(
        // Control Arguments
        DELAYC_DELAY_TIMEK    /* 0 */
        DELAYC_XK             /* 1 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void delayC_ak_calc(ugen u)
{
    DELAYC_CALC(
        // Control Arguments
        DELAYC_XK             /* 1 */,
        // Audio Arguments
        DELAYC_DELAY_TIMEA    /* 0 */
    )
}

// 2
void delayC_ka_calc(ugen u)
{
    DELAYC_CALC(
        // Control Arguments
        DELAYC_DELAY_TIMEK    /* 0 */,
        // Audio Arguments
        DELAYC_XA             /* 1 */
    )
}

// 3
void delayC_aa_calc(ugen u)
{
    DELAYC_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        DELAYC_DELAY_TIMEA    /* 0 */
        DELAYC_XA             /* 1 */
    )
}

#define INIT_COMB(u)                    \
double decay_time;                      \
double feedback;                        \
double* in2 = UGEN_INPUT_BUFFER(u, 2);

static inline double CALC_FEEDBACK(double delay_time, double decay_time)
{
    if (delay_time == 0.0 || decay_time == 0.0)
        return 0.0;

    double absret = exp(LOG_001 * delay_time / fabs(decay_time));
    return copysign(absret, decay_time);
}

#define COMBN_CALC(CONTROL_ARGS, AUDIO_ARGS)                                        \
INIT_DELAY(u);                                                                      \
INIT_COMB(u);                                                                       \
int64_t iread_index;                                                                \
CONTROL_ARGS                                                                        \
AUDIO_LOOP(                                                                         \
    AUDIO_ARGS                                                                      \
    iread_index = write_index - (int64_t) delay_time;                               \
    y = iread_index < 0 ? 0 : buffer.samples[iread_index & num_samples_mask];       \
    feedback = CALC_FEEDBACK(delay_time, decay_time);                               \
    buffer.samples[write_index & num_samples_mask] = x + (feedback * y);            \
    ++write_index;                                                                  \
    UGEN_OUT(out, y);                                                               \
);                                                                                  \
FINISH_DELAY();                                                                     \

#define COMBN_DELAY_TIMEK delay_time = fmin(data.max_delay_time, fmax(1, (*in0) * SAMPLE_RATE));
#define COMBN_DELAY_TIMEA delay_time = fmin(data.max_delay_time, fmax(1, UGEN_IN(in0) * SAMPLE_RATE));
#define COMBN_FEEDBACKK decay_time = (*in1) * SAMPLE_RATE;
#define COMBN_FEEDBACKA decay_time = UGEN_IN(in1) * SAMPLE_RATE;
#define COMBN_XK x = *in2;
#define COMBN_XA x = UGEN_IN(in2);

// 0
void combN_kkk_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        COMBN_DELAY_TIMEK     /* 0 */
        COMBN_FEEDBACKK       /* 1 */
        COMBN_XK              /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void combN_akk_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        COMBN_FEEDBACKK       /* 1 */
        COMBN_XK              /* 2 */,
        // Audio Arguments
        COMBN_DELAY_TIMEA     /* 0 */
    )
}

// 2
void combN_kak_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        COMBN_DELAY_TIMEK     /* 0 */
        COMBN_XK              /* 2 */,
        // Audio Arguments
        COMBN_FEEDBACKA       /* 1 */
    )
}

// 3
void combN_aak_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        COMBN_XK              /* 2 */,
        // Audio Arguments
        COMBN_DELAY_TIMEA     /* 0 */
        COMBN_FEEDBACKA       /* 1 */
    )
}

// 4
void combN_kka_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        COMBN_DELAY_TIMEK     /* 0 */
        COMBN_FEEDBACKK       /* 1 */,
        // Audio Arguments
        COMBN_XA              /* 2 */
    )
}

// 5
void combN_aka_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        COMBN_FEEDBACKK       /* 1 */,
        // Audio Arguments
        COMBN_DELAY_TIMEA     /* 0 */
        COMBN_XA              /* 2 */
    )
}

// 6
void combN_kaa_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        COMBN_DELAY_TIMEK     /* 0 */,
        // Audio Arguments
        COMBN_FEEDBACKA       /* 1 */
        COMBN_XA              /* 2 */
    )
}

// 7
void combN_aaa_calc(ugen u)
{
    COMBN_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        COMBN_DELAY_TIMEA     /* 0 */
        COMBN_FEEDBACKA       /* 1 */
        COMBN_XA              /* 2 */
    )
}

#define COMBL_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                \
INIT_DELAY(u);                                                                              \
INIT_COMB(u);                                                                               \
double delta;                                                                               \
int64_t idelay_time;                                                                        \
CONTROL_ARGS                                                                                \
AUDIO_LOOP(                                                                                 \
    AUDIO_ARGS                                                                              \
    idelay_time = delay_time;                                                               \
    delta = delay_time - (double) idelay_time;                                              \
    y = delayL(write_index, idelay_time, delta, num_samples_mask, buffer.samples);          \
    feedback = CALC_FEEDBACK(delay_time, decay_time);                                       \
    buffer.samples[write_index & num_samples_mask] = x + (feedback * y);                    \
    ++write_index;                                                                          \
    UGEN_OUT(out, y);                                                                       \
);                                                                                          \
FINISH_DELAY();                                                                             \

#define COMBL_DELAY_TIMEK delay_time = fmin(data.max_delay_time, fmax(1, (*in0) * SAMPLE_RATE));
#define COMBL_DELAY_TIMEA delay_time = fmin(data.max_delay_time, fmax(1, UGEN_IN(in0) * SAMPLE_RATE));
#define COMBL_FEEDBACKK decay_time = (*in1) * SAMPLE_RATE;
#define COMBL_FEEDBACKA decay_time = UGEN_IN(in1) * SAMPLE_RATE;
#define COMBL_XK x = *in2;
#define COMBL_XA x = UGEN_IN(in2);

// 0
void combL_kkk_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        COMBL_DELAY_TIMEK     /* 0 */
        COMBL_FEEDBACKK       /* 1 */
        COMBL_XK              /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void combL_akk_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        COMBL_FEEDBACKK       /* 1 */
        COMBL_XK              /* 2 */,
        // Audio Arguments
        COMBL_DELAY_TIMEA     /* 0 */
    )
}

// 2
void combL_kak_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        COMBL_DELAY_TIMEK     /* 0 */
        COMBL_XK              /* 2 */,
        // Audio Arguments
        COMBL_FEEDBACKA       /* 1 */
    )
}

// 3
void combL_aak_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        COMBL_XK              /* 2 */,
        // Audio Arguments
        COMBL_DELAY_TIMEA     /* 0 */
        COMBL_FEEDBACKA       /* 1 */
    )
}

// 4
void combL_kka_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        COMBL_DELAY_TIMEK     /* 0 */
        COMBL_FEEDBACKK       /* 1 */,
        // Audio Arguments
        COMBL_XA              /* 2 */
    )
}

// 5
void combL_aka_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        COMBL_FEEDBACKK       /* 1 */,
        // Audio Arguments
        COMBL_DELAY_TIMEA     /* 0 */
        COMBL_XA              /* 2 */
    )
}

// 6
void combL_kaa_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        COMBL_DELAY_TIMEK     /* 0 */,
        // Audio Arguments
        COMBL_FEEDBACKA       /* 1 */
        COMBL_XA              /* 2 */
    )
}

// 7
void combL_aaa_calc(ugen u)
{
    COMBL_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        COMBL_DELAY_TIMEA     /* 0 */
        COMBL_FEEDBACKA       /* 1 */
        COMBL_XA              /* 2 */
    )
}

#define COMBC_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                                        \
INIT_DELAY(u);                                                                                                      \
INIT_COMB(u);                                                                                                       \
double* samples = buffer.samples;                                                                                   \
double delta;                                                                                                       \
int64_t idelay_time;                                                                                                \
CONTROL_ARGS                                                                                                        \
AUDIO_LOOP(                                                                                                         \
    AUDIO_ARGS                                                                                                      \
    idelay_time = delay_time;                                                                                       \
    delta = delay_time - (double) idelay_time;                                                                      \
    y = delayC(write_index, idelay_time, delta, num_samples_mask, samples);                                         \
    feedback = CALC_FEEDBACK(delay_time, decay_time);                                                               \
    samples[write_index & num_samples_mask] = x + (feedback * y);                                                   \
    ++write_index;                                                                                                  \
    UGEN_OUT(out, y);                                                                                               \
);                                                                                                                  \
FINISH_DELAY();

#define COMBC_DELAY_TIMEK delay_time = fmin(data.max_delay_time, fmax(1.0, (*in0) * SAMPLE_RATE));
#define COMBC_DELAY_TIMEA delay_time = fmin(data.max_delay_time, fmax(1.0, UGEN_IN(in0) * SAMPLE_RATE));
#define COMBC_FEEDBACKK decay_time = (*in1) * SAMPLE_RATE;
#define COMBC_FEEDBACKA decay_time = UGEN_IN(in1) * SAMPLE_RATE;
#define COMBC_XK x = *in2;
#define COMBC_XA x = UGEN_IN(in2);

// 0
void combC_kkk_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        COMBC_DELAY_TIMEK     /* 0 */
        COMBC_FEEDBACKK       /* 1 */
        COMBC_XK              /* 2 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void combC_akk_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        COMBC_FEEDBACKK       /* 1 */
        COMBC_XK              /* 2 */,
        // Audio Arguments
        COMBC_DELAY_TIMEA     /* 0 */
    )
}

// 2
void combC_kak_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        COMBC_DELAY_TIMEK     /* 0 */
        COMBC_XK              /* 2 */,
        // Audio Arguments
        COMBC_FEEDBACKA       /* 1 */
    )
}

// 3
void combC_aak_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        COMBC_XK              /* 2 */,
        // Audio Arguments
        COMBC_DELAY_TIMEA     /* 0 */
        COMBC_FEEDBACKA       /* 1 */
    )
}

// 4
void combC_kka_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        COMBC_DELAY_TIMEK     /* 0 */
        COMBC_FEEDBACKK       /* 1 */,
        // Audio Arguments
        COMBC_XA              /* 2 */
    )
}

// 5
void combC_aka_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        COMBC_FEEDBACKK       /* 1 */,
        // Audio Arguments
        COMBC_DELAY_TIMEA     /* 0 */
        COMBC_XA              /* 2 */
    )
}

// 6
void combC_kaa_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        COMBC_DELAY_TIMEK     /* 0 */,
        // Audio Arguments
        COMBC_FEEDBACKA       /* 1 */
        COMBC_XA              /* 2 */
    )
}

// 7
void combC_aaa_calc(ugen u)
{
    COMBC_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        COMBC_DELAY_TIMEA     /* 0 */
        COMBC_FEEDBACKA       /* 1 */
        COMBC_XA              /* 2 */
    )
}

typedef struct
{
    sample_buffer* buffer;
    double minFreq;
    double last_sample;
    double prev_trig;
    int64_t trig_samples;
    uint64_t write_index;
} pluck_data;

void pluck_constructor(ugen* u)
{
    u->data = malloc(sizeof(pluck_data));
    const double min_freq = fmax(0.001, *((double*) u->constructor_args));
    pluck_data data = { acquire_sample_buffer(SAMPLE_RATE / min_freq), min_freq, 0.0, 0.0, 0L, 0 };
    *((pluck_data*) u->data) = data;
}

void pluck_deconstructor(ugen* u)
{
    release_sample_buffer(((pluck_data*) u->data)->buffer);
    free(u->data);
}

static inline double pluck_inline_calc(pluck_data* data, sample_buffer buffer, double delay, double decay, double coeff, double min_fabs_coeff, double input, double trig)
{
    const uint64_t write_index = data->write_index;
    const int64_t idelay_time = delay;
    const double delta = delay - (double) idelay_time;

    if (data->prev_trig <= 0.0 && trig > 0.0)
        data->trig_samples = delay + 0.5;

    double x;
    if (data->trig_samples > 0)
    {
        x = input;
        data->trig_samples--;
    }

    else
    {
        x = 0.0;
    }

    const double feedback = CALC_FEEDBACK(delay, decay);
    double y = delayC(write_index, idelay_time, delta, buffer.num_samples_mask, buffer.samples);
    y = (min_fabs_coeff * y) + (coeff * data->last_sample);
    buffer.samples[write_index & buffer.num_samples_mask] = x + (feedback * y);
    data->write_index++;
    data->prev_trig = trig;
    data->last_sample = y;
    return y;
}

#define PLUCK_CALC(CONTROL_ARGS, AUDIO_ARGS)                                                            \
pluck_data* data = ((pluck_data*) u.data);                                                              \
sample_buffer buffer = *data->buffer;                                                                   \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                                                  \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                                                                  \
double* in2 = UGEN_INPUT_BUFFER(u, 2);                                                                  \
double* in3 = UGEN_INPUT_BUFFER(u, 3);                                                                  \
double* in4 = UGEN_INPUT_BUFFER(u, 4);                                                                  \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                                                 \
double clamped, delay;                                                                                  \
double decay;                                                                                           \
double coeff, min_fabs_coeff;                                                                           \
double input;                                                                                           \
double trig;                                                                                            \
CONTROL_ARGS                                                                                            \
AUDIO_LOOP(                                                                                             \
    AUDIO_ARGS                                                                                          \
    UGEN_OUT(out, pluck_inline_calc(data, buffer, delay, decay, coeff, min_fabs_coeff, input, trig));   \
);

#define PLUCK_DELAY delay = (1.0 / clamped) * SAMPLE_RATE;
#define PLUCK_FREQK clamped = fmax(*in0, data->minFreq); PLUCK_DELAY
#define PLUCK_FREQA clamped = fmax(UGEN_IN(in0), data->minFreq); PLUCK_DELAY
#define PLUCK_DECAYK decay = (*in1) * SAMPLE_RATE;
#define PLUCK_DECAYA decay = UGEN_IN(in1) * SAMPLE_RATE;
#define PLUCK_MIN_FABS_COEFF min_fabs_coeff = 1 - fabs(coeff);
#define PLUCK_COEFFK coeff = fmax(-0.9999, fmin(0.9999, *in2)); PLUCK_MIN_FABS_COEFF
#define PLUCK_COEFFA coeff = fmax(-0.9999, fmin(0.9999, UGEN_IN(in2))); PLUCK_MIN_FABS_COEFF
#define PLUCK_INPUTK input = *in3;
#define PLUCK_INPUTA input = UGEN_IN(in3);
#define PLUCK_TRIGK trig = *in4;
#define PLUCK_TRIGA trig = UGEN_IN(in4);

// 0
void pluck_kkkkk_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_DECAYK          /* 1 */
        PLUCK_COEFFK          /* 2 */
        PLUCK_INPUTK          /* 3 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        /* no audio args */
    )
}

// 1
void pluck_akkkk_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_DECAYK          /* 1 */
        PLUCK_COEFFK          /* 2 */
        PLUCK_INPUTK          /* 3 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
    )
}

// 2
void pluck_kakkk_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_COEFFK          /* 2 */
        PLUCK_INPUTK          /* 3 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_DECAYA          /* 1 */
    )
}

// 3
void pluck_aakkk_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_COEFFK          /* 2 */
        PLUCK_INPUTK          /* 3 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_DECAYA          /* 1 */
    )
}

// 4
void pluck_kkakk_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_DECAYK          /* 1 */
        PLUCK_INPUTK          /* 3 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_COEFFA          /* 2 */
    )
}

// 5
void pluck_akakk_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_DECAYK          /* 1 */
        PLUCK_INPUTK          /* 3 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_COEFFA          /* 2 */
    )
}

// 6
void pluck_kaakk_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_INPUTK          /* 3 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_DECAYA          /* 1 */
        PLUCK_COEFFA          /* 2 */
    )
}

// 7
void pluck_aaakk_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_INPUTK          /* 3 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_DECAYA          /* 1 */
        PLUCK_COEFFA          /* 2 */
    )
}

// 8
void pluck_kkkak_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_DECAYK          /* 1 */
        PLUCK_COEFFK          /* 2 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_INPUTA          /* 3 */
    )
}

// 9
void pluck_akkak_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_DECAYK          /* 1 */
        PLUCK_COEFFK          /* 2 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_INPUTA          /* 3 */
    )
}

// 10
void pluck_kakak_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_COEFFK          /* 2 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_DECAYA          /* 1 */
        PLUCK_INPUTA          /* 3 */
    )
}

// 11
void pluck_aakak_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_COEFFK          /* 2 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_DECAYA          /* 1 */
        PLUCK_INPUTA          /* 3 */
    )
}

// 12
void pluck_kkaak_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_DECAYK          /* 1 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_COEFFA          /* 2 */
        PLUCK_INPUTA          /* 3 */
    )
}

// 13
void pluck_akaak_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_DECAYK          /* 1 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_COEFFA          /* 2 */
        PLUCK_INPUTA          /* 3 */
    )
}

// 14
void pluck_kaaak_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_DECAYA          /* 1 */
        PLUCK_COEFFA          /* 2 */
        PLUCK_INPUTA          /* 3 */
    )
}

// 15
void pluck_aaaak_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_TRIGK           /* 4 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_DECAYA          /* 1 */
        PLUCK_COEFFA          /* 2 */
        PLUCK_INPUTA          /* 3 */
    )
}

// 16
void pluck_kkkka_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_DECAYK          /* 1 */
        PLUCK_COEFFK          /* 2 */
        PLUCK_INPUTK          /* 3 */,
        // Audio Arguments
        PLUCK_TRIGA           /* 4 */
    )
}

// 17
void pluck_akkka_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_DECAYK          /* 1 */
        PLUCK_COEFFK          /* 2 */
        PLUCK_INPUTK          /* 3 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_TRIGA           /* 4 */
    )
}

// 18
void pluck_kakka_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_COEFFK          /* 2 */
        PLUCK_INPUTK          /* 3 */,
        // Audio Arguments
        PLUCK_DECAYA          /* 1 */
        PLUCK_TRIGA           /* 4 */
    )
}

// 19
void pluck_aakka_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_COEFFK          /* 2 */
        PLUCK_INPUTK          /* 3 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_DECAYA          /* 1 */
        PLUCK_TRIGA           /* 4 */
    )
}

// 20
void pluck_kkaka_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_DECAYK          /* 1 */
        PLUCK_INPUTK          /* 3 */,
        // Audio Arguments
        PLUCK_COEFFA          /* 2 */
        PLUCK_TRIGA           /* 4 */
    )
}

// 21
void pluck_akaka_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_DECAYK          /* 1 */
        PLUCK_INPUTK          /* 3 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_COEFFA          /* 2 */
        PLUCK_TRIGA           /* 4 */
    )
}

// 22
void pluck_kaaka_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_INPUTK          /* 3 */,
        // Audio Arguments
        PLUCK_DECAYA          /* 1 */
        PLUCK_COEFFA          /* 2 */
        PLUCK_TRIGA           /* 4 */
    )
}

// 23
void pluck_aaaka_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_INPUTK          /* 3 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_DECAYA          /* 1 */
        PLUCK_COEFFA          /* 2 */
        PLUCK_TRIGA           /* 4 */
    )
}

// 24
void pluck_kkkaa_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_DECAYK          /* 1 */
        PLUCK_COEFFK          /* 2 */,
        // Audio Arguments
        PLUCK_INPUTA          /* 3 */
        PLUCK_TRIGA           /* 4 */
    )
}

// 25
void pluck_akkaa_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_DECAYK          /* 1 */
        PLUCK_COEFFK          /* 2 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_INPUTA          /* 3 */
        PLUCK_TRIGA           /* 4 */
    )
}

// 26
void pluck_kakaa_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_COEFFK          /* 2 */,
        // Audio Arguments
        PLUCK_DECAYA          /* 1 */
        PLUCK_INPUTA          /* 3 */
        PLUCK_TRIGA           /* 4 */
    )
}

// 27
void pluck_aakaa_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_COEFFK          /* 2 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_DECAYA          /* 1 */
        PLUCK_INPUTA          /* 3 */
        PLUCK_TRIGA           /* 4 */
    )
}

// 28
void pluck_kkaaa_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */
        PLUCK_DECAYK          /* 1 */,
        // Audio Arguments
        PLUCK_COEFFA          /* 2 */
        PLUCK_INPUTA          /* 3 */
        PLUCK_TRIGA           /* 4 */
    )
}

// 29
void pluck_akaaa_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_DECAYK          /* 1 */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_COEFFA          /* 2 */
        PLUCK_INPUTA          /* 3 */
        PLUCK_TRIGA           /* 4 */
    )
}

// 30
void pluck_kaaaa_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        PLUCK_FREQK           /* 0 */,
        // Audio Arguments
        PLUCK_DECAYA          /* 1 */
        PLUCK_COEFFA          /* 2 */
        PLUCK_INPUTA          /* 3 */
        PLUCK_TRIGA           /* 4 */
    )
}

// 31
void pluck_aaaaa_calc(ugen u)
{
    PLUCK_CALC(
        // Control Arguments
        /* no control args */,
        // Audio Arguments
        PLUCK_FREQA           /* 0 */
        PLUCK_DECAYA          /* 1 */
        PLUCK_COEFFA          /* 2 */
        PLUCK_INPUTA          /* 3 */
        PLUCK_TRIGA           /* 4 */
    )
}
