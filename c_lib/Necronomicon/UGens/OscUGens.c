/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdio.h>

#include "../AudioRuntime.h"
#include "Util.h"

void accumulator_constructor(ugen* u)
{
    u->data = malloc(DOUBLE_SIZE); // Phase accumulator
    *((double*) u->data) = 0.0;
}

void accumulator_deconstructor(ugen* u)
{
    free(u->data);
}

void sin_constructor(ugen* u)
{
    u->data = malloc(DOUBLE_SIZE); // Phase accumulator
    *((double*) u->data) = 0;
}

void sin_deconstructor(ugen* u)
{
    free(u->data);
}

#define SIN_CALC(CONTROL_ARGS, AUDIO_ARGS)                              \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                  \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                 \
                                                                        \
double phase = *((double*) u.data);                                     \
double freq;                                                            \
uint16_t index1;                                                        \
uint16_t index2;                                                        \
double v1;                                                              \
double v2;                                                              \
double delta, phase_increment;                                          \
                                                                        \
CONTROL_ARGS                                                            \
                                                                        \
AUDIO_LOOP(                                                             \
    AUDIO_ARGS                                                          \
    index1 = phase;                                                     \
    index2 = index1 + 1;                                                \
    v1     = sine_table[index1];                                        \
    v2     = sine_table[index2];                                        \
    delta  = phase - ((int64_t) phase);                                 \
    UGEN_OUT(out, LERP(v1,v2,delta));                                   \
    phase += phase_increment;                                           \
    if (phase > DOUBLE_TABLE_SIZE)                                      \
        phase = phase - DOUBLE_TABLE_SIZE;                              \
);                                                                      \
*((double*) u.data) = phase;\
printf("sin_calc freq: %f\n", freq);

#define mmsin_a0  1.0
#define mmsin_a1 -1.666666666640169148537065260055e-1
#define mmsin_a2  8.333333316490113523036717102793e-3
#define mmsin_a3 -1.984126600659171392655484413285e-4
#define mmsin_a4  2.755690114917374804474016589137e-6
#define mmsin_a5 -2.502845227292692953118686710787e-8
#define mmsin_a6  1.538730635926417598443354215485e-10

#define MINIMAXSIN(X)                                                                                                       \
x2 = X * X;                                                                                                                 \
X * (mmsin_a0 + x2 * (mmsin_a1 + x2 * (mmsin_a2 + x2 * (mmsin_a3 + x2 * (mmsin_a4 + x2 * (mmsin_a5 + x2 * mmsin_a6))))));   \

#define SIN_PHASE_INCREMENT phase_increment = TABLE_MUL_RECIP_SAMPLE_RATE * freq;

void sin_a_calc(ugen u)
{
    SIN_CALC(
        /*no control args*/, // Control Args
        freq = UGEN_IN(in0); SIN_PHASE_INCREMENT // Audio Args
    );
}

void sin_k_calc(ugen u)
{
    SIN_CALC(
        freq = in0[0]; SIN_PHASE_INCREMENT,    // Control Args
        /*no audio args*/ // Audio Args
    );
}

//---------------------------------------------
//MinBlep Bandwidth-limited Saw and Square
//---------------------------------------------

#define KTABLE 64 // BLEP table oversampling factor

typedef struct
{
    double *lpTable;
    int32_t    c;
} minbleptable_t;

typedef struct
{
    double  output;
    double  phase;
    double  masterPhase;
    double *buffer;      // circular output buffer
    int32_t    cBuffer;     // buffer size
    int32_t    iBuffer;     // current buffer position
    int32_t    nInit;         // amount of initialized entries
    double  prevSyncAmp; //For hardsync
} minblep;

minbleptable_t gMinBLEP;

bool minBLEP_Init()
{
    // load table
    const int8_t* blep_table = "/misc/minblep.mat";
    int8_t path_to_blep_table[strlen(RESOUCES_PATH) + strlen(blep_table)];
    strcat(path_to_blep_table,RESOUCES_PATH);
    strcat(path_to_blep_table,blep_table);
    FILE *fp=fopen(path_to_blep_table,"rb");
    uint32_t iSize;

    if (!fp) return false;

    fseek(fp,0x134,SEEK_SET);

    size_t file_size = fread(&iSize,sizeof(int32_t),1,fp);
    gMinBLEP.c=iSize/sizeof(double);

    gMinBLEP.lpTable=(double*)malloc(iSize);
    if (!gMinBLEP.lpTable) return false;

    file_size = fread(gMinBLEP.lpTable,iSize,1,fp);

    fclose(fp);

    return true;
}

void minBLEP_Free()
{
    free(gMinBLEP.lpTable);
}

void minblep_constructor(ugen* u)
{
    minblep* mb     = malloc(sizeof(minblep));
    mb->output      = 0.0;
    mb->phase       = 0.0;
    mb->masterPhase = 0.0;
    mb->cBuffer     = gMinBLEP.c/KTABLE;
    mb->buffer      = (double*)malloc(sizeof(double) * mb->cBuffer);
    mb->iBuffer     = 0;
    mb->nInit       = 0;
    mb->prevSyncAmp = 0;
    u->data         = mb;
}

void minblep_deconstructor(ugen* u)
{
    minblep* mb = (minblep*) u->data;
    // free(mb->buffer);
    free(u->data);
}

// add impulse into buffer
inline void add_blep(minblep* mb, double offset, double amp)
{
    int32_t   i;
    double *out       = mb->buffer  + mb->iBuffer;
    double *in        = gMinBLEP.lpTable + (int32_t) (KTABLE*offset);
    double frac       = fmod(KTABLE*offset,1.0);
    int32_t   cBLEP      = (gMinBLEP.c / KTABLE) - 1;
    double *bufferEnd = mb->buffer  + mb->cBuffer;
    double f;

    // add
    for(i=0; i < mb->nInit; i++, in += KTABLE, out++)
    {
        if (out >= bufferEnd)
            out = mb->buffer;

        f = LERP(in[0],in[1],frac);
        *out += amp * (1-f);
    }

    // copy
    for(; i < cBLEP; i++, in += KTABLE, out++)
    {
        if (out >= bufferEnd)
            out = mb->buffer;

        f = LERP(in[0],in[1],frac);
        *out = amp*(1-f);
    }

    mb->nInit = cBLEP;
}

// saw

#define SAW_CALC(CONTROL_ARGS, AUDIO_ARGS)      \
double* in0 = UGEN_INPUT_BUFFER(u, 0);          \
double* out = UGEN_OUTPUT_BUFFER(u, 0);         \
minblep mb  = *((minblep*) u.data);             \
double freq;                                    \
double y;                                       \
CONTROL_ARGS                                    \
AUDIO_LOOP(                                     \
    AUDIO_ARGS                                  \
    /* create waveform */                       \
    mb.phase += freq;                           \
    /* add BLEP at end of waveform */           \
    if (mb.phase >= 1)                          \
    {                                           \
        mb.phase  = mb.phase - 1.0;             \
        mb.output = 0.0;                        \
        add_blep(&mb, mb.phase/freq,1.0);       \
    }                                           \
    y = mb.phase;                               \
    /* add BLEP buffer contents */              \
    if (mb.nInit)                               \
    {                                           \
        y += mb.buffer[mb.iBuffer];             \
        mb.nInit--;                             \
        if (++mb.iBuffer >= mb.cBuffer)         \
            mb.iBuffer=0;                       \
    }                                           \
    UGEN_OUT(out, y);                           \
);                                              \
*((minblep*) u.data) = mb;                      \

void saw_k_calc(ugen u)
{
    SAW_CALC(
        freq  = in0[0] * RECIP_SAMPLE_RATE;,
        /* no audio args */
    )
}

void saw_a_calc(ugen u)
{
    SAW_CALC(
        /* no control args */,
        freq  = UGEN_IN(in0) * RECIP_SAMPLE_RATE;
    )
}

// square

#define SQUARE_CALC(CONTROL_ARGS, AUDIO_ARGS)               \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                      \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                      \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                     \
minblep mb  = *((minblep*) u.data);                         \
double freq;                                                \
double pwm;                                                 \
double y;                                                   \
CONTROL_ARGS                                                \
AUDIO_LOOP(                                                 \
    AUDIO_ARGS                                              \
    /* create waveform */                                   \
    mb.phase += freq;                                       \
    /* add BLEP at end of waveform */                       \
    if (mb.phase >= 1)                                      \
    {                                                       \
        mb.phase  = mb.phase - 1.0;                         \
        mb.output = 0.0;                                    \
        add_blep(&mb, mb.phase/freq,1.0);                   \
    }                                                       \
    /* add BLEP in middle of wavefor for squarewave */      \
    if (!mb.output && mb.phase > pwm)                       \
    {                                                       \
        mb.output = 1.0;                                    \
        add_blep(&mb, (mb.phase - pwm) / freq,-1.0);        \
    }                                                       \
    y = mb.output;                                          \
    /* add BLEP buffer contents */                          \
    if (mb.nInit)                                           \
    {                                                       \
        y += mb.buffer[mb.iBuffer];                         \
        mb.nInit--;                                         \
        if (++mb.iBuffer >= mb.cBuffer)                     \
            mb.iBuffer=0;                                   \
    }                                                       \
    UGEN_OUT(out, y);                                       \
);                                                          \
*((minblep*) u.data) = mb;                                  \

void square_aa_calc(ugen u)
{
    SQUARE_CALC(
        /* no control args */,
        freq = UGEN_IN(in0) * RECIP_SAMPLE_RATE;
        pwm  = CLAMP(UGEN_IN(in1),0,1) * 0.5;
    )
}

void square_ka_calc(ugen u)
{
    SQUARE_CALC(
        freq = in0[0] * RECIP_SAMPLE_RATE;,
        pwm  = CLAMP(UGEN_IN(in1),0,1) * 0.5;
    )
}

void square_ak_calc(ugen u)
{
    SQUARE_CALC(
        pwm  = CLAMP(in1[0],0,1) * 0.5;,
        freq = UGEN_IN(in0) * RECIP_SAMPLE_RATE;
    )
}

void square_kk_calc(ugen u)
{
    SQUARE_CALC(
        freq = in0[0] * RECIP_SAMPLE_RATE;
        pwm  = CLAMP(in1[0],0,1) * 0.5;,
        /* no audio args */
    )
}

// syncsaw

#define SYNCSAW_CALC(CONTROL_ARGS, AUDIO_ARGS)      \
double* in0 = UGEN_INPUT_BUFFER(u, 0);              \
double* in1 = UGEN_INPUT_BUFFER(u, 1);              \
double* out = UGEN_OUTPUT_BUFFER(u, 0);             \
minblep mb  = *((minblep*) u.data);                 \
double freq;                                        \
double sync;                                        \
double y;                                           \
CONTROL_ARGS                                        \
AUDIO_LOOP(                                         \
    AUDIO_ARGS                                      \
    /* create waveform */                           \
    mb.phase += freq;                               \
    /* add BLEP at end of waveform */               \
    if (mb.phase >= 1)                              \
    {                                               \
        mb.phase  = mb.phase - 1.0;                 \
        mb.output = 0.0;                            \
        add_blep(&mb, mb.phase/freq,1.0);           \
    }                                               \
    else if (mb.prevSyncAmp < 0 && sync > 0)        \
    {                                               \
        mb.phase  = 0.0;                            \
        mb.output = 0.0;                            \
        add_blep(&mb, mb.phase/freq,1.0);           \
    }                                               \
    y = mb.phase;                                   \
    /* add BLEP buffer contents */                  \
    if (mb.nInit)                                   \
    {                                               \
        y += mb.buffer[mb.iBuffer];                 \
        mb.nInit--;                                 \
        if (++mb.iBuffer >= mb.cBuffer)             \
            mb.iBuffer=0;                           \
    }                                               \
    mb.prevSyncAmp = sync;                          \
    UGEN_OUT(out, y);                               \
);                                                  \
*((minblep*) u.data) = mb;                          \

void syncsaw_aa_calc(ugen u)
{
    SYNCSAW_CALC(
        /* no control args */,
        freq  = UGEN_IN(in0) * RECIP_SAMPLE_RATE;
        sync  = UGEN_IN(in1);
    )
}

void syncsaw_ka_calc(ugen u)
{
    SYNCSAW_CALC(
        freq  = in0[0] * RECIP_SAMPLE_RATE;,
        sync  = UGEN_IN(in1);
    )
}

void syncsaw_ak_calc(ugen u)
{
    SYNCSAW_CALC(
        sync  = in1[0];,
        freq  = UGEN_IN(in0) * RECIP_SAMPLE_RATE;
    )
}

void syncsaw_kk_calc(ugen u)
{
    SYNCSAW_CALC(
        freq  = in0[0] * RECIP_SAMPLE_RATE;
        sync  = in1[0];,
        /* no audio args */
    )
}

// syncsquare

#define SYNCSQUARE_CALC(CONTROL_ARGS, AUDIO_ARGS)           \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                      \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                      \
double* in2 = UGEN_INPUT_BUFFER(u, 2);                      \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                     \
minblep mb  = *((minblep*) u.data);                         \
double freq;                                                \
double pwm;                                                 \
double sync;                                                \
double y;                                                   \
CONTROL_ARGS                                                \
AUDIO_LOOP(                                                 \
    AUDIO_ARGS                                              \
    /* create waveform */                                   \
    mb.phase += freq;                                       \
    /* add BLEP at end of waveform */                       \
    if (mb.phase >= 1)                                      \
    {                                                       \
        mb.phase  = mb.phase - 1.0;                         \
        mb.output = -1.0;                                   \
        add_blep(&mb, mb.phase/freq,1.0);                   \
    }                                                       \
    /* add BLEP in middle of wavefor for squarewave */      \
    if (!mb.output && mb.phase > pwm)                       \
    {                                                       \
        mb.output = 1.0;                                    \
        add_blep(&mb, (mb.phase - pwm) / freq,-1.0);        \
    }                                                       \
    if (mb.prevSyncAmp < 0 && sync > 0)                     \
    {                                                       \
        mb.phase  = 0.0;                                    \
        mb.output = 1.0;                                    \
        add_blep(&mb, mb.phase/freq,1.0);                   \
    }                                                       \
    y = mb.output;                                          \
    /* add BLEP buffer contents */                          \
    if (mb.nInit)                                           \
    {                                                       \
        y += mb.buffer[mb.iBuffer];                         \
        mb.nInit--;                                         \
        if (++mb.iBuffer >= mb.cBuffer)                     \
            mb.iBuffer=0;                                   \
    }                                                       \
    mb.prevSyncAmp = sync;                                  \
    UGEN_OUT(out, y);                                       \
);                                                          \
*((minblep*) u.data) = mb;                                  \

#define SYNCSQUARE_FREQ_K freq = in0[0] * RECIP_SAMPLE_RATE;
#define SYNCSQUARE_PWM_K pwm  = CLAMP(in1[0],0,1) * 0.5;
#define SYNCSQUARE_SYNC_K sync = in2[0];

#define SYNCSQUARE_FREQ_A freq = UGEN_IN(in0) * RECIP_SAMPLE_RATE;
#define SYNCSQUARE_PWM_A pwm  = CLAMP(UGEN_IN(in1),0,1) * 0.5;
#define SYNCSQUARE_SYNC_A sync = UGEN_IN(in2);

void syncsquare_kkk_calc(ugen u)
{
    SYNCSQUARE_CALC(
        SYNCSQUARE_FREQ_K
        SYNCSQUARE_PWM_K
        SYNCSQUARE_SYNC_K,
        /* no audio args */
    )
}

void syncsquare_akk_calc(ugen u)
{
    SYNCSQUARE_CALC(
        SYNCSQUARE_PWM_K
        SYNCSQUARE_SYNC_K,
        SYNCSQUARE_FREQ_A
    )
}

void syncsquare_kak_calc(ugen u)
{
    SYNCSQUARE_CALC(
        SYNCSQUARE_FREQ_K
        SYNCSQUARE_SYNC_K,
        SYNCSQUARE_PWM_A
    )
}

void syncsquare_aak_calc(ugen u)
{
    SYNCSQUARE_CALC(
        SYNCSQUARE_SYNC_K,
        SYNCSQUARE_FREQ_A
        SYNCSQUARE_PWM_A
    )
}

void syncsquare_kka_calc(ugen u)
{
    SYNCSQUARE_CALC(
        SYNCSQUARE_FREQ_K
        SYNCSQUARE_PWM_K,
        SYNCSQUARE_SYNC_A
    )
}

void syncsquare_aka_calc(ugen u)
{
    SYNCSQUARE_CALC(
        SYNCSQUARE_PWM_K,
        SYNCSQUARE_FREQ_A
        SYNCSQUARE_SYNC_A
    )
}

void syncsquare_kaa_calc(ugen u)
{
    SYNCSQUARE_CALC(
        SYNCSQUARE_FREQ_K,
        SYNCSQUARE_PWM_A
        SYNCSQUARE_SYNC_A
    )
}

void syncsquare_aaa_calc(ugen u)
{
    SYNCSQUARE_CALC(
        /* no control args */,
        SYNCSQUARE_FREQ_A
        SYNCSQUARE_PWM_A
        SYNCSQUARE_SYNC_A
    )
}

// syncosc

#define SYNCOSC_CALC(CONTROL_ARGS, AUDIO_ARGS)                                              \
double* in0 = UGEN_INPUT_BUFFER(u, 0);                                                      \
double* in1 = UGEN_INPUT_BUFFER(u, 1);                                                      \
double* in2 = UGEN_INPUT_BUFFER(u, 2);                                                      \
double* in3 = UGEN_INPUT_BUFFER(u, 3);                                                      \
double* out = UGEN_OUTPUT_BUFFER(u, 0);                                                     \
minblep mb  = *((minblep*) u.data);                                                         \
double slaveFreq;                                                                           \
double slaveWave;                                                                           \
double pwm;                                                                                 \
double masterFreq;                                                                          \
double y;                                                                                   \
double freqN;                                                                               \
CONTROL_ARGS                                                                                \
AUDIO_LOOP(                                                                                 \
    AUDIO_ARGS                                                                              \
                                                                                            \
    /* create waveform */                                                                   \
    mb.phase       = mb.phase + freqN;                                                      \
    mb.masterPhase = WRAP(mb.masterPhase + (masterFreq * RECIP_SAMPLE_RATE) * 1.0,1.0);     \
                                                                                            \
    /* add BLEP at end of waveform */                                                       \
    if (mb.phase >= 1)                                                                      \
    {                                                                                       \
        mb.phase  = mb.phase - 1.0;                                                         \
        mb.output = -1.0;                                                                   \
        add_blep(&mb, mb.phase/freqN,1.0);                                                  \
    }                                                                                       \
                                                                                            \
    /* add BLEP in middle of wavefor for squarewave */                                      \
    else if (slaveWave && !mb.output && mb.phase > pwm)                                     \
    {                                                                                       \
        mb.output = 1.0;                                                                    \
        add_blep(&mb, (mb.phase - pwm) / freqN,-1.0);                                       \
    }                                                                                       \
                                                                                            \
    else if (mb.prevSyncAmp <= 0 && mb.masterPhase > 0)                                     \
    {                                                                                       \
        mb.phase  = mb.masterPhase * (slaveFreq / masterFreq);                              \
        if (!slaveWave)                                                                     \
            mb.output = mb.masterPhase * (slaveFreq / masterFreq);                          \
        else                                                                                \
            mb.output = -1.0;                                                               \
        add_blep(&mb, mb.phase/freqN,1.0);                                                  \
    }                                                                                       \
                                                                                            \
    if (!slaveWave)                                                                         \
        y = mb.phase;                                                                       \
    else                                                                                    \
        y = mb.output;                                                                      \
                                                                                            \
    /* add BLEP buffer contents */                                                          \
    if (mb.nInit)                                                                           \
    {                                                                                       \
        y += mb.buffer[mb.iBuffer];                                                         \
        mb.nInit--;                                                                         \
        if (++mb.iBuffer >= mb.cBuffer)                                                     \
            mb.iBuffer=0;                                                                   \
    }                                                                                       \
    mb.prevSyncAmp = mb.masterPhase;                                                        \
    UGEN_OUT(out, y);                                                                       \
);                                                                                          \
*((minblep*) u.data) = mb;                                                                  \

#define SYNCOSC_0K slaveFreq  = in0[0]; freqN = slaveFreq * RECIP_SAMPLE_RATE;
#define SYNCOSC_1K slaveWave  = (int32_t)in1[0];
#define SYNCOSC_2K pwm        = CLAMP(in2[0],0,1) * 0.5;
#define SYNCOSC_3K masterFreq = in3[0];

#define SYNCOSC_0A slaveFreq  = UGEN_IN(in0); freqN = slaveFreq * RECIP_SAMPLE_RATE;
#define SYNCOSC_1A slaveWave  = (int32_t)UGEN_IN(in1);
#define SYNCOSC_2A pwm        = CLAMP(UGEN_IN(in2),0,1) * 0.5;
#define SYNCOSC_3A masterFreq = UGEN_IN(in3);

// syncosc 0
void syncosc_kkkk_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K
        SYNCOSC_1K
        SYNCOSC_2K
        SYNCOSC_3K,
        /* no audio args */
    )
}

// syncosc 1
void syncosc_akkk_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_1K
        SYNCOSC_2K
        SYNCOSC_3K,
        SYNCOSC_0A
    )
}

// syncosc 2
void syncosc_kakk_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K
        SYNCOSC_2K
        SYNCOSC_3K,
        SYNCOSC_1A
    )
}

// syncosc 3
void syncosc_aakk_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_2K
        SYNCOSC_3K,
        SYNCOSC_0A
        SYNCOSC_1A
    )
}

// syncosc 4
void syncosc_kkak_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K
        SYNCOSC_1K
        SYNCOSC_3K,
        SYNCOSC_2A
    )
}

// syncosc 5
void syncosc_akak_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_1K
        SYNCOSC_3K,
        SYNCOSC_0A
        SYNCOSC_2A
    )
}

// syncosc 6
void syncosc_kaak_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K
        SYNCOSC_3K,
        SYNCOSC_1A
        SYNCOSC_2A
    )
}

// syncosc 7
void syncosc_aaak_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_3K,
        SYNCOSC_0A
        SYNCOSC_1A
        SYNCOSC_2A
    )
}

// syncosc 8
void syncosc_kkka_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K
        SYNCOSC_1K
        SYNCOSC_2K,
        SYNCOSC_3A
    )
}

// syncosc 9
void syncosc_akka_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_1K
        SYNCOSC_2K,
        SYNCOSC_0A
        SYNCOSC_3A
    )
}

// syncosc 10
void syncosc_kaka_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K
        SYNCOSC_2K,
        SYNCOSC_1A
        SYNCOSC_3A
    )
}

// syncosc 11
void syncosc_aaka_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_2K,
        SYNCOSC_0A
        SYNCOSC_1A
        SYNCOSC_3A
    )
}

// syncosc 12
void syncosc_kkaa_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K
        SYNCOSC_1K,
        SYNCOSC_2A
        SYNCOSC_3A
    )
}

// syncosc 13
void syncosc_akaa_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_1K,
        SYNCOSC_0A
        SYNCOSC_2A
        SYNCOSC_3A
    )
}

// syncosc 14
void syncosc_kaaa_calc(ugen u)
{
    SYNCOSC_CALC(
        SYNCOSC_0K,
        SYNCOSC_1A
        SYNCOSC_2A
        SYNCOSC_3A
    )
}

// syncosc 15
void syncosc_aaaa_calc(ugen u)
{
    SYNCOSC_CALC(
        /* no control args */,
        SYNCOSC_0A
        SYNCOSC_1A
        SYNCOSC_2A
        SYNCOSC_3A
    )
}
