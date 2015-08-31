/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#ifndef NECRONOMICON_UGEN_UTIL_H_INCLUDED
#define NECRONOMICON_UGEN_UTIL_H_INCLUDED

#include <math.h>

#define LERP(A,B,D) (A+D*(B-A))
#define likely(x)   __builtin_expect((x),1)
#define unlikely(x) __builtin_expect((x),0)

#define MAX(a, b) ((a > b) ? a : b)
#define MIN(a, b) ((a < b) ? a : b)

#define LINEAR_INTERP(A, B, DELTA) (A + DELTA * (B - A))

// does range checking but not wrapping
inline double linear_interp_buffer(double* samples, long num_samples, long double read_index)
{
    double y = 0;
    if (read_index >= 0 && read_index < num_samples)
    {
        const long lread_index = (long) read_index;
        const long lread_index2 = read_index + 1;
        const double delta = read_index - (long double) lread_index;
        const double a = samples[lread_index];
        const double b = lread_index2 < num_samples ? samples[lread_index2] : 0;
        y = LINEAR_INTERP(a, b, delta);
    }

    else if(read_index > -1)
    {
        const double delta = 1 - fabs(read_index);
        const double a = 0;
        const double b = samples[0];
        y = LINEAR_INTERP(a, b, delta);
    }

    return y;
}

// buffer lookup with linear interpolation and wrapping (assuming non-power-of-two buffer sizes). Not very fast, use sparingly and only when this functionality is required
inline double linear_interp_buffer_with_index_wrap(double* samples, long num_samples, long double read_index)
{
    long lread_index = floor(read_index);
    while (lread_index < 0)
        lread_index = num_samples + lread_index;

    if (lread_index >= num_samples)
        lread_index = lread_index % num_samples;

    const long lread_index2 = read_index + 1;
    const double delta = read_index - (long double) lread_index;
    const double a = samples[lread_index];
    const double b = samples[lread_index2 % num_samples];
    return LINEAR_INTERP(a, b, delta);
}

#define fast_pow(U,BASE,EXPONENT)                                   \
U.d = BASE;                                                         \
U.x[1] = (int32_t)(EXPONENT * (U.x[1] - 1072632447) + 1072632447);  \
U.x[0] = 0;                                                         \
U.d;                                                                \

#define AMP2DB(amp) (log10(amp * 20.0))
#define DB2AMP(db) (pow(10, db / 20.0))

#define LAGRANGE_2ND_ORDER(x0, x1, y0, y1, x) ((x - x1) / (x0 - x1) * y0 + (x - x0) / (x1 - x0) * y1)

/*
#define CUBIC_INTERP(A, B, C, D, DELTA)                 \
({                                                      \
    const double delta2 = DELTA * DELTA;                \
    const double a0     = D - C - A + B;                \
    const double a1     = A - B - a0;                   \
    const double a2     = C - A;                        \
    a0 * DELTA * delta2 + a1 * delta2 + a2 * DELTA + B; \
})*/


#define CUBIC_INTERP(y0, y1, y2, y3, x)                     \
({                                                          \
    const double c0 = y1;                                   \
    const double c1 = 0.5 * (y2 - y0);                      \
    const double c2 = y0 - 2.5 * y1 + 2.0 * y2 - 0.5 * y3;  \
    const double c3 = 0.5 * (y3 - y0) + 1.5 * (y1 - y2);    \
    ((c3 * x + c2) * x + c1) * x + c0;                      \
})

#define CLAMP(V,MIN,MAX)                    \
({                                          \
    double result = V < MIN ? MIN : V;      \
    result        = V > MAX ? MAX : V;      \
    result;                                 \
})

#define WRAP(X,AMOUNT)              \
({                                  \
    double x   = X * AMOUNT;        \
    double ret = x;                 \
    if (x >= 1)                     \
        ret = x - 2;                \
    else if (x < -1)                \
        ret = x + 2;                \
    ret;                            \
})

#define ROUND(f) ((float)((f > 0.0) ? floor(f + 0.5) : ceil(f - 0.5)))

static inline double zapgremlins(double x)
{
    double absx = fabs(x);
    // very small numbers fail the first test, eliminating denormalized numbers
    //    (zero also fails the first test, but that is OK since it returns zero.)
    // very large numbers fail the second test, eliminating infinities
    // Not-a-Numbers fail both tests and are eliminated.
    return (absx > 1e-63 && absx < 1e63) ? x : 0.0;
}

static inline uint32_t next_power_of_two(uint32_t v)
{
    --v;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    return ++v;
}

typedef enum { false, true } bool;

#endif // NECRONOMICON_UGEN_UTIL_H_INCLUDED
