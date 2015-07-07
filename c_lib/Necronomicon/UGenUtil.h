/*
  Necronomicon - Deterministic Audio Engine
  Copyright 2014 - Chad McKinney and Curtis McKinney
*/

#ifndef NECRONOMICON_UGEN_UTIL_H_INCLUDED
#define NECRONOMICON_UGEN_UTIL_H_INCLUDED

#define LERP(A,B,D) (A+D*(B-A))
#define likely(x)   __builtin_expect((x),1)
#define unlikely(x) __builtin_expect((x),0)

#define MAX(a, b) ((a > b) ? a : b)
#define MIN(a, b) ((a < b) ? a : b)

#define LINEAR_INTERP(A, B, DELTA) (A + DELTA * (B - A))

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

#endif // NECRONOMICON_UGEN_UTIL_H_INCLUDED
