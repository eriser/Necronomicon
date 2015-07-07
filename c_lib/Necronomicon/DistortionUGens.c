/*
  Necronomicon - Deterministic Audio Engine
  Copyright 2014 - Chad McKinney and Curtis McKinney
*/

#include <stdlib.h>
#include <math.h>

#include "../Necronomicon.h"
#include "UGenUtil.h"

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
