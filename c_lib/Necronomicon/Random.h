/*
  Necronomicon - Deterministic Audio Engine
  Copyright 2014 - Chad McKinney and Curtis McKinney
*/

#ifndef NECRONOMICON_RANDOM_H_INCLUDED
#define NECRONOMICON_RANDOM_H_INCLUDED

#include <stdint.h>

static inline uint32_t xorshift128()
{
    static uint32_t xor_x = 1243598713U;
    static uint32_t xor_y = 3093459404U;
    static uint32_t xor_z = 1821928721U;
    static uint32_t xor_w = 1791912391U;

    uint32_t t = xor_x ^ (xor_x << 11);
    xor_x = xor_y; xor_y = xor_z; xor_z = xor_w;
    return xor_w = xor_w ^ (xor_w >> 19) ^ t ^ (t >> 8);
}

static inline uint64_t xorshift128plus()
{
    static uint64_t sx = 18446744073709551557UL;
    static uint64_t sy = 12764787846358441471UL;
    uint64_t x = sx;
    uint64_t const y = sy;
    sx = y;
    x ^= x << 23; // a
    x ^= x >> 17; // b
    x ^= y ^ (y >> 26); // c
    sy = x;
    return x + y;
}


static inline uint32_t trand()
{
    static uint32_t s1 = 1243598713U;
    static uint32_t s2 = 3093459404U;
    static uint32_t s3 = 1821928721U;
    // static uint32_t seed = 1791912391U;

    s1 = ((s1 & (uint32_t) - 2)  << 12) ^ (((s1 << 13) ^  s1) >> 19);
    s2 = ((s2 & (uint32_t) - 8)  <<  4) ^ (((s2 <<  2) ^  s2) >> 25);
    s3 = ((s3 & (uint32_t) - 16) << 17) ^ (((s3 <<  3) ^  s3) >> 11);
    return s1 ^ s2 ^ s3;
}

const long double RECIP_ULONG_MAX = (long double) 1.0 / (long double) ULONG_MAX;
const long double TWO_RECIP_ULONG_MAX = (long double) 2.0 / (long double) ULONG_MAX;
const long double QUARTER_RECIP_ULONG_MAX = (long double) 0.25 / (long double) ULONG_MAX;

#define RAND_RANGE(MIN,MAX) (((double) (((long double) xorshift128plus()) * RECIP_ULONG_MAX)) * (MAX - MIN) + MIN)
#define RAND_ONE() ((double) (((long double) xorshift128plus()) * RECIP_ULONG_MAX))
#define RAND_TWO() ((double) (((long double) xorshift128plus()) * TWO_RECIP_ULONG_MAX))
#define RAND_SIG() (((double) (((long double) xorshift128plus()) * TWO_RECIP_ULONG_MAX)) - 1.0)
#define RAND_SIG_EIGHTH() (((double) (((long double) xorshift128plus()) * QUARTER_RECIP_ULONG_MAX)) - 0.125)

#endif // NECRONOMICON_RANDOM_H_INCLUDED
