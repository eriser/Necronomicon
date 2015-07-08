/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#ifndef NECRONOMICON_DELAY_H_INCLUDED
#define NECRONOMICON_DELAY_H_INCLUDED

typedef struct
{
    sample_buffer* buffer;
    double max_delay_time;
    int64_t write_index;
} delay_data;

extern const uint32_t DELAY_DATA_SIZE;

#endif // NECRONOMICON_DELAY_H_INCLUDED
