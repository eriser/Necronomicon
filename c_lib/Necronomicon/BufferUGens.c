/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <sndfile.h>

#include "../Necronomicon.h"
#include "Containers/HashTable.h"
#include "UGenUtil.h"

// Use haskell threads to do actual loading
// Spawn a thread that reads the sound file into a raw double*
// Have it send a message to the nrt thread with the name/double* pair to for storing in the sample_hash_table
// The RT thread never allocates/deallocates or accesses the sample_hash_table

const int32_t SAMPLE_HASH_TABLE_SIZE = 2048;

hash_table sample_hash_table;

void create_sample_hash_table()
{
    sample_hash_table = hash_table_new(SAMPLE_HASH_TABLE_SIZE);
}

void free_sample_buffer(void* void_sample_buffer)
{
    if (void_sample_buffer != NULL)
    {
        sample_buffer* buffer = (sample_buffer*) void_sample_buffer;
        if (buffer->samples != NULL)
            free(buffer->samples);

        free(buffer);
    }
}

void free_sample_hash_table()
{
    hash_table_free_with_callback(sample_hash_table, free_sample_buffer);
}

// expects a null terminated string
void register_sample_buffer(const char* file_path, sample_buffer* buffer)
{
    hash_table_insert_string_key(sample_hash_table, (void*) buffer, file_path);
}

sample_buffer* retrieve_sample_buffer(const char* file_path)
{
    return (sample_buffer*) hash_table_lookup_string_key(sample_hash_table, file_path);
}

sample_buffer* load_sample_into_buffer(const char* file_path)
{
    sample_buffer* buffer;
    memset(buffer, 0, SAMPLE_BUFFER_SIZE);

    SF_INFO sfinfo;
    memset(&sfinfo, 0, sizeof(sfinfo)); // zero initialize

    SNDFILE* sndfile = sf_open(file_path, SFM_READ, &sfinfo);
    if (sndfile)
    {
        const sf_count_t items = sfinfo.frames * (sf_count_t) sfinfo.channels;
        buffer->samples = calloc(items, DOUBLE_SIZE);
        buffer->num_samples = items;
        sf_read_double(sndfile, buffer->samples, items);

        if (sf_close(sndfile) != 0)
            printf("Error closing sound file %s\n", file_path);
    }

    else
    {
        printf("Error opening sound file %s\n", file_path);
    }

    return buffer;
}
