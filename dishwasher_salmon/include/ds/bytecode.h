#ifndef DS_BYTECODE_H
#define DS_BYTECODE_H

#include "ds/common.h"
#include "ds/memory.h"
#include "ds/value.h"

typedef enum ds_opcode {
    OP_CONST,
    OP_RETURN,
} opcode;

typedef struct ds_chunk {
    size_t capacity;
    size_t count;
    uint8_t *code;
    uint16_t *lines;
    ds_value_array consts;
} ds_chunk;

static inline void ds_chunk_init(ds_chunk *chunk)
{
    chunk->capacity = 0;
    chunk->count = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    ds_value_array_init(&chunk->consts);
}

static inline void ds_chunk_write(ds_chunk *chunk, uint8_t byte, uint16_t line)
{
    if (chunk->count + 1 > chunk->capacity) {
        size_t new_capacity = ds_grow_capacity(chunk->capacity);
        chunk->code = DS_ARRAY_REALLOC(uint8_t, chunk->code, new_capacity);
        chunk->lines = DS_ARRAY_REALLOC(uint16_t, chunk->lines, new_capacity);
        chunk->capacity = new_capacity;
    }
    chunk->lines[chunk->count] = line;
    chunk->code[chunk->count++] = byte;
}

static inline void ds_chunk_free(ds_chunk* chunk)
{
    DS_ARRAY_FREE(uint8_t, chunk->code);
    DS_ARRAY_FREE(uint16_t, chunk->lines);
    ds_value_array_free(&chunk->consts);
    ds_chunk_init(chunk);
}

static inline uint8_t ds_chunk_add_const(ds_chunk *chunk, ds_value value)
{
    if (chunk->consts.count == UINT8_MAX)
        DS_PANIC("too many consts");
    ds_value_array_write(&chunk->consts, value);
    return chunk->consts.count - 1;
}

#endif
