#ifndef DS_BYTECODE_H
#define DS_BYTECODE_H

#include "ds/common.h"
#include "ds/memory.h"

typedef enum ds_opcode {
    OP_RETURN,
} opcode;

typedef struct ds_chunk {
    size_t capacity;
    size_t count;
    uint8_t* code;
} ds_chunk;

static inline void ds_chunk_init(ds_chunk* chunk)
{
    chunk->capacity = 0;
    chunk->count = 0;
    chunk->code = NULL;
}

static inline void ds_chunk_write(ds_chunk* chunk, uint8_t byte)
{
    if (chunk->count + 1 > chunk->capacity) {
        size_t new_capacity = ds_grow_capacity(chunk->capacity);
        chunk->code = MEM_ARRAY_REALLOC(uint8_t, chunk->code, new_capacity);
        chunk->capacity = new_capacity;
    }

    chunk->code[chunk->count++] = byte;
}

#endif
