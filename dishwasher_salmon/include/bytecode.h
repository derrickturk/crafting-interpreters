#ifndef LOX_BYTECODE_H
#define LOX_BYTECODE_H

#include "common.h"
#include "memory.h"

typedef enum opcode {
    OP_RETURN,
} opcode;

typedef struct chunk {
    size_t capacity;
    size_t count;
    uint8_t* code;
} chunk;

static inline void chunk_init(chunk* chunk)
{
    chunk->capacity = 0;
    chunk->count = 0;
    chunk->code = NULL;
}

static inline void chunk_write(chunk* chunk, uint8_t byte)
{
    if (chunk->count + 1 > chunk->capacity) {
        size_t new_capacity = mem_grow_capacity(chunk->capacity);
        chunk->code = MEM_ARRAY_REALLOC(uint8_t, chunk->code, new_capacity);
        chunk->capacity = new_capacity;
    }

    chunk->code[chunk->count++] = byte;
}

#endif
