#ifndef DS_BYTECODE_H
#define DS_BYTECODE_H

#include "ds/common.h"
#include "ds/memory.h"
#include "ds/value.h"
#include "ds/vector.h"

typedef enum ds_opcode {
    OP_CONST,
    OP_RETURN,
} ds_opcode;

typedef struct ds_line_rle {
    uint16_t line;
    uint16_t repeat;
} ds_line_rle;

DS_VECTOR_DEFINE(ds_line_rle)

typedef struct ds_chunk {
    ds_vector_uint8_t code;
    ds_vector_ds_line_rle lines;
    ds_vector_ds_value consts;
} ds_chunk;

static inline void ds_chunk_init(ds_chunk *chunk)
{
    ds_vector_uint8_t_init(&chunk->code);
    ds_vector_ds_line_rle_init(&chunk->lines);
    ds_vector_ds_value_init(&chunk->consts);
}

static inline void ds_chunk_free(ds_chunk* chunk)
{
    ds_vector_uint8_t_free(&chunk->code);
    ds_vector_ds_line_rle_free(&chunk->lines);
    ds_vector_ds_value_free(&chunk->consts);
}

void ds_chunk_write(ds_chunk *chunk, uint8_t byte, uint16_t line);

static inline uint8_t ds_chunk_add_const(ds_chunk *chunk, ds_value value)
{
    if (chunk->consts.count == UINT8_MAX)
        DS_PANIC("too many consts");
    ds_vector_ds_value_append(&chunk->consts, value);
    return chunk->consts.count - 1;
}

uint16_t ds_instruction_line(const ds_chunk *chunk, size_t ip);

#endif
