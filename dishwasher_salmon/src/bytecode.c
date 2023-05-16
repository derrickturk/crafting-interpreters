#include "ds/bytecode.h"
#include "ds/vector.h"

static void record_instr_line(ds_vector_ds_line_rle *lines, uint16_t line);

void ds_chunk_write(ds_chunk *chunk, uint8_t byte, uint16_t line)
{
    ds_vector_uint8_t_append(&chunk->code, byte);
    record_instr_line(&chunk->lines, line);
}

void ds_chunk_write_const(ds_chunk *chunk, ds_value value, uint16_t line)
{
    if (chunk->consts.count <= UINT8_MAX) {
        uint8_t ix = ds_vector_ds_value_append(&chunk->consts, value);
        ds_vector_uint8_t_append(&chunk->code, OP_CONST);
        ds_vector_uint8_t_append(&chunk->code, ix);
    } else {
        size_t ix = ds_vector_ds_value_append(&chunk->consts, value);
        if (ix > 0xffffff)
            DS_PANIC("too many constants");
        ds_vector_uint8_t_append(&chunk->code, OP_CONST_LONG);
        ds_vector_uint8_t_append(&chunk->code, ix & 0x0000ff);
        ds_vector_uint8_t_append(&chunk->code, (ix & 0x00ff00) >> 8);
        ds_vector_uint8_t_append(&chunk->code, (ix & 0xff0000) >> 16);
    }
    record_instr_line(&chunk->lines, line);
}

uint16_t ds_chunk_instruction_line(const ds_chunk *chunk, size_t ip)
{
    size_t line_ip = 0;
    for (size_t i = 0; i < chunk->lines.count; ++i) {
        line_ip += chunk->lines.data[i].repeat;
        if (line_ip > ip)
            return chunk->lines.data[i].line;
    }
    return 0;
}

static void record_instr_line(ds_vector_ds_line_rle *lines, uint16_t line)
{
    if (lines->count == 0 || lines->data[lines->count - 1].line != line)
        ds_vector_ds_line_rle_append(lines, (ds_line_rle) { line, 1 });
    else
        ++lines->data[lines->count - 1].repeat;
}
