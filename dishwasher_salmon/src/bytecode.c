#include "ds/bytecode.h"
#include "ds/vector.h"

static void record_instr_line(ds_vector_ds_line_rle *lines, uint16_t line);

void ds_chunk_write(ds_chunk *chunk, uint8_t byte, uint16_t line)
{
    ds_vector_uint8_t_append(&chunk->code, byte);
    record_instr_line(&chunk->lines, line);
}

uint16_t ds_instruction_line(const ds_chunk *chunk, size_t ip)
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
