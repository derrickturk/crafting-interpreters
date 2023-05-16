#include "ds/debug.h"

#include <stdio.h>

static size_t simple_instr(const char *name, size_t ip);
static size_t const_instr(const char *name, const ds_chunk *chunk, size_t ip);

void ds_disassemble_chunk(const ds_chunk *chunk, const char *name)
{
    printf("== %s ==\n", name);
    for (size_t ip = 0; ip < chunk->code.count;)
        ip = ds_disassemble_instr(chunk, ip);
}

size_t ds_disassemble_instr(const ds_chunk *chunk, size_t ip)
{
    printf("%04zu ", ip);
    uint16_t line = ds_instruction_line(chunk, ip);
    if (ip > 0 && line == ds_instruction_line(chunk, ip - 1))
        printf("   | ");
    else
        printf("%4d ", line);
    switch (chunk->code.data[ip]) {
        // TODO: macrotize these...
        case OP_CONST:
            return const_instr("OP_CONST", chunk, ip);
        case OP_RETURN:
            return simple_instr("OP_RETURN", ip);
        default:
            DS_PANIC("invalid instruction");
    }
}

static inline size_t simple_instr(const char *name, size_t ip)
{
    printf("%s\n", name);
    return ip + 1;
}

static inline size_t const_instr(const char *name,
        const ds_chunk *chunk, size_t ip)
{
    uint8_t i = chunk->code.data[ip + 1];
    printf("%-16s %4d '", name, i);
    ds_value_print(chunk->consts.data[i]);
    printf("'\n");
    return ip + 2;
}
