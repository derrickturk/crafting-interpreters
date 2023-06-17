#include "ds/debug.h"

#include <stdio.h>

static size_t simple_instr(const char *name, const ds_chunk *chunk, size_t ip);
static size_t const_instr(const char *name, const ds_chunk *chunk, size_t ip);
static size_t const_long_instr(const char *name,
        const ds_chunk *chunk, size_t ip);

void ds_disassemble_chunk(const ds_chunk *chunk, const char *name)
{
    printf("== %s ==\n", name);
    for (size_t ip = 0; ip < chunk->code.count;)
        ip = ds_disassemble_instr(chunk, ip);
}

size_t ds_disassemble_instr(const ds_chunk *chunk, size_t ip)
{
    printf("%04zu ", ip);
    uint16_t line = ds_chunk_instr_line(chunk, ip);
    if (ip > 0 && line == ds_chunk_instr_line(chunk, ip - 1))
        printf("   | ");
    else
        printf("%4d ", line);

#define HANDLE(instr, handler) \
    case instr: \
        return handler(#instr, chunk, ip);

    switch (chunk->code.data[ip]) {
        HANDLE(DS_OP_CONST, const_instr)
        HANDLE(DS_OP_CONST_LONG, const_long_instr)
        HANDLE(DS_OP_NEGATE, simple_instr)
        HANDLE(DS_OP_RETURN, simple_instr)
        default:
            DS_PANIC("invalid instruction");
    }

#undef HANDLE
}

static inline size_t simple_instr(const char *name,
        const ds_chunk *chunk, size_t ip)
{
    (void)chunk; // unused
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

static inline size_t const_long_instr(const char *name,
        const ds_chunk *chunk, size_t ip)
{
    uint8_t i_low = chunk->code.data[ip + 1];
    uint8_t i_mid = chunk->code.data[ip + 2];
    uint8_t i_high = chunk->code.data[ip + 3];
    size_t i = i_low | i_mid << 8 | i_high << 16;
    printf("%-16s %4zu '", name, i);
    ds_value_print(chunk->consts.data[i]);
    printf("'\n");
    return ip + 4;
}
