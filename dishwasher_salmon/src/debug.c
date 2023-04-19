#include "ds/debug.h"

#include <stdio.h>

static int simple_instr(const char* name, size_t ip);

void ds_disassemble_chunk(const ds_chunk* chunk, const char* name)
{
    printf("== %s ==\n", name);
    for (size_t ip = 0; ip < chunk->count;)
        ip = ds_disassemble_instr(chunk, ip);
}

size_t ds_disassemble_instr(const ds_chunk* chunk, size_t ip)
{
    switch (chunk->code[ip]) {
        // TODO: macrotize these...
        case OP_RETURN:
            return simple_instr("OP_RETURN", ip);
        default:
            DS_PANIC("invalid instruction");
    }
}

static inline int simple_instr(const char* name, size_t ip)
{
    printf("%s\n", name);
    return ip + 1;
}
