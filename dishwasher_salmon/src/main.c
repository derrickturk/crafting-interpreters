#include "ds/common.h"
#include "ds/bytecode.h"
#include "ds/debug.h"

static inline uint8_t ds_chunk_add_const(ds_chunk *chunk, ds_value value)
{
    if (chunk->consts.count >= UINT8_MAX) {
        DS_PANIC("too many consts");
    } else {
        ds_vector_ds_value_append(&chunk->consts, value);
    }
    return chunk->consts.count - 1;
}

int main(int argc, char *argv[])
{
    ds_chunk c;
    ds_chunk_init(&c);
    ds_chunk_write(&c, OP_RETURN, 1);
    ds_chunk_write_const(&c, (ds_value){ .d = 37.2 }, 1);
    for (size_t i = 0; i < UINT8_MAX + 3; ++i) {
        uint16_t line = 2 + i / 3;
        ds_chunk_write_const(&c, (ds_value){ .d = i * 0.1 }, line);
    }
    ds_disassemble_chunk(&c, "test chunk");
    return 0;
}
