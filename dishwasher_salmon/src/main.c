#include "ds/common.h"
#include "ds/bytecode.h"
#include "ds/debug.h"

int main(int argc, char *argv[])
{
    ds_chunk c;
    ds_chunk_init(&c);
    ds_chunk_write(&c, OP_RETURN);
    uint8_t i = ds_chunk_add_const(&c, (ds_value){ .d = 37.2 });
    ds_chunk_write(&c, OP_CONST);
    ds_chunk_write(&c, i);
    ds_disassemble_chunk(&c, "test chunk");
    return 0;
}
