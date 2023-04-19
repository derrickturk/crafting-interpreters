#include "ds/common.h"
#include "ds/bytecode.h"
#include "ds/debug.h"

int main(int argc, char* argv[])
{
    ds_chunk c;
    ds_chunk_init(&c);
    ds_chunk_write(&c, OP_RETURN);
    ds_disassemble_chunk(&c, "test chunk");
    return 0;
}
