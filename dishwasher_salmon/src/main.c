#include "common.h"
#include "bytecode.h"
#include "debug.h"

int main(int argc, char* argv[])
{
    chunk c;
    chunk_init(&c);
    chunk_write(&c, OP_RETURN);
    dbg_disassemble_chunk(&c, "test chunk");
    return 0;
}
