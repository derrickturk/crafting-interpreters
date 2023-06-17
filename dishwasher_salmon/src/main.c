#include "ds/common.h"
#include "ds/bytecode.h"
#include "ds/debug.h"
#include "ds/vm.h"

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    ds_vm vm;
    ds_vm_init(&vm);

    ds_chunk c;
    ds_chunk_init(&c);

    ds_chunk_write_const(&c, (ds_value){ .d = 1.2 }, 1);
    ds_chunk_write_const(&c, (ds_value){ .d = 3.4 }, 2);
    ds_chunk_write(&c, DS_OP_ADD, 2);
    ds_chunk_write_const(&c, (ds_value){ .d = 5.6 }, 3);
    ds_chunk_write(&c, DS_OP_DIVIDE, 3);
    ds_chunk_write(&c, DS_OP_NEGATE, 3);
    ds_chunk_write(&c, DS_OP_RETURN, 3);

    ds_disassemble_chunk(&c, "test chunk");

    if (ds_vm_interpret(&vm, &c) != DS_VM_OK) {
        fprintf(stderr, "error from VM\n");
    }

    ds_chunk_free(&c);

    ds_vm_free(&vm);

    return 0;
}
