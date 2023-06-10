#include "ds/common.h"
#include "ds/bytecode.h"
#include "ds/value.h"
#include "ds/vm.h"

#ifdef DS_DEBUG_TRACE
#include <stdio.h>

#include "ds/debug.h"
#endif

inline static uint8_t fetch_code(ds_vm *vm)
{
    return *vm->ip++;
}

inline static ds_value fetch_const(ds_vm *vm)
{
    return vm->chunk->consts.data[fetch_code(vm)];
}

inline static ds_value fetch_const_long(ds_vm *vm)
{
    uint8_t i_low = fetch_code(vm);
    uint8_t i_mid = fetch_code(vm);
    uint8_t i_high = fetch_code(vm);
    size_t i = i_low | i_mid << 8 | i_high << 16;
    return vm->chunk->consts.data[i];
}

ds_vm_result ds_vm_run(ds_vm *vm)
{
    for (;;) {
#ifdef DS_DEBUG_TRACE
        printf("    ");
        for (ds_value *v = vm->stack; v < vm->stack_top; ++v) {
            printf("[ ");
            ds_value_print(*v);
            printf("] ");
        }
        printf("\n");
        ds_disassemble_instr(vm->chunk, vm->ip - vm->chunk->code.data);
#endif
        switch (fetch_code(vm)) {
            case DS_OP_RETURN:
                // TODO: temporary
                ds_value_print(ds_vm_stack_pop(vm));
                return DS_VM_OK;
            case DS_OP_CONST:
                {
                    ds_value v = fetch_const(vm);
                    ds_vm_stack_push(vm, v);
                    break;
                }
            case DS_OP_CONST_LONG:
                {
                    ds_value v = fetch_const_long(vm);
                    ds_vm_stack_push(vm, v);
                    break;
                }
        }
    }
}
