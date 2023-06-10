#ifndef DS_VM_H
#define DS_VM_H

#include "ds/common.h"
#include "ds/bytecode.h"
#include "ds/value.h"

typedef enum ds_vm_result {
    DS_VM_OK,
    DS_VM_COMPILE_ERROR,
    DS_VM_RUNTIME_ERROR,
} ds_vm_result;

enum {
    DS_VM_STACK_MAX = 256,
};

typedef struct ds_vm {
    const ds_chunk *chunk;
    const uint8_t *ip;
    ds_value stack[DS_VM_STACK_MAX];
    ds_value* stack_top;
} ds_vm;

static inline void ds_vm_init(ds_vm *vm)
{
    vm->chunk = NULL;
    vm->ip = NULL;
    vm->stack_top = &vm->stack[0];
}

static inline void ds_vm_free(ds_vm *vm)
{
    (void)vm;
}

ds_vm_result ds_vm_run(ds_vm *vm);

static inline ds_vm_result ds_vm_interpret(ds_vm *vm, const ds_chunk *chunk)
{
    vm->chunk = chunk;
    vm->ip = chunk->code.data;
    return ds_vm_run(vm);
}

// TODO: make this safe after we're sure it's sticking around
static inline void ds_vm_stack_push(ds_vm *vm, ds_value value)
{
    *vm->stack_top++ = value;
}

// TODO: make this safe after we're sure it's sticking around
static inline ds_value ds_vm_stack_pop(ds_vm *vm)
{
    return *--vm->stack_top;
}

#endif
