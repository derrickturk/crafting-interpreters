#ifndef LOX_MEMORY_H
#define LOX_MEMORY_H

#include "common.h"

#include <stdlib.h>

#define MEM_ARRAY_REALLOC(type, ptr, new_count) \
    mem_reallocate(ptr, sizeof(type) * new_count)

#define MEM_ARRAY_FREE(type, ptr) \
    mem_reallocate(ptr, 0)

static inline size_t mem_grow_capacity(size_t old)
{
    return old * 1.5;
}

static inline void* mem_reallocate(void* ptr, size_t new_size)
{
    if (!new_size) {
        free(ptr);
        return NULL;
    }

    void* ret = realloc(ptr, new_size);
    if (!ret)
        LOX_PANIC("(re)allocation failed");
    return ret;
}

#endif
