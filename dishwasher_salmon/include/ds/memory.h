#ifndef DS_MEMORY_H
#define DS_MEMORY_H

#include "ds/common.h"

#include <stdlib.h>

#define DS_ARRAY_REALLOC(type, ptr, new_count) \
    ds_reallocate(ptr, sizeof(type) * new_count)

#define DS_ARRAY_FREE(type, ptr) \
    ds_reallocate(ptr, 0)

static inline size_t ds_grow_capacity(size_t old)
{
    return old < 8 ? 8 : old * 1.5;
}

static inline void* ds_reallocate(void *ptr, size_t new_size)
{
    if (!new_size) {
        free(ptr);
        return NULL;
    }

    void *ret = realloc(ptr, new_size);
    if (!ret)
        DS_PANIC("(re)allocation failed");
    return ret;
}

#endif
