#ifndef DS_MEMORY_H
#define DS_MEMORY_H

#include "ds/common.h"

#include <stdlib.h>

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
