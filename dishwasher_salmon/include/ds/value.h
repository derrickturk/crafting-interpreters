#ifndef DS_VALUE_H
#define DS_VALUE_H

#include "ds/common.h"
#include "ds/memory.h"

typedef struct ds_value { double d; } ds_value;

void ds_value_print(ds_value value);

typedef struct ds_value_array {
    size_t capacity;
    size_t count;
    ds_value *values;
} ds_value_array;

static inline void ds_value_array_init(ds_value_array *arr)
{
    arr->capacity = 0;
    arr->count = 0;
    arr->values = NULL;
}

static inline void ds_value_array_write(ds_value_array *arr, ds_value value)
{
    if (arr->count + 1 > arr->capacity) {
        size_t new_capacity = ds_grow_capacity(arr->capacity);
        arr->values = DS_ARRAY_REALLOC(ds_value,
          arr->values, new_capacity);
        arr->capacity = new_capacity;
    }
    arr->values[arr->count++] = value;
}

static inline void ds_value_array_free(ds_value_array *arr)
{
    DS_ARRAY_FREE(ds_value, arr->values);
    ds_value_array_init(arr);
}

#endif
