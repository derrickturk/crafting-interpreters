#ifndef DS_VECTOR_H
#define DS_VECTOR_H

#include "ds/common.h"
#include "ds/memory.h"

static inline size_t ds_grow_capacity(size_t old)
{
    return old < 8 ? 8 : old * 1.5;
}

#define DS_VECTOR_DEFINE(ty) \
typedef struct ds_vector_##ty { \
    size_t capacity; \
    size_t count; \
    ty *data; \
} ds_vector_##ty; \
\
static inline void ds_vector_##ty##_init(ds_vector_##ty *vec) \
{ \
    vec->capacity = 0; \
    vec->count = 0; \
    vec->data = NULL; \
} \
\
static inline void ds_vector_##ty##_free(ds_vector_##ty *vec) \
{ \
    ds_reallocate(vec->data, 0); \
} \
\
static inline void ds_vector_##ty##_append(ds_vector_##ty *vec, ty value) \
{ \
    if (vec->count >= vec->capacity) { \
        size_t new_capacity = ds_grow_capacity(vec->capacity); \
        vec->data = ds_reallocate(vec->data, sizeof(ty) * new_capacity); \
        vec->capacity = new_capacity; \
    } \
    vec->data[vec->count++] = value; \
}

DS_VECTOR_DEFINE(uint8_t)
DS_VECTOR_DEFINE(uint16_t)

#endif
