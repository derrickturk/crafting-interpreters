#ifndef DS_VALUE_H
#define DS_VALUE_H

#include "ds/common.h"
#include "ds/vector.h"

typedef struct ds_value { double d; } ds_value;

DS_VECTOR(ds_value)

void ds_value_print(ds_value value);

#endif
