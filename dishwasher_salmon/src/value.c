#include "ds/value.h"

#include <stdio.h>

void ds_value_print(ds_value value)
{
    printf("%g", value.d);
}
