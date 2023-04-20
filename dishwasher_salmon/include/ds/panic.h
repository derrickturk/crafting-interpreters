#ifndef DS_PANIC_H
#define DS_PANIC_H

#include <stdio.h>
#include <stdlib.h>

#define DS_STRINGIZE1(x) #x
#define DS_STRINGIZE(x) DS_STRINGIZE1(x)
#define DS_PANIC(msg) \
    ds_panic(__FILE__ ", line " DS_STRINGIZE(__LINE__) ": " msg)

_Noreturn static inline void ds_panic(const char *msg)
{
    fprintf(stderr, "panic: %s\n", msg);
    exit(EXIT_FAILURE);
}

#endif
