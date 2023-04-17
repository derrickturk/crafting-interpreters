#ifndef LOX_PANIC_H
#define LOX_PANIC_H

#include <stdio.h>
#include <stdlib.h>

#define LOX_STRINGIZE1(x) #x
#define LOX_STRINGIZE(x) LOX_STRINGIZE1(x)
#define LOX_PANIC(msg) \
    lox_panic(__FILE__ ", line " LOX_STRINGIZE(__LINE__) ": " msg)

_Noreturn static inline void lox_panic(const char* msg)
{
    fprintf(stderr, "panic: %s\n", msg);
    exit(EXIT_FAILURE);
}

#endif
