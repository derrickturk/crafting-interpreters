#ifndef LOX_DEBUG_H
#define LOX_DEBUG_H

#include "common.h"
#include "bytecode.h"

void dbg_disassemble_chunk(const chunk*, const char* name);
size_t dbg_disassemble_instr(const chunk*, size_t ip);

#endif
