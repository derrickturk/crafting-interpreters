#ifndef DS_DEBUG_H
#define DS_DEBUG_H

#include "ds/common.h"
#include "ds/bytecode.h"

void ds_disassemble_chunk(const ds_chunk*, const char* name);
size_t ds_disassemble_instr(const ds_chunk*, size_t ip);

#endif
