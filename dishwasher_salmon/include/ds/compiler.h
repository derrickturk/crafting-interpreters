#ifndef DS_COMPILER_H
#define DS_COMPILER_H

#include "ds/common.h"
#include "ds/bytecode.h"

typedef enum ds_compiler_result {
    DS_COMPILER_LEXER_ERROR,
    DS_COMPILER_PARSER_ERROR,
    DS_COMPILER_ANALYZER_ERROR,
    DS_COMPILER_OK,
} ds_compiler_result;

ds_compiler_result ds_compile(const char *text, ds_chunk *chunk);

#endif
