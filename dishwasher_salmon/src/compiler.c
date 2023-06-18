#include "ds/compiler.h"
#include "ds/lexer.h"

ds_compiler_result ds_compile(const char *text, ds_chunk *chunk)
{
    ds_lexer lexer;
    ds_lexer_init(&lexer, text);
    for (;;) {
        ds_token token = ds_lexer_next(&lexer);
        if (token.type == DS_TOKEN_EOF)
            break;
        if (token.type == DS_TOKEN_ERROR)
            return DS_COMPILER_LEXER_ERROR;
    }
    return DS_COMPILER_OK;
}
