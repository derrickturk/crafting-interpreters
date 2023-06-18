#include <stddef.h>

#include "ds/common.h"
#include "ds/lexer.h"

ds_token ds_lexer_next(ds_lexer *lexer)
{
    return (ds_token) { DS_TOKEN_ERROR, NULL, 0, 0 };
}
