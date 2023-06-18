#ifndef DS_LEXER_H
#define DS_LEXER_H

#include "ds/common.h"

// TODO: consider making all these guys abstract/opaque...
typedef struct ds_lexer {
    const char *start;
    const char *current;
    uint16_t line;
} ds_lexer;

typedef enum ds_token_type {
    DS_TOKEN_ERROR, DS_TOKEN_EOF,
    DS_TOKEN_LPAREN, DS_TOKEN_RPAREN,
    DS_TOKEN_LBRACE, DS_TOKEN_RBRACE,
    DS_TOKEN_COMMA, DS_TOKEN_DOT, DS_TOKEN_SEMICOLON,
    DS_TOKEN_PLUS, DS_TOKEN_MINUS, DS_TOKEN_STAR, DS_TOKEN_SLASH,
    DS_TOKEN_EQ, DS_TOKEN_EQ_EQ, DS_TOKEN_EXCL, DS_TOKEN_EXCL_EQ,
    DS_TOKEN_GT, DS_TOKEN_GT_EQ, DS_TOKEN_LT, DS_TOKEN_LT_EQ,
    DS_TOKEN_IDENT,
    DS_TOKEN_LIT_STR, DS_TOKEN_LIT_NUM,
    DS_TOKEN_AND, DS_TOKEN_CLASS, DS_TOKEN_ELSE, DS_TOKEN_FALSE, DS_TOKEN_FOR,
    DS_TOKEN_FUN, DS_TOKEN_IF, DS_TOKEN_NIL, DS_TOKEN_OR, DS_TOKEN_PRINT,
    DS_TOKEN_RETURN, DS_TOKEN_SUPER, DS_TOKEN_THIS, DS_TOKEN_TRUE,
    DS_TOKEN_VAR, DS_TOKEN_WHILE,
} ds_token_type;

typedef struct ds_token {
    ds_token_type type;
    const char *start;
    uint16_t length;
    uint16_t line;
} ds_token;

static inline void ds_lexer_init(ds_lexer *lexer, const char *text)
{
    lexer->start = lexer->current = text;
    lexer->line = 0;
}

ds_token ds_lexer_next(ds_lexer *lexer);

#endif
