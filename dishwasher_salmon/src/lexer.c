#include <string.h>

#include "ds/common.h"
#include "ds/lexer.h"

static inline bool eof(const ds_lexer *lexer)
{
    return *lexer->current == '\0';
}

static inline ds_token make_token(const ds_lexer *lexer, ds_token_type type)
{
    return (ds_token) {
        type,
        lexer->start,
        lexer->current - lexer->start,
        lexer->line
    };
}

static inline ds_token make_error_token(const ds_lexer *lexer, const char* msg)
{
    return (ds_token) {
        DS_TOKEN_ERROR,
        msg,
        strlen(msg),
        lexer->line
    };
}

static inline char advance(ds_lexer *lexer)
{
    return *lexer->current++;
}

static inline bool match(ds_lexer *lexer, char what)
{
    if (eof(lexer))
        return false;
    if (*lexer->current != what)
        return false;
    advance(lexer);
    return true;
}

static inline char peek_next(ds_lexer *lexer)
{
    if (eof(lexer))
        return '\0';
    return lexer->current[1];
}

static inline void skip_whitespace(ds_lexer *lexer)
{
    for (;;) {
        switch (*lexer->current) {
            case '\n':
                ++lexer->line;
                // fallthrough
            case ' ':
            case '\t':
            case '\r':
                advance(lexer);
                break;
            case '/':
                if (peek_next(lexer) == '/') {
                    while (peek_next(lexer) != '\n' && !eof(lexer))
                        advance(lexer);
                    break;
                } else {
                    return;
                }
            default:
                return;
        }
    }
}

ds_token ds_lexer_next(ds_lexer *lexer)
{
    /* TODO: I don't like this being here, but I want to see how things work
     *   before moving it (to make_token?)
     */
    lexer->start = lexer->current;

    skip_whitespace(lexer);

    if (eof(lexer))
        return make_token(lexer, DS_TOKEN_EOF);

    char c = advance(lexer);
    switch (c) {
        case '(': return make_token(lexer, DS_TOKEN_LPAREN);
        case ')': return make_token(lexer, DS_TOKEN_RPAREN);
        case '{': return make_token(lexer, DS_TOKEN_LBRACE);
        case '}': return make_token(lexer, DS_TOKEN_RBRACE);
        case ',': return make_token(lexer, DS_TOKEN_COMMA);
        case '.': return make_token(lexer, DS_TOKEN_DOT);
        case ';': return make_token(lexer, DS_TOKEN_SEMICOLON);
        case '+': return make_token(lexer, DS_TOKEN_PLUS);
        case '-': return make_token(lexer, DS_TOKEN_MINUS);
        case '*': return make_token(lexer, DS_TOKEN_STAR);
        case '/': return make_token(lexer, DS_TOKEN_SLASH);
        case '=':
            return make_token(lexer,
              match(lexer, '=') ? DS_TOKEN_EQ_EQ : DS_TOKEN_EQ);
        case '!':
            return make_token(lexer,
              match(lexer, '=') ? DS_TOKEN_EXCL_EQ : DS_TOKEN_EXCL);
        case '<':
            return make_token(lexer,
              match(lexer, '=') ? DS_TOKEN_LT_EQ : DS_TOKEN_LT);
        case '>':
            return make_token(lexer,
              match(lexer, '=') ? DS_TOKEN_GT_EQ : DS_TOKEN_GT);
    }

    return make_error_token(lexer, "unexpected character");
}
