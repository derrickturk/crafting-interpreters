//! Tokens and the Loxide lexer/scanner

use crate::error;

/// The type and payload (if any) of a Lox token
#[derive(Copy, Clone, Debug)]
pub enum TokenKind<'a> {
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Not,
    NotEq,
    Eq,
    EqEq,
    Gt,
    GtEq,
    Lt,
    LtEq,
    Ident(&'a str),
    StrLit(&'a str),
    NumLit(f64),
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof, // I don't trust this guy one bit
}

/// A Lox token, along with the correponding lexeme text, and the line number
#[derive(Copy, Clone, Debug)]
pub struct Token<'a> {
    kind: TokenKind<'a>,
    lexeme: &'a str,
    line: usize,
}

/// A Lox lexer, holding the remaining source code
#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a str,
    line: usize,
    next_offset: usize,
}

impl<'a> Lexer<'a> {
    #[inline]
    pub fn new(source: &'a str) -> Self {
        Self { source, line: 0, next_offset: 0 }
    }

    /* this is extra horrible because we have to also attach the
     *   slice corresponding to the token's lexeme.
     * if we end up not using this, I will be Very Annoyed.
     */

    #[inline]
    fn peek(&self) -> Option<char> {
        let (_, rest) = self.split_here();
        rest.chars().next()
    }

    #[inline]
    fn consume(&mut self) -> Option<char> {
        if let Some(c) = self.peek() {
            self.next_offset += c.len_utf8();
            Some(c)
        } else {
            None
        }
    }

    #[inline]
    fn match_char<P: Fn(char) -> bool>(&mut self, pred: P) -> bool {
        if let Some(c) = self.peek() {
            if pred(c) {
                self.next_offset += c.len_utf8();
                return true;
            }
        }
        false
    }

    #[inline]
    fn split_here(&self) -> (&'a str, &'a str) {
        self.source.split_at(self.next_offset)
    }

    fn commit_here(&mut self) -> &'a str {
        let (lexeme, rest) = self.split_here();
        self.source = rest;
        self.next_offset = 0;
        lexeme
    }

    #[inline]
    fn token_here(&mut self, kind: TokenKind<'a>) -> Token<'a> {
        let lexeme = self.commit_here();
        Token { kind, lexeme, line: self.line }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = error::Result<Token<'a>>;

    fn next(&mut self) -> Option<error::Result<Token<'a>>> {
        loop {
            match self.consume()? {
                ' ' | '\r' | '\t' =>  { self.commit_here() },
                '\n' => { self.line += 1; self.commit_here() },

                '(' => return Some(Ok(self.token_here(TokenKind::LParen))),
                ')' => return Some(Ok(self.token_here(TokenKind::RParen))),
                '{' => return Some(Ok(self.token_here(TokenKind::LBrace))),
                '}' => return Some(Ok(self.token_here(TokenKind::RBrace))),
                ',' => return Some(Ok(self.token_here(TokenKind::Comma))),
                '.' => return Some(Ok(self.token_here(TokenKind::Dot))),
                '-' => return Some(Ok(self.token_here(TokenKind::Minus))),
                '+' => return Some(Ok(self.token_here(TokenKind::Plus))),
                ';' => return Some(Ok(self.token_here(TokenKind::Semicolon))),
                '*' => return Some(Ok(self.token_here(TokenKind::Star))),

                '!' => {
                    let kind = if self.match_char(|c| c == '=') {
                        TokenKind::NotEq
                    } else {
                        TokenKind::Not
                    };
                    return Some(Ok(self.token_here(kind)));
                },
                '=' => {
                    let kind = if self.match_char(|c| c == '=') {
                        TokenKind::EqEq
                    } else {
                        TokenKind::Eq
                    };
                    return Some(Ok(self.token_here(kind)));
                },
                '<' => {
                    let kind = if self.match_char(|c| c == '=') {
                        TokenKind::LtEq
                    } else {
                        TokenKind::Lt
                    };
                    return Some(Ok(self.token_here(kind)));
                },
                '>' => {
                    let kind = if self.match_char(|c| c == '=') {
                        TokenKind::GtEq
                    } else {
                        TokenKind::Gt
                    };
                    return Some(Ok(self.token_here(kind)));
                },

                _ => todo!("the rest of the owl"),
            };
        }
    }
}
