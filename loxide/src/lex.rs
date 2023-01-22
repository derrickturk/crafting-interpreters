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
}

impl<'a> Lexer<'a> {
    #[inline]
    pub fn new(source: &'a str) -> Self {
        Self { source, line: 0 }
    }

    fn token_here(&mut self, kind: TokenKind<'a>, offset: usize, cur: char
      ) -> Token<'a> {
        let lexeme = self.advance_here(offset, cur);
        Token { kind, lexeme, line: self.line }
    }

    fn advance_here(&mut self, offset: usize, cur: char) -> &'a str {
        let pos = offset + cur.len_utf8();
        let (lexeme, rest) = self.source.split_at(pos);
        self.source = rest;
        lexeme
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = error::Result<Token<'a>>;

    /* this is extra horrible because we have to also attach the
     *   slice corresponding to the token's lexeme.
     * if we end up not using this, I will be Very Annoyed.
     */
    fn next(&mut self) -> Option<error::Result<Token<'a>>> {
        loop {
            let mut chars = self.source.char_indices();
            let (off, c) = chars.next()?;

            /* we're going to use the `token_here` helper function plus
             *   a simple macro to de-duplicate this a bit.
             */
            macro_rules! token_here {
                ($what:expr) => {
                    return Some(Ok(self.token_here($what, off, c)))
                }
            }

            // same for "skips"
            macro_rules! skip_here {
                () => {
                    self.advance_here(off, c)
                }
            }

            match c {
                ' ' | '\r' | '\t' =>  { skip_here!() },
                '\n' => { self.line += 1; skip_here!() },

                '(' => token_here!(TokenKind::LParen),
                ')' => token_here!(TokenKind::RParen),
                '{' => token_here!(TokenKind::LBrace),
                '}' => token_here!(TokenKind::RBrace),
                ',' => token_here!(TokenKind::Comma),
                '.' => token_here!(TokenKind::Dot),
                '-' => token_here!(TokenKind::Minus),
                '+' => token_here!(TokenKind::Plus),
                ';' => token_here!(TokenKind::Semicolon),
                '*' => token_here!(TokenKind::Star),

                /*
                '!' => token_here!(TokenKind::Not),
                '=' => token_here!(TokenKind::Eq),
                '<' => token_here!(TokenKind::Lt),
                '>' => token_here!(TokenKind::Gt),
                */

                _ => todo!("the rest of the owl"),
            };
        }
    }
}
