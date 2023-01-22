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
    source: &'a str
}

impl<'a> Lexer<'a> {
    #[inline]
    pub fn new(source: &'a str) -> Self {
        Self { source }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = error::Result<Token<'a>>;

    fn next(&mut self) -> Option<error::Result<Token<'a>>> {
        None
    }
}
