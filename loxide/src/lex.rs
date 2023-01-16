//! Tokens and the Loxide lexer/scanner

/// A single Lox token
#[derive(Debug)]
pub enum Token { }

/// A Lox lexer, holding the remaining source code
#[derive(Debug)]
pub struct Lexer {
    source: String
}

impl Lexer {
    #[inline]
    pub fn new(source: String) -> Self {
        Self { source }
    }

    pub fn scan_tokens(self) -> Vec<Token> {
        todo!("write a lexer")
    }
}
