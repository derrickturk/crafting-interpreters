//! Tokens and the Loxide lexer/scanner

use crate::error::{self, Error, ErrorDetails};

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
}

impl<'a> TokenKind<'a> {
    pub fn keyword_or_ident(lexeme: &'a str) -> Self {
        match lexeme {
            "and" => TokenKind::And,
            "class" => TokenKind::Class,
            "else" => TokenKind::Else,
            "false" => TokenKind::False,
            "for" => TokenKind::For,
            "fun" => TokenKind::Fun,
            "if" => TokenKind::If,
            "nil" => TokenKind::Nil,
            "or" => TokenKind::Or,
            "print" => TokenKind::Print,
            "return" => TokenKind::Return,
            "super" => TokenKind::Super,
            "this" => TokenKind::This,
            "true" => TokenKind::True,
            "var" => TokenKind::Var,
            "while" => TokenKind::While,
            _ => TokenKind::Ident(lexeme),
        }
    }
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
        self.peek_nth(0)
    }

    #[inline]
    fn peek_nth(&self, n: usize) -> Option<char> {
        let (_, rest) = self.split_here();
        rest.chars().nth(n)
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
    fn match_char(&mut self, what: char) -> bool {
        self.match_where(|c| c == what)
    }

    #[inline]
    fn match_where<P: Fn(char) -> bool>(&mut self, pred: P) -> bool {
        match self.peek() {
            Some(c) if pred(c) => {
                self.next_offset += c.len_utf8();
                true
            },
            _ => false,
        }
    }

    #[inline]
    fn match_while<P: Fn(char) -> bool>(&mut self, pred: P) {
        while self.match_where(&pred) { }
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
                ' ' | '\r' | '\t' =>  { self.commit_here(); },
                '\n' => { self.line += 1; self.commit_here(); },

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
                    let kind = if self.match_char('=') {
                        TokenKind::NotEq
                    } else {
                        TokenKind::Not
                    };
                    return Some(Ok(self.token_here(kind)));
                },
                '=' => {
                    let kind = if self.match_char('=') {
                        TokenKind::EqEq
                    } else {
                        TokenKind::Eq
                    };
                    return Some(Ok(self.token_here(kind)));
                },
                '<' => {
                    let kind = if self.match_char('=') {
                        TokenKind::LtEq
                    } else {
                        TokenKind::Lt
                    };
                    return Some(Ok(self.token_here(kind)));
                },
                '>' => {
                    let kind = if self.match_char('=') {
                        TokenKind::GtEq
                    } else {
                        TokenKind::Gt
                    };
                    return Some(Ok(self.token_here(kind)));
                },

                '/' => {
                    if self.match_char('/') {
                        self.match_while(|c| c != '\n');
                        self.commit_here();
                    } else {
                        return Some(Ok(self.token_here(TokenKind::Slash)));
                    }
                },

                '"' => {
                    while let Some(next) = self.consume() {
                        match next {
                            '"' => {
                                let (this, _) = self.split_here();
                                let this = &this[1..this.len() - 1];
                                return Some(Ok(
                                  self.token_here(TokenKind::StrLit(this))));
                            },
                            '\n' => { self.line += 1; },
                            _ => { },
                        }
                    }
                    return Some(Err(Error{
                        line: self.line,
                        wurr: String::new(),
                        details: ErrorDetails::UnterminatedStrLit,
                    }));
                },

                d if d.is_ascii_digit() => {
                    self.match_while(|c| c.is_ascii_digit());
                    if let Some('.') = self.peek() {
                        match self.peek_nth(1) {
                            Some(d) if d.is_ascii_digit() => {
                                self.consume(); // the dot
                                self.match_while(|c| c.is_ascii_digit());
                            },
                            _ => { },
                        }
                    }
                    let (this, _) = self.split_here();
                    return Some(Ok(
                      self.token_here(TokenKind::NumLit(
                        this.parse::<f64>().unwrap()))));
                },

                i if i.is_alphabetic() || i == '_' => {
                    self.match_while(|i| i.is_alphanumeric() || i == '_');
                    let (this, _) = self.split_here();
                    return Some(Ok(
                      self.token_here(TokenKind::keyword_or_ident(this))));
                },

                c => {
                    return Some(Err(Error {
                        line: self.line,
                        wurr: String::new(), // IDK what the author intends here
                        details: ErrorDetails::UnexpectedCharacter(c),
                    }))
                },
            };
        }
    }
}

#[inline]
pub fn lex(source: &str) -> impl Iterator<Item=error::Result<Token<'_>>> {
    Lexer::new(source)
}
