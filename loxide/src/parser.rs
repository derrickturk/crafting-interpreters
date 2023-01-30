use std::iter::Peekable;

use crate::{
    error::{self, Error, ErrorBundle, ErrorDetails,},
    lexer::{Token, TokenKind,},
    syntax::{UnOp, BinOp, Expr,},
    value::{Value,},
};

pub struct Parser<'a, I: Iterator<Item=error::Result<Token<'a>>>> {
    tokens: Peekable<I>,
    errors: ErrorBundle,
}

macro_rules! match_token {
    ($self:ident, $($p:pat),*) => {
        if let Some(Ok(t)) = $self.tokens.peek() {
            match t.kind {
                $($p => {
                    // safe because of if let ^^^
                    Some($self.tokens.next().unwrap().unwrap())
                },)*
                _ => None,
            }
        } else {
            None
        }
    };
}

macro_rules! fold_bin_op {
    ($name:ident, $parser:ident $(,$p:pat)*) => {
        fn $name(&mut self) -> Option<Expr> {
            let mut lhs = self.$parser()?;
            while let Some(tok) = match_token!(self, $($p),*) {
                let rhs = self.$parser()?;
                lhs = Expr::BinOpApp(token_bin_op(&tok).unwrap(),
                  Box::new(lhs), Box::new(rhs));
            }
            Some(lhs)
        }
    };
}

macro_rules! require {
    ($self:ident, $msg:literal $(,$p:pat)*) => {
        match $self.tokens.next() {
            $(Some(Ok(tok @ Token { kind: $p, .. })) => {
                tok
            },)*

            Some(Ok(tok)) => {
                $self.errors.push(Error {
                    line: Some(tok.line),
                    lexeme: Some(tok.lexeme.to_string()),
                    details: ErrorDetails::ParseExpected($msg),
                });
                return None;
            },

            Some(Err(e)) => {
                $self.errors.push(e);
                return None;
            },

            None => {
                $self.errors.push(Error {
                    line: None,
                    lexeme: None,
                    details: ErrorDetails::ParseExpected($msg),
                });
                return None;
            },
        }
    };
}

impl<'a, I: Iterator<Item=error::Result<Token<'a>>>> Parser<'a, I> {
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
            errors: ErrorBundle::new(),
        }
    }

    fn expression(&mut self) -> Option<Expr> {
        self.equality()
    }

    fold_bin_op!(equality, comparison, TokenKind::EqEq | TokenKind::NotEq);
    fold_bin_op!(comparison, term,
      TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq);
    fold_bin_op!(term, factor, TokenKind::Plus | TokenKind::Minus);
    fold_bin_op!(factor, unary, TokenKind::Star | TokenKind::Slash);

    fn unary(&mut self) -> Option<Expr> {
        if let Some(tok) =
          match_token!(self, TokenKind::Not | TokenKind::Minus) {
            let rhs = self.unary()?;
            Some(Expr::UnOpApp(token_un_op(&tok).unwrap(), Box::new(rhs)))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Option<Expr> {
        if let Some(tok) = match_token!(self,
          TokenKind::Nil | TokenKind::True | TokenKind::False |
          TokenKind::StrLit(_) | TokenKind::NumLit(_)) {
            return Some(Expr::Literal(token_literal(&tok).unwrap()));
        }

        if let Some(_) = match_token!(self, TokenKind::LParen) {
            let e = self.expression();
            require!(self, "')'", TokenKind::RParen);
            return e;
        }

        require!(self, "an expression")
    }

    fn eof(&mut self) -> Option<()> {
        match self.tokens.next() {
            None => Some(()),

            Some(Err(e)) => {
                self.errors.push(e);
                None
            },

            Some(Ok(tok)) => {
                self.errors.push(Error {
                    line: Some(tok.line),
                    lexeme: Some(tok.lexeme.to_string()),
                    details: ErrorDetails::ParseExpected("end of input"),
                });
                None
            },
        }
    }

    // recover to a ; or the point before a keyword
    fn recover(&mut self) {
        loop {
            match self.tokens.next() {
                Some(Ok(Token { kind: TokenKind::Semicolon, .. })) => return,
                _ => { },
            };

            match self.tokens.peek() {
                None => return,
                Some(Ok(Token {
                    kind: TokenKind::Class
                        | TokenKind::Fun
                        | TokenKind::Var
                        | TokenKind::For
                        | TokenKind::If
                        | TokenKind::While
                        | TokenKind::Print
                        | TokenKind::Return,
                    ..
                })) => return,
                _ => { },
            };
        }
    }
}

fn token_un_op(token: &Token<'_>) -> Option<UnOp> {
    match token.kind {
        TokenKind::Not => Some(UnOp::Complement),
        TokenKind::Minus => Some(UnOp::Negate),
        _ => None,
    }
}

fn token_bin_op(token: &Token<'_>) -> Option<BinOp> {
    match token.kind {
        TokenKind::Plus => Some(BinOp::Add),
        TokenKind::Minus => Some(BinOp::Sub),
        TokenKind::Star => Some(BinOp::Mul),
        TokenKind::Slash => Some(BinOp::Div),
        TokenKind::EqEq => Some(BinOp::Eq),
        TokenKind::NotEq => Some(BinOp::NotEq),
        TokenKind::Lt => Some(BinOp::Lt),
        TokenKind::LtEq => Some(BinOp::LtEq),
        TokenKind::Gt => Some(BinOp::Gt),
        TokenKind::GtEq => Some(BinOp::GtEq),
        _ => None,
    }
}

fn token_literal(token: &Token<'_>) -> Option<Value> {
    match token.kind {
        TokenKind::Nil => Some(Value::Nil),
        TokenKind::True => Some(Value::Bool(true)),
        TokenKind::False => Some(Value::Bool(false)),
        TokenKind::NumLit(n) => Some(Value::Number(n)),
        TokenKind::StrLit(s) => Some(Value::String(s.to_string())),
        _ => None,
    }
}

#[inline]
pub fn parse_expression<'a, I: Iterator<Item=error::Result<Token<'a>>>>(
  tokens: I) -> Result<Expr, ErrorBundle> {
    let mut p = Parser::new(tokens);
    if let Some(e) = p.expression() {
        if let Some(()) = p.eof() {
            return Ok(e);
        }
    }
    Err(p.errors)
}
