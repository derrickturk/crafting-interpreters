use std::iter::Peekable;

use crate::{
    error::{self, Error, ErrorBundle, ErrorDetails,},
    lexer::{Token, TokenKind,},
    syntax::*,
    value::Value,
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

macro_rules! check_token {
    ($self:ident, $($p:pat),*) => {
        if let Some(Ok(t)) = $self.tokens.peek() {
            match t.kind {
                $($p => true,)*
                _ => false,
            }
        } else {
            false
        }
    };
}

macro_rules! fold_bin_op {
    ($name:ident, $parser:ident $(,$p:pat)*) => {
        fn $name(&mut self) -> Option<Expr<String>> {
            let mut lhs = self.$parser()?;
            while let Some(tok) = match_token!(self, $($p),*) {
                let rhs = self.$parser()?;
                lhs = Expr::BinOpApp(token_bin_op(&tok).unwrap(),
                  Box::new(lhs), Box::new(rhs), tok.loc);
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
                    loc: Some(tok.loc),
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
                    loc: None,
                    lexeme: None,
                    details: ErrorDetails::ParseExpected($msg),
                });
                return None;
            },
        }
    };
}

// TODO: refactor for less duplication with require
macro_rules! require_extract {
    ($self:ident, $msg:literal, $p:pat, $e:expr) => {
        match $self.tokens.next() {
            Some(Ok(Token { kind: $p, .. })) => {
                $e
            },

            Some(Ok(tok)) => {
                $self.errors.push(Error {
                    loc: Some(tok.loc),
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
                    loc: None,
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

    #[inline]
    fn declaration(&mut self) -> Option<Stmt<String>> {
        if let Some(tok) = match_token!(self, TokenKind::Var) {
            return self.var_decl_rest(tok);
        }

        if let Some(tok) = match_token!(self, TokenKind::Fun) {
            return self.fundef_or_method_rest(tok);
        }

        self.statement()
    }

    fn statement(&mut self) -> Option<Stmt<String>> {
        if let Some(tok) = match_token!(self, TokenKind::If) {
            return self.if_else_rest(tok);
        }

        if let Some(tok) = match_token!(self, TokenKind::While) {
            return self.while_rest(tok);
        }

        if let Some(tok) = match_token!(self, TokenKind::For) {
            return self.for_rest(tok);
        }

        if let Some(tok) = match_token!(self, TokenKind::Print) {
            let e = self.expression()?;
            require!(self, "';'", TokenKind::Semicolon);
            return Some(Stmt::Print(e, tok.loc));
        }

        if let Some(tok) = match_token!(self, TokenKind::Return) {
            if let Some(_) = match_token!(self, TokenKind::Semicolon) {
                return Some(Stmt::Return(None, tok.loc));
            }
            let e = self.expression()?;
            require!(self, "';'", TokenKind::Semicolon);
            return Some(Stmt::Return(Some(e), tok.loc));
        }

        if let Some(tok) = match_token!(self, TokenKind::LBrace) {
            let body = self.block_rest()?;
            return Some(Stmt::Block(body, tok.loc));
        }

        self.expression_statement()
    }

    fn var_decl_rest(&mut self, var: Token) -> Option<Stmt<String>> {
        let name = require_extract!(self,
          "identifier", TokenKind::Ident(name), name.to_string());
        let init = if let Some(_) = match_token!(self, TokenKind::Eq) {
            Some(self.expression()?)
        } else {
            None
        };
        require!(self, "';'", TokenKind::Semicolon);
        Some(Stmt::VarDecl(name, init, var.loc))
    }

    fn fundef_or_method_rest(&mut self, fun: Token) -> Option<Stmt<String>> {
        let name = require_extract!(self,
          "identifier", TokenKind::Ident(name), name.to_string());
        require!(self, "'('", TokenKind::LParen);

        let mut params = Vec::new();
        if !check_token!(self, TokenKind::RParen) {
            loop {
                params.push(require_extract!(self, "parameter name",
                  TokenKind::Ident(name), name.to_string()));
                if match_token!(self, TokenKind::Comma).is_none() {
                    break;
                }
            }
        }

        let rparen = require!(self, "')'", TokenKind::RParen);
        if params.len() > 255 {
            self.errors.push(Error {
                loc: Some(rparen.loc),
                lexeme: Some(rparen.lexeme.to_string()),
                details: ErrorDetails::TooManyArgs,
            });
            return None;
        }

        require!(self, "'{'", TokenKind::LBrace);
        let body = self.block_rest()?;

        Some(Stmt::FunDef(name, params, body, fun.loc))
    }

    fn if_else_rest(&mut self, t_if: Token) -> Option<Stmt<String>> {
        require!(self, "'('", TokenKind::LParen);
        let cond = self.expression()?;
        require!(self, "')'", TokenKind::LParen);
        let s_if = Box::new(self.statement()?);
        let s_else = if let Some(_) = match_token!(self, TokenKind::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        Some(Stmt::IfElse(cond, s_if, s_else, t_if.loc))
    }

    fn while_rest(&mut self, t_while: Token) -> Option<Stmt<String>> {
        require!(self, "'('", TokenKind::LParen);
        let cond = self.expression()?;
        require!(self, "')'", TokenKind::LParen);
        let body = Box::new(self.statement()?);
        Some(Stmt::While(cond, body, t_while.loc))
    }

    fn for_rest(&mut self, t_for: Token) -> Option<Stmt<String>> {
        require!(self, "'('", TokenKind::LParen);

        let init = if let Some(_) = match_token!(self, TokenKind::Semicolon) {
            None
        } else if let Some(tok) = match_token!(self, TokenKind::Var) {
            Some(self.var_decl_rest(tok)?)
        } else {
            Some(self.expression_statement()?)
        };

        let cond = if let Some(tok) = match_token!(self, TokenKind::Semicolon) {
            Expr::Literal(Value::Bool(true), tok.loc)
        } else {
            let e = self.expression()?;
            require!(self, "';'", TokenKind::Semicolon);
            e
        };

        let incr = if let Some(_) = match_token!(self, TokenKind::RParen) {
            None
        } else {
            let e = self.expression()?;
            require!(self, "')'", TokenKind::RParen);
            Some(e)
        };

        let body = self.statement()?;
        let body = if let Some(incr) = incr {
            let b_loc = *body.location();
            let i_loc = *incr.location();
            Stmt::Block(vec![body, Stmt::Expr(incr, i_loc)], b_loc)
        } else {
            body
        };
        let body = Stmt::While(cond, Box::new(body), t_for.loc);
        let body = match init {
            Some(init) => {
                let loc = *init.location();
                Stmt::Block(vec![init, body], loc)
            },
            None => body,
        };
        Some(body)
    }

    fn block_rest(&mut self) -> Option<Vec<Stmt<String>>> {
        let mut body = Vec::new();
        while !check_token!(self, TokenKind::RBrace) && !self.is_eof() {
            body.push(self.declaration()?);
        }
        require!(self, "'}'", TokenKind::RBrace);
        Some(body) // once told me
    }

    #[inline]
    fn expression_statement(&mut self) -> Option<Stmt<String>> {
        let e = self.expression()?;
        require!(self, "';'", TokenKind::Semicolon);
        let loc = *e.location();
        Some(Stmt::Expr(e, loc))
    }

    #[inline]
    fn expression(&mut self) -> Option<Expr<String>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Option<Expr<String>> {
        let e = self.logic_or()?;
        if let Some(tok) = match_token!(self, TokenKind::Eq) {
            let val = self.assignment()?;
            match e {
                Expr::Var(v, loc) =>
                    Some(Expr::Assign(v, Box::new(val), loc)),
                _ => {
                    self.errors.push(Error {
                        loc: Some(tok.loc),
                        lexeme: Some(tok.lexeme.to_string()),
                        details: ErrorDetails::NotLValue(format!("{}", val)),
                    });
                    return None;
                }
            }
        } else {
            Some(e)
        }
    }

    fold_bin_op!(logic_or, logic_and, TokenKind::Or);
    fold_bin_op!(logic_and, equality, TokenKind::And);
    fold_bin_op!(equality, comparison, TokenKind::EqEq | TokenKind::NotEq);
    fold_bin_op!(comparison, term,
      TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq);
    fold_bin_op!(term, factor, TokenKind::Plus | TokenKind::Minus);
    fold_bin_op!(factor, unary, TokenKind::Star | TokenKind::Slash);

    fn unary(&mut self) -> Option<Expr<String>> {
        if let Some(tok) =
          match_token!(self, TokenKind::Not | TokenKind::Minus) {
            let rhs = self.unary()?;
            Some(Expr::UnOpApp(
              token_un_op(&tok).unwrap(), Box::new(rhs), tok.loc))
        } else {
            self.call()
        }
    }

    #[inline]
    fn call(&mut self) -> Option<Expr<String>> {
        let mut e = self.primary()?;
        while let Some(lparen) = match_token!(self, TokenKind::LParen) {
            e = self.call_rest(e, lparen)?;
        }
        Some(e)
    }

    fn call_rest(&mut self, callee: Expr<String>, lparen: Token
      ) -> Option<Expr<String>> {
        let mut args = Vec::new();
        if !check_token!(self, TokenKind::RParen) {
            loop {
                args.push(self.expression()?);
                if match_token!(self, TokenKind::Comma).is_none() {
                    break;
                }
            }
        }

        let rparen = require!(self, "')'", TokenKind::RParen);
        if args.len() > 255 {
            self.errors.push(Error {
                loc: Some(rparen.loc),
                lexeme: Some(rparen.lexeme.to_string()),
                details: ErrorDetails::TooManyArgs,
            });
            return None;
        }

        Some(Expr::Call(Box::new(callee), args, lparen.loc))
    }

    fn primary(&mut self) -> Option<Expr<String>> {
        if let Some(tok) = match_token!(self,
          TokenKind::Nil | TokenKind::True | TokenKind::False |
          TokenKind::StrLit(_) | TokenKind::NumLit(_)) {
            return Some(Expr::Literal(token_literal(&tok).unwrap(), tok.loc));
        }

        if let Some(Token { kind: TokenKind::Ident(name), loc, .. }) =
          match_token!(self, TokenKind::Ident(_)) {
            return Some(Expr::Var(name.to_string(), loc));
        }

        if let Some(_) = match_token!(self, TokenKind::LParen) {
            let e = self.expression();
            require!(self, "')'", TokenKind::RParen);
            return e;
        }

        require!(self, "an expression")
    }

    #[inline]
    fn is_eof(&mut self) -> bool {
        self.tokens.peek().is_none()
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
                    loc: Some(tok.loc),
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
                Some(Err(e)) => { self.errors.push(e) },
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
        TokenKind::And => Some(BinOp::And),
        TokenKind::Or => Some(BinOp::Or),
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
pub fn parse_expr<'a, I: Iterator<Item=error::Result<Token<'a>>>>(
  tokens: I) -> Result<Expr<String>, ErrorBundle> {
    let mut p = Parser::new(tokens);
    if let Some(e) = p.expression() {
        if let Some(()) = p.eof() {
            return Ok(e);
        }
    }
    Err(p.errors)
}

#[inline]
pub fn parse<'a, I: Iterator<Item=error::Result<Token<'a>>>>(
  tokens: I) -> Result<Vec<Stmt<String>>, ErrorBundle> {
    let mut prog = Vec::new();
    let mut p = Parser::new(tokens);
    let mut ok = true;
    while !p.is_eof() {
        if let Some(s) = p.declaration() {
            prog.push(s);
        } else {
            ok = false;
            p.recover();
        }
    }
    if ok {
        Ok(prog)
    } else {
        Err(p.errors)
    }
}
