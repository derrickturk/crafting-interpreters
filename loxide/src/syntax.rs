//! Loxide syntax trees

use std::fmt;

use crate::{
    value::Value,
};

/// A unary operator
#[derive(Copy, Clone, Debug)]
pub enum UnOp {
    Complement,
    Negate,
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UnOp::Complement => write!(f, "!"),
            UnOp::Negate => write!(f, "-"),
        }
    }
}

/// A binary operator
#[derive(Copy, Clone, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Eq => write!(f, "=="),
            BinOp::NotEq => write!(f, "!="),
            BinOp::Lt => write!(f, "<"),
            BinOp::LtEq => write!(f, "<="),
            BinOp::Gt => write!(f, ">"),
            BinOp::GtEq => write!(f, ">="),
        }
    }
}

/// A Lox expression
// TODO: tag with line number from tokens?
#[derive(Clone, Debug)]
pub enum Expr {
    Literal(Value),
    UnOpApp(UnOp, Box<Expr>),
    BinOpApp(BinOp, Box<Expr>, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Literal(v) => write!(f, "{}", v),
            Expr::UnOpApp(op, e) => write!(f, "{}{}", op, e),
            Expr::BinOpApp(op, lhs, rhs) =>
              write!(f, "({} {} {})", lhs, op, rhs),
        }
    }
}
