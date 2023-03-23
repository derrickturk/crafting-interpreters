//! Loxide syntax trees

use std::fmt;

use crate::{
    srcloc::SrcLoc,
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
#[derive(Clone, Debug)]
pub enum Expr<V> {
    Literal(Value, SrcLoc),
    UnOpApp(UnOp, Box<Expr<V>>, SrcLoc),
    BinOpApp(BinOp, Box<Expr<V>>, Box<Expr<V>>, SrcLoc),
    Var(V, SrcLoc),
    Assign(V, Box<Expr<V>>, SrcLoc),
    Call(Box<Expr<V>>, Vec<Expr<V>>, SrcLoc),
}

impl<V> Expr<V> {
    fn location(&self) -> &SrcLoc {
        match self {
            Expr::Literal(_, loc) => loc,
            Expr::UnOpApp(_, _, loc) => loc,
            Expr::BinOpApp(_, _, _, loc) => loc,
            Expr::Var(_, loc) => loc,
            Expr::Assign(_, _, loc) => loc,
            Expr::Call(_, _, loc) => loc,
        }
    }
}

impl<V: fmt::Display> fmt::Display for Expr<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Literal(v, _) => write!(f, "{}", v),
            Expr::UnOpApp(op, e, _) => write!(f, "{}{}", op, e),
            Expr::BinOpApp(op, lhs, rhs, _) =>
              write!(f, "({} {} {})", lhs, op, rhs),
            Expr::Var(v, _) => write!(f, "{}", v),
            Expr::Assign(v, e, _) => write!(f, "({} = {})", v, e),
            Expr::Call(callee, args, _) => {
                write!(f, "({})(", callee)?;
                let mut sep = "";
                for a in args {
                    write!(f, "{}{}", sep, a)?;
                    sep = ", "
                }
                write!(f, ")")
            },
        }
    }
}
