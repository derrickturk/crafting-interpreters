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
    And,
    Or,
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
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
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
    pub fn location(&self) -> &SrcLoc {
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

/// A Lox statement
#[derive(Clone, Debug)]
pub enum Stmt<V> {
    Expr(Expr<V>, SrcLoc),
    IfElse(Expr<V>, Box<Stmt<V>>, Option<Box<Stmt<V>>>, SrcLoc),
    While(Expr<V>, Box<Stmt<V>>, SrcLoc),
    Print(Expr<V>, SrcLoc),
    Return(Option<Expr<V>>, SrcLoc),
    Block(Vec<Stmt<V>>, SrcLoc),
    VarDecl(V, Option<Expr<V>>, SrcLoc),
    FunDef(V, Vec<V>, Vec<Stmt<V>>, SrcLoc),
}

impl<V> Stmt<V> {
    pub fn location(&self) -> &SrcLoc {
        match self {
            Stmt::Expr(_, loc) => loc,
            Stmt::IfElse(_, _, _, loc) => loc,
            Stmt::While(_, _, loc) => loc,
            Stmt::Print(_, loc) => loc,
            Stmt::Return(_, loc) => loc,
            Stmt::Block(_, loc) => loc,
            Stmt::VarDecl(_, _, loc) => loc,
            Stmt::FunDef(_, _, _, loc) => loc,
        }
    }
}

impl<V: fmt::Display> fmt::Display for Stmt<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Expr(e, _) => write!(f, "{};", e),
            Stmt::IfElse(cond, s_if, s_else, _) => {
                write!(f, "if ({}) {{\n{}\n}}", cond, s_if)?;
                if let Some(s) = s_else {
                    write!(f, " else {{\n{}\n}}", s)?;
                }
                Ok(())
            },
            Stmt::While(cond, body, _) =>
                write!(f, "while ({}) {{\n{}\n}}", cond, body),
            Stmt::Print(e, _) => write!(f, "print {};", e),
            Stmt::Return(Some(e), _) => write!(f, "return {};", e),
            Stmt::Return(None, _) => write!(f, "return;"),
            Stmt::Block(body, _) => {
                write!(f, "{{\n")?;
                for s in body {
                    write!(f, "{}\n", s)?;
                }
                write!(f, "}}")
            },
            Stmt::VarDecl(v, init, _) => {
                write!(f, "var {}", v)?;
                if let Some(e) = init {
                    write!(f, " = {}", e)?;
                }
                write!(f, ";")
            },
            Stmt::FunDef(v, params, body, _) => {
                write!(f, "fun {} (", v)?;
                let mut sep = "";
                for p in params {
                    write!(f, "{}{}", sep, p)?;
                    sep = ", ";
                }
                write!(f, ") {{\n")?;
                for s in body {
                    write!(f, "{}\n", s)?;
                }
                write!(f, "}}")
            },
        }
    }
}
