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
    This(V, SrcLoc),
    PropertyGet(Box<Expr<V>>, String, SrcLoc),
    PropertySet(Box<Expr<V>>, String, Box<Expr<V>>, SrcLoc),
    /* this is hacky: super has two variable slots
     *   one for the superclass
     *   one for "this"
     */
    Super(V, V, String, SrcLoc),
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
            Expr::This(_, loc) => loc,
            Expr::PropertyGet(_, _, loc) => loc,
            Expr::PropertySet(_, _, _, loc) => loc,
            Expr::Super(_, _, _, loc) => loc,
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
            Expr::This(_, _) => write!(f, "this"),
            Expr::PropertyGet(o, name, _) => write!(f, "{}.{}", o, name),
            Expr::PropertySet(o, name, e, _) =>
              write!(f, "({}.{} = {})", o, name, e),
            Expr::Super(_, _, name, _) => write!(f, "super.{}", name),
        }
    }
}

/// A function or method definition
#[derive(Clone, Debug)]
pub struct FunOrMethod<V, S> {
    pub parameters: Vec<V>,
    pub body: Vec<Stmt<V, S>>,
    pub slots: S,
    pub location: SrcLoc,
}

/// A Lox statement
#[derive(Clone, Debug)]
pub enum Stmt<V, S> {
    Expr(Expr<V>, SrcLoc),
    IfElse(Expr<V>, Box<Stmt<V, S>>, Option<Box<Stmt<V, S>>>, SrcLoc),
    While(Expr<V>, Box<Stmt<V, S>>, SrcLoc),
    Print(Expr<V>, SrcLoc),
    Return(Option<Expr<V>>, SrcLoc),
    Block(Vec<Stmt<V, S>>, S, SrcLoc),
    VarDecl(V, Option<Expr<V>>, SrcLoc),
    FunDef(V, FunOrMethod<V, S>),
    /* this is hacky: the superclass has two "variable slots":
     *   the one pointing to the superclass (this is "read")
     *   the one where the superclass object gets stored (this is "written")
     */
    ClassDef(V, Option<(V, V)>, Vec<(String, FunOrMethod<V, S>)>, SrcLoc),
}

impl<V, S> Stmt<V, S> {
    pub fn location(&self) -> &SrcLoc {
        match self {
            Stmt::Expr(_, loc) => loc,
            Stmt::IfElse(_, _, _, loc) => loc,
            Stmt::While(_, _, loc) => loc,
            Stmt::Print(_, loc) => loc,
            Stmt::Return(_, loc) => loc,
            Stmt::Block(_, _, loc) => loc,
            Stmt::VarDecl(_, _, loc) => loc,
            Stmt::FunDef(_, f) => &f.location,
            Stmt::ClassDef(_, _, _, loc) => loc,
        }
    }
}

impl<V: fmt::Display, S> fmt::Display for Stmt<V, S> {
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
            Stmt::Block(body, _, _) => {
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
            Stmt::FunDef(name, def) => {
                write!(f, "fun {} (", name)?;
                let mut sep = "";
                for p in &def.parameters {
                    write!(f, "{}{}", sep, p)?;
                    sep = ", ";
                }
                write!(f, ") {{\n")?;
                for s in &def.body {
                    write!(f, "{}\n", s)?;
                }
                write!(f, "}}")
            },
            Stmt::ClassDef(name, sup, methods, _) => {
                write!(f, "class {}", name)?;
                if let Some((cls, _)) = sup {
                    write!(f, " < {}", cls)?;
                }
                write!(f, " {{\n")?;
                for (name, m) in methods {
                    write!(f, "{} (", name)?;
                    let mut sep = "";
                    for p in &m.parameters {
                        write!(f, "{}{}", sep, p)?;
                        sep = ", ";
                    }
                    write!(f, ") {{\n")?;
                    for s in &m.body {
                        write!(f, "{}\n", s)?;
                    }
                    write!(f, "}}\n")?;
                }
                write!(f, "}}")
            },
        }
    }
}
