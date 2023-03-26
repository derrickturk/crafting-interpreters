use std::rc::Rc;

use crate::{
    env::Env,
    error::{self, Error, ErrorDetails},
    syntax::*,
    value::*,
};

macro_rules! type_error {
    ($loc:expr, $lexeme:literal, $details:literal) => {
        Error {
            loc: Some($loc),
            lexeme: Some($lexeme.to_string()),
            details: ErrorDetails::TypeError($details),
        }
    };
}

macro_rules! undef_error {
    ($loc:expr, $varname:expr) => {
        Error {
            loc: Some($loc),
            lexeme: None,
            details: ErrorDetails::UndefinedVariable($varname),
        }
    };
}

pub fn eval(env: &Rc<Env>, expr: &Expr<String>) -> error::Result<Value> {
    match expr {
        Expr::Literal(v, _) => Ok(v.clone()),

        Expr::UnOpApp(UnOp::Complement, e, _) =>
            Ok(Value::Bool(!eval(env, e)?.truthy())),

        Expr::UnOpApp(UnOp::Negate, e, loc) => {
            if let Value::Number(n) = eval(env, e)? {
                Ok(Value::Number(-n))
            } else {
                Err(type_error!(*loc, "-", "operand must be number"))
            }
        },

        Expr::BinOpApp(BinOp::And, lhs, rhs, _) => {
            let lhs = eval(env, lhs)?;
            if !lhs.truthy() {
                return Ok(lhs);
            }
            eval(env, rhs)
        },

        Expr::BinOpApp(BinOp::Or, lhs, rhs, _) => {
            let lhs = eval(env, lhs)?;
            if lhs.truthy() {
                return Ok(lhs);
            }
            eval(env, rhs)
        },

        Expr::BinOpApp(BinOp::Add, lhs, rhs, loc) => {
            match (eval(env, lhs)?, eval(env, rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Number(l + r)),
                (Value::String(mut l), Value::String(r)) => {
                    l.push_str(&r);
                    Ok(Value::String(l))
                },
                (Value::Number(_), Value::String(_))
                  | (Value::String(_), Value::Number(_)) =>
                    Err(type_error!(*loc, "+",
                      "operands must have matching types")),
                _ =>
                    Err(type_error!(*loc, "+",
                      "operands must be numbers or strings")),
            }
        },

        Expr::BinOpApp(BinOp::Sub, lhs, rhs, loc) => {
            match (eval(env, lhs)?, eval(env, rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Number(l - r)),
                _ => Err(type_error!(*loc, "-", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::Mul, lhs, rhs, loc) => {
            match (eval(env, lhs)?, eval(env, rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Number(l * r)),
                _ => Err(type_error!(*loc, "*", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::Div, lhs, rhs, loc) => {
            match (eval(env, lhs)?, eval(env, rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Number(l / r)),
                _ => Err(type_error!(*loc, "/", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::Eq, lhs, rhs, _) =>
            Ok(Value::Bool(eval(env, lhs)? == eval(env, rhs)?)),

        Expr::BinOpApp(BinOp::NotEq, lhs, rhs, _) =>
            Ok(Value::Bool(eval(env, lhs)? != eval(env, rhs)?)),

        Expr::BinOpApp(BinOp::Lt, lhs, rhs, loc) => {
            match (eval(env, lhs)?, eval(env, rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Bool(l < r)),
                _ => Err(type_error!(*loc, "<", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::LtEq, lhs, rhs, loc) => {
            match (eval(env, lhs)?, eval(env, rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Bool(l <= r)),
                _ => Err(type_error!(*loc, "<=", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::Gt, lhs, rhs, loc) => {
            match (eval(env, lhs)?, eval(env, rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Bool(l > r)),
                _ => Err(type_error!(*loc, ">", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::GtEq, lhs, rhs, loc) => {
            match (eval(env, lhs)?, eval(env, rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Bool(l >= r)),
                _ => Err(type_error!(*loc, ">=", "operands must be numbers")),
            }
        },

        Expr::Var(v, loc) => {
            match env.get(&v) {
                Some(val) => Ok(val.clone()),
                None => Err(undef_error!(*loc, v.clone())),
            }
        },

        Expr::Assign(v, e, loc) => {
            let val = eval(env, e)?;
            match env.get_mut(&v) {
                Some(mut dst) => {
                    *dst = val.clone();
                    Ok(val)
                },
                None => Err(undef_error!(*loc, v.clone())),
            }
        },

        Expr::Call(_, _, _) => panic!("no fns yet"),
    }
}

pub fn exec(env: &Rc<Env>, stmt: &Stmt<String>) -> error::Result<()> {
    match stmt {
        Stmt::Expr(e, _) => {
            eval(env, e)?;
            Ok(())
        },

        Stmt::IfElse(cond, s_if, s_else, _) => {
            if eval(env, cond)?.truthy() {
                exec(env, s_if)
            } else {
                if let Some(s) = s_else {
                    exec(env, s)
                } else {
                    Ok(())
                }
            }
        },

        Stmt::While(cond, body, _) => {
            while eval(env, cond)?.truthy() {
                exec(env, body)?;
            }
            Ok(())
        },

        Stmt::Print(e, _) => {
            println!("{}", eval(env, e)?.print_string());
            Ok(())
        },

        Stmt::Return(e, _) => {
            panic!("return {}", eval(env, e)?);
        },

        Stmt::Block(body, _) => {
            let frame = env.child();
            for s in body {
                exec(&frame, s)?;
            }
            Ok(())
        },

        Stmt::VarDecl(v, init, loc) => {
            let val = if let Some(e) = init {
                eval(env, e)?
            } else {
                Value::Nil
            };
            if env.declare(v.clone(), val) {
                Ok(())
            } else {
                Err(Error {
                    loc: Some(*loc),
                    lexeme: Some("var".to_string()),
                    details: ErrorDetails::AlreadyDefined(v.clone()),
                })
            }
        },

        Stmt::FunDef(name, params, body, loc) => {
            panic!("fun {}", name);
        },
    }
}
