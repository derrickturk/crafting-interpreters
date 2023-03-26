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

pub fn eval_expr(env: &Env, expr: &Expr<String>) -> error::Result<Value> {
    match expr {
        Expr::Literal(v, _) => Ok(v.clone()),

        Expr::UnOpApp(UnOp::Complement, e, _) =>
            Ok(Value::Bool(!eval_expr(env, e)?.truthy())),

        Expr::UnOpApp(UnOp::Negate, e, loc) => {
            if let Value::Number(n) = eval_expr(env, e)? {
                Ok(Value::Number(-n))
            } else {
                Err(type_error!(*loc, "-", "operand must be number"))
            }
        },

        Expr::BinOpApp(BinOp::And, lhs, rhs, _) => {
            let lhs = eval_expr(env, lhs)?;
            if !lhs.truthy() {
                return Ok(lhs);
            }
            eval_expr(env, rhs)
        },

        Expr::BinOpApp(BinOp::Or, lhs, rhs, _) => {
            let lhs = eval_expr(env, lhs)?;
            if lhs.truthy() {
                return Ok(lhs);
            }
            eval_expr(env, rhs)
        },

        Expr::BinOpApp(BinOp::Add, lhs, rhs, loc) => {
            match (eval_expr(env, lhs)?, eval_expr(env, rhs)?) {
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
            match (eval_expr(env, lhs)?, eval_expr(env, rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Number(l - r)),
                _ => Err(type_error!(*loc, "-", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::Mul, lhs, rhs, loc) => {
            match (eval_expr(env, lhs)?, eval_expr(env, rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Number(l * r)),
                _ => Err(type_error!(*loc, "*", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::Div, lhs, rhs, loc) => {
            match (eval_expr(env, lhs)?, eval_expr(env, rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Number(l / r)),
                _ => Err(type_error!(*loc, "/", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::Eq, lhs, rhs, _) =>
            Ok(Value::Bool(eval_expr(env, lhs)? == eval_expr(env, rhs)?)),

        Expr::BinOpApp(BinOp::NotEq, lhs, rhs, _) =>
            Ok(Value::Bool(eval_expr(env, lhs)? != eval_expr(env, rhs)?)),

        Expr::BinOpApp(BinOp::Lt, lhs, rhs, loc) => {
            match (eval_expr(env, lhs)?, eval_expr(env, rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Bool(l < r)),
                _ => Err(type_error!(*loc, "<", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::LtEq, lhs, rhs, loc) => {
            match (eval_expr(env, lhs)?, eval_expr(env, rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Bool(l <= r)),
                _ => Err(type_error!(*loc, "<=", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::Gt, lhs, rhs, loc) => {
            match (eval_expr(env, lhs)?, eval_expr(env, rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Bool(l > r)),
                _ => Err(type_error!(*loc, ">", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::GtEq, lhs, rhs, loc) => {
            match (eval_expr(env, lhs)?, eval_expr(env, rhs)?) {
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
            let val = eval_expr(env, e)?;
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
