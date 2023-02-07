use crate::{
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

pub fn eval_expr(expr: &Expr) -> error::Result<Value> {
    match expr {
        Expr::Literal(v, _) => Ok(v.clone()),

        Expr::UnOpApp(UnOp::Complement, e, loc) => {
            if let Value::Bool(b) = eval_expr(e)? {
                Ok(Value::Bool(!b))
            } else {
                Err(type_error!(*loc, "!", "operand must be boolean"))
            }
        },

        Expr::UnOpApp(UnOp::Negate, e, loc) => {
            if let Value::Number(n) = eval_expr(e)? {
                Ok(Value::Number(-n))
            } else {
                Err(type_error!(*loc, "-", "operand must be number"))
            }
        },

        Expr::BinOpApp(BinOp::Add, lhs, rhs, loc) => {
            match (eval_expr(lhs)?, eval_expr(rhs)?) {
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
            match (eval_expr(lhs)?, eval_expr(rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Number(l - r)),
                _ => Err(type_error!(*loc, "-", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::Mul, lhs, rhs, loc) => {
            match (eval_expr(lhs)?, eval_expr(rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Number(l * r)),
                _ => Err(type_error!(*loc, "*", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::Div, lhs, rhs, loc) => {
            match (eval_expr(lhs)?, eval_expr(rhs)?) {
                (Value::Number(l), Value::Number(r)) => {
                    if r == 0.0 {
                        Err(Error {
                            loc: Some(*loc),
                            lexeme: Some("/".to_string()),
                            details: ErrorDetails::DivideByZero,
                        })
                    } else {
                        Ok(Value::Number(l / r))
                    }
                },
                _ => Err(type_error!(*loc, "/", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::Eq, lhs, rhs, _) =>
            Ok(Value::Bool(eval_expr(lhs)? == eval_expr(rhs)?)),

        Expr::BinOpApp(BinOp::NotEq, lhs, rhs, _) =>
            Ok(Value::Bool(eval_expr(lhs)? != eval_expr(rhs)?)),

        Expr::BinOpApp(BinOp::Lt, lhs, rhs, loc) => {
            match (eval_expr(lhs)?, eval_expr(rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Bool(l < r)),
                _ => Err(type_error!(*loc, "<", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::LtEq, lhs, rhs, loc) => {
            match (eval_expr(lhs)?, eval_expr(rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Bool(l <= r)),
                _ => Err(type_error!(*loc, "<=", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::Gt, lhs, rhs, loc) => {
            match (eval_expr(lhs)?, eval_expr(rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Bool(l > r)),
                _ => Err(type_error!(*loc, ">", "operands must be numbers")),
            }
        },

        Expr::BinOpApp(BinOp::GtEq, lhs, rhs, loc) => {
            match (eval_expr(lhs)?, eval_expr(rhs)?) {
                (Value::Number(l), Value::Number(r)) =>
                    Ok(Value::Bool(l >= r)),
                _ => Err(type_error!(*loc, ">=", "operands must be numbers")),
            }
        },
    }
}
