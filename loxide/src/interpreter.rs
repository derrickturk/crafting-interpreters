use std::rc::Rc;

use crate::{
    env::{Env, Slot},
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

pub fn eval(env: &Rc<Env>, expr: &Expr<Slot>) -> error::Result<Value> {
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
                (Value::String(l), Value::String(r)) => {
                    Ok(Value::String(Rc::new((*l).clone() + r.as_str())))
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
            match env.get(v.frame, v.index) {
                Some(val) => Ok(val.clone()),
                None => Err(undef_error!(*loc, v.name.clone())),
            }
        },

        Expr::Assign(v, e, loc) => {
            let val = eval(env, e)?;
            match env.get_mut(v.frame, v.index) {
                Some(mut dst) => {
                    *dst = val.clone();
                    Ok(val)
                },
                None => Err(undef_error!(*loc, v.name.clone())),
            }
        },

        Expr::Call(callee, args, loc) => {
            match eval(env, callee)? {
                Value::Fun(def, closure) => {
                    if args.len() != def.1.len() {
                        return Err(Error {
                            loc: Some(*loc),
                            lexeme: None,
                            details: ErrorDetails::ArityMismatch(
                              def.0.clone(), def.1.len(), args.len()),
                        });
                    }

                    let frame = closure.child(def.3);
                    for (p, a) in def.1.iter().zip(args.iter()) {
                        frame.set(p.frame, p.index, eval(env, a)?);
                    }

                    match run(&frame, &def.2) {
                        Err(Error { details: ErrorDetails::Return(val), .. }) =>
                            Ok(val),
                        Ok(()) =>
                            Ok(Value::Nil),
                        Err(e) =>
                            Err(e)
                    }
                },

                Value::BuiltinFun(name, arity, ptr) => {
                    if args.len() != arity {
                        return Err(Error {
                            loc: Some(*loc),
                            lexeme: None,
                            details: ErrorDetails::ArityMismatch(
                              name.to_string(), arity, args.len()),
                        });
                    }

                    let mut argv = Vec::new();
                    for a in args {
                        argv.push(eval(env, a)?);
                    }
                    Ok(ptr(argv))
                },

                _ => Err(type_error!(*loc, "(", "callee is not callable")),
            }
        }
    }
}

pub fn exec(env: &Rc<Env>, stmt: &Stmt<Slot, usize>) -> error::Result<()> {
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

        Stmt::Return(e, loc) => {
            let val = match e {
                Some(e) => eval(env, e)?,
                None => Value::Nil,
            };
            Err(Error {
                loc: Some(*loc),
                lexeme: None,
                details: ErrorDetails::Return(val),
            })
        },

        Stmt::Block(body, _, _) => {
            for s in body {
                exec(env, s)?;
            }
            Ok(())
        },

        Stmt::VarDecl(v, init, _) => {
            let val = if let Some(e) = init {
                eval(env, e)?
            } else {
                Value::Nil
            };
            env.set(v.frame, v.index, val);
            Ok(())
        },

        Stmt::FunDef(v, params, body, slots, _) => {
            let defn = Rc::new((
              v.name.clone(), params.clone(), body.clone(), *slots));
            let fun = Value::Fun(defn, env.clone());
            env.set(v.frame, v.index, fun);
            Ok(())
        },
    }
}

#[inline]
pub fn run(env: &Rc<Env>, prog: &[Stmt<Slot, usize>]) -> error::Result<()> {
    for stmt in prog {
        exec(env, stmt)?;
    }
    Ok(())
}
