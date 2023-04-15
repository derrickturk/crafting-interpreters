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

macro_rules! undef_var_error {
    ($loc:expr, $varname:expr) => {
        Error {
            loc: Some($loc),
            lexeme: None,
            details: ErrorDetails::UndefinedVariable($varname),
        }
    };
}

macro_rules! undef_property_error {
    ($loc:expr, $propname:expr) => {
        Error {
            loc: Some($loc),
            lexeme: None,
            details: ErrorDetails::UndefinedProperty($propname),
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
                None => Err(undef_var_error!(*loc, v.name.clone())),
            }
        },

        Expr::Assign(v, e, loc) => {
            let val = eval(env, e)?;
            match env.get_mut(v.frame, v.index) {
                Some(mut dst) => {
                    *dst = val.clone();
                    Ok(val)
                },
                None => Err(undef_var_error!(*loc, v.name.clone())),
            }
        },

        Expr::Call(callee, args, loc) => {
            match eval(env, callee)? {
                Value::Fun(def, closure) => {
                    if args.len() != def.1.parameters.len() {
                        return Err(Error {
                            loc: Some(*loc),
                            lexeme: None,
                            details: ErrorDetails::ArityMismatch(
                              def.0.clone(), def.1.parameters.len(),
                              args.len()),
                        });
                    }

                    let frame = closure.child(def.1.slots);
                    for (p, a) in def.1.parameters.iter().zip(args.iter()) {
                        frame.set(p.frame, p.index, eval(env, a)?);
                    }

                    match run(&frame, &def.1.body) {
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

                Value::BoundMethod(def, this, closure) => {
                    if args.len() != def.1.parameters.len() {
                        return Err(Error {
                            loc: Some(*loc),
                            lexeme: None,
                            details: ErrorDetails::ArityMismatch(
                              def.0.clone(), def.1.parameters.len(),
                              args.len()),
                        });
                    }

                    let frame = closure.child(def.1.slots);
                    /* in principle this shouldn't be hard coded, but hell,
                     *   I don't care anymore. */
                    frame.set(0, 0, Value::Object(this));
                    for (p, a) in def.1.parameters.iter().zip(args.iter()) {
                        frame.set(p.frame, p.index, eval(env, a)?);
                    }

                    match run(&frame, &def.1.body) {
                        Err(Error { details: ErrorDetails::Return(val), .. }) =>
                            Ok(val),
                        Ok(()) =>
                            Ok(Value::Nil),
                        Err(e) =>
                            Err(e)
                    }
                },

                // arggggg this sucks
                Value::Class(c) => {
                    if args.len() != c.arity() {
                        return Err(Error {
                            loc: Some(*loc),
                            lexeme: None,
                            details: ErrorDetails::ArityMismatch(
                              c.name(), c.arity(), args.len()),
                        });
                    }

                    let o = Value::Object(Rc::new(Object::new(Rc::clone(&c))));
                    if let Some((init, closure)) = c.find_method("init") {
                        let frame = closure.child(init.1.slots);
                        frame.set(0, 0, o.clone());
                        for (p, a) in init.1.parameters.iter().zip(args.iter()) {
                            frame.set(p.frame, p.index, eval(env, a)?);
                        }

                        match run(&frame, &init.1.body) {
                            Err(Error { details: ErrorDetails::Return(_), .. }) =>
                                Ok(o),
                            Ok(()) =>
                                Ok(o),
                            Err(e) =>
                                Err(e),
                        }
                    } else {
                        Ok(o)
                    }
                },

                _ => Err(type_error!(*loc, "(", "callee is not callable")),
            }
        }

        Expr::This(v, _) => {
            Ok(env.get(v.frame, v.index)
              .expect("internal error: this is missing").clone())
        },

        Expr::PropertyGet(e, name, loc) => {
            match eval(env, e)? {
                Value::Object(o) => {
                    if let Some(p) = o.get_property(&name) {
                        Ok(p)
                    } else {
                        Err(undef_property_error!(*loc, name.clone()))
                    }
                },
                _ => {
                    Err(type_error!(*loc, ".", "property access on non-object"))
                },
            }
        },

        Expr::PropertySet(lhs, name, rhs, loc) => {
            let obj = match eval(env, lhs)? {
                Value::Object(o) =>
                    Ok(o),
                _ =>
                    Err(type_error!(*loc, ".", "property access on non-object"))
            }?;
            let val = eval(env, rhs)?;
            obj.set_property(name.clone(), val.clone());
            Ok(val)
        },

        Expr::Super(sup, this, name, loc) => {
            let sup = match env.get(sup.frame, sup.index).map(|v| v.clone()) {
                Some(Value::Class(c)) => c,
                _ => panic!("internal error: super is missing or invalid"),
            };

            let this = match env.get(this.frame, this.index).map(|v| v.clone()) {
                Some(Value::Object(o)) => o,
                _ => panic!("internal error: this is missing or invalid"),
            };

            if let Some((def, env)) = sup.find_method(&name) {
                Ok(Value::BoundMethod(def, this, env))
            } else {
                Err(undef_property_error!(*loc, name.clone()))
            }
        },
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

        Stmt::Block(body, _) => {
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

        Stmt::FunDef(v, def) => {
            let fun = Value::Fun(
              Rc::new((v.name.clone(), def.clone())), env.clone());
            env.set(v.frame, v.index, fun);
            Ok(())
        },

        Stmt::ClassDef(v, sup, methods, loc) => {
            let sup = match sup {
                Some((src, dst)) => Some({
                    let val = match env.get(src.frame, src.index) {
                        Some(val) => Ok(val.clone()),
                        None => Err(undef_var_error!(*loc, v.name.clone())),
                    }?;
                    match val {
                        Value::Class(c) => {
                            env.set(dst.frame, dst.index,
                              Value::Class(Rc::clone(&c)));
                            Ok(c)
                        },
                        _ => {
                            Err(Error {
                                loc: Some(*loc),
                                lexeme: Some(src.name.clone()),
                                details: ErrorDetails::InvalidSuperclass(val),
                            })
                        },
                    }?
                }),
                None => None,
            };

            let mut cls = Class::new(v.name.clone(), sup);
            for (name, def) in methods {
                cls.add_method(name.clone(), def.clone(), Rc::clone(env));
            }
            env.set(v.frame, v.index, Value::Class(Rc::new(cls)));
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
