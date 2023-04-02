use std::{
    collections::hash_map::{HashMap, Entry},
    mem,
    rc::Rc,
};

use crate::{
    builtins::BUILTINS,
    error::{Error, ErrorBundle, ErrorDetails},
    env::{Env, Slot},
    srcloc::SrcLoc,
    syntax::{Expr, Stmt},
    value::Value,
};

#[derive(Copy, Clone, Debug)]
enum VarState {
    Declared,
    Defined,
    Deferred,
}

#[derive(Copy, Clone, Debug)]
enum FunKind {
    Function,
}

#[derive(Clone, Debug)]
struct Scope {
    slots: usize,
    locals: HashMap<String, (usize, VarState)>,
    kind: Option<FunKind>,
    parent: Option<Box<Scope>>,
}

// most methods go here!
impl Scope {
    #[inline]
    fn new() -> Self {
        Self {
            slots: 0,
            locals: HashMap::new(),
            kind: None,
            parent: None,
        }
    }

    #[inline]
    fn enter(&mut self) {
        let mut child = Scope::new();
        // the "child" becomes the "parent"
        mem::swap(self, &mut child);
        self.parent = Some(Box::new(child));
    }

    #[inline]
    fn exit(&mut self) {
        match self.parent.take() {
            None => { },
            Some(p) => {
                *self = *p;
            },
        }
    }

    #[inline]
    fn global(&self) -> bool {
        self.parent.is_none()
    }

    #[inline]
    fn slots(&self) -> usize {
        self.slots
    }

    fn resolve(&mut self, name: String) -> Slot {
        if let Some((index, _)) = self.locals.get_mut(&name) {
            Slot {
                name,
                frame: 0,
                index: *index,
            }
        } else if let Some(p) = &mut self.parent {
            let mut slot = p.resolve(name);
            slot.frame += 1;
            slot
        } else {
            let index = self.slots;
            self.slots += 1;
            self.locals.insert(name.clone(), (index, VarState::Deferred));
            Slot {
                name,
                frame: 0,
                index,
            }
        }
    }

    fn declare(&mut self, name: String, loc: SrcLoc) -> Result<Slot, Error> {
        if self.global() {
            match self.locals.entry(name.clone()) {
                Entry::Occupied(mut o) => {
                    o.get_mut().1 = VarState::Declared;
                    Ok(Slot {
                        name,
                        frame: 0,
                        index: o.get().0,
                    })
                },

                Entry::Vacant(v) => {
                    let index = self.slots;
                    self.slots += 1;
                    v.insert((index, VarState::Declared));
                    Ok(Slot {
                        name,
                        frame: 0,
                        index,
                    })
                },
            }
        } else {
            match self.locals.entry(name.clone()) {
                Entry::Occupied(_) => {
                    Err(Error {
                        loc: Some(loc),
                        lexeme: None,
                        details: ErrorDetails::AlreadyDefined(name),
                    })
                },

                Entry::Vacant(v) => {
                    let index = self.slots;
                    self.slots += 1;
                    v.insert((index, VarState::Declared));
                    Ok(Slot {
                        name,
                        frame: 0,
                        index,
                    })
                },
            }
        }
    }

    fn define(&mut self, name: String, loc: SrcLoc) -> Result<Slot, Error> {
        match self.locals.entry(name.clone()) {
            Entry::Occupied(mut o) => {
                match o.get_mut() {
                    (_, VarState::Defined) => {
                        Err(Error {
                            loc: Some(loc),
                            lexeme: None,
                            details: ErrorDetails::AlreadyDefined(name),
                        })
                    },
                    (index, state) => {
                        *state = VarState::Defined;
                        Ok(Slot {
                            name,
                            frame: 0,
                            index: *index,
                        })
                    },
                }
            },

            Entry::Vacant(v) => {
                let index = self.slots;
                self.slots += 1;
                v.insert((index, VarState::Defined));
                Ok(Slot {
                    name,
                    frame: 0,
                    index,
                })
            },
        }
    }
}

pub struct Resolver {
    scope: Scope,
    builtins: Vec<Slot>,
}

// TODO: I hate this ad-hoc validation-applicative stuff

impl Resolver {
    pub fn new() -> Self {
        let mut scope = Scope::new();
        let mut builtins = Vec::new();
        for (name, _, _) in &BUILTINS {
            builtins.push(
              scope.define(name.to_string(), SrcLoc { line: 0, pos: 0 })
                .expect("internal error: resolver can't define builtins"));
        }
        Self { scope, builtins }
    }

    pub fn resolve_expr(&mut self, expr: Expr<String>
      ) -> Result<Expr<Slot>, ErrorBundle> {
        match expr {
            Expr::Literal(val, loc) => {
                Ok(Expr::Literal(val, loc))
            },

            Expr::UnOpApp(op, e, loc) => {
                let e = self.resolve_expr(*e)?;
                Ok(Expr::UnOpApp(op, Box::new(e), loc))
            },

            Expr::BinOpApp(op, lhs, rhs, loc) => {
                match (self.resolve_expr(*lhs), self.resolve_expr(*rhs)) {
                    (Ok(lhs), Ok(rhs)) => Ok(
                        Expr::BinOpApp(op, Box::new(lhs), Box::new(rhs), loc)
                    ),
                    (Err(es), Ok(_)) => Err(es),
                    (Ok(_), Err(es)) => Err(es),
                    (Err(mut les), Err(res)) => {
                        les.append(res);
                        Err(les)
                    },
                }
            },

            Expr::Var(name, loc) => {
                let v = self.scope.resolve(name);
                Ok(Expr::Var(v, loc))
            },

            Expr::Assign(name, e, loc) => {
                let v = self.scope.resolve(name);
                let e = self.resolve_expr(*e)?;
                Ok(Expr::Assign(v, Box::new(e), loc))
            },

            Expr::Call(callee, args, loc) => {
                let mut r_args = Vec::new();
                let mut errs = ErrorBundle::new();

                let callee = match self.resolve_expr(*callee) {
                    Ok(callee) => Some(callee),
                    Err(es) => {
                        errs.append(es);
                        None
                    },
                };

                for a in args {
                    match self.resolve_expr(a) {
                        Ok(a) => r_args.push(a),
                        Err(es) => errs.append(es),
                    };
                }

                if errs.is_empty() {
                    Ok(Expr::Call(Box::new(callee.unwrap()), r_args, loc))
                } else {
                    Err(errs)
                }
            },
        }
    }

    pub fn resolve_stmt(&mut self, stmt: Stmt<String, ()>
      ) -> Result<Stmt<Slot, usize>, ErrorBundle> {
        match stmt {
            Stmt::Expr(e, loc) => {
                let e = self.resolve_expr(e)?;
                Ok(Stmt::Expr(e, loc))
            },

            Stmt::IfElse(cond, s_if, s_else, loc) => {
                let mut errs = ErrorBundle::new();

                let cond = match self.resolve_expr(cond) {
                    Ok(cond) => Some(cond),
                    Err(es) => {
                        errs.append(es);
                        None
                    },
                };

                let s_if = match self.resolve_stmt(*s_if) {
                    Ok(s_if) => Some(s_if),
                    Err(es) => {
                        errs.append(es);
                        None
                    },
                };

                let s_else = match s_else.map(|s| self.resolve_stmt(*s)) {
                    Some(Ok(s_else)) => Some(Box::new(s_else)),
                    Some(Err(es)) => {
                        errs.append(es);
                        None
                    },
                    None => None,
                };

                if errs.is_empty() {
                    Ok(Stmt::IfElse(
                      cond.unwrap(), Box::new(s_if.unwrap()), s_else, loc))
                } else {
                    Err(errs)
                }
            },

            Stmt::While(cond, body, loc) => {
                match (self.resolve_expr(cond), self.resolve_stmt(*body)) {
                    (Ok(cond), Ok(body)) =>
                        Ok(Stmt::While(cond, Box::new(body), loc)),
                    (Err(es), Ok(_)) =>
                        Err(es),
                    (Ok(_), Err(es)) =>
                        Err(es),
                    (Err(mut ces), Err(bes)) => {
                        ces.append(bes);
                        Err(ces)
                    },
                }
            },

            Stmt::Print(e, loc) => {
                Ok(Stmt::Print(self.resolve_expr(e)?, loc))
            },

            Stmt::Return(e, loc) => {
                if let Some(_) = self.scope.kind {
                    match e {
                        Some(e) =>
                            Ok(Stmt::Return(Some(self.resolve_expr(e)?), loc)),
                        None =>
                            Ok(Stmt::Return(None, loc)),
                    }
                } else {
                    let mut es = ErrorBundle::new();
                    es.push(Error {
                        loc: Some(loc),
                        lexeme: Some("return".to_string()),
                        details: ErrorDetails::InvalidReturn,
                    });
                    Err(es)
                }
            },

            Stmt::Block(body, (), loc) => {
                self.scope.enter();
                let mut r_body = Vec::new();
                let mut errs = ErrorBundle::new();
                for stmt in body {
                    match self.resolve_stmt(stmt) {
                        Ok(stmt) => r_body.push(stmt),
                        Err(es) => errs.append(es),
                    };
                }
                let slots = self.scope.slots();
                self.scope.exit();
                if errs.is_empty() {
                    Ok(Stmt::Block(r_body, slots, loc))
                } else {
                    Err(errs)
                }
            },

            Stmt::VarDecl(name, init, loc) => {
                let v = self.scope.declare(name.clone(), loc)?;
                let init = match init {
                    Some(e) => Some(self.resolve_expr(e)?),
                    None => None,
                };
                self.scope.define(name.clone(), loc)?;
                Ok(Stmt::VarDecl(v, init, loc))
            },

            Stmt::FunDef(name, params, body, (), loc) =>  {
                let mut errs = ErrorBundle::new();
                let v = match self.scope.declare(name, loc) {
                    Ok(slot) => Some(slot),
                    Err(e) => {
                        errs.push(e);
                        None
                    },
                };
                self.scope.enter();
                self.scope.kind = Some(FunKind::Function);
                let mut r_params = Vec::new();
                for p in params {
                    match self.scope.define(p, loc) {
                        Ok(p) => r_params.push(p),
                        Err(e) => errs.push(e),
                    };
                }
                let mut r_body = Vec::new();
                for stmt in body {
                    match self.resolve_stmt(stmt) {
                        Ok(s) => r_body.push(s),
                        Err(es) => errs.append(es),
                    };
                }
                let slots = self.scope.slots();
                self.scope.exit();
                if errs.is_empty() {
                    Ok(Stmt::FunDef(v.unwrap(), r_params, r_body, slots, loc))
                } else {
                    Err(errs)
                }
            },
        }
    }

    pub fn resolve(&mut self, program: Vec<Stmt<String, ()>>
      ) -> Result<Vec<Stmt<Slot, usize>>, ErrorBundle> {
        let mut resolved = Vec::new();
        let mut errs = ErrorBundle::new();
        for stmt in program {
            match self.resolve_stmt(stmt) {
                Ok(s) => resolved.push(s),
                Err(es) => errs.append(es),
            };
        }
        if errs.is_empty() {
            Ok(resolved)
        } else {
            Err(errs)
        }
    }

    #[inline]
    pub fn initialize_env(&self) -> Rc<Env> {
        let env = Env::new(self.scope.slots());
        for (b, &(name, n, ptr)) in self.builtins.iter().zip(BUILTINS.iter()) {
            env.set(b.frame, b.index, Value::BuiltinFun(name, n, ptr));
        }
        env
    }

    #[inline]
    pub fn update_env(&self, env: &Rc<Env>) {
        env.ensure_slots(self.scope.slots())
    }
}
