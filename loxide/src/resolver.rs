use std::{
    collections::hash_map::HashMap,
    ptr,
    rc::Rc,
};

use crate::{
    builtins::BUILTINS,
    error::{Error, ErrorBundle, ErrorDetails},
    env::{Env, Slot},
    srcloc::SrcLoc,
    syntax::{Expr, FunOrMethod, Stmt},
    value::Value,
};

#[derive(Copy, Clone, Debug)]
enum FunKind {
    Function,
    Method,
    Initializer,
}

#[derive(Copy, Clone, Debug)]
enum ClassKind {
    Class,
    SubClass,
}

#[derive(Copy, Clone, Debug)]
enum GlobalVarState {
    Declared,
    Defined,
    Deferred,
}

#[derive(Copy, Clone, Debug)]
enum LocalVarState {
    Declared,
    Defined,
}

impl From<LocalVarState> for GlobalVarState {
    #[inline]
    fn from(other: LocalVarState) -> Self {
        match other {
            LocalVarState::Declared => GlobalVarState::Declared,
            LocalVarState::Defined => GlobalVarState::Defined,
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct GlobalVarRecord {
    index: usize,
    state: GlobalVarState,
}

#[derive(Copy, Clone, Debug)]
struct LocalVarRecord {
    index: usize,
    state: LocalVarState,
}

#[derive(Clone, Debug)]
struct GlobalScope {
    pub slots: usize,
    pub globals: HashMap<String, GlobalVarRecord>,
}

impl GlobalScope {
    #[inline]
    fn make_slot(&mut self, name: String, state: GlobalVarState) -> Slot {
        let index = self.slots;
        self.slots += 1;
        self.globals.insert(name.clone(), GlobalVarRecord { index, state });
        Slot { name, frame: 0, index }
    }
}

#[derive(Clone, Debug)]
struct FunctionScope {
    pub slots: usize,
    pub locals: HashMap<String, LocalVarRecord>,
    pub parent: Box<Scope>,
}

impl FunctionScope {
    #[inline]
    fn make_slot(&mut self, name: String, state: LocalVarState) -> Slot {
        let index = self.slots;
        self.slots += 1;
        self.locals.insert(name.clone(), LocalVarRecord { index, state });
        Slot { name, frame: 0, index }
    }
}

#[derive(Clone, Debug)]
struct BlockScope {
    pub locals: HashMap<String, LocalVarRecord>,
    pub parent: Box<Scope>,
    pub parent_fun_kind: Option<FunKind>,
    pub parent_class_kind: Option<ClassKind>,
}

impl BlockScope {
    #[inline]
    fn make_slot(&mut self, name: String, state: LocalVarState) -> Slot {
        let mut parent_frame = &mut *self.parent;
        loop {
            match parent_frame {
                Scope::Global(g) => {
                    let index = g.slots;
                    g.slots += 1;
                    self.locals.insert(name.clone(),
                      LocalVarRecord { index, state });
                    return Slot { name, frame: 0, index };
                },

                Scope::Function(f, _, _) => {
                    let index = f.slots;
                    f.slots += 1;
                    self.locals.insert(name.clone(),
                      LocalVarRecord { index, state });
                    return Slot { name, frame: 0, index };
                },

                Scope::Block(b) => {
                    parent_frame = &mut *b.parent;
                },
            };
        }
    }
}

#[derive(Clone, Debug)]
enum Scope {
    Global(GlobalScope),
    Function(FunctionScope, FunKind, Option<ClassKind>),
    Block(BlockScope),
}

// most methods go here!
impl Scope {
    #[inline]
    fn global() -> Self {
        Self::Global(GlobalScope {
            slots: 0,
            globals: HashMap::new(),
        })
    }

    #[inline]
    fn enter_function(self, fun_kind: FunKind,
      class_kind: Option<ClassKind>) -> Self {
        Self::Function(FunctionScope {
            slots: 0,
            locals: HashMap::new(),
            parent: Box::new(self),
        }, fun_kind, class_kind)
    }

    #[inline]
    fn enter_block(self) -> Self {
        let parent_fun_kind = self.fun_kind();
        let parent_class_kind = self.class_kind();
        Self::Block(BlockScope {
            locals: HashMap::new(),
            parent: Box::new(self),
            parent_fun_kind,
            parent_class_kind,
        })
    }

    #[inline]
    fn exit(self) -> Self {
        match self {
            Self::Global(_) => self,
            Self::Function(f, _, _) => *f.parent,
            Self::Block(b) => *b.parent,
        }
    }

    #[inline]
    fn fun_kind(&self) -> Option<FunKind> {
        match self {
            Self::Global(_) => None,
            Self::Function(_, k, _) => Some(*k),
            Self::Block(b) => b.parent_fun_kind,
        }
    }

    #[inline]
    fn class_kind(&self) -> Option<ClassKind> {
        match self {
            Self::Global(_) => None,
            Self::Function(_, _, k) => *k,
            Self::Block(b) => b.parent_class_kind,
        }
    }

    #[inline]
    fn slots(&self) -> usize {
        match self {
            Self::Global(g) => g.slots,
            Self::Function(f, _, _) => f.slots,
            Self::Block(_) => 0,
        }
    }

    fn resolve(&mut self, name: String, loc: SrcLoc) -> Result<Slot, Error> {
        match self {
            // globals can be circular, and failed resolution defers
            Self::Global(g) => {
                if let Some(rec) = g.globals.get(&name) {
                    Ok(Slot { name, frame: 0, index: rec.index })
                } else {
                    Ok(g.make_slot(name.clone(), GlobalVarState::Deferred))
                }
            },

            Self::Function(f, _, _) => {
                if let Some(rec) = f.locals.get(&name) {
                    match rec.state {
                        LocalVarState::Defined => {
                            Ok(Slot { name, frame: 0, index: rec.index })
                        },
                        LocalVarState::Declared => {
                            Err(Error {
                                loc: Some(loc),
                                lexeme: None,
                                details: ErrorDetails::UndefinedVariable(name),
                            })
                        },
                    }
                } else {
                    let mut slot = f.parent.resolve(name, loc)?;
                    slot.frame += 1;
                    Ok(slot)
                }
            },

            Self::Block(b) => {
                if let Some(rec) = b.locals.get(&name) {
                    match rec.state {
                        LocalVarState::Defined => {
                            Ok(Slot { name, frame: 0, index: rec.index })
                        },
                        LocalVarState::Declared => {
                            Err(Error {
                                loc: Some(loc),
                                lexeme: None,
                                details: ErrorDetails::UndefinedVariable(name),
                            })
                        },
                    }
                } else {
                    b.parent.resolve(name, loc)
                }
            },
        }
    }

    fn declare(&mut self, name: String, loc: SrcLoc) -> Result<Slot, Error> {
        match self {
            Self::Global(g) => {
                if let Some(rec) = g.globals.get_mut(&name) {
                    rec.state = GlobalVarState::Declared;
                    Ok(Slot { name, frame: 0, index: rec.index })
                } else {
                    Ok(g.make_slot(name.clone(), GlobalVarState::Declared))
                }
            },

            Self::Function(f, _, _) => {
                if f.locals.contains_key(&name) {
                    Err(Error {
                        loc: Some(loc),
                        lexeme: None,
                        details: ErrorDetails::AlreadyDefined(name),
                    })
                } else {
                    Ok(f.make_slot(name.clone(), LocalVarState::Declared))
                }
            },

            Self::Block(b) => {
                if b.locals.contains_key(&name) {
                    Err(Error {
                        loc: Some(loc),
                        lexeme: None,
                        details: ErrorDetails::AlreadyDefined(name),
                    })
                } else {
                    Ok(b.make_slot(name.clone(), LocalVarState::Declared))
                }
            },
        }
    }

    fn define(&mut self, name: String, loc: SrcLoc) -> Result<Slot, Error> {
        match self {
            Self::Global(g) => {
                if let Some(rec) = g.globals.get_mut(&name) {
                    match rec.state {
                        GlobalVarState::Defined => {
                            Err(Error {
                                loc: Some(loc),
                                lexeme: None,
                                details: ErrorDetails::AlreadyDefined(name),
                            })
                        },
                        _ => {
                            rec.state = GlobalVarState::Defined;
                            Ok(Slot { name, frame: 0, index: rec.index })
                        },
                    }
                } else {
                    Ok(g.make_slot(name.clone(), GlobalVarState::Defined))
                }
            },

            Self::Function(f, _, _) => {
                if let Some(rec) = f.locals.get_mut(&name) {
                    match rec.state {
                        LocalVarState::Defined => {
                            Err(Error {
                                loc: Some(loc),
                                lexeme: None,
                                details: ErrorDetails::AlreadyDefined(name),
                            })
                        },
                        _ => {
                            rec.state = LocalVarState::Defined;
                            Ok(Slot { name, frame: 0, index: rec.index })
                        },
                    }
                } else {
                    Ok(f.make_slot(name.clone(), LocalVarState::Defined))
                }
            },

            Self::Block(b) => {
                if let Some(rec) = b.locals.get_mut(&name) {
                    match rec.state {
                        LocalVarState::Defined => {
                            Err(Error {
                                loc: Some(loc),
                                lexeme: None,
                                details: ErrorDetails::AlreadyDefined(name),
                            })
                        },
                        _ => {
                            rec.state = LocalVarState::Defined;
                            Ok(Slot { name, frame: 0, index: rec.index })
                        },
                    }
                } else {
                    Ok(b.make_slot(name.clone(), LocalVarState::Defined))
                }
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
        let mut scope = Scope::global();
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
                let v = self.scope.resolve(name, loc)?;
                Ok(Expr::Var(v, loc))
            },

            Expr::Assign(name, e, loc) => {
                match (self.scope.resolve(name, loc), self.resolve_expr(*e)) {
                    (Ok(v), Ok(e)) => Ok(Expr::Assign(v, Box::new(e), loc)),
                    (Err(e), Ok(_)) => Err(e.into()),
                    (Ok(_), Err(es)) => Err(es),
                    (Err(e), Err(es)) => {
                        let mut errs: ErrorBundle = e.into();
                        errs.append(es);
                        Err(errs)
                    },
                }
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

            Expr::This(this, loc) => {
                if self.scope.class_kind().is_none() {
                    Err(Error {
                        loc: Some(loc),
                        lexeme: Some(this),
                        details: ErrorDetails::InvalidThis,
                    })?
                } else {
                    let this = self.scope.resolve(this, loc)
                      .expect("internal error: 'this' not bound");
                    Ok(Expr::This(this, loc))
                }
            },

            Expr::PropertyGet(e, name, loc) => {
                Ok(Expr::PropertyGet(
                  Box::new(self.resolve_expr(*e)?), name, loc))
            },

            Expr::PropertySet(lhs, name, rhs, loc) => {
                match (self.resolve_expr(*lhs), self.resolve_expr(*rhs)) {
                    (Ok(lhs), Ok(rhs)) =>
                        Ok(Expr::PropertySet(
                          Box::new(lhs), name, Box::new(rhs), loc)),
                    (Err(es), Ok(_)) => Err(es),
                    (Ok(_), Err(es)) => Err(es),
                    (Err(mut les), Err(res)) => {
                        les.append(res);
                        Err(les)
                    }
                }
            },

            Expr::Super(sup, this, name, loc) => {
                match self.scope.class_kind() {
                    Some(ClassKind::SubClass) => {
                        let sup = self.scope.resolve(sup, loc)
                          .expect("internal error: 'super' not bound");
                        let this = self.scope.resolve(this, loc)
                          .expect("internal error: 'this' not bound");
                        Ok(Expr::Super(sup, this, name, loc))
                    },
                    _ => {
                        Err(Error {
                            loc: Some(loc),
                            lexeme: Some(sup),
                            details: ErrorDetails::InvalidSuper,
                        })?
                    }
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
                match self.scope.fun_kind() {
                    Some(FunKind::Initializer) => {
                        match e {
                            Some(e) =>
                                Err(Error {
                                    loc: Some(*e.location()),
                                    lexeme: None,
                                    details:
                                      ErrorDetails::ExplicitInitializerReturn,
                                })?,
                            None =>
                                Ok(Stmt::Return(None, loc)),
                        }
                    },

                    Some(_) => {
                        match e {
                            Some(e) =>
                                Ok(Stmt::Return(Some(self.resolve_expr(e)?), loc)),
                            None =>
                                Ok(Stmt::Return(None, loc)),
                        }
                    },

                    _ => {
                        Err(Error {
                            loc: Some(loc),
                            lexeme: Some("return".to_string()),
                            details: ErrorDetails::InvalidReturn,
                        })?
                    },
                }
            },

            Stmt::Block(body, loc) => {
                self.enter_block();
                let mut r_body = Vec::new();
                let mut errs = ErrorBundle::new();
                for stmt in body {
                    match self.resolve_stmt(stmt) {
                        Ok(stmt) => r_body.push(stmt),
                        Err(es) => errs.append(es),
                    };
                }
                self.exit_scope();
                if errs.is_empty() {
                    // TODO: block shouldn't carry slots
                    Ok(Stmt::Block(r_body, loc))
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

            Stmt::FunDef(name, def) => {
                let mut errs = ErrorBundle::new();
                let v = match self.scope.declare(name.clone(),
                  def.location) {
                    Ok(slot) => {
                        match self.scope.define(name, def.location) {
                            Ok(_) =>
                                Some(slot),
                            Err(e) => {
                                errs.push(e);
                                None
                            },
                        }
                    },
                    Err(e) => {
                        errs.push(e);
                        None
                    },
                };

                let def = match self.resolve_fun_or_method(
                  FunKind::Function, None, def) {
                    Ok(def) => Some(def),
                    Err(es) => {
                        errs.append(es);
                        None
                    },
                };

                if errs.is_empty() {
                    Ok(Stmt::FunDef(v.unwrap(), def.unwrap()))
                } else {
                    Err(errs)
                }
            },

            Stmt::ClassDef(name, sup, methods, loc) => {
                let mut errs = ErrorBundle::new();
                let v = match self.scope.declare(name.clone(), loc) {
                    Ok(slot) => {
                        match self.scope.define(name.clone(), loc) {
                            Ok(_) =>
                                Some(slot),
                            Err(e) => {
                                errs.push(e);
                                None
                            },
                        }
                    },
                    Err(e) => {
                        errs.push(e);
                        None
                    },
                };

                let class_kind = if sup.is_some() {
                    ClassKind::SubClass
                } else {
                    ClassKind::Class
                };

                let super_slots = if let Some((src, dst)) = sup {
                    self.enter_block();

                    if src == name {
                        errs.push(Error {
                            loc: Some(loc),
                            lexeme: Some(src.clone()),
                            details: ErrorDetails::CircularSuperclass(
                              src.clone()),
                        });
                    }

                    match self.scope.resolve(src, loc) {
                        Ok(src) => {
                            match self.scope.define(dst, loc) {
                                Ok(dst) => Some((src, dst)),
                                Err(e) => {
                                    errs.push(e);
                                    None
                                },
                            }
                        },

                        Err(e) => {
                            errs.push(e);
                            None
                        },
                    }
                } else {
                    None
                };

                let mut r_methods = Vec::new();
                for (name, def) in methods {
                    let fun_kind = if name == "init" {
                        FunKind::Initializer
                    } else {
                        FunKind::Method
                    };

                    match self.resolve_fun_or_method(fun_kind,
                      Some(class_kind), def) {
                        Ok(def) => {
                            r_methods.push((name, def));
                        },

                        Err(es) => {
                            errs.append(es);
                        },
                    };
                }

                match class_kind {
                    ClassKind::SubClass => self.exit_scope(),
                    _ => { },
                };

                if errs.is_empty() {
                    Ok(Stmt::ClassDef(v.unwrap(), super_slots, r_methods, loc))
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

    fn resolve_fun_or_method(&mut self, fun_kind: FunKind,
      class_kind: Option<ClassKind>, def: FunOrMethod<String, ()>
      ) -> Result<FunOrMethod<Slot, usize>, ErrorBundle> {
        let mut errs = ErrorBundle::new();
        self.enter_function(fun_kind, class_kind);

        if class_kind.is_some() {
            self.scope.define("this".to_string(), def.location)
              .expect("internal error: unable to define 'this'");
        }

        let mut parameters = Vec::new();
        for p in def.parameters {
            match self.scope.define(p, def.location) {
                Ok(p) => parameters.push(p),
                Err(e) => errs.push(e),
            };
        }

        let mut body = Vec::new();
        for stmt in def.body {
            match self.resolve_stmt(stmt) {
                Ok(s) => body.push(s),
                Err(es) => errs.append(es),
            };
        }

        let slots = self.scope.slots();

        self.exit_scope();

        if errs.is_empty() {
            Ok(FunOrMethod { parameters, body, slots, location: def.location })
        } else {
            Err(errs)
        }
    }

    /* these suck and I hate them. technically they could blow up if enter_*
     *   panics due to OOM but at that point the world is on fire
     *   so <shrug emoji>
     * but no way in hell am I adding yet another Rc<RefCell> shitshow
     */

    fn enter_block(&mut self) {
        let parent = unsafe { ptr::read(&self.scope) };
        let child = parent.enter_block();
        unsafe { ptr::write(&mut self.scope, child) };
    }

    fn enter_function(&mut self, fun_kind: FunKind,
      class_kind: Option<ClassKind>) {
        let parent = unsafe { ptr::read(&self.scope) };
        let child = parent.enter_function(fun_kind, class_kind);
        unsafe { ptr::write(&mut self.scope, child) };
    }

    fn exit_scope(&mut self) {
        let child = unsafe { ptr::read(&self.scope) };
        let parent = child.exit();
        unsafe { ptr::write(&mut self.scope, parent) };
    }
}
