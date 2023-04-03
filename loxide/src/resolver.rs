use std::{
    collections::hash_map::{HashMap, Entry},
    ptr,
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
enum FunKind {
    TopLevel,
    Function,
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
struct FunctionVarRecord {
    index: usize,
    state: LocalVarState,
}

// TODO
#[derive(Copy, Clone, Debug)]
struct BlockVarRecord {
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
    pub locals: HashMap<String, FunctionVarRecord>,
    pub parent: Box<Scope>,
}

impl FunctionScope {
    #[inline]
    fn make_slot(&mut self, name: String, state: LocalVarState) -> Slot {
        let index = self.slots;
        self.slots += 1;
        self.locals.insert(name.clone(), FunctionVarRecord { index, state });
        Slot { name, frame: 0, index }
    }
}

#[derive(Clone, Debug)]
struct BlockScope {
    pub locals: HashMap<String, BlockVarRecord>,
    pub parent: Box<Scope>,
    pub parent_kind: FunKind,
}

impl BlockScope {
    #[inline]
    fn make_slot(&mut self, name: String, state: LocalVarState) -> Slot {
        let mut parent_frame = &mut *self.parent;
        loop {
            match parent_frame {
                Scope::Global(g) => {
                    return g.make_slot(name, state.into());
                },

                Scope::Function(f) => {
                    return f.make_slot(name, state.into());
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
    Function(FunctionScope),
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
    fn enter_function(self) -> Self {
        Self::Function(FunctionScope {
            slots: 0,
            locals: HashMap::new(),
            parent: Box::new(self),
        })
    }

    #[inline]
    fn enter_block(self) -> Self {
        let parent_kind = self.kind();
        Self::Block(BlockScope {
            locals: HashMap::new(),
            parent: Box::new(self),
            parent_kind,
        })
    }

    #[inline]
    fn exit(self) -> Self {
        match self {
            Self::Global(_) => self,
            Self::Function(f) => *f.parent,
            Self::Block(b) => *b.parent,
        }
    }

    #[inline]
    fn kind(&self) -> FunKind {
        match self {
            Self::Global(_) => FunKind::TopLevel,
            Self::Function(_) => FunKind::Function,
            Self::Block(b) => b.parent_kind,
        }
    }

    #[inline]
    fn slots(&self) -> usize {
        match self {
            Self::Global(g) => g.slots,
            Self::Function(f) => f.slots,
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

            Self::Function(f) => {
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

            Self::Function(f) => {
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
        /*
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
        */
        todo!()
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
                match self.scope.kind() {
                    FunKind::Function => {
                        match e {
                            Some(e) =>
                                Ok(Stmt::Return(Some(self.resolve_expr(e)?), loc)),
                            None =>
                                Ok(Stmt::Return(None, loc)),
                        }
                    },

                    _ => {
                        let mut es = ErrorBundle::new();
                        es.push(Error {
                            loc: Some(loc),
                            lexeme: Some("return".to_string()),
                            details: ErrorDetails::InvalidReturn,
                        });
                        Err(es)
                    },
                }
            },

            Stmt::Block(body, (), loc) => {
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
                    Ok(Stmt::Block(r_body, 0, loc))
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

            Stmt::FunDef(name, params, body, (), loc) => {
                let mut errs = ErrorBundle::new();
                let v = match self.scope.declare(name.clone(), loc) {
                    Ok(slot) => Some(slot),
                    Err(e) => {
                        errs.push(e);
                        None
                    },
                };
                match self.scope.define(name, loc) {
                    Ok(_) => { },
                    Err(e) => {
                        errs.push(e);
                    },
                };
                self.enter_function();
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
                self.exit_scope();
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

    fn enter_function(&mut self) {
        let parent = unsafe { ptr::read(&self.scope) };
        let child = parent.enter_function();
        unsafe { ptr::write(&mut self.scope, child) };
    }

    fn exit_scope(&mut self) {
        let child = unsafe { ptr::read(&self.scope) };
        let parent = child.exit();
        unsafe { ptr::write(&mut self.scope, parent) };
    }
}
