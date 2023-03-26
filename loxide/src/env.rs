use std::{
    cell::{Ref, RefMut, RefCell,},
    collections::hash_map::{Entry, HashMap},
    rc::Rc,
};

use crate::{
    value::Value,
};

pub struct Scope(RefCell<HashMap<String, Value>>);

impl Scope {
    pub fn new() -> Self {
        Self(RefCell::new(HashMap::new()))
    }

    pub fn init(&self, var: String, value: Value) -> bool {
        match self.0.borrow_mut().entry(var) {
            Entry::Occupied(_) => false,
            Entry::Vacant(v) => {
                v.insert(value);
                true
            }
        }
    }

    pub fn get(&self, var: &str) -> Option<Ref<Value>> {
        Ref::filter_map(self.0.borrow(), |s| s.get(var)).ok()
    }

    pub fn get_mut(&self, var: &str) -> Option<RefMut<Value>> {
        RefMut::filter_map(self.0.borrow_mut(), |s| s.get_mut(var)).ok()
    }
}

// this will all change
pub enum Env {
    Global(Scope),
    Local(Scope, Rc<Env>),
}

impl Env {
    pub fn new() -> Rc<Self> {
        Rc::new(Env::Global(Scope::new()))
    }

    pub fn child(self: &Rc<Self>) -> Rc<Self> {
        Rc::new(Env::Local(Scope::new(), Rc::clone(self)))
    }

    pub fn declare(&self, var: String, value: Value) -> bool {
        match self {
            Env::Global(globals) => globals.init(var, value),
            Env::Local(locals, _) => locals.init(var, value),
        }
    }

    pub fn get(&self, var: &str) -> Option<Ref<Value>> {
        match self {
            Env::Global(globals) => globals.get(var),
            Env::Local(locals, parent) =>
                locals.get(var).or_else(|| parent.get(var)),
        }
    }

    /* oddity: because borrow-checking is dynamic for Envs, we  take
     * &self rather than &mut self here! */
    pub fn get_mut(&self, var: &str) -> Option<RefMut<Value>> {
        match self {
            Env::Global(globals) => globals.get_mut(var),
            Env::Local(locals, parent) =>
                locals.get_mut(var).or_else(|| parent.get_mut(var)),
        }
    }
}
