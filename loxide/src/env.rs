use std::{
    cell::{Ref, RefMut, RefCell,},
    collections::HashMap,
    rc::Rc,
};

use crate::{
    value::Value,
};

type Scope = RefCell<HashMap<String, Value>>;

// this will all change
pub enum Env {
    Global(Scope),
    Local(Scope, Rc<Env>),
}

impl Env {
    pub fn global() -> Rc<Self> {
        Rc::new(Env::Global(RefCell::new(HashMap::new())))
    }

    pub fn child(self: &Rc<Self>) -> Rc<Self> {
        Rc::new(Env::Local(RefCell::new(HashMap::new()), Rc::clone(self)))
    }

    pub fn get(&self, var: &str) -> Option<Ref<Value>> {
        match self {
            Env::Global(globals) => {
                let s = globals.borrow();
                if let Ok(r) = Ref::filter_map(s, |s| s.get(var)) {
                    Some(r)
                } else {
                    None
                }
            },

            Env::Local(locals, parent) => {
                let s = locals.borrow();
                if let Ok(r) = Ref::filter_map(s, |s| s.get(var)) {
                    Some(r)
                } else {
                    parent.get(var)
                }
            },
        }
    }

    /*
    pub fn get_mut(&mut self, var: &str) -> Option<&mut Value> {
        match self {
            Env::Global(globals) => globals.borrow().get_mut(var),
            Env::Local(locals, parent) =>
                // right, gonna need a refcell
                locals.borrow_mut().get_mut(var)
                  .or_else(|| parent.get_mut(var)),
        }
    }
    */
}
