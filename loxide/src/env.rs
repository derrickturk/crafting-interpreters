use std::{
    cell::{Ref, RefMut, RefCell,},
    rc::Rc,
};

use crate::{
    value::Value,
};

#[derive(Clone, Debug)]
pub struct Slot {
    pub name: String,
    pub frame: usize,
    pub index: usize,
}

// this will all change
#[derive(Clone, Debug)]
pub enum Env {
    Global(RefCell<Vec<Option<Value>>>),
    Local(RefCell<Vec<Value>>, Rc<Env>),
}

impl Env {
    #[inline]
    pub fn new(slots: usize) -> Rc<Self> {
        Rc::new(Env::Global(RefCell::new(vec![None; slots])))
    }

    #[inline]
    pub fn child(self: &Rc<Self>, slots: usize) -> Rc<Self> {
        Rc::new(
          Env::Local(RefCell::new(vec![Value::Nil; slots]), Rc::clone(self)))
    }

    pub fn get(&self, frame: usize, index: usize) -> Option<Ref<Value>> {
        if frame == 0 {
            match self {
                Env::Global(slots) =>
                    Ref::filter_map(slots.borrow(), |v| v[index].as_ref()).ok(),
                Env::Local(slots, _) =>
                    Some(Ref::map(slots.borrow(), |v| &v[index]))
            }
        } else {
            match self {
                Env::Global(_) => panic!("internal error: bad lookup"),
                Env::Local(_, parent) => parent.get(frame - 1, index),
            }
        }
    }

    pub fn get_mut(&self, frame: usize, index: usize) -> Option<RefMut<Value>> {
        if frame == 0 {
            match self {
                Env::Global(slots) =>
                    RefMut::filter_map(slots.borrow_mut(),
                      |v| v[index].as_mut()).ok(),
                Env::Local(slots, _) =>
                    Some(RefMut::map(slots.borrow_mut(), |v| &mut v[index]))
            }
        } else {
            match self {
                Env::Global(_) => panic!("internal error: bad lookup"),
                Env::Local(_, parent) => parent.get_mut(frame - 1, index),
            }
        }
    }

    pub fn set(&self, frame: usize, index: usize, value: Value) {
        if frame == 0 {
            match self {
                Env::Global(slots) =>
                    slots.borrow_mut()[index] = Some(value),
                Env::Local(slots, _) =>
                    slots.borrow_mut()[index] = value,
            }
        } else {
            match self {
                Env::Global(_) => panic!("internal error: bad lookup"),
                Env::Local(_, parent) => parent.set(frame - 1, index, value),
            }
        };
    }

    #[inline]
    pub fn ensure_slots(&self, slots: usize) {
        match self {
            Env::Global(v) => {
                let mut v = v.borrow_mut();
                while v.len() < slots {
                    v.push(None);
                }
            },
            Env::Local(v, _) => {
                let mut v = v.borrow_mut();
                while v.len() < slots {
                    v.push(Value::Nil);
                }
            },
        }
    }
}
