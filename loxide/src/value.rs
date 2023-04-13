//! Loxide runtime values

use std::{
    collections::HashMap,
    fmt,
    rc::Rc,
};

use crate::{
    env::{Env, Slot},
    syntax::FunOrMethod,
};

/// A runtime Loxide value in the interpreter; also used to represent literals
#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(Rc<String>),
    Fun(Rc<(String, FunOrMethod<Slot, usize>)>, Rc<Env>),
    BuiltinFun(&'static str, usize, fn(Vec<Value>) -> Value),
    Class(Rc<Class>),
    Object(Rc<Object>),
}

impl Value {
    #[inline]
    pub fn truthy(&self) -> bool {
        match self {
            Value::Nil | Value::Bool(false) => false,
            _ => true,
        }
    }

    #[inline]
    pub fn print_string(&self) -> String {
        match self {
            Value::String(s) => (**s).clone(),
            _ => format!("{}", self),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::Number(l), Value::Number(r)) => l == r,
            (Value::String(l), Value::String(r)) => l == r,
            (Value::Fun(ld, le), Value::Fun(rd, re)) => {
                Rc::as_ptr(ld) == Rc::as_ptr(rd)
                  && Rc::as_ptr(le) == Rc::as_ptr(re)
            },
            (Value::BuiltinFun(_, _, l), Value::BuiltinFun(_, _, r)) => l == r,
            (Value::Class(l), Value::Class(r)) => {
                Rc::as_ptr(l) == Rc::as_ptr(r)
            },
            (Value::Object(l), Value::Object(r)) => {
                Rc::as_ptr(l) == Rc::as_ptr(r)
            },
            (_, _) => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(true) => write!(f, "true"),
            Value::Bool(false) => write!(f, "false"),
            Value::Number(n) => write!(f, "{}", n),
            // NOTE: assumes no escaping; revisit if needed
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Fun(d, _) => write!(f, "<function {}>", d.0),
            Value::BuiltinFun(name, _, _) =>
                write!(f, "<built-in function {}>", name),
            Value::Class(c) =>
                write!(f, "<class {}>", c.name),
            Value::Object(o) =>
                write!(f, "<{} object>", o.class.name),
        }
    }
}

/// A runtime Lox class
#[derive(Clone, Debug)]
pub struct Class {
    pub name: String,
    pub superclass: Option<Rc<Class>>,
    pub methods: HashMap<String, Value>,
}

/// A runtime Lox object
#[derive(Clone, Debug)]
pub struct Object {
    pub class: Rc<Class>,
    pub fields: HashMap<String, Value>,
}
