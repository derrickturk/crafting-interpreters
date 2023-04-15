//! Loxide runtime values

use std::{
    cell::RefCell,
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
    BoundMethod(Rc<(String, FunOrMethod<Slot, usize>)>, Rc<Object>, Rc<Env>),
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

            (Value::BoundMethod(ld, lt, le), Value::BoundMethod(rd, rt, re)) => {
                Rc::as_ptr(ld) == Rc::as_ptr(rd)
                  && Rc::as_ptr(lt) == Rc::as_ptr(rt)
                  && Rc::as_ptr(le) == Rc::as_ptr(re)
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
            Value::BoundMethod(d, o, _) =>
                write!(f, "<bound method {}.{}>", o.class.name, d.0),
        }
    }
}

/// A runtime Lox class
#[derive(Clone, Debug)]
pub struct Class {
    name: String,
    superclass: Option<Rc<Class>>,
    methods: HashMap<String, (Rc<(String, FunOrMethod<Slot, usize>)>, Rc<Env>)>,
}

impl Class {
    #[inline]
    pub fn new(name: String, superclass: Option<Rc<Class>>) -> Self {
        Self {
            name,
            superclass,
            methods: HashMap::new(),
        }
    }

    #[inline]
    pub fn add_method(&mut self, name: String,
      def: FunOrMethod<Slot, usize>, env: Rc<Env>) {
        self.methods.insert(name.clone(), (Rc::new((name, def)), env));
    }

    #[inline]
    pub fn find_method(&self, name: &str
      ) -> Option<(Rc<(String, FunOrMethod<Slot, usize>)>, Rc<Env>)> {
        if let Some(val) = self.methods.get(name) {
            Some(val.clone())
        } else if let Some(sup) = &self.superclass {
            sup.find_method(name)
        } else {
            None
        }
    }

    #[inline]
    pub fn arity(&self) -> usize {
        if let Some((def, _)) = self.find_method("init") {
            def.1.parameters.len()
        } else {
            0
        }
    }

    #[inline]
    pub fn name(&self) -> String {
        self.name.clone()
    }
}

/// A runtime Lox object
#[derive(Clone, Debug)]
pub struct Object {
    class: Rc<Class>,
    fields: RefCell<HashMap<String, Value>>,
}

impl Object {
    #[inline]
    pub fn new(class: Rc<Class>) -> Self {
        Self {
            class,
            fields: RefCell::new(HashMap::new()),
        }
    }

    #[inline]
    pub fn get_property(self: Rc<Self>, name: &str) -> Option<Value> {
        if let Some(f) = self.fields.borrow().get(name) {
            Some(f.clone())
        } else if let Some((def, env)) = self.class.find_method(name) {
            Some(Value::BoundMethod(def, Rc::clone(&self), env))
        } else {
            None
        }
    }

    #[inline]
    pub fn set_property(&self, name: String, value: Value) {
        self.fields.borrow_mut().insert(name, value);
    }

    #[inline]
    pub fn class(&self) -> &Class {
        &self.class
    }
}
