//! Loxide runtime values

use std::fmt;

/// A runtime Loxide value in the interpreter; also used to represent literals
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
}

impl Value {
    #[inline]
    pub fn truthy(&self) -> bool {
        match self {
            Value::Nil | Value::Bool(false) => false,
            _ => true,
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
        }
    }
}
