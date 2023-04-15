//! Error types for Loxide

use std::{
    error,
    fmt::{self, Display, Formatter},
};

use crate::{
    srcloc::SrcLoc,
    value::Value,
};

/// Any old error
#[derive(Clone, Debug)]
pub struct Error {
    pub loc: Option<SrcLoc>,
    pub lexeme: Option<String>,
    pub details: ErrorDetails,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(loc) = self.loc {
            write!(f, "[line {}, char {}] Error", loc.line, loc.pos)?;
        } else {
            write!(f, "[end of input] Error")?;
        }

        if let Some(lexeme) = &self.lexeme {
            write!(f, " at \"{}\"", lexeme)?;
        }

        write!(f, ": {}", self.details)
    }
}

impl error::Error for Error { }

/// The details of what went wrong
#[derive(Clone, Debug)]
pub enum ErrorDetails {
    AlreadyDefined(String),
    ArityMismatch(String, usize, usize),
    CircularDefinition(String),
    CircularSuperclass(String),
    ExplicitInitializerReturn,
    InvalidReturn,
    InvalidSuper,
    InvalidSuperclass(Value),
    InvalidThis,
    NotLValue(String),
    ParseExpected(&'static str),
    Return(Value),
    TooManyArgs,
    TypeError(&'static str),
    UndefinedProperty(String),
    UndefinedVariable(String),
    UnexpectedCharacter(char),
    UnterminatedStrLit,
}

impl Display for ErrorDetails {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ErrorDetails::AlreadyDefined(var) =>
                write!(f, "variable {} already defined in scope", var),
            ErrorDetails::ArityMismatch(name, expected, got) =>
                write!(f,
                  "function/method {} expected {} arguments, received {}",
                  name, expected, got),
            ErrorDetails::CircularDefinition(var) =>
                write!(f, "local variable {} used in own definition", var),
            ErrorDetails::CircularSuperclass(cls) =>
                write!(f, "class {} used as own superclass", cls),
            ErrorDetails::ExplicitInitializerReturn =>
                write!(f, "explicit return value from init method"),
            ErrorDetails::InvalidReturn =>
                write!(f, "return outside function or method"),
            ErrorDetails::InvalidSuper =>
                write!(f, "super outside subclass method"),
            ErrorDetails::InvalidSuperclass(v) =>
                write!(f, "invalid superclass {}", v),
            ErrorDetails::InvalidThis =>
                write!(f, "this outside method"),
            ErrorDetails::NotLValue(what) =>
                write!(f, "{} is not a valid assignment target", what),
            ErrorDetails::ParseExpected(expected) =>
                write!(f, "expected {}", expected),
            ErrorDetails::Return(v) =>
                write!(f, "return {} [you shouldn't see this!]", v),
            ErrorDetails::TooManyArgs =>
                write!(f, "more than 255 arguments or parameters"),
            ErrorDetails::TypeError(msg) =>
                write!(f, "type error: {}", msg),
            ErrorDetails::UndefinedProperty(name) =>
                write!(f, "undefined property {}", name),
            ErrorDetails::UndefinedVariable(var) =>
                write!(f, "undefined variable {}", var),
            ErrorDetails::UnexpectedCharacter(c) =>
                write!(f, "unexpected character {}", c),
            ErrorDetails::UnterminatedStrLit =>
                write!(f, "unterminated string literal"),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Debug)]
pub struct ErrorBundle(Vec<Error>);

impl ErrorBundle {
    #[inline]
    pub fn new() -> Self {
        Self(Vec::new())
    }

    #[inline]
    pub fn push(&mut self, err: Error) {
        self.0.push(err);
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    pub fn append(&mut self, mut other: Self) {
        self.0.append(&mut other.0);
    }
}

impl Display for ErrorBundle {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for err in &self.0 {
            write!(f, "{}\n", err)?;
        }
        Ok(())
    }
}

impl error::Error for ErrorBundle { }

impl From<Error> for ErrorBundle {
    fn from(value: Error) -> Self {
        Self(vec![value])
    }
}
