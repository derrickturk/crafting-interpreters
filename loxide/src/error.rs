//! Error types for Loxide

use std::{
    error,
    fmt::{self, Display, Formatter},
};

use crate::srcloc::SrcLoc;

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
    ParseExpected(&'static str),
    AlreadyDefined(String),
    UndefinedVariable(String),
    UnexpectedCharacter(char),
    UnterminatedStrLit,
    TypeError(&'static str),
}

impl Display for ErrorDetails {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ErrorDetails::ParseExpected(expected) =>
                write!(f, "expected {}", expected),
            ErrorDetails::AlreadyDefined(var) =>
                write!(f, "variable {} already defined in scope", var),
            ErrorDetails::UndefinedVariable(var) =>
                write!(f, "undefined variable {}", var),
            ErrorDetails::UnexpectedCharacter(c) =>
                write!(f, "unexpected character {}", c),
            ErrorDetails::UnterminatedStrLit =>
                write!(f, "unterminated string literal"),
            ErrorDetails::TypeError(msg) =>
                write!(f, "type error: {}", msg),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Debug)]
pub struct ErrorBundle(pub Vec<Error>);

impl ErrorBundle {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, err: Error) {
        self.0.push(err);
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
