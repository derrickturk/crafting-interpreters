//! Error types for Loxide

use std::{
    error,
    fmt::{self, Display, Formatter},
};

/// Any old error
#[derive(Clone, Debug)]
pub struct Error {
    pub line: usize,
    pub wurr: String, // wurr is it
    pub details: ErrorDetails,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "[line {}] Error{}: {}", self.line, self.wurr, self.details)
    }
}

impl error::Error for Error { }

/// The details of what went wrong
#[derive(Clone, Debug)]
pub enum ErrorDetails {
    ParseError { expected: String, found: String },
    UnexpectedCharacter(char),
    UnterminatedStrLit,
}

impl Display for ErrorDetails {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ErrorDetails::ParseError { expected, found } =>
                write!(f, "expected {}; found {}", expected, found),
            ErrorDetails::UnexpectedCharacter(c) =>
                write!(f, "unexpected character {}", c),
            ErrorDetails::UnterminatedStrLit =>
                write!(f, "unterminated string literal"),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
