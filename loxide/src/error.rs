//! Error types for Loxide

use std::{
    error,
    fmt::{self, Display, Formatter},
};

/// Any old error
#[derive(Clone, Debug)]
pub struct Error {
    pub line: Option<usize>,
    pub wurr: String, // wurr is it
    pub details: ErrorDetails,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.line {
            Some(line) => write!(f, "[line {}] Error{}: {}",
              line, self.wurr, self.details),
            None => write!(f, "[end of input] Error{}: {}",
              self.wurr, self.details),
        }
    }
}

impl error::Error for Error { }

/// The details of what went wrong
#[derive(Copy, Clone, Debug)]
pub enum ErrorDetails {
    ParseExpected(&'static str),
    UnexpectedCharacter(char),
    UnterminatedStrLit,
}

impl Display for ErrorDetails {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ErrorDetails::ParseExpected(expected) =>
                write!(f, "expected {}", expected),
            ErrorDetails::UnexpectedCharacter(c) =>
                write!(f, "unexpected character {}", c),
            ErrorDetails::UnterminatedStrLit =>
                write!(f, "unterminated string literal"),
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
