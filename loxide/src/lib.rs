pub mod lexer;
pub mod error;
pub mod parser;
pub mod syntax;
pub mod value;

pub use lexer::{lex};
pub use error::{Error, ErrorDetails, Result};
pub use value::{Value};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
    }
}
