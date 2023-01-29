pub mod lexer;
pub mod error;
pub mod parser;
pub mod syntax;
pub mod value;

pub use error::{Error, ErrorBundle, ErrorDetails, Result};
pub use lexer::{lex};
pub use parser::{parse_expression};
pub use value::{Value};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
    }
}
