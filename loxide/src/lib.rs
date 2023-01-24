pub mod lex;
pub mod error;
pub mod syntax;
pub mod value;

pub use error::{Error, ErrorDetails, Result};
pub use value::{Value};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
    }
}
