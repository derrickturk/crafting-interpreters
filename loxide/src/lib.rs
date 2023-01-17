pub mod lex;
pub mod error;

pub use error::{Error, ErrorDetails, Result};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
    }
}
