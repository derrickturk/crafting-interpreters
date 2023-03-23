pub mod lexer;
pub mod env;
pub mod error;
pub mod interpreter;
pub mod parser;
pub mod srcloc;
pub mod syntax;
pub mod value;

pub use {
    error::{Error, ErrorBundle, ErrorDetails, Result},
    lexer::{lex},
    interpreter::{eval_expr},
    parser::{parse_expr},
    srcloc::{SrcLoc},
    value::{Value},
};
