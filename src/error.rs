use chumsky::error::Simple;

use crate::ast::Token;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    Lexer(Simple<char>),
    #[error("{0}")]
    Parser(Simple<Token>),
}

pub type Result<T> = std::result::Result<T, Error>;
