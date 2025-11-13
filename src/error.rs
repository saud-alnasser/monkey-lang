use chumsky::error::Rich;

use crate::ast::Token;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0:?}")]
    Lexer(Vec<Rich<'static, char>>),
    #[error("{0:?}")]
    Parser(Vec<Rich<'static, Token>>),
}

pub type Result<T> = std::result::Result<T, Error>;
