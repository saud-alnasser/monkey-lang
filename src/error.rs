#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("parse error: {0}")]
    Parser(#[from] crate::parser::error::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
