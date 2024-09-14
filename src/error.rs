#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("parse error: {0}")]
    Parser(#[from] crate::parser::error::Error),
    #[error("evaluation error: {0}")]
    Evaluator(#[from] crate::evaluator::error::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
