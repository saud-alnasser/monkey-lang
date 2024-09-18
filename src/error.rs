#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Parser(#[from] crate::parser::error::Error),
    #[error(transparent)]
    Evaluator(#[from] crate::evaluator::error::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
