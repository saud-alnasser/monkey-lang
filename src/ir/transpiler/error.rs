#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("transpilation error: {0}")]
    Generic(String),
}

pub type Result<T> = std::result::Result<T, Error>;
