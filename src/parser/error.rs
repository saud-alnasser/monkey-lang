use chumsky::error::{Simple, SimpleReason};
use std::fmt::Display;

use crate::ast::Token;

#[derive(Debug, thiserror::Error)]
pub struct Error(Simple<Token>);

impl From<Simple<Token>> for Error {
    fn from(value: Simple<Token>) -> Self {
        Self(value)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let error = &self.0;

        match error.reason() {
            SimpleReason::Unexpected => match error.found() {
                Some(found) => {
                    if let Token::ILLEGAL(ch) = found {
                        write!(
                            f,
                            "illegal token got={} at {}:{}",
                            ch,
                            error.span().start,
                            error.span().end
                        )
                    } else {
                        write!(
                            f,
                            "unexpected token got={} at {}:{}",
                            found,
                            error.span().start,
                            error.span().end
                        )
                    }
                }
                None => {
                    write!(
                        f,
                        "missing token got=EOF at {}:{}",
                        error.span().start,
                        error.span().end
                    )
                }
            },
            SimpleReason::Unclosed { delimiter, span } => {
                write!(
                    f,
                    "missing token got={} at {}:{}",
                    delimiter, span.start, span.end
                )
            }
            SimpleReason::Custom(message) => {
                write!(f, "{}", message)
            }
        }
    }
}

pub type Result<T> = std::result::Result<T, Vec<Error>>;
