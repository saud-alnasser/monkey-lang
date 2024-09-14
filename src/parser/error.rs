use std::fmt::Display;

use crate::Token;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    MissingToken,
    MissingOpeningParenthesis(Option<Token>),
    MissingClosingParenthesis(Option<Token>),
    MissingOpeningBrace(Option<Token>),
    MissingClosingBrace(Option<Token>),
    MissingOpeningBracket(Option<Token>),
    MissingClosingBracket(Option<Token>),
    MissingLetKeyword(Option<Token>),
    MissingAssignmentOperator(Option<Token>),
    MissingReturnKeyword(Option<Token>),
    MissingSemicolon(Option<Token>),
    MissingIdentifier(Option<Token>),
    MissingBlockStatement(Option<Token>),
    UnexpectedToken(Token),
    IllegalToken(Token),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::MissingToken => write!(f, "missing a token"),
            Error::MissingOpeningParenthesis(token) => match token {
                Some(token) => write!(
                    f,
                    "expected opening parenthesis, got {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                ),
                None => write!(f, "expected opening parenthesis, got EOF"),
            },
            Error::MissingClosingParenthesis(token) => match token {
                Some(token) => write!(
                    f,
                    "expected closing parenthesis, got {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                ),
                None => write!(f, "expected closing parenthesis, got EOF"),
            },
            Error::MissingOpeningBrace(token) => match token {
                Some(token) => write!(
                    f,
                    "expected opening brace, got {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                ),
                None => write!(f, "expected opening brace, got EOF"),
            },
            Error::MissingClosingBrace(token) => match token {
                Some(token) => write!(
                    f,
                    "expected closing brace, got {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                ),
                None => write!(f, "expected closing brace, got EOF"),
            },
            Error::MissingOpeningBracket(token) => match token {
                Some(token) => write!(
                    f,
                    "expected opening bracket, got {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                ),
                None => write!(f, "expected opening bracket, got EOF"),
            },
            Error::MissingClosingBracket(token) => match token {
                Some(token) => write!(
                    f,
                    "expected closing bracket, got {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                ),
                None => write!(f, "expected closing bracket, got EOF"),
            },
            Error::MissingLetKeyword(token) => match token {
                Some(token) => write!(
                    f,
                    "expected let keyword, got {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                ),
                None => write!(f, "expected let keyword, got EOF"),
            },
            Error::MissingAssignmentOperator(token) => match token {
                Some(token) => write!(
                    f,
                    "expected assignment operator, got {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                ),
                None => write!(f, "expected assignment operator, got EOF"),
            },
            Error::MissingReturnKeyword(token) => match token {
                Some(token) => write!(
                    f,
                    "expected return keyword, got {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                ),
                None => write!(f, "expected return keyword, got EOF"),
            },
            Error::MissingSemicolon(token) => match token {
                Some(token) => write!(
                    f,
                    "expected semicolon, got {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                ),
                None => write!(f, "expected semicolon, got EOF"),
            },
            Error::MissingIdentifier(token) => match token {
                Some(token) => write!(
                    f,
                    "expected identifier, got {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                ),
                None => write!(f, "expected identifier, got EOF"),
            },
            Error::MissingBlockStatement(token) => match token {
                Some(token) => write!(
                    f,
                    "expected block statement, got {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                ),
                None => write!(f, "expected block statement, got EOF"),
            },
            Error::UnexpectedToken(token) => write!(
                f,
                "unexpected token {} at {}:{}",
                token.literal, token.span.line, token.span.column
            ),
            Error::IllegalToken(token) => write!(
                f,
                "illegal token {} at {}:{}",
                token.literal, token.span.line, token.span.column
            ),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
