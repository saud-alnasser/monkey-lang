use std::fmt::Display;

use crate::ast::Token;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,

    // Comparison
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

impl From<Token> for BinaryOperator {
    fn from(token: Token) -> Self {
        match token {
            Token::PLUS => BinaryOperator::Add,
            Token::MINUS => BinaryOperator::Subtract,
            Token::ASTERISK => BinaryOperator::Multiply,
            Token::SLASH => BinaryOperator::Divide,
            Token::EQ => BinaryOperator::Equal,
            Token::NEQ => BinaryOperator::NotEqual,
            Token::LT => BinaryOperator::LessThan,
            Token::GT => BinaryOperator::GreaterThan,
            Token::LTE => BinaryOperator::LessThanOrEqual,
            Token::GTE => BinaryOperator::GreaterThanOrEqual,
            _ => panic!("invalid binary operator"),
        }
    }
}

impl From<&BinaryOperator> for Token {
    fn from(operator: &BinaryOperator) -> Self {
        match operator {
            BinaryOperator::Add => Token::PLUS,
            BinaryOperator::Subtract => Token::MINUS,
            BinaryOperator::Multiply => Token::ASTERISK,
            BinaryOperator::Divide => Token::SLASH,
            BinaryOperator::Equal => Token::EQ,
            BinaryOperator::NotEqual => Token::NEQ,
            BinaryOperator::LessThan => Token::LT,
            BinaryOperator::GreaterThan => Token::GT,
            BinaryOperator::LessThanOrEqual => Token::LTE,
            BinaryOperator::GreaterThanOrEqual => Token::GTE,
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Token::from(self))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Negate,
    Not,
}

impl From<Token> for UnaryOperator {
    fn from(token: Token) -> Self {
        match token {
            Token::MINUS => UnaryOperator::Negate,
            Token::BANG => UnaryOperator::Not,
            _ => panic!("invalid unary operator"),
        }
    }
}

impl From<&UnaryOperator> for Token {
    fn from(operator: &UnaryOperator) -> Self {
        match operator {
            UnaryOperator::Negate => Token::MINUS,
            UnaryOperator::Not => Token::BANG,
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Token::from(self))
    }
}
