use std::fmt::Display;

use super::{Expression, IdentifierExpression};

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let {
        identifier: IdentifierExpression,
        expression: Expression,
    },
    Return(Expression),
    Expression(Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let {
                identifier,
                expression,
            } => write!(f, "let {} = {};", identifier, expression),
            Statement::Return(expression) => write!(f, "return {};", expression),
            Statement::Expression(expression) => write!(f, "{};", expression),
        }
    }
}
