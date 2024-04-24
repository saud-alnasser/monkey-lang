use std::fmt::Display;

use crate::{Expression, Statement, TokenKind};

#[derive(Debug, PartialEq)]
pub enum DataType {
    INT(i64),
    BOOLEAN(bool),
    NULL,
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::INT(value) => write!(f, "{}", value),
            DataType::BOOLEAN(value) => write!(f, "{}", value),
            DataType::NULL => write!(f, "null"),
        }
    }
}

pub struct Evaluator;

impl Evaluator {
    fn eval_expression(expression: Expression) -> DataType {
        match expression {
            Expression::INT(int) => DataType::INT(int.value),
            Expression::BOOLEAN(boolean) => DataType::BOOLEAN(boolean.value),
            Expression::PREFIX(prefix) => {
                let right = Evaluator::eval_expression(*prefix.right);

                if prefix.operator.kind == TokenKind::BANG {
                    if let DataType::BOOLEAN(value) = right {
                        return DataType::BOOLEAN(!value);
                    }

                    if let DataType::INT(value) = right {
                        return DataType::BOOLEAN(value == 0);
                    }
                };

                if prefix.operator.kind == TokenKind::MINUS {
                    if let DataType::INT(value) = right {
                        return DataType::INT(-value);
                    };
                }

                DataType::NULL
            }
            _ => DataType::NULL,
        }
    }

    pub fn eval(statement: Statement) -> DataType {
        match statement {
            Statement::Expression(statement) => Evaluator::eval_expression(statement.expression),
            _ => DataType::NULL,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Lexer, Parser};

    use super::*;

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![("5;", 5), ("10;", 10), ("-5;", -5), ("-10;", -10)];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            for statement in program.statements {
                match Evaluator::eval(statement) {
                    DataType::INT(value) => assert_eq!(value, expected),
                    _ => panic!("expected an integer, got something else"),
                }
            }
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![("true;", true), ("false;", false)];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            for statement in program.statements {
                match Evaluator::eval(statement) {
                    DataType::BOOLEAN(value) => assert_eq!(value, expected),
                    _ => panic!("expected a boolean, got something else"),
                }
            }
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true;", false),
            ("!false;", true),
            ("!!true;", true),
            ("!!false;", false),
            ("!5;", false),
            ("!!5;", true),
            ("!-5;", false),
            ("!!-5;", true),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            for statement in program.statements {
                match Evaluator::eval(statement) {
                    DataType::BOOLEAN(value) => assert_eq!(value, expected),
                    _ => panic!("expected a boolean, got something else"),
                }
            }
        }
    }
}
