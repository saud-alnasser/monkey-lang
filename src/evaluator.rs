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
            Expression::INFIX(infix) => {
                let left = Evaluator::eval_expression(*infix.left);
                let right = Evaluator::eval_expression(*infix.right);

                if let (DataType::INT(left), DataType::INT(right)) = (&left, &right) {
                    match infix.operator.kind {
                        TokenKind::PLUS => return DataType::INT(left + right),
                        TokenKind::MINUS => return DataType::INT(left - right),
                        TokenKind::ASTERISK => return DataType::INT(left * right),
                        TokenKind::SLASH => return DataType::INT(left / right),
                        TokenKind::LT => return DataType::BOOLEAN(left < right),
                        TokenKind::GT => return DataType::BOOLEAN(left > right),
                        TokenKind::EQ => return DataType::BOOLEAN(left == right),
                        TokenKind::NEQ => return DataType::BOOLEAN(left != right),
                        _ => (),
                    }
                }

                if let (DataType::BOOLEAN(left), DataType::BOOLEAN(right)) = (&left, &right) {
                    match infix.operator.kind {
                        TokenKind::EQ => return DataType::BOOLEAN(left == right),
                        TokenKind::NEQ => return DataType::BOOLEAN(left != right),
                        _ => (),
                    }
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
    fn test_eval_integer_expressions() {
        let tests = vec![
            ("5;", 5),
            ("10;", 10),
            ("-5;", -5),
            ("-10;", -10),
            ("5 + 5 + 5 + 5 - 10;", 10),
            ("2 * 2 * 2 * 2 * 2;", 32),
            ("-50 + 100 + -50;", 0),
            ("5 * 2 + 10;", 20),
            ("5 + 2 * 10;", 25),
            ("20 + 2 * -10;", 0),
            ("50 / 2 * 2 + 10;", 60),
            ("2 * (5 + 10);", 30),
            ("3 * 3 * 3 + 10;", 37),
            ("3 * (3 * 3) + 10;", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10;", 50),
        ];

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
        let tests = vec![
            ("true;", true),
            ("false;", false),
            ("!true;", false),
            ("!false;", true),
            ("!!true;", true),
            ("!!false;", false),
            ("!5;", false),
            ("!!5;", true),
            ("!-5;", false),
            ("!!-5;", true),
            ("1 < 2;", true),
            ("1 > 2;", false),
            ("1 < 1;", false),
            ("1 > 1;", false),
            ("1 == 1;", true),
            ("1 != 1;", false),
            ("1 == 2;", false),
            ("1 != 2;", true),
            ("true == true;", true),
            ("false == false;", true),
            ("true == false;", false),
            ("true != false;", true),
            ("false != true;", true),
            ("(1 < 2) == true;", true),
            ("(1 < 2) == false;", false),
            ("(1 > 2) == true;", false),
            ("(1 > 2) == false;", true),
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
