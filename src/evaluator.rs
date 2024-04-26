use std::{error::Error, fmt::Display};

use crate::{Expression, Statement, Token, TokenKind};

#[derive(Debug, PartialEq)]
pub enum DataType {
    INT(i64),
    BOOLEAN(bool),
    RETURN(Box<DataType>),
    NULL,
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::INT(value) => write!(f, "{}", value),
            DataType::BOOLEAN(value) => write!(f, "{}", value),
            DataType::RETURN(value) => write!(f, "{}", value),
            DataType::NULL => write!(f, "null"),
        }
    }
}

#[derive(Debug)]
pub enum EvaluationError {
    TypeMismatch(DataType, Token, DataType),
    UnknownOperator(Token),
}

impl Error for EvaluationError {
    fn description(&self) -> &str {
        match self {
            EvaluationError::TypeMismatch(_, _, _) => "type mismatch",
            EvaluationError::UnknownOperator(_) => "unknown operator",
        }
    }
}

impl Display for EvaluationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvaluationError::TypeMismatch(left, token, right) => write!(
                f,
                "type mismatch {} {} {} at {}:{}",
                match left {
                    DataType::INT(_) => "INTEGER",
                    DataType::BOOLEAN(_) => "BOOLEAN",
                    _ => "UNKNOWN",
                },
                token.literal,
                match right {
                    DataType::INT(_) => "INTEGER",
                    DataType::BOOLEAN(_) => "BOOLEAN",
                    _ => "UNKNOWN",
                },
                token.span.line,
                token.span.column
            ),
            EvaluationError::UnknownOperator(token) => {
                write!(
                    f,
                    "unknown operator {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                )
            }
        }
    }
}

pub struct Evaluator;

impl Evaluator {
    fn eval_expression(expression: Expression) -> Result<DataType, Box<dyn Error>> {
        match expression {
            Expression::INT(int) => Ok(DataType::INT(int.value)),
            Expression::BOOLEAN(boolean) => Ok(DataType::BOOLEAN(boolean.value)),
            Expression::PREFIX(prefix) => {
                let right = Evaluator::eval_expression(*prefix.right)?;

                if prefix.operator.kind == TokenKind::BANG {
                    if let DataType::BOOLEAN(value) = right {
                        return Ok(DataType::BOOLEAN(!value));
                    }

                    if let DataType::INT(value) = right {
                        return Ok(DataType::BOOLEAN(value == 0));
                    }
                };

                if prefix.operator.kind == TokenKind::MINUS {
                    if let DataType::INT(value) = right {
                        return Ok(DataType::INT(-value));
                    };
                }

                Err(Box::new(EvaluationError::UnknownOperator(prefix.operator)))
            }
            Expression::INFIX(infix) => {
                let left = Evaluator::eval_expression(*infix.left)?;
                let right = Evaluator::eval_expression(*infix.right)?;
                let operator = infix.operator;

                if let (DataType::INT(left), DataType::INT(right)) = (&left, &right) {
                    match operator.kind {
                        TokenKind::PLUS => return Ok(DataType::INT(left + right)),
                        TokenKind::MINUS => return Ok(DataType::INT(left - right)),
                        TokenKind::ASTERISK => return Ok(DataType::INT(left * right)),
                        TokenKind::SLASH => return Ok(DataType::INT(left / right)),
                        TokenKind::LT => return Ok(DataType::BOOLEAN(left < right)),
                        TokenKind::GT => return Ok(DataType::BOOLEAN(left > right)),
                        TokenKind::LTE => return Ok(DataType::BOOLEAN(left <= right)),
                        TokenKind::GTE => return Ok(DataType::BOOLEAN(left >= right)),
                        TokenKind::EQ => return Ok(DataType::BOOLEAN(left == right)),
                        TokenKind::NEQ => return Ok(DataType::BOOLEAN(left != right)),
                        _ => return Err(Box::new(EvaluationError::UnknownOperator(operator))),
                    }
                }

                if let (DataType::BOOLEAN(left), DataType::BOOLEAN(right)) = (&left, &right) {
                    match operator.kind {
                        TokenKind::EQ => return Ok(DataType::BOOLEAN(left == right)),
                        TokenKind::NEQ => return Ok(DataType::BOOLEAN(left != right)),
                        _ => return Err(Box::new(EvaluationError::UnknownOperator(operator))),
                    }
                }

                Err(Box::new(EvaluationError::TypeMismatch(
                    left, operator, right,
                )))
            }
            Expression::IF(expression) => {
                let condition = match Evaluator::eval_expression(*expression.condition)? {
                    DataType::BOOLEAN(value) => value,
                    DataType::INT(value) => value != 0,
                    _ => false,
                };

                match condition {
                    true => Ok(Evaluator::eval(Statement::Block(expression.consequence))?),
                    false => match expression.alternative {
                        Some(statement) => Ok(Evaluator::eval(Statement::Block(statement))?),
                        None => Ok(DataType::NULL),
                    },
                }
            }
            _ => Ok(DataType::NULL),
        }
    }

    pub fn eval(statement: Statement) -> Result<DataType, Box<dyn Error>> {
        match statement {
            Statement::Block(statement) => {
                let mut result = DataType::NULL;

                for statement in statement.statements {
                    result = Evaluator::eval(statement)?;

                    if let DataType::RETURN(_) = result {
                        return Ok(result);
                    }
                }

                Ok(result)
            }
            Statement::Return(statement) => Ok(DataType::RETURN(Box::new(
                Evaluator::eval_expression(statement.expression)?,
            ))),
            Statement::Expression(statement) => {
                Ok(Evaluator::eval_expression(statement.expression)?)
            }
            _ => Ok(DataType::NULL),
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
                    Ok(DataType::INT(value)) => assert_eq!(value, expected),
                    _ => panic!("expected an integer, got something else"),
                }
            }
        }
    }

    #[test]
    fn test_eval_boolean_expressions() {
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
            ("1 <= 2;", true),
            ("1 >= 2;", false),
            ("1 <= 1;", true),
            ("1 >= 1;", true),
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
            ("1 >= 2 == true;", false),
            ("1 >= 2 == false;", true),
            ("1 <= 1 == true;", true),
            ("1 <= 1 == false;", false),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            for statement in program.statements {
                match Evaluator::eval(statement) {
                    Ok(DataType::BOOLEAN(value)) => assert_eq!(value, expected),
                    _ => panic!("expected a boolean, got something else"),
                }
            }
        }
    }

    #[test]
    fn test_eval_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10; };", 10),
            ("if (false) { 10; };", 0),
            ("if (1) { 10; };", 10),
            ("if (1 < 2) { 10; };", 10),
            ("if (1 > 2) { 10; };", 0),
            ("if (1 > 2) { 10; } else { 20; };", 20),
            ("if (1 < 2) { 10; } else { 20; };", 10),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            for statement in program.statements {
                match Evaluator::eval(statement) {
                    Ok(DataType::INT(value)) => assert_eq!(value, expected),
                    Ok(DataType::NULL) => assert_eq!(expected, 0),
                    _ => panic!("expected an integer, got something else"),
                }
            }
        }
    }

    #[test]
    fn test_eval_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    };

                    return 1;
                };",
                10,
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            for statement in program.statements {
                if let Ok(DataType::RETURN(value)) = Evaluator::eval(statement) {
                    match *value {
                        DataType::INT(value) => assert_eq!(value, expected),
                        _ => panic!("expected an integer, got something else"),
                    }
                }
            }
        }
    }

    #[test]
    fn test_eval_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch INTEGER + BOOLEAN at 1:3"),
            ("5 + true; 5;", "type mismatch INTEGER + BOOLEAN at 1:3"),
            ("-true;", "unknown operator - at 1:1"),
            ("true + false;", "unknown operator + at 1:6"),
            ("5; true + false; 5;", "unknown operator + at 1:9"),
            (
                "if (10 > 1) { true + false; };",
                "unknown operator + at 1:20",
            ),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    };

                    return 1;
                };",
                "unknown operator + at 3:37",
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            for statement in program.statements {
                if let Err(error) = Evaluator::eval(statement) {
                    assert_eq!(error.to_string(), expected);
                }
            }
        }
    }
}
