use crate::{Expression, ExpressionStatement, IntExpression, Statement};

pub enum DataType {
    INT(i64),
    BOOLEAN(bool),
    NULL,
}

pub struct Evaluator;

impl Evaluator {
    pub fn eval(statement: Statement) -> DataType {
        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::INT(IntExpression { value, .. }) => DataType::INT(value),
                _ => DataType::NULL,
            },
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
        let tests = vec![("5;", 5), ("10;", 10)];

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
}
