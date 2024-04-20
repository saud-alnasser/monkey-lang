mod ast;
mod quantifiers;

pub use self::ast::*;
use self::quantifiers::Quantifier;
use crate::{lexer::TokenKind, Lexer, Token};
use std::{error::Error, iter::Peekable};

pub struct Parser<'a> {
    quantifiers: Vec<Box<dyn Quantifier>>,
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            // NOTE: order of quantifiers is important
            quantifiers: vec![
                Box::new(quantifiers::LetStatementQuantifier),
                Box::new(quantifiers::ReturnStatementQuantifier),
                Box::new(quantifiers::ExpressionStatementQuantifier),
            ],
            lexer: lexer.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Program, Box<dyn Error>> {
        let mut program = Program::new();

        while let Some(token) = self.lexer.peek() {
            match token {
                token if token.kind == TokenKind::EOF => break,
                _ => {
                    for quantifier in &self.quantifiers {
                        if quantifier.is_applicable(&mut self.lexer) {
                            let statement = quantifier.process(&mut self.lexer)?;

                            program.statements.push(statement);
                        }
                    }
                }
            }
        }

        Ok(program)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "let x = 5; let y = 10;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 2);

        let expected = ["x", "y"];

        for (i, expected) in expected.iter().enumerate() {
            let statement = &program.statements[i];

            match statement {
                Statement::Let { identifier, .. } => {
                    assert_eq!(**identifier, **expected);
                }
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "return 5; return 10;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 2);

        let expected = [5, 10];

        for (i, expected) in expected.iter().enumerate() {
            let statement = &program.statements[i];

            match statement {
                Statement::Return { expression, .. } => match expression {
                    Expression::INT { value, .. } => {
                        assert_eq!(value, expected);
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_identifier_expressions() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression { expression, .. } => match expression {
                Expression::IDENT { value, .. } => {
                    assert_eq!(&**value, "foobar");
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression { expression, .. } => match expression {
                Expression::INT { value, .. } => {
                    assert_eq!(*value, 5);
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let input = "!5; -15;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 2);

        let expected = [(TokenKind::BANG, 5), (TokenKind::MINUS, 15)];

        for (i, (expected_operator, expected_value)) in expected.iter().enumerate() {
            let statement = &program.statements[i];

            match statement {
                Statement::Expression { expression, .. } => match expression {
                    Expression::PREFIX {
                        operator, right, ..
                    } => {
                        assert_eq!(operator, expected_operator);

                        match right.as_ref() {
                            Expression::INT { value, .. } => {
                                assert_eq!(value, expected_value);
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }
    }
}
