use std::error::Error;

use crate::{Expression, Lexer, Precedence, Program, Statement, TokenKind};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser { lexer }
    }

    fn parse_expression(
        lexer: &mut Lexer,
        precedence: Precedence,
    ) -> Result<Expression, Box<dyn Error>> {
        let mut left = match lexer.peek() {
            Some(token) if token.kind == TokenKind::IDENT => {
                let token = lexer.next().unwrap();
                let value = token.literal.clone();

                Expression::IDENT { token, value }
            }
            Some(token) if token.kind == TokenKind::INT => {
                let token = lexer.next().unwrap();
                let value = token.literal.parse::<i64>()?;

                Expression::INT { token, value }
            }
            Some(token) if token.kind == TokenKind::MINUS || token.kind == TokenKind::BANG => {
                let operator = lexer.next().unwrap();
                let right = Box::new(Parser::parse_expression(lexer, Precedence::PREFIX)?);

                Expression::PREFIX { operator, right }
            }
            _ => return Err("Expected expression".into()),
        };

        while let Some(next) = lexer.peek() {
            if precedence >= Precedence::from(&next.kind) {
                break;
            }

            let operator = lexer.next().unwrap();
            let right = Parser::parse_expression(lexer, Precedence::from(&operator.kind))?;

            left = Expression::INFIX {
                operator: operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_statement(lexer: &mut Lexer) -> Result<Statement, Box<dyn Error>> {
        match lexer.peek() {
            Some(token) if token.kind == TokenKind::LET => {
                let token = match lexer.next() {
                    Some(token) if token.kind == TokenKind::LET => token,
                    _ => return Err("Expected keyword let".into()),
                };

                let identifier = match lexer.next() {
                    Some(token) if token.kind == TokenKind::IDENT => token.literal,
                    _ => return Err("Expected identifier".into()),
                };

                match lexer.next() {
                    Some(token) if token.kind == TokenKind::ASSIGN => (),
                    _ => return Err("Expected assignment operator".into()),
                }

                let expression = Parser::parse_expression(lexer, Precedence::LOWEST)?;

                match lexer.next() {
                    Some(token) if token.kind == TokenKind::SEMICOLON => (),
                    _ => return Err("Expected semicolon".into()),
                }

                Ok(Statement::Let {
                    token,
                    identifier,
                    expression,
                })
            }
            Some(token) if token.kind == TokenKind::RETURN => {
                let token = match lexer.next() {
                    Some(token) if token.kind == TokenKind::RETURN => token,
                    _ => return Err("Expected keyword return".into()),
                };

                let expression = Parser::parse_expression(lexer, Precedence::LOWEST)?;

                match lexer.next() {
                    Some(token) if token.kind == TokenKind::SEMICOLON => (),
                    _ => return Err("Expected semicolon".into()),
                }

                Ok(Statement::Return { token, expression })
            }
            _ => {
                let token = match lexer.peek() {
                    Some(token) => token,
                    _ => return Err("Expected a token".into()),
                }
                .clone();

                let expression = Parser::parse_expression(lexer, Precedence::LOWEST)?;

                match lexer.next() {
                    Some(token) if token.kind == TokenKind::SEMICOLON => (),
                    _ => return Err("Expected semicolon".into()),
                }

                Ok(Statement::Expression { token, expression })
            }
        }
    }

    pub fn parse(&mut self) -> Result<Program, Box<dyn Error>> {
        let mut program = Program::new();

        while let Some(token) = self.lexer.peek() {
            if token.kind == TokenKind::EOF {
                break;
            }

            if token.kind == TokenKind::ILLEGAL {
                return Err(format!(
                    "\"{}\" at {}:{} is an illegal token",
                    token.literal, token.span.line, token.span.column
                )
                .into());
            }

            program
                .statements
                .push(Parser::parse_statement(&mut self.lexer)?);
        }

        Ok(program)
    }
}

#[cfg(test)]
mod tests {
    use crate::{Expression, Lexer, Statement, TokenKind};

    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "let x = 5; let y = 10;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 2);

        let expected = [("x", 5), ("y", 10)];

        for (i, (expected_identifier, expected_value)) in expected.iter().enumerate() {
            let statement = &program.statements[i];

            match statement {
                Statement::Let {
                    identifier,
                    expression,
                    ..
                } => {
                    assert_eq!(**identifier, **expected_identifier);

                    match expression {
                        Expression::INT { value, .. } => {
                            assert_eq!(value, expected_value);
                        }
                        _ => unreachable!(),
                    }
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
    fn test_integer_literal_expressions() {
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
                        assert_eq!(operator.kind, *expected_operator);

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

    #[test]
    fn test_parsing_infix_expressions() {
        let input = "5 + 5; 5 - 5; 5 * 5; 5 / 5; 5 > 5; 5 < 5; 5 == 5; 5 != 5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 8);

        let expected = [
            (TokenKind::INT, 5, TokenKind::PLUS, 5),
            (TokenKind::INT, 5, TokenKind::MINUS, 5),
            (TokenKind::INT, 5, TokenKind::ASTERISK, 5),
            (TokenKind::INT, 5, TokenKind::SLASH, 5),
            (TokenKind::INT, 5, TokenKind::GT, 5),
            (TokenKind::INT, 5, TokenKind::LT, 5),
            (TokenKind::INT, 5, TokenKind::EQ, 5),
            (TokenKind::INT, 5, TokenKind::NEQ, 5),
        ];

        for (i, (_, left_value, operator_kind, right_value)) in expected.iter().enumerate() {
            let statement = &program.statements[i];

            match statement {
                Statement::Expression { expression, .. } => match expression {
                    Expression::INFIX {
                        left,
                        operator,
                        right,
                        ..
                    } => {
                        match left.as_ref() {
                            Expression::INT { value, .. } => {
                                assert_eq!(value, left_value);
                            }
                            _ => unreachable!(),
                        }

                        assert_eq!(operator.kind, *operator_kind);

                        match right.as_ref() {
                            Expression::INT { value, .. } => {
                                assert_eq!(value, right_value);
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
