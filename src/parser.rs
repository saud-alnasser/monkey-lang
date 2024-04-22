use std::error::Error;

use crate::{
    BlockStatement, BooleanExpression, Expression, ExpressionStatement, FunctionExpression,
    IdentExpression, IfExpression, InfixExpression, IntExpression, LetStatement, Lexer, Precedence,
    PrefixExpression, Program, ReturnStatement, Statement, TokenKind,
};

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
        let mut left = match lexer.next() {
            Some(token) if token.kind == TokenKind::LPAREN => {
                let expression = Parser::parse_expression(lexer, Precedence::LOWEST)?;

                match lexer.next() {
                    Some(token) if token.kind == TokenKind::RPAREN => (),
                    _ => return Err("Expected closing parenthesis".into()),
                }

                expression
            }
            Some(token) if token.kind == TokenKind::IDENT => {
                let value = token.literal.clone();

                Expression::IDENT(IdentExpression { token, value })
            }
            Some(token) if token.kind == TokenKind::INT => {
                let value = token.literal.parse::<i64>()?;

                Expression::INT(IntExpression { token, value })
            }
            Some(token) if token.kind == TokenKind::TRUE || token.kind == TokenKind::FALSE => {
                let value = token.kind == TokenKind::TRUE;

                Expression::BOOLEAN(BooleanExpression { token, value })
            }
            Some(token) if token.kind == TokenKind::IF => {
                let condition = Box::new(Parser::parse_expression(lexer, Precedence::LOWEST)?);

                let consequence = match Parser::parse_statement(lexer)? {
                    Statement::Block(block) => block,
                    _ => return Err("Expected block statement".into()),
                };

                let alternative = match lexer.peek() {
                    Some(token) if token.kind == TokenKind::ELSE => {
                        lexer.next().unwrap();

                        let block = match Parser::parse_statement(lexer)? {
                            Statement::Block(block) => block,
                            _ => return Err("Expected block statement".into()),
                        };

                        Some(block)
                    }
                    _ => None,
                };

                Expression::IF(IfExpression {
                    token,
                    condition,
                    consequence,
                    alternative,
                })
            }
            Some(token) if token.kind == TokenKind::FUNCTION => {
                let parameters = {
                    match lexer.next() {
                        Some(token) if token.kind == TokenKind::LPAREN => (),
                        _ => return Err("Expected opening parenthesis".into()),
                    }

                    let mut parameters = Vec::new();

                    while let Some(token) = lexer.peek() {
                        if token.kind == TokenKind::RPAREN {
                            break;
                        }

                        match lexer.next() {
                            Some(token) if token.kind == TokenKind::IDENT => {
                                let value = token.literal.clone();
                                parameters.push(IdentExpression { token, value });
                            }
                            _ => return Err("Expected identifier".into()),
                        }

                        match lexer.peek() {
                            Some(token) if token.kind == TokenKind::COMMA => {
                                lexer.next().unwrap();
                            }
                            _ => continue,
                        }
                    }

                    match lexer.next() {
                        Some(token) if token.kind == TokenKind::RPAREN => (),
                        _ => return Err("Expected closing parenthesis".into()),
                    }

                    parameters
                };

                let body = match Parser::parse_statement(lexer)? {
                    Statement::Block(block) => block,
                    _ => return Err("Expected block statement".into()),
                };

                Expression::FUNCTION(FunctionExpression {
                    token,
                    parameters,
                    body,
                })
            }
            Some(token) if token.kind == TokenKind::MINUS || token.kind == TokenKind::BANG => {
                let right = Box::new(Parser::parse_expression(lexer, Precedence::PREFIX)?);

                Expression::PREFIX(PrefixExpression {
                    operator: token,
                    right,
                })
            }
            _ => return Err("Expected expression".into()),
        };

        while let Some(next) = lexer.peek() {
            if precedence >= Precedence::from(&next.kind) {
                break;
            }

            let operator = lexer.next().unwrap();
            let right = Parser::parse_expression(lexer, Precedence::from(&operator.kind))?;

            left = Expression::INFIX(InfixExpression {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            });
        }

        Ok(left)
    }

    fn parse_statement(lexer: &mut Lexer) -> Result<Statement, Box<dyn Error>> {
        match lexer.peek() {
            Some(token) if token.kind == TokenKind::LBRACE => {
                let token = match lexer.next() {
                    Some(token) if token.kind == TokenKind::LBRACE => token,
                    _ => return Err("Expected opening brace".into()),
                };

                let mut statements = Vec::new();

                while let Some(token) = lexer.peek() {
                    if token.kind == TokenKind::RBRACE {
                        break;
                    }

                    statements.push(Parser::parse_statement(lexer)?);
                }

                match lexer.next() {
                    Some(token) if token.kind == TokenKind::RBRACE => (),
                    _ => return Err("Expected closing brace".into()),
                }

                Ok(Statement::Block(BlockStatement { token, statements }))
            }
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

                Ok(Statement::Let(LetStatement {
                    token,
                    identifier,
                    expression,
                }))
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

                Ok(Statement::Return(ReturnStatement { token, expression }))
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

                Ok(Statement::Expression(ExpressionStatement {
                    token,
                    expression,
                }))
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
    use crate::{
        BooleanExpression, Expression, IdentExpression, IfExpression, InfixExpression,
        IntExpression, LetStatement, Lexer, PrefixExpression, ReturnStatement, Statement,
        TokenKind,
    };

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
                Statement::Let(LetStatement {
                    identifier,
                    expression,
                    ..
                }) => {
                    assert_eq!(**identifier, **expected_identifier);

                    match expression {
                        Expression::INT(IntExpression { value, .. }) => {
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
                Statement::Return(ReturnStatement { expression, .. }) => match expression {
                    Expression::INT(IntExpression { value, .. }) => {
                        assert_eq!(value, expected);
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_identifier_literal_expressions() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::IDENT(IdentExpression { value, .. }) => {
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
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::INT(IntExpression { value, .. }) => {
                    assert_eq!(*value, 5);
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_boolean_literal_expressions() {
        let input = "true; false;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 2);

        let expected = [true, false];

        for (i, expected) in expected.iter().enumerate() {
            let statement = &program.statements[i];

            match statement {
                Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                    Expression::BOOLEAN(BooleanExpression { value, .. }) => {
                        assert_eq!(*value, *expected);
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
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
                Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                    Expression::PREFIX(PrefixExpression {
                        operator, right, ..
                    }) => {
                        assert_eq!(operator.kind, *expected_operator);

                        match right.as_ref() {
                            Expression::INT(IntExpression { value, .. }) => {
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
                Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                    Expression::INFIX(InfixExpression {
                        left,
                        operator,
                        right,
                        ..
                    }) => {
                        match left.as_ref() {
                            Expression::INT(IntExpression { value, .. }) => {
                                assert_eq!(value, left_value);
                            }
                            _ => unreachable!(),
                        }

                        assert_eq!(operator.kind, *operator_kind);

                        match right.as_ref() {
                            Expression::INT(IntExpression { value, .. }) => {
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

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = [
            // arithmetic operators
            ("1 + 2;", "(1 + 2);"),
            ("1 - 2;", "(1 - 2);"),
            ("1 * 2;", "(1 * 2);"),
            ("1 / 2;", "(1 / 2);"),
            // comparison operators
            ("1 > 2;", "(1 > 2);"),
            ("1 < 2;", "(1 < 2);"),
            ("1 == 2;", "(1 == 2);"),
            ("1 != 2;", "(1 != 2);"),
            // logical operators
            ("true == true;", "(true == true);"),
            ("true != false;", "(true != false);"),
            // prefix operators
            ("-a;", "(-a);"),
            ("!true;", "(!true);"),
            // combined operators
            ("a + b + c;", "((a + b) + c);"),
            ("a + b - c;", "((a + b) - c);"),
            ("a * b * c;", "((a * b) * c);"),
            ("a * b / c;", "((a * b) / c);"),
            ("a + b / c;", "(a + (b / c));"),
            ("a + b * c + d / e - f;", "(((a + (b * c)) + (d / e)) - f);"),
            ("3 + 4; -5 * 5;", "(3 + 4);((-5) * 5);"),
            ("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4));"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5;",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            ),
            // grouping
            ("1 + (2 + 3) + 4;", "((1 + (2 + 3)) + 4);"),
            ("(5 + 5) * 2;", "((5 + 5) * 2);"),
            ("2 / (5 + 5);", "(2 / (5 + 5));"),
            ("-(5 + 5);", "(-(5 + 5));"),
            ("!(true == true);", "(!(true == true));"),
        ];

        for (input, expected) in tests.iter() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse().unwrap();

            assert_eq!(format!("{}", program), *expected);
        }
    }

    #[test]
    fn test_if_expressions() {
        let input = "if (x < y) { x; };";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::IF(IfExpression {
                    condition,
                    consequence,
                    alternative,
                    ..
                }) => {
                    match condition.as_ref() {
                        Expression::INFIX(InfixExpression { operator, .. }) => {
                            assert_eq!(operator.kind, TokenKind::LT);
                        }
                        _ => unreachable!(),
                    }

                    match &consequence.statements[0] {
                        Statement::Expression(ExpressionStatement { expression, .. }) => {
                            match expression {
                                Expression::IDENT(IdentExpression { value, .. }) => {
                                    assert_eq!(&**value, "x");
                                }
                                _ => unreachable!(),
                            }
                        }
                        _ => unreachable!(),
                    }

                    assert_eq!(*alternative, None);
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let input = "if (x < y) { x; } else { y; };";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::IF(IfExpression {
                    condition,
                    consequence,
                    alternative,
                    ..
                }) => {
                    match condition.as_ref() {
                        Expression::INFIX(InfixExpression { operator, .. }) => {
                            assert_eq!(operator.kind, TokenKind::LT);
                        }
                        _ => unreachable!(),
                    }

                    match &consequence.statements[0] {
                        Statement::Expression(ExpressionStatement { expression, .. }) => {
                            match expression {
                                Expression::IDENT(IdentExpression { value, .. }) => {
                                    assert_eq!(&**value, "x");
                                }
                                _ => unreachable!(),
                            }
                        }
                        _ => unreachable!(),
                    }

                    match alternative {
                        Some(alternative) => match &alternative.statements[0] {
                            Statement::Expression(ExpressionStatement { expression, .. }) => {
                                match expression {
                                    Expression::IDENT(IdentExpression { value, .. }) => {
                                        assert_eq!(&**value, "y");
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_function_expressions() {
        let test_parameters: [(&str, &[&str]); 4] = [
            ("fn() {};", &[]),
            ("fn(x) {};", &["x"]),
            ("fn(x, y) {};", &["x", "y"]),
            ("fn(x, y, z) {};", &["x", "y", "z"]),
        ];

        for (input, expected) in test_parameters.iter() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse().unwrap();

            assert_eq!(program.statements.len(), 1);

            let statement = &program.statements[0];

            match statement {
                Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                    Expression::FUNCTION(FunctionExpression { parameters, .. }) => {
                        assert_eq!(parameters.len(), expected.len());

                        for (i, expected) in expected.iter().enumerate() {
                            match &parameters[i] {
                                IdentExpression { value, .. } => {
                                    assert_eq!(&**value, *expected);
                                }
                            }
                        }
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }

        let test_body = "fn(x, y) { x + y; };";

        let lexer = Lexer::new(test_body);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::FUNCTION(FunctionExpression { body, .. }) => {
                    assert_eq!(body.statements.len(), 1);

                    match &body.statements[0] {
                        Statement::Expression(ExpressionStatement { expression, .. }) => {
                            match expression {
                                Expression::INFIX(InfixExpression { operator, .. }) => {
                                    assert_eq!(operator.kind, TokenKind::PLUS);
                                }
                                _ => unreachable!(),
                            }
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
