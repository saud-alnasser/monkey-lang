pub mod error;
mod expression;
mod rule;
mod statement;

use self::{
    error::{Error, Result},
    statement::StatementParser,
};
use crate::{Lexer, Program, TokenKind};

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser { lexer }
    }

    pub fn parse(&mut self) -> Result<Program> {
        let mut program = Program::new();

        while let Some(token) = self.lexer.peek(0) {
            if token.kind == TokenKind::EOF {
                break;
            }

            if token.kind == TokenKind::ILLEGAL {
                return Err(Error::IllegalToken(token.clone()));
            }

            program
                .statements
                .push(StatementParser::parse(&mut self.lexer)?);
        }

        Ok(program)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ArrayExpression, BooleanExpression, Expression, ExpressionStatement, FunctionExpression,
        IdentExpression, IfExpression, InfixExpression, IntExpression, LetStatement, Lexer,
        PrefixExpression, ReturnStatement, Statement, StringExpression, TokenKind,
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
    fn test_string_literal_expressions() {
        let input = "\"hello world\";";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::STRING(StringExpression { value, .. }) => {
                    assert_eq!(&**value, "hello world");
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
    fn test_array_literal_expressions() {
        let input = "[1, 2 * 2, 3 + 3];";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::ARRAY(ArrayExpression { elements, .. }) => {
                    assert_eq!(elements.len(), 3);

                    if let Expression::INT(IntExpression { value, .. }) = &elements[0] {
                        assert_eq!(*value, 1);
                    } else {
                        panic!("expected integer expression with value 1 in array[0]");
                    }

                    if let Expression::INFIX(InfixExpression { left, right, .. }) = &elements[1] {
                        if let Expression::INT(IntExpression { value, .. }) = &**left {
                            assert_eq!(*value, 2);
                        } else {
                            panic!("expected integer expression with value 2 in array[1]");
                        }

                        if let Expression::INT(IntExpression { value, .. }) = &**right {
                            assert_eq!(*value, 2);
                        } else {
                            panic!("expected integer expression with value 2 in array[1]");
                        }
                    } else {
                        panic!("expected infix expression with value 2 * 2 in array[1]");
                    }

                    if let Expression::INFIX(InfixExpression { left, right, .. }) = &elements[2] {
                        if let Expression::INT(IntExpression { value, .. }) = &**left {
                            assert_eq!(*value, 3);
                        } else {
                            panic!("expected integer expression with value 3 in array[2]");
                        }

                        if let Expression::INT(IntExpression { value, .. }) = &**right {
                            assert_eq!(*value, 3);
                        } else {
                            panic!("expected integer expression with value 3 in array[2]");
                        }
                    } else {
                        panic!("expected infix expression with value 3 + 3 in array[2]");
                    }
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
            // function calls
            ("a + add(b * c) + d;", "((a + add((b * c))) + d);"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
            ),
            (
                "add(a + b + c * d / f + g);",
                "add((((a + b) + ((c * d) / f)) + g));",
            ),
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

    #[test]
    fn test_call_expressions() {
        let empty_call_input = "func();";

        let lexer = Lexer::new(empty_call_input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::CALL(call) => {
                    match call.function.as_ref() {
                        Expression::IDENT(IdentExpression { value, .. }) => {
                            assert_eq!(&**value, "func");
                        }
                        _ => unreachable!(),
                    }

                    assert_eq!(call.arguments.len(), 0);
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }

        let call_input = "add(1, 2 * 3, 4 + 5);";

        let lexer = Lexer::new(call_input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::CALL(call) => {
                    match call.function.as_ref() {
                        Expression::IDENT(IdentExpression { value, .. }) => {
                            assert_eq!(&**value, "add");
                        }
                        _ => unreachable!(),
                    }

                    assert_eq!(call.arguments.len(), 3);

                    match &call.arguments[0] {
                        Expression::INT(IntExpression { value, .. }) => {
                            assert_eq!(*value, 1);
                        }
                        _ => unreachable!(),
                    }

                    match &call.arguments[1] {
                        Expression::INFIX(InfixExpression { operator, .. }) => {
                            assert_eq!(operator.kind, TokenKind::ASTERISK);
                        }
                        _ => unreachable!(),
                    }

                    match &call.arguments[2] {
                        Expression::INFIX(InfixExpression { operator, .. }) => {
                            assert_eq!(operator.kind, TokenKind::PLUS);
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }

        let literal_call_input = "fn(x, y){ x + y; }(2, 3);";

        let lexer = Lexer::new(literal_call_input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::CALL(call) => {
                    match call.function.as_ref() {
                        Expression::FUNCTION(FunctionExpression { parameters, .. }) => {
                            assert_eq!(parameters.len(), 2);
                        }
                        _ => unreachable!(),
                    }

                    assert_eq!(call.arguments.len(), 2);

                    let expected = [(TokenKind::INT, 2), (TokenKind::INT, 3)];

                    for (i, (expected_kind, expected_value)) in expected.iter().enumerate() {
                        match &call.arguments[i] {
                            Expression::INT(IntExpression { token, value }) => {
                                assert_eq!(token.kind, *expected_kind);
                                assert_eq!(*value, *expected_value);
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_index_expressions() {
        let input = "array[1 + 1];";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::INDEX(index) => {
                    match index.left.as_ref() {
                        Expression::IDENT(IdentExpression { value, .. }) => {
                            assert_eq!(&**value, "array");
                        }
                        _ => unreachable!(),
                    }

                    match index.index.as_ref() {
                        Expression::INFIX(InfixExpression { operator, .. }) => {
                            assert_eq!(operator.kind, TokenKind::PLUS);
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
