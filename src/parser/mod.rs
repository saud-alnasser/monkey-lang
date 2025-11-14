use chumsky::{Parser, prelude::*};

use crate::ast::{
    ArrayExpression, BlockExpression, BooleanExpression, CallExpression, Expression,
    FunctionExpression, IdentifierExpression, IfExpression, IndexExpression, InfixExpression,
    IntExpression, ObjectExpression, PrefixExpression, Statement, StringExpression, Token,
};

pub fn parse(tokens: &[Token]) -> ParseResult<Vec<Statement>, Rich<'_, Token>> {
    statement()
        .repeated()
        .collect::<Vec<Statement>>()
        .then_ignore(end())
        .parse(tokens)
}

fn statement<'src>() -> impl Parser<'src, &'src [Token], Statement, extra::Err<Rich<'src, Token>>> {
    recursive(|stmt| {
        let expression = recursive(|expr| {
            let int = select! {
                Token::INT(value) => IntExpression { value },
            }
            .labelled("integer");

            let boolean = select! {
                Token::TRUE => BooleanExpression { value: true },
                Token::FALSE => BooleanExpression { value: false },
            }
            .labelled("boolean");

            let string = select! {
                Token::STRING(value) => StringExpression { value },
            }
            .labelled("string");

            let identifier = select! {
                Token::IDENTIFIER(value) => IdentifierExpression { value },
            }
            .labelled("identifier");

            let block = stmt
                .repeated()
                .collect::<Vec<Statement>>()
                .delimited_by(just(Token::LBRACE), just(Token::RBRACE))
                .map(|statements| BlockExpression { statements });

            let object = expr
                .clone()
                .then_ignore(just(Token::COLON))
                .then(expr.clone())
                .separated_by(just(Token::COMMA))
                .collect::<Vec<(Expression, Expression)>>()
                .delimited_by(just(Token::LBRACE), just(Token::RBRACE))
                .map(|pairs| ObjectExpression { pairs });

            let function = just(Token::FUNCTION)
                .ignore_then(
                    identifier
                        .separated_by(just(Token::COMMA))
                        .collect::<Vec<IdentifierExpression>>()
                        .delimited_by(just(Token::LPAREN), just(Token::RPAREN)),
                )
                .then(block.clone())
                .map(|(parameters, body)| FunctionExpression {
                    parameters,
                    body: Box::new(body),
                });

            let array = expr
                .clone()
                .separated_by(just(Token::COMMA))
                .collect::<Vec<Expression>>()
                .delimited_by(just(Token::LBRACKET), just(Token::RBRACKET))
                .map(|elements| ArrayExpression { elements });

            let r#if = just(Token::IF)
                .ignore_then(
                    expr.clone()
                        .delimited_by(just(Token::LPAREN), just(Token::RPAREN)),
                )
                .then(block.clone())
                .then(just(Token::ELSE).ignore_then(block.clone()).or_not())
                .map(|((condition, consequence), alternative)| IfExpression {
                    condition: Box::new(condition),
                    consequence: Box::new(consequence),
                    alternative: alternative.map(|alternative| Box::new(alternative)),
                });

            let atom = block
                .map(|block| Expression::Block(block))
                .or(function.map(|function| Expression::Function(function)))
                .or(object.map(|object| Expression::Object(object)))
                .or(array.map(|array| Expression::Array(array)))
                .or(r#if.map(|if_expression| Expression::If(if_expression)))
                .or(int.map(|int| Expression::Int(IntExpression { value: int.value })))
                .or(string.map(|string| Expression::String(string)))
                .or(boolean.map(|boolean| Expression::Boolean(boolean)))
                .or(identifier.map(|identifier| Expression::Identifier(identifier)))
                .or(expr
                    .clone()
                    .delimited_by(just(Token::LPAREN), just(Token::RPAREN)));

            // NOTE: highest precedence to lowest precedence

            let unary = just(Token::MINUS)
                .or(just(Token::BANG))
                .repeated()
                .collect::<Vec<Token>>()
                .then(atom)
                .map(|(ops, expr)| {
                    ops.into_iter().rev().fold(expr, |acc, op| {
                        Expression::Prefix(PrefixExpression {
                            operator: op.into(),
                            right: Box::new(acc),
                        })
                    })
                });

            let call = unary.foldl(
                expr.clone()
                    .separated_by(just(Token::COMMA))
                    .collect::<Vec<Expression>>()
                    .delimited_by(just(Token::LPAREN), just(Token::RPAREN))
                    .repeated(),
                |callable, arguments| {
                    Expression::Call(CallExpression {
                        callable: Box::new(callable),
                        arguments,
                    })
                },
            );

            let index = call.clone().foldl(
                expr.delimited_by(just(Token::LBRACKET), just(Token::RBRACKET))
                    .repeated(),
                |left, index| {
                    Expression::Index(IndexExpression {
                        left: Box::new(left),
                        index: Box::new(index),
                    })
                },
            );

            let product = index.clone().foldl(
                just(Token::ASTERISK)
                    .or(just(Token::SLASH))
                    .then(index)
                    .repeated(),
                |lhs, (op, rhs)| {
                    Expression::Infix(InfixExpression {
                        left: Box::new(lhs),
                        operator: op.into(),
                        right: Box::new(rhs),
                    })
                },
            );

            let sum = product.clone().foldl(
                just(Token::PLUS)
                    .or(just(Token::MINUS))
                    .then(product)
                    .repeated(),
                |lhs, (op, rhs)| {
                    Expression::Infix(InfixExpression {
                        left: Box::new(lhs),
                        operator: op.into(),
                        right: Box::new(rhs),
                    })
                },
            );

            let comparison = sum.clone().foldl(
                just(Token::LT)
                    .or(just(Token::GT))
                    .or(just(Token::LTE))
                    .or(just(Token::GTE))
                    .then(sum)
                    .repeated(),
                |lhs, (op, rhs)| {
                    Expression::Infix(InfixExpression {
                        left: Box::new(lhs),
                        operator: op.into(),
                        right: Box::new(rhs),
                    })
                },
            );

            let equality = comparison.clone().foldl(
                just(Token::EQ)
                    .or(just(Token::NEQ))
                    .then(comparison)
                    .repeated(),
                |lhs, (op, rhs)| {
                    Expression::Infix(InfixExpression {
                        left: Box::new(lhs),
                        operator: op.into(),
                        right: Box::new(rhs),
                    })
                },
            );

            equality
        });

        let r#let = just(Token::LET)
            .ignore_then(
                select! {
                    Token::IDENTIFIER(value) => IdentifierExpression { value },
                }
                .labelled("identifier"),
            )
            .then_ignore(just(Token::ASSIGN))
            .then(expression.clone())
            .map(|(identifier, expression)| Statement::Let {
                identifier,
                expression,
            });

        let r#return = just(Token::RETURN)
            .ignore_then(expression.clone())
            .map(|expression| Statement::Return(expression));

        r#let
            .or(r#return)
            .or(expression.map(|expression| Statement::Expression(expression)))
            .then_ignore(just(Token::SEMICOLON))
    })
}

#[cfg(test)]
mod tests {
    use internment::Intern;
    use std::vec;

    use super::*;
    use crate::ast::{BinaryOperator, UnaryOperator};

    #[test]
    fn test_let() {
        let input = vec![
            Token::LET,
            Token::IDENTIFIER(Intern::new("x".into())),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
        ];

        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Let {
                identifier,
                expression,
            } => {
                assert_eq!(
                    identifier,
                    IdentifierExpression {
                        value: Intern::new("x".into())
                    }
                );

                assert_eq!(expression, Expression::Int(IntExpression { value: 5 }));
            }
            _ => panic!("expected a let statement, got something else"),
        }
    }

    #[test]
    fn test_return() {
        let input = vec![Token::RETURN, Token::INT(5), Token::SEMICOLON];

        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Return(expression) => match expression {
                Expression::Int(IntExpression { value }) => assert_eq!(value, 5),
                _ => panic!("expected an integer, got something else"),
            },
            _ => panic!("expected a return statement, got something else"),
        }
    }

    #[test]
    fn test_index() {
        let input = vec![
            Token::IDENTIFIER(Intern::new("array".into())),
            Token::LBRACKET,
            Token::INT(1),
            Token::RBRACKET,
            Token::SEMICOLON,
        ];

        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Expression(Expression::Index(expression)) => {
                assert_eq!(
                    *expression.left.as_ref(),
                    Expression::Identifier(IdentifierExpression {
                        value: Intern::new("array".into())
                    })
                );

                assert_eq!(
                    expression.index,
                    Box::new(Expression::Int(IntExpression { value: 1 }))
                );
            }
            _ => panic!("expected an index expression, got something else"),
        }
    }

    #[test]
    fn test_call() {
        let input = vec![
            Token::IDENTIFIER(Intern::new("add".into())),
            Token::LPAREN,
            Token::INT(1),
            Token::COMMA,
            Token::INT(2),
            Token::RPAREN,
            Token::SEMICOLON,
        ];

        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Expression(Expression::Call(expression)) => {
                assert_eq!(
                    *expression.callable.as_ref(),
                    Expression::Identifier(IdentifierExpression {
                        value: Intern::new("add".into())
                    })
                );

                assert_eq!(
                    expression.arguments,
                    vec![
                        Expression::Int(IntExpression { value: 1 }),
                        Expression::Int(IntExpression { value: 2 })
                    ]
                );
            }
            _ => panic!("expected a call expression, got something else"),
        }
    }

    #[test]
    fn test_block() {
        let input = vec![
            Token::LBRACE,
            Token::RETURN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
        ];

        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Expression(Expression::Block(expression)) => {
                assert_eq!(
                    expression.statements,
                    vec![Statement::Return(Expression::Int(IntExpression {
                        value: 5
                    }))]
                );
            }
            _ => panic!("expected a block expression, got something else"),
        }
    }

    #[test]
    fn test_object() {
        let input = vec![
            Token::LBRACE,
            Token::STRING(Intern::new("one".into())),
            Token::COLON,
            Token::INT(1),
            Token::COMMA,
            Token::STRING(Intern::new("two".into())),
            Token::COLON,
            Token::INT(2),
            Token::COMMA,
            Token::STRING(Intern::new("three".into())),
            Token::COLON,
            Token::INT(3),
            Token::RBRACE,
            Token::SEMICOLON,
        ];

        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Expression(Expression::Object(expression)) => {
                assert_eq!(
                    expression.pairs,
                    vec![
                        (
                            Expression::String(StringExpression {
                                value: Intern::new("one".into())
                            }),
                            Expression::Int(IntExpression { value: 1 })
                        ),
                        (
                            Expression::String(StringExpression {
                                value: Intern::new("two".into())
                            }),
                            Expression::Int(IntExpression { value: 2 })
                        ),
                        (
                            Expression::String(StringExpression {
                                value: Intern::new("three".into())
                            }),
                            Expression::Int(IntExpression { value: 3 })
                        )
                    ]
                );
            }
            _ => panic!("expected an object expression, got something else"),
        }
    }

    #[test]
    fn test_function() {
        let input = vec![
            Token::FUNCTION,
            Token::LPAREN,
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
        ];

        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Expression(Expression::Function(expression)) => {
                assert_eq!(expression.parameters, vec![]);
                assert_eq!(
                    expression.body.statements,
                    vec![Statement::Return(Expression::Int(IntExpression {
                        value: 5
                    }))]
                )
            }
            _ => panic!("expected a function expression, got something else"),
        }
    }

    #[test]
    fn test_array() {
        let input = vec![
            Token::LBRACKET,
            Token::LBRACKET,
            Token::INT(1),
            Token::RBRACKET,
            Token::COMMA,
            Token::INT(2),
            Token::COMMA,
            Token::INT(3),
            Token::RBRACKET,
            Token::SEMICOLON,
        ];

        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Expression(Expression::Array(expression)) => {
                assert_eq!(
                    expression.elements,
                    vec![
                        Expression::Array(ArrayExpression {
                            elements: vec![Expression::Int(IntExpression { value: 1 }),]
                        }),
                        Expression::Int(IntExpression { value: 2 }),
                        Expression::Int(IntExpression { value: 3 })
                    ]
                )
            }
            _ => panic!("expected an array expression, got something else"),
        }
    }

    #[test]
    fn test_if() {
        let input = vec![
            Token::IF,
            Token::LPAREN,
            Token::TRUE,
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
        ];

        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Expression(Expression::If(expression)) => {
                assert_eq!(
                    *expression.condition.as_ref(),
                    Expression::Boolean(BooleanExpression { value: true })
                );

                assert_eq!(
                    expression.consequence.statements,
                    vec![Statement::Return(Expression::Int(IntExpression {
                        value: 5
                    }))]
                );

                assert_eq!(
                    expression.alternative.unwrap().statements,
                    vec![Statement::Return(Expression::Int(IntExpression {
                        value: 10
                    }))]
                );
            }
            _ => panic!("expected an if expression, got something else"),
        }
    }

    #[test]
    fn test_boolean() {
        let input = vec![Token::TRUE, Token::SEMICOLON];
        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Expression(Expression::Boolean(expression)) => {
                assert_eq!(expression.value, true)
            }
            _ => panic!("expected a boolean expression, got something else"),
        }

        let input = vec![Token::FALSE, Token::SEMICOLON];
        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Expression(Expression::Boolean(expression)) => {
                assert_eq!(expression.value, false)
            }
            _ => panic!("expected a boolean expression, got something else"),
        }
    }

    #[test]
    fn test_int() {
        let input = vec![Token::INT(5), Token::SEMICOLON];
        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Expression(Expression::Int(expression)) => {
                assert_eq!(expression.value, 5)
            }
            _ => panic!("expected an integer expression, got something else"),
        }
    }

    #[test]
    fn test_string() {
        let input = vec![
            Token::STRING(Intern::new("hello, world!".into())),
            Token::SEMICOLON,
        ];
        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Expression(Expression::String(expression)) => {
                assert_eq!(expression.value.as_str(), "hello, world!")
            }
            _ => panic!("expected a string expression, got something else"),
        }
    }

    #[test]
    fn test_identifier() {
        let input = vec![
            Token::IDENTIFIER(Intern::new("ident".into())),
            Token::SEMICOLON,
        ];
        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Expression(Expression::Identifier(expression)) => {
                assert_eq!(expression.value.as_str(), "ident")
            }
            _ => panic!("expected an identifier expression, got something else"),
        }
    }

    #[test]
    fn test_prefix_expression() {
        let input = vec![Token::MINUS, Token::INT(5), Token::SEMICOLON];

        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Expression(Expression::Prefix(expression)) => {
                assert_eq!(
                    expression,
                    PrefixExpression {
                        operator: UnaryOperator::Negate,
                        right: Box::new(Expression::Int(IntExpression { value: 5 }))
                    }
                )
            }
            _ => panic!("expected an expression, got something else"),
        }
    }

    #[test]
    fn test_infix_expression() {
        let input = vec![Token::INT(5), Token::PLUS, Token::INT(5), Token::SEMICOLON];

        let stmt = statement().parse(&input).unwrap();

        match stmt {
            Statement::Expression(Expression::Infix(expression)) => {
                assert_eq!(
                    expression,
                    InfixExpression {
                        left: Box::new(Expression::Int(IntExpression { value: 5 })),
                        operator: BinaryOperator::Add,
                        right: Box::new(Expression::Int(IntExpression { value: 5 }))
                    }
                );
            }
            _ => panic!("expected an expression, got something else"),
        }
    }
}
