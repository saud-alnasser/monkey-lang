use crate::{
    parser::{
        error::{Error, Result},
        rule::Rule,
        statement::StatementParser,
    },
    ArrayExpression, BooleanExpression, CallExpression, Expression, FunctionExpression,
    IdentExpression, IfExpression, IndexExpression, InfixExpression, IntExpression, Lexer,
    PrefixExpression, Statement, StringExpression, TokenKind,
};

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone)]
#[allow(dead_code)]
pub enum Precedence {
    LOWEST = 1,
    EQUIVALENCE = 2, // equivalence: == or !=
    COMPARISON = 3,  // comparison: > or < or >= or <=
    SUM = 4,         // sum: + or -
    PRODUCT = 5,     // product: * or /
    PREFIX = 6,      // prefix: -x or !x
    CALL = 7,        // call: func(x)
}

impl From<&TokenKind> for Precedence {
    fn from(kind: &TokenKind) -> Self {
        match kind {
            TokenKind::EQ | TokenKind::NEQ => Precedence::EQUIVALENCE,
            TokenKind::LT | TokenKind::GT | TokenKind::LTE | TokenKind::GTE => {
                Precedence::COMPARISON
            }
            TokenKind::PLUS | TokenKind::MINUS => Precedence::SUM,
            TokenKind::ASTERISK | TokenKind::SLASH => Precedence::PRODUCT,
            _ => Precedence::LOWEST,
        }
    }
}

pub struct ExpressionParser;

impl ExpressionParser {
    fn parse_left(lexer: &mut Lexer) -> Result<Expression> {
        for rule in &EXPRESSION_RULES {
            if let Some(expression) = rule.parse(lexer, None)? {
                return Ok(expression);
            }
        }

        match lexer.next() {
            Some(token) => Err(Error::UnexpectedToken(token)),
            None => Err(Error::MissingToken),
        }
    }

    pub fn parse(lexer: &mut Lexer, precedence: &Precedence) -> Result<Expression> {
        let mut left = ExpressionParser::parse_left(lexer)?;

        while let Some(token) = lexer.peek() {
            if precedence >= &Precedence::from(&token.kind) {
                break;
            }

            let operator = match lexer.next() {
                Some(token) => token,
                None => return Err(Error::MissingToken),
            };

            let right = ExpressionParser::parse(lexer, &Precedence::from(&operator.kind))?;

            left = Expression::INFIX(InfixExpression {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            });
        }

        Ok(left)
    }
}

// dependent rules should be placed on the rules they depend on not in this slice.
const EXPRESSION_RULES: [Rule<Expression>; 9] = [
    GROUPING_EXPRESSION_RULE,
    IDENTIFIER_EXPRESSION_RULE,
    INTEGER_EXPRESSION_RULE,
    STRING_EXPRESSION_RULE,
    BOOLEAN_EXPRESSION_RULE,
    ARRAY_EXPRESSION_RULE,
    PREFIX_EXPRESSION_RULE,
    IF_EXPRESSION_RULE,
    FUNCTION_EXPRESSION_RULE,
];

const GROUPING_EXPRESSION_RULE: Rule<Expression> = Rule {
    consume: |lexer, _| {
        if let Some(token) = lexer.peek() {
            if token.kind != TokenKind::LPAREN {
                return Ok(None);
            }

            match lexer.next() {
                Some(token) if token.kind == TokenKind::LPAREN => token,
                option => return Err(Error::MissingOpeningParenthesis(option)),
            };

            let expression = ExpressionParser::parse(lexer, &Precedence::LOWEST)?;

            match lexer.next() {
                Some(token) if token.kind == TokenKind::RPAREN => token,
                option => return Err(Error::MissingClosingParenthesis(option)),
            };

            return Ok(Some(expression));
        }

        Ok(None)
    },
    dependents: None,
};

const IDENTIFIER_EXPRESSION_RULE: Rule<Expression> = Rule {
    consume: |lexer, _| {
        if let Some(token) = lexer.peek() {
            if token.kind != TokenKind::IDENT {
                return Ok(None);
            }

            let token = lexer.next().unwrap();
            let value = token.literal.clone();

            return Ok(Some(Expression::IDENT(IdentExpression { token, value })));
        }

        Ok(None)
    },
    dependents: Some(&[CALL_EXPRESSION_RULE, INDEX_EXPRESSION_RULE]),
};

const INTEGER_EXPRESSION_RULE: Rule<Expression> = Rule {
    consume: |lexer, _| {
        if let Some(token) = lexer.peek() {
            if token.kind != TokenKind::INT {
                return Ok(None);
            }

            let token = lexer.next().unwrap();

            let value = match token.literal.parse::<i64>() {
                Ok(value) => value,
                Err(_) => return Err(Error::IllegalToken(token)),
            };

            return Ok(Some(Expression::INT(IntExpression { token, value })));
        }

        Ok(None)
    },
    dependents: None,
};

const STRING_EXPRESSION_RULE: Rule<Expression> = Rule {
    consume: |lexer, _| {
        if let Some(token) = lexer.peek() {
            if token.kind != TokenKind::STRING {
                return Ok(None);
            }

            let token = lexer.next().unwrap();
            let value = token.literal[1..token.literal.len() - 1].into();

            return Ok(Some(Expression::STRING(StringExpression { token, value })));
        }

        Ok(None)
    },
    dependents: None,
};

const BOOLEAN_EXPRESSION_RULE: Rule<Expression> = Rule {
    consume: |lexer, _| {
        if let Some(token) = lexer.peek() {
            if token.kind != TokenKind::TRUE && token.kind != TokenKind::FALSE {
                return Ok(None);
            }

            let token = lexer.next().unwrap();
            let value = token.kind == TokenKind::TRUE;

            return Ok(Some(Expression::BOOLEAN(BooleanExpression {
                token,
                value,
            })));
        }

        Ok(None)
    },
    dependents: None,
};

const ARRAY_EXPRESSION_RULE: Rule<Expression> = Rule {
    consume: |lexer, _| {
        if let Some(token) = lexer.peek() {
            if token.kind != TokenKind::LBRACKET {
                return Ok(None);
            }

            let token = match lexer.next() {
                Some(token) if token.kind == TokenKind::LBRACKET => token,
                option => return Err(Error::MissingOpeningBracket(option)),
            };

            let mut elements = Vec::new();

            while let Some(token) = lexer.peek() {
                if token.kind == TokenKind::RBRACKET {
                    break;
                }

                let expression = ExpressionParser::parse(lexer, &Precedence::LOWEST)?;

                elements.push(expression);

                if let Some(token) = lexer.peek() {
                    if token.kind == TokenKind::COMMA {
                        lexer.next();
                    }
                }
            }

            match lexer.next() {
                Some(token) if token.kind == TokenKind::RBRACKET => token,
                option => return Err(Error::MissingClosingBracket(option)),
            };

            return Ok(Some(Expression::ARRAY(ArrayExpression { token, elements })));
        }

        Ok(None)
    },
    dependents: Some(&[INDEX_EXPRESSION_RULE]),
};

const PREFIX_EXPRESSION_RULE: Rule<Expression> = Rule {
    consume: |lexer, _| {
        if let Some(token) = lexer.peek() {
            if token.kind != TokenKind::MINUS && token.kind != TokenKind::BANG {
                return Ok(None);
            }

            let operator = lexer.next().unwrap();

            let right = Box::new(ExpressionParser::parse(lexer, &Precedence::PREFIX)?);

            return Ok(Some(Expression::PREFIX(PrefixExpression {
                operator,
                right,
            })));
        }

        Ok(None)
    },
    dependents: None,
};

const IF_EXPRESSION_RULE: Rule<Expression> = Rule {
    consume: |lexer, _| {
        if let Some(token) = lexer.peek() {
            if token.kind != TokenKind::IF {
                return Ok(None);
            }

            let token = lexer.next().unwrap();

            let condition = Box::new(ExpressionParser::parse(lexer, &Precedence::LOWEST)?);

            let consequence = match StatementParser::parse(lexer)? {
                Statement::Block(expression) => expression,
                Statement::Let(expression) => {
                    return Err(Error::MissingBlockStatement(Some(expression.token)))
                }
                Statement::Return(expression) => {
                    return Err(Error::MissingBlockStatement(Some(expression.token)))
                }
                Statement::Expression(expression) => {
                    return Err(Error::MissingBlockStatement(Some(expression.token)))
                }
            };

            let alternative = match lexer.peek() {
                Some(token) if token.kind == TokenKind::ELSE => {
                    lexer.next().unwrap();

                    match StatementParser::parse(lexer)? {
                        Statement::Block(expression) => Some(expression),
                        Statement::Let(expression) => {
                            return Err(Error::MissingBlockStatement(Some(expression.token)))
                        }
                        Statement::Return(expression) => {
                            return Err(Error::MissingBlockStatement(Some(expression.token)))
                        }
                        Statement::Expression(expression) => {
                            return Err(Error::MissingBlockStatement(Some(expression.token)))
                        }
                    }
                }
                _ => None,
            };

            return Ok(Some(Expression::IF(IfExpression {
                token,
                condition,
                consequence,
                alternative,
            })));
        }

        Ok(None)
    },
    dependents: None,
};

const FUNCTION_EXPRESSION_RULE: Rule<Expression> = Rule {
    consume: |lexer, _| {
        if let Some(token) = lexer.peek() {
            if token.kind != TokenKind::FUNCTION {
                return Ok(None);
            }

            let token = lexer.next().unwrap();

            let parameters = {
                match lexer.next() {
                    Some(token) if token.kind == TokenKind::LPAREN => token,
                    option => return Err(Error::MissingOpeningParenthesis(option)),
                };

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
                        option => return Err(Error::MissingIdentifier(option)),
                    }

                    if let Some(token) = lexer.peek() {
                        if token.kind == TokenKind::COMMA {
                            lexer.next();
                        }
                    }
                }

                match lexer.next() {
                    Some(token) if token.kind == TokenKind::RPAREN => token,
                    option => return Err(Error::MissingClosingParenthesis(option)),
                };

                parameters
            };

            let body = match StatementParser::parse(lexer)? {
                Statement::Block(expression) => expression,
                Statement::Let(expression) => {
                    return Err(Error::MissingBlockStatement(Some(expression.token)))
                }
                Statement::Return(expression) => {
                    return Err(Error::MissingBlockStatement(Some(expression.token)))
                }
                Statement::Expression(expression) => {
                    return Err(Error::MissingBlockStatement(Some(expression.token)))
                }
            };

            return Ok(Some(Expression::FUNCTION(FunctionExpression {
                token,
                parameters,
                body,
            })));
        }

        Ok(None)
    },
    dependents: Some(&[CALL_EXPRESSION_RULE]),
};

const CALL_EXPRESSION_RULE: Rule<Expression> = Rule {
    consume: |lexer, previous| {
        if let Some(token) = lexer.peek() {
            if previous.is_none() || token.kind != TokenKind::LPAREN {
                return Ok(None);
            }

            let token = match &previous {
                Some(Expression::IDENT(ident)) => ident.token.clone(),
                Some(Expression::FUNCTION(func)) => func.token.clone(),
                _ => unreachable!(),
            };

            let function = match &previous {
                Some(Expression::IDENT(ident)) => Box::new(Expression::IDENT(ident.clone())),
                Some(Expression::FUNCTION(func)) => Box::new(Expression::FUNCTION(func.clone())),
                _ => unreachable!(),
            };

            let arguments = {
                match lexer.next() {
                    Some(token) if token.kind == TokenKind::LPAREN => token,
                    option => return Err(Error::MissingOpeningParenthesis(option)),
                };

                let mut arguments = Vec::new();

                while let Some(token) = lexer.peek() {
                    if token.kind == TokenKind::RPAREN {
                        break;
                    }

                    let expression = ExpressionParser::parse(lexer, &Precedence::LOWEST)?;

                    arguments.push(expression);

                    if let Some(token) = lexer.peek() {
                        if token.kind == TokenKind::COMMA {
                            lexer.next();
                        }
                    }
                }

                match lexer.next() {
                    Some(token) if token.kind == TokenKind::RPAREN => token,
                    option => return Err(Error::MissingClosingParenthesis(option)),
                };

                arguments
            };

            return Ok(Some(Expression::CALL(CallExpression {
                token,
                function,
                arguments,
            })));
        }

        Ok(None)
    },
    dependents: None,
};

const INDEX_EXPRESSION_RULE: Rule<Expression> = Rule {
    consume: |lexer, previous| {
        if let Some(token) = lexer.peek() {
            if previous.is_none() || token.kind != TokenKind::LBRACKET {
                return Ok(None);
            }

            let token = match previous {
                Some(Expression::IDENT(ident)) => ident.token.clone(),
                Some(Expression::ARRAY(array)) => array.token.clone(),
                _ => unreachable!(),
            };

            let left = match previous {
                Some(Expression::IDENT(ident)) => Box::new(Expression::IDENT(ident.clone())),
                Some(Expression::ARRAY(array)) => Box::new(Expression::ARRAY(array.clone())),
                _ => unreachable!(),
            };

            match lexer.next() {
                Some(token) if token.kind == TokenKind::LBRACKET => token,
                option => return Err(Error::MissingOpeningBracket(option)),
            };

            let index = Box::new(ExpressionParser::parse(lexer, &Precedence::LOWEST)?);

            match lexer.next() {
                Some(token) if token.kind == TokenKind::RBRACKET => token,
                option => return Err(Error::MissingClosingBracket(option)),
            };

            return Ok(Some(Expression::INDEX(IndexExpression {
                token,
                left,
                index,
            })));
        }

        Ok(None)
    },
    dependents: None,
};
