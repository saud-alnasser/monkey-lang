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
            if let Some(expression) = Rule::parse(&None, rule, lexer)? {
                return Ok(expression);
            }
        }

        match lexer.next() {
            Some(token) => Err(Error::UnrecognizedToken(token)),
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
    accept: |token| token.kind == TokenKind::LPAREN,
    parse: |lexer, _| {
        match lexer.next() {
            Some(token) if token.kind == TokenKind::LPAREN => token,
            option => return Err(Error::MissingOpeningParenthesis(option)),
        };

        let expression = ExpressionParser::parse(lexer, &Precedence::LOWEST)?;

        match lexer.next() {
            Some(token) if token.kind == TokenKind::RPAREN => token,
            option => return Err(Error::MissingClosingParenthesis(option)),
        };

        Ok(expression)
    },
    dependents: None,
};

const IDENTIFIER_EXPRESSION_RULE: Rule<Expression> = Rule {
    accept: |token| token.kind == TokenKind::IDENT,
    parse: |lexer, _| {
        let token = lexer.next().unwrap();
        let value = token.literal.clone();

        Ok(Expression::IDENT(IdentExpression { token, value }))
    },
    dependents: Some(&[CALL_EXPRESSION_RULE, INDEX_EXPRESSION_RULE]),
};

const INTEGER_EXPRESSION_RULE: Rule<Expression> = Rule {
    accept: |token| token.kind == TokenKind::INT,
    parse: |lexer, _| {
        let token = lexer.next().unwrap();

        let value = match token.literal.parse::<i64>() {
            Ok(value) => value,
            Err(_) => return Err(Error::IllegalToken(token)),
        };

        Ok(Expression::INT(IntExpression { token, value }))
    },
    dependents: None,
};

const STRING_EXPRESSION_RULE: Rule<Expression> = Rule {
    accept: |token| token.kind == TokenKind::STRING,
    parse: |lexer, _| {
        let token = lexer.next().unwrap();
        let value = token.literal.clone();

        Ok(Expression::STRING(StringExpression { token, value }))
    },
    dependents: None,
};

const BOOLEAN_EXPRESSION_RULE: Rule<Expression> = Rule {
    accept: |token| token.kind == TokenKind::TRUE || token.kind == TokenKind::FALSE,
    parse: |lexer, _| {
        let token = lexer.next().unwrap();
        let value = token.kind == TokenKind::TRUE;

        Ok(Expression::BOOLEAN(BooleanExpression { token, value }))
    },
    dependents: None,
};

const ARRAY_EXPRESSION_RULE: Rule<Expression> = Rule {
    accept: |token| token.kind == TokenKind::LBRACKET,
    parse: |lexer, _| {
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

        Ok(Expression::ARRAY(ArrayExpression { token, elements }))
    },
    dependents: Some(&[INDEX_EXPRESSION_RULE]),
};

const PREFIX_EXPRESSION_RULE: Rule<Expression> = Rule {
    accept: |token| token.kind == TokenKind::MINUS || token.kind == TokenKind::BANG,
    parse: |lexer, _| {
        let operator = lexer.next().unwrap();

        let right = Box::new(ExpressionParser::parse(lexer, &Precedence::PREFIX)?);

        Ok(Expression::PREFIX(PrefixExpression { operator, right }))
    },
    dependents: None,
};

const IF_EXPRESSION_RULE: Rule<Expression> = Rule {
    accept: |token| token.kind == TokenKind::IF,
    parse: |lexer, _| {
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

        Ok(Expression::IF(IfExpression {
            token,
            condition,
            consequence,
            alternative,
        }))
    },
    dependents: None,
};

const FUNCTION_EXPRESSION_RULE: Rule<Expression> = Rule {
    accept: |token| token.kind == TokenKind::FUNCTION,
    parse: |lexer, _| {
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

        Ok(Expression::FUNCTION(FunctionExpression {
            token,
            parameters,
            body,
        }))
    },
    dependents: Some(&[CALL_EXPRESSION_RULE]),
};

const CALL_EXPRESSION_RULE: Rule<Expression> = Rule {
    accept: |token| token.kind == TokenKind::LPAREN,
    parse: |lexer, previous| {
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

        Ok(Expression::CALL(CallExpression {
            token,
            function,
            arguments,
        }))
    },
    dependents: None,
};

const INDEX_EXPRESSION_RULE: Rule<Expression> = Rule {
    accept: |token| token.kind == TokenKind::LBRACKET,
    parse: |lexer, previous| {
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

        Ok(Expression::INDEX(IndexExpression { token, left, index }))
    },
    dependents: None,
};
