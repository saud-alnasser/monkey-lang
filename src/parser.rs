use std::error::Error;

use crate::{Lexer, Program, Token, TokenKind};

use self::{errors::ParseError, statements::StatementParser};

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser { lexer }
    }

    pub fn parse(&mut self) -> Result<Program, Box<dyn Error>> {
        let mut program = Program::new();

        while let Some(token) = self.lexer.peek() {
            if token.kind == TokenKind::EOF {
                break;
            }

            if token.kind == TokenKind::ILLEGAL {
                return Err(Box::new(ParseError::IllegalToken(token.clone())));
            }

            program
                .statements
                .push(StatementParser::parse(&mut self.lexer)?);
        }

        Ok(program)
    }
}

#[derive(Debug)]
struct ParsingRule<'a, T> {
    pub accept: fn(token: &Token) -> bool,
    pub parse: fn(lexer: &mut Lexer, previous: &Option<T>) -> Result<T, ParseError>,
    pub dependents: Option<&'a [ParsingRule<'a, T>]>,
}

impl<'a, T> ParsingRule<'a, T> {
    fn parse(
        previous: &Option<T>,
        rule: &ParsingRule<T>,
        lexer: &mut Lexer,
    ) -> Result<Option<T>, ParseError> {
        if let Some(token) = lexer.peek() {
            if (rule.accept)(token) {
                let statement = Some((rule.parse)(lexer, previous)?);

                if let Some(dependents) = &rule.dependents {
                    for dependent in dependents.iter() {
                        if let Some(dependent_statement) =
                            ParsingRule::parse(&statement, dependent, lexer)?
                        {
                            return Ok(Some(dependent_statement));
                        }
                    }
                }

                return Ok(statement);
            }
        }

        Ok(None)
    }
}

mod statements {
    use crate::{
        BlockStatement, ExpressionStatement, LetStatement, Lexer, ReturnStatement, Statement,
        TokenKind,
    };

    use super::{
        errors::ParseError,
        expressions::{ExpressionParser, Precedence},
        ParsingRule,
    };

    pub struct StatementParser;

    impl StatementParser {
        pub fn parse(lexer: &mut Lexer) -> Result<Statement, ParseError> {
            for rule in &STATEMENT_RULES {
                if let Some(statement) = ParsingRule::parse(&None, rule, lexer)? {
                    return Ok(statement);
                }
            }

            match lexer.next() {
                Some(token) => Err(ParseError::UnrecognizedToken(token)),
                None => Err(ParseError::MissingToken),
            }
        }
    }

    // order matters
    // from with most constraints to least constraints
    const STATEMENT_RULES: [ParsingRule<Statement>; 4] = [
        BLOCK_STATEMENT_RULE,
        LET_STATEMENT_RULE,
        RETURN_STATEMENT_RULE,
        EXPRESSION_STATEMENT_RULE,
    ];

    const BLOCK_STATEMENT_RULE: ParsingRule<Statement> = ParsingRule {
        accept: |token| token.kind == TokenKind::LBRACE,
        parse: |lexer, _| {
            let token = match lexer.next() {
                Some(token) if token.kind == TokenKind::LBRACE => token,
                option => return Err(ParseError::MissingOpeningBrace(option)),
            };

            let mut statements = Vec::new();

            while let Some(token) = lexer.peek() {
                if token.kind == TokenKind::RBRACE {
                    break;
                }

                statements.push(StatementParser::parse(lexer)?);
            }

            match lexer.next() {
                Some(token) if token.kind == TokenKind::RBRACE => token,
                option => return Err(ParseError::MissingClosingBrace(option)),
            };

            Ok(Statement::Block(BlockStatement { token, statements }))
        },
        dependents: None,
    };

    const LET_STATEMENT_RULE: ParsingRule<Statement> = ParsingRule {
        accept: |token| token.kind == TokenKind::LET,
        parse: |lexer, _| {
            let token = match lexer.next() {
                Some(token) if token.kind == TokenKind::LET => token,
                option => return Err(ParseError::MissingLetKeyword(option)),
            };

            let identifier = match lexer.next() {
                Some(token) if token.kind == TokenKind::IDENT => token.literal,
                option => return Err(ParseError::MissingIdentifier(option)),
            };

            match lexer.next() {
                Some(token) if token.kind == TokenKind::ASSIGN => token,
                option => return Err(ParseError::MissingAssignmentOperator(option)),
            };

            let expression = ExpressionParser::parse(lexer, &Precedence::LOWEST)?;

            match lexer.next() {
                Some(token) if token.kind == TokenKind::SEMICOLON => token,
                option => return Err(ParseError::MissingSemicolon(option)),
            };

            Ok(Statement::Let(LetStatement {
                token,
                identifier,
                expression,
            }))
        },
        dependents: None,
    };

    const RETURN_STATEMENT_RULE: ParsingRule<Statement> = ParsingRule {
        accept: |token| token.kind == TokenKind::RETURN,
        parse: |lexer, _| {
            let token = match lexer.next() {
                Some(token) if token.kind == TokenKind::RETURN => token,
                option => return Err(ParseError::MissingReturnKeyword(option)),
            };

            let expression = ExpressionParser::parse(lexer, &Precedence::LOWEST)?;

            match lexer.next() {
                Some(token) if token.kind == TokenKind::SEMICOLON => token,
                option => return Err(ParseError::MissingSemicolon(option)),
            };

            Ok(Statement::Return(ReturnStatement { token, expression }))
        },
        dependents: None,
    };

    const EXPRESSION_STATEMENT_RULE: ParsingRule<Statement> = ParsingRule {
        accept: |_| true,
        parse: |lexer, _| {
            let token = lexer.peek().unwrap().clone();

            let expression = ExpressionParser::parse(lexer, &Precedence::LOWEST)?;

            match lexer.next() {
                Some(token) if token.kind == TokenKind::SEMICOLON => token,
                option => return Err(ParseError::MissingSemicolon(option)),
            };

            return Ok(Statement::Expression(ExpressionStatement {
                token,
                expression,
            }));
        },
        dependents: None,
    };
}

mod expressions {
    use crate::{
        ArrayExpression, BooleanExpression, CallExpression, Expression, FunctionExpression,
        IdentExpression, IfExpression, IndexExpression, InfixExpression, IntExpression, Lexer,
        PrefixExpression, Statement, StringExpression, TokenKind,
    };

    use super::{errors::ParseError, statements::StatementParser, ParsingRule};

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
        fn parse_left(lexer: &mut Lexer) -> Result<Expression, ParseError> {
            for rule in &EXPRESSION_RULES {
                if let Some(expression) = ParsingRule::parse(&None, rule, lexer)? {
                    return Ok(expression);
                }
            }

            match lexer.next() {
                Some(token) => Err(ParseError::UnrecognizedToken(token)),
                None => Err(ParseError::MissingToken),
            }
        }

        pub fn parse(lexer: &mut Lexer, precedence: &Precedence) -> Result<Expression, ParseError> {
            let mut left = ExpressionParser::parse_left(lexer)?;

            while let Some(token) = lexer.peek() {
                if precedence >= &Precedence::from(&token.kind) {
                    break;
                }

                let operator = match lexer.next() {
                    Some(token) => token,
                    None => return Err(ParseError::MissingToken),
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
    const EXPRESSION_RULES: [ParsingRule<Expression>; 9] = [
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

    const GROUPING_EXPRESSION_RULE: ParsingRule<Expression> = ParsingRule {
        accept: |token| token.kind == TokenKind::LPAREN,
        parse: |lexer, _| {
            match lexer.next() {
                Some(token) if token.kind == TokenKind::LPAREN => token,
                option => return Err(ParseError::MissingOpeningParenthesis(option)),
            };

            let expression = ExpressionParser::parse(lexer, &Precedence::LOWEST)?;

            match lexer.next() {
                Some(token) if token.kind == TokenKind::RPAREN => token,
                option => return Err(ParseError::MissingClosingParenthesis(option)),
            };

            Ok(expression)
        },
        dependents: None,
    };

    const IDENTIFIER_EXPRESSION_RULE: ParsingRule<Expression> = ParsingRule {
        accept: |token| token.kind == TokenKind::IDENT,
        parse: |lexer, _| {
            let token = lexer.next().unwrap();
            let value = token.literal.clone();

            Ok(Expression::IDENT(IdentExpression { token, value }))
        },
        dependents: Some(&[CALL_EXPRESSION_RULE, INDEX_EXPRESSION_RULE]),
    };

    const INTEGER_EXPRESSION_RULE: ParsingRule<Expression> = ParsingRule {
        accept: |token| token.kind == TokenKind::INT,
        parse: |lexer, _| {
            let token = lexer.next().unwrap();

            let value = match token.literal.parse::<i64>() {
                Ok(value) => value,
                Err(_) => return Err(ParseError::IllegalToken(token)),
            };

            Ok(Expression::INT(IntExpression { token, value }))
        },
        dependents: None,
    };

    const STRING_EXPRESSION_RULE: ParsingRule<Expression> = ParsingRule {
        accept: |token| token.kind == TokenKind::STRING,
        parse: |lexer, _| {
            let token = lexer.next().unwrap();
            let value = token.literal.clone();

            Ok(Expression::STRING(StringExpression { token, value }))
        },
        dependents: None,
    };

    const BOOLEAN_EXPRESSION_RULE: ParsingRule<Expression> = ParsingRule {
        accept: |token| token.kind == TokenKind::TRUE || token.kind == TokenKind::FALSE,
        parse: |lexer, _| {
            let token = lexer.next().unwrap();
            let value = token.kind == TokenKind::TRUE;

            Ok(Expression::BOOLEAN(BooleanExpression { token, value }))
        },
        dependents: None,
    };

    const ARRAY_EXPRESSION_RULE: ParsingRule<Expression> = ParsingRule {
        accept: |token| token.kind == TokenKind::LBRACKET,
        parse: |lexer, _| {
            let token = match lexer.next() {
                Some(token) if token.kind == TokenKind::LBRACKET => token,
                option => return Err(ParseError::MissingOpeningBracket(option)),
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
                option => return Err(ParseError::MissingClosingBracket(option)),
            };

            Ok(Expression::ARRAY(ArrayExpression { token, elements }))
        },
        dependents: Some(&[INDEX_EXPRESSION_RULE]),
    };

    const PREFIX_EXPRESSION_RULE: ParsingRule<Expression> = ParsingRule {
        accept: |token| token.kind == TokenKind::MINUS || token.kind == TokenKind::BANG,
        parse: |lexer, _| {
            let operator = lexer.next().unwrap();

            let right = Box::new(ExpressionParser::parse(lexer, &Precedence::PREFIX)?);

            Ok(Expression::PREFIX(PrefixExpression { operator, right }))
        },
        dependents: None,
    };

    const IF_EXPRESSION_RULE: ParsingRule<Expression> = ParsingRule {
        accept: |token| token.kind == TokenKind::IF,
        parse: |lexer, _| {
            let token = lexer.next().unwrap();

            let condition = Box::new(ExpressionParser::parse(lexer, &Precedence::LOWEST)?);

            let consequence = match StatementParser::parse(lexer)? {
                Statement::Block(expression) => expression,
                Statement::Let(expression) => {
                    return Err(ParseError::MissingBlockStatement(Some(expression.token)))
                }
                Statement::Return(expression) => {
                    return Err(ParseError::MissingBlockStatement(Some(expression.token)))
                }
                Statement::Expression(expression) => {
                    return Err(ParseError::MissingBlockStatement(Some(expression.token)))
                }
            };

            let alternative = match lexer.peek() {
                Some(token) if token.kind == TokenKind::ELSE => {
                    lexer.next().unwrap();

                    match StatementParser::parse(lexer)? {
                        Statement::Block(expression) => Some(expression),
                        Statement::Let(expression) => {
                            return Err(ParseError::MissingBlockStatement(Some(expression.token)))
                        }
                        Statement::Return(expression) => {
                            return Err(ParseError::MissingBlockStatement(Some(expression.token)))
                        }
                        Statement::Expression(expression) => {
                            return Err(ParseError::MissingBlockStatement(Some(expression.token)))
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

    const FUNCTION_EXPRESSION_RULE: ParsingRule<Expression> = ParsingRule {
        accept: |token| token.kind == TokenKind::FUNCTION,
        parse: |lexer, _| {
            let token = lexer.next().unwrap();

            let parameters = {
                match lexer.next() {
                    Some(token) if token.kind == TokenKind::LPAREN => token,
                    option => return Err(ParseError::MissingOpeningParenthesis(option)),
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
                        option => return Err(ParseError::MissingIdentifier(option)),
                    }

                    if let Some(token) = lexer.peek() {
                        if token.kind == TokenKind::COMMA {
                            lexer.next();
                        }
                    }
                }

                match lexer.next() {
                    Some(token) if token.kind == TokenKind::RPAREN => token,
                    option => return Err(ParseError::MissingClosingParenthesis(option)),
                };

                parameters
            };

            let body = match StatementParser::parse(lexer)? {
                Statement::Block(expression) => expression,
                Statement::Let(expression) => {
                    return Err(ParseError::MissingBlockStatement(Some(expression.token)))
                }
                Statement::Return(expression) => {
                    return Err(ParseError::MissingBlockStatement(Some(expression.token)))
                }
                Statement::Expression(expression) => {
                    return Err(ParseError::MissingBlockStatement(Some(expression.token)))
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

    const CALL_EXPRESSION_RULE: ParsingRule<Expression> = ParsingRule {
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
                    option => return Err(ParseError::MissingOpeningParenthesis(option)),
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
                    option => return Err(ParseError::MissingClosingParenthesis(option)),
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

    const INDEX_EXPRESSION_RULE: ParsingRule<Expression> = ParsingRule {
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
                option => return Err(ParseError::MissingOpeningBracket(option)),
            };

            let index = Box::new(ExpressionParser::parse(lexer, &Precedence::LOWEST)?);

            match lexer.next() {
                Some(token) if token.kind == TokenKind::RBRACKET => token,
                option => return Err(ParseError::MissingClosingBracket(option)),
            };

            Ok(Expression::INDEX(IndexExpression { token, left, index }))
        },
        dependents: None,
    };
}

mod errors {
    use std::{error::Error, fmt::Display};

    use crate::Token;

    #[derive(Debug)]
    pub enum ParseError {
        MissingToken,
        MissingOpeningParenthesis(Option<Token>),
        MissingClosingParenthesis(Option<Token>),
        MissingOpeningBrace(Option<Token>),
        MissingClosingBrace(Option<Token>),
        MissingOpeningBracket(Option<Token>),
        MissingClosingBracket(Option<Token>),
        MissingLetKeyword(Option<Token>),
        MissingAssignmentOperator(Option<Token>),
        MissingReturnKeyword(Option<Token>),
        MissingSemicolon(Option<Token>),
        MissingIdentifier(Option<Token>),
        MissingBlockStatement(Option<Token>),
        UnrecognizedToken(Token),
        IllegalToken(Token),
    }

    impl Error for ParseError {
        fn description(&self) -> &str {
            match self {
                ParseError::MissingToken => "missing a token",
                ParseError::MissingOpeningParenthesis(_) => "missing opening parenthesis",
                ParseError::MissingClosingParenthesis(_) => "missing closing parenthesis",
                ParseError::MissingOpeningBrace(_) => "missing opening brace",
                ParseError::MissingClosingBrace(_) => "missing closing brace",
                ParseError::MissingOpeningBracket(_) => "missing opening bracket",
                ParseError::MissingClosingBracket(_) => "missing closing bracket",
                ParseError::MissingLetKeyword(_) => "missing let keyword",
                ParseError::MissingAssignmentOperator(_) => "missing assignment operator",
                ParseError::MissingReturnKeyword(_) => "missing return keyword",
                ParseError::MissingSemicolon(_) => "missing semicolon",
                ParseError::MissingIdentifier(_) => "missing identifier",
                ParseError::MissingBlockStatement(_) => "expected block statement",
                ParseError::UnrecognizedToken(_) => "unrecognized token",
                ParseError::IllegalToken(_) => "illegal token",
            }
        }
    }

    impl Display for ParseError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                ParseError::MissingToken => write!(f, "missing a token"),
                ParseError::MissingOpeningParenthesis(token) => match token {
                    Some(token) => write!(
                        f,
                        "expected opening parenthesis, got {} at {}:{}",
                        token.literal, token.span.line, token.span.column
                    ),
                    None => write!(f, "expected opening parenthesis, got EOF"),
                },
                ParseError::MissingClosingParenthesis(token) => match token {
                    Some(token) => write!(
                        f,
                        "expected closing parenthesis, got {} at {}:{}",
                        token.literal, token.span.line, token.span.column
                    ),
                    None => write!(f, "expected closing parenthesis, got EOF"),
                },
                ParseError::MissingOpeningBrace(token) => match token {
                    Some(token) => write!(
                        f,
                        "expected opening brace, got {} at {}:{}",
                        token.literal, token.span.line, token.span.column
                    ),
                    None => write!(f, "expected opening brace, got EOF"),
                },
                ParseError::MissingClosingBrace(token) => match token {
                    Some(token) => write!(
                        f,
                        "expected closing brace, got {} at {}:{}",
                        token.literal, token.span.line, token.span.column
                    ),
                    None => write!(f, "expected closing brace, got EOF"),
                },
                ParseError::MissingOpeningBracket(token) => match token {
                    Some(token) => write!(
                        f,
                        "expected opening bracket, got {} at {}:{}",
                        token.literal, token.span.line, token.span.column
                    ),
                    None => write!(f, "expected opening bracket, got EOF"),
                },
                ParseError::MissingClosingBracket(token) => match token {
                    Some(token) => write!(
                        f,
                        "expected closing bracket, got {} at {}:{}",
                        token.literal, token.span.line, token.span.column
                    ),
                    None => write!(f, "expected closing bracket, got EOF"),
                },
                ParseError::MissingLetKeyword(token) => match token {
                    Some(token) => write!(
                        f,
                        "expected let keyword, got {} at {}:{}",
                        token.literal, token.span.line, token.span.column
                    ),
                    None => write!(f, "expected let keyword, got EOF"),
                },
                ParseError::MissingAssignmentOperator(token) => match token {
                    Some(token) => write!(
                        f,
                        "expected assignment operator, got {} at {}:{}",
                        token.literal, token.span.line, token.span.column
                    ),
                    None => write!(f, "expected assignment operator, got EOF"),
                },
                ParseError::MissingReturnKeyword(token) => match token {
                    Some(token) => write!(
                        f,
                        "expected return keyword, got {} at {}:{}",
                        token.literal, token.span.line, token.span.column
                    ),
                    None => write!(f, "expected return keyword, got EOF"),
                },
                ParseError::MissingSemicolon(token) => match token {
                    Some(token) => write!(
                        f,
                        "expected semicolon, got {} at {}:{}",
                        token.literal, token.span.line, token.span.column
                    ),
                    None => write!(f, "expected semicolon, got EOF"),
                },
                ParseError::MissingIdentifier(token) => match token {
                    Some(token) => write!(
                        f,
                        "expected identifier, got {} at {}:{}",
                        token.literal, token.span.line, token.span.column
                    ),
                    None => write!(f, "expected identifier, got EOF"),
                },
                ParseError::MissingBlockStatement(token) => match token {
                    Some(token) => write!(
                        f,
                        "expected block statement, got {} at {}:{}",
                        token.literal, token.span.line, token.span.column
                    ),
                    None => write!(f, "expected block statement, got EOF"),
                },
                ParseError::UnrecognizedToken(token) => write!(
                    f,
                    "unrecognized token {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                ),
                ParseError::IllegalToken(token) => write!(
                    f,
                    "illegal token {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                ),
            }
        }
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
