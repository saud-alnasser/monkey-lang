use crate::{
    parser::{
        error::{Error, Result},
        expression::{ExpressionParser, Precedence},
        rule::Rule,
    },
    BlockStatement, ExpressionStatement, LetStatement, Lexer, ReturnStatement, Statement,
    TokenKind,
};

pub struct StatementParser;

impl StatementParser {
    pub fn parse(lexer: &mut Lexer) -> Result<Statement> {
        for rule in &STATEMENT_RULES {
            if let Some(statement) = Rule::parse(&None, rule, lexer)? {
                return Ok(statement);
            }
        }

        match lexer.next() {
            Some(token) => Err(Error::UnrecognizedToken(token)),
            None => Err(Error::MissingToken),
        }
    }
}

// order matters
// from with most constraints to least constraints
const STATEMENT_RULES: [Rule<Statement>; 4] = [
    BLOCK_STATEMENT_RULE,
    LET_STATEMENT_RULE,
    RETURN_STATEMENT_RULE,
    EXPRESSION_STATEMENT_RULE,
];

const BLOCK_STATEMENT_RULE: Rule<Statement> = Rule {
    accept: |token| token.kind == TokenKind::LBRACE,
    parse: |lexer, _| {
        let token = match lexer.next() {
            Some(token) if token.kind == TokenKind::LBRACE => token,
            option => return Err(Error::MissingOpeningBrace(option)),
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
            option => return Err(Error::MissingClosingBrace(option)),
        };

        Ok(Statement::Block(BlockStatement { token, statements }))
    },
    dependents: None,
};

const LET_STATEMENT_RULE: Rule<Statement> = Rule {
    accept: |token| token.kind == TokenKind::LET,
    parse: |lexer, _| {
        let token = match lexer.next() {
            Some(token) if token.kind == TokenKind::LET => token,
            option => return Err(Error::MissingLetKeyword(option)),
        };

        let identifier = match lexer.next() {
            Some(token) if token.kind == TokenKind::IDENT => token.literal,
            option => return Err(Error::MissingIdentifier(option)),
        };

        match lexer.next() {
            Some(token) if token.kind == TokenKind::ASSIGN => token,
            option => return Err(Error::MissingAssignmentOperator(option)),
        };

        let expression = ExpressionParser::parse(lexer, &Precedence::LOWEST)?;

        match lexer.next() {
            Some(token) if token.kind == TokenKind::SEMICOLON => token,
            option => return Err(Error::MissingSemicolon(option)),
        };

        Ok(Statement::Let(LetStatement {
            token,
            identifier,
            expression,
        }))
    },
    dependents: None,
};

const RETURN_STATEMENT_RULE: Rule<Statement> = Rule {
    accept: |token| token.kind == TokenKind::RETURN,
    parse: |lexer, _| {
        let token = match lexer.next() {
            Some(token) if token.kind == TokenKind::RETURN => token,
            option => return Err(Error::MissingReturnKeyword(option)),
        };

        let expression = ExpressionParser::parse(lexer, &Precedence::LOWEST)?;

        match lexer.next() {
            Some(token) if token.kind == TokenKind::SEMICOLON => token,
            option => return Err(Error::MissingSemicolon(option)),
        };

        Ok(Statement::Return(ReturnStatement { token, expression }))
    },
    dependents: None,
};

const EXPRESSION_STATEMENT_RULE: Rule<Statement> = Rule {
    accept: |_| true,
    parse: |lexer, _| {
        let token = lexer.peek().unwrap().clone();

        let expression = ExpressionParser::parse(lexer, &Precedence::LOWEST)?;

        match lexer.next() {
            Some(token) if token.kind == TokenKind::SEMICOLON => token,
            option => return Err(Error::MissingSemicolon(option)),
        };

        return Ok(Statement::Expression(ExpressionStatement {
            token,
            expression,
        }));
    },
    dependents: None,
};
