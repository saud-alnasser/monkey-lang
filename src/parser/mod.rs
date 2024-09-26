pub mod error;

use crate::{
    parser::error::*, ArrayExpression, BlockExpression, BooleanExpression, CallExpression,
    Expression, ExpressionStatement, FunctionExpression, HashExpression, IdentExpression,
    IfExpression, IndexExpression, InfixExpression, IntExpression, LetStatement, Lexer,
    PrefixExpression, Program, ReturnStatement, Statement, StringExpression, TokenKind,
};

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

            program.statements.push(self.parse_statement()?);
        }

        Ok(program)
    }

    fn parse_statement(self: &mut Parser<'a>) -> Result<Statement> {
        if let Some(expression) = self.parse_let_statement()? {
            match self.lexer.next() {
                Some(token) if token.kind == TokenKind::SEMICOLON => token,
                option => return Err(Error::MissingSemicolon(option)),
            };

            return Ok(expression);
        }

        if let Some(expression) = self.parse_return_statement()? {
            match self.lexer.next() {
                Some(token) if token.kind == TokenKind::SEMICOLON => token,
                option => return Err(Error::MissingSemicolon(option)),
            };

            return Ok(expression);
        }

        let token = self.lexer.peek(0).unwrap().clone();

        let expression = self.parse_expression(&Precedence::LOWEST)?;

        match self.lexer.next() {
            Some(token) if token.kind == TokenKind::SEMICOLON => token,
            option => return Err(Error::MissingSemicolon(option)),
        };

        return Ok(Statement::Expression(ExpressionStatement {
            token,
            expression,
        }));
    }

    fn parse_let_statement(self: &mut Parser<'a>) -> Result<Option<Statement>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::LET {
                return Ok(None);
            }

            let token = match self.lexer.next() {
                Some(token) if token.kind == TokenKind::LET => token,
                option => return Err(Error::MissingLetKeyword(option)),
            };

            let identifier = match self.lexer.next() {
                Some(token) if token.kind == TokenKind::IDENT => token.literal,
                option => return Err(Error::MissingIdentifier(option)),
            };

            match self.lexer.next() {
                Some(token) if token.kind == TokenKind::ASSIGN => token,
                option => return Err(Error::MissingAssignmentOperator(option)),
            };

            let expression = self.parse_expression(&Precedence::LOWEST)?;

            return Ok(Some(Statement::Let(LetStatement {
                token,
                identifier,
                expression,
            })));
        }

        Ok(None)
    }

    fn parse_return_statement(self: &mut Parser<'a>) -> Result<Option<Statement>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::RETURN {
                return Ok(None);
            }

            let token = match self.lexer.next() {
                Some(token) if token.kind == TokenKind::RETURN => token,
                option => return Err(Error::MissingReturnKeyword(option)),
            };

            let expression = self.parse_expression(&Precedence::LOWEST)?;

            return Ok(Some(Statement::Return(ReturnStatement {
                token,
                expression,
            })));
        }

        Ok(None)
    }

    fn parse_expression(self: &mut Parser<'a>, precedence: &Precedence) -> Result<Expression> {
        let mut left = self.parse_expression_left()?;

        while let Some(token) = self.lexer.peek(0) {
            if precedence >= &Precedence::from(&token.kind) {
                break;
            }

            let operator = match self.lexer.next() {
                Some(token) => token,
                None => return Err(Error::MissingToken),
            };

            let right = self.parse_expression(&Precedence::from(&operator.kind))?;

            left = Expression::Infix(InfixExpression {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            });
        }

        Ok(left)
    }

    fn parse_expression_left(self: &mut Parser<'a>) -> Result<Expression> {
        if let Some(expression) = self.parse_hash_expression()? {
            if let Some(expression) = self.parse_index_expression(&expression)? {
                return Ok(expression);
            }

            return Ok(expression);
        }

        if let Some(expression) = self.parse_block_expression()? {
            return Ok(expression);
        }

        if let Some(expression) = self.prase_grouping_expression()? {
            return Ok(expression);
        }

        if let Some(ident_expression) = self.parse_ident_expression()? {
            if let Some(expression) = self.parse_call_expression(&ident_expression)? {
                return Ok(expression);
            }

            if let Some(expression) = self.parse_index_expression(&ident_expression)? {
                return Ok(expression);
            }

            return Ok(ident_expression);
        }

        if let Some(expression) = self.parse_integer_expression()? {
            return Ok(expression);
        }

        if let Some(expression) = self.parse_string_expression()? {
            return Ok(expression);
        }

        if let Some(expression) = self.parse_boolean_expression()? {
            return Ok(expression);
        }

        if let Some(array_expression) = self.parse_array_expression()? {
            if let Some(expression) = self.parse_index_expression(&array_expression)? {
                return Ok(expression);
            }

            return Ok(array_expression);
        }

        if let Some(expression) = self.parse_prefix_expression()? {
            return Ok(expression);
        }

        if let Some(expression) = self.parse_if_expression()? {
            return Ok(expression);
        }

        if let Some(function_expression) = self.parse_function_expression()? {
            if let Some(expression) = self.parse_call_expression(&function_expression)? {
                return Ok(expression);
            }

            return Ok(function_expression);
        }

        match self.lexer.next() {
            Some(token) => Err(Error::UnexpectedToken(token)),
            None => Err(Error::MissingToken),
        }
    }

    fn parse_hash_expression(self: &mut Parser<'a>) -> Result<Option<Expression>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::LBRACE {
                return Ok(None);
            }

            let mut i = 0;

            while let Some(token) = self.lexer.peek(i) {
                if token.kind == TokenKind::RBRACE {
                    return Ok(None);
                }

                if token.kind == TokenKind::COLON {
                    let token = match self.lexer.next() {
                        Some(token) if token.kind == TokenKind::LBRACE => token,
                        option => return Err(Error::MissingOpeningBrace(option)),
                    };

                    let mut pairs = Vec::new();

                    while let Some(token) = self.lexer.peek(0) {
                        if token.kind == TokenKind::RBRACE {
                            break;
                        }

                        let key = self.parse_expression(&Precedence::LOWEST)?;

                        match self.lexer.next() {
                            Some(token) if token.kind == TokenKind::COLON => token,
                            option => return Err(Error::MissingColon(option)),
                        };

                        let value = self.parse_expression(&Precedence::LOWEST)?;

                        pairs.push((Box::new(key), Box::new(value)));

                        if let Some(token) = self.lexer.peek(0) {
                            if token.kind == TokenKind::COMMA {
                                self.lexer.next();
                            }
                        }
                    }

                    match self.lexer.next() {
                        Some(token) if token.kind == TokenKind::RBRACE => token,
                        option => return Err(Error::MissingClosingBrace(option)),
                    };

                    return Ok(Some(Expression::Hash(HashExpression { token, pairs })));
                }

                i += 1;
            }
        }

        Ok(None)
    }

    fn parse_block_expression(self: &mut Parser<'a>) -> Result<Option<Expression>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::LBRACE {
                return Ok(None);
            }

            let token = match self.lexer.next() {
                Some(token) if token.kind == TokenKind::LBRACE => token,
                option => return Err(Error::MissingOpeningBrace(option)),
            };

            let mut statements = Vec::new();

            while let Some(token) = self.lexer.peek(0) {
                if token.kind == TokenKind::RBRACE {
                    break;
                }

                statements.push(self.parse_statement()?);
            }

            match self.lexer.next() {
                Some(token) if token.kind == TokenKind::RBRACE => token,
                option => return Err(Error::MissingClosingBrace(option)),
            };

            return Ok(Some(Expression::Block(BlockExpression {
                token,
                statements,
            })));
        }

        Ok(None)
    }

    fn prase_grouping_expression(self: &mut Parser<'a>) -> Result<Option<Expression>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::LPAREN {
                return Ok(None);
            }

            match self.lexer.next() {
                Some(token) if token.kind == TokenKind::LPAREN => token,
                option => return Err(Error::MissingOpeningParenthesis(option)),
            };

            let expression = self.parse_expression(&Precedence::LOWEST)?;

            match self.lexer.next() {
                Some(token) if token.kind == TokenKind::RPAREN => token,
                option => return Err(Error::MissingClosingParenthesis(option)),
            };

            return Ok(Some(expression));
        }

        Ok(None)
    }

    fn parse_ident_expression(self: &mut Parser<'a>) -> Result<Option<Expression>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::IDENT {
                return Ok(None);
            }

            let token = self.lexer.next().unwrap();
            let value = token.literal.clone();

            return Ok(Some(Expression::Ident(IdentExpression { token, value })));
        }

        Ok(None)
    }

    fn parse_integer_expression(self: &mut Parser<'a>) -> Result<Option<Expression>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::INT {
                return Ok(None);
            }

            let token = self.lexer.next().unwrap();

            let value = match token.literal.parse::<i64>() {
                Ok(value) => value,
                Err(_) => return Err(Error::IllegalToken(token)),
            };

            return Ok(Some(Expression::Int(IntExpression { token, value })));
        }

        Ok(None)
    }

    fn parse_string_expression(self: &mut Parser<'a>) -> Result<Option<Expression>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::STRING {
                return Ok(None);
            }

            let token = self.lexer.next().unwrap();
            let value = token.literal[1..token.literal.len() - 1].into();

            return Ok(Some(Expression::String(StringExpression { token, value })));
        }

        Ok(None)
    }

    fn parse_boolean_expression(self: &mut Parser<'a>) -> Result<Option<Expression>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::TRUE && token.kind != TokenKind::FALSE {
                return Ok(None);
            }

            let token = self.lexer.next().unwrap();
            let value = token.kind == TokenKind::TRUE;

            return Ok(Some(Expression::Boolean(BooleanExpression {
                token,
                value,
            })));
        }

        Ok(None)
    }

    fn parse_array_expression(self: &mut Parser<'a>) -> Result<Option<Expression>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::LBRACKET {
                return Ok(None);
            }

            let token = match self.lexer.next() {
                Some(token) if token.kind == TokenKind::LBRACKET => token,
                option => return Err(Error::MissingOpeningBracket(option)),
            };

            let mut elements = Vec::new();

            while let Some(token) = self.lexer.peek(0) {
                if token.kind == TokenKind::RBRACKET {
                    break;
                }

                let expression = self.parse_expression(&Precedence::LOWEST)?;

                elements.push(expression);

                if let Some(token) = self.lexer.peek(0) {
                    if token.kind == TokenKind::COMMA {
                        self.lexer.next();
                    }
                }
            }

            match self.lexer.next() {
                Some(token) if token.kind == TokenKind::RBRACKET => token,
                option => return Err(Error::MissingClosingBracket(option)),
            };

            return Ok(Some(Expression::Array(ArrayExpression { token, elements })));
        }

        Ok(None)
    }

    fn parse_prefix_expression(self: &mut Parser<'a>) -> Result<Option<Expression>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::MINUS && token.kind != TokenKind::BANG {
                return Ok(None);
            }

            let operator = self.lexer.next().unwrap();

            let right = Box::new(self.parse_expression(&Precedence::PREFIX)?);

            return Ok(Some(Expression::Prefix(PrefixExpression {
                operator,
                right,
            })));
        }

        Ok(None)
    }

    fn parse_if_expression(self: &mut Parser<'a>) -> Result<Option<Expression>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::IF {
                return Ok(None);
            }

            let token = self.lexer.next().unwrap();

            let condition = Box::new(self.parse_expression(&Precedence::LOWEST)?);

            let consequence = match self.parse_expression(&Precedence::LOWEST)? {
                Expression::Block(expression) => expression,
                Expression::Ident(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Int(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::String(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Boolean(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Array(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::If(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Function(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Hash(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Call(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Index(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Prefix(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.operator)))
                }
                Expression::Infix(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.operator)))
                }
            };

            let alternative = match self.lexer.peek(0) {
                Some(token) if token.kind == TokenKind::ELSE => {
                    self.lexer.next().unwrap();

                    Some(match self.parse_expression(&Precedence::LOWEST)? {
                        Expression::Block(expression) => expression,
                        Expression::Ident(expression) => {
                            return Err(Error::MissingBlockExpression(Some(expression.token)))
                        }
                        Expression::Int(expression) => {
                            return Err(Error::MissingBlockExpression(Some(expression.token)))
                        }
                        Expression::String(expression) => {
                            return Err(Error::MissingBlockExpression(Some(expression.token)))
                        }
                        Expression::Boolean(expression) => {
                            return Err(Error::MissingBlockExpression(Some(expression.token)))
                        }
                        Expression::Array(expression) => {
                            return Err(Error::MissingBlockExpression(Some(expression.token)))
                        }
                        Expression::If(expression) => {
                            return Err(Error::MissingBlockExpression(Some(expression.token)))
                        }
                        Expression::Function(expression) => {
                            return Err(Error::MissingBlockExpression(Some(expression.token)))
                        }
                        Expression::Hash(expression) => {
                            return Err(Error::MissingBlockExpression(Some(expression.token)))
                        }
                        Expression::Call(expression) => {
                            return Err(Error::MissingBlockExpression(Some(expression.token)))
                        }
                        Expression::Index(expression) => {
                            return Err(Error::MissingBlockExpression(Some(expression.token)))
                        }
                        Expression::Prefix(expression) => {
                            return Err(Error::MissingBlockExpression(Some(expression.operator)))
                        }
                        Expression::Infix(expression) => {
                            return Err(Error::MissingBlockExpression(Some(expression.operator)))
                        }
                    })
                }
                _ => None,
            };

            return Ok(Some(Expression::If(IfExpression {
                token,
                condition,
                consequence,
                alternative,
            })));
        }

        Ok(None)
    }

    fn parse_function_expression(self: &mut Parser<'a>) -> Result<Option<Expression>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::FUNCTION {
                return Ok(None);
            }

            let token = self.lexer.next().unwrap();

            let parameters = {
                match self.lexer.next() {
                    Some(token) if token.kind == TokenKind::LPAREN => token,
                    option => return Err(Error::MissingOpeningParenthesis(option)),
                };

                let mut parameters = Vec::new();

                while let Some(token) = self.lexer.peek(0) {
                    if token.kind == TokenKind::RPAREN {
                        break;
                    }

                    match self.lexer.next() {
                        Some(token) if token.kind == TokenKind::IDENT => {
                            let value = token.literal.clone();

                            parameters.push(IdentExpression { token, value });
                        }
                        option => return Err(Error::MissingIdentifier(option)),
                    }

                    if let Some(token) = self.lexer.peek(0) {
                        if token.kind == TokenKind::COMMA {
                            self.lexer.next();
                        }
                    }
                }

                match self.lexer.next() {
                    Some(token) if token.kind == TokenKind::RPAREN => token,
                    option => return Err(Error::MissingClosingParenthesis(option)),
                };

                parameters
            };

            let body = match self.parse_expression(&Precedence::LOWEST)? {
                Expression::Block(expression) => expression,
                Expression::Ident(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Int(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::String(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Boolean(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Array(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::If(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Function(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Hash(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Call(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Index(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.token)))
                }
                Expression::Prefix(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.operator)))
                }
                Expression::Infix(expression) => {
                    return Err(Error::MissingBlockExpression(Some(expression.operator)))
                }
            };

            return Ok(Some(Expression::Function(FunctionExpression {
                token,
                parameters,
                body,
            })));
        }

        Ok(None)
    }

    fn parse_call_expression(
        self: &mut Parser<'a>,
        callable: &Expression,
    ) -> Result<Option<Expression>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::LPAREN {
                return Ok(None);
            }

            let token = match &callable {
                Expression::Ident(ident) => ident.token.clone(),
                Expression::Function(func) => func.token.clone(),
                _ => unreachable!(),
            };

            let function = match &callable {
                Expression::Ident(ident) => Box::new(Expression::Ident(ident.clone())),
                Expression::Function(func) => Box::new(Expression::Function(func.clone())),
                _ => unreachable!(),
            };

            let arguments = {
                match self.lexer.next() {
                    Some(token) if token.kind == TokenKind::LPAREN => token,
                    option => return Err(Error::MissingOpeningParenthesis(option)),
                };

                let mut arguments = Vec::new();

                while let Some(token) = self.lexer.peek(0) {
                    if token.kind == TokenKind::RPAREN {
                        break;
                    }

                    let expression = self.parse_expression(&Precedence::LOWEST)?;

                    arguments.push(expression);

                    if let Some(token) = self.lexer.peek(0) {
                        if token.kind == TokenKind::COMMA {
                            self.lexer.next();
                        }
                    }
                }

                match self.lexer.next() {
                    Some(token) if token.kind == TokenKind::RPAREN => token,
                    option => return Err(Error::MissingClosingParenthesis(option)),
                };

                arguments
            };

            return Ok(Some(Expression::Call(CallExpression {
                token,
                function,
                arguments,
            })));
        }

        Ok(None)
    }

    fn parse_index_expression(
        self: &mut Parser<'a>,
        indexable: &Expression,
    ) -> Result<Option<Expression>> {
        if let Some(token) = self.lexer.peek(0) {
            if token.kind != TokenKind::LBRACKET {
                return Ok(None);
            }

            let token = match indexable {
                Expression::Ident(ident) => ident.token.clone(),
                Expression::Array(array) => array.token.clone(),
                Expression::Hash(hash) => hash.token.clone(),
                Expression::Index(index) => index.token.clone(),
                _ => unreachable!(),
            };

            let left = match indexable {
                Expression::Ident(ident) => Box::new(Expression::Ident(ident.clone())),
                Expression::Array(array) => Box::new(Expression::Array(array.clone())),
                Expression::Hash(hash) => Box::new(Expression::Hash(hash.clone())),
                Expression::Index(index) => Box::new(Expression::Index(index.clone())),
                _ => unreachable!(),
            };

            match self.lexer.next() {
                Some(token) if token.kind == TokenKind::LBRACKET => token,
                option => return Err(Error::MissingOpeningBracket(option)),
            };

            let index = Box::new(self.parse_expression(&Precedence::LOWEST)?);

            match self.lexer.next() {
                Some(token) if token.kind == TokenKind::RBRACKET => token,
                option => return Err(Error::MissingClosingBracket(option)),
            };

            let expression = Expression::Index(IndexExpression { token, left, index });

            if let Some(expression) = self.parse_index_expression(&expression)? {
                return Ok(Some(expression));
            }

            return Ok(Some(expression));
        }

        Ok(None)
    }
}

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
    STATEMENT = 8,   // statement: let x = 5;
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

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        ArrayExpression, BooleanExpression, DataType, Environment, Evaluator, Expression,
        ExpressionStatement, FunctionExpression, HashExpression, IdentExpression, IfExpression,
        InfixExpression, IntExpression, LetStatement, Lexer, PrefixExpression, ReturnStatement,
        StringExpression, Token, TokenKind,
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

                    match *expression {
                        Expression::Int(IntExpression { value, .. }) => {
                            assert_eq!(value, *expected_value);
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
                Statement::Return(ReturnStatement { expression, .. }) => match *expression {
                    Expression::Int(IntExpression { value, .. }) => {
                        assert_eq!(value, *expected);
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
                Expression::Ident(IdentExpression { value, .. }) => {
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
                Expression::Int(IntExpression { value, .. }) => {
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
                Expression::String(StringExpression { value, .. }) => {
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
                    Expression::Boolean(BooleanExpression { value, .. }) => {
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
                Expression::Array(ArrayExpression { elements, .. }) => {
                    assert_eq!(elements.len(), 3);

                    if let Expression::Int(IntExpression { value, .. }) = &elements[0] {
                        assert_eq!(*value, 1);
                    } else {
                        panic!("expected integer expression with value 1 in array[0]");
                    }

                    if let Expression::Infix(InfixExpression { left, right, .. }) = &elements[1] {
                        if let Expression::Int(IntExpression { value, .. }) = &**left {
                            assert_eq!(*value, 2);
                        } else {
                            panic!("expected integer expression with value 2 in array[1]");
                        }

                        if let Expression::Int(IntExpression { value, .. }) = &**right {
                            assert_eq!(*value, 2);
                        } else {
                            panic!("expected integer expression with value 2 in array[1]");
                        }
                    } else {
                        panic!("expected infix expression with value 2 * 2 in array[1]");
                    }

                    if let Expression::Infix(InfixExpression { left, right, .. }) = &elements[2] {
                        if let Expression::Int(IntExpression { value, .. }) = &**left {
                            assert_eq!(*value, 3);
                        } else {
                            panic!("expected integer expression with value 3 in array[2]");
                        }

                        if let Expression::Int(IntExpression { value, .. }) = &**right {
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
                    Expression::Prefix(PrefixExpression {
                        operator, right, ..
                    }) => {
                        assert_eq!(operator.kind, *expected_operator);

                        match right.as_ref() {
                            Expression::Int(IntExpression { value, .. }) => {
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
                    Expression::Infix(InfixExpression {
                        left,
                        operator,
                        right,
                        ..
                    }) => {
                        match left.as_ref() {
                            Expression::Int(IntExpression { value, .. }) => {
                                assert_eq!(value, left_value);
                            }
                            _ => unreachable!(),
                        }

                        assert_eq!(operator.kind, *operator_kind);

                        match right.as_ref() {
                            Expression::Int(IntExpression { value, .. }) => {
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
                Expression::If(IfExpression {
                    condition,
                    consequence,
                    alternative,
                    ..
                }) => {
                    match condition.as_ref() {
                        Expression::Infix(InfixExpression { operator, .. }) => {
                            assert_eq!(operator.kind, TokenKind::LT);
                        }
                        _ => unreachable!(),
                    }

                    match &consequence.statements[0] {
                        Statement::Expression(ExpressionStatement { expression, .. }) => {
                            match expression {
                                Expression::Ident(IdentExpression { value, .. }) => {
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
                Expression::If(IfExpression {
                    condition,
                    consequence,
                    alternative,
                    ..
                }) => {
                    match condition.as_ref() {
                        Expression::Infix(InfixExpression { operator, .. }) => {
                            assert_eq!(operator.kind, TokenKind::LT);
                        }
                        _ => unreachable!(),
                    }

                    match &consequence.statements[0] {
                        Statement::Expression(ExpressionStatement { expression, .. }) => {
                            match expression {
                                Expression::Ident(IdentExpression { value, .. }) => {
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
                                    Expression::Ident(IdentExpression { value, .. }) => {
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
                    Expression::Function(FunctionExpression { parameters, .. }) => {
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
                Expression::Function(FunctionExpression { body, .. }) => {
                    assert_eq!(body.statements.len(), 1);

                    match &body.statements[0] {
                        Statement::Expression(ExpressionStatement { expression, .. }) => {
                            match expression {
                                Expression::Infix(InfixExpression { operator, .. }) => {
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
    fn test_hash_expressions() {
        let input = r#"{ "one": 1, "two": 2, "three": 3 };"#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        let expected = [("one", 1), ("two", 2), ("three", 3)];

        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::Hash(HashExpression { pairs, .. }) => {
                    assert_eq!(pairs.len(), expected.len());

                    for (i, (expected_key, expected_value)) in expected.iter().enumerate() {
                        let pair = &pairs[i];

                        match &*pair.0 {
                            Expression::String(StringExpression { value, .. }) => {
                                assert_eq!(&**value, *expected_key);
                            }
                            _ => unreachable!(),
                        }

                        match *pair.1 {
                            Expression::Int(IntExpression { value, .. }) => {
                                assert_eq!(value, *expected_value);
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }

        let input = "{};";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::Block(BlockExpression { statements, .. }) => {
                    assert_eq!(statements.len(), 0);
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }

        let input = r#"{ "one": (1 * 3), "two": (2 + 5) };"#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::Hash(HashExpression { pairs, .. }) => {
                    let expected = [
                        ("one", 1, TokenKind::ASTERISK, 3),
                        ("two", 2, TokenKind::PLUS, 5),
                    ];

                    assert_eq!(pairs.len(), expected.len());

                    for (
                        i,
                        (
                            expected_key,
                            expected_left_value,
                            expected_operator,
                            expected_right_value,
                        ),
                    ) in expected.iter().enumerate()
                    {
                        let pair = &pairs[i];

                        match &*pair.0 {
                            Expression::String(StringExpression { value, .. }) => {
                                assert_eq!(&**value, *expected_key);
                            }
                            _ => unreachable!(),
                        }

                        match &*pair.1 {
                            Expression::Infix(InfixExpression {
                                left,
                                right,
                                operator,
                            }) => {
                                match &**left {
                                    Expression::Int(IntExpression { value, .. }) => {
                                        assert_eq!(*value, *expected_left_value);
                                    }
                                    _ => unreachable!(),
                                }

                                assert_eq!(operator.kind, *expected_operator);

                                match &**right {
                                    Expression::Int(IntExpression { value, .. }) => {
                                        assert_eq!(*value, *expected_right_value);
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }

        let input = r#"{ "one": 1, "two": 2, "three": 3 }["one"];"#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        match statement {
            Statement::Expression(ExpressionStatement { expression, .. }) => match expression {
                Expression::Index(IndexExpression { left, .. }) => match &**left {
                    Expression::Hash(HashExpression { pairs, .. }) => {
                        assert_eq!(pairs.len(), 3);

                        assert_eq!(
                            pairs[0].0,
                            Box::new(Expression::String(StringExpression {
                                token: Token {
                                    kind: TokenKind::STRING,
                                    line: 1,
                                    column: 3,
                                    literal: "\"one\"".into(),
                                },
                                value: "one".into(),
                            }))
                        );
                        assert_eq!(
                            pairs[0].1,
                            Box::new(Expression::Int(IntExpression {
                                token: Token {
                                    kind: TokenKind::INT,
                                    line: 1,
                                    column: 10,
                                    literal: "1".into(),
                                },
                                value: 1,
                            }))
                        );
                    }
                    _ => unreachable!(),
                },
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
                Expression::Call(call) => {
                    match call.function.as_ref() {
                        Expression::Ident(IdentExpression { value, .. }) => {
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
                Expression::Call(call) => {
                    match call.function.as_ref() {
                        Expression::Ident(IdentExpression { value, .. }) => {
                            assert_eq!(&**value, "add");
                        }
                        _ => unreachable!(),
                    }

                    assert_eq!(call.arguments.len(), 3);

                    match &call.arguments[0] {
                        Expression::Int(IntExpression { value, .. }) => {
                            assert_eq!(*value, 1);
                        }
                        _ => unreachable!(),
                    }

                    match &call.arguments[1] {
                        Expression::Infix(InfixExpression { operator, .. }) => {
                            assert_eq!(operator.kind, TokenKind::ASTERISK);
                        }
                        _ => unreachable!(),
                    }

                    match &call.arguments[2] {
                        Expression::Infix(InfixExpression { operator, .. }) => {
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
                Expression::Call(call) => {
                    match call.function.as_ref() {
                        Expression::Function(FunctionExpression { parameters, .. }) => {
                            assert_eq!(parameters.len(), 2);
                        }
                        _ => unreachable!(),
                    }

                    assert_eq!(call.arguments.len(), 2);

                    let expected = [(TokenKind::INT, 2), (TokenKind::INT, 3)];

                    for (i, (expected_kind, expected_value)) in expected.iter().enumerate() {
                        match &call.arguments[i] {
                            Expression::Int(IntExpression { token, value }) => {
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
                Expression::Index(index) => {
                    match index.left.as_ref() {
                        Expression::Ident(IdentExpression { value, .. }) => {
                            assert_eq!(&**value, "array");
                        }
                        _ => unreachable!(),
                    }

                    match index.index.as_ref() {
                        Expression::Infix(InfixExpression { operator, .. }) => {
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

    #[test]
    fn test_array_of_hashes_expressions() {
        let input = r#"[ { "one": 1, "two": 2, "three": 3 }, { "one": 1, "two": 2, "three": 3 } ][0]["one"];"#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        let env = Rc::new(RefCell::new(Environment::new(None)));

        let result = Evaluator::execute(program, Rc::clone(&env)).unwrap();

        assert_eq!(result, DataType::INT(1));
    }
}
