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
            quantifiers: vec![Box::new(quantifiers::LetStatementQuantifier)],
            lexer: lexer.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Program, Box<dyn Error>> {
        let mut program = Program::new();

        while let Some(token) = self.lexer.peek() {
            match token {
                Token {
                    kind: TokenKind::EOF,
                    ..
                } => break,
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
            }
        }
    }
}
