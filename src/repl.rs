use crate::{Lexer, Token};
use std::io::{self, Write};

pub struct REPL;

impl REPL {
    pub fn start() {
        let mut input = String::new();

        loop {
            print!(">> ");
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut input).unwrap();

            let tokens: Vec<Token> = Lexer::new(input.trim()).collect();

            let illegal: Option<&Token> = tokens
                .iter()
                .find(|token| matches!(token, Token::ILLEGAL { .. }));

            match illegal {
                Some(token) => match token {
                    Token::ILLEGAL { span, literal } => {
                        println!("illegal {:?} at {}:{}", literal, span.line(), span.column());

                        input.clear();
                    }
                    _ => unreachable!("unreachable state"),
                },
                None => {
                    for token in tokens {
                        println!("{:?}", token);
                    }
                }
            }
        }
    }
}
