use std::io::{self, Write};

use crate::{Lexer, Parser};

pub struct REPL;

impl REPL {
    pub fn start() {
        let mut input = String::new();

        loop {
            print!(">> ");
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut input).unwrap();

            let lexer = Lexer::new(input.as_str());
            let mut parser = Parser::new(lexer);

            match parser.parse() {
                Ok(program) => {
                    println!("{:#?}", program);
                }
                Err(e) => {
                    println!("{}", e);

                    input.clear();
                }
            }
        }
    }
}
