use crate::Lexer;
use std::io::{self, Write};

pub struct REPL;

impl REPL {
    pub fn start() {
        let mut input = String::new();

        loop {
            print!(">> ");
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut input).unwrap();

            let input = input.trim();
            let lexer = Lexer::new(&input);

            for token in lexer {
                println!("{:?}", token);
            }
        }
    }
}
