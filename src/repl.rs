use std::{
    error::Error,
    io::{self, Write},
};

use crate::{DataType, Evaluator, Lexer, Parser};

static MONKEY_FACE: &str = r#"            
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

pub struct REPL;

impl REPL {
    fn eval(input: &str) -> Result<DataType, Box<dyn Error>> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse()?;

        let mut output = DataType::NULL;

        for statement in program.statements {
            output = Evaluator::eval(statement)?;

            if let DataType::RETURN(_) = output {
                break;
            }
        }

        Ok(output)
    }

    pub fn run() {
        println!("{}", MONKEY_FACE);
        println!("Welcome to the Monkey programming language REPL!");
        println!("Feel free to type in commands");

        let mut input = String::new();

        loop {
            print!(">> ");
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut input).unwrap();

            match REPL::eval(&input) {
                Ok(data) => {
                    println!("{}", data);
                }
                Err(error) => {
                    eprintln!("{}", MONKEY_FACE);
                    eprintln!("Woops! We ran into some monkey business here!");
                    eprintln!("error: {}", error);
                }
            }

            input.clear();
        }
    }
}
