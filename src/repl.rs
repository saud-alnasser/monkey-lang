use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use crate::{evaluator, lexer, parser, DataType, Environment};

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

pub struct REPL {
    env: Rc<RefCell<Environment>>,
}

impl REPL {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new(None))),
        }
    }

    pub fn run(&mut self) {
        println!("{}", MONKEY_FACE);
        println!("Welcome to the Monkey programming language REPL!");
        println!("Feel free to type in commands");

        let mut code = String::new();

        loop {
            print!(">> ");
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut code).unwrap();

            match lexer::parse(&code) {
                Ok(tokens) => match parser::parse(tokens) {
                    Ok(program) => match evaluator::execute(program, Rc::clone(&self.env)) {
                        Ok(DataType::Undefined) => (),
                        Ok(data) => println!("{}", data),
                        Err(error) => eprintln!("error: {}", error),
                    },
                    Err(error) => {
                        eprintln!("error: {}", error);
                    }
                },
                Err(error) => {
                    eprintln!("error: {}", error);
                }
            };

            code.clear()
        }
    }
}
