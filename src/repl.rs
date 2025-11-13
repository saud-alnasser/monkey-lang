use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use crate::{DataType, Environment, evaluator, lexer, parser};

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

            match lexer::parse(&code).into_result() {
                Ok(tokens) => match parser::parse(&tokens).into_result() {
                    Ok(statements) => match evaluator::execute(statements, Rc::clone(&self.env)) {
                        Ok(DataType::Undefined) => (),
                        Ok(data) => println!("{}", data),
                        Err(error) => eprintln!("error: {}", error),
                    },
                    Err(error) => {
                        eprintln!("error: {}", error[0]);
                    }
                },
                Err(error) => {
                    eprintln!("error: {}", error[0]);
                }
            }

            code.clear()
        }
    }
}
