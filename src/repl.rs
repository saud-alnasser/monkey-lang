use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use crate::{
    ast::{lexer, parser},
    ir::{interpreter, program::Program, transpiler},
    runtime::{DataType, Environment},
};

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
    program: Program,
}

impl REPL {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new(None))),
            program: Program::new(),
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
                    Ok(statements) => match transpiler::transpile(statements) {
                        Ok(other) => {
                            let entry = self.program.merge(other);

                            match interpreter::execute_from(
                                self.program.clone(),
                                self.env.clone(),
                                entry,
                            ) {
                                Ok(DataType::Undefined) => (),
                                Ok(data) => println!("{}", data),
                                Err(error) => eprintln!("runtime error: {}", error),
                            }
                        }
                        Err(error) => eprintln!("transpilation error: {}", error),
                    },
                    Err(error) => {
                        eprintln!("parse error: {}", error[0]);
                    }
                },
                Err(error) => {
                    eprintln!("lexer error: {}", error[0]);
                }
            }

            code.clear()
        }
    }
}
