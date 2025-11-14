use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use crate::{
    ast::lexer,
    ast::parser,
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
                        Ok(new_program) => {
                            for (label, block) in new_program.blocks {
                                self.program.blocks.insert(label, block);
                            }
                            self.program.entry_label = new_program.entry_label;

                            match interpreter::execute(self.program.clone(), Rc::clone(&self.env)) {
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
