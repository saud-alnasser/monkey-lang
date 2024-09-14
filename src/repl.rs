use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use crate::{
    error::{Error, Result},
    DataType, Environment, Evaluator, Lexer, Parser,
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

pub struct REPL;

impl REPL {
    fn execute(code: &str, env: Rc<RefCell<Environment>>) -> Result<DataType> {
        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().map_err(|error| Error::Parser(error))?;

        Evaluator::execute(program, env).map_err(|error| Error::Evaluator(error))
    }

    pub fn run() {
        println!("{}", MONKEY_FACE);
        println!("Welcome to the Monkey programming language REPL!");
        println!("Feel free to type in commands");

        let mut code = String::new();
        let env = Rc::new(RefCell::new(Environment::new(None)));

        loop {
            print!(">> ");
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut code).unwrap();

            match REPL::execute(&code, Rc::clone(&env)) {
                Ok(data) => match data {
                    DataType::UNDEFINED => (),
                    _ => println!("{}", data),
                },
                Err(error) => {
                    eprintln!("{}", MONKEY_FACE);
                    eprintln!("Woops! We ran into some monkey business here!");
                    eprintln!("error: {}", error);
                }
            }

            code.clear();
        }
    }
}
