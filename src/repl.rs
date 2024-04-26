use std::{
    cell::RefCell,
    error::Error,
    io::{self, Write},
    rc::Rc,
};

use crate::{DataType, Environment, Evaluator, Lexer, Parser};

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
    fn eval(code: &str, env: Rc<RefCell<Environment>>) -> Result<DataType, Box<dyn Error>> {
        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let program = parser.parse()?;

        let mut output = DataType::NULL;

        for statement in program.statements {
            output = Evaluator::eval(statement, Rc::clone(&env))?;

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

        let mut code = String::new();
        let env = Rc::new(RefCell::new(Environment::new(None)));

        loop {
            print!(">> ");
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut code).unwrap();

            match REPL::eval(&code, Rc::clone(&env)) {
                Ok(data) => match data {
                    DataType::NULL => (),
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
