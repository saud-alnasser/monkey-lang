use std::io::{self, Write};

use crate::{evaluator::DataType, Evaluator, Lexer, Parser};

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
                    let mut output = DataType::NULL;

                    for statement in program.statements {
                        output = Evaluator::eval(statement);

                        if let DataType::RETURN(_) = output {
                            break;
                        }
                    }

                    println!("{}", output);
                }
                Err(error) => {
                    eprintln!("{}", MONKEY_FACE);
                    eprintln!("Woops! We ran into some monkey business here!");
                    eprintln!(" parser error:");
                    eprintln!("\t{}", error);
                }
            }

            input.clear();
        }
    }
}
