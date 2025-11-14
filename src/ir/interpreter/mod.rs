mod error;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::ir::{self, program::*};
use crate::runtime::datatype::*;
use crate::runtime::environment::Environment;

pub use error::{Error, Result};

pub struct Interpreter {
    program: Program,
    env: Rc<RefCell<Environment>>,
    temps: HashMap<Temp, DataType>,
}

impl Interpreter {
    pub fn new(program: Program, env: Rc<RefCell<Environment>>) -> Self {
        Self {
            program,
            env,
            temps: HashMap::new(),
        }
    }

    /// Execute the IR program
    pub fn execute(mut self) -> Result<DataType> {
        let entry = self.program.entry_label.clone();
        self.execute_block(&entry)
    }

    /// Execute a basic block
    fn execute_block(&mut self, label: &Label) -> Result<DataType> {
        let block = self
            .program
            .get_block(label)
            .ok_or_else(|| Error::InvalidLabel(label.clone()))?
            .clone();

        // Execute all instruction s
        for instr in block.instructions {
            self.execute_instruction(instr)?;
        }

        // Execute terminator
        self.execute_terminator(block.terminator)
    }

    /// Execute a single instruction
    fn execute_instruction(&mut self, instr: Instruction) -> Result<()> {
        match instr {
            Instruction::Assign { dest, value } => {
                let val = self.eval_value(value)?;
                self.temps.insert(dest, val);
            }

            Instruction::Binary {
                dest,
                op,
                left,
                right,
            } => {
                let l = self.eval_value(left)?;
                let r = self.eval_value(right)?;
                let result = self.eval_binary(op, l, r)?;
                self.temps.insert(dest, result);
            }

            Instruction::Unary { dest, op, operand } => {
                let val = self.eval_value(operand)?;
                let result = self.eval_unary(op, val)?;
                self.temps.insert(dest, result);
            }

            Instruction::StoreVar { name, value } => {
                let val = self.eval_value(value)?;
                self.env.borrow_mut().set(&name, val);
            }

            Instruction::LoadVar { dest, name } => {
                let val = self
                    .env
                    .borrow()
                    .get(&name)
                    .ok_or_else(|| Error::UndefinedVariable(name))?;
                self.temps.insert(dest, val);
            }

            Instruction::MakeArray { dest, elements } => {
                self.temps.insert(
                    dest,
                    DataType::Array(Array::new(
                        elements
                            .into_iter()
                            .map(|e| self.eval_value(e))
                            .collect::<Result<Vec<_>>>()?,
                    )),
                );
            }

            Instruction::MakeObject { dest, pairs } => {
                let mut map = HashMap::new();
                for (k, v) in pairs {
                    let key_val = self.eval_value(k)?;
                    let value = self.eval_value(v)?;

                    // Convert DataType to ObjectKey
                    let key = match key_val {
                        DataType::String(s) => ObjectKey::String(s),
                        DataType::Integer(i) => ObjectKey::Integer(i),
                        DataType::Boolean(b) => ObjectKey::Boolean(b),
                        _ => return Err(Error::IndexTypeMismatch(key_val.name_of().to_string())),
                    };

                    map.insert(key, value);
                }
                self.temps.insert(dest, DataType::Object(Object::new(map)));
            }

            Instruction::MakeFunction {
                dest,
                params,
                body_label,
            } => {
                use crate::ir::Function;

                self.temps.insert(
                    dest,
                    DataType::Opaque(Rc::new(Function::new(
                        params,
                        body_label,
                        Rc::clone(&self.env),
                    ))),
                );
            }

            Instruction::Call {
                dest,
                callable,
                args,
            } => {
                let func = self.eval_value(callable)?;
                let arguments = args
                    .into_iter()
                    .map(|a| self.eval_value(a))
                    .collect::<Result<Vec<_>>>()?;

                let result = self.call_function(func, arguments)?;
                self.temps.insert(dest, result);
            }

            Instruction::Index {
                dest,
                indexable,
                index,
            } => {
                let idx_val = self.eval_value(indexable)?;
                let idx = self.eval_value(index)?;
                let result = self.eval_index(idx_val, idx)?;
                self.temps.insert(dest, result);
            }
        }

        Ok(())
    }

    /// Execute a terminator
    fn execute_terminator(&mut self, term: Terminator) -> Result<DataType> {
        match term {
            Terminator::Goto { target } => self.execute_block(&target),

            Terminator::Branch {
                condition,
                true_label,
                false_label,
            } => {
                let cond = self.eval_value(condition)?;
                let is_true = match cond {
                    DataType::Boolean(b) => *b,
                    DataType::Integer(i) => *i != 0,
                    _ => false,
                };

                let target = if is_true { true_label } else { false_label };
                self.execute_block(&target)
            }

            Terminator::Return { value } => match value {
                Some(v) => Ok(self.eval_value(v)?),
                None => Ok(DataType::Undefined),
            },

            Terminator::Exit { value } => Ok(self.eval_value(value)?),
        }
    }

    /// Evaluate a Value to a DataType
    fn eval_value(&self, value: Value) -> Result<DataType> {
        match value {
            Value::Constant(c) => Ok(c),
            Value::Temp(t) => self
                .temps
                .get(&t)
                .cloned()
                .ok_or_else(|| Error::UndefinedTemp(t)),
            Value::Var(name) => self
                .env
                .borrow()
                .get(&name)
                .ok_or_else(|| Error::UndefinedVariable(name)),
        }
    }

    /// Evaluate binary operation
    fn eval_binary(&self, op: BinaryOperator, left: DataType, right: DataType) -> Result<DataType> {
        match (&left, &right) {
            (DataType::Integer(l), DataType::Integer(r)) => {
                let l = **l;
                let r = **r;
                Ok(match op {
                    BinaryOperator::Add => DataType::Integer(Integer::new(l + r)),
                    BinaryOperator::Subtract => DataType::Integer(Integer::new(l - r)),
                    BinaryOperator::Multiply => DataType::Integer(Integer::new(l * r)),
                    BinaryOperator::Divide => DataType::Integer(Integer::new(l / r)),
                    BinaryOperator::LessThan => DataType::Boolean(Boolean::new(l < r)),
                    BinaryOperator::GreaterThan => DataType::Boolean(Boolean::new(l > r)),
                    BinaryOperator::LessThanOrEqual => DataType::Boolean(Boolean::new(l <= r)),
                    BinaryOperator::GreaterThanOrEqual => DataType::Boolean(Boolean::new(l >= r)),
                    BinaryOperator::Equal => DataType::Boolean(Boolean::new(l == r)),
                    BinaryOperator::NotEqual => DataType::Boolean(Boolean::new(l != r)),
                })
            }
            (DataType::Boolean(l), DataType::Boolean(r)) => {
                let l = **l;
                let r = **r;
                Ok(match op {
                    BinaryOperator::Equal => DataType::Boolean(Boolean::new(l == r)),
                    BinaryOperator::NotEqual => DataType::Boolean(Boolean::new(l != r)),
                    _ => {
                        return Err(Error::TypeMismatch(
                            op,
                            left.name_of().to_string(),
                            right.name_of().to_string(),
                        ));
                    }
                })
            }
            (DataType::String(l), DataType::String(r)) => {
                if op == BinaryOperator::Add {
                    Ok(DataType::String(String::new(format!("{}{}", l, r).into())))
                } else {
                    Err(Error::TypeMismatch(
                        op,
                        left.name_of().to_string(),
                        right.name_of().to_string(),
                    ))
                }
            }
            _ => Err(Error::TypeMismatch(
                op,
                left.name_of().to_string(),
                right.name_of().to_string(),
            )),
        }
    }

    /// Evaluate unary operation
    fn eval_unary(&self, op: UnaryOperator, operand: DataType) -> Result<DataType> {
        match &operand {
            DataType::Integer(i) => Ok(match op {
                UnaryOperator::Negate => DataType::Integer(Integer::new(-**i)),
                UnaryOperator::Not => DataType::Boolean(Boolean::new(**i == 0)),
            }),
            DataType::Boolean(b) => Ok(match op {
                UnaryOperator::Not => DataType::Boolean(Boolean::new(!**b)),
                _ => return Err(Error::UnaryTypeMismatch(op, operand.name_of().to_string())),
            }),
            _ => Err(Error::UnaryTypeMismatch(op, operand.name_of().to_string())),
        }
    }

    /// Evaluate indexing
    fn eval_index(&self, indexable: DataType, index: DataType) -> Result<DataType> {
        match indexable {
            DataType::Array(arr) => {
                if let DataType::Integer(i) = index {
                    let idx = *i as usize;
                    Ok(arr.get(idx).cloned().unwrap_or(DataType::Null))
                } else {
                    Err(Error::IndexTypeMismatch(index.name_of().to_string()))
                }
            }
            DataType::Object(obj) => {
                // Convert DataType to ObjectKey
                let key = match index {
                    DataType::String(s) => ObjectKey::String(s),
                    DataType::Integer(i) => ObjectKey::Integer(i),
                    DataType::Boolean(b) => ObjectKey::Boolean(b),
                    _ => return Err(Error::IndexTypeMismatch(index.name_of().to_string())),
                };
                Ok(obj.get(&key).cloned().unwrap_or(DataType::Null))
            }
            _ => Err(Error::NotIndexable(indexable.name_of().to_string())),
        }
    }

    /// Call a function
    fn call_function(&mut self, func: DataType, args: Vec<DataType>) -> Result<DataType> {
        match func {
            DataType::Function(Function(f)) => f(args).map_err(Error::BuiltinError),

            DataType::Opaque(opaque) => {
                if let Some(func) = opaque.downcast_ref::<ir::Function>() {
                    let scoped_env =
                        Rc::new(RefCell::new(Environment::new(Some(Rc::clone(&func.env)))));

                    for (param, arg) in func.parameters.iter().zip(args.iter()) {
                        scoped_env.borrow_mut().set(param, arg.clone());
                    }

                    let saved_env = Rc::clone(&self.env);
                    let saved_temps = self.temps.clone();

                    self.env = scoped_env;
                    self.temps.clear();

                    let result = self.execute_block(&func.body_label);

                    self.env = saved_env;
                    self.temps = saved_temps;

                    result
                } else {
                    Err(Error::NotCallable("OPAQUE".to_string()))
                }
            }

            _ => Err(Error::NotCallable(func.name_of().to_string())),
        }
    }
}

pub fn execute(program: Program, env: Rc<RefCell<Environment>>) -> Result<DataType> {
    let interpreter = Interpreter::new(program, env);
    interpreter.execute()
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::{ast::lexer, ast::parser, ir::transpiler};

    #[test]
    fn test_eval_integer_expressions() {
        let tests = vec![
            ("5;", 5),
            ("10;", 10),
            ("-5;", -5),
            ("-10;", -10),
            ("5 + 5 + 5 + 5 - 10;", 10),
            ("2 * 2 * 2 * 2 * 2;", 32),
            ("-50 + 100 + -50;", 0),
            ("5 * 2 + 10;", 20),
            ("5 + 2 * 10;", 25),
            ("20 + 2 * -10;", 0),
            ("50 / 2 * 2 + 10;", 60),
            ("2 * (5 + 10);", 30),
            ("3 * 3 * 3 + 10;", 37),
            ("3 * (3 * 3) + 10;", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10;", 50),
        ];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let statements = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(statements).unwrap(), env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_string_expressions() {
        let tests = vec![("\"hello world\";", "hello world")];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::String(value)) => assert_eq!(&value[..], expected),
                _ => panic!("expected a string, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_string_concatenation() {
        let tests = vec![(r#""hello" + ", " + "world!";"#, "hello, world!")];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::String(value)) => assert_eq!(&value[..], expected),
                _ => panic!("expected a string, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_boolean_expressions() {
        let tests = vec![
            ("true;", true),
            ("false;", false),
            ("!true;", false),
            ("!false;", true),
            ("!!true;", true),
            ("!!false;", false),
            ("!5;", false),
            ("!!5;", true),
            ("!-5;", false),
            ("!!-5;", true),
            ("1 < 2;", true),
            ("1 > 2;", false),
            ("1 < 1;", false),
            ("1 > 1;", false),
            ("1 <= 2;", true),
            ("1 >= 2;", false),
            ("1 <= 1;", true),
            ("1 >= 1;", true),
            ("1 == 1;", true),
            ("1 != 1;", false),
            ("1 == 2;", false),
            ("1 != 2;", true),
            ("true == true;", true),
            ("false == false;", true),
            ("true == false;", false),
            ("true != false;", true),
            ("false != true;", true),
            ("(1 < 2) == true;", true),
            ("(1 < 2) == false;", false),
            ("(1 > 2) == true;", false),
            ("(1 > 2) == false;", true),
            ("1 >= 2 == true;", false),
            ("1 >= 2 == false;", true),
            ("1 <= 1 == true;", true),
            ("1 <= 1 == false;", false),
        ];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Boolean(value)) => assert_eq!(*value, expected),
                _ => panic!("expected a boolean, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_array_expressions() {
        let tests = vec![
            ("[];", vec![]),
            ("[1, 2, 3];", vec![1, 2, 3]),
            ("[1 + 2, 3 * 4, 5 - 6];", vec![3, 12, -1]),
        ];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Array(value)) => {
                    assert_eq!(
                        value
                            .iter()
                            .map(|element| match element {
                                DataType::Integer(value) => **value,
                                _ => panic!("expected an integer, got something else"),
                            })
                            .collect::<Vec<i64>>(),
                        expected
                    );
                }
                _ => panic!("expected an array, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0];", 1),
            ("[1, 2, 3][1];", 2),
            ("[1, 2, 3][2];", 3),
            ("let i = 0; [1][i];", 1),
            ("[1, 2, 3][1 + 1];", 3),
            ("let array = [1, 2, 3]; array[2];", 3),
            ("let array = [1, 2, 3]; array[0] + array[1] + array[2];", 6),
            ("let array = [1, 2, 3]; let i = array[0]; array[i];", 2),
            ("[1, 2, 3][3];", 0),
            ("[1, 2, 3][-1];", 0),
        ];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                Ok(DataType::Null) => assert_eq!(0, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10; };", 10),
            ("if (false) { 10; };", 0),
            ("if (1) { 10; };", 10),
            ("if (1 < 2) { 10; };", 10),
            ("if (1 > 2) { 10; };", 0),
            ("if (1 > 2) { 10; } else { 20; };", 20),
            ("if (1 < 2) { 10; } else { 20; };", 10),
        ];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                Ok(DataType::Undefined) => assert_eq!(expected, 0),
                _ => panic!("expected an integer, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    };

                    return 1;
                };",
                10,
            ),
        ];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                _ => panic!("expected a return statement, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_function_expressions() {
        let input = "fn(x) { x + 2; };";

        let tokens = lexer::parse(input).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let env = Rc::new(RefCell::new(Environment::new(None)));

        match execute(transpiler::transpile(program).unwrap(), env) {
            Ok(DataType::Opaque(opaque)) => {
                use crate::ir::Function;
                let ir_func = opaque.downcast_ref::<Function>().unwrap();

                assert_eq!(ir_func.parameters.len(), 1);
                assert_eq!(&**ir_func.parameters.first().unwrap(), "x");
                assert_eq!(ir_func.body_label.0, 1);
            }
            _ => panic!("expected a function, got something else"),
        }
    }

    #[test]
    fn test_eval_object_expressions() {
        let inputs = vec![
            (
                r#"{ "one": 1, "two": 2, "three": 3 }["one"];"#,
                DataType::Integer(Integer::new(1)),
            ),
            (
                r#"{ "one": 1, "two": 2, "three": 3 }["two"];"#,
                DataType::Integer(Integer::new(2)),
            ),
            (
                r#"{ "one": 1, "two": 2, "three": 3 }["three"];"#,
                DataType::Integer(Integer::new(3)),
            ),
            (
                r#"{ "one": 1, "two": 2, "three": 3 }["four"];"#,
                DataType::Null,
            ),
        ];

        for (input, expected) in inputs {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            let result = execute(transpiler::transpile(program).unwrap(), Rc::clone(&env)).unwrap();

            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_eval_function_calls() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5);", 5),
        ];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_closures() {
        let input =
            "let new_adder = fn(x) { fn(y) { x + y; }; }; let add_two = new_adder(2); add_two(2);";

        let tokens = lexer::parse(input).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let env = Rc::new(RefCell::new(Environment::new(None)));

        match execute(transpiler::transpile(program).unwrap(), env) {
            Ok(DataType::Integer(value)) => assert_eq!(*value, 4),
            _ => panic!("expected an integer, got something else"),
        }
    }

    #[test]
    fn test_eval_builtin_len_function() {
        let tests = vec![(r#"len("");"#, 0), (r#"len("hello, world!");"#, 13)];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }

        let test_errors = vec![
            (
                "len(1);",
                r#"builtin error: argument[0] passed to BUILTIN("len") is not supported. got=1, want=STRING|ARRAY"#,
            ),
            (
                r#"len("one", "two");"#,
                r#"builtin error: extra arguments are passed to BUILTIN("len"). got=2, want=1"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Err(error) => assert_eq!(error.to_string(), expected),
                _ => panic!("expected an error, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_builtin_first_function() {
        let tests = vec![
            ("first([2, 2, 3]);", 2),
            ("first([]);", 0),
            ("first([3]);", 3),
            ("first([5, 2]);", 5),
            ("first([1, 2, 3, 4]);", 1),
        ];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                Ok(DataType::Null) => assert_eq!(0, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }

        let test_errors = vec![
            (
                "first(1);",
                r#"builtin error: argument[0] passed to BUILTIN("first") is not supported. got=1, want=ARRAY"#,
            ),
            (
                r#"first([1], [2]);"#,
                r#"builtin error: extra arguments are passed to BUILTIN("first"). got=2, want=1"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Err(error) => assert_eq!(error.to_string(), expected),
                _ => panic!("expected an error, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_builtin_last_function() {
        let tests = vec![
            ("last([2, 2, 3]);", 3),
            ("last([]);", 0),
            ("last([3]);", 3),
            ("last([5, 2]);", 2),
            ("last([1, 2, 3, 4]);", 4),
        ];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                Ok(DataType::Null) => assert_eq!(0, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }

        let test_errors = vec![
            (
                "last(1);",
                r#"builtin error: argument[0] passed to BUILTIN("last") is not supported. got=1, want=ARRAY"#,
            ),
            (
                r#"last([1], [2]);"#,
                r#"builtin error: extra arguments are passed to BUILTIN("last"). got=2, want=1"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Err(error) => assert_eq!(error.to_string(), expected),
                _ => panic!("expected an error, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_builtin_rest_function() {
        let tests = vec![
            ("rest([2, 2, 3]);", vec![2, 3]),
            ("rest([]);", vec![]),
            ("rest([3]);", vec![]),
            ("rest([5, 2]);", vec![2]),
            ("rest([1, 2, 3, 4]);", vec![2, 3, 4]),
        ];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Array(value)) => {
                    assert_eq!(
                        value
                            .iter()
                            .map(|element| match element {
                                DataType::Integer(value) => **value,
                                _ => panic!("expected an integer, got something else"),
                            })
                            .collect::<Vec<i64>>(),
                        expected
                    );
                }
                Ok(DataType::Null) => assert_eq!(0, expected.len()),
                _ => panic!("expected an array, got something else"),
            }
        }

        let test_errors = vec![
            (
                "rest(1);",
                r#"builtin error: argument[0] passed to BUILTIN("rest") is not supported. got=1, want=ARRAY"#,
            ),
            (
                r#"rest([1], [2]);"#,
                r#"builtin error: extra arguments are passed to BUILTIN("rest"). got=2, want=1"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Err(error) => assert_eq!(error.to_string(), expected),
                _ => panic!("expected an error, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_builtins_push_function() {
        let tests = vec![
            ("push([], 1);", vec![1]),
            ("push([1], 2);", vec![1, 2]),
            ("push([1, 2], 3);", vec![1, 2, 3]),
            ("push([1, 2, 3], 4);", vec![1, 2, 3, 4]),
        ];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Array(value)) => {
                    assert_eq!(
                        value
                            .iter()
                            .map(|element| match element {
                                DataType::Integer(value) => **value,
                                _ => panic!("expected an integer, got something else"),
                            })
                            .collect::<Vec<i64>>(),
                        expected
                    );
                }
                _ => panic!("expected an array, got something else"),
            }
        }

        let test_errors = vec![
            (
                "push(1, 1);",
                r#"builtin error: argument[0] passed to BUILTIN("push") is not supported. got=1, want=ARRAY"#,
            ),
            (
                "push([1]);",
                r#"builtin error: extra arguments are passed to BUILTIN("push"). got=1, want=2"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Err(error) => assert_eq!(error.to_string(), expected),
                _ => panic!("expected an error, got something else"),
            }
        }
    }
}
