use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::DataType;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, DataType>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    fn builtins() -> Environment {
        let mut env = Environment {
            store: HashMap::new(),
            outer: None,
        };

        env.set(
            "len",
            DataType::BUILTIN {
                func: |args| {
                    if args.len() != 1 {
                        return Err(format!(
                            r#"extra arguments are passed to BUILTIN("len"). got={}, want=1"#,
                            args.len()
                        )
                        .into());
                    }

                    match &args[0] {
                        DataType::STRING(s) => Ok(DataType::INT(s.len() as i64)),
                        _ => Err(format!(
                            r#"argument passed to BUILTIN("len") is not supported. got={:?}, want=STRING(any)"#,
                            args[0]
                        )
                        .into()),
                    }
                },
            },
        );

        env
    }

    pub fn new(outer: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: match outer {
                Some(outer) => Some(outer),
                None => Some(Rc::new(RefCell::new(Environment::builtins()))),
            },
        }
    }

    pub fn get(&self, key: &str) -> Option<DataType> {
        match self.store.get(key) {
            Some(value) => Some(value.clone()),
            None => match &self.outer {
                Some(outer) => outer.borrow().get(key),
                None => None,
            },
        }
    }

    pub fn set(&mut self, key: &str, value: DataType) {
        self.store.insert(key.to_string(), value);
    }
}
