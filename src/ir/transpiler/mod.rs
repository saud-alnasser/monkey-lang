mod error;

use crate::ast::*;
use crate::ir::program::*;
use crate::runtime::datatype::DataType;

pub use error::{Error, Result};

/// a transpiler converts an AST into an IR program
pub struct Transpiler {
    program: Program,
    current_temp: usize,
    current_label: usize,
    current_block: Option<Block>,
}

impl Transpiler {
    pub fn new() -> Self {
        Self {
            program: Program::new(),
            current_temp: 0,
            current_label: 0,
            current_block: None,
        }
    }

    /// generate the next available temporary variable.
    fn generate_temp(&mut self) -> Temp {
        let temp = Temp(self.current_temp);
        self.current_temp += 1;
        temp
    }

    /// generate the next available label.
    fn generate_label(&mut self) -> Label {
        let label = Label(self.current_label);
        self.current_label += 1;
        label
    }

    /// start a new basic block with the given label.
    ///
    /// note: the previous block must have already been finalized with `end`.
    fn start(&mut self, label: Label) {
        debug_assert!(
            self.current_block.is_none(),
            "starting a new block while another is still open; call end() first",
        );

        self.current_block = Some(Block::new(label));
    }

    /// emit an instruction into the current block
    fn emit(&mut self, instruction: Instruction) {
        if let Some(block) = &mut self.current_block {
            block.add(instruction);
        }
    }

    /// set terminator for current block and add it to the program
    fn end(&mut self, terminator: Terminator) {
        if let Some(mut block) = self.current_block.take() {
            block.end(terminator);
            self.program.add(block);
        }
    }

    /// take the current block and return it
    fn take(&mut self) -> Option<Block> {
        self.current_block.take()
    }

    /// restore a block as the current block
    fn restore(&mut self, block: Option<Block>) {
        self.current_block = block;
    }

    pub fn transpile(mut self, statements: Vec<Statement>) -> Result<Program> {
        let entry = self.generate_label();

        self.start(entry);

        let mut result = Value::Constant(DataType::Undefined);

        for statement in statements {
            result = statement.transpile(&mut self)?;
        }

        self.end(Terminator::Exit(Exit { value: result }));

        Ok(self.program)
    }
}

/// generic trait for lowering AST nodes into IR using a shared `Transpiler` context.
pub trait Transpile<T> {
    /// convert an AST node into IR using the given `Transpiler` and produce a value of type `T`.
    fn transpile(&self, t: &mut Transpiler) -> Result<T>;
}

impl Transpile<Value> for Statement {
    fn transpile(&self, t: &mut Transpiler) -> Result<Value> {
        match self {
            Statement::Let {
                identifier,
                expression,
            } => {
                let value = expression.transpile(t)?;

                t.emit(Instruction::StoreVar(StoreVar {
                    name: identifier.value,
                    value,
                }));

                Ok(Value::Constant(DataType::Undefined))
            }

            Statement::Return(expression) => {
                let value = expression.transpile(t)?;
                t.end(Terminator::Return(Return { value: Some(value) }));

                let unreachable = t.generate_label();
                t.start(unreachable);

                Ok(Value::Constant(DataType::Undefined))
            }

            Statement::Expression(expression) => expression.transpile(t),
        }
    }
}

impl Transpile<Value> for Expression {
    fn transpile(&self, t: &mut Transpiler) -> Result<Value> {
        match self {
            Expression::Int(e) => {
                use crate::runtime::datatype::Integer;
                Ok(Value::Constant(DataType::Integer(Integer::new(e.value))))
            }

            Expression::String(e) => {
                use crate::runtime::datatype::String;
                Ok(Value::Constant(DataType::String(String::new(e.value))))
            }

            Expression::Boolean(e) => {
                use crate::runtime::datatype::Boolean;
                Ok(Value::Constant(DataType::Boolean(Boolean::new(e.value))))
            }

            Expression::Identifier(e) => {
                let dest = t.generate_temp();
                t.emit(Instruction::LoadVar(LoadVar {
                    dest: dest.clone(),
                    name: e.value,
                }));
                Ok(Value::Temp(dest))
            }

            Expression::Prefix(e) => e.transpile(t),
            Expression::Infix(e) => e.transpile(t),
            Expression::If(e) => e.transpile(t),
            Expression::Block(e) => e.transpile(t),
            Expression::Function(e) => e.transpile(t),
            Expression::Call(e) => e.transpile(t),
            Expression::Array(e) => e.transpile(t),
            Expression::Object(e) => e.transpile(t),
            Expression::Index(e) => e.transpile(t),
        }
    }
}

impl Transpile<Value> for PrefixExpression {
    fn transpile(&self, t: &mut Transpiler) -> Result<Value> {
        let operand = self.right.transpile(t)?;
        let dest = t.generate_temp();
        t.emit(Instruction::Unary(Unary {
            dest: dest.clone(),
            op: self.operator.clone(),
            operand,
        }));
        Ok(Value::Temp(dest))
    }
}

impl Transpile<Value> for InfixExpression {
    fn transpile(&self, t: &mut Transpiler) -> Result<Value> {
        let left = self.left.transpile(t)?;
        let right = self.right.transpile(t)?;
        let dest = t.generate_temp();
        t.emit(Instruction::Binary(Binary {
            dest: dest.clone(),
            op: self.operator.clone(),
            left,
            right,
        }));
        Ok(Value::Temp(dest))
    }
}

impl Transpile<Value> for IfExpression {
    fn transpile(&self, t: &mut Transpiler) -> Result<Value> {
        let condition = self.condition.transpile(t)?;

        let result = t.generate_temp();

        let then_label = t.generate_label();
        let else_label = t.generate_label();
        let merge_label = t.generate_label();

        t.end(Terminator::Branch(Branch {
            condition,
            true_label: then_label,
            false_label: else_label,
        }));

        // then block
        t.start(then_label);

        let then_value = self.consequence.transpile(t)?;

        t.emit(Instruction::Assign(Assign {
            dest: result.clone(),
            value: then_value,
        }));

        t.end(Terminator::Goto(Goto {
            target: merge_label,
        }));

        // else block
        t.start(else_label);

        let else_value = if let Some(alt) = &self.alternative {
            alt.transpile(t)?
        } else {
            Value::Constant(DataType::Undefined)
        };

        t.emit(Instruction::Assign(Assign {
            dest: result.clone(),
            value: else_value,
        }));

        t.end(Terminator::Goto(Goto {
            target: merge_label,
        }));

        // merge block
        t.start(merge_label);

        Ok(Value::Temp(result))
    }
}

impl Transpile<Value> for BlockExpression {
    fn transpile(&self, t: &mut Transpiler) -> Result<Value> {
        let mut value = Value::Constant(DataType::Undefined);

        for statement in &self.statements {
            value = statement.transpile(t)?;
        }

        Ok(value)
    }
}

impl Transpile<Value> for FunctionExpression {
    fn transpile(&self, t: &mut Transpiler) -> Result<Value> {
        let saved_block = t.take();

        let body = t.generate_label();
        t.start(body);
        let value = self.body.transpile(t)?;
        t.end(Terminator::Return(Return { value: Some(value) }));

        t.restore(saved_block);

        let dest = t.generate_temp();
        let params = self.parameters.iter().map(|p| p.value).collect();
        t.emit(Instruction::MakeFunction(MakeFunction {
            dest: dest.clone(),
            params,
            body,
        }));

        Ok(Value::Temp(dest))
    }
}

impl Transpile<Value> for CallExpression {
    fn transpile(&self, t: &mut Transpiler) -> Result<Value> {
        let callable = self.callable.transpile(t)?;
        let args = self
            .arguments
            .iter()
            .map(|arg| arg.transpile(t))
            .collect::<Result<Vec<_>>>()?;

        let dest = t.generate_temp();
        t.emit(Instruction::Call(Call {
            dest: dest.clone(),
            callable,
            args,
        }));

        Ok(Value::Temp(dest))
    }
}

impl Transpile<Value> for ArrayExpression {
    fn transpile(&self, t: &mut Transpiler) -> Result<Value> {
        let elements = self
            .elements
            .iter()
            .map(|elem| elem.transpile(t))
            .collect::<Result<Vec<_>>>()?;

        let dest = t.generate_temp();
        t.emit(Instruction::MakeArray(MakeArray {
            dest: dest.clone(),
            elements,
        }));

        Ok(Value::Temp(dest))
    }
}

impl Transpile<Value> for ObjectExpression {
    fn transpile(&self, t: &mut Transpiler) -> Result<Value> {
        let mut pairs = Vec::new();

        for (key, value) in &self.pairs {
            let k = key.transpile(t)?;
            let v = value.transpile(t)?;

            pairs.push((k, v));
        }

        let dest = t.generate_temp();
        t.emit(Instruction::MakeObject(MakeObject {
            dest: dest.clone(),
            pairs,
        }));

        Ok(Value::Temp(dest))
    }
}

impl Transpile<Value> for IndexExpression {
    fn transpile(&self, t: &mut Transpiler) -> Result<Value> {
        let indexable = self.left.transpile(t)?;
        let index = self.index.transpile(t)?;

        let dest = t.generate_temp();
        t.emit(Instruction::Index(Index {
            dest: dest.clone(),
            indexable,
            index,
        }));

        Ok(Value::Temp(dest))
    }
}

pub fn transpile(statements: Vec<Statement>) -> Result<Program> {
    Transpiler::new().transpile(statements)
}
