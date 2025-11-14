mod error;

use crate::ast::*;
use crate::ir::program::*;
use crate::runtime::datatype::DataType;

pub use error::{Error, Result};

pub struct Transpiler {
    program: Program,
    next_temp: usize,
    next_label: usize,
    current_block: Option<Block>,
}

impl Transpiler {
    pub fn new() -> Self {
        Self {
            program: Program::new(),
            next_temp: 0,
            next_label: 0,
            current_block: None,
        }
    }

    /// Generate a fresh temporary variable
    fn fresh_temp(&mut self) -> Temp {
        let temp = Temp(self.next_temp);
        self.next_temp += 1;
        temp
    }

    /// Generate a fresh label
    fn fresh_label(&mut self) -> Label {
        let label = Label(self.next_label);
        self.next_label += 1;
        label
    }

    /// Start a new basic block
    fn start_block(&mut self, label: Label) {
        self.finish_block();

        self.current_block = Some(Block::new(label));
    }

    /// Finish current block
    fn finish_block(&mut self) {
        if let Some(block) = self.current_block.take() {
            self.program.add_block(block);
        }
    }

    /// Add instruction to current block
    fn emit(&mut self, instr: Instruction) {
        if let Some(block) = &mut self.current_block {
            block.add_instruction(instr);
        }
    }

    /// Set terminator for current block
    fn terminate(&mut self, term: Terminator) {
        if let Some(block) = &mut self.current_block {
            block.set_terminator(term);
        }
    }

    pub fn transpile(mut self, statements: Vec<Statement>) -> Result<Program> {
        let entry = self.fresh_label();

        self.program.entry_label = entry;
        self.start_block(entry);

        let mut last_value = Value::Constant(DataType::Undefined);

        for statement in statements {
            last_value = self.transpile_statement(statement)?;
        }

        self.terminate(Terminator::Exit { value: last_value });
        self.finish_block();

        Ok(self.program)
    }

    /// Transpile a statement
    fn transpile_statement(&mut self, stmt: Statement) -> Result<Value> {
        match stmt {
            Statement::Let {
                identifier,
                expression,
            } => {
                let value = self.transpile_expression(expression)?;
                self.emit(Instruction::StoreVar {
                    name: identifier.value,
                    value,
                });
                Ok(Value::Constant(DataType::Undefined))
            }

            Statement::Return(expr) => {
                let value = self.transpile_expression(expr)?;
                self.terminate(Terminator::Return { value: Some(value) });

                // Create unreachable block for any following code
                let unreachable = self.fresh_label();
                self.finish_block();
                self.start_block(unreachable);

                Ok(Value::Constant(DataType::Undefined))
            }

            Statement::Expression(expr) => self.transpile_expression(expr),
        }
    }

    /// Transpile an expression (returns the Value representing the result)
    fn transpile_expression(&mut self, expr: Expression) -> Result<Value> {
        match expr {
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
                let dest = self.fresh_temp();
                self.emit(Instruction::LoadVar {
                    dest: dest.clone(),
                    name: e.value,
                });
                Ok(Value::Temp(dest))
            }

            Expression::Prefix(e) => {
                let operand = self.transpile_expression(*e.right)?;
                let dest = self.fresh_temp();
                self.emit(Instruction::Unary {
                    dest: dest.clone(),
                    op: e.operator,
                    operand,
                });
                Ok(Value::Temp(dest))
            }

            Expression::Infix(e) => {
                let left = self.transpile_expression(*e.left)?;
                let right = self.transpile_expression(*e.right)?;
                let dest = self.fresh_temp();
                self.emit(Instruction::Binary {
                    dest: dest.clone(),
                    op: e.operator,
                    left,
                    right,
                });
                Ok(Value::Temp(dest))
            }

            Expression::If(e) => self.transpile_if(e),

            Expression::Block(e) => self.transpile_block(e),

            Expression::Function(e) => self.transpile_function(e),

            Expression::Call(e) => self.transpile_call(e),

            Expression::Array(e) => self.transpile_array(e),

            Expression::Object(e) => self.transpile_object(e),

            Expression::Index(e) => self.transpile_index(e),
        }
    }

    /// Transpile if expression
    fn transpile_if(&mut self, expr: IfExpression) -> Result<Value> {
        // Evaluate condition
        let condition = self.transpile_expression(*expr.condition)?;

        // Create a result temporary that will hold the final value
        let result_temp = self.fresh_temp();

        // Create labels
        let then_label = self.fresh_label();
        let else_label = self.fresh_label();
        let merge_label = self.fresh_label();

        // Branch on condition
        self.terminate(Terminator::Branch {
            condition,
            true_label: then_label,
            false_label: else_label,
        });
        self.finish_block();

        // Then block
        self.start_block(then_label);
        let then_value = self.transpile_block(*expr.consequence)?;
        self.emit(Instruction::Assign {
            dest: result_temp.clone(),
            value: then_value,
        });
        self.terminate(Terminator::Goto {
            target: merge_label,
        });
        self.finish_block();

        // Else block
        self.start_block(else_label);
        let else_value = if let Some(alt) = expr.alternative {
            self.transpile_block(*alt)?
        } else {
            Value::Constant(DataType::Undefined)
        };
        self.emit(Instruction::Assign {
            dest: result_temp.clone(),
            value: else_value,
        });
        self.terminate(Terminator::Goto {
            target: merge_label,
        });
        self.finish_block();

        // Merge block
        self.start_block(merge_label);

        // Return the result temporary that was assigned in both branches
        Ok(Value::Temp(result_temp))
    }

    /// Transpile block expression
    fn transpile_block(&mut self, expr: BlockExpression) -> Result<Value> {
        let mut last_value = Value::Constant(DataType::Undefined);

        for stmt in expr.statements {
            last_value = self.transpile_statement(stmt)?;
        }

        Ok(last_value)
    }

    /// Transpile function expression
    fn transpile_function(&mut self, expr: FunctionExpression) -> Result<Value> {
        // Save current state
        let saved_block = self.current_block.take();

        // Create function body label
        let body_label = self.fresh_label();

        // Transpile function body in new context
        self.start_block(body_label);
        let body_value = self.transpile_block(*expr.body)?;
        self.terminate(Terminator::Return {
            value: Some(body_value),
        });
        self.finish_block();

        // Restore state
        self.current_block = saved_block;

        // Create function object
        let dest = self.fresh_temp();
        let params = expr.parameters.into_iter().map(|p| p.value).collect();
        self.emit(Instruction::MakeFunction {
            dest: dest.clone(),
            params,
            body_label,
        });

        Ok(Value::Temp(dest))
    }

    /// Transpile function call
    fn transpile_call(&mut self, expr: CallExpression) -> Result<Value> {
        let callable = self.transpile_expression(*expr.callable)?;
        let args = expr
            .arguments
            .into_iter()
            .map(|arg| self.transpile_expression(arg))
            .collect::<Result<Vec<_>>>()?;

        let dest = self.fresh_temp();
        self.emit(Instruction::Call {
            dest: dest.clone(),
            callable,
            args,
        });

        Ok(Value::Temp(dest))
    }

    /// Transpile array expression
    fn transpile_array(&mut self, expr: ArrayExpression) -> Result<Value> {
        let elements = expr
            .elements
            .into_iter()
            .map(|elem| self.transpile_expression(elem))
            .collect::<Result<Vec<_>>>()?;

        let dest = self.fresh_temp();
        self.emit(Instruction::MakeArray {
            dest: dest.clone(),
            elements,
        });

        Ok(Value::Temp(dest))
    }

    /// Transpile object expression
    fn transpile_object(&mut self, expr: ObjectExpression) -> Result<Value> {
        let mut pairs = Vec::new();
        for (key, value) in expr.pairs {
            let k = self.transpile_expression(key)?;
            let v = self.transpile_expression(value)?;
            pairs.push((k, v));
        }

        let dest = self.fresh_temp();
        self.emit(Instruction::MakeObject {
            dest: dest.clone(),
            pairs,
        });

        Ok(Value::Temp(dest))
    }

    /// Transpile index expression
    fn transpile_index(&mut self, expr: IndexExpression) -> Result<Value> {
        let indexable = self.transpile_expression(*expr.left)?;
        let index = self.transpile_expression(*expr.index)?;

        let dest = self.fresh_temp();
        self.emit(Instruction::Index {
            dest: dest.clone(),
            indexable,
            index,
        });

        Ok(Value::Temp(dest))
    }
}

pub fn transpile(statements: Vec<Statement>) -> Result<Program> {
    let transpiler = Transpiler::new();
    transpiler.transpile(statements)
}
