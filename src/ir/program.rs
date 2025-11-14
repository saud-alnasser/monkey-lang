use std::collections::HashMap;
use std::fmt::Display;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::runtime::datatype::DataType;
use internment::Intern;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub blocks: HashMap<Label, Block>,
    pub entry_label: Label,
}

impl Program {
    pub fn new() -> Self {
        Self {
            blocks: HashMap::new(),
            entry_label: Label(0),
        }
    }

    pub fn add_block(&mut self, block: Block) {
        self.blocks.insert(block.label, block);
    }

    pub fn get_block(&self, label: &Label) -> Option<&Block> {
        self.blocks.get(label)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub label: Label,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

impl Block {
    pub fn new(label: Label) -> Self {
        Self {
            label,
            instructions: Vec::new(),
            terminator: Terminator::Exit {
                value: Value::Constant(DataType::Undefined),
            },
        }
    }

    pub fn add_instruction(&mut self, instr: Instruction) {
        self.instructions.push(instr);
    }

    pub fn set_terminator(&mut self, term: Terminator) {
        self.terminator = term;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label(pub usize);

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "L{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Temp(pub usize);

impl Display for Temp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%t{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    /// Assignment: dest = value
    Assign { dest: Temp, value: Value },

    /// Binary operation: dest = left op right
    Binary {
        dest: Temp,
        op: BinaryOperator,
        left: Value,
        right: Value,
    },

    /// Unary operation: dest = op operand
    Unary {
        dest: Temp,
        op: UnaryOperator,
        operand: Value,
    },

    /// Variable store: var_name = value
    StoreVar { name: Intern<String>, value: Value },

    /// Variable load: dest = var_name
    LoadVar { dest: Temp, name: Intern<String> },

    /// Array creation: dest = [elements...]
    MakeArray { dest: Temp, elements: Vec<Value> },

    /// Object creation: dest = {key1: val1, key2: val2, ...}
    MakeObject {
        dest: Temp,
        pairs: Vec<(Value, Value)>,
    },

    /// Function creation: dest = fn(params) { body_label }
    MakeFunction {
        dest: Temp,
        params: Vec<Intern<String>>,
        body_label: Label,
    },

    /// Function call: dest = callable(args...)
    Call {
        dest: Temp,
        callable: Value,
        args: Vec<Value>,
    },

    /// Array/Object indexing: dest = indexable[index]
    Index {
        dest: Temp,
        indexable: Value,
        index: Value,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    /// Unconditional jump
    Goto { target: Label },

    /// Conditional branch: if condition then true_label else false_label
    Branch {
        condition: Value,
        true_label: Label,
        false_label: Label,
    },

    /// Return from function/program
    Return { value: Option<Value> },

    /// Exit program (end of main)
    Exit { value: Value },
}

/// Values in IR (operands)
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Constant literal
    Constant(DataType),

    /// Temporary variable
    Temp(Temp),

    /// Named variable
    Var(Intern<String>),
}
