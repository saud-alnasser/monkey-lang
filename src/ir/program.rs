use std::fmt::Display;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::runtime::datatype::DataType;
use internment::Intern;

/// a program is a list of basic blocks
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    blocks: Vec<Option<Block>>,
}

impl Program {
    pub fn new() -> Self {
        Self { blocks: Vec::new() }
    }

    /// add a basic block to the program at the given label
    pub fn add(&mut self, block: Block) {
        let index = block.label.0;

        if index >= self.blocks.len() {
            self.blocks.resize_with(index + 1, || None);
        }

        self.blocks[index] = Some(block);
    }

    /// get a basic block from the program at the given label
    pub fn get(&self, label: &Label) -> Option<&Block> {
        self.blocks.get(label.0).and_then(|b| b.as_ref())
    }

    /// get an iterator over all basic blocks in the program
    pub fn iter(&self) -> impl Iterator<Item = &Option<Block>> {
        self.blocks.iter()
    }

    /// consume the other program and append the blocks from it to the end of this program
    pub fn merge(&mut self, other: Program) -> Label {
        let offset = self.blocks.len();
        let entry = Label(offset);

        for maybe_block in other.blocks.into_iter() {
            if let Some(mut block) = maybe_block {
                block.offset_labels(offset);
                self.add(block);
            }
        }

        entry
    }
}

/// a basic block is a list of instructions and a terminator with a label
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
            terminator: Terminator::Exit(Exit {
                value: Value::Constant(DataType::Undefined),
            }),
        }
    }

    /// add an instruction to the basic block
    pub fn add(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    /// set the terminator for the basic block
    pub fn end(&mut self, terminator: Terminator) {
        self.terminator = terminator;
    }

    /// deep offset all labels in the block
    pub fn offset_labels(&mut self, offset: usize) {
        self.label = Label(self.label.0 + offset);

        match &mut self.terminator {
            Terminator::Goto(goto) => {
                goto.target = Label(goto.target.0 + offset);
            }
            Terminator::Branch(branch) => {
                branch.true_label = Label(branch.true_label.0 + offset);
                branch.false_label = Label(branch.false_label.0 + offset);
            }
            Terminator::Return(_) | Terminator::Exit(_) => {}
        }

        for instr in &mut self.instructions {
            if let Instruction::MakeFunction(func) = instr {
                func.body = Label(func.body.0 + offset);
            }
        }
    }
}

/// a label for a block of instructions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label(pub usize);

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "L{}", self.0)
    }
}

/// a temporary variable in a block of instructions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Temp(pub usize);

impl Display for Temp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%t{}", self.0)
    }
}

/// instructions are statements that perform an action.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Assign(Assign),
    Binary(Binary),
    Unary(Unary),
    StoreVar(StoreVar),
    LoadVar(LoadVar),
    MakeArray(MakeArray),
    MakeObject(MakeObject),
    MakeFunction(MakeFunction),
    Call(Call),
    Index(Index),
}

/// assignment: dest = value
#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub dest: Temp,
    pub value: Value,
}

/// binary operation: dest = left op right
#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub dest: Temp,
    pub op: BinaryOperator,
    pub left: Value,
    pub right: Value,
}

/// unary operation: dest = op operand
#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub dest: Temp,
    pub op: UnaryOperator,
    pub operand: Value,
}

/// variable store: var_name = value
#[derive(Debug, Clone, PartialEq)]
pub struct StoreVar {
    pub name: Intern<String>,
    pub value: Value,
}

/// variable load: dest = var_name
#[derive(Debug, Clone, PartialEq)]
pub struct LoadVar {
    pub dest: Temp,
    pub name: Intern<String>,
}

/// array creation: dest = [elements...]
#[derive(Debug, Clone, PartialEq)]
pub struct MakeArray {
    pub dest: Temp,
    pub elements: Vec<Value>,
}

/// object creation: dest = {key1: val1, key2: val2, ...}
#[derive(Debug, Clone, PartialEq)]
pub struct MakeObject {
    pub dest: Temp,
    pub pairs: Vec<(Value, Value)>,
}

/// function creation: dest = fn(params) { body_label }
#[derive(Debug, Clone, PartialEq)]
pub struct MakeFunction {
    pub dest: Temp,
    pub params: Vec<Intern<String>>,
    pub body: Label,
}

/// function call: dest = callable(args...)
#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub dest: Temp,
    pub callable: Value,
    pub args: Vec<Value>,
}

/// array/object indexing: dest = indexable[index]
#[derive(Debug, Clone, PartialEq)]
pub struct Index {
    pub dest: Temp,
    pub indexable: Value,
    pub index: Value,
}

/// terminators are instructions that terminate a block.
#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    Goto(Goto),
    Branch(Branch),
    Return(Return),
    Exit(Exit),
}

/// unconditional jump to a labeled block
#[derive(Debug, Clone, PartialEq)]
pub struct Goto {
    pub target: Label,
}

/// conditional branch: if condition then true_label else false_label
#[derive(Debug, Clone, PartialEq)]
pub struct Branch {
    pub condition: Value,
    pub true_label: Label,
    pub false_label: Label,
}

/// return from function/program
#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Option<Value>,
}

/// exit program (end of main)
#[derive(Debug, Clone, PartialEq)]
pub struct Exit {
    pub value: Value,
}

/// values in IR (operands)
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// constant literal
    Constant(DataType),

    /// temporary variable
    Temp(Temp),

    /// named variable
    Var(Intern<String>),
}
