//! Intermediate representation in control flow graph form.

use wasm_encoder::InstructionSink;

#[derive(Default)]
pub struct ControlFlowGraph {
    pub blocks: Vec<BasicBlock>,
    pub resumes: Vec<usize>,
}

#[derive(Default)]
pub struct BasicBlock {
    pub instructions: Vec<u8>,
    pub out: Out,
}

#[derive(Default)]
pub enum Out {
    /// Not set.
    #[default]
    None,
    /// Return from the current function with what's on the stack. 0 successors.
    Return,
    /// 1 unconditional successor.
    Next(usize),
    /// 2 successors, false case then true case.
    If { f: usize, t: usize },
}

impl ControlFlowGraph {
    pub fn add_block(&mut self) -> usize {
        let len = self.blocks.len();
        self.blocks.push(BasicBlock::default());
        len
    }

    pub fn instructions(&mut self, block: usize) -> InstructionSink<'_> {
        InstructionSink::new(&mut self.blocks[block].instructions)
    }

    pub fn set_out(&mut self, block: usize, out: Out) {
        assert!(!matches!(out, Out::None));
        assert!(matches!(self.blocks[block].out, Out::None));
        self.blocks[block].out = out;
    }
}

impl BasicBlock {
    pub fn instructions(&mut self) -> InstructionSink<'_> {
        InstructionSink::new(&mut self.instructions)
    }
}
