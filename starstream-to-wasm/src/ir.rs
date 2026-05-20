//! Intermediate representation in control flow graph form.

use wasm_encoder::InstructionSink;

#[derive(Default)]
pub struct ControlFlowGraph {
    pub blocks: Vec<BasicBlock>,
    pub resumes: Vec<usize>,
}

#[derive(Default)]
pub struct BasicBlock {
    /// True if no further predecessors will be added.
    pub sealed: bool,
    /// Known predecessors.
    pub ins: Vec<usize>,
    pub instructions: Vec<u8>,
    /// Successors.
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

    pub fn seal(&mut self, block: usize) {
        assert!(!self.blocks[block].sealed);
        self.blocks[block].sealed = true;
    }

    pub fn fill(&mut self, block: usize, out: Out) {
        assert!(!matches!(out, Out::None));
        assert!(matches!(self.blocks[block].out, Out::None));
        match out {
            Out::None | Out::Return => {}
            Out::Next(next) => {
                assert!(!self.blocks[next].sealed);
                self.blocks[next].ins.push(block);
            }
            Out::If { f, t } => {
                assert!(!self.blocks[f].sealed);
                assert!(!self.blocks[t].sealed);
                self.blocks[f].ins.push(block);
                self.blocks[t].ins.push(block);
            }
        }
        self.blocks[block].out = out;
    }
}

impl BasicBlock {
    pub fn instructions(&mut self) -> InstructionSink<'_> {
        InstructionSink::new(&mut self.instructions)
    }

    /// True if this block's full contents and successors are known.
    pub fn is_filled(&self) -> bool {
        !matches!(self.out, Out::None)
    }
}
