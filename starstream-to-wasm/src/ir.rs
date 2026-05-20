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
    /// Like Return, but names the BB that will be resumed.
    Yield(usize),
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

    pub fn instructions(&mut self, bb: usize) -> InstructionSink<'_> {
        InstructionSink::new(&mut self.blocks[bb].instructions)
    }

    pub fn seal(&mut self, bb: usize) {
        assert!(!self.blocks[bb].sealed);
        self.blocks[bb].sealed = true;
    }

    pub fn fill(&mut self, bb: usize, out: Out) {
        assert!(!matches!(out, Out::None));
        assert!(matches!(self.blocks[bb].out, Out::None));
        match out {
            Out::None | Out::Return | Out::Yield(_) => {}
            Out::Next(next) => {
                assert!(!self.blocks[next].sealed);
                self.blocks[next].ins.push(bb);
            }
            Out::If { f, t } => {
                assert!(!self.blocks[f].sealed);
                assert!(!self.blocks[t].sealed);
                self.blocks[f].ins.push(bb);
                self.blocks[t].ins.push(bb);
            }
        }
        self.blocks[bb].out = out;
    }

    pub fn assert_complete(&self) {
        for (i, block) in self.blocks.iter().enumerate() {
            assert!(block.sealed, "block {i} not sealed");
            assert!(block.is_filled(), "block {i} not filled");
        }
    }

    #[allow(dead_code)]
    pub fn to_graphviz(&self) -> String {
        use std::fmt::Write;
        let mut gv = String::new();
        _ = writeln!(gv, "digraph {{");
        _ = writeln!(gv, "start [shape=box] [style=rounded];");
        _ = writeln!(gv, "start -> 0;");
        for bb in self.resumes.iter() {
            _ = writeln!(gv, "resume_{bb} [shape=box] [style=rounded];");
            _ = writeln!(gv, "resume_{bb} -> {bb};");
        }
        for (i, block) in self.blocks.iter().enumerate() {
            _ = writeln!(
                gv,
                "{} [label=\"{}\"] [shape=box];",
                i,
                block.disassemble().replace("\n", "\\l")
            );
            match block.out {
                Out::None => {}
                Out::Return => {
                    _ = writeln!(gv, "{i} -> return_{i};");
                    _ = writeln!(gv, "return_{i} [label=return] [shape=box] [style=rounded];");
                }
                Out::Yield(bb) => {
                    _ = writeln!(gv, "{i} -> yield_{i};");
                    _ = writeln!(gv, "yield_{i} [label=yield] [shape=box] [style=rounded];");
                    _ = writeln!(gv, "yield_{i} -> resume_{bb} [style=dotted];");
                }
                Out::Next(bb) => {
                    _ = writeln!(gv, "{i} -> {bb};");
                }
                Out::If { f, t } => {
                    _ = writeln!(gv, "{i} -> {f} [color=red];");
                    _ = writeln!(gv, "{i} -> {t} [color=green];");
                }
            }
        }
        _ = writeln!(gv, "}}");
        gv
    }

    #[allow(dead_code)]
    pub fn to_mermaid(&self) -> String {
        use std::fmt::Write;
        let mut mm = String::new();
        _ = writeln!(mm, "flowchart TB");
        _ = writeln!(mm, "start([start])");
        _ = writeln!(mm, "start --> 0");
        for bb in self.resumes.iter() {
            _ = writeln!(mm, "resume_{bb}([resume_{bb}])");
            _ = writeln!(mm, "resume_{bb} --> {bb}");
        }
        for (i, block) in self.blocks.iter().enumerate() {
            let d = block.disassemble();
            _ = writeln!(mm, "{}[{:?}]", i, if d.is_empty() { " " } else { &d });
            _ = writeln!(mm, "style {i} text-align: left, white-space: nowrap");
            match block.out {
                Out::None => {}
                Out::Return => {
                    _ = writeln!(mm, "{i} --> return_{i}");
                    _ = writeln!(mm, "return_{i}([return])");
                }
                Out::Yield(bb) => {
                    _ = writeln!(mm, "{i} --> yield_{i}");
                    _ = writeln!(mm, "yield_{i}([yield])");
                    _ = writeln!(mm, "yield_{i} -.-> resume_{bb}");
                }
                Out::Next(bb) => {
                    _ = writeln!(mm, "{i} --> {bb}");
                }
                Out::If { f, t } => {
                    _ = writeln!(mm, "{i} -- false --> {f}");
                    _ = writeln!(mm, "{i} -- true --> {t}");
                }
            }
        }
        mm
    }
}

impl BasicBlock {
    /// True if this block's full contents and successors are known.
    pub fn is_filled(&self) -> bool {
        !matches!(self.out, Out::None)
    }

    fn disassemble(&self) -> String {
        use std::fmt::Write;
        let mut disassembly = String::new();
        let br = wasmparser::BinaryReader::new(&self.instructions, 0);
        let or = wasmparser::OperatorsReader::new(br);
        for result in or.into_iter() {
            let op = result.unwrap();
            _ = write!(disassembly, "{op:?}\n");
        }
        disassembly
    }
}
