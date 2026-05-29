//! Intermediate representation in control flow graph form.

// Definitions of "sealed" and "filled" are based on those in:
//     Simple and Efficient Construction of Static Single Assignment Form.
//     Matthias Braun, Sebastian Buchwald, Sebastian Hack, Roland Leißa, Christoph Mallon, and Andreas Zwinkau.
// Sealed: No further predecessors will be added.
// Filled: In Braun, means local value numbering is finished and successors may be added.
// We have a single "fill" point that both finishes adding instructions, "fills", and immediately adds all successors.

use std::fmt;

use wasm_encoder::{BlockType, InstructionSink};

use crate::DisplayClosure;

#[derive(Default)]
pub struct ControlFlowGraph {
    pub blocks: Vec<BasicBlock>,
    pub resumes: Vec<usize>,
}

#[derive(Default)]
pub struct BasicBlock {
    /// Type explicitly being left on the stack as part of control flow.
    pub in_type: Option<BlockType>,
    /// Known predecessors.
    pub ins: Vec<usize>,
    pub instructions: Vec<u8>,
    /// Successors.
    pub out: Out,
}

#[derive(Default, Debug)]
pub enum Out {
    /// Not set.
    #[default]
    None,
    /// Return from the current function with what's on the stack. 0 successors.
    Return,
    /// Tail-call another function.
    ReturnCall { func: u32 },
    /// Like Return, but names the BB that will be resumed.
    Yield { bb_resume: usize },
    /// Wasm `unreachable`.
    Unreachable,
    /// 1 unconditional successor.
    Next(usize),
    /// 2 successors, false case then true case.
    If { f: usize, t: usize },
}

impl Out {
    pub fn for_each_successor<F: FnMut(usize)>(&self, mut func: F) {
        match *self {
            Out::None
            | Out::Return
            | Out::ReturnCall { .. }
            | Out::Yield { .. }
            | Out::Unreachable => {}
            Out::Next(a) => func(a),
            Out::If { f, t } => {
                // Prefer visiting true branch first for readability, since
                // the true branch of an if() or while() usually follows it
                // directly in source.
                func(t);
                func(f);
            }
        }
    }

    pub fn to_mermaid(&self, i: usize) -> impl fmt::Display {
        DisplayClosure(move |fmt| {
            match *self {
                Out::None => {}
                Out::Return => {
                    writeln!(fmt, "{i} --> return_{i}")?;
                    writeln!(fmt, "return_{i}([return])")?;
                }
                Out::ReturnCall { func } => {
                    writeln!(fmt, "{i} --> return_{i}")?;
                    writeln!(fmt, "return_{i}([return_call {func}])")?;
                }
                Out::Yield { bb_resume, .. } => {
                    writeln!(fmt, "{i} --> yield_{bb_resume}")?;
                    writeln!(fmt, "yield_{bb_resume}([yield {bb_resume}])")?;
                }
                Out::Unreachable => {
                    writeln!(fmt, "{i} --> unreachable_{i}")?;
                    writeln!(fmt, "unreachable_{i}([unreachable])")?;
                }
                Out::Next(bb) => {
                    writeln!(fmt, "{i} --> {bb}")?;
                }
                Out::If { f, t } => {
                    writeln!(fmt, "{i} -- false --> {f}")?;
                    writeln!(fmt, "{i} -- true --> {t}")?;
                }
            }
            Ok(())
        })
    }
}

impl ControlFlowGraph {
    pub fn add_block(&mut self) -> usize {
        let len = self.blocks.len();
        self.blocks.push(BasicBlock::default());
        len
    }

    pub fn instructions(&mut self, bb: usize) -> InstructionSink<'_> {
        assert!(!self.blocks[bb].is_filled());
        InstructionSink::new(&mut self.blocks[bb].instructions)
    }

    pub fn seal(&mut self, bb: usize, ty: BlockType) {
        assert!(!self.blocks[bb].is_sealed());
        self.blocks[bb].in_type = Some(ty);
    }

    pub fn fill(&mut self, bb: usize, out: Out) {
        if bb == usize::MAX {
            return;
        }
        assert!(!matches!(out, Out::None));
        assert!(matches!(self.blocks[bb].out, Out::None));
        out.for_each_successor(|next| {
            assert!(!self.blocks[next].is_sealed());
            self.blocks[next].ins.push(bb);
        });
        self.blocks[bb].out = out;
    }

    pub fn assert_complete(&self) {
        for (i, block) in self.blocks.iter().enumerate() {
            assert!(block.is_sealed(), "block {i} not sealed");
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
            _ = writeln!(gv, "yield_{bb} -> resume_{bb} [style=dotted];");
            _ = writeln!(gv, "resume_{bb} [shape=box] [style=rounded];");
            _ = writeln!(gv, "resume_{bb} -> {bb};");
        }
        for (i, block) in self.blocks.iter().enumerate() {
            _ = writeln!(
                gv,
                "{} [label=\"{}\"] [shape=box];",
                i,
                block.disassemble().to_string().replace("\n", "\\l")
            );
            match block.out {
                Out::None => {}
                Out::Return => {
                    _ = writeln!(gv, "{i} -> return_{i};");
                    _ = writeln!(gv, "return_{i} [label=return] [shape=box] [style=rounded];");
                }
                Out::ReturnCall { func } => {
                    _ = writeln!(gv, "{i} -> return_{i};");
                    _ = writeln!(
                        gv,
                        "return_{i} [label=\"return_call {func}\"] [shape=box] [style=rounded];"
                    );
                }
                Out::Yield { bb_resume, .. } => {
                    _ = writeln!(gv, "{i} -> yield_{bb_resume};");
                    _ = writeln!(
                        gv,
                        "yield_{bb_resume} [label=yield] [shape=box] [style=rounded];"
                    );
                }
                Out::Unreachable => {
                    _ = writeln!(gv, "{i} -> unreachable_{i};");
                    _ = writeln!(
                        gv,
                        "unreachable_{i} [label=yield] [shape=box] [style=rounded];"
                    );
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

    pub fn to_mermaid(&self) -> impl fmt::Display {
        DisplayClosure(|fmt| {
            writeln!(fmt, "flowchart TB")?;
            writeln!(fmt, "start([start])")?;
            writeln!(fmt, "start --> 0")?;
            for bb in self.resumes.iter() {
                writeln!(fmt, "yield_{bb} -.-> resume_{bb}")?;
                writeln!(fmt, "resume_{bb}([resume {bb}])")?;
                writeln!(fmt, "resume_{bb} --> {bb}")?;
            }
            for (i, block) in self.blocks.iter().enumerate() {
                let d = block.disassemble().to_string();
                writeln!(fmt, "{}[{:?}]", i, if d.is_empty() { " " } else { &d })?;
                writeln!(fmt, "style {i} text-align: left, white-space: nowrap")?;
                write!(fmt, "{}", block.out.to_mermaid(i))?;
            }
            Ok(())
        })
    }
}

impl BasicBlock {
    /// True if this block's predecessors are known.
    pub fn is_sealed(&self) -> bool {
        self.in_type.is_some()
    }

    /// True if this block's full contents and successors are known.
    pub fn is_filled(&self) -> bool {
        !matches!(self.out, Out::None)
    }

    pub fn disassemble(&self) -> impl fmt::Display {
        DisplayClosure(|fmt| {
            match self.in_type {
                None | Some(BlockType::Empty) => {}
                Some(other) => {
                    writeln!(fmt, "=> {:?}", other)?;
                }
            }
            let br = wasmparser::BinaryReader::new(&self.instructions, 0);
            let or = wasmparser::OperatorsReader::new(br);
            for result in or.into_iter() {
                let op = result.unwrap();
                writeln!(fmt, "{op:?}")?;
            }
            Ok(())
        })
    }
}
