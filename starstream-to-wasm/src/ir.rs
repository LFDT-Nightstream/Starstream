//! Intermediate representation in control flow graph form.

// Definitions of "sealed" and "filled" are based on those in:
//     Simple and Efficient Construction of Static Single Assignment Form.
//     Matthias Braun, Sebastian Buchwald, Sebastian Hack, Roland Leißa, Christoph Mallon, and Andreas Zwinkau.
// Sealed: No further predecessors will be added.
// Filled: In Braun, means local value numbering is finished and successors may be added.
// We have a single "fill" point that both finishes adding instructions, "fills", and immediately adds all successors.

use std::fmt;

use wasm_encoder::{BlockType, InstructionSink};

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
    /// Like Return, but names the BB that will be resumed.
    Yield(usize),
    /// 1 unconditional successor.
    Next(usize),
    /// 2 successors, false case then true case.
    If { f: usize, t: usize },
}

impl Out {
    pub fn for_each_successor<F: FnMut(usize)>(&self, mut func: F) {
        match *self {
            Out::None | Out::Return | Out::Yield(_) => {}
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

    pub fn to_mermaid(&self, id: usize) -> impl fmt::Display {
        struct Mermaid<'a>(&'a Out, usize);
        impl<'a> fmt::Display for Mermaid<'a> {
            fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
                let &Mermaid(out, i) = self;
                match out {
                    Out::None => {}
                    Out::Return => {
                        writeln!(fmt, "{i} --> return_{i}")?;
                        writeln!(fmt, "return_{i}([return])")?;
                    }
                    Out::Yield(bb) => {
                        writeln!(fmt, "{i} --> yield_{bb}")?;
                        writeln!(fmt, "yield_{bb}([yield {bb}])")?;
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
            }
        }
        Mermaid(self, id)
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
    pub fn to_mermaid(&self) -> impl fmt::Display {
        struct Mermaid<'a>(&'a ControlFlowGraph);
        impl<'a> fmt::Display for Mermaid<'a> {
            fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
                let &Mermaid(this) = self;
                writeln!(fmt, "flowchart TB")?;
                writeln!(fmt, "start([start])")?;
                writeln!(fmt, "start --> 0")?;
                for bb in this.resumes.iter() {
                    writeln!(fmt, "yield_{bb} -.-> resume_{bb}")?;
                    writeln!(fmt, "resume_{bb}([resume {bb}])")?;
                    writeln!(fmt, "resume_{bb} --> {bb}")?;
                }
                for (i, block) in this.blocks.iter().enumerate() {
                    let d = block.disassemble().to_string();
                    writeln!(fmt, "{}[{:?}]", i, if d.is_empty() { " " } else { &d })?;
                    writeln!(fmt, "style {i} text-align: left, white-space: nowrap")?;
                    write!(fmt, "{}", block.out.to_mermaid(i))?;
                }
                Ok(())
            }
        }
        Mermaid(self)
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
        struct Disassemble<'a>(&'a BasicBlock);
        impl<'a> fmt::Display for Disassemble<'a> {
            fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
                let &Disassemble(this) = self;
                match this.in_type {
                    None | Some(BlockType::Empty) => {}
                    Some(other) => {
                        writeln!(fmt, "=> {:?}", other)?;
                    }
                }
                let br = wasmparser::BinaryReader::new(&this.instructions, 0);
                let or = wasmparser::OperatorsReader::new(br);
                for result in or.into_iter() {
                    let op = result.unwrap();
                    writeln!(fmt, "{op:?}")?;
                }
                Ok(())
            }
        }
        Disassemble(self)
    }
}
