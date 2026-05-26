//! Converter from basic-block control flow graphs to Wasm stack machine structured control flow.

// Based on the algorithm described in "Solving the structured control flow problem once and for all", Yuri Iozzelli.
// - https://medium.com/leaningtech/solving-the-structured-control-flow-problem-once-and-for-all-5123117b1ee2
// which loosely describes LLVM's CFG stackifier for Wasm.
//
// Other references:
// - https://yazandaba.hashnode.dev/ssa-to-stack-retargeting-llvm-to-stack-machinesa-deep-dive-through-the-webassembly-backend
// - https://eli.thegreenplace.net/2013/09/16/analyzing-function-cfgs-with-llvm
//
// Currently only supports reducible CFGs (those without gotos into loops).

use std::collections::{HashMap, HashSet};
use std::fmt;

use wasm_encoder::{Encode, InstructionSink};

use crate::{
    StFunction,
    ir::{ControlFlowGraph, Out},
};

#[derive(Debug)]
enum SeqItem {
    Basic(usize),
    StartLoop(usize),
    EndLoop(usize),
    StartBlock(usize),
    EndBlock(usize),
}

pub struct Stackified<'a> {
    func: &'a StFunction,
    entry: usize,
    pub code: Vec<u8>,
    seq: Vec<SeqItem>,
}

pub fn stackify(func: &StFunction, entry: usize) -> Stackified<'_> {
    let mut s = Stackified {
        func,
        entry,
        code: Vec::new(),
        seq: Vec::new(),
    };
    s.compile();
    s
}

impl<'a> Stackified<'a> {
    fn compile(&mut self) {
        let &mut Stackified { func, entry, .. } = self;
        func.cfg.assert_complete();

        // Basic function header
        let sink = &mut self.code;
        func.locals.len().encode(sink);
        for (count, ty) in &func.locals {
            count.encode(sink);
            ty.encode(sink);
        }

        // Simultaneously SCC split (loop detect) & toposort with Tarjan's algorithm.
        let mut seq = Vec::new();
        Tarjan::new(&func.cfg, &mut seq, Default::default()).strongconnect(entry);
        // NOTE: Tarjan emits SCCs in reverse of depth-first order, so reverse here.
        seq.reverse();

        // Now we know `loop` + `end` pairs and must insert `block` + `end` pairs.
        let mut i = 0;
        let mut seen = HashSet::new();
        while i < seq.len() {
            if let SeqItem::Basic(bb) = seq[i] {
                seen.insert(bb);
                func.cfg.blocks[bb].out.for_each_successor(|next| {
                    if !seen.contains(&next) {
                        // Forward edge.
                        if !next_block_is(&seq[i + 1..], next) {
                            // Insert `end` before destination (next),
                            // then `block` before source (bb) at corresponding depth.
                            // Algorithmic complexity suspicion point - risk of N^2 behavior?
                            let (j, depth) = find_node_backwards(&seq, next);
                            seq.insert(j, SeqItem::EndBlock(next));
                            let k = find_depth_forwards(&seq[..i], depth);
                            seq.insert(k, SeqItem::StartBlock(next));
                            i += 1;
                        }
                    }
                });
            }
            i += 1;
        }

        // Now emit everything
        let mut depth = DepthTracker::new();
        for (i, item) in seq.iter().enumerate() {
            depth.track(item);
            match *item {
                SeqItem::Basic(bb) => {
                    sink.extend_from_slice(&func.cfg.blocks[bb].instructions);
                    match func.cfg.blocks[bb].out {
                        Out::None => unreachable!(),
                        Out::Return | Out::Yield(_) => {
                            InstructionSink::new(sink).return_();
                        }
                        Out::Unreachable => {
                            InstructionSink::new(sink).unreachable();
                        }
                        Out::Next(next) => {
                            if next_block_is(&seq[i + 1..], next) {
                                // Just continue
                            } else {
                                // Forward jump
                                InstructionSink::new(sink).br(depth.diff(next));
                            }
                        }
                        Out::If { f, t } => {
                            if next_block_is(&seq[i + 1..], f) {
                                // Forward jump if true, just continue if false
                                InstructionSink::new(sink).br_if(depth.diff(t));
                            } else if next_block_is(&seq[i + 1..], t) {
                                // Forward jump if false, just continue if true
                                InstructionSink::new(sink).i32_eqz().br_if(depth.diff(f));
                            } else {
                                // Forward jump if true
                                InstructionSink::new(sink).br_if(depth.diff(t));
                                // Forward jump
                                InstructionSink::new(sink).br(depth.diff(f));
                            }
                        }
                    }
                }
                SeqItem::StartLoop(bb) => {
                    InstructionSink::new(sink).loop_(func.cfg.blocks[bb].in_type.unwrap());
                }
                SeqItem::StartBlock(bb) => {
                    InstructionSink::new(sink).block(func.cfg.blocks[bb].in_type.unwrap());
                }
                SeqItem::EndLoop(_) | SeqItem::EndBlock(_) => {
                    InstructionSink::new(sink).end();
                }
            }
        }

        // Finishing touch
        InstructionSink::new(sink).end();
        self.seq = seq;
    }

    pub fn to_mermaid(&self) -> impl fmt::Display {
        struct Mermaid<'a>(&'a Stackified<'a>);
        impl<'a> fmt::Display for Mermaid<'a> {
            fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
                let &Mermaid(this) = self;
                writeln!(fmt, "flowchart TB")?;
                if this.entry == 0 {
                    writeln!(fmt, "start([start])")?;
                    writeln!(fmt, "start --> {}", this.entry)?;
                } else {
                    writeln!(fmt, "start([resume {}])", this.entry)?;
                    writeln!(fmt, "start --> {}", this.entry)?;
                }
                let mut depth = DepthTracker::new();
                for (i, item) in this.seq.iter().enumerate() {
                    depth.track(item);
                    match *item {
                        SeqItem::Basic(bb) => {
                            writeln!(fmt, "{bb}[\"")?;
                            write!(fmt, "{}", this.func.cfg.blocks[bb].disassemble())?;
                            match this.func.cfg.blocks[bb].out {
                                Out::None => unreachable!(),
                                Out::Return | Out::Yield(_) => {
                                    writeln!(fmt, "return")?;
                                }
                                Out::Unreachable => {
                                    writeln!(fmt, "unreachable")?;
                                }
                                Out::Next(next) => {
                                    if !next_block_is(&this.seq[i + 1..], next) {
                                        writeln!(fmt, "br {}", depth.diff(next))?;
                                    }
                                }
                                Out::If { f, t } => {
                                    if next_block_is(&this.seq[i + 1..], f) {
                                        writeln!(fmt, "br_if {}", depth.diff(t))?;
                                    } else if next_block_is(&this.seq[i + 1..], t) {
                                        writeln!(fmt, "i32_eqz")?;
                                        writeln!(fmt, "br_if {}", depth.diff(f))?;
                                    } else {
                                        writeln!(fmt, "br_if {}", depth.diff(t))?;
                                        writeln!(fmt, "br {}", depth.diff(f))?;
                                    }
                                }
                            }
                            writeln!(fmt, "\"]")?;
                            writeln!(fmt, "style {bb} text-align: left, white-space: nowrap")?;
                        }
                        SeqItem::StartLoop(bb) => {
                            let block_type = this.func.cfg.blocks[bb].in_type.unwrap();
                            writeln!(fmt, "subgraph loop_{i} [\"loop {block_type:?}\"]")?;
                        }
                        SeqItem::StartBlock(bb) => {
                            let block_type = this.func.cfg.blocks[bb].in_type.unwrap();
                            writeln!(fmt, "subgraph block_{i} [\"block {block_type:?}\"]")?;
                            writeln!(fmt, "style block_{i} fill:#efe")?;
                        }
                        SeqItem::EndBlock(_) | SeqItem::EndLoop(_) => {
                            writeln!(fmt, "end")?;
                        }
                    }
                }
                for item in this.seq.iter() {
                    if let &SeqItem::Basic(bb) = item {
                        write!(fmt, "{}", this.func.cfg.blocks[bb].out.to_mermaid(bb))?;
                    }
                }
                Ok(())
            }
        }
        Mermaid(self)
    }
}

struct DepthTracker {
    depth: usize,
    map: HashMap<usize, Vec<usize>>,
}

impl DepthTracker {
    fn new() -> Self {
        DepthTracker {
            depth: 0,
            map: HashMap::new(),
        }
    }

    fn diff(&self, bb: usize) -> u32 {
        (self.depth - self.map[&bb].last().unwrap()) as u32
    }

    fn push(&mut self, bb: usize) {
        self.depth += 1;
        self.map.entry(bb).or_default().push(self.depth);
    }

    fn pop(&mut self, bb: usize) {
        self.map.get_mut(&bb).unwrap().pop().unwrap();
        self.depth -= 1;
    }

    fn track(&mut self, item: &SeqItem) {
        match *item {
            SeqItem::Basic(_) => {}
            SeqItem::StartLoop(bb) | SeqItem::StartBlock(bb) => self.push(bb),
            SeqItem::EndLoop(bb) | SeqItem::EndBlock(bb) => self.pop(bb),
        }
    }
}

fn next_block_is(seq: &[SeqItem], next: usize) -> bool {
    for each in seq {
        match each {
            &SeqItem::Basic(bb) => return bb == next,
            _ => {}
        }
    }
    false
}

fn find_node_backwards(seq: &[SeqItem], next: usize) -> (usize, usize) {
    let mut depth = 0;
    for (i, item) in seq.iter().rev().enumerate() {
        match item {
            &SeqItem::Basic(found) => {
                if found == next {
                    return (seq.len() - i - 1, depth);
                }
            }
            SeqItem::EndLoop(_) | SeqItem::EndBlock(_) => depth += 1,
            SeqItem::StartLoop(_) | SeqItem::StartBlock(_) => depth -= 1,
        }
    }
    panic!()
}

fn find_depth_forwards(seq: &[SeqItem], target_depth: usize) -> usize {
    let mut depth = 0;
    let mut last = if target_depth == 0 { 0 } else { usize::MAX };
    for (i, item) in seq.iter().enumerate() {
        match item {
            SeqItem::Basic(_) => {}
            SeqItem::StartLoop(_) | SeqItem::StartBlock(_) => {
                if depth == target_depth {
                    last = i;
                }
                depth += 1;
            }
            SeqItem::EndLoop(_) | SeqItem::EndBlock(_) => depth -= 1,
        }
    }
    assert_ne!(last, usize::MAX);
    last
}

// https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
/// Tarjan's algorithm for combined SCC detection & toposort.
struct Tarjan<'a> {
    cfg: &'a ControlFlowGraph,
    seq: &'a mut Vec<SeqItem>,
    limit_to: HashSet<usize>,
    index: usize,
    stack: Vec<usize>,
    v_index: HashMap<usize, usize>,
    v_lowlink: HashMap<usize, usize>,
    v_on_stack: HashSet<usize>,
}

impl<'a> Tarjan<'a> {
    fn new(
        cfg: &'a ControlFlowGraph,
        seq: &'a mut Vec<SeqItem>,
        limit_to: HashSet<usize>,
    ) -> Tarjan<'a> {
        Tarjan {
            cfg,
            seq,
            limit_to,
            index: 0,
            stack: Default::default(),
            v_index: Default::default(),
            v_lowlink: Default::default(),
            v_on_stack: Default::default(),
        }
    }

    fn strongconnect(&mut self, v: usize) {
        // Set depth of v to smallest unused index
        self.v_index.insert(v, self.index);
        self.v_lowlink.insert(v, self.index);
        self.index += 1;
        self.stack.push(v);
        self.v_on_stack.insert(v);

        // Consider successors of v
        self.cfg.blocks[v].out.for_each_successor(|w| {
            // Modification: skip successors outside our limit set
            if !self.limit_to.is_empty() && !self.limit_to.contains(&w) {
                return;
            }

            if !self.v_index.contains_key(&w) {
                // Not yet visited; recurse
                self.strongconnect(w);
                self.v_lowlink.insert(
                    v,
                    *self
                        .v_lowlink
                        .get(&v)
                        .unwrap()
                        .min(self.v_lowlink.get(&w).unwrap()),
                );
            } else if self.v_on_stack.contains(&w) {
                // w is in the stack & in the current SCC
                self.v_lowlink.insert(
                    v,
                    *self
                        .v_lowlink
                        .get(&v)
                        .unwrap()
                        .min(self.v_index.get(&w).unwrap()),
                );
            }
            // w is not on the stack, which must mean it's in an SCC we've already found
        });

        // If v is a root node, pop the stack to generate an SCC
        if self.v_lowlink.get(&v).unwrap() == self.v_index.get(&v).unwrap() {
            let mut scc = Vec::new();
            loop {
                let w = self.stack.pop().unwrap();
                self.v_on_stack.remove(&w);
                scc.push(w);
                if w == v {
                    break;
                }
            }
            self.on_scc(scc);
        }
    }

    // Our own handling on discovering an SCC: recurse to split it into nested
    // SCCs by ignoring edges outside the SCC or back to its root (the loop header)
    fn on_scc(&mut self, scc: Vec<usize>) {
        let v = *scc.last().unwrap();
        if scc.len() > 1 {
            let mut limit_to = HashSet::from_iter(scc);
            limit_to.remove(&v);
            self.seq.push(SeqItem::EndLoop(v));
            // Algorithmic complexity suspicion point - should be n log(n) tho?
            Tarjan::new(self.cfg, self.seq, limit_to).strongconnect(v);
            self.seq.push(SeqItem::StartLoop(v));
        } else {
            self.seq.push(SeqItem::Basic(v));
        }
    }
}
