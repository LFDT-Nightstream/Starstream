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

#[derive(Default)]
pub struct Stackified {
    pub code: Vec<u8>,
}

pub fn stackify(func: &StFunction, entry: usize) -> Stackified {
    let mut s = Stackified::default();
    s.compile(func, entry);
    s
}

impl Stackified {
    fn compile(&mut self, func: &StFunction, entry: usize) {
        func.cfg.assert_complete();
        eprintln!("{}", func.cfg.to_mermaid());

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

        eprint_seq(&seq);
        eprintln!();

        // Now we know `loop` + `end` pairs and must insert `block` + `end` pairs.
        let mut i = 0;
        let mut seen = HashSet::new();
        while i < seq.len() {
            if let SeqItem::Basic(bb) = seq[i] {
                seen.insert(bb);
                func.cfg.blocks[bb].out.for_each_successor(|next| {
                    if !seen.contains(&next) {
                        // Forward edge.
                        eprintln!("forward edge {bb} -> {next}");
                        if next_block_is(&seq[i + 1..], next) {
                            eprintln!("    immediate");
                            // Nothing to do, it's already the immediate next block.
                        } else {
                            // Insert `end` before destination (next),
                            // then `block` before source (bb) at corresponding depth.
                            // Algorithmic complexity suspicion point - risk of N^2 behavior?
                            let (j, depth) = find_node_backwards(&seq, next);
                            seq.insert(j, SeqItem::EndBlock(next));
                            let k = find_depth_forwards(&seq[..i], depth);
                            seq.insert(k, SeqItem::StartBlock(next));
                            eprintln!("    block k={k} j={j}");
                            i += 1;
                        }
                    }
                });
            }
            i += 1;
        }

        eprint_seq(&seq);
        eprintln!();

        // Now emit everything
        let mut depth_map = HashMap::<usize, Vec<usize>>::new();
        let mut depth = 0;
        for (i, item) in seq.iter().enumerate() {
            eprintln!("{:?}", item);
            match *item {
                SeqItem::Basic(bb) => {
                    eprintln!(
                        "    {}",
                        func.cfg.blocks[bb]
                            .disassemble()
                            .trim()
                            .replace("\n", "\n    ")
                    );
                    sink.extend_from_slice(&func.cfg.blocks[bb].instructions);
                    eprintln!("  {:?}", func.cfg.blocks[bb].out);
                    match func.cfg.blocks[bb].out {
                        Out::None => unreachable!(),
                        Out::Return | Out::Yield(_) => {
                            eprintln!("    return");
                            InstructionSink::new(sink).return_();
                        }
                        Out::Next(next) => {
                            if next_block_is(&seq[i + 1..], next) {
                                // Just continue
                            } else {
                                // Forward jump
                                let depth_diff =
                                    depth - depth_map.get(&next).unwrap().last().unwrap();
                                eprintln!("    br {depth_diff}");
                                InstructionSink::new(sink).br(depth_diff as u32);
                            }
                        }
                        Out::If { f, t } => {
                            if next_block_is(&seq[i + 1..], f) {
                                // Forward jump if true, just continue if false
                                let depth_diff = depth - depth_map.get(&t).unwrap().last().unwrap();
                                InstructionSink::new(sink).br_if(depth_diff as u32);
                                eprintln!("    br_if {depth_diff}");
                            } else if next_block_is(&seq[i + 1..], t) {
                                // Forward jump if false, just continue if true
                                let depth_diff = depth - depth_map.get(&f).unwrap().last().unwrap();
                                InstructionSink::new(sink)
                                    .i32_eqz()
                                    .br_if(depth_diff as u32);
                                eprintln!("    i32_eqz");
                                eprintln!("    br_if {depth_diff}");
                            } else {
                                // Forward jump if true
                                let depth_diff = depth - depth_map.get(&t).unwrap().last().unwrap();
                                InstructionSink::new(sink).br_if(depth_diff as u32);
                                eprintln!("    br_if {depth_diff}");
                                // Forward jump
                                let depth_diff = depth - depth_map.get(&f).unwrap().last().unwrap();
                                InstructionSink::new(sink).br(depth_diff as u32);
                                eprintln!("    br {depth_diff}");
                            }
                        }
                    }
                }
                SeqItem::StartLoop(bb) => {
                    eprintln!("    loop {:?}", func.cfg.blocks[bb].in_type.unwrap());
                    InstructionSink::new(sink).loop_(func.cfg.blocks[bb].in_type.unwrap());
                    depth += 1;
                    depth_map.entry(bb).or_default().push(depth);
                }
                SeqItem::StartBlock(bb) => {
                    eprintln!("    block {:?}", func.cfg.blocks[bb].in_type.unwrap());
                    InstructionSink::new(sink).block(func.cfg.blocks[bb].in_type.unwrap());
                    depth += 1;
                    depth_map.entry(bb).or_default().push(depth);
                }
                SeqItem::EndLoop(bb) | SeqItem::EndBlock(bb) => {
                    eprintln!("    end");
                    InstructionSink::new(sink).end();
                    depth -= 1;
                    depth_map.get_mut(&bb).unwrap().pop();
                }
            }
        }

        // Finishing touch
        InstructionSink::new(sink).end();
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

fn eprint_seq(seq: &[SeqItem]) {
    let mut depth = String::new();
    for item in seq.iter() {
        match item {
            SeqItem::EndLoop(_) | SeqItem::EndBlock(_) => {
                depth.pop();
            }
            _ => {}
        }
        eprintln!("{depth}{item:?}");
        match item {
            SeqItem::StartLoop(_) | SeqItem::StartBlock(_) => depth.push(' '),
            _ => {}
        }
    }
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
