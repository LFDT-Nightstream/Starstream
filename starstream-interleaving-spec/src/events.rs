//! Canonical outer event encoding shared by native replay and the relation.
//!
//! Each semantic step belongs to the coroutine executing it, before transfer.
//! Tags are stable protocol discriminants, not Rust enum ordinals. Scalar words
//! stream across blocks. Opaque roots occupy four consecutive words, with at
//! most one root per block; pad before a root if necessary, and at event end.
//! This matches neo-wasm's EventSequenceBuilder. Object-internal hashing is NOT
//! included in the outer stream. Program bindings must use this same schema.

use crate::Step;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EventKind {
    NewUtxo,
    EnterConstructor,
    YieldBegin,
    RegisterMethod,
    Return,
    CallMethod,
    EnterMethod,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Word {
    Constant(u32),
    Resource,
    Method(usize),
    Argument(usize),
    Result(usize),
}

impl From<&Step> for EventKind {
    fn from(step: &Step) -> Self {
        match step {
            Step::NewUtxo { .. } => Self::NewUtxo,
            Step::EnterConstructor { .. } => Self::EnterConstructor,
            Step::YieldBegin => Self::YieldBegin,
            Step::RegisterMethod { .. } => Self::RegisterMethod,
            Step::Return { .. } => Self::Return,
            Step::CallMethod { .. } => Self::CallMethod,
            Step::EnterMethod { .. } => Self::EnterMethod,
        }
    }
}

impl EventKind {
    pub fn blocks(self) -> Vec<[Word; 8]> {
        use Word::*;
        let tag = match self {
            Self::NewUtxo => 1,
            Self::EnterConstructor => 2,
            Self::YieldBegin => 3,
            Self::RegisterMethod => 4,
            Self::Return => 5,
            Self::CallMethod => 6,
            Self::EnterMethod => 7,
        };
        let mut words = vec![Constant(tag)];
        let mut last_root_block = None;
        let root = |words: &mut Vec<Word>, last: &mut Option<usize>, argument: bool| {
            if words.len() % 8 > 4 || *last == Some(words.len() / 8) {
                words.resize(words.len().div_ceil(8) * 8, Constant(0));
            }
            *last = Some(words.len() / 8);
            words.extend((0..4).map(|i| if argument { Argument(i) } else { Result(i) }));
        };
        match self {
            Self::NewUtxo => {
                root(&mut words, &mut last_root_block, true);
                words.push(Resource);
            }
            Self::EnterConstructor => root(&mut words, &mut last_root_block, true),
            Self::YieldBegin => {}
            Self::RegisterMethod => words.extend((0..8).map(Method)),
            Self::Return => root(&mut words, &mut last_root_block, false),
            Self::CallMethod => {
                words.push(Resource);
                words.extend((0..8).map(Method));
                root(&mut words, &mut last_root_block, true);
                root(&mut words, &mut last_root_block, false);
            }
            Self::EnterMethod => {
                words.extend((0..8).map(Method));
                root(&mut words, &mut last_root_block, true);
            }
        }
        words.resize(words.len().div_ceil(8) * 8, Constant(0));
        words
            .chunks_exact(8)
            .map(|b| b.try_into().unwrap())
            .collect()
    }
}

pub fn encode(step: &Step) -> Vec<[u64; 8]> {
    let (resource, method, argument, result) = match step {
        Step::NewUtxo {
            arguments,
            resource,
        } => (resource.0.0, None, Some(arguments), None),
        Step::EnterConstructor { arguments } => (0, None, Some(arguments), None),
        Step::YieldBegin => (0, None, None, None),
        Step::RegisterMethod { method } => (0, Some(method), None, None),
        Step::Return { result } => (0, None, None, Some(&result.0)),
        Step::CallMethod {
            resource,
            method,
            arguments,
            result,
        } => (resource.0, Some(method), Some(arguments), Some(&result.0)),
        Step::EnterMethod { method, arguments } => (0, Some(method), Some(arguments), None),
    };
    EventKind::from(step)
        .blocks()
        .into_iter()
        .map(|block| {
            block.map(|word| match word {
                Word::Constant(x) => u64::from(x),
                Word::Resource => u64::from(resource),
                Word::Method(i) => u64::from(method.unwrap().0[i]),
                Word::Argument(i) => argument.unwrap().0[i],
                Word::Result(i) => result.unwrap().0[i],
            })
        })
        .collect()
}
