//! Canonical outer event encoding shared by native replay and the relation.
//!
//! Events advance the executing coroutine's chain (before control transfer),
//! or the loaded/exported UTXO's chain for storage. Chains start from zero;
//! ABI preloads and phase transitions emit nothing. Bindings must share this schema.
//! Packing follows neo-wasm's EventSequenceBuilder: eight-word blocks, at most
//! one four-field root per block, zero-padding before roots as needed and at
//! event end. Continuation blocks have no extra tag. Roots are already encoded;
//! object-internal hashing is excluded, and unit is four literal zero words.

use crate::{
    BLOCK_SIZE, FIELD_MODULUS, METHOD_WORDS, MethodHash, Out, ResourceHandle, StarstreamValue, Step,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EventKind {
    SetStorage,
    GetStorage,
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

impl EventKind {
    pub fn for_step(step: &Step) -> Option<Self> {
        Some(match step {
            Step::SetStorage { .. } => Self::SetStorage,
            Step::GetStorage { .. } => Self::GetStorage,
            Step::NewUtxo { .. } => Self::NewUtxo,
            Step::EnterConstructor { .. } => Self::EnterConstructor,
            Step::YieldBegin => Self::YieldBegin,
            Step::RegisterMethod { .. } => Self::RegisterMethod,
            Step::Return { .. } => Self::Return,
            Step::CallMethod { .. } => Self::CallMethod,
            Step::EnterMethod { .. } => Self::EnterMethod,
            // these are circuit-only instructions
            Step::PreloadMethod { .. }
            | Step::ReadAbi { .. }
            | Step::SkipConsumed
            | Step::FinalizeCoordinator
            | Step::FinishTransaction => return None,
        })
    }

    pub const fn tag(self) -> u32 {
        match self {
            Self::NewUtxo => 1,
            Self::EnterConstructor => 2,
            Self::YieldBegin => 3,
            Self::RegisterMethod => 4,
            Self::Return => 5,
            Self::CallMethod => 6,
            Self::EnterMethod => 7,
            Self::SetStorage => 8,
            Self::GetStorage => 9,
        }
    }

    pub fn blocks(self) -> Vec<[Word; BLOCK_SIZE]> {
        use Word::*;
        let tag = self.tag();
        let mut words = vec![Constant(tag)];
        let mut last_root_block = None;
        let root = |words: &mut Vec<Word>, last: &mut Option<usize>, argument: bool| {
            if words.len() % BLOCK_SIZE > BLOCK_SIZE - 4 || *last == Some(words.len() / BLOCK_SIZE)
            {
                words.resize(words.len().div_ceil(BLOCK_SIZE) * BLOCK_SIZE, Constant(0));
            }
            *last = Some(words.len() / BLOCK_SIZE);
            words.extend((0..4).map(|i| if argument { Argument(i) } else { Result(i) }));
        };
        match self {
            Self::SetStorage => root(&mut words, &mut last_root_block, true),
            Self::GetStorage => root(&mut words, &mut last_root_block, false),
            Self::NewUtxo => {
                root(&mut words, &mut last_root_block, true);
                words.push(Resource);
            }
            Self::EnterConstructor => root(&mut words, &mut last_root_block, true),
            Self::YieldBegin => {}
            Self::RegisterMethod => words.extend((0..METHOD_WORDS).map(Method)),
            Self::Return => root(&mut words, &mut last_root_block, false),
            Self::CallMethod => {
                words.push(Resource);
                words.extend((0..METHOD_WORDS).map(Method));
                root(&mut words, &mut last_root_block, true);
                root(&mut words, &mut last_root_block, false);
            }
            Self::EnterMethod => {
                words.extend((0..METHOD_WORDS).map(Method));
                root(&mut words, &mut last_root_block, true);
            }
        }
        words.resize(words.len().div_ceil(BLOCK_SIZE) * BLOCK_SIZE, Constant(0));
        words
            .chunks_exact(BLOCK_SIZE)
            .map(|b| b.try_into().unwrap())
            .collect()
    }

    /// Decode an event using this kind's layout. `expected_method`, when supplied,
    /// is checked only for `CallMethod` and `EnterMethod`; other kinds ignore it.
    /// `coordinator_handle` is required for `SetStorage` and ignored otherwise:
    /// it is host metadata, absent from the committed blocks.
    pub fn decode(
        &self,
        blocks: &[[u64; BLOCK_SIZE]],
        expected_method: Option<MethodHash>,
        coordinator_handle: Option<ResourceHandle>,
    ) -> Result<Step, EventCodecError> {
        let layout = self.blocks();
        if blocks.len() != layout.len() {
            return Err(EventCodecError::WrongBlockCount {
                expected: layout.len(),
                actual: blocks.len(),
            });
        }
        let mut resource = 0_u32;
        let mut method = [0_u32; METHOD_WORDS];
        let mut argument = [0_u64; 4];
        let mut result = [0_u64; 4];
        for (block_index, (layout_block, block)) in layout.iter().zip(blocks).enumerate() {
            for (word_index, (word, &value)) in layout_block.iter().zip(block).enumerate() {
                if value >= FIELD_MODULUS {
                    return Err(EventCodecError::NonCanonicalField {
                        block: block_index,
                        word: word_index,
                        value,
                    });
                }
                let named = |name: &'static str| {
                    u32::try_from(value).map_err(|_| EventCodecError::NamedArgumentOutOfRange {
                        block: block_index,
                        word: word_index,
                        name,
                        value,
                    })
                };
                match *word {
                    Word::Constant(expected) => {
                        if value != u64::from(expected) {
                            return if (block_index, word_index) == (0, 0) {
                                Err(EventCodecError::WrongDiscriminant {
                                    expected: u64::from(expected),
                                    actual: value,
                                })
                            } else {
                                Err(EventCodecError::UnexpectedConstant {
                                    block: block_index,
                                    word: word_index,
                                    expected: u64::from(expected),
                                    value,
                                })
                            };
                        }
                    }
                    Word::Resource => resource = named("resource")?,
                    Word::Method(index) => method[index] = named("method")?,
                    Word::Argument(index) => argument[index] = value,
                    Word::Result(index) => result[index] = value,
                }
            }
        }
        let method = MethodHash(method);
        if let Some(expected) = expected_method
            && matches!(self, EventKind::CallMethod | EventKind::EnterMethod)
            && method != expected
        {
            return Err(EventCodecError::MethodMismatch {
                expected,
                actual: method,
            });
        }

        let argument = StarstreamValue(argument);
        let result = StarstreamValue(result);

        Ok(match self {
            // The coordinator-local handle of a loaded input is host state
            // outside the transcript; the host binds it when it loads the
            // UTXO.
            EventKind::SetStorage => Step::SetStorage {
                storage: argument,
                coordinator_handle: coordinator_handle
                    .ok_or(EventCodecError::MissingCoordinatorHandle)?,
            },
            EventKind::GetStorage => Step::GetStorage {
                storage: Out(result),
            },
            EventKind::NewUtxo => Step::NewUtxo {
                arguments: argument,
                resource: Out(ResourceHandle(resource)),
            },
            EventKind::EnterConstructor => Step::EnterConstructor {
                arguments: argument,
            },
            EventKind::YieldBegin => Step::YieldBegin,
            EventKind::RegisterMethod => Step::RegisterMethod { method },
            EventKind::Return => Step::Return {
                result: Out(result),
            },
            EventKind::CallMethod => Step::CallMethod {
                resource: ResourceHandle(resource),
                method,
                arguments: argument,
                result: Out(result),
            },
            EventKind::EnterMethod => Step::EnterMethod {
                method,
                arguments: argument,
            },
        })
    }
}

pub fn encode(step: &Step) -> Vec<[u64; BLOCK_SIZE]> {
    let Some(kind) = EventKind::for_step(step) else {
        return vec![];
    };

    let (resource, method, argument, result) = match step {
        Step::SetStorage { storage, .. } => (0, None, Some(storage), None),
        Step::GetStorage { storage } => (0, None, None, Some(&storage.0)),
        Step::PreloadMethod { .. }
        | Step::ReadAbi { .. }
        | Step::SkipConsumed
        | Step::FinalizeCoordinator
        | Step::FinishTransaction => unreachable!(),
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
    kind.blocks()
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

/// Failure to decode or dispatch an absorbed block assignment.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum EventCodecError {
    #[error("SetStorage requires a host-supplied coordinator handle")]
    MissingCoordinatorHandle,
    #[error("expected {expected} blocks from the static template, found {actual}")]
    WrongBlockCount { expected: usize, actual: usize },

    #[error("block {block}, word {word}: non-canonical Goldilocks value {value}")]
    NonCanonicalField {
        block: usize,
        word: usize,
        value: u64,
    },

    #[error("expected discriminant {expected}, found {actual}")]
    WrongDiscriminant { expected: u64, actual: u64 },

    #[error("block {block}, word {word}: expected constant {expected}, found {value}")]
    UnexpectedConstant {
        block: usize,
        word: usize,
        expected: u64,
        value: u64,
    },

    #[error("block {block}, word {word}: named argument {name} does not fit in u32: {value}")]
    NamedArgumentOutOfRange {
        block: usize,
        word: usize,
        name: &'static str,
        value: u64,
    },

    #[error("committed method {actual:?} differs from the template's {expected:?}")]
    MethodMismatch {
        expected: MethodHash,
        actual: MethodHash,
    },
}

#[cfg(test)]
mod test {
    use crate::{
        FIELD_MODULUS, MethodHash, Out, ResourceHandle, StarstreamValue, Step,
        events::{EventCodecError, EventKind, encode},
    };

    const METHOD: MethodHash = MethodHash([1, 2, 3, 4, 5, 6, 7, 8]);
    const ROOT: StarstreamValue = StarstreamValue([11, 12, 13, 14]);
    const OTHER: StarstreamValue = StarstreamValue([21, 22, 23, 24]);

    fn program_steps() -> Vec<Step> {
        vec![
            Step::SetStorage {
                storage: ROOT,
                coordinator_handle: ResourceHandle(7),
            },
            Step::GetStorage { storage: Out(ROOT) },
            Step::NewUtxo {
                arguments: ROOT,
                resource: Out(ResourceHandle(7)),
            },
            Step::EnterConstructor { arguments: ROOT },
            Step::YieldBegin,
            Step::RegisterMethod { method: METHOD },
            Step::Return { result: Out(ROOT) },
            Step::CallMethod {
                resource: ResourceHandle(7),
                method: METHOD,
                arguments: ROOT,
                result: Out(OTHER),
            },
            Step::EnterMethod {
                method: METHOD,
                arguments: ROOT,
            },
        ]
    }

    fn expected_method(step: &Step) -> Option<MethodHash> {
        match step {
            Step::CallMethod { method, .. } | Step::EnterMethod { method, .. } => Some(*method),
            _ => None,
        }
    }

    #[test]
    fn every_program_event_round_trips_through_its_template() {
        for step in program_steps() {
            let kind = EventKind::for_step(&step).unwrap();
            let expected_method = expected_method(&step);
            let encoded = encode(&step);
            assert_eq!(encoded.len(), kind.blocks().len(), "{step:?}");
            assert_eq!(encoded[0][0], kind.tag() as u64, "{step:?}");
            let coordinator_handle = match &step {
                Step::SetStorage {
                    coordinator_handle, ..
                } => Some(*coordinator_handle),
                _ => None,
            };
            assert_eq!(
                kind.decode(&encoded, expected_method, coordinator_handle)
                    .unwrap(),
                step
            );
        }
    }

    #[test]
    fn storage_handle_is_host_metadata_not_committed_data() {
        let first = Step::SetStorage {
            storage: ROOT,
            coordinator_handle: ResourceHandle(7),
        };
        let second = Step::SetStorage {
            storage: ROOT,
            coordinator_handle: ResourceHandle(9),
        };
        let blocks = encode(&first);
        assert_eq!(blocks, encode(&second));
        assert_eq!(
            EventKind::SetStorage.decode(&blocks, None, None),
            Err(EventCodecError::MissingCoordinatorHandle)
        );
        assert_eq!(
            EventKind::SetStorage
                .decode(&blocks, None, Some(ResourceHandle(9)))
                .unwrap(),
            second
        );
    }

    #[test]
    fn call_method_spans_three_blocks_with_one_root_each() {
        let step = Step::CallMethod {
            resource: ResourceHandle(7),
            method: METHOD,
            arguments: ROOT,
            result: Out(OTHER),
        };
        assert_eq!(
            encode(&step),
            [
                [6, 7, 1, 2, 3, 4, 5, 6],
                [7, 8, 11, 12, 13, 14, 0, 0],
                [21, 22, 23, 24, 0, 0, 0, 0],
            ]
        );
    }

    #[test]
    fn rejects_the_wrong_number_of_blocks() {
        let step = Step::EnterMethod {
            method: METHOD,
            arguments: ROOT,
        };
        let encoded = encode(&step);
        assert_eq!(
            EventKind::for_step(&step)
                .unwrap()
                .decode(&encoded[..1], None, None),
            Err(EventCodecError::WrongBlockCount {
                expected: 2,
                actual: 1
            })
        );
    }

    #[test]
    fn rejects_nonzero_padding_and_foreign_discriminants() {
        let step = Step::Return { result: Out(ROOT) };
        let kind = EventKind::for_step(&step).unwrap();
        let mut encoded = encode(&step);
        encoded[0][7] = 9;
        assert_eq!(
            kind.decode(&encoded, None, None),
            Err(EventCodecError::UnexpectedConstant {
                block: 0,
                word: 7,
                expected: 0,
                value: 9
            })
        );
        let mut encoded = encode(&step);
        encoded[0][0] = 6;
        assert_eq!(
            kind.decode(&encoded, None, None),
            Err(EventCodecError::WrongDiscriminant {
                expected: 5,
                actual: 6
            })
        );
    }

    #[test]
    fn rejects_named_words_that_do_not_fit_the_wasm_encoding() {
        let step = Step::NewUtxo {
            arguments: ROOT,
            resource: Out(ResourceHandle(7)),
        };
        let mut encoded = encode(&step);
        encoded[0][5] = 1 << 40;
        let kind = EventKind::for_step(&step).unwrap();
        assert_eq!(
            kind.decode(&encoded, None, None),
            Err(EventCodecError::NamedArgumentOutOfRange {
                block: 0,
                word: 5,
                name: "resource",
                value: 1 << 40
            })
        );
        let mut encoded = encode(&step);
        encoded[0][1] = FIELD_MODULUS;
        assert_eq!(
            kind.decode(&encoded, None, None),
            Err(EventCodecError::NonCanonicalField {
                block: 0,
                word: 1,
                value: FIELD_MODULUS
            })
        );
    }

    #[test]
    fn rejects_committed_methods_that_differ_from_the_template() {
        let step = Step::EnterMethod {
            method: METHOD,
            arguments: ROOT,
        };
        let other = MethodHash([9; 8]);
        let encoded = encode(&step);

        assert_eq!(
            EventKind::EnterMethod.decode(&encoded, Some(other), None),
            Err(EventCodecError::MethodMismatch {
                expected: other,
                actual: METHOD
            })
        );
    }
}
