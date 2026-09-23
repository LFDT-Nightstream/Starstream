use std::collections::BTreeMap;

use starstream_interleaving_spec::events::EventKind;
use starstream_interleaving_spec::{BLOCK_SIZE, MethodHash, ResourceHandle, Step, Trace};

/// One block as returned in `AbsorbedEventBlock::words` by
/// `neo_wasm::comm_chain::absorbed_event_blocks`.
pub type AbsorbedBlock = [u64; BLOCK_SIZE];

/// A compiler-selected event template.
///
/// The kind determines the block layout and count; `method` is the identity
/// derived from the import or export name for `CallMethod`/`EnterMethod`
/// templates, cross-checked against the committed words.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EventTemplate {
    pub kind: EventKind,
    pub method: Option<MethodHash>,
}

impl EventTemplate {
    #[must_use]
    pub const fn new(kind: EventKind) -> Self {
        Self { kind, method: None }
    }

    #[must_use]
    pub const fn with_method(kind: EventKind, method: MethodHash) -> Self {
        Self {
            kind,
            method: Some(method),
        }
    }

    /// First word of the first block.
    #[must_use]
    pub const fn discriminant(&self) -> u64 {
        self.kind.tag() as u64
    }

    /// Blocks the event occupies.
    #[must_use]
    pub fn block_count(&self) -> usize {
        self.kind.blocks().len()
    }

    fn decode(&self, blocks: &[AbsorbedBlock]) -> Result<Step, BlockCodecError> {
        Ok(self
            .kind
            .decode(blocks, self.method, Some(ResourceHandle(0)))?)
    }
}

/// Nightstream metadata attached to an absorbed event block.
///
/// It selects the component-local template and lets the decoder ensure that
/// all blocks of one logical event came from the same attributed function and
/// export turn. It is not encoded into `words`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BlockMetadata {
    pub attributed_fref: u32,
    pub turn_export_fref: u32,
}

/// An absorbed Nightstream block together with its projection metadata.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AttributedBlock {
    pub words: AbsorbedBlock,
    pub metadata: BlockMetadata,
}

impl AttributedBlock {
    #[must_use]
    pub const fn new(words: AbsorbedBlock, attributed_fref: u32, turn_export_fref: u32) -> Self {
        Self {
            words,
            metadata: BlockMetadata {
                attributed_fref,
                turn_export_fref,
            },
        }
    }
}

impl From<&neo_wasm::comm_chain::AbsorbedEventBlock> for AttributedBlock {
    fn from(block: &neo_wasm::comm_chain::AbsorbedEventBlock) -> Self {
        Self::new(
            block.words,
            block.metadata.attributed_fref,
            block.metadata.turn_export_fref,
        )
    }
}

/// Component-local registry of the templates emitted by the compiler.
///
/// Function references are local to a component. The first-block
/// discriminant is also part of the key because one function may emit
/// distinct entry and exit events.
#[derive(Clone, Debug, Default)]
pub struct TemplateRegistry {
    templates: BTreeMap<(u32, u64), EventTemplate>,
}

impl TemplateRegistry {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(
        &mut self,
        attributed_fref: u32,
        template: EventTemplate,
    ) -> Result<(), BlockCodecError> {
        let discriminant = template.discriminant();
        let key = (attributed_fref, discriminant);
        if self.templates.contains_key(&key) {
            return Err(BlockCodecError::DuplicateTemplate {
                attributed_fref,
                discriminant,
            });
        }
        self.templates.insert(key, template);
        Ok(())
    }

    /// The template registered for `(attributed_fref, discriminant)`, if any.
    #[must_use]
    pub fn get(&self, attributed_fref: u32, discriminant: u64) -> Option<&EventTemplate> {
        self.templates.get(&(attributed_fref, discriminant))
    }

    /// Decode a complete absorbed-block stream into a semantic trace.
    ///
    /// Each first block selects a template with
    /// `(attributed_fref, discriminant)`. That template then consumes its
    /// statically known number of blocks.
    pub fn decode_blocks(&self, blocks: &[AttributedBlock]) -> Result<Trace, BlockCodecError> {
        let mut steps = Vec::new();
        let mut block_index = 0;

        while block_index < blocks.len() {
            let first = blocks[block_index];
            let discriminant = first.words[0];
            let attributed_fref = first.metadata.attributed_fref;

            let Some(template) = self.get(attributed_fref, discriminant).copied() else {
                return Err(BlockCodecError::UnknownTemplate {
                    block: block_index,
                    attributed_fref,
                    discriminant,
                });
            };

            let expected_blocks = template.block_count();
            let available_blocks = blocks.len() - block_index;
            if available_blocks < expected_blocks {
                return Err(BlockCodecError::TruncatedTemplate {
                    block: block_index,
                    attributed_fref,
                    discriminant,
                    expected_blocks,
                    available_blocks,
                });
            }

            let event_blocks = &blocks[block_index..block_index + expected_blocks];
            for (offset, block) in event_blocks.iter().enumerate().skip(1) {
                let actual_index = block_index + offset;
                if block.metadata.attributed_fref != attributed_fref {
                    return Err(BlockCodecError::AttributionChanged {
                        block: actual_index,
                        expected: attributed_fref,
                        actual: block.metadata.attributed_fref,
                    });
                }
                if block.metadata.turn_export_fref != first.metadata.turn_export_fref {
                    return Err(BlockCodecError::TurnChanged {
                        block: actual_index,
                        expected: first.metadata.turn_export_fref,
                        actual: block.metadata.turn_export_fref,
                    });
                }
            }

            let words = event_blocks
                .iter()
                .map(|block| block.words)
                .collect::<Vec<_>>();
            steps.push(template.decode(&words)?);
            block_index += expected_blocks;
        }

        Ok(Trace::new(steps))
    }
}

/// Failure to decode or dispatch an absorbed block assignment.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum BlockCodecError {
    #[error(
        "block {block}: attributed function changed inside an event from {expected} to {actual}"
    )]
    AttributionChanged {
        block: usize,
        expected: u32,
        actual: u32,
    },

    #[error("block {block}: export turn changed inside an event from {expected} to {actual}")]
    TurnChanged {
        block: usize,
        expected: u32,
        actual: u32,
    },

    #[error(
        "block {block}: template for function {attributed_fref} and discriminant {discriminant} \
         requires {expected_blocks} blocks, but only {available_blocks} remain"
    )]
    TruncatedTemplate {
        block: usize,
        attributed_fref: u32,
        discriminant: u64,
        expected_blocks: usize,
        available_blocks: usize,
    },

    #[error(
        "block {block}: no template for function {attributed_fref} and discriminant {discriminant}"
    )]
    UnknownTemplate {
        block: usize,
        attributed_fref: u32,
        discriminant: u64,
    },

    #[error(
        "template already registered for function {attributed_fref} and discriminant {discriminant}"
    )]
    DuplicateTemplate {
        attributed_fref: u32,
        discriminant: u64,
    },

    #[error("single event block decode error")]
    SingleEventError(#[from] starstream_interleaving_spec::events::EventCodecError),
}

#[cfg(test)]
mod tests {
    use starstream_interleaving_spec::{Out, StarstreamValue, events};

    use super::*;

    const METHOD: MethodHash = MethodHash([1, 2, 3, 4, 5, 6, 7, 8]);
    const ROOT: StarstreamValue = StarstreamValue([11, 12, 13, 14]);
    const OTHER: StarstreamValue = StarstreamValue([21, 22, 23, 24]);

    fn attributed(step: &Step, fref: u32) -> Vec<AttributedBlock> {
        events::encode(step)
            .into_iter()
            .map(|words| AttributedBlock::new(words, fref, 1))
            .collect()
    }

    fn template_for(step: &Step) -> EventTemplate {
        let kind = EventKind::for_step(step).expect("program event");
        match step {
            Step::CallMethod { method, .. } | Step::EnterMethod { method, .. } => {
                EventTemplate::with_method(kind, *method)
            }
            _ => EventTemplate::new(kind),
        }
    }

    #[test]
    fn registry_dispatches_on_function_and_discriminant() {
        let mut registry = TemplateRegistry::new();
        let enter = Step::EnterMethod {
            method: METHOD,
            arguments: ROOT,
        };
        let ret = Step::Return { result: Out(OTHER) };
        registry.register(9, template_for(&enter)).unwrap();
        registry.register(9, template_for(&ret)).unwrap();
        let mut stream = attributed(&enter, 9);
        stream.extend(attributed(&ret, 9));
        assert_eq!(
            registry.decode_blocks(&stream).unwrap(),
            Trace::new([enter, ret])
        );
    }

    #[test]
    fn registry_rejects_duplicate_unknown_and_truncated_templates() {
        let mut registry = TemplateRegistry::new();
        let enter = Step::EnterMethod {
            method: METHOD,
            arguments: ROOT,
        };
        registry.register(9, template_for(&enter)).unwrap();
        assert_eq!(
            registry.register(9, template_for(&enter)),
            Err(BlockCodecError::DuplicateTemplate {
                attributed_fref: 9,
                discriminant: 7
            })
        );
        assert_eq!(
            registry.decode_blocks(&attributed(&enter, 4)),
            Err(BlockCodecError::UnknownTemplate {
                block: 0,
                attributed_fref: 4,
                discriminant: 7
            })
        );
        assert_eq!(
            registry.decode_blocks(&attributed(&enter, 9)[..1]),
            Err(BlockCodecError::TruncatedTemplate {
                block: 0,
                attributed_fref: 9,
                discriminant: 7,
                expected_blocks: 2,
                available_blocks: 1
            })
        );
    }

    #[test]
    fn registry_rejects_metadata_changes_inside_an_event() {
        let mut registry = TemplateRegistry::new();
        let enter = Step::EnterMethod {
            method: METHOD,
            arguments: ROOT,
        };
        registry.register(9, template_for(&enter)).unwrap();
        let mut stream = attributed(&enter, 9);
        stream[1].metadata.attributed_fref = 4;
        assert_eq!(
            registry.decode_blocks(&stream),
            Err(BlockCodecError::AttributionChanged {
                block: 1,
                expected: 9,
                actual: 4
            })
        );
        let mut stream = attributed(&enter, 9);
        stream[1].metadata.turn_export_fref = 2;
        assert_eq!(
            registry.decode_blocks(&stream),
            Err(BlockCodecError::TurnChanged {
                block: 1,
                expected: 1,
                actual: 2
            })
        );
    }
}
