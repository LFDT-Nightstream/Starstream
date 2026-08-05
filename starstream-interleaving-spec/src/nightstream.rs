//! Starstream opcode decoding over Nightstream absorbed blocks.
//!
//! Nightstream's grammar templates and the component's static type information
//! determine every opcode's block count and slot meanings. The absorbed stream
//! is deliberately not self-describing: this module does not add event-level
//! lengths, continuation tags, or IVC-state details.

use std::collections::BTreeMap;

use crate::{ExecutionEvent, ExecutionTrace, MethodHash, ResourceHandle, StarstreamValue};

/// Number of Goldilocks words in one Nightstream absorbed block.
pub const ABSORBED_BLOCK_WORDS: usize = 8;

/// Number of named argument slots after a single-block opcode discriminant.
pub const OPCODE_ARG_WORDS: usize = ABSORBED_BLOCK_WORDS - 1;

/// One block as returned in `AbsorbedEventBlock::words` by
/// `neo_wasm::comm_chain::absorbed_event_blocks`.
pub type AbsorbedBlock = [u64; ABSORBED_BLOCK_WORDS];

/// Goldilocks' canonical modulus.
pub const GOLDILOCKS_ORDER: u64 = 0xffff_ffff_0000_0001;

/// Provisional Starstream opcode discriminants.
///
/// The assignments are kept separate from the semantic event enum so a future
/// transcript version can change them without changing the Quint model.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u64)]
pub enum OpcodeDiscriminant {
    Init = 1,
    NewUtxo = 2,
    // 3 was the split NewUtxoReturn opcode. Constructor imports are now one
    // atomic event containing both their arguments and returned resource.
    ClearAbi = 4,
    AdvertiseMethod = 5,
    ReturnControl = 6,
    CallMethod = 7,
    // 8 was the provisional HandlerReturn opcode; keep it retired so existing
    // captured prototype traces cannot silently acquire a different meaning.
    CoroutineReturn = 9,
    CoordReturn = 10,
    EnterMethod = 11,
}

/// Static decoding information for one imported Starstream method.
///
/// The import/function identity supplies `method`; it is not redundantly
/// encoded in the absorbed blocks. The method's statically known parameter
/// type supplies `value_word_count`, while the flat core result supplies
/// `result_word_count`. Blocks use the provisional compact
/// assignment:
///
/// ```text
/// first block: [CallMethod, resource, value_0, ..., value_5]
/// continuation: [value_6, ..., value_13]
///                [value_14, ...]
/// result block: [result_0, result_1, 0, ..., 0]
/// ```
///
/// Continuation blocks are untagged because their number is derived from the
/// template.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CallMethodTemplate {
    pub discriminant: u64,
    pub method: MethodHash,
    pub value_word_count: usize,
    pub result_word_count: usize,
}

impl CallMethodTemplate {
    #[must_use]
    pub const fn new(
        method: MethodHash,
        value_word_count: usize,
        result_word_count: usize,
    ) -> Self {
        Self {
            discriminant: OpcodeDiscriminant::CallMethod as u64,
            method,
            value_word_count,
            result_word_count,
        }
    }

    #[must_use]
    pub fn block_count(self) -> usize {
        payload_block_count(self.value_word_count) + result_block_count(self.result_word_count)
    }
}

/// Static decoding information for the result published when a UTXO export
/// returns control. Constructors use zero semantic result words because their
/// internal resource representation is not the caller-local handle.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReturnControlTemplate {
    pub discriminant: u64,
    pub result_word_count: usize,
}

/// Static decoding information for a method export's entry boundary.
///
/// The entry claim words are ordered with the user arguments first, followed
/// by internal bootstrap words such as the callee-local resource receiver and
/// zero-initialized declared locals. All words are committed and constrained
/// as entry-local writes, but only the user argument prefix enters the
/// interleaving semantics.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EnterMethodTemplate {
    pub discriminant: u64,
    pub argument_word_count: usize,
    pub bootstrap_word_count: usize,
}

impl EnterMethodTemplate {
    #[must_use]
    pub const fn new(argument_word_count: usize, bootstrap_word_count: usize) -> Self {
        Self {
            discriminant: OpcodeDiscriminant::EnterMethod as u64,
            argument_word_count,
            bootstrap_word_count,
        }
    }

    #[must_use]
    pub fn block_count(self) -> usize {
        op_payload_block_count(self.bootstrap_word_count)
    }
}

impl ReturnControlTemplate {
    #[must_use]
    pub const fn new(result_word_count: usize) -> Self {
        Self {
            discriminant: OpcodeDiscriminant::ReturnControl as u64,
            result_word_count,
        }
    }

    #[must_use]
    pub fn block_count(self) -> usize {
        op_payload_block_count(self.result_word_count)
    }
}

/// Static decoding information for an atomic UTXO constructor import.
///
/// Constructor arguments and the caller-local returned resource are observed
/// together by Neo-Wasm. Arguments use a statically sized, untagged
/// Starstream-value encoding; the final two words are the low/high limbs of
/// the import's `i32` result:
///
/// ```text
/// [NewUtxo, value_0, ..., value_n, resource_lo, resource_hi, padding...]
/// ```
///
/// Process allocation and program/ledger identity are supplied by the replay
/// and transaction contexts rather than committed as host-call fields.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NewUtxoTemplate {
    pub discriminant: u64,
    pub value_word_count: usize,
}

impl NewUtxoTemplate {
    #[must_use]
    pub const fn new(value_word_count: usize) -> Self {
        Self {
            discriminant: OpcodeDiscriminant::NewUtxo as u64,
            value_word_count,
        }
    }

    #[must_use]
    pub fn block_count(self) -> usize {
        new_utxo_block_count(self.value_word_count)
    }
}

/// Static two-block encoding of one advertised SHA-256 method identity.
///
/// The Component Model import carries four `u64` values. Neo-Wasm exposes
/// each as low/high 32-bit limbs:
///
/// ```text
/// first block: [AdvertiseMethod, hash_0_lo, hash_0_hi, ..., hash_3_lo]
/// continuation: [hash_3_hi, 0, ..., 0]
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AdvertiseMethodTemplate {
    pub discriminant: u64,
}

impl AdvertiseMethodTemplate {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            discriminant: OpcodeDiscriminant::AdvertiseMethod as u64,
        }
    }

    #[must_use]
    pub const fn block_count(self) -> usize {
        2
    }
}

impl Default for AdvertiseMethodTemplate {
    fn default() -> Self {
        Self::new()
    }
}

/// Nightstream metadata attached to an absorbed event block.
///
/// It selects the component-local template and lets the projector ensure that
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

/// A semantic event whose current low-level encoding has no arguments.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FixedEvent {
    ClearAbi,
    CoroutineReturn,
    CoordReturn,
}

impl FixedEvent {
    pub const fn default_discriminant(self) -> u64 {
        match self {
            Self::ClearAbi => OpcodeDiscriminant::ClearAbi as u64,
            Self::CoroutineReturn => OpcodeDiscriminant::CoroutineReturn as u64,
            Self::CoordReturn => OpcodeDiscriminant::CoordReturn as u64,
        }
    }

    pub fn into_execution_event(self) -> ExecutionEvent {
        match self {
            Self::ClearAbi => ExecutionEvent::ClearAbi,
            Self::CoroutineReturn => ExecutionEvent::CoroutineReturn,
            Self::CoordReturn => ExecutionEvent::CoordReturn,
        }
    }
}

/// Static encoding for a zero-argument event.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FixedEventTemplate {
    pub discriminant: u64,
    pub event: FixedEvent,
}

impl FixedEventTemplate {
    pub const fn new(event: FixedEvent) -> Self {
        Self {
            discriminant: event.default_discriminant(),
            event,
        }
    }
}

/// A compiler-selected event template.
///
/// Its variant determines how many blocks to consume. Continuation blocks are
/// dense data and deliberately carry no opcode tag of their own.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EventTemplate {
    NewUtxo(NewUtxoTemplate),
    Fixed(FixedEventTemplate),
    AdvertiseMethod(AdvertiseMethodTemplate),
    CallMethod(CallMethodTemplate),
    EnterMethod(EnterMethodTemplate),
    ReturnControl(ReturnControlTemplate),
}

impl EventTemplate {
    pub const fn discriminant(&self) -> u64 {
        match self {
            Self::NewUtxo(template) => template.discriminant,
            Self::Fixed(template) => template.discriminant,
            Self::AdvertiseMethod(template) => template.discriminant,
            Self::CallMethod(template) => template.discriminant,
            Self::EnterMethod(template) => template.discriminant,
            Self::ReturnControl(template) => template.discriminant,
        }
    }

    pub fn block_count(&self) -> usize {
        match self {
            Self::NewUtxo(template) => template.block_count(),
            Self::Fixed(_) => 1,
            Self::AdvertiseMethod(template) => template.block_count(),
            Self::CallMethod(template) => template.block_count(),
            Self::EnterMethod(template) => template.block_count(),
            Self::ReturnControl(template) => template.block_count(),
        }
    }

    fn validate(&self) -> Result<(), BlockCodecError> {
        validate_discriminant(self.discriminant())
    }

    fn decode(&self, blocks: &[AbsorbedBlock]) -> Result<ExecutionEvent, BlockCodecError> {
        match self {
            Self::NewUtxo(template) => decode_new_utxo_blocks(blocks, template),
            Self::Fixed(template) => decode_fixed_blocks(blocks, *template),
            Self::AdvertiseMethod(template) => decode_advertise_method_blocks(blocks, *template),
            Self::CallMethod(template) => decode_call_method_blocks(blocks, *template),
            Self::EnterMethod(template) => decode_enter_method_blocks(blocks, *template),
            Self::ReturnControl(template) => decode_return_control_blocks(blocks, *template),
        }
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
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(
        &mut self,
        attributed_fref: u32,
        template: EventTemplate,
    ) -> Result<(), BlockCodecError> {
        template.validate()?;
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

    /// Decode a complete absorbed-block stream into semantic events.
    ///
    /// Each first block selects a template with
    /// `(attributed_fref, discriminant)`. That template then consumes its
    /// statically known number of blocks.
    pub fn decode_blocks(
        &self,
        blocks: &[AttributedBlock],
    ) -> Result<ExecutionTrace, BlockCodecError> {
        let mut events = Vec::new();
        let mut block_index = 0;

        while block_index < blocks.len() {
            let first = blocks[block_index];
            validate_field(first.words[0], block_index, 0)?;
            let discriminant = first.words[0];
            let attributed_fref = first.metadata.attributed_fref;
            let Some(template) = self
                .templates
                .get(&(attributed_fref, discriminant))
                .cloned()
            else {
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
            events.push(template.decode(&words)?);
            block_index += expected_blocks;
        }

        Ok(ExecutionTrace::new(events))
    }
}

/// Failure to encode, decode, or dispatch an absorbed block assignment.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum BlockCodecError {
    #[error("template discriminant {value} is not a canonical Goldilocks element")]
    NonCanonicalDiscriminant { value: u64 },

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

    #[error("block {block}, word {word}: named argument {name} does not fit in u32: {value}")]
    NamedArgumentOutOfRange {
        block: usize,
        word: usize,
        name: &'static str,
        value: u64,
    },

    #[error("block {block}, word {word}: non-zero unused slot {value}")]
    NonZeroPadding {
        block: usize,
        word: usize,
        value: u64,
    },

    #[error(
        "template expects {expected} value words, but the supplied StarstreamValue contains {actual}"
    )]
    WrongValueWordCount { expected: usize, actual: usize },

    #[error(
        "template expects {expected} result words, but the supplied StarstreamValue contains {actual}"
    )]
    WrongResultWordCount { expected: usize, actual: usize },

    #[error(
        "template expects {expected} internal bootstrap words, but the supplied value contains {actual}"
    )]
    WrongBootstrapWordCount { expected: usize, actual: usize },

    #[error(
        "template already registered for function {attributed_fref} and discriminant {discriminant}"
    )]
    DuplicateTemplate {
        attributed_fref: u32,
        discriminant: u64,
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
}

/// Decode one atomic linked UTXO constructor import.
pub fn decode_new_utxo_blocks(
    blocks: &[AbsorbedBlock],
    template: &NewUtxoTemplate,
) -> Result<ExecutionEvent, BlockCodecError> {
    validate_discriminant(template.discriminant)?;
    let expected_blocks = template.block_count();
    if blocks.len() != expected_blocks {
        return Err(BlockCodecError::WrongBlockCount {
            expected: expected_blocks,
            actual: blocks.len(),
        });
    }
    validate_fields(blocks)?;
    if blocks[0][0] != template.discriminant {
        return Err(BlockCodecError::WrongDiscriminant {
            expected: template.discriminant,
            actual: blocks[0][0],
        });
    }

    let locations = new_utxo_value_locations(template.value_word_count);
    let mut value = Vec::with_capacity(locations.len());
    for (block, word) in locations {
        let raw = blocks[block][word];
        value.push(
            u32::try_from(raw).map_err(|_| BlockCodecError::NamedArgumentOutOfRange {
                block,
                word,
                name: "value",
                value: raw,
            })?,
        );
    }
    let (resource_block, resource_word) = new_utxo_resource_location(template.value_word_count);
    let raw_resource = blocks[resource_block][resource_word];
    let resource =
        u32::try_from(raw_resource).map_err(|_| BlockCodecError::NamedArgumentOutOfRange {
            block: resource_block,
            word: resource_word,
            name: "resource",
            value: raw_resource,
        })?;
    validate_new_utxo_padding(blocks, template.value_word_count)?;

    Ok(ExecutionEvent::NewUtxo {
        arguments: StarstreamValue(value),
        resource: ResourceHandle(resource),
    })
}

/// Encode an atomic constructor import for pinned vectors and grammar tests.
pub fn encode_new_utxo_blocks(
    value: &StarstreamValue,
    resource: ResourceHandle,
    template: NewUtxoTemplate,
) -> Result<Vec<AbsorbedBlock>, BlockCodecError> {
    validate_discriminant(template.discriminant)?;
    if value.0.len() != template.value_word_count {
        return Err(BlockCodecError::WrongValueWordCount {
            expected: template.value_word_count,
            actual: value.0.len(),
        });
    }

    let mut blocks = vec![[0; ABSORBED_BLOCK_WORDS]; template.block_count()];
    blocks[0][0] = template.discriminant;
    for ((block, word), &limb) in new_utxo_value_locations(template.value_word_count)
        .into_iter()
        .zip(&value.0)
    {
        blocks[block][word] = u64::from(limb);
    }
    let (resource_block, resource_word) = new_utxo_resource_location(template.value_word_count);
    blocks[resource_block][resource_word] = u64::from(resource.0);
    Ok(blocks)
}

/// Decode one logical method call using its statically derived template.
pub fn decode_call_method_blocks(
    blocks: &[AbsorbedBlock],
    template: CallMethodTemplate,
) -> Result<ExecutionEvent, BlockCodecError> {
    validate_template(template)?;
    let expected_blocks = template.block_count();
    if blocks.len() != expected_blocks {
        return Err(BlockCodecError::WrongBlockCount {
            expected: expected_blocks,
            actual: blocks.len(),
        });
    }
    validate_fields(blocks)?;

    if blocks[0][0] != template.discriminant {
        return Err(BlockCodecError::WrongDiscriminant {
            expected: template.discriminant,
            actual: blocks[0][0],
        });
    }

    let resource =
        u32::try_from(blocks[0][1]).map_err(|_| BlockCodecError::NamedArgumentOutOfRange {
            block: 0,
            word: 1,
            name: "resource",
            value: blocks[0][1],
        })?;

    let locations = value_locations(template.value_word_count);
    let mut value = Vec::with_capacity(locations.len());
    for (block, word) in locations {
        let raw = blocks[block][word];
        value.push(
            u32::try_from(raw).map_err(|_| BlockCodecError::NamedArgumentOutOfRange {
                block,
                word,
                name: "value",
                value: raw,
            })?,
        );
    }
    let result_locations =
        call_result_locations(template.value_word_count, template.result_word_count);
    let mut result = Vec::with_capacity(result_locations.len());
    for (block, word) in result_locations {
        let raw = blocks[block][word];
        result.push(
            u32::try_from(raw).map_err(|_| BlockCodecError::NamedArgumentOutOfRange {
                block,
                word,
                name: "result",
                value: raw,
            })?,
        );
    }
    validate_call_method_padding(blocks, template)?;

    Ok(ExecutionEvent::CallMethod {
        resource: ResourceHandle(resource),
        method: template.method,
        arguments: StarstreamValue(value),
        result: StarstreamValue(result),
    })
}

/// Encode one method call according to the same static template.
///
/// This is intended for pinned vectors and template conformance tests. Runtime
/// grammar expansion should produce these words directly.
pub fn encode_call_method_blocks(
    resource: ResourceHandle,
    value: &StarstreamValue,
    result: &StarstreamValue,
    template: CallMethodTemplate,
) -> Result<Vec<AbsorbedBlock>, BlockCodecError> {
    validate_template(template)?;
    if value.0.len() != template.value_word_count {
        return Err(BlockCodecError::WrongValueWordCount {
            expected: template.value_word_count,
            actual: value.0.len(),
        });
    }
    if result.0.len() != template.result_word_count {
        return Err(BlockCodecError::WrongResultWordCount {
            expected: template.result_word_count,
            actual: result.0.len(),
        });
    }

    let mut blocks = vec![[0; ABSORBED_BLOCK_WORDS]; template.block_count()];
    blocks[0][0] = template.discriminant;
    blocks[0][1] = u64::from(resource.0);
    for ((block, word), &limb) in value_locations(template.value_word_count)
        .into_iter()
        .zip(&value.0)
    {
        blocks[block][word] = u64::from(limb);
    }
    for ((block, word), &limb) in
        call_result_locations(template.value_word_count, template.result_word_count)
            .into_iter()
            .zip(&result.0)
    {
        blocks[block][word] = u64::from(limb);
    }
    Ok(blocks)
}

/// Decode the committed user-argument prefix of a method export entry.
pub fn decode_enter_method_blocks(
    blocks: &[AbsorbedBlock],
    template: EnterMethodTemplate,
) -> Result<ExecutionEvent, BlockCodecError> {
    validate_discriminant(template.discriminant)?;
    if template.argument_word_count > template.bootstrap_word_count {
        return Err(BlockCodecError::WrongBootstrapWordCount {
            expected: template.argument_word_count,
            actual: template.bootstrap_word_count,
        });
    }
    if blocks.len() != template.block_count() {
        return Err(BlockCodecError::WrongBlockCount {
            expected: template.block_count(),
            actual: blocks.len(),
        });
    }
    validate_fields(blocks)?;
    if blocks[0][0] != template.discriminant {
        return Err(BlockCodecError::WrongDiscriminant {
            expected: template.discriminant,
            actual: blocks[0][0],
        });
    }

    let locations = op_value_locations(template.bootstrap_word_count);
    let mut arguments = Vec::with_capacity(template.argument_word_count);
    for (index, (block, word)) in locations.into_iter().enumerate() {
        let raw = blocks[block][word];
        let limb = u32::try_from(raw).map_err(|_| BlockCodecError::NamedArgumentOutOfRange {
            block,
            word,
            name: "method entry bootstrap",
            value: raw,
        })?;
        if index < template.argument_word_count {
            arguments.push(limb);
        }
    }
    validate_op_padding(blocks, template.bootstrap_word_count)?;

    Ok(ExecutionEvent::EnterMethod {
        arguments: StarstreamValue(arguments),
    })
}

/// Encode a method entry for pinned vectors and template conformance tests.
pub fn encode_enter_method_blocks(
    arguments: &StarstreamValue,
    internal_bootstrap: &StarstreamValue,
    template: EnterMethodTemplate,
) -> Result<Vec<AbsorbedBlock>, BlockCodecError> {
    validate_discriminant(template.discriminant)?;
    if arguments.0.len() != template.argument_word_count {
        return Err(BlockCodecError::WrongValueWordCount {
            expected: template.argument_word_count,
            actual: arguments.0.len(),
        });
    }
    let expected_internal = template
        .bootstrap_word_count
        .checked_sub(template.argument_word_count)
        .ok_or(BlockCodecError::WrongBootstrapWordCount {
            expected: template.argument_word_count,
            actual: template.bootstrap_word_count,
        })?;
    if internal_bootstrap.0.len() != expected_internal {
        return Err(BlockCodecError::WrongBootstrapWordCount {
            expected: expected_internal,
            actual: internal_bootstrap.0.len(),
        });
    }

    let mut blocks = vec![[0; ABSORBED_BLOCK_WORDS]; template.block_count()];
    blocks[0][0] = template.discriminant;
    let values = arguments.0.iter().chain(&internal_bootstrap.0);
    for ((block, word), &limb) in op_value_locations(template.bootstrap_word_count)
        .into_iter()
        .zip(values)
    {
        blocks[block][word] = u64::from(limb);
    }
    Ok(blocks)
}

/// Decode the value published at a UTXO export boundary.
pub fn decode_return_control_blocks(
    blocks: &[AbsorbedBlock],
    template: ReturnControlTemplate,
) -> Result<ExecutionEvent, BlockCodecError> {
    validate_discriminant(template.discriminant)?;
    if blocks.len() != template.block_count() {
        return Err(BlockCodecError::WrongBlockCount {
            expected: template.block_count(),
            actual: blocks.len(),
        });
    }
    validate_fields(blocks)?;
    if blocks[0][0] != template.discriminant {
        return Err(BlockCodecError::WrongDiscriminant {
            expected: template.discriminant,
            actual: blocks[0][0],
        });
    }

    let locations = op_value_locations(template.result_word_count);
    let mut result = Vec::with_capacity(locations.len());
    for (block, word) in locations {
        let raw = blocks[block][word];
        result.push(
            u32::try_from(raw).map_err(|_| BlockCodecError::NamedArgumentOutOfRange {
                block,
                word,
                name: "result",
                value: raw,
            })?,
        );
    }
    validate_op_padding(blocks, template.result_word_count)?;

    Ok(ExecutionEvent::ReturnControl {
        result: StarstreamValue(result),
    })
}

/// Encode a UTXO export result for pinned vectors and template tests.
pub fn encode_return_control_blocks(
    result: &StarstreamValue,
    template: ReturnControlTemplate,
) -> Result<Vec<AbsorbedBlock>, BlockCodecError> {
    validate_discriminant(template.discriminant)?;
    if result.0.len() != template.result_word_count {
        return Err(BlockCodecError::WrongResultWordCount {
            expected: template.result_word_count,
            actual: result.0.len(),
        });
    }

    let mut blocks = vec![[0; ABSORBED_BLOCK_WORDS]; template.block_count()];
    blocks[0][0] = template.discriminant;
    for ((block, word), &limb) in op_value_locations(template.result_word_count)
        .into_iter()
        .zip(&result.0)
    {
        blocks[block][word] = u64::from(limb);
    }
    Ok(blocks)
}

/// Decode an `implements-method` call using its fixed four-`u64` interface.
pub fn decode_advertise_method_blocks(
    blocks: &[AbsorbedBlock],
    template: AdvertiseMethodTemplate,
) -> Result<ExecutionEvent, BlockCodecError> {
    if blocks.len() != template.block_count() {
        return Err(BlockCodecError::WrongBlockCount {
            expected: template.block_count(),
            actual: blocks.len(),
        });
    }

    validate_discriminant(template.discriminant)?;
    validate_fields(blocks)?;
    if blocks[0][0] != template.discriminant {
        return Err(BlockCodecError::WrongDiscriminant {
            expected: template.discriminant,
            actual: blocks[0][0],
        });
    }

    let locations = (1..ABSORBED_BLOCK_WORDS)
        .map(|word| (0, word))
        .chain([(1, 0)]);
    let mut hash_words = [0_u32; 8];
    for (output, (block, word)) in hash_words.iter_mut().zip(locations) {
        *output = u32::try_from(blocks[block][word]).map_err(|_| {
            BlockCodecError::NamedArgumentOutOfRange {
                block,
                word,
                name: "method",
                value: blocks[block][word],
            }
        })?;
    }
    for (word, &value) in blocks[1].iter().enumerate().skip(1) {
        if value != 0 {
            return Err(BlockCodecError::NonZeroPadding {
                block: 1,
                word,
                value,
            });
        }
    }

    let mut method = [0_u64; 4];
    for (limb, words) in method.iter_mut().zip(hash_words.chunks_exact(2)) {
        *limb = u64::from(words[0]) | (u64::from(words[1]) << 32);
    }
    Ok(ExecutionEvent::AdvertiseMethod {
        method: MethodHash(method),
    })
}

fn decode_fixed_blocks(
    blocks: &[AbsorbedBlock],
    template: FixedEventTemplate,
) -> Result<ExecutionEvent, BlockCodecError> {
    if blocks.len() != 1 {
        return Err(BlockCodecError::WrongBlockCount {
            expected: 1,
            actual: blocks.len(),
        });
    }

    validate_discriminant(template.discriminant)?;
    validate_fields(blocks)?;
    if blocks[0][0] != template.discriminant {
        return Err(BlockCodecError::WrongDiscriminant {
            expected: template.discriminant,
            actual: blocks[0][0],
        });
    }

    for (word, &value) in blocks[0].iter().enumerate().skip(1) {
        if value != 0 {
            return Err(BlockCodecError::NonZeroPadding {
                block: 0,
                word,
                value,
            });
        }
    }

    Ok(template.event.into_execution_event())
}

fn validate_template(template: CallMethodTemplate) -> Result<(), BlockCodecError> {
    validate_discriminant(template.discriminant)
}

fn validate_discriminant(discriminant: u64) -> Result<(), BlockCodecError> {
    if discriminant >= GOLDILOCKS_ORDER {
        return Err(BlockCodecError::NonCanonicalDiscriminant {
            value: discriminant,
        });
    }
    Ok(())
}

fn validate_field(value: u64, block: usize, word: usize) -> Result<(), BlockCodecError> {
    if value >= GOLDILOCKS_ORDER {
        return Err(BlockCodecError::NonCanonicalField { block, word, value });
    }
    Ok(())
}

fn validate_fields(blocks: &[AbsorbedBlock]) -> Result<(), BlockCodecError> {
    for (block_index, block) in blocks.iter().enumerate() {
        for (word_index, &value) in block.iter().enumerate() {
            validate_field(value, block_index, word_index)?;
        }
    }
    Ok(())
}

fn payload_block_count(value_word_count: usize) -> usize {
    1 + value_word_count
        .saturating_sub(OPCODE_ARG_WORDS - 1)
        .div_ceil(ABSORBED_BLOCK_WORDS)
}

fn result_block_count(result_word_count: usize) -> usize {
    result_word_count.div_ceil(ABSORBED_BLOCK_WORDS)
}

fn op_payload_block_count(value_word_count: usize) -> usize {
    1 + value_word_count
        .saturating_sub(OPCODE_ARG_WORDS)
        .div_ceil(ABSORBED_BLOCK_WORDS)
}

fn new_utxo_block_count(value_word_count: usize) -> usize {
    value_word_count
        .saturating_add(3)
        .div_ceil(ABSORBED_BLOCK_WORDS)
}

fn value_locations(count: usize) -> Vec<(usize, usize)> {
    let first_capacity = OPCODE_ARG_WORDS - 1;
    (0..count)
        .map(|index| {
            if index < first_capacity {
                (0, index + 2)
            } else {
                let continuation_index = index - first_capacity;
                (
                    1 + continuation_index / ABSORBED_BLOCK_WORDS,
                    continuation_index % ABSORBED_BLOCK_WORDS,
                )
            }
        })
        .collect()
}

fn call_result_locations(value_word_count: usize, result_word_count: usize) -> Vec<(usize, usize)> {
    let first_block = payload_block_count(value_word_count);
    (0..result_word_count)
        .map(|index| {
            (
                first_block + index / ABSORBED_BLOCK_WORDS,
                index % ABSORBED_BLOCK_WORDS,
            )
        })
        .collect()
}

fn op_value_locations(count: usize) -> Vec<(usize, usize)> {
    (0..count)
        .map(|index| {
            if index < OPCODE_ARG_WORDS {
                (0, index + 1)
            } else {
                let continuation_index = index - OPCODE_ARG_WORDS;
                (
                    1 + continuation_index / ABSORBED_BLOCK_WORDS,
                    continuation_index % ABSORBED_BLOCK_WORDS,
                )
            }
        })
        .collect()
}

fn dense_location(offset: usize) -> (usize, usize) {
    (offset / ABSORBED_BLOCK_WORDS, offset % ABSORBED_BLOCK_WORDS)
}

fn new_utxo_value_locations(count: usize) -> Vec<(usize, usize)> {
    (0..count).map(|index| dense_location(index + 1)).collect()
}

fn new_utxo_resource_location(value_word_count: usize) -> (usize, usize) {
    dense_location(value_word_count + 1)
}

fn validate_new_utxo_padding(
    blocks: &[AbsorbedBlock],
    value_word_count: usize,
) -> Result<(), BlockCodecError> {
    let mut used = vec![[false; ABSORBED_BLOCK_WORDS]; blocks.len()];
    used[0][0] = true;
    for (block, word) in new_utxo_value_locations(value_word_count) {
        used[block][word] = true;
    }
    let (resource_block, resource_word) = new_utxo_resource_location(value_word_count);
    used[resource_block][resource_word] = true;
    // The following slot is ResultElem::Hi. Constructor imports return i32,
    // so it must be zero and is deliberately validated as padding.

    validate_unused_slots(blocks, &used)
}

fn validate_call_method_padding(
    blocks: &[AbsorbedBlock],
    template: CallMethodTemplate,
) -> Result<(), BlockCodecError> {
    let mut used = vec![[false; ABSORBED_BLOCK_WORDS]; blocks.len()];
    used[0][0] = true;
    used[0][1] = true;
    for (block, word) in value_locations(template.value_word_count) {
        used[block][word] = true;
    }
    for (block, word) in
        call_result_locations(template.value_word_count, template.result_word_count)
    {
        used[block][word] = true;
    }

    validate_unused_slots(blocks, &used)
}

fn validate_op_padding(
    blocks: &[AbsorbedBlock],
    value_word_count: usize,
) -> Result<(), BlockCodecError> {
    let mut used = vec![[false; ABSORBED_BLOCK_WORDS]; blocks.len()];
    used[0][0] = true;
    for (block, word) in op_value_locations(value_word_count) {
        used[block][word] = true;
    }
    validate_unused_slots(blocks, &used)
}

fn validate_unused_slots(
    blocks: &[AbsorbedBlock],
    used: &[[bool; ABSORBED_BLOCK_WORDS]],
) -> Result<(), BlockCodecError> {
    for (block_index, block) in blocks.iter().enumerate() {
        for (word_index, &value) in block.iter().enumerate() {
            if !used[block_index][word_index] && value != 0 {
                return Err(BlockCodecError::NonZeroPadding {
                    block: block_index,
                    word: word_index,
                    value,
                });
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn method() -> MethodHash {
        MethodHash([1, 2, 3, 4])
    }

    #[test]
    fn constructor_import_is_one_atomic_event() {
        let template = NewUtxoTemplate::new(3);
        let arguments = StarstreamValue(vec![10, 11, 12]);
        let blocks = encode_new_utxo_blocks(&arguments, ResourceHandle(7), template)
            .expect("constructor import encodes");

        assert_eq!(blocks, [[2, 10, 11, 12, 7, 0, 0, 0]]);
        assert_eq!(
            decode_new_utxo_blocks(&blocks, &template).expect("constructor import decodes"),
            ExecutionEvent::NewUtxo {
                arguments,
                resource: ResourceHandle(7),
            }
        );
    }

    #[test]
    fn constructor_result_follows_its_statically_sized_arguments() {
        let template = NewUtxoTemplate::new(7);
        let arguments = StarstreamValue((0..7).collect());
        let blocks = encode_new_utxo_blocks(&arguments, ResourceHandle(9), template)
            .expect("constructor import encodes");

        assert_eq!(blocks, [[2, 0, 1, 2, 3, 4, 5, 6], [9, 0, 0, 0, 0, 0, 0, 0]]);
        assert_eq!(
            decode_new_utxo_blocks(&blocks, &template).expect("constructor import decodes"),
            ExecutionEvent::NewUtxo {
                arguments,
                resource: ResourceHandle(9),
            }
        );

        let mut invalid = blocks;
        invalid[1][1] = 1;
        assert!(matches!(
            decode_new_utxo_blocks(&invalid, &template),
            Err(BlockCodecError::NonZeroPadding {
                block: 1,
                word: 1,
                value: 1,
            })
        ));
    }

    #[test]
    fn one_block_call_has_named_resource_and_no_length_tag() {
        let template = CallMethodTemplate::new(method(), 3, 0);
        let value = StarstreamValue(vec![10, 11, 12]);
        let blocks = encode_call_method_blocks(
            ResourceHandle(7),
            &value,
            &StarstreamValue::default(),
            template,
        )
        .expect("call encodes");

        assert_eq!(blocks, vec![[7, 7, 10, 11, 12, 0, 0, 0]]);
        assert_eq!(
            decode_call_method_blocks(&blocks, template).expect("call decodes"),
            ExecutionEvent::CallMethod {
                resource: ResourceHandle(7),
                method: method(),
                arguments: value,
                result: StarstreamValue::default(),
            }
        );
    }

    #[test]
    fn flat_method_result_is_a_static_continuation() {
        let template = CallMethodTemplate::new(method(), 1, 2);
        let arguments = StarstreamValue(vec![10]);
        let result = StarstreamValue(vec![41, 9]);
        let blocks = encode_call_method_blocks(ResourceHandle(7), &arguments, &result, template)
            .expect("call encodes");

        assert_eq!(
            blocks,
            vec![[7, 7, 10, 0, 0, 0, 0, 0], [41, 9, 0, 0, 0, 0, 0, 0]]
        );
        assert_eq!(
            decode_call_method_blocks(&blocks, template).expect("call decodes"),
            ExecutionEvent::CallMethod {
                resource: ResourceHandle(7),
                method: method(),
                arguments,
                result,
            }
        );
    }

    #[test]
    fn return_control_publishes_the_flat_method_result() {
        let template = ReturnControlTemplate::new(2);
        let result = StarstreamValue(vec![41, 9]);
        let blocks = encode_return_control_blocks(&result, template).expect("return encodes");

        assert_eq!(blocks, vec![[6, 41, 9, 0, 0, 0, 0, 0]]);
        assert_eq!(
            decode_return_control_blocks(&blocks, template).expect("return decodes"),
            ExecutionEvent::ReturnControl { result }
        );
    }

    #[test]
    fn method_entry_exposes_arguments_but_not_internal_bootstrap_words() {
        let template = EnterMethodTemplate::new(2, 4);
        let arguments = StarstreamValue(vec![11, 12]);
        let internal = StarstreamValue(vec![7, 0]);
        let blocks = encode_enter_method_blocks(&arguments, &internal, template)
            .expect("method entry encodes");

        assert_eq!(blocks, [[11, 11, 12, 7, 0, 0, 0, 0]]);
        assert_eq!(
            decode_enter_method_blocks(&blocks, template).expect("method entry decodes"),
            ExecutionEvent::EnterMethod { arguments }
        );
    }

    #[test]
    fn static_value_width_determines_untagged_continuations() {
        let template = CallMethodTemplate::new(method(), 16, 0);
        let value = StarstreamValue((0..16).collect());
        let blocks = encode_call_method_blocks(
            ResourceHandle(9),
            &value,
            &StarstreamValue::default(),
            template,
        )
        .expect("call encodes");

        assert_eq!(blocks.len(), 3);
        assert_eq!(blocks[0], [7, 9, 0, 1, 2, 3, 4, 5]);
        assert_eq!(blocks[1], [6, 7, 8, 9, 10, 11, 12, 13]);
        assert_eq!(blocks[2], [14, 15, 0, 0, 0, 0, 0, 0]);
        assert_eq!(
            decode_call_method_blocks(&blocks, template).expect("call decodes"),
            ExecutionEvent::CallMethod {
                resource: ResourceHandle(9),
                method: method(),
                arguments: value,
                result: StarstreamValue::default(),
            }
        );
    }

    #[test]
    fn rejects_the_wrong_number_of_blocks_for_the_static_type() {
        let template = CallMethodTemplate::new(method(), 16, 0);
        let value = StarstreamValue((0..16).collect());
        let mut blocks = encode_call_method_blocks(
            ResourceHandle(9),
            &value,
            &StarstreamValue::default(),
            template,
        )
        .expect("call encodes");
        blocks.pop();

        assert!(matches!(
            decode_call_method_blocks(&blocks, template),
            Err(BlockCodecError::WrongBlockCount {
                expected: 3,
                actual: 2
            })
        ));
    }

    #[test]
    fn rejects_nonzero_unused_slots() {
        let template = CallMethodTemplate::new(method(), 1, 0);
        let value = StarstreamValue(vec![10]);
        let mut blocks = encode_call_method_blocks(
            ResourceHandle(7),
            &value,
            &StarstreamValue::default(),
            template,
        )
        .expect("call encodes");
        blocks[0][7] = 99;

        assert!(matches!(
            decode_call_method_blocks(&blocks, template),
            Err(BlockCodecError::NonZeroPadding {
                block: 0,
                word: 7,
                value: 99
            })
        ));
    }

    #[test]
    fn rejects_named_arguments_that_do_not_fit_the_wasm_encoding() {
        let template = CallMethodTemplate::new(method(), 1, 0);
        let mut blocks = vec![[7, 7, u64::from(u32::MAX) + 1, 0, 0, 0, 0, 0]];

        assert!(matches!(
            decode_call_method_blocks(&blocks, template),
            Err(BlockCodecError::NamedArgumentOutOfRange {
                block: 0,
                word: 2,
                name: "value",
                ..
            })
        ));

        blocks[0][2] = 0;
        blocks[0][1] = u64::from(u32::MAX) + 1;
        assert!(matches!(
            decode_call_method_blocks(&blocks, template),
            Err(BlockCodecError::NamedArgumentOutOfRange {
                block: 0,
                word: 1,
                name: "resource",
                ..
            })
        ));
    }

    #[test]
    fn advertises_a_four_u64_method_hash_across_two_static_blocks() {
        let template = AdvertiseMethodTemplate::new();
        let blocks = [[5, 1, 0, 2, 0, 3, 0, 4], [0; 8]];

        assert_eq!(
            decode_advertise_method_blocks(&blocks, template).expect("method decodes"),
            ExecutionEvent::AdvertiseMethod { method: method() }
        );
    }

    #[test]
    fn registry_uses_static_width_before_dispatching_the_next_event() {
        let call_template = CallMethodTemplate::new(method(), 10, 0);
        let mut registry = TemplateRegistry::new();
        registry
            .register(12, EventTemplate::CallMethod(call_template))
            .expect("call template registers");
        registry
            .register(
                12,
                EventTemplate::ReturnControl(ReturnControlTemplate::new(0)),
            )
            .expect("return-control template registers");

        let value = StarstreamValue((0..10).collect());
        let mut blocks = encode_call_method_blocks(
            ResourceHandle(3),
            &value,
            &StarstreamValue::default(),
            call_template,
        )
        .expect("call encodes")
        .into_iter()
        .map(|words| AttributedBlock::new(words, 12, 40))
        .collect::<Vec<_>>();
        // The continuation begins with 6, the ReturnControl discriminant. The
        // selected CallMethod template still consumes it as value data.
        assert_eq!(blocks[1].words[0], OpcodeDiscriminant::ReturnControl as u64);
        blocks.push(AttributedBlock::new(
            [
                OpcodeDiscriminant::ReturnControl as u64,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
            ],
            12,
            40,
        ));

        assert_eq!(
            registry.decode_blocks(&blocks).expect("stream decodes"),
            ExecutionTrace::new([
                ExecutionEvent::CallMethod {
                    resource: ResourceHandle(3),
                    method: method(),
                    arguments: value,
                    result: StarstreamValue::default(),
                },
                ExecutionEvent::ReturnControl {
                    result: StarstreamValue::default(),
                },
            ])
        );
    }

    #[test]
    fn registry_key_is_function_and_first_block_discriminant() {
        let mut registry = TemplateRegistry::new();
        registry
            .register(
                21,
                EventTemplate::ReturnControl(ReturnControlTemplate::new(0)),
            )
            .expect("return-control template registers");
        registry
            .register(
                21,
                EventTemplate::Fixed(FixedEventTemplate::new(FixedEvent::CoroutineReturn)),
            )
            .expect("coroutine-return template registers for same function");
        registry
            .register(
                22,
                EventTemplate::ReturnControl(ReturnControlTemplate::new(0)),
            )
            .expect("same opcode registers for another function");

        let blocks = [
            AttributedBlock::new([6, 0, 0, 0, 0, 0, 0, 0], 21, 50),
            AttributedBlock::new([9, 0, 0, 0, 0, 0, 0, 0], 21, 50),
            AttributedBlock::new([6, 0, 0, 0, 0, 0, 0, 0], 22, 51),
        ];
        assert_eq!(
            registry.decode_blocks(&blocks).expect("stream decodes"),
            ExecutionTrace::new([
                ExecutionEvent::ReturnControl {
                    result: StarstreamValue::default(),
                },
                ExecutionEvent::CoroutineReturn,
                ExecutionEvent::ReturnControl {
                    result: StarstreamValue::default(),
                },
            ])
        );
    }

    #[test]
    fn registry_rejects_duplicate_and_unknown_templates() {
        let template = EventTemplate::ReturnControl(ReturnControlTemplate::new(0));
        let mut registry = TemplateRegistry::new();
        registry
            .register(4, template.clone())
            .expect("template registers");

        assert!(matches!(
            registry.register(4, template),
            Err(BlockCodecError::DuplicateTemplate {
                attributed_fref: 4,
                discriminant: 6,
            })
        ));
        assert!(matches!(
            registry.decode_blocks(&[AttributedBlock::new([6, 0, 0, 0, 0, 0, 0, 0], 5, 9,)]),
            Err(BlockCodecError::UnknownTemplate {
                block: 0,
                attributed_fref: 5,
                discriminant: 6,
            })
        ));
    }

    #[test]
    fn registry_rejects_truncated_static_events() {
        let template = CallMethodTemplate::new(method(), 16, 0);
        let mut registry = TemplateRegistry::new();
        registry
            .register(4, EventTemplate::CallMethod(template))
            .expect("template registers");
        let value = StarstreamValue((0..16).collect());
        let blocks = encode_call_method_blocks(
            ResourceHandle(2),
            &value,
            &StarstreamValue::default(),
            template,
        )
        .expect("call encodes")
        .into_iter()
        .take(2)
        .map(|words| AttributedBlock::new(words, 4, 9))
        .collect::<Vec<_>>();

        assert!(matches!(
            registry.decode_blocks(&blocks),
            Err(BlockCodecError::TruncatedTemplate {
                block: 0,
                expected_blocks: 3,
                available_blocks: 2,
                ..
            })
        ));
    }

    #[test]
    fn registry_rejects_metadata_changes_inside_an_event() {
        let template = CallMethodTemplate::new(method(), 10, 0);
        let mut registry = TemplateRegistry::new();
        registry
            .register(4, EventTemplate::CallMethod(template))
            .expect("template registers");
        let value = StarstreamValue((0..10).collect());
        let encoded = encode_call_method_blocks(
            ResourceHandle(2),
            &value,
            &StarstreamValue::default(),
            template,
        )
        .expect("call encodes");

        let wrong_attribution = [
            AttributedBlock::new(encoded[0], 4, 9),
            AttributedBlock::new(encoded[1], 5, 9),
        ];
        assert!(matches!(
            registry.decode_blocks(&wrong_attribution),
            Err(BlockCodecError::AttributionChanged {
                block: 1,
                expected: 4,
                actual: 5,
            })
        ));

        let wrong_turn = [
            AttributedBlock::new(encoded[0], 4, 9),
            AttributedBlock::new(encoded[1], 4, 10),
        ];
        assert!(matches!(
            registry.decode_blocks(&wrong_turn),
            Err(BlockCodecError::TurnChanged {
                block: 1,
                expected: 9,
                actual: 10,
            })
        ));
    }
}
