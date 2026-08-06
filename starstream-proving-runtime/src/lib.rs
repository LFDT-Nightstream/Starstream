//! Bridge between Starstream's semantic execution trace and the host-event
//! grammar emitted by the Wasm tracing adapter.
//!
//! `starstream-interleaving-spec` deliberately does not depend on `neo-wasm`. This
//! crate owns the paired construction of:
//!
//! - the [`neo_wasm::event_grammar::HostEventGrammar`] that emits committed
//!   blocks; and
//! - the [`starstream_interleaving_spec::nightstream::TemplateRegistry`] that
//!   decodes those blocks into semantic events.
//!
//! The initial integration covers compiler-emitted ABI publication calls,
//! imported Starstream ABI method calls, and UTXO constructors. Other imports
//! remain advice-only until their semantic opcode assignments are added.

mod runtime;

pub use runtime::{TracedContract, WasmTraceHost, new_tracing_wasmtime_store, new_wasmtime_config};

use std::collections::BTreeSet;

use neo_wasm::comm_chain::COMM_CHAIN_BLOCK_WORDS;
use neo_wasm::event_grammar::{
    ExportTemplate, GrammarEvent, HostEventGrammar, ImportTemplate, Limb, SlotSource,
};
use sha2::{Digest as _, Sha256};
use starstream_interleaving_spec::nightstream::{
    AdvertiseMethodTemplate, AttributedBlock, BlockCodecError, CallMethodTemplate,
    EnterMethodTemplate, EventTemplate, FixedEvent, FixedEventTemplate, NewUtxoTemplate,
    OpcodeDiscriminant, ReturnControlTemplate, TemplateRegistry,
};
use starstream_interleaving_spec::{ExecutionTrace, MethodHash};
use wasmparser::{CompositeInnerType, Parser, Payload, TypeRef, ValType};

#[derive(Clone, Debug)]
pub struct ComponentTemplates {
    pub grammar: HostEventGrammar,
    pub decoder: TemplateRegistry,
}

#[derive(Debug, thiserror::Error)]
pub enum TemplateBuildError {
    #[error("failed to parse core Wasm: {0}")]
    Wasm(#[from] wasmparser::BinaryReaderError),

    #[error("component contains no embedded core module")]
    MissingCoreModule,

    #[error("embedded core module range {0:?} is outside the component")]
    InvalidCoreModuleRange(std::ops::Range<usize>),

    #[error("core type {0} is not a function type")]
    UnsupportedCoreType(usize),

    #[error("function import `{module}`.`{field}` references missing type {type_index}")]
    MissingFunctionType {
        module: String,
        field: String,
        type_index: u32,
    },

    #[error("function import `{module}`.`{field}` has too many parameters or results")]
    FunctionArityOverflow { module: String, field: String },

    #[error("import `{module}`.`{field}` has {count} results; at most one is supported")]
    UnsupportedResultCount {
        module: String,
        field: String,
        count: usize,
    },

    #[error("method import `{module}`.`{field}` does not start with an i32 resource handle")]
    InvalidMethodReceiver { module: String, field: String },

    #[error(
        "method import `{module}`.`{field}` has unsupported flattened parameter type {ty:?} at index {index}"
    )]
    UnsupportedMethodParameter {
        module: String,
        field: String,
        index: usize,
        ty: ValType,
    },

    #[error(
        "method import `{module}`.`{field}` has unsupported flat result types {results:?}; \
         only no result or one i32/i64 result is supported until opaque values are available"
    )]
    UnsupportedMethodResult {
        module: String,
        field: String,
        results: Box<[ValType]>,
    },

    #[error(
        "UTXO method export `{name}` has unsupported flat result types {results:?}; \
         only no result or one i32/i64 result is supported until opaque values are available"
    )]
    UnsupportedMethodExportResult {
        name: String,
        results: Box<[ValType]>,
    },

    #[error("UTXO method export `{name}` does not start with an i32 resource receiver")]
    InvalidMethodExportReceiver { name: String },

    #[error(
        "UTXO method export `{name}` has unsupported flattened {kind} type {ty:?} at local {index}"
    )]
    UnsupportedMethodExportLocal {
        name: String,
        kind: &'static str,
        index: usize,
        ty: ValType,
    },

    #[error("function export `{name}` has too many locals or flattened input limbs")]
    ExportArityOverflow { name: String },

    #[error("defined function export `{name}` has no matching code body")]
    MissingExportBody { name: String },

    #[error("function export `{name}` references missing function index {index}")]
    MissingExportFunction { name: String, index: u32 },

    #[error("function export `{name}` references missing type {type_index}")]
    MissingExportFunctionType { name: String, type_index: u32 },

    #[error("invalid host-event export template for `{name}`: {message}")]
    InvalidExportGrammar { name: String, message: String },

    #[error("method import name `{0}` has no method segment")]
    InvalidMethodName(String),

    #[error(
        "constructor import `{module}`.`{field}` must return exactly one i32 resource handle, \
         found {results:?}"
    )]
    InvalidConstructorResult {
        module: String,
        field: String,
        results: Box<[ValType]>,
    },

    #[error(
        "constructor import `{module}`.`{field}` has unsupported flattened parameter type {ty:?} \
         at index {index}"
    )]
    UnsupportedConstructorParameter {
        module: String,
        field: String,
        index: usize,
        ty: ValType,
    },

    #[error(
        "builtin import `{module}`.`{field}` has unexpected core signature {params:?} -> {results:?}"
    )]
    InvalidBuiltinSignature {
        module: String,
        field: String,
        params: Box<[ValType]>,
        results: Box<[ValType]>,
    },

    #[error("invalid semantic decoder template: {0}")]
    Decoder(#[from] BlockCodecError),

    #[error("invalid host-event import template for `{module}`.`{field}`: {message}")]
    InvalidGrammar {
        module: String,
        field: String,
        message: String,
    },

    #[error(
        "plain function export `{name}` is not classified; add a coordination-script allowlist \
         entry or an explicit export mapping"
    )]
    UnclassifiedFunctionExport { name: String },

    #[error(
        "coordination-script export `{name}` was allowlisted, but no matching function export exists"
    )]
    MissingCoordinationExport { name: String },
}

/// Build matching emitter and decoder templates from a core Wasm module or a
/// component containing one core module.
///
/// Every coordination script must be named explicitly. Compiler-shaped UTXO
/// constructor and method exports are classified automatically; an unknown
/// plain function export is rejected rather than silently producing a
/// `CoordReturn`.
pub fn build_component_templates(
    wasm: &[u8],
    coordination_exports: &[&str],
) -> Result<ComponentTemplates, TemplateBuildError> {
    let module = first_core_module(wasm)?;
    let imports = parse_function_imports(module)?;
    let exports = parse_function_exports(module)?;
    let coordination_exports = coordination_exports
        .iter()
        .copied()
        .collect::<BTreeSet<_>>();
    let mut matched_coordination_exports = BTreeSet::new();
    let mut grammar = HostEventGrammar::default();
    let mut decoder = TemplateRegistry::new();

    for export in &exports {
        let fref = export.index.saturating_add(1);
        let (template, semantic) = build_export_template(export, &coordination_exports)?;
        let local_bound =
            u8::try_from(export.params.len() + export.locals.len()).map_err(|_| {
                TemplateBuildError::ExportArityOverflow {
                    name: export.name.clone(),
                }
            })?;
        template.validate(local_bound).map_err(|error| {
            TemplateBuildError::InvalidExportGrammar {
                name: export.name.clone(),
                message: error.to_string(),
            }
        })?;
        grammar.exports.insert(fref, template);
        for event in semantic {
            if matches!(
                &event,
                EventTemplate::Fixed(FixedEventTemplate {
                    event: FixedEvent::CoordReturn,
                    ..
                })
            ) {
                matched_coordination_exports.insert(export.name.as_str());
            }
            decoder.register(fref, event)?;
        }
    }
    if let Some(&name) = coordination_exports
        .difference(&matched_coordination_exports)
        .next()
    {
        return Err(TemplateBuildError::MissingCoordinationExport {
            name: name.to_owned(),
        });
    }

    for import in imports {
        let (template, semantic) = build_import_template(&import)?;
        let param_count = u8::try_from(import.params.len()).map_err(|_| {
            TemplateBuildError::FunctionArityOverflow {
                module: import.module.clone(),
                field: import.field.clone(),
            }
        })?;
        let result_count = u8::try_from(import.results.len()).map_err(|_| {
            TemplateBuildError::FunctionArityOverflow {
                module: import.module.clone(),
                field: import.field.clone(),
            }
        })?;
        template
            .validate(param_count, result_count)
            .map_err(|error| TemplateBuildError::InvalidGrammar {
                module: import.module.clone(),
                field: import.field.clone(),
                message: error.to_string(),
            })?;
        grammar.imports.insert(import.fref, template);
        for template in semantic {
            decoder.register(import.fref, template)?;
        }
    }

    Ok(ComponentTemplates { grammar, decoder })
}

/// Decode absorbed event blocks through the matching Starstream semantic
/// registry.
pub fn decode_absorbed_blocks(
    decoder: &TemplateRegistry,
    blocks: &[neo_wasm::comm_chain::AbsorbedEventBlock],
) -> Result<ExecutionTrace, BlockCodecError> {
    let blocks = blocks
        .iter()
        .map(|block| {
            AttributedBlock::new(
                block.words,
                block.metadata.attributed_fref,
                block.metadata.turn_export_fref,
            )
        })
        .collect::<Vec<_>>();
    decoder.decode_blocks(&blocks)
}

#[derive(Clone, Debug)]
struct FunctionImport {
    fref: u32,
    module: String,
    field: String,
    params: Box<[ValType]>,
    results: Box<[ValType]>,
}

#[derive(Clone, Debug)]
struct FunctionExport {
    index: u32,
    name: String,
    params: Box<[ValType]>,
    results: Box<[ValType]>,
    locals: Box<[ValType]>,
}

fn build_export_template(
    export: &FunctionExport,
    coordination_exports: &BTreeSet<&str>,
) -> Result<(ExportTemplate, Vec<EventTemplate>), TemplateBuildError> {
    if is_utxo_method_export(&export.name) {
        let (entry, entry_semantic, entry_claim_count) = build_method_entry_template(export)?;
        let result_word_count = flat_export_result_word_count(export)?;
        let exit = EventTemplate::ReturnControl(ReturnControlTemplate::new(result_word_count));
        let export_template = build_export_boundary_template(entry, entry_claim_count, Some(&exit));
        return Ok((export_template, vec![entry_semantic, exit]));
    }

    let exit_semantic = if is_utxo_constructor_export(&export.name) {
        Some(EventTemplate::ReturnControl(ReturnControlTemplate::new(0)))
    } else if coordination_exports.contains(export.name.as_str()) {
        Some(EventTemplate::Fixed(FixedEventTemplate::new(
            FixedEvent::CoordReturn,
        )))
    } else if export.name.contains('#') {
        None
    } else {
        return Err(TemplateBuildError::UnclassifiedFunctionExport {
            name: export.name.clone(),
        });
    };
    let template = build_export_boundary_template(Vec::new(), 0, exit_semantic.as_ref());
    Ok((template, exit_semantic.into_iter().collect()))
}

fn build_export_boundary_template(
    entry: Vec<GrammarEvent>,
    entry_claim_count: u8,
    exit_semantic: Option<&EventTemplate>,
) -> ExportTemplate {
    let exit = match exit_semantic {
        Some(EventTemplate::ReturnControl(template)) => {
            let mut slots = [SlotSource::Const(0); 7];
            if template.result_word_count > 0 {
                slots[0] = SlotSource::OutputElem { limb: Limb::Lo };
            }
            if template.result_word_count > 1 {
                slots[1] = SlotSource::OutputElem { limb: Limb::Hi };
            }
            vec![GrammarEvent::op(template.discriminant, slots)]
        }
        Some(EventTemplate::Fixed(template)) => vec![GrammarEvent::op(
            template.discriminant,
            [SlotSource::Const(0); 7],
        )],
        None => Vec::new(),
        Some(_) => unreachable!("export classification only emits return templates"),
    };
    ExportTemplate {
        entry,
        exit,
        entry_claim_count,
        exit_claim_count: 0,
    }
}

fn build_method_entry_template(
    export: &FunctionExport,
) -> Result<(Vec<GrammarEvent>, EventTemplate, u8), TemplateBuildError> {
    if !matches!(export.params.first(), Some(ValType::I32)) {
        return Err(TemplateBuildError::InvalidMethodExportReceiver {
            name: export.name.clone(),
        });
    }

    // User arguments lead the claim sequence so the semantic decoder can
    // compare a contiguous prefix with CallMethod. The callee-local resource
    // receiver and declared locals follow as execution-bootstrap advice.
    let mut words = Vec::new();
    for (local, ty) in export.params.iter().copied().enumerate().skip(1) {
        append_entry_local_words(export, &mut words, local, ty, "parameter")?;
    }
    let argument_word_count = words.len();
    append_entry_local_words(export, &mut words, 0, export.params[0], "receiver")?;
    for (offset, ty) in export.locals.iter().copied().enumerate() {
        append_entry_local_words(
            export,
            &mut words,
            export.params.len() + offset,
            ty,
            "declared local",
        )?;
    }

    let entry_claim_count =
        u8::try_from(words.len()).map_err(|_| TemplateBuildError::ExportArityOverflow {
            name: export.name.clone(),
        })?;
    let semantic =
        EventTemplate::EnterMethod(EnterMethodTemplate::new(argument_word_count, words.len()));
    let mut slots = Vec::with_capacity(words.len() + 1);
    slots.push(SlotSource::Const(semantic.discriminant()));
    slots.extend(words);
    let entry = slots
        .chunks(COMM_CHAIN_BLOCK_WORDS)
        .map(|chunk| {
            let mut block = [SlotSource::Const(0); COMM_CHAIN_BLOCK_WORDS];
            block[..chunk.len()].copy_from_slice(chunk);
            GrammarEvent {
                block,
                absorb: true,
            }
        })
        .collect();

    Ok((entry, semantic, entry_claim_count))
}

fn append_entry_local_words(
    export: &FunctionExport,
    words: &mut Vec<SlotSource>,
    local: usize,
    ty: ValType,
    kind: &'static str,
) -> Result<(), TemplateBuildError> {
    let local = u8::try_from(local).map_err(|_| TemplateBuildError::ExportArityOverflow {
        name: export.name.clone(),
    })?;
    let next_claim = |words: &[SlotSource]| {
        u8::try_from(words.len()).map_err(|_| TemplateBuildError::ExportArityOverflow {
            name: export.name.clone(),
        })
    };
    match ty {
        ValType::I32 => words.push(SlotSource::ClaimLocal {
            idx: next_claim(words)?,
            local,
            limb: Limb::Lo,
        }),
        ValType::I64 => {
            words.push(SlotSource::ClaimLocal {
                idx: next_claim(words)?,
                local,
                limb: Limb::Lo,
            });
            words.push(SlotSource::ClaimLocal {
                idx: next_claim(words)?,
                local,
                limb: Limb::Hi,
            });
        }
        ty => {
            return Err(TemplateBuildError::UnsupportedMethodExportLocal {
                name: export.name.clone(),
                kind,
                index: usize::from(local),
                ty,
            });
        }
    }
    Ok(())
}

fn is_utxo_constructor_export(name: &str) -> bool {
    let Some((instance, item)) = name.split_once('#') else {
        return false;
    };
    !instance.is_empty() && item.starts_with("[static]utxo.")
}

fn is_utxo_method_export(name: &str) -> bool {
    let Some((instance, item)) = name.split_once('#') else {
        return false;
    };
    !instance.is_empty() && item.starts_with("[method]utxo.")
}

fn build_import_template(
    import: &FunctionImport,
) -> Result<(ImportTemplate, Vec<EventTemplate>), TemplateBuildError> {
    if import.module == "starstream:std/builtin" && import.field == "abis-clear" {
        build_clear_abi_template(import)
    } else if import.module == "starstream:std/builtin" && import.field == "implements-method" {
        build_advertise_method_template(import)
    } else if import.field.starts_with("[method]") {
        build_call_method_template(import)
    } else if import.field.starts_with("[static]utxo.") {
        build_new_utxo_template(import)
    } else {
        Ok((advice_template(import)?, Vec::new()))
    }
}

fn build_new_utxo_template(
    import: &FunctionImport,
) -> Result<(ImportTemplate, Vec<EventTemplate>), TemplateBuildError> {
    if import.results.as_ref() != [ValType::I32] {
        return Err(TemplateBuildError::InvalidConstructorResult {
            module: import.module.clone(),
            field: import.field.clone(),
            results: import.results.clone(),
        });
    }

    let mut payload = Vec::new();
    for (index, ty) in import.params.iter().copied().enumerate() {
        let arg = u8::try_from(index).map_err(|_| TemplateBuildError::FunctionArityOverflow {
            module: import.module.clone(),
            field: import.field.clone(),
        })?;
        match ty {
            ValType::I32 => payload.push(SlotSource::ArgElem {
                arg,
                limb: Limb::Lo,
            }),
            ValType::I64 => payload.extend([
                SlotSource::ArgElem {
                    arg,
                    limb: Limb::Lo,
                },
                SlotSource::ArgElem {
                    arg,
                    limb: Limb::Hi,
                },
            ]),
            ty => {
                return Err(TemplateBuildError::UnsupportedConstructorParameter {
                    module: import.module.clone(),
                    field: import.field.clone(),
                    index,
                    ty,
                });
            }
        }
    }

    let mut slots = Vec::with_capacity(payload.len() + 3);
    slots.push(SlotSource::Const(OpcodeDiscriminant::NewUtxo as u64));
    slots.extend(payload.iter().copied());
    slots.extend([
        SlotSource::ResultElem { limb: Limb::Lo },
        SlotSource::ResultElem { limb: Limb::Hi },
    ]);
    let events = slots
        .chunks(COMM_CHAIN_BLOCK_WORDS)
        .map(|chunk| {
            let mut block = [SlotSource::Const(0); COMM_CHAIN_BLOCK_WORDS];
            block[..chunk.len()].copy_from_slice(chunk);
            GrammarEvent {
                block,
                absorb: true,
            }
        })
        .collect();

    Ok((
        ImportTemplate {
            events,
            claim_count: 0,
        },
        vec![EventTemplate::NewUtxo(NewUtxoTemplate::new(payload.len()))],
    ))
}

fn build_clear_abi_template(
    import: &FunctionImport,
) -> Result<(ImportTemplate, Vec<EventTemplate>), TemplateBuildError> {
    require_signature(import, &[], &[])?;
    Ok((
        ImportTemplate {
            events: vec![GrammarEvent::op(
                OpcodeDiscriminant::ClearAbi as u64,
                [SlotSource::Const(0); 7],
            )],
            claim_count: 0,
        },
        vec![EventTemplate::Fixed(FixedEventTemplate::new(
            FixedEvent::ClearAbi,
        ))],
    ))
}

fn build_advertise_method_template(
    import: &FunctionImport,
) -> Result<(ImportTemplate, Vec<EventTemplate>), TemplateBuildError> {
    require_signature(
        import,
        &[ValType::I64, ValType::I64, ValType::I64, ValType::I64],
        &[],
    )?;
    let limbs = (0_u8..4).flat_map(|arg| {
        [
            SlotSource::ArgElem {
                arg,
                limb: Limb::Lo,
            },
            SlotSource::ArgElem {
                arg,
                limb: Limb::Hi,
            },
        ]
    });
    let mut first = [SlotSource::Const(0); COMM_CHAIN_BLOCK_WORDS];
    first[0] = SlotSource::Const(OpcodeDiscriminant::AdvertiseMethod as u64);
    let mut continuation = [SlotSource::Const(0); COMM_CHAIN_BLOCK_WORDS];
    for (index, source) in limbs.enumerate() {
        if index < COMM_CHAIN_BLOCK_WORDS - 1 {
            first[index + 1] = source;
        } else {
            continuation[index - (COMM_CHAIN_BLOCK_WORDS - 1)] = source;
        }
    }
    Ok((
        ImportTemplate {
            events: vec![
                GrammarEvent {
                    block: first,
                    absorb: true,
                },
                GrammarEvent {
                    block: continuation,
                    absorb: true,
                },
            ],
            claim_count: 0,
        },
        vec![EventTemplate::AdvertiseMethod(
            AdvertiseMethodTemplate::new(),
        )],
    ))
}

fn build_call_method_template(
    import: &FunctionImport,
) -> Result<(ImportTemplate, Vec<EventTemplate>), TemplateBuildError> {
    if !matches!(import.params.first(), Some(ValType::I32)) {
        return Err(TemplateBuildError::InvalidMethodReceiver {
            module: import.module.clone(),
            field: import.field.clone(),
        });
    }

    let mut payload = Vec::new();
    for (index, ty) in import.params.iter().copied().enumerate().skip(1) {
        let arg = u8::try_from(index).map_err(|_| TemplateBuildError::FunctionArityOverflow {
            module: import.module.clone(),
            field: import.field.clone(),
        })?;
        match ty {
            ValType::I32 => payload.push(SlotSource::ArgElem {
                arg,
                limb: Limb::Lo,
            }),
            ValType::I64 => payload.extend([
                SlotSource::ArgElem {
                    arg,
                    limb: Limb::Lo,
                },
                SlotSource::ArgElem {
                    arg,
                    limb: Limb::Hi,
                },
            ]),
            ty => {
                return Err(TemplateBuildError::UnsupportedMethodParameter {
                    module: import.module.clone(),
                    field: import.field.clone(),
                    index,
                    ty,
                });
            }
        }
    }

    let mut events = Vec::new();
    let mut first = [SlotSource::Const(0); COMM_CHAIN_BLOCK_WORDS];
    first[0] = SlotSource::Const(OpcodeDiscriminant::CallMethod as u64);
    first[1] = SlotSource::ArgElem {
        arg: 0,
        limb: Limb::Lo,
    };
    let first_payload_count = payload.len().min(COMM_CHAIN_BLOCK_WORDS - 2);
    first[2..2 + first_payload_count].copy_from_slice(&payload[..first_payload_count]);
    events.push(GrammarEvent {
        block: first,
        absorb: true,
    });
    for chunk in payload[first_payload_count..].chunks(COMM_CHAIN_BLOCK_WORDS) {
        let mut continuation = [SlotSource::Const(0); COMM_CHAIN_BLOCK_WORDS];
        continuation[..chunk.len()].copy_from_slice(chunk);
        events.push(GrammarEvent {
            block: continuation,
            absorb: true,
        });
    }
    let result_word_count = flat_import_result_word_count(import)?;
    if result_word_count > 0 {
        let mut result = [SlotSource::Const(0); COMM_CHAIN_BLOCK_WORDS];
        // Neo-Wasm requires both lanes for every single-result import. The Lo
        // slot pushes a fresh stack cell and clears its high lane; the Hi slot
        // then completes an i64 result. For i32 it stages zero, which the
        // semantic decoder deliberately validates as committed padding.
        result[0] = SlotSource::ResultElem { limb: Limb::Lo };
        result[1] = SlotSource::ResultElem { limb: Limb::Hi };
        events.push(GrammarEvent {
            block: result,
            absorb: true,
        });
    }

    let method = method_hash_from_import(&import.field)?;
    let semantic = EventTemplate::CallMethod(CallMethodTemplate::new(
        method,
        payload.len(),
        result_word_count,
    ));
    Ok((
        ImportTemplate {
            events,
            claim_count: 0,
        },
        vec![semantic],
    ))
}

fn flat_import_result_word_count(import: &FunctionImport) -> Result<usize, TemplateBuildError> {
    match import.results.as_ref() {
        [] => Ok(0),
        [ValType::I32] => Ok(1),
        [ValType::I64] => Ok(2),
        _ => Err(TemplateBuildError::UnsupportedMethodResult {
            module: import.module.clone(),
            field: import.field.clone(),
            results: import.results.clone(),
        }),
    }
}

fn flat_export_result_word_count(export: &FunctionExport) -> Result<usize, TemplateBuildError> {
    match export.results.as_ref() {
        [] => Ok(0),
        [ValType::I32] => Ok(1),
        [ValType::I64] => Ok(2),
        _ => Err(TemplateBuildError::UnsupportedMethodExportResult {
            name: export.name.clone(),
            results: export.results.clone(),
        }),
    }
}

fn advice_template(import: &FunctionImport) -> Result<ImportTemplate, TemplateBuildError> {
    Ok(ImportTemplate {
        events: result_advice_events(import)?,
        claim_count: 0,
    })
}

fn require_signature(
    import: &FunctionImport,
    params: &[ValType],
    results: &[ValType],
) -> Result<(), TemplateBuildError> {
    if import.params.as_ref() != params || import.results.as_ref() != results {
        return Err(TemplateBuildError::InvalidBuiltinSignature {
            module: import.module.clone(),
            field: import.field.clone(),
            params: import.params.clone(),
            results: import.results.clone(),
        });
    }
    Ok(())
}

fn result_advice_events(import: &FunctionImport) -> Result<Vec<GrammarEvent>, TemplateBuildError> {
    match import.results.len() {
        0 => Ok(Vec::new()),
        1 => {
            let mut block = [SlotSource::Const(0); COMM_CHAIN_BLOCK_WORDS];
            block[0] = SlotSource::ResultElem { limb: Limb::Lo };
            block[1] = SlotSource::ResultElem { limb: Limb::Hi };
            Ok(vec![GrammarEvent::advice(block)])
        }
        count => Err(TemplateBuildError::UnsupportedResultCount {
            module: import.module.clone(),
            field: import.field.clone(),
            count,
        }),
    }
}

fn method_hash_from_import(field: &str) -> Result<MethodHash, TemplateBuildError> {
    let method = field
        .rsplit_once('.')
        .map(|(_, method)| method)
        .filter(|method| !method.is_empty())
        .ok_or_else(|| TemplateBuildError::InvalidMethodName(field.to_owned()))?
        .replace('-', "_");
    let digest = Sha256::digest(method.as_bytes());
    let mut limbs = [0; 4];
    for (limb, bytes) in limbs.iter_mut().zip(digest.chunks_exact(8)) {
        *limb = u64::from_le_bytes(bytes.try_into().expect("SHA-256 chunks have eight bytes"));
    }
    Ok(MethodHash(limbs))
}

fn first_core_module(wasm: &[u8]) -> Result<&[u8], TemplateBuildError> {
    if Parser::is_core_wasm(wasm) {
        return Ok(wasm);
    }

    for payload in Parser::new(0).parse_all(wasm) {
        if let Payload::ModuleSection {
            unchecked_range, ..
        } = payload?
        {
            return wasm
                .get(unchecked_range.clone())
                .ok_or(TemplateBuildError::InvalidCoreModuleRange(unchecked_range));
        }
    }
    Err(TemplateBuildError::MissingCoreModule)
}

fn parse_function_imports(module: &[u8]) -> Result<Vec<FunctionImport>, TemplateBuildError> {
    let mut types = Vec::<(Box<[ValType]>, Box<[ValType]>)>::new();
    let mut imports = Vec::new();

    for payload in Parser::new(0).parse_all(module) {
        match payload? {
            Payload::TypeSection(reader) => {
                for rec_group in reader {
                    for subtype in rec_group?.into_types() {
                        let type_index = types.len();
                        let CompositeInnerType::Func(function) = &subtype.composite_type.inner
                        else {
                            return Err(TemplateBuildError::UnsupportedCoreType(type_index));
                        };
                        types.push((function.params().into(), function.results().into()));
                    }
                }
            }
            Payload::ImportSection(reader) => {
                for import in reader {
                    let import = import?;
                    let TypeRef::Func(type_index) = import.ty else {
                        continue;
                    };
                    let Some((params, results)) = types.get(type_index as usize) else {
                        return Err(TemplateBuildError::MissingFunctionType {
                            module: import.module.to_owned(),
                            field: import.name.to_owned(),
                            type_index,
                        });
                    };
                    imports.push(FunctionImport {
                        fref: u32::try_from(imports.len() + 1).map_err(|_| {
                            TemplateBuildError::FunctionArityOverflow {
                                module: import.module.to_owned(),
                                field: import.name.to_owned(),
                            }
                        })?,
                        module: import.module.to_owned(),
                        field: import.name.to_owned(),
                        params: params.clone(),
                        results: results.clone(),
                    });
                }
            }
            _ => {}
        }
    }

    Ok(imports)
}

fn parse_function_exports(module: &[u8]) -> Result<Vec<FunctionExport>, TemplateBuildError> {
    let mut types = Vec::<(Box<[ValType]>, Box<[ValType]>)>::new();
    let mut function_types = Vec::<u32>::new();
    let mut imported_function_count = 0_usize;
    let mut raw_exports = Vec::<(u32, String)>::new();
    let mut code_locals = Vec::<Vec<(u32, ValType)>>::new();
    for payload in Parser::new(0).parse_all(module) {
        match payload? {
            Payload::TypeSection(reader) => {
                for rec_group in reader {
                    for subtype in rec_group?.into_types() {
                        let type_index = types.len();
                        let CompositeInnerType::Func(function) = &subtype.composite_type.inner
                        else {
                            return Err(TemplateBuildError::UnsupportedCoreType(type_index));
                        };
                        types.push((function.params().into(), function.results().into()));
                    }
                }
            }
            Payload::ImportSection(reader) => {
                for import in reader {
                    let import = import?;
                    if let TypeRef::Func(type_index) = import.ty {
                        function_types.push(type_index);
                        imported_function_count += 1;
                    }
                }
            }
            Payload::FunctionSection(reader) => {
                for type_index in reader {
                    function_types.push(type_index?);
                }
            }
            Payload::ExportSection(reader) => {
                for export in reader {
                    let export = export?;
                    if export.kind != wasmparser::ExternalKind::Func {
                        continue;
                    }
                    raw_exports.push((export.index, export.name.to_owned()));
                }
            }
            Payload::CodeSectionEntry(body) => {
                let declarations = body
                    .get_locals_reader()?
                    .into_iter()
                    .collect::<Result<Vec<_>, _>>()?;
                code_locals.push(declarations);
            }
            _ => {}
        }
    }

    raw_exports
        .into_iter()
        .map(|(index, name)| {
            let Some(&type_index) = function_types.get(index as usize) else {
                return Err(TemplateBuildError::MissingExportFunction { name, index });
            };
            let Some((params, results)) = types.get(type_index as usize) else {
                return Err(TemplateBuildError::MissingExportFunctionType { name, type_index });
            };
            let mut locals = Vec::new();
            if let Some(defined_index) = (index as usize).checked_sub(imported_function_count) {
                let declarations = code_locals
                    .get(defined_index)
                    .ok_or_else(|| TemplateBuildError::MissingExportBody { name: name.clone() })?;
                for &(count, ty) in declarations {
                    let count = usize::try_from(count).map_err(|_| {
                        TemplateBuildError::ExportArityOverflow { name: name.clone() }
                    })?;
                    let total = params
                        .len()
                        .checked_add(locals.len())
                        .and_then(|total| total.checked_add(count))
                        .ok_or_else(|| TemplateBuildError::ExportArityOverflow {
                            name: name.clone(),
                        })?;
                    if total > usize::from(u8::MAX) {
                        return Err(TemplateBuildError::ExportArityOverflow { name });
                    }
                    locals.extend(std::iter::repeat_n(ty, count));
                }
            }
            Ok(FunctionExport {
                index,
                name,
                params: params.clone(),
                results: results.clone(),
                locals: locals.into_boxed_slice(),
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use neo_wasm::event_grammar::{
        absorbed_blocks, expand_export_entry, expand_export_exit, expand_import_events,
    };
    use starstream_interleaving_spec::{ExecutionEvent, ResourceHandle, StarstreamValue};

    #[test]
    fn constructor_import_is_one_atomic_semantic_event() {
        let wasm = wat::parse_str(
            r#"
                (module
                  (import "counter-utxo" "[static]utxo.new"
                    (func (param i64 i32) (result i32)))
                )
            "#,
        )
        .expect("test module compiles");
        let templates = build_component_templates(&wasm, &[]).expect("templates build");
        let import = &templates.grammar.imports[&1];
        let expanded = expand_import_events(import, &[(11, 12), (13, 0)], Some((7, 0)), &[])
            .expect("template expands");
        let blocks = absorbed_blocks(import, &expanded).expect("blocks absorb");

        assert_eq!(blocks, [[2, 11, 12, 13, 7, 0, 0, 0]]);
        let attributed = blocks
            .into_iter()
            .map(|words| AttributedBlock::new(words, 1, 9))
            .collect::<Vec<_>>();
        assert_eq!(
            templates
                .decoder
                .decode_blocks(&attributed)
                .expect("blocks decode"),
            ExecutionTrace::new([ExecutionEvent::NewUtxo {
                arguments: StarstreamValue(vec![11, 12, 13]),
                resource: ResourceHandle(7),
            }])
        );
    }

    #[test]
    fn method_width_and_blocks_are_derived_from_the_core_interface() {
        let wasm = wat::parse_str(
            r#"
                (module
                  (import "score" "[method]utxo.add"
                    (func (param i32 i64 i32)))
                )
            "#,
        )
        .expect("test module compiles");
        let templates = build_component_templates(&wasm, &[]).expect("templates build");
        let import = &templates.grammar.imports[&1];
        let expanded = expand_import_events(import, &[(7, 0), (11, 12), (13, 0)], None, &[])
            .expect("template expands");
        let blocks = absorbed_blocks(import, &expanded).expect("blocks absorb");

        assert_eq!(blocks, [[7, 7, 11, 12, 13, 0, 0, 0]]);
        let attributed = blocks
            .into_iter()
            .map(|words| AttributedBlock::new(words, 1, 9))
            .collect::<Vec<_>>();
        assert_eq!(
            templates
                .decoder
                .decode_blocks(&attributed)
                .expect("blocks decode"),
            ExecutionTrace::new([ExecutionEvent::CallMethod {
                resource: ResourceHandle(7),
                method: method_hash_from_import("[method]utxo.add").unwrap(),
                arguments: StarstreamValue(vec![11, 12, 13]),
                result: StarstreamValue::default(),
            }])
        );
    }

    #[test]
    fn flat_method_inputs_and_result_are_absorbed_at_both_boundaries() {
        let wasm = wat::parse_str(
            r#"
                (module
                  (import "score" "[method]utxo.read"
                    (func (param i32 i64) (result i64)))
                  (func (export "score#[method]utxo.read")
                    (param i32 i64) (result i64) (local i32)
                    i64.const 0)
                )
            "#,
        )
        .expect("test module compiles");
        let templates = build_component_templates(&wasm, &[]).expect("templates build");

        let import = &templates.grammar.imports[&1];
        let imported = expand_import_events(import, &[(7, 0), (11, 12)], Some((41, 9)), &[])
            .expect("import template expands");
        let import_blocks = absorbed_blocks(import, &imported).expect("import blocks absorb");
        assert_eq!(
            import_blocks,
            [[7, 7, 11, 12, 0, 0, 0, 0], [41, 9, 0, 0, 0, 0, 0, 0]]
        );

        let export = &templates.grammar.exports[&2];
        let entered =
            expand_export_entry(export, &[11, 12, 7, 0]).expect("export entry template expands");
        assert_eq!(entered, [[11, 11, 12, 7, 0, 0, 0, 0]]);

        let exported =
            expand_export_exit(export, Some((41, 9)), &[]).expect("export template expands");
        assert_eq!(exported, [[6, 41, 9, 0, 0, 0, 0, 0]]);

        let blocks = [
            AttributedBlock::new(import_blocks[0], 1, 8),
            AttributedBlock::new(import_blocks[1], 1, 8),
            AttributedBlock::new(entered[0], 2, 2),
            AttributedBlock::new(exported[0], 2, 2),
        ];
        assert_eq!(
            templates
                .decoder
                .decode_blocks(&blocks)
                .expect("method results decode"),
            ExecutionTrace::new([
                ExecutionEvent::CallMethod {
                    resource: ResourceHandle(7),
                    method: method_hash_from_import("[method]utxo.read").unwrap(),
                    arguments: StarstreamValue(vec![11, 12]),
                    result: StarstreamValue(vec![41, 9]),
                },
                ExecutionEvent::EnterMethod {
                    arguments: StarstreamValue(vec![11, 12]),
                },
                ExecutionEvent::ReturnControl {
                    result: StarstreamValue(vec![41, 9]),
                },
            ])
        );
    }

    #[test]
    fn i32_method_result_absorbs_a_zero_high_lane_as_padding() {
        let wasm = wat::parse_str(
            r#"
                (module
                  (import "score" "[method]utxo.read32"
                    (func (param i32) (result i32))))
            "#,
        )
        .expect("test module compiles");
        let templates = build_component_templates(&wasm, &[]).expect("templates build");
        let import = &templates.grammar.imports[&1];
        let expanded = expand_import_events(import, &[(7, 0)], Some((41, 0)), &[])
            .expect("i32 result template expands");
        let blocks = absorbed_blocks(import, &expanded).expect("result blocks absorb");

        assert_eq!(
            blocks,
            [[7, 7, 0, 0, 0, 0, 0, 0], [41, 0, 0, 0, 0, 0, 0, 0]]
        );
        let attributed = blocks
            .iter()
            .copied()
            .map(|words| AttributedBlock::new(words, 1, 8))
            .collect::<Vec<_>>();
        assert_eq!(
            templates
                .decoder
                .decode_blocks(&attributed)
                .expect("i32 result decodes"),
            ExecutionTrace::new([ExecutionEvent::CallMethod {
                resource: ResourceHandle(7),
                method: method_hash_from_import("[method]utxo.read32").unwrap(),
                arguments: StarstreamValue::default(),
                result: StarstreamValue(vec![41]),
            }])
        );

        let mut invalid = attributed;
        invalid[1].words[1] = 9;
        assert_eq!(
            templates.decoder.decode_blocks(&invalid),
            Err(BlockCodecError::NonZeroPadding {
                block: 1,
                word: 1,
                value: 9,
            })
        );
    }

    #[test]
    fn rejects_plain_exports_outside_the_coordination_allowlist() {
        let wasm = wat::parse_str(
            r#"
                (module
                  (func (export "cabi_realloc"))
                )
            "#,
        )
        .expect("test module compiles");

        assert!(matches!(
            build_component_templates(&wasm, &[]),
            Err(TemplateBuildError::UnclassifiedFunctionExport { name })
                if name == "cabi_realloc"
        ));
    }

    #[test]
    fn rejects_missing_coordination_allowlist_entries() {
        let wasm = wat::parse_str("(module)").expect("test module compiles");

        assert!(matches!(
            build_component_templates(&wasm, &["missing"]),
            Err(TemplateBuildError::MissingCoordinationExport { name })
                if name == "missing"
        ));
    }

    #[test]
    fn utxo_and_coordination_exports_emit_control_returns() {
        let wasm = wat::parse_str(
            r#"
                (module
                  (func (export "counter#[static]utxo.new") (result i32)
                    i32.const 7)
                  (func (export "counter#[method]utxo.add") (param i32))
                  (func (export "example"))
                )
            "#,
        )
        .expect("test module compiles");
        let templates = build_component_templates(&wasm, &["example"]).expect("templates build");

        let constructor = expand_export_exit(&templates.grammar.exports[&1], Some((7, 0)), &[])
            .expect("constructor exit expands");
        let method = expand_export_exit(&templates.grammar.exports[&2], None, &[])
            .expect("method exit expands");
        let coord = expand_export_exit(&templates.grammar.exports[&3], None, &[])
            .expect("coordination exit expands");
        assert_eq!(constructor, [[6, 0, 0, 0, 0, 0, 0, 0]]);
        assert_eq!(method, [[6, 0, 0, 0, 0, 0, 0, 0]]);
        assert_eq!(coord, [[10, 0, 0, 0, 0, 0, 0, 0]]);

        let blocks = [
            AttributedBlock::new(constructor[0], 1, 1),
            AttributedBlock::new(method[0], 2, 2),
            AttributedBlock::new(coord[0], 3, 3),
        ];
        assert_eq!(
            templates
                .decoder
                .decode_blocks(&blocks)
                .expect("exit blocks decode"),
            ExecutionTrace::new([
                ExecutionEvent::ReturnControl {
                    result: StarstreamValue::default(),
                },
                ExecutionEvent::ReturnControl {
                    result: StarstreamValue::default(),
                },
                ExecutionEvent::CoordReturn,
            ])
        );
    }
}
