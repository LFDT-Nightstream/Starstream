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
    AdvertiseMethodTemplate, AttributedBlock, BlockCodecError, CallMethodTemplate, EventTemplate,
    FixedEvent, FixedEventTemplate, NewUtxoTemplate, OpcodeDiscriminant, TemplateRegistry,
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
        grammar.exports.insert(fref, template);
        if let Some(event) = semantic {
            if event == FixedEvent::CoordReturn {
                matched_coordination_exports.insert(export.name.as_str());
            }
            decoder.register(fref, EventTemplate::Fixed(FixedEventTemplate::new(event)))?;
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
}

fn build_export_template(
    export: &FunctionExport,
    coordination_exports: &BTreeSet<&str>,
) -> Result<(ExportTemplate, Option<FixedEvent>), TemplateBuildError> {
    let event = if is_utxo_control_export(&export.name) {
        Some(FixedEvent::ReturnControl)
    } else if coordination_exports.contains(export.name.as_str()) {
        Some(FixedEvent::CoordReturn)
    } else if export.name.contains('#') {
        None
    } else {
        return Err(TemplateBuildError::UnclassifiedFunctionExport {
            name: export.name.clone(),
        });
    };
    let exit = event
        .map(|event| {
            vec![GrammarEvent::op(
                event.default_discriminant(),
                [SlotSource::Const(0); 7],
            )]
        })
        .unwrap_or_default();
    Ok((
        ExportTemplate {
            exit,
            ..ExportTemplate::default()
        },
        event,
    ))
}

fn is_utxo_control_export(name: &str) -> bool {
    let Some((instance, item)) = name.split_once('#') else {
        return false;
    };
    !instance.is_empty() && (item.starts_with("[static]utxo.") || item.starts_with("[method]utxo."))
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
    events.extend(result_advice_events(import)?);

    let method = method_hash_from_import(&import.field)?;
    let semantic = EventTemplate::CallMethod(CallMethodTemplate::new(method, payload.len()));
    Ok((
        ImportTemplate {
            events,
            claim_count: 0,
        },
        vec![semantic],
    ))
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
    let mut exports = Vec::new();
    for payload in Parser::new(0).parse_all(module) {
        if let Payload::ExportSection(reader) = payload? {
            for export in reader {
                let export = export?;
                if export.kind == wasmparser::ExternalKind::Func {
                    exports.push(FunctionExport {
                        index: export.index,
                        name: export.name.to_owned(),
                    });
                }
            }
        }
    }
    Ok(exports)
}

#[cfg(test)]
mod tests {
    use super::*;
    use neo_wasm::event_grammar::{absorbed_blocks, expand_export_exit, expand_import_events};
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
            }])
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
                  (func (export "counter#[method]utxo.add"))
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
                ExecutionEvent::ReturnControl,
                ExecutionEvent::ReturnControl,
                ExecutionEvent::CoordReturn,
            ])
        );
    }
}
