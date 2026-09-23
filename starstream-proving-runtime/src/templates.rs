use std::collections::{BTreeMap, BTreeSet};

use neo_wasm::host_event_bindings::{
    EventBlock, EventSequenceBuilder, EventSources, HostEventBindings, HostEventBindingsBuilder,
    Limb, MemoryBase, SlotBinding, opaque_value_root,
};
use sha2::{Digest as _, Sha256};
use starstream_interleaving_spec::events::EventKind;
use starstream_interleaving_spec::{BLOCK_SIZE, METHOD_WORDS, MethodHash, StarstreamValue, Trace};
use wasmparser::{CompositeInnerType, Parser, Payload, TypeRef, ValType};

use crate::decoder::{AttributedBlock, BlockCodecError, EventTemplate, TemplateRegistry};

const UTXO_CONTEXT_INTERFACE: &str = "starstream:std/utxo-context";
const RESUME_METHOD: &str = "[method]utxo-context.resume";
const IMPLEMENTS_METHOD: &str = "[method]utxo-context.implements-method";
const DYNAMIC_UTXO_INTERFACE: &str = "starstream:contract/dynamic-utxo";
const SELF_INTERFACE_PREFIX: &str = "starstream:self/";
const TYPED_UTXO_INTERFACE_PREFIX: &str = "starstream:utxo/";
const CONSTRUCTOR_PREFIX: &str = "[static]utxo.";
const METHOD_PREFIX: &str = "[method]utxo.";
const GET_STORAGE_EXPORT: &str = "get-storage";
const SET_STORAGE_EXPORT: &str = "set-storage";

/// Schema tag of a flat `i32`/`i64` value tuple committed as an opaque root.
pub const FLAT_VALUE_SCHEMA_TAG: u64 = u64::from_le_bytes(*b"SSFLAT01");

/// Flat tuples of up to this many words are committed inline.
pub const INLINE_ROOT_WORDS: usize = 4;
const ROOT_WORDS: usize = 4;
const _: () = assert!(INLINE_ROOT_WORDS <= ROOT_WORDS);

/// Emitter and decoder templates for one compiled component.
#[derive(Clone, Debug)]
pub struct ComponentTemplates {
    pub program_tables: neo_wasm::WasmProgramTables,
    pub bindings: HostEventBindings,
    pub decoder: TemplateRegistry,
    /// Import frefs keyed by `(module, field)`.
    pub import_frefs: BTreeMap<(String, String), u32>,
    /// Export frefs keyed by name.
    pub export_frefs: BTreeMap<String, u32>,
}

impl ComponentTemplates {
    /// The function reference of the `module`.`field` import.
    #[must_use]
    pub fn import_fref(&self, module: &str, field: &str) -> Option<u32> {
        self.import_frefs
            .get(&(module.to_owned(), field.to_owned()))
            .copied()
    }

    /// The function reference of the named export.
    #[must_use]
    pub fn export_fref(&self, name: &str) -> Option<u32> {
        self.export_frefs.get(name).copied()
    }
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

struct ExportEvents {
    entry: Vec<EventBlock>,
    exit: Vec<EventBlock>,
    semantic: Vec<EventTemplate>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ExportKind {
    /// `<utxo>#[static]utxo.<name>`: the exported constructor takes the
    /// callee-local `utxo-context` handle before the user arguments.
    Constructor,
    /// `<utxo>#[method]utxo.<name>`: the exported method takes the
    /// callee-local resource receiver before the user arguments.
    Method(MethodHash),
    /// An allowlisted plain export: the transaction entrypoint.
    CoordinationScript,
    /// `<utxo>#get-storage`: publishes the storage record when it returns.
    GetStorage,
    /// `<utxo>#set-storage`: loads the storage record when it is entered.
    SetStorage,
    /// A compiler-generated export with no interleaving semantics.
    Internal,
}

/// Event builder that reserves whole blocks for roots.
struct Sequence {
    builder: EventSequenceBuilder,
    words: usize,
    root_block: Option<usize>,
}

/// Sources and schema for one four-word value root.
struct Root {
    schema: [u64; 4],
    sources: Vec<SlotBinding>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ImportKind {
    YieldBegin,
    RegisterMethod,
    NewUtxo,
    CallMethod(MethodHash),
    Advice,
}

/// Build emitter and decoder templates from a core Wasm module or component.
/// Coordination-script exports must be allowlisted.
pub fn build_component_templates(
    wasm: &[u8],
    coordination_exports: &[&str],
) -> Result<ComponentTemplates, TemplateBuildError> {
    let module = first_core_module(wasm)?;
    let artifacts = neo_wasm::extract_wasm_program_artifacts(module)?;
    let types = parse_function_types(module)?;
    let imports = parse_function_imports(module, &types)?;
    let exports = parse_function_exports(module, &types)?;
    let coordination_exports = coordination_exports
        .iter()
        .copied()
        .collect::<BTreeSet<_>>();
    let mut matched_coordination_exports = BTreeSet::new();
    let mut bindings = HostEventBindingsBuilder::new(&artifacts.tables);
    let mut decoder = TemplateRegistry::new();
    let mut import_frefs = BTreeMap::new();
    let mut export_frefs = BTreeMap::new();

    for export in &exports {
        let fref = export.index.saturating_add(1);
        export_frefs.insert(export.name.clone(), fref);
        let classified = classify_export(export, &coordination_exports)?;
        if matches!(classified, ExportKind::CoordinationScript) {
            matched_coordination_exports.insert(export.name.as_str());
        }
        let export_events = build_export_events(export, classified, &exports)?;
        bindings
            .export(fref, export_events.entry, export_events.exit)
            .map_err(|error| TemplateBuildError::InvalidExportBindings {
                name: export.name.clone(),
                message: error.to_string(),
            })?;
        for event in export_events.semantic {
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
        import_frefs.insert((import.module.clone(), import.field.clone()), import.fref);
        let (events, semantic) = build_import_template(&import)?;
        bindings.import(import.fref, events).map_err(|error| {
            TemplateBuildError::InvalidImportBindings {
                module: import.module.clone(),
                field: import.field.clone(),
                message: error.to_string(),
            }
        })?;
        if let Some(template) = semantic {
            decoder.register(import.fref, template)?;
        }
    }

    let bindings = bindings.finish()?;

    Ok(ComponentTemplates {
        program_tables: artifacts.tables,
        bindings,
        decoder,
        import_frefs,
        export_frefs,
    })
}

/// Decode absorbed event blocks through the matching Starstream semantic
/// registry.
pub fn decode_absorbed_blocks(
    decoder: &TemplateRegistry,
    blocks: &[neo_wasm::comm_chain::AbsorbedEventBlock],
) -> Result<Trace, BlockCodecError> {
    let blocks = blocks.iter().map(AttributedBlock::from).collect::<Vec<_>>();
    decoder.decode_blocks(&blocks)
}

/// SHA-256 method identity as eight little-endian `u32` limbs.
/// WIT's kebab-case names are converted back to snake_case before hashing.
///
/// TODO: this needs to use the full ABI
/// TODO: and probably we want to share this logic with the compiler somehow
///
// A codegen mismatch causes registered-method lookup to fail.
pub fn method_hash_from_name(name: &str) -> MethodHash {
    let digest = Sha256::digest(name.replace('-', "_").as_bytes());
    let mut words = [0; METHOD_WORDS];
    for (word, bytes) in words.iter_mut().zip(digest.chunks_exact(4)) {
        *word = u32::from_le_bytes(bytes.try_into().expect("SHA-256 chunks have four bytes"));
    }
    MethodHash(words)
}

/// Opaque schema for a flat tuple: tag, length, and `i64` bitmap.
///
/// The bitmap creates a different schema for (u32, u64) and (u32, u32, 32),
/// which are otherwise the same (64bits don't fit on the goldilocks field)
///
/// TODO: in practice we can't extract everything from the lowered signature
/// though, so eventually we'll need to hook-up to compiler/WIT metadata
pub fn value_schema(types: &[ValType]) -> [u64; 4] {
    let wide = types
        .iter()
        .enumerate()
        .filter(|(_, ty)| **ty == ValType::I64)
        .fold(0_u64, |mask, (index, _)| mask | (1 << index.min(63)));
    [FLAT_VALUE_SCHEMA_TAG, types.len() as u64, wide, 0]
}

/// Native root for a flat `i32`/`i64` tuple, matching template commitment.
/// Tuples of at most four words are zero-padded; wider tuples use the
/// `SSFLAT01` opaque schema.
pub fn flat_value_root(types: &[ValType], values: &[u64]) -> StarstreamValue {
    assert_eq!(types.len(), values.len(), "one value per flat type");
    let mut words = Vec::with_capacity(types.len() * 2);
    for (ty, &value) in types.iter().zip(values) {
        match ty {
            ValType::I32 => {
                assert!(value <= u64::from(u32::MAX), "i32 value fits 32 bits");
                words.push(value);
            }
            ValType::I64 => words.extend([value & 0xffff_ffff, value >> 32]),
            other => panic!("flat values are i32 or i64, not {other:?}"),
        }
    }
    if words.len() <= INLINE_ROOT_WORDS {
        let mut root = [0; ROOT_WORDS];
        root[..words.len()].copy_from_slice(&words);
        return StarstreamValue(root);
    }
    StarstreamValue(
        opaque_value_root(value_schema(types), &words).expect("32-bit limbs are canonical"),
    )
}

impl ExportKind {
    const fn name(self) -> &'static str {
        match self {
            Self::Constructor => "constructor",
            Self::Method(_) => "method",
            Self::CoordinationScript => "coordination script",
            Self::GetStorage => "get-storage",
            Self::SetStorage => "set-storage",
            Self::Internal => "internal",
        }
    }
}

fn classify_export(
    export: &FunctionExport,
    coordination_exports: &BTreeSet<&str>,
) -> Result<ExportKind, TemplateBuildError> {
    if let Some((instance, item)) = export.name.split_once('#') {
        if instance.is_empty() {
            return Err(TemplateBuildError::UnclassifiedFunctionExport {
                name: export.name.clone(),
            });
        }
        if item.starts_with(CONSTRUCTOR_PREFIX) {
            return Ok(ExportKind::Constructor);
        }
        if let Some(method) = item.strip_prefix(METHOD_PREFIX) {
            return Ok(ExportKind::Method(method_hash_from_segment(method)?));
        }
        return Ok(match item {
            GET_STORAGE_EXPORT => ExportKind::GetStorage,
            SET_STORAGE_EXPORT => ExportKind::SetStorage,
            _ => ExportKind::Internal,
        });
    }
    if coordination_exports.contains(export.name.as_str()) {
        return Ok(ExportKind::CoordinationScript);
    }
    Err(TemplateBuildError::UnclassifiedFunctionExport {
        name: export.name.clone(),
    })
}

fn flat_limbs(ty: ValType) -> impl ExactSizeIterator<Item = Limb> {
    let count = match ty {
        ValType::I32 => 1,
        ValType::I64 => 2,
        _ => unreachable!("flat values must be i32 or i64"),
    };
    [Limb::Lo, Limb::Hi].into_iter().take(count)
}

impl Root {
    fn unit() -> Self {
        Self::value(&[], Vec::new())
    }

    fn value(types: &[ValType], sources: Vec<SlotBinding>) -> Self {
        Self {
            schema: value_schema(types),
            sources,
        }
    }
}

impl Sequence {
    fn op(discriminant: EventKind) -> Self {
        Self {
            builder: EventSequenceBuilder::op(u64::from(discriminant.tag())),
            words: 1,
            root_block: None,
        }
    }

    fn map(
        mut self,
        words: usize,
        f: impl FnOnce(EventSequenceBuilder) -> Result<EventSequenceBuilder, neo_wasm::WasmBuildError>,
    ) -> Result<Self, neo_wasm::WasmBuildError> {
        self.builder = f(self.builder)?;
        self.words += words;
        Ok(self)
    }

    fn constant_i32(self, value: u32) -> Result<Self, neo_wasm::WasmBuildError> {
        self.map(1, |builder| builder.constant_i32(value))
    }

    fn arg_i32(self, arg: u8) -> Result<Self, neo_wasm::WasmBuildError> {
        self.map(1, |builder| builder.arg_i32(arg))
    }

    fn arg_i64(self, arg: u8) -> Result<Self, neo_wasm::WasmBuildError> {
        self.map(2, |builder| builder.arg_i64(arg))
    }

    /// Both result lanes.
    fn result(self) -> Result<Self, neo_wasm::WasmBuildError> {
        self.map(2, |builder| builder.result())
    }

    /// Append a root in its own four-word slot.
    fn root(mut self, root: Root) -> Result<Self, neo_wasm::WasmBuildError> {
        let offset = self.words % BLOCK_SIZE;
        if offset + ROOT_WORDS > BLOCK_SIZE || self.root_block == Some(self.words / BLOCK_SIZE) {
            for _ in offset..BLOCK_SIZE {
                self = self.constant_i32(0)?;
            }
        }
        self.root_block = Some(self.words / BLOCK_SIZE);
        let words = root.sources.len();
        if words <= INLINE_ROOT_WORDS {
            for source in root.sources {
                self = self.map(1, |builder| builder.push(source))?;
            }
            for _ in words..ROOT_WORDS {
                self = self.constant_i32(0)?;
            }
            return Ok(self);
        }
        let mut sources = EventSources::new();
        for source in root.sources {
            sources = sources.push(source);
        }
        self.map(ROOT_WORDS, |builder| builder.opaque(root.schema, sources))
    }

    fn finish(self) -> Result<Vec<EventBlock>, neo_wasm::WasmBuildError> {
        self.builder.finish()
    }
}

struct EntryInputs {
    next: usize,
}

impl EntryInputs {
    fn take(&mut self, words: usize, export: &FunctionExport) -> Result<u8, TemplateBuildError> {
        let input =
            u8::try_from(self.next).map_err(|_| TemplateBuildError::ExportArityOverflow {
                name: export.name.clone(),
            })?;
        self.next += words;
        u8::try_from(self.next)
            .map_err(|_| TemplateBuildError::ExportArityOverflow {
                name: export.name.clone(),
            })
            .map(|_| input)
    }
}

fn local_index(export: &FunctionExport, local: usize) -> Result<u8, TemplateBuildError> {
    u8::try_from(local).map_err(|_| TemplateBuildError::ExportArityOverflow {
        name: export.name.clone(),
    })
}

/// Bootstrap locals from entry inputs.
fn bootstrap_advice(
    export: &FunctionExport,
    inputs: &mut EntryInputs,
    locals: impl IntoIterator<Item = (usize, ValType)>,
    kind: &'static str,
) -> Result<Vec<EventBlock>, TemplateBuildError> {
    let mut builder = EventSequenceBuilder::advice();
    for (local, ty) in locals {
        let local_index = local_index(export, local)?;
        builder = match ty {
            ValType::I32 => builder.input_local_i32(inputs.take(1, export)?, local_index)?,
            ValType::I64 => builder.input_local_i64(inputs.take(2, export)?, local_index)?,
            ty => {
                return Err(TemplateBuildError::UnsupportedExportLocal {
                    name: export.name.clone(),
                    kind,
                    index: local,
                    ty,
                });
            }
        };
    }
    Ok(builder.finish()?)
}

/// Root over export parameters after `skip`.
fn entry_arguments_root(
    export: &FunctionExport,
    inputs: &mut EntryInputs,
    skip: usize,
) -> Result<Root, TemplateBuildError> {
    let types = &export.params[skip.min(export.params.len())..];
    let mut sources = Vec::with_capacity(types.len());
    for (offset, ty) in types.iter().copied().enumerate() {
        let local = local_index(export, skip + offset)?;
        let limbs = match ty {
            ValType::I32 | ValType::I64 => flat_limbs(ty),
            ty => {
                return Err(TemplateBuildError::UnsupportedExportLocal {
                    name: export.name.clone(),
                    kind: "parameter",
                    index: skip + offset,
                    ty,
                });
            }
        };
        let input = inputs.take(limbs.len(), export)?;
        sources.extend(
            limbs
                .enumerate()
                .map(|(offset, limb)| SlotBinding::InputLocal {
                    input: input + offset as u8,
                    local,
                    limb,
                }),
        );
    }
    Ok(Root::value(types, sources))
}

/// Root over an export result.
fn export_result_root(export: &FunctionExport) -> Result<Root, TemplateBuildError> {
    let sources = match export.results.as_ref() {
        [] => Vec::new(),
        [ty @ (ValType::I32 | ValType::I64)] => flat_limbs(*ty)
            .map(|limb| SlotBinding::OutputElem { limb })
            .collect(),
        _ => {
            return Err(TemplateBuildError::UnsupportedExportResult {
                name: export.name.clone(),
                results: export.results.clone(),
            });
        }
    };
    Ok(Root::value(&export.results, sources))
}

fn return_exit_events(root: Root) -> Result<Vec<EventBlock>, TemplateBuildError> {
    Ok(Sequence::op(EventKind::Return).root(root)?.finish()?)
}

fn bootstrap_receiver_and_locals(
    export: &FunctionExport,
    inputs: &mut EntryInputs,
) -> Result<Vec<EventBlock>, TemplateBuildError> {
    bootstrap_advice(
        export,
        inputs,
        std::iter::once((0, ValType::I32)).chain(declared_locals(export)),
        "bootstrap local",
    )
}

/// Declared locals, which must be reset on export entry.
fn declared_locals(export: &FunctionExport) -> impl Iterator<Item = (usize, ValType)> + '_ {
    export
        .locals
        .iter()
        .copied()
        .enumerate()
        .map(move |(offset, ty)| (export.params.len() + offset, ty))
}

fn require_receiver(
    export: &FunctionExport,
    kind: ExportKind,
    receiver: &'static str,
) -> Result<(), TemplateBuildError> {
    if matches!(export.params.first(), Some(ValType::I32)) {
        Ok(())
    } else {
        Err(TemplateBuildError::InvalidUtxoExportReceiver {
            name: export.name.clone(),
            kind: kind.name(),
            receiver,
        })
    }
}

/// Storage record types from the matching `set-storage` export.
fn storage_record_types(
    export: &FunctionExport,
    exports: &[FunctionExport],
) -> Result<Box<[ValType]>, TemplateBuildError> {
    // `set-storage` keeps the flat record as direct parameters; multi-result
    // `get-storage` instead returns an output-memory pointer.
    let (instance, _) = export
        .name
        .split_once('#')
        .expect("storage exports are instance-qualified");
    let sibling = format!("{instance}#{SET_STORAGE_EXPORT}");
    let setter = exports
        .iter()
        .find(|candidate| candidate.name == sibling)
        .ok_or_else(|| TemplateBuildError::MissingStorageSibling {
            name: export.name.clone(),
            sibling: sibling.clone(),
        })?;
    let Some((ValType::I32, record)) = setter.params.split_first() else {
        return Err(TemplateBuildError::InvalidStorageExport {
            name: sibling,
            message: format!(
                "expected an i32 context handle followed by the flat record, found {:?}",
                setter.params
            ),
        });
    };
    if setter.results.as_ref() != [ValType::I32] {
        return Err(TemplateBuildError::InvalidStorageExport {
            name: sibling,
            message: format!("expected one i32 result, found {:?}", setter.results),
        });
    }
    if let Some(ty) = record
        .iter()
        .find(|ty| !matches!(ty, ValType::I32 | ValType::I64))
    {
        return Err(TemplateBuildError::InvalidStorageExport {
            name: sibling,
            message: format!("unsupported flat storage field type {ty:?}"),
        });
    }
    Ok(record.into())
}

/// Canonical-ABI byte offsets for flat record fields.
fn record_field_offsets(types: &[ValType]) -> Vec<u32> {
    let mut cursor = 0_u32;
    types
        .iter()
        .map(|ty| {
            let size = match ty {
                ValType::I64 => 8,
                _ => 4,
            };
            let offset = cursor.div_ceil(size) * size;
            cursor = offset + size;
            offset
        })
        .collect()
}

/// Root over the record returned by `get-storage`.
fn get_storage_root(
    export: &FunctionExport,
    record: &[ValType],
) -> Result<Root, TemplateBuildError> {
    if export.params.as_ref() != [ValType::I32] || export.results.as_ref() != [ValType::I32] {
        return Err(TemplateBuildError::InvalidStorageExport {
            name: export.name.clone(),
            message: format!(
                "expected (i32) -> i32, found {:?} -> {:?}",
                export.params, export.results
            ),
        });
    }
    let sources = match record {
        [] => Vec::new(),
        [ValType::I32] => vec![SlotBinding::OutputElem { limb: Limb::Lo }],
        record => record
            .iter()
            .zip(record_field_offsets(record))
            .flat_map(|(ty, offset)| {
                flat_limbs(*ty)
                    .enumerate()
                    .map(move |(index, _)| SlotBinding::MemoryRead32 {
                        base: MemoryBase::Output,
                        byte_offset: offset + index as u32 * 4,
                    })
            })
            .collect(),
    };
    Ok(Root::value(record, sources))
}

fn build_export_events(
    export: &FunctionExport,
    kind: ExportKind,
    exports: &[FunctionExport],
) -> Result<ExportEvents, TemplateBuildError> {
    let mut inputs = EntryInputs { next: 0 };
    match kind {
        ExportKind::Constructor => {
            require_receiver(export, kind, "utxo-context handle")?;
            let entry = Sequence::op(EventKind::EnterConstructor).root(entry_arguments_root(
                export,
                &mut inputs,
                1,
            )?)?;
            let mut entry = entry.finish()?;
            entry.extend(bootstrap_receiver_and_locals(export, &mut inputs)?);
            // Constructor returns expose no caller-local handle.
            Ok(ExportEvents {
                entry,
                exit: return_exit_events(Root::unit())?,
                semantic: vec![
                    EventTemplate::new(EventKind::EnterConstructor),
                    EventTemplate::new(EventKind::Return),
                ],
            })
        }
        ExportKind::Method(method) => {
            require_receiver(export, kind, "resource receiver")?;
            let mut entry = Sequence::op(EventKind::EnterMethod);
            for word in method.0 {
                entry = entry.constant_i32(word)?;
            }
            entry = entry.root(entry_arguments_root(export, &mut inputs, 1)?)?;
            let mut entry = entry.finish()?;
            entry.extend(bootstrap_receiver_and_locals(export, &mut inputs)?);
            Ok(ExportEvents {
                entry,
                exit: return_exit_events(export_result_root(export)?)?,
                semantic: vec![
                    EventTemplate::with_method(EventKind::EnterMethod, method),
                    EventTemplate::new(EventKind::Return),
                ],
            })
        }
        ExportKind::CoordinationScript => {
            // The entrypoint has no semantic entry event.
            let entry = bootstrap_advice(
                export,
                &mut inputs,
                export
                    .params
                    .iter()
                    .copied()
                    .enumerate()
                    .chain(declared_locals(export)),
                "bootstrap local",
            )?;
            Ok(ExportEvents {
                entry,
                exit: return_exit_events(export_result_root(export)?)?,
                semantic: vec![EventTemplate::new(EventKind::Return)],
            })
        }
        ExportKind::GetStorage => {
            require_receiver(export, kind, "resource receiver")?;
            let record = storage_record_types(export, exports)?;
            let entry = bootstrap_receiver_and_locals(export, &mut inputs)?;
            let exit =
                Sequence::op(EventKind::GetStorage).root(get_storage_root(export, &record)?)?;
            Ok(ExportEvents {
                entry,
                exit: exit.finish()?,
                semantic: vec![EventTemplate::new(EventKind::GetStorage)],
            })
        }
        ExportKind::SetStorage => {
            require_receiver(export, kind, "utxo-context handle")?;
            storage_record_types(export, exports)?;
            let entry = Sequence::op(EventKind::SetStorage).root(entry_arguments_root(
                export,
                &mut inputs,
                1,
            )?)?;
            let mut entry = entry.finish()?;
            entry.extend(bootstrap_receiver_and_locals(export, &mut inputs)?);
            // The returned resource is not the coordinator-local binding.
            Ok(ExportEvents {
                entry,
                exit: Vec::new(),
                semantic: vec![EventTemplate::new(EventKind::SetStorage)],
            })
        }
        ExportKind::Internal => Ok(ExportEvents {
            entry: Vec::new(),
            exit: Vec::new(),
            semantic: Vec::new(),
        }),
    }
}

fn classify_import(import: &FunctionImport) -> Result<ImportKind, TemplateBuildError> {
    if import.module == UTXO_CONTEXT_INTERFACE {
        return Ok(match import.field.as_str() {
            RESUME_METHOD => ImportKind::YieldBegin,
            IMPLEMENTS_METHOD => ImportKind::RegisterMethod,
            _ => ImportKind::Advice,
        });
    }
    if import.module.starts_with(SELF_INTERFACE_PREFIX)
        || import.module.starts_with(TYPED_UTXO_INTERFACE_PREFIX)
    {
        if import.field.starts_with(CONSTRUCTOR_PREFIX) {
            return Ok(ImportKind::NewUtxo);
        }
        if let Some(method) = import.field.strip_prefix(METHOD_PREFIX) {
            return Ok(ImportKind::CallMethod(method_hash_from_segment(method)?));
        }
        return Ok(ImportKind::Advice);
    }
    if import.module == DYNAMIC_UTXO_INTERFACE {
        return Ok(ImportKind::CallMethod(method_hash_from_segment(
            &import.field,
        )?));
    }
    Ok(ImportKind::Advice)
}

fn build_import_template(
    import: &FunctionImport,
) -> Result<(Vec<EventBlock>, Option<EventTemplate>), TemplateBuildError> {
    match classify_import(import)? {
        ImportKind::YieldBegin => build_yield_begin_template(import),
        ImportKind::RegisterMethod => build_register_method_template(import),
        ImportKind::NewUtxo => build_new_utxo_template(import),
        ImportKind::CallMethod(method) => build_call_method_template(import, method),
        ImportKind::Advice => Ok((result_advice_events(import)?, None)),
    }
}

/// Root over import arguments after `skip`.
fn import_arguments_root(import: &FunctionImport, skip: usize) -> Result<Root, TemplateBuildError> {
    let types = &import.params[skip.min(import.params.len())..];
    let mut sources = Vec::with_capacity(types.len());
    for (offset, ty) in types.iter().copied().enumerate() {
        let index = skip + offset;
        let arg = u8::try_from(index).map_err(|_| TemplateBuildError::FunctionArityOverflow {
            module: import.module.clone(),
            field: import.field.clone(),
        })?;
        let limbs = match ty {
            ValType::I32 | ValType::I64 => flat_limbs(ty),
            ty => {
                return Err(TemplateBuildError::UnsupportedImportParameter {
                    module: import.module.clone(),
                    field: import.field.clone(),
                    index,
                    ty,
                });
            }
        };
        sources.extend(limbs.map(|limb| SlotBinding::ArgElem { arg, limb }));
    }
    Ok(Root::value(types, sources))
}

/// Root over an import result and any required high-lane advice.
fn import_result_root(
    import: &FunctionImport,
) -> Result<(Root, Vec<EventBlock>), TemplateBuildError> {
    Ok(match import.results.as_ref() {
        [] => (Root::unit(), Vec::new()),
        [ValType::I32] => (
            Root::value(
                &import.results,
                vec![SlotBinding::ResultElem { limb: Limb::Lo }],
            ),
            EventSequenceBuilder::advice()
                .push(SlotBinding::ResultElem { limb: Limb::Hi })?
                .finish()?,
        ),
        [ValType::I64] => (
            Root::value(
                &import.results,
                vec![
                    SlotBinding::ResultElem { limb: Limb::Lo },
                    SlotBinding::ResultElem { limb: Limb::Hi },
                ],
            ),
            Vec::new(),
        ),
        _ => {
            return Err(TemplateBuildError::UnsupportedImportResult {
                module: import.module.clone(),
                field: import.field.clone(),
                results: import.results.clone(),
            });
        }
    })
}

fn build_new_utxo_template(
    import: &FunctionImport,
) -> Result<(Vec<EventBlock>, Option<EventTemplate>), TemplateBuildError> {
    if import.results.as_ref() != [ValType::I32] {
        return Err(TemplateBuildError::InvalidConstructorResult {
            module: import.module.clone(),
            field: import.field.clone(),
            results: import.results.clone(),
        });
    }
    let sequence = Sequence::op(EventKind::NewUtxo).root(import_arguments_root(import, 0)?)?;
    // The i32 result's high lane is advice-only.
    let sequence = sequence.result()?;
    Ok((
        sequence.finish()?,
        Some(EventTemplate::new(EventKind::NewUtxo)),
    ))
}

fn build_yield_begin_template(
    import: &FunctionImport,
) -> Result<(Vec<EventBlock>, Option<EventTemplate>), TemplateBuildError> {
    // The receiver is advice-only.
    require_signature(import, &[ValType::I32], &[])?;
    Ok((
        Sequence::op(EventKind::YieldBegin).finish()?,
        Some(EventTemplate::new(EventKind::YieldBegin)),
    ))
}

fn build_register_method_template(
    import: &FunctionImport,
) -> Result<(Vec<EventBlock>, Option<EventTemplate>), TemplateBuildError> {
    require_signature(
        import,
        &[
            ValType::I32,
            ValType::I64,
            ValType::I64,
            ValType::I64,
            ValType::I64,
        ],
        &[],
    )?;
    let mut sequence = Sequence::op(EventKind::RegisterMethod);
    for arg in 1..=4 {
        sequence = sequence.arg_i64(arg)?;
    }
    Ok((
        sequence.finish()?,
        Some(EventTemplate::new(EventKind::RegisterMethod)),
    ))
}

fn build_call_method_template(
    import: &FunctionImport,
    method: MethodHash,
) -> Result<(Vec<EventBlock>, Option<EventTemplate>), TemplateBuildError> {
    if !matches!(import.params.first(), Some(ValType::I32)) {
        return Err(TemplateBuildError::InvalidMethodReceiver {
            module: import.module.clone(),
            field: import.field.clone(),
        });
    }
    let mut sequence = Sequence::op(EventKind::CallMethod).arg_i32(0)?;
    for word in method.0 {
        sequence = sequence.constant_i32(word)?;
    }
    sequence = sequence.root(import_arguments_root(import, 1)?)?;
    let (result, high_lane) = import_result_root(import)?;
    sequence = sequence.root(result)?;
    let mut events = sequence.finish()?;
    events.extend(high_lane);
    Ok((
        events,
        Some(EventTemplate::with_method(EventKind::CallMethod, method)),
    ))
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

fn result_advice_events(import: &FunctionImport) -> Result<Vec<EventBlock>, TemplateBuildError> {
    match import.results.len() {
        0 => Ok(Vec::new()),
        1 => Ok(EventSequenceBuilder::advice().result()?.finish()?),
        count => Err(TemplateBuildError::UnsupportedResultCount {
            module: import.module.clone(),
            field: import.field.clone(),
            count,
        }),
    }
}

fn method_hash_from_segment(method: &str) -> Result<MethodHash, TemplateBuildError> {
    if method.is_empty() {
        return Err(TemplateBuildError::InvalidMethodName(method.to_owned()));
    }
    Ok(method_hash_from_name(method))
}

pub(crate) fn first_core_module(wasm: &[u8]) -> Result<&[u8], TemplateBuildError> {
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

/// Flattened core parameter and result types of one function type.
type FunctionSignature = (Box<[ValType]>, Box<[ValType]>);

fn parse_function_types(module: &[u8]) -> Result<Vec<FunctionSignature>, TemplateBuildError> {
    let mut types = Vec::new();
    for payload in Parser::new(0).parse_all(module) {
        if let Payload::TypeSection(reader) = payload? {
            for rec_group in reader {
                for subtype in rec_group?.into_types() {
                    let type_index = types.len();
                    let CompositeInnerType::Func(function) = &subtype.composite_type.inner else {
                        return Err(TemplateBuildError::UnsupportedCoreType(type_index));
                    };
                    types.push((function.params().into(), function.results().into()));
                }
            }
        }
    }
    Ok(types)
}

fn parse_function_imports(
    module: &[u8],
    types: &[FunctionSignature],
) -> Result<Vec<FunctionImport>, TemplateBuildError> {
    let mut imports = Vec::new();

    for payload in Parser::new(0).parse_all(module) {
        if let Payload::ImportSection(reader) = payload? {
            for import in reader.into_imports() {
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
    }

    Ok(imports)
}

fn parse_function_exports(
    module: &[u8],
    types: &[FunctionSignature],
) -> Result<Vec<FunctionExport>, TemplateBuildError> {
    let mut function_types = Vec::<u32>::new();
    let mut imported_function_count = 0_usize;
    let mut raw_exports = Vec::<(u32, String)>::new();
    let mut code_locals = Vec::<Vec<(u32, ValType)>>::new();
    for payload in Parser::new(0).parse_all(module) {
        match payload? {
            Payload::ImportSection(reader) => {
                for import in reader.into_imports() {
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
        "import `{module}`.`{field}` has unsupported flattened parameter type {ty:?} at index {index}"
    )]
    UnsupportedImportParameter {
        module: String,
        field: String,
        index: usize,
        ty: ValType,
    },

    #[error(
        "import `{module}`.`{field}` has unsupported flat result types {results:?}; \
         only no result or one i32/i64 result is supported"
    )]
    UnsupportedImportResult {
        module: String,
        field: String,
        results: Box<[ValType]>,
    },

    #[error(
        "export `{name}` has unsupported flat result types {results:?}; \
         only no result or one i32/i64 result is supported"
    )]
    UnsupportedExportResult {
        name: String,
        results: Box<[ValType]>,
    },

    #[error("UTXO {kind} export `{name}` does not start with an i32 {receiver}")]
    InvalidUtxoExportReceiver {
        name: String,
        kind: &'static str,
        receiver: &'static str,
    },

    #[error("export `{name}` has unsupported flattened {kind} type {ty:?} at local {index}")]
    UnsupportedExportLocal {
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

    #[error("invalid host-event export bindings for `{name}`: {message}")]
    InvalidExportBindings { name: String, message: String },

    #[error("method name `{0}` has no method segment")]
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
        "builtin import `{module}`.`{field}` has unexpected core signature {params:?} -> {results:?}"
    )]
    InvalidBuiltinSignature {
        module: String,
        field: String,
        params: Box<[ValType]>,
        results: Box<[ValType]>,
    },

    #[error(
        "storage export `{name}` has no `{sibling}` sibling export to derive the storage \
         record's flat layout from"
    )]
    MissingStorageSibling { name: String, sibling: String },

    #[error("storage export `{name}` has an unexpected signature: {message}")]
    InvalidStorageExport { name: String, message: String },

    #[error("invalid semantic decoder template: {0}")]
    Decoder(#[from] BlockCodecError),

    #[error("invalid host-event import bindings for `{module}`.`{field}`: {message}")]
    InvalidImportBindings {
        module: String,
        field: String,
        message: String,
    },

    #[error("invalid component host-event bindings: {0}")]
    InvalidHostEventBindings(#[from] neo_wasm::WasmBuildError),

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

#[cfg(test)]
mod tests {
    use neo_wasm::host_event_bindings::{
        ExportTemplate, ImportTemplate, absorbed_blocks, expand_export_entry, expand_export_exit,
        expand_import_events,
    };
    use starstream_interleaving_spec::{Out, ResourceHandle, Step};

    use super::*;

    fn method_hash(name: &str) -> MethodHash {
        let digest = Sha256::digest(name.as_bytes());
        let mut words = [0; METHOD_WORDS];
        for (word, bytes) in words.iter_mut().zip(digest.chunks_exact(4)) {
            *word = u32::from_le_bytes(bytes.try_into().unwrap());
        }
        MethodHash(words)
    }

    fn limbs(value: u64) -> (u32, u32) {
        (value as u32, (value >> 32) as u32)
    }

    fn build(wat: &str, scripts: &[&str]) -> ComponentTemplates {
        build_component_templates(&wat::parse_str(wat).unwrap(), scripts).unwrap()
    }

    fn import<'a>(
        templates: &'a ComponentTemplates,
        module: &str,
        field: &str,
    ) -> (u32, &'a ImportTemplate) {
        let fref = templates.import_fref(module, field).unwrap();
        (fref, &templates.bindings.imports[&fref])
    }

    fn export<'a>(templates: &'a ComponentTemplates, name: &str) -> (u32, &'a ExportTemplate) {
        let fref = templates.export_fref(name).unwrap();
        (fref, &templates.bindings.exports[&fref])
    }

    /// Decode the transcript of one import call.
    fn call(
        templates: &ComponentTemplates,
        module: &str,
        field: &str,
        args: &[(u32, u32)],
        result: Option<(u32, u32)>,
    ) -> Trace {
        let (fref, template) = import(templates, module, field);
        let expanded = expand_import_events(template, args, result, &[], &[]).unwrap();
        let blocks = absorbed_blocks(&template.events, &expanded)
            .unwrap()
            .into_iter()
            .map(|words| AttributedBlock::new(words, fref, 0))
            .collect::<Vec<_>>();
        templates.decoder.decode_blocks(&blocks).unwrap()
    }

    /// Decode the entry transcript of one export turn.
    fn enter(templates: &ComponentTemplates, name: &str, inputs: &[u64]) -> Trace {
        let (fref, template) = export(templates, name);
        let expanded = expand_export_entry(template, inputs).unwrap();
        let blocks = absorbed_blocks(&template.entry, &expanded)
            .unwrap()
            .into_iter()
            .map(|words| AttributedBlock::new(words, fref, fref))
            .collect::<Vec<_>>();
        templates.decoder.decode_blocks(&blocks).unwrap()
    }

    /// Decode the exit transcript of one export turn.
    fn exit(
        templates: &ComponentTemplates,
        name: &str,
        output: Option<(u32, u32)>,
        memory_reads: &[u32],
    ) -> Trace {
        let (fref, template) = export(templates, name);
        let expanded = expand_export_exit(template, output, memory_reads).unwrap();
        let blocks = absorbed_blocks(&template.exit, &expanded)
            .unwrap()
            .into_iter()
            .map(|words| AttributedBlock::new(words, fref, fref))
            .collect::<Vec<_>>();
        templates.decoder.decode_blocks(&blocks).unwrap()
    }

    /// Bootstrap writes of an export entry: `(input, local, hi lane)`.
    fn bootstrap_writes(template: &ExportTemplate) -> Vec<(u8, u8, bool)> {
        template
            .entry
            .iter()
            .filter(|event| !event.absorb)
            .flat_map(|event| event.block)
            .filter_map(|slot| match slot {
                SlotBinding::InputLocal { input, local, limb } => {
                    Some((input, local, limb == Limb::Hi))
                }
                _ => None,
            })
            .collect()
    }

    const CORE_WASM_FIXTURE: &str = r#"
        (module
          (import "starstream:std/utxo-context" "[method]utxo-context.resume"
            (func (param i32)))
          (import "starstream:std/utxo-context" "[method]utxo-context.implements-method"
            (func (param i32 i64 i64 i64 i64)))
          (import "starstream:std/builtin" "[method]utxo.has-method"
            (func (param i32 i64 i64 i64 i64) (result i32)))
          (import "starstream:self/counter" "[static]utxo.new" (func (param i64) (result i32)))
          (import "starstream:self/counter" "[method]utxo.add" (func (param i32 i64)))
          (import "starstream:self/counter" "[method]utxo.read" (func (param i32) (result i32)))
          (import "starstream:contract/dynamic-utxo" "poke" (func (param i32 i32 i32)))
          (func (export "counter#[static]utxo.new") (param i32 i64) (result i32) (local i64)
            i32.const 0)
          (func (export "counter#[method]utxo.add") (param i32 i64) (local i32))
          (func (export "counter#[method]utxo.read") (param i32) (result i32)
            i32.const 41)
          (func (export "counter#get-storage") (param i32) (result i32)
            i32.const 8)
          (func (export "counter#set-storage") (param i32 i32 i64 i64) (result i32)
            i32.const 0)
          (func (export "example") (param i32 i64) (local i32))
          (func (export "example_unit"))
          (memory (export "memory") 1 1))
    "#;

    fn counter() -> ComponentTemplates {
        build(CORE_WASM_FIXTURE, &["example", "example_unit"])
    }

    #[test]
    fn method_names_are_hashed_in_starstream_spelling() {
        assert_eq!(
            method_hash_from_name("plus-chips"),
            method_hash("plus_chips")
        );
        assert_eq!(method_hash_from_name("add"), method_hash("add"));
    }

    #[test]
    fn flat_roots_are_inline_up_to_four_words_and_hashed_beyond() {
        assert_eq!(flat_value_root(&[], &[]), StarstreamValue::UNIT_VALUE);
        assert_eq!(
            flat_value_root(&[ValType::I64], &[55 | (7 << 32)]),
            StarstreamValue([55, 7, 0, 0])
        );
        assert_eq!(
            flat_value_root(&[ValType::I64, ValType::I64], &[1, 2]),
            StarstreamValue([1, 0, 2, 0])
        );
        let wide = [ValType::I32, ValType::I64, ValType::I64];
        assert_eq!(
            flat_value_root(&wide, &[1, 20, 30]).0,
            opaque_value_root(value_schema(&wide), &[1, 20, 0, 30, 0]).unwrap()
        );
    }

    #[test]
    fn constructor_roots_keep_resource_after_four_words() {
        for words in 0..=6 {
            let mut types = vec![ValType::I64; words / 2];
            if words % 2 != 0 {
                types.push(ValType::I32);
            }
            let params = types
                .iter()
                .map(|ty| if *ty == ValType::I64 { "i64" } else { "i32" })
                .collect::<Vec<_>>()
                .join(" ");
            let templates = build(
                &format!(
                    r#"(module (import "starstream:self/counter" "[static]utxo.new"
                        (func (param {params}) (result i32))))"#
                ),
                &[],
            );
            let values = types
                .iter()
                .map(|ty| {
                    if *ty == ValType::I64 {
                        13 | (7 << 32)
                    } else {
                        11
                    }
                })
                .collect::<Vec<_>>();
            let args = values.iter().copied().map(limbs).collect::<Vec<_>>();
            assert_eq!(
                call(
                    &templates,
                    "starstream:self/counter",
                    "[static]utxo.new",
                    &args,
                    Some((42, 0)),
                ),
                Trace::new([Step::NewUtxo {
                    arguments: flat_value_root(&types, &values),
                    resource: Out(ResourceHandle(42)),
                }]),
                "{words}-word arguments must leave the resource in its fixed slot",
            );
        }
    }

    #[test]
    fn constructor_boundary_commits_the_same_root_on_both_sides() {
        let templates = counter();
        let arguments = flat_value_root(&[ValType::I64], &[55]);
        assert_eq!(
            call(
                &templates,
                "starstream:self/counter",
                "[static]utxo.new",
                &[limbs(55)],
                Some((7, 0))
            ),
            Trace::new([Step::NewUtxo {
                arguments: arguments.clone(),
                resource: Out(ResourceHandle(7)),
            }])
        );
        // Entry inputs: the i64 argument's limbs, then the context handle,
        // then the declared i64 local.
        let (_, template) = export(&templates, "counter#[static]utxo.new");
        assert_eq!(template.entry_input_count, 5);
        assert_eq!(
            bootstrap_writes(template),
            [(2, 0, false), (3, 2, false), (4, 2, true)]
        );
        assert_eq!(
            enter(&templates, "counter#[static]utxo.new", &[55, 0, 3, 0, 0]),
            Trace::new([Step::EnterConstructor { arguments }])
        );
        assert_eq!(
            exit(&templates, "counter#[static]utxo.new", Some((0, 0)), &[]),
            Trace::new([Step::Return {
                result: Out(StarstreamValue::UNIT_VALUE)
            }])
        );
    }

    #[test]
    fn method_boundary_commits_method_words_and_matching_roots() {
        let templates = counter();
        let method = method_hash("add");
        let arguments = flat_value_root(&[ValType::I64], &[13]);
        let (_, template) = import(&templates, "starstream:self/counter", "[method]utxo.add");
        assert_eq!(
            absorbed_blocks(
                &template.events,
                &expand_import_events(template, &[(7, 0), limbs(13)], None, &[], &[]).unwrap()
            )
            .unwrap()
            .len(),
            3,
            "tag+resource+method, method tail+arguments, unit result"
        );
        assert_eq!(
            call(
                &templates,
                "starstream:self/counter",
                "[method]utxo.add",
                &[(7, 0), limbs(13)],
                None
            ),
            Trace::new([Step::CallMethod {
                resource: ResourceHandle(7),
                method,
                arguments: arguments.clone(),
                result: Out(StarstreamValue::UNIT_VALUE),
            }])
        );
        let (_, template) = export(&templates, "counter#[method]utxo.add");
        assert_eq!(bootstrap_writes(template), [(2, 0, false), (3, 2, false)]);
        assert_eq!(
            enter(&templates, "counter#[method]utxo.add", &[13, 0, 0, 0]),
            Trace::new([Step::EnterMethod { method, arguments }])
        );
    }

    #[test]
    fn i32_results_commit_one_word_on_both_sides() {
        let templates = counter();
        let result = flat_value_root(&[ValType::I32], &[41]);
        let Trace(steps) = call(
            &templates,
            "starstream:self/counter",
            "[method]utxo.read",
            &[(7, 0)],
            Some((41, 0)),
        );
        let [
            Step::CallMethod {
                result: Out(called),
                ..
            },
        ] = steps.as_slice()
        else {
            panic!("{steps:?}");
        };
        assert_eq!(*called, result);
        assert_eq!(
            exit(&templates, "counter#[method]utxo.read", Some((41, 0)), &[]),
            Trace::new([Step::Return {
                result: Out(result)
            }])
        );
    }

    #[test]
    fn dynamic_utxo_imports_are_method_calls() {
        let templates = counter();
        let arguments = flat_value_root(&[ValType::I32, ValType::I32], &[1, 2]);
        assert_eq!(
            call(
                &templates,
                "starstream:contract/dynamic-utxo",
                "poke",
                &[(7, 0), (1, 0), (2, 0)],
                None
            ),
            Trace::new([Step::CallMethod {
                resource: ResourceHandle(7),
                method: method_hash("poke"),
                arguments,
                result: Out(StarstreamValue::UNIT_VALUE),
            }])
        );
    }

    #[test]
    fn builtin_has_method_is_advice_only() {
        let templates = counter();
        let (fref, template) = import(
            &templates,
            "starstream:std/builtin",
            "[method]utxo.has-method",
        );
        assert!(template.events.iter().all(|event| !event.absorb));
        assert!(templates.decoder.get(fref, 6).is_none());
    }

    #[test]
    fn utxo_context_calls_are_yield_and_register_events() {
        let templates = counter();
        assert_eq!(
            call(
                &templates,
                "starstream:std/utxo-context",
                "[method]utxo-context.resume",
                &[(3, 0)],
                None
            ),
            Trace::new([Step::YieldBegin])
        );
        let method = method_hash("add");
        let mut args = vec![(3, 0)];
        args.extend(method.0.chunks_exact(2).map(|pair| (pair[0], pair[1])));
        assert_eq!(
            call(
                &templates,
                "starstream:std/utxo-context",
                "[method]utxo-context.implements-method",
                &args,
                None
            ),
            Trace::new([Step::RegisterMethod { method }])
        );
    }

    #[test]
    fn storage_exports_commit_the_record_root() {
        let templates = counter();
        let record = [ValType::I32, ValType::I64, ValType::I64];
        let storage = flat_value_root(&record, &[1, 20, 30]);
        // get-storage writes the record behind its result pointer: an i32 at
        // offset 0 and two i64s at offsets 8 and 16.
        let (_, template) = export(&templates, "counter#get-storage");
        assert_eq!(bootstrap_writes(template), [(0, 0, false)]);
        assert_eq!(
            exit(
                &templates,
                "counter#get-storage",
                Some((8, 0)),
                &[1, 20, 0, 30, 0]
            ),
            Trace::new([Step::GetStorage {
                storage: Out(storage.clone())
            }])
        );
        // set-storage receives the same record as flat parameters after the
        // context handle.
        let (_, template) = export(&templates, "counter#set-storage");
        assert_eq!(template.entry_input_count, 6);
        assert_eq!(bootstrap_writes(template), [(5, 0, false)]);
        assert_eq!(
            enter(&templates, "counter#set-storage", &[1, 20, 0, 30, 0, 3]),
            Trace::new([Step::SetStorage {
                storage,
                // TODO: review
                coordinator_handle: ResourceHandle(0),
            }])
        );
    }

    #[test]
    fn coordination_scripts_bootstrap_parameters_and_publish_only_their_return() {
        let templates = counter();
        let (fref, template) = export(&templates, "example");
        assert!(template.entry.iter().all(|event| !event.absorb));
        assert_eq!(
            bootstrap_writes(template),
            [(0, 0, false), (1, 1, false), (2, 1, true), (3, 2, false)]
        );
        assert!(templates.decoder.get(fref, 7).is_none());
        assert_eq!(
            exit(&templates, "example", None, &[]),
            Trace::new([Step::Return {
                result: Out(StarstreamValue::UNIT_VALUE)
            }])
        );
        let (_, template) = export(&templates, "example_unit");
        assert!(template.entry.is_empty());
    }

    #[test]
    fn rejects_plain_exports_outside_the_coordination_allowlist() {
        let error =
            build_component_templates(&wat::parse_str(CORE_WASM_FIXTURE).unwrap(), &["example"])
                .err()
                .unwrap();
        assert!(
            matches!(error, TemplateBuildError::UnclassifiedFunctionExport { ref name } if name == "example_unit"),
            "{error}"
        );
    }

    #[test]
    fn rejects_missing_coordination_allowlist_entries() {
        let error = build_component_templates(
            &wat::parse_str(CORE_WASM_FIXTURE).unwrap(),
            &["example", "example_unit", "missing"],
        )
        .err()
        .unwrap();
        assert!(
            matches!(error, TemplateBuildError::MissingCoordinationExport { ref name } if name == "missing"),
            "{error}"
        );
    }

    #[test]
    fn rejects_constructor_exports_without_a_context_handle() {
        let wat = r#"
            (module
              (func (export "counter#[static]utxo.new") (param i64) (result i32) i32.const 0)
              (func (export "example")))
        "#;
        let error = build_component_templates(&wat::parse_str(wat).unwrap(), &["example"])
            .err()
            .unwrap();
        assert!(
            matches!(error, TemplateBuildError::InvalidUtxoExportReceiver { .. }),
            "{error}"
        );
    }
}
