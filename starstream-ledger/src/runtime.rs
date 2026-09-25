use core::slice;

use anyhow::{Context as _, bail, ensure};
use minicbor::{Decode, Encode};
use serde::Serialize;
use wasm_encoder::reencode::{Reencode, ReencodeComponent};
use wasmparser::{DataSectionReader, MemorySectionReader, Parser, Payload};

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Encode, Decode, Serialize)]
pub enum GlobalValue {
    #[n(0)]
    I32(#[n(0)] i32),
    #[n(1)]
    I64(#[n(0)] i64),
    #[n(2)]
    F32(#[n(0)] u32),
    #[n(3)]
    F64(#[n(0)] u64),
    #[n(4)]
    V128(#[cbor(n(0), with = "minicbor::bytes")] [u8; 16]),
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Encode, Decode, Serialize)]
pub struct DataState {
    #[n(0)]
    pub memory_index: u32,
    #[n(1)]
    pub offset: u32,
    #[cbor(n(2), with = "minicbor::bytes")]
    #[serde(serialize_with = "crate::serialize_bytes")]
    pub data: Vec<u8>,
}

impl DataState {
    fn encode(&self, section: &mut wasm_encoder::DataSection) {
        section.active(
            self.memory_index,
            &wasm_encoder::ConstExpr::i32_const(self.offset.cast_signed()),
            self.data.iter().copied(),
        );
    }
}

#[derive(Default, Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Encode, Decode, Serialize)]
pub struct ModuleState {
    #[n(0)]
    pub memories: Vec<u64>,
    #[n(1)]
    pub globals: Vec<GlobalValue>,
    #[n(2)]
    pub data: Vec<DataState>,
}

/// Parse the state of every core module in `wasm`, in definition order.
/// This function performs no input validation and assumes Wasm to be valid.
pub fn parse_state(wasm: &[u8]) -> impl Iterator<Item = anyhow::Result<ModuleState>> {
    Parser::new(0)
        .parse_all(wasm)
        .filter_map(|payload| match payload {
            Ok(Payload::ModuleSection {
                parser,
                unchecked_range,
            }) => Some(parse_module_section(parser, &wasm[unchecked_range])),
            Err(err) => Some(Err(err.into())),
            _ => None,
        })
}

fn parse_module_section(parser: wasmparser::Parser, wasm: &[u8]) -> anyhow::Result<ModuleState> {
    let mut state = ModuleState::default();
    for payload in parser.parse_all(wasm) {
        let payload = payload?;
        match payload {
            Payload::ImportSection(reader) => {
                for import in reader.into_imports() {
                    let wasmparser::Import { module, name, ty } = import?;
                    if let wasmparser::TypeRef::Memory(..) | wasmparser::TypeRef::Global(..) = ty {
                        bail!(
                            "module `{module}` imports `{name}`, which is of unsupported type `{ty:?}`",
                        )
                    }
                }
            }
            Payload::MemorySection(reader) => {
                for memory in reader.into_iter() {
                    let wasmparser::MemoryType { initial, .. } = memory?;
                    state.memories.push(initial);
                }
            }
            Payload::GlobalSection(reader) => {
                for global in reader.into_iter() {
                    let wasmparser::Global {
                        ty: wasmparser::GlobalType { content_type, .. },
                        init_expr,
                    } = global?;
                    ensure!(content_type.is_defaultable());

                    let mut init_expr = init_expr.get_operators_reader();
                    let op = init_expr.read()?;
                    ensure!(
                        init_expr.is_end_then_eof(),
                        "global initialization expression is not a single instruction"
                    );
                    match op {
                        wasmparser::Operator::I32Const { value } => {
                            state.globals.push(GlobalValue::I32(value))
                        }
                        wasmparser::Operator::I64Const { value } => {
                            state.globals.push(GlobalValue::I64(value))
                        }
                        wasmparser::Operator::F32Const { value } => {
                            state.globals.push(GlobalValue::F32(value.bits()))
                        }
                        wasmparser::Operator::F64Const { value } => {
                            state.globals.push(GlobalValue::F64(value.bits()))
                        }
                        wasmparser::Operator::V128Const { value } => {
                            state.globals.push(GlobalValue::V128(*value.bytes()))
                        }
                        op => {
                            bail!("unexpected global initialization expression operator `{op:?}`")
                        }
                    }
                }
            }
            Payload::DataSection(reader) => {
                for data in reader.into_iter() {
                    let wasmparser::Data {
                        kind:
                            wasmparser::DataKind::Active {
                                memory_index,
                                offset_expr,
                            },
                        data,
                        ..
                    } = data?
                    else {
                        continue;
                    };
                    let mut offset_expr = offset_expr.get_operators_reader();
                    let op = offset_expr.read()?;
                    ensure!(
                        offset_expr.is_end_then_eof(),
                        "active data section offset expression is not a single instruction"
                    );
                    let offset = match op {
                        wasmparser::Operator::I32Const { value } => value,
                        op => {
                            bail!(
                                "unexpected active data section offset expression operator `{op:?}`"
                            )
                        }
                    };
                    state.data.push(DataState {
                        memory_index,
                        offset: offset.cast_unsigned(),
                        data: data.into(),
                    })
                }
            }
            _ => {}
        }
    }
    Ok(state)
}

type ReencodeError = wasm_encoder::reencode::Error<anyhow::Error>;

/// Apply component the state previously parsed via [parse_state].
pub fn apply_state<'a>(
    wasm: &[u8],
    state: impl IntoIterator<Item = &'a ModuleState>,
) -> anyhow::Result<Vec<u8>> {
    let state = state.into_iter();
    let mut enc = ComponentEncoder { state };
    let mut component = wasm_encoder::Component::new();
    enc.parse_component(&mut component, Parser::new(0), wasm)
        .map_err(|err| match err {
            ReencodeError::UserError(err) => err,
            ReencodeError::ParseError(err) => err.into(),
            err => anyhow::Error::msg(err),
        })?;
    ensure!(
        enc.state.next().is_none(),
        "state defines more core modules than the contract"
    );
    Ok(component.finish())
}

struct ComponentEncoder<I> {
    state: I,
}

impl<I> Reencode for ComponentEncoder<I> {
    type Error = anyhow::Error;
}

impl<'a, I: Iterator<Item = &'a ModuleState>> ReencodeComponent for ComponentEncoder<I> {
    fn parse_component_submodule(
        &mut self,
        component: &mut wasm_encoder::Component,
        parser: Parser,
        wasm: &[u8],
    ) -> Result<(), ReencodeError> {
        let state = self
            .state
            .next()
            .context("state defines fewer core modules than the contract")
            .map_err(ReencodeError::UserError)?;
        let mut encoder = ModuleEncoder {
            state,
            memories: state.memories.iter(),
            globals: state.globals.iter(),
            data: Some(&state.data),
        };
        let mut module = wasm_encoder::Module::new();
        wasm_encoder::reencode::utils::parse_core_module(&mut encoder, &mut module, parser, wasm)?;
        encoder
            .finish(&mut module)
            .map_err(ReencodeError::UserError)?;
        component.section(&wasm_encoder::ModuleSection(&module));
        Ok(())
    }
}

struct ModuleEncoder<'a> {
    state: &'a ModuleState,
    memories: slice::Iter<'a, u64>,
    globals: slice::Iter<'a, GlobalValue>,
    data: Option<&'a [DataState]>,
}

impl ModuleEncoder<'_> {
    fn finish(mut self, module: &mut wasm_encoder::Module) -> anyhow::Result<()> {
        ensure!(
            self.memories.next().is_none(),
            "state defines more memories than the contract module"
        );
        ensure!(
            self.globals.next().is_none(),
            "state defines more globals than the contract module"
        );
        if let Some(segments) = self.data
            && !segments.is_empty()
        {
            let mut section = wasm_encoder::DataSection::new();
            for state in segments {
                state.encode(&mut section);
            }
            module.section(&section);
        }
        Ok(())
    }
}

impl Reencode for ModuleEncoder<'_> {
    type Error = anyhow::Error;

    fn parse_memory_section(
        &mut self,
        memories: &mut wasm_encoder::MemorySection,
        section: MemorySectionReader<'_>,
    ) -> Result<(), ReencodeError> {
        for memory in section {
            let ty = self.memory_type(memory?)?;
            let minimum = self
                .memories
                .next()
                .context("state defines fewer memories than the contract module")
                .map_err(ReencodeError::UserError)?;
            memories.memory(wasm_encoder::MemoryType {
                minimum: *minimum,
                ..ty
            });
        }
        Ok(())
    }

    fn parse_global(
        &mut self,
        globals: &mut wasm_encoder::GlobalSection,
        wasmparser::Global { ty, .. }: wasmparser::Global<'_>,
    ) -> Result<(), ReencodeError> {
        let ty = self.global_type(ty)?;
        let value = self
            .globals
            .next()
            .context("state defines fewer globals than the contract module")
            .map_err(ReencodeError::UserError)?;
        let expr = match value {
            GlobalValue::I32(v) => wasm_encoder::ConstExpr::i32_const(*v),
            GlobalValue::I64(v) => wasm_encoder::ConstExpr::i64_const(*v),
            GlobalValue::F32(v) => {
                wasm_encoder::ConstExpr::f32_const(wasm_encoder::Ieee32::new(*v))
            }
            GlobalValue::F64(v) => {
                wasm_encoder::ConstExpr::f64_const(wasm_encoder::Ieee64::new(*v))
            }
            GlobalValue::V128(v) => wasm_encoder::ConstExpr::v128_const(i128::from_le_bytes(*v)),
        };
        globals.global(ty, &expr);
        Ok(())
    }

    fn data_count(&mut self, count: u32) -> Result<u32, ReencodeError> {
        let n = u32::try_from(self.state.data.len())
            .context("state data segment count does not fit in u32")
            .map_err(ReencodeError::UserError)?;
        n.checked_add(count)
            .context("merged data segment count does not fit in u32")
            .map_err(ReencodeError::UserError)
    }

    fn parse_data_section(
        &mut self,
        data: &mut wasm_encoder::DataSection,
        section: DataSectionReader<'_>,
    ) -> Result<(), ReencodeError> {
        for seg in section {
            let seg = seg?;
            match seg.kind {
                wasmparser::DataKind::Active { .. } => data.passive([]),
                wasmparser::DataKind::Passive => data.passive(seg.data.iter().copied()),
            };
        }
        if let Some(segments) = self.data.take() {
            for state in segments {
                state.encode(data);
            }
        }
        Ok(())
    }
}
