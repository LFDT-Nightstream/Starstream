//! Canonical ABI result layouts for effect imports and handler exports.
//! Scalar handler values are wrapped as `(status, value)` by the compiler.
//! Core `(result i32)` alone cannot distinguish a status from that wrapper pointer.
//! User-level aggregate effect results are not supported here.
use std::collections::BTreeMap;

use wasmparser::ValType;
use wit_parser::{Resolve, SizeAlign, Type, TypeDefKind, WorldItem};

use crate::TemplateBuildError;

#[derive(Clone, Debug, Default)]
pub(crate) struct ResultLayout {
    pub types: Vec<ValType>,
    /// One entry per 32-bit limb: (byte offset, memory access width in bytes).
    pub lanes: Vec<(u32, u8)>,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct EffectAbi {
    pub result: ResultLayout,
    pub handler_value_offset: u32,
}

#[derive(Default)]
pub(crate) struct EffectAbis {
    pub exports: BTreeMap<String, EffectAbi>,
}

fn error(message: impl Into<String>) -> TemplateBuildError {
    TemplateBuildError::EffectAbi {
        message: message.into(),
    }
}

impl EffectAbis {
    pub fn decode(wasm: &[u8]) -> Result<Self, TemplateBuildError> {
        let (resolve, world) = if wasmparser::Parser::is_core_wasm(wasm) {
            let (_, metadata) = wit_component::metadata::decode(wasm)
                .map_err(|e| error(format!("could not decode effect metadata: {e:#}")))?;
            (metadata.resolve, metadata.world)
        } else {
            match wit_parser::decoding::decode(wasm)
                .map_err(|e| error(format!("could not decode component types: {e:#}")))?
            {
                wit_parser::decoding::DecodedWasm::Component(resolve, world) => (resolve, world),
                _ => return Err(error("expected a component, not a WIT package")),
            }
        };
        let mut sizes = SizeAlign::default();
        sizes
            .fill(&resolve)
            .map_err(|e| error(format!("could not lay out WIT types: {e:#}")))?;
        let mut abis = Self::default();
        for (handler, items) in [
            (false, &resolve.worlds[world].imports),
            (true, &resolve.worlds[world].exports),
        ] {
            for (key, item) in items {
                let name = resolve.name_world_key(key);
                let prefix = if handler {
                    "starstream:effect-handler/"
                } else {
                    "starstream:effects/"
                };
                if !name.starts_with(prefix) {
                    continue;
                }
                let WorldItem::Interface { id, .. } = item else {
                    continue;
                };
                for (function_name, function) in &resolve.interfaces[*id].functions {
                    let mut abi = EffectAbi::default();
                    let result = if handler {
                        match function.result.map(|ty| unalias(&resolve, ty)) {
                            Some(Type::U8) => None,
                            Some(Type::Id(id)) => {
                                let TypeDefKind::Tuple(tuple) = &resolve.types[id].kind else {
                                    return Err(error(format!(
                                        "handler {name}#{function_name} must return a status or (status, value)"
                                    )));
                                };
                                if tuple.types.len() != 2
                                    || unalias(&resolve, tuple.types[0]) != Type::U8
                                {
                                    return Err(error(format!(
                                        "invalid handler result for {name}#{function_name}"
                                    )));
                                }
                                abi.handler_value_offset =
                                    sizes.field_offsets(&tuple.types)[1].0.size_wasm32() as u32;
                                Some(tuple.types[1])
                            }
                            _ => {
                                return Err(error(format!(
                                    "invalid handler status for {name}#{function_name}"
                                )));
                            }
                        }
                    } else {
                        function.result
                    };
                    if let Some(ty) = result {
                        scalar_layout(&resolve, ty, &mut abi.result)?;
                    }
                    if handler {
                        abis.exports.insert(format!("{name}#{function_name}"), abi);
                    }
                }
            }
        }
        Ok(abis)
    }
}

fn unalias(resolve: &Resolve, mut ty: Type) -> Type {
    while let Type::Id(id) = ty {
        if let TypeDefKind::Type(inner) = resolve.types[id].kind {
            ty = inner;
        } else {
            break;
        }
    }
    ty
}

fn scalar_layout(
    resolve: &Resolve,
    ty: Type,
    out: &mut ResultLayout,
) -> Result<(), TemplateBuildError> {
    let ty = unalias(resolve, ty);
    let width = match ty {
        Type::Bool | Type::U8 => 1,
        Type::U16 => 2,
        Type::S32 | Type::U32 => 4,
        Type::S64 | Type::U64 => 8,
        Type::Id(_) => {
            return Err(error(
                "only scalar effect results are supported by proving templates",
            ));
        }
        // Signed subword loads need sign extension in the event-binding language.
        other => return Err(error(format!("unsupported effect result type: {other:?}"))),
    };
    out.types.push(if width == 8 {
        ValType::I64
    } else {
        ValType::I32
    });
    out.lanes.push((0, width.min(4)));
    if width == 8 {
        out.lanes.push((4, 4));
    }
    Ok(())
}
