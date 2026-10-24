use std::collections::HashMap;

use anyhow::{Context as _, bail, ensure};
use bytes::Bytes;
use wasmparser::component_types::{
    ComponentAnyTypeId, ComponentDefinedType, ComponentEntityType, ComponentValType,
};
use wasmparser::types::Types;
use wasmparser::{ComponentExternalKind, Encoding, Parser, Payload, PrimitiveValType};
use wit_parser::{
    Docs, Function, FunctionKind, IndexMap, Interface, Package, PackageName, Param, Resolve, Span,
    Stability, Type, TypeDef, TypeDefKind, TypeOwner,
};

pub const LEDGER_WIT: &str = include_str!("../../wit/ledger.wit");
pub const LEDGER_WASM: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/ledger.wasm"));

/// Build the WIT of every UTXO instance exported by the component `wasm`, validated into `types`,
/// keyed by instance name, as text and encoded as Wasm.
///
/// Each is the `starstream:ledger` package containing the `utxo` interface, which exposes the
/// methods of the UTXO exported by the instance the way `/rpc` invokes them: as freestanding
/// functions taking the output reference followed by the method parameters, which are prefixed
/// with `arg-` so they never clash with the output reference parameters. Only the shapes the
/// compiler produces are handled: instances exporting a `utxo` resource, with synchronous methods
/// taking and returning booleans, integers and options, results and tuples of them. Methods
/// taking or returning anything else are rejected.
pub fn utxo_wits(wasm: &[u8], types: &Types) -> anyhow::Result<HashMap<Box<str>, (Bytes, Bytes)>> {
    let mut instances = Vec::new();
    let mut depth = 0usize;
    for payload in Parser::new(0).parse_all(wasm) {
        let payload = payload?;
        match payload {
            Payload::Version {
                encoding: Encoding::Module,
                ..
            } if depth == 0 => bail!("Wasm is not a component"),
            Payload::ModuleSection { .. } | Payload::ComponentSection { .. } => depth += 1,
            Payload::End(..) => depth = depth.saturating_sub(1),
            Payload::ComponentExportSection(exports) if depth == 0 => {
                for export in exports {
                    let export = export?;
                    if export.kind != ComponentExternalKind::Instance {
                        continue;
                    }
                    let item = types
                        .component_item_for_export(export.name.name)
                        .with_context(|| format!("export `{}` not found", export.name.name))?;
                    let ComponentEntityType::Instance(id) = item.ty else {
                        bail!("export `{}` is not an instance", export.name.name)
                    };
                    let exports = &types[id].exports;
                    if let Some(utxo) = exports.get("utxo")
                        && let ComponentEntityType::Type {
                            created: ComponentAnyTypeId::Resource(..),
                            ..
                        } = utxo.ty
                    {
                        instances.push((export.name.name, exports));
                    }
                }
            }
            _ => {}
        }
    }

    let mut resolve = Resolve::default();
    let transaction = resolve.types.alloc(TypeDef {
        name: None,
        kind: TypeDefKind::Option(Type::String),
        owner: TypeOwner::None,
        docs: Docs::default(),
        stability: Stability::Unknown,
        span: Span::default(),
        external_id: None,
    });
    let pkg = resolve.packages.alloc(Package {
        name: PackageName {
            namespace: "starstream".into(),
            name: "ledger".into(),
            version: None,
        },
        docs: Docs::default(),
        interfaces: Default::default(),
        worlds: Default::default(),
    });
    let utxo = resolve.interfaces.alloc(Interface {
        name: Some("utxo".into()),
        types: Default::default(),
        functions: Default::default(),
        docs: Docs::default(),
        stability: Stability::Unknown,
        package: Some(pkg),
        span: Span::default(),
        clone_of: None,
    });
    resolve.packages[pkg].interfaces.insert("utxo".into(), utxo);

    let mut wits = HashMap::new();
    for (instance, exports) in instances {
        let mut functions = IndexMap::default();
        for (name, item) in exports {
            let Some(name) = name.strip_prefix("[method]utxo.") else {
                continue;
            };
            let ComponentEntityType::Func(id) = item.ty else {
                bail!(
                    "export `[method]utxo.{name}` of UTXO instance `{instance}` is not a function"
                )
            };
            let func = &types[id];
            ensure!(
                !func.async_,
                "method `{name}` of UTXO instance `{instance}` is asynchronous",
            );
            let mut params = Vec::with_capacity(func.params.len() + 1);
            params.extend([
                Param {
                    name: "transaction".into(),
                    ty: Type::Id(transaction),
                    span: Span::default(),
                },
                Param {
                    name: "index".into(),
                    ty: Type::U32,
                    span: Span::default(),
                },
            ]);
            for (param, ty) in func.params.iter().skip(1) {
                let ty = convert(&mut resolve, types, ty).with_context(|| {
                    format!(
                        "parameter `{param}` of method `{name}` of UTXO instance `{instance}` has an unsupported type"
                    )
                })?;
                params.push(Param {
                    name: format!("arg-{param}"),
                    ty,
                    span: Span::default(),
                });
            }
            let result = match &func.result {
                Some(ty) => {
                    let ty = convert(&mut resolve, types, ty).with_context(|| {
                        format!(
                            "method `{name}` of UTXO instance `{instance}` has an unsupported result type"
                        )
                    })?;
                    Some(ty)
                }
                None => None,
            };
            functions.insert(
                name.to_string(),
                Function {
                    name: name.to_string(),
                    kind: FunctionKind::Freestanding,
                    params,
                    result,
                    docs: Docs::default(),
                    stability: Stability::Unknown,
                    span: Span::default(),
                    external_id: None,
                },
            );
        }
        resolve.interfaces[utxo].functions = functions;

        let mut printer = wit_component::WitPrinter::default();
        () = printer
            .print(&resolve, pkg, &[])
            .with_context(|| format!("failed to print WIT of instance `{instance}`"))?;
        let wasm = wit_component::encode(&resolve, pkg, false)
            .with_context(|| format!("failed to encode WIT of instance `{instance}`"))?;
        wits.insert(
            instance.into(),
            (printer.output.to_string().into(), wasm.into()),
        );
    }
    Ok(wits)
}

/// Convert `ty`, if the compiler passes values of its type to or from UTXO methods, all of which
/// `/rpc` can pass, into an anonymous type of `resolve`.
fn convert(resolve: &mut Resolve, types: &Types, ty: &ComponentValType) -> Option<Type> {
    let id = match *ty {
        ComponentValType::Primitive(ty) => return convert_primitive(ty),
        ComponentValType::Type(id) => id,
    };
    let kind = match &types[id] {
        ComponentDefinedType::Primitive(ty) => return convert_primitive(*ty),
        ComponentDefinedType::Option { ty, .. } => {
            let ty = convert(resolve, types, ty)?;
            TypeDefKind::Option(ty)
        }
        ComponentDefinedType::Result { ok, err, .. } => {
            let ok = match ok {
                Some(ty) => {
                    let ty = convert(resolve, types, ty)?;
                    Some(ty)
                }
                None => None,
            };
            let err = match err {
                Some(ty) => {
                    let ty = convert(resolve, types, ty)?;
                    Some(ty)
                }
                None => None,
            };
            TypeDefKind::Result(wit_parser::Result_ { ok, err })
        }
        ComponentDefinedType::Tuple(tuple) => {
            let items = tuple
                .types
                .iter()
                .map(|ty| convert(resolve, types, ty))
                .collect::<Option<_>>()?;
            TypeDefKind::Tuple(wit_parser::Tuple { types: items })
        }
        _ => return None,
    };
    Some(Type::Id(resolve.types.alloc(TypeDef {
        name: None,
        kind,
        owner: TypeOwner::None,
        docs: Docs::default(),
        stability: Stability::Unknown,
        span: Span::default(),
        external_id: None,
    })))
}

fn convert_primitive(ty: PrimitiveValType) -> Option<Type> {
    match ty {
        PrimitiveValType::Bool => Some(Type::Bool),
        PrimitiveValType::U8 => Some(Type::U8),
        PrimitiveValType::U16 => Some(Type::U16),
        PrimitiveValType::U32 => Some(Type::U32),
        PrimitiveValType::U64 => Some(Type::U64),
        PrimitiveValType::S8 => Some(Type::S8),
        PrimitiveValType::S16 => Some(Type::S16),
        PrimitiveValType::S32 => Some(Type::S32),
        PrimitiveValType::S64 => Some(Type::S64),
        _ => None,
    }
}
