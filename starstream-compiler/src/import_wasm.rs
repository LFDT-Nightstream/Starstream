use std::sync::Arc;

use miette::miette;
use starstream_types::{
    DUMMY_SPAN, FunctionKind, FunctionType, Identifier, IntWidth, NameId, StaticFunction,
    Type::{self, Function},
    TypedFunctionParam,
};
use wit_parser::{Resolve, WorldItem, WorldKey, decoding::DecodedWasm};

use crate::typecheck::env::{ConstantInfo, Namespace};

pub fn import_wasm(name_id: &mut NameId, wasm: &[u8]) -> miette::Result<Namespace> {
    // Accepts both component .wasm files and core .wasm files with a binary WIT custom section.
    let decoded = wit_parser::decoding::decode(wasm)
        .map_err(|e| miette!("error decoding .wasm file: {e}"))?;
    let (mut resolve, world_id) = match decoded {
        DecodedWasm::Component(resolve, world) => (resolve, world),
        DecodedWasm::WitPackage(_, _) => {
            // Reached if the file is a component .wasm with no implementation attached (binary WIT).
            return Err(miette!(
                "cannot import `.wasm` file containing binary WIT only"
            ));
        }
    };

    // We now have resolve, package, and world.
    // "Importize" the world to convert it to WIT that would be imported.
    resolve
        .importize(world_id, None)
        .map_err(|e| miette!("error importizing WIT world: {e}"))?;
    let world = &resolve.worlds[world_id];

    let mut namespace = Namespace::default();
    for (key, item) in &world.imports {
        let WorldKey::Name(name) = key else { continue };
        match item {
            WorldItem::Function(function) => {
                let id = name_id.fresh();
                namespace.constants.insert(
                    from_kebab_case(name),
                    ConstantInfo::new(
                        DUMMY_SPAN,
                        Function(Arc::new(FunctionType {
                            kind: FunctionKind::Normal,
                            name_span: DUMMY_SPAN,
                            params: function
                                .params
                                .iter()
                                .map(|p| TypedFunctionParam {
                                    public: false,
                                    name: Identifier::anon(&p.name),
                                    ty: wit_to_star_type(&resolve, p.ty),
                                    ty_span: DUMMY_SPAN,
                                })
                                .collect(),
                            result: function
                                .result
                                .map_or(Type::Unit, |ty| wit_to_star_type(&resolve, ty)),
                            callee: Some(StaticFunction::Named(id)),
                        })),
                    ),
                );
            }
            _ => todo!(),
        }
    }

    Ok(namespace)
}

fn wit_to_star_type(resolve: &Resolve, ty: wit_parser::Type) -> Type {
    match ty {
        wit_parser::Type::Bool => Type::Bool,
        wit_parser::Type::U8 => Type::Int(IntWidth::U8),
        wit_parser::Type::U16 => Type::Int(IntWidth::U16),
        wit_parser::Type::U32 => Type::Int(IntWidth::U32),
        wit_parser::Type::U64 => Type::Int(IntWidth::U64),
        wit_parser::Type::S8 => Type::Int(IntWidth::I8),
        wit_parser::Type::S16 => Type::Int(IntWidth::I16),
        wit_parser::Type::S32 => Type::Int(IntWidth::I32),
        wit_parser::Type::S64 => Type::Int(IntWidth::I64),
        wit_parser::Type::F32 => todo!(),
        wit_parser::Type::F64 => todo!(),
        wit_parser::Type::Char => todo!(),
        wit_parser::Type::String => todo!(),
        wit_parser::Type::ErrorContext => todo!(),
        wit_parser::Type::Id(id) => {
            let ty = &resolve.types[id];
            match &ty.kind {
                wit_parser::TypeDefKind::Tuple(tuple) => Type::Tuple(Arc::new(
                    tuple
                        .types
                        .iter()
                        .map(|&ty| wit_to_star_type(resolve, ty))
                        .collect(),
                )),
                _ => todo!(),
            }
        }
    }
}

fn from_kebab_case(name: &str) -> String {
    // TODO: probably a better way to do this is to check that to_kebab_case(imported_name) == wit_name.
    name.replace("-", "_")
}
