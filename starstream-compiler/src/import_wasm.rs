use std::{collections::HashMap, error::Error, fmt::Display, sync::Arc};

use miette::Diagnostic;
use starstream_types::{
    DUMMY_SPAN, FunctionKind, FunctionType, Identifier, IntWidth, NameId, StaticFunction, Type,
    TypedFunctionParam,
};
use wit_parser::{Function, Resolve, TypeDef, WorldId, WorldItem, WorldKey, decoding::DecodedWasm};

use crate::typecheck::env::{ConstantInfo, Namespace};

#[derive(Debug)]
pub struct TypedWasmModule {
    pub name: String,
    pub wasm: Arc<[u8]>,
    pub linkage: WasmLinkage,
    pub resolve: Resolve,
    pub world_id: WorldId,
    pub functions: HashMap<NameId, (String, Arc<FunctionType>)>,
}

#[derive(Debug, Clone, Copy)]
pub enum WasmLinkage {
    External,
    Core,
    Component,
}

#[derive(Debug)]
pub enum ImportWasmError {
    DecodeComponent(Box<dyn Error + Send + Sync>),
    DecodeCore(Box<dyn Error + Send + Sync>),
    UntypedCore,
    TypesOnly,
    Importize(Box<dyn Error + Send + Sync>),
}

struct TypeNotAvailable;

pub fn import_wasm(
    next_id: &mut NameId,
    name: String,
    wasm: &Arc<[u8]>,
) -> Result<(Namespace, TypedWasmModule), ImportWasmError> {
    // Accepts both component .wasm files and core .wasm files with a binary WIT custom section.
    let (mut resolve, world_id, linkage) = if wasmparser::Parser::is_component(wasm) {
        match wit_parser::decoding::decode(wasm)
            .map_err(|error| ImportWasmError::DecodeComponent(error.into()))?
        {
            // Component with root world.
            DecodedWasm::Component(resolve, world) => (resolve, world, WasmLinkage::Component),
            // Component containing binary WIT only.
            DecodedWasm::WitPackage(_, _) => {
                return Err(ImportWasmError::TypesOnly);
            }
        }
    } else {
        let (wasm, bindgen) = wit_component::metadata::decode(wasm)
            .map_err(|error| ImportWasmError::DecodeCore(error.into()))?;
        if wasm.is_none() {
            return Err(ImportWasmError::UntypedCore);
        }
        (bindgen.resolve, bindgen.world, WasmLinkage::Core)
    };

    // We now have resolve, package, and world.
    // "Importize" the world to convert it to WIT that would be imported.
    resolve
        .importize(world_id, None)
        .map_err(|error| ImportWasmError::Importize(error.into()))?;

    let mut module = TypedWasmModule {
        name,
        wasm: wasm.clone(),
        linkage,
        resolve,
        world_id,
        functions: Default::default(),
    };
    let world = &module.resolve.worlds[world_id];
    let mut namespace = Namespace::default();

    for (key, item) in &world.imports {
        let WorldKey::Name(name) = key else { continue };
        match item {
            WorldItem::Function(function) => {
                let id = next_id.fresh();
                if let Ok(ty) = wit_to_star_function(&module.resolve, id, function) {
                    _ = namespace.insert_constant(
                        &Identifier::anon(from_kebab_case(name)),
                        ConstantInfo::new(DUMMY_SPAN, Type::Function(ty.clone())),
                    );
                    module.functions.insert(id, (name.clone(), ty));
                }
                // TODO: arrange for a more specific error message if user attempts to import a bad function
            }
            _ => todo!(),
        }
    }

    Ok((namespace, module))
}

fn wit_to_star_function(
    resolve: &Resolve,
    id: NameId,
    function: &Function,
) -> Result<Arc<FunctionType>, TypeNotAvailable> {
    Ok(Arc::new(FunctionType {
        kind: FunctionKind::Normal,
        name_span: DUMMY_SPAN,
        params: function
            .params
            .iter()
            .map(|p| {
                Ok(TypedFunctionParam {
                    public: false,
                    name: Identifier::anon(&p.name),
                    ty: wit_to_star_type(resolve, p.ty)?,
                    ty_span: DUMMY_SPAN,
                })
            })
            .collect::<Result<_, _>>()?,
        result: function
            .result
            .map_or(Ok(Type::Unit), |ty| wit_to_star_type(resolve, ty))?,
        callee: Some(StaticFunction::Named(id)),
    }))
}

fn wit_to_star_type(resolve: &Resolve, ty: wit_parser::Type) -> Result<Type, TypeNotAvailable> {
    match ty {
        wit_parser::Type::Bool => Ok(Type::Bool),
        wit_parser::Type::U8 => Ok(Type::Int(IntWidth::U8)),
        wit_parser::Type::U16 => Ok(Type::Int(IntWidth::U16)),
        wit_parser::Type::U32 => Ok(Type::Int(IntWidth::U32)),
        wit_parser::Type::U64 => Ok(Type::Int(IntWidth::U64)),
        wit_parser::Type::S8 => Ok(Type::Int(IntWidth::I8)),
        wit_parser::Type::S16 => Ok(Type::Int(IntWidth::I16)),
        wit_parser::Type::S32 => Ok(Type::Int(IntWidth::I32)),
        wit_parser::Type::S64 => Ok(Type::Int(IntWidth::I64)),
        wit_parser::Type::F32 => Err(TypeNotAvailable),
        wit_parser::Type::F64 => Err(TypeNotAvailable),
        wit_parser::Type::Char => Err(TypeNotAvailable),
        wit_parser::Type::String => Err(TypeNotAvailable),
        wit_parser::Type::ErrorContext => Err(TypeNotAvailable),
        wit_parser::Type::Id(id) => wit_to_star_type_def(resolve, &resolve.types[id]),
    }
}

fn wit_to_star_type_def(resolve: &Resolve, ty: &TypeDef) -> Result<Type, TypeNotAvailable> {
    match &ty.kind {
        wit_parser::TypeDefKind::Tuple(tuple) => Ok(Type::Tuple(Arc::new(
            tuple
                .types
                .iter()
                .map(|&ty| wit_to_star_type(resolve, ty))
                .collect::<Result<_, _>>()?,
        ))),
        _ => Err(TypeNotAvailable),
    }
}

fn from_kebab_case(name: &str) -> String {
    // TODO: probably a better way to do this is to check that to_kebab_case(imported_name) == wit_name.
    name.replace("-", "_")
}

impl Display for ImportWasmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImportWasmError::DecodeComponent(error) => {
                write!(f, "error decoding component: {error}")
            }
            ImportWasmError::DecodeCore(error) => write!(f, "error decoding core module: {error}"),
            ImportWasmError::UntypedCore => {
                write!(f, "core module does not contain `component-type` section")
            }
            ImportWasmError::TypesOnly => {
                write!(f, "cannot import `.wasm` file containing binary WIT only")
            }
            ImportWasmError::Importize(error) => write!(f, "error importizing WIT world: {error}"),
        }
    }
}

impl std::error::Error for ImportWasmError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ImportWasmError::DecodeComponent(error) => Some(error.as_ref()),
            ImportWasmError::DecodeCore(error) => Some(error.as_ref()),
            ImportWasmError::UntypedCore => None,
            ImportWasmError::TypesOnly => None,
            ImportWasmError::Importize(error) => Some(error.as_ref()),
        }
    }
}

impl Diagnostic for ImportWasmError {}
