use crate::{
    ast::{Sig, Storage, TypeArg, TypeDefRhs},
    typechecking::{ComparableType, EffectSet, TypeVar},
};
use chumsky::span::SimpleSpan;
use std::collections::{BTreeMap, HashMap, HashSet};

#[derive(Debug, Default)]
pub struct Symbols {
    pub vars: HashMap<SymbolId, SymbolInformation<VarInfo>>,
    pub types: HashMap<SymbolId, SymbolInformation<TypeInfo>>,
    pub functions: HashMap<SymbolId, SymbolInformation<FuncInfo>>,
    pub constants: HashMap<SymbolId, SymbolInformation<ConstInfo>>,
    pub interfaces: HashMap<SymbolId, SymbolInformation<AbiInfo>>,
    pub effects: HashMap<SymbolId, SymbolInformation<EffectInfo>>,

    // lookup for builtin types inside the `types`, since these don't have
    // identifiers on the ast
    pub builtins: HashMap<&'static str, SymbolId>,

    // stores unification results after type inference
    pub type_vars: HashMap<TypeVar, ComparableType>,
}

#[derive(Debug, Clone, Default)]
pub struct VarInfo {
    pub wasm_local_index: Option<u64>,
    pub mutable: bool,
    pub ty: Option<ComparableType>,
    pub is_storage: Option<SymbolId>,
    pub is_frame_pointer: bool,
    pub is_captured: bool,
    // only should be set if the variable is marked as `is_captured` however,
    // computing this has to be done in a different step, that's why is_captured
    // is not an option.
    pub frame_offset: Option<u32>,

    pub is_argument: bool,
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub declarations: HashSet<SymbolId>,

    pub storage: Option<Storage>,
    pub storage_ty: Option<ComparableType>,
    // TODO: may want to separate typedefs from utxo and token types
    pub type_def: Option<TypeDefRhs>,
    pub yield_ty: Option<TypeArg>,
    pub resume_ty: Option<TypeArg>,
    pub interfaces: EffectSet,
    pub yield_fn: Option<SymbolId>,
}

#[derive(Debug, Clone, Default)]
pub struct FuncInfo {
    pub inputs_ty: Vec<TypeArg>,
    pub output_ty: Option<TypeArg>,

    pub output_canonical_ty: Option<ComparableType>,

    pub effects: EffectSet,
    pub locals: Vec<SymbolId>,
    pub mangled_name: Option<String>,
    // index into the wasm functions table
    pub index: Option<u32>,

    pub storage: Option<SymbolId>,

    pub is_main: bool,

    pub is_effect_handler: Option<SymbolId>,

    pub is_utxo_method: Option<SymbolId>,
    pub frame_size: u32,

    pub effect_handlers: EffectHandlers,

    pub frame_var: Option<SymbolId>,

    // the stack manipulations available on the wasm stack are fairly limited,
    // so most things require the use of indices into the typed stack.
    //
    // this variable is used to save and restore the frame pointer of the caller
    //
    // currently it's set to the index of the first non-argument local.
    pub saved_frame_local_index: Option<u32>,

    // currently bind and unbind are compiled as different functions, but
    // dispatched through a single import
    //
    // this has the function id of that dispatcher
    //
    // there is most likely a better abstraction for this
    pub dispatch_through: Option<SymbolId>,
    pub is_imported: Option<&'static str>,

    // TODO: other constant types
    pub is_constant: Option<u64>,

    // kind of hacky, since in theory this should depend on the type
    //
    // but for now it just makes things simpler
    //
    // this means the function moves the receiver when used in method form
    pub moves_variable: bool,
}

pub type EffectHandlers = BTreeMap<SymbolId, ArgOrConst>;

#[derive(Debug, Clone)]
pub enum ArgOrConst {
    Arg(u32),
    Const(SymbolId),
}

#[derive(Debug, Clone)]
pub struct ConstInfo {
    pub ty: Option<ComparableType>,
}

#[derive(Debug, Clone)]
pub struct AbiInfo {
    pub effects: HashSet<SymbolId>,
    pub fns: HashMap<String, Sig>,

    pub is_user_defined: bool,
}

#[derive(Debug, Clone, Default)]
pub struct EffectInfo {
    pub inputs_ty: Vec<TypeArg>,
    pub inputs_canonical_ty: Vec<ComparableType>,

    pub output_ty: Option<TypeArg>,
    pub output_canonical_ty: Option<ComparableType>,
    pub index: Option<usize>,
    pub is_user_defined: bool,
}

#[derive(Debug)]
pub struct SymbolInformation<T> {
    pub source: String,
    pub span: Option<SimpleSpan>,
    pub info: T,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct SymbolId {
    pub id: u64,
}
