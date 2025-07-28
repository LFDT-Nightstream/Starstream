use crate::{
    ast::{Sig, Storage, TypeArg, TypeDefRhs},
    typechecking::{ComparableType, EffectSet, TypeVar},
};
use chumsky::span::SimpleSpan;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Default)]
pub struct Symbols {
    pub vars: HashMap<SymbolId, SymbolInformation<VarInfo>>,
    pub types: HashMap<SymbolId, SymbolInformation<TypeInfo>>,
    pub functions: HashMap<SymbolId, SymbolInformation<FuncInfo>>,
    pub constants: HashMap<SymbolId, SymbolInformation<ConstInfo>>,
    pub interfaces: HashMap<SymbolId, SymbolInformation<AbiInfo>>,

    // lookup for builtin types inside the `types`, since these don't have
    // identifiers on the ast
    pub builtins: HashMap<&'static str, SymbolId>,

    // stores unification results after type inference
    pub type_vars: HashMap<TypeVar, ComparableType>,
}

#[derive(Debug, Clone)]
pub struct VarInfo {
    pub index: Option<u64>,
    pub mutable: bool,
    pub ty: Option<ComparableType>,
    pub is_storage: Option<SymbolId>,
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
    pub is_utxo_method: bool,
}

#[derive(Debug, Clone)]
pub struct ConstInfo {
    pub ty: Option<ComparableType>,
}

#[derive(Debug, Clone)]
pub struct AbiInfo {
    pub effects: HashSet<SymbolId>,
    pub fns: HashMap<String, Sig>,
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
