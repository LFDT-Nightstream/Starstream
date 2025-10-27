#![allow(dead_code)]
use std::{cmp::Ordering, collections::HashMap, ops::Range, rc::Rc};

use ariadne::{Label, Report, ReportBuilder, ReportKind};
use chumsky::span::SimpleSpan;
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, Encode, EntityType, ExportSection, FuncType,
    FunctionSection, GlobalSection, GlobalType, ImportSection, InstructionSink, MemArg,
    MemorySection, MemoryType, Module, RefType, TypeSection, ValType,
};

use crate::{
    ast::*,
    symbols::{
        AbiInfo, ArgOrConst, EffectHandlers, FuncInfo, SymbolId, SymbolInformation, Symbols,
        VarInfo,
    },
    typechecking::{ComparableType, PrimitiveType, TypeVar},
};

const GLOBAL_FRAME_PTR: u32 = 0;
const GLOBAL_STACK_PTR: u32 = 1;

/// Compile a Starstream AST to a binary WebAssembly module.
pub fn compile<'a>(
    program: &'a StarstreamProgram,
    symbols: Symbols,
) -> (Option<Vec<u8>>, Vec<Report<'a>>) {
    let mut compiler = Compiler::new(symbols);
    compiler.visit_program(program);
    compiler.finish()
}

/// A static type in the Starstream type system.
#[derive(Debug, Clone)]
enum StaticType {
    Void,

    // Built-in types: primitive types
    // https://component-model.bytecodealliance.org/design/wit.html#primitive-types
    Bool,
    // S8,
    // S16,
    I32,
    I64,
    // U8,
    // U16,
    U32,
    U64,
    F32,
    F64,
    // Char,
    StrRef,

    Reference(Box<StaticType>),

    // Built-in types: lists, options, results, tuples
    // https://component-model.bytecodealliance.org/design/wit.html#lists
    // List(Box<StaticType>),
    // https://component-model.bytecodealliance.org/design/wit.html#options
    // Option(Box<StaticType>),
    // https://component-model.bytecodealliance.org/design/wit.html#results
    // Result(Box<StaticType>, Box<StaticType>),
    // https://component-model.bytecodealliance.org/design/wit.html#tuples
    // Tuple(Vec<StaticType>),

    // User-defined types
    Record(Record),
    // Variant(Variant),
    // Enum(Enum),
    Resource(Rc<ResourceType>),

    //
    Function(Rc<StarFunctionType>),
}

#[derive(Debug, Clone)]
pub struct Record {
    offsets: HashMap<String, (usize, Box<StaticType>)>,
}

impl StaticType {
    fn stack_intermediate(&self) -> Intermediate {
        match self {
            StaticType::Void => Intermediate::Void,
            StaticType::Bool => Intermediate::StackBool,
            StaticType::I32 => Intermediate::StackI32,
            StaticType::I64 => Intermediate::StackI64,
            StaticType::U32 => Intermediate::StackU32,
            StaticType::U64 => Intermediate::StackU64,
            StaticType::F32 => Intermediate::StackF32,
            StaticType::F64 => Intermediate::StackF64,
            StaticType::StrRef => Intermediate::StackStrRef,
            StaticType::Resource(_) => Intermediate::StackExternRef,

            StaticType::Reference(_) => Intermediate::StackI64,
            s @ StaticType::Record(_) => Intermediate::StackPtr(s.clone()),
            _ => todo!(),
        }
    }

    fn lower(&self) -> &'static [ValType] {
        self.stack_intermediate().stack_types()
    }

    fn from_canonical_type(
        ty: &ComparableType,
        type_vars: &HashMap<TypeVar, ComparableType>,
    ) -> Self {
        match ty {
            ComparableType::Primitive(PrimitiveType::Unit) => StaticType::Void,
            ComparableType::Primitive(PrimitiveType::U32) => StaticType::U32,
            ComparableType::Primitive(PrimitiveType::I32) => StaticType::I32,
            ComparableType::Primitive(PrimitiveType::U64) => StaticType::U64,
            ComparableType::Primitive(PrimitiveType::I64) => StaticType::I64,
            ComparableType::Primitive(PrimitiveType::F32) => StaticType::F32,
            ComparableType::Primitive(PrimitiveType::F64) => StaticType::F64,
            ComparableType::Primitive(PrimitiveType::Bool) => StaticType::Bool,
            ComparableType::Primitive(PrimitiveType::StrRef) => StaticType::I32,
            ComparableType::Intermediate => StaticType::I64,
            ComparableType::FnType(_, _) => todo!(),
            ComparableType::Utxo(_symbol_id, _) => StaticType::I64,
            ComparableType::Var(type_var) => {
                StaticType::from_canonical_type(type_vars.get(type_var).unwrap(), type_vars)
            }
            ComparableType::Ref(ty) => {
                StaticType::Reference(Box::new(StaticType::from_canonical_type(ty, type_vars)))
            }
            ComparableType::Void => StaticType::Void,
            // represent product types as pointers to linear memory
            ComparableType::Product(pairs) => {
                let mut offsets = HashMap::new();
                let mut offset = 0usize;
                for (name, ty) in pairs {
                    let ty = StaticType::from_canonical_type(ty, type_vars);
                    let ty_mem_size = ty.mem_size();
                    offsets.insert(name.clone(), (offset, Box::new(ty)));

                    offset += ty_mem_size;
                }

                StaticType::Record(Record { offsets })
            }
            _ => todo!("from_canonical_type({:?})", ty),
        }
    }

    fn mem_size(&self) -> usize {
        match self {
            StaticType::Void => 0,
            StaticType::Bool => 1,
            StaticType::I32 => 4,
            StaticType::I64 => 8,
            StaticType::U32 => 4,
            StaticType::U64 => 8,
            StaticType::F32 => 4,
            StaticType::F64 => 8,
            StaticType::StrRef => 4,
            StaticType::Reference(_static_type) => 4,
            StaticType::Record(_record) => 4,
            StaticType::Resource(_resource_type) => todo!(),
            StaticType::Function(_star_function_type) => todo!(),
        }
    }
}

/// Typed intermediate value.
///
/// A product of static type, stack slot size, and constness.
#[derive(Debug, Clone)]
#[must_use]
enum Intermediate {
    /// Nothing! Absolutely nothing!
    Void,
    /// An error intermediate. Suppress further typechecking errors.
    Error,
    /// `()` The null constant.
    ConstNull,
    /// `()` An imported or local function by ID.
    ConstFunction(u32),
    /// `(i32)` 0 is false, 1 is true, other values are disallowed.
    StackBool,
    /// `(i32)`
    StackI32,
    /// `(i32)` But use unsigned math where relevant.
    StackU32,
    /// `(i64)`
    StackI64,
    /// `(i64)` But use unsigned math where relevant.
    StackU64,
    /// `(f32)`
    StackF32,
    /// `(f64)`
    StackF64,
    StackExternRef,
    /// `(i32 i32)` A string reference, pointer and length.
    StackStrRef,

    /// pointer to linear memory
    StackPtr(StaticType),
}

impl Intermediate {
    fn stack_types(&self) -> &'static [ValType] {
        match self {
            Intermediate::Void | Intermediate::Error => &[],
            Intermediate::StackBool => &[ValType::I32],
            Intermediate::StackI32 => &[ValType::I32],
            Intermediate::StackI64 => &[ValType::I64],
            Intermediate::StackU32 => &[ValType::I32],
            Intermediate::StackU64 => &[ValType::I64],
            Intermediate::StackF32 => &[ValType::F32],
            Intermediate::StackF64 => &[ValType::F64],
            Intermediate::StackStrRef => &[ValType::I32, ValType::I32],
            Intermediate::StackExternRef => &[ValType::EXTERNREF],
            Intermediate::StackPtr(_) => &[ValType::I32],
            _ => todo!("Intermediate::stack_types({self:?})"),
        }
    }

    fn stack_size(&self) -> usize {
        self.stack_types().len()
    }
}

impl From<ValType> for Intermediate {
    fn from(value: ValType) -> Self {
        match value {
            ValType::I32 => Intermediate::StackI32,
            ValType::I64 => Intermediate::StackI64,
            ValType::F32 => Intermediate::StackF32,
            ValType::F64 => Intermediate::StackF64,
            ValType::V128 => todo!(),
            ValType::Ref(RefType::EXTERNREF) => Intermediate::StackExternRef,
            ValType::Ref(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
struct StarFunctionType {
    params: Vec<StaticType>,
    results: Vec<StaticType>,
}

impl StarFunctionType {
    fn lower(&self) -> FuncType {
        FuncType::new(
            self.params.iter().flat_map(|p| p.lower()).copied(),
            self.results.iter().flat_map(|p| p.lower()).copied(),
        )
    }
}

// https://component-model.bytecodealliance.org/design/wit.html#resources
#[derive(Debug, Clone)]
struct ResourceType {
    // WIT splits this out... maybe we'll just say a method literally named "constructor" is the constructor.
    constructor: Option<StarFunctionType>,
    methods: HashMap<String, (MethodType, StarFunctionType)>,
}

#[derive(Debug, Clone, Copy)]
enum MethodType {
    Static,
    BorrowSelf,
}

#[repr(usize)]
#[derive(Clone, Copy)]
enum FunctionCallType {
    FunctionCall = 0,
    Method = 1,
    EffectHandler = 3,
}

#[derive(Default)]
struct Compiler {
    // Diagnostic output.
    errors: Vec<Report<'static>>,
    // Wasm binary output.
    types: TypeSection,
    imports: ImportSection,
    functions: FunctionSection,
    memory: MemorySection,
    exports: ExportSection,
    code: CodeSection,
    data: DataSection,
    globals: GlobalSection,

    // Compiler state.
    bump_ptr: u32,
    raw_func_type_cache: HashMap<FuncType, u32>,

    // we insert all the generated functions at the end
    functions_builder: Vec<(StarFunctionType, Option<Function>)>,

    global_scope_functions: HashMap<String, u32>,

    symbols_table: Symbols,

    current_utxo: Vec<SymbolId>,

    unbind_tokens_fn: Option<SymbolId>,
}

impl Compiler {
    fn new(mut symbols_table: Symbols) -> Compiler {
        let mut this = Compiler::default();

        // Function indices in calls, exports, etc. are based on the combined
        // imports + declared functions list. The easiest way to handle this is
        // to know the whole list of imported functions before compiling. Do
        // that here for now.
        let print = this.import_function(
            "env",
            "eprint",
            StarFunctionType {
                params: vec![StaticType::StrRef],
                results: vec![],
            },
        );
        this.global_scope_functions
            .insert("print".to_owned(), print);
        let print_f64 = this.import_function(
            "starstream_debug",
            "f64",
            StarFunctionType {
                params: vec![StaticType::F64],
                results: vec![StaticType::F64],
            },
        );
        this.global_scope_functions
            .insert("print_f64".to_owned(), print_f64);

        let starstream_get_tokens = this.import_function(
            "starstream_utxo_env",
            "starstream_get_tokens",
            StarFunctionType {
                params: vec![StaticType::U32, StaticType::U32, StaticType::U32],
                results: vec![StaticType::U32],
            },
        );

        this.global_scope_functions
            .insert("get_tokens".to_owned(), starstream_get_tokens);

        //

        // Always export memory 0. It's created in finish().
        this.exports
            .export("memory", wasm_encoder::ExportKind::Memory, 0);

        let mut fns = symbols_table.functions.iter_mut().collect::<Vec<_>>();

        fns.sort_by_key(|f| f.1.source.clone());

        for (_f_id, f_info) in fns.iter_mut() {
            cache_required_effect_handlers(&symbols_table.interfaces, f_info);

            if f_info.source == "resume" && f_info.info.mangled_name.is_some() {
                let index = this.import_function(
                    "starstream_utxo:this",
                    f_info.info.mangled_name.as_ref().unwrap(),
                    StarFunctionType {
                        params: std::iter::once(StaticType::I64)
                            .chain(
                                f_info
                                    .info
                                    .effect_handlers
                                    .iter()
                                    // TODO: figure out a way of not having to repeat this everywhere
                                    .flat_map(|_effect_id| std::iter::repeat_n(StaticType::I32, 3)),
                            )
                            .chain(std::iter::once(StaticType::Reference(Box::new(
                                StaticType::Void,
                            ))))
                            .collect(),

                        results: vec![],
                    },
                );

                f_info.info.index.replace(index);
            } else if f_info.source == "yield" && f_info.info.mangled_name.is_some() {
                let index = this.import_function(
                    "starstream_utxo_env:this",
                    f_info.info.mangled_name.as_ref().unwrap(),
                    StarFunctionType {
                        // TODO: maybe could get this from the fn info
                        params: std::iter::repeat_n(StaticType::U32, 6).collect(),
                        results: f_info
                            .info
                            .effect_handlers
                            .iter()
                            // TODO: figure out a way of not having to repeat this
                            .flat_map(|_effect_id| std::iter::repeat_n(StaticType::I32, 3))
                            .collect(),
                    },
                );

                f_info.info.index.replace(index);
            } else if f_info.source == "new" && f_info.info.mangled_name.is_some() {
                let index = this.import_function(
                    "starstream_utxo:this",
                    f_info.info.mangled_name.as_ref().unwrap(),
                    StarFunctionType {
                        params: f_info
                            .info
                            .effect_handlers
                            .iter()
                            // TODO: figure out a way of not having to repeat this
                            .flat_map(|_effect_id| std::iter::repeat_n(StaticType::I32, 3))
                            .chain(
                                f_info
                                    .info
                                    .inputs_ty
                                    .iter()
                                    .zip(f_info.info.locals.iter().skip(1).map(|local| {
                                        symbols_table
                                            .vars
                                            .get(local)
                                            .as_ref()
                                            .unwrap()
                                            .info
                                            .ty
                                            .as_ref()
                                            .unwrap()
                                    }))
                                    .map(|(_, ty)| {
                                        StaticType::from_canonical_type(
                                            ty,
                                            &symbols_table.type_vars,
                                        )
                                    }),
                            )
                            .collect(),
                        results: f_info
                            .info
                            .output_canonical_ty
                            .as_ref()
                            .map(|ty| {
                                vec![StaticType::from_canonical_type(
                                    ty,
                                    &symbols_table.type_vars,
                                )]
                            })
                            .unwrap_or(vec![]),
                    },
                );

                f_info.info.index.replace(index);
            } else if f_info
                .info
                .mangled_name
                .as_ref()
                .map(|name| name.starts_with("starstream_query"))
                .unwrap_or(false)
            {
                let index = this.import_function(
                    "starstream_utxo:this",
                    f_info.info.mangled_name.as_ref().unwrap(),
                    StarFunctionType {
                        params: std::iter::once(StaticType::I64)
                            .chain(
                                f_info
                                    .info
                                    .effect_handlers
                                    .iter()
                                    // TODO: figure out a way of not having to repeat this everywhere
                                    .flat_map(|_effect_id| std::iter::repeat_n(StaticType::I32, 3)),
                            )
                            .chain(
                                f_info
                                    .info
                                    .inputs_ty
                                    .iter()
                                    .skip(1)
                                    .zip(
                                        f_info
                                            .info
                                            .locals
                                            .iter()
                                            .skip(1)
                                            .filter_map(|local| {
                                                symbols_table
                                                    .vars
                                                    .get(local)
                                                    .as_ref()
                                                    .filter(|var_info| {
                                                        !var_info.info.is_captured
                                                            && var_info.info.is_storage.is_none()
                                                    })
                                                    .and_then(|var_info| var_info.info.ty.as_ref())
                                            })
                                            .map(|ty| {
                                                StaticType::from_canonical_type(
                                                    ty,
                                                    &symbols_table.type_vars,
                                                )
                                            }),
                                    )
                                    .map(|(_, ty)| ty),
                            )
                            .collect(),
                        results: f_info
                            .info
                            .output_canonical_ty
                            .as_ref()
                            .map(|ty| {
                                vec![StaticType::from_canonical_type(
                                    ty,
                                    &symbols_table.type_vars,
                                )]
                            })
                            .unwrap_or(vec![]),
                    },
                );

                f_info.info.index.replace(index);
            } else if ["bind", "unbind"].contains(&f_info.source.as_str())
                && f_info.info.is_imported.is_some()
            {
                let index = this.import_function(
                    f_info.info.is_imported.unwrap(),
                    f_info.info.mangled_name.as_ref().unwrap(),
                    StarFunctionType {
                        params: vec![StaticType::I64],
                        results: vec![],
                    },
                );

                f_info.info.index.replace(index);

                this.global_scope_functions
                    .insert(f_info.source.clone(), index);
            } else if ["spend", "burn", "amount", "type", "mint"].contains(&f_info.source.as_str())
                && f_info.info.is_imported.is_some()
            {
                let map_canonical_to_static_type = |ty: &TypeArg| {
                    StaticType::from_canonical_type(
                        &ty.canonical_form_tys(&symbols_table.types),
                        &symbols_table.type_vars,
                    )
                };

                let params = f_info
                    .info
                    .inputs_ty
                    .iter()
                    .map(map_canonical_to_static_type)
                    .collect();

                let results = f_info
                    .info
                    .output_ty
                    .iter()
                    .map(map_canonical_to_static_type)
                    .collect();

                let index = this.import_function(
                    f_info.info.is_imported.unwrap(),
                    f_info.info.mangled_name.as_ref().unwrap(),
                    StarFunctionType { params, results },
                );

                f_info.info.index.replace(index);
            }
        }

        let starstream_yield = this.import_function(
            "starstream_utxo_env",
            "starstream_yield",
            StarFunctionType {
                params: vec![
                    StaticType::I32,
                    StaticType::I32,
                    StaticType::I32,
                    StaticType::I32,
                    StaticType::I32,
                    StaticType::I32,
                ],
                results: vec![],
            },
        );
        this.global_scope_functions
            .insert("starstream_yield".to_owned(), starstream_yield);

        for effect_info in symbols_table.effects.values_mut() {
            if !effect_info.info.is_user_defined {
                continue;
            }

            let index =
                this.import_function(
                    "starstream_env:this",
                    &format!("starstream_handler_{}", effect_info.source),
                    StarFunctionType {
                        params: vec![
                            // program id
                            StaticType::U32,
                            // handler id (function id)
                            StaticType::U32,
                            // frame pointer
                            StaticType::U32,
                        ]
                        .into_iter()
                        .chain(effect_info.info.inputs_canonical_ty.iter().map(|ty| {
                            StaticType::from_canonical_type(ty, &symbols_table.type_vars)
                        }))
                        .collect(),
                        results: effect_info
                            .info
                            .output_canonical_ty
                            .as_ref()
                            .map(|ty| {
                                vec![StaticType::from_canonical_type(
                                    ty,
                                    &symbols_table.type_vars,
                                )]
                            })
                            .unwrap_or(vec![]),
                    },
                );

            assert!(effect_info.info.index.replace(index as usize).is_none());
        }

        add_builtin_assert(&mut this);
        add_builtin_is_tx_signed_by(&mut this);

        // exports have to be after all the imports
        for (f_id, f_info) in fns {
            if f_info.info.mangled_name.is_some()
                && f_info.info.index.is_none()
                && f_info.info.is_imported.is_none()
                && f_info.info.is_constant.is_none()
            {
                cache_required_effect_handlers(&symbols_table.interfaces, f_info);

                let (ty, f) = build_func(
                    *f_id,
                    f_info,
                    &symbols_table.type_vars,
                    &symbols_table.vars,
                    f_info.info.is_main,
                );

                let index = this.add_function(ty, f);

                if f_info.source == "unbind_utxo_tokens" {
                    this.unbind_tokens_fn.replace(*f_id);
                }

                this.exports.export(
                    f_info.info.mangled_name.as_ref().unwrap(),
                    wasm_encoder::ExportKind::Func,
                    index,
                );

                assert!(f_info.info.index.replace(index).is_none());
            }

            let mut offset = 0;
            for var in &f_info.info.locals {
                let var_info = symbols_table.vars.get_mut(var).unwrap();

                if !var_info.info.is_captured {
                    continue;
                }

                let wasm_ty = StaticType::from_canonical_type(
                    var_info.info.ty.as_ref().unwrap(),
                    &symbols_table.type_vars,
                );

                var_info.info.frame_offset.replace(offset);

                // TODO: consider alignment?
                offset += wasm_ty.mem_size() as u32;
            }

            f_info.info.frame_size = offset;
        }

        let mut this = Compiler {
            symbols_table,
            ..this
        };

        if let Some(f_id) = this.unbind_tokens_fn {
            add_builtin_unbind_tokens(&mut this, f_id);
        }

        this
    }

    fn finish(mut self) -> (Option<Vec<u8>>, Vec<Report<'static>>) {
        for _ in [GLOBAL_FRAME_PTR, GLOBAL_STACK_PTR] {
            self.globals.global(
                GlobalType {
                    val_type: ValType::I32,
                    mutable: true,
                    shared: false,
                },
                // we need the stack to start after the statics
                //
                // we need to do this in finish instead of the constructor since
                // otherwise the value of this won't be fully computed.
                &ConstExpr::i32_const(self.bump_ptr as i32),
            );
        }

        let page_size = 64 * 1024;
        self.memory.memory(MemoryType {
            minimum: std::cmp::min(
                u64::from(self.bump_ptr.div_ceil(page_size)),
                // NOTE: we probably need some sort of pragma to setup the
                // maximum stack size, and use that to compute the minimum
                // memory. Or we need to insert grow instructions when needed.
                // it may also depend on whether we want a heap.
                //
                // We could technically optimize this by checking if there are
                // captured variables, but for now just set a minimum of one
                // page, since in the common case there will always be some
                // effect handler.
                1,
            ),
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        for (_, code) in &self.functions_builder {
            if let Some(code) = code {
                let mut sink = Vec::new();
                code.encode(&mut sink);
                self.code.raw(&sink);
            }
        }

        // TODO: return None if the errors were fatal.
        let module = self.to_module();
        (Some(module.finish()), self.errors)
    }

    fn to_module(&self) -> Module {
        assert_eq!(self.functions.len(), self.code.len());
        // Write sections to module.
        // Mandatory WASM order per https://webassembly.github.io/spec/core/binary/modules.html#binary-module:
        // type, import, func, table, mem, global, export, start, elem, datacount, code, data.
        let mut module = Module::new();
        if !self.types.is_empty() {
            module.section(&self.types);
        }
        if !self.imports.is_empty() {
            module.section(&self.imports);
        }
        if !self.functions.is_empty() {
            module.section(&self.functions);
        }
        if !self.memory.is_empty() {
            module.section(&self.memory);
        }
        if !self.globals.is_empty() {
            module.section(&self.globals);
        }
        if !self.exports.is_empty() {
            module.section(&self.exports);
        }
        if !self.code.is_empty() {
            module.section(&self.code);
        }
        if !self.data.is_empty() {
            module.section(&self.data);
        }
        module
    }

    // ------------------------------------------------------------------------
    // Diagnostics

    fn todo(&mut self, why: String) {
        Report::build(ReportKind::Custom("Todo", ariadne::Color::Red), 0..0)
            .with_message(why)
            .push(self);
    }

    // ------------------------------------------------------------------------
    // Memory management

    fn alloc_constant(&mut self, bytes: &[u8]) -> u32 {
        if self.bump_ptr == 0 {
            // Leave 1K of zeroes at the bottom.
            self.bump_ptr = 1024;
        }

        let ptr = self.bump_ptr;
        self.data.active(
            0,
            &ConstExpr::i32_const(ptr.cast_signed()),
            bytes.iter().copied(),
        );
        self.bump_ptr += u32::try_from(bytes.len()).unwrap();
        ptr
    }

    // ------------------------------------------------------------------------
    // Table management

    fn add_raw_func_type(&mut self, ty: FuncType) -> u32 {
        match self.raw_func_type_cache.get(&ty) {
            Some(&index) => index,
            None => {
                let index = self.types.len();
                self.types.ty().func_type(&ty);
                self.raw_func_type_cache.insert(ty, index);
                index
            }
        }
    }

    fn add_function(&mut self, ty: StarFunctionType, code: Function) -> u32 {
        let type_index = self.add_raw_func_type(ty.lower());
        let func_index = u32::try_from(self.functions_builder.len()).unwrap();
        self.functions_builder.push((ty, Some(code)));
        self.functions.function(type_index);
        func_index
    }

    fn import_function(&mut self, module: &str, field: &str, ty: StarFunctionType) -> u32 {
        // Not allowed to import functions after creating our own, since it bumps all the indices.
        assert!(self.functions.is_empty());

        let type_index = self.add_raw_func_type(ty.lower());
        let func_index = u32::try_from(self.functions_builder.len()).unwrap();
        self.functions_builder.push((ty, None));
        self.imports
            .import(module, field, EntityType::Function(type_index));
        func_index
    }

    // ------------------------------------------------------------------------
    // Visitors

    fn visit_program(&mut self, program: &StarstreamProgram) {
        for item in &program.items {
            self.visit_item(item);
        }
    }

    fn visit_item(&mut self, item: &ProgramItem) {
        match item {
            ProgramItem::Script(script) => self.visit_script(script),
            ProgramItem::Utxo(utxo) => self.visit_utxo(utxo),
            ProgramItem::Token(token) => self.visit_token(token),
            ProgramItem::Abi(_abi) => {}
            ProgramItem::TypeDef(_) => {}
            _ => self.todo(format!("ProgramItem::{:?}", item)),
        }
    }

    fn visit_utxo(&mut self, utxo: &Utxo) {
        self.current_utxo.push(utxo.name.uid.unwrap());

        for item in &utxo.items {
            match item {
                UtxoItem::Main(main) => {
                    let symbol_id = main.ident.uid.unwrap();

                    let f_info = self.symbols_table.functions.get_mut(&symbol_id).unwrap();

                    let (ty, mut function) = build_func(
                        symbol_id,
                        f_info,
                        &self.symbols_table.type_vars,
                        &self.symbols_table.vars,
                        true,
                    );

                    let effect_handlers = f_info.info.effect_handlers.clone();

                    let return_value =
                        self.visit_block(&mut function, &main.block, &effect_handlers);
                    self.drop_intermediate(&mut function, return_value);

                    function.instructions().end();

                    let index = self.add_function(ty, function);

                    self.exports.export(
                        self.symbols_table.functions[&main.ident.uid.unwrap()]
                            .info
                            .mangled_name
                            .as_ref()
                            .unwrap(),
                        wasm_encoder::ExportKind::Func,
                        index,
                    );
                }
                UtxoItem::Impl(utxo_impl) => self.visit_utxo_impl(utxo_impl),
                UtxoItem::Storage(_storage) => {}
                UtxoItem::Yield(_type_arg) => {}
                UtxoItem::Resume(_type_arg) => self.todo("resuming utxo with data".to_string()),
            }
        }

        self.current_utxo.pop();
    }

    fn visit_token(&mut self, token: &Token) {
        self.current_utxo.push(token.name.uid.unwrap());

        for item in &token.items {
            match item {
                TokenItem::Mint(mint) => {
                    let symbol_id = mint.1.uid.unwrap();
                    let f_info = self.symbols_table.functions.get_mut(&symbol_id).unwrap();

                    let (ty, mut function) = build_func(
                        symbol_id,
                        f_info,
                        &self.symbols_table.type_vars,
                        &self.symbols_table.vars,
                        f_info.info.is_main,
                    );

                    let symbol_id = mint.1.uid.unwrap();
                    let f_info = self.symbols_table.functions.get(&symbol_id).unwrap();
                    let effect_handlers = f_info.info.effect_handlers.clone();

                    let return_value = self.visit_block(&mut function, &mint.0, &effect_handlers);
                    self.drop_intermediate(&mut function, return_value);
                    function.instructions().end();

                    let index = self.add_function(ty, function);
                    let name = self
                        .symbols_table
                        .functions
                        .get(&mint.1.uid.unwrap())
                        .unwrap()
                        .info
                        .mangled_name
                        .clone()
                        .unwrap();

                    self.exports
                        .export(&name, wasm_encoder::ExportKind::Func, index);
                }
                p @ (TokenItem::Bind(_) | TokenItem::Unbind(_)) => {
                    let symbol_id = match p {
                        TokenItem::Bind(bind) => bind.1.uid.unwrap(),
                        TokenItem::Unbind(unbind) => unbind.1.uid.unwrap(),
                        _ => unreachable!(),
                    };

                    let f_info = self.symbols_table.functions.get_mut(&symbol_id).unwrap();
                    let effect_handlers = f_info.info.effect_handlers.clone();

                    let index = f_info.info.index.unwrap();
                    let mut function = self.get_function_body(index);

                    let return_value = match p {
                        TokenItem::Bind(bind) => {
                            self.visit_block(&mut function, &bind.0, &effect_handlers)
                        }
                        TokenItem::Unbind(unbind) => {
                            self.visit_block(&mut function, &unbind.0, &effect_handlers)
                        }
                        _ => unreachable!(),
                    };

                    self.drop_intermediate(&mut function, return_value);

                    function.instructions().end();

                    self.replace_function_body(index, function);
                }
            }
        }

        self.current_utxo.pop();
    }

    fn visit_script(&mut self, script: &Script) {
        for fndef in &script.definitions {
            let symbol_id = fndef.ident.uid.unwrap();
            let f_info = self.symbols_table.functions.get(&symbol_id).unwrap();
            let effect_handlers = f_info.info.effect_handlers.clone();
            let frame_size = f_info.info.frame_size;
            let saved_frame_local_index = f_info.info.saved_frame_local_index.unwrap();

            let index = f_info.info.index.unwrap();
            let mut function = self.get_function_body(index);

            // allocate stack space
            if frame_size > 0 {
                function_preamble(frame_size, saved_frame_local_index, &mut function);
            }

            let return_value = self.visit_block(&mut function, &fndef.body, &effect_handlers);

            if matches!(return_value, Intermediate::Void) {
                self.drop_intermediate(&mut function, return_value);
            }

            if frame_size > 0 {
                function_exit(frame_size, saved_frame_local_index, &mut function);
            }

            function.instructions().end();

            self.replace_function_body(index, function);
        }
    }

    fn visit_block(
        &mut self,
        func: &mut Function,
        mut block: &Block,
        effect_handlers: &EffectHandlers,
    ) -> Intermediate {
        let mut last = Intermediate::Void;
        loop {
            match block {
                Block::Chain { head, tail } => {
                    match &**head {
                        ExprOrStatement::Statement(statement) => {
                            self.visit_statement(func, statement, effect_handlers);
                        }
                        ExprOrStatement::Expr(expr) => {
                            self.drop_intermediate(func, last);
                            last = self.visit_expr(func, expr, effect_handlers);
                        }
                    }
                    block = tail;
                }
                Block::Close { semicolon: true } => {
                    self.drop_intermediate(func, last);
                    return Intermediate::Void;
                }
                Block::Close { semicolon: false } => {
                    return last;
                }
            }
        }
    }

    fn drop_intermediate(&mut self, func: &mut Function, im: Intermediate) {
        for _ in 0..im.stack_size() {
            func.instructions().drop();
        }
    }

    fn visit_statement(
        &mut self,
        func: &mut Function,
        statement: &Statement,
        effect_handlers: &EffectHandlers,
    ) {
        match statement {
            Statement::Return(expr) | Statement::Resume(expr) => {
                if let Some(expr) = expr {
                    let _im = self.visit_expr(func, expr, effect_handlers);
                }
                let f_info = self
                    .symbols_table
                    .functions
                    .get(&func.fn_id.unwrap())
                    .unwrap();
                function_exit(
                    f_info.info.frame_size,
                    f_info.info.saved_frame_local_index.unwrap(),
                    func,
                );
                func.instructions().return_();
            }
            Statement::BindVar {
                var,
                mutable: _,
                ty: _,
                value,
            } => {
                let var_info = self.symbols_table.vars.get(&var.uid.unwrap()).unwrap();
                let wasm_local_index = var_info.info.wasm_local_index;
                let frame_offset = var_info.info.frame_offset;
                let ty = var_info.info.ty.clone();

                if var_info.info.is_captured {
                    func.instructions().global_get(GLOBAL_FRAME_PTR);
                }

                let im = self.visit_expr(func, value, effect_handlers);

                if matches!(im, Intermediate::Error) {
                    Report::build(ReportKind::Error, 0..0)
                        .with_message(format_args!("can't assign expression to variable"))
                        .push(self);

                    return;
                }

                let current_fn_info = self
                    .symbols_table
                    .functions
                    .get(&func.fn_id.unwrap())
                    .unwrap();

                if let Some(wasm_local_index) = wasm_local_index {
                    func.instructions().local_set(
                        wasm_local_index as u32 + current_fn_info.info.effect_handlers.len() as u32,
                    );
                } else if let Some(frame_offset) = frame_offset {
                    let static_type = StaticType::from_canonical_type(
                        &ty.unwrap(),
                        &self.symbols_table.type_vars,
                    );
                    let _im = self.visit_mem(
                        func,
                        Some(static_type.stack_intermediate()),
                        frame_offset as usize,
                        &static_type,
                    );
                }
            }
            Statement::Assign { var, expr } => {
                let im = self.visit_field_access_expr(func, var, Some(expr), effect_handlers);

                assert!(matches!(im, Intermediate::Void));
            }
            Statement::While(cond, body) => {
                func.instructions().block(BlockType::Empty);
                func.instructions().loop_(BlockType::Empty);

                let im = self.visit_expr(func, cond, effect_handlers);

                assert!(matches!(im, Intermediate::StackBool));

                func.instructions().br_if(1);

                let body = match body {
                    LoopBody::Statement(statement) => {
                        self.visit_statement(func, statement, effect_handlers);
                        Intermediate::Void
                    }
                    LoopBody::Block(block) => self.visit_block(func, block, effect_handlers),
                    LoopBody::Expr(expr) => self.visit_expr(func, expr, effect_handlers),
                };

                assert!(matches!(body, Intermediate::Void));
                self.drop_intermediate(func, body);

                func.instructions().br(0).end().end();
            }
            Statement::With(block, handlers) => {
                let mut effect_handlers = effect_handlers.clone();

                for (decl, body) in handlers {
                    let fn_id = decl.ident.uid.unwrap();
                    let f_info = self.symbols_table.functions.get(&fn_id).unwrap();
                    let saved_frame = f_info.info.saved_frame_local_index.unwrap();

                    let index = f_info.info.index.unwrap();

                    effect_handlers.insert(
                        *f_info.info.is_effect_handler.as_ref().unwrap(),
                        ArgOrConst::Const(decl.ident.uid.unwrap()),
                    );

                    let mut func = self.get_function_body(index);

                    func.instructions()
                        // save frame pointer
                        .global_get(GLOBAL_FRAME_PTR)
                        .local_set(saved_frame)
                        // set frame pointer to received frame
                        //
                        // this way can always reference captured variables to
                        // the frame pointer
                        .local_get(0)
                        .global_set(GLOBAL_FRAME_PTR);

                    let im = self.visit_block(&mut func, body, &effect_handlers);

                    self.drop_intermediate(&mut func, im);
                    func.instructions()
                        // restore frame pointer
                        .local_get(saved_frame)
                        .global_set(GLOBAL_FRAME_PTR)
                        .end();

                    self.replace_function_body(index, func);
                }

                let im = self.visit_block(func, block, &effect_handlers);

                self.drop_intermediate(func, im);
            }
            _ => self.todo(format!("Statement::{:?}", statement)),
        }
    }

    fn visit_expr(
        &mut self,
        func: &mut Function,
        expr: &Spanned<Expr>,
        effect_handlers: &EffectHandlers,
    ) -> Intermediate {
        match &expr.node {
            Expr::PrimaryExpr(secondary) => {
                self.visit_field_access_expr(func, secondary, None, effect_handlers)
            }
            Expr::Equals(lhs, rhs) => {
                let lhs = self.visit_expr(func, lhs, effect_handlers);
                let rhs = self.visit_expr(func, rhs, effect_handlers);
                match (lhs, rhs) {
                    (Intermediate::Error, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI32, Intermediate::StackI32)
                    | (Intermediate::StackU32, Intermediate::StackU32) => {
                        func.instructions().i32_eq();
                        Intermediate::StackBool
                    }
                    (Intermediate::StackI64, Intermediate::StackI64)
                    | (Intermediate::StackU64, Intermediate::StackU64) => {
                        func.instructions().i64_eq();
                        Intermediate::StackBool
                    }
                    (Intermediate::StackF64, Intermediate::StackF64) => {
                        func.instructions().f64_eq();
                        Intermediate::StackBool
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Expr::Equals({:?}, {:?})", lhs, rhs));
                        Intermediate::Error
                    }
                }
            }
            Expr::NotEquals(lhs, rhs) => {
                let lhs = self.visit_expr(func, lhs, effect_handlers);
                let rhs = self.visit_expr(func, rhs, effect_handlers);
                match (lhs, rhs) {
                    (Intermediate::Error, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI32, Intermediate::StackI32)
                    | (Intermediate::StackU32, Intermediate::StackU32) => {
                        func.instructions().i32_ne();
                        Intermediate::StackBool
                    }
                    (Intermediate::StackI64, Intermediate::StackI64)
                    | (Intermediate::StackU64, Intermediate::StackU64) => {
                        func.instructions().i64_ne();
                        Intermediate::StackBool
                    }
                    (Intermediate::StackF64, Intermediate::StackF64) => {
                        func.instructions().f64_ne();
                        Intermediate::StackBool
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Expr::Equals({:?}, {:?})", lhs, rhs));
                        Intermediate::Error
                    }
                }
            }
            Expr::Add(lhs, rhs) => {
                let lhs = self.visit_expr(func, lhs, effect_handlers);
                let rhs = self.visit_expr(func, rhs, effect_handlers);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackF64, Intermediate::StackF64) => {
                        func.instructions().f64_add();
                        Intermediate::StackF64
                    }
                    (Intermediate::StackI32, Intermediate::StackI32)
                    | (Intermediate::StackU32, Intermediate::StackU32) => {
                        func.instructions().i32_add();
                        Intermediate::StackI32 // TODO: separate branch that produces StackU32
                    }
                    (Intermediate::StackI64, Intermediate::StackI64)
                    | (Intermediate::StackU64, Intermediate::StackU64) => {
                        func.instructions().i64_add();
                        Intermediate::StackI64 // TODO: separate branch that produces StackU64
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Expr::Add({:?}, {:?})", lhs, rhs));
                        Intermediate::Error
                    }
                }
            }
            Expr::Sub(lhs, rhs) => {
                let lhs = self.visit_expr(func, lhs, effect_handlers);
                let rhs = self.visit_expr(func, rhs, effect_handlers);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackF64, Intermediate::StackF64) => {
                        func.instructions().f64_sub();
                        Intermediate::StackF64
                    }
                    (Intermediate::StackI32, Intermediate::StackI32)
                    | (Intermediate::StackU32, Intermediate::StackU32) => {
                        func.instructions().i32_sub();
                        Intermediate::StackI32 // TODO: separate branch that produces StackU32
                    }
                    (Intermediate::StackI64, Intermediate::StackI64)
                    | (Intermediate::StackU64, Intermediate::StackU64) => {
                        func.instructions().i64_sub();
                        Intermediate::StackI64 // TODO: separate branch that produces StackU64
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Expr::Sub({:?}, {:?})", lhs, rhs));
                        Intermediate::Error
                    }
                }
            }
            Expr::Mul(lhs, rhs) => {
                let lhs = self.visit_expr(func, lhs, effect_handlers);
                let rhs = self.visit_expr(func, rhs, effect_handlers);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackF64, Intermediate::StackF64) => {
                        func.instructions().f64_mul();
                        Intermediate::StackF64
                    }
                    (Intermediate::StackI32, Intermediate::StackI32)
                    | (Intermediate::StackU32, Intermediate::StackU32) => {
                        func.instructions().i32_mul();
                        Intermediate::StackI32 // TODO: separate branch that produces StackU32
                    }
                    (Intermediate::StackI64, Intermediate::StackI64)
                    | (Intermediate::StackU64, Intermediate::StackU64) => {
                        func.instructions().i64_mul();
                        Intermediate::StackI64 // TODO: separate branch that produces StackU64
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Expr::Mul({:?}, {:?})", lhs, rhs));
                        Intermediate::Error
                    }
                }
            }
            // TODO: Div
            Expr::BitNot(operand) => match self.visit_expr(func, operand, effect_handlers) {
                Intermediate::Error => Intermediate::Error,
                Intermediate::StackI32 => {
                    // Wasm doesn't have a native bitnot instruction, so XOR with all-ones.
                    func.instructions().i32_const(-1);
                    func.instructions().i32_xor();
                    Intermediate::StackI32
                }
                Intermediate::StackU32 => {
                    func.instructions().i32_const(-1);
                    func.instructions().i32_xor();
                    Intermediate::StackU32
                }
                Intermediate::StackI64 => {
                    func.instructions().i64_const(-1);
                    func.instructions().i64_xor();
                    Intermediate::StackI64
                }
                Intermediate::StackU64 => {
                    func.instructions().i64_const(-1);
                    func.instructions().i64_xor();
                    Intermediate::StackU64
                }
                other => {
                    self.todo(format!("Expr::BitNot({:?})", other));
                    Intermediate::Error
                }
            },
            Expr::BitAnd(lhs, rhs) => {
                let lhs = self.visit_expr(func, lhs, effect_handlers);
                let rhs = self.visit_expr(func, rhs, effect_handlers);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI32, Intermediate::StackI32) => {
                        func.instructions().i32_and();
                        Intermediate::StackI32
                    }
                    (Intermediate::StackU32, Intermediate::StackU32) => {
                        func.instructions().i32_add();
                        Intermediate::StackU32
                    }
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_and();
                        Intermediate::StackI64
                    }
                    (Intermediate::StackU64, Intermediate::StackU64) => {
                        func.instructions().i64_and();
                        Intermediate::StackU64
                    }
                    other => {
                        self.todo(format!("Expr::BitAnd({:?})", other));
                        Intermediate::Error
                    }
                }
            }
            Expr::BitOr(lhs, rhs) => {
                let lhs = self.visit_expr(func, lhs, effect_handlers);
                let rhs = self.visit_expr(func, rhs, effect_handlers);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI32, Intermediate::StackI32) => {
                        func.instructions().i32_or();
                        Intermediate::StackI32
                    }
                    (Intermediate::StackU32, Intermediate::StackU32) => {
                        func.instructions().i32_or();
                        Intermediate::StackU32
                    }
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_or();
                        Intermediate::StackI64
                    }
                    (Intermediate::StackU64, Intermediate::StackU64) => {
                        func.instructions().i64_or();
                        Intermediate::StackU64
                    }
                    other => {
                        self.todo(format!("Expr::BitOr({:?})", other));
                        Intermediate::Error
                    }
                }
            }
            e @ (Expr::LessThan(lhs, rhs)
            | Expr::GreaterThan(lhs, rhs)
            | Expr::LessEq(lhs, rhs)
            | Expr::GreaterEq(lhs, rhs)) => {
                let lhs = self.visit_expr(func, lhs, effect_handlers);
                let rhs = self.visit_expr(func, rhs, effect_handlers);

                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => {
                        return Intermediate::Error;
                    }
                    (Intermediate::StackI32, Intermediate::StackI32) => match e {
                        Expr::LessThan(_, _) => {
                            func.instructions().i32_lt_s();
                        }
                        Expr::GreaterThan(_, _) => {
                            func.instructions().i32_gt_s();
                        }
                        Expr::LessEq(_, _) => {
                            func.instructions().i32_le_s();
                        }
                        Expr::GreaterEq(_, _) => {
                            func.instructions().i32_ge_s();
                        }
                        _ => {
                            return Intermediate::Error;
                        }
                    },
                    (Intermediate::StackU32, Intermediate::StackU32) => match e {
                        Expr::LessThan(_, _) => {
                            func.instructions().i32_lt_u();
                        }
                        Expr::GreaterThan(_, _) => {
                            func.instructions().i32_gt_u();
                        }
                        Expr::LessEq(_, _) => {
                            func.instructions().i32_le_u();
                        }
                        Expr::GreaterEq(_, _) => {
                            func.instructions().i32_ge_u();
                        }
                        _ => {
                            return Intermediate::Error;
                        }
                    },
                    (lhs, rhs) => {
                        self.todo(format!("Expr::LessThan({:?}, {:?})", lhs, rhs));
                        return Intermediate::Error;
                    }
                };

                Intermediate::StackBool
            }
            Expr::And(lhs, rhs) => match self.visit_expr(func, lhs, effect_handlers) {
                // Short-circuiting.
                Intermediate::Error => Intermediate::Error,
                Intermediate::StackBool => {
                    func.instructions().if_(BlockType::Result(ValType::I32));
                    match self.visit_expr(func, rhs, effect_handlers) {
                        Intermediate::Error => return Intermediate::Error,
                        Intermediate::StackBool => {}
                        rhs => {
                            Report::build(ReportKind::Error, 0..0)
                                .with_message(format_args!(
                                    "type mismatch: `&&` requires bools, but right side was {rhs:?}"
                                ))
                                .push(self);
                            return Intermediate::Error;
                        }
                    }
                    func.instructions().else_().i32_const(0).end();
                    Intermediate::StackBool
                }
                lhs => {
                    Report::build(ReportKind::Error, 0..0)
                        .with_message(format_args!(
                            "type mismatch: `&&` requires bools, but left side was {lhs:?}"
                        ))
                        .push(self);
                    Intermediate::Error
                }
            },
            Expr::Or(lhs, rhs) => match self.visit_expr(func, lhs, effect_handlers) {
                // Short-circuiting.
                Intermediate::Error => Intermediate::Error,
                Intermediate::StackBool => {
                    func.instructions()
                        .if_(BlockType::Result(ValType::I32))
                        .i32_const(1)
                        .else_();
                    match self.visit_expr(func, rhs, effect_handlers) {
                        Intermediate::Error => return Intermediate::Error,
                        Intermediate::StackBool => {}
                        rhs => {
                            Report::build(ReportKind::Error, 0..0)
                                .with_message(format_args!(
                                    "type mismatch: `&&` requires bools, but right side was {rhs:?}"
                                ))
                                .push(self);
                            return Intermediate::Error;
                        }
                    }
                    func.instructions().end();
                    Intermediate::StackBool
                }
                lhs => {
                    Report::build(ReportKind::Error, 0..0)
                        .with_message(format_args!(
                            "type mismatch: `&&` requires bools, but left side was {lhs:?}"
                        ))
                        .push(self);
                    Intermediate::Error
                }
            },
            Expr::BlockExpr(BlockExpr::Block(block)) => {
                self.visit_block(func, block, effect_handlers)
            }
            Expr::BlockExpr(BlockExpr::IfThenElse(cond, if_, else_)) => {
                match self.visit_expr(func, cond, effect_handlers) {
                    Intermediate::Error => Intermediate::Error,
                    Intermediate::StackBool => {
                        // TODO: handle non-Void if blocks.
                        func.instructions().if_(BlockType::Empty);
                        let im = self.visit_block(func, if_, effect_handlers);
                        self.drop_intermediate(func, im);
                        if let Some(else_) = else_ {
                            func.instructions().else_();
                            let im = self.visit_block(func, else_, effect_handlers);
                            self.drop_intermediate(func, im);
                        }
                        func.instructions().end();
                        Intermediate::Void
                    }
                    other => {
                        Report::build(ReportKind::Error, 0..0)
                            .with_message(format_args!(
                                "type mismatch: `if` requires bool, got {other:?}"
                            ))
                            .push(self);
                        Intermediate::Error
                    }
                }
            }

            Expr::Not(e) => match self.visit_expr(func, e, effect_handlers) {
                // Short-circuiting.
                Intermediate::Error => Intermediate::Error,
                Intermediate::StackBool => {
                    func.instructions().i32_eqz();

                    Intermediate::StackBool
                }
                lhs => {
                    Report::build(ReportKind::Error, 0..0)
                        .with_message(format_args!(
                            "type mismatch: `&&` requires bools, but left side was {lhs:?}"
                        ))
                        .push(self);
                    Intermediate::Error
                }
            },
            _ => {
                self.todo(format!("Expr::{:?}", expr));
                Intermediate::Error
            }
        }
    }

    fn visit_field_access_expr(
        &mut self,
        func: &mut Function,
        expr: &FieldAccessExpression,
        // if we have something like x.foo = rhs;
        rhs: Option<&Spanned<Expr>>,
        effect_handlers: &EffectHandlers,
    ) -> Intermediate {
        match expr {
            FieldAccessExpression::PrimaryExpr(primary) => {
                self.visit_primary_expr(func, primary, effect_handlers)
            }
            FieldAccessExpression::FieldAccess { base, field } => {
                let receiver = self.visit_field_access_expr(func, base, rhs, effect_handlers);

                let expr: &IdentifierExpr = field;
                if let Intermediate::Error = receiver {
                    return Intermediate::Error;
                }

                if let Some(args) = &expr.args {
                    let Some(f_info) = self.symbols_table.functions.get(&expr.name.uid.unwrap())
                    else {
                        Report::build(ReportKind::Error, expr.name.span.unwrap().into_range())
                            .with_message(format_args!(
                                "function info not found for {}",
                                expr.name.raw
                            ))
                            .push(self);

                        return Intermediate::Error;
                    };

                    let Some(f_index) = f_info.info.index else {
                        Report::build(ReportKind::Error, expr.name.span.unwrap().into_range())
                            .with_message(format_args!(
                                "function not supported yet {}",
                                expr.name.raw
                            ))
                            .push(self);

                        return Intermediate::Error;
                    };
                    let func_im = Intermediate::ConstFunction(f_index);

                    let xs = &args.xs;

                    let effect_handlers_required = f_info.info.effect_handlers.clone();

                    self.visit_call(
                        func,
                        expr.name.span.unwrap(),
                        func_im,
                        xs,
                        FunctionCallType::Method,
                        effect_handlers,
                        effect_handlers_required,
                        None,
                    )
                } else {
                    let rhs = rhs.map(|expr| self.visit_expr(func, expr, effect_handlers));

                    match &receiver {
                        Intermediate::StackPtr(StaticType::Record(record)) => {
                            let (offset, ty) = record.offsets.get(&expr.name.raw).unwrap();
                            self.visit_mem(func, rhs, *offset, ty)
                        }
                        _ => {
                            _ = func;
                            self.todo(format!("Field {:?}.{:?}", receiver, expr));
                            Intermediate::Error
                        }
                    }
                }
            }
        }
    }

    fn visit_mem(
        &mut self,
        func: &mut Function,
        rhs: Option<Intermediate>,
        offset: usize,
        ty: &StaticType,
    ) -> Intermediate {
        let offset = MemArg {
            offset: offset as u64,
            // TODO:
            align: 0,
            memory_index: 0,
        };

        if let Some(actual_ty) = rhs.as_ref() {
            match (actual_ty, ty) {
                (Intermediate::Void, StaticType::Void) => {}
                (Intermediate::StackI32, StaticType::I32) => {}
                (Intermediate::StackU32, StaticType::U32) => {}
                (Intermediate::StackU64, StaticType::U64) => {}
                (Intermediate::StackI64, StaticType::I64) => {}
                (Intermediate::StackBool, StaticType::Bool) => {}
                (expected, found) => {
                    Report::build(ReportKind::Error, 0..0)
                        .with_message(format_args!(
                            "parameter type mismatch: expected {expected:?}, got {found:?}"
                        ))
                        .push(self);
                    return Intermediate::Error;
                }
            }
        }

        match ty {
            StaticType::I32 | StaticType::U32 => {
                if let Some(Intermediate::StackI32 | Intermediate::StackU32) = rhs {
                    func.instructions().i32_store(offset);
                    Intermediate::Void
                } else {
                    func.instructions().i32_load(offset);
                    ty.stack_intermediate()
                }
            }
            StaticType::I64 | StaticType::U64 => {
                if let Some(Intermediate::StackI64 | Intermediate::StackU64) = rhs {
                    func.instructions().i64_store(offset);
                    Intermediate::Void
                } else {
                    func.instructions().i64_load(offset);
                    ty.stack_intermediate()
                }
            }
            ty => {
                self.todo(format!("record field access of ty {:?}", ty));

                Intermediate::Error
            }
        }
    }

    fn visit_primary_expr(
        &mut self,
        func: &mut Function,
        primary: &PrimaryExpr,
        effect_handlers: &EffectHandlers,
    ) -> Intermediate {
        match primary {
            PrimaryExpr::Number { literal, ty } => {
                match StaticType::from_canonical_type(
                    ty.as_ref().unwrap(),
                    &self.symbols_table.type_vars,
                ) {
                    StaticType::I32 => {
                        func.instructions().i32_const(*literal as i32);
                        Intermediate::StackI32
                    }
                    StaticType::I64 => {
                        func.instructions().i64_const(*literal as i64);
                        Intermediate::StackI64
                    }
                    StaticType::U32 => {
                        func.instructions().i32_const(*literal as i32);
                        Intermediate::StackU32
                    }
                    StaticType::U64 => {
                        func.instructions().i64_const(*literal as i64);
                        Intermediate::StackU64
                    }
                    ty => {
                        self.todo(format!("numeric literal of ty {:?}", ty));
                        Intermediate::Error
                    }
                }
            }
            PrimaryExpr::Bool(true) => {
                func.instructions().i32_const(1);
                Intermediate::StackBool
            }
            PrimaryExpr::Bool(false) => {
                func.instructions().i32_const(0);
                Intermediate::StackBool
            }
            PrimaryExpr::Ident(ident)
            | PrimaryExpr::Namespace {
                namespaces: _,
                ident,
            } => {
                if let Some(args) = &ident.args {
                    let mut effect_handlers_required = Default::default();

                    // A function call, so look in the function table.
                    let im = if let Some(global_scope_fn) =
                        self.global_scope_functions.get(&ident.name.raw)
                    {
                        Intermediate::ConstFunction(*global_scope_fn)
                    } else if let Some(mut fn_info) =
                        self.symbols_table.functions.get(&ident.name.uid.unwrap())
                    {
                        if let Some(proxy) = fn_info.info.dispatch_through {
                            fn_info = self.symbols_table.functions.get(&proxy).unwrap();
                        };

                        if let Some(index) = fn_info.info.index {
                            effect_handlers_required = fn_info.info.effect_handlers.clone();

                            Intermediate::ConstFunction(index)
                        } else if let Some(constant_value) = fn_info.info.is_constant {
                            // TODO: other types
                            func.instructions().i64_const(constant_value as i64);
                            return Intermediate::StackI64;
                        } else {
                            Report::build(ReportKind::Error, ident.name.span.unwrap().into_range())
                                .with_message(format_args!(
                                    "effect {:?} is not directly callable",
                                    &ident.name.raw
                                ))
                                .with_label(
                                    Label::new(ident.name.span.unwrap().into_range())
                                        .with_message("called here"),
                                )
                                .push(self);
                            return Intermediate::Error;
                        }
                    } else {
                        Report::build(ReportKind::Error, 0..0)
                            .with_message(format_args!(
                                "no function named {:?} in scope",
                                &ident.name.raw
                            ))
                            .push(self);
                        return Intermediate::Error;
                    };

                    self.visit_call(
                        func,
                        ident.name.span.unwrap(),
                        im,
                        &args.xs,
                        FunctionCallType::FunctionCall,
                        effect_handlers,
                        effect_handlers_required,
                        None,
                    )
                } else {
                    // Not a function call, so look in the variable table.
                    let Some(var_info) = self.symbols_table.vars.get(&ident.name.uid.unwrap())
                    else {
                        Report::build(ReportKind::Error, ident.name.span.unwrap().into_range())
                            .with_message(format_args!(
                                "variable info not found for {}",
                                ident.name.raw
                            ))
                            .push(self);

                        return Intermediate::Error;
                    };

                    let ty = var_info.info.ty.as_ref().unwrap().clone();
                    let static_type =
                        StaticType::from_canonical_type(&ty, &self.symbols_table.type_vars);

                    if var_info.info.is_storage.is_some() {
                        func.instructions().i32_const(0);
                    } else if let Some(frame_offset) = var_info.info.frame_offset {
                        func.instructions().global_get(GLOBAL_FRAME_PTR);
                        let im = self.visit_mem(func, None, frame_offset as usize, &static_type);
                        return im;
                    } else {
                        // we can't use `effect_handlers` here since we may be
                        // inside a try block, but here we just need to know the
                        // offset in the function's parameters
                        let current_fn_info = self
                            .symbols_table
                            .functions
                            .get(&func.fn_id.unwrap())
                            .unwrap();
                        func.instructions().local_get(
                            // TODO: kind of duplicated code
                            var_info.info.wasm_local_index.unwrap() as u32
                                + (current_fn_info.info.effect_handlers.len() as u32) * 3,
                        );
                    }

                    StaticType::from_canonical_type(&ty, &self.symbols_table.type_vars)
                        .stack_intermediate()
                }
            }
            PrimaryExpr::RaiseNamespaced {
                ident,
                namespaces: _,
            } => {
                let effect_handler_id = ident.name.uid.as_ref().unwrap();

                if let Some(args) = &ident.args {
                    let effect_info = &self
                        .symbols_table
                        .effects
                        .get(effect_handler_id)
                        .unwrap()
                        .info;

                    let Some(index) = effect_info.index else {
                        Report::build(ReportKind::Error, 0..0)
                            .with_message(format_args!(
                                "TODO: effect can't be called: {:?}",
                                ident.name
                            ))
                            .push(self);
                        return Intermediate::Error;
                    };

                    self.visit_call(
                        func,
                        ident.name.span.unwrap(),
                        Intermediate::ConstFunction(index as u32),
                        &args.xs,
                        FunctionCallType::EffectHandler,
                        effect_handlers,
                        // TODO: allow effects in handlers
                        Default::default(),
                        Some(*effect_handler_id),
                    )
                } else {
                    unreachable!();
                }
            }
            PrimaryExpr::ParExpr(expr) => self.visit_expr(func, expr, effect_handlers),
            PrimaryExpr::StringLiteral(string) => {
                let ptr = self.alloc_constant(string.as_bytes());
                let len = string.len();
                func.instructions()
                    .i32_const(ptr.cast_signed())
                    .i32_const(u32::try_from(len).unwrap().cast_signed());
                Intermediate::StackStrRef
            }
            PrimaryExpr::Yield(expr) => self.visit_yield(func, expr, effect_handlers),
            PrimaryExpr::Tuple(elems) if elems.is_empty() => Intermediate::Void,
            _ => {
                self.todo(format!("PrimaryExpr::{:?}", primary));
                Intermediate::Error
            }
        }
    }

    fn visit_yield(
        &mut self,
        func: &mut Function,
        expr: &Option<Box<Spanned<Expr>>>,
        effect_handlers: &EffectHandlers,
    ) -> Intermediate {
        // TODO: yielding outside utxos
        let utxo_id = self.current_utxo.last().unwrap();

        let utxo_info = self.symbols_table.types.get(utxo_id).unwrap();

        let f_id = utxo_info.info.yield_fn.unwrap();
        let f_id = self
            .symbols_table
            .functions
            .get(&f_id)
            .unwrap()
            .info
            .index
            .unwrap();

        let utxo_name = utxo_info.source.clone();
        let ptr = self.alloc_constant(utxo_name.as_bytes());
        let len = utxo_name.len();

        // TODO: yield data but the thing is that coordination scripts are a
        // bit different from utxos in this regard so we may want to do some
        // transformations first, or split into two cases here.
        let _im = if let Some(expr) = expr {
            // address
            //
            // assume that the utxo storage is always at address 0, which is sound since
            // the utxo has its own memory space anyway.
            func.instructions().i32_const(0);

            self.visit_expr(func, expr, effect_handlers)
        } else {
            Intermediate::Void
        };

        let mut instructions = func.instructions();
        instructions
            .i32_const(ptr.cast_signed())
            .i32_const(u32::try_from(len).unwrap().cast_signed());

        // data
        instructions.i32_const(0);
        // data_len
        instructions.i32_const(0);
        // resume_arg
        instructions.i32_const(0);
        // resume_arg_len
        instructions.i32_const(0);

        instructions.call(f_id);

        // the call to resume may have set different handlers
        // we need to pop those from the stack and assign them
        //
        // we need to do it in reverse because that's how multi-return works
        //
        // NOTE: this assumes that the effects before the first yield are the
        // same as the effects after
        for i in (0..effect_handlers.len() * 3).rev() {
            instructions.local_set(i as u32);
        }

        Intermediate::Void
    }

    fn visit_call(
        &mut self,
        func: &mut Function,
        id_span: SimpleSpan,
        im: Intermediate,
        args: &[Spanned<Expr>],
        call_type: FunctionCallType,
        effect_handlers_in_scope: &EffectHandlers,
        effect_handlers_required: EffectHandlers,
        handler_for: Option<SymbolId>,
    ) -> Intermediate {
        match im {
            Intermediate::Error => Intermediate::Error,
            Intermediate::ConstFunction(id) => {
                let func_type = self.functions_builder[id as usize].0.clone();

                for effect in effect_handlers_required.keys().chain(handler_for.iter()) {
                    let Some(handler) = effect_handlers_in_scope.get(effect) else {
                        continue;
                    };

                    match handler {
                        ArgOrConst::Arg(index) => {
                            func.instructions()
                                .local_get(index * 3)
                                .local_get(index * 3 + 1)
                                .local_get(index * 3 + 2);
                        }
                        ArgOrConst::Const(effect_id) => {
                            func.instructions()
                                // TODO: PROGRAM ID
                                // but this works for the simple case where
                                // effect handlers are only defined in the
                                // single coordination script
                                .i32_const(0_i32)
                                .i32_const(effect_id.id as i32)
                                .global_get(GLOBAL_FRAME_PTR);
                        }
                    }
                }

                for (param, arg) in func_type
                    .params
                    .iter()
                    .skip(call_type as usize + effect_handlers_required.len() * 3)
                    .zip(args)
                {
                    let arg = self.visit_expr(func, arg, effect_handlers_in_scope);
                    match (param, arg) {
                        (StaticType::Void, Intermediate::Void) => {}
                        (StaticType::F64, Intermediate::StackF64) => {}
                        (StaticType::I32, Intermediate::StackI32) => {}
                        (StaticType::I32, Intermediate::StackU32) => {}
                        (StaticType::U32, Intermediate::StackI32) => {}
                        (StaticType::U32, Intermediate::StackU32) => {}
                        (StaticType::I64, Intermediate::StackI64) => {}
                        (StaticType::U64, Intermediate::StackI64) => {}
                        (StaticType::U64, Intermediate::StackU64) => {}
                        (StaticType::StrRef, Intermediate::StackStrRef) => {}
                        (StaticType::Bool, Intermediate::StackBool) => {}
                        (StaticType::Reference(_s), Intermediate::Void) => {
                            // null pointer
                            func.instructions().i64_const(0);
                            // references to other types will need to be handled
                            // by allocating memory
                        }
                        (param, arg) => {
                            Report::build(ReportKind::Error, id_span.into_range())
                                .with_message(format_args!(
                                    "parameter type mismatch: expected {param:?}, got {arg:?}"
                                ))
                                .push(self);
                        }
                    }
                }

                let params_required = func_type.params.len();
                let args_given =
                    args.len() + call_type as usize + effect_handlers_required.len() * 3;
                match params_required.cmp(&args_given) {
                    Ordering::Equal => {}
                    Ordering::Less => {
                        Report::build(ReportKind::Error, id_span.into_range())
                            .with_message(format!(
                                "too many arguments to function call: expected {params_required}, got {args_given}"
                            ))
                            .with_label(
                                Label::new(id_span.into_range())
                                    .with_message("function called here"),
                            )
                            .push(self);
                    }
                    Ordering::Greater => {
                        Report::build(ReportKind::Error, id_span.into_range())
                            .with_message(format!("not enough arguments to function call: expected {params_required}, got {args_given}"))
                            .with_label(
                                Label::new(id_span.into_range())
                                    .with_message("function called here")
                            )
                            .push(self);
                    }
                }
                func.instructions().call(id);
                match func_type.results.first() {
                    // TODO: handle functions with multiple results
                    Some(r) => r.stack_intermediate(),
                    None => Intermediate::Void,
                }
            }
            _ => {
                Report::build(ReportKind::Error, id_span.into_range())
                    .with_message(format_args!("attempting to call non-function {im:?}"))
                    .push(self);
                self.drop_intermediate(func, im);
                Intermediate::Error
            }
        }
    }

    fn visit_utxo_impl(&mut self, utxo_impl: &Impl) {
        for fndef in &utxo_impl.definitions {
            let symbol_id = fndef.ident.uid.unwrap();
            let f_info = self.symbols_table.functions.get_mut(&symbol_id).unwrap();
            let effect_handlers = f_info.info.effect_handlers.clone();

            let (ty, mut function) = build_func(
                symbol_id,
                f_info,
                &self.symbols_table.type_vars,
                &self.symbols_table.vars,
                false,
            );

            let _return_value = self.visit_block(&mut function, &fndef.body, &effect_handlers);
            function.instructions().end();

            let index = self.add_function(ty, function);

            // TODO: probably can avoid this lookup
            let name = self
                .symbols_table
                .functions
                .get(&fndef.ident.uid.unwrap())
                .unwrap()
                .info
                .mangled_name
                .clone()
                .unwrap_or(fndef.ident.raw.clone());

            self.exports
                .export(&name, wasm_encoder::ExportKind::Func, index);
        }
    }

    fn replace_function_body(&mut self, index: u32, function: Function) {
        self.functions_builder
            .get_mut(index as usize)
            .map(|(_, f)| f)
            .unwrap()
            .replace(function);
    }

    fn get_function_body(&mut self, index: u32) -> Function {
        self.functions_builder
            .get_mut(index as usize)
            .map(|(_, f)| f.take().unwrap())
            .unwrap()
    }
}

fn cache_required_effect_handlers(
    abis: &HashMap<SymbolId, SymbolInformation<AbiInfo>>,
    f_info: &mut SymbolInformation<FuncInfo>,
) {
    let mut flattened_effects = vec![];

    for abi in f_info.info.effects.iter() {
        let Some(abi) = abis.get(abi) else {
            continue;
        };

        // there are many builtins right now just to have the examples typecheck
        // those probably need to be moved to some sort of include so that they
        // can be re-used instead.
        if !abi.info.is_user_defined {
            continue;
        }

        for effect in &abi.info.effects {
            flattened_effects.push(effect);
        }
    }

    flattened_effects.sort();

    f_info.info.effect_handlers = flattened_effects
        .into_iter()
        .enumerate()
        .map(|(usize, effect_id)| (*effect_id, ArgOrConst::Arg(usize as u32)))
        .collect();
}

fn function_preamble(frame_size: u32, saved_frame_local_index: u32, function: &mut Function) {
    let mut instructions = function.instructions();

    instructions
        //
        // save frame pointer
        .global_get(GLOBAL_FRAME_PTR)
        .local_set(saved_frame_local_index)
        //
        // set frame pointer to current stack pointer
        .global_get(GLOBAL_STACK_PTR)
        .global_set(GLOBAL_FRAME_PTR);

    if frame_size > 0 {
        instructions
            //
            // increase stack pointer
            .global_get(GLOBAL_STACK_PTR)
            .i32_const(frame_size as i32)
            .i32_add()
            .global_set(GLOBAL_STACK_PTR);
    }
}

fn function_exit(frame_size: u32, saved_frame_local_index: u32, function: &mut Function) {
    let mut instructions = function.instructions();

    if frame_size > 0 {
        instructions
            // restore stack
            .global_get(GLOBAL_STACK_PTR)
            .i32_const(frame_size as i32)
            .i32_sub()
            .global_set(GLOBAL_STACK_PTR);
    }

    instructions
        // restore frame pointer of callee
        .local_get(saved_frame_local_index)
        .global_set(GLOBAL_FRAME_PTR);
}

fn add_builtin_assert(this: &mut Compiler) {
    let mut function = Function::new(&[ValType::I32]);

    function
        .instructions()
        .local_get(0)
        .if_(BlockType::Empty)
        .else_()
        .unreachable()
        .end()
        .end();

    let assert_fn = this.add_function(
        StarFunctionType {
            params: vec![StaticType::Bool],
            results: vec![],
        },
        function,
    );

    this.global_scope_functions
        .insert("assert".to_owned(), assert_fn);
}

fn add_builtin_is_tx_signed_by(this: &mut Compiler) {
    let mut function = Function::new(&[ValType::I32]);

    function.instructions().i32_const(1).end();

    let assert_fn = this.add_function(
        StarFunctionType {
            params: vec![StaticType::I32],
            results: vec![StaticType::Bool],
        },
        function,
    );

    this.global_scope_functions
        .insert("IsTxSignedBy".to_owned(), assert_fn);
}

// the DSL still doesn't have enough features to write this directly
// so we just add it as an intrinsic for now
fn add_builtin_unbind_tokens(this: &mut Compiler, f_id: SymbolId) {
    let f_info = this.symbols_table.functions.get(&f_id).unwrap();
    let f_index = f_info.info.index.unwrap();
    let effect_handlers = f_info.info.effect_handlers.clone();

    assert_eq!(effect_handlers.len(), 1);

    let mut function = this.get_function_body(f_index);

    let (_effect_id, effect_info) = this
        .symbols_table
        .effects
        .iter()
        .find(|(_, info)| &info.source == "TokenUnbound")
        .unwrap();

    function
        .instructions()
        .loop_(BlockType::Empty)
        // pointer to memory
        //
        // this is just ephemeral, so we don't need to push and pop from the
        // stack really.
        //
        // although we may need to generalize this later
        .global_get(GLOBAL_STACK_PTR)
        // how many tokens
        .i32_const(1)
        // skip
        .i32_const(0)
        .call(this.global_scope_functions["get_tokens"])
        .if_(BlockType::Empty)
        .global_get(GLOBAL_STACK_PTR)
        .i64_load(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        })
        .call(this.global_scope_functions["unbind"])
        // the current handler for Starstream::TokenUnbound this function only
        // has one effect, so we can fix these for now.
        //
        // but this will break if the Starstream abi gets a new effect
        .local_get(0)
        .local_get(1)
        .local_get(2)
        // we read it again for the effect
        .global_get(GLOBAL_STACK_PTR)
        .i64_load(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        })
        .call(effect_info.info.index.unwrap() as u32)
        .br(0)
        // end if
        .end()
        // end loop
        .end()
        .end();

    this.replace_function_body(f_index, function);
}

trait ReportExt {
    fn push(self, c: &mut Compiler);
}

impl ReportExt for Report<'static> {
    fn push(self, c: &mut Compiler) {
        c.errors.push(self);
    }
}

impl ReportExt for ReportBuilder<'static, Range<usize>> {
    fn push(self, c: &mut Compiler) {
        c.errors.push(self.finish());
    }
}

/// A replacement for [wasm_encoder::Function] that allows adding locals gradually.
#[derive(Default)]
pub struct Function {
    num_locals: u32,
    locals: Vec<(u32, ValType)>,
    bytes: Vec<u8>,
    pub fn_id: Option<SymbolId>,
}

impl Function {
    fn new(params: &[ValType]) -> Function {
        let mut this = Function::default();
        for param in params {
            this.add_local(*param);
        }
        this
    }

    fn add_local(&mut self, ty: ValType) -> u32 {
        let id = self.num_locals;
        self.num_locals += 1;
        if let Some((last_count, last_type)) = self.locals.last_mut() {
            if ty == *last_type {
                *last_count += 1;
                return id;
            }
        }
        self.locals.push((1, ty));
        id
    }

    fn instructions(&mut self) -> InstructionSink {
        InstructionSink::new(&mut self.bytes)
    }
}

impl wasm_encoder::Encode for Function {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.locals.len().encode(sink);
        for (count, ty) in &self.locals {
            count.encode(sink);
            ty.encode(sink);
        }
        sink.extend_from_slice(&self.bytes);
    }
}

fn build_func(
    fn_id: SymbolId,
    f_info: &mut SymbolInformation<FuncInfo>,
    type_vars: &HashMap<TypeVar, ComparableType>,
    vars: &HashMap<SymbolId, SymbolInformation<VarInfo>>,
    is_main: bool,
) -> (StarFunctionType, Function) {
    // TODO: duplicated code
    let ty = StarFunctionType {
        params: f_info
            .info
            .effect_handlers
            .iter()
            .flat_map(|_effect_id| std::iter::repeat_n(StaticType::I32, 3))
            .chain(
                f_info
                    .info
                    .inputs_ty
                    .iter()
                    .zip(f_info.info.locals.iter().filter_map(|local| {
                        let var_info = &vars.get(local).as_ref().unwrap().info;

                        var_info.ty.as_ref().filter(|_| {
                            var_info.is_storage.is_none()
                                && !var_info.is_captured
                                && (var_info.is_argument || var_info.is_frame_pointer)
                        })
                    }))
                    .map(|(_, ty)| StaticType::from_canonical_type(ty, type_vars)),
            )
            .collect(),
        results: if is_main {
            vec![]
        } else {
            f_info
                .info
                .output_canonical_ty
                .as_ref()
                .map(|ty| vec![StaticType::from_canonical_type(ty, type_vars)])
                .unwrap_or_default()
        },
    };
    let lower_ty = ty.lower();
    let mut function = Function::new(lower_ty.params());

    function.fn_id.replace(fn_id);

    // used to save the caller's frame pointer
    f_info
        .info
        .saved_frame_local_index
        .replace(function.add_local(ValType::I32));

    for local in &f_info.info.locals {
        let var_info = vars.get(local).unwrap();

        if var_info.info.is_captured
            || var_info.info.is_argument
            || var_info.info.is_frame_pointer
            || var_info.info.is_storage.is_some()
        {
            continue;
        }

        let val_type =
            StaticType::from_canonical_type(var_info.info.ty.as_ref().unwrap(), type_vars).lower()
                [0];

        function.add_local(val_type);
    }
    (ty, function)
}

// -----------------------------------------------------------------------------
// Tests

#[cfg(test)]
mod tests {
    use crate::{compile, do_scope_analysis, do_type_inference, parse};
    use wasmparser::{Parser, Payload};

    /// Collect all export names from a WASM module.
    fn export_names(bytes: &[u8]) -> Vec<String> {
        let mut names = Vec::new();
        for payload in Parser::new(0).parse_all(bytes) {
            if let Ok(Payload::ExportSection(reader)) = payload {
                for export in reader {
                    let export = export.unwrap();
                    names.push(export.name.to_string());
                }
            }
        }
        names
    }

    #[test]
    fn compile_hello_world() {
        let src = include_str!("../../grammar/examples/hello_world.star");
        let (program, parse_errors) = parse(src);
        assert!(parse_errors.is_empty(), "parse errors: {parse_errors:?}");
        let program = program.expect("parse failed");

        let (program, mut symbols) = do_scope_analysis(program).unwrap();

        let (program, _warnings) = do_type_inference(program, &mut symbols)
            .map_err(|errors| {
                for e in &errors {
                    ariadne::Report::from(e)
                        .print(ariadne::Source::from(src))
                        .unwrap();
                }
            })
            .unwrap();

        let (wasm, compile_errors) = compile(&program, symbols);
        assert!(
            compile_errors.is_empty(),
            "compile errors: {compile_errors:?}"
        );
        let wasm = wasm.expect("compilation failed");

        let exports = export_names(&wasm);
        assert!(exports.iter().any(|e| e == "main"), "exports: {exports:?}");
    }

    #[test]
    fn compile_pay_to_public_key_hash() {
        let src = include_str!("../../grammar/examples/pay_to_public_key_hash.star");
        test_example(src);
    }

    #[test]
    fn compile_simple_oracle() {
        let src = include_str!("../../grammar/examples/simple_oracle.star");
        test_example(src);
    }

    #[test]
    fn compile_effect_handlers() {
        let src = include_str!("../../grammar/examples/effect_handlers.star");
        test_example(src);
    }

    #[test]
    fn compile_token_binding() {
        let src = include_str!("../../grammar/examples/tokens.star");
        test_example(src);
    }

    fn test_example(src: &str) {
        let (program, parse_errors) = parse(src);
        assert!(parse_errors.is_empty(), "parse errors: {parse_errors:?}");
        let program = program.expect("parse failed");

        let (program, mut symbols) = do_scope_analysis(program)
            .map_err(|errors| {
                for e in &errors {
                    ariadne::Report::from(e)
                        .print(ariadne::Source::from(src))
                        .unwrap();
                }
            })
            .unwrap();

        let (program, _warnings) = do_type_inference(program, &mut symbols)
            .map_err(|errors| {
                for e in &errors {
                    ariadne::Report::from(e)
                        .print(ariadne::Source::from(src))
                        .unwrap();
                }
            })
            .unwrap();

        let (wasm, compile_errors) = compile(&program, symbols);
        assert!(
            compile_errors.is_empty(),
            "compile errors: {compile_errors:?}"
        );

        for e in compile_errors {
            e.eprint(ariadne::Source::from(src)).unwrap();
        }

        let wasm = wasm.expect("compilation failed");

        let exports = export_names(&wasm);
        assert!(exports.iter().any(|e| e == "main"), "exports: {exports:?}");
    }
}
