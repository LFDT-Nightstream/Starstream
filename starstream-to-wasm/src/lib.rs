use std::{borrow::Cow, collections::HashMap, rc::Rc};

use miette::{Diagnostic, LabeledSpan};
use starstream_types::*;
use thiserror::Error;
use wasm_encoder::*;

use crate::component_abi::{ComponentAbiType, MAX_FLAT_PARAMS, MAX_FLAT_RESULTS, TypeBuilder};
use crate::decision_tree::{Ctor, DecisionTree, Matrix, Pat, Row};

mod component_abi;
mod decision_tree;

/*
    The entry point [compile] is responsible for the overall AST-to-WASM-module
    compilation that is exposed to the outside world. Most of the rest of this
    crate should stay private.

    One Starstream program is always converted to one Wasm module.

    The compiler walks the AST and generates section entries for types,
    imports, functions, and so on. Each function (for example `main`) is
    compiled to Wasm bytecode and added to the code section.

    Optimizations like constant-evaluation are left on the table for now.
    The easiest way to get them is probably to import [wasm-opt].

    [wasm-opt]: https://docs.rs/wasm-opt/latest/wasm_opt/
*/

/*
    Note on order of operations:

    - In binary operators, visit both the LHS and the RHS before `?`ing them.
    - When asserting or matching on a subexpression's type, do so after visiting it.

    This makes it possible to analyze the partial output of the compiler during
    debugging.
*/

/// Compile a Starstream program to a Wasm module.
pub fn compile(program: &TypedProgram) -> (Option<Vec<u8>>, Vec<CompileError>) {
    let mut compiler = Compiler::default();
    compiler.visit_program(program);
    compiler.finish()
}

struct CompilerOptions {
    check_overflows: bool,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            check_overflows: true,
        }
    }
}

/// A Wasm compiler error.
#[derive(Debug, Error)]
#[error("{message}")]
pub struct CompileError {
    message: String,
    span: Span,
}

impl Diagnostic for CompileError {
    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        Some(Box::new(std::iter::once(
            LabeledSpan::new_primary_with_span(Some(self.message.clone()), self.span.into_range()),
        )))
    }
}

/// Token type to represent an error that's already been logged.
struct ErrorToken;

type Result<T> = std::result::Result<T, ErrorToken>;

/// Holds the in-progress Wasm sections and other module-wide information
/// needed to build them.
#[derive(Default)]
struct Compiler {
    options: CompilerOptions,

    // Wasm binary output.
    types: TypeSection,
    imports: ImportSection,
    functions: FunctionSection,
    memory: MemorySection,
    globals: GlobalSection,
    exports: ExportSection,
    code: CodeSection,
    data: DataSection,

    // Component binary output.
    world_type: TypeBuilder<ComponentType>,
    star_to_component: HashMap<Type, Rc<ComponentAbiType>>,
    imported_interfaces: HashMap<String, TypeBuilder<InstanceType>>,

    // Diagnostics.
    fatal: bool,
    errors: Vec<CompileError>,

    // Function building.
    core_func_type_cache: HashMap<FuncType, u32>,
    /// Map from name to function index.
    callables: HashMap<String, u32>,

    // Memory building.
    bump_ptr: u32,
}

impl Compiler {
    /// After [Compiler::visit_program], this function collates all the
    /// in-progress sections into an actual Wasm binary module.
    fn finish(mut self) -> (Option<Vec<u8>>, Vec<CompileError>) {
        // TODO: any other final activity on the sections here, such as
        // committing constants to the memory/data section.

        // Generate memory.
        if self.bump_ptr > 0 {
            const PAGE_SIZE: u32 = 64 * 1024;
            self.memory.memory(MemoryType {
                minimum: std::cmp::min(u64::from(self.bump_ptr.div_ceil(PAGE_SIZE)), 1),
                maximum: None,
                memory64: false,
                shared: false,
                page_size_log2: None,
            });
            self.exports
                .export("memory", wasm_encoder::ExportKind::Memory, 0);
        }

        // Verify
        assert_eq!(self.functions.len(), self.code.len());

        // No reports generated beyond this point, so bail if fatal is set.
        if self.fatal {
            return (None, self.errors);
        }

        // Build the component custom section.
        /*
        (@custom "component-type"
            (component ;; the component embedded in the file exports a single Package type
                (type
                    (component ;; that Package exports a single World type
                        (type
                            (component ;; that World exports our actual functions
                                (type (instance (
                                    (type (func ...))
                                    (export "something-we-import" (func (type 0)))
                                )))
                                (import "foo:bar/baz@0.0.0" (instance (type 0)))

                                (type (func ...))
                                (export "function-name" (func (type 1)))
                            )
                        )
                        (export "namespace-name:package-name/world-name@0.1.0" (component (type 0)))
                    )
                )
                (export "anything here, it's ignored" (type 0))
            )
        )
        */

        for (interface_name, instance) in self.imported_interfaces {
            let i = self.world_type.inner.type_count();
            self.world_type.inner.ty().instance(&instance.inner);
            self.world_type
                .inner
                .import(&interface_name, ComponentTypeRef::Instance(i));
        }

        // The package type must always have 0 imports and 1 export which is the world.
        // Export must be named namespace:package/world, but @version is optional.
        let mut package_type = ComponentType::new();
        package_type.ty().component(&self.world_type.inner);
        package_type.export(
            "my-namespace:my-package/my-world",
            ComponentTypeRef::Component(0),
        );

        let mut component_types = ComponentTypeSection::new();
        let mut component_exports: ComponentExportSection = ComponentExportSection::new();
        // Embedded component must have 1 export which is the package type.
        // Export name doesn't matter.
        component_types.component(&package_type);
        component_exports.export("x", ComponentExportKind::Type, 0, None);

        // Write component sections to embedded component.
        let mut component = Component::new();
        component.section(&CustomSection {
            name: Cow::Borrowed("wit-component-encoding"),
            data: Cow::Borrowed(b"\x04\x00"),
        });
        component.section(&component_types);
        component.section(&component_exports);
        let component = component.finish();

        // Write sections to core module.
        // Mandatory WASM order per https://webassembly.github.io/spec/core/binary/modules.html#binary-module:
        // type, import, func, table, mem, tag, global, export, start, elem, datacount, code, data.
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
        module.section(&CustomSection {
            name: Cow::Borrowed("component-type"),
            data: Cow::Owned(component),
        });

        (Some(module.finish()), self.errors)
    }

    fn push_error(&mut self, span: Span, message: impl Into<String>) -> ErrorToken {
        self.errors.push(CompileError {
            message: message.into(),
            span,
        });
        self.fatal = true;
        ErrorToken
    }

    fn todo(&mut self, why: String) -> ErrorToken {
        // TODO: better span
        self.push_error(DUMMY_SPAN, format!("TODO: {why}"))
    }

    fn generate_storage_exports(
        &mut self,
        name: &Identifier,
        scope: &dyn Locals,
        fields: Vec<TypedStructField>,
    ) {
        if !fields.is_empty() {
            let storage_struct = Type::Record(RecordType {
                name: name.name.clone(),
                fields: fields
                    .iter()
                    .map(|f| RecordFieldType {
                        name: f.name.name.clone(),
                        ty: f.ty.clone(),
                    })
                    .collect(),
            });
            self.visit_struct(&TypedStructDef {
                name: name.clone(),
                fields: fields.clone(),
                ty: storage_struct.clone(),
            });
            self.visit_function(
                &TypedFunctionDef {
                    export: Some(FunctionExport::Script),
                    name: Identifier::anon("get_storage"),
                    params: Vec::new(),
                    return_type: storage_struct.clone(),
                    effect: EffectKind::Pure,
                    body: TypedBlock::from(Spanned::none(TypedExpr {
                        ty: storage_struct.clone(),
                        kind: TypedExprKind::StructLiteral {
                            name: name.clone(),
                            fields: fields
                                .iter()
                                .map(|f| TypedStructLiteralField {
                                    name: f.name.clone(),
                                    value: Spanned::none(TypedExpr {
                                        ty: f.ty.clone(),
                                        kind: TypedExprKind::Identifier(f.name.clone()),
                                    }),
                                })
                                .collect(),
                        },
                    })),
                },
                scope,
            );
            self.visit_function(
                &TypedFunctionDef {
                    export: Some(FunctionExport::Script),
                    name: Identifier::anon("set_storage"),
                    params: vec![TypedFunctionParam {
                        name: Identifier::anon("storage"),
                        ty: storage_struct.clone(),
                    }],
                    return_type: Type::Unit,
                    effect: EffectKind::Pure,
                    body: TypedBlock::from(
                        fields
                            .iter()
                            .map(|f| TypedStatement::Assignment {
                                target: f.name.clone(),
                                value: Spanned::none(TypedExpr {
                                    ty: f.ty.clone(),
                                    kind: TypedExprKind::FieldAccess {
                                        target: Box::new(Spanned::none(TypedExpr {
                                            ty: storage_struct.clone(),
                                            kind: TypedExprKind::Identifier(Identifier::anon(
                                                "storage",
                                            )),
                                        })),
                                        field: f.name.clone(),
                                    },
                                }),
                            })
                            .collect::<Vec<_>>(),
                    ),
                },
                scope,
            );
        }
    }

    // ------------------------------------------------------------------------
    // Core table management

    /// Add a function signature to the `types` section if needed, and return
    /// the index of the new or existing entry.
    fn add_core_func_type(&mut self, ty: FuncType) -> u32 {
        match self.core_func_type_cache.get(&ty) {
            Some(&index) => index,
            None => {
                let index = self.types.len();
                self.types.ty().func_type(&ty);
                self.core_func_type_cache.insert(ty, index);
                index
            }
        }
    }

    /// Add a new function to both the `functions` and `code` section, and
    /// return its index.
    fn add_function(&mut self, ty: FuncType, code: Function) -> u32 {
        // TODO: enforce that all *imported* function IDs are known before this
        // is called, as Wasm requires all imports to precede all of the
        // module's own functions.

        let type_index = self.add_core_func_type(ty);
        let func_index = self.functions.len();
        self.functions.function(type_index);

        let mut vec = Vec::new();
        code.encode(&mut vec);
        self.code.raw(&vec);

        func_index
    }

    fn add_globals(&mut self, types: impl IntoIterator<Item = ValType>) -> u32 {
        let idx = self.globals.len();
        for ty in types {
            self.globals.global(
                GlobalType {
                    val_type: ty,
                    mutable: true,
                    shared: false,
                },
                &ConstExpr::i64_const(0),
            );
        }
        idx
    }

    fn export_core_fn(&mut self, name: &str, idx: u32) {
        self.exports.export(name, ExportKind::Func, idx);
    }

    /// Get or create the `__starstream_i64_add_checked` function for overflow-checked addition.
    /// Returns the function index.
    fn get_or_create_i64_add_checked(&mut self) -> u32 {
        if let Some(&checked_sum) = self.callables.get("__starstream_i64_add_checked") {
            return checked_sum;
        }

        let params = [ValType::I64, ValType::I64];
        let result = [ValType::I64];
        let mut code = Function::from_params(&params);

        let sum_local = code.add_locals([ValType::I64]);

        code.instructions().local_get(0); //  [x]
        code.instructions().local_get(1); //  [x, y]
        code.instructions().i64_add();
        code.instructions().local_tee(sum_local); //  [sum]

        // Check for overflow.

        // (x^y)>=0
        code.instructions().local_get(0); //  [sum, x]
        code.instructions().local_get(1); //  [sum, x, y]
        code.instructions().i64_xor(); //  [sum, x^y]
        code.instructions().i64_const(0); //  [sum, x^y, 0]  
        code.instructions().i64_ge_s(); //  [sum, (x^y)>=0]

        // (sum^x)<0
        code.instructions().local_get(sum_local); //  [sum, (x^y)>=0, sum]
        code.instructions().local_get(0); //  [sum, (x^y)>=0, sum, x]
        code.instructions().i64_xor(); //  [sum, (x^y)>=0, sum^x]
        code.instructions().i64_const(0); //  [sum, (x^y)>=0, sum^x, 0]
        code.instructions().i64_lt_s(); //  [sum, (x^y)>=0, (sum^x)<0]
        code.instructions().i32_and(); //  [sum, overflow_flag]

        // If overflow_flag != 0, trap.
        code.instructions().i32_const(0); //  [sum, overflow_flag, 0]
        code.instructions().i32_ne(); //  [sum, overflowed?]
        code.instructions().if_(BlockType::Empty); //  [sum]
        code.instructions().unreachable();
        code.instructions().end();

        // Sum is already on stack, add function body end
        code.instructions().end();

        let idx = self.add_function(FuncType::new(params, result), code);

        self.callables
            .insert("__starstream_i64_add_checked".to_string(), idx);

        idx
    }

    /// Get or create the `__starstream_i64_sub_checked` function for overflow-checked subtraction.
    /// Returns the function index.
    fn get_or_create_i64_sub_checked(&mut self) -> u32 {
        if let Some(&checked_sub) = self.callables.get("__starstream_i64_sub_checked") {
            return checked_sub;
        }

        let params = [ValType::I64, ValType::I64];
        let result = [ValType::I64];
        let mut code = Function::from_params(&params);

        let diff_local = code.add_locals([ValType::I64]);

        code.instructions().local_get(0); //  [x]
        code.instructions().local_get(1); //  [x, y]
        code.instructions().i64_sub();
        code.instructions().local_tee(diff_local); //  [diff]

        // Check for overflow.
        // Overflow occurs when subtracting a negative from a positive yields negative,
        // or subtracting a positive from a negative yields positive.
        // In other words: (x^y)<0 && (diff^x)<0

        // (x^y)<0
        code.instructions().local_get(0); //  [diff, x]
        code.instructions().local_get(1); //  [diff, x, y]
        code.instructions().i64_xor(); //  [diff, x^y]
        code.instructions().i64_const(0); //  [diff, x^y, 0]
        code.instructions().i64_lt_s(); //  [diff, (x^y)<0]

        // (diff^x)<0
        code.instructions().local_get(diff_local); //  [diff, (x^y)<0, diff]
        code.instructions().local_get(0); //  [diff, (x^y)<0, diff, x]
        code.instructions().i64_xor(); //  [diff, (x^y)<0, diff^x]
        code.instructions().i64_const(0); //  [diff, (x^y)<0, diff^x, 0]
        code.instructions().i64_lt_s(); //  [diff, (x^y)<0, (diff^x)<0]
        code.instructions().i32_and(); //  [diff, overflow_flag]

        // If overflow_flag != 0, trap.
        code.instructions().i32_const(0); //  [diff, overflow_flag, 0]
        code.instructions().i32_ne(); //  [diff, overflowed?]
        code.instructions().if_(BlockType::Empty); //  [diff]
        code.instructions().unreachable();
        code.instructions().end();

        // Diff is already on stack, add function body end
        code.instructions().end();

        let idx = self.add_function(FuncType::new(params, result), code);

        self.callables
            .insert("__starstream_i64_sub_checked".to_string(), idx);

        idx
    }

    /// Get or create the `__starstream_i64_mul_checked` function for overflow-checked multiplication.
    /// Returns the function index.
    fn get_or_create_i64_mul_checked(&mut self) -> u32 {
        if let Some(&checked_mul) = self.callables.get("__starstream_i64_mul_checked") {
            return checked_mul;
        }

        let params = [ValType::I64, ValType::I64];
        let result = [ValType::I64];
        let mut code = Function::from_params(&params);

        let product_local = code.add_locals([ValType::I64]);
        let x_local = code.add_locals([ValType::I64]);
        let y_local = code.add_locals([ValType::I64]);

        // Store parameters in locals for reuse
        code.instructions().local_get(0); //  [x]
        code.instructions().local_tee(x_local); //  [x]
        code.instructions().local_get(1); //  [x, y]
        code.instructions().local_tee(y_local); //  [x, y]
        code.instructions().i64_mul();
        code.instructions().local_tee(product_local); //  [product]

        // Check for overflow.
        // For multiplication overflow detection, we check:
        // 1. If y == 0, no overflow
        // 2. If product / y != x, then overflow occurred
        // Special case: MIN * -1 overflows (since -MIN doesn't fit in i64)

        // Special case: check if x == MIN && y == -1
        code.instructions().local_get(x_local); //  [product, x]
        code.instructions().i64_const(i64::MIN); //  [product, x, MIN]
        code.instructions().i64_eq(); //  [product, x==MIN]
        code.instructions().local_get(y_local); //  [product, x==MIN, y]
        code.instructions().i64_const(-1); //  [product, x==MIN, y, -1]
        code.instructions().i64_eq(); //  [product, x==MIN, y==-1]
        code.instructions().i32_and(); //  [product, (x==MIN && y==-1)]

        // Also check y == MIN && x == -1
        code.instructions().local_get(y_local); //  [product, (x==MIN && y==-1), y]
        code.instructions().i64_const(i64::MIN); //  [product, (x==MIN && y==-1), y, MIN]
        code.instructions().i64_eq(); //  [product, (x==MIN && y==-1), y==MIN]
        code.instructions().local_get(x_local); //  [product, (x==MIN && y==-1), y==MIN, x]
        code.instructions().i64_const(-1); //  [product, (x==MIN && y==-1), y==MIN, x, -1]
        code.instructions().i64_eq(); //  [product, (x==MIN && y==-1), y==MIN, x==-1]
        code.instructions().i32_and(); //  [product, (x==MIN && y==-1), (y==MIN && x==-1)]
        code.instructions().i32_or(); //  [product, overflow_special]

        code.instructions().if_(BlockType::Empty); //  [product]
        code.instructions().unreachable();
        code.instructions().end();

        // Check regular overflow: if y != 0 and product / y != x
        code.instructions().local_get(y_local); //  [product, y]
        code.instructions().i64_const(0); //  [product, y, 0]
        code.instructions().i64_ne(); //  [product, y!=0]
        code.instructions().if_(BlockType::Empty); //  [product]
        // y != 0, so check if product / y == x
        code.instructions().local_get(product_local); //  [product, product]
        code.instructions().local_get(y_local); //  [product, product, y]
        code.instructions().i64_div_s(); //  [product, product/y]
        code.instructions().local_get(x_local); //  [product, product/y, x]
        code.instructions().i64_ne(); //  [product, (product/y)!=x]
        code.instructions().if_(BlockType::Empty); //  [product]
        code.instructions().unreachable();
        code.instructions().end();
        code.instructions().end();

        // Product is already on stack, add function body end
        code.instructions().end();

        let idx = self.add_function(FuncType::new(params, result), code);

        self.callables
            .insert("__starstream_i64_mul_checked".to_string(), idx);

        idx
    }

    // ------------------------------------------------------------------------
    // Component table management

    fn encode_component_func_type(&mut self, function: &TypedFunctionDef) -> u32 {
        let params = function
            .params
            .iter()
            .flat_map(|p| {
                self.star_to_component_type(&p.ty)
                    .map(|t| (p.name.as_str(), t))
            })
            .collect::<Vec<_>>();
        let result = self.star_to_component_type(&function.return_type);
        self.world_type
            .encode_func(params.into_iter(), result.as_ref())
    }

    fn encode_component_type(&mut self, ty: &Rc<ComponentAbiType>) -> ComponentValType {
        self.world_type.encode_value(ty)
    }

    fn export_component_fn(
        &mut self,
        function: &TypedFunctionDef,
        func_idx: u32,
        params: &[ValType],
        core_results: &[ValType],
    ) {
        let name = to_kebab_case(function.name.as_str());

        if params.len() <= MAX_FLAT_PARAMS && core_results.len() <= MAX_FLAT_RESULTS {
            // No need to spill params or results to heap, so don't wrap.
            self.export_core_fn(&name, func_idx);
            let type_idx = self.encode_component_func_type(function);
            self.world_type
                .inner
                .export(&name, ComponentTypeRef::Func(type_idx));
        } else if params.len() <= MAX_FLAT_PARAMS {
            // results.len() > MAX_FLAT_RESULTS, so spill to linear memory.
            let result = self.star_to_component_type(&function.return_type).unwrap();
            let (size, align) = result.size_align();
            let return_slot = self.alloc_static(size, align);

            let mut wrapper_func = Function::from_params(params);
            wrapper_func.instructions().i32_const(return_slot as i32);
            // Push parameters and call inner function.
            for i in 0..params.len() {
                wrapper_func.instructions().local_get(i as u32);
            }
            wrapper_func.instructions().call(func_idx);
            // Write to our return slot.
            self.component_store(function.name.span(), &mut wrapper_func, &result, 0);
            // Return our return slot.
            wrapper_func.instructions().i32_const(return_slot as i32);
            wrapper_func.instructions().end();

            let wrapper_func_idx = self.add_function(
                FuncType::new(params.iter().copied(), [ValType::I32]),
                wrapper_func,
            );

            self.export_core_fn(&name, wrapper_func_idx);
            let type_idx = self.encode_component_func_type(function);
            self.world_type
                .inner
                .export(&name, ComponentTypeRef::Func(type_idx));
        } else {
            self.push_error(
                function.name.span(),
                "TODO: Component ABI for function with too many params",
            );
        }
    }

    fn export_component_ty(&mut self, name: &str, ty: &Type) {
        let component_ty = self.star_to_component_type(ty).unwrap();
        let ComponentValType::Type(idx) = self.encode_component_type(&component_ty) else {
            unreachable!()
        };
        // "Exporting" a type consists of importing it with an equality constraint.
        let new_idx = self.world_type.inner.type_count();
        self.world_type.inner.import(
            &to_kebab_case(name),
            ComponentTypeRef::Type(TypeBounds::Eq(idx)),
        );
        // Future uses must also refer to the imported version.
        self.world_type
            .component_to_encoded
            .insert(component_ty, ComponentValType::Type(new_idx));
    }

    // ------------------------------------------------------------------------
    // Memory management

    fn alloc_static(&mut self, size: u32, align: u32) -> u32 {
        const MINIMUM_ADDR: u32 = 4;
        let addr = self.bump_ptr.max(MINIMUM_ADDR).next_multiple_of(align);
        self.bump_ptr = addr + size;
        addr
    }

    // Expects address then values on the stack.
    // https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md#storing
    fn component_store(
        &mut self,
        span: Span,
        func: &mut Function,
        ty: &ComponentAbiType,
        offset: u64,
    ) {
        let mut core_types = Vec::new();
        _ = self.component_to_core_types(span, &mut core_types, ty);

        let mut store_fns = Vec::new();
        ty.get_store_fns(0, offset, &mut store_fns);

        assert_eq!(core_types.len(), store_fns.len());

        if let [only] = &store_fns[..] {
            only(func.instructions());
        } else if store_fns.len() > 1 {
            // Pop values then address into locals.
            let addr_local = func.add_locals([ValType::I32]);
            let value_local = func.add_locals(core_types.iter().copied());
            for i in (0..core_types.len()).rev() {
                func.instructions().local_set(value_local + (i as u32));
            }
            func.instructions().local_set(addr_local);

            // Store each thing one by one.
            for (i, store) in store_fns.iter().enumerate() {
                func.instructions().local_get(addr_local);
                func.instructions().local_get(value_local + (i as u32));
                store(func.instructions());
            }
        }
    }

    // ------------------------------------------------------------------------
    // Type conversion

    fn star_to_component_type(&mut self, ty: &Type) -> Option<Rc<ComponentAbiType>> {
        if let Some(cat) = self.star_to_component.get(ty) {
            return Some(cat.clone());
        }

        let cat = match ty {
            Type::Var(_) => todo!(),
            Type::Int => ComponentAbiType::S64,
            Type::Bool => ComponentAbiType::Bool,
            Type::Unit => {
                return None;
            }
            Type::Function { .. } => todo!(),
            Type::Tuple(_) => todo!(),
            Type::Record(record) => {
                let fields = record
                    .fields
                    .iter()
                    .flat_map(|f| {
                        self.star_to_component_type(&f.ty)
                            .map(|ty| (f.name.as_str().to_owned(), ty))
                    })
                    .collect();
                ComponentAbiType::Record { fields }
            }
            Type::Enum(enum_variant_types) => {
                // Detect builtin Option type
                if enum_variant_types.name == "Option"
                    && enum_variant_types.variants.len() == 2
                    && enum_variant_types.variants.iter().any(|v| v.name == "Some")
                    && enum_variant_types.variants.iter().any(|v| v.name == "None")
                    && let Some(some_variant) = enum_variant_types
                        .variants
                        .iter()
                        .find(|v| v.name == "Some")
                    && let EnumVariantKind::Tuple(fields) = &some_variant.kind
                    && let Some(inner) = self.star_to_component_type(&fields[0])
                {
                    return Some(Rc::new(ComponentAbiType::Option { inner }));
                }

                // Detect builtin Result type
                if enum_variant_types.name == "Result"
                    && enum_variant_types.variants.len() == 2
                    && enum_variant_types.variants.iter().any(|v| v.name == "Ok")
                    && enum_variant_types.variants.iter().any(|v| v.name == "Err")
                {
                    let ok_variant = enum_variant_types.variants.iter().find(|v| v.name == "Ok");
                    let err_variant = enum_variant_types.variants.iter().find(|v| v.name == "Err");
                    if let (Some(ok_v), Some(err_v)) = (ok_variant, err_variant) {
                        let ok = match &ok_v.kind {
                            EnumVariantKind::Tuple(fields) => {
                                Some(self.star_to_component_type(&fields[0])?)
                            }
                            EnumVariantKind::Unit => None,
                            _ => None,
                        };
                        let err = match &err_v.kind {
                            EnumVariantKind::Tuple(fields) => {
                                Some(self.star_to_component_type(&fields[0])?)
                            }
                            EnumVariantKind::Unit => None,
                            _ => None,
                        };
                        return Some(Rc::new(ComponentAbiType::Result { ok, err }));
                    }
                }

                // Generic variant conversion
                let cases = enum_variant_types
                    .variants
                    .iter()
                    .map(|v| {
                        (
                            to_kebab_case(&v.name),
                            match &v.kind {
                                EnumVariantKind::Unit => None,
                                EnumVariantKind::Tuple(fields) => {
                                    let fields: Vec<_> = fields
                                        .iter()
                                        .flat_map(|f| self.star_to_component_type(f))
                                        .collect();
                                    if fields.is_empty() {
                                        None
                                    } else {
                                        Some(Rc::new(ComponentAbiType::Tuple { fields }))
                                    }
                                }
                                EnumVariantKind::Struct(fields) => {
                                    let fields: Vec<_> = fields
                                        .iter()
                                        .flat_map(|f| self.star_to_component_type(&f.ty))
                                        .collect();
                                    if fields.is_empty() {
                                        None
                                    } else {
                                        Some(Rc::new(ComponentAbiType::Tuple { fields }))
                                    }
                                }
                            },
                        )
                    })
                    .collect();
                ComponentAbiType::Variant { cases }
            }
        };

        let cat = Rc::new(cat);
        self.star_to_component.insert(ty.clone(), cat.clone());
        Some(cat)
    }

    fn star_to_core_types(&mut self, span: Span, dest: &mut Vec<ValType>, ty: &Type) -> Result<()> {
        let mut ok = Ok(());
        match ty {
            Type::Unit => {}
            Type::Bool => dest.push(ValType::I32),
            Type::Int => dest.push(ValType::I64),
            Type::Tuple(items) => {
                // flatten_record
                for each in items {
                    ok = self.star_to_core_types(span, dest, each).and(ok);
                }
            }
            Type::Record(record) => {
                // flatten_record
                for f in &record.fields {
                    ok = self.star_to_core_types(span, dest, &f.ty).and(ok);
                }
            }
            Type::Enum(EnumType {
                name: _, variants, ..
            }) => {
                // flatten_variant
                let mut flat = Vec::new();
                for v in variants {
                    let mut case_flat = Vec::new();
                    match &v.kind {
                        EnumVariantKind::Unit => {}
                        EnumVariantKind::Tuple(fields) => {
                            for each in fields {
                                ok = self.star_to_core_types(span, &mut case_flat, each).and(ok);
                            }
                        }
                        EnumVariantKind::Struct(fields) => {
                            for each in fields {
                                ok = self
                                    .star_to_core_types(span, &mut case_flat, &each.ty)
                                    .and(ok);
                            }
                        }
                    }
                    for (i, ft) in case_flat.into_iter().enumerate() {
                        if let Some(e) = flat.get_mut(i) {
                            *e = Self::join(*e, ft);
                        } else {
                            flat.push(ft);
                        }
                    }
                }
                ok = self
                    .component_to_core_types(
                        span,
                        dest,
                        &ComponentAbiType::discriminant_type(variants.len()),
                    )
                    .and(ok);
                dest.extend(flat);
            }
            _ => ok = Err(self.push_error(span, format!("unknown core lowering for {ty:?}"))),
        }
        ok
    }

    fn component_to_core_types(
        &mut self,
        span: Span,
        dest: &mut Vec<ValType>,
        ty: &ComponentAbiType,
    ) -> Result<()> {
        let mut ok = Ok(());
        match ty {
            ComponentAbiType::Bool => dest.push(ValType::I32),
            ComponentAbiType::U8 => dest.push(ValType::I32),
            ComponentAbiType::S8 => dest.push(ValType::I32),
            ComponentAbiType::U16 => dest.push(ValType::I32),
            ComponentAbiType::S16 => dest.push(ValType::I32),
            ComponentAbiType::U32 => dest.push(ValType::I32),
            ComponentAbiType::S32 => dest.push(ValType::I32),
            ComponentAbiType::U64 => dest.push(ValType::I64),
            ComponentAbiType::S64 => dest.push(ValType::I64),
            ComponentAbiType::F32 => dest.push(ValType::F32),
            ComponentAbiType::F64 => dest.push(ValType::F64),
            ComponentAbiType::Char => dest.push(ValType::I32),
            ComponentAbiType::Record { fields } => {
                // flatten_record
                for (_, f) in fields {
                    ok = self.component_to_core_types(span, dest, f).and(ok);
                }
            }
            ComponentAbiType::Tuple { fields } => {
                // flatten_record
                for f in fields {
                    ok = self.component_to_core_types(span, dest, f).and(ok);
                }
            }
            ComponentAbiType::Variant { cases } => {
                // flatten_variant
                let mut flat = Vec::new();
                for (_, t) in cases {
                    if let Some(t) = t {
                        let mut case_flat = Vec::new();
                        ok = self
                            .component_to_core_types(span, &mut case_flat, t)
                            .and(ok);
                        for (i, ft) in case_flat.into_iter().enumerate() {
                            if let Some(e) = flat.get_mut(i) {
                                *e = Self::join(*e, ft);
                            } else {
                                flat.push(ft);
                            }
                        }
                    }
                }
                ok = self
                    .component_to_core_types(
                        span,
                        dest,
                        &ComponentAbiType::discriminant_type(cases.len()),
                    )
                    .and(ok);
                dest.extend(flat);
            }
            _ => ok = Err(self.push_error(span, format!("unknown component lowering for {ty:?}"))),
        }
        ok
    }

    fn join(a: ValType, b: ValType) -> ValType {
        match (a, b) {
            (a, b) if a == b => a,
            (ValType::I32, ValType::F32) | (ValType::F32, ValType::I32) => ValType::I32,
            _ => ValType::I64,
        }
    }

    fn star_count_core_types(&mut self, ty: &Type) -> u32 {
        match ty {
            Type::Unit => 0,
            Type::Bool => 1,
            Type::Int => 1,
            Type::Tuple(items) => items.iter().map(|t| self.star_count_core_types(t)).sum(),
            Type::Record(record) => record
                .fields
                .iter()
                .map(|f| self.star_count_core_types(&f.ty))
                .sum(),
            Type::Enum(enum_) => {
                // discriminator + the max number of slots used by any variant
                1 + enum_
                    .variants
                    .iter()
                    .map(|v| match &v.kind {
                        EnumVariantKind::Unit => 0,
                        EnumVariantKind::Tuple(fields) => {
                            fields.iter().map(|t| self.star_count_core_types(t)).sum()
                        }
                        EnumVariantKind::Struct(fields) => fields
                            .iter()
                            .map(|f| self.star_count_core_types(&f.ty))
                            .sum(),
                    })
                    .max()
                    .unwrap_or(0)
            }
            Type::Function { .. } => todo!(),
            Type::Var(_) => todo!(),
        }
    }

    // ------------------------------------------------------------------------
    // Visitors

    /// Root visitor called by [compile] to start walking the AST for a program,
    /// building the Wasm sections on the way.
    fn visit_program(&mut self, program: &TypedProgram) {
        // In the Wasm output, imported functions must precede defined
        // functions, so take care of them now.
        for definition in &program.definitions {
            match definition {
                TypedDefinition::Import(def) => self.visit_import(def),
                TypedDefinition::Abi(def) => self.visit_abi(def),
                // All others handled below.
                _ => {}
            }
        }

        for definition in &program.definitions {
            match definition {
                TypedDefinition::Import(_) => { /* Handled above. */ }
                TypedDefinition::Abi(_) => { /* Handled above. */ }

                TypedDefinition::Function(func) => self.visit_function(func, &()),
                TypedDefinition::Struct(struct_) => self.visit_struct(struct_),
                TypedDefinition::Utxo(utxo) => self.visit_utxo(utxo),
                TypedDefinition::Enum(enum_) => self.visit_enum(enum_),
            }
        }
    }

    fn visit_import(&mut self, def: &TypedImportDef) {
        let (namespace, list) = match &def.items {
            TypedImportItems::Named(functions) => (None, functions),
            TypedImportItems::Namespace { alias, functions } => (Some(alias), functions),
        };
        for item in list {
            let local_name = if let Some(namespace) = namespace {
                format!("{}::{}", namespace, item.local)
            } else {
                item.local.to_string()
            };

            match &item.ty {
                Type::Function {
                    params,
                    result,
                    effect: _,
                } => {
                    let mut core_params = Vec::with_capacity(16);
                    let mut core_results = Vec::with_capacity(1);
                    let span = item.local.span();
                    for p in params {
                        _ = self.star_to_core_types(span, &mut core_params, p);
                    }
                    _ = self.star_to_core_types(span, &mut core_results, result);

                    let kebab = to_kebab_case(&item.imported.name);

                    // Core import
                    let core_fn_ty = self.add_core_func_type(FuncType::new(
                        core_params.iter().copied(),
                        core_results.iter().copied(),
                    ));
                    let func = self.imports.len(); // TODO: Might be incorrect if we import non-functions
                    self.imports.import(
                        &def.from.to_string(),
                        &kebab,
                        EntityType::Function(core_fn_ty),
                    );
                    self.callables.insert(local_name, func);

                    // Component import
                    let comp_params = params
                        .iter()
                        .flat_map(|p| self.star_to_component_type(p).map(|t| ("x", t)))
                        .collect::<Vec<_>>();
                    let comp_result = self.star_to_component_type(result);
                    let iface = self
                        .imported_interfaces
                        .entry(def.from.to_string())
                        .or_default();
                    let comp_fn_ty =
                        iface.encode_func(comp_params.into_iter(), comp_result.as_ref());
                    iface
                        .inner
                        .export(&kebab, ComponentTypeRef::Func(comp_fn_ty));
                }
                _ => todo!(),
            }
        }
    }

    fn visit_abi(&mut self, def: &TypedAbiDef) {
        for part in &def.parts {
            match part {
                TypedAbiPart::Event(event) => {
                    let mut core_params = Vec::with_capacity(16);
                    let span = event.name.span();
                    for p in &event.params {
                        _ = self.star_to_core_types(span, &mut core_params, &p.ty);
                    }

                    let interface = to_kebab_case(def.name.as_str());
                    let kebab = to_kebab_case(event.name.as_str());

                    // Core import
                    let core_fn_ty = self.add_core_func_type(FuncType::new(
                        core_params.iter().copied(),
                        std::iter::empty(),
                    ));
                    let func = self.imports.len(); // TODO: Might be incorrect if we import non-functions
                    self.imports
                        .import(&interface, &kebab, EntityType::Function(core_fn_ty));
                    self.callables.insert(event.name.as_str().to_owned(), func);

                    // Component import
                    let comp_params = event
                        .params
                        .iter()
                        .flat_map(|p| self.star_to_component_type(&p.ty).map(|t| ("x", t)))
                        .collect::<Vec<_>>();
                    let comp_result = None;
                    let iface = self.imported_interfaces.entry(interface).or_default();
                    let comp_fn_ty =
                        iface.encode_func(comp_params.into_iter(), comp_result.as_ref());
                    iface
                        .inner
                        .export(&kebab, ComponentTypeRef::Func(comp_fn_ty));
                }
            }
        }
    }

    fn visit_function(&mut self, function: &TypedFunctionDef, parent: &dyn Locals) {
        let mut locals = HashMap::<String, Var>::new();
        let mut params = Vec::with_capacity(16);
        for p in &function.params {
            locals.insert(
                p.name.name.clone(),
                Var::Local(u32::try_from(params.len()).unwrap()),
            );
            _ = self.star_to_core_types(p.name.span_or(function.name.span()), &mut params, &p.ty);
        }

        let mut func = Function::from_params(&params);

        let mut results = Vec::with_capacity(1);
        _ = self.star_to_core_types(function.name.span(), &mut results, &function.return_type);

        let _ = self.visit_block_stack(&mut func, &(parent, &locals), &function.body);
        func.instructions().end();

        let idx = self.add_function(
            FuncType::new(params.iter().copied(), results.iter().copied()),
            func,
        );
        self.callables
            .insert(function.name.as_str().to_owned(), idx);

        match function.export {
            Some(FunctionExport::Script) => {
                self.export_component_fn(function, idx, &params, &results);
            }
            Some(FunctionExport::UtxoMain) => {
                self.export_component_fn(function, idx, &params, &results);
            }
            None => {}
        }
    }

    fn visit_struct(&mut self, struct_: &TypedStructDef) {
        self.export_component_ty(struct_.name.as_str(), &struct_.ty);
    }

    fn visit_enum(&mut self, enum_: &TypedEnumDef) {
        self.export_component_ty(enum_.name.as_str(), &enum_.ty);
    }

    fn visit_utxo(&mut self, utxo: &TypedUtxoDef) {
        // TODO: use the utxo name to declare the type, etc.
        let mut utxo_storage = HashMap::new();
        let mut utxo_record_type = Vec::new();
        for part in &utxo.parts {
            match part {
                TypedUtxoPart::Storage(vars) => {
                    for var in vars {
                        let mut types = Vec::new();
                        _ = self.star_to_core_types(utxo.name.span(), &mut types, &var.ty);
                        let idx = self.add_globals(types.iter().copied());
                        utxo_storage.insert(var.name.name.clone(), Var::Global(idx));
                        utxo_record_type.push(TypedStructField {
                            name: var.name.clone(),
                            ty: var.ty.clone(),
                        });
                    }
                }
                TypedUtxoPart::MainFn(function) => {
                    self.visit_function(function, &(&() as &dyn Locals, &utxo_storage));
                }
            }
        }
        self.generate_storage_exports(
            &utxo.name,
            &(&() as &dyn Locals, &utxo_storage),
            utxo_record_type,
        );
    }

    /// Start a new identifier scope and generate bytecode for the statements
    /// of the block in sequence. Only creates a Wasm `block` when specifically
    /// needed for control flow reasons.
    fn visit_block_common(
        &mut self,
        func: &mut Function,
        parent: &dyn Locals,
        block: &TypedBlock,
    ) -> Result<HashMap<String, Var>> {
        let mut locals = HashMap::new();
        for statement in &block.statements {
            match statement {
                TypedStatement::Expression(expr) => {
                    self.visit_expr_drop(func, &(parent, &locals), expr.span, &expr.node)?;
                }
                TypedStatement::VariableDeclaration {
                    mutable: _,
                    name,
                    value,
                } => {
                    // Allocate local space.
                    let mut local_types = Vec::new();
                    _ = self.star_to_core_types(value.span, &mut local_types, &value.node.ty);
                    let local = func.add_locals(local_types.iter().copied());
                    locals.insert(name.name.clone(), Var::Local(local));

                    if self
                        .visit_expr_stack(func, &(parent, &locals), value.span, &value.node)
                        .is_ok()
                    {
                        // Pop from stack to set locals in reverse order.
                        for i in (0..local_types.len()).rev() {
                            func.instructions().local_set(local + (i as u32));
                        }
                    }
                }
                TypedStatement::Assignment { target, value } => {
                    if let Some(var) = (parent, &locals).get(&target.name) {
                        if self
                            .visit_expr_stack(func, &(parent, &locals), value.span, &value.node)
                            .is_ok()
                        {
                            let mut types = Vec::new();
                            _ = self.star_to_core_types(value.span, &mut types, &value.node.ty);

                            // Pop from stack to set locals in reverse order.
                            match var {
                                Var::Local(local) => {
                                    for i in (0..types.len()).rev() {
                                        func.instructions().local_set(local + (i as u32));
                                    }
                                }
                                Var::Global(global) => {
                                    for i in (0..types.len()).rev() {
                                        func.instructions().global_set(global + (i as u32));
                                    }
                                }
                            }
                        }
                    } else {
                        self.push_error(
                            target.span_or(value.span),
                            format!("unknown name {:?}", target.name),
                        );
                    }
                }
                // Recursive
                TypedStatement::While { condition, body } => {
                    func.instructions().block(BlockType::Empty); // br(1) is break
                    func.instructions().loop_(BlockType::Empty); // br(0) is continue

                    let _ = self.visit_expr_stack(
                        func,
                        &(parent, &locals),
                        condition.span,
                        &condition.node,
                    );
                    assert_eq!(condition.node.ty, Type::Bool);
                    // if condition == 0, break
                    func.instructions().i32_eqz();
                    func.instructions().br_if(1);
                    // contents
                    self.visit_block_drop(func, &(parent, &locals), body)?;
                    // continue
                    func.instructions().br(0).end().end();
                }
                TypedStatement::Return(Some(expr)) => {
                    let _ = self.visit_expr_stack(func, &(parent, &locals), expr.span, &expr.node);
                    func.instructions().return_();
                }
                TypedStatement::Return(None) => {
                    func.instructions().return_();
                }
            }
        }
        Ok(locals)
    }

    /// Visit a block, dropping its result.
    fn visit_block_drop(
        &mut self,
        func: &mut Function,
        parent: &dyn Locals,
        block: &TypedBlock,
    ) -> Result<()> {
        let locals = self.visit_block_common(func, parent, block)?;
        if let Some(expr) = &block.tail_expression {
            self.visit_expr_drop(func, &(parent, &locals), expr.span, &expr.node)
        } else {
            Ok(())
        }
    }

    /// Visit a block, storing its result to the stack.
    fn visit_block_stack(
        &mut self,
        func: &mut Function,
        parent: &dyn Locals,
        block: &TypedBlock,
    ) -> Result<()> {
        let locals = self.visit_block_common(func, parent, block)?;
        if let Some(expr) = &block.tail_expression {
            self.visit_expr_stack(func, &(parent, &locals), expr.span, &expr.node)
        } else {
            Ok(())
        }
    }

    /// Visit an expression, dropping its result.
    fn visit_expr_drop(
        &mut self,
        func: &mut Function,
        locals: &dyn Locals,
        span: Span,
        expr: &TypedExpr,
    ) -> Result<()> {
        // TODO: Warn on expressions that have no effect.
        match &expr.kind {
            TypedExprKind::Literal(_) => {}
            TypedExprKind::Identifier(_) => {}
            TypedExprKind::Unary { op: _, expr } => {
                self.visit_expr_drop(func, locals, expr.span, &expr.node)?;
            }
            // _ = lhs && rhs --> if (lhs) { _ = rhs; }
            TypedExprKind::Binary {
                op: BinaryOp::And,
                left,
                right,
            } => {
                self.visit_expr_stack(func, locals, left.span, &left.node)?;
                assert_eq!(left.node.ty, Type::Bool);
                func.instructions().if_(BlockType::Empty);
                self.visit_expr_drop(func, locals, right.span, &right.node)?;
                assert_eq!(right.node.ty, Type::Bool);
                func.instructions().end();
            }
            // _ = lhs || rhs --> if (!lhs) { _ = rhs; }
            TypedExprKind::Binary {
                op: BinaryOp::Or,
                left,
                right,
            } => {
                self.visit_expr_stack(func, locals, left.span, &left.node)?;
                assert_eq!(left.node.ty, Type::Bool);
                func.instructions().i32_eqz();
                func.instructions().if_(BlockType::Empty);
                self.visit_expr_drop(func, locals, right.span, &right.node)?;
                assert_eq!(right.node.ty, Type::Bool);
                func.instructions().end();
            }
            // Other binary operators have no control flow or side effects.
            TypedExprKind::Binary { op: _, left, right } => {
                self.visit_expr_drop(func, locals, left.span, &left.node)?;
                self.visit_expr_drop(func, locals, right.span, &right.node)?;
            }
            TypedExprKind::Grouping(spanned) => {
                self.visit_expr_drop(func, locals, spanned.span, &spanned.node)?;
            }
            TypedExprKind::StructLiteral { name: _, fields } => {
                for field in fields {
                    self.visit_expr_drop(func, locals, field.value.span, &field.value.node)?;
                }
            }
            TypedExprKind::FieldAccess { target, field: _ } => {
                self.visit_expr_drop(func, locals, target.span, &target.node)?;
            }
            TypedExprKind::EnumConstructor {
                enum_name: _,
                variant: _,
                payload,
            } => match payload {
                TypedEnumConstructorPayload::Unit => {}
                TypedEnumConstructorPayload::Tuple(fields) => {
                    for field in fields {
                        self.visit_expr_drop(func, locals, field.span, &field.node)?;
                    }
                }
                TypedEnumConstructorPayload::Struct(fields) => {
                    for field in fields {
                        self.visit_expr_drop(func, locals, field.value.span, &field.value.node)?;
                    }
                }
            },
            TypedExprKind::Block(block) => self.visit_block_drop(func, locals, block)?,
            TypedExprKind::If {
                branches,
                else_branch,
            } => {
                // Emit basic double-block and trust the optimizer.
                func.instructions().block(BlockType::Empty);
                for (condition, block) in branches {
                    // Inner block for each condition.
                    func.instructions().block(BlockType::Empty);
                    let _ = self.visit_expr_stack(func, locals, condition.span, &condition.node);
                    assert_eq!(condition.node.ty, Type::Bool);
                    func.instructions().i32_eqz(); // If condition is false,
                    func.instructions().br_if(0); // then try the next condition.
                    self.visit_block_drop(func, locals, block)?;
                    func.instructions().br(1); // Go past end.
                    func.instructions().end();
                }
                // Final `else` branch is just inline.
                if let Some(else_branch) = else_branch {
                    self.visit_block_drop(func, locals, else_branch)?;
                }
                // End.
                func.instructions().end();
            }
            // Function calls
            TypedExprKind::Call { .. }
            | TypedExprKind::Emit { .. }
            | TypedExprKind::Raise { .. }
            | TypedExprKind::Runtime { .. } => {
                // Function calls could have any side effect, so always really
                // call them then drop whatever they might have returned.
                self.visit_expr_stack(func, locals, span, expr)?;
                for _ in 0..self.star_count_core_types(&expr.ty) {
                    func.instructions().drop();
                }
            }
            TypedExprKind::Match { scrutinee, arms } => {
                self.visit_match_drop(func, locals, span, scrutinee, arms)?;
            }
        }
        Ok(())
    }

    /// Compile a single [Expr] into the current function, returning an
    /// [Intermediate] representing that expression's output on the stack.
    fn visit_expr_stack(
        &mut self,
        func: &mut Function,
        locals: &dyn Locals,
        span: Span,
        expr: &TypedExpr,
    ) -> Result<()> {
        match &expr.kind {
            // Identifiers
            TypedExprKind::Identifier(ident) => {
                if let Some(var) = locals.get(&ident.name) {
                    match var {
                        Var::Local(local) => {
                            for i in 0..self.star_count_core_types(&expr.ty) {
                                func.instructions().local_get(local + i);
                            }
                        }
                        Var::Global(global) => {
                            for i in 0..self.star_count_core_types(&expr.ty) {
                                func.instructions().global_get(global + i);
                            }
                        }
                    }
                    Ok(())
                } else {
                    Err(self.push_error(
                        ident.span_or(span),
                        format!("unknown name {:?}", &ident.name),
                    ))
                }
            }
            // Literals
            TypedExprKind::Literal(Literal::Integer(i)) => {
                func.instructions().i64_const(*i);
                assert_eq!(expr.ty, Type::Int);
                Ok(())
            }
            TypedExprKind::Literal(Literal::Boolean(b)) => {
                func.instructions().i32_const(*b as i32);
                assert_eq!(expr.ty, Type::Bool);
                Ok(())
            }
            TypedExprKind::Literal(Literal::Unit) => {
                assert_eq!(expr.ty, Type::Unit);
                Ok(())
            }
            // Arithmetic operators
            TypedExprKind::Binary {
                op: BinaryOp::Add,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        if self.options.check_overflows {
                            let checked_sum = self.get_or_create_i64_add_checked();
                            func.instructions().call(checked_sum);
                        } else {
                            func.instructions().i64_add();
                        }

                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Add({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Subtract,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        if self.options.check_overflows {
                            let checked_sub = self.get_or_create_i64_sub_checked();
                            func.instructions().call(checked_sub);
                        } else {
                            func.instructions().i64_sub();
                        }

                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Subtract({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Multiply,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        if self.options.check_overflows {
                            let checked_mul = self.get_or_create_i64_mul_checked();
                            func.instructions().call(checked_mul);
                        } else {
                            func.instructions().i64_mul();
                        }

                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Multiply({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Divide,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_div_s();
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Divide({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Remainder,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_rem_s();
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Remainder({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Unary {
                op: UnaryOp::Negate,
                expr: inner,
            } => {
                // `-x` compiles to `0 - x`.
                func.instructions().i64_const(0);
                self.visit_expr_stack(func, locals, inner.span, &inner.node)?;
                match &inner.node.ty {
                    Type::Int => {
                        func.instructions().i64_sub();
                        assert_eq!(expr.ty, Type::Int);
                        Ok(())
                    }
                    lhs => Err(self.todo(format!("Negate({lhs:?})"))),
                }
            }
            TypedExprKind::Unary {
                op: UnaryOp::Not,
                expr: inner,
            } => {
                self.visit_expr_stack(func, locals, inner.span, &inner.node)?;
                match &inner.node.ty {
                    Type::Bool => {
                        func.instructions().i32_eqz();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    lhs => Err(self.todo(format!("Not({lhs:?})"))),
                }
            }
            // Comparison operators
            TypedExprKind::Binary {
                op: BinaryOp::Equal,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_eq();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (Type::Bool, Type::Bool) => {
                        func.instructions().i32_eq();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Equal({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::NotEqual,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_ne();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (Type::Bool, Type::Bool) => {
                        func.instructions().i32_ne();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("NotEqual({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Less,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_lt_s();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (Type::Bool, Type::Bool) => {
                        func.instructions().i32_lt_u();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Less({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Greater,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_gt_s();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (Type::Bool, Type::Bool) => {
                        func.instructions().i32_gt_u();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Greater({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::LessEqual,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_le_s();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (Type::Bool, Type::Bool) => {
                        func.instructions().i32_le_u();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("LessEqual({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::GreaterEqual,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_ge_s();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (Type::Bool, Type::Bool) => {
                        func.instructions().i32_ge_u();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Greater({lhs:?}, {rhs:?})"))),
                }
            }
            // Short-circuiting operators
            TypedExprKind::Binary {
                op: BinaryOp::And,
                left,
                right,
            } => {
                self.visit_expr_stack(func, locals, left.span, &left.node)?;
                assert_eq!(left.node.ty, Type::Bool);
                func.instructions().if_(BlockType::Result(ValType::I32));
                self.visit_expr_stack(func, locals, right.span, &right.node)?;
                assert_eq!(right.node.ty, Type::Bool);
                func.instructions().else_().i32_const(0).end();
                Ok(())
            }
            TypedExprKind::Binary {
                op: BinaryOp::Or,
                left,
                right,
            } => {
                self.visit_expr_stack(func, locals, left.span, &left.node)?;
                assert_eq!(left.node.ty, Type::Bool);
                func.instructions()
                    .if_(BlockType::Result(ValType::I32))
                    .i32_const(1)
                    .else_();
                self.visit_expr_stack(func, locals, right.span, &right.node)?;
                assert_eq!(right.node.ty, Type::Bool);
                func.instructions().end();
                Ok(())
            }
            // Field access
            TypedExprKind::FieldAccess { target, field } => {
                // Right now intermediates point to the stack only, which is
                // awkward when we want to access only one field of a local.
                // This implementation is inefficient but at least it's correct.
                self.visit_expr_stack(func, locals, target.span, &target.node)?;
                match &target.node.ty {
                    Type::Record(record) => {
                        let mut offset = 0;
                        let mut total = 0;
                        let mut ty = None;
                        for f in &record.fields {
                            if f.name.as_str() == field.as_str() {
                                ty = Some(&f.ty);
                            }
                            let slots = self.star_count_core_types(&f.ty);
                            total += slots;
                            if ty.is_none() {
                                offset += slots;
                            }
                        }
                        if let Some(ty) = ty {
                            let mut new_locals = Vec::new();
                            _ = self.star_to_core_types(target.span, &mut new_locals, ty);

                            // Drop everything after what we are selecting.
                            for _ in offset + (new_locals.len() as u32)..total {
                                func.instructions().drop();
                            }

                            // If offset != 0, use locals to drop stuff before the offset.
                            if offset != 0 && !new_locals.is_empty() {
                                // Allocate local slots.
                                let first_local = func.add_locals(new_locals.iter().copied());

                                // Store.
                                for i in (0..new_locals.len()).rev() {
                                    func.instructions().local_set(first_local + (i as u32));
                                }

                                // Drop.
                                for _ in 0..offset {
                                    func.instructions().drop();
                                }

                                // Get.
                                for i in 0..new_locals.len() {
                                    func.instructions().local_get(first_local + (i as u32));
                                }
                            }

                            Ok(())
                        } else {
                            Err(self.push_error(
                                field.span_or(target.span),
                                format!(
                                    "no field {:?} on type {:?}",
                                    field.as_str(),
                                    target.node.ty
                                ),
                            ))
                        }
                    }
                    other => Err(self.push_error(
                        field.span_or(target.span),
                        format!("field access is only valid on structs, not {:?}", other),
                    )),
                }
            }
            // Nesting
            TypedExprKind::Grouping(expr) => {
                self.visit_expr_stack(func, locals, expr.span, &expr.node)
            }
            TypedExprKind::Block(block) => self.visit_block_stack(func, locals, block),
            TypedExprKind::If {
                branches,
                else_branch,
            } => {
                // Create locals to store expression result.
                let mut new_locals = Vec::new();
                self.star_to_core_types(span, &mut new_locals, &expr.ty)?;
                let first_local = func.add_locals(new_locals.iter().copied());

                // Emit basic double-block and trust the optimizer.
                func.instructions().block(BlockType::Empty);
                for (condition, block) in branches {
                    // Inner block for each condition.
                    func.instructions().block(BlockType::Empty);
                    self.visit_expr_stack(func, locals, condition.span, &condition.node)?;
                    assert_eq!(condition.node.ty, Type::Bool);
                    func.instructions().i32_eqz(); // If condition is false,
                    func.instructions().br_if(0); // then try the next condition.
                    self.visit_block_stack(func, locals, block)?;
                    for i in (0..new_locals.len()).rev() {
                        func.instructions().local_set(first_local + (i as u32));
                    }
                    func.instructions().br(1); // Go past end.
                    func.instructions().end();
                }
                // Final `else` branch is just inline.
                if let Some(else_branch) = else_branch {
                    self.visit_block_stack(func, locals, else_branch)?;
                    for i in (0..new_locals.len()).rev() {
                        func.instructions().local_set(first_local + (i as u32));
                    }
                }
                // End.
                func.instructions().end();

                // Read locals back onto stack.
                for i in 0..new_locals.len() {
                    func.instructions().local_get(first_local + (i as u32));
                }
                Ok(())
            }
            // Data constructors
            TypedExprKind::StructLiteral { name: _, fields } => {
                let Type::Record(record) = &expr.ty else {
                    panic!("StructLiteral type must be a Record");
                };
                let fields = fields
                    .iter()
                    .map(|f| (f.name.as_str(), &f.value))
                    .collect::<HashMap<_, _>>();
                // NB: currently compiles in declaration order, not the order written in source.
                for field in &record.fields {
                    let expr = fields
                        .get(field.name.as_str())
                        .expect("StructLiteral missing field");
                    self.visit_expr_stack(func, locals, expr.span, &expr.node)?;
                }
                Ok(())
            }
            TypedExprKind::EnumConstructor {
                enum_name: _,
                variant,
                payload,
            } => {
                let Type::Enum(enum_) = &expr.ty else {
                    panic!("EnumConstructor type should be Enum, got {:?}", &expr.ty);
                };

                let mut dest_types = Vec::new();
                _ = self.star_to_core_types(span, &mut dest_types, &expr.ty);
                let mut iter = dest_types.into_iter();

                // Push the discriminant
                assert_eq!(iter.next().unwrap(), ValType::I32);
                let Some((discriminant, variant_ty)) = enum_
                    .variants
                    .iter()
                    .enumerate()
                    .find(|(_, v)| v.name == variant.as_str())
                else {
                    panic!(
                        "EnumConstructor variant {} not found in {:?}",
                        variant, &expr.ty
                    );
                };
                let discriminant = u32::try_from(discriminant).unwrap();
                func.instructions().i32_const(discriminant as i32);

                // Push the values
                match payload {
                    TypedEnumConstructorPayload::Unit => {}
                    TypedEnumConstructorPayload::Tuple(fields) => {
                        for f in fields {
                            self.visit_expr_stack(func, locals, f.span, &f.node)?;
                            self.enum_promote(func, f.span, &f.node.ty, &mut iter);
                        }
                    }
                    TypedEnumConstructorPayload::Struct(fields) => {
                        let EnumVariantKind::Struct(struct_) = &variant_ty.kind else {
                            panic!(
                                "EnumConstructor struct used for non-struct variant {}",
                                variant
                            );
                        };
                        let fields = fields
                            .iter()
                            .map(|f| (f.name.as_str(), &f.value))
                            .collect::<HashMap<_, _>>();
                        // NB: currently compiles in declaration order, not the order written in source.
                        for field in struct_ {
                            let expr = fields
                                .get(field.name.as_str())
                                .expect("StructLiteral missing field");
                            self.visit_expr_stack(func, locals, expr.span, &expr.node)?;
                            self.enum_promote(func, expr.span, &expr.node.ty, &mut iter);
                        }
                    }
                }

                // Push 0 for the rest
                for ty in iter {
                    match ty {
                        ValType::I32 => func.instructions().i32_const(0),
                        ValType::I64 => func.instructions().i64_const(0),
                        ValType::F32 => func.instructions().f32_const(Ieee32::new(0)),
                        ValType::F64 => func.instructions().f64_const(Ieee64::new(0)),
                        _ => todo!(),
                    };
                }

                Ok(())
            }
            // Function calls
            TypedExprKind::Call { callee, args } => {
                let TypedExprKind::Identifier(i) = &callee.node.kind else {
                    return Err(self.todo("cannot call non-identifier".into()));
                };
                let target = *self
                    .callables
                    .get(i.as_str())
                    .expect("no callable found for identifier");
                self.visit_call(func, locals, span, target, args)
            }
            TypedExprKind::Emit { event, args } => {
                let target = *self
                    .callables
                    .get(event.as_str())
                    .expect("no callable found for identifier");
                self.visit_call(func, locals, span, target, args)
            }
            TypedExprKind::Raise { expr: inner } => {
                let TypedExprKind::Call { callee, args } = &inner.node.kind else {
                    panic!("raise expr must be a call");
                };
                let TypedExprKind::Identifier(i) = &callee.node.kind else {
                    return Err(self.todo("raise expr cannot call non-identifier".into()));
                };
                let target = *self
                    .callables
                    .get(i.as_str())
                    .expect("no callable found for identifier");
                self.visit_call(func, locals, span, target, args)
            }
            TypedExprKind::Runtime { expr: inner } => {
                let TypedExprKind::Call { callee, args } = &inner.node.kind else {
                    panic!("runtime expr must be a call");
                };
                let TypedExprKind::Identifier(i) = &callee.node.kind else {
                    return Err(self.todo("runtime expr cannot call non-identifier".into()));
                };
                let target = *self
                    .callables
                    .get(i.as_str())
                    .expect("no callable found for identifier");
                self.visit_call(func, locals, span, target, args)
            }
            TypedExprKind::Match { scrutinee, arms } => {
                self.visit_match_stack(func, locals, span, expr, scrutinee, arms)
            }
        }
    }

    fn enum_promote(
        &mut self,
        func: &mut Function,
        span: Span,
        ty: &Type,
        iter: &mut dyn Iterator<Item = ValType>,
    ) {
        // Collect from & to vectors.
        let mut from = Vec::new();
        _ = self.star_to_core_types(span, &mut from, ty);
        let to = iter.take(from.len()).collect::<Vec<_>>();

        let mut from = &from[..];
        let mut to = &to[..];

        // Erase the common prefix of from and to that don't need promoting.
        while let Some((from_first, from_rest)) = from.split_first()
            && let Some((to_first, to_rest)) = to.split_first()
            && from_first == to_first
        {
            from = from_rest;
            to = to_rest;
        }

        // Store promoted values into locals in reverse order.
        let first_local = func.add_locals(to.iter().copied());
        for (i, (&src, &dst)) in from.iter().zip(to).enumerate().rev() {
            match (src, dst) {
                (a, b) if a == b => {}
                (ValType::I32, ValType::I64) => {
                    // Do we need to distinguish i32 and u32 here?
                    func.instructions().i64_extend_i32_u();
                }
                _ => {
                    self.push_error(span, format!("Unknown promotion from {src:?} to {dst:?}"));
                }
            }
            func.instructions().local_set(first_local + (i as u32));
        }

        // Read the locals back in forward order.
        for i in 0..to.len() {
            func.instructions().local_get(first_local + (i as u32));
        }
    }

    // ------------------------------------------------------------------------
    // Match expression compilation

    /// Build the pattern matrix, compile to decision tree, emit wasm.
    /// Result is left on the stack.
    fn visit_match_stack(
        &mut self,
        func: &mut Function,
        locals: &dyn Locals,
        span: Span,
        expr: &TypedExpr,
        scrutinee: &Spanned<TypedExpr>,
        arms: &[TypedMatchArm],
    ) -> Result<()> {
        // 1. Compile scrutinee to stack and store in locals.
        self.visit_expr_stack(func, locals, scrutinee.span, &scrutinee.node)?;
        let mut scrut_types = Vec::new();
        _ = self.star_to_core_types(scrutinee.span, &mut scrut_types, &scrutinee.node.ty);
        let scrut_base = func.add_locals(scrut_types.iter().copied());
        for i in (0..scrut_types.len()).rev() {
            func.instructions().local_set(scrut_base + (i as u32));
        }

        // 2. Build the pattern matrix.
        let mut matrix_rows = Vec::new();
        for (arm_idx, arm) in arms.iter().enumerate() {
            let pat = self.lower_pattern(&arm.pattern, arm_idx, &scrutinee.node.ty);
            matrix_rows.push(Row {
                pats: vec![pat],
                action: arm_idx,
            });
        }
        let matrix = Matrix { rows: matrix_rows };
        let col_types = vec![scrutinee.node.ty.clone()];

        // 3. Compile to decision tree.
        let tree = decision_tree::compile(&matrix, &col_types);

        // 4. Allocate result locals.
        let mut result_types = Vec::new();
        _ = self.star_to_core_types(span, &mut result_types, &expr.ty);
        let result_base = func.add_locals(result_types.iter().copied());

        // 5. Emit wasm from decision tree.
        func.instructions().block(BlockType::Empty);
        self.emit_decision_tree(
            func,
            locals,
            span,
            &tree,
            &[(scrut_base, scrutinee.node.ty.clone())],
            arms,
            result_base,
            result_types.len() as u32,
            0,
        );
        func.instructions().end();

        // 6. Read result locals back to stack.
        for i in 0..result_types.len() {
            func.instructions().local_get(result_base + (i as u32));
        }
        Ok(())
    }

    /// Same as visit_match_stack but drops the result.
    fn visit_match_drop(
        &mut self,
        func: &mut Function,
        locals: &dyn Locals,
        span: Span,
        scrutinee: &Spanned<TypedExpr>,
        arms: &[TypedMatchArm],
    ) -> Result<()> {
        self.visit_expr_stack(func, locals, scrutinee.span, &scrutinee.node)?;
        let mut scrut_types = Vec::new();
        _ = self.star_to_core_types(scrutinee.span, &mut scrut_types, &scrutinee.node.ty);
        let scrut_base = func.add_locals(scrut_types.iter().copied());
        for i in (0..scrut_types.len()).rev() {
            func.instructions().local_set(scrut_base + (i as u32));
        }

        let mut matrix_rows = Vec::new();
        for (arm_idx, arm) in arms.iter().enumerate() {
            let pat = self.lower_pattern(&arm.pattern, arm_idx, &scrutinee.node.ty);
            matrix_rows.push(Row {
                pats: vec![pat],
                action: arm_idx,
            });
        }
        let matrix = Matrix { rows: matrix_rows };
        let col_types = vec![scrutinee.node.ty.clone()];
        let tree = decision_tree::compile(&matrix, &col_types);

        func.instructions().block(BlockType::Empty);
        self.emit_decision_tree(
            func,
            locals,
            span,
            &tree,
            &[(scrut_base, scrutinee.node.ty.clone())],
            arms,
            0,
            0,
            0,
        );
        func.instructions().end();
        Ok(())
    }

    /// Lower a TypedPattern into the simplified Pat for the decision tree matrix.
    /// Bindings are embedded as `Pat::Wildcard { binding: Some(...) }` so that
    /// the CC algorithm's specialization automatically tracks them through columns.
    fn lower_pattern(&mut self, pattern: &TypedPattern, arm_idx: usize, ty: &Type) -> Pat {
        match pattern {
            TypedPattern::Wildcard => Pat::Wildcard { binding: None },
            TypedPattern::Binding(ident) => Pat::Wildcard {
                binding: Some((arm_idx, ident.name.clone())),
            },
            TypedPattern::Literal(Literal::Integer(n)) => Pat::Ctor {
                ctor: Ctor::IntLiteral(*n),
                args: vec![],
            },
            TypedPattern::Literal(Literal::Boolean(b)) => Pat::Ctor {
                ctor: if *b { Ctor::BoolTrue } else { Ctor::BoolFalse },
                args: vec![],
            },
            TypedPattern::Literal(Literal::Unit) => Pat::Ctor {
                ctor: Ctor::Unit,
                args: vec![],
            },
            TypedPattern::EnumVariant {
                enum_name: _,
                variant,
                payload,
            } => {
                let Type::Enum(enum_ty) = ty else {
                    return Pat::Wildcard { binding: None };
                };
                let variant_index = enum_ty
                    .variants
                    .iter()
                    .position(|v| v.name == variant.as_str())
                    .expect("variant not found in enum type");

                let sub_pats = match payload {
                    TypedEnumPatternPayload::Unit => vec![],
                    TypedEnumPatternPayload::Tuple(pats) => {
                        let variant_ty = &enum_ty.variants[variant_index];
                        let EnumVariantKind::Tuple(field_types) = &variant_ty.kind else {
                            return Pat::Wildcard { binding: None };
                        };
                        pats.iter()
                            .zip(field_types.iter())
                            .map(|(p, ft)| self.lower_pattern(p, arm_idx, ft))
                            .collect()
                    }
                    TypedEnumPatternPayload::Struct(fields) => {
                        let variant_ty = &enum_ty.variants[variant_index];
                        let EnumVariantKind::Struct(field_defs) = &variant_ty.kind else {
                            return Pat::Wildcard { binding: None };
                        };
                        // Lower in declaration order
                        field_defs
                            .iter()
                            .map(|fd| {
                                let sf = fields
                                    .iter()
                                    .find(|sf| sf.name.as_str() == fd.name)
                                    .expect("missing field in struct pattern");
                                self.lower_pattern(&sf.pattern, arm_idx, &fd.ty)
                            })
                            .collect()
                    }
                };
                Pat::Ctor {
                    ctor: Ctor::EnumVariant { variant_index },
                    args: sub_pats,
                }
            }
            TypedPattern::Struct { name: _, fields } => {
                let Type::Record(record) = ty else {
                    return Pat::Wildcard { binding: None };
                };
                let sub_pats = record
                    .fields
                    .iter()
                    .map(|field_def| {
                        if let Some(sf) =
                            fields.iter().find(|sf| sf.name.as_str() == field_def.name)
                        {
                            self.lower_pattern(&sf.pattern, arm_idx, &field_def.ty)
                        } else {
                            Pat::Wildcard { binding: None }
                        }
                    })
                    .collect();
                Pat::Ctor {
                    ctor: Ctor::Struct,
                    args: sub_pats,
                }
            }
        }
    }

    /// Recursively emit wasm for a decision tree node.
    fn emit_decision_tree(
        &mut self,
        func: &mut Function,
        locals: &dyn Locals,
        span: Span,
        tree: &DecisionTree,
        col_locals: &[(u32, Type)],
        arms: &[TypedMatchArm],
        result_base: u32,
        result_count: u32,
        outer_depth: u32,
    ) {
        match tree {
            DecisionTree::Leaf { action, bindings } => {
                // Resolve bindings: map variable names to wasm locals via col_locals.
                let mut arm_locals = HashMap::new();
                for b in bindings {
                    if b.action == *action {
                        let (local, _ty) = &col_locals[b.column];
                        arm_locals.insert(b.name.clone(), Var::Local(*local));
                    }
                }

                if result_count > 0 {
                    let _ =
                        self.visit_block_stack(func, &(locals, &arm_locals), &arms[*action].body);
                    for i in (0..result_count).rev() {
                        func.instructions().local_set(result_base + i);
                    }
                } else {
                    let _ =
                        self.visit_block_drop(func, &(locals, &arm_locals), &arms[*action].body);
                }
                func.instructions().br(outer_depth);
            }
            DecisionTree::Fail => {
                func.instructions().unreachable();
            }
            DecisionTree::Switch {
                column,
                cases,
                default,
            } => {
                let (base_local, col_type) = &col_locals[*column];
                let base_local = *base_local;
                match col_type {
                    Type::Enum(enum_ty) => {
                        self.emit_enum_switch(
                            func,
                            locals,
                            span,
                            *column,
                            base_local,
                            enum_ty,
                            col_type,
                            cases,
                            default,
                            col_locals,
                            arms,
                            result_base,
                            result_count,
                            outer_depth,
                        );
                    }
                    Type::Bool => {
                        self.emit_simple_switch(
                            func,
                            locals,
                            span,
                            *column,
                            base_local,
                            cases,
                            default,
                            col_locals,
                            arms,
                            result_base,
                            result_count,
                            outer_depth,
                            |func_inst, ctor, base| match ctor {
                                Ctor::BoolTrue => {
                                    func_inst.local_get(base);
                                    func_inst.i32_eqz();
                                    func_inst.br_if(0);
                                }
                                Ctor::BoolFalse => {
                                    func_inst.local_get(base);
                                    func_inst.br_if(0);
                                }
                                _ => {}
                            },
                        );
                    }
                    Type::Int => {
                        self.emit_simple_switch(
                            func,
                            locals,
                            span,
                            *column,
                            base_local,
                            cases,
                            default,
                            col_locals,
                            arms,
                            result_base,
                            result_count,
                            outer_depth,
                            |func_inst, ctor, base| {
                                if let Ctor::IntLiteral(n) = ctor {
                                    func_inst.local_get(base);
                                    func_inst.i64_const(*n);
                                    func_inst.i64_ne();
                                    func_inst.br_if(0);
                                }
                            },
                        );
                    }
                    Type::Record(_) => {
                        if let Some((ctor, subtree)) = cases.first() {
                            let field_types = ctor.field_types(col_type);
                            let new_col_locals = self.expand_col_locals_for_struct(
                                col_locals,
                                *column,
                                base_local,
                                &field_types,
                            );
                            self.emit_decision_tree(
                                func,
                                locals,
                                span,
                                subtree,
                                &new_col_locals,
                                arms,
                                result_base,
                                result_count,
                                outer_depth,
                            );
                        }
                    }
                    Type::Unit => {
                        if let Some((_, subtree)) = cases.first() {
                            let new_col_locals = remove_col(col_locals, *column);
                            self.emit_decision_tree(
                                func,
                                locals,
                                span,
                                subtree,
                                &new_col_locals,
                                arms,
                                result_base,
                                result_count,
                                outer_depth,
                            );
                        }
                    }
                    _ => {
                        func.instructions().unreachable();
                    }
                }
            }
        }
    }

    fn emit_enum_switch(
        &mut self,
        func: &mut Function,
        locals: &dyn Locals,
        span: Span,
        column: usize,
        base_local: u32,
        enum_ty: &EnumType,
        col_type: &Type,
        cases: &[(Ctor, DecisionTree)],
        default: &Option<Box<DecisionTree>>,
        col_locals: &[(u32, Type)],
        arms: &[TypedMatchArm],
        result_base: u32,
        result_count: u32,
        outer_depth: u32,
    ) {
        let mut enum_core_types = Vec::new();
        _ = self.star_to_core_types(span, &mut enum_core_types, col_type);

        for (ctor, subtree) in cases {
            let Ctor::EnumVariant { variant_index } = ctor else {
                continue;
            };
            func.instructions().block(BlockType::Empty);

            // Test discriminant
            func.instructions().local_get(base_local);
            func.instructions().i32_const(*variant_index as i32);
            func.instructions().i32_ne();
            func.instructions().br_if(0);

            // Demote payload fields
            let variant_ty = &enum_ty.variants[*variant_index];
            let field_types = match &variant_ty.kind {
                EnumVariantKind::Unit => vec![],
                EnumVariantKind::Tuple(ft) => ft.clone(),
                EnumVariantKind::Struct(ft) => ft.iter().map(|f| f.ty.clone()).collect(),
            };

            let new_col_locals = self.demote_enum_fields(
                func,
                span,
                column,
                base_local,
                &enum_core_types,
                &field_types,
                col_locals,
            );

            self.emit_decision_tree(
                func,
                locals,
                span,
                subtree,
                &new_col_locals,
                arms,
                result_base,
                result_count,
                outer_depth + 1,
            );

            func.instructions().end();
        }

        if let Some(def) = default {
            let new_col_locals = remove_col(col_locals, column);
            self.emit_decision_tree(
                func,
                locals,
                span,
                def,
                &new_col_locals,
                arms,
                result_base,
                result_count,
                outer_depth,
            );
        } else {
            func.instructions().unreachable();
        }
    }

    /// Demote enum fields from joined slots to actual field types,
    /// allocating fresh locals and emitting conversion instructions.
    fn demote_enum_fields(
        &mut self,
        func: &mut Function,
        span: Span,
        column: usize,
        enum_base_local: u32,
        enum_core_types: &[ValType],
        field_types: &[Type],
        col_locals: &[(u32, Type)],
    ) -> Vec<(u32, Type)> {
        let mut new_entries = Vec::new();
        let mut slot_offset = 1u32; // skip discriminant

        for field_ty in field_types {
            let mut field_core = Vec::new();
            _ = self.star_to_core_types(span, &mut field_core, field_ty);
            let demoted_base = func.add_locals(field_core.iter().copied());

            for (j, &field_vt) in field_core.iter().enumerate() {
                let slot_idx = slot_offset + j as u32;
                if slot_idx < enum_core_types.len() as u32 {
                    let joined_vt = enum_core_types[slot_idx as usize];
                    func.instructions().local_get(enum_base_local + slot_idx);
                    if joined_vt != field_vt
                        && let (ValType::I64, ValType::I32) = (joined_vt, field_vt)
                    {
                        func.instructions().i32_wrap_i64();
                    }
                    func.instructions().local_set(demoted_base + j as u32);
                }
            }

            new_entries.push((demoted_base, field_ty.clone()));
            slot_offset += field_core.len() as u32;
        }

        let mut result = Vec::new();
        result.extend_from_slice(&col_locals[..column]);
        result.extend(new_entries);
        result.extend_from_slice(&col_locals[column + 1..]);
        result
    }

    /// Emit a switch for simple (non-enum) types: bool, int.
    /// `emit_test` emits a br_if(0) that skips the case if the constructor doesn't match.
    fn emit_simple_switch(
        &mut self,
        func: &mut Function,
        locals: &dyn Locals,
        span: Span,
        column: usize,
        base_local: u32,
        cases: &[(Ctor, DecisionTree)],
        default: &Option<Box<DecisionTree>>,
        col_locals: &[(u32, Type)],
        arms: &[TypedMatchArm],
        result_base: u32,
        result_count: u32,
        outer_depth: u32,
        emit_test: impl Fn(&mut InstructionSink<'_>, &Ctor, u32),
    ) {
        let new_col_locals = remove_col(col_locals, column);

        for (ctor, subtree) in cases {
            func.instructions().block(BlockType::Empty);
            emit_test(&mut func.instructions(), ctor, base_local);
            self.emit_decision_tree(
                func,
                locals,
                span,
                subtree,
                &new_col_locals,
                arms,
                result_base,
                result_count,
                outer_depth + 1,
            );
            func.instructions().end();
        }

        if let Some(def) = default {
            self.emit_decision_tree(
                func,
                locals,
                span,
                def,
                &new_col_locals,
                arms,
                result_base,
                result_count,
                outer_depth,
            );
        } else {
            func.instructions().unreachable();
        }
    }

    /// Expand col_locals for a struct match: replace the struct column with field columns.
    fn expand_col_locals_for_struct(
        &mut self,
        col_locals: &[(u32, Type)],
        column: usize,
        base_local: u32,
        field_types: &[Type],
    ) -> Vec<(u32, Type)> {
        let mut result = Vec::new();
        result.extend_from_slice(&col_locals[..column]);
        let mut offset = base_local;
        for ft in field_types {
            let count = self.star_count_core_types(ft);
            result.push((offset, ft.clone()));
            offset += count;
        }
        result.extend_from_slice(&col_locals[column + 1..]);
        result
    }

    fn visit_call(
        &mut self,
        func: &mut Function,
        locals: &dyn Locals,
        span: Span,
        core_fn_idx: u32,
        args: &[Spanned<TypedExpr>],
    ) -> Result<()> {
        let _ = span;
        for arg in args {
            self.visit_expr_stack(func, locals, arg.span, &arg.node)?;
        }
        func.instructions().call(core_fn_idx);
        Ok(())
    }
}

#[derive(Clone, Copy)]
enum Var {
    Local(u32),
    Global(u32),
}

// Probably inefficient, but fun. Fix later?
trait Locals {
    fn get(&self, name: &str) -> Option<Var>;
}

impl Locals for () {
    fn get(&self, _: &str) -> Option<Var> {
        None
    }
}

impl Locals for (&dyn Locals, &HashMap<String, Var>) {
    fn get(&self, name: &str) -> Option<Var> {
        match self.1.get(name) {
            Some(v) => Some(*v),
            None => self.0.get(name),
        }
    }
}

/// A replacement for [wasm_encoder::Function] that allows adding locals gradually.
///
/// Bytecode can be encoded to the return value of [Function::instructions].
#[derive(Default)]
struct Function {
    num_locals: u32,
    locals: Vec<(u32, ValType)>,
    bytes: Vec<u8>,
}

impl Function {
    fn from_params(params: &[ValType]) -> Function {
        Function {
            num_locals: u32::try_from(params.len()).unwrap(),
            ..Function::default()
        }
    }

    fn add_locals(&mut self, types: impl IntoIterator<Item = ValType>) -> u32 {
        let id = self.num_locals;
        for ty in types {
            self.num_locals += 1;
            if let Some((last_count, last_type)) = self.locals.last_mut()
                && ty == *last_type
            {
                *last_count += 1;
            } else {
                self.locals.push((1, ty));
            }
        }
        id
    }

    fn instructions(&mut self) -> InstructionSink<'_> {
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

/// Remove column `col` from a col_locals slice.
fn remove_col(col_locals: &[(u32, Type)], col: usize) -> Vec<(u32, Type)> {
    let mut result = Vec::with_capacity(col_locals.len() - 1);
    result.extend_from_slice(&col_locals[..col]);
    result.extend_from_slice(&col_locals[col + 1..]);
    result
}

fn to_kebab_case(name: &str) -> String {
    // ^A -> a
    // AB -> ab
    // AbC -> ab-c
    // Ab2 -> ab2
    let mut out = String::with_capacity(name.len());
    let mut prev: Option<char> = None;
    for ch in name.chars() {
        if ch.is_ascii_uppercase() {
            if let Some(p) = prev
                && p.is_ascii_lowercase()
            {
                out.push('-');
            }
            out.push(ch.to_ascii_lowercase());
        } else if ch.is_ascii_alphanumeric() {
            if ch.is_numeric() {
                // WIT doesn't permit segments to start with numbers.
                out.truncate(out.trim_end_matches('-').len());
                if out.is_empty() {
                    out.push('x');
                }
            }
            out.push(ch);
        } else if ch == '_'
            && let Some(p) = prev
            && p != '_'
        {
            out.push('-');
        }
        prev = Some(ch);
    }
    out.truncate(out.trim_end_matches('-').len());
    out
}
