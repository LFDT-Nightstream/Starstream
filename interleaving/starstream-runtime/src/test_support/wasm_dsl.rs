#![allow(dead_code)]

use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, GlobalSection, GlobalType, ImportSection, Instruction, Module, TypeSection,
    ValType,
};

#[derive(Clone, Copy, Debug)]
pub struct Local(u32);

#[derive(Clone, Copy, Debug)]
pub struct FuncRef {
    pub idx: u32,
    pub results: usize,
}

#[derive(Clone, Copy, Debug)]
pub enum Value {
    Local(Local),
    Const(i64),
}

impl Value {
    fn emit(self, instrs: &mut Vec<Instruction<'static>>) {
        match self {
            Value::Local(Local(idx)) => instrs.push(Instruction::LocalGet(idx)),
            Value::Const(value) => instrs.push(Instruction::I64Const(value)),
        }
    }
}

pub struct FuncBuilder {
    locals: Vec<ValType>,
    instrs: Vec<Instruction<'static>>,
}

impl FuncBuilder {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            instrs: Vec::new(),
        }
    }

    pub fn local_i64(&mut self) -> Local {
        let idx = self.locals.len() as u32;
        self.locals.push(ValType::I64);
        Local(idx)
    }

    pub fn set_const(&mut self, dst: Local, value: i64) {
        self.instrs.push(Instruction::I64Const(value));
        self.instrs.push(Instruction::LocalSet(dst.0));
    }

    pub fn set_local(&mut self, dst: Local, src: Local) {
        self.instrs.push(Instruction::LocalGet(src.0));
        self.instrs.push(Instruction::LocalSet(dst.0));
    }

    pub fn call(&mut self, func: FuncRef, args: Vec<Value>, results: &[Local]) {
        for arg in args {
            arg.emit(&mut self.instrs);
        }
        self.instrs.push(Instruction::Call(func.idx));
        for local in results.iter().rev() {
            self.instrs.push(Instruction::LocalSet(local.0));
        }
    }

    pub fn assert_eq(&mut self, lhs: Value, rhs: Value) {
        lhs.emit(&mut self.instrs);
        rhs.emit(&mut self.instrs);
        self.instrs.push(Instruction::I64Ne);
        self.instrs.push(Instruction::If(BlockType::Empty));
        self.instrs.push(Instruction::Unreachable);
        self.instrs.push(Instruction::End);
    }

    pub fn add_i64(&mut self, a: Value, b: Value, dst: Local) {
        a.emit(&mut self.instrs);
        b.emit(&mut self.instrs);
        self.instrs.push(Instruction::I64Add);
        self.instrs.push(Instruction::LocalSet(dst.0));
    }

    pub fn sub_i64(&mut self, a: Value, b: Value, dst: Local) {
        a.emit(&mut self.instrs);
        b.emit(&mut self.instrs);
        self.instrs.push(Instruction::I64Sub);
        self.instrs.push(Instruction::LocalSet(dst.0));
    }

    pub fn mul_i64(&mut self, a: Value, b: Value, dst: Local) {
        a.emit(&mut self.instrs);
        b.emit(&mut self.instrs);
        self.instrs.push(Instruction::I64Mul);
        self.instrs.push(Instruction::LocalSet(dst.0));
    }

    pub fn div_i64(&mut self, a: Value, b: Value, dst: Local) {
        a.emit(&mut self.instrs);
        b.emit(&mut self.instrs);
        self.instrs.push(Instruction::I64DivS);
        self.instrs.push(Instruction::LocalSet(dst.0));
    }

    pub fn global_get(&mut self, global: u32, dst: Local) {
        self.instrs.push(Instruction::GlobalGet(global));
        self.instrs.push(Instruction::LocalSet(dst.0));
    }

    pub fn global_set(&mut self, global: u32, val: Value) {
        val.emit(&mut self.instrs);
        self.instrs.push(Instruction::GlobalSet(global));
    }

    pub fn emit_eq_i64(&mut self, a: Value, b: Value) {
        a.emit(&mut self.instrs);
        b.emit(&mut self.instrs);
        self.instrs.push(Instruction::I64Eq);
    }

    pub fn emit_lt_i64(&mut self, a: Value, b: Value) {
        a.emit(&mut self.instrs);
        b.emit(&mut self.instrs);
        self.instrs.push(Instruction::I64LtS);
    }

    pub fn loop_begin(&mut self) {
        // block { loop { ... } } so depth 1 is break and depth 0 is continue.
        self.instrs.push(Instruction::Block(BlockType::Empty));
        self.instrs.push(Instruction::Loop(BlockType::Empty));
    }

    pub fn loop_end(&mut self) {
        self.instrs.push(Instruction::End);
        self.instrs.push(Instruction::End);
    }

    pub fn br(&mut self, depth: u32) {
        self.instrs.push(Instruction::Br(depth));
    }

    pub fn br_if(&mut self, depth: u32) {
        self.instrs.push(Instruction::BrIf(depth));
    }

    pub fn if_eq<F>(&mut self, lhs: Value, rhs: Value, f: F)
    where
        F: FnOnce(&mut FuncBuilder),
    {
        lhs.emit(&mut self.instrs);
        rhs.emit(&mut self.instrs);
        self.instrs.push(Instruction::I64Eq);
        self.instrs.push(Instruction::If(BlockType::Empty));
        f(self);
        self.instrs.push(Instruction::End);
    }

    fn finish(self) -> Function {
        let mut groups: Vec<(u32, ValType)> = Vec::new();
        for ty in self.locals {
            if let Some((count, last_ty)) = groups.last_mut()
                && *last_ty == ty
            {
                *count += 1;
                continue;
            }
            groups.push((1, ty));
        }
        let mut func = Function::new(groups);
        for instr in self.instrs {
            func.instruction(&instr);
        }
        func.instruction(&Instruction::End);
        func
    }
}

impl Default for FuncBuilder {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ModuleBuilder {
    types: TypeSection,
    imports: ImportSection,
    functions: FunctionSection,
    codes: CodeSection,
    exports: ExportSection,
    globals: GlobalSection,
    type_count: u32,
    import_count: u32,
    starstream: Option<Imports>,
}

#[derive(Clone, Copy, Debug)]
pub struct Imports {
    pub activation: FuncRef,
    pub get_program_hash: FuncRef,
    pub get_handler_for: FuncRef,
    pub install_handler: FuncRef,
    pub uninstall_handler: FuncRef,
    pub new_ref: FuncRef,
    pub ref_push: FuncRef,
    pub ref_get: FuncRef,
    pub ref_write: FuncRef,
    pub resume: FuncRef,
    pub yield_: FuncRef,
    pub return_: FuncRef,
    pub new_utxo: FuncRef,
    pub new_coord: FuncRef,
    pub burn: FuncRef,
    pub bind: FuncRef,
    pub unbind: FuncRef,
    pub init: FuncRef,
}

impl ModuleBuilder {
    pub fn new() -> Self {
        let mut builder = Self {
            types: TypeSection::new(),
            imports: ImportSection::new(),
            functions: FunctionSection::new(),
            codes: CodeSection::new(),
            exports: ExportSection::new(),
            globals: GlobalSection::new(),
            type_count: 0,
            import_count: 0,
            starstream: None,
        };
        let imports = builder.import_starstream();
        builder.starstream = Some(imports);
        builder
    }

    pub fn starstream(&self) -> Imports {
        self.starstream.expect("starstream imports available")
    }

    pub fn import_starstream(&mut self) -> Imports {
        let activation = self.import_func(
            "env",
            "starstream_activation",
            &[],
            &[ValType::I64, ValType::I64],
        );
        let get_program_hash = self.import_func(
            "env",
            "starstream_get_program_hash",
            &[ValType::I64],
            &[ValType::I64, ValType::I64, ValType::I64, ValType::I64],
        );
        let get_handler_for = self.import_func(
            "env",
            "starstream_get_handler_for",
            &[ValType::I64, ValType::I64, ValType::I64, ValType::I64],
            &[ValType::I64],
        );
        let install_handler = self.import_func(
            "env",
            "starstream_install_handler",
            &[ValType::I64, ValType::I64, ValType::I64, ValType::I64],
            &[],
        );
        let uninstall_handler = self.import_func(
            "env",
            "starstream_uninstall_handler",
            &[ValType::I64, ValType::I64, ValType::I64, ValType::I64],
            &[],
        );
        let new_ref = self.import_func(
            "env",
            "starstream_new_ref",
            &[ValType::I64],
            &[ValType::I64],
        );
        let ref_push = self.import_func(
            "env",
            "starstream_ref_push",
            &[ValType::I64, ValType::I64, ValType::I64, ValType::I64],
            &[],
        );
        let ref_get = self.import_func(
            "env",
            "starstream_ref_get",
            &[ValType::I64, ValType::I64],
            &[ValType::I64, ValType::I64, ValType::I64, ValType::I64],
        );
        let ref_write = self.import_func(
            "env",
            "starstream_ref_write",
            &[
                ValType::I64,
                ValType::I64,
                ValType::I64,
                ValType::I64,
                ValType::I64,
                ValType::I64,
            ],
            &[],
        );
        let resume = self.import_func(
            "env",
            "starstream_resume",
            &[ValType::I64, ValType::I64],
            &[ValType::I64, ValType::I64],
        );
        let yield_ = self.import_func("env", "starstream_yield", &[ValType::I64], &[]);
        let return_ = self.import_func("env", "starstream_return", &[], &[]);
        let new_utxo = self.import_func(
            "env",
            "starstream_new_utxo",
            &[
                ValType::I64,
                ValType::I64,
                ValType::I64,
                ValType::I64,
                ValType::I64,
            ],
            &[ValType::I64],
        );
        let new_coord = self.import_func(
            "env",
            "starstream_new_coord",
            &[
                ValType::I64,
                ValType::I64,
                ValType::I64,
                ValType::I64,
                ValType::I64,
            ],
            &[ValType::I64],
        );
        let burn = self.import_func("env", "starstream_burn", &[ValType::I64], &[]);
        let bind = self.import_func("env", "starstream_bind", &[ValType::I64], &[]);
        let unbind = self.import_func("env", "starstream_unbind", &[ValType::I64], &[]);
        let init = self.import_func("env", "starstream_init", &[], &[ValType::I64, ValType::I64]);

        Imports {
            activation,
            get_program_hash,
            get_handler_for,
            install_handler,
            uninstall_handler,
            new_ref,
            ref_push,
            ref_get,
            ref_write,
            resume,
            yield_,
            return_,
            new_utxo,
            new_coord,
            burn,
            bind,
            unbind,
            init,
        }
    }

    pub fn import_func(
        &mut self,
        module: &str,
        name: &str,
        params: &[ValType],
        results: &[ValType],
    ) -> FuncRef {
        let type_idx = self.type_count;
        self.type_count += 1;
        self.types
            .ty()
            .function(params.iter().copied(), results.iter().copied());
        self.imports
            .import(module, name, EntityType::Function(type_idx));
        let idx = self.import_count;
        self.import_count += 1;
        FuncRef {
            idx,
            results: results.len(),
        }
    }

    pub fn func(&self) -> FuncBuilder {
        FuncBuilder::new()
    }

    pub fn add_global_i64(&mut self, initial: i64, mutable: bool) -> u32 {
        let global_type = GlobalType {
            val_type: ValType::I64,
            mutable,
            shared: false,
        };
        self.globals
            .global(global_type, &ConstExpr::i64_const(initial));
        let idx = self.globals.len() - 1;
        let name = format!("__global_{}", idx);
        self.exports.export(&name, ExportKind::Global, idx);
        idx
    }

    pub fn finish(mut self, func: FuncBuilder) -> Vec<u8> {
        let type_idx = self.type_count;
        self.type_count += 1;
        self.types.ty().function([], []);
        self.functions.function(type_idx);
        self.codes.function(&func.finish());
        let start_idx = self.import_count;
        self.exports.export("_start", ExportKind::Func, start_idx);

        let mut module = Module::new();
        module.section(&self.types);
        module.section(&self.imports);
        module.section(&self.functions);
        if !self.globals.is_empty() {
            module.section(&self.globals);
        }
        module.section(&self.exports);
        module.section(&self.codes);
        module.finish()
    }
}

impl Default for ModuleBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[macro_export]
macro_rules! wasm_module {
    ({ $($body:tt)* }) => {{
        let mut __builder = $crate::test_support::wasm_dsl::ModuleBuilder::new();
        let __imports = __builder.starstream();
        let mut __func = __builder.func();
        $crate::wasm!(__func, __imports, { $($body)* });
        __builder.finish(__func)
    }};
    ($builder:expr, { $($body:tt)* }) => {{
        let __imports = $builder.starstream();
        let mut __func = $builder.func();
        $crate::wasm!(__func, __imports, { $($body)* });
        $builder.finish(__func)
    }};
}

#[macro_export]
macro_rules! wasm_value {
    (const($expr:expr)) => {
        $crate::test_support::wasm_dsl::Value::Const($expr as i64)
    };
    ($lit:literal) => {
        $crate::test_support::wasm_dsl::Value::Const($lit)
    };
    ($var:ident) => {
        $crate::test_support::wasm_dsl::Value::Local($var)
    };
}

#[macro_export]
macro_rules! wasm_args {
    () => {
        Vec::<$crate::test_support::wasm_dsl::Value>::new()
    };
    ($($arg:tt)+) => {{
        let mut args = Vec::<$crate::test_support::wasm_dsl::Value>::new();
        $crate::wasm_args_push!(args, $($arg)+);
        args
    }};
}

#[macro_export]
macro_rules! wasm_args_push {
    ($args:ident,) => {};
    ($args:ident, const($expr:expr) $(, $($rest:tt)*)?) => {{
        $args.push($crate::test_support::wasm_dsl::Value::Const($expr as i64));
        $( $crate::wasm_args_push!($args, $($rest)*); )?
    }};
    ($args:ident, $lit:literal $(, $($rest:tt)*)?) => {{
        $args.push($crate::test_support::wasm_dsl::Value::Const($lit));
        $( $crate::wasm_args_push!($args, $($rest)*); )?
    }};
    ($args:ident, $var:ident $(, $($rest:tt)*)?) => {{
        $args.push($crate::test_support::wasm_dsl::Value::Local($var));
        $( $crate::wasm_args_push!($args, $($rest)*); )?
    }};
}

#[macro_export]
macro_rules! wasm {
    ($f:ident, $imports:ident, { $($body:tt)* }) => {
        $crate::wasm_stmt!($f, $imports, $($body)*);
    };
}

#[macro_export]
macro_rules! wasm_repeat {
    ($f:ident, $imports:ident, 0, { $($body:tt)* }) => {};
    ($f:ident, $imports:ident, 1, { $($body:tt)* }) => {
        $crate::wasm_stmt!($f, $imports, $($body)*);
    };
    ($f:ident, $imports:ident, 2, { $($body:tt)* }) => {
        $crate::wasm_stmt!($f, $imports, $($body)*);
        $crate::wasm_stmt!($f, $imports, $($body)*);
    };
    ($f:ident, $imports:ident, 3, { $($body:tt)* }) => {
        $crate::wasm_stmt!($f, $imports, $($body)*);
        $crate::wasm_stmt!($f, $imports, $($body)*);
        $crate::wasm_stmt!($f, $imports, $($body)*);
    };
    ($f:ident, $imports:ident, 4, { $($body:tt)* }) => {
        $crate::wasm_stmt!($f, $imports, $($body)*);
        $crate::wasm_stmt!($f, $imports, $($body)*);
        $crate::wasm_stmt!($f, $imports, $($body)*);
        $crate::wasm_stmt!($f, $imports, $($body)*);
    };
    ($f:ident, $imports:ident, 5, { $($body:tt)* }) => {
        $crate::wasm_stmt!($f, $imports, $($body)*);
        $crate::wasm_stmt!($f, $imports, $($body)*);
        $crate::wasm_stmt!($f, $imports, $($body)*);
        $crate::wasm_stmt!($f, $imports, $($body)*);
        $crate::wasm_stmt!($f, $imports, $($body)*);
    };
}

#[macro_export]
macro_rules! wasm_stmt {
    ($f:ident, $imports:ident,) => {};

    ($f:ident, $imports:ident, repeat $n:literal { $($body:tt)* } $($rest:tt)*) => {
        $crate::wasm_repeat!($f, $imports, $n, { $($body)* });
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    // Structured loop: `block { loop { ... } }` so depth 1 is "break" and depth 0 is "continue".
    ($f:ident, $imports:ident, loop { $($body:tt)* } $($rest:tt)*) => {
        $f.loop_begin();
        $crate::wasm_stmt!($f, $imports, $($body)*);
        $f.loop_end();
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    // Loop control (valid inside the `loop { ... }` form above).
    ($f:ident, $imports:ident, break; $($rest:tt)*) => {
        $f.br(1);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, continue; $($rest:tt)*) => {
        $f.br(0);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, break_if $a:tt == $b:tt; $($rest:tt)*) => {
        $f.emit_eq_i64($crate::wasm_value!($a), $crate::wasm_value!($b));
        $f.br_if(1);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, continue_if $a:tt == $b:tt; $($rest:tt)*) => {
        $f.emit_eq_i64($crate::wasm_value!($a), $crate::wasm_value!($b));
        $f.br_if(0);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, break_if $a:tt < $b:tt; $($rest:tt)*) => {
        $f.emit_lt_i64($crate::wasm_value!($a), $crate::wasm_value!($b));
        $f.br_if(1);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, continue_if $a:tt < $b:tt; $($rest:tt)*) => {
        $f.emit_lt_i64($crate::wasm_value!($a), $crate::wasm_value!($b));
        $f.br_if(0);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    // Assignment (must target an existing local).
    ($f:ident, $imports:ident, set $var:ident = const($expr:expr); $($rest:tt)*) => {
        $f.set_const($var, $expr as i64);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, set $var:ident = $lit:literal; $($rest:tt)*) => {
        $f.set_const($var, $lit);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, set $var:ident = $src:ident; $($rest:tt)*) => {
        $f.set_local($var, $src);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, set $var:ident = add $a:tt, $b:tt; $($rest:tt)*) => {
        $f.add_i64($crate::wasm_value!($a), $crate::wasm_value!($b), $var);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, set $var:ident = sub $a:tt, $b:tt; $($rest:tt)*) => {
        $f.sub_i64($crate::wasm_value!($a), $crate::wasm_value!($b), $var);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, set $var:ident = mul $a:tt, $b:tt; $($rest:tt)*) => {
        $f.mul_i64($crate::wasm_value!($a), $crate::wasm_value!($b), $var);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, set $var:ident = div $a:tt, $b:tt; $($rest:tt)*) => {
        $f.div_i64($crate::wasm_value!($a), $crate::wasm_value!($b), $var);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, set ($($var:ident),+ $(,)?) = call $func:ident ( $($arg:tt)* ); $($rest:tt)*) => {
        $f.call($imports.$func, $crate::wasm_args!($($arg)*), &[$($var),+]);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, set $var:ident = call $func:ident ( $($arg:tt)* ); $($rest:tt)*) => {
        $f.call($imports.$func, $crate::wasm_args!($($arg)*), &[$var]);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, let $var:ident = const($expr:expr); $($rest:tt)*) => {
        let $var = $f.local_i64();
        $f.set_const($var, $expr as i64);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, let $var:ident = $lit:literal; $($rest:tt)*) => {
        let $var = $f.local_i64();
        $f.set_const($var, $lit);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, let $var:ident = $src:ident; $($rest:tt)*) => {
        let $var = $f.local_i64();
        $f.set_local($var, $src);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, let $var:ident = add $a:tt, $b:tt; $($rest:tt)*) => {
        let $var = $f.local_i64();
        $f.add_i64($crate::wasm_value!($a), $crate::wasm_value!($b), $var);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, let $var:ident = sub $a:tt, $b:tt; $($rest:tt)*) => {
        let $var = $f.local_i64();
        $f.sub_i64($crate::wasm_value!($a), $crate::wasm_value!($b), $var);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, let $var:ident = mul $a:tt, $b:tt; $($rest:tt)*) => {
        let $var = $f.local_i64();
        $f.mul_i64($crate::wasm_value!($a), $crate::wasm_value!($b), $var);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, let $var:ident = div $a:tt, $b:tt; $($rest:tt)*) => {
        let $var = $f.local_i64();
        $f.div_i64($crate::wasm_value!($a), $crate::wasm_value!($b), $var);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, let $var:ident = global_get $idx:literal; $($rest:tt)*) => {
        let $var = $f.local_i64();
        $f.global_get($idx as u32, $var);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, set_global $idx:literal = $val:tt; $($rest:tt)*) => {
        $f.global_set($idx as u32, $crate::wasm_value!($val));
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, let ($($var:ident),+ $(,)?) = call $func:ident ( $($arg:tt)* ); $($rest:tt)*) => {
        $(let $var = $f.local_i64();)+
        $f.call($imports.$func, $crate::wasm_args!($($arg)*), &[$($var),+]);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, let $var:ident = call $func:ident ( $($arg:tt)* ); $($rest:tt)*) => {
        let $var = $f.local_i64();
        $f.call($imports.$func, $crate::wasm_args!($($arg)*), &[$var]);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, call $func:ident ( $($arg:tt)* ); $($rest:tt)*) => {
        $f.call($imports.$func, $crate::wasm_args!($($arg)*), &[]);
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, assert_eq $lhs:ident, $rhs:tt; $($rest:tt)*) => {
        $f.assert_eq($crate::wasm_value!($lhs), $crate::wasm_value!($rhs));
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };

    ($f:ident, $imports:ident, if $lhs:ident == $rhs:tt { $($body:tt)* } $($rest:tt)*) => {
        $f.if_eq($crate::wasm_value!($lhs), $crate::wasm_value!($rhs), |$f| {
            $crate::wasm_stmt!($f, $imports, $($body)*);
        });
        $crate::wasm_stmt!($f, $imports, $($rest)*);
    };
}
