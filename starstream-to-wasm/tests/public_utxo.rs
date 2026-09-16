use starstream_compiler::TypecheckOptions;
use wasmtime::{Engine, Linker, Module, Store};

#[test]
fn public_methods_run_and_are_advertised_at_each_yield() {
    let source = r#"
        abi Extra { fn extra(); }
        utxo Counter {
            storage { let mut count: i64; }
            main fn new() {
                count = 10;
                yield();
                count = count + 100;
                yield(Extra);
                count = count + 1000;
                yield();
            }
            main fn other() {
                count = 40;
                if (true) { yield(); } else { yield(Extra); }
            }
            pub fn add(x: i64) -> i64 { count = disclose(count + x); count }
            pub fn advance() { resume; }
            impl Extra { fn extra() {} }
        }
        script fn chained() -> i64 { Counter::new().add(5) }
    "#;
    let parsed = starstream_compiler::parse_program(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let typed = starstream_compiler::typecheck_program(
        &parsed.into_program().unwrap(),
        TypecheckOptions::default(),
    )
    .unwrap();
    let compiled = starstream_to_wasm::compile(&typed.program);
    assert!(compiled.errors.is_empty(), "{:?}", compiled.errors);
    let engine = Engine::default();
    let module = Module::new(&engine, compiled.wasm.unwrap()).unwrap();
    let mut linker = Linker::<Vec<[i64; 4]>>::new(&engine);
    linker
        .func_wrap("[export]counter", "[resource-new]utxo", |value: i32| value)
        .unwrap();
    linker
        .func_wrap(
            "starstream:std/utxo-context",
            "[method]utxo-context.implements-method",
            |mut caller: wasmtime::Caller<'_, Vec<[i64; 4]>>,
             _context: i32,
             a: i64,
             b: i64,
             c: i64,
             d: i64| {
                caller.data_mut().push([a, b, c, d]);
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "starstream:std/utxo-context",
            "[method]utxo-context.resume",
            |_context: i32| {},
        )
        .unwrap();
    // Model the component linker forwarding calls on concrete handles back
    // to the exported UTXO methods, including constructor-call chaining.
    linker
        .func_wrap(
            "starstream:self/counter",
            "[static]utxo.new",
            |mut caller: wasmtime::Caller<'_, Vec<[i64; 4]>>| -> wasmtime::Result<i32> {
                let function = caller
                    .get_export("counter#[static]utxo.new")
                    .unwrap()
                    .into_func()
                    .unwrap();
                function.typed::<i32, i32>(&caller)?.call(&mut caller, 0)
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "starstream:self/counter",
            "[method]utxo.add",
            |mut caller: wasmtime::Caller<'_, Vec<[i64; 4]>>,
             handle: i32,
             value: i64|
             -> wasmtime::Result<i64> {
                let function = caller
                    .get_export("counter#[method]utxo.add")
                    .unwrap()
                    .into_func()
                    .unwrap();
                function
                    .typed::<(i32, i64), i64>(&caller)?
                    .call(&mut caller, (handle, value))
            },
        )
        .unwrap();
    linker.define_unknown_imports_as_traps(&module).unwrap();
    let mut store = Store::new(&engine, Vec::new());
    let instance = linker.instantiate(&mut store, &module).unwrap();
    let new = instance
        .get_typed_func::<i32, i32>(&mut store, "counter#[static]utxo.new")
        .unwrap();
    let add = instance
        .get_typed_func::<(i32, i64), i64>(&mut store, "counter#[method]utxo.add")
        .unwrap();
    let advance = instance
        .get_typed_func::<i32, ()>(&mut store, "counter#[method]utxo.advance")
        .unwrap();
    let handle = new.call(&mut store, 0).unwrap();
    let public_methods = store.data().clone();
    assert_eq!(public_methods.len(), 2);
    assert_eq!(add.call(&mut store, (handle, 5)).unwrap(), 15);
    store.data_mut().clear();
    advance.call(&mut store, handle).unwrap();
    assert_eq!(store.data().len(), 3);
    assert!(
        public_methods
            .iter()
            .all(|method| store.data().contains(method))
    );
    assert_eq!(add.call(&mut store, (handle, 7)).unwrap(), 122);
    store.data_mut().clear();
    advance.call(&mut store, handle).unwrap();
    assert_eq!(store.data(), &public_methods);
    assert_eq!(add.call(&mut store, (handle, 8)).unwrap(), 1130);
    store.data_mut().clear();
    let other = instance
        .get_typed_func::<i32, i32>(&mut store, "counter#[static]utxo.other")
        .unwrap();
    let other_handle = other.call(&mut store, 0).unwrap();
    assert_eq!(store.data(), &public_methods);
    assert_eq!(add.call(&mut store, (other_handle, 2)).unwrap(), 42);
    let chained = instance
        .get_typed_func::<(), i64>(&mut store, "chained")
        .unwrap();
    assert_eq!(chained.call(&mut store, ()).unwrap(), 15);
}
