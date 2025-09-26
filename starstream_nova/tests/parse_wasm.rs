use starstream_nova::wasm_parser;

#[test]
#[ignore]
fn parse_wasm() {
    let example_module = r#"
        (module
          (func $f (param i32 i32 i32 i32) (result i32)
            local.get 0
            local.get 1
            i32.add
            local.get 2
            i32.add
            local.get 3
            i32.add
          )
          (func $g (result i32)
            i32.const 1
            i32.const 2
            i32.const 3
            i32.const 4
            call $f
          )
        )
    "#;
    let binary: Vec<u8> = wat::parse_str(example_module).unwrap();
    let out = wasm_parser::parse(binary.as_slice());
    println!("{out:?}");
}
