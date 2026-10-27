use crate::optimize;

/// Optimize `before` and check that it prints the same as `after`, that it's
/// valid, and that optimizing it again changes nothing.
fn assert_optimizes(before: &str, after: &str) {
    let optimized = optimize(&wat::parse_str(before).unwrap()).unwrap();
    wasmparser::validate(&optimized).unwrap();
    assert_eq!(
        wasmprinter::print_bytes(&optimized).unwrap(),
        wasmprinter::print_bytes(wat::parse_str(after).unwrap()).unwrap(),
    );
    assert_eq!(optimize(&optimized).unwrap(), optimized);
}

#[test]
fn forwards_single_use_copies() {
    // What `let x = f(); g(x, 10)` compiles to.
    assert_optimizes(
        r#"
        (module
          (import "host" "make" (func (result i32)))
          (import "host" "use" (func (param i32 i64)))
          (func (local i32 i32)
            call 0
            local.set 0
            local.get 0
            local.set 1
            local.get 1
            i64.const 10
            call 1))
        "#,
        r#"
        (module
          (import "host" "make" (func (result i32)))
          (import "host" "use" (func (param i32 i64)))
          (func
            call 0
            i64.const 10
            call 1))
        "#,
    );
}

#[test]
fn keeps_locals_that_are_read_again() {
    assert_optimizes(
        r#"
        (module
          (func (param i32) (result i32) (local i32)
            local.get 0
            local.set 1
            local.get 1
            local.get 1
            i32.add))
        "#,
        r#"
        (module
          (func (param i32) (result i32) (local i32)
            local.get 0
            local.tee 1
            local.get 1
            i32.add))
        "#,
    );
}

#[test]
fn tee_then_drop_becomes_set() {
    assert_optimizes(
        r#"
        (module
          (import "host" "f" (func))
          (func (param i32) (result i32) (local i32)
            local.get 0
            local.tee 1
            drop
            call 0
            local.get 1))
        "#,
        r#"
        (module
          (import "host" "f" (func))
          (func (param i32) (result i32) (local i32)
            local.get 0
            local.set 1
            call 0
            local.get 1))
        "#,
    );
}

#[test]
fn dead_stores_keep_side_effects() {
    // The call stays; the global read and the constants go.
    assert_optimizes(
        r#"
        (module
          (import "host" "f" (func (result i32)))
          (global (mut i64) (i64.const 0))
          (func (param i32) (local i32 i64)
            call 0
            local.set 1
            global.get 0
            local.set 2
            local.get 0
            drop
            i32.const 1
            drop
            i64.const 2
            drop))
        "#,
        r#"
        (module
          (import "host" "f" (func (result i32)))
          (global (mut i64) (i64.const 0))
          (func (param i32)
            call 0
            drop))
        "#,
    );
}

#[test]
fn renumbers_locals_after_parameters() {
    // Local 1 is never read and goes; local 2 becomes local 1. The parameter
    // stays even though it's only read once.
    assert_optimizes(
        r#"
        (module
          (func (param i64) (result i64) (local i32 i64)
            i32.const 7
            local.set 1
            local.get 0
            local.set 2
            local.get 2
            local.get 2
            i64.add))
        "#,
        r#"
        (module
          (func (param i64) (result i64) (local i64)
            local.get 0
            local.tee 1
            local.get 1
            i64.add))
        "#,
    );
}

#[test]
fn copies_everything_else_byte_for_byte() {
    let wasm = wat::parse_str(
        r#"
        (module
          (import "host" "f" (func (param i32) (result i32)))
          (memory 1)
          (global (mut i64) (i64.const 5))
          (func (export "run") (param i32) (result i32) (local i32)
            local.get 0
            call 0
            local.tee 1
            local.get 1
            i32.add)
          (data (i32.const 8) "state")
          (@custom "component-type" "\00\01\02"))
        "#,
    )
    .unwrap();
    assert_eq!(optimize(&wasm).unwrap(), wasm);
}
