use starstream_compiler::TypecheckOptions;
use wasmtime::{Config, Engine, Linker, Module, Store};

/// Helper to compile a Starstream program
fn compile_program(source: &str) -> Vec<u8> {
    let parse_output = starstream_compiler::parse_program(source);
    let (program, errors) = parse_output.into_output_errors();
    assert!(errors.is_empty(), "Parse errors: {:?}", errors);

    let program = program.expect("No program parsed");
    let success = starstream_compiler::typecheck_program(
        &program,
        TypecheckOptions {
            capture_traces: false,
        },
    )
    .expect("Typecheck failed");

    let (wasm, errors) = starstream_to_wasm::compile(&success.program);
    assert!(errors.is_empty(), "Compile errors: {:?}", errors);

    wasm.expect("No WASM generated")
}

/// Helper to execute WASM and check if it traps
fn execute_wasm(wasm: &[u8], func_name: &str, params: &[i64]) -> Result<Vec<i64>, String> {
    let mut config = Config::new();
    config.wasm_component_model(false);
    let engine = Engine::new(&config).unwrap();
    let module = Module::from_binary(&engine, wasm).unwrap();

    let linker = Linker::new(&engine);
    let mut store = Store::new(&engine, ());

    let instance = linker.instantiate(&mut store, &module).unwrap();
    let func = instance
        .get_func(&mut store, func_name)
        .unwrap_or_else(|| panic!("Function {} not found", func_name));

    let mut results = vec![wasmtime::Val::I64(0)];
    let params: Vec<wasmtime::Val> = params.iter().map(|&p| wasmtime::Val::I64(p)).collect();

    match func.call(&mut store, &params, &mut results) {
        Ok(_) => {
            let result: Vec<i64> = results.iter().map(|v| v.i64().unwrap()).collect();
            Ok(result)
        }
        Err(e) => Err(format!("{}", e)),
    }
}

// ============================================================================
// Tests that SHOULD overflow (trap at runtime)
// ============================================================================

#[test]
fn test_overflow_i64_max_plus_one() {
    let source = r#"
script fn overflow_test() -> i64 {
    9223372036854775807 + 1
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-test", &[]);
    assert!(result.is_err(), "Should trap on overflow");
}

#[test]
fn test_overflow_i64_max_plus_max() {
    let source = r#"
script fn overflow_test() -> i64 {
    9223372036854775807 + 9223372036854775807
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-test", &[]);
    assert!(result.is_err(), "Should trap on overflow");
}

#[test]
fn test_overflow_i64_min_minus_one() {
    let source = r#"
script fn overflow_test() -> i64 {
    -9223372036854775807 - 1 + -1
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-test", &[]);
    assert!(result.is_err(), "Should trap on overflow");
}

#[test]
fn test_overflow_i64_min_plus_min() {
    let source = r#"
script fn overflow_test() -> i64 {
    let min = -9223372036854775807 - 1;
    min + min
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-test", &[]);
    assert!(result.is_err(), "Should trap on overflow");
}

#[test]
fn test_overflow_large_positives() {
    let source = r#"
script fn overflow_test() -> i64 {
    5000000000000000000 + 5000000000000000000
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-test", &[]);
    assert!(result.is_err(), "Should trap on overflow");
}

#[test]
fn test_overflow_large_negatives() {
    let source = r#"
script fn overflow_test() -> i64 {
    -5000000000000000000 + -5000000000000000000
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-test", &[]);
    assert!(result.is_err(), "Should trap on overflow");
}

#[test]
fn test_overflow_with_parameters() {
    let source = r#"
script fn overflow_test(x: i64, y: i64) -> i64 {
    x + y
}
"#;

    let wasm = compile_program(source);

    // Test with values that overflow
    let result = execute_wasm(&wasm, "overflow-test", &[i64::MAX, 1]);
    assert!(result.is_err(), "Should trap on overflow with MAX + 1");

    let result = execute_wasm(&wasm, "overflow-test", &[i64::MIN, -1]);
    assert!(result.is_err(), "Should trap on overflow with MIN + -1");
}

// ============================================================================
// Tests that should NOT overflow (return successful results)
// ============================================================================

#[test]
fn test_no_overflow_max_plus_zero() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    9223372036854775807 + 0
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MAX);
}

#[test]
fn test_no_overflow_min_plus_zero() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    -9223372036854775807 - 1 + 0
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MIN);
}

#[test]
fn test_no_overflow_max_plus_min() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    9223372036854775807 + (-9223372036854775807 - 1)
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(
        result.is_ok(),
        "Should not trap - different signs never overflow"
    );
    assert_eq!(result.unwrap()[0], -1);
}

#[test]
fn test_no_overflow_max_minus_one() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    9223372036854775807 + -1
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MAX - 1);
}

#[test]
fn test_no_overflow_min_plus_one() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    -9223372036854775807 - 1 + 1
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MIN + 1);
}

#[test]
fn test_no_overflow_boundary_positive() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    4611686018427387903 + 4611686018427387903
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap at boundary");
    assert_eq!(result.unwrap()[0], 9223372036854775806);
}

#[test]
fn test_no_overflow_boundary_negative() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    -4611686018427387904 + -4611686018427387904
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap at boundary");
    assert_eq!(result.unwrap()[0], i64::MIN);
}

#[test]
fn test_no_overflow_mixed_signs() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    5000000000000000000 + -3000000000000000000
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap - different signs");
    assert_eq!(result.unwrap()[0], 2000000000000000000);
}

#[test]
fn test_no_overflow_zero_plus_zero() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    0 + 0
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], 0);
}

#[test]
fn test_no_overflow_small_positive() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    100 + 200
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], 300);
}

#[test]
fn test_no_overflow_small_negative() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    -100 + -200
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], -300);
}

#[test]
fn test_no_overflow_with_parameters() {
    let source = r#"
script fn no_overflow_test(x: i64, y: i64) -> i64 {
    x + y
}
"#;

    let wasm = compile_program(source);

    // Test with values that don't overflow
    let result = execute_wasm(&wasm, "no-overflow-test", &[100, 200]);
    assert!(result.is_ok(), "Should not trap with small values");
    assert_eq!(result.unwrap()[0], 300);

    let result = execute_wasm(&wasm, "no-overflow-test", &[i64::MAX, 0]);
    assert!(result.is_ok(), "Should not trap with MAX + 0");
    assert_eq!(result.unwrap()[0], i64::MAX);

    let result = execute_wasm(&wasm, "no-overflow-test", &[i64::MAX, i64::MIN]);
    assert!(result.is_ok(), "Should not trap with opposite signs");
    assert_eq!(result.unwrap()[0], -1);
}

// ============================================================================
// Tests for the overflow detection formula itself
// ============================================================================

#[test]
fn test_overflow_formula_positive_overflow() {
    // Tests the formula: (x ^ y) >= 0 && (sum ^ x) < 0
    // For positive overflow: both positive, sum negative
    let source = r#"
script fn test(x: i64, y: i64) -> i64 {
    x + y
}
"#;

    let wasm = compile_program(source);

    // Both positive, sum wraps to negative
    let result = execute_wasm(&wasm, "test", &[i64::MAX, 1]);
    assert!(result.is_err(), "Positive overflow not detected");
}

#[test]
fn test_overflow_formula_negative_overflow() {
    // For negative overflow: both negative, sum wraps to positive
    let source = r#"
script fn test(x: i64, y: i64) -> i64 {
    x + y
}
"#;

    let wasm = compile_program(source);

    // Both negative, sum wraps to positive
    let result = execute_wasm(&wasm, "test", &[i64::MIN, -1]);
    assert!(result.is_err(), "Negative overflow not detected");
}

#[test]
fn test_overflow_formula_different_signs_never_overflow() {
    // When signs differ, (x ^ y) < 0, so overflow check short-circuits
    let source = r#"
script fn test(x: i64, y: i64) -> i64 {
    x + y
}
"#;

    let wasm = compile_program(source);

    // Different signs - should never overflow
    let result = execute_wasm(&wasm, "test", &[i64::MAX, -1]);
    assert!(result.is_ok(), "Different signs should not overflow");

    let result = execute_wasm(&wasm, "test", &[i64::MIN, 1]);
    assert!(result.is_ok(), "Different signs should not overflow");

    let result = execute_wasm(&wasm, "test", &[i64::MAX, i64::MIN]);
    assert!(result.is_ok(), "Different signs should not overflow");
}
