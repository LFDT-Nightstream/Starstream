//! Chained insta snapshot tests for the Starstream DSL
//! 
//! This module uses the insta crate for snapshot testing with a chained approach:
//! 1. Parser generates the right AST (snapshot)
//! 2. Compiler generates the right opcodes (snapshot) 
//! 3. Interpreter produces the right result (snapshot)

use starstream_dsl::*;
use std::fs;

/// Helper function to capture interpreter state as a snapshot
fn capture_interpreter_state(interpreter: &StreamingInterpreter<DefaultOpcodeHandler>) -> String {
    let context = interpreter.context();
    
    // Capture all variables in the context
    // The context.variables field is a HashMap<String, i64>
    let state = &context.variables;
    
    serde_json::to_string_pretty(state).unwrap()
}

/// Test a single input file through the complete pipeline
fn test_input_file_pipeline(input_file: &str) {
    let source_code = fs::read_to_string(&format!("tests/inputs/{}", input_file)).unwrap();
    
    // Step 1: Parse and snapshot AST
    let program = parse_program(&source_code).unwrap();
    insta::assert_snapshot!(
        format!("{}_ast", input_file.replace(".imp", "")),
        serde_json::to_string_pretty(&program).unwrap()
    );
    
    // Step 2: Compile and snapshot opcodes
    let mut compiler = Compiler::new();
    let opcodes = compiler.compile_program(&program).unwrap();
    insta::assert_snapshot!(
        format!("{}_opcodes", input_file.replace(".imp", "")),
        serde_json::to_string_pretty(&opcodes).unwrap()
    );
    
    // Step 3: Execute and snapshot interpreter state
    let handler = DefaultOpcodeHandler;
    let mut interpreter = StreamingInterpreter::new(handler);
    interpreter.execute_sequence(&opcodes.opcodes).unwrap();
    
    let final_state = capture_interpreter_state(&interpreter);
    insta::assert_snapshot!(
        format!("{}_result", input_file.replace(".imp", "")),
        final_state
    );
}

/// Test error cases through the pipeline
fn test_error_file_pipeline(input_file: &str) {
    let source_code = fs::read_to_string(&format!("tests/inputs/{}", input_file)).unwrap();
    
    // Step 1: Parse and snapshot AST (should still work for error cases)
    let program = parse_program(&source_code).unwrap();
    insta::assert_snapshot!(
        format!("{}_ast", input_file.replace(".imp", "")),
        serde_json::to_string_pretty(&program).unwrap()
    );
    
    // Step 2: Compile and snapshot compilation error
    let mut compiler = Compiler::new();
    let result = compiler.compile_program(&program);
    assert!(result.is_err());
    insta::assert_snapshot!(
        format!("{}_compilation_error", input_file.replace(".imp", "")),
        serde_json::to_string_pretty(&result.unwrap_err()).unwrap()
    );
}

// Test cases for each input file
#[test]
fn test_simple_arithmetic_pipeline() {
    test_input_file_pipeline("simple_arithmetic.imp");
}

#[test]
fn test_conditional_pipeline() {
    test_input_file_pipeline("conditional.imp");
}

#[test]
fn test_while_loop_pipeline() {
    test_input_file_pipeline("while_loop.imp");
}

#[test]
fn test_boolean_operations_pipeline() {
    test_input_file_pipeline("boolean_operations.imp");
}

#[test]
fn test_error_undefined_variable_pipeline() {
    test_error_file_pipeline("error_undefined_variable.imp");
}
