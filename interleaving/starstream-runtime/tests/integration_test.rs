use starstream_runtime::UnprovenTransaction;

#[test]
fn test_simple_resume_yield() {
    // Modified WAT: _start takes no params and returns nothing.
    // Host call still returns i64 (which is the 'next_arg' passed via resume).

    let program0_wat = r#"
        (module
          (import "env" "starstream_host_call" (func $host_call (param i64 i64 i64 i64 i64) (result i64)))
          (memory (export "memory") 1)
          (func (export "_start")
            ;; Call Resume(target=1, val=100, ret=200, id_prev=MAX)
            ;; RESUME = 0
            (call $host_call
              (i64.const 0)   ;; discriminant
              (i64.const 1)   ;; target
              (i64.const 100) ;; val
              (i64.const 200) ;; ret
              (i64.const -1)  ;; id_prev (MAX)
            )
            drop ;; drop the result of host_call (which is the return value from the target's yield)
          )
        )
    "#;

    let program1_wat = r#"
        (module
          (import "env" "starstream_host_call" (func $host_call (param i64 i64 i64 i64 i64) (result i64)))
          (memory (export "memory") 1)
          (func (export "_start")
            ;; Program 1 is resumed.
            ;; We don't receive args via function params anymore (since _start is () -> ()).
            ;; But we can pretend we did logic.

            ;; Yield(val=101, ret=MAX, id_prev=0)
            ;; YIELD = 1
            (call $host_call
              (i64.const 1)   ;; discriminant
              (i64.const 101) ;; val
              (i64.const -1)  ;; ret (MAX -> None)
              (i64.const 0)   ;; id_prev
              (i64.const 0)   ;; unused arg4
            )
            drop
          )
        )
    "#;

    let program0 = wat::parse_str(program0_wat).unwrap();
    let program1 = wat::parse_str(program1_wat).unwrap();

    let tx = UnprovenTransaction {
        inputs: vec![],
        programs: vec![program0, program1],
        entrypoint: 0,
        is_utxo: vec![false, false],
    };

    let instance = tx.to_instance();

    dbg!(instance.host_calls_roots);

    // We could inspect internal state if we exposed it, but for now we just ensure it runs.
}
