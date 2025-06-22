(module
  (import "starstream_utxo:wat:status" "starstream_new_main" (func $make_utxo (result i64)))
  (import "starstream_utxo_env" "starstream_yield" (func $yield (param i32 i32 i32 i32 i32 i32)))
  (import "env" "eprint" (func $eprint (param i32 i32)))

  (memory $mem 1)
  (data (i32.const 0) "Hello World\nstatus")

  (func $coord
    i32.const 0
    i32.const 12
    call $eprint
    call $make_utxo
    drop
  )

  (func $main
    i32.const 12
    i32.const 6
    i32.const 0
    i32.const 0
    i32.const 0
    i32.const 0
    call $yield
  )

  (export "starstream_new_main" (func $main))
  (export "coord" (func $coord))
  (export "memory" (memory $mem))
)

