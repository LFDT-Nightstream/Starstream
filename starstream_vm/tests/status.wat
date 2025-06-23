(module
  (import "starstream_utxo:wat:status" "starstream_new_main" (func $make_utxo (result i64)))
  (import "starstream_utxo:wat:status" "starstream_resume_" (func $resume (param i64 i32)))
  (import "starstream_utxo_env" "starstream_yield" (func $yield (param i32 i32 i32 i32 i32 i32)))
  (import "starstream_utxo" "starstream_status" (func $status (param i64) (result i32)))
  (import "env" "eprint" (func $eprint (param i32 i32)))

  (memory $mem 1)
  ;; 'x' is overwritten with '0' or '1' at runtime
  (data (i32.const 0) "Hello World\nstatus: x\n")

  (func $print_status (param $status i32)
    i32.const 20 ;; address
    local.get $status
    i32.const 48
    i32.add ;; status + '0' -> '0' or '1'
    i32.store8
    i32.const 12
    i32.const 10
    call $eprint
  )

  (func $coord
    (local $utxo i64)
    i32.const 0
    i32.const 12
    call $eprint
    call $make_utxo
    local.tee $utxo
    call $status
    call $print_status
    local.get $utxo
    i32.const 0
    call $resume
    local.get $utxo
    call $status
    call $print_status
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

