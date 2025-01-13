;; input.wat
(module
  (import "spectest" "print" (func $print (param i32)))
  (import "asyncify" "start_unwind" (func $asyncify_start_unwind (param i32)))
  (import "asyncify" "stop_unwind" (func $asyncify_stop_unwind))
  (import "asyncify" "start_rewind" (func $asyncify_start_rewind (param i32)))
  (import "asyncify" "stop_rewind" (func $asyncify_stop_rewind))
  (memory 1 1)
  (global $sleeping (mut i32) (i32.const 0))
  (start $runtime)
  (func $main
    (call $print (i32.const 1))
    (call $sleep)
    (call $print (i32.const 3))
  )
  (func $sleep
    (if
      (i32.eqz (global.get $sleeping))
      (then
      (block
        ;; Start to sleep.
        (global.set $sleeping (i32.const 1))
        (i32.store (i32.const 16) (i32.const 24))
        (i32.store (i32.const 20) (i32.const 1024))
        (call $asyncify_start_unwind (i32.const 16))
      )
      )
      (else
      (block
        ;; Resume after sleep.
        (call $asyncify_stop_rewind)
        (global.set $sleeping (i32.const 0))
      )
      )
    )
  )
  (func $runtime
    ;; Call main the first time, let the stack unwind.
    (call $main)
    (call $asyncify_stop_unwind)
    ;; We could do anything we want around here while
    ;; the code is paused!
    (call $print (i32.const 2))
    ;; Set the rewind in motion.
    (call $asyncify_start_rewind (i32.const 16))
    (call $main)
  )
)
