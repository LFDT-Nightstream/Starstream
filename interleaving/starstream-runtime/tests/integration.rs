use sha2::{Digest, Sha256};
use starstream_interleaving_spec::Ledger;
use starstream_runtime::UnprovenTransaction;
use wat::parse_str;

#[test]
fn test_runtime_simple_effect_handlers() {
    // Pseudocode (UTXO):
    // - (ret, caller) = activation()
    // - assert program_hash(caller) == program_hash(1) // caller is the script in this test
    // - handler_id = get_handler_for(interface_id=1)
    // - req = new_ref(1); ref_push(42)
    // - (ret, resp) = resume(handler_id, req)
    // - assert get(resp, 0) == 1
    // - yield(resp)
    let utxo_wat = r#"(
module
    (import "env" "starstream_activation" (func $activation (result i64 i64)))
    (import "env" "starstream_get_program_hash" (func $program_hash (param i64) (result i64 i64 i64 i64)))
    (import "env" "starstream_get_handler_for" (func $get_handler_for (param i64 i64 i64 i64) (result i64)))
    (import "env" "starstream_new_ref" (func $new_ref (param i64) (result i64)))
    (import "env" "starstream_ref_push" (func $ref_push (param i64)))
    (import "env" "starstream_get" (func $get (param i64 i64) (result i64)))
    (import "env" "starstream_resume" (func $resume (param i64 i64) (result i64 i64)))
    (import "env" "starstream_yield" (func $yield (param i64) (result i64 i64)))

    (func (export "_start")
        (local $init_ref i64) (local $caller i64) (local $handler_id i64) (local $req i64) (local $resp i64)
        (local $caller_hash_a i64) (local $caller_hash_b i64) (local $caller_hash_c i64) (local $caller_hash_d i64)
        (local $script_hash_a i64) (local $script_hash_b i64) (local $script_hash_c i64) (local $script_hash_d i64)
        (local $resp_val i64)
        
        ;; ACTIVATION
        (call $activation)
        local.set $caller
        local.set $init_ref
        
        ;; PROGRAM_HASH(target=caller)
        (call $program_hash (local.get $caller))
        local.set $caller_hash_d
        local.set $caller_hash_c
        local.set $caller_hash_b
        local.set $caller_hash_a

        ;; PROGRAM_HASH(target=script id 1)
        (call $program_hash (i64.const 1))
        local.set $script_hash_d
        local.set $script_hash_c
        local.set $script_hash_b
        local.set $script_hash_a

        ;; Ensure caller hash matches script hash
        (local.get $caller_hash_a)
        (local.get $script_hash_a)
        i64.ne
        if unreachable end
        (local.get $caller_hash_b)
        (local.get $script_hash_b)
        i64.ne
        if unreachable end
        (local.get $caller_hash_c)
        (local.get $script_hash_c)
        i64.ne
        if unreachable end
        (local.get $caller_hash_d)
        (local.get $script_hash_d)
        i64.ne
        if unreachable end
        
        ;; GET_HANDLER_FOR(interface_id=limbs at 0)
        (call $get_handler_for (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0))
        local.set $handler_id
        
        ;; NEW_REF(size=1)
        (call $new_ref (i64.const 1))
        local.set $req
        
        ;; REF_PUSH(val=42)
        (call $ref_push (i64.const 42))
        
        ;; RESUME(target=$handler_id, val=$req, ret=2, prev=1)
        (call $resume (local.get $handler_id) (local.get $req))
        drop
        local.set $resp

        ;; GET(bool) from handler response
        (call $get (local.get $resp) (i64.const 0))
        local.set $resp_val
        (local.get $resp_val)
        (i64.const 1)
        i64.ne
        if unreachable end
        
        ;; YIELD(val=$resp, ret=-1, prev=0)
        (call $yield (local.get $resp))
        drop
        drop
    )
)
"#;
    let utxo_bin = parse_str(utxo_wat).unwrap();

    // TODO: this should be poseidon at some point later
    let mut hasher = Sha256::new();
    hasher.update(&utxo_bin);
    let utxo_hash_bytes = hasher.finalize();

    let utxo_hash_limb_a = u64::from_le_bytes(utxo_hash_bytes[0..8].try_into().unwrap());
    let utxo_hash_limb_b = u64::from_le_bytes(utxo_hash_bytes[8..8 * 2].try_into().unwrap());
    let utxo_hash_limb_c = u64::from_le_bytes(utxo_hash_bytes[8 * 2..8 * 3].try_into().unwrap());
    let utxo_hash_limb_d = u64::from_le_bytes(utxo_hash_bytes[8 * 3..8 * 4].try_into().unwrap());

    // 2. Compile Coord Program
    // Pseudocode (Coord):
    // - install_handler(interface_id=1)
    // - init = new_ref(1); ref_push(0)
    // - new_utxo(hash(utxo_bin), init)
    // - (req, caller) = resume(utxo_id=0, init)
    // - assert get(req, 0) == 42
    // - resp = new_ref(1); ref_push(1)
    // - resume(utxo_id=0, resp)
    // - uninstall_handler(interface_id=1)
    let coord_wat = format!(
        r#"(
module
    (import "env" "starstream_install_handler" (func $install_handler (param i64 i64 i64 i64)))
    (import "env" "starstream_new_ref" (func $new_ref (param i64) (result i64)))
    (import "env" "starstream_ref_push" (func $ref_push (param i64)))
    (import "env" "starstream_get" (func $get (param i64 i64) (result i64)))
    (import "env" "starstream_new_utxo" (func $new_utxo (param i64 i64 i64 i64 i64) (result i64)))
    (import "env" "starstream_resume" (func $resume (param i64 i64) (result i64 i64)))
    (import "env" "starstream_uninstall_handler" (func $uninstall_handler (param i64 i64 i64 i64)))
    
    (func (export "_start")
        (local $init_val i64) (local $req i64) (local $req_val i64) (local $resp i64) (local $caller i64)
        
        ;; INSTALL_HANDLER(interface_id=limbs at 0)
        (call $install_handler (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0))
        
        ;; NEW_REF(size=1)
        (call $new_ref (i64.const 1))
        local.set $init_val
        
        ;; REF_PUSH(val=0)
        (call $ref_push (i64.const 0))
        
        ;; NEW_UTXO(program_hash=limbs at 32, val=$init_val, id=0)
        (call $new_utxo
            (i64.const {utxo_hash_limb_a}) 
            (i64.const {utxo_hash_limb_b}) 
            (i64.const {utxo_hash_limb_c}) 
            (i64.const {utxo_hash_limb_d}) 
            (local.get $init_val) 
        )
        drop
        
        ;; RESUME(target=0, val=$init_val, ret=1, prev=-1)
        (call $resume (i64.const 0) (local.get $init_val))
        local.set $caller
        local.set $req

        ;; GET request val
        (call $get (local.get $req) (i64.const 0))
        local.set $req_val
        (local.get $req_val)
        (i64.const 42)
        i64.ne
        if unreachable end
        
        ;; NEW_REF(size=1)
        (call $new_ref (i64.const 1))
        local.set $resp
        
        ;; REF_PUSH(val=true)
        (call $ref_push (i64.const 1))
        
        ;; RESUME(target=0, val=$resp, ret=$resp, prev=0)
        (call $resume (i64.const 0) (local.get $resp))
        drop
        drop
        
        ;; UNINSTALL_HANDLER(interface_id=limbs at 0)
        (call $uninstall_handler (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0))
    )
)
"#,
        utxo_hash_limb_a = utxo_hash_limb_a,
        utxo_hash_limb_b = utxo_hash_limb_b,
        utxo_hash_limb_c = utxo_hash_limb_c,
        utxo_hash_limb_d = utxo_hash_limb_d,
    );
    let coord_bin = parse_str(&coord_wat).unwrap();

    let programs = vec![utxo_bin, coord_bin.clone()];

    let tx = UnprovenTransaction {
        inputs: vec![],
        programs,
        is_utxo: vec![true, false],
        entrypoint: 1,
    };

    let proven_tx = tx.prove().unwrap();

    let ledger = Ledger::new();

    let ledger = ledger.apply_transaction(&proven_tx).unwrap();

    assert_eq!(ledger.utxos.len(), 1);
}

#[test]
fn test_runtime_effect_handlers_star_flow() {
    // Pseudocode (UTXO):
    // - (init, caller) = activation()
    // - loop:
    //   - yield(resp_msg) -> (req_msg, caller)
    //   - assert req_msg.iface == UtxoAbi
    //   - match req_msg.disc:
    //       1 => raise Foo(33) via handler, respond
    //       2 => respond 1
    //       3 => raise Bar(payload) via handler, respond with handler result
    //
    // Pseudocode (Coord):
    // - install_handler(A) outer
    // - create UTXO and start it
    // - call abi_call1, abi_call2, abi_call3(false) with outer handlers
    // - install_handler(A) inner, call abi_call3(true) with inner handlers
    // - uninstall handler inner, uninstall handler outer
    let utxo_wat = r#"(
module
    (import "env" "starstream_activation" (func $activation (result i64 i64)))
    (import "env" "starstream_get_handler_for" (func $get_handler_for (param i64 i64 i64 i64) (result i64)))
    (import "env" "starstream_new_ref" (func $new_ref (param i64) (result i64)))
    (import "env" "starstream_ref_push" (func $ref_push (param i64)))
    (import "env" "starstream_get" (func $get (param i64 i64) (result i64)))
    (import "env" "starstream_resume" (func $resume (param i64 i64) (result i64 i64)))
    (import "env" "starstream_yield" (func $yield (param i64) (result i64 i64)))

    (func (export "_start")
        (local $init i64) (local $caller i64) (local $handler_id i64)
        (local $req i64) (local $resp_msg i64)
        (local $iface0 i64) (local $iface1 i64) (local $iface2 i64) (local $iface3 i64)
        (local $disc i64) (local $payload i64)
        (local $effect_req i64) (local $effect_resp i64) (local $effect_val i64)

        ;; ACTIVATION
        (call $activation)
        local.set $caller
        local.set $init

        ;; Prepare initial response message (iface=UtxoAbi, disc=0, payload=0)
        (call $new_ref (i64.const 6))
        local.set $resp_msg
        (call $ref_push (i64.const 2))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 0))

        (block $exit
            (loop $loop
                ;; YIELD(response) => (req, caller)
                (call $yield (local.get $resp_msg))
                local.set $caller
                local.set $req

                ;; Read iface + discriminant + payload
                (call $get (local.get $req) (i64.const 0))
                local.set $iface0
                (call $get (local.get $req) (i64.const 1))
                local.set $iface1
                (call $get (local.get $req) (i64.const 2))
                local.set $iface2
                (call $get (local.get $req) (i64.const 3))
                local.set $iface3
                (call $get (local.get $req) (i64.const 4))
                local.set $disc
                (call $get (local.get $req) (i64.const 5))
                local.set $payload

                ;; Assert iface == UtxoAbi (2,0,0,0)
                (local.get $iface0)
                (i64.const 2)
                i64.ne
                if unreachable end
                (local.get $iface1)
                (i64.const 0)
                i64.ne
                if unreachable end
                (local.get $iface2)
                (i64.const 0)
                i64.ne
                if unreachable end
                (local.get $iface3)
                (i64.const 0)
                i64.ne
                if unreachable end

                ;; Match discriminant
                (local.get $disc)
                (i64.const 1)
                i64.eq
                if
                    ;; AbiCall1 => raise Foo(33)
                    (call $get_handler_for (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0))
                    local.set $handler_id

                    (call $new_ref (i64.const 6))
                    local.set $effect_req
                    (call $ref_push (i64.const 1))
                    (call $ref_push (i64.const 0))
                    (call $ref_push (i64.const 0))
                    (call $ref_push (i64.const 0))
                    (call $ref_push (i64.const 1))
                    (call $ref_push (i64.const 33))

                    (call $resume (local.get $handler_id) (local.get $effect_req))
                    local.set $caller
                    local.set $effect_resp

                    ;; Respond with iface=UtxoAbi, disc=1, payload=0
                    (call $new_ref (i64.const 6))
                    local.set $resp_msg
                    (call $ref_push (i64.const 2))
                    (call $ref_push (i64.const 0))
                    (call $ref_push (i64.const 0))
                    (call $ref_push (i64.const 0))
                    (call $ref_push (i64.const 1))
                    (call $ref_push (i64.const 0))
                else
                    (local.get $disc)
                    (i64.const 2)
                    i64.eq
                    if
                        ;; AbiCall2 => respond 1
                        (call $new_ref (i64.const 6))
                        local.set $resp_msg
                        (call $ref_push (i64.const 2))
                        (call $ref_push (i64.const 0))
                        (call $ref_push (i64.const 0))
                        (call $ref_push (i64.const 0))
                        (call $ref_push (i64.const 2))
                        (call $ref_push (i64.const 1))
                    else
                        (local.get $disc)
                        (i64.const 3)
                        i64.eq
                        if
                            ;; AbiCall3 => raise Bar(payload)
                            (call $get_handler_for (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0))
                            local.set $handler_id

                            (call $new_ref (i64.const 6))
                            local.set $effect_req
                            (call $ref_push (i64.const 1))
                            (call $ref_push (i64.const 0))
                            (call $ref_push (i64.const 0))
                            (call $ref_push (i64.const 0))
                            (call $ref_push (i64.const 2))
                            (call $ref_push (local.get $payload))

                            (call $resume (local.get $handler_id) (local.get $effect_req))
                            local.set $caller
                            local.set $effect_resp

                            (call $get (local.get $effect_resp) (i64.const 0))
                            local.set $effect_val

                            ;; Respond with iface=UtxoAbi, disc=3, payload=effect_val
                            (call $new_ref (i64.const 6))
                            local.set $resp_msg
                            (call $ref_push (i64.const 2))
                            (call $ref_push (i64.const 0))
                            (call $ref_push (i64.const 0))
                            (call $ref_push (i64.const 0))
                            (call $ref_push (i64.const 3))
                            (call $ref_push (local.get $effect_val))
                        else
                            unreachable
                        end
                    end
                end

                br $loop
            )
        )
    )
)
"#;
    let utxo_bin = parse_str(utxo_wat).unwrap();

    let mut hasher = Sha256::new();
    hasher.update(&utxo_bin);
    let utxo_hash_bytes = hasher.finalize();

    let utxo_hash_limb_a = u64::from_le_bytes(utxo_hash_bytes[0..8].try_into().unwrap());
    let utxo_hash_limb_b = u64::from_le_bytes(utxo_hash_bytes[8..8 * 2].try_into().unwrap());
    let utxo_hash_limb_c = u64::from_le_bytes(utxo_hash_bytes[8 * 2..8 * 3].try_into().unwrap());
    let utxo_hash_limb_d = u64::from_le_bytes(utxo_hash_bytes[8 * 3..8 * 4].try_into().unwrap());

    let coord_wat = format!(
        r#"(
module
    (import "env" "starstream_install_handler" (func $install_handler (param i64 i64 i64 i64)))
    (import "env" "starstream_uninstall_handler" (func $uninstall_handler (param i64 i64 i64 i64)))
    (import "env" "starstream_new_ref" (func $new_ref (param i64) (result i64)))
    (import "env" "starstream_ref_push" (func $ref_push (param i64)))
    (import "env" "starstream_get" (func $get (param i64 i64) (result i64)))
    (import "env" "starstream_new_utxo" (func $new_utxo (param i64 i64 i64 i64 i64) (result i64)))
    (import "env" "starstream_resume" (func $resume (param i64 i64) (result i64 i64)))

    (func (export "_start")
        (local $init i64) (local $req i64) (local $msg i64) (local $caller i64)
        (local $iface0 i64) (local $iface1 i64) (local $iface2 i64) (local $iface3 i64)
        (local $disc i64) (local $payload i64)
        (local $resp i64) (local $handler_mode i64)

        ;; install outer handler for A
        (call $install_handler (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0))
        (i64.const 0)
        local.set $handler_mode

        ;; init ref
        (call $new_ref (i64.const 1))
        local.set $init
        (call $ref_push (i64.const 0))

        ;; new utxo
        (call $new_utxo
            (i64.const {utxo_hash_limb_a})
            (i64.const {utxo_hash_limb_b})
            (i64.const {utxo_hash_limb_c})
            (i64.const {utxo_hash_limb_d})
            (local.get $init)
        )
        drop

        ;; start utxo
        (call $resume (i64.const 0) (local.get $init))
        drop
        drop

        ;; abi_call1()
        (call $new_ref (i64.const 6))
        local.set $req
        (call $ref_push (i64.const 2))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 1))
        (call $ref_push (i64.const 0))

        (call $resume (i64.const 0) (local.get $req))
        local.set $caller
        local.set $msg

        (block $done1
            (loop $loop1
                ;; parse iface
                (call $get (local.get $msg) (i64.const 0))
                local.set $iface0
                (call $get (local.get $msg) (i64.const 1))
                local.set $iface1
                (call $get (local.get $msg) (i64.const 2))
                local.set $iface2
                (call $get (local.get $msg) (i64.const 3))
                local.set $iface3
                (call $get (local.get $msg) (i64.const 4))
                local.set $disc
                (call $get (local.get $msg) (i64.const 5))
                local.set $payload

                ;; iface == A?
                (local.get $iface0)
                (i64.const 1)
                i64.eq
                (local.get $iface1)
                (i64.const 0)
                i64.eq
                i32.and
                (local.get $iface2)
                (i64.const 0)
                i64.eq
                i32.and
                (local.get $iface3)
                (i64.const 0)
                i64.eq
                i32.and
                if
                    ;; handle A::Foo/A::Bar
                    (local.get $disc)
                    (i64.const 1)
                    i64.eq
                    if
                        ;; Foo: outer => x * i, inner => i
                        (local.get $handler_mode)
                        (i64.const 1)
                        i64.eq
                        if
                            (local.get $payload)
                            local.set $resp
                        else
                            (i64.const 5)
                            (local.get $payload)
                            i64.mul
                            local.set $resp
                        end
                    else
                        ;; Bar: outer => !b, inner => b
                        (local.get $handler_mode)
                        (i64.const 1)
                        i64.eq
                        if
                            (local.get $payload)
                            local.set $resp
                        else
                            (local.get $payload)
                            (i64.const 0)
                            i64.eq
                            if
                                (i64.const 1)
                                local.set $resp
                            else
                                (i64.const 0)
                                local.set $resp
                            end
                        end
                    end

                    (call $new_ref (i64.const 1))
                    local.set $req
                    (call $ref_push (local.get $resp))
                    (call $resume (local.get $caller) (local.get $req))
                    local.set $caller
                    local.set $msg
                    br $loop1
                end

                ;; iface == UtxoAbi? expect response for call1
                (local.get $iface0)
                (i64.const 2)
                i64.ne
                if unreachable end
                (local.get $iface1)
                (i64.const 0)
                i64.ne
                if unreachable end
                (local.get $iface2)
                (i64.const 0)
                i64.ne
                if unreachable end
                (local.get $iface3)
                (i64.const 0)
                i64.ne
                if unreachable end
                (local.get $disc)
                (i64.const 1)
                i64.ne
                if unreachable end
                br $done1
            )
        )

        ;; abi_call2() => expect payload 1
        (call $new_ref (i64.const 6))
        local.set $req
        (call $ref_push (i64.const 2))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 2))
        (call $ref_push (i64.const 0))

        (call $resume (i64.const 0) (local.get $req))
        local.set $caller
        local.set $msg

        (block $done2
            (loop $loop2
                (call $get (local.get $msg) (i64.const 0))
                local.set $iface0
                (call $get (local.get $msg) (i64.const 1))
                local.set $iface1
                (call $get (local.get $msg) (i64.const 2))
                local.set $iface2
                (call $get (local.get $msg) (i64.const 3))
                local.set $iface3
                (call $get (local.get $msg) (i64.const 4))
                local.set $disc
                (call $get (local.get $msg) (i64.const 5))
                local.set $payload

                (local.get $iface0)
                (i64.const 1)
                i64.eq
                (local.get $iface1)
                (i64.const 0)
                i64.eq
                i32.and
                (local.get $iface2)
                (i64.const 0)
                i64.eq
                i32.and
                (local.get $iface3)
                (i64.const 0)
                i64.eq
                i32.and
                if
                    ;; handle A::Foo/A::Bar (same as above)
                    (local.get $disc)
                    (i64.const 1)
                    i64.eq
                    if
                        (local.get $handler_mode)
                        (i64.const 1)
                        i64.eq
                        if
                            (local.get $payload)
                            local.set $resp
                        else
                            (i64.const 5)
                            (local.get $payload)
                            i64.mul
                            local.set $resp
                        end
                    else
                        (local.get $handler_mode)
                        (i64.const 1)
                        i64.eq
                        if
                            (local.get $payload)
                            local.set $resp
                        else
                            (local.get $payload)
                            (i64.const 0)
                            i64.eq
                            if
                                (i64.const 1)
                                local.set $resp
                            else
                                (i64.const 0)
                                local.set $resp
                            end
                        end
                    end

                    (call $new_ref (i64.const 1))
                    local.set $req
                    (call $ref_push (local.get $resp))
                    (call $resume (local.get $caller) (local.get $req))
                    local.set $caller
                    local.set $msg
                    br $loop2
                end

                (local.get $iface0)
                (i64.const 2)
                i64.ne
                if unreachable end
                (local.get $iface1)
                (i64.const 0)
                i64.ne
                if unreachable end
                (local.get $iface2)
                (i64.const 0)
                i64.ne
                if unreachable end
                (local.get $iface3)
                (i64.const 0)
                i64.ne
                if unreachable end
                (local.get $disc)
                (i64.const 2)
                i64.ne
                if unreachable end
                (local.get $payload)
                (i64.const 1)
                i64.ne
                if unreachable end
                br $done2
            )
        )

        ;; abi_call3(false) => expect payload 1
        (call $new_ref (i64.const 6))
        local.set $req
        (call $ref_push (i64.const 2))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 3))
        (call $ref_push (i64.const 0))

        (call $resume (i64.const 0) (local.get $req))
        local.set $caller
        local.set $msg

        (block $done3
            (loop $loop3
                (call $get (local.get $msg) (i64.const 0))
                local.set $iface0
                (call $get (local.get $msg) (i64.const 1))
                local.set $iface1
                (call $get (local.get $msg) (i64.const 2))
                local.set $iface2
                (call $get (local.get $msg) (i64.const 3))
                local.set $iface3
                (call $get (local.get $msg) (i64.const 4))
                local.set $disc
                (call $get (local.get $msg) (i64.const 5))
                local.set $payload

                (local.get $iface0)
                (i64.const 1)
                i64.eq
                (local.get $iface1)
                (i64.const 0)
                i64.eq
                i32.and
                (local.get $iface2)
                (i64.const 0)
                i64.eq
                i32.and
                (local.get $iface3)
                (i64.const 0)
                i64.eq
                i32.and
                if
                    (local.get $disc)
                    (i64.const 1)
                    i64.eq
                    if
                        (local.get $handler_mode)
                        (i64.const 1)
                        i64.eq
                        if
                            (local.get $payload)
                            local.set $resp
                        else
                            (i64.const 5)
                            (local.get $payload)
                            i64.mul
                            local.set $resp
                        end
                    else
                        (local.get $handler_mode)
                        (i64.const 1)
                        i64.eq
                        if
                            (local.get $payload)
                            local.set $resp
                        else
                            (local.get $payload)
                            (i64.const 0)
                            i64.eq
                            if
                                (i64.const 1)
                                local.set $resp
                            else
                                (i64.const 0)
                                local.set $resp
                            end
                        end
                    end

                    (call $new_ref (i64.const 1))
                    local.set $req
                    (call $ref_push (local.get $resp))
                    (call $resume (local.get $caller) (local.get $req))
                    local.set $caller
                    local.set $msg
                    br $loop3
                end

                (local.get $iface0)
                (i64.const 2)
                i64.ne
                if unreachable end
                (local.get $iface1)
                (i64.const 0)
                i64.ne
                if unreachable end
                (local.get $iface2)
                (i64.const 0)
                i64.ne
                if unreachable end
                (local.get $iface3)
                (i64.const 0)
                i64.ne
                if unreachable end
                (local.get $disc)
                (i64.const 3)
                i64.ne
                if unreachable end
                (local.get $payload)
                (i64.const 1)
                i64.ne
                if unreachable end
                br $done3
            )
        )

        ;; inner handlers for abi_call3(true)
        (call $install_handler (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0))
        (i64.const 1)
        local.set $handler_mode

        (call $new_ref (i64.const 6))
        local.set $req
        (call $ref_push (i64.const 2))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 0))
        (call $ref_push (i64.const 3))
        (call $ref_push (i64.const 1))

        (call $resume (i64.const 0) (local.get $req))
        local.set $caller
        local.set $msg

        (block $done4
            (loop $loop4
                (call $get (local.get $msg) (i64.const 0))
                local.set $iface0
                (call $get (local.get $msg) (i64.const 1))
                local.set $iface1
                (call $get (local.get $msg) (i64.const 2))
                local.set $iface2
                (call $get (local.get $msg) (i64.const 3))
                local.set $iface3
                (call $get (local.get $msg) (i64.const 4))
                local.set $disc
                (call $get (local.get $msg) (i64.const 5))
                local.set $payload

                (local.get $iface0)
                (i64.const 1)
                i64.eq
                (local.get $iface1)
                (i64.const 0)
                i64.eq
                i32.and
                (local.get $iface2)
                (i64.const 0)
                i64.eq
                i32.and
                (local.get $iface3)
                (i64.const 0)
                i64.eq
                i32.and
                if
                    (local.get $disc)
                    (i64.const 1)
                    i64.eq
                    if
                        (local.get $handler_mode)
                        (i64.const 1)
                        i64.eq
                        if
                            (local.get $payload)
                            local.set $resp
                        else
                            (i64.const 5)
                            (local.get $payload)
                            i64.mul
                            local.set $resp
                        end
                    else
                        (local.get $handler_mode)
                        (i64.const 1)
                        i64.eq
                        if
                            (local.get $payload)
                            local.set $resp
                        else
                            (local.get $payload)
                            (i64.const 0)
                            i64.eq
                            if
                                (i64.const 1)
                                local.set $resp
                            else
                                (i64.const 0)
                                local.set $resp
                            end
                        end
                    end

                    (call $new_ref (i64.const 1))
                    local.set $req
                    (call $ref_push (local.get $resp))
                    (call $resume (local.get $caller) (local.get $req))
                    local.set $caller
                    local.set $msg
                    br $loop4
                end

                (local.get $iface0)
                (i64.const 2)
                i64.ne
                if unreachable end
                (local.get $iface1)
                (i64.const 0)
                i64.ne
                if unreachable end
                (local.get $iface2)
                (i64.const 0)
                i64.ne
                if unreachable end
                (local.get $iface3)
                (i64.const 0)
                i64.ne
                if unreachable end
                (local.get $disc)
                (i64.const 3)
                i64.ne
                if unreachable end
                (local.get $payload)
                (i64.const 1)
                i64.ne
                if unreachable end
                br $done4
            )
        )

        (call $uninstall_handler (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0))
        (i64.const 0)
        local.set $handler_mode

        (call $uninstall_handler (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0))
    )
)
"#,
        utxo_hash_limb_a = utxo_hash_limb_a,
        utxo_hash_limb_b = utxo_hash_limb_b,
        utxo_hash_limb_c = utxo_hash_limb_c,
        utxo_hash_limb_d = utxo_hash_limb_d,
    );
    let coord_bin = parse_str(&coord_wat).unwrap();

    let programs = vec![utxo_bin, coord_bin.clone()];

    let tx = UnprovenTransaction {
        inputs: vec![],
        programs,
        is_utxo: vec![true, false],
        entrypoint: 1,
    };

    let proven_tx = tx.prove().unwrap();

    let ledger = Ledger::new();

    let ledger = ledger.apply_transaction(&proven_tx).unwrap();

    assert_eq!(ledger.utxos.len(), 1);
}
