use starstream_interleaving_spec::{Hash, InterfaceId, Ledger};
use starstream_runtime::{
    UnprovenTransaction, poseidon_program_hash, register_mermaid_decoder, test_support::wasm_dsl,
    wasm_module,
};
use std::marker::PhantomData;

fn interface_id(a: u64, b: u64, c: u64, d: u64) -> InterfaceId {
    Hash([a, b, c, d], PhantomData)
}

#[test]
fn test_runtime_simple_effect_handlers() {
    register_mermaid_decoder(interface_id(1, 0, 0, 0), |values| {
        let v0 = values.first()?.0;
        Some(format!("val={v0}"))
    });
    let mut utxo_builder = wasm_dsl::ModuleBuilder::new();
    utxo_builder.add_global_i64(0, true); // g0: pc
    utxo_builder.add_global_i64(0, true); // g1: req ref
    let utxo_bin = wasm_module!(utxo_builder, {
        let pc = global_get 0;
        if pc == 0 {
            let (_init_ref, caller) = call untraced_activation();
            call trace(8, 0, _init_ref, 0, caller, 0, 0, 0);

            let (caller_hash_a, caller_hash_b, caller_hash_c, caller_hash_d) = call get_program_hash(caller);
            call trace(15, caller, 0, 0, caller_hash_a, caller_hash_b, caller_hash_c, caller_hash_d);
            let (script_hash_a, script_hash_b, script_hash_c, script_hash_d) = call get_program_hash(1);
            call trace(15, 1, 0, 0, script_hash_a, script_hash_b, script_hash_c, script_hash_d);

            assert_eq caller_hash_a, script_hash_a;
            assert_eq caller_hash_b, script_hash_b;
            assert_eq caller_hash_c, script_hash_c;
            assert_eq caller_hash_d, script_hash_d;

            let req = call new_ref(1);
            call trace(10, 0, 0, req, 1, 0, 0, 0);
            call ref_push(42, 0, 0, 0);
            call trace(11, 42, 0, 0, 0, 0, 0, 0);
            set_global 1 = req;
            set_global 0 = 1;
            call call_effect_handler(1, 0, 0, 0, req);
        }
        if pc == 1 {
            let req = global_get 1;
            let (resp, _caller_effect) = call untraced_activation();
            call trace(18, 0, req, resp, 1, 0, 0, 0);
            let (resp_val, _b, _c, _d) = call ref_get(resp, 0);
            call trace(12, resp_val, resp, _b, 0, _c, _d, 0);
            assert_eq resp_val, 1;
            set_global 0 = 2;
            call trace(1, 0, resp, 0, 0, 0, 0, 0);
            call yield_(resp);
        }
    });

    let (utxo_hash_limb_a, utxo_hash_limb_b, utxo_hash_limb_c, utxo_hash_limb_d) =
        hash_program(&utxo_bin);

    let mut coord_builder = wasm_dsl::ModuleBuilder::new();
    coord_builder.add_global_i64(0, true); // g0: pc
    coord_builder.add_global_i64(0, true); // g1: init/resp ref
    let coord_bin = wasm_module!(coord_builder, {
        let pc = global_get 0;
        if pc == 0 {
            call install_handler(1, 0, 0, 0);
            call trace(4, 0, 0, 0, 1, 0, 0, 0);

            let init_val = call new_ref(1);
            call trace(10, 0, 0, init_val, 1, 0, 0, 0);
            call ref_push(0, 0, 0, 0);
            call trace(11, 0, 0, 0, 0, 0, 0, 0);
            set_global 1 = init_val;

            let utxo_id = call new_utxo(
                const(utxo_hash_limb_a),
                const(utxo_hash_limb_b),
                const(utxo_hash_limb_c),
                const(utxo_hash_limb_d),
                init_val
            );
            call trace(
                2,
                utxo_id,
                init_val,
                0,
                const(utxo_hash_limb_a),
                const(utxo_hash_limb_b),
                const(utxo_hash_limb_c),
                const(utxo_hash_limb_d)
            );

            set_global 0 = 1;
            call resume(0, init_val);
        }
        if pc == 1 {
            let init_val = global_get 1;
            let (req, caller_pid) = call untraced_activation();
            let caller_pid_enc = add caller_pid, 1;
            call trace(0, 0, init_val, req, caller_pid_enc, 0, 0, 0);
            let (req_val, _b, _c, _d) = call ref_get(req, 0);
            call trace(12, req_val, req, _b, 0, _c, _d, 0);
            assert_eq req_val, 42;

            let resp = call new_ref(1);
            call trace(10, 0, 0, resp, 1, 0, 0, 0);
            call ref_push(1, 0, 0, 0);
            call trace(11, 1, 0, 0, 0, 0, 0, 0);
            set_global 1 = resp;
            set_global 0 = 2;
            call resume(0, resp);
        }
        if pc == 2 {
            let resp = global_get 1;
            let (ret, caller2) = call untraced_activation();
            let caller2_enc = add caller2, 1;
            call trace(0, 0, resp, ret, caller2_enc, 0, 0, 0);
            call uninstall_handler(1, 0, 0, 0);
            call trace(5, 0, 0, 0, 1, 0, 0, 0);
            call trace(17, 0, 0, 0, 0, 0, 0, 0);
            set_global 0 = 3;
            call return_();
        }
    });

    print_wat("simple/utxo", &utxo_bin);
    print_wat("simple/coord", &coord_bin);

    let programs = vec![utxo_bin, coord_bin.clone()];

    let tx = UnprovenTransaction {
        inputs: vec![],
        input_states: vec![],
        input_ownership: vec![],
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
fn test_runtime_effect_handlers_cross_calls() {
    register_mermaid_decoder(interface_id(1, 2, 3, 4), |values| {
        let disc = values.first()?.0;
        let v1 = values.get(1).map(|v| v.0).unwrap_or(0);
        let label = match disc {
            1 => format!("disc=forward num_ref={v1}"),
            2 => format!("disc=stop num_ref={v1}"),
            _ => return None,
        };
        Some(label)
    });
    // this test emulates a coordination script acting as a middle-man for a channel-like flow
    //
    // utxo1 sends numbers, by encoding the request as (1, arg)
    //
    // the coord script recognizes 1 as a request to forward the message (like
    // an enum), and sends the arg utxo2.
    //
    // utxo2 gets the new message, and answers with x+1
    //
    // coord manages the hand-out of that value to utxo1 again
    //
    // TODO:
    //   - each coroutine allocates a new ref each time, this is not as efficient
    //   - coord should check that the answer it receives actually comes from
    //     the right process (or maybe this should be an optional arg to resume and be enforced by the circuit?)
    let mut utxo1_builder = wasm_dsl::ModuleBuilder::new();
    utxo1_builder.add_global_i64(0, true); // g0: pc
    utxo1_builder.add_global_i64(99, true); // g1: x
    utxo1_builder.add_global_i64(0, true); // g2: i
    utxo1_builder.add_global_i64(0, true); // g3: last req ref
    let utxo1_bin = wasm_module!(utxo1_builder, {
        let pc = global_get 0;

        // pc=0: boot once, build first forward request, then hand control to coord.
        if pc == 0 {
            let (init_ref, caller) = call untraced_activation();
            // 8=Activation: a1=init_ref, a3=caller pid.
            call trace(8, 0, init_ref, 0, caller, 0, 0, 0);

            let x = global_get 1;
            let num_ref = call new_ref(1);
            // 10=NewRef: a2=ret ref id, a3=size_words.
            call trace(10, 0, 0, num_ref, 1, 0, 0, 0);
            call ref_push(x, 0, 0, 0);
            // 11=RefPush: a0..a3=packed pushed values.
            call trace(11, x, 0, 0, 0, 0, 0, 0);

            let req = call new_ref(1);
            // 10=NewRef: request envelope ref.
            call trace(10, 0, 0, req, 1, 0, 0, 0);
            call ref_push(1, num_ref, 0, 0);
            // 11=RefPush payload: [disc=1 (forward), num_ref, 0, 0].
            call trace(11, 1, num_ref, 0, 0, 0, 0, 0);
            set_global 3 = req;
            set_global 0 = 1;
            call call_effect_handler(1, 2, 3, 4, req);
        }

        // pc=1: resumed by coord with a response. Validate y=x+1 and either
        // send next forward request or send a stop request after 5 rounds.
        if pc == 1 {
            let req = global_get 3;
            let (resp, _caller_effect) = call untraced_activation();
            // 18=CallEffectHandler: a1=req, a2=resp, a3..a6=interface id.
            call trace(18, 0, req, resp, 1, 2, 3, 4);
            let (y, _b, _c, _d) = call ref_get(resp, 0);
            // 12=RefGet: a0=first lane read (y), a1=ref, a3=offset.
            call trace(12, y, resp, _b, 0, _c, _d, 0);

            let x = global_get 1;
            let expected = add x, 1;
            assert_eq y, expected;
            set_global 1 = y;

            let i = global_get 2;
            let i_next = add i, 1;
            set_global 2 = i_next;

            // `i` is the pre-increment round index (old value before i_next write).
            // i==4 means this is the 5th response, so emit final `disc=2` stop.
            // This branch is intentionally first to make the terminal path explicit.
            if i == 4 {
                let stop_num_ref = call new_ref(1);
                // 10=NewRef for stop payload value.
                call trace(10, 0, 0, stop_num_ref, 1, 0, 0, 0);
                call ref_push(y, 0, 0, 0);
                // 11=RefPush payload value [y,0,0,0].
                call trace(11, y, 0, 0, 0, 0, 0, 0);
                let stop = call new_ref(1);
                // 10=NewRef for stop request envelope.
                call trace(10, 0, 0, stop, 1, 0, 0, 0);
                call ref_push(2, stop_num_ref, 0, 0);
                // 11=RefPush payload: [disc=2 (stop), num_ref, 0, 0].
                call trace(11, 2, stop_num_ref, 0, 0, 0, 0, 0);
                set_global 3 = stop;
                set_global 0 = 2;
                call call_effect_handler(1, 2, 3, 4, stop);
            }

            // i<4 are the normal forward rounds (disc=1).
            if i < 4 {
                let num_ref = call new_ref(1);
                // 10=NewRef for next forwarded value.
                call trace(10, 0, 0, num_ref, 1, 0, 0, 0);
                call ref_push(y, 0, 0, 0);
                // 11=RefPush payload value [y,0,0,0].
                call trace(11, y, 0, 0, 0, 0, 0, 0);
                let next_req = call new_ref(1);
                // 10=NewRef for next request envelope.
                call trace(10, 0, 0, next_req, 1, 0, 0, 0);
                call ref_push(1, num_ref, 0, 0);
                // 11=RefPush payload: [disc=1 (forward), num_ref, 0, 0].
                call trace(11, 1, num_ref, 0, 0, 0, 0, 0);
                set_global 3 = next_req;
                set_global 0 = 1;
                call call_effect_handler(1, 2, 3, 4, next_req);
            }
        }

        // pc=2: ack the stop response and yield final control back to coord.
        if pc == 2 {
            let stop = global_get 3;
            let (resp_stop, _caller_effect) = call untraced_activation();
            // 18=CallEffectHandler completion for stop request.
            call trace(18, 0, stop, resp_stop, 1, 2, 3, 4);
            // 1=Yield: a1=yielded ref.
            call trace(1, 0, stop, 0, 0, 0, 0, 0);
            set_global 0 = 3;
            call yield_(stop);
        }
    });

    let mut utxo2_builder = wasm_dsl::ModuleBuilder::new();
    utxo2_builder.add_global_i64(0, true); // g0: pc
    utxo2_builder.add_global_i64(0, true); // g1: current req ref
    let utxo2_bin = wasm_module!(utxo2_builder, {
        let pc = global_get 0;
        // pc=0: first activation, process forwarded value, write y=x+1, yield response.
        if pc == 0 {
            let (init_ref, caller) = call untraced_activation();
            // 8=Activation: a1=activation ref, a3=caller pid.
            call trace(8, 0, init_ref, 0, caller, 0, 0, 0);
            set_global 1 = init_ref;

            let req = init_ref;
            let (x, _b, _c, _d) = call ref_get(req, 0);
            // 12=RefGet first lane read from request value ref.
            call trace(12, x, req, _b, 0, _c, _d, 0);
            let y = add x, 1;
            call ref_write(req, 0, y, 0, 0, 0);
            // 16=RefWrite: a0=y, a1=ref, a3=offset.
            call trace(16, y, req, 0, 0, 0, 0, 0);
            // 1=Yield returning same ref as response.
            call trace(1, 0, req, 0, 0, 0, 0, 0);
            set_global 0 = 1;
            call yield_(req);
        }
        // pc=1: steady-state loop for all following forwarded requests.
        if pc == 1 {
            let (next_req, caller2) = call untraced_activation();
            // 8=Activation for subsequent forwarded request.
            call trace(8, 0, next_req, 0, caller2, 0, 0, 0);
            set_global 1 = next_req;

            let req = next_req;
            let (x, _b, _c, _d) = call ref_get(req, 0);
            // 12=RefGet first lane read from request value ref.
            call trace(12, x, req, _b, 0, _c, _d, 0);
            let y = add x, 1;
            call ref_write(req, 0, y, 0, 0, 0);
            // 16=RefWrite writes response value in-place.
            call trace(16, y, req, 0, 0, 0, 0, 0);
            // 1=Yield response ref to coordinator.
            call trace(1, 0, req, 0, 0, 0, 0, 0);
            call yield_(req);
        }
    });

    let (utxo1_hash_limb_a, utxo1_hash_limb_b, utxo1_hash_limb_c, utxo1_hash_limb_d) =
        hash_program(&utxo1_bin);

    let (utxo2_hash_limb_a, utxo2_hash_limb_b, utxo2_hash_limb_c, utxo2_hash_limb_d) =
        hash_program(&utxo2_bin);

    let mut coord_builder = wasm_dsl::ModuleBuilder::new();
    coord_builder.add_global_i64(0, true); // g0: pc
    coord_builder.add_global_i64(0, true); // g1: utxo1 id
    coord_builder.add_global_i64(0, true); // g2: utxo2 id
    coord_builder.add_global_i64(0, true); // g3: req/current ref
    coord_builder.add_global_i64(0, true); // g4: caller1
    coord_builder.add_global_i64(0, true); // g5: last resume target
    coord_builder.add_global_i64(0, true); // g6: last resume val
    let coord_bin = wasm_module!(coord_builder, {
        let pc = global_get 0;
        // pc=0: install handler, spawn both utxos, resume utxo1 with init value.
        if pc == 0 {
            call install_handler(1, 2, 3, 4);
            // 4=InstallHandler: a3..a6=interface id.
            call trace(4, 0, 0, 0, 1, 2, 3, 4);

            let init_val = call new_ref(1);
            // 10=NewRef init payload for boot resume.
            call trace(10, 0, 0, init_val, 1, 0, 0, 0);
            call ref_push(0, 0, 0, 0);
            // 11=RefPush payload [0,0,0,0].
            call trace(11, 0, 0, 0, 0, 0, 0, 0);

            let utxo_id1 = call new_utxo(
                const(utxo1_hash_limb_a),
                const(utxo1_hash_limb_b),
                const(utxo1_hash_limb_c),
                const(utxo1_hash_limb_d),
                init_val
            );
            // 2=NewUtxo: a0=new pid, a1=init ref, a3..a6=program hash.
            call trace(
                2,
                utxo_id1,
                init_val,
                0,
                const(utxo1_hash_limb_a),
                const(utxo1_hash_limb_b),
                const(utxo1_hash_limb_c),
                const(utxo1_hash_limb_d)
            );

            let utxo_id2 = call new_utxo(
                const(utxo2_hash_limb_a),
                const(utxo2_hash_limb_b),
                const(utxo2_hash_limb_c),
                const(utxo2_hash_limb_d),
                init_val
            );
            // 2=NewUtxo for the second worker.
            call trace(
                2,
                utxo_id2,
                init_val,
                0,
                const(utxo2_hash_limb_a),
                const(utxo2_hash_limb_b),
                const(utxo2_hash_limb_c),
                const(utxo2_hash_limb_d)
            );

            set_global 1 = utxo_id1;
            set_global 2 = utxo_id2;
            set_global 5 = utxo_id1;
            set_global 6 = init_val;
            set_global 0 = 1;
            call resume(utxo_id1, init_val);
        }

        // pc=1: receive request from utxo1.
        // disc=1 => forward num_ref to utxo2.
        // disc=2 => stop flow and resume utxo1 with stop payload.
        if pc == 1 {
            let last_target = global_get 5;
            let last_val = global_get 6;
            let (req, caller0) = call untraced_activation();
            let caller0_enc = add caller0, 1;
            // 0=Resume: a0=target, a1=val, a2=ret, a3=encoded caller.
            call trace(0, last_target, last_val, req, caller0_enc, 0, 0, 0);
            set_global 3 = req;
            set_global 4 = caller0;

            let (disc, num_ref, _c, _d) = call ref_get(req, 0);
            // 12=RefGet: decode request envelope [disc, num_ref, ...].
            call trace(12, disc, req, num_ref, 0, _c, _d, 0);

            if disc == 2 {
                let caller1 = global_get 4;
                set_global 5 = caller1;
                set_global 6 = num_ref;
                set_global 0 = 3;
                call resume(caller1, num_ref);
            }
            if disc == 1 {
                let utxo_id2 = global_get 2;
                set_global 5 = utxo_id2;
                set_global 6 = num_ref;
                set_global 0 = 2;
                call resume(utxo_id2, num_ref);
            }
        }

        // pc=2: receive utxo2 response and route it back to utxo1.
        if pc == 2 {
            let last_target = global_get 5;
            let last_val = global_get 6;
            let (resp2, caller2) = call untraced_activation();
            let caller2_enc = add caller2, 1;
            // 0=Resume return from utxo2.
            call trace(0, last_target, last_val, resp2, caller2_enc, 0, 0, 0);

            let caller1 = global_get 4;
            set_global 5 = caller1;
            set_global 6 = resp2;
            set_global 0 = 1;
            call resume(caller1, resp2);
        }

        // pc=3: receive terminal ack, uninstall handler, and return.
        if pc == 3 {
            let last_target = global_get 5;
            let last_val = global_get 6;
            let (ret_stop, caller_stop) = call untraced_activation();
            let caller_stop_enc = add caller_stop, 1;
            // 0=Resume return from final stop handoff.
            call trace(0, last_target, last_val, ret_stop, caller_stop_enc, 0, 0, 0);

            call uninstall_handler(1, 2, 3, 4);
            // 5=UninstallHandler for the effect interface.
            call trace(5, 0, 0, 0, 1, 2, 3, 4);
            // 17=Return from coordinator.
            call trace(17, 0, 0, 0, 0, 0, 0, 0);
            set_global 0 = 4;
            call return_();
        }
    });

    print_wat("cross/utxo1", &utxo1_bin);
    print_wat("cross/utxo2", &utxo2_bin);
    print_wat("cross/coord", &coord_bin);

    let programs = vec![utxo1_bin.clone(), utxo2_bin.clone(), coord_bin.clone()];

    let tx = UnprovenTransaction {
        inputs: vec![],
        input_states: vec![],
        input_ownership: vec![],
        programs,
        is_utxo: vec![true, true, false],
        entrypoint: 2,
    };

    let proven_tx = tx.prove().unwrap();

    let ledger = Ledger::new();

    let ledger = ledger.apply_transaction(&proven_tx).unwrap();

    assert_eq!(ledger.utxos.len(), 2);
}

fn hash_program(utxo_bin: &Vec<u8>) -> (i64, i64, i64, i64) {
    let limbs = poseidon_program_hash(utxo_bin);
    let utxo_hash_limb_a = limbs[0] as i64;
    let utxo_hash_limb_b = limbs[1] as i64;
    let utxo_hash_limb_c = limbs[2] as i64;
    let utxo_hash_limb_d = limbs[3] as i64;

    (
        utxo_hash_limb_a,
        utxo_hash_limb_b,
        utxo_hash_limb_c,
        utxo_hash_limb_d,
    )
}

fn print_wat(name: &str, wasm: &[u8]) {
    if std::env::var_os("DEBUG_WAT").is_none() {
        return;
    }

    match wasmprinter::print_bytes(wasm) {
        Ok(wat) => eprintln!("--- WAT: {name} ---\n{wat}"),
        Err(err) => eprintln!("--- WAT: {name} (failed: {err}) ---"),
    }
}
