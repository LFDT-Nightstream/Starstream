use starstream_interleaving_spec::{Ledger, UtxoId, Value};
use starstream_runtime::{
    UnprovenTransaction, poseidon_program_hash, register_mermaid_default_decoder,
    register_mermaid_process_labels, test_support::wasm_dsl, wasm_module,
};

fn hash_program(wasm: &Vec<u8>) -> (i64, i64, i64, i64) {
    let limbs = poseidon_program_hash(wasm);
    (
        limbs[0] as i64,
        limbs[1] as i64,
        limbs[2] as i64,
        limbs[3] as i64,
    )
}

fn print_wat(name: &str, wasm: &[u8]) {
    if std::env::var_os("DEBUG_COMPONENTS").is_none() {
        return;
    }

    match wasmprinter::print_bytes(wasm) {
        Ok(wat) => eprintln!("--- WAT: {name} ---\n{wat}"),
        Err(err) => eprintln!("--- WAT: {name} (failed: {err}) ---"),
    }
}

fn print_component_wit(name: &str, wasm: &[u8]) {
    if std::env::var_os("DEBUG_COMPONENTS").is_none() {
        return;
    }

    match wit_component::decode(wasm) {
        Ok(decoded) => {
            let mut printer = wit_component::WitPrinter::default();
            match printer.print(decoded.resolve(), decoded.package(), &[]) {
                Ok(()) => eprintln!("--- WIT: {name} ---\n{}", printer.output),
                Err(err) => eprintln!("--- WIT: {name} (print failed: {err}) ---"),
            }
        }
        Err(err) => eprintln!("--- WIT: {name} (decode failed: {err}) ---"),
    }
}

#[test]
fn test_dex_swap_flow() {
    register_mermaid_default_decoder(|values| {
        let disc = values.first()?.0;
        let arg1 = values.get(1).map(|v| v.0).unwrap_or(0);
        let arg2 = values.get(2).map(|v| v.0).unwrap_or(0);
        let label = match disc {
            1 => "start_swap".to_string(),
            2 => format!("add_token token_id={arg1} dy={arg2}"),
            3 => format!("remove_token token_id={arg1}"),
            4 => "end_swap".to_string(),
            101 => "token_get_amount".to_string(),
            102 => format!("token_bind owner={arg1}"),
            _ => return None,
        };
        Some(label)
    });

    let token_builder = wasm_dsl::ModuleBuilder::new();
    let token_bin = wasm_module!(token_builder, {
        let initialized = call get_datum(1);
        if initialized == 0 {
            let (init_ref, init_caller) = call init();
            call trace(9, 0, init_ref, 0, init_caller, 0, 0, 0);
            let (amt, _b0, _c0, _d0) = call ref_get(init_ref, 0);
            call trace(12, amt, init_ref, _b0, 0, _c0, _d0, 0);
            call set_datum(0, amt);
            call set_datum(1, 1);
        }
        let (req, caller_id) = call activation();
        call trace(8, 0, req, 0, caller_id, 0, 0, 0);
        let (disc, arg, _b, _c) = call ref_get(req, 0);
        call trace(12, disc, req, arg, 0, _b, _c, 0);

        if disc == 102 {
            call bind(arg);
            call trace(13, arg, 0, 0, 0, 0, 0, 0);
        }

        let resp = call new_ref(1);
        call trace(10, 0, 0, resp, 1, 0, 0, 0);

        if disc == 101 {
            let amt = call get_datum(0);
            call ref_push(amt, 0, 0, 0);
            call trace(11, amt, 0, 0, 0, 0, 0, 0);
        }

        if disc == 102 {
            call ref_push(0, 0, 0, 0);
            call trace(11, 0, 0, 0, 0, 0, 0, 0);
        }

        call trace(1, 0, resp, 0, 0, 0, 0, 0);
        call yield_(resp);
    });

    let (token_hash_a, token_hash_b, token_hash_c, token_hash_d) = hash_program(&token_bin);

    let coord_swap_builder = wasm_dsl::ModuleBuilder::new();
    let coord_swap_bin = wasm_module!(coord_swap_builder, {
        let pc = call get_datum(0);
        let utxo_id = const(0);
        let token_y_id = const(1);
        let token_x_id = const(2);

        if pc == 0 {
            let start = call new_ref(1);
            call trace(10, 0, 0, start, 1, 0, 0, 0);
            call ref_push(1, 0, 0, 0);
            call trace(11, 1, 0, 0, 0, 0, 0, 0);
            call set_datum(3, utxo_id);
            call set_datum(4, start);
            call set_datum(0, 1);
            call resume(utxo_id, start);
        }
        if pc == 1 {
            let last_target = call get_datum(3);
            let last_val = call get_datum(4);
            let (_resp_start, caller) = call untraced_activation();
            let caller_enc = add caller, 1;
            call trace(0, last_target, last_val, _resp_start, caller_enc, 0, 0, 0);
            call set_datum(1, caller);

            let get_amt = call new_ref(1);
            call trace(10, 0, 0, get_amt, 1, 0, 0, 0);
            call ref_push(101, 0, 0, 0);
            call trace(11, 101, 0, 0, 0, 0, 0, 0);
            call set_datum(3, token_y_id);
            call set_datum(4, get_amt);
            call set_datum(0, 2);
            call resume(token_y_id, get_amt);
        }
        if pc == 2 {
            let last_target = call get_datum(3);
            let last_val = call get_datum(4);
            let (resp_amt, caller_amt) = call untraced_activation();
            let caller_amt_enc = add caller_amt, 1;
            call trace(0, last_target, last_val, resp_amt, caller_amt_enc, 0, 0, 0);
            let (dy, _b0, _c0, _d0) = call ref_get(resp_amt, 0);
            call trace(12, dy, resp_amt, _b0, 0, _c0, _d0, 0);
            call set_datum(2, dy);

            let caller_next = call get_datum(1);
            let add = call new_ref(1);
            call trace(10, 0, 0, add, 1, 0, 0, 0);
            call ref_push(2, token_y_id, dy, 0);
            call trace(11, 2, token_y_id, dy, 0, 0, 0, 0);
            call set_datum(3, caller_next);
            call set_datum(4, add);
            call set_datum(0, 3);
            call resume(caller_next, add);
        }
        if pc == 3 {
            let last_target = call get_datum(3);
            let last_val = call get_datum(4);
            let (_resp_add, caller) = call untraced_activation();
            let caller_enc = add caller, 1;
            call trace(0, last_target, last_val, _resp_add, caller_enc, 0, 0, 0);
            call set_datum(1, caller);

            let remove = call new_ref(1);
            call trace(10, 0, 0, remove, 1, 0, 0, 0);
            call ref_push(3, token_x_id, 0, 0);
            call trace(11, 3, token_x_id, 0, 0, 0, 0, 0);
            call set_datum(3, caller);
            call set_datum(4, remove);
            call set_datum(0, 4);
            call resume(caller, remove);
        }
        if pc == 4 {
            let last_target = call get_datum(3);
            let last_val = call get_datum(4);
            let (resp_remove, caller) = call untraced_activation();
            let caller_enc = add caller, 1;
            call trace(0, last_target, last_val, resp_remove, caller_enc, 0, 0, 0);
            call set_datum(1, caller);
            let (dx, _b1, _c1, _d1) = call ref_get(resp_remove, 0);
            call trace(12, dx, resp_remove, _b1, 0, _c1, _d1, 0);

            let read_x = call new_ref(1);
            call trace(10, 0, 0, read_x, 1, 0, 0, 0);
            call ref_push(101, 0, 0, 0);
            call trace(11, 101, 0, 0, 0, 0, 0, 0);
            call set_datum(3, token_x_id);
            call set_datum(4, read_x);
            call set_datum(0, 5);
            call resume(token_x_id, read_x);
        }
        if pc == 5 {
            let last_target = call get_datum(3);
            let last_val = call get_datum(4);
            let (_resp_x, caller_x) = call untraced_activation();
            let caller_x_enc = add caller_x, 1;
            call trace(0, last_target, last_val, _resp_x, caller_x_enc, 0, 0, 0);

            let caller_next = call get_datum(1);
            let end = call new_ref(1);
            call trace(10, 0, 0, end, 1, 0, 0, 0);
            call ref_push(4, 0, 0, 0);
            call trace(11, 4, 0, 0, 0, 0, 0, 0);
            call set_datum(3, caller_next);
            call set_datum(4, end);
            call set_datum(0, 6);
            call resume(caller_next, end);
        }
        if pc == 6 {
            let last_target = call get_datum(3);
            let last_val = call get_datum(4);
            let (resp_end, caller_end) = call untraced_activation();
            let caller_end_enc = add caller_end, 1;
            call trace(0, last_target, last_val, resp_end, caller_end_enc, 0, 0, 0);
            let (_k_val, _b2, _c2, _d2) = call ref_get(resp_end, 0);
            call trace(12, _k_val, resp_end, _b2, 0, _c2, _d2, 0);
            call set_datum(0, 7);
            call trace(17, 0, 0, 0, 0, 0, 0, 0);
            call return_();
        }
    });

    let (coord_hash_a, coord_hash_b, coord_hash_c, coord_hash_d) = hash_program(&coord_swap_bin);

    let builder = wasm_dsl::ModuleBuilder::new();
    let utxo_bin = wasm_module!(builder, {
        // datum 0 = x, 1 = y, 2 = k_saved, 3 = in_swap
        // datum 4..7 = expected coord hash limbs, 8 = init flag
        let initialized = call get_datum(8);
        if initialized == 0 {
            call set_datum(0, 10);
            call set_datum(1, 20);
            call set_datum(2, 0);
            call set_datum(3, 0);
            call set_datum(4, const(coord_hash_a));
            call set_datum(5, const(coord_hash_b));
            call set_datum(6, const(coord_hash_c));
            call set_datum(7, const(coord_hash_d));
            call set_datum(8, 1);
        }

        let (state_ref, caller_id) = call activation();
        call trace(8, 0, state_ref, 0, caller_id, 0, 0, 0);
        let (disc, token_id, dy, _c) = call ref_get(state_ref, 0);
        call trace(12, disc, state_ref, token_id, 0, dy, _c, 0);

        if disc == 1 {
            let (_ch_a, _ch_b, _ch_c, _ch_d) = call get_program_hash(caller_id);
            call trace(15, caller_id, 0, 0, _ch_a, _ch_b, _ch_c, _ch_d);
            let _exp_a = call get_datum(4);
            let _exp_b = call get_datum(5);
            let _exp_c = call get_datum(6);
            let _exp_d = call get_datum(7);

            call set_datum(3, 1);

            let x = call get_datum(0);
            let y = call get_datum(1);
            let k = mul x, y;
            call set_datum(2, k);
        }

        if disc == 2 {
            let y = call get_datum(1);
            let next_y = add y, dy;
            call set_datum(1, next_y);
        }

        if disc == 3 {
            call unbind(token_id);
            call trace(14, token_id, 0, 0, 0, 0, 0, 0);
        }

        let resp = call new_ref(1);
        call trace(10, 0, 0, resp, 1, 0, 0, 0);

        if disc == 1 {
            call ref_push(0, 0, 0, 0);
            call trace(11, 0, 0, 0, 0, 0, 0, 0);
        }

        if disc == 2 {
            call ref_push(0, 0, 0, 0);
            call trace(11, 0, 0, 0, 0, 0, 0, 0);
        }

        if disc == 0 {
            call ref_push(0, 0, 0, 0);
            call trace(11, 0, 0, 0, 0, 0, 0, 0);
        }

        if disc == 3 {
            let x = call get_datum(0);
            let y = call get_datum(1);
            let k = call get_datum(2);
            let next_x = div k, y;
            let dx = sub x, next_x;
            call set_datum(0, next_x);
            call ref_push(dx, 0, 0, 0);
            call trace(11, dx, 0, 0, 0, 0, 0, 0);
        }

        if disc == 4 {
            let x = call get_datum(0);
            let y = call get_datum(1);
            let k = call get_datum(2);
            let k_curr = mul x, y;
            call ref_push(k_curr, 0, 0, 0);
            call trace(11, k_curr, 0, 0, 0, 0, 0, 0);
            call set_datum(3, 0);
        }

        call trace(1, 0, resp, 0, 0, 0, 0, 0);
        call yield_(resp);
    });

    let (utxo_hash_a, utxo_hash_b, utxo_hash_c, utxo_hash_d) = hash_program(&utxo_bin);

    let coord_create_builder = wasm_dsl::ModuleBuilder::new();
    let coord_create_bin = wasm_module!(coord_create_builder, {
        let pc = call get_datum(0);
        if pc == 0 {
            let init_ref = call new_ref(1);
            call trace(10, 0, 0, init_ref, 1, 0, 0, 0);
            call ref_push(0, 0, 0, 0);
            call trace(11, 0, 0, 0, 0, 0, 0, 0);

            let utxo_id = call new_utxo(
                const(utxo_hash_a),
                const(utxo_hash_b),
                const(utxo_hash_c),
                const(utxo_hash_d),
                init_ref
            );
            call trace(
                2,
                utxo_id,
                init_ref,
                0,
                const(utxo_hash_a),
                const(utxo_hash_b),
                const(utxo_hash_c),
                const(utxo_hash_d)
            );
            call set_datum(1, utxo_id);

            let token_init = call new_ref(1);
            call trace(10, 0, 0, token_init, 1, 0, 0, 0);
            call ref_push(5, 0, 0, 0);
            call trace(11, 5, 0, 0, 0, 0, 0, 0);
            let token_y_id = call new_utxo(
                const(token_hash_a),
                const(token_hash_b),
                const(token_hash_c),
                const(token_hash_d),
                token_init
            );
            call trace(
                2,
                token_y_id,
                token_init,
                0,
                const(token_hash_a),
                const(token_hash_b),
                const(token_hash_c),
                const(token_hash_d)
            );
            call set_datum(2, token_y_id);

            let token_x_init = call new_ref(1);
            call trace(10, 0, 0, token_x_init, 1, 0, 0, 0);
            call ref_push(2, 0, 0, 0);
            call trace(11, 2, 0, 0, 0, 0, 0, 0);
            let token_x_id = call new_utxo(
                const(token_hash_a),
                const(token_hash_b),
                const(token_hash_c),
                const(token_hash_d),
                token_x_init
            );
            call trace(
                2,
                token_x_id,
                token_x_init,
                0,
                const(token_hash_a),
                const(token_hash_b),
                const(token_hash_c),
                const(token_hash_d)
            );
            call set_datum(3, token_x_id);

            let bind_y = call new_ref(1);
            call trace(10, 0, 0, bind_y, 1, 0, 0, 0);
            call ref_push(102, utxo_id, 0, 0);
            call trace(11, 102, utxo_id, 0, 0, 0, 0, 0);
            call set_datum(4, token_y_id);
            call set_datum(5, bind_y);
            call set_datum(0, 1);
            call resume(token_y_id, bind_y);
        }
        if pc == 1 {
            let last_target = call get_datum(4);
            let last_val = call get_datum(5);
            let (_resp_bind_y, caller_bind_y) = call untraced_activation();
            let caller_enc = add caller_bind_y, 1;
            call trace(0, last_target, last_val, _resp_bind_y, caller_enc, 0, 0, 0);

            let utxo_id = call get_datum(1);
            let token_x_id = call get_datum(3);
            let bind_x = call new_ref(1);
            call trace(10, 0, 0, bind_x, 1, 0, 0, 0);
            call ref_push(102, utxo_id, 0, 0);
            call trace(11, 102, utxo_id, 0, 0, 0, 0, 0);
            call set_datum(4, token_x_id);
            call set_datum(5, bind_x);
            call set_datum(0, 2);
            call resume(token_x_id, bind_x);
        }
        if pc == 2 {
            let last_target = call get_datum(4);
            let last_val = call get_datum(5);
            let (_resp_bind_x, caller_bind_x) = call untraced_activation();
            let caller_enc = add caller_bind_x, 1;
            call trace(0, last_target, last_val, _resp_bind_x, caller_enc, 0, 0, 0);

            let token_y_id = call get_datum(2);
            let read_y = call new_ref(1);
            call trace(10, 0, 0, read_y, 1, 0, 0, 0);
            call ref_push(101, 0, 0, 0);
            call trace(11, 101, 0, 0, 0, 0, 0, 0);
            call set_datum(4, token_y_id);
            call set_datum(5, read_y);
            call set_datum(0, 3);
            call resume(token_y_id, read_y);
        }
        if pc == 3 {
            let last_target = call get_datum(4);
            let last_val = call get_datum(5);
            let (_resp_read_y, caller_read_y) = call untraced_activation();
            let caller_enc = add caller_read_y, 1;
            call trace(0, last_target, last_val, _resp_read_y, caller_enc, 0, 0, 0);

            let token_x_id = call get_datum(3);
            let read_x = call new_ref(1);
            call trace(10, 0, 0, read_x, 1, 0, 0, 0);
            call ref_push(101, 0, 0, 0);
            call trace(11, 101, 0, 0, 0, 0, 0, 0);
            call set_datum(4, token_x_id);
            call set_datum(5, read_x);
            call set_datum(0, 4);
            call resume(token_x_id, read_x);
        }
        if pc == 4 {
            let last_target = call get_datum(4);
            let last_val = call get_datum(5);
            let (_resp_read_x, caller_read_x) = call untraced_activation();
            let caller_enc = add caller_read_x, 1;
            call trace(0, last_target, last_val, _resp_read_x, caller_enc, 0, 0, 0);

            let utxo_id = call get_datum(1);
            let noop = call new_ref(1);
            call trace(10, 0, 0, noop, 1, 0, 0, 0);
            call ref_push(0, 0, 0, 0);
            call trace(11, 0, 0, 0, 0, 0, 0, 0);
            call set_datum(4, utxo_id);
            call set_datum(5, noop);
            call set_datum(0, 5);
            call resume(utxo_id, noop);
        }
        if pc == 5 {
            let last_target = call get_datum(4);
            let last_val = call get_datum(5);
            let (_resp_noop, caller_noop) = call untraced_activation();
            let caller_enc = add caller_noop, 1;
            call trace(0, last_target, last_val, _resp_noop, caller_enc, 0, 0, 0);
            call set_datum(0, 6);
            call trace(17, 0, 0, 0, 0, 0, 0, 0);
            call return_();
        }
    });

    print_wat("dex/token", &token_bin);
    print_component_wit("dex", &token_bin);
    print_wat("dex/utxo", &utxo_bin);
    print_wat("dex/coord_create", &coord_create_bin);
    print_wat("dex/coord_swap", &coord_swap_bin);

    register_mermaid_process_labels(vec![
        "DEX".to_string(),
        "token_x".to_string(),
        "token_y".to_string(),
        "coord".to_string(),
    ]);

    let token_bin_2 = token_bin.clone();
    let tx_init = UnprovenTransaction {
        inputs: vec![],
        input_states: vec![],
        input_ownership: vec![],
        programs: vec![
            utxo_bin.clone(),
            token_bin.clone(),
            token_bin_2.clone(),
            coord_create_bin,
        ],
        is_utxo: vec![true, true, true, false],
        entrypoint: 3,
    };

    let proven_init = tx_init.prove().unwrap();
    let mut ledger = Ledger::new();
    ledger = ledger.apply_transaction(&proven_init).unwrap();

    assert_eq!(ledger.utxos.len(), 3);
    let mut dex_id: Option<UtxoId> = None;
    let mut token_y_id: Option<UtxoId> = None;
    let mut token_x_id: Option<UtxoId> = None;
    for (id, entry) in &ledger.utxos {
        if entry.state.globals.len() >= 4 {
            dex_id = Some(id.clone());
        } else if !entry.state.globals.is_empty() && entry.state.globals[0] == Value(5) {
            token_y_id = Some(id.clone());
        } else if !entry.state.globals.is_empty() && entry.state.globals[0] == Value(2) {
            token_x_id = Some(id.clone());
        }
    }
    let dex_id = dex_id.unwrap();
    let token_y_id = token_y_id.unwrap();
    let token_x_id = token_x_id.unwrap();
    let swap_inputs = vec![dex_id.clone(), token_y_id.clone(), token_x_id.clone()];
    let swap_input_ownership = ledger.input_ownership_for_inputs(&swap_inputs);

    let tx_swap = UnprovenTransaction {
        inputs: swap_inputs,
        input_states: vec![
            ledger.utxos[&dex_id].state.clone(),
            ledger.utxos[&token_y_id].state.clone(),
            ledger.utxos[&token_x_id].state.clone(),
        ],
        input_ownership: swap_input_ownership,
        programs: vec![utxo_bin, token_bin, token_bin_2, coord_swap_bin],
        is_utxo: vec![true, true, true, false],
        entrypoint: 3,
    };

    let proven_swap = tx_swap.prove().unwrap();
    ledger = ledger.apply_transaction(&proven_swap).unwrap();

    assert_eq!(ledger.utxos.len(), 3);
    let utxos: Vec<_> = ledger.utxos.values().collect();
    let utxo = utxos.iter().find(|u| u.state.globals.len() >= 4).unwrap();
    assert_eq!(
        &utxo.state.globals[..4],
        &[Value(8), Value(25), Value(200), Value(0)]
    );
    let tokens: Vec<_> = utxos
        .iter()
        .filter(|u| !u.state.globals.is_empty() && u.state.globals.len() < 4)
        .collect();
    assert_eq!(tokens.len(), 2);
    let mut amounts: Vec<_> = tokens.iter().map(|u| u.state.globals[0]).collect();
    amounts.sort_by_key(|v| v.0);
    assert_eq!(amounts, vec![Value(2), Value(5)]);
}
