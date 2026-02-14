use sha2::{Digest, Sha256};
use starstream_interleaving_spec::{Ledger, UtxoId, Value};
use starstream_runtime::{
    UnprovenTransaction, register_mermaid_default_decoder, register_mermaid_process_labels,
    test_support::wasm_dsl, wasm_module,
};

fn hash_program(wasm: &Vec<u8>) -> (i64, i64, i64, i64) {
    let mut hasher = Sha256::new();
    hasher.update(wasm);
    let hash_bytes = hasher.finalize();
    let mut limbs = [0u64; 4];
    for (i, limb) in limbs.iter_mut().enumerate() {
        let start = i * 8;
        let end = start + 8;
        *limb = u64::from_le_bytes(hash_bytes[start..end].try_into().unwrap());
    }
    (
        limbs[0] as i64,
        limbs[1] as i64,
        limbs[2] as i64,
        limbs[3] as i64,
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

    let mut token_builder = wasm_dsl::ModuleBuilder::new();
    token_builder.add_global_i64(-1, true);
    let token_bin = wasm_module!(token_builder, {
        let uninit = const(-1);
        let curr = global_get 0;
        if curr == uninit {
            let (init_ref, _init_caller) = call init();
            let (amt, _b0, _c0, _d0) = call ref_get(init_ref, 0);
            set_global 0 = amt;
        }
        let (req, _caller_id) = call activation();

        loop {
            let (disc, arg, _b, _c) = call ref_get(req, 0);

            if disc == 102 {
                call bind(arg);
            }

            let resp = call new_ref(1);

            if disc == 101 {
                let amt = global_get 0;
                call ref_push(amt, 0, 0, 0);
            }

            if disc == 102 {
                call ref_push(0, 0, 0, 0);
            }

            call yield_(resp);
            let (next_req, _caller2) = call activation();
            set req = next_req;
            continue;
        }
    });

    let (token_hash_a, token_hash_b, token_hash_c, token_hash_d) = hash_program(&token_bin);

    let coord_swap_bin = wasm_module!({
        let utxo_id = const(0);
        let token_y_id = const(1);
        let token_x_id = const(2);

        // start_swap
        let start = call new_ref(1);
        call ref_push(1, 0, 0, 0);
        let (resp_start, caller) = call resume(utxo_id, start);
        let caller_next = caller;

        // read token_y amount
        let get_amt = call new_ref(1);
        call ref_push(101, 0, 0, 0);
        let (resp_amt, _caller_amt) = call resume(token_y_id, get_amt);
        let (dy, _b0, _c0, _d0) = call ref_get(resp_amt, 0);

        // add_token(token_y_id, dy)
        let add = call new_ref(1);
        call ref_push(2, token_y_id, dy, 0);
        let (resp_add, caller) = call resume(caller_next, add);
        let caller_next = caller;

        // remove_token(token_x_id) -> dx
        let remove = call new_ref(1);
        call ref_push(3, token_x_id, 0, 0);
        let (resp_remove, caller) = call resume(caller_next, remove);
        let caller_next = caller;
        let (dx, _b1, _c1, _d1) = call ref_get(resp_remove, 0);

        // finalize token_x process in tx_swap without mutating it
        let read_x = call new_ref(1);
        call ref_push(101, 0, 0, 0);
        let (_resp_x, _caller_x) = call resume(token_x_id, read_x);

        // end_swap (k must match)
        let end = call new_ref(1);
        call ref_push(4, 0, 0, 0);
        let (resp_end, _caller_end) = call resume(caller_next, end);
        let (_k_val, _b2, _c2, _d2) = call ref_get(resp_end, 0);
    });

    let (coord_hash_a, coord_hash_b, coord_hash_c, coord_hash_d) = hash_program(&coord_swap_bin);

    let mut builder = wasm_dsl::ModuleBuilder::new();
    // global 0 = x, global 1 = y, global 2 = k_saved, global 3 = in_swap
    // global 4..7 = coord hash limbs
    builder.add_global_i64(10, true);
    builder.add_global_i64(20, true);
    builder.add_global_i64(0, true);
    builder.add_global_i64(0, true);
    builder.add_global_i64(coord_hash_a, false);
    builder.add_global_i64(coord_hash_b, false);
    builder.add_global_i64(coord_hash_c, false);
    builder.add_global_i64(coord_hash_d, false);
    let utxo_bin = wasm_module!(builder, {
        let (state_ref, caller_id) = call activation();
        let req = state_ref;
        let caller = caller_id;
        let caller_auth = caller_id;

        loop {
            let (disc, token_id, dy, _c) = call ref_get(req, 0);

            if disc == 1 {
                let (_ch_a, _ch_b, _ch_c, _ch_d) = call get_program_hash(caller_auth);
                let _exp_a = global_get 4;
                let _exp_b = global_get 5;
                let _exp_c = global_get 6;
                let _exp_d = global_get 7;

                set_global 3 = 1;

                let x = global_get 0;
                let y = global_get 1;
                let k = mul x, y;
                set_global 2 = k;
            }

            if disc == 2 {
                let y = global_get 1;
                let next_y = add y, dy;
                set_global 1 = next_y;
            }

            if disc == 3 {
                call unbind(token_id);
            }

            let resp = call new_ref(1);

            if disc == 1 {
                call ref_push(0, 0, 0, 0);
            }

            if disc == 2 {
                call ref_push(0, 0, 0, 0);
            }

            if disc == 0 {
                call ref_push(0, 0, 0, 0);
            }

            if disc == 3 {
                let x = global_get 0;
                let y = global_get 1;
                let k = global_get 2;
                let next_x = div k, y;
                let dx = sub x, next_x;
                set_global 0 = next_x;
                call ref_push(dx, 0, 0, 0);
            }

            if disc == 4 {
                let x = global_get 0;
                let y = global_get 1;
                let k = global_get 2;
                let k_curr = mul x, y;
                call ref_push(k_curr, 0, 0, 0);
                set_global 3 = 0;
            }

            call yield_(resp);
            let (next_req, _caller_next) = call activation();
            set req = next_req;
            continue;
        }
    });

    let (utxo_hash_a, utxo_hash_b, utxo_hash_c, utxo_hash_d) = hash_program(&utxo_bin);

    let coord_create_bin = wasm_module!({
        let init_ref = call new_ref(1);
        call ref_push(0, 0, 0, 0);

        let utxo_id = call new_utxo(
            const(utxo_hash_a),
            const(utxo_hash_b),
            const(utxo_hash_c),
            const(utxo_hash_d),
            init_ref
        );

        // create token_y with amount=5
        let token_init = call new_ref(1);
        call ref_push(5, 0, 0, 0);
        let token_y_id = call new_utxo(
            const(token_hash_a),
            const(token_hash_b),
            const(token_hash_c),
            const(token_hash_d),
            token_init
        );

        // create token_x with amount=2
        let token_x_init = call new_ref(1);
        call ref_push(2, 0, 0, 0);
        let token_x_id = call new_utxo(
            const(token_hash_a),
            const(token_hash_b),
            const(token_hash_c),
            const(token_hash_d),
            token_x_init
        );

        // pre-bind both tokens to DEX in tx_init_pool
        let bind_y = call new_ref(1);
        call ref_push(102, utxo_id, 0, 0);
        let (_resp_bind_y, _caller_bind_y) = call resume(token_y_id, bind_y);

        let bind_x = call new_ref(1);
        call ref_push(102, utxo_id, 0, 0);
        let (_resp_bind_x, _caller_bind_x) = call resume(token_x_id, bind_x);

        // finalize token_y once in tx_init_pool without changing state
        let read_y = call new_ref(1);
        call ref_push(101, 0, 0, 0);
        let (_resp_read_y, _caller_read_y) = call resume(token_y_id, read_y);

        // finalize token_x once in tx_init_pool without changing state
        let read_x = call new_ref(1);
        call ref_push(101, 0, 0, 0);
        let (_resp_read_x, _caller_read_x) = call resume(token_x_id, read_x);

        // finalize DEX once in tx_init_pool without changing state
        let noop = call new_ref(1);
        call ref_push(0, 0, 0, 0);
        let (_resp_noop, _caller_noop) = call resume(utxo_id, noop);
    });

    print_wat("dex/token", &token_bin);
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
        } else if entry.state.globals.len() == 1 && entry.state.globals[0] == Value(5) {
            token_y_id = Some(id.clone());
        } else if entry.state.globals.len() == 1 && entry.state.globals[0] == Value(2) {
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
        .filter(|u| u.state.globals.len() == 1)
        .collect();
    assert_eq!(tokens.len(), 2);
    let mut amounts: Vec<_> = tokens.iter().map(|u| u.state.globals[0]).collect();
    amounts.sort_by_key(|v| v.0);
    assert_eq!(amounts, vec![Value(2), Value(5)]);
}
