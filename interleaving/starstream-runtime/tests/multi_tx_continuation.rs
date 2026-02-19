use starstream_interleaving_spec::{Ledger, UtxoId, Value};
use starstream_runtime::{
    UnprovenTransaction, poseidon_program_hash, test_support::wasm_dsl, wasm_module,
};

fn hash_program(wasm: &Vec<u8>) -> (i64, i64, i64, i64) {
    let limbs = poseidon_program_hash(wasm);
    let a = limbs[0] as i64;
    let b = limbs[1] as i64;
    let c = limbs[2] as i64;
    let d = limbs[3] as i64;

    (a, b, c, d)
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

fn print_ledger(label: &str, ledger: &Ledger) {
    eprintln!("--- Ledger: {label} ---");
    eprintln!("utxos: {}", ledger.utxos.len());
    let mut utxos: Vec<_> = ledger.utxos.iter().collect();
    utxos.sort_by_key(|(id, _)| (id.contract_hash.0, id.nonce));
    for (id, entry) in utxos {
        eprintln!(
            "  utxo hash={} nonce={} pc={} globals={:?}",
            format!("{:?}", id.contract_hash),
            id.nonce,
            entry.state.pc,
            entry.state.globals
        );
    }
    eprintln!("ownership: {:?}", ledger.ownership_registry);
    eprintln!("--- /Ledger: {label} ---");
}

#[test]
fn test_multi_tx_accumulator_global() {
    let mut builder = wasm_dsl::ModuleBuilder::new();
    // global 0 = gpc, global 1 = acc
    builder.add_global_i64(0, true);
    builder.add_global_i64(0, true);
    let utxo_bin = wasm_module!(builder, {
        let (state_ref, _caller) = call activation();
        let (disc, arg, _b, _c) = call ref_get(state_ref, 0);
        if disc == 1 {
            let curr = global_get 1;
            let next = add curr, arg;
            set_global 1 = next;
            let pc = global_get 0;
            let next_pc = add pc, 1;
            set_global 0 = next_pc;
            call ref_write(state_ref, 0, next, 0, 0, 0);
        }
        let resp = call new_ref(1);
        let acc = global_get 1;
        call ref_push(acc, 0, 0, 0);
        call yield_(resp);
    });

    let (utxo_hash_a, utxo_hash_b, utxo_hash_c, utxo_hash_d) = hash_program(&utxo_bin);

    let coord_bin = wasm_module!({
        let init_ref = call new_ref(1);
        call ref_push(0, 0, 0, 0);

        let utxo_id = call new_utxo(
            const(utxo_hash_a),
            const(utxo_hash_b),
            const(utxo_hash_c),
            const(utxo_hash_d),
            init_ref
        );

        let req = call new_ref(1);
        call ref_push(1, 5, 0, 0);
        let (resp, _caller) = call resume(utxo_id, req);
        let (val, _b, _c, _d) = call ref_get(resp, 0);
        assert_eq val, 5;
        call return_();
    });

    let coord2_bin = wasm_module!({
        let req = call new_ref(1);
        call ref_push(1, 7, 0, 0);
        let (resp, _caller) = call resume(0, req);
        let (val, _b, _c, _d) = call ref_get(resp, 0);
        assert_eq val, 12;
        call return_();
    });

    let coord3_bin = wasm_module!({
        let req = call new_ref(1);
        call ref_push(2, 0, 0, 0);
        let (resp, _caller) = call resume(0, req);
        let (val, _b, _c, _d) = call ref_get(resp, 0);
        assert_eq val, 12;
        call return_();
    });

    print_wat("globals/utxo", &utxo_bin);
    print_wat("globals/coord1", &coord_bin);
    print_wat("globals/coord2", &coord2_bin);

    let tx1 = UnprovenTransaction {
        inputs: vec![],
        input_states: vec![],
        input_ownership: vec![],
        programs: vec![utxo_bin.clone(), coord_bin.clone()],
        is_utxo: vec![true, false],
        entrypoint: 1,
    };

    let proven_tx1 = tx1.prove().unwrap();
    let mut ledger = Ledger::new();
    ledger = ledger.apply_transaction(&proven_tx1).unwrap();
    print_ledger("after tx1", &ledger);

    let input_id: UtxoId = ledger.utxos.keys().next().cloned().unwrap();
    assert_eq!(input_id.nonce, 0);
    assert_eq!(
        ledger.utxos[&input_id].state.globals,
        vec![Value(1), Value(5)]
    );

    let tx2 = UnprovenTransaction {
        inputs: vec![input_id.clone()],
        input_states: vec![ledger.utxos[&input_id].state.clone()],
        input_ownership: vec![None],
        programs: vec![utxo_bin.clone(), coord2_bin],
        is_utxo: vec![true, false],
        entrypoint: 1,
    };

    let proven_tx2 = tx2.prove().unwrap();
    ledger = ledger.apply_transaction(&proven_tx2).unwrap();
    print_ledger("after tx2", &ledger);

    let output_id: UtxoId = ledger.utxos.keys().next().cloned().unwrap();
    assert_eq!(output_id.nonce, 1);
    let globals = &ledger.utxos[&output_id].state.globals;
    assert_eq!(globals, &[Value(2), Value(12)]);

    let tx3 = UnprovenTransaction {
        inputs: vec![output_id.clone()],
        input_states: vec![ledger.utxos[&output_id].state.clone()],
        input_ownership: vec![None],
        programs: vec![utxo_bin, coord3_bin],
        is_utxo: vec![true, false],
        entrypoint: 1,
    };

    let proven_tx3 = tx3.prove().unwrap();
    ledger = ledger.apply_transaction(&proven_tx3).unwrap();
    print_ledger("after tx3", &ledger);

    let output_id3: UtxoId = ledger.utxos.keys().next().cloned().unwrap();
    assert_eq!(output_id3, output_id);
    let globals = &ledger.utxos[&output_id3].state.globals;
    assert_eq!(globals, &[Value(2), Value(12)]);
}
