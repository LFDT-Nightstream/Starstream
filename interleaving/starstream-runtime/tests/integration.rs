#[macro_use]
pub mod wasm_dsl;

use sha2::{Digest, Sha256};
use starstream_interleaving_spec::Ledger;
use starstream_runtime::UnprovenTransaction;

#[test]
fn test_runtime_simple_effect_handlers() {
    let utxo_bin = wasm_module!({
        let (_init_ref, caller) = call activation();

        let (caller_hash_a, caller_hash_b, caller_hash_c, caller_hash_d) = call get_program_hash(caller);
        let (script_hash_a, script_hash_b, script_hash_c, script_hash_d) = call get_program_hash(1);

        assert_eq caller_hash_a, script_hash_a;
        assert_eq caller_hash_b, script_hash_b;
        assert_eq caller_hash_c, script_hash_c;
        assert_eq caller_hash_d, script_hash_d;

        let handler_id = call get_handler_for(1, 0, 0, 0);
        let req = call new_ref(7);
        call ref_push(42, 0, 0, 0, 0, 0, 0);

        let (resp, _caller) = call resume(handler_id, req);
        let (resp_val, _b, _c, _d, _e) = call get(resp, 0);

        assert_eq resp_val, 1;
        let (_req2, _caller2) = call yield_(resp);
    });

    let (utxo_hash_limb_a, utxo_hash_limb_b, utxo_hash_limb_c, utxo_hash_limb_d) =
        hash_program(&utxo_bin);

    let coord_bin = wasm_module!({
        call install_handler(1, 0, 0, 0);

        let init_val = call new_ref(7);

        call ref_push(0, 0, 0, 0, 0, 0, 0);

        let _utxo_id = call new_utxo(
            const(utxo_hash_limb_a),
            const(utxo_hash_limb_b),
            const(utxo_hash_limb_c),
            const(utxo_hash_limb_d),
            init_val
        );

        let (req, _caller) = call resume(0, init_val);
        let (req_val, _b, _c, _d, _e) = call get(req, 0);

        assert_eq req_val, 42;

        let resp = call new_ref(7);
        call ref_push(1, 0, 0, 0, 0, 0, 0);

        let (_ret, _caller2) = call resume(0, resp);

        call uninstall_handler(1, 0, 0, 0);
    });

    print_wat("simple/utxo", &utxo_bin);
    print_wat("simple/coord", &coord_bin);

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
#[ignore = "this test is still quite expensive to run in the CI (it generates like a 100 folding steps)"]
fn test_runtime_effect_handlers_cross_calls() {
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
    let utxo1_bin = wasm_module!({
        let (_init_ref, _caller) = call activation();

        let handler_id = call get_handler_for(1, 2, 3, 4);
        let x0 = 99;
        let n = 5;
        let i0 = 0;
        let x = x0;
        let i = i0;

        // Call the +1 service n times, then send disc=2 to break.
        loop {
            break_if i == n;

            let req = call new_ref(2);
            call ref_push(1, x, 0, 0, 0, 0, 0);

            let (resp, _caller2) = call resume(handler_id, req);
            let (y, _b, _c, _d, _e) = call get(resp, 0);
            let expected = add x, 1;
            assert_eq y, expected;

            set x = y;
            set i = add i, 1;
            continue;
        }

        let stop = call new_ref(2);
        call ref_push(2, x, 0, 0, 0, 0, 0);
        let (_resp_stop, _caller_stop) = call resume(handler_id, stop);

        let (_req3, _caller3) = call yield_(stop);
    });

    let utxo2_bin = wasm_module!({
        let (init_ref, _caller) = call activation();
        let req = init_ref;

        // Serve x -> x+1 for each incoming request.
        loop {
            let (x, _b, _c, _d, _e) = call get(req, 0);
            let y = add x, 1;
            let resp = call new_ref(1);
            call ref_push(y, 0, 0, 0, 0, 0, 0);
            let (next_req, _caller2) = call yield_(resp);
            set req = next_req;
            continue;
        }
    });

    let (utxo1_hash_limb_a, utxo1_hash_limb_b, utxo1_hash_limb_c, utxo1_hash_limb_d) =
        hash_program(&utxo1_bin);

    let (utxo2_hash_limb_a, utxo2_hash_limb_b, utxo2_hash_limb_c, utxo2_hash_limb_d) =
        hash_program(&utxo2_bin);

    let coord_bin = wasm_module!({
        call install_handler(1, 2, 3, 4);

        let init_val = call new_ref(1);
        call ref_push(0, 0, 0, 0, 0, 0, 0);

        let utxo_id1 = call new_utxo(
            const(utxo1_hash_limb_a),
            const(utxo1_hash_limb_b),
            const(utxo1_hash_limb_c),
            const(utxo1_hash_limb_d),
            init_val
        );

        let utxo_id2 = call new_utxo(
            const(utxo2_hash_limb_a),
            const(utxo2_hash_limb_b),
            const(utxo2_hash_limb_c),
            const(utxo2_hash_limb_d),
            init_val
        );

        // Start utxo1 and then route messages until disc=2.
        let (req0, caller0) = call resume(utxo_id1, init_val);
        let req = req0;
        let caller1 = caller0;

        loop {
            let (disc, x, _c, _d, _e) = call get(req, 0);
            if disc == 2 {
                let back = call new_ref(1);
                call ref_push(x, 0, 0, 0, 0, 0, 0);
                let (_ret_stop, _caller_stop) = call resume(caller1, back);
            }
            break_if disc == 2;

            // coord -> utxo2
            let msg = call new_ref(1);
            call ref_push(x, 0, 0, 0, 0, 0, 0);
            let (resp2, _caller2) = call resume(utxo_id2, msg);
            let (y, _b2, _c2, _d2, _e2) = call get(resp2, 0);

            // coord -> utxo1, which will resume the handler again
            let back = call new_ref(1);
            call ref_push(y, 0, 0, 0, 0, 0, 0);
            let (req_next, caller_next) = call resume(caller1, back);
            set req = req_next;
            set caller1 = caller_next;
            continue;
        }

        call uninstall_handler(1, 2, 3, 4);
    });

    print_wat("cross/utxo1", &utxo1_bin);
    print_wat("cross/utxo2", &utxo2_bin);
    print_wat("cross/coord", &coord_bin);

    let programs = vec![utxo1_bin.clone(), utxo2_bin.clone(), coord_bin.clone()];

    let tx = UnprovenTransaction {
        inputs: vec![],
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
    // TODO: this would be poseidon2 later
    let mut hasher = Sha256::new();
    hasher.update(utxo_bin);
    let utxo_hash_bytes = hasher.finalize();

    let utxo_hash_limb_a = i64::from_le_bytes(utxo_hash_bytes[0..8].try_into().unwrap());
    let utxo_hash_limb_b = i64::from_le_bytes(utxo_hash_bytes[8..8 * 2].try_into().unwrap());
    let utxo_hash_limb_c = i64::from_le_bytes(utxo_hash_bytes[8 * 2..8 * 3].try_into().unwrap());
    let utxo_hash_limb_d = i64::from_le_bytes(utxo_hash_bytes[8 * 3..8 * 4].try_into().unwrap());

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
