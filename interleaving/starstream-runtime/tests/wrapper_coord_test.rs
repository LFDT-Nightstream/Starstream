use sha2::{Digest, Sha256};
use starstream_interleaving_spec::{Hash, InterfaceId, Ledger};
use starstream_runtime::{UnprovenTransaction, register_mermaid_decoder, wasm_module};
use std::marker::PhantomData;

// this tests tries to encode something like a coordination script that provides a Cell interface
//
// we have the Cell implementation
//
// fn wrapper(inner: Coroutine<Cell>) {
//   let cells = [];
//
//   try {
//     resume(inner, ());
//   }
//   with Cell {
//     fn new(): CellId {
//       cells.push(F::ZERO); // note however that the test is simplified to a single cell to avoid needing an array
//       resume cells.len();
//     }
//
//     fn write(cell: CellId, val: Val) {
//       cells[cell] = val;
//
//       resume ();
//     }
//
//     fn read(cell: CellId): Val {
//       resume cells[cell];
//     }
//   }
// }
//
// fn inner(utxo1: Utxo1, utxo2: Utxo2) {
//   let cell = raise Cell::new();
//
//   utxo1.foo(cell);
//   utxo2.bar(cell);
// }
//
// utxo Utxo1 {
//   fn foo(cell: CellId) / { Cell } {
//     raise Cell::write(cell, 42);
//   }
// }
//
// utxo Utxo2 {
//   fn bar(cell: CellId) / { Cell } {
//     let v = raise Cell::read(cell);
//   }
// }
//
// fn main() {
//   let utxo1 = Utxo1::new();
//   let utxo2 = Utxo2::new();
//
//   wrapper {
//     inner(utxo1, utxo2)
//   }
// }

#[test]
fn test_runtime_wrapper_coord_newcoord_handlers() {
    register_mermaid_decoder(interface_id(1, 2, 3, 4), |values| {
        let disc = values.first()?.0;
        let v1 = values.get(1).map(|v| v.0).unwrap_or(0);
        let v2 = values.get(2).map(|v| v.0).unwrap_or(0);
        let label = match disc {
            1 => "disc=new_cell".to_string(),
            2 => format!("disc=write cell={v1} value={v2}"),
            3 => format!("disc=read cell={v1}"),
            4 => "disc=end".to_string(),
            10 => "disc=ack".to_string(),
            11 => format!("disc=new_cell_resp cell={v1}"),
            12 => format!("disc=read_resp value={v1}"),
            13 => "disc=end_ack".to_string(),
            _ => return None,
        };
        Some(label)
    });

    let utxo1_bin = wasm_module!({
        let (init_ref, _caller) = call activation();
        let (cell_ref, _b, _c, _d) = call ref_get(init_ref, 0);

        let req = call new_ref(1);
        call ref_push(2, cell_ref, 42, 0);

        let _resp = call call_effect_handler(1, 2, 3, 4, req);

        let done = call new_ref(1);
        call ref_push(0, 0, 0, 0);
        call yield_(done);
    });

    let utxo2_bin = wasm_module!({
        let (init_ref, _caller) = call activation();
        let (cell_ref, _b, _c, _d) = call ref_get(init_ref, 0);

        let req = call new_ref(1);
        call ref_push(3, cell_ref, 0, 0);

        let resp = call call_effect_handler(1, 2, 3, 4, req);
        let (_disc, val, _c2, _d2) = call ref_get(resp, 0);
        assert_eq val, 42;

        let done = call new_ref(1);
        call ref_push(0, 0, 0, 0);
        call yield_(done);
    });

    let (utxo1_hash_limb_a, utxo1_hash_limb_b, utxo1_hash_limb_c, utxo1_hash_limb_d) =
        hash_program(&utxo1_bin);
    let (utxo2_hash_limb_a, utxo2_hash_limb_b, utxo2_hash_limb_c, utxo2_hash_limb_d) =
        hash_program(&utxo2_bin);

    let inner_coord_bin = wasm_module!({
        let (init_ref, _caller) = call init();
        let (utxo1_id, utxo2_id, _c, _d) = call ref_get(init_ref, 0);

        let handler_id = call get_handler_for(1, 2, 3, 4);

        // new_cell
        let req_new = call new_ref(1);
        call ref_push(1, 0, 0, 0);
        let (resp_new, _caller2) = call resume(handler_id, req_new);
        let (_disc, cell_ref, _c2, _d2) = call ref_get(resp_new, 0);

        let cell_init = call new_ref(1);
        call ref_push(cell_ref, 0, 0, 0);

        // utxo1 writes 42
        let (_ret1, _caller3) = call resume(utxo1_id, cell_init);

        // utxo2 reads 42
        let (_ret2, _caller4) = call resume(utxo2_id, cell_init);

        // end
        let req_end = call new_ref(1);
        call ref_push(4, 0, 0, 0);
        let (_resp_end, _caller5) = call resume(handler_id, req_end);

        call return_();
    });

    let (inner_hash_limb_a, inner_hash_limb_b, inner_hash_limb_c, inner_hash_limb_d) =
        hash_program(&inner_coord_bin);

    let wrapper_coord_bin = wasm_module!({
        let (init_ref, _caller) = call init();
        let (inner_id, inner_init, _c, _d) = call ref_get(init_ref, 0);

        call install_handler(1, 2, 3, 4);

        let (req0, caller0) = call resume(inner_id, 0);
        let req = req0;
        let caller = caller0;
        let handled = const(0);
        let cell_val = const(0);
        // Single-cell wrapper for this test: ignore cell_ref and return a fixed cell id.
        let cell_id = const(1);

        loop {
            set handled = const(0);
            let (disc, cell_ref, value, _d2) = call ref_get(req, 0);

            if disc == 4 {
                let resp = call new_ref(1);
                call ref_push(13, 0, 0, 0);
                let (_req_next, _caller_next) = call resume(caller, resp);
                set handled = const(2);
            }

            if disc == 1 {
                let resp = call new_ref(1);
                call ref_push(11, cell_id, 0, 0);

                let (req_next, caller_next) = call resume(caller, resp);
                set req = req_next;
                set caller = caller_next;
                set handled = const(1);
            }

            if disc == 2 {
                set cell_val = value;
                let resp = call new_ref(1);
                call ref_push(10, 0, 0, 0);
                let (req_next, caller_next) = call resume(caller, resp);
                set req = req_next;
                set caller = caller_next;
                set handled = const(1);
            }

            // disc == 3 (read)
            if handled == 0 {
                let resp = call new_ref(1);
                call ref_push(12, cell_val, 0, 0);
                let (req_next, caller_next) = call resume(caller, resp);
                set req = req_next;
                set caller = caller_next;
                set handled = const(1);
            }

            break_if handled == 2;
            continue_if handled == 1;
        }

        call uninstall_handler(1, 2, 3, 4);
        call return_();
    });

    print_wat("wrapper", &wrapper_coord_bin);

    let (wrapper_hash_limb_a, wrapper_hash_limb_b, wrapper_hash_limb_c, wrapper_hash_limb_d) =
        hash_program(&wrapper_coord_bin);

    // Patch wrapper hash constants into driver.
    let driver_coord_bin = wasm_module!({
        let init_val = call new_ref(1);
        call ref_push(0, 0, 0, 0);

        let utxo1_id = call new_utxo(
            const(utxo1_hash_limb_a),
            const(utxo1_hash_limb_b),
            const(utxo1_hash_limb_c),
            const(utxo1_hash_limb_d),
            init_val
        );

        let utxo2_id = call new_utxo(
            const(utxo2_hash_limb_a),
            const(utxo2_hash_limb_b),
            const(utxo2_hash_limb_c),
            const(utxo2_hash_limb_d),
            init_val
        );

        let inner_init = call new_ref(1);
        call ref_push(utxo1_id, utxo2_id, 0, 0);

        let inner_id = call new_coord(
            const(inner_hash_limb_a),
            const(inner_hash_limb_b),
            const(inner_hash_limb_c),
            const(inner_hash_limb_d),
            inner_init
        );

        let wrapper_init = call new_ref(1);
        call ref_push(inner_id, inner_init, 0, 0);

        let wrapper_id = call new_coord(
            const(wrapper_hash_limb_a),
            const(wrapper_hash_limb_b),
            const(wrapper_hash_limb_c),
            const(wrapper_hash_limb_d),
            wrapper_init
        );

        let (_ret, _caller) = call resume(wrapper_id, wrapper_init);
        call return_();
    });

    let programs = vec![
        utxo1_bin.clone(),
        utxo2_bin.clone(),
        inner_coord_bin.clone(),
        wrapper_coord_bin.clone(),
        driver_coord_bin.clone(),
    ];

    let tx = UnprovenTransaction {
        inputs: vec![],
        input_states: vec![],
        input_ownership: vec![],
        programs,
        is_utxo: vec![true, true, false, false, false],
        entrypoint: 4,
    };

    let proven_tx = match tx.prove() {
        Ok(tx) => tx,
        Err(err) => {
            if std::env::var_os("DEBUG_TRACE").is_some() {
                eprintln!("prove failed: {err:?}");
            }
            panic!("{err:?}");
        }
    };
    let ledger = Ledger::new();
    let ledger = ledger.apply_transaction(&proven_tx).unwrap();
    assert_eq!(ledger.utxos.len(), 2);
}

fn hash_program(utxo_bin: &Vec<u8>) -> (i64, i64, i64, i64) {
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

fn interface_id(a: u64, b: u64, c: u64, d: u64) -> InterfaceId {
    let mut buffer = [0u8; 32];
    buffer[0..8].copy_from_slice(&a.to_le_bytes());
    buffer[8..16].copy_from_slice(&b.to_le_bytes());
    buffer[16..24].copy_from_slice(&c.to_le_bytes());
    buffer[24..32].copy_from_slice(&d.to_le_bytes());
    Hash(buffer, PhantomData)
}
