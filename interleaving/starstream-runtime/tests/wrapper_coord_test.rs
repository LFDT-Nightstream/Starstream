use starstream_interleaving_spec::{Hash, InterfaceId, Ledger};
use starstream_runtime::{
    UnprovenTransaction, poseidon_program_hash, register_mermaid_decoder, test_support::wasm_dsl,
    wasm_module,
};
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

    let utxo1_builder = wasm_dsl::ModuleBuilder::new();
    let utxo1_bin = wasm_module!(utxo1_builder, {
        let pc = call get_datum(0);
        if pc == 0 {
            let (init_ref, caller) = call activation();
            call trace(8, 0, init_ref, 0, caller, 0, 0, 0);
            let (cell_ref, _b, _c, _d) = call ref_get(init_ref, 0);
            call trace(12, cell_ref, init_ref, _b, 0, _c, _d, 0);

            let req = call new_ref(1);
            call trace(10, 0, 0, req, 1, 0, 0, 0);
            call ref_push(2, cell_ref, 42, 0);
            call trace(11, 2, cell_ref, 42, 0, 0, 0, 0);

            call set_datum(1, req);
            call set_datum(0, 1);
            call call_effect_handler(1, 2, 3, 4, req);
        }
        if pc == 1 {
            let req = call get_datum(1);
            let (resp, _caller_effect) = call untraced_activation();
            call trace(18, 0, req, resp, 1, 2, 3, 4);
            let done = call new_ref(1);
            call trace(10, 0, 0, done, 1, 0, 0, 0);
            call ref_push(0, 0, 0, 0);
            call trace(11, 0, 0, 0, 0, 0, 0, 0);
            call set_datum(0, 2);
            call trace(1, 0, done, 0, 0, 0, 0, 0);
            call yield_(done);
        }
    });

    let utxo2_builder = wasm_dsl::ModuleBuilder::new();
    let utxo2_bin = wasm_module!(utxo2_builder, {
        let pc = call get_datum(0);
        if pc == 0 {
            let (init_ref, caller) = call activation();
            call trace(8, 0, init_ref, 0, caller, 0, 0, 0);
            let (cell_ref, _b, _c, _d) = call ref_get(init_ref, 0);
            call trace(12, cell_ref, init_ref, _b, 0, _c, _d, 0);

            let req = call new_ref(1);
            call trace(10, 0, 0, req, 1, 0, 0, 0);
            call ref_push(3, cell_ref, 0, 0);
            call trace(11, 3, cell_ref, 0, 0, 0, 0, 0);

            call set_datum(1, req);
            call set_datum(0, 1);
            call call_effect_handler(1, 2, 3, 4, req);
        }
        if pc == 1 {
            let req = call get_datum(1);
            let (resp, _caller_effect) = call untraced_activation();
            call trace(18, 0, req, resp, 1, 2, 3, 4);
            let (_disc, val, _c2, _d2) = call ref_get(resp, 0);
            call trace(12, _disc, resp, val, 0, _c2, _d2, 0);
            assert_eq val, 42;

            let done = call new_ref(1);
            call trace(10, 0, 0, done, 1, 0, 0, 0);
            call ref_push(0, 0, 0, 0);
            call trace(11, 0, 0, 0, 0, 0, 0, 0);
            call set_datum(0, 2);
            call trace(1, 0, done, 0, 0, 0, 0, 0);
            call yield_(done);
        }
    });

    let (utxo1_hash_limb_a, utxo1_hash_limb_b, utxo1_hash_limb_c, utxo1_hash_limb_d) =
        hash_program(&utxo1_bin);
    let (utxo2_hash_limb_a, utxo2_hash_limb_b, utxo2_hash_limb_c, utxo2_hash_limb_d) =
        hash_program(&utxo2_bin);

    let inner_builder = wasm_dsl::ModuleBuilder::new();
    let inner_coord_bin = wasm_module!(inner_builder, {
        let pc = call get_datum(0);
        if pc == 0 {
            let (init_ref, caller) = call init();
            call trace(9, 0, init_ref, 0, caller, 0, 0, 0);
            let (utxo1_id, utxo2_id, _c, _d) = call ref_get(init_ref, 0);
            call trace(12, utxo1_id, init_ref, utxo2_id, 0, _c, _d, 0);
            let handler_id = call get_handler_for(1, 2, 3, 4);
            call trace(6, handler_id, 0, 0, 1, 2, 3, 4);
            call set_datum(1, utxo1_id);
            call set_datum(2, utxo2_id);
            call set_datum(3, handler_id);

            let req_new = call new_ref(1);
            call trace(10, 0, 0, req_new, 1, 0, 0, 0);
            call ref_push(1, 0, 0, 0);
            call trace(11, 1, 0, 0, 0, 0, 0, 0);
            call set_datum(5, handler_id);
            call set_datum(6, req_new);
            call set_datum(0, 1);
            call resume(handler_id, req_new);
        }
        if pc == 1 {
            let last_target = call get_datum(5);
            let last_val = call get_datum(6);
            let (resp_new, caller2) = call untraced_activation();
            let caller2_enc = add caller2, 1;
            call trace(0, last_target, last_val, resp_new, caller2_enc, 0, 0, 0);
            let (_disc, cell_ref, _c2, _d2) = call ref_get(resp_new, 0);
            call trace(12, _disc, resp_new, cell_ref, 0, _c2, _d2, 0);

            let cell_init = call new_ref(1);
            call trace(10, 0, 0, cell_init, 1, 0, 0, 0);
            call ref_push(cell_ref, 0, 0, 0);
            call trace(11, cell_ref, 0, 0, 0, 0, 0, 0);
            call set_datum(4, cell_init);

            let utxo1_id = call get_datum(1);
            call set_datum(5, utxo1_id);
            call set_datum(6, cell_init);
            call set_datum(0, 2);
            call resume(utxo1_id, cell_init);
        }
        if pc == 2 {
            let last_target = call get_datum(5);
            let last_val = call get_datum(6);
            let (ret1, caller3) = call untraced_activation();
            let caller3_enc = add caller3, 1;
            call trace(0, last_target, last_val, ret1, caller3_enc, 0, 0, 0);
            let utxo2_id = call get_datum(2);
            let cell_init = call get_datum(4);
            call set_datum(5, utxo2_id);
            call set_datum(6, cell_init);
            call set_datum(0, 3);
            call resume(utxo2_id, cell_init);
        }
        if pc == 3 {
            let last_target = call get_datum(5);
            let last_val = call get_datum(6);
            let (ret2, caller4) = call untraced_activation();
            let caller4_enc = add caller4, 1;
            call trace(0, last_target, last_val, ret2, caller4_enc, 0, 0, 0);
            let handler_id = call get_datum(3);
            let req_end = call new_ref(1);
            call trace(10, 0, 0, req_end, 1, 0, 0, 0);
            call ref_push(4, 0, 0, 0);
            call trace(11, 4, 0, 0, 0, 0, 0, 0);
            call set_datum(5, handler_id);
            call set_datum(6, req_end);
            call set_datum(0, 4);
            call resume(handler_id, req_end);
        }
        if pc == 4 {
            let last_target = call get_datum(5);
            let last_val = call get_datum(6);
            let (resp_end, caller5) = call untraced_activation();
            let caller5_enc = add caller5, 1;
            call trace(0, last_target, last_val, resp_end, caller5_enc, 0, 0, 0);
            call set_datum(0, 5);
            call trace(17, 0, 0, 0, 0, 0, 0, 0);
            call return_();
        }
    });

    let (inner_hash_limb_a, inner_hash_limb_b, inner_hash_limb_c, inner_hash_limb_d) =
        hash_program(&inner_coord_bin);

    let wrapper_builder = wasm_dsl::ModuleBuilder::new();
    let wrapper_coord_bin = wasm_module!(wrapper_builder, {
        let pc = call get_datum(0);
        if pc == 0 {
            let (init_ref, caller) = call init();
            call trace(9, 0, init_ref, 0, caller, 0, 0, 0);
            let (inner_id, _inner_init, _c, _d) = call ref_get(init_ref, 0);
            call trace(12, inner_id, init_ref, _inner_init, 0, _c, _d, 0);
            call set_datum(1, inner_id);
            call set_datum(2, 0);
            call install_handler(1, 2, 3, 4);
            call trace(4, 0, 0, 0, 1, 2, 3, 4);
            call set_datum(4, inner_id);
            call set_datum(5, 0);
            call set_datum(0, 1);
            call resume(inner_id, 0);
        }

        if pc == 1 {
            let last_target = call get_datum(4);
            let last_val = call get_datum(5);
            let (req, caller) = call untraced_activation();
            let caller_enc = add caller, 1;
            call trace(0, last_target, last_val, req, caller_enc, 0, 0, 0);
            let (disc, _cell_ref, value, _d2) = call ref_get(req, 0);
            call trace(12, disc, req, _cell_ref, 0, value, _d2, 0);

            if disc == 4 {
                let resp = call new_ref(1);
                call trace(10, 0, 0, resp, 1, 0, 0, 0);
                call ref_push(13, 0, 0, 0);
                call trace(11, 13, 0, 0, 0, 0, 0, 0);
                call set_datum(4, caller);
                call set_datum(5, resp);
                call set_datum(0, 2);
                call resume(caller, resp);
            }

            if disc == 1 {
                let cell_id = call get_datum(3);
                let resp = call new_ref(1);
                call trace(10, 0, 0, resp, 1, 0, 0, 0);
                call ref_push(11, cell_id, 0, 0);
                call trace(11, 11, cell_id, 0, 0, 0, 0, 0);
                call set_datum(4, caller);
                call set_datum(5, resp);
                call set_datum(0, 1);
                call resume(caller, resp);
            }

            if disc == 2 {
                call set_datum(2, value);
                let resp = call new_ref(1);
                call trace(10, 0, 0, resp, 1, 0, 0, 0);
                call ref_push(10, 0, 0, 0);
                call trace(11, 10, 0, 0, 0, 0, 0, 0);
                call set_datum(4, caller);
                call set_datum(5, resp);
                call set_datum(0, 1);
                call resume(caller, resp);
            }

            if disc == 3 {
                let cell_val = call get_datum(2);
                let resp = call new_ref(1);
                call trace(10, 0, 0, resp, 1, 0, 0, 0);
                call ref_push(12, cell_val, 0, 0);
                call trace(11, 12, cell_val, 0, 0, 0, 0, 0);
                call set_datum(4, caller);
                call set_datum(5, resp);
                call set_datum(0, 1);
                call resume(caller, resp);
            }
        }

        if pc == 2 {
            let last_target = call get_datum(4);
            let last_val = call get_datum(5);
            let (ret, caller) = call untraced_activation();
            let caller_enc = add caller, 1;
            call trace(0, last_target, last_val, ret, caller_enc, 0, 0, 0);
            call uninstall_handler(1, 2, 3, 4);
            call trace(5, 0, 0, 0, 1, 2, 3, 4);
            call set_datum(0, 3);
            call trace(17, 0, 0, 0, 0, 0, 0, 0);
            call return_();
        }
    });

    print_wat("wrapper", &wrapper_coord_bin);

    let (wrapper_hash_limb_a, wrapper_hash_limb_b, wrapper_hash_limb_c, wrapper_hash_limb_d) =
        hash_program(&wrapper_coord_bin);

    // Patch wrapper hash constants into driver.
    let driver_builder = wasm_dsl::ModuleBuilder::new();
    let driver_coord_bin = wasm_module!(driver_builder, {
        let pc = call get_datum(0);
        if pc == 0 {
            let init_val = call new_ref(1);
            call trace(10, 0, 0, init_val, 1, 0, 0, 0);
            call ref_push(0, 0, 0, 0);
            call trace(11, 0, 0, 0, 0, 0, 0, 0);

            let utxo1_id = call new_utxo(
                const(utxo1_hash_limb_a),
                const(utxo1_hash_limb_b),
                const(utxo1_hash_limb_c),
                const(utxo1_hash_limb_d),
                init_val
            );
            call trace(
                2,
                utxo1_id,
                init_val,
                0,
                const(utxo1_hash_limb_a),
                const(utxo1_hash_limb_b),
                const(utxo1_hash_limb_c),
                const(utxo1_hash_limb_d)
            );

            let utxo2_id = call new_utxo(
                const(utxo2_hash_limb_a),
                const(utxo2_hash_limb_b),
                const(utxo2_hash_limb_c),
                const(utxo2_hash_limb_d),
                init_val
            );
            call trace(
                2,
                utxo2_id,
                init_val,
                0,
                const(utxo2_hash_limb_a),
                const(utxo2_hash_limb_b),
                const(utxo2_hash_limb_c),
                const(utxo2_hash_limb_d)
            );

            let inner_init = call new_ref(1);
            call trace(10, 0, 0, inner_init, 1, 0, 0, 0);
            call ref_push(utxo1_id, utxo2_id, 0, 0);
            call trace(11, utxo1_id, utxo2_id, 0, 0, 0, 0, 0);

            let inner_id = call new_coord(
                const(inner_hash_limb_a),
                const(inner_hash_limb_b),
                const(inner_hash_limb_c),
                const(inner_hash_limb_d),
                inner_init
            );
            call trace(
                3,
                inner_id,
                inner_init,
                0,
                const(inner_hash_limb_a),
                const(inner_hash_limb_b),
                const(inner_hash_limb_c),
                const(inner_hash_limb_d)
            );

            let wrapper_init = call new_ref(1);
            call trace(10, 0, 0, wrapper_init, 1, 0, 0, 0);
            call ref_push(inner_id, inner_init, 0, 0);
            call trace(11, inner_id, inner_init, 0, 0, 0, 0, 0);

            let wrapper_id = call new_coord(
                const(wrapper_hash_limb_a),
                const(wrapper_hash_limb_b),
                const(wrapper_hash_limb_c),
                const(wrapper_hash_limb_d),
                wrapper_init
            );
            call trace(
                3,
                wrapper_id,
                wrapper_init,
                0,
                const(wrapper_hash_limb_a),
                const(wrapper_hash_limb_b),
                const(wrapper_hash_limb_c),
                const(wrapper_hash_limb_d)
            );

            call set_datum(1, wrapper_id);
            call set_datum(2, wrapper_init);
            call set_datum(0, 1);
            call resume(wrapper_id, wrapper_init);
        }
        if pc == 1 {
            let last_target = call get_datum(1);
            let last_val = call get_datum(2);
            let (ret, caller) = call untraced_activation();
            let caller_enc = add caller, 1;
            call trace(0, last_target, last_val, ret, caller_enc, 0, 0, 0);
            call set_datum(0, 2);
            call trace(17, 0, 0, 0, 0, 0, 0, 0);
            call return_();
        }
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

fn interface_id(a: u64, b: u64, c: u64, d: u64) -> InterfaceId {
    Hash([a, b, c, d], PhantomData)
}
