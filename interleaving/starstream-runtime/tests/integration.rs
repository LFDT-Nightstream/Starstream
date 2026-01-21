use sha2::{Digest, Sha256};
use starstream_interleaving_spec::Ledger;
use starstream_runtime::UnprovenTransaction;
use wat::parse_str;

#[test]
fn test_runtime_effect_handlers() {
    let utxo_wat = r#"(
module
    (import "env" "starstream_activation" (func $activation (result i64 i64)))
    (import "env" "starstream_get_program_hash" (func $program_hash (param i64) (result i64 i64 i64 i64)))
    (import "env" "starstream_get_handler_for" (func $get_handler_for (param i64 i64 i64 i64) (result i64)))
    (import "env" "starstream_new_ref" (func $new_ref (param i64) (result i64)))
    (import "env" "starstream_ref_push" (func $ref_push (param i64)))
    (import "env" "starstream_resume" (func $resume (param i64 i64) (result i64 i64)))
    (import "env" "starstream_yield" (func $yield (param i64) (result i64 i64)))

    (func (export "_start")
        (local $val i64) (local $handler_id i64) (local $req i64) (local $resp i64)
        
        ;; ACTIVATION
        (call $activation)
        drop
        local.set $val
        
        ;; PROGRAM_HASH(target=1)
        (call $program_hash (i64.const 1))
        drop
        drop
        drop
        drop
        
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
    let coord_wat = format!(
        r#"(
module
    (import "env" "starstream_install_handler" (func $install_handler (param i64 i64 i64 i64)))
    (import "env" "starstream_new_ref" (func $new_ref (param i64) (result i64)))
    (import "env" "starstream_ref_push" (func $ref_push (param i64)))
    (import "env" "starstream_new_utxo" (func $new_utxo (param i64 i64 i64 i64 i64) (result i64)))
    (import "env" "starstream_resume" (func $resume (param i64 i64) (result i64 i64)))
    (import "env" "starstream_uninstall_handler" (func $uninstall_handler (param i64 i64 i64 i64)))
    
    (func (export "_start")
        (local $init_val i64) (local $resp i64) (local $final i64)
        
        ;; INSTALL_HANDLER(interface_id=limbs at 0)
        (call $install_handler (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0))
        
        ;; NEW_REF(size=1)
        (call $new_ref (i64.const 1))
        local.set $init_val
        
        ;; REF_PUSH(val=11111)
        (call $ref_push (i64.const 11111))
        
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
        drop
        drop
        
        ;; NEW_REF(size=1)
        (call $new_ref (i64.const 1))
        local.set $resp
        
        ;; REF_PUSH(val=43)
        (call $ref_push (i64.const 43))
        
        ;; NEW_REF(size=1)
        (call $new_ref (i64.const 1))
        local.set $final
        
        ;; REF_PUSH(val=33333)
        (call $ref_push (i64.const 33333))
        
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
