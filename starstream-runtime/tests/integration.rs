use sha2::{Digest, Sha256};
use starstream_mock_ledger::{EffectDiscriminant, Ledger};
use starstream_runtime::UnprovenTransaction;
use wat::parse_str;

#[test]
fn test_runtime_effect_handlers() {
    let utxo_wat = format!(
        r#"(
module
    (import "env" "starstream_host_call" (func $host_call (param i64 i64 i64 i64 i64 i64) (result i64 i64)))
    (import "env" "starstream_get_program_hash" (func $program_hash (param i64 i64) (result i64 i64 i64 i64)))

    (func (export "_start")
        (local $val i64) (local $handler_id i64) (local $req i64) (local $resp i64)
        
        ;; ACTIVATION
        (call $host_call (i64.const {ACTIVATION}) (i64.const 0) (i64.const 0) (i64.const 0) (i64.const 0) (i64.const 0))
        drop
        local.set $val
        
        ;; PROGRAM_HASH(target=1)
        (call $program_hash (i64.const {PROGRAM_HASH}) (i64.const 1))
        drop
        drop
        drop
        drop
        
        ;; GET_HANDLER_FOR(interface_id=limbs at 0)
        (call $host_call (i64.const {GET_HANDLER_FOR})
            (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0) (i64.const 0)
        )
        drop
        local.set $handler_id
        
        ;; NEW_REF(size=1)
        (call $host_call (i64.const {NEW_REF}) (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0) (i64.const 0))
        drop
        local.set $req
        
        ;; REF_PUSH(val=42)
        (call $host_call (i64.const {REF_PUSH}) (i64.const 42) (i64.const 0) (i64.const 0) (i64.const 0) (i64.const 0)) 
        drop
        drop
        
        ;; RESUME(target=$handler_id, val=$req, ret=2, prev=1)
        (call $host_call (i64.const {RESUME}) (local.get $handler_id) (local.get $req) (i64.const 0) (i64.const 1) (i64.const 0))
        drop
        local.set $resp
        
        ;; YIELD(val=$resp, ret=-1, prev=0)
        (call $host_call (i64.const {YIELD}) (local.get $resp) (i64.const 0) (i64.const 1) (i64.const 0) (i64.const 0))
        drop
        drop
    )
)
"#,
        ACTIVATION = EffectDiscriminant::Activation as u64,
        GET_HANDLER_FOR = EffectDiscriminant::GetHandlerFor as u64,
        NEW_REF = EffectDiscriminant::NewRef as u64,
        REF_PUSH = EffectDiscriminant::RefPush as u64,
        RESUME = EffectDiscriminant::Resume as u64,
        YIELD = EffectDiscriminant::Yield as u64,
        PROGRAM_HASH = EffectDiscriminant::ProgramHash as u64,
    );
    let utxo_bin = parse_str(&utxo_wat).unwrap();

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
    (import "env" "starstream_host_call" (func $host_call (param i64 i64 i64 i64 i64 i64) (result i64 i64)))
    
    (func (export "_start")
        (local $init_val i64) (local $resp i64) (local $final i64)
        
        ;; INSTALL_HANDLER(interface_id=limbs at 0)
        (call $host_call (i64.const {INSTALL_HANDLER})
            (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0) (i64.const 0)
        )
        drop
        drop
        
        ;; NEW_REF(size=1)
        (call $host_call (i64.const {NEW_REF}) (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0) (i64.const 0))
        drop
        local.set $init_val
        
        ;; REF_PUSH(val=11111)
        (call $host_call (i64.const {REF_PUSH}) (i64.const 11111) (i64.const 0) (i64.const 0) (i64.const 0) (i64.const 0))
        drop
        drop
        
        ;; NEW_UTXO(program_hash=limbs at 32, val=$init_val, id=0)
        (call $host_call (i64.const {NEW_UTXO}) 
            (i64.const {utxo_hash_limb_a}) 
            (i64.const {utxo_hash_limb_b}) 
            (i64.const {utxo_hash_limb_c}) 
            (i64.const {utxo_hash_limb_d}) 
            (local.get $init_val) 
        )
        drop
        drop
        
        ;; RESUME(target=0, val=$init_val, ret=1, prev=-1)
        (call $host_call (i64.const {RESUME}) (i64.const 0) (local.get $init_val) (i64.const 0) (i64.const -1) (i64.const 0))
        drop
        drop
        
        ;; NEW_REF(size=1)
        (call $host_call (i64.const {NEW_REF}) (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0) (i64.const 0))
        drop
        local.set $resp
        
        ;; REF_PUSH(val=43)
        (call $host_call (i64.const {REF_PUSH}) (i64.const 43) (i64.const 0) (i64.const 0) (i64.const 0) (i64.const 0))
        drop
        drop
        
        ;; NEW_REF(size=1)
        (call $host_call (i64.const {NEW_REF}) (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0) (i64.const 0))
        drop
        local.set $final
        
        ;; REF_PUSH(val=33333)
        (call $host_call (i64.const {REF_PUSH}) (i64.const 33333) (i64.const 0) (i64.const 0) (i64.const 0) (i64.const 0))
        drop
        drop
        
        ;; RESUME(target=0, val=$resp, ret=$resp, prev=0)
        (call $host_call (i64.const {RESUME}) (i64.const 0) (local.get $resp) (i64.const 0) (i64.const 0) (i64.const 0))
        drop
        drop
        
        ;; UNINSTALL_HANDLER(interface_id=limbs at 0)
        (call $host_call (i64.const {UNINSTALL_HANDLER})
            (i64.const 1) (i64.const 0) (i64.const 0) (i64.const 0) (i64.const 0)
        )
        drop
        drop
    )
)
"#,
        INSTALL_HANDLER = EffectDiscriminant::InstallHandler as u64,
        NEW_REF = EffectDiscriminant::NewRef as u64,
        REF_PUSH = EffectDiscriminant::RefPush as u64,
        NEW_UTXO = EffectDiscriminant::NewUtxo as u64,
        RESUME = EffectDiscriminant::Resume as u64,
        UNINSTALL_HANDLER = EffectDiscriminant::UninstallHandler as u64,
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
