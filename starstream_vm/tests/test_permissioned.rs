use std::sync::Arc;

use starstream_vm::*;
use wasmi::Value;

#[test]
pub fn main() {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("debug")).init();

    std::process::Command::new("cargo")
        .arg("build")
        .arg("-p")
        .arg("example_contract_permissioned")
        .status()
        .unwrap();

    let mut tx = Transaction::new();

    let contract = tx.code_cache().load_debug("example_contract_permissioned");

    // as a simplification (using i32 instead of public keys), the empty list
    // technically blacklists the set {0, i32::MAX}
    let head = tx.run_coordination_script(&contract, "blacklist_empty", vec![]);

    // first we insert in order: [3, 5, 7]
    let new_node = tx.run_coordination_script(
        &contract,
        "blacklist_insert",
        vec![head.clone(), Value::I32(3)],
    );

    let new_node =
        tx.run_coordination_script(&contract, "blacklist_insert", vec![new_node, Value::I32(5)]);

    let _new_node =
        tx.run_coordination_script(&contract, "blacklist_insert", vec![new_node, Value::I32(7)]);

    // the list currently has [3,5,7], so this would be inserted at index 1.
    // find_prev_node should return the address of the utxo with the key of 3.
    let new_key = 6;
    let prev_node = find_prev_node(&mut tx, &contract, new_key);

    let _new_node = tx.run_coordination_script(
        &contract,
        "blacklist_insert",
        vec![prev_node, Value::I32(new_key)],
    );

    let minter = tx.run_coordination_script(&contract, "token_mint_new", vec![]);

    let mint_to = 4;
    let proof_to = find_prev_node(&mut tx, &contract, mint_to);

    let minted_token = tx.run_coordination_script(
        &contract,
        "token_mint_to",
        vec![
            minter.clone(),
            Value::I32(mint_to),
            Value::I32(100),
            proof_to,
        ],
    );

    // blacklist: [3, 5, 6, 7]
    let from = 4;
    // let from = 7; // blacklisted
    let to = 8;
    // let to = 7; // blacklisted

    let proof_from = find_prev_node(&mut tx, &contract, from);
    let proof_to = find_prev_node(&mut tx, &contract, to);

    let _transfer_output = tx.run_coordination_script(
        &contract,
        "transfer_permissioned_token",
        vec![
            minted_token,
            proof_from,
            proof_to,
            Value::I32(to),
            // amount to transfer
            Value::I32(50),
        ],
    );

    let utxos = tx
        .utxos()
        .into_iter()
        .filter(|(_, entry_point)| entry_point == "starstream_new_PayToPublicKeyHash_new")
        .collect::<Vec<_>>();

    let owner0 = tx.run_coordination_script(
        &contract,
        "pay_to_public_key_hash_owner",
        vec![utxos[0].0.clone()],
    );

    let owner1 = tx.run_coordination_script(
        &contract,
        "pay_to_public_key_hash_owner",
        vec![utxos[1].0.clone()],
    );

    dbg!(owner0);
    dbg!(owner1);
}

fn find_prev_node(tx: &mut Transaction, contract: &Arc<ContractCode>, new_key: i32) -> Value {
    let mut utxos = tx
        .utxos()
        .into_iter()
        .filter(|(_, entry_point)| entry_point == "starstream_new_LinkedListNode_new")
        .collect::<Vec<_>>();

    utxos.sort_unstable_by_key(|(utxo_id, _entry_point)| {
        match tx.run_coordination_script(contract, "blacklist_node_get_key", vec![utxo_id.clone()])
        {
            Value::I32(i) => i,
            _ => unreachable!(),
        }
    });

    let Err(insert_at) = utxos.binary_search_by_key(&new_key, |(utxo_id, _entry_point)| {
        match tx.run_coordination_script(contract, "blacklist_node_get_key", vec![utxo_id.clone()])
        {
            Value::I32(i) => i,
            _ => unreachable!(),
        }
    }) else {
        // just return whatever so that the coordination script fails instead of
        // failing here for testing sake
        return utxos[0].0.clone();
    };

    utxos[insert_at - 1].0.clone()
}
