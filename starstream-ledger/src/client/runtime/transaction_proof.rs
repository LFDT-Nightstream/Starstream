use minicbor::{Decode, Encode};
use starstream_proving_runtime::{
    EncodedTransactionProof, EntryCall, InputUtxo, MethodHash, OutputUtxo, ProofSizes,
    StarstreamValue, TransactionStatement, coordination_script_identity, flat_value_root,
};
use wasmparser::ValType;

use super::proving::get_or_insert_program;
use super::*;

#[derive(Encode, Decode)]
struct SubmittedProof {
    #[n(0)]
    contract: [u8; 32],
    #[n(1)]
    script: String,
    #[n(2)]
    arguments: [u64; 4],
    #[n(3)]
    outputs: Vec<(u32, Vec<[u32; 8]>)>,
    #[n(4)]
    proof: EncodedTransactionProof,
}

pub fn attach_proof(
    transaction: &mut Transaction,
    contract: [u8; 32],
    script: &str,
    statement: &TransactionStatement,
    proof: &TransactionProof,
) -> anyhow::Result<ProofSizes> {
    let submitted = SubmittedProof {
        contract,
        script: script.into(),
        arguments: statement.entry.arguments.0,
        outputs: statement
            .outputs
            .iter()
            .map(|o| (o.utxo, o.methods.iter().map(|m| m.0).collect()))
            .collect(),
        proof: proof.encode_proof()?,
    };
    let sizes = submitted.proof.component_sizes();
    transaction.proof = minicbor::to_vec(submitted)?.into();
    Ok(sizes)
}

async fn storage_root(
    contract: &CompiledContract,
    output: &TransactionOutput,
) -> anyhow::Result<StarstreamValue> {
    let export = contract.get_utxo(&output.instance)?;
    let Some(storage) = export.storage() else {
        anyhow::ensure!(
            output.storage.is_empty(),
            "unexpected storage for stateless UTXO"
        );
        return Ok(StarstreamValue::UNIT_VALUE);
    };
    let mut bytes = output.storage.as_ref();
    let mut record = Val::Record(Vec::new());
    read_value(&mut bytes, &mut record, &Type::Record(storage.ty().clone())).await?;
    anyhow::ensure!(bytes.is_empty(), "trailing UTXO storage bytes");
    let Val::Record(fields) = record else {
        anyhow::bail!("expected storage record")
    };
    let mut types = Vec::with_capacity(fields.len());
    let mut words = Vec::with_capacity(fields.len());
    for (_, field) in fields {
        let (ty, word) = match field {
            Val::S32(x) => (ValType::I32, u64::from(x.cast_unsigned())),
            Val::U32(x) => (ValType::I32, u64::from(x)),
            Val::S64(x) => (ValType::I64, x.cast_unsigned()),
            Val::U64(x) => (ValType::I64, x),
            _ => anyhow::bail!("unsupported storage record field {field:?}"),
        };
        types.push(ty);
        words.push(word);
    }
    Ok(flat_value_root(&types, &words))
}

/// Verify the transaction's execution proof using the contract store and referenced outputs.
/// The proof binds the transaction's required signers. The ledger authenticates signatures separately.
pub async fn verify_transaction_proof(
    prover: &mut TransactionProver,
    transaction: &Transaction,
    client: &impl Client,
    engine: &Engine,
    wizer: &Wizer,
) -> anyhow::Result<()> {
    let mut decoder = minicbor::Decoder::new(&transaction.proof);
    let submitted: SubmittedProof = decoder.decode()?;
    anyhow::ensure!(
        decoder.position() == transaction.proof.len(),
        "trailing transaction proof bytes"
    );
    anyhow::ensure!(
        submitted.outputs.len() == transaction.outputs.len(),
        "output count mismatch"
    );
    let wasm = client.get_contract(submitted.contract).await?;
    anyhow::ensure!(
        <[u8; 32]>::from(Sha256::digest(&wasm)) == submitted.contract,
        "entry contract digest mismatch"
    );
    let mut imports = HashMap::new();
    let root = compile_contract(client, engine, wizer, &wasm, None, &mut imports).await?;
    root.get_coordination_script(&submitted.script)?;
    let entry = EntryCall {
        script: coordination_script_identity(
            &root.instrumented,
            &submitted.contract,
            &submitted.script,
        )?,
        arguments: StarstreamValue(submitted.arguments),
    };
    let mut sources = Vec::new();
    get_or_insert_program(&mut sources, &root, &submitted.contract).is_original_image = true;
    // Contracts as stored, without an input snapshot: what constructors run on.
    let mut originals = HashMap::from([(submitted.contract, root.clone())]);
    let mut storage_contracts = HashMap::from([(submitted.contract, root)]);
    let mut inputs = Vec::with_capacity(transaction.inputs.len());
    let mut input_exports = Vec::with_capacity(transaction.inputs.len());
    for input in &transaction.inputs {
        let utxo = client.get_input_utxo(input).await?;
        let digest = parse_digest(&utxo.contract)?;
        let original = if digest == submitted.contract {
            wasm.clone()
        } else {
            client.get_contract(digest).await?
        };
        anyhow::ensure!(
            <[u8; 32]>::from(Sha256::digest(&original)) == digest,
            "input contract digest mismatch"
        );
        let snapshot = apply_state(&original, &utxo.state)?;
        let external_id = (digest != submitted.contract).then_some(utxo.contract.as_ref());
        let contract =
            compile_contract(client, engine, wizer, &snapshot, external_id, &mut imports).await?;
        get_or_insert_program(&mut sources, &contract, &digest)
            .input_utxos
            .push((inputs.len(), utxo.instance.to_string()));
        inputs.push(InputUtxo {
            storage: storage_root(&contract, &utxo).await?,
            methods: utxo
                .methods
                .iter()
                .map(|&(a, b, c, d)| MethodHash::from_u64_words([a, b, c, d]))
                .collect(),
        });
        input_exports.push((digest, utxo.instance));
        storage_contracts.entry(digest).or_insert(contract);
    }
    for (digest, import) in imports {
        anyhow::ensure!(
            <[u8; 32]>::from(Sha256::digest(&import.wasm)) == digest,
            "import contract digest mismatch"
        );
        if let Some(contract) = import.contract {
            get_or_insert_program(&mut sources, &contract, &digest).is_original_image = true;
            originals.insert(digest, contract.clone());
            storage_contracts.entry(digest).or_insert(contract);
        }
    }

    let mut outputs = Vec::with_capacity(transaction.outputs.len());
    for (output, (utxo, registrations)) in transaction.outputs.iter().zip(submitted.outputs) {
        let digest = parse_digest(&output.contract)?;
        // Input UTXOs receive the first local IDs in transaction order.
        if let Some((original, export)) = input_exports.get(utxo as usize) {
            anyhow::ensure!(
                *original == digest && export == &output.instance,
                "surviving input changed its contract or UTXO export"
            );
        } else {
            // A new output's proof must run a constructor of the contract and
            // UTXO export the ledger will record for it.
            let original = originals
                .get(&digest)
                .context("unknown contract of new output")?;
            get_or_insert_program(&mut sources, original, &digest)
                .new_utxos
                .push((utxo, output.instance.to_string()));
        }
        let contract = storage_contracts
            .get(&digest)
            .context("unknown output contract")?;
        let stored_methods: BTreeSet<_> = output
            .methods
            .iter()
            .map(|&(a, b, c, d)| MethodHash::from_u64_words([a, b, c, d]).0)
            .collect();
        anyhow::ensure!(
            stored_methods == registrations.iter().copied().collect(),
            "output method set mismatch"
        );
        outputs.push(OutputUtxo {
            utxo,
            methods: registrations.into_iter().map(MethodHash).collect(),
            storage: storage_root(contract, output).await?,
        });
    }
    let expected = prepare_verification(&sources, submitted.proof.circuit_sizes()?)?;
    let proof = submitted.proof.decode_proof(&expected)?;
    prover.verify(
        &proof,
        &expected,
        &TransactionStatement {
            input_tokens: Vec::new(),
            output_tokens: Vec::new(),
            signers: transaction.required_signers.clone(),
            entry,
            inputs,
            outputs,
        },
    )?;
    Ok(())
}
