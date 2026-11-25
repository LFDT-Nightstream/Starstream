//! Coordination-script and UTXO-constructor identities from component metadata.
//!
//! Imports are scalar-only: a contract cannot call a script whose signature
//! carries resources or aggregates. Exports get an identity whenever their
//! arguments and result are passed by value, so such a script can be a
//! transaction's entry; a non-scalar one is simply uncallable through an import.
//!
//! Entry events commit to core Wasm values. A signature that passes a pointer
//! (lists, strings, or more flat values than the canonical ABI passes directly)
//! would commit to an address rather than its contents, so it is rejected.
//!
//! The callee's contract digest is supplied by the contract loader (a contract cannot
//! embed its own content hash). Future proof composition must authenticate that
//! digest against the callee program; matching event identities alone is not enough.
//!
//! A constructor identity binds the contract digest, the UTXO export and the
//! constructor name and its ordered user-parameter types.
use std::collections::{BTreeMap, BTreeSet};

use sha2::{Digest, Sha256};
use wasm_encoder::Encode;
use wit_parser::abi::AbiVariant;
use wit_parser::{Function, Resolve, Type, TypeDefKind, WorldItem};

use crate::{ConstructorIdentity, ScriptIdentity, TemplateBuildError};

// TODO: there probably should be a shared crate with all these constants, but
// currently the compiler hardcodes this
pub(crate) const SCRIPT_INTERFACE: &str = "starstream:contract/scripts";
pub(crate) const TYPED_UTXO_INTERFACE_PREFIX: &str = "starstream:utxo/";

type ConstructorSignatures = BTreeMap<String, BTreeMap<String, [u8; 32]>>;

#[derive(Default)]
pub(crate) struct CoordinatorAbis {
    pub imports: BTreeMap<String, ScriptIdentity>,
    pub exports: BTreeMap<String, ScriptIdentity>,
    /// Constructor parameter signatures keyed by the core interface and field names.
    pub constructor_imports: ConstructorSignatures,
    pub constructor_exports: ConstructorSignatures,
    pub token_interfaces: BTreeSet<String>,
    pub is_component: bool,
    /// Digest of this contract, which owns its `starstream:self` UTXOs.
    pub contract_digest: [u8; 32],
    /// Contract digests of typed UTXO imports, keyed by UTXO interface name.
    pub utxo_contracts: BTreeMap<String, [u8; 32]>,
}

fn error(message: impl Into<String>) -> TemplateBuildError {
    TemplateBuildError::CoordinatorAbi {
        message: message.into(),
    }
}

/// Decode the compiler/CLI's hex digest or the ledger's multibase multihash.
/// Encoding choices never become part of the script identity.
pub(super) fn decode_contract_digest(id: &str) -> Result<[u8; 32], TemplateBuildError> {
    if id.len() == 64 && id.bytes().all(|byte| byte.is_ascii_hexdigit()) {
        let mut digest = [0; 32];
        for (i, byte) in digest.iter_mut().enumerate() {
            *byte = u8::from_str_radix(&id[i * 2..i * 2 + 2], 16).unwrap();
        }
        return Ok(digest);
    }
    let (_, bytes) =
        multibase::decode(id).map_err(|e| error(format!("invalid script contract digest: {e}")))?;
    let hash = multihash::Multihash::<32>::from_bytes(&bytes)
        .map_err(|e| error(format!("invalid script contract multihash: {e}")))?;
    if hash.code() != 0x12 || hash.size() != 32 {
        return Err(error("script contract digest must be SHA-256 (32 bytes)"));
    }
    Ok(hash.digest().try_into().unwrap())
}

impl CoordinatorAbis {
    pub fn constructor_signature(
        &self,
        signatures: &ConstructorSignatures,
        interface: &str,
        field: &str,
    ) -> Result<Option<[u8; 32]>, TemplateBuildError> {
        let signature = signatures.get(interface).and_then(|s| s.get(field));
        if self.is_component && signature.is_none() {
            return Err(error(format!(
                "missing constructor ABI for {interface}#{field}"
            )));
        }
        Ok(signature.copied())
    }

    pub fn decode(
        wasm: &[u8],
        exports: &[&str],
        contract_digest: &[u8; 32],
    ) -> Result<Self, TemplateBuildError> {
        let (resolve, world) = if wasmparser::Parser::is_core_wasm(wasm) {
            let (_, metadata) = wit_component::metadata::decode(wasm)
                .map_err(|e| error(format!("could not decode coordinator metadata: {e:#}")))?;
            (metadata.resolve, metadata.world)
        } else {
            match wit_parser::decoding::decode(wasm)
                .map_err(|e| error(format!("could not decode coordinator types: {e:#}")))?
            {
                wit_parser::decoding::DecodedWasm::Component(resolve, world) => (resolve, world),
                _ => return Err(error("expected a component")),
            }
        };
        let mut abis = Self {
            contract_digest: *contract_digest,
            is_component: !wasmparser::Parser::is_core_wasm(wasm),
            ..Self::default()
        };
        let mut types =
            crate::constructor_types::ConstructorTypes::new(&resolve, world, contract_digest);
        for (key, item) in &resolve.worlds[world].imports {
            let name = resolve.name_world_key(key);
            if (name.starts_with("starstream:self/")
                || name.starts_with(TYPED_UTXO_INTERFACE_PREFIX))
                && let WorldItem::Interface { id, .. } = item
            {
                abis.constructor_imports
                    .insert(name.clone(), types.signatures(*id, false)?);
            }
            if let Some(utxo) = name.strip_prefix(TYPED_UTXO_INTERFACE_PREFIX) {
                // A missing id is reported if a constructor of it is imported.
                if let WorldItem::Interface {
                    external_id: Some(id),
                    ..
                } = item
                {
                    abis.utxo_contracts
                        .insert(utxo.to_owned(), decode_contract_digest(id)?);
                }
                continue;
            }
            if name != SCRIPT_INTERFACE {
                continue;
            }
            let WorldItem::Interface { id, .. } = item else {
                return Err(error("script imports must be an interface"));
            };
            for (name, function) in &resolve.interfaces[*id].functions {
                let id = function
                    .external_id
                    .as_deref()
                    .ok_or_else(|| error(format!("script import {name} has no external-id")))?;
                abis.imports.insert(
                    name.clone(),
                    identity(
                        &resolve,
                        &decode_contract_digest(id)?,
                        function,
                        Signature::Scalar,
                    )?,
                );
            }
        }
        for (key, item) in &resolve.worlds[world].exports {
            let name = resolve.name_world_key(key);
            if let WorldItem::Interface { id, .. } = item {
                let interface = &resolve.interfaces[*id];
                if let Some(Type::Id(result)) = interface
                    .functions
                    .get("set-storage")
                    .and_then(|f| f.result)
                    && let TypeDefKind::Handle(wit_parser::Handle::Own(resource)) =
                        resolve.types[result].kind
                    && interface.types.get("token") == Some(&resource)
                {
                    abis.token_interfaces.insert(name.clone());
                }
                abis.constructor_exports
                    .insert(name.clone(), types.signatures(*id, true)?);
            }
            if !exports.contains(&name.as_str()) {
                continue;
            }
            let WorldItem::Function(function) = item else {
                return Err(error(format!("script export {name} is not a function")));
            };
            abis.exports.insert(
                name,
                identity(&resolve, contract_digest, function, Signature::ByValue)?,
            );
        }
        for name in exports {
            if !abis.exports.contains_key(*name) {
                return Err(error(format!(
                    "script export {name} has no component ABI metadata"
                )));
            }
        }
        Ok(abis)
    }
}

/// Which signatures may be given an identity.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Signature {
    /// Callable through an import: every type is a scalar.
    Scalar,
    /// An export: other by-value types are marked opaque. No import can name
    /// such an identity, and the contract digest already fixes the
    /// full type.
    ByValue,
}

/// Type code of anything that is not a supported scalar.
const OPAQUE: u8 = 0xff;

fn type_code(resolve: &Resolve, ty: Type, mode: Signature) -> Result<u8, TemplateBuildError> {
    match scalar(resolve, ty) {
        Err(_) if mode == Signature::ByValue => Ok(OPAQUE),
        code => code,
    }
}

/// Whether the canonical ABI represents `ty` by its flat values alone, with
/// nothing behind a pointer.
fn by_value(resolve: &Resolve, ty: Type) -> bool {
    let all = |types: &mut dyn Iterator<Item = Type>| {
        for ty in types {
            if !by_value(resolve, ty) {
                return false;
            }
        }
        true
    };
    let Type::Id(id) = ty else {
        return !matches!(ty, Type::String | Type::ErrorContext);
    };
    match &resolve.types[id].kind {
        TypeDefKind::Type(inner) => by_value(resolve, *inner),
        TypeDefKind::Record(record) => all(&mut record.fields.iter().map(|field| field.ty)),
        TypeDefKind::Tuple(tuple) => all(&mut tuple.types.iter().copied()),
        TypeDefKind::Variant(variant) => all(&mut variant.cases.iter().filter_map(|case| case.ty)),
        TypeDefKind::Option(inner) => by_value(resolve, *inner),
        TypeDefKind::Result(result) => all(&mut [result.ok, result.err].into_iter().flatten()),
        TypeDefKind::Flags(_) | TypeDefKind::Enum(_) | TypeDefKind::Handle(_) => true,
        _ => false,
    }
}

fn scalar(resolve: &Resolve, mut ty: Type) -> Result<u8, TemplateBuildError> {
    while let Type::Id(id) = ty {
        if let TypeDefKind::Type(inner) = resolve.types[id].kind {
            ty = inner;
        } else {
            return Err(error(
                "coordinator calls currently support only scalar arguments/results; resources and aggregates are deferred",
            ));
        }
    }
    Ok(match ty {
        Type::Bool => 1,
        Type::U8 => 2,
        Type::S8 => 3,
        Type::U16 => 4,
        Type::S16 => 5,
        Type::U32 => 6,
        Type::S32 => 7,
        Type::U64 => 8,
        Type::S64 => 9,
        _ => return Err(error("unsupported coordinator scalar type")),
    })
}

fn identity(
    resolve: &Resolve,
    contract_digest: &[u8; 32],
    function: &Function,
    mode: Signature,
) -> Result<ScriptIdentity, TemplateBuildError> {
    if mode == Signature::Scalar && function.params.len() > 16 {
        return Err(error(
            "coordinator calls with indirect canonical ABI arguments are deferred (maximum 16 scalar parameters)",
        ));
    }
    if mode == Signature::ByValue {
        let signature = resolve.wasm_signature(AbiVariant::GuestExport, function);
        let types = function.params.iter().map(|param| param.ty);
        if signature.indirect_params
            || signature.retptr
            || !types.chain(function.result).all(|ty| by_value(resolve, ty))
        {
            return Err(error(format!(
                "script export {} passes arguments or its result through memory, which entry events do not commit to yet",
                function.name
            )));
        }
    }
    let mut hash = Sha256::new();
    hash.update(b"starstream:coordinator:v1\0");
    hash.update(contract_digest);
    hash.update((function.name.len() as u64).to_le_bytes());
    hash.update(function.name.as_bytes());
    hash.update((function.params.len() as u64).to_le_bytes());
    for param in &function.params {
        hash.update([type_code(resolve, param.ty, mode)?]);
    }
    hash.update([match function.result {
        Some(ty) => type_code(resolve, ty, mode)?,
        None => 0,
    }]);
    let digest = hash.finalize();
    Ok(ScriptIdentity(std::array::from_fn(|i| {
        u32::from_le_bytes(digest[i * 4..i * 4 + 4].try_into().unwrap())
    })))
}

/// Identity committed by `NewUtxo` and by the constructor's `EnterConstructor`.
///
/// `utxo` is the UTXO's interface name and `constructor` its name without the
/// `[static]utxo.` prefix, both as exported by the contract with `contract_digest`.
pub(crate) fn hash_constructor(
    contract_digest: &[u8; 32],
    utxo: &str,
    constructor: &str,
    params: &[wasmparser::ValType],
    signature: Option<[u8; 32]>,
) -> Result<ConstructorIdentity, TemplateBuildError> {
    let mut hash = Sha256::new();
    hash.update(b"starstream:constructor\0");
    hash.update(contract_digest);
    for name in [utxo, constructor] {
        hash.update((name.len() as u64).to_le_bytes());
        hash.update(name.as_bytes());
    }
    hash.update((params.len() as u64).to_le_bytes());
    let mut encoded_params = Vec::with_capacity(params.len());
    for ty in params {
        let ty = match ty {
            wasmparser::ValType::I32 => wasm_encoder::ValType::I32,
            wasmparser::ValType::I64 => wasm_encoder::ValType::I64,
            _ => return Err(error(format!("unsupported constructor parameter {ty}"))),
        };
        ty.encode(&mut encoded_params);
    }
    hash.update(encoded_params);
    if let Some(signature) = signature {
        hash.update(signature);
    }
    let digest = hash.finalize();
    Ok(ConstructorIdentity(std::array::from_fn(|i| {
        u32::from_le_bytes(digest[i * 4..i * 4 + 4].try_into().unwrap())
    })))
}

/// Identity committed by the entry event of a coordination script.
pub fn coordination_script_identity(
    wasm: &[u8],
    digest: &[u8; 32],
    name: &str,
) -> Result<ScriptIdentity, TemplateBuildError> {
    Ok(CoordinatorAbis::decode(wasm, &[name], digest)?.exports[name])
}

#[cfg(test)]
mod tests {
    use super::*;

    fn script(id: &str, declaration: &str) -> ScriptIdentity {
        try_script(id, declaration, Signature::Scalar).unwrap()
    }

    fn try_script(
        id: &str,
        declaration: &str,
        mode: Signature,
    ) -> Result<ScriptIdentity, TemplateBuildError> {
        script_for_digest(&Sha256::digest(id.as_bytes()).into(), declaration, mode)
    }

    fn script_for_digest(
        digest: &[u8; 32],
        declaration: &str,
        mode: Signature,
    ) -> Result<ScriptIdentity, TemplateBuildError> {
        let mut resolve = Resolve::default();
        let package = resolve
            .push_str(
                "test.wit",
                &format!("package test:coord; world root {{ {declaration} }}"),
            )
            .unwrap();
        let world = resolve.packages[package].worlds["root"];
        let function = resolve.worlds[world]
            .exports
            .values()
            .find_map(|item| match item {
                WorldItem::Function(function) => Some(function),
                _ => None,
            })
            .unwrap();
        identity(&resolve, digest, function, mode)
    }

    #[test]
    fn identity_is_independent_of_contract_digest_encoding() {
        let digest = [0xab; 32];
        let declaration = "export foo: func(x: s64) -> s64;";
        let expected = script_for_digest(&digest, declaration, Signature::ByValue).unwrap();
        let hash = multihash::Multihash::<32>::wrap(0x12, &digest).unwrap();
        let encodings = [
            "ab".repeat(32),
            "AB".repeat(32),
            multibase::encode(multibase::Base::Base32Lower, hash.to_bytes()),
            multibase::encode(multibase::Base::Base32Upper, hash.to_bytes()),
            multibase::encode(multibase::Base::Base58Btc, hash.to_bytes()),
        ];
        for id in encodings {
            let decoded = decode_contract_digest(&id).unwrap();
            assert_eq!(decoded, digest);
            assert_eq!(
                script_for_digest(&decoded, declaration, Signature::Scalar).unwrap(),
                expected,
            );
        }
    }

    #[test]
    fn rejects_invalid_contract_digests() {
        for id in ["", "contract-a", &"ab".repeat(31), &"gg".repeat(32)] {
            assert!(decode_contract_digest(id).is_err());
        }
        for (code, bytes) in [
            (0x13, vec![0; 32]),
            (0x12, vec![0; 31]),
            (0x12, vec![0; 33]),
        ] {
            let hash = multihash::Multihash::<64>::wrap(code, &bytes).unwrap();
            let id = multibase::encode(multibase::Base::Base32Lower, hash.to_bytes());
            assert!(decode_contract_digest(&id).is_err());
        }
    }

    #[test]
    fn only_scalar_scripts_can_be_imported() {
        for declaration in [
            "record pair { a: s64, b: s64 } export foo: func(x: pair);",
            "resource r; export foo: func(x: borrow<r>);",
            "export foo: func(x: option<u8>) -> s64;",
        ] {
            // An importer cannot name the script...
            assert!(try_script("contract-a", declaration, Signature::Scalar).is_err());
            // ...but it still has an identity as a transaction's entry, which
            // no scalar signature can collide with.
            let entry = try_script("contract-a", declaration, Signature::ByValue).unwrap();
            assert_ne!(entry, script("contract-a", "export foo: func(x: s64);"));
            assert_ne!(entry, script("contract-a", "export foo: func() -> s64;"));
            assert_ne!(
                entry,
                script("contract-a", "export foo: func(x: s64) -> s64;")
            );
        }
        // Anything passed through memory would be committed by address only.
        let wide = (0..17).map(|i| format!("p{i}: s64")).collect::<Vec<_>>();
        for declaration in [
            "export foo: func() -> list<u8>;",
            "export foo: func(x: string) -> s64;",
            "record bytes { data: list<u8> } export foo: func(x: bytes);",
            "export foo: func(x: option<string>);",
            "export foo: func() -> tuple<s64, s64>;",
            &format!("export foo: func({});", wide.join(", ")),
        ] {
            assert!(try_script("contract-a", declaration, Signature::ByValue).is_err());
        }
        // A scalar signature has the same identity on both sides.
        let declaration = "export foo: func(x: s64) -> s64;";
        assert_eq!(
            try_script("contract-a", declaration, Signature::ByValue).unwrap(),
            script("contract-a", declaration)
        );
    }

    #[test]
    fn identity_binds_contract_export_and_scalar_signature() {
        let base = script("contract-a", "export foo: func(x: s64) -> s64;");
        assert_ne!(
            base,
            script("contract-b", "export foo: func(x: s64) -> s64;")
        );
        assert_ne!(
            base,
            script("contract-a", "export bar: func(x: s64) -> s64;")
        );
        assert_ne!(
            base,
            script("contract-a", "export foo: func(x: u64) -> s64;")
        );
        assert_ne!(
            base,
            script("contract-a", "export foo: func(x: s64) -> u64;")
        );
        assert_eq!(
            base,
            script("contract-a", "export foo: func(renamed: s64) -> s64;")
        );
    }
}
