//! Structural component types used in constructor signatures. Each child type
//! contributes a fixed-size digest so repeated uses of a type share the work.
use std::collections::{BTreeMap, HashMap};

use sha2::{Digest, Sha256};
use wit_parser::{Handle, InterfaceId, Resolve, Type, TypeDefKind, TypeOwner, WorldId, WorldItem};

use crate::TemplateBuildError;
use crate::coordinator_abi::decode_contract_digest;

pub(super) struct ConstructorTypes<'a> {
    resolve: &'a Resolve,
    world: WorldId,
    contract: &'a [u8; 32],
    cache: HashMap<Type, [u8; 32]>,
}

fn text(hash: &mut Sha256, value: &str) {
    hash.update((value.len() as u64).to_le_bytes());
    hash.update(value.as_bytes());
}

impl<'a> ConstructorTypes<'a> {
    pub fn new(resolve: &'a Resolve, world: WorldId, contract: &'a [u8; 32]) -> Self {
        Self {
            resolve,
            world,
            contract,
            cache: HashMap::new(),
        }
    }

    pub fn signatures(
        &mut self,
        interface: InterfaceId,
        exported: bool,
    ) -> Result<BTreeMap<String, [u8; 32]>, TemplateBuildError> {
        let mut signatures = BTreeMap::new();
        let interface = &self.resolve.interfaces[interface];
        for (name, function) in &interface.functions {
            let utxo = name.starts_with("[static]utxo.");
            // Mint and burn both use static methods. Only mint returns an owned token.
            let mint = match function.result {
                Some(Type::Id(id)) if name.starts_with("[static]token.") => {
                    matches!(self.resolve.types[id].kind, TypeDefKind::Handle(Handle::Own(resource))
                        if Some(&resource) == interface.types.get("token"))
                }
                _ => false,
            };
            if !utxo && !mint {
                continue;
            }
            if mint {
                let abi = if exported {
                    wit_parser::abi::AbiVariant::GuestExport
                } else {
                    wit_parser::abi::AbiVariant::GuestImport
                };
                if self.resolve.wasm_signature(abi, function).indirect_params
                    || !function.params.iter().all(|param| {
                        crate::coordinator_abi::by_value(self.resolve, param.ty, false)
                    })
                {
                    return Err(TemplateBuildError::CoordinatorAbi {
                        message: format!(
                            "mint {name} requires direct by-value arguments without resource handles"
                        ),
                    });
                }
            }
            let params = function
                .params
                .get(usize::from(exported && utxo)..)
                .ok_or_else(|| TemplateBuildError::CoordinatorAbi {
                    message: format!("missing context parameter on constructor {name}"),
                })?;
            let mut signature = Sha256::new();
            for param in params {
                signature.update(self.digest(param.ty)?);
            }
            signatures.insert(name.clone(), signature.finalize().into());
        }
        Ok(signatures)
    }

    fn digest(&mut self, ty: Type) -> Result<[u8; 32], TemplateBuildError> {
        if let Some(digest) = self.cache.get(&ty) {
            return Ok(*digest);
        }
        let mut hash = Sha256::new();
        match ty {
            Type::Id(id) => {
                let def = &self.resolve.types[id];
                if let TypeDefKind::Type(inner) = def.kind {
                    let digest = self.digest(inner)?;
                    self.cache.insert(ty, digest);
                    return Ok(digest);
                }
                text(&mut hash, def.kind.as_str());
                match &def.kind {
                    TypeDefKind::Record(record) => {
                        for field in &record.fields {
                            text(&mut hash, &field.name);
                            hash.update(self.digest(field.ty)?);
                        }
                    }
                    TypeDefKind::Tuple(tuple) => {
                        for ty in &tuple.types {
                            hash.update(self.digest(*ty)?);
                        }
                    }
                    TypeDefKind::Variant(variant) => {
                        for case in &variant.cases {
                            text(&mut hash, &case.name);
                            self.optional(&mut hash, case.ty)?;
                        }
                    }
                    TypeDefKind::Enum(e) => {
                        for case in &e.cases {
                            text(&mut hash, &case.name);
                        }
                    }
                    TypeDefKind::Flags(flags) => {
                        for flag in &flags.flags {
                            text(&mut hash, &flag.name);
                        }
                    }
                    TypeDefKind::Option(ty) | TypeDefKind::List(ty) => {
                        hash.update(self.digest(*ty)?)
                    }
                    TypeDefKind::FixedLengthList(ty, len) => {
                        hash.update(len.to_le_bytes());
                        hash.update(self.digest(*ty)?);
                    }
                    TypeDefKind::Map(k, v) => {
                        hash.update(self.digest(*k)?);
                        hash.update(self.digest(*v)?);
                    }
                    TypeDefKind::Result(result) => {
                        self.optional(&mut hash, result.ok)?;
                        self.optional(&mut hash, result.err)?;
                    }
                    TypeDefKind::Future(ty) | TypeDefKind::Stream(ty) => {
                        self.optional(&mut hash, *ty)?
                    }
                    TypeDefKind::Handle(Handle::Own(id) | Handle::Borrow(id)) => {
                        hash.update(self.digest(Type::Id(*id))?)
                    }
                    TypeDefKind::Resource => {
                        self.resource_owner(&mut hash, def.owner)?;
                        text(&mut hash, def.name.as_deref().unwrap_or(""));
                    }
                    TypeDefKind::Unknown | TypeDefKind::Type(_) => unreachable!("resolved type"),
                }
            }
            primitive => text(
                &mut hash,
                match primitive {
                    Type::Bool => "bool",
                    Type::S8 => "s8",
                    Type::U8 => "u8",
                    Type::S16 => "s16",
                    Type::U16 => "u16",
                    Type::S32 => "s32",
                    Type::U32 => "u32",
                    Type::S64 => "s64",
                    Type::U64 => "u64",
                    Type::Char => "char",
                    Type::F32 => "f32",
                    Type::F64 => "f64",
                    Type::String => "string",
                    Type::ErrorContext => "error-context",
                    Type::Id(_) => unreachable!(),
                },
            ),
        }
        let digest = hash.finalize().into();
        self.cache.insert(ty, digest);
        Ok(digest)
    }

    fn resource_owner(
        &self,
        hash: &mut Sha256,
        owner: TypeOwner,
    ) -> Result<(), TemplateBuildError> {
        let TypeOwner::Interface(interface) = owner else {
            text(hash, "world");
            hash.update(self.contract);
            return Ok(());
        };
        let world = &self.resolve.worlds[self.world];
        for (key, item) in world.imports.iter().chain(&world.exports) {
            let WorldItem::Interface {
                id, external_id, ..
            } = item
            else {
                continue;
            };
            if *id != interface {
                continue;
            }
            let name = self.resolve.name_world_key(key);
            if let Some(target) = name
                .strip_prefix("starstream:utxo/")
                .or_else(|| name.strip_prefix("starstream:token/"))
            {
                let id =
                    external_id
                        .as_deref()
                        .ok_or_else(|| TemplateBuildError::CoordinatorAbi {
                            message: format!("missing external-id for {name}"),
                        })?;
                text(hash, "contract");
                hash.update(decode_contract_digest(id)?);
                text(hash, target);
            } else if name.starts_with("starstream:self/") || !name.contains('/') {
                text(hash, "contract");
                hash.update(self.contract);
                text(hash, name.strip_prefix("starstream:self/").unwrap_or(&name));
            } else {
                text(hash, "interface");
                text(hash, &name);
            }
            return Ok(());
        }
        let name =
            self.resolve
                .id_of(interface)
                .ok_or_else(|| TemplateBuildError::CoordinatorAbi {
                    message: "constructor resource has no named interface".into(),
                })?;
        text(hash, "interface");
        text(hash, &name);
        Ok(())
    }

    fn optional(&mut self, hash: &mut Sha256, ty: Option<Type>) -> Result<(), TemplateBuildError> {
        hash.update([u8::from(ty.is_some())]);
        if let Some(ty) = ty {
            hash.update(self.digest(ty)?);
        }
        Ok(())
    }
}
