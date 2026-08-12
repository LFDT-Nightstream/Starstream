use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use wasm_encoder::{
    Alias, ComponentExportKind, ComponentOuterAliasKind, ComponentType, ComponentTypeEncoder,
    ComponentTypeRef, ComponentValType, InstanceType, PrimitiveValType, TypeBounds,
};

use crate::{
    component_abi::{ComponentAbiFunctionSignature, ComponentAbiType, Resource},
    to_kebab_case,
};

#[derive(Default, Clone)]
pub struct TypeBuilder<T> {
    pub inner: T,
    imported_interfaces: HashSet<String>,
    component_to_encoded: HashMap<Rc<ComponentAbiType>, ComponentValType>,
    resources: Vec<(Rc<Resource>, u32)>,
}

impl<T: TypeRegistry> TypeBuilder<T> {
    pub fn ty(&mut self) -> (u32, ComponentTypeEncoder<'_>) {
        (self.inner.type_count(), self.inner.ty())
    }

    pub fn encode_func<'a>(
        &mut self,
        params: impl Iterator<Item = (&'a str, Rc<ComponentAbiType>)>,
        result: Option<&Rc<ComponentAbiType>>,
    ) -> u32 {
        let params = params
            .map(|p| (p.0, self.encode_value(&p.1)))
            .collect::<Vec<_>>();
        let result = result.map(|r| self.encode_value(r));

        let (idx, ty) = self.ty();
        ty.function().params(params).result(result);
        idx
    }

    pub fn encode_func_sig(&mut self, signature: &ComponentAbiFunctionSignature) -> u32 {
        let params = signature
            .params
            .iter()
            .map(|p| (p.0.as_str(), self.encode_value(&p.1)))
            .collect::<Vec<_>>();
        let result = signature.result.as_ref().map(|r| self.encode_value(r));

        let (idx, ty) = self.ty();
        ty.function().params(params).result(result);
        idx
    }

    pub fn encode_value(&mut self, ty: &Rc<ComponentAbiType>) -> ComponentValType {
        if let Some(&cvt) = self.component_to_encoded.get(ty) {
            return cvt;
        }

        let cvt = match &**ty {
            ComponentAbiType::Bool => ComponentValType::Primitive(PrimitiveValType::Bool),
            ComponentAbiType::S8 => ComponentValType::Primitive(PrimitiveValType::S8),
            ComponentAbiType::U8 => ComponentValType::Primitive(PrimitiveValType::U8),
            ComponentAbiType::S16 => ComponentValType::Primitive(PrimitiveValType::S16),
            ComponentAbiType::U16 => ComponentValType::Primitive(PrimitiveValType::U16),
            ComponentAbiType::S32 => ComponentValType::Primitive(PrimitiveValType::S32),
            ComponentAbiType::U32 => ComponentValType::Primitive(PrimitiveValType::U32),
            ComponentAbiType::S64 => ComponentValType::Primitive(PrimitiveValType::S64),
            ComponentAbiType::U64 => ComponentValType::Primitive(PrimitiveValType::U64),
            ComponentAbiType::F32 => ComponentValType::Primitive(PrimitiveValType::F32),
            ComponentAbiType::F64 => ComponentValType::Primitive(PrimitiveValType::F64),
            ComponentAbiType::Char => ComponentValType::Primitive(PrimitiveValType::Char),
            ComponentAbiType::String => ComponentValType::Primitive(PrimitiveValType::String),
            ComponentAbiType::ErrorContext => {
                ComponentValType::Primitive(PrimitiveValType::ErrorContext)
            }
            ComponentAbiType::List { .. } => todo!(),
            ComponentAbiType::Record { fields } => {
                let fields: Vec<_> = fields
                    .iter()
                    .map(|f| (f.0.as_str(), self.encode_value(&f.1)))
                    .collect();
                let (idx, ty) = self.ty();
                ty.defined_type().record(fields);
                ComponentValType::Type(idx)
            }
            ComponentAbiType::Tuple { fields } => {
                let fields: Vec<_> = fields.iter().map(|f| self.encode_value(f)).collect();
                let (idx, ty) = self.ty();
                ty.defined_type().tuple(fields);
                ComponentValType::Type(idx)
            }
            ComponentAbiType::Variant { cases } => {
                let cases: Vec<_> = cases
                    .iter()
                    .map(|(name, ty)| (name.as_str(), ty.as_ref().map(|ty| self.encode_value(ty))))
                    .collect();
                let (idx, ty) = self.ty();
                ty.defined_type().variant(cases);
                ComponentValType::Type(idx)
            }
            ComponentAbiType::Option { inner } => {
                let inner_val = self.encode_value(inner);
                let (idx, ty) = self.ty();
                ty.defined_type().option(inner_val);
                ComponentValType::Type(idx)
            }
            ComponentAbiType::Result { ok, err } => {
                let ok_val = ok.as_ref().map(|t| self.encode_value(t));
                let err_val = err.as_ref().map(|t| self.encode_value(t));
                let (idx, ty) = self.ty();
                ty.defined_type().result(ok_val, err_val);
                ComponentValType::Type(idx)
            }
            ComponentAbiType::Flags { .. } => todo!(),
            ComponentAbiType::Own { resource } | ComponentAbiType::Borrow { resource } => {
                // Handled separately by register_resource.
                panic!("referencing unimported resource {resource:?}")
            }
            ComponentAbiType::Stream => todo!(),
            ComponentAbiType::Future => todo!(),
        };

        self.component_to_encoded.insert(ty.clone(), cvt);
        cvt
    }

    pub fn export_ty(&mut self, name: &str, ty: &Rc<ComponentAbiType>) {
        let ComponentValType::Type(idx) = self.encode_value(ty) else {
            unreachable!()
        };
        // "Exporting" a type consists of importing it with an equality constraint.
        let new_idx = self.inner.type_count();
        self.inner
            .import_or_export(name, ComponentTypeRef::Type(TypeBounds::Eq(idx)));
        // Future uses must also refer to the imported version.
        self.component_to_encoded
            .insert(ty.clone(), ComponentValType::Type(new_idx));
    }

    pub fn export_fn(&mut self, name: &str, signature: &ComponentAbiFunctionSignature) {
        let type_idx = self.encode_func_sig(signature);
        self.inner.export(name, ComponentTypeRef::Func(type_idx));
    }

    pub fn export_fn_2<'a>(
        &mut self,
        name: &str,
        params: impl Iterator<Item = (&'a str, Rc<ComponentAbiType>)>,
        result: Option<&Rc<ComponentAbiType>>,
    ) {
        let type_idx = self.encode_func(params, result);
        self.inner.export(name, ComponentTypeRef::Func(type_idx));
    }

    pub fn fresh_resource(&mut self, name: &str, full_name: &str) -> Rc<Resource> {
        let rc = Rc::new(Resource {
            name: to_kebab_case(name),
            full_name: to_kebab_case(full_name),
        });
        let idx = self.inner.type_count();
        self.inner
            .import_or_export(&rc.name, ComponentTypeRef::Type(TypeBounds::SubResource));
        self.register_resource(&rc, idx);
        self.resources.push((rc.clone(), idx));
        rc
    }

    fn register_resource(&mut self, resource: &Rc<Resource>, resource_ty: u32) {
        {
            let (idx, ty) = (self.inner.type_count(), self.inner.ty());
            ty.defined_type().borrow(resource_ty);
            self.component_to_encoded.insert(
                Rc::new(ComponentAbiType::Borrow {
                    resource: resource.clone(),
                }),
                ComponentValType::Type(idx),
            );
        }
        {
            let (idx, ty) = (self.inner.type_count(), self.inner.ty());
            ty.defined_type().own(resource_ty);
            self.component_to_encoded.insert(
                Rc::new(ComponentAbiType::Own {
                    resource: resource.clone(),
                }),
                ComponentValType::Type(idx),
            );
        }
    }

    pub fn export_interface(&mut self, name: &str, child: &TypeBuilder<InstanceType>) -> u32 {
        let (idx, ty) = self.ty();
        ty.instance(&child.inner);
        self.inner.export(name, ComponentTypeRef::Instance(idx));
        // No lift_resources here as it isn't legal binary WIT.
        idx
    }
}

impl TypeBuilder<ComponentType> {
    pub fn has_imported(&self, name: &str) -> bool {
        self.imported_interfaces.contains(name)
    }

    pub fn import_interface(&mut self, name: &str, child: &TypeBuilder<InstanceType>) -> u32 {
        assert!(self.imported_interfaces.insert(name.to_owned()));

        let (idx, ty) = self.ty();
        ty.instance(&child.inner);

        let instance_idx = self.inner.instance_count();
        self.inner
            .import_or_export(name, ComponentTypeRef::Instance(idx));
        self.lift_resources(child, instance_idx);

        idx
    }

    fn lift_resources(&mut self, child: &TypeBuilder<InstanceType>, instance: u32) {
        // Lift resources declared in the child interface.
        for (resource, _) in &child.resources {
            let alias_idx = self.inner.type_count();
            self.inner.alias(Alias::InstanceExport {
                instance,
                kind: ComponentExportKind::Type,
                name: &resource.name,
            });

            self.register_resource(resource, alias_idx);
            // Following must use alias_idx or else wit-parser hits an unreachable!(), nice.
            self.resources.push((resource.clone(), alias_idx));
        }
    }
}

impl TypeBuilder<InstanceType> {
    pub fn new_interface() -> Self {
        Self::default()
    }

    pub fn inherit_parent(&mut self, parent: &TypeBuilder<ComponentType>) {
        // Lower resources declared in the parent interface.
        for (resource, instance) in &parent.resources {
            // "alias" makes the type available on a binary level.
            let alias_idx = self.inner.type_count();
            self.inner.alias(Alias::Outer {
                kind: ComponentOuterAliasKind::Type,
                count: 1,
                index: *instance,
            });

            // "equality" amounts to a `use` statement (mandatory).
            let equality_idx = self.inner.type_count();
            self.inner.export(
                // The name to `use` as.
                &resource.full_name,
                ComponentTypeRef::Type(TypeBounds::Eq(alias_idx)),
            );

            self.register_resource(resource, equality_idx);
            // no self.resources.push so as to not double-export
        }
    }
}

/// Abstraction over [`ComponentType`] and [`InstanceType`].
#[allow(dead_code)]
pub trait TypeRegistry {
    fn ty(&mut self) -> ComponentTypeEncoder<'_>;
    fn alias(&mut self, alias: Alias<'_>);
    fn export(&mut self, name: &str, ty: ComponentTypeRef);
    fn import_or_export(&mut self, name: &str, ty: ComponentTypeRef);
    fn type_count(&self) -> u32;
    fn instance_count(&self) -> u32;
}

// `world`
impl TypeRegistry for ComponentType {
    fn ty(&mut self) -> ComponentTypeEncoder<'_> {
        self.ty()
    }

    fn alias(&mut self, alias: Alias<'_>) {
        self.alias(alias);
    }

    fn export(&mut self, name: &str, ty: ComponentTypeRef) {
        self.export(name, ty);
    }

    fn import_or_export(&mut self, name: &str, ty: ComponentTypeRef) {
        self.import(name, ty);
    }

    fn type_count(&self) -> u32 {
        self.type_count()
    }

    fn instance_count(&self) -> u32 {
        self.instance_count()
    }
}

// `interface`
impl TypeRegistry for InstanceType {
    fn ty(&mut self) -> ComponentTypeEncoder<'_> {
        self.ty()
    }

    fn alias(&mut self, alias: Alias<'_>) {
        self.alias(alias);
    }

    fn export(&mut self, name: &str, ty: ComponentTypeRef) {
        self.export(name, ty);
    }

    fn import_or_export(&mut self, name: &str, ty: ComponentTypeRef) {
        self.export(name, ty);
    }

    fn type_count(&self) -> u32 {
        self.type_count()
    }

    fn instance_count(&self) -> u32 {
        self.instance_count()
    }
}
