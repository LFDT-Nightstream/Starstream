use std::{collections::HashMap, rc::Rc};

use wasm_encoder::{
    Alias, ComponentType, ComponentTypeEncoder, ComponentTypeRef, ComponentValType, InstanceType,
    PrimitiveValType,
};

use crate::component_abi::ComponentAbiType;

#[derive(Default)]
pub struct TypeBuilder<T: ?Sized> {
    pub component_to_encoded: HashMap<Rc<ComponentAbiType>, ComponentValType>,
    pub inner: T,
}

impl<T: TypeRegistry> TypeBuilder<T> {
    pub fn ty(&mut self) -> (u32, ComponentTypeEncoder<'_>) {
        let idx = self.inner.type_count();
        (idx, self.inner.ty())
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
                    .map(|(name, ty)| {
                        (
                            name.as_str(),
                            ty.as_ref().map(|ty| self.encode_value(ty)),
                            None,
                        )
                    })
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
            ComponentAbiType::Own { resource } => {
                let (idx, ty) = self.ty();
                ty.defined_type().own(*resource);
                ComponentValType::Type(idx)
            }
            ComponentAbiType::Borrow { resource } => {
                let (idx, ty) = self.ty();
                ty.defined_type().borrow(*resource);
                ComponentValType::Type(idx)
            }
            ComponentAbiType::Stream => todo!(),
            ComponentAbiType::Future => todo!(),
        };

        self.component_to_encoded.insert(ty.clone(), cvt);
        cvt
    }
}

/// Abstraction over [ComponentType] and [InstanceType].
#[allow(dead_code)]
pub trait TypeRegistry {
    fn ty(&mut self) -> ComponentTypeEncoder<'_>;
    fn alias(&mut self, alias: Alias<'_>);
    fn export(&mut self, name: &str, ty: ComponentTypeRef);
    fn type_count(&self) -> u32;
    fn instance_count(&self) -> u32;
}

// ComponentType also has `export`.
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

    fn type_count(&self) -> u32 {
        self.type_count()
    }

    fn instance_count(&self) -> u32 {
        self.instance_count()
    }
}

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

    fn type_count(&self) -> u32 {
        self.type_count()
    }

    fn instance_count(&self) -> u32 {
        self.instance_count()
    }
}
