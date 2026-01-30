#![allow(dead_code)]
//! Component model canonical ABI implementation.
//!
//! Spec: https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md
#![allow(unused_variables)]

use std::{collections::HashMap, rc::Rc};

use wasm_encoder::{
    ComponentType, ComponentTypeEncoder, ComponentValType, InstanceType, InstructionSink, MemArg,
    PrimitiveValType,
};

/// Despecialized component value type. Like [wasm_encoder::ComponentValType].
#[derive(Hash, PartialEq, Eq, Debug)]
pub enum ComponentAbiType {
    Bool,
    S8,
    U8,
    S16,
    U16,
    S32,
    U32,
    S64,
    U64,
    F32,
    F64,
    Char,
    String,
    ErrorContext,
    List {
        ty: Rc<ComponentAbiType>,
        len: Option<u32>,
    },
    Record {
        fields: Vec<(String, Rc<ComponentAbiType>)>,
    },
    Tuple {
        fields: Vec<Rc<ComponentAbiType>>,
    },
    Variant {
        cases: Vec<(String, Option<Rc<ComponentAbiType>>)>,
    },
    // TODO: Enum
    // TODO: Option
    // TODO: Result
    Flags {
        labels: Vec<String>,
    },
    Own,
    Borrow,
    Stream,
    Future,
}

impl ComponentAbiType {
    pub fn size_align(&self) -> (u32, u32) {
        (self.elem_size(), self.alignment())
    }

    // https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md#alignment
    pub fn alignment(&self) -> u32 {
        match self {
            ComponentAbiType::Bool => 1,
            ComponentAbiType::S8 | ComponentAbiType::U8 => 1,
            ComponentAbiType::S16 | ComponentAbiType::U16 => 2,
            ComponentAbiType::S32 | ComponentAbiType::U32 => 4,
            ComponentAbiType::S64 | ComponentAbiType::U64 => 8,
            ComponentAbiType::F32 => 4,
            ComponentAbiType::F64 => 8,
            ComponentAbiType::Char => 4,
            ComponentAbiType::String => 4,
            ComponentAbiType::ErrorContext => 4,
            ComponentAbiType::List { ty, len } => todo!(),
            ComponentAbiType::Record { fields } => {
                Self::alignment_record(fields.iter().map(|x| &*x.1))
            }
            ComponentAbiType::Tuple { fields } => {
                Self::alignment_record(fields.iter().map(|f| &**f))
            }
            ComponentAbiType::Variant { cases } => Self::alignment_variant(cases),
            ComponentAbiType::Flags { labels } => todo!(),
            ComponentAbiType::Own | ComponentAbiType::Borrow => 4,
            ComponentAbiType::Stream | ComponentAbiType::Future => 4,
        }
    }

    fn alignment_record<'a>(fields: impl Iterator<Item = &'a ComponentAbiType>) -> u32 {
        let mut a = 1;
        for f in fields {
            a = a.max(f.alignment());
        }
        a
    }

    fn alignment_variant(cases: &[(String, Option<Rc<ComponentAbiType>>)]) -> u32 {
        Self::discriminant_type(cases)
            .alignment()
            .max(Self::max_case_alignment(cases))
    }

    fn discriminant_type(cases: &[(String, Option<Rc<ComponentAbiType>>)]) -> ComponentAbiType {
        let n = cases.len();
        if n <= usize::from(u8::MAX) {
            ComponentAbiType::U8
        } else if n <= usize::from(u16::MAX) {
            ComponentAbiType::U16
        } else if let Ok(m) = usize::try_from(u32::MAX)
            && n <= m
        {
            ComponentAbiType::U32
        } else {
            panic!("number of cases out of range for variant: {n}")
        }
    }

    fn max_case_alignment<'a>(cases: &[(String, Option<Rc<ComponentAbiType>>)]) -> u32 {
        let mut a = 1;
        for c in cases {
            if let Some(t) = &c.1 {
                a = a.max(t.alignment());
            }
        }
        a
    }

    // https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md#element-size
    pub fn elem_size(&self) -> u32 {
        match self {
            ComponentAbiType::Bool => 1,
            ComponentAbiType::S8 | ComponentAbiType::U8 => 1,
            ComponentAbiType::S16 | ComponentAbiType::U16 => 2,
            ComponentAbiType::S32 | ComponentAbiType::U32 => 4,
            ComponentAbiType::S64 | ComponentAbiType::U64 => 8,
            ComponentAbiType::F32 => 4,
            ComponentAbiType::F64 => 8,
            ComponentAbiType::Char => 4,
            ComponentAbiType::String => 8,
            ComponentAbiType::ErrorContext => 4,
            ComponentAbiType::List { ty, len } => todo!(),
            ComponentAbiType::Record { fields } => {
                Self::elem_size_record(fields.iter().map(|x| &*x.1))
            }
            ComponentAbiType::Tuple { fields } => {
                Self::elem_size_record(fields.iter().map(|f| &**f))
            }
            ComponentAbiType::Variant { cases } => Self::elem_size_variant(cases),
            ComponentAbiType::Flags { labels } => todo!(),
            ComponentAbiType::Own | ComponentAbiType::Borrow => 4,
            ComponentAbiType::Stream | ComponentAbiType::Future => 4,
        }
    }

    fn elem_size_record<'a>(fields: impl Iterator<Item = &'a ComponentAbiType> + Clone) -> u32 {
        let mut s = 0u32;
        for f in fields.clone() {
            s = s.next_multiple_of(f.alignment());
            s += f.elem_size();
        }
        assert!(s > 0);
        s.next_multiple_of(Self::alignment_record(fields))
    }

    fn elem_size_variant(cases: &[(String, Option<Rc<ComponentAbiType>>)]) -> u32 {
        let s = Self::discriminant_type(cases)
            .elem_size()
            .next_multiple_of(Self::max_case_alignment(cases));
        let mut cs = 0;
        for c in cases {
            if let Some(t) = &c.1 {
                cs = cs.max(t.elem_size());
            }
        }
        (s + cs).next_multiple_of(Self::alignment_variant(cases))
    }

    // https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md#storing
    pub fn get_store_fns(
        &self,
        memory_index: u32,
        offset: u64,
        out: &mut Vec<Box<dyn Fn(InstructionSink)>>,
    ) {
        let mem_arg = MemArg {
            offset,
            align: log2(self.alignment()),
            memory_index,
        };
        match self {
            ComponentAbiType::Bool => {
                out.push(Box::new(move |mut i| {
                    i.i32_store8(mem_arg);
                }));
            }
            ComponentAbiType::S8 => todo!(),
            ComponentAbiType::U8 => todo!(),
            ComponentAbiType::S16 => todo!(),
            ComponentAbiType::U16 => todo!(),
            ComponentAbiType::S32 => todo!(),
            ComponentAbiType::U32 => todo!(),
            ComponentAbiType::S64 => {
                out.push(Box::new(move |mut i: InstructionSink<'_>| {
                    i.i64_store(mem_arg);
                }));
            }
            ComponentAbiType::U64 => todo!(),
            ComponentAbiType::F32 => todo!(),
            ComponentAbiType::F64 => todo!(),
            ComponentAbiType::Char => todo!(),
            ComponentAbiType::String => todo!(),
            ComponentAbiType::ErrorContext => todo!(),
            ComponentAbiType::List { .. } => todo!(),
            ComponentAbiType::Record { fields } => {
                let mut o = 0u64;
                for (_, f) in fields {
                    o = o.next_multiple_of(u64::from(f.alignment()));
                    f.get_store_fns(memory_index, offset + o, out);
                    o += u64::from(f.elem_size());
                }
            }
            ComponentAbiType::Tuple { fields } => {
                let mut o = 0u64;
                for f in fields {
                    o = o.next_multiple_of(u64::from(f.alignment()));
                    f.get_store_fns(memory_index, offset + o, out);
                    o += u64::from(f.elem_size());
                }
            }
            ComponentAbiType::Variant { .. } => todo!(),
            ComponentAbiType::Flags { .. } => todo!(),
            ComponentAbiType::Own => todo!(),
            ComponentAbiType::Borrow => todo!(),
            ComponentAbiType::Stream => todo!(),
            ComponentAbiType::Future => todo!(),
        }
    }
}

/// Abstraction over [ComponentType] and [InstanceType].
pub trait TypeRegistry {
    fn ty(&mut self) -> (u32, ComponentTypeEncoder<'_>);
}

impl TypeRegistry for ComponentType {
    fn ty(&mut self) -> (u32, ComponentTypeEncoder<'_>) {
        let idx = self.type_count();
        (idx, self.ty())
    }
}

impl TypeRegistry for InstanceType {
    fn ty(&mut self) -> (u32, ComponentTypeEncoder<'_>) {
        let idx = self.type_count();
        (idx, self.ty())
    }
}

#[derive(Default)]
pub struct TypeBuilder<T> {
    pub inner: T,
    pub component_to_encoded: HashMap<Rc<ComponentAbiType>, ComponentValType>,
}

impl<T: TypeRegistry> TypeBuilder<T> {
    pub fn encode_func<'a>(
        &mut self,
        params: impl Iterator<Item = (&'a str, Rc<ComponentAbiType>)>,
        result: Option<&Rc<ComponentAbiType>>,
    ) -> u32 {
        let params = params
            .map(|p| (p.0, self.encode_value(&p.1)))
            .collect::<Vec<_>>();
        let result = result.map(|r| self.encode_value(r));

        let (idx, enc) = self.inner.ty();
        enc.function().params(params).result(result);
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
                let (idx, ty) = self.inner.ty();
                ty.defined_type().record(fields);
                ComponentValType::Type(idx)
            }
            ComponentAbiType::Tuple { fields } => {
                let fields: Vec<_> = fields.iter().map(|f| self.encode_value(f)).collect();
                let (idx, ty) = self.inner.ty();
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
                let (idx, ty) = self.inner.ty();
                ty.defined_type().variant(cases);
                ComponentValType::Type(idx)
            }
            ComponentAbiType::Flags { .. } => todo!(),
            ComponentAbiType::Own => todo!(),
            ComponentAbiType::Borrow => todo!(),
            ComponentAbiType::Stream => todo!(),
            ComponentAbiType::Future => todo!(),
        };

        self.component_to_encoded.insert(ty.clone(), cvt);
        cvt
    }
}

// https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md#flattening
pub const MAX_FLAT_PARAMS: usize = 16;
pub const MAX_FLAT_ASYNC_PARAMS: usize = 4;
pub const MAX_FLAT_RESULTS: usize = 1;

// Misc
fn log2(alignment: u32) -> u32 {
    match alignment {
        1 => 0,
        2 => 1,
        4 => 2,
        8 => 3,
        _ => panic!("alignment too big or not a power of 2: {alignment}"),
    }
}
