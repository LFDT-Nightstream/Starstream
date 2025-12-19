#![allow(dead_code)]
//! Component model canonical ABI implementation.
//!
//! Spec: https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md
#![allow(unused_variables)]

use std::rc::Rc;

/// Despecialized component type.
#[derive(Hash, PartialEq, Eq)]
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
    // TODO: Tuple
    Variant {
        cases: Vec<(String, Rc<ComponentAbiType>)>,
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
            ComponentAbiType::Variant { cases } => todo!(),
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
            ComponentAbiType::Variant { cases } => todo!(),
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
}

// https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md#flattening
pub const MAX_FLAT_PARAMS: usize = 16;
pub const MAX_FLAT_ASYNC_PARAMS: usize = 4;
pub const MAX_FLAT_RESULTS: usize = 1;
