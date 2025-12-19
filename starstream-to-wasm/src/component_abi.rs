#![allow(dead_code)]
//! Component model canonical ABI implementation.
//!
//! Spec: https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md

use wasm_encoder::{ComponentValType, PrimitiveValType};

// https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md#alignment
pub fn alignment(ty: &ComponentValType) -> u32 {
    match ty {
        ComponentValType::Primitive(p) => alignment_primitive(p),
        ComponentValType::Type(_) => todo!(),
    }
}

pub fn alignment_primitive(ty: &PrimitiveValType) -> u32 {
    match ty {
        PrimitiveValType::Bool => 1,
        PrimitiveValType::S8 | PrimitiveValType::U8 => 1,
        PrimitiveValType::S16 | PrimitiveValType::U16 => 2,
        PrimitiveValType::S32 | PrimitiveValType::U32 => 4,
        PrimitiveValType::S64 | PrimitiveValType::U64 => 8,
        PrimitiveValType::F32 => 4,
        PrimitiveValType::F64 => 8,
        PrimitiveValType::Char => 4,
        PrimitiveValType::String => 4,
        PrimitiveValType::ErrorContext => 4,
    }
}

pub fn alignment_record(fields: &[ComponentValType]) -> u32 {
    let mut a = 1;
    for f in fields {
        a = a.max(alignment(f));
    }
    a
}

// https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md#element-size
pub fn elem_size(ty: &ComponentValType) -> u32 {
    match ty {
        ComponentValType::Primitive(p) => elem_size_primitive(p),
        ComponentValType::Type(_) => todo!(),
    }
}

pub fn elem_size_primitive(ty: &PrimitiveValType) -> u32 {
    match ty {
        PrimitiveValType::Bool => 1,
        PrimitiveValType::S8 | PrimitiveValType::U8 => 1,
        PrimitiveValType::S16 | PrimitiveValType::U16 => 2,
        PrimitiveValType::S32 | PrimitiveValType::U32 => 4,
        PrimitiveValType::S64 | PrimitiveValType::U64 => 8,
        PrimitiveValType::F32 => 4,
        PrimitiveValType::F64 => 8,
        PrimitiveValType::Char => 4,
        PrimitiveValType::String => 8,
        PrimitiveValType::ErrorContext => 4,
    }
}

pub fn elem_size_record(fields: &[ComponentValType]) -> u32 {
    let mut s = 0u32;
    for f in fields {
        s = s.next_multiple_of(alignment(f));
        s += elem_size(f);
    }
    assert!(s > 0);
    s.next_multiple_of(alignment_record(fields))
}

pub fn layout_record(fields: &[ComponentValType]) -> (u32, u32) {
    let mut s = 0u32;
    for f in fields {
        s = s.next_multiple_of(alignment(f));
        s += elem_size(f);
    }
    assert!(s > 0);
    let a = alignment_record(fields);
    s = s.next_multiple_of(a);
    (s, a)
}

// https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md#flattening
pub const MAX_FLAT_PARAMS: usize = 16;
pub const MAX_FLAT_ASYNC_PARAMS: usize = 4;
pub const MAX_FLAT_RESULTS: usize = 1;
