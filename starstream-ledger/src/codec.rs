//! The wRPC value codec over [`wasmtime::component::Val`], used for the
//! parameters and results of `/rpc` invocations.

use core::iter::zip;
use core::ops::{BitOrAssign, Shl};

use std::collections::HashSet;

use bytes::{BufMut as _, BytesMut};
use tokio::io::{AsyncRead, AsyncReadExt as _};
use tokio_util::codec::Encoder;
use wasm_tokio::cm::AsyncReadValue as _;
use wasm_tokio::{
    AsyncReadCore as _, AsyncReadLeb128 as _, AsyncReadUtf8 as _, CoreNameEncoder, Leb128Encoder,
    Utf8Codec,
};
use wasmtime::bail;
use wasmtime::component::types::{Case, Field};
use wasmtime::component::{Type, Val};
use wasmtime::error::Context as _;

/// [`Encoder`] writing a [`Val`] of a given [`Type`] in the wRPC value
/// encoding.
pub struct ValEncoder<'a> {
    pub ty: &'a Type,
}

impl<'a> ValEncoder<'a> {
    #[must_use]
    pub fn new(ty: &'a Type) -> Self {
        Self { ty }
    }
}

fn find_enum_discriminant<'a, T>(
    iter: impl IntoIterator<Item = T>,
    names: impl IntoIterator<Item = &'a str>,
    discriminant: &str,
) -> wasmtime::Result<T> {
    zip(iter, names)
        .find_map(|(i, name)| (name == discriminant).then_some(i))
        .context("unknown enum discriminant")
}

fn find_variant_discriminant<'a, T>(
    iter: impl IntoIterator<Item = T>,
    cases: impl IntoIterator<Item = Case<'a>>,
    discriminant: &str,
) -> wasmtime::Result<(T, Option<Type>)> {
    zip(iter, cases)
        .find_map(|(i, Case { name, ty })| (name == discriminant).then_some((i, ty)))
        .context("unknown variant discriminant")
}

#[inline]
fn flag_bits<'a, T: BitOrAssign + Shl<u8, Output = T> + From<u8>>(
    names: impl IntoIterator<Item = &'a str>,
    flags: impl IntoIterator<Item = &'a str>,
) -> T {
    let mut v = T::from(0);
    let flags: HashSet<&str> = flags.into_iter().collect();
    for (i, name) in zip(0u8.., names) {
        if flags.contains(name) {
            v |= T::from(1) << i;
        }
    }
    v
}

impl Encoder<&Val> for ValEncoder<'_> {
    type Error = wasmtime::Error;

    fn encode(&mut self, v: &Val, dst: &mut BytesMut) -> Result<(), Self::Error> {
        match (v, self.ty) {
            (Val::Bool(v), Type::Bool) => {
                dst.reserve(1);
                dst.put_u8((*v).into());
                Ok(())
            }
            (Val::S8(v), Type::S8) => {
                dst.reserve(1);
                dst.put_i8(*v);
                Ok(())
            }
            (Val::U8(v), Type::U8) => {
                dst.reserve(1);
                dst.put_u8(*v);
                Ok(())
            }
            (Val::S16(v), Type::S16) => Leb128Encoder
                .encode(*v, dst)
                .context("failed to encode s16"),
            (Val::U16(v), Type::U16) => Leb128Encoder
                .encode(*v, dst)
                .context("failed to encode u16"),
            (Val::S32(v), Type::S32) => Leb128Encoder
                .encode(*v, dst)
                .context("failed to encode s32"),
            (Val::U32(v), Type::U32) => Leb128Encoder
                .encode(*v, dst)
                .context("failed to encode u32"),
            (Val::S64(v), Type::S64) => Leb128Encoder
                .encode(*v, dst)
                .context("failed to encode s64"),
            (Val::U64(v), Type::U64) => Leb128Encoder
                .encode(*v, dst)
                .context("failed to encode u64"),
            (Val::Float32(v), Type::Float32) => {
                dst.reserve(4);
                dst.put_f32_le(*v);
                Ok(())
            }
            (Val::Float64(v), Type::Float64) => {
                dst.reserve(8);
                dst.put_f64_le(*v);
                Ok(())
            }
            (Val::Char(v), Type::Char) => {
                Utf8Codec.encode(*v, dst).context("failed to encode char")
            }
            (Val::String(v), Type::String) => CoreNameEncoder
                .encode(v.as_str(), dst)
                .context("failed to encode string"),
            (Val::List(vs), Type::List(ty)) => {
                let ty = ty.ty();
                let n = u32::try_from(vs.len()).context("list length does not fit in u32")?;
                dst.reserve(5 + vs.len());
                Leb128Encoder
                    .encode(n, dst)
                    .context("failed to encode list length")?;
                for v in vs {
                    ValEncoder::new(&ty)
                        .encode(v, dst)
                        .context("failed to encode list element")?;
                }
                Ok(())
            }
            (Val::Record(vs), Type::Record(ty)) => {
                dst.reserve(vs.len());
                for ((name, v), Field { ty, .. }) in zip(vs, ty.fields()) {
                    ValEncoder::new(&ty)
                        .encode(v, dst)
                        .with_context(|| format!("failed to encode `{name}` field"))?;
                }
                Ok(())
            }
            (Val::Tuple(vs), Type::Tuple(ty)) => {
                dst.reserve(vs.len());
                for (v, ty) in zip(vs, ty.types()) {
                    ValEncoder::new(&ty)
                        .encode(v, dst)
                        .context("failed to encode tuple element")?;
                }
                Ok(())
            }
            (Val::Variant(discriminant, v), Type::Variant(ty)) => {
                let cases = ty.cases();
                let ty = match u32::try_from(cases.len()) {
                    Ok(..=0x0000_00ff) => {
                        let (discriminant, ty) =
                            find_variant_discriminant(0u8.., cases, discriminant)?;
                        dst.reserve(2 + usize::from(v.is_some()));
                        Leb128Encoder.encode(discriminant, dst)?;
                        ty
                    }
                    Ok(0x0000_0100..=0x0000_ffff) => {
                        let (discriminant, ty) =
                            find_variant_discriminant(0u16.., cases, discriminant)?;
                        dst.reserve(3 + usize::from(v.is_some()));
                        Leb128Encoder.encode(discriminant, dst)?;
                        ty
                    }
                    Ok(0x0001_0000..=0x00ff_ffff) => {
                        let (discriminant, ty) =
                            find_variant_discriminant(0u32.., cases, discriminant)?;
                        dst.reserve(4 + usize::from(v.is_some()));
                        Leb128Encoder.encode(discriminant, dst)?;
                        ty
                    }
                    Ok(0x0100_0000..=0xffff_ffff) => {
                        let (discriminant, ty) =
                            find_variant_discriminant(0u32.., cases, discriminant)?;
                        dst.reserve(5 + usize::from(v.is_some()));
                        Leb128Encoder.encode(discriminant, dst)?;
                        ty
                    }
                    Err(_) => bail!("case count does not fit in u32"),
                };
                if let Some(v) = v {
                    let ty = ty.context("type missing for variant")?;
                    ValEncoder::new(&ty)
                        .encode(v, dst)
                        .context("failed to encode variant value")?;
                }
                Ok(())
            }
            (Val::Enum(discriminant), Type::Enum(ty)) => {
                let names = ty.names();
                match u32::try_from(names.len()) {
                    Ok(..=0x0000_00ff) => {
                        let discriminant = find_enum_discriminant(0u8.., names, discriminant)?;
                        dst.reserve(2);
                        Leb128Encoder.encode(discriminant, dst)?;
                    }
                    Ok(0x0000_0100..=0x0000_ffff) => {
                        let discriminant = find_enum_discriminant(0u16.., names, discriminant)?;
                        dst.reserve(3);
                        Leb128Encoder.encode(discriminant, dst)?;
                    }
                    Ok(0x0001_0000..=0x00ff_ffff) => {
                        let discriminant = find_enum_discriminant(0u32.., names, discriminant)?;
                        dst.reserve(4);
                        Leb128Encoder.encode(discriminant, dst)?;
                    }
                    Ok(0x0100_0000..=0xffff_ffff) => {
                        let discriminant = find_enum_discriminant(0u32.., names, discriminant)?;
                        dst.reserve(5);
                        Leb128Encoder.encode(discriminant, dst)?;
                    }
                    Err(_) => bail!("name count does not fit in u32"),
                }
                Ok(())
            }
            (Val::Option(None), Type::Option(_)) => {
                dst.reserve(1);
                dst.put_u8(0);
                Ok(())
            }
            (Val::Option(Some(v)), Type::Option(ty)) => {
                dst.reserve(2);
                dst.put_u8(1);
                let ty = ty.ty();
                ValEncoder::new(&ty)
                    .encode(v, dst)
                    .context("failed to encode `option::some` value")
            }
            (Val::Result(v), Type::Result(ty)) => match v {
                Ok(v) => match (v, ty.ok()) {
                    (Some(v), Some(ty)) => {
                        dst.reserve(2);
                        dst.put_u8(0);
                        ValEncoder::new(&ty)
                            .encode(v, dst)
                            .context("failed to encode `result::ok` value")
                    }
                    (Some(_), None) => bail!("`result::ok` value of unknown type"),
                    (None, Some(_)) => bail!("`result::ok` value missing"),
                    (None, None) => {
                        dst.reserve(1);
                        dst.put_u8(0);
                        Ok(())
                    }
                },
                Err(v) => match (v, ty.err()) {
                    (Some(v), Some(ty)) => {
                        dst.reserve(2);
                        dst.put_u8(1);
                        ValEncoder::new(&ty)
                            .encode(v, dst)
                            .context("failed to encode `result::err` value")
                    }
                    (Some(_), None) => bail!("`result::err` value of unknown type"),
                    (None, Some(_)) => bail!("`result::err` value missing"),
                    (None, None) => {
                        dst.reserve(1);
                        dst.put_u8(1);
                        Ok(())
                    }
                },
            },
            (Val::Flags(vs), Type::Flags(ty)) => {
                let names = ty.names();
                let vs = vs.iter().map(String::as_str);
                match names.len() {
                    ..=8 => {
                        dst.reserve(1);
                        dst.put_u8(flag_bits(names, vs));
                    }
                    9..=16 => {
                        dst.reserve(2);
                        dst.put_u16_le(flag_bits(names, vs));
                    }
                    17..=24 => {
                        dst.reserve(3);
                        dst.put_slice(&u32::to_le_bytes(flag_bits(names, vs))[..3]);
                    }
                    25..=32 => {
                        dst.reserve(4);
                        dst.put_u32_le(flag_bits(names, vs));
                    }
                    33..=40 => {
                        dst.reserve(5);
                        dst.put_slice(&u64::to_le_bytes(flag_bits(names, vs))[..5]);
                    }
                    41..=48 => {
                        dst.reserve(6);
                        dst.put_slice(&u64::to_le_bytes(flag_bits(names, vs))[..6]);
                    }
                    49..=56 => {
                        dst.reserve(7);
                        dst.put_slice(&u64::to_le_bytes(flag_bits(names, vs))[..7]);
                    }
                    57..=64 => {
                        dst.reserve(8);
                        dst.put_u64_le(flag_bits(names, vs));
                    }
                    65..=72 => {
                        dst.reserve(9);
                        dst.put_slice(&u128::to_le_bytes(flag_bits(names, vs))[..9]);
                    }
                    73..=80 => {
                        dst.reserve(10);
                        dst.put_slice(&u128::to_le_bytes(flag_bits(names, vs))[..10]);
                    }
                    81..=88 => {
                        dst.reserve(11);
                        dst.put_slice(&u128::to_le_bytes(flag_bits(names, vs))[..11]);
                    }
                    89..=96 => {
                        dst.reserve(12);
                        dst.put_slice(&u128::to_le_bytes(flag_bits(names, vs))[..12]);
                    }
                    97..=104 => {
                        dst.reserve(13);
                        dst.put_slice(&u128::to_le_bytes(flag_bits(names, vs))[..13]);
                    }
                    105..=112 => {
                        dst.reserve(14);
                        dst.put_slice(&u128::to_le_bytes(flag_bits(names, vs))[..14]);
                    }
                    113..=120 => {
                        dst.reserve(15);
                        dst.put_slice(&u128::to_le_bytes(flag_bits(names, vs))[..15]);
                    }
                    121..=128 => {
                        dst.reserve(16);
                        dst.put_u128_le(flag_bits(names, vs));
                    }
                    bits @ 129.. => {
                        let mut cap = bits / 8;
                        if bits % 8 != 0 {
                            cap = cap.saturating_add(1);
                        }
                        let mut buf = vec![0; cap];
                        let flags: HashSet<&str> = vs.into_iter().collect();
                        for (i, name) in names.enumerate() {
                            if flags.contains(name) {
                                buf[i / 8] |= 1 << (i % 8);
                            }
                        }
                        dst.extend_from_slice(&buf);
                    }
                }
                Ok(())
            }
            (_, Type::Map(..)) => bail!("encoding maps not supported by Starstream"),
            (_, Type::Own(..) | Type::Borrow(..)) => {
                bail!("encoding resources not supported by Starstream")
            }
            (_, Type::Future(..)) => bail!("encoding futures not supported by Starstream"),
            (_, Type::Stream(..)) => bail!("encoding streams not supported by Starstream"),
            (_, Type::ErrorContext) => bail!("encoding error contexts not supported by Starstream"),
            (_, Type::FixedLengthList(..)) => {
                bail!("encoding fixed length lists not supported by Starstream")
            }
            _ => bail!("value type mismatch"),
        }
    }
}

fn unsupported(msg: &'static str) -> std::io::Error {
    std::io::Error::new(std::io::ErrorKind::Unsupported, msg)
}

/// Read a wRPC-encoded [`Val`] of type `ty` from `r` into `val`.
#[allow(clippy::too_many_lines)]
pub async fn read_value<R: AsyncRead + Unpin>(
    r: &mut R,
    val: &mut Val,
    ty: &Type,
) -> std::io::Result<()> {
    match ty {
        Type::Bool => {
            *val = Val::Bool(r.read_bool().await?);
            Ok(())
        }
        Type::S8 => {
            *val = Val::S8(r.read_i8().await?);
            Ok(())
        }
        Type::U8 => {
            *val = Val::U8(r.read_u8().await?);
            Ok(())
        }
        Type::S16 => {
            *val = Val::S16(r.read_i16_leb128().await?);
            Ok(())
        }
        Type::U16 => {
            *val = Val::U16(r.read_u16_leb128().await?);
            Ok(())
        }
        Type::S32 => {
            *val = Val::S32(r.read_i32_leb128().await?);
            Ok(())
        }
        Type::U32 => {
            *val = Val::U32(r.read_u32_leb128().await?);
            Ok(())
        }
        Type::S64 => {
            *val = Val::S64(r.read_i64_leb128().await?);
            Ok(())
        }
        Type::U64 => {
            *val = Val::U64(r.read_u64_leb128().await?);
            Ok(())
        }
        Type::Float32 => {
            *val = Val::Float32(r.read_f32_le().await?);
            Ok(())
        }
        Type::Float64 => {
            *val = Val::Float64(r.read_f64_le().await?);
            Ok(())
        }
        Type::Char => {
            *val = Val::Char(r.read_char_utf8().await?);
            Ok(())
        }
        Type::String => {
            let mut s = String::default();
            r.read_core_name(&mut s).await?;
            *val = Val::String(s);
            Ok(())
        }
        Type::List(ty) => {
            let n = r.read_u32_leb128().await?;
            let n = n.try_into().unwrap_or(usize::MAX);
            let ty = ty.ty();
            let mut vs = Vec::with_capacity(n);
            for _ in 0..n {
                let mut v = Val::Bool(false);
                Box::pin(read_value(&mut *r, &mut v, &ty)).await?;
                vs.push(v);
            }
            *val = Val::List(vs);
            Ok(())
        }
        Type::Record(ty) => {
            let fields = ty.fields();
            let mut vs = Vec::with_capacity(fields.len());
            for Field { name, ty } in fields {
                let mut v = Val::Bool(false);
                Box::pin(read_value(&mut *r, &mut v, &ty)).await?;
                vs.push((name.to_string(), v));
            }
            *val = Val::Record(vs);
            Ok(())
        }
        Type::Tuple(ty) => {
            let types = ty.types();
            let mut vs = Vec::with_capacity(types.len());
            for ty in types {
                let mut v = Val::Bool(false);
                Box::pin(read_value(&mut *r, &mut v, &ty)).await?;
                vs.push(v);
            }
            *val = Val::Tuple(vs);
            Ok(())
        }
        Type::Variant(ty) => {
            let discriminant = r.read_u32_leb128().await?;
            let discriminant = discriminant
                .try_into()
                .map_err(|err| std::io::Error::new(std::io::ErrorKind::InvalidInput, err))?;
            let Case { name, ty } = ty.cases().nth(discriminant).ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!("unknown variant discriminant `{discriminant}`"),
                )
            })?;
            let name = name.to_string();
            if let Some(ty) = ty {
                let mut v = Val::Bool(false);
                Box::pin(read_value(&mut *r, &mut v, &ty)).await?;
                *val = Val::Variant(name, Some(Box::new(v)));
            } else {
                *val = Val::Variant(name, None);
            }
            Ok(())
        }
        Type::Enum(ty) => {
            let discriminant = r.read_u32_leb128().await?;
            let discriminant = discriminant
                .try_into()
                .map_err(|err| std::io::Error::new(std::io::ErrorKind::InvalidInput, err))?;
            let name = ty.names().nth(discriminant).ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!("unknown enum discriminant `{discriminant}`"),
                )
            })?;
            *val = Val::Enum(name.to_string());
            Ok(())
        }
        Type::Option(ty) => {
            if r.read_option_status().await? {
                let mut v = Val::Bool(false);
                Box::pin(read_value(&mut *r, &mut v, &ty.ty())).await?;
                *val = Val::Option(Some(Box::new(v)));
            } else {
                *val = Val::Option(None);
            }
            Ok(())
        }
        Type::Result(ty) => {
            if r.read_result_status().await? {
                if let Some(ty) = ty.ok() {
                    let mut v = Val::Bool(false);
                    Box::pin(read_value(&mut *r, &mut v, &ty)).await?;
                    *val = Val::Result(Ok(Some(Box::new(v))));
                } else {
                    *val = Val::Result(Ok(None));
                }
            } else if let Some(ty) = ty.err() {
                let mut v = Val::Bool(false);
                Box::pin(read_value(&mut *r, &mut v, &ty)).await?;
                *val = Val::Result(Err(Some(Box::new(v))));
            } else {
                *val = Val::Result(Err(None));
            }
            Ok(())
        }
        Type::Flags(ty) => {
            let names = ty.names();
            let mut buf = vec![0u8; names.len().max(1).div_ceil(8)];
            r.read_exact(&mut buf).await?;
            let mut vs = Vec::new();
            for (i, name) in names.enumerate() {
                if buf[i / 8] & (1 << (i % 8)) != 0 {
                    vs.push(name.to_string());
                }
            }
            *val = Val::Flags(vs);
            Ok(())
        }
        Type::Map(..) => Err(unsupported("decoding maps not supported by Starstream")),
        Type::Own(..) | Type::Borrow(..) => Err(unsupported(
            "decoding resources not supported by Starstream",
        )),
        Type::Future(..) => Err(unsupported("decoding futures not supported by Starstream")),
        Type::Stream(..) => Err(unsupported("decoding streams not supported by Starstream")),
        Type::ErrorContext => Err(unsupported(
            "decoding error contexts not supported by Starstream",
        )),
        Type::FixedLengthList(..) => Err(unsupported(
            "decoding fixed length lists not supported by Starstream",
        )),
    }
}
