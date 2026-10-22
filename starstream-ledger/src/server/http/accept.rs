use core::cmp::Reverse;
use core::iter::zip;

use std::borrow::Cow;

use http::header::ACCEPT;
use mediatype::{MediaType, ReadParams as _, names};

use crate::server::http::error::AcceptHeaderError;

struct MediaRange<'a> {
    ty: &'a [u8],
    subty: &'a [u8],
    params: Vec<(&'a [u8], Cow<'a, [u8]>)>,
    q: u16,
}

fn parse_quality(s: &[u8]) -> Option<u16> {
    let (int, frac) = match s {
        [int] => (int, &[][..]),
        [int, b'.', frac @ ..] if frac.len() <= 3 => (int, frac),
        _ => return None,
    };
    let mut q = match int {
        b'0' => 0,
        b'1' => 1000,
        _ => return None,
    };
    for (digit, scale) in zip(frac, [100, 10, 1]) {
        if !digit.is_ascii_digit() {
            return None;
        }
        q += u16::from(digit - b'0') * scale;
    }
    (q <= 1000).then_some(q)
}

fn is_tchar(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b"!#$%&'*+-.^_`|~".contains(&b)
}

fn trim_ows(s: &[u8]) -> &[u8] {
    let start = s
        .iter()
        .position(|b| !matches!(b, b' ' | b'\t'))
        .unwrap_or(s.len());
    &s[start..]
}

fn token(s: &[u8]) -> Result<(&[u8], &[u8]), AcceptHeaderError> {
    let end = s.iter().position(|&b| !is_tchar(b)).unwrap_or(s.len());
    if end == 0 {
        return Err(AcceptHeaderError::Malformed);
    }
    Ok(s.split_at(end))
}

fn quoted_string(s: &[u8]) -> Result<(Cow<'_, [u8]>, &[u8]), AcceptHeaderError> {
    let end = s
        .iter()
        .position(|&b| matches!(b, b'"' | b'\\'))
        .ok_or(AcceptHeaderError::Malformed)?;
    let (value, s) = s.split_at(end);
    let mut value = Cow::Borrowed(value);
    let mut bytes = s.iter().enumerate();
    while let Some((i, &b)) = bytes.next() {
        match b {
            b'"' => return Ok((value, &s[i + 1..])),
            b'\\' => {
                let Some((.., &b)) = bytes.next() else {
                    break;
                };
                value.to_mut().push(b);
            }
            _ => value.to_mut().push(b),
        }
    }
    Err(AcceptHeaderError::Malformed)
}

fn media_range(s: &[u8]) -> Result<(MediaRange<'_>, &[u8]), AcceptHeaderError> {
    let (ty, s) = token(s)?;
    let s = s.strip_prefix(b"/").ok_or(AcceptHeaderError::Malformed)?;
    let (subty, mut s) = token(s)?;
    let mut params = Vec::new();
    let mut q = None;
    while let Some(rest) = trim_ows(s).strip_prefix(b";") {
        s = trim_ows(rest);
        if !s.first().is_some_and(|&b| is_tchar(b)) {
            continue;
        }
        let (name, rest) = token(s)?;
        let rest = rest
            .strip_prefix(b"=")
            .ok_or(AcceptHeaderError::Malformed)?;
        if name.eq_ignore_ascii_case(b"q") {
            let (value, rest) = token(rest)?;
            let value = parse_quality(value).ok_or_else(|| {
                AcceptHeaderError::Quality(String::from_utf8_lossy(value).into_owned())
            })?;
            if q.replace(value).is_some() {
                return Err(AcceptHeaderError::QualityDuplicate);
            }
            s = rest;
        } else if let Some(rest) = rest.strip_prefix(b"\"") {
            let (value, rest) = quoted_string(rest)?;
            params.push((name, value));
            s = rest;
        } else {
            let (value, rest) = token(rest)?;
            params.push((name, Cow::Borrowed(value)));
            s = rest;
        }
    }
    let range = MediaRange {
        ty,
        subty,
        params,
        q: q.unwrap_or(1000),
    };
    Ok((range, s))
}

fn specificity(range: &MediaRange<'_>) -> (bool, bool, usize) {
    (range.ty != b"*", range.subty != b"*", range.params.len())
}

fn subtype_matches(range: &[u8], ty: &MediaType<'_>) -> bool {
    let subty = ty.subty.as_str().as_bytes();
    let Some(suffix) = ty.suffix else {
        return range.eq_ignore_ascii_case(subty);
    };
    range
        .split_at_checked(subty.len())
        .is_some_and(|(head, tail)| {
            head.eq_ignore_ascii_case(subty)
                && tail
                    .strip_prefix(b"+")
                    .is_some_and(|tail| tail.eq_ignore_ascii_case(suffix.as_str().as_bytes()))
        })
}

fn matches(range: &MediaRange<'_>, ty: &MediaType<'_>) -> bool {
    let essence = match (range.ty, range.subty) {
        (b"*", b"*") => true,
        (range_ty, b"*") => range_ty.eq_ignore_ascii_case(ty.ty.as_str().as_bytes()),
        (range_ty, range_subty) => {
            range_ty.eq_ignore_ascii_case(ty.ty.as_str().as_bytes())
                && subtype_matches(range_subty, ty)
        }
    };
    essence
        && range.params.iter().all(|(name, value)| {
            ty.params().any(|(n, v)| {
                let v = v.unquoted_str();
                n.as_str().as_bytes().eq_ignore_ascii_case(name)
                    && if n == names::CHARSET {
                        v.as_bytes().eq_ignore_ascii_case(value)
                    } else {
                        v.as_bytes() == value.as_ref()
                    }
            })
        })
}

/// Selects the media type in `available` most preferred by the `Accept`
/// header in `headers`, following RFC 9110 section 12.5.1.
///
/// The most specific matching media range, the first in the header among
/// equally specific ones, determines the quality of each available type.
/// Ties are broken by specificity, then by the order of the media ranges in
/// the header, then by the order of `available`. Without an `Accept` header,
/// or with one listing no media ranges, the first available type is selected.
pub fn negotiate(
    headers: &http::HeaderMap,
    available: &'static [MediaType<'static>],
) -> Result<&'static MediaType<'static>, AcceptHeaderError> {
    let mut ranges = Vec::new();
    for value in headers.get_all(ACCEPT) {
        let mut s = trim_ows(value.as_bytes());
        while !s.is_empty() {
            if let Some(rest) = s.strip_prefix(b",") {
                s = trim_ows(rest);
                continue;
            }
            let (range, rest) = media_range(s)?;
            ranges.push(range);
            s = trim_ows(rest);
            if !s.is_empty() && !s.starts_with(b",") {
                return Err(AcceptHeaderError::Malformed);
            }
        }
    }
    if ranges.is_empty() {
        return available
            .first()
            .ok_or(AcceptHeaderError::NotAcceptable(available));
    }
    available
        .iter()
        .enumerate()
        .filter_map(|(priority, ty)| {
            let (specificity, order, q) = ranges
                .iter()
                .enumerate()
                .filter(|(.., range)| matches(range, ty))
                .map(|(order, range)| (specificity(range), Reverse(order), range.q))
                .max()?;
            (q > 0).then_some(((q, specificity, order, Reverse(priority)), ty))
        })
        .max_by_key(|(key, ..)| *key)
        .map(|(.., ty)| ty)
        .ok_or(AcceptHeaderError::NotAcceptable(available))
}

#[cfg(test)]
mod tests {
    use http::header::ACCEPT;
    use http::{HeaderMap, HeaderValue, StatusCode};
    use mediatype::MediaType;

    use super::negotiate;
    use crate::APPLICATION_WASM;
    use crate::server::http::TEXT_PLAIN_UTF_8;

    const AVAILABLE: &[MediaType] = &[TEXT_PLAIN_UTF_8, APPLICATION_WASM];

    #[test]
    fn missing() {
        let negotiated = negotiate(&HeaderMap::new(), AVAILABLE).unwrap();
        assert_eq!(negotiated, &TEXT_PLAIN_UTF_8);
    }

    #[test]
    fn header() {
        for (accept, expected) in [
            ("text/plain;q=0.9", Ok(&TEXT_PLAIN_UTF_8)),
            ("text/plain; charset=utf-8", Ok(&TEXT_PLAIN_UTF_8)),
            (r#"text/plain; charset="utf-8""#, Ok(&TEXT_PLAIN_UTF_8)),
            ("TEXT/Plain", Ok(&TEXT_PLAIN_UTF_8)),
            ("*/*", Ok(&TEXT_PLAIN_UTF_8)),
            ("application/*", Ok(&APPLICATION_WASM)),
            ("text/plain;q=0.5, application/wasm", Ok(&APPLICATION_WASM)),
            ("application/wasm;q=0.5, text/*", Ok(&TEXT_PLAIN_UTF_8)),
            ("application/wasm, text/plain", Ok(&APPLICATION_WASM)),
            ("*/*, application/wasm", Ok(&APPLICATION_WASM)),
            ("*/*, text/plain;q=0", Ok(&APPLICATION_WASM)),
            ("*/*, text/plain;Q=0", Ok(&APPLICATION_WASM)),
            (
                "text/plain;q=0.5;charset=utf-8, application/wasm;q=0.4",
                Ok(&TEXT_PLAIN_UTF_8),
            ),
            (
                "text/plain;;q=0.5;, application/wasm",
                Ok(&APPLICATION_WASM),
            ),
            (" , ,application/wasm ,, ", Ok(&APPLICATION_WASM)),
            ("", Ok(&TEXT_PLAIN_UTF_8)),
            (" , ", Ok(&TEXT_PLAIN_UTF_8)),
            (
                r#"text/plain; foo="a,b", application/wasm"#,
                Ok(&APPLICATION_WASM),
            ),
            (
                r#"text/plain; foo="a\",b", application/wasm"#,
                Ok(&APPLICATION_WASM),
            ),
            (
                "text/plain; foo=\"\u{e9}\", application/wasm",
                Ok(&APPLICATION_WASM),
            ),
            (
                "text/plain; charset=US-ASCII",
                Err(StatusCode::NOT_ACCEPTABLE),
            ),
            ("text/plain;q=0.5;foo=bar", Err(StatusCode::NOT_ACCEPTABLE)),
            ("application/json", Err(StatusCode::NOT_ACCEPTABLE)),
            ("application/x~wasm", Err(StatusCode::NOT_ACCEPTABLE)),
            ("*/wasm", Err(StatusCode::NOT_ACCEPTABLE)),
            ("*/*+json", Err(StatusCode::NOT_ACCEPTABLE)),
            ("application/*+json", Err(StatusCode::NOT_ACCEPTABLE)),
            (
                "application/wasm;q=0, application/wasm",
                Err(StatusCode::NOT_ACCEPTABLE),
            ),
            (
                "application/wasm, application/wasm;q=0",
                Ok(&APPLICATION_WASM),
            ),
            ("text/plain;q=1.5", Err(StatusCode::BAD_REQUEST)),
            ("text/plain;q=0.1234", Err(StatusCode::BAD_REQUEST)),
            (r#"text/plain;q="0.5""#, Err(StatusCode::BAD_REQUEST)),
            ("text/plain; q = 0.5", Err(StatusCode::BAD_REQUEST)),
            ("application/wasm;q=0.5;q=1", Err(StatusCode::BAD_REQUEST)),
            ("text/plain;charset=", Err(StatusCode::BAD_REQUEST)),
            (r#"text/plain;foo="bar"#, Err(StatusCode::BAD_REQUEST)),
            ("text/plain application/wasm", Err(StatusCode::BAD_REQUEST)),
            ("text/", Err(StatusCode::BAD_REQUEST)),
            ("text", Err(StatusCode::BAD_REQUEST)),
        ] {
            let headers = HeaderMap::from_iter([(ACCEPT, HeaderValue::from_str(accept).unwrap())]);
            let negotiated = negotiate(&headers, AVAILABLE).map_err(|err| err.http_status_code());
            assert_eq!(negotiated, expected, "`{accept}`");
        }
    }
}
