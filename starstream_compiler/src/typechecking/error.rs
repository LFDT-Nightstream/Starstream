use super::ComparableType;
use crate::{
    error::TypeError,
    symbols::{AbiInfo, EffectInfo, SymbolInformation, VarInfo},
};
use chumsky::span::SimpleSpan;
use std::collections::HashSet;

pub(super) fn error_field_not_found(span: SimpleSpan, expected: &str) -> TypeError {
    TypeError::FieldNotFound {
        span,
        expected: expected.to_string(),
    }
}

pub(super) fn error_type_mismatch(
    span: SimpleSpan,
    expected: &ComparableType,
    found: &ComparableType,
) -> TypeError {
    TypeError::TypeMismatch {
        span,
        expected: expected.clone(),
        found: found.clone(),
    }
}

pub(super) fn error_non_signed(span: SimpleSpan, found: &ComparableType) -> TypeError {
    TypeError::NonSigned {
        span,
        found: found.clone(),
    }
}

pub(super) fn error_variable_used_more_than_once(
    var: &SymbolInformation<VarInfo>,
    span1: SimpleSpan,
    span2: SimpleSpan,
) -> TypeError {
    TypeError::LinearVariableUsedMoreThanOnce {
        var_span: var.span.unwrap(),
        span1,
        span2,
    }
}

pub(super) fn error_unused_variable(var: &SymbolInformation<VarInfo>, is_error: bool) -> TypeError {
    TypeError::UnusedVariable {
        var_span: var.span.unwrap(),
        is_error,
    }
}

pub(super) fn error_linear_variable_affine(
    var: &SymbolInformation<VarInfo>,
    span: SimpleSpan,
) -> TypeError {
    TypeError::LinearVariableAffine {
        var_span: var.span.unwrap(),
        branch_span: span,
    }
}

pub(super) fn error_invalid_return_type_for_utxo_main(span: SimpleSpan) -> TypeError {
    TypeError::UtxoMainInvalidType { span }
}

pub(super) fn error_effect_type_mismatch(
    span: SimpleSpan,
    expected: HashSet<String>,
    found: HashSet<String>,
) -> TypeError {
    TypeError::EffectTypeMismatch {
        span,
        expected,
        found,
    }
}

pub(super) fn error_missing_effect_handler(
    span: SimpleSpan,
    effect_info: &SymbolInformation<EffectInfo>,
    interface_info: &SymbolInformation<AbiInfo>,
) -> TypeError {
    TypeError::MissingEffectHandler {
        span,
        effect_name: effect_info.source.clone(),
        interface_name: interface_info.source.clone(),
    }
}
