use std::collections::HashSet;

use crate::symbols::{AbiInfo, FuncInfo, SymbolInformation, VarInfo};

use super::ComparableType;
use ariadne::{Color, Label, Report, ReportKind};
use chumsky::span::SimpleSpan;

#[repr(u32)]
pub enum TypeErrorCode {
    TypeMismatch = 300,
    NonSigned = 301,
    LinearVariableReUsed = 302,
    UnusedLinearVariable = 303,
    UtxoMainBlockInvalidType = 304,
    EffectTypeMismatch = 305,
    LinearVariableNotUsedInAllBranches = 306,
}

pub(super) fn error_field_not_found(span: SimpleSpan, expected: &str) -> Report<'static> {
    error_report(span)
        .with_code(TypeErrorCode::TypeMismatch as u32)
        .with_label(
            Label::new(span.into_range())
                .with_message(format!("method or field not found: {}", expected))
                .with_color(Color::Red),
        )
        .finish()
}

pub(super) fn error_type_mismatch(
    span: SimpleSpan,
    expected: &ComparableType,
    found: &ComparableType,
) -> Report<'static> {
    error_report(span)
        .with_code(TypeErrorCode::TypeMismatch as u32)
        .with_label(
            Label::new(span.into_range())
                .with_message(format!("expected {:?}, found {:?}", expected, found))
                .with_color(Color::Red),
        )
        .finish()
}

pub(super) fn error_non_signed(span: SimpleSpan, found: &ComparableType) -> Report<'static> {
    error_report(span)
        .with_code(TypeErrorCode::NonSigned as u32)
        .with_label(
            Label::new(span.into_range())
                .with_message(format!("expected signed numeric type, found: {:?}", found))
                .with_color(Color::Red),
        )
        .finish()
}

pub(super) fn error_variable_used_more_than_once(
    var: &SymbolInformation<VarInfo>,
    span1: SimpleSpan,
    span2: SimpleSpan,
) -> Report<'static> {
    error_report(var.span.unwrap())
        .with_code(TypeErrorCode::LinearVariableReUsed as u32)
        .with_label(
            Label::new(span1.into_range())
                .with_message(format!("{} first used here", var.source))
                .with_color(Color::Red),
        )
        .with_label(
            Label::new(span2.into_range())
                .with_message("then used here")
                .with_color(Color::Red),
        )
        .finish()
}

pub(super) fn error_unused_variable(
    var: &SymbolInformation<VarInfo>,
    is_error: bool,
) -> Report<'static> {
    if is_error {
        error_report(var.span.unwrap())
    } else {
        warning_report(var.span.unwrap())
    }
    .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
    .with_code(TypeErrorCode::UnusedLinearVariable as u32)
    .with_label(
        Label::new(var.span.unwrap().into_range())
            .with_message("unused variable")
            .with_color(Color::Red),
    )
    .finish()
}

pub(super) fn error_linear_variable_affine(
    var: &SymbolInformation<VarInfo>,
    span: SimpleSpan,
) -> Report<'static> {
    error_report(var.span.unwrap())
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_code(TypeErrorCode::LinearVariableNotUsedInAllBranches as u32)
        .with_label(
            Label::new(var.span.unwrap().into_range())
                .with_message("linear variable consumed partially")
                .with_color(Color::Red),
        )
        .with_label(
            Label::new(span.into_range())
                .with_message("the variable is used in this branch, so it must be consumed in all the branches")
                .with_color(Color::Red),
        )
        .finish()
}

pub(super) fn error_invalid_return_type_for_utxo_main(span: SimpleSpan) -> Report<'static> {
    error_report(span)
        .with_code(TypeErrorCode::UtxoMainBlockInvalidType as u32)
        .with_label(
            Label::new(span.into_range())
                .with_message("main block in utxo should not return values")
                .with_color(Color::Red),
        )
        .finish()
}

pub(super) fn error_effect_type_mismatch(
    span: SimpleSpan,
    expected: HashSet<String>,
    found: HashSet<String>,
) -> Report<'static> {
    error_report(span)
        .with_code(TypeErrorCode::EffectTypeMismatch as u32)
        .with_label(
            Label::new(span.into_range())
                .with_message(format!("expected {:?}, found {:?}", expected, found))
                .with_color(Color::Red),
        )
        .finish()
}

pub(super) fn error_missing_effect_handler(
    span: SimpleSpan,
    effect_info: &SymbolInformation<FuncInfo>,
    interface_info: &SymbolInformation<AbiInfo>,
) -> Report<'static> {
    error_report(span)
        .with_code(10)
        .with_label(
            Label::new(span.into_range())
                .with_message(format!(
                    "missing handler for {} from {} interface",
                    effect_info.source, interface_info.source
                ))
                .with_color(Color::Red),
        )
        .finish()
}

fn error_report(span: SimpleSpan) -> ariadne::ReportBuilder<'static, std::ops::Range<usize>> {
    Report::build(ReportKind::Error, span.into_range())
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
}

fn warning_report(span: SimpleSpan) -> ariadne::ReportBuilder<'static, std::ops::Range<usize>> {
    Report::build(ReportKind::Warning, span.into_range())
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
}
