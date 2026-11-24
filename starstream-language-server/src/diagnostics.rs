//! Helpers to convert `miette` diagnostics into LSP diagnostics.
use std::{fmt::Display, path::Path};

use miette::{Diagnostic, LabeledSpan, Severity, SourceSpan};
use ropey::Rope;
use tower_lsp_server::lsp_types::{
    CodeDescription, DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString,
    Position, Range, Uri,
};

/// Transform a `miette::Diagnostic` into a single `lsp_types::Diagnostic`.
///
/// The returned diagnostic highlights the primary span (if any) and records all
/// secondary labels as `related_information` entries so editors can surface the
/// extra context. Related diagnostics that carry their own source, such as a
/// label pointing into an imported file, become entries in that file. Help text
/// is appended to the message when available.
pub fn diagnostic_to_lsp<D>(
    rope: &Rope,
    uri: &Uri,
    diagnostic: &D,
) -> tower_lsp_server::lsp_types::Diagnostic
where
    D: Diagnostic + Display,
{
    let mut message = diagnostic.to_string();

    if let Some(help) = diagnostic.help() {
        let help_text = help.to_string();
        if !help_text.is_empty() {
            message.push_str("\nhelp: ");
            message.push_str(&help_text);
        }
    }

    let labels: Vec<LabeledSpan> = diagnostic
        .labels()
        .map(Iterator::collect)
        .unwrap_or_default();

    let mut range = Range::default();
    let mut related_information = Vec::new();

    if let Some(primary) = labels.first() {
        range = source_span_to_range(rope, primary.inner());
        if let Some(label_text) = primary.label().filter(|label| !label.is_empty()) {
            message.push('\n');
            message.push_str(label_text);
        }
    }

    for label in labels.iter().skip(1) {
        let related_range = source_span_to_range(rope, label.inner());
        let related_message = label.label().map(ToString::to_string).unwrap_or_default();

        related_information.push(DiagnosticRelatedInformation {
            message: related_message,
            location: Location {
                uri: uri.clone(),
                range: related_range,
            },
        });
    }

    related_information.extend(
        diagnostic
            .related()
            .into_iter()
            .flatten()
            .filter_map(related_in_own_file),
    );

    tower_lsp_server::lsp_types::Diagnostic {
        range,
        severity: diagnostic
            .severity()
            .map(severity_to_lsp)
            .or(Some(DiagnosticSeverity::ERROR)),
        code: diagnostic
            .code()
            .map(|code| NumberOrString::String(format!("star-{code}"))),
        code_description: diagnostic.url().and_then(|url| {
            url.to_string()
                .parse()
                .ok()
                .map(|href| CodeDescription { href })
        }),
        source: None,
        message,
        related_information: if related_information.is_empty() {
            None
        } else {
            Some(related_information)
        },
        tags: None,
        data: None,
    }
}

/// Locate a related diagnostic that carries its own source, like a label in
/// another file, as related information in that file.
fn related_in_own_file(related: &dyn Diagnostic) -> Option<DiagnosticRelatedInformation> {
    let source = related.source_code()?;
    let label = related.labels()?.next()?;
    // Reading from the start with unlimited context lines after gives back
    // the whole source along with its name, which is the file's path.
    let contents = source
        .read_span(&SourceSpan::from(0..0), 0, usize::MAX)
        .ok()?;
    let uri = crate::document::file_path_to_uri(Path::new(contents.name()?))?;
    let rope = Rope::from_str(std::str::from_utf8(contents.data()).ok()?);

    Some(DiagnosticRelatedInformation {
        message: related.to_string(),
        location: Location {
            uri,
            range: source_span_to_range(&rope, label.inner()),
        },
    })
}

fn severity_to_lsp(severity: Severity) -> DiagnosticSeverity {
    match severity {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Advice => DiagnosticSeverity::HINT,
    }
}

fn source_span_to_range(rope: &Rope, span: &miette::SourceSpan) -> Range {
    let start = span.offset();
    let len = span.len();
    let end = start + len;

    Range {
        start: offset_to_position(rope, start),
        end: offset_to_position(rope, end),
    }
}

fn offset_to_position(rope: &Rope, mut offset: usize) -> Position {
    let total = rope.len_chars();
    if offset > total {
        offset = total;
    }

    let line = rope.char_to_line(offset);
    let line_start = rope.line_to_char(line);
    let column = offset.saturating_sub(line_start);

    Position {
        line: line as u32,
        character: column as u32,
    }
}
