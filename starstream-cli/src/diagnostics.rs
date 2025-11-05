//! Helpers for rendering diagnostics in a consistent, pretty way across CLI commands.
//!
//! The CLI presents `miette` diagnostics in multiple subcommands. Centralising the
//! printing logic keeps the formatting identical everywhere and avoids copying the
//! `GraphicalReportHandler` boilerplate.

use miette::{Diagnostic, GraphicalReportHandler, NamedSource, Report};

/// Render a diagnostic using the fancy miette renderer and print it to stderr.
///
/// This helper wraps the common pattern of turning an error into a `Report`,
/// configuring the graphical renderer, and handling any rendering failures so
/// callers only need to supply the source and diagnostic value.
pub fn print_diagnostic<E>(source: NamedSource<String>, error: E) -> miette::Result<()>
where
    E: Diagnostic + std::error::Error + Send + Sync + 'static,
{
    let report = Report::new(error).with_source_code(source);

    let mut rendered = String::new();

    GraphicalReportHandler::new()
        .render_report(&mut rendered, report.as_ref())
        .map_err(|err| miette::miette!("failed to render diagnostic: {err}"))?;

    eprintln!("{rendered}");

    Ok(())
}
