use std::{fs, path::PathBuf, str::FromStr};

use clap::Args;
use console::{Color, style};
use miette::{IntoDiagnostic, NamedSource};
use similar::{ChangeTag, TextDiff};
use starstream_compiler::formatter;

use crate::{diagnostics::print_diagnostic, starstream_files_excluding_gitignore, style};

/// Format Starstream source
#[derive(Args, Debug)]
pub struct Format {
    /// Files to format
    #[clap(default_value = ".")]
    files: Vec<String>,

    /// Check if inputs are formatted without changing them
    #[clap(short, long)]
    check: bool,
}

impl Format {
    pub fn exec(self) -> miette::Result<()> {
        if self.check {
            check_files(self.files)
        } else {
            format_files(self.files)
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Unformatted {
    pub source: PathBuf,
    pub destination: PathBuf,
    pub input: String,
    pub output: String,
}

fn check_files(files: Vec<String>) -> miette::Result<()> {
    let problem_files = unformatted_files(files)?;

    if problem_files.is_empty() {
        Ok(())
    } else {
        let total = problem_files.len();

        for file in problem_files {
            let diff = diff_file(&file.input, &file.output);

            eprintln!(
                "{}:",
                style::INFO.underlined().apply_to(file.source.display())
            );

            eprintln!("{diff}");
        }

        miette::bail!(
            "failed to format {} file{}",
            style::INFO.bold().apply_to(total),
            style::r_if_then(total > 1, "s")
        )
    }
}

fn format_files(files: Vec<String>) -> miette::Result<()> {
    for file in unformatted_files(files)? {
        fs::write(file.destination, file.output).into_diagnostic()?;
    }

    Ok(())
}

fn unformatted_files(files: Vec<String>) -> miette::Result<Vec<Unformatted>> {
    let mut problem_files = Vec::with_capacity(files.len());

    for file_path in files {
        let path = PathBuf::from_str(&file_path).into_diagnostic()?;

        if path.is_dir() {
            for path in starstream_files_excluding_gitignore(&path) {
                format_file(&mut problem_files, path)?
            }
        } else {
            format_file(&mut problem_files, path)?
        }
    }

    Ok(problem_files)
}

/// Format a single file, collecting an entry if the formatted output differs.
fn format_file(problem_files: &mut Vec<Unformatted>, path: PathBuf) -> miette::Result<()> {
    let input = fs::read_to_string(&path).into_diagnostic()?;
    let parse_output = starstream_compiler::parse_program(&input);

    if !parse_output.errors().is_empty() {
        let named = NamedSource::new(path.display().to_string(), input.clone());

        for error in parse_output.errors {
            print_diagnostic(named.clone(), error)?;
        }
    }

    let Some(program) = parse_output.program else {
        std::process::exit(1)
    };

    let output = formatter::program(&program).into_diagnostic()?;

    if input != output {
        problem_files.push(Unformatted {
            source: path.clone(),
            destination: path,
            input,
            output,
        });
    }

    Ok(())
}

fn diff_file(original: &str, formatted: &str) -> String {
    let diff = TextDiff::from_lines(original, formatted);
    let mut result = String::new();

    for (i, group) in diff.grouped_ops(3).iter().enumerate() {
        if i > 0 {
            result.push_str(&format!("{}\n", style("---").dim()));
        }

        for op in group {
            for change in diff.iter_changes(op) {
                let (sign, color, line_no) = match change.tag() {
                    ChangeTag::Delete => ("-", Color::Red, change.old_index()),
                    ChangeTag::Insert => ("+", Color::Green, change.new_index()),
                    ChangeTag::Equal => (" ", Color::White, change.old_index()),
                };

                if let Some(line_no) = line_no {
                    result.push_str(&format!(
                        "{}",
                        style(format!(
                            "{}{:4} | {}{}",
                            sign,
                            line_no + 1,
                            sign,
                            change.value()
                        ))
                        .fg(color)
                    ));
                }
            }
        }
    }

    result
}
