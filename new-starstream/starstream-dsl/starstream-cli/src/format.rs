use std::{
    fs,
    path::{Path, PathBuf},
    str::FromStr,
};

use ariadne::{Cache, FileCache};
use clap::Args;
use console::{Color, style};
use miette::IntoDiagnostic;
use similar::{ChangeTag, TextDiff};
use starstream_compiler::formatter;

use crate::style;

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
        let mut cache = FileCache::default();

        if self.check {
            check_files(self.files, &mut cache)
        } else {
            format_files(self.files, &mut cache)
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

fn check_files(files: Vec<String>, cache: &mut FileCache) -> miette::Result<()> {
    let problem_files = unformatted_files(files, cache)?;

    if problem_files.is_empty() {
        Ok(())
    } else {
        for file in problem_files {
            let diff = diff_file(&file.input, &file.output);

            eprintln!("{}:", style::INFO.apply_to(file.source.display()));
            eprintln!("{diff}");
        }

        miette::bail!("failed to format files")
    }
}

fn format_files(files: Vec<String>, cache: &mut FileCache) -> miette::Result<()> {
    for file in unformatted_files(files, cache)? {
        fs::write(file.destination, file.output).into_diagnostic()?;
    }

    Ok(())
}

fn unformatted_files(
    files: Vec<String>,
    cache: &mut FileCache,
) -> miette::Result<Vec<Unformatted>> {
    let mut problem_files = Vec::with_capacity(files.len());

    for file_path in files {
        let path = PathBuf::from_str(&file_path).into_diagnostic()?;

        if path.is_dir() {
            for path in starstream_files_excluding_gitignore(&path) {
                format_file(&mut problem_files, path, cache)?
            }
        } else {
            format_file(&mut problem_files, path, cache)?
        }
    }

    Ok(problem_files)
}

fn format_file(
    problem_files: &mut Vec<Unformatted>,
    path: PathBuf,
    cache: &mut FileCache,
) -> miette::Result<()> {
    let source = cache.fetch(&path).expect("Error reading Starstream input");

    let parse_result = starstream_compiler::parse_program(source.text());

    for error in parse_result.errors() {
        starstream_compiler::error_to_report(error.clone())
            .eprint(source)
            .into_diagnostic()?;
    }

    let Some(program) = parse_result.into_output() else {
        std::process::exit(1)
    };

    let output = formatter::program(&program).into_diagnostic()?;

    if source.text() != output {
        problem_files.push(Unformatted {
            source: path.clone(),
            destination: path,
            input: source.text().to_string(),
            output,
        });
    }

    Ok(())
}

pub fn starstream_files_excluding_gitignore(dir: &Path) -> impl Iterator<Item = PathBuf> + '_ {
    ignore::WalkBuilder::new(dir)
        .follow_links(true)
        .require_git(false)
        .build()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().map(|t| t.is_file()).unwrap_or(false))
        .map(ignore::DirEntry::into_path)
        .filter(move |d| is_starstream_path(d))
}

fn is_starstream_path(path: &Path) -> bool {
    path.extension()
        .map(|e| e == starstream_compiler::FILE_EXTENSION)
        .unwrap_or_default()
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
