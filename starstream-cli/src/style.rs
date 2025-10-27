#![allow(dead_code)]
use console::{Emoji, Style};

pub const ARROW: Emoji = Emoji("→", "->");

pub const INFO: Style = Style::new().cyan();
pub const TEXT: Style = Style::new().white();
pub const SUCCESS: Style = Style::new().green();
pub const WARNING: Style = Style::new().yellow();
pub const ERROR: Style = Style::new().red();

/// Returns the given string if the condition is true, otherwise returns an empty string.
///
/// Useful for conditionally including text in formatted output.
pub fn r_if_then(cond: bool, then: &str) -> &str {
    r_if(cond, then, "")
}

/// Returns one of two strings based on a condition.
///
/// This is particularly useful for pluralization and other conditional text formatting
/// where you need to choose between two alternatives.
pub fn r_if<'a>(cond: bool, then: &'a str, els: &'a str) -> &'a str {
    if cond { then } else { els }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_r_if_pluralization() {
        let count = 2;
        let msg = format!("{} {}", count, r_if(count != 1, "files", "file"));
        assert_eq!(msg, "2 files");

        let count = 1;
        let msg = format!("{} {}", count, r_if(count != 1, "files", "file"));
        assert_eq!(msg, "1 file");
    }

    #[test]
    fn test_r_if_then_conditional_message() {
        let has_error = true;
        let msg = format!(
            "Compilation complete{}",
            r_if_then(has_error, " with errors")
        );
        assert_eq!(msg, "Compilation complete with errors");

        let has_error = false;
        let msg = format!(
            "Compilation complete{}",
            r_if_then(has_error, " with errors")
        );
        assert_eq!(msg, "Compilation complete");
    }
}
