#![allow(dead_code)]
use console::{Emoji, Style};

pub const ARROW: Emoji = Emoji("→", "->");

pub const INFO: Style = Style::new().cyan().bold();
pub const TEXT: Style = Style::new().white().bold();
pub const SUCCESS: Style = Style::new().green().bold();
pub const WARNING: Style = Style::new().yellow();
pub const ERROR: Style = Style::new().red();
