use std::fmt;

use console::Emoji;

pub struct Icons {
    pub arrow: console::Emoji<'static, 'static>,
}

impl Default for Icons {
    fn default() -> Self {
        Self {
            arrow: Emoji::new("→", "->"),
        }
    }
}

impl fmt::Debug for Icons {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Icons").finish_non_exhaustive()
    }
}
