#[derive(Debug)]
pub struct Style {
    pub info: console::Style,
    pub text: console::Style,
    pub success: console::Style,
    pub warning: console::Style,
    pub error: console::Style,
}

impl Default for Style {
    fn default() -> Self {
        Self {
            info: console::Style::new().cyan().bold(),
            text: console::Style::new().white().bold(),
            success: console::Style::new().green().bold(),
            warning: console::Style::new().yellow(),
            error: console::Style::new().red(),
        }
    }
}
