use std::fmt;

/// Rich inference trace node capturing the rule name, typing environment (when
/// relevant), the term being analyzed, the result, and any nested steps.
#[derive(Clone, Debug, Default)]
pub struct InferenceTree {
    pub rule: String,
    pub context: String,
    pub subject: String,
    pub result: String,
    pub children: Vec<InferenceTree>,
}

impl InferenceTree {
    pub fn new(
        rule: impl Into<String>,
        context: impl Into<String>,
        subject: impl Into<String>,
        result: impl Into<String>,
    ) -> Self {
        Self {
            rule: rule.into(),
            context: context.into(),
            subject: subject.into(),
            result: result.into(),
            children: Vec::new(),
        }
    }

    pub fn with_children(mut self, children: Vec<InferenceTree>) -> Self {
        self.children = children;
        self
    }

    pub fn push_child(&mut self, child: InferenceTree) {
        self.children.push(child);
    }

    fn has_context(&self) -> bool {
        !self.context.is_empty()
    }
}

impl fmt::Display for InferenceTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

impl InferenceTree {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        // Skip empty/default trees (e.g., from struct/enum definitions)
        if self.rule.is_empty() && self.subject.is_empty() && self.result.is_empty() {
            return Ok(());
        }

        let prefix = "  ".repeat(indent);

        if self.has_context() {
            writeln!(
                f,
                "{}{}: {} ⊢ {} ⇒ {}",
                prefix, self.rule, self.context, self.subject, self.result
            )?;
        } else {
            writeln!(
                f,
                "{}{}: {} => {}",
                prefix, self.rule, self.subject, self.result
            )?;
        }

        for child in &self.children {
            child.fmt_with_indent(f, indent + 1)?;
        }

        Ok(())
    }
}
