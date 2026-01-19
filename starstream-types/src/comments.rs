//! Comment map for tracking and querying comments separately from the AST.

use crate::{Span, ast::Comment};

/// A map of all comments in a source file, enabling span-based queries.
///
/// Comments are stored sorted by start position, allowing efficient
/// lookups for comments before/after any AST node span.
#[derive(Debug, Default, Clone)]
pub struct CommentMap {
    comments: Vec<Comment>,
}

impl CommentMap {
    /// Create an empty comment map.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a comment map from a list of comments (will be sorted and deduplicated).
    pub fn from_comments(mut comments: Vec<Comment>) -> Self {
        comments.sort_by_key(|c| c.0.start);
        comments.dedup_by_key(|c| (c.0.start, c.0.end));
        Self { comments }
    }

    /// Add a comment to the map (maintains sorted order).
    pub fn push(&mut self, comment: Comment) {
        let pos = self
            .comments
            .binary_search_by_key(&comment.0.start, |c| c.0.start)
            .unwrap_or_else(|i| i);
        self.comments.insert(pos, comment);
    }

    /// Get all comments in the map.
    pub fn all(&self) -> &[Comment] {
        &self.comments
    }

    /// Find comments that appear before a node (on previous lines).
    ///
    /// Returns comments whose span ends before `node_start` and which are
    /// not on the same line as any preceding code.
    pub fn comments_before(&self, node_span: Span, source: &str) -> Vec<&Comment> {
        let node_start = node_span.start;
        let node_line = line_of_offset(source, node_start);

        self.comments
            .iter()
            .filter(|c| {
                // Comment must end before the node starts
                if c.0.end > node_start {
                    return false;
                }
                // Comment must be on a line before the node
                let comment_line = line_of_offset(source, c.0.start);
                comment_line < node_line
            })
            .collect()
    }

    /// Find comments that logically belong before a node for formatting.
    ///
    /// This finds comments between `prev_end` and `node_span.start`
    /// that are on their own line (not inline with previous code).
    pub fn comments_between(
        &self,
        prev_end: usize,
        node_span: Span,
        source: &str,
    ) -> Vec<&Comment> {
        // Bounds check for manually constructed AST with invalid spans
        if prev_end > source.len() || node_span.start > source.len() {
            return Vec::new();
        }

        self.comments
            .iter()
            .filter(|c| {
                // Comment must be between prev_end and node_start
                c.0.start >= prev_end && c.0.end <= node_span.start
            })
            .filter(|c| {
                // Bounds check for comment
                if c.0.start > source.len() {
                    return false;
                }
                // Comments at the start of the range are always "on their own line"
                if c.0.start == prev_end && prev_end == 0 {
                    return true;
                }
                // Otherwise check if it's on its own line (has newline before it)
                if c.0.start <= prev_end {
                    return true; // Comment starts at or before prev_end boundary
                }
                let before = &source[prev_end..c.0.start];
                before.contains('\n')
            })
            .collect()
    }

    /// Find an inline comment after a node (on the same line).
    ///
    /// Returns the first comment that starts after `node_span.end` but
    /// is on the same line as the node's end (no newline between them).
    pub fn inline_comment_after(&self, node_span: Span, source: &str) -> Option<&Comment> {
        // Bounds check - span might be out of range for manually constructed AST
        if node_span.start > source.len() || node_span.end > source.len() {
            return None;
        }

        // Find the actual end of content (excluding trailing whitespace in the span)
        let span_content = &source[node_span.start..node_span.end];
        let content_end = node_span.start + span_content.trim_end().len();

        self.comments.iter().find(|c| {
            // Comment must start after the node ends
            if c.0.start < node_span.end {
                return false;
            }
            // Bounds check for comment
            if c.0.start > source.len() {
                return false;
            }
            // No newline allowed between actual content end and comment start
            let between = &source[content_end..c.0.start];
            !between.contains('\n')
        })
    }

    /// Extract doc comments (`///`) that appear immediately before a node.
    ///
    /// Returns the concatenated content of consecutive `///` comments
    /// directly preceding the node (no other code between), or `None` if there are none.
    pub fn doc_comments(&self, node_span: Span, source: &str) -> Option<String> {
        let node_start = node_span.start;

        // Find comments that end before this node, sorted by position (descending)
        let mut candidates: Vec<_> = self
            .comments
            .iter()
            .filter(|c| c.0.end <= node_start)
            .collect();
        candidates.sort_by_key(|c| std::cmp::Reverse(c.0.end));

        // Walk backwards from the node, collecting consecutive doc comments
        let mut doc_comments = Vec::new();
        let mut current_pos = node_start;

        for comment in candidates {
            // Check if there's only whitespace between this comment and current_pos
            let between = &source[comment.0.end..current_pos];
            if !between.chars().all(|c| c.is_whitespace()) {
                // There's code between, stop looking
                break;
            }

            let text = source.get(comment.0.start..comment.0.end)?;
            if text.starts_with("///") {
                // It's a doc comment, add it
                let content = text.strip_prefix("///").unwrap_or("");
                let content = content.strip_prefix(' ').unwrap_or(content);
                // Strip trailing newline if present
                let content = content.strip_suffix('\n').unwrap_or(content);
                doc_comments.push(content);
                current_pos = comment.0.start;
            } else if text.starts_with("//") || text.starts_with("/*") {
                // Regular comment breaks the doc comment chain
                break;
            } else {
                break;
            }
        }

        if doc_comments.is_empty() {
            None
        } else {
            // Reverse since we collected them backwards
            doc_comments.reverse();
            Some(doc_comments.join("\n"))
        }
    }
}

/// Get the 0-indexed line number for a byte offset in source.
fn line_of_offset(source: &str, offset: usize) -> usize {
    source[..offset.min(source.len())].matches('\n').count()
}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::span::Span as SpanTrait;

    fn span(start: usize, end: usize) -> Span {
        Span::new((), start..end)
    }

    fn comment(start: usize, end: usize) -> Comment {
        Comment(span(start, end))
    }

    #[test]
    fn test_comments_before() {
        let source = "// comment\nfn foo() {}";
        let map = CommentMap::from_comments(vec![comment(0, 10)]); // "// comment"

        let fn_span = span(11, 22); // "fn foo() {}"
        let before = map.comments_before(fn_span, source);

        assert_eq!(before.len(), 1);
        assert_eq!(before[0].0.start, 0);
    }

    #[test]
    fn test_inline_comment_after() {
        let source = "let x = 1; // inline\nlet y = 2;";
        let map = CommentMap::from_comments(vec![comment(11, 20)]); // "// inline"

        let stmt_span = span(0, 10); // "let x = 1;"
        let inline = map.inline_comment_after(stmt_span, source);

        assert!(inline.is_some());
        assert_eq!(inline.unwrap().0.start, 11);
    }

    #[test]
    fn test_no_inline_comment_on_different_line() {
        let source = "let x = 1;\n// not inline\nlet y = 2;";
        let map = CommentMap::from_comments(vec![comment(11, 24)]); // "// not inline"

        let stmt_span = span(0, 10); // "let x = 1;"
        let inline = map.inline_comment_after(stmt_span, source);

        assert!(inline.is_none());
    }

    #[test]
    fn test_doc_comments() {
        let source = "/// Doc line 1\n/// Doc line 2\nfn foo() {}";
        let map = CommentMap::from_comments(vec![
            comment(0, 14),  // "/// Doc line 1"
            comment(15, 29), // "/// Doc line 2"
        ]);

        let fn_span = span(30, 41); // "fn foo() {}"
        let doc = map.doc_comments(fn_span, source);

        assert_eq!(doc, Some("Doc line 1\nDoc line 2".to_string()));
    }

    #[test]
    fn test_doc_comments_mixed_with_regular() {
        let source = "// regular\n/// doc\nfn foo() {}";
        let map = CommentMap::from_comments(vec![
            comment(0, 10),  // "// regular"
            comment(11, 18), // "/// doc"
        ]);

        let fn_span = span(19, 30); // "fn foo() {}"
        let doc = map.doc_comments(fn_span, source);

        // Only doc comments are extracted
        assert_eq!(doc, Some("doc".to_string()));
    }

    #[test]
    fn test_doc_comments_multiple_functions() {
        // Two functions with separate doc comments
        let source = "/// Doc for foo\nfn foo() {}\n\n/// Doc for bar\nfn bar() {}";
        let map = CommentMap::from_comments(vec![
            comment(0, 15),  // "/// Doc for foo"
            comment(29, 44), // "/// Doc for bar"
        ]);

        // fn foo() {} starts at 16, ends at 27
        let foo_span = span(16, 27);
        let foo_doc = map.doc_comments(foo_span, source);
        assert_eq!(foo_doc, Some("Doc for foo".to_string()));

        // fn bar() {} starts at 45, ends at 56
        let bar_span = span(45, 56);
        let bar_doc = map.doc_comments(bar_span, source);
        // Should only get "Doc for bar", not both
        assert_eq!(bar_doc, Some("Doc for bar".to_string()));
    }
}
