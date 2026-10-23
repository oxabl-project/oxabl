//! Inline suppressions: `/* oxabl-ignore <CODE>: <reason> */`.
//!
//! A suppression names one rule and must give a reason. It covers findings of
//! that rule on one line:
//!
//! - a comment that follows code on its line covers that line;
//! - a comment that is alone on its line covers the line after it (the line
//!   after the comment's last line, for a multi-line block comment).
//!
//! `//` line comments work the same way. A suppression with no reason, with a
//! rule the registry does not know, or with no rule at all is
//! [`DirectiveKind::Invalid`]: it suppresses nothing and the caller reports
//! [`LINT0007`]. A valid suppression that covers no finding is reported as
//! [`LINT0008`].
//!
//! This module only reads comments and does the line arithmetic. The caller
//! owns the diagnostics: it knows the file ids and the configured severities.

use crate::Registry;

/// Diagnostic code for `invalid-suppression` (LINT0007).
pub const LINT0007: &str = "LINT0007";
/// Diagnostic code for `unused-suppression` (LINT0008).
pub const LINT0008: &str = "LINT0008";

/// Name of `invalid-suppression`.
pub const INVALID_NAME: &str = "invalid-suppression";
/// Name of `unused-suppression`.
pub const UNUSED_NAME: &str = "unused-suppression";

const MARKER: &str = "oxabl-ignore";

/// Why a suppression comment does not suppress.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Invalid {
    /// No rule code follows `oxabl-ignore`.
    NoCode,
    /// The rule is named but the reason is missing or empty.
    NoReason(String),
    /// The code names no registered rule.
    UnknownCode(String),
}

/// What a suppression comment says.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirectiveKind {
    /// A well-formed suppression of `code` with a `reason`.
    Valid {
        /// The rule's canonical code.
        code: String,
        /// The text after the colon, trimmed.
        reason: String,
    },
    /// A comment that starts with `oxabl-ignore` but cannot suppress.
    Invalid(Invalid),
}

/// One `oxabl-ignore` comment.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Directive {
    /// Byte offset of the comment start.
    pub start: u32,
    /// Byte offset one past the comment end.
    pub end: u32,
    /// The 1-based line the comment covers.
    pub target_line: u32,
    /// What the comment says.
    pub kind: DirectiveKind,
}

impl Directive {
    /// Whether this is a valid suppression of `code` on `line`.
    pub fn covers(&self, code: &str, line: u32) -> bool {
        self.target_line == line
            && matches!(&self.kind, DirectiveKind::Valid { code: c, .. } if c.eq_ignore_ascii_case(code))
    }
}

/// Every `oxabl-ignore` comment in `source`, in source order. Comment text
/// inside a string literal is not a comment.
pub fn scan(source: &str, registry: &Registry) -> Vec<Directive> {
    let bytes = source.as_bytes();
    let mut out = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            quote @ (b'"' | b'\'') => i = skip_string(bytes, i, quote),
            b'/' if bytes.get(i + 1) == Some(&b'*') => {
                let end = block_comment_end(bytes, i);
                let body_end = if source[i..end].ends_with("*/") && end - i >= 4 {
                    end - 2
                } else {
                    end
                };
                push(&mut out, source, registry, i, end, i + 2, body_end);
                i = end;
            }
            b'/' if bytes.get(i + 1) == Some(&b'/') => {
                let end = source[i..].find('\n').map_or(source.len(), |n| i + n);
                push(&mut out, source, registry, i, end, i + 2, end);
                i = end;
            }
            _ => i += 1,
        }
    }
    out
}

/// The index after the string literal that opens at `at`. `~` escapes the next
/// byte; a doubled quote stays inside the string.
fn skip_string(bytes: &[u8], at: usize, quote: u8) -> usize {
    let mut i = at + 1;
    while i < bytes.len() {
        match bytes[i] {
            b'~' => i += 2,
            b if b == quote => {
                if bytes.get(i + 1) == Some(&quote) {
                    i += 2;
                } else {
                    return i + 1;
                }
            }
            _ => i += 1,
        }
    }
    bytes.len()
}

/// The index after the block comment that opens at `at`. Block comments nest.
fn block_comment_end(bytes: &[u8], at: usize) -> usize {
    let mut depth = 0usize;
    let mut i = at;
    while i < bytes.len() {
        if bytes[i] == b'/' && bytes.get(i + 1) == Some(&b'*') {
            depth += 1;
            i += 2;
        } else if bytes[i] == b'*' && bytes.get(i + 1) == Some(&b'/') {
            depth -= 1;
            i += 2;
            if depth == 0 {
                return i;
            }
        } else {
            i += 1;
        }
    }
    bytes.len()
}

fn push(
    out: &mut Vec<Directive>,
    source: &str,
    registry: &Registry,
    start: usize,
    end: usize,
    body_start: usize,
    body_end: usize,
) {
    let body = source[body_start..body_end.max(body_start)].trim();
    let Some(rest) = body.strip_prefix(MARKER) else {
        return;
    };
    if rest.chars().next().is_some_and(|c| !c.is_whitespace()) {
        return;
    }
    let kind = parse(rest.trim(), registry);
    let line_start = source[..start].rfind('\n').map_or(0, |n| n + 1);
    let alone = source[line_start..start].trim().is_empty();
    let first_line = line_of(source, start);
    let target_line = if alone {
        line_of(source, end.saturating_sub(1).max(start)) + 1
    } else {
        first_line
    };
    out.push(Directive {
        start: start as u32,
        end: end as u32,
        target_line,
        kind,
    });
}

fn parse(rest: &str, registry: &Registry) -> DirectiveKind {
    let code_end = rest
        .find(|c: char| c == ':' || c.is_whitespace())
        .unwrap_or(rest.len());
    let (code, after) = rest.split_at(code_end);
    if code.is_empty() {
        return DirectiveKind::Invalid(Invalid::NoCode);
    }
    let Some(rule) = registry.rule(code) else {
        return DirectiveKind::Invalid(Invalid::UnknownCode(code.to_string()));
    };
    let reason = after.trim_start().strip_prefix(':').map(str::trim);
    match reason {
        Some(reason) if !reason.is_empty() => DirectiveKind::Valid {
            code: rule.meta().code.to_string(),
            reason: reason.to_string(),
        },
        _ => DirectiveKind::Invalid(Invalid::NoReason(code.to_string())),
    }
}

/// The 1-based line of byte offset `at`.
fn line_of(source: &str, at: usize) -> u32 {
    1 + source.as_bytes()[..at.min(source.len())]
        .iter()
        .filter(|&&b| b == b'\n')
        .count() as u32
}

/// Which directive suppresses a finding of `code` that starts at byte `at`.
pub fn suppressor(directives: &[Directive], source: &str, code: &str, at: u32) -> Option<usize> {
    let line = line_of(source, at as usize);
    directives.iter().position(|d| d.covers(code, line))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scan_all(source: &str) -> Vec<Directive> {
        scan(source, Registry::builtin_shared())
    }

    fn valid(code: &str, reason: &str) -> DirectiveKind {
        DirectiveKind::Valid {
            code: code.to_string(),
            reason: reason.to_string(),
        }
    }

    #[test]
    fn a_comment_alone_on_its_line_covers_the_next_line() {
        let src =
            "/* oxabl-ignore LINT0002: kept for layout */\nDEFINE VARIABLE v AS INTEGER NO-UNDO.\n";
        let found = scan_all(src);
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].target_line, 2);
        assert_eq!(found[0].kind, valid("LINT0002", "kept for layout"));
    }

    #[test]
    fn a_trailing_comment_covers_its_own_line() {
        let src = "DEFINE VARIABLE v AS INTEGER NO-UNDO. // oxabl-ignore LINT0002: reason\nx.\n";
        let found = scan_all(src);
        assert_eq!(found[0].target_line, 1);
        let src = "DEFINE VARIABLE v AS INTEGER NO-UNDO. /* oxabl-ignore LINT0002: reason */\n";
        assert_eq!(scan_all(src)[0].target_line, 1);
    }

    #[test]
    fn a_multi_line_block_comment_covers_the_line_after_its_end() {
        let src = "/* oxabl-ignore LINT0002:\n   a long reason */\nDEFINE VARIABLE v AS INTEGER NO-UNDO.\n";
        let found = scan_all(src);
        assert_eq!(found[0].target_line, 3);
        assert_eq!(found[0].kind, valid("LINT0002", "a long reason"));
    }

    #[test]
    fn the_rule_name_and_lower_case_code_resolve_to_the_code() {
        let found = scan_all("// oxabl-ignore unused-variable: r\n// oxabl-ignore lint0002: r\n");
        assert_eq!(found[0].kind, valid("LINT0002", "r"));
        assert_eq!(found[1].kind, valid("LINT0002", "r"));
    }

    #[test]
    fn a_missing_reason_is_invalid() {
        for src in [
            "// oxabl-ignore LINT0002\n",
            "// oxabl-ignore LINT0002:\n",
            "/* oxabl-ignore LINT0002:   */\n",
            "// oxabl-ignore LINT0002 because\n",
        ] {
            assert_eq!(
                scan_all(src)[0].kind,
                DirectiveKind::Invalid(Invalid::NoReason("LINT0002".into())),
                "{src}"
            );
        }
    }

    #[test]
    fn an_unknown_code_or_no_code_is_invalid() {
        assert_eq!(
            scan_all("// oxabl-ignore NOPE001: why\n")[0].kind,
            DirectiveKind::Invalid(Invalid::UnknownCode("NOPE001".into()))
        );
        assert_eq!(
            scan_all("// oxabl-ignore\n")[0].kind,
            DirectiveKind::Invalid(Invalid::NoCode)
        );
    }

    #[test]
    fn other_comments_and_string_contents_are_not_directives() {
        let src = "// oxabl-ignored LINT0002: x\n/* note */\nMESSAGE \"/* oxabl-ignore LINT0002: x */\".\nMESSAGE 'a ~' // oxabl-ignore LINT0002: x'.\n";
        assert!(scan_all(src).is_empty());
    }

    #[test]
    fn a_suppressor_matches_code_and_line_only() {
        let src = "// oxabl-ignore LINT0002: r\nDEFINE VARIABLE v AS INTEGER NO-UNDO.\nDEFINE VARIABLE w AS INTEGER NO-UNDO.\n";
        let found = scan_all(src);
        let second = src.find("DEFINE").unwrap() as u32;
        let third = src.rfind("DEFINE").unwrap() as u32;
        assert_eq!(suppressor(&found, src, "LINT0002", second), Some(0));
        assert_eq!(suppressor(&found, src, "LINT0006", second), None);
        assert_eq!(suppressor(&found, src, "LINT0002", third), None);
    }
}
