//! Blank-line normalization pass (U6 / R2.4 / KTD7).
//!
//! The printer emits every physical source line into the buffer, so blank runs
//! are already present as blank [`Line`]s — the observed blank counts from the
//! source gaps, never tokenized (R2.3). This pass rewrites those runs in place:
//!
//! - clamp each run to `max_consecutive_blank_lines`;
//! - drop a run immediately **after** a block opener (a line ending in `:`);
//! - drop a run immediately **before** a block `END`;
//! - trim leading file blanks;
//! - drop trailing blanks so the flush leaves exactly one trailing newline.
//!
//! It is idempotent by construction: re-running it on already-normalized output
//! changes nothing (KTD7 / the R6.1 anchor for blanks).
//!
//! `blank_lines_between_sections` (section-boundary blanks) depends on the
//! section/ordering machinery deferred to v2, so it is read but not enforced in
//! v1 — consistent with the reflow no-op (R4.3/R4.4).

use oxabl_lexer::{Kind, tokenize};
use oxabl_style::StyleGuide;

use crate::ir::{Line, LineBuf};

/// The first "word" of a line's content (leading alphanumerics/hyphens), for
/// detecting an `END` line without matching identifiers like `end-date`.
fn first_word(content: &str) -> &str {
    let s = content.trim_start();
    let end = s
        .find(|c: char| !(c.is_ascii_alphanumeric() || c == '-'))
        .unwrap_or(s.len());
    &s[..end]
}

fn is_block_opener(content: &str) -> bool {
    // A block opener's last *code* token is `:` (`DO:`, `PROCEDURE foo:`, a
    // label, `OTHERWISE:`). Tokenize and look at the last non-comment token so a
    // trailing comment (`DO: /* x */`) doesn't hide the colon, and a `:` inside a
    // string or a member-access colon that isn't line-final doesn't fake one.
    tokenize(content)
        .iter()
        .rev()
        .find(|t| t.kind != Kind::Comment && t.kind != Kind::Eof)
        .is_some_and(|t| t.kind == Kind::Colon)
}

fn is_block_end(content: &str) -> bool {
    first_word(content).eq_ignore_ascii_case("END")
}

/// A blank line the normalizer may clamp or drop. A `protected` line (one that
/// begins inside a multi-line token) is never droppable even when its content is
/// empty: an empty physical line inside a multi-line string literal is a
/// significant byte of the string's value, so it must survive verbatim (#95).
fn is_droppable_blank(line: &Line) -> bool {
    line.is_blank() && !line.protected
}

/// Normalize blank runs in `buf` per `style`.
pub(crate) fn normalize(buf: &mut LineBuf, style: &StyleGuide) {
    let max = style.max_consecutive_blank_lines;
    let lines = buf.lines();
    let mut out: Vec<Line> = Vec::with_capacity(lines.len());

    // Trim leading file blanks.
    let mut i = 0;
    while i < lines.len() && is_droppable_blank(&lines[i]) {
        i += 1;
    }

    while i < lines.len() {
        if is_droppable_blank(&lines[i]) {
            let mut j = i;
            while j < lines.len() && is_droppable_blank(&lines[j]) {
                j += 1;
            }
            // Trailing blanks (nothing follows): drop entirely — flush adds the
            // single trailing newline.
            if j >= lines.len() {
                break;
            }
            let run_len = j - i;
            let after_opener = out.last().is_some_and(|p| is_block_opener(&p.content));
            let before_end = is_block_end(&lines[j].content);
            let allowed = if after_opener || before_end { 0 } else { max };
            for _ in 0..run_len.min(allowed) {
                out.push(Line {
                    indent: 0,
                    content: String::new(),
                    protected: false,
                });
            }
            i = j;
        } else {
            out.push(lines[i].clone());
            i += 1;
        }
    }

    buf.set_lines(out);
}
