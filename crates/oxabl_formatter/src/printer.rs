//! Layout printer (U4 / R5): a source-order walk of the AST that reindents each
//! source line to its structural block depth, places own-line comments at the
//! right depth from the [`CommentMap`], and applies the in-place token rules
//! (`end_with_type`), while emitting identifiers, literals, and comment bodies
//! **verbatim from source** (R5.2 — the core anti-mangle guarantee).
//!
//! ## Emit mechanism (Fable finding 4, generalized)
//!
//! Rather than reconstruct each statement token-by-token, the printer works at
//! **line granularity**: every physical source line is preserved and only its
//! *leading whitespace* is rewritten. A line's indentation comes from the
//! innermost AST node governing it:
//!
//! - a line on which a statement **starts** takes that statement's block depth
//!   (the shallowest, if several start there — the leading token belongs to the
//!   outermost);
//! - a **continuation** line (no statement starts on it) is shifted by the same
//!   delta as the first line of the innermost statement covering it, preserving
//!   the author's intra-statement alignment;
//! - an **own-line comment** line takes the depth attachment assigned it
//!   (leading → the led node's depth; dangling → the block's body depth).
//!
//! This is generic over *every* `StatementKind` with no per-construct emitter,
//! preserves existing intra-statement line breaks by construction (the
//! no-movement boundary), and makes interior/trailing comments a non-event —
//! they ride along inside their line's verbatim content. It is idempotent: a
//! second pass finds every start line already at its target depth, so the delta
//! is zero and nothing moves.

use std::collections::HashMap;

use oxabl_ast::{NodeId, Statement};
use oxabl_lexer::{Kind, Token, tokenize};
use oxabl_style::StyleGuide;

use crate::attach::CommentMap;
use crate::ir::LineBuf;
use crate::keywords;
use crate::tree::{children_with_deltas, is_prefix_wrapper, typed_end_keyword};

/// A physical source line with its measured leading indent and content
/// (leading whitespace and line terminator stripped).
struct SrcLine {
    start: usize,
    /// Byte offset where the content (after leading whitespace) begins.
    content_start: usize,
    leading_cols: usize,
    content: String,
}

/// Split `source` into physical lines, measuring each line's leading-indent
/// width (spaces count 1, tabs count `size`) and stripping the terminator.
fn split_lines(source: &str, size: usize) -> Vec<SrcLine> {
    let bytes = source.as_bytes();
    let n = bytes.len();
    let mut lines = Vec::new();
    let mut i = 0;
    while i < n {
        let mut j = i;
        while j < n && bytes[j] != b'\n' {
            j += 1;
        }
        let mut end = j;
        if end > i && bytes[end - 1] == b'\r' {
            end -= 1;
        }
        let mut k = i;
        let mut cols = 0;
        while k < end {
            match bytes[k] {
                b' ' => {
                    cols += 1;
                    k += 1;
                }
                b'\t' => {
                    cols += size;
                    k += 1;
                }
                _ => break,
            }
        }
        lines.push(SrcLine {
            start: i,
            content_start: k,
            leading_cols: cols,
            content: source[k..end].to_string(),
        });
        i = j + 1;
    }
    lines
}

fn line_index(line_starts: &[usize], offset: usize) -> usize {
    if line_starts.is_empty() {
        return 0;
    }
    line_starts
        .partition_point(|&s| s <= offset)
        .saturating_sub(1)
}

/// Recursively record, per line: the shallowest statement starting on it
/// (`starter`), the deepest statement covering it (`cover`, carrying that
/// statement's first-line indent for the continuation delta), each node's
/// depth, and the last line + type keyword of every typed block (for
/// `end_with_type`).
#[allow(clippy::too_many_arguments)]
fn collect(
    stmt: &Statement,
    depth: usize,
    line_starts: &[usize],
    leadings: &[usize],
    starter: &mut [Option<usize>],
    cover: &mut [Option<(usize, usize)>],
    depth_of: &mut HashMap<NodeId, usize>,
    typed_ends: &mut Vec<(usize, &'static str)>,
    block_ends: &mut Vec<(usize, usize)>,
) {
    let fl = line_index(line_starts, stmt.span.start as usize);
    let ll = line_index(line_starts, (stmt.span.end.saturating_sub(1)) as usize);
    depth_of.insert(stmt.id, depth);
    starter[fl] = Some(starter[fl].map_or(depth, |e| e.min(depth)));
    let fleading = leadings[fl];
    for slot in cover[fl..=ll].iter_mut() {
        match slot {
            Some((d, _)) if *d >= depth => {}
            _ => *slot = Some((depth, fleading)),
        }
    }
    if let Some(ty) = typed_end_keyword(&stmt.kind) {
        typed_ends.push((ll, ty));
    }
    if let Some(children) = children_with_deltas(&stmt.kind) {
        // A block's closing `END` line is a structural line that must snap to the
        // block's own depth, not delta-preserve like an intra-statement
        // continuation. Prefix wrappers (`IF … THEN`, `ELSE`, a label, `ON …`)
        // are excluded: they have no closer of their own, so their "last line"
        // is just their branch's last physical line — which, for a wrapped
        // multi-line non-block branch, must keep its continuation indent rather
        // than be snapped to the wrapper's depth (issue #98).
        if !is_prefix_wrapper(&stmt.kind) {
            block_ends.push((ll, depth));
        }
        // Each child nests by its own delta: normally +1, but a prefix wrapper
        // (`IF … THEN`, `ELSE`, a label, `ON …`) contributes 0 for a branch that
        // is itself a self-delimiting block or an else-if (the block's own
        // `DO:`/`END` supplies the level), so `IF x THEN DO:` is one level, not
        // two — while a leaf or a THEN-nested bare `IF` still gets its +1.
        for (ch, delta) in children {
            collect(
                ch,
                depth + delta,
                line_starts,
                leadings,
                starter,
                cover,
                depth_of,
                typed_ends,
                block_ends,
            );
        }
    }
}

/// Case a block-type keyword to match the resolved keyword case.
fn cased_type(ty: &str, style: &StyleGuide) -> String {
    match style.keyword_case {
        oxabl_style::KeywordCase::Lowercase => ty.to_ascii_lowercase(),
        _ => ty.to_string(),
    }
}

/// Insert the block-type keyword after a bare closing `END` on `content`, or
/// return `None` if the `END` is already typed or absent (idempotent).
fn apply_end_type(content: &str, ty: &str) -> Option<String> {
    let toks = tokenize(content);
    let end_idx = toks.iter().rposition(|t| t.kind == Kind::End)?;
    let end_tok = &toks[end_idx];
    if let Some(nt) = toks.get(end_idx + 1) {
        // Already typed (e.g. `END PROCEDURE`) → nothing to do.
        if content[nt.start..nt.end].eq_ignore_ascii_case(ty) {
            return None;
        }
    }
    let at = end_tok.end;
    let mut s = String::with_capacity(content.len() + ty.len() + 1);
    s.push_str(&content[..at]);
    s.push(' ');
    s.push_str(ty);
    s.push_str(&content[at..]);
    Some(s)
}

/// Print `program` against `source` and the attached comments into a line
/// buffer. Blank-line normalization (U6) and the guard (U7) run afterward.
pub(crate) fn print(
    source: &str,
    program: &oxabl_parser::Program,
    cmap: &CommentMap,
    style: &StyleGuide,
    toks: &[Token],
) -> LineBuf {
    let size = style.indent_size.max(1);
    let lines = split_lines(source, size);
    let mut buf = LineBuf::new();
    if lines.is_empty() {
        return buf;
    }

    let line_starts: Vec<usize> = lines.iter().map(|l| l.start).collect();
    let leadings: Vec<usize> = lines.iter().map(|l| l.leading_cols).collect();
    let n = lines.len();

    let mut starter: Vec<Option<usize>> = vec![None; n];
    let mut cover: Vec<Option<(usize, usize)>> = vec![None; n];
    let mut depth_of: HashMap<NodeId, usize> = HashMap::new();
    let mut typed_ends: Vec<(usize, &'static str)> = Vec::new();
    let mut block_ends: Vec<(usize, usize)> = Vec::new();

    for stmt in &program.statements {
        collect(
            stmt,
            0,
            &line_starts,
            &leadings,
            &mut starter,
            &mut cover,
            &mut depth_of,
            &mut typed_ends,
            &mut block_ends,
        );
    }

    // Absolute indent per line from the structural walk. Start-lines snap to
    // their depth; continuation lines delta-preserve the innermost covering
    // statement's alignment.
    let mut indent = vec![0usize; n];
    for (l, slot) in indent.iter_mut().enumerate() {
        if let Some(d) = starter[l] {
            *slot = d * size;
        } else if let Some((d, fleading)) = cover[l] {
            let delta = d as isize * size as isize - fleading as isize;
            *slot = (leadings[l] as isize + delta).max(0) as usize;
        }
    }
    // Snap block `END` lines to the block's depth (a structural line, not an
    // intra-statement continuation) — but never override a line that a statement
    // actually starts on (e.g. an inline `MESSAGE x. END.`).
    for (ll, d) in block_ends {
        if starter[ll].is_none() {
            indent[ll] = d * size;
        }
    }

    // Own-line comments override to the depth attachment gave them: leading →
    // the led node's depth, dangling → the block's body depth. This corrects the
    // one-off where a comment line sits inside a block's span (so `cover` put it
    // at the block's depth, not the body's).
    //
    // A line a statement *starts* on already carries that statement's structural
    // indent, so a comment that merely shares such a line — a trailing comment
    // like `IF … THEN DO: /* x */`, which attachment may hand back as a *leading*
    // comment of the block body — must not drag the code line to the comment's
    // depth. Skip those lines; the code's indent wins (matching the `block_ends`
    // guard above).
    let set_comment = |indent: &mut [usize], span_start: usize, span_end: usize, depth: usize| {
        let cfl = line_index(&line_starts, span_start);
        let cll = line_index(&line_starts, span_end.saturating_sub(1));
        let delta = depth as isize * size as isize - leadings[cfl] as isize;
        for l in cfl..=cll {
            if starter[l].is_some() {
                continue;
            }
            indent[l] = (leadings[l] as isize + delta).max(0) as usize;
        }
    };
    for (id, nc) in cmap.iter_nodes() {
        let d = *depth_of.get(&id).unwrap_or(&0);
        for c in &nc.leading {
            set_comment(&mut indent, c.span.start as usize, c.span.end as usize, d);
        }
        for c in &nc.dangling {
            set_comment(
                &mut indent,
                c.span.start as usize,
                c.span.end as usize,
                d + 1,
            );
        }
    }
    for c in cmap.file_trailing() {
        set_comment(&mut indent, c.span.start as usize, c.span.end as usize, 0);
    }

    let content_starts: Vec<usize> = lines.iter().map(|l| l.content_start).collect();
    let mut content: Vec<String> = lines.into_iter().map(|l| l.content).collect();

    // `toks` is the caller's single tokenization of `source`, shared with the
    // semantic guard so the file is lexed once, not twice (the protected-line
    // scan below and the keyword transform both consume it). A single pass keeps
    // multi-line tokens (strings, includes, block comments) intact as one token
    // each.

    // Protected lines: physical lines that *begin inside* a multi-line token
    // whose interior bytes are significant — a string literal or an
    // `{include}`/preprocessor reference. The line-based reindent would rewrite
    // their leading whitespace, which lands inside the token and trips the
    // semantic guard (#95). Leave them verbatim. A line `l` begins inside token
    // `t` iff `t.start < line_starts[l] < t.end`, i.e. `l` is strictly after the
    // token's first line and no later than its last. Comments are trivia (the
    // guard ignores them), so they keep their existing delta-preserving reindent.
    //
    // Line endings are still normalized to the file's dominant ending at flush, so
    // a multi-line token whose interior newline differs from that dominant ending
    // (e.g. an interior `\r\n` in a mostly-`\n` file) trips the guard and bails —
    // fails safe (file returned unchanged), not corrupted.
    //
    // Fast path: only a token that actually spans a newline can protect a line,
    // and single-line tokens are the overwhelming majority — so gate the two
    // line-index binary searches behind a cheap newline scan of the token's
    // bytes. This keeps the whole scan ~O(source) instead of O(tokens · log lines).
    let src_bytes = source.as_bytes();
    let mut protected = vec![false; n];
    for t in toks {
        if t.kind == Kind::Eof {
            break;
        }
        if t.kind == Kind::Comment || t.end <= t.start {
            continue;
        }
        if !src_bytes[t.start..t.end].contains(&b'\n') {
            continue;
        }
        let sl = line_index(&line_starts, t.start);
        let el = line_index(&line_starts, t.end - 1);
        for slot in protected[(sl + 1).min(n)..(el + 1).min(n)].iter_mut() {
            *slot = true;
        }
    }

    // Keyword recasing/abbreviation (U5) — driven by the whole-source
    // tokenization so multi-line block comments stay a single comment token and
    // their interior is never mistaken for keywords. No-op under a preserving
    // style. Edits are applied per line, right-to-left, to keep offsets valid.
    if keywords::wants_transform(style) {
        let mut edits: Vec<Vec<(usize, usize, String)>> = vec![Vec::new(); n];
        for t in toks {
            if t.kind == Kind::Eof {
                break;
            }
            if !keywords::is_transformable(t.kind) {
                continue;
            }
            let raw = &source[t.start..t.end];
            let new_text = keywords::transform_token(raw, t.kind, style);
            if new_text == raw {
                continue;
            }
            let li = line_index(&line_starts, t.start);
            let cs = content_starts[li];
            if t.start < cs {
                continue;
            }
            let col = t.start - cs;
            let len = t.end - t.start;
            if col + len <= content[li].len() {
                edits[li].push((col, len, new_text));
            }
        }
        for (li, es) in edits.iter_mut().enumerate() {
            es.sort_by_key(|e| e.0);
            for (col, len, new_text) in es.iter().rev() {
                content[li].replace_range(*col..*col + *len, new_text);
            }
        }
    }

    // `end_with_type`: insert the (correctly-cased) type keyword after each
    // typed block's bare END. Runs after recasing so an inserted keyword is
    // consistent with the rest.
    if style.end_with_type {
        for (line, ty) in &typed_ends {
            let cased = cased_type(ty, style);
            if let Some(updated) = apply_end_type(&content[*line], &cased) {
                content[*line] = updated;
            }
        }
    }

    for (l, text) in content.into_iter().enumerate() {
        if protected[l] {
            // Emit verbatim: prepend the original leading whitespace (the reindent
            // never applies to a line inside a multi-line token) and mark the line
            // protected so blank-normalization leaves it untouched.
            let leading = &source[line_starts[l]..content_starts[l]];
            buf.push_protected(format!("{leading}{text}"));
        } else if text.is_empty() {
            buf.push_blank();
        } else {
            buf.push(indent[l], text);
        }
    }

    buf
}
