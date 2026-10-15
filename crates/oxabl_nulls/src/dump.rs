//! A stable text rendering of [`FileNulls`], for snapshots.

use std::fmt::Write;

use oxabl_ast::Span;
use oxabl_scope::FileScopes;

use crate::FileNulls;

fn line_of(source: &str, at: u32) -> usize {
    let at = (at as usize).min(source.len());
    source.as_bytes()[..at]
        .iter()
        .filter(|&&b| b == b'\n')
        .count()
        + 1
}

/// The text at `span` on one line.
fn text(source: &str, span: Span) -> String {
    let t = source
        .get(span.start as usize..span.end as usize)
        .unwrap_or("");
    let first = t.lines().next().unwrap_or("").trim();
    if t.trim().contains('\n') {
        format!("{first} ...")
    } else {
        first.to_string()
    }
}

pub(crate) fn dump(nulls: &FileNulls, source: &str, scopes: &FileScopes) -> String {
    let mut out = String::new();
    for u in nulls.unknown() {
        let _ = writeln!(out, "unknown {}", u.reason.as_str());
    }
    for (i, f) in nulls.finds().iter().enumerate() {
        let _ = writeln!(
            out,
            "find#{i} u{} @{} {} `{}`",
            f.unit.index(),
            line_of(source, f.at.start),
            scopes.buffer(f.buffer).name,
            text(source, f.at),
        );
    }
    for m in nulls.missing() {
        let finds: Vec<String> = m.finds.iter().map(|i| format!("find#{i}")).collect();
        let _ = writeln!(
            out,
            "missing u{} @{} {} `{}` {} after {}",
            m.unit.index(),
            line_of(source, m.at.start),
            scopes.buffer(m.buffer).name,
            text(source, m.at),
            if m.write { "write" } else { "read" },
            finds.join(", "),
        );
    }
    out
}
