//! A stable text rendering of a [`FileScopes`], for snapshot tests.

use std::fmt::Write;

use oxabl_ast::Span;

use crate::FileScopes;
use crate::model::*;

pub(crate) fn dump(fs: &FileScopes, source: &str) -> String {
    let lines = LineIndex::new(source);
    let mut out = String::new();
    for u in &fs.unknown {
        let _ = writeln!(out, "file unknown {}", unknown(u, &lines));
    }
    for u in fs.units() {
        let _ = write!(out, "unit u{} {}", u.unit.index(), u.kind.as_str());
        if let Some(name) = &u.name {
            let _ = write!(out, " {name}");
        }
        out.push('\n');
        for b in &u.blocks {
            let _ = write!(
                out,
                "  b{} {} @{}",
                b.id.index(),
                b.kind.as_str(),
                lines.line(b.span)
            );
            if let Some(l) = &b.label {
                let _ = write!(out, " label={l}");
            }
            match b.transaction.kind {
                TxKind::None => {}
                TxKind::Explicit => out.push_str(" tx=explicit"),
                TxKind::Implicit(up) => {
                    let _ = write!(
                        out,
                        " tx=implicit({} @{}",
                        up.kind.as_str(),
                        lines.line(up.at)
                    );
                    if let Some(buf) = up.buffer {
                        let _ = write!(out, " {}", fs.buffer(buf).name);
                    }
                    out.push(')');
                }
                TxKind::Unknown(u) => {
                    let _ = write!(out, " tx=unknown({})", unknown(&u, &lines));
                }
            }
            if let Some(w) = b.transaction.within {
                let _ = write!(out, " within=b{}", w.index());
            }
            if !b.buffers.is_empty() {
                let names: Vec<&str> = b
                    .buffers
                    .iter()
                    .map(|&id| fs.buffer(id).name.as_str())
                    .collect();
                let _ = write!(out, " buffers={}", names.join(","));
            }
            out.push('\n');
        }
    }
    for b in fs.buffers() {
        let _ = write!(
            out,
            "buffer {} {} {} owner=u{}",
            b.name,
            b.kind.as_str(),
            table(&b.table),
            b.owner.index()
        );
        if let Some(d) = b.defined {
            let _ = write!(out, " @{}", lines.line(d));
        }
        let _ = write!(out, " refs={}", b.references);
        if b.is_external() {
            out.push_str(" external");
        }
        out.push('\n');
        for s in &b.seen {
            let _ = write!(
                out,
                "  scope u{}/b{} {} @{}",
                s.unit.index(),
                s.block.index(),
                s.kind.as_str(),
                lines.line(s.at)
            );
            if !s.absorbed.is_empty() {
                let at: Vec<String> = s
                    .absorbed
                    .iter()
                    .map(|a| format!("@{}", lines.line(*a)))
                    .collect();
                let _ = write!(out, " absorbed={}", at.join(","));
            }
            let _ = writeln!(out, " refs={}", s.references);
        }
        for u in &b.unknown {
            let _ = writeln!(out, "  unknown {}", unknown(u, &lines));
        }
    }
    for r in fs.reads() {
        let _ = write!(
            out,
            "read {} {} @{} u{}/b{} lock=",
            r.kind.as_str(),
            r.name,
            lines.line(r.at),
            r.unit.index(),
            r.block.index()
        );
        match r.lock {
            Lock::Explicit(m) => out.push_str(m.as_str()),
            Lock::Default => out.push_str("default(share-lock)"),
            Lock::TempTable => out.push_str("n/a"),
            Lock::Unknown(reason) => {
                let _ = write!(out, "unknown({})", reason.as_str());
            }
        }
        if r.buffer.is_none() && r.kind != ReadKind::CanFind {
            out.push_str(" unresolved");
        }
        match r.written {
            Written::No => {}
            Written::Yes => out.push_str(" written"),
            Written::Unknown(reason) => {
                let _ = write!(out, " written=unknown({})", reason.as_str());
            }
        }
        out.push('\n');
    }
    out
}

fn unknown(u: &Unknown, lines: &LineIndex) -> String {
    if u.at == Span::DUMMY {
        u.reason.as_str().to_string()
    } else {
        format!("{} @{}", u.reason.as_str(), lines.line(u.at))
    }
}

fn table(t: &Table) -> String {
    match t {
        Table::Database(n) => format!("db:{n}"),
        Table::TempTable { name, no_undo } => {
            format!("tt:{name}{}", if *no_undo { " no-undo" } else { "" })
        }
        Table::Unknown(n) => format!("unknown:{n}"),
    }
}

pub(crate) struct LineIndex {
    starts: Vec<u32>,
}

impl LineIndex {
    pub(crate) fn new(source: &str) -> Self {
        let mut starts = vec![0];
        starts.extend(
            source
                .bytes()
                .enumerate()
                .filter(|(_, b)| *b == b'\n')
                .map(|(i, _)| i as u32 + 1),
        );
        LineIndex { starts }
    }

    pub(crate) fn line(&self, span: Span) -> u32 {
        self.starts.partition_point(|&s| s <= span.start) as u32
    }
}
