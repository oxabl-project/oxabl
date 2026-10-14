//! A stable text rendering of a [`FileHandles`], for snapshot tests.

use std::fmt::Write;

use oxabl_ast::Span;
use oxabl_flow::{EdgeKind, ExitKind};

use crate::FileHandles;
use crate::model::*;

pub(crate) fn dump(fh: &FileHandles, source: &str) -> String {
    let lines = LineIndex::new(source);
    let mut out = String::new();
    for u in fh.units() {
        let _ = write!(out, "unit u{} {}", u.unit.index(), u.kind.as_str());
        if let Some(name) = &u.name {
            let _ = write!(out, " {name}");
        }
        out.push('\n');
        for p in &u.pools {
            let _ = writeln!(
                out,
                "  pool @{} {}{}",
                lines.line(p.site.span),
                p.name
                    .as_deref()
                    .map_or("unnamed".to_string(), |n| format!("\"{n}\"")),
                if p.persistent { " persistent" } else { "" }
            );
        }
        for o in &u.objects {
            let _ = write!(
                out,
                "  {} @{} {}",
                o.kind.as_str(),
                lines.line(o.create.span),
                o.verdict.as_str()
            );
            if let Some(h) = &o.holder {
                let _ = write!(out, " holder={h}");
            }
            match &o.placement {
                Placement::NotPooled => {}
                p => {
                    let _ = write!(out, " pool={}", placement(p));
                }
            }
            out.push('\n');
            for f in &o.frees {
                let _ = writeln!(
                    out,
                    "    free {} @{}{}",
                    f.kind.as_str(),
                    lines.line(f.site.span),
                    if f.in_finally { " finally" } else { "" }
                );
            }
            for e in &o.escapes {
                let _ = writeln!(
                    out,
                    "    escape {} @{}{}",
                    e.kind.as_str(),
                    lines.line(e.site.span),
                    if e.freed_elsewhere {
                        " freed-elsewhere"
                    } else {
                        ""
                    }
                );
            }
            for x in &o.opaque {
                let _ = writeln!(
                    out,
                    "    opaque {} @{}",
                    x.reason.as_str(),
                    lines.line(x.site.span)
                );
            }
            for l in &o.leaks {
                let _ = write!(
                    out,
                    "    leak {} @{} {}",
                    leak_kind(l.kind),
                    lines.line(l.at),
                    if l.normal { "normal" } else { "abnormal" }
                );
                if let Some(v) = l.via {
                    let _ = write!(out, " via {}@{}", edge(v.edge), lines.line(v.span));
                }
                let mut path: Vec<u32> = l.path.iter().map(|s| lines.line(s.span)).collect();
                path.dedup();
                let path: Vec<String> = path.iter().map(u32::to_string).collect();
                let _ = writeln!(out, " path {}", path.join(">"));
            }
        }
    }
    out
}

fn placement(p: &Placement) -> String {
    match p {
        Placement::NotPooled => "none".into(),
        Placement::UnitPool => "unit".into(),
        Placement::FilePool => "file".into(),
        Placement::ClassPool => "class".into(),
        Placement::NamedPool { name, persistent } => {
            format!("\"{name}\"{}", if *persistent { " persistent" } else { "" })
        }
        Placement::ForeignPool { name } => format!("foreign \"{name}\""),
        Placement::Unscoped => "unscoped".into(),
    }
}

fn leak_kind(k: LeakKind) -> &'static str {
    match k {
        LeakKind::Exit(ExitKind::Return) => "exit-return",
        LeakKind::Exit(ExitKind::Error) => "exit-error",
        LeakKind::Exit(ExitKind::Stop) => "exit-stop",
        LeakKind::Exit(ExitKind::Quit) => "exit-quit",
        LeakKind::Overwrite => "overwrite",
    }
}

fn edge(k: EdgeKind) -> String {
    match k {
        EdgeKind::Raise(c) => format!("raise({})", c.as_str()),
        EdgeKind::Throw => "throw".into(),
        EdgeKind::ReturnError => "return-error".into(),
        EdgeKind::Catch => "catch".into(),
        other => format!("{other:?}").to_ascii_lowercase(),
    }
}

/// 1-based line of a byte offset.
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
