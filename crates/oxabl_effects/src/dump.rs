//! A stable text rendering of [`FileEffects`], for snapshots.

use std::fmt::Write;

use oxabl_index::calls::{CallKind, CallTarget};
use oxabl_scope::{Lock, Table};

use crate::FileEffects;
use crate::model::{Access, Effect};

fn line_of(source: &str, at: u32) -> usize {
    let at = (at as usize).min(source.len());
    source.as_bytes()[..at]
        .iter()
        .filter(|&&b| b == b'\n')
        .count()
        + 1
}

pub(crate) fn table(t: &Table) -> String {
    match t {
        Table::Database(n) => n.clone(),
        Table::TempTable { name, .. } => format!("tt:{name}"),
        Table::Unknown(n) => format!("?:{n}"),
    }
}

pub(crate) fn lock(l: &Lock) -> String {
    match l {
        Lock::Explicit(m) => m.as_str().to_string(),
        Lock::Default => "share-lock(default)".to_string(),
        Lock::TempTable => "n/a".to_string(),
        Lock::Unknown(r) => format!("unknown({})", r.as_str()),
    }
}

/// The access of a search, for a dump or a report.
pub fn access(a: &Access) -> String {
    match a {
        Access::Index(c) => {
            let mut s = format!("index {}", c.name);
            if c.whole {
                s.push_str(" whole-index");
            } else {
                let _ = write!(s, " eq={}", c.equality);
                if c.range {
                    s.push_str(" range");
                }
            }
            let _ = write!(s, " ({}", c.reason.as_str());
            if c.tie != crate::TieBreak::None {
                let _ = write!(s, ", tie {}", c.tie.as_str());
            }
            s.push(')');
            s
        }
        Access::TableScan => "table-scan".to_string(),
        Access::Rowid => "rowid".to_string(),
        Access::Unknown(u) => format!("index unknown({})", u.as_str()),
    }
}

fn effect(out: &mut String, source: &str, e: &Effect) {
    let _ = write!(
        out,
        "  @{} {} {}",
        line_of(source, e.at.start),
        e.op.as_str(),
        table(&e.table)
    );
    if let Some(r) = e.read {
        let _ = write!(out, " {}", r.as_str());
    }
    if let Some(l) = &e.lock {
        let _ = write!(out, " {}", lock(l));
    }
    let _ = write!(out, " {}", e.shape.render());
    if let Some(a) = &e.access {
        let _ = write!(out, " {}", access(a));
    }
    out.push('\n');
}

pub(crate) fn target(t: &CallTarget) -> String {
    match t {
        CallTarget::Procedure(n) => format!("procedure {n}"),
        CallTarget::Function(n) => format!("function {n}"),
        CallTarget::Method(n) => format!("method {n}"),
        CallTarget::Program(n) => format!("program {n}"),
        CallTarget::InProgram { program, procedure } => format!("{procedure} in {program}"),
        CallTarget::Library(n) => format!("library {n}"),
        CallTarget::Unknown { name, reason } => match name {
            Some(n) => format!("unknown({}) {n}", reason.as_str()),
            None => format!("unknown({})", reason.as_str()),
        },
    }
}

fn kind(k: CallKind) -> &'static str {
    match k {
        CallKind::Run => "run",
        CallKind::Persistent => "run-persistent",
        CallKind::Remote => "run-remote",
        CallKind::Call => "call",
        CallKind::Publish => "publish",
    }
}

pub(crate) fn dump(fe: &FileEffects, source: &str) -> String {
    let mut out = String::new();
    for u in fe.units() {
        let _ = write!(out, "unit {}", u.kind.as_str());
        if let Some(n) = &u.name {
            let _ = write!(out, " {n}");
        }
        out.push('\n');
        for e in fe.unit_effects(u.unit) {
            effect(&mut out, source, e);
        }
        for c in &u.calls {
            let _ = writeln!(
                out,
                "  call @{} {} {} {}",
                line_of(source, c.at.start),
                kind(c.kind),
                target(&c.target),
                c.shape.render()
            );
        }
        for (r, at) in &u.unknown {
            let _ = writeln!(
                out,
                "  unknown {} @{}",
                r.as_str(),
                line_of(source, at.start)
            );
        }
    }
    out
}
