//! Version 1 of the independent `explain` document. Analyze is unchanged.

use std::fmt::Write;

use oxabl_effects::{Access, Shape, summary::SummaryLock};
use oxabl_scope::{Lock, Table};
use serde::Serialize;

use crate::EffectFile;

#[derive(Serialize)]
struct Document<'a> {
    version: u32,
    file: &'a str,
    coordinates: &'static str,
    units: Vec<Unit>,
}

#[derive(Serialize)]
struct Unit {
    kind: String,
    name: Option<String>,
    line: usize,
    effects: Vec<Effect>,
    summary: Transitive,
}

#[derive(Serialize)]
struct Effect {
    table: String,
    operation: String,
    lock: String,
    shape: String,
    line: Option<usize>,
    index: Option<String>,
    reason: Option<String>,
    via: Option<String>,
}

#[derive(Serialize)]
struct Transitive {
    complete: bool,
    partial: Vec<String>,
    recursive: bool,
    effects: Vec<Effect>,
}

fn table(t: &Table) -> String {
    match t {
        Table::Database(n) => n.clone(),
        Table::TempTable { name, .. } => format!("tt:{name}"),
        Table::Unknown(n) => format!("?:{n}"),
    }
}

fn lock(l: Option<&Lock>) -> String {
    match l {
        None => "n/a".into(),
        Some(Lock::Explicit(m)) => m.as_str().into(),
        Some(Lock::Default) => "share-lock(default)".into(),
        Some(Lock::TempTable) => "n/a".into(),
        Some(Lock::Unknown(u)) => format!("unknown({})", u.as_str()),
    }
}

impl EffectFile {
    /// Render each unit, or the innermost unit containing a root byte offset.
    /// Lines in the result refer to expanded text. `json` uses document version 1.
    pub fn explain(
        &self,
        file: &str,
        root_offset: Option<u32>,
        json: bool,
    ) -> Result<String, serde_json::Error> {
        let selected = root_offset.and_then(|at| {
            self.root_units
                .iter()
                .enumerate()
                .filter_map(|(i, span)| {
                    span.filter(|s| s.start <= at && at < s.end)
                        .map(|s| (i, s.end - s.start))
                })
                .min_by_key(|(_, len)| *len)
                .map(|(i, _)| i)
        });
        let line = |at| self.lines.partition_point(|&start| start <= at);
        let units = self
            .effects
            .units()
            .iter()
            .enumerate()
            .filter(|(i, _)| root_offset.is_none() || Some(*i) == selected)
            .map(|(i, u)| {
                let effects = self
                    .effects
                    .unit_effects(u.unit)
                    .map(|e| {
                        let (index, reason) = match &e.access {
                            Some(Access::Index(c)) => (
                                Some(c.name.clone()),
                                Some(format!(
                                    "{}; tie={}; whole={}; equality={}; range={}",
                                    c.reason.as_str(),
                                    c.tie.as_str(),
                                    c.whole,
                                    c.equality,
                                    c.range
                                )),
                            ),
                            Some(Access::Unknown(u)) => {
                                (None, Some(format!("unknown:{}", u.as_str())))
                            }
                            Some(Access::Rowid) => (None, Some("rowid".into())),
                            Some(Access::TableScan) => (None, Some("table-scan".into())),
                            None => (None, None),
                        };
                        Effect {
                            table: table(&e.table),
                            operation: e.op.as_str().into(),
                            lock: lock(e.lock.as_ref()),
                            shape: e.shape.render(),
                            line: Some(line(e.at.start)),
                            index,
                            reason,
                            via: None,
                        }
                    })
                    .collect();
                let summary = &self.summaries()[i];
                let inherited = summary
                    .effects
                    .iter()
                    .map(|e| Effect {
                        table: e.table.clone(),
                        operation: e.op.as_str().into(),
                        lock: match e.lock {
                            SummaryLock::None => "n/a".into(),
                            SummaryLock::Mode(m) => m.as_str().into(),
                            SummaryLock::Default => "share-lock(default)".into(),
                            SummaryLock::Unknown => "unknown".into(),
                        },
                        shape: Shape::from_depth(e.depth).render(),
                        line: None,
                        index: None,
                        reason: None,
                        via: e.via.and_then(|n| self.callee_name(n)).map(str::to_owned),
                    })
                    .collect();
                Unit {
                    kind: u.kind.as_str().into(),
                    name: u.name.clone(),
                    line: line(u.span.start),
                    effects,
                    summary: Transitive {
                        complete: summary.partial.is_empty(),
                        partial: summary.partial.iter().map(|p| p.as_str()).collect(),
                        recursive: summary.recursive,
                        effects: inherited,
                    },
                }
            })
            .collect();
        let doc = Document {
            version: 1,
            file,
            coordinates: "expanded-lines",
            units,
        };
        if json {
            return serde_json::to_string_pretty(&doc).map(|mut s| {
                s.push('\n');
                s
            });
        }
        let mut out = format!("{} (expanded lines)\n", doc.file);
        for u in doc.units {
            let _ = writeln!(
                out,
                "unit {} {} @{}",
                u.kind,
                u.name.as_deref().unwrap_or("<main>"),
                u.line
            );
            if u.effects.is_empty() {
                out.push_str("  direct: none\n");
            }
            for e in u.effects {
                render_effect(&mut out, "direct", &e);
            }
            let _ = writeln!(
                out,
                "  summary: {}{}",
                if u.summary.complete {
                    "complete"
                } else {
                    "partial"
                },
                if u.summary.recursive {
                    ", recursive"
                } else {
                    ""
                }
            );
            for p in u.summary.partial {
                let _ = writeln!(out, "    unknown: {p}");
            }
            for e in u.summary.effects {
                render_effect(&mut out, "transitive", &e);
            }
        }
        Ok(out)
    }
}

fn render_effect(out: &mut String, scope: &str, e: &Effect) {
    let _ = write!(
        out,
        "  {scope}: {} {} {} {}",
        e.operation, e.table, e.lock, e.shape
    );
    if let Some(line) = e.line {
        let _ = write!(out, " @{line}");
    }
    if let Some(index) = &e.index {
        let _ = write!(out, " index={index}");
    }
    if let Some(reason) = &e.reason {
        let _ = write!(out, " ({reason})");
    }
    if let Some(via) = &e.via {
        let _ = write!(out, " via {via}");
    }
    out.push('\n');
}
