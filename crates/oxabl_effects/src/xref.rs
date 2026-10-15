//! The compiler's `COMPILE ... XREF` output: the oracle for index selection.
//!
//! [`parse`] reads the text form (section 8.1, R rows): one reference per
//! line, `<compiled file> <source file> <line> <TYPE> <details...>`, with
//! file names in double quotes when they hold spaces. The format is
//! remembered, not read from a real file on this host, so the parser is
//! lenient: it splits on white space, keeps quoted fields whole, and skips a
//! line it cannot read. [`searches`] keeps the `SEARCH` rows, and [`compare`]
//! matches them with the model's searches by file, line and table, and
//! reports every disagreement with both sides.
//!
//! The test files in `crates/oxabl_effects/tests/xref/` are **synthetic**:
//! written from this description, not produced by a compiler.

use crate::model::Access;

/// One XREF line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Row {
    /// The file that was compiled.
    pub compiled: String,
    /// The file the line is in (an include for a line from an include).
    pub source: String,
    pub line: u32,
    /// The reference type, upper case (`SEARCH`, `ACCESS`, `RUN`, ...).
    pub kind: String,
    /// The rest of the line, split on white space.
    pub details: Vec<String>,
}

/// Split a line into fields: white space separates, double quotes group.
fn fields(line: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut quoted = false;
    let mut any = false;
    for c in line.chars() {
        match c {
            '"' => {
                quoted = !quoted;
                any = true;
            }
            c if c.is_whitespace() && !quoted => {
                if any {
                    out.push(std::mem::take(&mut cur));
                    any = false;
                }
            }
            c => {
                cur.push(c);
                any = true;
            }
        }
    }
    if any {
        out.push(cur);
    }
    out
}

/// Parse XREF text. Lines that do not have the four leading fields are
/// skipped.
pub fn parse(text: &str) -> Vec<Row> {
    text.lines()
        .filter_map(|l| {
            let mut f = fields(l).into_iter();
            let compiled = f.next()?;
            let source = f.next()?;
            let line = f.next()?.parse().ok()?;
            let kind = f.next()?.to_ascii_uppercase();
            Some(Row {
                compiled,
                source,
                line,
                kind,
                details: f.collect(),
            })
        })
        .collect()
}

/// A `SEARCH` row.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct XrefSearch {
    pub source: String,
    pub line: u32,
    /// The table as written (`db.table`, or a temp-table name).
    pub table: String,
    /// The index; `None` for a `TABLE-SCAN` row without one.
    pub index: Option<String>,
    pub whole: bool,
    pub table_scan: bool,
}

/// The `SEARCH` rows of an XREF file.
pub fn searches(rows: &[Row]) -> Vec<XrefSearch> {
    rows.iter()
        .filter(|r| r.kind == "SEARCH" && !r.details.is_empty())
        .map(|r| {
            let flag = |w: &str| r.details.iter().skip(1).any(|d| d.eq_ignore_ascii_case(w));
            let index = r
                .details
                .get(1)
                .filter(|d| {
                    !["WHOLE-INDEX", "TABLE-SCAN", "TEMPTABLE"]
                        .iter()
                        .any(|w| d.eq_ignore_ascii_case(w))
                })
                .cloned();
            XrefSearch {
                source: r.source.clone(),
                line: r.line,
                table: r.details[0].clone(),
                index,
                whole: flag("WHOLE-INDEX"),
                table_scan: flag("TABLE-SCAN"),
            }
        })
        .collect()
}

/// One search of the model, placed in a file and line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModelSearch {
    pub file: String,
    pub line: u32,
    pub table: String,
    pub access: Access,
}

/// A disagreement, with both sides.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Disagreement {
    pub file: String,
    pub line: u32,
    pub table: String,
    /// The model's searches at this place (empty: the model has none).
    pub model: Vec<String>,
    /// The XREF searches at this place (empty: XREF has none).
    pub xref: Vec<String>,
    /// Whether the two sides at least agree on `WHOLE-INDEX`.
    pub whole_agrees: bool,
}

/// The counts of a comparison.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Comparison {
    /// Places (file, line, table) with a search on either side.
    pub places: usize,
    /// Places with a search on both sides.
    pub matched: usize,
    /// Matched places where the model answers Unknown: not compared.
    pub unknown: usize,
    /// Matched places where every search agrees on the index and
    /// `WHOLE-INDEX`.
    pub agree: usize,
    /// Matched, known places where the two sides agree on `WHOLE-INDEX` for
    /// every search (the DB002 gate).
    pub whole_agree: usize,
    /// Of the matched, known places: XREF says `WHOLE-INDEX` and the model
    /// does not.
    pub whole_missed: usize,
    /// Of the matched, known places: the model says `WHOLE-INDEX` and XREF
    /// does not.
    pub whole_extra: usize,
    /// Places only the model has (a `ROWID` search counted apart).
    pub model_only: usize,
    /// `ROWID` searches of the model with no XREF row.
    pub rowid: usize,
    /// Places only XREF has.
    pub xref_only: usize,
    pub disagreements: Vec<Disagreement>,
}

impl Comparison {
    /// Add another comparison's counts and disagreements.
    pub fn add(&mut self, o: &Comparison) {
        self.places += o.places;
        self.matched += o.matched;
        self.unknown += o.unknown;
        self.agree += o.agree;
        self.whole_agree += o.whole_agree;
        self.whole_missed += o.whole_missed;
        self.whole_extra += o.whole_extra;
        self.model_only += o.model_only;
        self.rowid += o.rowid;
        self.xref_only += o.xref_only;
        self.disagreements.extend(o.disagreements.iter().cloned());
    }
}

/// Whether two paths name the same file: equal after normalising slashes and
/// case, or one ends with the other at a `/`.
pub fn same_file(a: &str, b: &str) -> bool {
    let norm = |s: &str| {
        let s = s.replace('\\', "/").to_ascii_lowercase();
        s.trim_start_matches("./").to_string()
    };
    let (a, b) = (norm(a), norm(b));
    if a == b {
        return true;
    }
    let (long, short) = if a.len() > b.len() {
        (&a, &b)
    } else {
        (&b, &a)
    };
    long.ends_with(short.as_str()) && long.as_bytes()[long.len() - short.len() - 1] == b'/'
}

fn last(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
}

fn render_xref(x: &XrefSearch) -> String {
    match (&x.index, x.table_scan, x.whole) {
        (_, true, _) => "table-scan".to_string(),
        (Some(i), _, true) => format!("{i} whole-index"),
        (Some(i), _, false) => i.clone(),
        (None, _, _) => "?".to_string(),
    }
}

fn render_model(a: &Access) -> String {
    match a {
        Access::Index(c) if c.whole => format!("{} whole-index", c.name),
        Access::Index(c) => c.name.clone(),
        Access::TableScan => "table-scan".to_string(),
        Access::Rowid => "rowid".to_string(),
        Access::Unknown(u) => format!("unknown({})", u.as_str()),
    }
}

fn whole_of_model(a: &Access) -> bool {
    matches!(a, Access::Index(c) if c.whole) || matches!(a, Access::TableScan)
}

fn whole_of_xref(x: &XrefSearch) -> bool {
    x.whole || x.table_scan
}

/// Compare the model's searches with the XREF `SEARCH` rows, place by place
/// (file, line, table). The XREF rows of every file the model does not name
/// are ignored.
pub fn compare(model: &[ModelSearch], xref: &[XrefSearch]) -> Comparison {
    let mut c = Comparison::default();
    let mut used = vec![false; xref.len()];
    let mut done = vec![false; model.len()];
    for i in 0..model.len() {
        if done[i] {
            continue;
        }
        let m = &model[i];
        let key = |o: &ModelSearch| {
            o.line == m.line
                && o.file == m.file
                && last(&o.table).eq_ignore_ascii_case(last(&m.table))
        };
        let ms: Vec<&ModelSearch> = model
            .iter()
            .enumerate()
            .filter(|(j, o)| !done[*j] && key(o))
            .map(|(_, o)| o)
            .collect();
        for (j, o) in model.iter().enumerate() {
            if !done[j] && key(o) {
                done[j] = true;
            }
        }
        let xs: Vec<usize> = xref
            .iter()
            .enumerate()
            .filter(|(j, x)| {
                !used[*j]
                    && x.line == m.line
                    && last(&x.table).eq_ignore_ascii_case(last(&m.table))
                    && same_file(&x.source, &m.file)
            })
            .map(|(j, _)| j)
            .collect();
        for &j in &xs {
            used[j] = true;
        }
        c.places += 1;
        let xs: Vec<&XrefSearch> = xs.iter().map(|&j| &xref[j]).collect();
        let disagreement = |whole_agrees: bool| Disagreement {
            file: m.file.clone(),
            line: m.line,
            table: m.table.clone(),
            model: ms.iter().map(|o| render_model(&o.access)).collect(),
            xref: xs.iter().map(|x| render_xref(x)).collect(),
            whole_agrees,
        };
        if xs.is_empty() {
            if ms.iter().all(|o| o.access == Access::Rowid) {
                c.rowid += 1;
            } else if ms.iter().any(|o| matches!(o.access, Access::Unknown(_))) {
                c.unknown += 1;
            } else {
                c.model_only += 1;
                c.disagreements.push(disagreement(false));
            }
            continue;
        }
        c.matched += 1;
        if ms.iter().any(|o| matches!(o.access, Access::Unknown(_))) {
            c.unknown += 1;
            continue;
        }
        let mut model_side: Vec<String> = ms.iter().map(|o| render_model(&o.access)).collect();
        let mut xref_side: Vec<String> = xs.iter().map(|x| render_xref(x)).collect();
        for s in model_side.iter_mut().chain(xref_side.iter_mut()) {
            *s = s.to_ascii_lowercase();
        }
        model_side.sort();
        xref_side.sort();
        let mut mw: Vec<bool> = ms.iter().map(|o| whole_of_model(&o.access)).collect();
        let mut xw: Vec<bool> = xs.iter().map(|x| whole_of_xref(x)).collect();
        mw.sort();
        xw.sort();
        let whole_agrees = mw == xw;
        if whole_agrees {
            c.whole_agree += 1;
        } else if xw.iter().any(|&w| w) && !mw.iter().any(|&w| w) {
            c.whole_missed += 1;
        } else {
            c.whole_extra += 1;
        }
        if model_side == xref_side {
            c.agree += 1;
        } else {
            c.disagreements.push(disagreement(whole_agrees));
        }
    }
    // XREF rows of the model's files that the model has no search for.
    let files: Vec<&str> = {
        let mut f: Vec<&str> = model.iter().map(|m| m.file.as_str()).collect();
        f.sort_unstable();
        f.dedup();
        f
    };
    let mut extra: Vec<&XrefSearch> = xref
        .iter()
        .enumerate()
        .filter(|(j, x)| !used[*j] && files.iter().any(|f| same_file(&x.source, f)))
        .map(|(_, x)| x)
        .collect();
    extra.sort_by(|a, b| (&a.source, a.line, &a.table).cmp(&(&b.source, b.line, &b.table)));
    let mut k = 0;
    while k < extra.len() {
        let x = extra[k];
        let group: Vec<&XrefSearch> = extra[k..]
            .iter()
            .take_while(|y| y.source == x.source && y.line == x.line && y.table == x.table)
            .copied()
            .collect();
        k += group.len();
        c.places += 1;
        c.xref_only += 1;
        c.disagreements.push(Disagreement {
            file: x.source.clone(),
            line: x.line,
            table: x.table.clone(),
            model: Vec::new(),
            xref: group.iter().map(|y| render_xref(y)).collect(),
            whole_agrees: false,
        });
    }
    c
}
