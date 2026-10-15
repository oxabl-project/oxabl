//! The block table of a `COMPILE ... LISTING` file, and its comparison with
//! the model.
//!
//! The format is remembered, not read from a real listing on this host
//! (`docs/design/strict-analysis-open-questions.md`, section 7.5), so the
//! parser is lenient: it finds the table by its header, reads each row from
//! its words rather than fixed columns, skips page headers and repeated
//! headers, and takes the buffers under a row from `Buffers:` and the lines
//! that continue it, comma- or line-separated.

use std::collections::BTreeMap;

/// One row of the block table.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ListingBlock {
    /// The file the block statement is written in, as the listing shows it.
    pub file: String,
    /// Its line in that file; `0` for the main procedure block.
    pub line: u32,
    /// The block type as written (`Procedure`, `For`, `Do`, ...).
    pub kind: String,
    /// The `Tran` column.
    pub transaction: bool,
    /// The `Blk. Label` column, trimmed.
    pub label: String,
    /// The buffers scoped to the block, as written (`sports.Customer`).
    pub buffers: Vec<String>,
    /// The frames scoped to the block, as written.
    pub frames: Vec<String>,
}

/// The block table of one listing.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Listing {
    pub blocks: Vec<ListingBlock>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum List {
    Buffers,
    Frames,
}

/// Parse a listing. Text before the block table (the numbered source) is
/// ignored; a listing with no block table gives no blocks.
pub fn parse(text: &str) -> Listing {
    let mut out = Listing::default();
    let mut in_table = false;
    let mut list: Option<List> = None;
    for raw in text.lines() {
        let line = raw.trim_start_matches('\u{c}');
        if is_header(line) {
            in_table = true;
            list = None;
            continue;
        }
        if !in_table {
            continue;
        }
        let trimmed = line.trim();
        if trimmed.is_empty() || is_rule(trimmed) || is_page_header(trimmed) {
            list = None;
            continue;
        }
        let indented = line.starts_with(char::is_whitespace);
        if indented {
            let (kind, rest) = if let Some(rest) = strip_label(trimmed, "Buffers:") {
                (Some(List::Buffers), rest)
            } else if let Some(rest) = strip_label(trimmed, "Frames:") {
                (Some(List::Frames), rest)
            } else {
                (list, trimmed)
            };
            if let (Some(kind), Some(block)) = (kind, out.blocks.last_mut()) {
                let names = rest
                    .split(|c: char| c == ',' || c.is_whitespace())
                    .filter(|n| !n.is_empty())
                    .map(str::to_string);
                match kind {
                    List::Buffers => block.buffers.extend(names),
                    List::Frames => block.frames.extend(names),
                }
                list = Some(kind);
                continue;
            }
        }
        list = None;
        if let Some(block) = parse_row(trimmed) {
            out.blocks.push(block);
        }
    }
    out
}

fn is_header(line: &str) -> bool {
    line.contains("File Name") && line.contains("Line") && line.contains("Tran")
}

fn is_rule(line: &str) -> bool {
    line.chars().all(|c| c == '-' || c.is_whitespace())
}

fn is_page_header(line: &str) -> bool {
    line.contains("PROGRESS(R)") || line.contains("Page ") && line.contains(':')
}

fn strip_label<'a>(line: &'a str, label: &str) -> Option<&'a str> {
    line.get(..label.len())
        .filter(|p| p.eq_ignore_ascii_case(label))
        .map(|_| line[label.len()..].trim())
}

/// `file line type... Yes|No label...`. The file name may hold spaces, so
/// the line is the first all-digit word after the first.
fn parse_row(line: &str) -> Option<ListingBlock> {
    let words: Vec<&str> = line.split_whitespace().collect();
    let at = (1..words.len()).find(|&i| words[i].bytes().all(|b| b.is_ascii_digit()))?;
    let tran = (at + 2..words.len())
        .find(|&i| words[i].eq_ignore_ascii_case("yes") || words[i].eq_ignore_ascii_case("no"))?;
    Some(ListingBlock {
        file: words[..at].join(" "),
        line: words[at].parse().ok()?,
        kind: words[at + 1..tran].join(" "),
        transaction: words[tran].eq_ignore_ascii_case("yes"),
        label: words[tran + 1..].join(" "),
        buffers: Vec::new(),
        frames: Vec::new(),
    })
}

/// One block as the model sees it, placed as the listing would place it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModelBlock {
    /// The file the block statement is written in.
    pub file: String,
    /// Its line in that file; `0` for the main block.
    pub line: u32,
    /// The listing's name for the block type ([`listing_kind`]).
    pub kind: &'static str,
    /// Whether it is a transaction block; `None` when Unknown.
    pub transaction: Option<bool>,
    /// Whether it is a transaction block and no enclosing block of its unit
    /// is one; `None` when Unknown.
    pub outermost: Option<bool>,
    /// The buffers scoped to it (last name segment, lowercase); `None` when
    /// a buffer the block can see is Unknown.
    pub buffers: Option<Vec<String>>,
}

/// The listing's name for a block kind (section 7.5, X3).
pub fn listing_kind(kind: oxabl_flow::BlockKind) -> &'static str {
    use oxabl_flow::{BlockKind, UnitKind};
    match kind {
        BlockKind::Routine(UnitKind::Main | UnitKind::DbTrigger | UnitKind::Procedure) => {
            "Procedure"
        }
        BlockKind::Routine(UnitKind::Function) => "Function",
        BlockKind::Routine(UnitKind::Method) => "Method",
        BlockKind::Routine(UnitKind::Constructor) => "Constructor",
        BlockKind::Routine(UnitKind::Destructor) => "Destructor",
        BlockKind::Routine(UnitKind::PropertyGet | UnitKind::PropertySet) => "Property",
        BlockKind::Routine(UnitKind::Trigger) => "Trigger",
        BlockKind::Do => "Do",
        BlockKind::Repeat => "Repeat",
        BlockKind::For(_) => "For",
        BlockKind::Catch => "Catch",
        BlockKind::Finally => "Finally",
    }
}

/// A difference between the listing and the model.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Disagreement {
    /// A listing row with no model block at that file and line.
    ListingOnly(ListingBlock),
    /// A model block with no listing row.
    ModelOnly(ModelBlock),
    /// Both have the block, with another type.
    Kind(ListingBlock, ModelBlock),
    /// The `Tran` column differs from the model's transaction block.
    Transaction(ListingBlock, ModelBlock),
    /// The buffers scoped to the block differ.
    Buffers(ListingBlock, ModelBlock),
}

/// The agreement of one listing with the model.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Comparison {
    pub listing_blocks: usize,
    pub model_blocks: usize,
    pub matched: usize,
    pub kind_agree: usize,
    pub tran_agree: usize,
    /// Agreement under the other reading of `Tran` (outermost only).
    pub tran_outermost_agree: usize,
    pub tran_unknown: usize,
    pub buffers_agree: usize,
    pub buffers_unknown: usize,
    pub disagreements: Vec<Disagreement>,
}

impl Comparison {
    /// Add another comparison's counts (not its disagreements).
    pub fn add(&mut self, other: &Comparison) {
        self.listing_blocks += other.listing_blocks;
        self.model_blocks += other.model_blocks;
        self.matched += other.matched;
        self.kind_agree += other.kind_agree;
        self.tran_agree += other.tran_agree;
        self.tran_outermost_agree += other.tran_outermost_agree;
        self.tran_unknown += other.tran_unknown;
        self.buffers_agree += other.buffers_agree;
        self.buffers_unknown += other.buffers_unknown;
    }
}

/// The last path component, lowercase, with `\` and `/` both separators.
fn file_key(file: &str) -> String {
    file.rsplit(['/', '\\'])
        .next()
        .unwrap_or(file)
        .to_ascii_lowercase()
}

/// Whether two paths name the same file: the components of the shorter one
/// (without `.`) end the longer one, case-insensitively. The listing writes
/// an include as the compiler found it, the model as it resolved it.
fn same_file(a: &str, b: &str) -> bool {
    let parts = |p: &'_ str| -> Vec<String> {
        p.split(['/', '\\'])
            .filter(|c| !c.is_empty() && *c != ".")
            .map(str::to_ascii_lowercase)
            .collect()
    };
    let (a, b) = (parts(a), parts(b));
    let (short, long) = if a.len() <= b.len() { (a, b) } else { (b, a) };
    long.ends_with(&short)
}

/// The last dotted segment of a buffer name, lowercase.
pub fn buffer_key(name: &str) -> String {
    name.rsplit('.').next().unwrap_or(name).to_ascii_lowercase()
}

/// Compare a listing with the model's blocks of the same compile. Blocks
/// pair by file and line, in order within one line; a file pairs by the end
/// of its path.
pub fn compare(listing: &Listing, model: &[ModelBlock]) -> Comparison {
    let mut c = Comparison {
        listing_blocks: listing.blocks.len(),
        model_blocks: model.len(),
        ..Comparison::default()
    };
    let mut by_place: BTreeMap<(String, u32), Vec<&ModelBlock>> = BTreeMap::new();
    for m in model {
        by_place
            .entry((file_key(&m.file), m.line))
            .or_default()
            .push(m);
    }
    for l in &listing.blocks {
        let key = (file_key(&l.file), l.line);
        let slot = by_place.get_mut(&key);
        let Some(m) = slot.and_then(|v| {
            // Prefer a block of the same type on that line.
            let file = |m: &&ModelBlock| same_file(&m.file, &l.file);
            let i = v
                .iter()
                .position(|m| file(m) && m.kind.eq_ignore_ascii_case(&l.kind))
                .or_else(|| v.iter().position(file))?;
            Some(v.remove(i))
        }) else {
            c.disagreements.push(Disagreement::ListingOnly(l.clone()));
            continue;
        };
        c.matched += 1;
        if m.kind.eq_ignore_ascii_case(&l.kind) {
            c.kind_agree += 1;
        } else {
            c.disagreements
                .push(Disagreement::Kind(l.clone(), m.clone()));
        }
        match m.transaction {
            None => c.tran_unknown += 1,
            Some(t) => {
                if t == l.transaction {
                    c.tran_agree += 1;
                } else {
                    c.disagreements
                        .push(Disagreement::Transaction(l.clone(), m.clone()));
                }
                if m.outermost == Some(l.transaction) {
                    c.tran_outermost_agree += 1;
                }
            }
        }
        match &m.buffers {
            None => c.buffers_unknown += 1,
            Some(bs) => {
                let mut want: Vec<String> = l.buffers.iter().map(|b| buffer_key(b)).collect();
                let mut have = bs.clone();
                want.sort();
                want.dedup();
                have.sort();
                have.dedup();
                if want == have {
                    c.buffers_agree += 1;
                } else {
                    c.disagreements
                        .push(Disagreement::Buffers(l.clone(), m.clone()));
                }
            }
        }
    }
    for rest in by_place.into_values() {
        for m in rest {
            c.disagreements.push(Disagreement::ModelOnly(m.clone()));
        }
    }
    c
}

/// The model's blocks of one file, placed by `place`, which gives the file
/// and line of a span of the analysed text (the root file and line `0` for
/// the main block is the caller's choice too).
pub fn model_blocks(
    scopes: &crate::FileScopes,
    mut place: impl FnMut(oxabl_ast::Span) -> Option<(String, u32)>,
    root: &str,
) -> Vec<ModelBlock> {
    use crate::TxKind;
    // A unit's buffers are known when every buffer it can see is.
    let unit_known: Vec<bool> = scopes
        .units()
        .iter()
        .map(|u| {
            let chain: Vec<oxabl_flow::UnitId> =
                std::iter::successors(Some(u.unit), |x| scopes.unit(*x).parent).collect();
            scopes
                .buffers()
                .iter()
                .all(|b| b.is_external() || b.unknown.is_empty() || !chain.contains(&b.owner))
                && scopes.unknown().is_empty()
        })
        .collect();
    let mut out = Vec::new();
    for u in scopes.units() {
        for b in &u.blocks {
            let (file, line) = if u.parent.is_none() && b.parent.is_none() {
                (root.to_string(), 0)
            } else {
                match place(b.span) {
                    Some(p) => p,
                    None => continue,
                }
            };
            let transaction = match b.transaction.kind {
                TxKind::Unknown(_) => None,
                k => Some(k.is_transaction()),
            };
            let outermost = transaction.map(|t| t && b.transaction.within.is_none());
            let buffers = unit_known[u.unit.index()].then(|| {
                b.buffers
                    .iter()
                    .map(|&id| buffer_key(&scopes.buffer(id).name))
                    .collect()
            });
            out.push(ModelBlock {
                file,
                line,
                kind: listing_kind(b.kind),
                transaction,
                outermost,
                buffers,
            });
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_row_with_a_spaced_file_name_and_a_label() {
        let row = parse_row("C:\\My Work\\a.p   12 For         Yes  outer").unwrap();
        assert_eq!(row.file, "C:\\My Work\\a.p");
        assert_eq!(row.line, 12);
        assert_eq!(row.kind, "For");
        assert!(row.transaction);
        assert_eq!(row.label, "outer");
    }

    #[test]
    fn a_two_word_type_still_finds_the_tran_column() {
        let row = parse_row("a.p 3 Internal Procedure No Procedure x").unwrap();
        assert_eq!(row.kind, "Internal Procedure");
        assert!(!row.transaction);
        assert_eq!(row.label, "Procedure x");
    }

    #[test]
    fn keys_ignore_directories_case_and_database() {
        assert_eq!(file_key(".\\src\\A.P"), "a.p");
        assert_eq!(file_key("src/inc/x.i"), "x.i");
        assert_eq!(buffer_key("sports2000.Customer"), "customer");
    }

    #[test]
    fn a_file_pairs_by_the_end_of_its_path() {
        assert!(same_file("/proj/inc/X.i", "inc\\x.i"));
        assert!(same_file("x.i", "/proj/ms/x.i"));
        assert!(same_file(".\\a.p", "a.p"));
        assert!(!same_file("/proj/ms/x.i", "inc/x.i"));
    }

    #[test]
    fn includes_with_one_name_in_two_directories_pair_by_directory() {
        let block = |file: &str, transaction| ModelBlock {
            file: file.to_string(),
            line: 3,
            kind: "Do",
            transaction: Some(transaction),
            outermost: Some(transaction),
            buffers: Some(Vec::new()),
        };
        let model = [block("/proj/ms/x.i", false), block("/proj/inc/x.i", true)];
        let listing = parse(
            "File Name   Line Blk. Type   Tran   Blk. Label\n\
             ----------- ---- ---------- ------ ---------------\n\
             inc/x.i        3 Do         Yes\n\
             ms/x.i         3 Do         No\n",
        );
        let c = compare(&listing, &model);
        assert_eq!(c.matched, 2);
        assert_eq!(c.tran_agree, 2, "{:?}", c.disagreements);
    }
}
