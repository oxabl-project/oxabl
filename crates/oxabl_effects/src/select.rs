//! Index selection for one search (section 8.2): the `WHERE` criteria as
//! matches on fields, then the compiler's choice among the table's indexes.

use std::ops::Range;

use oxabl_lexer::Kind;

use crate::model::{Access, IndexChoice, IndexReason, IndexUnknown, TieBreak};
use crate::phrase::{Phrase, Reader};

/// One index of a table, with folded component names.
#[derive(Debug, Clone)]
pub(crate) struct Idx {
    pub name: String,
    pub unique: bool,
    pub primary: bool,
    pub word: bool,
    pub inactive: bool,
    pub components: Vec<String>,
}

/// The fields and indexes of a table, folded.
#[derive(Debug, Clone, Default)]
pub(crate) struct TableInfo {
    pub fields: Vec<String>,
    pub indexes: Vec<Idx>,
}

impl TableInfo {
    pub fn from_schema(t: &oxabl_schema::Table) -> TableInfo {
        TableInfo {
            fields: t.fields.iter().map(|f| f.name.to_string()).collect(),
            indexes: t
                .indexes
                .iter()
                .map(|i| Idx {
                    name: i.display_name.clone(),
                    unique: i.unique,
                    primary: i.primary,
                    word: i.word,
                    inactive: i.inactive,
                    components: i.fields.iter().map(|f| f.name.to_string()).collect(),
                })
                .collect(),
        }
    }

    /// The folded field a name means: exact, else a unique prefix.
    pub fn field(&self, name: &str) -> Option<&str> {
        let folded = name.to_ascii_lowercase();
        if let Some(f) = self.fields.iter().find(|f| **f == folded) {
            return Some(f);
        }
        let mut it = self.fields.iter().filter(|f| f.starts_with(&folded));
        match (it.next(), it.next()) {
            (Some(f), None) => Some(f),
            _ => None,
        }
    }
}

/// How a criterion can bracket a component.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Match {
    Equality,
    Range,
    Contains,
}

/// The criteria as a tree of `AND`, `OR` and leaves.
#[derive(Debug)]
enum Tree {
    And(Vec<Tree>),
    Or(Vec<Tree>),
    /// A field of the phrase's table and how it is compared.
    Leaf(String, Match),
    /// `ROWID(b) = ...` or `RECID(b) = ...`.
    Rowid,
    /// Anything else: brackets nothing.
    Other,
}

/// What the criteria refer to: the phrase's buffer and the names that are not
/// fields.
pub(crate) struct Context<'a> {
    pub reader: &'a Reader<'a>,
    /// The buffer name as written in the phrase (last segment compared).
    pub buffer: &'a str,
    pub table: &'a TableInfo,
    /// Names declared as variables or parameters (folded): they win over
    /// fields.
    pub variables: &'a rustc_hash::FxHashSet<String>,
}

fn last(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
}

impl Context<'_> {
    /// Whether a dotted name qualifies the phrase's buffer. The table name
    /// names the default buffer, which is the phrase's only when the phrase
    /// names it.
    fn own_qualifier(&self, q: &str) -> bool {
        last(q).eq_ignore_ascii_case(last(self.buffer))
    }

    /// The field of the phrase's table that the name at tokens `r` means, if
    /// it is a plain field reference.
    fn own_field(&self, r: Range<usize>) -> Option<String> {
        if r.is_empty() {
            return None;
        }
        let (end, name) = self.reader.name(r.start);
        if end != r.end {
            return None;
        }
        let kind = self.reader.toks[r.start].kind;
        if matches!(
            kind,
            Kind::StringLiteral | Kind::IntegerLiteral | Kind::DecimalLiteral | Kind::BigIntLiteral
        ) {
            return None;
        }
        match name.rsplit_once('.') {
            Some((q, f)) if self.own_qualifier(q) => self.table.field(f).map(str::to_string),
            Some(_) => None,
            None => {
                if self.variables.contains(&name.to_ascii_lowercase()) {
                    return None;
                }
                self.table.field(&name).map(str::to_string)
            }
        }
    }

    /// Whether the tokens `r` name the phrase's own record: a field of it, or
    /// the buffer.
    fn mentions_own(&self, r: Range<usize>) -> bool {
        let mut i = r.start;
        while i < r.end {
            let k = self.reader.toks[i].kind;
            let after_colon = i > 0 && self.reader.toks[i - 1].kind == Kind::Colon;
            if !after_colon
                && !matches!(
                    k,
                    Kind::StringLiteral
                        | Kind::IntegerLiteral
                        | Kind::DecimalLiteral
                        | Kind::BigIntLiteral
                )
            {
                let (end, name) = self.reader.name(i);
                let end = end.min(r.end);
                let own = match name.rsplit_once('.') {
                    Some((q, _)) => self.own_qualifier(q),
                    None => {
                        name.eq_ignore_ascii_case(last(self.buffer))
                            || k == Kind::Identifier
                                && !self.variables.contains(&name.to_ascii_lowercase())
                                && self.table.field(&name).is_some()
                    }
                };
                if own {
                    return true;
                }
                i = end.max(i + 1);
                continue;
            }
            i += 1;
        }
        false
    }

    fn parse(&self, r: Range<usize>) -> Tree {
        let parts = self.split(r.clone(), Kind::Or);
        if parts.len() > 1 {
            return Tree::Or(parts.into_iter().map(|p| self.parse(p)).collect());
        }
        let parts = self.split(r.clone(), Kind::And);
        if parts.len() > 1 {
            return Tree::And(parts.into_iter().map(|p| self.parse(p)).collect());
        }
        let toks = self.reader.toks;
        if r.is_empty() {
            return Tree::Other;
        }
        if toks[r.start].kind == Kind::Not {
            return Tree::Other;
        }
        if toks[r.start].kind == Kind::LeftParen && self.closing(r.start) == Some(r.end - 1) {
            return self.parse(r.start + 1..r.end - 1);
        }
        self.leaf(r)
    }

    /// The index of the parenthesis that closes the one at `open`.
    fn closing(&self, open: usize) -> Option<usize> {
        let mut depth = 0i32;
        for (i, t) in self.reader.toks.iter().enumerate().skip(open) {
            match t.kind {
                Kind::LeftParen | Kind::LeftBracket => depth += 1,
                Kind::RightParen | Kind::RightBracket => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(i);
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Split at depth-0 tokens of `kind`.
    fn split(&self, r: Range<usize>, kind: Kind) -> Vec<Range<usize>> {
        let mut out = Vec::new();
        let mut depth = 0i32;
        let mut from = r.start;
        let mut conditional = 0i32;
        for i in r.clone() {
            match self.reader.toks[i].kind {
                Kind::LeftParen | Kind::LeftBracket => depth += 1,
                Kind::RightParen | Kind::RightBracket => depth -= 1,
                Kind::KwIf if depth == 0 => conditional += 1,
                Kind::KwElse if depth == 0 => conditional -= 1,
                k if k == kind && depth == 0 && conditional <= 0 => {
                    out.push(from..i);
                    from = i + 1;
                }
                _ => {}
            }
        }
        out.push(from..r.end);
        out
    }

    fn leaf(&self, r: Range<usize>) -> Tree {
        let toks = self.reader.toks;
        let mut depth = 0i32;
        let mut op: Option<(usize, Kind)> = None;
        for i in r.clone() {
            let k = toks[i].kind;
            match k {
                Kind::LeftParen | Kind::LeftBracket => depth += 1,
                Kind::RightParen | Kind::RightBracket => depth -= 1,
                Kind::KwIf | Kind::Then | Kind::KwElse if depth == 0 => return Tree::Other,
                Kind::Equals
                | Kind::Eq
                | Kind::NotEqual
                | Kind::Ne
                | Kind::LessThan
                | Kind::Lt
                | Kind::LessThanOrEqual
                | Kind::Le
                | Kind::GreaterThan
                | Kind::Gt
                | Kind::GreaterThanOrEqual
                | Kind::Ge
                | Kind::Begins
                | Kind::Matches
                | Kind::Contains
                    if depth == 0 =>
                {
                    if op.is_some() {
                        return Tree::Other;
                    }
                    op = Some((i, k));
                }
                _ => {}
            }
        }
        let Some((at, k)) = op else {
            return Tree::Other;
        };
        let (lhs, rhs) = (r.start..at, at + 1..r.end);
        if matches!(k, Kind::Equals | Kind::Eq)
            && (self.rowid(lhs.clone()) || self.rowid(rhs.clone()))
        {
            return Tree::Rowid;
        }
        let m = match k {
            Kind::Equals | Kind::Eq => Some(Match::Equality),
            Kind::LessThan
            | Kind::Lt
            | Kind::LessThanOrEqual
            | Kind::Le
            | Kind::GreaterThan
            | Kind::Gt
            | Kind::GreaterThanOrEqual
            | Kind::Ge => Some(Match::Range),
            Kind::Begins => Some(Match::Range),
            Kind::Contains => Some(Match::Contains),
            _ => None,
        };
        let Some(m) = m else {
            return Tree::Other;
        };
        // BEGINS and CONTAINS bracket only with the field on the left.
        let sides: &[(Range<usize>, Range<usize>)] = match m {
            Match::Equality | Match::Range if k != Kind::Begins => {
                &[(lhs.clone(), rhs.clone()), (rhs.clone(), lhs.clone())]
            }
            _ => &[(lhs.clone(), rhs.clone())],
        };
        for (field, other) in sides {
            if let Some(f) = self.own_field(field.clone())
                && !self.mentions_own(other.clone())
            {
                return Tree::Leaf(f, m);
            }
        }
        Tree::Other
    }

    /// `ROWID(b)` or `RECID(b)` of the phrase's own buffer.
    fn rowid(&self, r: Range<usize>) -> bool {
        let toks = self.reader.toks;
        r.len() == 4
            && matches!(toks[r.start].kind, Kind::Rowid | Kind::Recid)
            && toks[r.start + 1].kind == Kind::LeftParen
            && toks[r.start + 3].kind == Kind::RightParen
            && self.own_qualifier(self.reader.text(r.start + 2))
    }
}

/// The conjuncts that hold for every record found: the top-level `AND`s.
fn conjuncts(t: &Tree) -> Vec<&Tree> {
    match t {
        Tree::And(parts) => parts.iter().flat_map(conjuncts).collect(),
        other => vec![other],
    }
}

/// Whether a branch of an `OR` brackets some index: an equality or range
/// match on a first component.
fn brackets_any(t: &Tree, table: &TableInfo) -> bool {
    conjuncts(t).iter().any(|c| match c {
        Tree::Leaf(f, Match::Equality | Match::Range) => table
            .indexes
            .iter()
            .any(|i| !i.inactive && !i.word && i.components.first() == Some(f)),
        Tree::Leaf(_, Match::Contains) | Tree::Rowid => true,
        Tree::Or(branches) => branches.iter().all(|b| brackets_any(b, table)),
        _ => false,
    })
}

/// What one index gets from the active criteria.
#[derive(Debug, Clone, Copy, Default)]
struct Score {
    equality: u8,
    range: bool,
    sort: u8,
}

fn score(idx: &Idx, eq: &[&str], range: &[&str], by: &[String]) -> Score {
    let mut s = Score::default();
    for c in &idx.components {
        if eq.contains(&c.as_str()) {
            s.equality += 1;
        } else {
            s.range = range.contains(&c.as_str());
            break;
        }
    }
    // BY fields in the order of the components, from the first.
    for (c, b) in idx.components.iter().zip(by) {
        if c == b {
            s.sort += 1;
        } else {
            break;
        }
    }
    s
}

fn choice(idx: &Idx, s: Score, reason: IndexReason, tie: TieBreak) -> IndexChoice {
    IndexChoice {
        name: idx.name.clone(),
        reason,
        tie,
        equality: s.equality,
        range: s.range,
        whole: s.equality == 0 && !s.range,
    }
}

/// The search a phrase does.
pub(crate) struct Search<'a> {
    pub phrase: &'a Phrase,
    /// A `FOR`, `PRESELECT` or `OPEN QUERY` phrase: the compiler may use
    /// several indexes.
    pub multi: bool,
    /// The `BY` fields, folded fields of the table (empty for a `BY` on
    /// something else).
    pub by: Vec<String>,
    /// Equality matches an `OF` join gives (folded fields).
    pub of_fields: Vec<String>,
}

/// The index the compiler is expected to pick, or why it is not known.
pub(crate) fn choose(ctx: &Context<'_>, search: &Search<'_>) -> Access {
    let table = ctx.table;
    let p = search.phrase;
    if p.unexpanded {
        return Access::Unknown(IndexUnknown::Unexpanded);
    }
    if p.table_scan {
        return Access::TableScan;
    }
    let tree = p.criteria.clone().map(|r| ctx.parse(r));
    let top: Vec<&Tree> = tree.as_ref().map(conjuncts).unwrap_or_default();
    if top.iter().any(|t| matches!(t, Tree::Rowid)) {
        return Access::Rowid;
    }
    let mut eq: Vec<&str> = search.of_fields.iter().map(String::as_str).collect();
    let mut range: Vec<&str> = Vec::new();
    let mut contains: Vec<&str> = Vec::new();
    for t in &top {
        match t {
            Tree::Leaf(f, Match::Equality) => eq.push(f),
            Tree::Leaf(f, Match::Range) => range.push(f),
            Tree::Leaf(f, Match::Contains) => contains.push(f),
            _ => {}
        }
    }
    let active: Vec<&Idx> = table.indexes.iter().filter(|i| !i.inactive).collect();
    let by: Vec<String> = search.by.clone();

    if let Some(name) = &p.use_index {
        let Some(idx) = table
            .indexes
            .iter()
            .find(|i| i.name.eq_ignore_ascii_case(name))
        else {
            return Access::Unknown(IndexUnknown::UnknownIndex);
        };
        let s = score(idx, &eq, &range, &by);
        return Access::Index(choice(idx, s, IndexReason::UseIndex, TieBreak::None));
    }
    if p.key_value {
        let Some(idx) = active.iter().find(|i| i.primary) else {
            return Access::Unknown(IndexUnknown::Unreadable);
        };
        let s = Score {
            equality: 1,
            range: false,
            sort: 0,
        };
        return Access::Index(choice(idx, s, IndexReason::KeyValue, TieBreak::None));
    }
    if let Some(f) = contains.first() {
        return match active
            .iter()
            .find(|i| i.word && i.components.first().is_some_and(|c| c == f))
        {
            Some(idx) => Access::Index(IndexChoice {
                name: idx.name.clone(),
                reason: IndexReason::Contains,
                tie: TieBreak::None,
                equality: 0,
                range: true,
                whole: false,
            }),
            None => Access::Unknown(IndexUnknown::NoWordIndex),
        };
    }
    if search.multi
        && top.iter().any(|t| match t {
            Tree::Or(branches) => branches.iter().all(|b| brackets_any(b, table)),
            _ => false,
        })
    {
        return Access::Unknown(IndexUnknown::MultiIndex);
    }
    let candidates: Vec<(&Idx, Score)> = active
        .iter()
        .filter(|i| !i.word)
        .map(|i| (*i, score(i, &eq, &range, &by)))
        .collect();
    if candidates.is_empty() {
        return Access::Unknown(IndexUnknown::UnknownTable);
    }
    let full = |(i, s): &(&Idx, Score)| {
        !i.components.is_empty() && usize::from(s.equality) == i.components.len()
    };
    let unique_full: Vec<(&Idx, Score)> = candidates
        .iter()
        .filter(|c| c.0.unique && full(c))
        .copied()
        .collect();
    if !unique_full.is_empty() {
        return pick(unique_full, IndexReason::UniqueEquality);
    }
    if search.multi && candidates.iter().filter(|c| full(c)).count() > 1 {
        return Access::Unknown(IndexUnknown::MultiIndex);
    }
    let best_eq = candidates.iter().map(|c| c.1.equality).max().unwrap_or(0);
    if best_eq > 0 {
        let top: Vec<(&Idx, Score)> = candidates
            .iter()
            .filter(|c| c.1.equality == best_eq)
            .copied()
            .collect();
        return pick(top, IndexReason::Equality);
    }
    let ranged: Vec<(&Idx, Score)> = candidates.iter().filter(|c| c.1.range).copied().collect();
    if !ranged.is_empty() {
        return pick(ranged, IndexReason::Range);
    }
    let best_sort = candidates.iter().map(|c| c.1.sort).max().unwrap_or(0);
    if search.multi && best_sort > 0 {
        let sorted: Vec<(&Idx, Score)> = candidates
            .iter()
            .filter(|c| c.1.sort == best_sort)
            .copied()
            .collect();
        return pick(sorted, IndexReason::Sort);
    }
    match candidates.iter().find(|c| c.0.primary) {
        Some(&(idx, s)) => Access::Index(choice(idx, s, IndexReason::Primary, TieBreak::None)),
        None => pick(candidates, IndexReason::Primary),
    }
}

/// Break a tie among candidates with the same main score: range, then sort,
/// then the primary index, then the first by name.
fn pick(mut c: Vec<(&Idx, Score)>, reason: IndexReason) -> Access {
    let mut tie = TieBreak::None;
    if c.len() > 1 && reason == IndexReason::Equality && c.iter().any(|x| x.1.range) {
        c.retain(|x| x.1.range);
        tie = TieBreak::Range;
    }
    if c.len() > 1 {
        let best = c.iter().map(|x| x.1.sort).max().unwrap_or(0);
        if best > 0 && c.iter().any(|x| x.1.sort < best) {
            c.retain(|x| x.1.sort == best);
            tie = TieBreak::Sort;
        }
    }
    if c.len() > 1 {
        if c.iter().any(|x| x.0.primary) {
            c.retain(|x| x.0.primary);
            tie = TieBreak::Primary;
        } else {
            c.sort_by_key(|x| x.0.name.to_ascii_lowercase());
            c.truncate(1);
            tie = TieBreak::Alphabetical;
        }
    }
    let (idx, s) = c[0];
    Access::Index(choice(idx, s, reason, tie))
}
