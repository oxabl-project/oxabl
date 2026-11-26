//! Semantic tokens: what each name that oxabl resolves is, for an editor that
//! colours a name by its meaning. See `docs/design/semantic-tokens.md`.
//!
//! The tokens come from the sites and declarations of the cached analysis, the
//! same facts that go to definition and highlights read. A name that does not
//! resolve, or that resolves to more than one thing, has no token: the
//! editor's grammar colours it.

use std::path::Path;

use oxabl_common::VirtualSpan;
use oxabl_index::search;
use oxabl_semantic::{Resolution, ScopeKind, SiteTarget, SymbolFlags, SymbolId, SymbolKind};

use oxabl_ast::NodeId;

use super::{Navigation, Span};

/// What a name is. The order is the legend of the language server: an index
/// into [`SemanticKind::ALL`] is the number a client sees.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemanticKind {
    /// A variable, stream, frame or widget.
    Variable,
    Parameter,
    /// A property of a class.
    Property,
    /// A user-defined function or an internal procedure.
    Function,
    /// A method of a class.
    Method,
    Class,
    Interface,
    Event,
    /// A table of a database, or its default buffer.
    Table,
    TempTable,
    /// A buffer that the code declares.
    Buffer,
    Dataset,
    /// A field of a database table.
    DatabaseField,
    /// A field of a temp-table, also one that copies a database field.
    TempTableField,
}

impl SemanticKind {
    /// Every kind, in legend order.
    pub const ALL: [SemanticKind; 14] = [
        SemanticKind::Variable,
        SemanticKind::Parameter,
        SemanticKind::Property,
        SemanticKind::Function,
        SemanticKind::Method,
        SemanticKind::Class,
        SemanticKind::Interface,
        SemanticKind::Event,
        SemanticKind::Table,
        SemanticKind::TempTable,
        SemanticKind::Buffer,
        SemanticKind::Dataset,
        SemanticKind::DatabaseField,
        SemanticKind::TempTableField,
    ];

    /// The name of the kind in the legend. Standard LSP names where one fits.
    pub const fn name(self) -> &'static str {
        match self {
            SemanticKind::Variable => "variable",
            SemanticKind::Parameter => "parameter",
            SemanticKind::Property => "property",
            SemanticKind::Function => "function",
            SemanticKind::Method => "method",
            SemanticKind::Class => "class",
            SemanticKind::Interface => "interface",
            SemanticKind::Event => "event",
            SemanticKind::Table => "table",
            SemanticKind::TempTable => "tempTable",
            SemanticKind::Buffer => "buffer",
            SemanticKind::Dataset => "dataset",
            SemanticKind::DatabaseField => "databaseField",
            SemanticKind::TempTableField => "tempTableField",
        }
    }

    /// The number of the kind in the legend.
    pub fn index(self) -> u32 {
        Self::ALL
            .iter()
            .position(|kind| *kind == self)
            .map_or(0, |at| at as u32)
    }
}

/// The modifiers of a token, as a bit set. The bit number is the position in
/// [`SemanticModifiers::NAMES`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct SemanticModifiers(u32);

impl SemanticModifiers {
    /// The modifiers, in legend order.
    pub const NAMES: [&'static str; 2] = ["declaration", "static"];
    /// The name is the one that declares the symbol.
    pub const DECLARATION: SemanticModifiers = SemanticModifiers(1);
    /// A class member that belongs to the class, not to an object.
    pub const STATIC: SemanticModifiers = SemanticModifiers(1 << 1);

    /// The bits, as the client reads them.
    pub const fn bits(self) -> u32 {
        self.0
    }

    pub const fn contains(self, other: SemanticModifiers) -> bool {
        self.0 & other.0 == other.0
    }

    fn with(self, other: SemanticModifiers) -> SemanticModifiers {
        SemanticModifiers(self.0 | other.0)
    }
}

/// Tokens placed in one file, sorted. An include read twice gives one name
/// twice: the same token is kept once. Tokens that overlap and differ in span,
/// kind or modifiers are two meanings of the same bytes, so none is kept. A
/// token also needs one agreeing token from each of the `readings(token)`
/// expansions that kept its bytes: a reading with no token for the name does
/// not agree.
fn one_meaning_each(
    tokens: Vec<SemanticToken>,
    readings: impl Fn(&SemanticToken) -> usize,
) -> Vec<SemanticToken> {
    let mut kept: Vec<SemanticToken> = Vec::with_capacity(tokens.len());
    let mut group_start = 0;
    let mut group_end = 0;
    let mut group_agrees = true;
    let mut group_count = 0;
    // Close the open group: its token when all its tokens are the same one and
    // every reading gave it.
    let close = |kept: &mut Vec<SemanticToken>, from: usize, agrees: bool, count: usize| {
        if from < kept.len() && !(agrees && count == readings(&kept[from])) {
            kept.truncate(from);
        }
    };
    for token in tokens {
        if kept.len() > group_start && token.span.start < group_end {
            group_agrees &= kept[group_start] == token;
            group_end = group_end.max(token.span.end);
            group_count += 1;
            continue;
        }
        close(&mut kept, group_start, group_agrees, group_count);
        group_start = kept.len();
        group_end = token.span.end;
        group_agrees = true;
        group_count = 1;
        kept.push(token);
    }
    close(&mut kept, group_start, group_agrees, group_count);
    kept
}

/// One name with its meaning, in the bytes of the analysed file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SemanticToken {
    pub span: Span,
    pub kind: SemanticKind,
    pub modifiers: SemanticModifiers,
}

/// A name in the expanded text, before it is placed in a file.
struct Candidate {
    span: VirtualSpan,
    kind: SemanticKind,
    modifiers: SemanticModifiers,
}

/// The longest name that gets a token. A name of this length or less is a
/// plausible identifier; anything longer is text the analysis spans for
/// another reason.
const MAX_NAME: u32 = 255;

impl Navigation<'_, '_> {
    /// The tokens of the names in `file` (the analysed file or an include its
    /// expansion read) that lie in `range`, if given, sorted, none inside
    /// another. A name has a token only when oxabl resolves it to one thing and
    /// the analysis spans exactly the bytes of the name.
    pub fn semantic_tokens(&self, file: &Path, range: Option<Span>) -> Vec<SemanticToken> {
        let file = search::normalize_lexically(file);
        let symbols = &self.model.semantic.symbols;
        let mut found: Vec<Candidate> = Vec::new();
        for site in symbols.sites() {
            if let Some(kind) = self.site_kind(&site.target, site.span) {
                found.push(Candidate {
                    span: site.span,
                    kind,
                    modifiers: self.symbol_modifiers(&site.target),
                });
            }
        }
        for (sym, symbol) in symbols.iter() {
            if symbol.declaration == NodeId::DUMMY
                || symbol.flags.contains(SymbolFlags::PARAM_TABLE_LIKE)
                || !self.is_written_as(symbol.name_span, symbol.name.as_ref())
            {
                continue;
            }
            if let Some(kind) = self.symbol_kind(sym, symbol.name_span) {
                // `FOR EACH table` names a table; it declares nothing.
                let modifiers = if self.scopes_its_block(symbol) {
                    self.modifiers_of(sym)
                } else {
                    self.modifiers_of(sym).with(SemanticModifiers::DECLARATION)
                };
                found.push(Candidate {
                    span: symbol.name_span,
                    kind,
                    modifiers,
                });
            }
        }
        let mut placer = Placer::new(self, &file);
        let mut tokens: Vec<SemanticToken> = innermost(found)
            .into_iter()
            .filter(|found| self.is_name(found.span))
            .filter_map(|found| {
                let span = placer.place(found.span)?;
                // A name that a macro or an argument rewrote has other bytes
                // in the file than in the expansion.
                if span.end - span.start != found.span.end - found.span.start {
                    return None;
                }
                if let Some(range) = range
                    && (span.end <= range.start || span.start >= range.end)
                {
                    return None;
                }
                Some(SemanticToken {
                    span,
                    kind: found.kind,
                    modifiers: found.modifiers,
                })
            })
            .collect();
        tokens.sort_by_key(|token| (token.span.start, token.span.end));
        // The expansions of an include that kept each byte; the analysed file
        // is read once.
        // Counted by two sorted lists: the chunks that start at or before a
        // byte, less the chunks that end at or before it.
        let kept = (!placer.is_root).then(|| {
            let spans = self
                .expanded
                .include_kept_spans(|path| search::normalize_lexically(path) == file);
            let mut starts: Vec<u32> = spans.iter().map(|span| span.start).collect();
            let mut ends: Vec<u32> = spans.iter().map(|span| span.end).collect();
            starts.sort_unstable();
            ends.sort_unstable();
            (starts, ends)
        });
        one_meaning_each(tokens, |token| match &kept {
            None => 1,
            Some((starts, ends)) => {
                let at = token.span.start;
                starts.partition_point(|&start| start <= at)
                    - ends.partition_point(|&end| end <= at)
            }
        })
    }

    /// Whether the expanded text at `span` is written as `name`, up to case.
    fn is_written_as(&self, span: VirtualSpan, name: &str) -> bool {
        self.text(span)
            .is_some_and(|text| text.eq_ignore_ascii_case(name))
    }

    /// Whether the text at `span` can be one name: on one line, no longer than
    /// [`MAX_NAME`], with no space, bracket, quote or operator in it.
    fn is_name(&self, span: VirtualSpan) -> bool {
        span.end > span.start
            && span.end - span.start <= MAX_NAME
            && self.text(span).is_some_and(|text| {
                !text.bytes().any(|byte| {
                    byte.is_ascii_whitespace()
                        || matches!(
                            byte,
                            b'(' | b')'
                                | b'['
                                | b']'
                                | b'{'
                                | b'}'
                                | b','
                                | b';'
                                | b':'
                                | b'+'
                                | b'*'
                                | b'/'
                                | b'='
                                | b'<'
                                | b'>'
                                | b'"'
                                | b'\''
                                | b'~'
                                | b'@'
                                | b'^'
                        )
                })
            })
    }

    /// What the name at a site is. `None` when it resolves to nothing, to
    /// more than one thing, or to a thing that has no kind.
    fn site_kind(&self, target: &SiteTarget, span: VirtualSpan) -> Option<SemanticKind> {
        let semantic = &self.model.semantic;
        let resolved = |node: NodeId| match semantic.references.get(node)? {
            Resolution::Resolved(sym) => Some(*sym),
            Resolution::Unresolved { .. } => None,
        };
        match target {
            SiteTarget::Reference(node) => self.symbol_kind(resolved(*node)?, span),
            SiteTarget::Run(node) => {
                let sym = resolved(*node)?;
                // A program is a file, and `RUN p IN h` names a procedure of
                // another program.
                if semantic.symbols.program_file(sym).is_some()
                    || self.handle_call_target(span).is_some()
                {
                    return None;
                }
                self.symbol_kind(sym, span)
            }
            SiteTarget::Using(node) => self.class_kind(resolved(*node)?),
            SiteTarget::Type(Resolution::Resolved(sym)) => self.class_kind(*sym),
            SiteTarget::Type(Resolution::Unresolved { .. }) => None,
            SiteTarget::Symbol(sym) => self.symbol_kind(*sym, span),
            SiteTarget::Table(_) => Some(SemanticKind::Table),
            SiteTarget::SchemaField(_) => Some(if self.qualifier_temp_table(span).is_some() {
                SemanticKind::TempTableField
            } else {
                SemanticKind::DatabaseField
            }),
        }
    }

    fn class_kind(&self, sym: SymbolId) -> Option<SemanticKind> {
        match self.model.semantic.symbols.get(sym).kind {
            SymbolKind::Class => Some(SemanticKind::Class),
            SymbolKind::Interface => Some(SemanticKind::Interface),
            _ => None,
        }
    }

    /// What the symbol `sym` is, where it is named at `span`.
    fn symbol_kind(&self, sym: SymbolId, span: VirtualSpan) -> Option<SemanticKind> {
        let symbols = &self.model.semantic.symbols;
        let symbol = symbols.get(sym);
        if symbols.field_table(sym).is_some() {
            // A temp-table that is `LIKE` a table has its own copy of each field.
            return Some(if symbols.like_field_owner(span).is_some() {
                SemanticKind::TempTableField
            } else {
                SemanticKind::DatabaseField
            });
        }
        Some(match symbol.kind {
            SymbolKind::Variable | SymbolKind::Stream | SymbolKind::Frame | SymbolKind::Widget => {
                SemanticKind::Variable
            }
            SymbolKind::Parameter => SemanticKind::Parameter,
            SymbolKind::Property => SemanticKind::Property,
            SymbolKind::Field => SemanticKind::TempTableField,
            SymbolKind::TempTable => SemanticKind::TempTable,
            // The default buffer of a database table, and the buffer that
            // `FOR EACH table` opens, are the table. A buffer the code
            // declares is its own record scope, as highlights treat it.
            SymbolKind::Buffer if symbol.declaration == NodeId::DUMMY => match symbol.table_id {
                Some(_) => SemanticKind::Table,
                None => return None,
            },
            SymbolKind::Buffer if symbol.table_id.is_some() && self.scopes_its_block(symbol) => {
                SemanticKind::Table
            }
            SymbolKind::Buffer => SemanticKind::Buffer,
            SymbolKind::Event => SemanticKind::Event,
            SymbolKind::Procedure => SemanticKind::Function,
            SymbolKind::Function => {
                if symbols.member_file(sym).is_some() || self.is_member(sym) {
                    SemanticKind::Method
                } else {
                    SemanticKind::Function
                }
            }
            SymbolKind::Class => SemanticKind::Class,
            SymbolKind::Interface => SemanticKind::Interface,
            SymbolKind::Dataset => SemanticKind::Dataset,
            // A built-in name and a sequence keep the colour of the grammar.
            SymbolKind::BuiltIn | SymbolKind::DataSource | SymbolKind::Sequence => return None,
        })
    }

    /// Whether the function symbol `sym` is declared in a class or interface.
    fn is_member(&self, sym: SymbolId) -> bool {
        let semantic = &self.model.semantic;
        let scope = semantic.symbols.get(sym).declared_in;
        matches!(
            semantic.scope_tree.get(scope).kind(),
            ScopeKind::Class | ScopeKind::Interface
        )
    }

    fn symbol_modifiers(&self, target: &SiteTarget) -> SemanticModifiers {
        let semantic = &self.model.semantic;
        let sym = match target {
            SiteTarget::Reference(node) | SiteTarget::Run(node) => {
                match semantic.references.get(*node) {
                    Some(Resolution::Resolved(sym)) => Some(*sym),
                    _ => None,
                }
            }
            SiteTarget::Symbol(sym) => Some(*sym),
            _ => None,
        };
        sym.map_or_else(SemanticModifiers::default, |sym| self.modifiers_of(sym))
    }

    fn modifiers_of(&self, sym: SymbolId) -> SemanticModifiers {
        let symbol = self.model.semantic.symbols.get(sym);
        if symbol.flags.contains(SymbolFlags::STATIC) {
            SemanticModifiers::STATIC
        } else {
            SemanticModifiers::default()
        }
    }
}

/// Places spans of the expanded text in one file. The path of the root and of
/// an include is compared once, not once for each span.
struct Placer<'n, 's, 'a> {
    navigation: &'n Navigation<'s, 'a>,
    file: &'n Path,
    /// Whether `file` is the analysed file.
    is_root: bool,
    /// The last include path asked about, and whether it is `file`.
    include: Option<(&'n Path, bool)>,
}

impl<'n, 's, 'a> Placer<'n, 's, 'a> {
    fn new(navigation: &'n Navigation<'s, 'a>, file: &'n Path) -> Self {
        Placer {
            navigation,
            file,
            is_root: navigation.root_path().as_deref() == Some(file),
            include: None,
        }
    }

    fn place(&mut self, span: VirtualSpan) -> Option<Span> {
        let expanded = self.navigation.expanded;
        let span = Span {
            start: span.start,
            end: span.end,
        };
        if let Some(root) = expanded.resolve_root_span(span) {
            return self.is_root.then_some(root);
        }
        let (path, placed, _) = expanded.resolve_include_span(span)?;
        let ours = match self.include {
            Some((last, ours)) if last == path => ours,
            _ => {
                let ours = search::normalize_lexically(path) == self.file;
                self.include = Some((path, ours));
                ours
            }
        };
        ours.then_some(placed)
    }
}

/// The candidates that contain no other one, one per span. A span that two
/// sites name with different kinds is not one thing: it has no token, and it
/// still keeps an enclosing span from being one. A call site covers its whole
/// call as well as its name; only the name is a token.
fn innermost(mut found: Vec<Candidate>) -> Vec<Candidate> {
    found.sort_by_key(|c| (c.span.start, std::cmp::Reverse(c.span.end)));
    let mut merged: Vec<(VirtualSpan, Option<(SemanticKind, SemanticModifiers)>)> =
        Vec::with_capacity(found.len());
    for candidate in found {
        match merged.last_mut() {
            Some((span, named)) if *span == candidate.span => {
                *named = match *named {
                    Some((kind, modifiers)) if kind == candidate.kind => {
                        Some((kind, modifiers.with(candidate.modifiers)))
                    }
                    _ => None,
                };
            }
            _ => merged.push((candidate.span, Some((candidate.kind, candidate.modifiers)))),
        }
    }
    // Later entries start at or after an entry's start, so the entry holds one
    // when the least end after it is within its own end.
    let mut least_end = u32::MAX;
    let mut keep = vec![false; merged.len()];
    for (at, (span, _)) in merged.iter().enumerate().rev() {
        keep[at] = least_end > span.end;
        least_end = least_end.min(span.end);
    }
    merged
        .into_iter()
        .zip(keep)
        .filter_map(|((span, named), keep)| {
            let (kind, modifiers) = named.filter(|_| keep)?;
            Some(Candidate {
                span,
                kind,
                modifiers,
            })
        })
        .collect()
}
