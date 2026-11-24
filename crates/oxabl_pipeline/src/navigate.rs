//! Go to definition and document links: what a name in a file refers to, and
//! the file and bytes that declare it.
//!
//! A [`Navigation`] holds one analysis of a compilation unit with its navigation
//! sites recorded ([`SymbolTable::sites`](oxabl_semantic::SymbolTable::sites)).
//! Every answer is in the bytes of a physical file: a declaration written in an
//! include is placed in that include, never in the expanded text.
//!
//! An answer never guesses. A name that does not resolve, or that is computed at
//! run time (`RUN VALUE(...)`, `DYNAMIC-NEW`), has no target. A `RUN` target or
//! a class name that more than one file on the search path supplies has each of
//! them as a target. A link is only made to a file that the name resolves to
//! alone.
//!
//! `RUN p IN h` goes to `p` in the program the handle runs when the static call
//! graph knows it ([`CallTarget::InProgram`]): every `RUN ... PERSISTENT SET h`
//! of the file runs one program and nothing else writes `h`.

use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use oxabl_analyze::{ExpandedFile, analyze_for_navigation};
use oxabl_ast::{NodeId, Span};
use oxabl_common::{FileId, VirtualSpan};
use oxabl_index::calls::{
    CallKind, CallTarget, FileCalls, ProgramAnswer, UnknownCall, resolve_program,
};
use oxabl_index::search::{self, NameKind};
use oxabl_semantic::{
    IndexName, IndexedFileId, Resolution, Semantic, SiteTarget, SourceContext, SymbolId,
    SymbolKind, UnresolvedReason,
};
use oxabl_workspace::is_include_fragment;

use crate::{Expansion, LintPipeline, OutlineKind, OutlineSymbol, outline};

/// Where a name is declared: a file, and the declaring name's bytes in it.
/// `span` is `None` when the target is the file itself (a program, an include).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Target {
    pub path: PathBuf,
    pub span: Option<Span>,
}

/// What a name refers to, as navigation finds it. Two names refer to one
/// thing when they have one key.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Key {
    /// A declaration, a file, or a place in a file.
    At(Target),
    /// Internal procedure `procedure` (folded) of `program`, whose text does
    /// not declare it: a super procedure of the program can supply it.
    Undeclared { program: PathBuf, procedure: String },
}

impl Key {
    /// The place go to definition shows: the program for an undeclared
    /// procedure.
    pub(crate) fn into_target(self) -> Target {
        match self {
            Key::At(target) => target,
            Key::Undeclared { program, .. } => Target {
                path: program,
                span: None,
            },
        }
    }
}

/// A part of the analysed file that names another file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Link {
    /// The bytes of the reference, in the analysed file.
    pub span: Span,
    pub target: PathBuf,
}

/// One compilation unit analysed for navigation. See the module docs.
pub struct Navigation<'s, 'a> {
    pub(crate) run: &'s LintPipeline<'a>,
    pub(crate) expanded: &'s ExpandedFile,
    search_base: Option<PathBuf>,
    pub(crate) semantic: Semantic,
    file_of: &'s dyn Fn(IndexedFileId) -> Option<PathBuf>,
    /// The unit's calls, extracted on the first `RUN` site asked about.
    calls: OnceCell<FileCalls>,
    /// The outline of each file read for an answer, by path. `None` when the
    /// file cannot be read or outlined.
    outlines: RefCell<HashMap<PathBuf, Option<Rc<[OutlineSymbol]>>>>,
    /// The routines of the expanded text, outlined on first use.
    pub(crate) routines: OnceCell<Vec<OutlineSymbol>>,
}

impl<'a> LintPipeline<'a> {
    /// Analyse `expansion` for navigation. `None` when its preprocessing failed.
    ///
    /// `file_of` names the file behind an index id when the handle's index is
    /// one the caller supplied ([`with_index`](Self::with_index)); a handle with
    /// its own index answers that itself.
    ///
    /// **Unguarded** like [`collect`](Self::collect): the caller contains a panic.
    pub fn navigation<'s>(
        &'s self,
        expansion: &'s Expansion,
        file_of: &'s dyn Fn(IndexedFileId) -> Option<PathBuf>,
    ) -> Option<Navigation<'s, 'a>> {
        let expanded = expansion.expanded()?;
        let source_context = if self.file().is_some_and(is_include_fragment) {
            SourceContext::IncludeFragment
        } else {
            SourceContext::CompilationUnit
        };
        let semantic = self.with_run_index(|index| {
            analyze_for_navigation(
                expanded,
                &self.config().schema,
                self.config().schema_loaded,
                index,
                source_context,
            )
        });
        Some(Navigation {
            run: self,
            expanded,
            search_base: expansion.search_base().map(Path::to_path_buf),
            semantic,
            file_of,
            calls: OnceCell::new(),
            outlines: RefCell::new(HashMap::new()),
            routines: OnceCell::new(),
        })
    }
}

impl Navigation<'_, '_> {
    /// The declarations the name at byte `offset` of `file` refers to. `file`
    /// is the analysed file or an include its expansion read. Empty when the
    /// name has none, or the offset is on no name.
    pub fn definitions(&self, file: &Path, offset: u32) -> Vec<Target> {
        dedupe(
            self.keys_at(file, offset)
                .into_iter()
                .map(Key::into_target)
                .collect(),
        )
    }

    /// What the name at byte `offset` of `file` refers to, as
    /// [`definitions`](Self::definitions) finds it.
    pub(crate) fn keys_at(&self, file: &Path, offset: u32) -> Vec<Key> {
        let file = search::normalize_lexically(file);
        if let Some(target) = self.include_at(&file, offset) {
            return vec![Key::At(target)];
        }
        // A cursor just after a name, as a click at its end gives, still means it.
        for at in [Some(offset), offset.checked_sub(1)].into_iter().flatten() {
            if let Some(virt) = self.expanded_at(&file, at)
                && let Some(keys) = self.answer_at(virt)
            {
                return dedupe(keys);
            }
        }
        Vec::new()
    }

    /// What the innermost name at expanded offset `virt` refers to.
    pub(crate) fn keys_in_expanded(&self, virt: u32) -> Vec<Key> {
        self.answer_at(virt).map(dedupe).unwrap_or_default()
    }

    /// Where byte `offset` of `file` (the root or an include the expansion
    /// read) lies in the expanded text.
    pub(crate) fn expanded_at(&self, file: &Path, offset: u32) -> Option<u32> {
        self.file_ids(file)
            .iter()
            .find_map(|id| self.expanded.expanded_offset(*id, offset))
    }

    /// The include references, literal `RUN` targets and `USING` names written
    /// in the analysed file itself that name exactly one file. Text in a branch
    /// of `&IF` the preprocessor did not take is not analysed, so it has none.
    pub fn links(&self) -> Vec<Link> {
        let mut links: Vec<Link> = Vec::new();
        for expansion in self.expanded.include_expansions() {
            if expansion.parent.is_some() {
                continue;
            }
            if let Some(site) = expansion.site {
                links.push(Link {
                    span: site,
                    target: search::normalize_lexically(expansion.path),
                });
            }
        }
        let symbols = &self.semantic.symbols;
        for site in symbols.sites() {
            let (SiteTarget::Run(node) | SiteTarget::Using(node)) = site.target else {
                continue;
            };
            let Some(Resolution::Resolved(sym)) = self.semantic.references.get(node) else {
                continue;
            };
            let file = symbols
                .program_file(*sym)
                .or_else(|| symbols.class_file(*sym));
            let Some(target) = file.and_then(|file| self.path_of(file)) else {
                continue;
            };
            let Some(span) = self.expanded.resolve_root_span(to_span(site.span)) else {
                continue;
            };
            links.push(Link { span, target });
        }
        links.sort_by_key(|link| (link.span.start, link.span.end));
        links.dedup();
        links
    }

    /// The include whose `{...}` reference in `file` holds `offset`.
    fn include_at(&self, file: &Path, offset: u32) -> Option<Target> {
        let expansions = self.expanded.include_expansions();
        expansions.iter().find_map(|expansion| {
            let site = expansion.site?;
            if offset < site.start || offset >= site.end {
                return None;
            }
            let parent = match expansion.parent {
                None => self.root_path()?,
                Some(at) => search::normalize_lexically(expansions.get(at)?.path),
            };
            (parent == file).then(|| Target {
                path: search::normalize_lexically(expansion.path),
                span: None,
            })
        })
    }

    /// The file ids under which the expansion holds the bytes of `file`: the
    /// root, or each expansion of an include.
    fn file_ids(&self, file: &Path) -> Vec<FileId> {
        if self.root_path().as_deref() == Some(file) {
            return vec![self.expanded.root_file()];
        }
        self.expanded
            .include_expansions()
            .iter()
            .filter(|expansion| search::normalize_lexically(expansion.path) == file)
            .filter_map(|expansion| expansion.file)
            .collect()
    }

    pub(crate) fn root_path(&self) -> Option<PathBuf> {
        self.run.file().map(search::normalize_lexically)
    }

    /// The targets of the innermost name at expanded offset `virt`. `None` when
    /// no name there has an answer, `Some(empty)` when the name has no target.
    fn answer_at(&self, virt: u32) -> Option<Vec<Key>> {
        let symbols = &self.semantic.symbols;
        let mut sites: Vec<_> = symbols
            .sites()
            .iter()
            .filter(|site| contains(site.span, virt))
            .collect();
        sites.sort_by_key(|site| site.span.end - site.span.start);
        let declaration = symbols
            .iter()
            .filter(|(_, symbol)| {
                symbol.declaration != NodeId::DUMMY && contains(symbol.name_span, virt)
            })
            .min_by_key(|(_, symbol)| symbol.name_span.end - symbol.name_span.start);
        // Sites of one length name one thing; the first with a target wins, and
        // one that answers "nothing" only stops the search when none has one.
        let mut at = 0;
        while at < sites.len() {
            let length = sites[at].span.end - sites[at].span.start;
            if let Some((sym, symbol)) = declaration
                && symbol.name_span.end - symbol.name_span.start <= length
            {
                return Some(self.symbol_keys(sym));
            }
            let mut answered = false;
            while at < sites.len() && sites[at].span.end - sites[at].span.start == length {
                match self.site_keys(&sites[at].target, sites[at].span) {
                    Some(keys) if !keys.is_empty() => return Some(keys),
                    Some(_) => answered = true,
                    None => {}
                }
                at += 1;
            }
            if answered {
                return Some(Vec::new());
            }
        }
        declaration.map(|(sym, _)| self.symbol_keys(sym))
    }

    fn symbol_keys(&self, sym: SymbolId) -> Vec<Key> {
        self.symbol_targets(sym).into_iter().map(Key::At).collect()
    }

    /// What the name at a site refers to. `None` when the site has no answer,
    /// `Some(empty)` when its name has no target.
    pub(crate) fn site_keys(&self, target: &SiteTarget, span: VirtualSpan) -> Option<Vec<Key>> {
        let at = |targets: Vec<Target>| targets.into_iter().map(Key::At).collect();
        match target {
            SiteTarget::Reference(node) => match self.semantic.references.get(*node)? {
                Resolution::Resolved(sym) => Some(self.symbol_keys(*sym)),
                Resolution::Unresolved { .. } => Some(Vec::new()),
            },
            SiteTarget::Run(node) => match self.semantic.references.get(*node)? {
                // A program is never run through a handle.
                Resolution::Resolved(sym) if self.semantic.symbols.program_file(*sym).is_some() => {
                    Some(self.symbol_keys(*sym))
                }
                _ if let Some(keys) = self.handle_call_keys(span) => Some(keys),
                Resolution::Resolved(sym) => Some(self.symbol_keys(*sym)),
                Resolution::Unresolved {
                    reason: UnresolvedReason::Unknowable,
                    ..
                } => Some(at(self.candidates(span, NameKind::Program))),
                Resolution::Unresolved { .. } => Some(Vec::new()),
            },
            SiteTarget::Using(node) => self
                .type_targets(self.semantic.references.get(*node)?, span)
                .map(at),
            SiteTarget::Type(resolution) => self.type_targets(resolution, span).map(at),
            SiteTarget::Symbol(sym) => Some(self.symbol_keys(*sym)),
            SiteTarget::Table(table) => Some(at(self.table_target(*table).into_iter().collect())),
            SiteTarget::SchemaField(table) => {
                let name = self.text(span)?;
                Some(at(self.field_target(*table, name)))
            }
        }
    }

    fn type_targets(&self, resolution: &Resolution, span: VirtualSpan) -> Option<Vec<Target>> {
        Some(match resolution {
            Resolution::Resolved(sym) => self.symbol_targets(*sym),
            Resolution::Unresolved {
                reason: UnresolvedReason::Unknowable,
                ..
            } => self.candidates(span, NameKind::Class),
            Resolution::Unresolved { .. } => Vec::new(),
        })
    }

    /// The targets of a `RUN p IN h` whose name is at `span`. `None` when the
    /// `RUN` there names no handle, or names `THIS-PROCEDURE`.
    fn handle_call_keys(&self, span: VirtualSpan) -> Option<Vec<Key>> {
        let calls = self.calls();
        let site = calls.sites.iter().find(|site| {
            matches!(
                site.kind,
                CallKind::Run | CallKind::Persistent | CallKind::Remote
            ) && site.at.start <= span.start
                && span.end <= site.at.end
        })?;
        match &site.target {
            CallTarget::InProgram { program, procedure } => {
                Some(self.procedure_keys(program, procedure))
            }
            CallTarget::Unknown {
                reason: UnknownCall::Handle,
                ..
            } => Some(Vec::new()),
            _ => None,
        }
    }

    /// The calls of the expanded unit, extracted once.
    pub(crate) fn calls(&self) -> &FileCalls {
        self.calls.get_or_init(|| {
            let text = &self.expanded.text;
            let tokens = oxabl_lexer::tokenize(text);
            let program = oxabl_parser::Parser::new(&tokens, text).parse_program();
            FileCalls::extract(&program.statements, text, &tokens)
        })
    }

    /// Internal procedure `procedure` in each file that supplies `program`, or
    /// the program when its text declares no such procedure.
    fn procedure_keys(&self, program: &str, procedure: &str) -> Vec<Key> {
        let paths = match self
            .run
            .with_run_index(|index| resolve_program(index, program))
        {
            ProgramAnswer::Found(file) => self.path_of(file).into_iter().collect(),
            ProgramAnswer::Unknown(UnknownCall::Ambiguous) => self
                .candidates_named(program, NameKind::Program)
                .into_iter()
                .map(|target| target.path)
                .collect(),
            ProgramAnswer::Unknown(_) => Vec::new(),
        };
        let procedure = procedure.trim_matches(['"', '\'']);
        let mut keys = Vec::new();
        for path in paths {
            let spans: Vec<Span> = self
                .outline_of(&path)
                .as_deref()
                .unwrap_or_default()
                .iter()
                .filter(|symbol| {
                    symbol.kind == OutlineKind::Procedure
                        && symbol.name.eq_ignore_ascii_case(procedure)
                })
                .map(|symbol| symbol.selection_span)
                .collect();
            if spans.is_empty() {
                keys.push(Key::Undeclared {
                    program: path,
                    procedure: procedure.to_ascii_lowercase(),
                });
                continue;
            }
            for span in spans {
                keys.push(Key::At(Target {
                    path: path.clone(),
                    span: Some(span),
                }));
            }
        }
        keys
    }

    /// Every file on the search path that the name written at `span` could
    /// mean, for a name the index declined to choose for.
    fn candidates(&self, span: VirtualSpan, kind: NameKind) -> Vec<Target> {
        match self.text(span) {
            Some(raw) => self.candidates_named(raw, kind),
            None => Vec::new(),
        }
    }

    fn candidates_named(&self, raw: &str, kind: NameKind) -> Vec<Target> {
        let raw = raw.trim().trim_matches(['"', '\'']);
        if raw.is_empty() {
            return Vec::new();
        }
        let config = self.run.config();
        let paths = config.include_paths_from(self.search_base.as_deref());
        search::find_candidates_with(
            self.run.file_system(),
            &paths,
            &IndexName::new(raw),
            kind,
            &config.run_extensions,
        )
        .into_iter()
        .map(|path| {
            let span = match kind {
                NameKind::Class => self.class_span(&path),
                NameKind::Program => None,
            };
            Target { path, span }
        })
        .collect()
    }

    /// The expanded text at `span`.
    pub(crate) fn text(&self, span: VirtualSpan) -> Option<&str> {
        self.expanded
            .text
            .get(span.start as usize..span.end as usize)
    }

    /// Where `sym` is declared. A buffer named like the table it reads (the
    /// buffer `FOR EACH customer` declares) means the table.
    fn symbol_targets(&self, sym: SymbolId) -> Vec<Target> {
        let symbols = &self.semantic.symbols;
        let symbol = symbols.get(sym);
        if let Some(file) = symbols.program_file(sym) {
            return self.file_target(file, |_| None);
        }
        if let Some(file) = symbols.class_file(sym) {
            return self.file_target(file, |path| self.class_span(path));
        }
        if let Some(file) = symbols.member_file(sym) {
            let Some(path) = self.path_of(file) else {
                return Vec::new();
            };
            let spans = self.member_spans(&path, symbol.name.as_ref());
            if spans.is_empty() {
                return vec![Target { path, span: None }];
            }
            return spans
                .into_iter()
                .map(|span| Target {
                    path: path.clone(),
                    span: Some(span),
                })
                .collect();
        }
        if let Some(table) = symbols.field_table(sym) {
            return self.field_target(table, symbol.name.as_ref());
        }
        let table_named = matches!(symbol.kind, SymbolKind::Buffer | SymbolKind::TempTable)
            && symbol.table_id.is_some()
            && (symbol.declaration == NodeId::DUMMY || self.names_its_table(symbol));
        if table_named {
            return symbol
                .table_id
                .and_then(|table| self.table_target(table))
                .into_iter()
                .collect();
        }
        if symbol.declaration == NodeId::DUMMY {
            return Vec::new();
        }
        self.place(symbol.name_span).into_iter().collect()
    }

    /// Whether a buffer's name is the name of the table it reads, so the name at
    /// its declaration names the table.
    fn names_its_table(&self, symbol: &oxabl_semantic::Symbol) -> bool {
        let Some(table) = symbol
            .table_id
            .and_then(|id| self.run.config().schema.get_by_id(id))
        else {
            return false;
        };
        let written = self.text(symbol.name_span).unwrap_or_default();
        let unqualified = written.rsplit('.').next().unwrap_or(written);
        unqualified.eq_ignore_ascii_case(table.name.as_ref())
    }

    fn file_target(
        &self,
        file: IndexedFileId,
        span: impl FnOnce(&Path) -> Option<Span>,
    ) -> Vec<Target> {
        let Some(path) = self.path_of(file) else {
            return Vec::new();
        };
        let span = span(&path);
        vec![Target { path, span }]
    }

    pub(crate) fn path_of(&self, file: IndexedFileId) -> Option<PathBuf> {
        self.run
            .indexed_path(file)
            .or_else(|| (self.file_of)(file))
            .map(|path| search::normalize_lexically(&path))
    }

    fn table_target(&self, table: oxabl_schema::TableId) -> Option<Target> {
        let (path, table) = self.run.config().schema.table_source(table)?;
        Some(Target {
            path: path.to_path_buf(),
            span: Some(table.source.span),
        })
    }

    fn field_target(&self, table: oxabl_schema::TableId, name: &str) -> Vec<Target> {
        let Some((path, table)) = self.run.config().schema.table_source(table) else {
            return Vec::new();
        };
        match table.resolve_field_by_name(name) {
            oxabl_schema::FieldResolution::Unique(field) => vec![Target {
                path: path.to_path_buf(),
                span: Some(field.source.span),
            }],
            _ => Vec::new(),
        }
    }

    /// A span of the expanded text in the file that wrote it.
    pub(crate) fn place(&self, span: VirtualSpan) -> Option<Target> {
        let span = to_span(span);
        if let Some(root) = self.expanded.resolve_root_span(span) {
            return Some(Target {
                path: self.root_path()?,
                span: Some(root),
            });
        }
        let (path, span, _) = self.expanded.resolve_include_span(span)?;
        Some(Target {
            path: search::normalize_lexically(path),
            span: Some(span),
        })
    }

    /// The name of the class, interface or enum a `.cls` file declares.
    fn class_span(&self, path: &Path) -> Option<Span> {
        let symbols = self.outline_of(path)?;
        symbols
            .iter()
            .find(|symbol| {
                matches!(
                    symbol.kind,
                    OutlineKind::Class | OutlineKind::Interface | OutlineKind::Enum
                )
            })
            .map(|symbol| symbol.selection_span)
    }

    /// Each declaration of member `name` in the class of a `.cls` file: one per
    /// overload of a method.
    fn member_spans(&self, path: &Path, name: &str) -> Vec<Span> {
        let Some(symbols) = self.outline_of(path) else {
            return Vec::new();
        };
        let mut spans = Vec::new();
        for class in symbols.iter() {
            for member in &class.children {
                let is_member = matches!(
                    member.kind,
                    OutlineKind::Method
                        | OutlineKind::Property
                        | OutlineKind::Event
                        | OutlineKind::Variable
                        | OutlineKind::Buffer
                        | OutlineKind::TempTable
                        | OutlineKind::Dataset
                        | OutlineKind::DataSource
                );
                if is_member && member.name.eq_ignore_ascii_case(name) {
                    spans.push(member.selection_span);
                }
            }
        }
        spans
    }

    /// The outline of a file, read and outlined once per analysis.
    pub(crate) fn outline_of(&self, path: &Path) -> Option<Rc<[OutlineSymbol]>> {
        if let Some(known) = self.outlines.borrow().get(path) {
            return known.clone();
        }
        let outlined = self
            .run
            .file_system()
            .read(path)
            .ok()
            .and_then(|text| outline(&text).ok())
            .map(Rc::from);
        self.outlines
            .borrow_mut()
            .insert(path.to_path_buf(), outlined.clone());
        outlined
    }
}

fn contains(span: VirtualSpan, at: u32) -> bool {
    span.start <= at && at < span.end
}

fn to_span(span: VirtualSpan) -> Span {
    Span {
        start: span.start,
        end: span.end,
    }
}

fn dedupe<T: PartialEq>(targets: Vec<T>) -> Vec<T> {
    let mut kept: Vec<T> = Vec::with_capacity(targets.len());
    for target in targets {
        if !kept.contains(&target) {
            kept.push(target);
        }
    }
    kept
}
