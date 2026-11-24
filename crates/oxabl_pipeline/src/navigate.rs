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

use std::cell::{Cell, OnceCell, RefCell};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::{Arc, OnceLock};

use oxabl_analyze::{ExpandedFile, analyze_for_navigation};
use oxabl_ast::{NodeId, Span};
use oxabl_common::{FileId, VirtualSpan};
use oxabl_index::calls::{
    CallKind, CallTarget, FileCalls, ProgramAnswer, UnknownCall, resolve_program,
};
use oxabl_index::search::{self, NameKind};
use oxabl_schema::{FieldResolution, TableId};
use oxabl_semantic::{
    IndexName, IndexedFileId, Resolution, Semantic, SiteTarget, SourceContext, SymbolId,
    SymbolKind, UnresolvedReason,
};
use oxabl_workspace::is_include_fragment;

use crate::{Expansion, LintPipeline, OutlineKind, OutlineSymbol, outline};

mod hover;

pub use hover::Hover;

/// Where a name is declared: a file, and the declaring name's bytes in it.
/// `span` is `None` when the target is the file itself (a program, an include).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Target {
    pub path: PathBuf,
    pub span: Option<Span>,
}

/// A part of the analysed file that names another file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Link {
    /// The bytes of the reference, in the analysed file.
    pub span: Span,
    pub target: PathBuf,
}

/// One use of a name in the analysed file: the bytes of the name and whether
/// the statement changes its value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Highlight {
    pub span: Span,
    pub write: bool,
}

/// What a name stands for, in terms two uses can share. Two uses highlight
/// each other when they have the same identity.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Identity {
    /// A program or a class: its file.
    File(PathBuf),
    /// A member that a class file declares, by lower-case name.
    Member(PathBuf, String),
    /// A schema field, by table and full lower-case name.
    Field(TableId, String),
    Table(TableId),
    /// A declaration written in the unit. Each expansion of an include declares
    /// its own symbols, so the symbol, not the place of its name, is the identity.
    Declared(SymbolId),
    /// A target from a lookup that found one or several files.
    At(Target),
}

/// The analysis of one expansion with its navigation sites recorded, apart from
/// the borrowed state of a [`Navigation`], so a caller can keep it for as long
/// as the expansion is current. The unit's calls are extracted on the first
/// `RUN` site asked about and kept with it.
pub struct NavigationModel {
    semantic: Semantic,
    calls: OnceLock<FileCalls>,
}

/// Which sites a highlight may search the file system for.
#[derive(Clone, Copy)]
enum Spelling<'t> {
    /// Every site: the cursor's own.
    Any,
    /// Sites written like this name.
    Of(&'t str),
    /// No site.
    None,
}

/// One compilation unit analysed for navigation. See the module docs.
pub struct Navigation<'s, 'a> {
    run: &'s LintPipeline<'a>,
    expanded: &'s ExpandedFile,
    search_base: Option<PathBuf>,
    model: Arc<NavigationModel>,
    file_of: &'s dyn Fn(IndexedFileId) -> Option<PathBuf>,
    /// The expanded unit lexed and parsed again, on the first request that
    /// needs its statements.
    parsed: OnceCell<Parsed>,
    /// The identity of each symbol a highlight asked about.
    identities: RefCell<HashMap<SymbolId, Option<Identity>>>,
    /// The outline of each file read for a member or a procedure.
    outlines: RefCell<HashMap<PathBuf, Option<Rc<Vec<OutlineSymbol>>>>>,
}

/// One text lexed and parsed.
struct Parsed {
    tokens: Vec<oxabl_lexer::Token>,
    program: oxabl_parser::Program,
}

impl Parsed {
    fn of(text: &str) -> Parsed {
        let tokens = oxabl_lexer::tokenize(text);
        let program = oxabl_parser::Parser::new(&tokens, text).parse_program();
        Parsed { tokens, program }
    }
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
        let model = self.navigation_model(expansion)?;
        self.navigation_with(expansion, Arc::new(model), file_of)
    }

    /// The analysis [`navigation`](Self::navigation) runs, for a caller that
    /// keeps it. `None` when the preprocessing of `expansion` failed.
    ///
    /// **Unguarded** like [`collect`](Self::collect).
    pub fn navigation_model(&self, expansion: &Expansion) -> Option<NavigationModel> {
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
        Some(NavigationModel {
            semantic,
            calls: OnceLock::new(),
        })
    }

    /// A [`Navigation`] over a `model` that [`navigation_model`](Self::navigation_model)
    /// made from this `expansion`. `None` when its preprocessing failed.
    pub fn navigation_with<'s>(
        &'s self,
        expansion: &'s Expansion,
        model: Arc<NavigationModel>,
        file_of: &'s dyn Fn(IndexedFileId) -> Option<PathBuf>,
    ) -> Option<Navigation<'s, 'a>> {
        let expanded = expansion.expanded()?;
        Some(Navigation {
            run: self,
            expanded,
            search_base: expansion.search_base().map(Path::to_path_buf),
            model,
            file_of,
            parsed: OnceCell::new(),
            identities: RefCell::new(HashMap::new()),
            outlines: RefCell::new(HashMap::new()),
        })
    }
}

impl Navigation<'_, '_> {
    /// The declarations the name at byte `offset` of `file` refers to. `file`
    /// is the analysed file or an include its expansion read. Empty when the
    /// name has none, or the offset is on no name.
    pub fn definitions(&self, file: &Path, offset: u32) -> Vec<Target> {
        let file = search::normalize_lexically(file);
        if let Some(target) = self.include_at(&file, offset) {
            return vec![target];
        }
        let ids = self.file_ids(&file);
        // A cursor just after a name, as a click at its end gives, still means it.
        for at in [Some(offset), offset.checked_sub(1)].into_iter().flatten() {
            let virt = ids
                .iter()
                .find_map(|id| self.expanded.expanded_offset(*id, at));
            if let Some(virt) = virt
                && let Some(targets) = self.answer_at(virt)
            {
                return dedupe(targets);
            }
        }
        Vec::new()
    }

    /// The uses of the name at byte `offset` of `file` that are written in
    /// `file`, in order, each once. The name is the one go to definition would
    /// answer for, and a use counts when it resolves to the same declaration.
    /// Empty when the name does not resolve, the offset is on no name, or the
    /// name has more than one possible declaration.
    ///
    /// The cost is one pass over the recorded sites. A site is compared by what
    /// its resolution already holds; only the cursor's own name, and with it
    /// the sites that spell the same name, ask the file system.
    pub fn highlights(&self, file: &Path, offset: u32) -> Vec<Highlight> {
        let file = search::normalize_lexically(file);
        let ids = self.file_ids(&file);
        let spelled = Cell::new(None);
        let mut wanted = None;
        for virt in self.cursor_candidates(&ids, offset) {
            if let Some(found) = self.innermost(
                virt,
                &|sym| self.symbol_identities(sym),
                &|target, span| {
                    let found = self.site_identities(target, span, Spelling::Any);
                    if found.as_ref().is_some_and(|found| !found.is_empty()) {
                        spelled.set(Some(span));
                    }
                    found
                },
                &|identities| !identities.is_empty(),
                Vec::new,
            ) {
                wanted = Some(found);
                break;
            }
        }
        // Fail closed: a name with several declarations marks none of them.
        let Some([wanted]) = wanted.as_deref() else {
            return Vec::new();
        };
        let wanted = wanted.clone();
        let same_spelling = match (&wanted, spelled.get().and_then(|span| self.text(span))) {
            (Identity::At(_), Some(text)) => Spelling::Of(text),
            _ => Spelling::None,
        };
        let symbols = &self.model.semantic.symbols;
        let mut uses: Vec<(VirtualSpan, bool)> = Vec::new();
        for site in symbols.sites() {
            let found = self.site_identities(&site.target, site.span, same_spelling);
            if found.as_deref() == Some(std::slice::from_ref(&wanted)) {
                uses.push((site.span, site.write));
            }
        }
        for (sym, symbol) in symbols.iter() {
            if symbol.declaration != NodeId::DUMMY
                && self.symbol_identities(sym) == std::slice::from_ref(&wanted)
            {
                // A buffer holds no value of its own: naming it declares no write.
                uses.push((symbol.name_span, symbol.kind != SymbolKind::Buffer));
            }
        }
        let mut found: Vec<Highlight> = innermost(uses)
            .into_iter()
            .filter_map(|(span, write)| {
                let span = self.physical(span, &file)?;
                Some(Highlight { span, write })
            })
            .collect();
        found.sort_by_key(|highlight| (highlight.span.start, highlight.span.end));
        let mut merged: Vec<Highlight> = Vec::with_capacity(found.len());
        for highlight in found {
            match merged.last_mut() {
                Some(last) if last.span == highlight.span => last.write |= highlight.write,
                _ => merged.push(highlight),
            }
        }
        merged
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
        let symbols = &self.model.semantic.symbols;
        for site in symbols.sites() {
            let (SiteTarget::Run(node) | SiteTarget::Using(node)) = site.target else {
                continue;
            };
            let Some(Resolution::Resolved(sym)) = self.model.semantic.references.get(node) else {
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

    fn root_path(&self) -> Option<PathBuf> {
        self.run.file().map(search::normalize_lexically)
    }

    /// `span` of the expanded text as bytes of `file`: the root, or an include
    /// that the expansion read.
    fn physical(&self, span: VirtualSpan, file: &Path) -> Option<Span> {
        let span = to_span(span);
        if let Some(root) = self.expanded.resolve_root_span(span) {
            return (self.root_path().as_deref() == Some(file)).then_some(root);
        }
        let (path, span, _) = self.expanded.resolve_include_span(span)?;
        (search::normalize_lexically(path) == file).then_some(span)
    }

    /// What the site names, for [`highlights`](Self::highlights). The shape of
    /// the answer is that of [`site_targets`](Self::site_targets). A name that
    /// only a file search can answer (a `RUN` through a handle, a name with
    /// several candidates) is searched for when `spelling` allows it; otherwise
    /// the site has no answer here.
    fn site_identities(
        &self,
        target: &SiteTarget,
        span: VirtualSpan,
        spelling: Spelling<'_>,
    ) -> Option<Vec<Identity>> {
        let symbols = &self.model.semantic.symbols;
        let resolved = match target {
            SiteTarget::Symbol(sym) => Some(*sym),
            SiteTarget::Reference(node) => match self.model.semantic.references.get(*node)? {
                Resolution::Resolved(sym) => Some(*sym),
                Resolution::Unresolved { .. } => return Some(Vec::new()),
            },
            SiteTarget::Run(node) => match self.model.semantic.references.get(*node)? {
                Resolution::Resolved(sym)
                    if symbols.program_file(*sym).is_some()
                        || self.handle_call_target(span).is_none() =>
                {
                    Some(*sym)
                }
                _ => None,
            },
            SiteTarget::Using(node) => match self.model.semantic.references.get(*node)? {
                Resolution::Resolved(sym) => Some(*sym),
                Resolution::Unresolved { .. } => None,
            },
            SiteTarget::Type(Resolution::Resolved(sym)) => Some(*sym),
            SiteTarget::Type(_) => None,
            SiteTarget::Table(table) => return Some(vec![Identity::Table(*table)]),
            SiteTarget::SchemaField(table) => {
                let name = self.text(span)?;
                return Some(vec![self.field_identity(*table, name)]);
            }
        };
        if let Some(sym) = resolved {
            return Some(self.symbol_identities(sym));
        }
        let searched = match spelling {
            Spelling::Any => true,
            Spelling::None => false,
            Spelling::Of(wanted) => self.text(span).is_some_and(|text| {
                let bare = |name: &str| name.trim().trim_matches(['"', '\'']).to_ascii_lowercase();
                bare(text) == bare(wanted)
            }),
        };
        if !searched {
            return None;
        }
        let targets = self.site_targets(target, span)?;
        Some(targets.into_iter().map(Identity::At).collect())
    }

    /// The expanded offsets to try for a cursor at byte `offset` of a file, the
    /// likelier name first. A cursor at the end of a word means that word, as a
    /// click or a typed character at its end gives, and not what encloses it.
    fn cursor_candidates(&self, ids: &[FileId], offset: u32) -> Vec<u32> {
        let virt = |at: u32| {
            ids.iter()
                .find_map(|id| self.expanded.expanded_offset(*id, at))
        };
        let here = virt(offset);
        let before = offset.checked_sub(1).and_then(virt);
        let bytes = self.expanded.text.as_bytes();
        let word = |at: Option<u32>| {
            at.and_then(|at| bytes.get(at as usize))
                .is_some_and(|byte| {
                    byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_') || *byte >= 0x80
                })
        };
        match (here, before) {
            (Some(here), Some(before)) if word(Some(before)) && !word(Some(here)) => {
                vec![before, here]
            }
            (here, before) => here.into_iter().chain(before).collect(),
        }
    }

    /// What the symbol stands for. Empty when oxabl cannot say where it is
    /// declared.
    fn symbol_identities(&self, sym: SymbolId) -> Vec<Identity> {
        let cached = self.identities.borrow().get(&sym).cloned();
        let identity = cached.unwrap_or_else(|| {
            let identity = self.symbol_identity(sym);
            self.identities.borrow_mut().insert(sym, identity.clone());
            identity
        });
        identity.into_iter().collect()
    }

    fn symbol_identity(&self, sym: SymbolId) -> Option<Identity> {
        let symbols = &self.model.semantic.symbols;
        let symbol = symbols.get(sym);
        if let Some(file) = symbols
            .program_file(sym)
            .or_else(|| symbols.class_file(sym))
        {
            return self.path_of(file).map(Identity::File);
        }
        if let Some(file) = symbols.member_file(sym) {
            let name = str::to_ascii_lowercase(&symbol.name);
            return self.path_of(file).map(|path| Identity::Member(path, name));
        }
        if let Some(table) = symbols.field_table(sym) {
            return Some(self.field_identity(table, symbol.name.as_ref()));
        }
        // Only the default buffer of a table is the table. A buffer the code
        // declares (`DEFINE BUFFER`, the implicit buffer of `FOR EACH`) is its
        // own record scope, whatever it is named.
        if symbol.declaration == NodeId::DUMMY {
            return self.table_of(symbol).map(Identity::Table);
        }
        Some(Identity::Declared(sym))
    }

    /// The field by its full name, so an abbreviation names the same field.
    fn field_identity(&self, table: TableId, name: &str) -> Identity {
        let full = self.run.config().schema.get_by_id(table).and_then(|found| {
            match found.resolve_field_by_name(name) {
                FieldResolution::Unique(field) => Some(str::to_ascii_lowercase(&field.name)),
                _ => None,
            }
        });
        Identity::Field(table, full.unwrap_or_else(|| str::to_ascii_lowercase(name)))
    }

    /// The targets of the innermost name at expanded offset `virt`. `None` when
    /// no name there has an answer, `Some(empty)` when the name has no target.
    fn answer_at(&self, virt: u32) -> Option<Vec<Target>> {
        self.innermost(
            virt,
            &|sym| self.symbol_targets(sym),
            &|target, span| self.site_targets(target, span),
            &|targets| !targets.is_empty(),
            Vec::new,
        )
    }

    /// What the innermost name at expanded offset `virt` refers to, as
    /// `symbol` and `site` answer for it. `found` tells an answer from an
    /// answer of "nothing"; `nothing` makes the latter. `None` when no name
    /// there has an answer.
    fn innermost<T>(
        &self,
        virt: u32,
        symbol: &dyn Fn(SymbolId) -> T,
        site: &dyn Fn(&SiteTarget, VirtualSpan) -> Option<T>,
        found: &dyn Fn(&T) -> bool,
        nothing: fn() -> T,
    ) -> Option<T> {
        let symbols = &self.model.semantic.symbols;
        let mut sites: Vec<_> = symbols
            .sites()
            .iter()
            .filter(|site| contains(site.span, virt))
            .collect();
        sites.sort_by_key(|site| site.span.end - site.span.start);
        let declaration = self.declaration_at(virt);
        // Sites of one length name one thing; the first with an answer wins, and
        // one that answers "nothing" only stops the search when none has one.
        let mut at = 0;
        while at < sites.len() {
            let length = sites[at].span.end - sites[at].span.start;
            if let Some((sym, symbol_at)) = declaration
                && symbol_at.name_span.end - symbol_at.name_span.start <= length
            {
                return Some(symbol(sym));
            }
            let mut answered = false;
            while at < sites.len() && sites[at].span.end - sites[at].span.start == length {
                match site(&sites[at].target, sites[at].span) {
                    Some(answer) if found(&answer) => return Some(answer),
                    Some(_) => answered = true,
                    None => {}
                }
                at += 1;
            }
            if answered {
                return Some(nothing());
            }
        }
        declaration.map(|(sym, _)| symbol(sym))
    }

    /// The declaration whose own name holds expanded offset `virt`.
    fn declaration_at(&self, virt: u32) -> Option<(SymbolId, &oxabl_semantic::Symbol)> {
        self.model
            .semantic
            .symbols
            .iter()
            .filter(|(_, symbol)| {
                symbol.declaration != NodeId::DUMMY && contains(symbol.name_span, virt)
            })
            .min_by_key(|(_, symbol)| symbol.name_span.end - symbol.name_span.start)
    }

    /// The expanded unit, lexed and parsed once.
    fn parsed(&self) -> &Parsed {
        self.parsed.get_or_init(|| Parsed::of(&self.expanded.text))
    }

    fn site_targets(&self, target: &SiteTarget, span: VirtualSpan) -> Option<Vec<Target>> {
        match target {
            SiteTarget::Reference(node) => match self.model.semantic.references.get(*node)? {
                Resolution::Resolved(sym) => Some(self.symbol_targets(*sym)),
                Resolution::Unresolved { .. } => Some(Vec::new()),
            },
            SiteTarget::Run(node) => match self.model.semantic.references.get(*node)? {
                // A program is never run through a handle.
                Resolution::Resolved(sym)
                    if self.model.semantic.symbols.program_file(*sym).is_some() =>
                {
                    Some(self.symbol_targets(*sym))
                }
                _ if let Some(targets) = self.handle_call_targets(span) => Some(targets),
                Resolution::Resolved(sym) => Some(self.symbol_targets(*sym)),
                Resolution::Unresolved {
                    reason: UnresolvedReason::Unknowable,
                    ..
                } => Some(self.candidates(span, NameKind::Program)),
                Resolution::Unresolved { .. } => Some(Vec::new()),
            },
            SiteTarget::Using(node) => {
                self.type_targets(self.model.semantic.references.get(*node)?, span)
            }
            SiteTarget::Type(resolution) => self.type_targets(resolution, span),
            SiteTarget::Symbol(sym) => Some(self.symbol_targets(*sym)),
            SiteTarget::Table(table) => Some(self.table_target(*table).into_iter().collect()),
            SiteTarget::SchemaField(table) => {
                let name = self.text(span)?;
                Some(self.field_target(*table, name))
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

    /// The call at `span` when it is a `RUN p IN h` through a handle: the
    /// handle's program is known, or the handle cannot be followed. `None` for
    /// every other `RUN`, `THIS-PROCEDURE` included.
    fn handle_call_target(&self, span: VirtualSpan) -> Option<&CallTarget> {
        let calls = self.model.calls.get_or_init(|| {
            let parsed = self.parsed();
            FileCalls::extract(
                &parsed.program.statements,
                &self.expanded.text,
                &parsed.tokens,
            )
        });
        let site = calls.sites.iter().find(|site| {
            matches!(
                site.kind,
                CallKind::Run | CallKind::Persistent | CallKind::Remote
            ) && site.at.start <= span.start
                && span.end <= site.at.end
        })?;
        match &site.target {
            target @ (CallTarget::InProgram { .. }
            | CallTarget::Unknown {
                reason: UnknownCall::Handle,
                ..
            }) => Some(target),
            _ => None,
        }
    }

    /// The targets of a `RUN p IN h` whose name is at `span`. `None` when the
    /// `RUN` there names no handle, or names `THIS-PROCEDURE`.
    fn handle_call_targets(&self, span: VirtualSpan) -> Option<Vec<Target>> {
        match self.handle_call_target(span)? {
            CallTarget::InProgram { program, procedure } => {
                Some(self.procedure_targets(program, procedure))
            }
            _ => Some(Vec::new()),
        }
    }

    /// Internal procedure `procedure` in each file that supplies `program`, or
    /// the file when its text declares no such procedure.
    fn procedure_targets(&self, program: &str, procedure: &str) -> Vec<Target> {
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
        let mut targets = Vec::new();
        for path in paths {
            let spans: Vec<Span> = self
                .outline_of(&path)
                .iter()
                .flat_map(|symbols| symbols.iter())
                .filter(|symbol| {
                    symbol.kind == OutlineKind::Procedure
                        && symbol.name.eq_ignore_ascii_case(procedure)
                })
                .map(|symbol| symbol.selection_span)
                .collect();
            if spans.is_empty() {
                targets.push(Target { path, span: None });
                continue;
            }
            for span in spans {
                targets.push(Target {
                    path: path.clone(),
                    span: Some(span),
                });
            }
        }
        targets
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
    fn text(&self, span: VirtualSpan) -> Option<&str> {
        self.expanded
            .text
            .get(span.start as usize..span.end as usize)
    }

    /// Where `sym` is declared. A buffer named like the table it reads (the
    /// buffer `FOR EACH customer` declares) means the table.
    fn symbol_targets(&self, sym: SymbolId) -> Vec<Target> {
        let symbols = &self.model.semantic.symbols;
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
        if let Some(table) = self.table_of(symbol) {
            return self.table_target(table).into_iter().collect();
        }
        if symbol.declaration == NodeId::DUMMY {
            return Vec::new();
        }
        self.place(symbol.name_span).into_iter().collect()
    }

    /// The schema table a buffer means: one the analysis made for a table name
    /// (`FOR EACH customer`), or one named like the table it reads.
    fn table_of(&self, symbol: &oxabl_semantic::Symbol) -> Option<oxabl_schema::TableId> {
        let table_named = matches!(symbol.kind, SymbolKind::Buffer | SymbolKind::TempTable)
            && (symbol.declaration == NodeId::DUMMY || self.names_its_table(symbol));
        symbol.table_id.filter(|_| table_named)
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

    fn path_of(&self, file: IndexedFileId) -> Option<PathBuf> {
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
    fn place(&self, span: VirtualSpan) -> Option<Target> {
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

    fn outline_of(&self, path: &Path) -> Option<Rc<Vec<OutlineSymbol>>> {
        if let Some(known) = self.outlines.borrow().get(path) {
            return known.clone();
        }
        let read = self.run.file_system().read(path).ok();
        let found = read.and_then(|text| outline(&text).ok()).map(Rc::new);
        self.outlines
            .borrow_mut()
            .insert(path.to_path_buf(), found.clone());
        found
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

/// The spans of `uses` that contain no other span of it, one per span, a write
/// when any use of it writes. A call site covers its whole call as well as its
/// name; only the name is a use.
fn innermost(mut uses: Vec<(VirtualSpan, bool)>) -> Vec<(VirtualSpan, bool)> {
    uses.sort_by_key(|(span, _)| (span.start, std::cmp::Reverse(span.end)));
    let mut merged: Vec<(VirtualSpan, bool)> = Vec::with_capacity(uses.len());
    for (span, write) in uses {
        match merged.last_mut() {
            Some((last, last_write)) if *last == span => *last_write |= write,
            _ => merged.push((span, write)),
        }
    }
    // Later entries start at or after an entry's start, so the entry contains
    // one when the least end after it is within its own end.
    let mut least_end = u32::MAX;
    let mut keep = vec![false; merged.len()];
    for (at, (span, _)) in merged.iter().enumerate().rev() {
        keep[at] = least_end > span.end;
        least_end = least_end.min(span.end);
    }
    merged
        .into_iter()
        .zip(keep)
        .filter_map(|(use_, keep)| keep.then_some(use_))
        .collect()
}

fn dedupe(targets: Vec<Target>) -> Vec<Target> {
    let mut kept: Vec<Target> = Vec::with_capacity(targets.len());
    for target in targets {
        if !kept.contains(&target) {
            kept.push(target);
        }
    }
    kept
}
