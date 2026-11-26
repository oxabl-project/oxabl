//! Fact extraction and the batch workspace index.
//!
//! This crate is the *implementation* side of the seam
//! `oxabl_semantic::index` defines: one routine, [`index_file`], that turns a
//! referenced file into the facts the index answers with, and one plain in-run
//! cache over it, [`BatchIndex`], that dedups shared dependencies.
//!
//! # Why the extraction lives here and not in a client
//!
//! Both caches — this crate's `BatchIndex` and the language server's
//! salsa-backed one — call [`index_file`]. Only the memoization differs. That is
//! what makes R7 ("one shared question set, no client carve-outs, so every
//! client resolves identically and differs only in what files exist and how
//! answers are cached") structural rather than documented: there is no second
//! copy of the projection to drift.
//!
//! # What is not done here
//!
//! No panic guard. Every query is *total* in its answers — found, not-found,
//! unknowable — but totality is not licence to swallow unwinding: salsa's
//! `Cancelled` travels as a panic payload in this workspace, so a guard around a
//! lookup would turn a cancelled recompute into `NotFound` and freeze a buffer
//! on stale results. Cancellation propagates, exactly as it does through the
//! deliberately unguarded `LintPipeline::expand`/`collect`.
//!
//! Include expansion only when the text as written does not answer. A
//! referenced file is indexed as written first; a declaration that only exists
//! after an `{include}` splice is invisible there. That is the conservative
//! direction — a missing fact yields a missing link, which by the R11 firewall
//! produces no finding.
//!
//! The exception is a file whose text as written does not parse, is nothing but
//! includes and directives, or declares a class that names an include or holds
//! a `{&name}` or `{N}` reference (in its header, a member or a method). That
//! file is preprocessed, reading its includes through [`IncludeContext`], and the
//! result is used when it parses and every include was read. Facts then depend on
//! the file's text, on the includes read ([`index_file_with_includes`] names
//! them, so an incremental cache can invalidate on them), and on the configured
//! predefined names (`{&OPSYS}` and friends).
//!
//! So a file with code of its own does not produce a `SHARED` name that only an
//! include defines: a `DEFINE NEW SHARED` spliced into such a file is not found.
//! A header that defines a shared name is included by many programs, and
//! counting each of them as a producer would make the name ambiguous.
//!
//! The daemon and the language server invalidate facts on the includes the
//! expansion read or could not read. An include that was not found is not an
//! input: creating it later, or adding an earlier include path that shadows a
//! resolved include, does not re-extract the facts until the file itself
//! changes. The language server also watches only `*.i` files, so an include
//! with another extension is not seen to change.

mod batch;
pub mod calls;
mod closure;
pub mod declared;
mod deps;
mod expansion_scan;
mod facts;
pub mod hierarchy;
mod producers;
// Public because a second cache lives outside this crate: the language server's
// salsa-backed index does its own memoization but must use the *same*
// name-to-path policy — two spellings tried in order, exactly one match, `.i`
// never a root, no escaping the configured paths. A private module would force
// that policy to be written twice, which is precisely the drift R7 exists to
// prevent.
pub mod search;
mod written;

pub use batch::{BatchIndex, ExcludingFile, SearchingFrom};
pub use closure::{
    ClosureExpansions, ClosureTarget, ConditionalIncludeName, conditional_include_closure,
    conditional_include_closure_cached, conditional_include_closure_with,
};
pub use deps::{
    BranchKind, ClosureReference, Completeness, ConditionalBranch, ConditionalInclude,
    ConditionalIncludeClosure, ConditionalReference, DegradedReason, DependencyEdge,
    DependencyEdges, DirectIncludeInput, EdgeInputs, EdgeKind, EdgeTarget, IncludeName,
    IncludeOriginInput, NestedUnresolvedIncludeInput, UnreadableIncludeInput,
    UnresolvedIncludeInput, UnresolvedReference, build_edge_set,
};
pub use facts::{ClassFacts, FileFacts};
pub use producers::{DeferredProducers, FileScan, SharedProducers, definitions_in};
pub use written::{ReferenceSyntax, WrittenReference, written_imports, written_references};

use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use oxabl_ast::{
    AccessModifier, BufferTarget, DataType, ParameterExtent, Statement, StatementKind, TypeSource,
};
use oxabl_common::FileId;
use oxabl_lexer::{Kind, Token};
use oxabl_parser::{Parser, Program};
use oxabl_preprocessor::{ElidedKind, PreprocessedFile, Preprocessor, SpanNode};
use oxabl_schema::Schema;
use oxabl_semantic::{
    AnalysisContext, ClassDescriptor, ClassKind, ImportContext, IndexName, IndexedFileId,
    MemberDescriptor, MemberParamShape, MemberRecord, MemberType, ProgramSignatures, ResolvedType,
    ScopeId, SymbolFlags, SymbolKind, declare_pass, member_params,
};
use oxabl_workspace::{FileSystem, PreprocessorConfig};
use rustc_hash::FxHashMap;

/// The [`FileId`] the declare pass is run under while indexing.
///
/// Every diagnostic the pass produces is discarded — a referenced file's errors
/// belong to *that* file's own analysis, not to the file that referenced it — so
/// this id never reaches a rendered span. It exists because
/// [`AnalysisContext`] requires one.
const INDEXED_FILE: FileId = FileId::new(1);

/// Where an indexed file's `{include}` references are read from, and the
/// environment its directives are applied under.
#[derive(Clone, Copy)]
pub struct IncludeContext<'a> {
    pub fs: &'a dyn FileSystem,
    pub include_paths: &'a [PathBuf],
    /// Paths resolved against the base directory of the file being indexed and searched
    /// before `include_paths`; see [`search::root_include_paths`].
    pub root_relative_paths: &'a [PathBuf],
    /// Answers the AVM's predefined names (`{&OPSYS}` and friends).
    pub predefined: &'a PreprocessorConfig,
}

impl<'a> IncludeContext<'a> {
    /// A context that reads no include: only the file's own directives apply.
    pub fn in_file(predefined: &'a PreprocessorConfig) -> Self {
        IncludeContext {
            fs: &NoFiles,
            include_paths: &[],
            root_relative_paths: &[],
            predefined,
        }
    }

    /// The search paths of the file at `file` with the text `source`: the
    /// root-relative paths against its base directory
    /// ([`search::search_base`]), then the include paths.
    pub fn paths_for(&self, file: &Path, source: &str) -> std::borrow::Cow<'a, [PathBuf]> {
        if self.root_relative_paths.is_empty() {
            return std::borrow::Cow::Borrowed(self.include_paths);
        }
        std::borrow::Cow::Owned(search::root_include_paths(
            search::search_base(file, source).as_deref(),
            self.root_relative_paths,
            self.include_paths,
        ))
    }
}

/// [`index_file`] for the file at `path`, its includes read with the search
/// paths of its base directory.
pub fn index_file_at(
    file: IndexedFileId,
    path: &Path,
    source: &str,
    includes: &IncludeContext<'_>,
) -> (FileFacts, Vec<PathBuf>) {
    let paths = includes.paths_for(path, source);
    let includes = IncludeContext {
        include_paths: &paths,
        root_relative_paths: &[],
        ..*includes
    };
    index_file_with_includes(file, source, &includes)
}

/// The parameter lists of the file at `path`, and the include files they were
/// read through (as [`index_file_with_includes`] reports them).
///
/// A list the text as written does not show whole (an `{include}` may hold a
/// parameter) is read again from the expanded text; when that does not settle
/// either, the list stays unknown. `None` when the file does not parse.
pub fn program_signatures_at(
    path: &Path,
    source: &str,
    includes: &IncludeContext<'_>,
) -> (Option<ProgramSignatures>, Vec<PathBuf>) {
    fn parse(source: &str) -> Option<ProgramSignatures> {
        let tokens = oxabl_lexer::tokenize(source);
        let program = Parser::new(&tokens, source).parse_program();
        program
            .is_ok()
            .then(|| ProgramSignatures::of(&program.statements))
    }
    let Some(written) = parse(source) else {
        return (None, Vec::new());
    };
    if written.main.is_some() && written.procedures.iter().all(|(_, s)| s.is_some()) {
        return (Some(written), Vec::new());
    }
    let paths = includes.paths_for(path, source);
    let includes = IncludeContext {
        include_paths: &paths,
        root_relative_paths: &[],
        ..*includes
    };
    let Some(expansion) = expand(source, &includes) else {
        return (Some(written), Vec::new());
    };
    let read = included_paths(&expansion.processed);
    let signatures = expansion
        .text
        .and_then(|text| parse(&text))
        .map(|expanded| expanded.marking_includes(&written));
    (Some(signatures.unwrap_or(written)), read)
}

/// A filesystem with nothing in it.
struct NoFiles;

impl FileSystem for NoFiles {
    fn read(&self, _path: &Path) -> Result<Arc<str>, io::Error> {
        Err(io::ErrorKind::NotFound.into())
    }

    fn exists(&self, _path: &Path) -> bool {
        false
    }
}

/// Extract the facts `source` contributes to the index, under the identity
/// `file`.
///
/// Tokenizes, parses, and runs the **declare pass**, then projects the result
/// down to [`FileFacts`] and drops everything else.
///
/// A parse that recovered any error yields [`FileFacts::unparseable`] rather
/// than partial facts. Error recovery resynchronizes on period boundaries, so a
/// broken statement can leave a class body missing members or a member carrying
/// the wrong type — and a *wrong* fact is poison in a way a missing one is not:
/// it mis-attributes symbols across the program graph, while a missing fact just
/// leaves a name unresolved and silent.
///
/// When the text as written does not answer, the file is preprocessed through
/// `includes` and indexed again; see the module docs.
///
/// Not guarded against panics, on purpose — see the module docs.
pub fn index_file(file: IndexedFileId, source: &str, includes: &IncludeContext<'_>) -> FileFacts {
    index_file_with_includes(file, source, includes).0
}

/// [`index_file`], with the include files the facts were read through.
///
/// The paths are every include the expansion read or tried to read, lexically
/// normalized, sorted, and empty when the file was not expanded. An edit to any
/// of them can change the facts, including from unusable to usable.
pub fn index_file_with_includes(
    file: IndexedFileId,
    source: &str,
    includes: &IncludeContext<'_>,
) -> (FileFacts, Vec<PathBuf>) {
    let written = Written::parse(source);
    if let Some(written) = written.as_ref().filter(|w| !w.needs_expansion()) {
        return (written.facts(file, None), Vec::new());
    }
    // The facts of the text as written are read only when the expansion does
    // not give them.
    let as_written = |values: Option<&KnownValues>| {
        written
            .as_ref()
            .map_or_else(|| FileFacts::unparseable(file), |w| w.facts(file, values))
    };
    let Some(expansion) = expand(source, includes) else {
        return (as_written(None), Vec::new());
    };
    let read = included_paths(&expansion.processed);
    let expanded = expansion
        .text
        .as_deref()
        .and_then(Written::parse)
        .map(|expanded| expanded.facts(file, None));
    match expanded {
        Some(expanded) => (expanded, read),
        // The expansion settled and its text does not parse: the values of
        // the references are known, so the text as written can still be whole.
        None if expansion.text.is_some() => (
            as_written(Some(&KnownValues::of(&expansion.processed))),
            read,
        ),
        None => (as_written(None), read),
    }
}

/// The values of the `{&name}` and `{N}` references of the file itself, by
/// the offset where each starts, from an expansion that settled. An empty
/// reference has the value "".
struct KnownValues(FxHashMap<u32, String>);

impl KnownValues {
    fn of(processed: &PreprocessedFile) -> Self {
        fn text(processed: &PreprocessedFile, nodes: &[SpanNode], out: &mut String) {
            for node in nodes {
                match node {
                    SpanNode::Chunk { file, start, end } => {
                        if let Some(source) = processed.source(*file) {
                            out.push_str(&source[*start as usize..*end as usize]);
                        }
                    }
                    SpanNode::Include { children, .. } => text(processed, children, out),
                }
            }
        }
        let mut values = FxHashMap::default();
        for node in &processed.tree {
            if let SpanNode::Include {
                site,
                path: None,
                children,
            } = node
                && site.file == INDEXED_FILE
            {
                let mut value = String::new();
                text(processed, children, &mut value);
                values.insert(site.span.start, value);
            }
        }
        for elided in &processed.elided {
            if elided.within.is_none()
                && elided.site.file == INDEXED_FILE
                && matches!(elided.kind, ElidedKind::Name | ElidedKind::Argument)
            {
                values.entry(elided.site.span.start).or_default();
            }
        }
        KnownValues(values)
    }

    /// Whether the reference at `start` has a known value that cannot end a
    /// statement or open a block, so it cannot add a declaration.
    fn harmless(&self, start: usize) -> bool {
        let Some(value) = self.0.get(&(start as u32)) else {
            return false;
        };
        let bytes = value.as_bytes();
        !bytes.iter().enumerate().any(|(at, b)| {
            matches!(b, b'.' | b':')
                && bytes
                    .get(at + 1)
                    .is_none_or(|next| next.is_ascii_whitespace())
        })
    }
}

/// Whether [`index_file`] may read the facts of `source` from its expanded text:
/// a superset of the files it expands. A class or interface header answers from
/// the tokens; any other file is parsed.
pub(crate) fn may_be_expanded(source: &str) -> bool {
    may_be_expanded_tokens(source, &oxabl_lexer::tokenize(source))
}

/// [`may_be_expanded`] of `source` with its tokens.
pub(crate) fn may_be_expanded_tokens(source: &str, tokens: &[Token]) -> bool {
    expansion_scan::may_declare_a_type(tokens) || expansion_scan::parsed(source, tokens)
}

/// [`may_be_expanded`] of a text already parsed.
pub(crate) fn may_be_expanded_parsed(program: &Program) -> bool {
    !program.is_ok()
        || only_includes(&program.statements)
        || program.statements.iter().any(|stmt| {
            matches!(
                stmt.kind,
                StatementKind::Class { .. } | StatementKind::Interface { .. }
            )
        })
}

/// The names the declare pass gives the `NEW [GLOBAL] SHARED` flags in
/// `statements`, the text of `source`: what [`index_file`] reports.
pub(crate) fn declared_shared(statements: &[Statement], source: &str) -> Vec<IndexName> {
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(INDEXED_FILE, source, &schema);
    let (_scopes, symbols, _diagnostics, _schema_revision) = declare_pass(statements, &ctx);
    symbols
        .iter()
        .filter(|(_, symbol)| {
            symbol
                .flags
                .intersects(SymbolFlags::NEW_SHARED | SymbolFlags::NEW_GLOBAL_SHARED)
        })
        .map(|(_, symbol)| IndexName::new(&symbol.name))
        .collect()
}

/// The expanded text of `source`, the file at `path`, with its includes read as
/// [`index_file_at`] reads them, or `None` when the expansion does not settle.
pub(crate) fn expanded_text_at(
    path: &Path,
    source: &str,
    includes: &IncludeContext<'_>,
) -> Option<String> {
    let paths = includes.paths_for(path, source);
    let includes = IncludeContext {
        include_paths: &paths,
        root_relative_paths: &[],
        ..*includes
    };
    expand(source, &includes)?.text
}

/// One preprocessor run over an indexed file.
struct Expansion {
    processed: PreprocessedFile,
    /// The expanded text, or `None` when the run did not settle it.
    text: Option<String>,
}

/// `source` with its directives applied and its includes read through
/// `includes`.
///
/// The fallback for a file whose declaration is built from `&GLOBAL-DEFINE`
/// names (`CLASS {&fullName}:`) or spliced in from an `{include}`. `None` when
/// the preprocessor failed outright.
fn expand(source: &str, includes: &IncludeContext<'_>) -> Option<Expansion> {
    oxabl_common::test_tally::count("index.expand");
    let processed = Preprocessor::new(includes.fs, includes.include_paths)
        .with_predefined(includes.predefined)
        .process(INDEXED_FILE, source)
        .ok()?;
    // An include left unexpanded could have declared part of the class or
    // defined a name a later `&IF` reads, so the facts would be a guess.
    let guessed = !processed.diagnostics.is_empty()
        || !processed.unresolved_includes.is_empty()
        || !processed.unreadable_includes.is_empty();
    let text = (!guessed).then(|| processed.to_text().to_string());
    Some(Expansion { processed, text })
}

/// Every include file the run read or failed to read, normalized and sorted.
fn included_paths(processed: &PreprocessedFile) -> Vec<PathBuf> {
    fn walk(nodes: &[SpanNode], out: &mut Vec<PathBuf>) {
        for node in nodes {
            if let SpanNode::Include { path, children, .. } = node {
                if let Some(path) = path {
                    out.push(search::normalize_lexically(path));
                }
                walk(children, out);
            }
        }
    }
    let mut paths = Vec::new();
    walk(&processed.tree, &mut paths);
    paths.extend(
        processed
            .unreadable_includes
            .iter()
            .map(|include| search::normalize_lexically(&include.path)),
    );
    paths.sort();
    paths.dedup();
    paths
}

/// A text that parses with no recovered error: its tokens and statements.
struct Written<'s> {
    source: &'s str,
    tokens: Vec<Token>,
    program: oxabl_parser::Program,
}

impl<'s> Written<'s> {
    /// `None` when the parse recovered an error.
    fn parse(source: &'s str) -> Option<Self> {
        let tokens = oxabl_lexer::tokenize(source);
        let program = Parser::new(&tokens, source).parse_program();
        program.is_ok().then_some(Written {
            source,
            tokens,
            program,
        })
    }

    /// Whether includes or preprocessor names may hold part of what the text
    /// declares: it is nothing but includes and directives, or it declares a
    /// class or interface and names an include anywhere or holds a `{&name}`
    /// or `{N}` reference in the class (a header, a member, a method body).
    fn needs_expansion(&self) -> bool {
        let is_class = |stmt: &&Statement| {
            matches!(
                stmt.kind,
                StatementKind::Class { .. } | StatementKind::Interface { .. }
            )
        };
        let statements = &self.program.statements;
        only_includes(statements)
            || (statements.iter().any(|stmt| is_class(&stmt))
                && self.tokens.iter().any(|t| t.kind == Kind::IncludeReference))
            || statements
                .iter()
                .filter(is_class)
                .any(|stmt| holds_reference(stmt, &self.tokens, self.source))
    }

    /// The facts of the text. `values` are the values of its references, when
    /// an expansion knows them.
    fn facts(&self, file: IndexedFileId, values: Option<&KnownValues>) -> FileFacts {
        // The declare pass is the same machinery a client runs over the file it is
        // analyzing, which is what keeps an indexed declaration and a local one from
        // being recognized by two different sets of rules. It is also why the
        // routine-level facts below read the *symbol table* rather than the
        // statement list: a `FUNCTION` prototype and its later definition merge into
        // one symbol there (#69), and a name the pass refused to declare twice
        // appears once.
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(INDEXED_FILE, self.source, &schema);
        let (_scopes, symbols, _diagnostics, _schema_revision) =
            declare_pass(&self.program.statements, &ctx);

        let mut procedures = Vec::new();
        let mut functions = Vec::new();
        let mut shared_definitions = Vec::new();
        for (_id, symbol) in symbols.iter() {
            // `IndexName::new` folds, and the atom is already folded, so this is an
            // idempotent second pass over ASCII bytes plus an intern-table hit.
            if symbol.kind == SymbolKind::Procedure {
                procedures.push(IndexName::new(&symbol.name));
            } else if symbol.kind == SymbolKind::Function && symbol.declared_in == ScopeId::ROOT {
                // A method carries `SymbolKind::Function` too, but is declared in
                // its class's scope and is reported as a member instead.
                functions.push(IndexName::new(&symbol.name));
            }
            if symbol
                .flags
                .intersects(SymbolFlags::NEW_SHARED | SymbolFlags::NEW_GLOBAL_SHARED)
            {
                shared_definitions.push(IndexName::new(&symbol.name));
            }
        }

        let mut imports = ImportContext::from_statements(&self.program.statements);
        if include_before_header(&self.tokens, &self.program.statements) {
            imports = imports.with_unseen_imports();
        }

        FileFacts {
            file,
            parsed: true,
            classes: declared_classes(
                file,
                &self.program.statements,
                &self.tokens,
                self.source,
                values,
                &Arc::new(imports),
            )
            .into_iter()
            .chain(declared_enums(file, &self.tokens, self.source))
            .collect(),
            procedures,
            functions,
            shared_definitions,
        }
    }
}

/// Whether `statements` are includes and directives, with at least one include:
/// a file whose code all comes from what it includes.
fn only_includes(statements: &[Statement]) -> bool {
    fn walk(statements: &[Statement], includes: &mut bool) -> bool {
        statements.iter().all(|stmt| match &stmt.kind {
            StatementKind::IncludeReference { .. } | StatementKind::IncludeArgReference { .. } => {
                *includes = true;
                true
            }
            StatementKind::PreprocDefine { .. }
            | StatementKind::PreprocUndefine { .. }
            | StatementKind::PreprocMessage { .. } => true,
            StatementKind::PreprocIf(branches) => {
                walk(&branches.then_branch, includes)
                    && branches
                        .elseif_branches
                        .iter()
                        .all(|(_, branch)| walk(branch, includes))
                    && branches
                        .else_branch
                        .as_ref()
                        .is_none_or(|branch| walk(branch, includes))
            }
            _ => false,
        })
    }
    let mut includes = false;
    walk(statements, &mut includes) && includes
}

/// Whether an `{include}` stands ahead of the first class or interface header
/// statement, where it could bring in `USING` statements the parse does not see.
///
/// The header is found in the statements, not the tokens: `USING Progress.Lang.Class.`
/// holds a `CLASS` token that is no header. With no header statement at file
/// level, an include anywhere counts.
fn include_before_header(tokens: &[Token], statements: &[Statement]) -> bool {
    let header = statements
        .iter()
        .find(|stmt| {
            matches!(
                stmt.kind,
                StatementKind::Class { .. } | StatementKind::Interface { .. }
            )
        })
        .map_or(usize::MAX, |stmt| stmt.span.start as usize);
    tokens
        .iter()
        .take_while(|t| t.start < header)
        .any(|t| t.kind == Kind::IncludeReference)
}

/// Project the class and interface declarations of `statements`.
///
/// Read from the AST rather than from the declare pass's symbols, because the
/// two facts the seam needs are precisely the two the pass discards: it keeps no
/// record of a class header (`INHERITS` / `IMPLEMENTS` are not symbols), and it
/// lowers a class-typed declaration to
/// [`ResolvedType::Unknown`](oxabl_semantic::ResolvedType::Unknown) by design,
/// dropping the type *name* that [`MemberType::Named`] exists to carry. The AST
/// spelling — an unresolved name, as the declaring file wrote it — is exactly
/// what a cross-file answer must carry anyway, since resolving it is the
/// consumer's job in the consumer's own symbol space.
///
/// File level only: a class nested inside a preprocessor `&IF` branch is not
/// indexed, which is the conservative direction (a missing fact, not a wrong
/// one).
///
/// A class that names no supertype and no class-typed member or parameter
/// carries no imports, so an edit to the file's `USING` list leaves its
/// descriptor, and every answer keyed on it, unchanged.
fn declared_classes(
    file: IndexedFileId,
    statements: &[Statement],
    tokens: &[Token],
    source: &str,
    values: Option<&KnownValues>,
    imports: &Arc<ImportContext>,
) -> Vec<ClassFacts> {
    let imports_for = |has_supertype: bool, members: &[MemberDescriptor]| {
        if has_supertype || members.iter().any(names_a_class) {
            Arc::clone(imports)
        } else {
            Arc::default()
        }
    };
    let mut classes = Vec::new();
    for stmt in statements {
        if let StatementKind::Class {
            name,
            inherits,
            implements,
            body,
            is_abstract,
            ..
        } = &stmt.kind
        {
            let members = declared_members(body, tokens, source);
            classes.push(ClassFacts {
                descriptor: Arc::new(ClassDescriptor {
                    name: IndexName::new(&name.name),
                    file,
                    kind: ClassKind::Class,
                    inherits: inherits.as_ref().map(|id| IndexName::new(&id.name)),
                    implements: implements
                        .iter()
                        .map(|id| IndexName::new(&id.name))
                        .collect(),
                    imports: imports_for(inherits.is_some() || !implements.is_empty(), &members),
                    members_complete: members_readable(stmt, body, tokens, source, values),
                    is_abstract: *is_abstract,
                }),
                members,
            });
        } else if let StatementKind::Interface {
            name,
            inherits,
            body,
        } = &stmt.kind
        {
            let members = declared_members(body, tokens, source);
            classes.push(ClassFacts {
                descriptor: Arc::new(ClassDescriptor {
                    name: IndexName::new(&name.name),
                    file,
                    kind: ClassKind::Interface,
                    // An interface may extend *several* interfaces, so its
                    // `INHERITS` list cannot fit `inherits: Option<_>`. The list
                    // goes to `implements`, which is already "the other
                    // supertypes, as a set" — no information is lost and the
                    // consumer's walk is unchanged, whereas picking a first
                    // parent and discarding the rest would silently drop
                    // members.
                    inherits: None,
                    implements: inherits.iter().map(|id| IndexName::new(&id.name)).collect(),
                    imports: imports_for(!inherits.is_empty(), &members),
                    members_complete: members_readable(stmt, body, tokens, source, values),
                    is_abstract: false,
                }),
                members,
            });
        }
    }
    classes
}

/// Whether a member or one of its parameters names a class, which the
/// declaring file's imports give a meaning.
fn names_a_class(member: &MemberDescriptor) -> bool {
    matches!(member.ty, MemberType::Named(_))
        || member.params.as_deref().is_some_and(|params| {
            params
                .iter()
                .any(|p| matches!(p.shape, MemberParamShape::Value(MemberType::Named(_))))
        })
}

/// Whether the members of a class are all that it declares: no `{include}`
/// or `&IF` can add one ([`hides_members`]), and no preprocessor reference in
/// the class can. A reference anywhere in a class, also inside a declaration,
/// can expand to a period and a new declaration, so it counts unless `values`
/// know a value of it that cannot end a statement. A file with a reference is
/// expanded ([`index_file_with_includes`]), and the expanded text has none.
fn members_readable(
    class: &Statement,
    body: &[Statement],
    tokens: &[Token],
    source: &str,
    values: Option<&KnownValues>,
) -> bool {
    !hides_members(body)
        && !references(class, tokens, source)
            .any(|t| !values.is_some_and(|values| values.harmless(t.start)))
}

/// The `{...}` references in the text of `stmt`.
fn references<'t>(
    stmt: &Statement,
    tokens: &'t [Token],
    source: &'t str,
) -> impl Iterator<Item = &'t Token> {
    let (start, end) = (stmt.span.start as usize, stmt.span.end as usize);
    let from = tokens.partition_point(|t| t.start < start);
    tokens[from..]
        .iter()
        .take_while(move |t| t.start < end)
        .filter(move |t| source[t.start..t.end].starts_with('{'))
}

/// Whether a `{...}` reference stands in the text of `stmt`.
fn holds_reference(stmt: &Statement, tokens: &[Token], source: &str) -> bool {
    references(stmt, tokens, source).next().is_some()
}

/// Whether a class body may declare more members than its statements show: it
/// names an `{include}` at member level, or holds an `&IF` around a member
/// declaration. The facts of an expanded text hold neither.
fn hides_members(body: &[Statement]) -> bool {
    fn declares(stmt: &Statement) -> bool {
        match &stmt.kind {
            StatementKind::Block(inner) | StatementKind::VariableDeclarationList(inner) => {
                inner.iter().any(declares)
            }
            StatementKind::PreprocIf(branches) => branches
                .then_branch
                .iter()
                .chain(branches.elseif_branches.iter().flat_map(|(_, b)| b))
                .chain(branches.else_branch.iter().flatten())
                .any(declares),
            StatementKind::IncludeReference { .. }
            | StatementKind::IncludeArgReference { .. }
            | StatementKind::Method { .. }
            | StatementKind::Property { .. }
            | StatementKind::VariableDeclaration { .. }
            | StatementKind::DefineTempTable { .. }
            | StatementKind::DefineBuffer { .. }
            | StatementKind::DefineDataset { .. }
            | StatementKind::DefineDataSource { .. }
            | StatementKind::DefineEvent { .. } => true,
            _ => false,
        }
    }
    body.iter().any(|stmt| match &stmt.kind {
        StatementKind::IncludeReference { .. } | StatementKind::IncludeArgReference { .. } => true,
        StatementKind::PreprocIf(_) => declares(stmt),
        StatementKind::Block(inner) | StatementKind::VariableDeclarationList(inner) => {
            hides_members(inner)
        }
        _ => false,
    })
}

/// Project the `ENUM` declarations at file level.
///
/// The parser skips an enum body without keeping a node, so this reads the token
/// stream: an `ENUM` at the start of a statement is a declaration, and a
/// `DEFINE ENUM` inside it is a member list. A member list that does not follow
/// the grammar yields no members rather than a guessed one.
fn declared_enums(file: IndexedFileId, tokens: &[Token], source: &str) -> Vec<ClassFacts> {
    let text = |t: &Token| &source[t.start..t.end];
    let is_word = |t: &Token| text(t).starts_with(|c: char| c.is_alphabetic() || c == '_');
    let mut enums = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        let previous = tokens[..i].iter().rev().find(|t| t.kind != Kind::Comment);
        let at_statement_start = previous.is_none_or(|t| t.kind == Kind::Period);
        if !(at_statement_start && tokens[i].kind == Kind::KwEnum) {
            i += 1;
            continue;
        }
        let Some(first) = tokens.get(i + 1).filter(|t| is_word(t)) else {
            i += 1;
            continue;
        };
        let mut name = text(first).to_string();
        i += 2;
        while let (
            Some(Token {
                kind: Kind::Period, ..
            }),
            Some(next),
        ) = (tokens.get(i), tokens.get(i + 1).filter(|t| is_word(t)))
        {
            name.push('.');
            name.push_str(text(next));
            i += 2;
        }
        let mut members = Vec::new();
        let mut well_formed = true;
        let own_type = IndexName::new(&name);
        while i < tokens.len() && !(tokens[i].kind == Kind::End) {
            if tokens[i].kind == Kind::Define
                && tokens.get(i + 1).is_some_and(|t| t.kind == Kind::KwEnum)
            {
                let (names, next) = enum_members(&tokens[i + 2..], source, &own_type);
                well_formed &= names.is_some();
                members.extend(names.unwrap_or_default());
                i += 2 + next;
            } else {
                i += 1;
            }
        }
        enums.push(ClassFacts {
            descriptor: Arc::new(ClassDescriptor {
                name: IndexName::new(&name),
                file,
                kind: ClassKind::Enum,
                inherits: None,
                implements: Vec::new(),
                imports: Arc::default(),
                members_complete: well_formed,
                is_abstract: false,
            }),
            members: if well_formed {
                members.into()
            } else {
                Arc::from([])
            },
        });
    }
    enums
}

/// The member names of one `DEFINE ENUM` list, and how many tokens it spans.
///
/// `None` when the list is not `name [= value] [, ...]` with `|`-joined values.
/// Each member is a static property whose type is the enum itself.
fn enum_members(
    tokens: &[Token],
    source: &str,
    own_type: &IndexName,
) -> (Option<Vec<MemberDescriptor>>, usize) {
    let text = |t: &Token| &source[t.start..t.end];
    let mut members = Vec::new();
    let mut i = 0;
    let mut ok = true;
    while let Some(token) = tokens.get(i) {
        match token.kind {
            Kind::Period => {
                i += 1;
                break;
            }
            Kind::Comma => i += 1,
            _ if text(token).starts_with(|c: char| c.is_alphabetic() || c == '_') => {
                members.push(MemberDescriptor {
                    name: IndexName::new(text(token)),
                    kind: SymbolKind::Property,
                    ty: MemberType::Named(own_type.clone()),
                    access: AccessModifier::Public,
                    is_static: true,
                    record: None,
                    params: None,
                });
                i += 1;
                if tokens.get(i).is_some_and(|t| t.kind == Kind::Equals) {
                    i += 1;
                    if tokens.get(i).is_some_and(|t| t.kind == Kind::Minus) {
                        i += 1;
                    }
                    i += 1;
                    while tokens.get(i).is_some_and(|t| text(t) == "|") {
                        i += 2;
                    }
                }
            }
            _ => {
                ok = false;
                i += 1;
            }
        }
    }
    (ok.then_some(members), i)
}

/// Project the members a class or interface body declares itself: methods,
/// properties, and the data members a subclass reaches by name (variables,
/// buffers, temp-tables, datasets, data-sources, events).
///
/// A data member written without an access modifier is `PRIVATE`, as in ABL.
fn declared_members(body: &[Statement], tokens: &[Token], source: &str) -> Arc<[MemberDescriptor]> {
    let mut members = Vec::new();
    collect_members(body, tokens, source, &mut members);
    members.into()
}

/// Whether the parameter list after a method name that ends at `name_end`
/// holds an include or preprocessor reference. The parser skips such a
/// reference, so the statements of the list are not the whole list.
fn list_hides_params(tokens: &[Token], source: &str, name_end: usize) -> bool {
    let from = tokens.partition_point(|t| t.start < name_end);
    let mut list = tokens[from..].iter().filter(|t| t.kind != Kind::Comment);
    if !list.next().is_some_and(|t| t.kind == Kind::LeftParen) {
        return false;
    }
    let mut depth = 1usize;
    for token in list {
        match token.kind {
            Kind::LeftParen => depth += 1,
            Kind::RightParen => {
                depth -= 1;
                if depth == 0 {
                    return false;
                }
            }
            Kind::IncludeReference
            | Kind::IncludeArgReference
            | Kind::PreprocIf
            | Kind::PreprocElseif
            | Kind::PreprocElse
            | Kind::PreprocEndif
            | Kind::PreprocGlobalDefine
            | Kind::PreprocScopedDefine
            | Kind::PreprocUndefine
            | Kind::Eof => return true,
            // `{&name}`, which the lexer can also give a keyword kind.
            _ if source[token.start..token.end].starts_with('{') => return true,
            _ => {}
        }
    }
    true
}

fn collect_members(
    body: &[Statement],
    tokens: &[Token],
    source: &str,
    members: &mut Vec<MemberDescriptor>,
) {
    let data = |name: &oxabl_ast::Identifier,
                kind: SymbolKind,
                ty: MemberType,
                access: Option<AccessModifier>,
                is_static: bool| MemberDescriptor {
        name: IndexName::new(&name.name),
        kind,
        ty,
        access: access.unwrap_or(AccessModifier::Private),
        is_static,
        record: None,
        params: None,
    };
    for stmt in body {
        match &stmt.kind {
            StatementKind::Method {
                access,
                is_static,
                return_type,
                extent,
                name,
                parameters,
                ..
            } => members.push(MemberDescriptor {
                name: IndexName::new(&name.name),
                // The same kind the declare pass gives a locally declared
                // method, so a synthesized member is indistinguishable from a
                // local one downstream.
                kind: SymbolKind::Function,
                // `None` is `VOID`: the declaration names no type at all, which
                // is a different fact from naming one the index cannot carry.
                ty: return_type
                    .as_ref()
                    .map_or(MemberType::Untyped, |dt| member_type(dt, *extent)),
                access: *access,
                is_static: *is_static,
                record: None,
                params: if list_hides_params(tokens, source, name.span.end as usize) {
                    None
                } else {
                    member_params(parameters).map(Into::into)
                },
            }),
            StatementKind::Property {
                access,
                is_static,
                name,
                data_type,
                extent,
                ..
            } => members.push(MemberDescriptor {
                name: IndexName::new(&name.name),
                kind: SymbolKind::Property,
                ty: member_type(data_type, *extent),
                access: *access,
                is_static: *is_static,
                record: None,
                params: None,
            }),
            StatementKind::VariableDeclaration {
                name,
                type_source,
                extent,
                access,
                is_static,
                ..
            } => {
                let ty = match type_source {
                    TypeSource::Explicit(declared) => {
                        member_type(declared, extent.map(ParameterExtent::Fixed))
                    }
                    TypeSource::Like { .. } => MemberType::Untyped,
                };
                members.push(data(name, SymbolKind::Variable, ty, *access, *is_static));
            }
            StatementKind::DefineTempTable {
                name,
                access,
                is_static,
                ..
            } => members.push(data(
                name,
                SymbolKind::TempTable,
                MemberType::Untyped,
                *access,
                *is_static,
            )),
            StatementKind::DefineBuffer {
                name,
                target,
                access,
                is_static,
                ..
            } => {
                let (table, temp_table) = match target {
                    BufferTarget::Table(t) => (t, false),
                    BufferTarget::TempTable(t) => (t, true),
                };
                members.push(MemberDescriptor {
                    record: Some(MemberRecord {
                        table: IndexName::new(&table.name),
                        temp_table,
                    }),
                    ..data(
                        name,
                        SymbolKind::Buffer,
                        MemberType::Untyped,
                        *access,
                        *is_static,
                    )
                });
            }
            StatementKind::DefineDataset {
                name,
                access,
                is_static,
                ..
            } => members.push(data(
                name,
                SymbolKind::Dataset,
                MemberType::Untyped,
                *access,
                *is_static,
            )),
            StatementKind::DefineDataSource {
                name,
                access,
                is_static,
                ..
            } => members.push(data(
                name,
                SymbolKind::DataSource,
                MemberType::Untyped,
                *access,
                *is_static,
            )),
            StatementKind::DefineEvent {
                access,
                is_static,
                name,
                ..
            } => members.push(data(
                name,
                SymbolKind::Event,
                MemberType::Untyped,
                Some(*access),
                *is_static,
            )),
            StatementKind::Block(inner) | StatementKind::VariableDeclarationList(inner) => {
                collect_members(inner, tokens, source, members);
            }
            _ => {}
        }
    }
}

/// Lower a declared [`DataType`] into a form that survives the file boundary.
///
/// A class-typed declaration becomes [`MemberType::Named`] — the name as
/// written, for the consumer to resolve in its own symbol space. Every other
/// spelling goes through the declare pass's own
/// [`ResolvedType::from_data_type`], so an indexed member's type and a local
/// declaration's type are computed by one conversion rather than two that could
/// disagree.
///
/// With an `EXTENT` clause the type is an array, through
/// [`ResolvedType::from_declared`]: a class-typed array carries `Unknown`, as
/// it does for a local declaration.
fn member_type(declared: &DataType, extent: Option<ParameterExtent>) -> MemberType {
    if extent.is_some() {
        return MemberType::portable(ResolvedType::from_declared(declared, extent))
            .unwrap_or(MemberType::Untyped);
    }
    if let Some(name) = declared.class_name() {
        return MemberType::Named(IndexName::new(name));
    }
    // `from_data_type` yields only primitives, arrays over them, and `Unknown`
    // (its answer for a class type and for an unresolved `&IF` type position),
    // all of which are portable — so the fallback is unreachable today and is
    // here only so a future non-portable lowering degrades to "carries no type"
    // instead of dropping the member.
    MemberType::portable(ResolvedType::from_data_type(declared)).unwrap_or(MemberType::Untyped)
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_ast::AccessModifier;
    use oxabl_semantic::{ParamMode, PortableType, PrimitiveTy};

    const FILE: IndexedFileId = IndexedFileId::new(7);

    fn index_file(file: IndexedFileId, source: &str) -> FileFacts {
        super::index_file(
            file,
            source,
            &IncludeContext::in_file(&PreprocessorConfig::default()),
        )
    }

    fn portable(ty: ResolvedType) -> MemberType {
        MemberType::Portable(PortableType::new(ty).expect("fixture type is portable"))
    }

    #[test]
    fn an_enum_is_indexed_as_a_class_with_its_members() {
        let facts = index_file(
            FILE,
            "/* header */\nENUM orders.Shade FLAGS:\n    \
             DEFINE ENUM light = 1, dark = 2\n        both = light | dark\n        none.\n\
             END ENUM.\n",
        );

        assert!(facts.parsed);
        let class = facts
            .class(&IndexName::new("orders.shade"))
            .expect("the enum is indexed under its qualified name");
        assert_eq!(class.descriptor.kind, ClassKind::Enum);
        let names: Vec<_> = class.members.iter().map(|m| m.name.as_str()).collect();
        assert_eq!(names, ["light", "dark", "both", "none"]);
        assert!(class.members.iter().all(|m| m.is_static));
        assert!(
            class
                .members
                .iter()
                .all(|m| m.ty == MemberType::Named(IndexName::new("orders.Shade"))),
            "an enum member has the type of its enum"
        );
    }

    #[test]
    fn a_define_enum_that_is_not_an_enum_declaration_indexes_nothing() {
        let facts = index_file(
            FILE,
            "DEFINE VARIABLE enum AS INTEGER NO-UNDO.\nenum = 1.\n",
        );
        assert!(facts.classes.is_empty());
    }

    #[test]
    fn a_class_named_by_its_own_directives_is_indexed_under_the_expanded_name() {
        let facts = index_file(
            FILE,
            "&GLOBAL-DEFINE fullName orders.built\nCLASS {&fullName}:\nEND CLASS.\n",
        );
        assert!(facts.class(&IndexName::new("orders.built")).is_some());
    }

    #[test]
    fn a_class_whose_directives_depend_on_an_include_is_not_guessed_at() {
        let facts = index_file(
            FILE,
            "{names.i}\n&IF DEFINED(legacy) = 0 &THEN\nCLASS orders.built:\n&ELSE\n\
             CLASS orders.built INHERITS orders.old:\n&ENDIF\nEND CLASS.\n",
        );
        assert!(facts.class(&IndexName::new("orders.built")).is_none());
    }

    fn index_through(
        source: &str,
        includes: &[(&str, &str)],
    ) -> (FileFacts, Vec<std::path::PathBuf>) {
        let mut fs = oxabl_workspace::InMemoryFileSystem::new();
        for (path, text) in includes {
            fs.insert(std::path::PathBuf::from(path), *text);
        }
        let predefined = PreprocessorConfig::default();
        let include_paths = [std::path::PathBuf::from("/src")];
        super::index_file_with_includes(
            FILE,
            source,
            &IncludeContext {
                fs: &fs,
                include_paths: &include_paths,
                root_relative_paths: &[],
                predefined: &predefined,
            },
        )
    }

    const BODY: (&str, &str) = (
        "/src/body.i",
        "METHOD PUBLIC INTEGER calc-total():\n    RETURN 0.\nEND METHOD.\n",
    );

    fn member_names(facts: &FileFacts, class: &str) -> Vec<String> {
        facts
            .class(&IndexName::new(class))
            .expect("the class is indexed")
            .members
            .iter()
            .map(|m| m.name.as_str().to_string())
            .collect()
    }

    #[test]
    fn a_class_whose_body_comes_from_an_include_is_indexed_through_it() {
        let (facts, read) = index_through("CLASS orders.built:\n{body.i}\nEND CLASS.\n", &[BODY]);
        assert_eq!(member_names(&facts, "orders.built"), ["calc-total"]);
        assert_eq!(read, [std::path::PathBuf::from("/src/body.i")]);
    }

    #[test]
    fn a_class_named_by_an_includes_define_is_indexed_under_that_name() {
        let (facts, read) = index_through(
            "{names.i}\nCLASS {&fullName}:\n{body.i}\nEND CLASS.\n",
            &[
                ("/src/names.i", "&GLOBAL-DEFINE fullName orders.built\n"),
                BODY,
            ],
        );
        assert_eq!(member_names(&facts, "orders.built"), ["calc-total"]);
        assert_eq!(read.len(), 2);
    }

    #[test]
    fn a_file_that_is_only_includes_declares_what_they_declare() {
        let (facts, read) = index_through(
            "&SCOPED-DEFINE web yes\n{shared.cls}\n",
            &[
                (
                    "/src/shared.cls",
                    "&IF DEFINED(web) = 0 &THEN\nCLASS orders.shared:\n&ELSE\n\
                     CLASS orders.web:\n&ENDIF\n{body.i}\nEND CLASS.\n",
                ),
                BODY,
            ],
        );
        assert_eq!(member_names(&facts, "orders.web"), ["calc-total"]);
        assert!(facts.class(&IndexName::new("orders.shared")).is_none());
        assert_eq!(read.len(), 2);
    }

    #[test]
    fn a_class_with_an_include_that_is_missing_keeps_its_text_as_written() {
        let (facts, read) = index_through(
            "CLASS orders.built:\n{body.i}\n{gone.i}\nEND CLASS.\n",
            &[BODY],
        );
        assert!(member_names(&facts, "orders.built").is_empty());
        assert_eq!(
            read,
            [std::path::PathBuf::from("/src/body.i")],
            "an edit to the include that was read can still change the facts"
        );
    }

    fn members_complete(facts: &FileFacts, class: &str) -> bool {
        facts
            .class(&IndexName::new(class))
            .expect("the class is indexed")
            .descriptor
            .members_complete
    }

    #[test]
    fn a_member_list_is_complete_only_when_no_include_or_if_may_add_to_it() {
        let (read, _) = index_through(
            "CLASS orders.built:
{body.i}
END CLASS.
",
            &[BODY],
        );
        assert!(
            members_complete(&read, "orders.built"),
            "the include was read"
        );

        let (missing, _) = index_through(
            "CLASS orders.built:
{gone.i}
END CLASS.
",
            &[],
        );
        assert!(!members_complete(&missing, "orders.built"));

        let conditional = index_file(
            FILE,
            "CLASS orders.built:
&IF DEFINED(web) &THEN
             METHOD PUBLIC INTEGER extra():
    RETURN 0.
END METHOD.
&ENDIF
END CLASS.
",
        );
        assert!(!members_complete(&conditional, "orders.built"));

        let in_a_body = index_file(
            FILE,
            "CLASS orders.built:
METHOD PUBLIC INTEGER calc():
             &IF DEFINED(web) &THEN
    RETURN 1.
&ENDIF
    RETURN 0.
END METHOD.
END CLASS.
",
        );
        assert!(
            members_complete(&in_a_body, "orders.built"),
            "an &IF inside a method body declares no member"
        );
    }

    #[test]
    fn a_file_that_answers_as_written_reads_no_include() {
        let (facts, read) = index_through("{body.i}\nPROCEDURE p:\nEND PROCEDURE.\n", &[BODY]);
        assert_eq!(facts.procedures, [IndexName::new("p")]);
        assert!(read.is_empty());
    }

    #[test]
    fn class_facts_carry_header_and_public_members() {
        let facts = index_file(
            FILE,
            r#"CLASS orders.total-calc INHERITS orders.calc-base IMPLEMENTS orders.i-calc, orders.i-audit:
                   DEFINE PUBLIC PROPERTY label AS CHARACTER NO-UNDO GET. SET.
                   METHOD PUBLIC INTEGER calc-total():
                       RETURN 0.
                   END METHOD.
                   METHOD PUBLIC VOID reset():
                   END METHOD.
               END CLASS."#,
        );

        assert!(facts.parsed);
        assert_eq!(facts.classes.len(), 1);
        let class = facts
            .class(&IndexName::new("Orders.Total-Calc"))
            .expect("the class is indexed under its folded qualified name");
        assert_eq!(class.descriptor.file, FILE);
        assert_eq!(class.descriptor.kind, ClassKind::Class);
        assert_eq!(
            class.descriptor.inherits,
            Some(IndexName::new("orders.calc-base"))
        );
        assert_eq!(
            class.descriptor.implements,
            vec![
                IndexName::new("orders.i-calc"),
                IndexName::new("orders.i-audit")
            ]
        );

        let members = &class.members;
        assert_eq!(members.len(), 3, "one property and two methods");
        assert_eq!(members[0].name, IndexName::new("label"));
        assert_eq!(members[0].kind, SymbolKind::Property);
        assert_eq!(
            members[0].ty,
            portable(ResolvedType::Primitive(PrimitiveTy::Character))
        );
        assert_eq!(members[1].name, IndexName::new("calc-total"));
        assert_eq!(members[1].kind, SymbolKind::Function);
        assert_eq!(
            members[1].ty,
            portable(ResolvedType::Primitive(PrimitiveTy::Integer))
        );
        assert_eq!(members[2].name, IndexName::new("reset"));
        assert_eq!(
            members[2].ty,
            MemberType::Untyped,
            "VOID names no type at all"
        );
        assert!(members.iter().all(|m| m.access == AccessModifier::Public));
        assert!(members.iter().all(|m| !m.is_static));
    }

    #[test]
    fn a_class_typed_member_carries_the_name_not_a_dangling_id() {
        let facts = index_file(
            FILE,
            r#"CLASS orders.total-calc:
                   DEFINE PUBLIC PROPERTY owner AS CLASS orders.calc-base NO-UNDO GET.
               END CLASS."#,
        );
        let class = facts.class(&IndexName::new("orders.total-calc")).unwrap();
        assert_eq!(
            class.members[0].ty,
            MemberType::Named(IndexName::new("orders.calc-base")),
        );
    }

    #[test]
    fn an_extent_on_a_property_or_a_return_type_gives_an_array_member() {
        let facts = index_file(
            FILE,
            r#"CLASS orders.tags:
                   DEFINE PUBLIC PROPERTY fixed AS CHARACTER EXTENT 3 NO-UNDO GET. SET.
                   DEFINE PUBLIC PROPERTY open AS INTEGER EXTENT NO-UNDO GET. SET.
                   DEFINE PUBLIC PROPERTY owners AS orders.owner EXTENT 2 NO-UNDO GET. SET.
                   METHOD PUBLIC DECIMAL EXTENT 4 totals():
                   END METHOD.
               END CLASS."#,
        );
        let class = facts.class(&IndexName::new("orders.tags")).unwrap();
        let array = |element: PrimitiveTy, extent| {
            portable(ResolvedType::Array {
                element: Box::new(ResolvedType::Primitive(element)),
                extent,
            })
        };
        assert_eq!(class.members[0].ty, array(PrimitiveTy::Character, Some(3)));
        assert_eq!(class.members[1].ty, array(PrimitiveTy::Integer, None));
        assert_eq!(
            class.members[2].ty,
            portable(ResolvedType::Unknown),
            "a class-typed array is not modelled"
        );
        assert_eq!(class.members[3].ty, array(PrimitiveTy::Decimal, Some(4)));
    }

    #[test]
    fn a_method_carries_its_parameter_list() {
        let facts = index_file(
            FILE,
            r#"CLASS orders.calc:
                   METHOD PUBLIC INTEGER score(INPUT a AS INTEGER, OUTPUT b AS orders.item,
                       INPUT-OUTPUT c AS CHARACTER EXTENT 2, INPUT d LIKE e,
                       BUFFER bf FOR customer, INPUT TABLE FOR tt, INPUT DATASET-HANDLE h):
                   END METHOD.
                   METHOD PUBLIC VOID none():
                   END METHOD.
                   DEFINE PUBLIC PROPERTY label AS CHARACTER NO-UNDO GET.
               END CLASS."#,
        );
        let class = facts.class(&IndexName::new("orders.calc")).unwrap();
        let params = class.members[0].params.as_deref().expect("a known list");
        let value = |ty| MemberParamShape::Value(ty);
        let shapes: Vec<_> = params.iter().map(|p| (p.mode, p.shape.clone())).collect();
        assert_eq!(
            shapes,
            [
                (
                    ParamMode::Input,
                    value(portable(ResolvedType::Primitive(PrimitiveTy::Integer)))
                ),
                (
                    ParamMode::Output,
                    value(MemberType::Named(IndexName::new("orders.item")))
                ),
                (
                    ParamMode::InputOutput,
                    value(portable(ResolvedType::Array {
                        element: Box::new(ResolvedType::Primitive(PrimitiveTy::Character)),
                        extent: Some(2),
                    }))
                ),
                (
                    ParamMode::Input,
                    MemberParamShape::Like {
                        source: IndexName::new("e"),
                        extent: None
                    }
                ),
                (ParamMode::Input, MemberParamShape::Buffer),
                (ParamMode::Input, MemberParamShape::Table),
                (ParamMode::Input, MemberParamShape::DatasetHandle),
            ]
        );
        assert_eq!(class.members[1].params.as_deref(), Some(&[][..]));
        assert_eq!(class.members[2].params, None, "a property has no list");
    }

    #[test]
    fn a_class_with_a_class_typed_member_keeps_its_imports() {
        let facts = index_file(
            FILE,
            "USING orders.*.
CLASS orders.calc:
             DEFINE PUBLIC PROPERTY owner AS item NO-UNDO GET.
END CLASS.
",
        );
        let class = facts.class(&IndexName::new("orders.calc")).unwrap();
        assert!(
            !class.descriptor.imports.is_empty(),
            "the member type is written under the file's imports"
        );

        let plain = index_file(
            FILE,
            "USING orders.*.
CLASS orders.calc:
             DEFINE PUBLIC PROPERTY label AS CHARACTER NO-UNDO GET.
END CLASS.
",
        );
        let class = plain.class(&IndexName::new("orders.calc")).unwrap();
        assert!(class.descriptor.imports.is_empty());
    }

    #[test]
    fn non_public_members_are_indexed_but_not_inherited() {
        let facts = index_file(
            FILE,
            r#"CLASS orders.total-calc:
                   METHOD PRIVATE VOID audit():
                   END METHOD.
                   METHOD PROTECTED STATIC VOID reset():
                   END METHOD.
               END CLASS."#,
        );
        let class = facts.class(&IndexName::new("orders.total-calc")).unwrap();
        assert_eq!(class.members[0].access, AccessModifier::Private);
        assert!(!class.members[0].inherited_by_subclass());
        assert_eq!(class.members[1].access, AccessModifier::Protected);
        assert!(class.members[1].is_static);
        assert!(class.members[1].inherited_by_subclass());
    }

    #[test]
    fn an_interface_is_indexed_with_its_supertypes_as_implements() {
        let facts = index_file(
            FILE,
            "INTERFACE orders.i-calc INHERITS orders.i-audit: END INTERFACE.",
        );
        let iface = facts.class(&IndexName::new("orders.i-calc")).unwrap();
        assert_eq!(iface.descriptor.kind, ClassKind::Interface);
        assert_eq!(iface.descriptor.inherits, None);
        assert_eq!(
            iface.descriptor.implements,
            vec![IndexName::new("orders.i-audit")]
        );
    }

    #[test]
    fn internal_procedures_and_functions_are_indexed() {
        let facts = index_file(
            FILE,
            r#"PROCEDURE post-order:
                   MESSAGE "posted".
               END PROCEDURE.

               FUNCTION calc-tax RETURNS DECIMAL (INPUT p-amount AS DECIMAL):
                   RETURN p-amount * 0.1.
               END FUNCTION."#,
        );
        assert!(facts.parsed);
        assert_eq!(facts.procedures, vec![IndexName::new("post-order")]);
        assert_eq!(facts.functions, vec![IndexName::new("calc-tax")]);
    }

    #[test]
    fn a_method_is_not_reported_as_a_file_level_function() {
        let facts = index_file(
            FILE,
            r#"CLASS orders.total-calc:
                   METHOD PUBLIC VOID reset():
                   END METHOD.
               END CLASS."#,
        );
        assert!(
            facts.functions.is_empty(),
            "a method is reached through its class, not as a file-level function"
        );
    }

    #[test]
    fn only_new_shared_definitions_are_producers() {
        let facts = index_file(
            FILE,
            r#"DEFINE NEW SHARED VARIABLE v-order-count AS INTEGER NO-UNDO.
               DEFINE NEW GLOBAL SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.
               DEFINE SHARED VARIABLE v-consumed AS INTEGER NO-UNDO."#,
        );
        assert!(facts.parsed);
        assert!(facts.defines_shared(&IndexName::new("v-order-count")));
        assert!(facts.defines_shared(&IndexName::new("V-SITE-CODE")));
        assert!(
            !facts.defines_shared(&IndexName::new("v-consumed")),
            "a plain DEFINE SHARED is a consumer, not a producer"
        );
        assert_eq!(facts.shared_definitions.len(), 2);
    }

    #[test]
    fn a_broken_file_is_distinguishable_from_one_that_declares_nothing() {
        let broken = index_file(FILE, "DEFINE VARIABLE .");
        assert!(!broken.parsed, "a recovered parse error is not usable");
        assert!(broken.classes.is_empty());
        assert!(broken.procedures.is_empty());
        assert!(broken.functions.is_empty());
        assert!(broken.shared_definitions.is_empty());

        let quiet = index_file(FILE, "MESSAGE \"nothing declared here\".");
        assert!(quiet.parsed, "a file may parse and declare nothing");
        assert!(quiet.classes.is_empty());
        assert!(quiet.procedures.is_empty());
        assert!(quiet.functions.is_empty());
        assert!(quiet.shared_definitions.is_empty());
    }

    #[test]
    fn a_broken_class_body_yields_no_partial_class() {
        // The whole-file conservatism: recovery could leave the member list
        // short or mistyped, and a wrong member is worse than no class.
        let facts = index_file(
            FILE,
            r#"CLASS orders.total-calc:
                   DEFINE PUBLIC PROPERTY AS CHARACTER NO-UNDO GET.
               END CLASS."#,
        );
        assert!(!facts.parsed);
        assert!(facts.class(&IndexName::new("orders.total-calc")).is_none());
    }

    fn imports_of(facts: &FileFacts, class: &str) -> Arc<ImportContext> {
        Arc::clone(
            &facts
                .class(&IndexName::new(class))
                .expect("the class is indexed")
                .descriptor
                .imports,
        )
    }

    #[test]
    fn a_class_with_a_parent_carries_the_imports_it_names_it_under() {
        let wildcard = index_file(
            FILE,
            "USING rules.*.\nCLASS orders.mid INHERITS base:\nEND CLASS.\n",
        );
        let explicit = index_file(
            FILE,
            "USING rules.base.\nCLASS orders.mid INHERITS base:\nEND CLASS.\n",
        );
        let bare = index_file(FILE, "CLASS orders.mid INHERITS base:\nEND CLASS.\n");

        let (wildcard, explicit, bare) = (
            imports_of(&wildcard, "orders.mid"),
            imports_of(&explicit, "orders.mid"),
            imports_of(&bare, "orders.mid"),
        );
        assert!(!wildcard.is_empty() && !explicit.is_empty() && bare.is_empty());
        assert_ne!(wildcard, explicit, "the two import forms differ");
        assert_ne!(wildcard, bare);
    }

    #[test]
    fn an_interface_carries_the_imports_of_the_interfaces_it_extends() {
        let facts = index_file(
            FILE,
            "USING rules.*.\nINTERFACE orders.i-mid INHERITS i-base:\nEND INTERFACE.\n",
        );
        assert!(!imports_of(&facts, "orders.i-mid").is_empty());
    }

    #[test]
    fn a_class_with_no_parent_carries_no_imports() {
        // An edit to the `USING` list of a class nothing walks past leaves its
        // descriptor, and so every answer keyed on it, unchanged.
        let facts = index_file(FILE, "USING rules.*.\nCLASS orders.leaf:\nEND CLASS.\n");
        assert_eq!(*imports_of(&facts, "orders.leaf"), ImportContext::default());
    }

    #[test]
    fn imports_that_differ_only_in_case_are_different_facts() {
        // A cache keyed on equality must not serve the paths one spelling derives
        // for the other.
        let upper = index_file(
            FILE,
            "USING Rules.Base.\nCLASS orders.mid INHERITS Base:\nEND CLASS.\n",
        );
        let lower = index_file(
            FILE,
            "USING rules.base.\nCLASS orders.mid INHERITS Base:\nEND CLASS.\n",
        );
        assert_ne!(
            imports_of(&upper, "orders.mid"),
            imports_of(&lower, "orders.mid")
        );
    }

    #[test]
    fn an_include_ahead_of_the_header_may_hold_imports_the_index_does_not_read() {
        let hidden = index_file(
            FILE,
            "{usings.i}\nCLASS orders.mid INHERITS base:\nEND CLASS.\n",
        );
        assert!(imports_of(&hidden, "orders.mid").has_unseen_imports());

        let after_header = index_file(
            FILE,
            "CLASS orders.mid INHERITS base:\n    METHOD PUBLIC VOID m():\n        {body.i}\n    \
             END METHOD.\nEND CLASS.\n",
        );
        assert!(!imports_of(&after_header, "orders.mid").has_unseen_imports());
    }
}
