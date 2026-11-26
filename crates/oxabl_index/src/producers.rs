//! A precomputed map from a `SHARED` name to the files that produce it.
//!
//! A `DEFINE NEW [GLOBAL] SHARED` name maps onto no path, so the only way to find
//! its producer is to look at a set of files. [`SharedProducers`] is the answer
//! for a fixed file set, computed once and then read by any number of indexes —
//! it is `Send + Sync`, and [`BatchIndex`](crate::BatchIndex) is not, so a client
//! that runs one index per thread builds this once and hands every thread a
//! borrow, instead of every thread parsing the whole set itself.
//!
//! # Seed or don't trust
//!
//! An index with no producer map and no seeded file list answers
//! [`shared_producer`](oxabl_semantic::WorkspaceIndex::shared_producer) only from
//! files some other lookup happened to pull in. That answer depends on the order
//! of the run, so a `SharedProducer` edge built that way is not reproducible.
//! Build the map over every file the run can see, and the edge is the same on
//! every thread and in every order.
//!
//! A file that produces a name only through an include is found when it is
//! nothing but includes and directives. One with code of its own is judged by
//! its text as written, as [`index_file`] judges it, so its producer edge is
//! missing, on every thread and in every order alike.
//!
//! # Candidates, confirmed by the index
//!
//! The map lists the files that *may* define each name: every file that
//! [`index_file`] finds a definition in, and possibly some more. It finds them
//! with one parse of the text as written, at most one expansion, and a token
//! scan, not with a full index of each file. An index that asks for a producer
//! indexes each listed file and keeps only the ones whose facts define the
//! name, so a file listed in excess changes no answer.
//!
//! [`index_file`]: crate::index_file

use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use std::sync::atomic::{AtomicUsize, Ordering};

use oxabl_ast::{Statement, StatementKind};
use oxabl_lexer::{Kind, Token};
use oxabl_parser::Parser;
use oxabl_semantic::IndexName;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::IncludeContext;

/// Which files may define each `DEFINE NEW [GLOBAL] SHARED` name.
///
/// Holds paths as given, so an index reads a producer the same way it reads any
/// other file. The list of a name holds every file that [`definitions_in`]
/// finds it in: each file whose index defines it, and possibly a few more (see
/// the module docs). A file the map lists is a producer only as far as its
/// index defines the name: one that is unreadable or does not parse defines
/// nothing, and a file with code of its own defines only what its text as
/// written defines, not a name an include defines for it.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct SharedProducers {
    by_name: FxHashMap<Box<str>, Vec<PathBuf>>,
    /// The files the map was built over, in path order.
    files: Vec<PathBuf>,
    procedures: ProcedureNames,
}

/// The names of the internal procedures the files of a [`SharedProducers`]
/// define, read on the first question and not before. Two maps are equal
/// whatever either has read.
#[derive(Debug, Default)]
struct ProcedureNames(OnceLock<FxHashSet<Box<str>>>);

impl Clone for ProcedureNames {
    fn clone(&self) -> Self {
        ProcedureNames::default()
    }
}

impl PartialEq for ProcedureNames {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl Eq for ProcedureNames {}

impl SharedProducers {
    /// Scan `files` and record which of them may produce each `SHARED` name.
    ///
    /// Reads every file once. For a client that wants to scan on several threads,
    /// [`definitions_in`] does one file and [`from_definitions`](Self::from_definitions)
    /// joins the results.
    /// `includes` reads both the files and the includes a file is expanded
    /// through, as `index_file` does.
    pub fn build(files: &[PathBuf], includes: &IncludeContext<'_>) -> Self {
        Self::from_definitions(
            files
                .iter()
                .map(|path| (path.clone(), definitions_in(path, includes))),
        )
    }

    /// [`build`](Self::build) on `threads` threads (at least one). The result is
    /// the same as `build`'s for any thread count.
    pub fn build_parallel(
        files: &[PathBuf],
        includes: &IncludeContext<'_>,
        threads: usize,
    ) -> Self {
        let threads = threads.clamp(1, files.len().max(1));
        if threads == 1 {
            return Self::build(files, includes);
        }
        let next = AtomicUsize::new(0);
        let definitions = std::thread::scope(|scope| {
            let workers: Vec<_> = (0..threads)
                .map(|_| {
                    oxabl_common::stack::builder()
                        .spawn_scoped(scope, || {
                            let mut mine = Vec::new();
                            while let Some(path) = files.get(next.fetch_add(1, Ordering::Relaxed)) {
                                mine.push((path.clone(), definitions_in(path, includes)));
                            }
                            mine
                        })
                        .expect("failed to spawn an analysis thread")
                })
                .collect();
            workers
                .into_iter()
                .flat_map(|worker| match worker.join() {
                    Ok(mine) => mine,
                    Err(panic) => std::panic::resume_unwind(panic),
                })
                .collect::<Vec<_>>()
        });
        Self::from_definitions(definitions)
    }

    /// Join per-file results of [`definitions_in`]. The result does not depend on
    /// the order of `files`.
    pub fn from_definitions(files: impl IntoIterator<Item = (PathBuf, Vec<IndexName>)>) -> Self {
        let mut by_name: FxHashMap<Box<str>, Vec<PathBuf>> = FxHashMap::default();
        let mut listed = Vec::new();
        for (path, names) in files {
            listed.push(path.clone());
            for name in names {
                by_name
                    .entry(name.as_str().into())
                    .or_default()
                    .push(path.clone());
            }
        }
        for paths in by_name.values_mut() {
            paths.sort();
            paths.dedup();
        }
        listed.sort();
        SharedProducers {
            by_name,
            files: listed,
            procedures: ProcedureNames::default(),
        }
    }

    /// Whether any file of the map defines an internal procedure called
    /// `name`, as the text of the file is written.
    ///
    /// The files are read on the first question, on every available thread;
    /// a map that is never asked reads nothing. The include files the files
    /// name are read too (see [`scan_procedures`]). A name in a file that does
    /// not read, and one that only an include named by `{&name}` or `{1}`
    /// supplies, is not seen.
    pub fn defines_procedure(&self, name: &IndexName, includes: &IncludeContext<'_>) -> bool {
        self.procedures
            .0
            .get_or_init(|| {
                let threads = std::thread::available_parallelism().map_or(1, |n| n.get());
                scan_procedures(&self.files, includes, threads)
            })
            .contains(name.as_str())
    }

    /// The files that may define `name`, in path order. Index each one to know
    /// whether it does.
    pub fn producers_of(&self, name: &IndexName) -> &[PathBuf] {
        self.by_name.get(name.as_str()).map_or(&[], Vec::as_slice)
    }

    /// The number of distinct `SHARED` names with a listed file.
    pub fn len(&self) -> usize {
        self.by_name.len()
    }

    /// Whether no file in the set may produce a `SHARED` name.
    pub fn is_empty(&self) -> bool {
        self.by_name.is_empty()
    }
}

/// A [`SharedProducers`] over a fixed file list that is built on the first
/// question, not before.
///
/// A client that analyses many files can start them at once: a file that asks
/// for no producer does not wait for the scan, and a run in which no file asks
/// reads no file for it. The first question builds the whole map, on the
/// threads given to [`new`](Self::new), and a question asked meanwhile waits
/// for it. So every answer comes from the complete map, as if it was built
/// first.
///
/// `Send + Sync`: build one per run and lend it to each thread's index with
/// [`BatchIndex::with_deferred_producers`](crate::BatchIndex::with_deferred_producers).
#[derive(Debug, Default)]
pub struct DeferredProducers {
    /// The files, in the order the scan claims them.
    files: Vec<PathBuf>,
    threads: usize,
    map: OnceLock<SharedProducers>,
    procedures: ProcedureNames,
}

impl DeferredProducers {
    /// The map of `files`, to be scanned on `threads` threads (at least one)
    /// when it is first asked.
    pub fn new(files: &[PathBuf], threads: usize) -> Self {
        DeferredProducers {
            files: files.to_vec(),
            threads: threads.max(1),
            map: OnceLock::new(),
            procedures: ProcedureNames::default(),
        }
    }

    /// The map, built through `includes` when this is the first question.
    /// Every caller gets the same map.
    pub fn map(&self, includes: &IncludeContext<'_>) -> &SharedProducers {
        self.map
            .get_or_init(|| SharedProducers::build_parallel(&self.files, includes, self.threads))
    }

    /// The map, when a question has built it.
    pub fn built(&self) -> Option<&SharedProducers> {
        self.map.get()
    }

    /// [`SharedProducers::defines_procedure`] over the files, without the
    /// scan for `SHARED` names.
    pub fn defines_procedure(&self, name: &IndexName, includes: &IncludeContext<'_>) -> bool {
        self.procedures
            .0
            .get_or_init(|| scan_procedures(&self.files, includes, self.threads))
            .contains(name.as_str())
    }
}

/// The `SHARED` names the file at `path` may produce: every name that indexing
/// the file finds, and possibly some more. None when the file cannot be read.
///
/// A text without the words `NEW SHARED` or `NEW GLOBAL` defines no shared
/// name, and that is nearly every file of a tree: such a file is read only
/// when it holds a `{`, and only to know whether [`index_file`] may read it
/// through its includes. A file that says the words is parsed once as written
/// and its own definitions are found with a token scan. A file that
/// [`index_file`] may read through its includes is expanded once and its
/// expanded text is scanned too, up to its last `NEW SHARED`. Neither text
/// gets a declare pass, and the expanded text gets no whole parse.
///
/// [`index_file`]: crate::index_file
pub fn definitions_in(path: &Path, includes: &IncludeContext<'_>) -> Vec<IndexName> {
    let Ok(source) = includes.fs.read(path) else {
        return Vec::new();
    };
    oxabl_common::test_tally::within(
        || path.display().to_string(),
        || candidates_in(path, &source, includes),
    )
}

/// [`definitions_in`] of the file at `path` with the text `source`.
fn candidates_in(path: &Path, source: &str, includes: &IncludeContext<'_>) -> Vec<IndexName> {
    let written_says = says_new_shared(source);
    if !written_says && !source.contains('{') {
        return Vec::new();
    }
    let tokens = oxabl_lexer::tokenize(source);
    let mut names = Vec::new();
    let expanded = if written_says {
        let program = crate::parse(&tokens, source);
        // The text as written answers when it parses, unless the index reads
        // the expanded text; and when the expanded text does not parse, the
        // index falls back to it. So its names are kept in both cases.
        if program.is_ok() && !scan_definitions(&tokens, source, &mut names) {
            // A statement that does not parse alone in a text that parses
            // whole: the names of the declare pass, as the index finds them.
            names = crate::declared_shared(&program.statements, source);
        }
        // A superset of the files `index_file_at` reads expanded: those it
        // may expand, and those with an enum named by a reference.
        crate::may_be_expanded_parsed(&program)
            || crate::names_an_enum_with_a_reference(&tokens, source)
    } else {
        // Only the expanded text can say `NEW SHARED`, and the index reads it
        // only when it may expand the file: the test `may_define_shared` makes.
        crate::may_be_expanded_tokens(source, &tokens)
    };
    if expanded
        && let Some(text) = crate::expanded_text_at(path, source, includes)
        && let Some(last) = new_shared_said_at(&text).last()
    {
        // A statement that does not parse alone makes the whole expanded text
        // fail to parse, and the index then reads the text as written.
        scan_definitions(&tokens_through(&text, last), &text, &mut names);
    }
    names.sort_unstable_by(|a, b| a.as_str().cmp(b.as_str()));
    names.dedup();
    names
}

/// Add to `out` the name of each `DEFINE NEW [GLOBAL] SHARED` variable,
/// temp-table, work-table, buffer and dataset in `tokens`, the tokens of
/// `source`: the definitions the declare pass flags as producers. Each
/// `DEFINE` with `NEW` ahead is parsed alone, with the parser that parses it in
/// the whole text. False when one of them does not parse alone.
fn scan_definitions(tokens: &[Token], source: &str, out: &mut Vec<IndexName>) -> bool {
    let mut whole = true;
    let mut defines = tokens
        .iter()
        .enumerate()
        .filter(|(_, token)| token.kind == Kind::Define)
        .map(|(at, _)| at)
        .peekable();
    while let Some(at) = defines.next() {
        // As the parser reads `DEFINE`: references may stand before `NEW`.
        let new_ahead = tokens[at + 1..]
            .iter()
            .find(|t| {
                !matches!(
                    t.kind,
                    Kind::Comment | Kind::Preprop | Kind::IncludeArgReference
                )
            })
            .is_some_and(|t| t.kind == Kind::New);
        if !new_ahead {
            continue;
        }
        // Up to the next `DEFINE`, which no statement holds, or the end: the
        // parser sees the same tokens as in the whole text and no others.
        let end = defines.peek().map_or(tokens.len(), |next| next + 1);
        match Parser::new(&tokens[at..end], source).parse_statement() {
            Ok(statement) => out.extend(new_shared_name(&statement).map(IndexName::new)),
            Err(_) => whole = false,
        }
    }
    whole
}

/// The name `statement` defines when the declare pass flags it `NEW SHARED`
/// or `NEW GLOBAL SHARED`.
fn new_shared_name(statement: &Statement) -> Option<&str> {
    let (name, new_shared, new_global_shared) = match &statement.kind {
        StatementKind::VariableDeclaration {
            name,
            is_new_shared,
            is_new_global_shared,
            ..
        }
        | StatementKind::DefineTempTable {
            name,
            is_new_shared,
            is_new_global_shared,
            ..
        }
        | StatementKind::DefineBuffer {
            name,
            is_new_shared,
            is_new_global_shared,
            ..
        }
        | StatementKind::DefineDataset {
            name,
            is_new_shared,
            is_new_global_shared,
            ..
        } => (name, *is_new_shared, *is_new_global_shared),
        _ => return None,
    };
    (new_shared || new_global_shared).then_some(name.name.as_str())
}

/// The folded names of the internal procedures `files` define, and the files
/// they include, read on `threads` threads (at least one). The result does not
/// depend on the number of threads.
///
/// An `{include}` is followed when its name is written out (`{lib/units.i}`, not
/// `{&name}` or `{1}`) and it resolves in the file's own directory or on the
/// include paths. A procedure that an include file defines may be supplied
/// to any program that includes it, such as a super procedure.
pub fn scan_procedures(
    files: &[PathBuf],
    includes: &IncludeContext<'_>,
    threads: usize,
) -> FxHashSet<Box<str>> {
    let threads = threads.clamp(1, files.len().max(1));
    let next = AtomicUsize::new(0);
    let scan = || {
        let mut mine = Scanned::default();
        while let Some(path) = files.get(next.fetch_add(1, Ordering::Relaxed)) {
            if let Ok(source) = includes.fs.read(path) {
                mine.add(path.parent(), &source);
            }
        }
        mine
    };
    let mut all = if threads == 1 {
        scan()
    } else {
        std::thread::scope(|scope| {
            let workers: Vec<_> = (0..threads)
                .map(|_| {
                    oxabl_common::stack::builder()
                        .spawn_scoped(scope, scan)
                        .expect("failed to spawn an analysis thread")
                })
                .collect();
            let mut all = Scanned::default();
            for worker in workers {
                match worker.join() {
                    Ok(mine) => all.merge(mine),
                    Err(panic) => std::panic::resume_unwind(panic),
                }
            }
            all
        })
    };
    // The include files, each read once, and the ones they include.
    let mut seen: FxHashSet<PathBuf> = FxHashSet::default();
    let mut pending: Vec<(Option<PathBuf>, String)> = all.includes.drain().collect();
    pending.sort();
    while let Some((dir, name)) = pending.pop() {
        let own: Vec<PathBuf> = dir.into_iter().collect();
        let Some(path) = includes
            .fs
            .resolve_include(&own, &name)
            .or_else(|| includes.fs.resolve_include(includes.include_paths, &name))
            .or_else(|| {
                includes
                    .fs
                    .resolve_include(includes.root_relative_paths, &name)
            })
        else {
            continue;
        };
        if !seen.insert(path.clone()) {
            continue;
        }
        if let Ok(source) = includes.fs.read(&path) {
            let mut found = Scanned::default();
            found.add(path.parent(), &source);
            all.names.extend(found.names);
            pending.extend(found.includes);
        }
    }
    all.names
}

/// What a scan of source files found: procedure names, and the include
/// references to follow, each with the directory of the file that holds it.
#[derive(Default)]
struct Scanned {
    names: FxHashSet<Box<str>>,
    includes: FxHashSet<(Option<PathBuf>, String)>,
}

impl Scanned {
    fn add(&mut self, dir: Option<&Path>, source: &str) {
        procedure_names(source, &mut self.names);
        for name in include_names(source) {
            self.includes
                .insert((dir.map(Path::to_path_buf), name.to_string()));
        }
    }

    fn merge(&mut self, other: Scanned) {
        self.names.extend(other.names);
        self.includes.extend(other.includes);
    }
}

/// The written-out names of the `{include}` references of `source`: the word
/// after a `{` that does not start a `{&name}`, a `{1}` or a `{}`.
fn include_names(source: &str) -> impl Iterator<Item = &str> {
    source.match_indices('{').filter_map(|(at, _)| {
        let rest = &source[at + 1..];
        let first = rest.chars().next()?;
        if !(first.is_alphanumeric() || matches!(first, '_' | '.' | '/' | '\\' | '"' | '\''))
            || first.is_ascii_digit()
        {
            return None;
        }
        let end = rest.find(|c: char| c.is_whitespace() || matches!(c, '}' | '{'))?;
        let name = rest[..end].trim_matches(['"', '\'']);
        (!name.is_empty() && !name.contains(['&', '*'])).then_some(name)
    })
}

/// Add the folded name of every `PROCEDURE name` in `source` to `out`. A word
/// after `PROCEDURE` that is not a name is added too: `SUBSCRIBE PROCEDURE h`
/// names a handle. That only makes a question answer "defined" more often.
fn procedure_names(source: &str, out: &mut FxHashSet<Box<str>>) {
    // The keyword may be abbreviated to `PROC`.
    if !source
        .as_bytes()
        .windows(4)
        .any(|w| w.eq_ignore_ascii_case(b"proc"))
    {
        return;
    }
    let tokens = oxabl_lexer::tokenize(source);
    for pair in tokens.windows(2) {
        // The lexer reads the abbreviations as plain words.
        let keyword = pair[0].kind == Kind::Procedure
            || (pair[0].kind == Kind::Identifier
                && is_procedure_abbreviation(&source[pair[0].start..pair[0].end]));
        if !keyword {
            continue;
        }
        let word = source[pair[1].start..pair[1].end].trim_matches(['"', '\'']);
        if word.starts_with(|c: char| c.is_alphanumeric() || c == '_') {
            out.insert(IndexName::new(word).as_str().into());
        }
    }
}

/// `PROC`, `PROCE`, ... up to `PROCEDURE`, in any case.
fn is_procedure_abbreviation(word: &str) -> bool {
    const FULL: &str = "procedure";
    (4..=FULL.len()).contains(&word.len()) && FULL[..word.len()].eq_ignore_ascii_case(word)
}

/// Whether [`index_file_at`] can find a shared definition in `source`, the file
/// at `path`: a superset test. A file whose text does not say `NEW SHARED` or
/// `NEW GLOBAL` is parsed only when it holds a `{`, and expanded only when
/// `index_file_at` may expand it.
///
/// A definition the index reads from the text as written says the two words
/// there. Text expansion can only bring them together through a `{...}`
/// reference or include, or a directive or comment between them.
pub(crate) fn may_define_shared(path: &Path, source: &str, includes: &IncludeContext<'_>) -> bool {
    says_new_shared(source)
        || (source.contains('{')
            && crate::may_be_expanded(source)
            && crate::expanded_text_at(path, source, includes)
                .is_some_and(|text| says_new_shared(&text)))
}

/// Whether `source` holds the word `NEW`, in any case, followed by `SHARED`,
/// `GLOBAL`, or anything expansion may remove or replace (a comment, a
/// directive, a `{...}` reference or a line continuation). A superset of the
/// texts that hold a `NEW [GLOBAL] SHARED` definition.
fn says_new_shared(source: &str) -> bool {
    new_shared_said_at(source).next().is_some()
}

/// The byte offsets of the words `NEW` that [`says_new_shared`] accepts, in
/// text order.
fn new_shared_said_at(source: &str) -> impl Iterator<Item = usize> {
    const NEW: &[u8] = b"new";
    let bytes = source.as_bytes();
    let word = |b: u8| b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_' | b'#' | b'$' | b'%');
    let mut at = 0;
    std::iter::from_fn(move || {
        while let Some(found) = bytes[at..]
            .windows(NEW.len())
            .position(|w| w.eq_ignore_ascii_case(NEW))
        {
            let start = at + found;
            let end = start + NEW.len();
            at = start + 1;
            let alone = (start == 0 || !word(bytes[start - 1]) && bytes[start - 1] != b'&')
                && bytes.get(end).is_none_or(|b| !word(*b));
            if !alone {
                continue;
            }
            let rest = &bytes[end..];
            let next = rest
                .iter()
                .position(|b| !b.is_ascii_whitespace())
                .map_or(&[][..], |skip| &rest[skip..]);
            let starts =
                |w: &[u8]| next.len() >= w.len() && next[..w.len()].eq_ignore_ascii_case(w);
            if next.is_empty()
                || matches!(next[0], b'/' | b'{' | b'&' | b'~')
                || starts(b"shared")
                || starts(b"global")
            {
                return Some(start);
            }
        }
        None
    })
}

/// The tokens of `source` up to the first `DEFINE` that starts after the
/// offset `last`, that one included, or all of them. Every `DEFINE` whose
/// `NEW` starts at or before `last` is in them, with the tokens of its
/// statement: no statement holds a second `DEFINE`.
fn tokens_through(source: &str, last: usize) -> Vec<Token> {
    let mut tokens = Vec::new();
    for token in oxabl_lexer::Lexer::new(source) {
        let stop = token.kind == Kind::Define && token.start > last;
        tokens.push(token);
        if stop {
            break;
        }
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{IndexedFileId, index_file_at};
    use oxabl_workspace::{InMemoryFileSystem, PreprocessorConfig};

    #[test]
    fn new_shared_is_said_with_anything_expansion_can_remove_between() {
        for text in [
            "DEFINE NEW SHARED VARIABLE x AS INTEGER.",
            "def new global shared var x as int.",
            "DEFINE NEW /* producer */ SHARED VARIABLE x AS INTEGER.",
            "DEFINE NEW\n&IF DEFINED(x) &THEN\n&ENDIF\nSHARED VARIABLE x AS INTEGER.",
            "DEFINE NEW {&shared-word} VARIABLE x AS INTEGER.",
            "DEFINE new\n   Shared FRAME f.",
            "DEFINE NEW",
        ] {
            assert!(says_new_shared(text), "{text}");
        }
        for text in [
            "DEFINE SHARED VARIABLE x AS INTEGER.",
            "o = NEW app.Thing(). DEFINE SHARED VARIABLE y AS INTEGER.",
            "renew shared things",
            "DEFINE VARIABLE new-shared AS LOGICAL.",
            "{&new} SHARED",
        ] {
            assert!(!says_new_shared(text), "{text}");
        }
    }

    /// Every text built from the parts below that the indexer finds a `NEW
    /// SHARED` definition in passes the prefilter.
    #[test]
    fn the_prefilter_passes_every_definition_the_indexer_finds() {
        const PREFIXES: [&str; 3] = ["", "o = NEW app.Thing(). ", "&SCOPED-DEFINE sh SHARED\n"];
        const DEFINES: [&str; 3] = ["DEFINE", "def", "Defi"];
        const NEWS: [&str; 3] = ["NEW", "new", "nEw"];
        const SEPARATORS: [&str; 10] = [
            " ",
            "\n",
            "\t",
            "\r\n",
            " /* note */ ",
            "/*a*//*b*/",
            " ~\n ",
            "\n&SCOPED-DEFINE x 1\n",
            "\n&IF TRUE &THEN\n&ENDIF\n",
            " {&nothing} ",
        ];
        const SHAREDS: [&str; 4] = ["SHARED", "shared", "GLOBAL SHARED", "{&sh}"];
        const OBJECTS: [&str; 3] = [
            "VARIABLE v AS INTEGER NO-UNDO.",
            "TEMP-TABLE tt NO-UNDO FIELD f AS INTEGER.",
            "STREAM s.",
        ];
        let fs = InMemoryFileSystem::new();
        let predefined = PreprocessorConfig::default();
        let includes = in_fs(&fs, &predefined);
        let path = Path::new("/src/p.p");
        let mut found = 0;
        for prefix in PREFIXES {
            for define in DEFINES {
                for new in NEWS {
                    for before in SEPARATORS {
                        for shared in SHAREDS {
                            for after in SEPARATORS {
                                for object in OBJECTS {
                                    let text = format!(
                                        "{prefix}{define} {new}{before}{shared}{after}{object}"
                                    );
                                    let facts = index_file_at(
                                        IndexedFileId::new(1),
                                        path,
                                        &text,
                                        &includes,
                                    )
                                    .0;
                                    if facts.shared_definitions.is_empty() {
                                        continue;
                                    }
                                    found += 1;
                                    assert!(may_define_shared(path, &text, &includes), "{text:?}");
                                    let mut fs = InMemoryFileSystem::new();
                                    fs.insert(path.to_path_buf(), text.as_str());
                                    let listed = definitions_in(path, &in_fs(&fs, &predefined));
                                    for name in &facts.shared_definitions {
                                        assert!(listed.contains(name), "{name:?} in {text:?}");
                                    }
                                    if !text.contains('{') {
                                        assert!(says_new_shared(&text), "{text:?}");
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        assert!(found > 1000, "the parts make definitions: {found}");
    }

    fn build(fs: &InMemoryFileSystem, files: &[PathBuf]) -> SharedProducers {
        let predefined = PreprocessorConfig::default();
        SharedProducers::build(files, &in_fs(fs, &predefined))
    }

    fn in_fs<'a>(
        fs: &'a InMemoryFileSystem,
        predefined: &'a PreprocessorConfig,
    ) -> IncludeContext<'a> {
        IncludeContext {
            fs,
            include_paths: &[],
            root_relative_paths: &[],
            predefined,
        }
    }

    fn fs_with(files: &[(&str, &str)]) -> InMemoryFileSystem {
        let mut fs = InMemoryFileSystem::new();
        for (path, source) in files {
            fs.insert(PathBuf::from(path), *source);
        }
        fs
    }

    fn paths(files: &[(&str, &str)]) -> Vec<PathBuf> {
        files.iter().map(|(p, _)| PathBuf::from(p)).collect()
    }

    #[test]
    fn a_new_shared_definition_is_a_producer_and_a_consumer_is_not() {
        let files = [
            (
                "/a.p",
                "DEFINE NEW SHARED VARIABLE v-one AS INTEGER NO-UNDO.",
            ),
            ("/b.p", "DEFINE SHARED VARIABLE v-one AS INTEGER NO-UNDO."),
            (
                "/c.p",
                "define new global shared variable V-Two as integer no-undo.",
            ),
            ("/d.p", "MESSAGE \"nothing\"."),
        ];
        let fs = fs_with(&files);
        let map = build(&fs, &paths(&files));

        assert_eq!(
            map.producers_of(&IndexName::new("v-one")),
            [PathBuf::from("/a.p")]
        );
        assert_eq!(
            map.producers_of(&IndexName::new("v-two")),
            [PathBuf::from("/c.p")]
        );
        assert!(map.producers_of(&IndexName::new("v-three")).is_empty());
        assert_eq!(map.len(), 2);
    }

    #[test]
    fn two_producers_of_one_name_are_both_listed_whatever_the_order() {
        let files = [
            ("/b.p", "DEFINE NEW SHARED VARIABLE v-x AS INTEGER NO-UNDO."),
            ("/a.p", "DEFINE NEW SHARED VARIABLE v-x AS INTEGER NO-UNDO."),
        ];
        let fs = fs_with(&files);
        let forward = build(&fs, &paths(&files));
        let mut reversed = paths(&files);
        reversed.reverse();
        let backward = build(&fs, &reversed);

        assert_eq!(forward, backward);
        assert_eq!(
            forward.producers_of(&IndexName::new("v-x")),
            [PathBuf::from("/a.p"), PathBuf::from("/b.p")]
        );
    }

    #[test]
    fn an_unreadable_or_broken_file_produces_nothing() {
        let files = [("/broken.p", "DEFINE NEW SHARED VARIABLE v-y AS @ @ @.")];
        let fs = fs_with(&files);
        let mut listed = paths(&files);
        listed.push(PathBuf::from("/missing.p"));
        let map = build(&fs, &listed);

        assert!(map.is_empty(), "{map:?}");
    }

    #[test]
    fn the_prefilter_agrees_with_indexing_the_whole_file() {
        for source in [
            "DEFINE NEW SHARED VARIABLE v-a AS INTEGER NO-UNDO.",
            "define new shared variable v-a as integer no-undo.",
            "DEFINE NEW GLOBAL SHARED VARIABLE v-a AS INTEGER NO-UNDO.",
            "DEFINE NEW SHARED TEMP-TABLE tt-a FIELD f AS INTEGER.",
            "DEFINE SHARED VARIABLE v-a AS INTEGER NO-UNDO.",
            "DEF NEW SHARED VAR v-a AS INTEGER NO-UNDO.",
            "&GLOBAL-DEFINE scope NEW SHARED\nDEFINE {&scope} VARIABLE v-a AS INTEGER NO-UNDO.",
            "PROCEDURE p:\nEND PROCEDURE.",
        ] {
            let fs = fs_with(&[("/f.p", source)]);
            let predefined = PreprocessorConfig::default();
            assert_eq!(
                definitions_in(Path::new("/f.p"), &in_fs(&fs, &predefined)),
                crate::index_file(IndexedFileId::new(0), source, &in_fs(&fs, &predefined))
                    .shared_definitions,
                "{source}"
            );
        }
    }

    #[test]
    fn the_prefilter_agrees_with_indexing_a_file_through_its_includes() {
        let include_paths = [PathBuf::from("/")];
        let predefined = PreprocessorConfig::default();
        for source in [
            "{defs.i}",
            "DEFINE VARIABLE v-own AS INTEGER NO-UNDO.\n{defs.i}",
            "{plain.i}\nDEFINE VARIABLE v-own AS INTEGER NO-UNDO.",
            "{gone.i}\nDEFINE VARIABLE v-own AS INTEGER NO-UNDO.",
            "DEFINE {head.i} VARIABLE v-b AS INTEGER NO-UNDO.",
            "CLASS c:\n{defs.i}\nEND CLASS.",
        ] {
            let fs = fs_with(&[
                ("/f.p", source),
                (
                    "/defs.i",
                    "DEFINE NEW SHARED VARIABLE v-a AS INTEGER NO-UNDO.",
                ),
                ("/plain.i", "MESSAGE 1."),
                ("/head.i", "NEW SHARED"),
            ]);
            let includes = IncludeContext {
                fs: &fs,
                include_paths: &include_paths,
                root_relative_paths: &[],
                predefined: &predefined,
            };
            assert_eq!(
                definitions_in(Path::new("/f.p"), &includes),
                index_file_at(IndexedFileId::new(0), Path::new("/f.p"), source, &includes)
                    .0
                    .shared_definitions,
                "{source}"
            );
        }
        for (source, want) in [
            ("{defs.i}", &["v-a"][..]),
            ("DEFINE {head.i} VARIABLE v-b AS INTEGER NO-UNDO.", &["v-b"]),
            ("MESSAGE 2.\n{defs.i}", &[]),
            // The last `NEW SHARED` starts a statement that goes on after it.
            ("{two.i}", &["tt-c", "v-a"]),
        ] {
            let fs = fs_with(&[
                ("/f.p", source),
                (
                    "/defs.i",
                    "DEFINE NEW SHARED VARIABLE v-a AS INTEGER NO-UNDO.",
                ),
                ("/head.i", "NEW SHARED"),
                (
                    "/two.i",
                    "DEFINE NEW SHARED VARIABLE v-a AS INTEGER NO-UNDO.\n\
                     DEFINE VARIABLE v-b AS INTEGER NO-UNDO.\n\
                     DEFINE NEW SHARED TEMP-TABLE tt-c NO-UNDO\n\
                     FIELD f AS INTEGER\n\
                     INDEX ix f.\n\
                     MESSAGE \"done\".",
                ),
            ]);
            let includes = IncludeContext {
                fs: &fs,
                include_paths: &include_paths,
                root_relative_paths: &[],
                predefined: &predefined,
            };
            let want: Vec<IndexName> = want.iter().map(|name| IndexName::new(name)).collect();
            assert_eq!(
                definitions_in(Path::new("/f.p"), &includes),
                want,
                "{source}"
            );
        }
    }

    #[test]
    fn procedure_names_are_read_in_any_spelling() {
        let files = [
            (
                "/a.p",
                "PROCEDURE one-a:\nEND PROCEDURE.\nproc two-b:\nEND PROCEDURE.\nPROCEDURE \"Three-C\":\nEND.",
            ),
            ("/b.p", "MESSAGE \"proc nothing\".\nEND PROCEDURE."),
        ];
        let fs = fs_with(&files);
        let predefined = PreprocessorConfig::default();
        let includes = in_fs(&fs, &predefined);
        let map = build(&fs, &paths(&files));
        for (name, want) in [
            ("one-a", true),
            ("TWO-B", true),
            ("three-c", true),
            ("nothing", false),
            ("missing", false),
        ] {
            assert_eq!(
                map.defines_procedure(&IndexName::new(name), &includes),
                want,
                "{name}"
            );
        }
        // The answer does not depend on the number of threads.
        let one = scan_procedures(&paths(&files), &includes, 1);
        assert_eq!(scan_procedures(&paths(&files), &includes, 8), one);
        assert_eq!(one.len(), 3);
    }

    #[test]
    fn the_include_files_a_file_names_are_followed() {
        let files = [
            ("/src/super.p", "{lib/units.i}\n{&dynamic}\n{1}\n"),
            (
                "/src/lib/units.i",
                "PROCEDURE convert-units:\nEND PROCEDURE.\n{deeper.i}",
            ),
            (
                "/src/lib/deeper.i",
                "PROCEDURE from-deeper:\nEND PROCEDURE.",
            ),
            (
                "/src/unused.i",
                "PROCEDURE nobody-includes-me:\nEND PROCEDURE.",
            ),
        ];
        let fs = fs_with(&files);
        let predefined = PreprocessorConfig::default();
        let include_paths = [PathBuf::from("/src")];
        let includes = IncludeContext {
            fs: &fs,
            include_paths: &include_paths,
            root_relative_paths: &[],
            predefined: &predefined,
        };
        let names = scan_procedures(&paths(&files[..1]), &includes, 2);
        assert!(names.contains("convert-units"));
        // An include of an include is followed, from the including file's own
        // directory.
        assert!(names.contains("from-deeper"));
        assert!(!names.contains("nobody-includes-me"));
    }

    #[test]
    fn a_map_reads_no_procedure_until_it_is_asked() {
        let files = [("/a.p", "PROCEDURE x:\nEND PROCEDURE.")];
        let fs = fs_with(&files);
        let map = build(&fs, &paths(&files));
        assert!(map.procedures.0.get().is_none());
        // Two maps are equal whatever either has read.
        let other = build(&fs, &paths(&files));
        let predefined = PreprocessorConfig::default();
        other.defines_procedure(&IndexName::new("x"), &in_fs(&fs, &predefined));
        assert_eq!(map, other);
    }

    /// Questions that arrive while the map is built wait for the whole map:
    /// none sees a part of it.
    #[test]
    fn every_question_to_a_deferred_map_gets_the_whole_map() {
        let sources: Vec<(String, String)> = (0..60)
            .map(|n| {
                (
                    format!("/src/f{n:02}.p"),
                    format!("DEFINE NEW SHARED VARIABLE v-{n} AS CHARACTER NO-UNDO."),
                )
            })
            .collect();
        let listed: Vec<(&str, &str)> = sources
            .iter()
            .map(|(p, b)| (p.as_str(), b.as_str()))
            .collect();
        let fs = fs_with(&listed);
        let files = paths(&listed);
        let predefined = PreprocessorConfig::default();
        let includes = in_fs(&fs, &predefined);
        let whole = SharedProducers::build(&files, &includes);

        let deferred = DeferredProducers::new(&files, 3);
        assert!(
            deferred.built().is_none(),
            "nothing is read before a question"
        );
        let seen: Vec<SharedProducers> = std::thread::scope(|scope| {
            let askers: Vec<_> = (0..8)
                .map(|_| scope.spawn(|| deferred.map(&includes).clone()))
                .collect();
            askers.into_iter().map(|a| a.join().unwrap()).collect()
        });
        for map in seen {
            assert_eq!(map, whole);
        }
        assert_eq!(whole.len(), 60);
    }

    #[test]
    fn a_parallel_scan_builds_the_map_a_serial_scan_builds() {
        let sources: Vec<(String, String)> = (0..40)
            .map(|n| {
                let body = match n % 3 {
                    0 => format!(
                        "DEFINE NEW SHARED VARIABLE v-{} AS CHARACTER NO-UNDO.",
                        n % 5
                    ),
                    1 => "DEFINE SHARED VARIABLE v-0 AS CHARACTER NO-UNDO.".to_string(),
                    _ => "MESSAGE \"quiet\".".to_string(),
                };
                (format!("/src/f{n:02}.p"), body)
            })
            .collect();
        let listed: Vec<(&str, &str)> = sources
            .iter()
            .map(|(p, b)| (p.as_str(), b.as_str()))
            .collect();
        let fs = fs_with(&listed);
        let files = paths(&listed);
        let predefined = PreprocessorConfig::default();
        let includes = in_fs(&fs, &predefined);

        let serial = SharedProducers::build(&files, &includes);
        assert!(!serial.is_empty());
        for threads in [0, 1, 4, 64] {
            assert_eq!(
                SharedProducers::build_parallel(&files, &includes, threads),
                serial,
                "{threads} threads"
            );
        }
    }
}
