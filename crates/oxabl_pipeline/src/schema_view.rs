//! The configured schema as a browsable tree: one database per `.df` file, with
//! its tables, fields, indexes and sequences, each with its place in the file.
//!
//! The tree reads each `.df` file alone, under the loader's merge rules
//! ([`SchemaLoader::load_file_alone`]). The analysis merges every file into
//! one schema where the last declaration of a table wins; a browser shows what
//! each file declares, so a table declared in two files shows in both. A file
//! that adds fields or indexes to a table that an earlier file declares shows
//! that table with `declared: false`. Whatever the loader refuses (a field of a
//! table no earlier file declares, a type conflict, the caps) is a problem of
//! the file, and an `UPDATE`, `CHANGE`, `DROP` or `RENAME` directive, which the
//! loader does not apply, makes the file incomplete.
//!
//! An attribute the `.df` does not set is left out of the serialized node.
//!
//! Every node keeps the byte span of its name in its file. A client fills
//! [`range`](SchemaTable::range) in its own position encoding with
//! [`SchemaDatabase::place`]; the tree has no positions of its own.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use oxabl_ast::Span;
use oxabl_common::{Diagnostic, FileId, Severity};
use oxabl_lexer::oxabl_atom::OxablAtom;
use oxabl_schema::{
    Field, Index, SCHEMA0020, SchemaLoader, SchemaType, Sequence, Table, fold_atom,
};
use oxabl_workspace::{EncodedFileSystem, FileSystem};
use serde::Serialize;

use crate::config::{SchemaSources, schema_sources};

/// A line and a character offset, in the unit a client chose. The shape of an
/// LSP `Position`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct TextPosition {
    pub line: u32,
    pub character: u32,
}

/// The shape of an LSP `Range`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct TextRange {
    pub start: TextPosition,
    pub end: TextPosition,
}

/// The configured schema, or why there is none.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SchemaView {
    /// The `oxabl.toml` the files come from, when one was found.
    #[serde(serialize_with = "lossy_path_opt")]
    pub config: Option<PathBuf>,
    /// One per configured `.df` file, in load order.
    pub databases: Vec<SchemaDatabase>,
    /// The directories that hold the configured `.df` files, and each
    /// configured schema directory, also an empty one: a client watches them.
    #[serde(serialize_with = "lossy_paths")]
    pub directories: Vec<PathBuf>,
    /// Why `databases` is empty. `None` whenever a file is configured.
    pub reason: Option<String>,
    /// Problems of the configuration itself, such as a schema directory that
    /// holds no `.df` file.
    pub warnings: Vec<String>,
}

/// One `.df` file.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SchemaDatabase {
    /// The file name without its extension: the logical name it most likely
    /// stands for.
    pub name: String,
    /// The path as text. A path that is not UTF-8 is written with replacement
    /// characters, and the database has a problem that says so.
    #[serde(serialize_with = "lossy_path")]
    pub path: PathBuf,
    /// False when the file cannot be read, when its parse or merge reported an
    /// error, when it has a directive the loader does not apply, or when its
    /// path is not UTF-8: the tree can then differ from what the file says.
    pub complete: bool,
    pub problems: Vec<SchemaProblem>,
    pub tables: Vec<SchemaTable>,
    pub sequences: Vec<SchemaSequence>,
    /// The decoded text of the file, which every span is an offset into.
    #[serde(skip)]
    pub text: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SchemaProblem {
    pub severity: &'static str,
    pub code: &'static str,
    pub message: String,
    #[serde(skip)]
    pub span: Option<Span>,
    pub range: Option<TextRange>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SchemaTable {
    pub name: String,
    /// False for a table this file adds fields or indexes to without an
    /// `ADD TABLE`; its location is then the first `ADD FIELD` or `ADD INDEX`
    /// that names it.
    pub declared: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dump_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub area: Option<String>,
    pub hidden: bool,
    pub frozen: bool,
    pub fields: Vec<SchemaField>,
    pub indexes: Vec<SchemaIndex>,
    #[serde(skip)]
    pub span: Span,
    pub range: Option<TextRange>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SchemaField {
    pub name: String,
    /// The data type as the `.df` spells it, lower case. `None` when the field
    /// is declared twice with types that conflict: in one file for the tree, or
    /// in two files for a table of the merged schema.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data_type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extent: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub format: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub initial: Option<String>,
    pub mandatory: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub decimals: Option<u32>,
    pub case_sensitive: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub help: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub order: Option<u32>,
    #[serde(skip)]
    pub span: Span,
    pub range: Option<TextRange>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SchemaIndex {
    pub name: String,
    pub unique: bool,
    pub primary: bool,
    pub word: bool,
    pub inactive: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub fields: Vec<SchemaIndexField>,
    #[serde(skip)]
    pub span: Span,
    pub range: Option<TextRange>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SchemaIndexField {
    pub name: String,
    pub ascending: bool,
    pub abbreviated: bool,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SchemaSequence {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub initial: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub increment: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_value: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_value: Option<i64>,
    pub cycle_on_limit: bool,
    #[serde(skip)]
    pub span: Span,
    pub range: Option<TextRange>,
}

/// The `.df` files that the `oxabl.toml` above an anchor names, each once.
struct Resolved {
    config: PathBuf,
    files: Vec<PathBuf>,
    directories: Vec<PathBuf>,
    warnings: Vec<String>,
    decoding: oxabl_workspace::SourceDecoding,
}

/// The directories to watch for the schema `entries`: a directory entry
/// itself, and the directory of a file entry. Each once, in entry order.
fn watched_directories(entries: &[PathBuf]) -> Vec<PathBuf> {
    let mut directories: Vec<PathBuf> = Vec::new();
    for entry in entries {
        let directory = if entry.is_dir() {
            Some(entry.as_path())
        } else {
            entry.parent()
        };
        if let Some(directory) = directory
            && !directories.iter().any(|d| d == directory)
        {
            directories.push(directory.to_path_buf());
        }
    }
    directories
}

/// The configured files, or the empty view that says why there are none.
fn resolve(anchor: &Path) -> Result<Resolved, SchemaView> {
    let empty = |config: Option<PathBuf>,
                 reason: String,
                 warnings: Vec<String>,
                 directories: Vec<PathBuf>| SchemaView {
        config,
        databases: Vec::new(),
        directories,
        reason: Some(reason),
        warnings,
    };
    match schema_sources(anchor) {
        SchemaSources::NoConfig { searched_from } => Err(empty(
            None,
            format!(
                "no oxabl.toml in {} or a directory above it. Name the .df files \
                 in `[workspace.schema] files` of an oxabl.toml.",
                searched_from.display()
            ),
            Vec::new(),
            Vec::new(),
        )),
        SchemaSources::Unreadable { config, message } => Err(empty(
            Some(config),
            format!("oxabl.toml cannot be used: {message}"),
            Vec::new(),
            Vec::new(),
        )),
        SchemaSources::Files {
            config,
            files,
            entries,
            warnings,
            decoding,
        } => {
            let warnings: Vec<String> = warnings.iter().map(ToString::to_string).collect();
            let directories = watched_directories(&entries);
            if files.is_empty() {
                let reason = if warnings.is_empty() {
                    format!(
                        "{} names no .df file in `[workspace.schema] files`.",
                        config.display()
                    )
                } else {
                    format!(
                        "the `[workspace.schema] files` of {} give no .df file: {}",
                        config.display(),
                        warnings.join("; ")
                    )
                };
                return Err(empty(Some(config), reason, warnings, directories));
            }
            Ok(Resolved {
                config,
                files,
                directories,
                warnings,
                decoding,
            })
        }
    }
}

/// The schema that the `oxabl.toml` above `anchor` names, each `.df` file read
/// now with the configured source encoding.
///
/// Never an error: no `oxabl.toml`, an unreadable one, or one that names no
/// `.df` file gives no database and a [`reason`](SchemaView::reason). A file
/// that cannot be read is a database with a problem and `complete: false`.
pub fn schema_view(anchor: &Path) -> SchemaView {
    schema_view_until(anchor, &|| false).expect("never stopped")
}

/// [`schema_view`], given up between two files when `stop` says so: `None`.
pub fn schema_view_until(anchor: &Path, stop: &dyn Fn() -> bool) -> Option<SchemaView> {
    let resolved = match resolve(anchor) {
        Ok(resolved) => resolved,
        Err(view) => return Some(view),
    };
    let fs = EncodedFileSystem::new(resolved.decoding);
    // Tables declared by the files read so far: the loader accepts a field
    // of one of them in a later file.
    let mut declared: HashSet<OxablAtom> = HashSet::new();
    let mut databases = Vec::with_capacity(resolved.files.len());
    for path in &resolved.files {
        if stop() {
            return None;
        }
        let database = match fs.read(path) {
            Ok(text) => {
                let (database, names) =
                    read_database(path, text.to_string(), &|name| declared.contains(name));
                declared.extend(names);
                database
            }
            Err(error) => unreadable_database(path, &error.to_string()),
        };
        let mut database = database;
        check_path(&mut database);
        databases.push(database);
    }
    Some(SchemaView {
        config: Some(resolved.config),
        databases,
        directories: resolved.directories,
        reason: None,
        warnings: resolved.warnings,
    })
}

/// The database `database` (a configured `.df` file) and its table `table`,
/// case-insensitive, with only that file read, and the earlier files only when
/// it adds to a table it does not declare. `Ok(None)` when the file has no such
/// table; an error when `database` is not configured.
pub fn schema_view_table(
    anchor: &Path,
    database: &Path,
    table: &str,
) -> Result<Option<(SchemaDatabase, SchemaTable)>, String> {
    let resolved = resolve(anchor).map_err(|view| {
        view.reason
            .unwrap_or_else(|| "no schema is configured".to_string())
    })?;
    let wanted = std::fs::canonicalize(database).unwrap_or_else(|_| database.to_path_buf());
    let Some(at) = resolved
        .files
        .iter()
        .position(|f| std::fs::canonicalize(f).unwrap_or_else(|_| f.clone()) == wanted)
    else {
        return Err(format!(
            "{} is not a configured .df file",
            database.display()
        ));
    };
    let fs = EncodedFileSystem::new(resolved.decoding);
    let path = &resolved.files[at];
    let database = match fs.read(path) {
        Ok(text) => {
            let earlier = std::cell::OnceCell::new();
            let declared_before = |name: &OxablAtom| {
                earlier
                    .get_or_init(|| declared_in(&fs, &resolved.files[..at]))
                    .contains(name)
            };
            read_database(path, text.to_string(), &declared_before).0
        }
        Err(error) => unreadable_database(path, &error.to_string()),
    };
    let mut database = database;
    check_path(&mut database);
    let found = database.table(table).cloned();
    Ok(found.map(|table| (database, table)))
}

/// The tables that `files` declare with `ADD TABLE`.
fn declared_in(fs: &EncodedFileSystem, files: &[PathBuf]) -> HashSet<OxablAtom> {
    let mut declared = HashSet::new();
    for path in files {
        if let Ok(text) = fs.read(path) {
            let (_, names) = read_database(path, text.to_string(), &|name| declared.contains(name));
            declared.extend(names);
        }
    }
    declared
}

fn lossy_path<S: serde::Serializer>(path: &Path, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.serialize_str(&path.to_string_lossy())
}

fn lossy_paths<S: serde::Serializer>(paths: &[PathBuf], serializer: S) -> Result<S::Ok, S::Error> {
    serializer.collect_seq(paths.iter().map(|path| path.to_string_lossy()))
}

fn lossy_path_opt<S: serde::Serializer>(
    path: &Option<PathBuf>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    match path {
        Some(path) => lossy_path(path, serializer),
        None => serializer.serialize_none(),
    }
}

/// A path a client cannot be given: it cannot open the file.
fn check_path(database: &mut SchemaDatabase) {
    if database.path.to_str().is_none() {
        database.complete = false;
        database.problems.insert(
            0,
            SchemaProblem {
                severity: Severity::Error.as_str(),
                code: oxabl_schema::SCHEMA0021,
                message: format!(
                    "the path {} is not UTF-8, so the editor cannot open this file",
                    database.path.to_string_lossy()
                ),
                span: None,
                range: None,
            },
        );
    }
}

fn database_name(path: &Path) -> String {
    path.file_stem()
        .map(|stem| stem.to_string_lossy().into_owned())
        .unwrap_or_default()
}

fn unreadable_database(path: &Path, error: &str) -> SchemaDatabase {
    SchemaDatabase {
        name: database_name(path),
        path: path.to_path_buf(),
        complete: false,
        problems: vec![SchemaProblem {
            severity: Severity::Error.as_str(),
            code: oxabl_schema::SCHEMA0001,
            message: format!("cannot read {}: {error}", path.display()),
            span: None,
            range: None,
        }],
        tables: Vec::new(),
        sequences: Vec::new(),
        text: String::new(),
    }
}

/// What the `.df` file at `path` with the text `text` declares, read alone
/// under the loader's merge rules ([`SchemaLoader::load_file_alone`]): a
/// second declaration replaces the first with the loader's warning, a field
/// redeclared with a conflicting type has no type, and the caps apply.
///
/// A field or index of a table that the file does not declare is shown under
/// that table, with `declared: false`, when `declared_before` says an earlier
/// file declares it, as the loader accepts it. Otherwise the loader drops it,
/// and so does the tree, with the loader's error.
///
/// `complete` is false when the parse or the merge reported an error, and when
/// the file has an `UPDATE`, `CHANGE`, `DROP` or `RENAME` directive, which the
/// loader does not apply: the tree then shows a schema that the file changes.
pub fn schema_database(
    path: &Path,
    text: String,
    declared_before: &dyn Fn(&OxablAtom) -> bool,
) -> SchemaDatabase {
    read_database(path, text, declared_before).0
}

/// [`schema_database`], and the tables the file declares.
fn read_database(
    path: &Path,
    text: String,
    declared_before: &dyn Fn(&OxablAtom) -> bool,
) -> (SchemaDatabase, Vec<OxablAtom>) {
    let read = SchemaLoader::load_file_alone(&text, FileId::new(1), declared_before);
    let mut problems: Vec<SchemaProblem> = read.diagnostics.iter().map(problem).collect();
    for skipped in &read.skipped {
        problems.push(SchemaProblem {
            severity: Severity::Warning.as_str(),
            code: SCHEMA0020,
            message: format!(
                "`{}` directive not applied: the tree shows the schema without it",
                skipped.keyword
            ),
            span: Some(skipped.source.span),
            range: None,
        });
    }
    problems.sort_by_key(|p| p.span.map(|s| s.start));
    let complete = read.skipped.is_empty()
        && !read
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error);

    let mut tables: Vec<(&Table, bool)> = read
        .schema
        .tables()
        .map(|(id, table)| (table, !read.extended.contains(&id)))
        .collect();
    let declared = tables
        .iter()
        .filter(|(_, declared)| *declared)
        .map(|(t, _)| t.name.clone())
        .collect();
    // File order, with an extended table at its first `ADD`.
    tables.sort_by_key(|(t, _)| t.source.span.start);
    let database = SchemaDatabase {
        name: database_name(path),
        path: path.to_path_buf(),
        complete,
        problems,
        tables: tables
            .iter()
            .map(|(table, declared)| SchemaTable {
                declared: *declared,
                ..schema_table(table)
            })
            .collect(),
        sequences: read.schema.sequences().map(schema_sequence).collect(),
        text,
    };
    (database, declared)
}

fn problem(diagnostic: &Diagnostic) -> SchemaProblem {
    SchemaProblem {
        severity: diagnostic.severity.as_str(),
        code: diagnostic.code.0,
        message: diagnostic.message.clone(),
        span: Some(diagnostic.span.span),
        range: None,
    }
}

/// One table as the browser shows it, and as a hover can, from a table of any
/// schema: one `.df` file read alone, or the merged schema of the analysis.
/// Every span is in the file of the table's own `source`, except a field or
/// index that a merged schema took from another file.
pub fn schema_table(table: &Table) -> SchemaTable {
    SchemaTable {
        name: table.display_name.clone(),
        declared: true,
        label: table.label.clone(),
        description: table.description.clone(),
        dump_name: table.dump_name.clone(),
        area: table.area.clone(),
        hidden: table.hidden,
        frozen: table.frozen,
        fields: table.fields.iter().map(schema_field).collect(),
        indexes: table.indexes.iter().map(schema_index).collect(),
        span: table.source.span,
        range: None,
    }
}

fn schema_field(field: &Field) -> SchemaField {
    SchemaField {
        name: field.display_name.clone(),
        data_type: type_name(&field.data_type),
        extent: field.extent,
        format: field.format.clone(),
        label: field.label.clone(),
        initial: field.initial.clone(),
        mandatory: field.mandatory,
        description: field.description.clone(),
        decimals: field.decimals,
        case_sensitive: field.case_sensitive,
        help: field.help.clone(),
        order: field.order,
        span: field.source.span,
        range: None,
    }
}

fn schema_index(index: &Index) -> SchemaIndex {
    SchemaIndex {
        name: index.display_name.clone(),
        unique: index.unique,
        primary: index.primary,
        word: index.word,
        inactive: index.inactive,
        description: index.description.clone(),
        fields: index
            .fields
            .iter()
            .map(|f| SchemaIndexField {
                name: f.display_name.clone(),
                ascending: f.ascending,
                abbreviated: f.abbreviated,
            })
            .collect(),
        span: index.source.span,
        range: None,
    }
}

fn schema_sequence(sequence: &Sequence) -> SchemaSequence {
    SchemaSequence {
        name: sequence.display_name.clone(),
        initial: sequence.initial,
        increment: sequence.increment,
        min_value: sequence.min_val,
        max_value: sequence.max_val,
        cycle_on_limit: sequence.cycle_on_limit,
        span: sequence.source.span,
        range: None,
    }
}

fn type_name(data_type: &SchemaType) -> Option<String> {
    let name = match data_type {
        SchemaType::Integer => "integer",
        SchemaType::Int64 => "int64",
        SchemaType::Decimal => "decimal",
        SchemaType::Character => "character",
        SchemaType::Longchar => "longchar",
        SchemaType::Logical => "logical",
        SchemaType::Date => "date",
        SchemaType::Datetime => "datetime",
        SchemaType::DatetimeTz => "datetime-tz",
        SchemaType::Handle => "handle",
        SchemaType::Raw => "raw",
        SchemaType::Recid => "recid",
        SchemaType::Rowid => "rowid",
        SchemaType::Blob => "blob",
        SchemaType::Clob => "clob",
        SchemaType::Unknown(atom) => return Some(atom.to_string()),
        SchemaType::Error => return None,
    };
    Some(name.to_string())
}

impl SchemaDatabase {
    /// Fill every `range` from its span with `place`.
    pub fn place(&mut self, mut place: impl FnMut(Span) -> TextRange) {
        for problem in &mut self.problems {
            problem.range = problem.span.map(&mut place);
        }
        for table in &mut self.tables {
            table.place(&mut place);
        }
        for sequence in &mut self.sequences {
            sequence.range = Some(place(sequence.span));
        }
    }

    /// Every span of the file's nodes and problems.
    pub fn spans(&self) -> Vec<Span> {
        let mut spans: Vec<Span> = self.problems.iter().filter_map(|p| p.span).collect();
        for table in &self.tables {
            spans.push(table.span);
            spans.extend(table.fields.iter().map(|f| f.span));
            spans.extend(table.indexes.iter().map(|i| i.span));
        }
        spans.extend(self.sequences.iter().map(|s| s.span));
        spans
    }

    /// The table `name` of this file, case-insensitive, as ABL compares names.
    pub fn table(&self, name: &str) -> Option<&SchemaTable> {
        let folded = fold_atom(name);
        self.tables
            .iter()
            .find(|table| fold_atom(&table.name) == folded)
    }
}

impl SchemaTable {
    /// Fill every `range` of the table and its fields and indexes.
    pub fn place(&mut self, place: &mut impl FnMut(Span) -> TextRange) {
        self.range = Some(place(self.span));
        for field in &mut self.fields {
            field.range = Some(place(field.span));
        }
        for index in &mut self.indexes {
            index.range = Some(place(index.span));
        }
    }
}
