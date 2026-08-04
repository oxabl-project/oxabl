//! JSON dump for the [`Semantic`] model plus lint-diagnostics envelope.
//!
//! `oxabl_analyze` lives outside `oxabl_semantic` so the semantic crate
//! stays free of any `serde_json` dependency — formatter, LSP, and future
//! workspace consumers shouldn't transitively pull a JSON encoder just to
//! reach the side-table model.
//!
//! The dump uses **per-section versioning** so breaking changes to any one
//! section (scopes, symbols, references, types, diagnostics, preproc, coverage,
//! dependencies) bump only that section's version, not the whole envelope. Every
//! version lives in exactly one place — `section_versions` — so the two dump
//! entry points cannot report different numbers for the same section.
//!
//! `symbols` section v2: the symbol table now includes schema-derived
//! entries — synthesized `field` symbols for schema-validated field
//! references and default-`buffer` symbols for bare table names (both
//! marked with `declaration == NodeId::DUMMY`, i.e. `u32::MAX`).
//!
//! `symbols` v3 / `references` v2: cross-file resolution. A symbol row says
//! where it came from (`origin`), what its declared type is *and which file
//! declared it* (`data_type_source`), and what its class header named
//! (`supertypes`); a reference row says whether the symbol it resolved to is
//! local or cross-file (`origin`). Both are facts about an existing section's
//! rows, which is why neither spawned a sibling section.
//!
//! `symbols` v4: a cross-file row's `data_type` is populated. No key was added —
//! the field changed *meaning*. An inherited member's declared type used to be
//! held off `Symbol::data_type` so it could not reach the type lattice, which made
//! its absence a reliable marker for "this row is a cross-file member"; the type
//! is on the symbol now and the rules judge it. Branch on `data_type_source`
//! instead.
//!
//! ```text
//! {
//!   "envelope": 1,
//!   "sections": {
//!     "scopes": 1,
//!     "symbols": 4,
//!     "types": 1,
//!     "references": 2,
//!     "diagnostics": 1,
//!     "preproc": 1,
//!     "coverage": 1,
//!     "dependencies": 2
//!   },
//!   "schema_revision": 0,
//!   "scopes": [ ... ],
//!   "symbols": [ ... ],
//!   "references": [ ... ],
//!   "types": [ ... ],
//!   "diagnostics": [ ... ],
//!   "preproc": [ ... ],
//!   "coverage": { "unjudged_symbols": 0 },
//!   "dependencies": { "index_revision": 0, "files": [], "unresolved": [] }
//! }
//! ```
//!
//! `preproc` and `coverage` were, until the CLI moved onto the shared pipelines,
//! keys the `analyze` subcommand spliced into this document *after* the library
//! handed it back. That made them invisible to `--format text`, unversioned, and
//! impossible for any other consumer of these functions to receive. They are
//! ordinary sections now.

mod collect;

pub use collect::{
    CollectedDiagnostic, CollectedDiagnostics, DiagnosticSource, DirectInclude, ExpandedFile,
    collect_diagnostics, collect_from_expanded, collect_with_model, expand_source, is_loud,
};
// Re-exported so a consumer of an expansion need not also depend on the
// preprocessor just to name the unresolved-include rows the expansion carries.
pub use oxabl_preprocessor::UnresolvedInclude;

use oxabl_ast::{NodeId, Statement};
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{
    AnalysisContext, NamespaceId, Resolution, ResolvedType, ScopeId, ScopeKind, Semantic, SymbolId,
    SymbolKind, UnresolvedReason,
};
use serde::Serialize;
use serde_json::{Map, Value, json};

/// Current envelope version. Bump only on **breaking structural** changes to
/// the outermost JSON object (e.g. moving a section out into its own file).
pub const ENVELOPE_VERSION: u32 = 1;

/// The `sections` map: one version per section, and the **only** place those
/// numbers are written.
///
/// Both dump entry points call this. That matters more than it looks: the map
/// used to be built inline in each of them, so adding a section meant editing two
/// literal lists and a version bump applied to one of them was a silent
/// divergence a consumer would only notice by diffing two dumps of the same file.
///
/// Bump a section here when *that section's* shape changes; leave
/// [`ENVELOPE_VERSION`] alone unless the outer object itself is restructured.
///
/// * `scopes` 1, `types` 1, `diagnostics` 1 — unchanged since the envelope was
///   introduced.
/// * `symbols` 2 — schema-derived synthetic entries (`declaration ==
///   NodeId::DUMMY`) appear when a schema is loaded.
/// * `symbols` 3 — cross-file rows: `origin`, `data_type_source`, `supertypes`.
/// * `symbols` 4 — no new key, but `data_type` **changes meaning** for a
///   cross-file row: it was reliably absent, because the type was held off
///   `Symbol::data_type` to keep it out of the type lattice, and it is populated
///   now that the rules judge that population. A consumer branching on its
///   absence to detect a cross-file member would silently change behavior, which
///   is what a section version exists to announce. `data_type_source` is the
///   field to branch on instead.
/// * `references` 2 — a resolved row carries the `origin` of the symbol it
///   resolved to, so a cross-file resolution is distinguishable from a local one.
/// * `preproc` 1, `coverage` 1 — sections promoted from keys the CLI used to
///   splice in after the fact.
/// * `dependencies` 1 — cross-file *index* state: which files the run consulted
///   and which class lookups came back empty. Its own section because it is a
///   property of neither a symbol nor a reference.
/// * `dependencies` 2 — an `unresolved` row's `reason` strings changed.
///   `not_found_in_workspace` split into `absent_from_workspace` (searched, no
///   such file) and `present_but_unusable` (located, unreadable or unparseable),
///   because only the first licenses telling a user the name does not exist.
fn section_versions() -> Value {
    let mut sections = Map::new();
    sections.insert("scopes".into(), json!(1));
    sections.insert("symbols".into(), json!(4));
    sections.insert("types".into(), json!(1));
    sections.insert("references".into(), json!(2));
    sections.insert("diagnostics".into(), json!(1));
    sections.insert("preproc".into(), json!(1));
    sections.insert("coverage".into(), json!(1));
    sections.insert("dependencies".into(), json!(2));
    Value::Object(sections)
}

/// Produce a stable, versioned JSON document describing a file's semantic
/// analysis. Includes lint diagnostics from [`oxabl_lint::lint_file`].
pub fn dump_json(
    program: &[Statement],
    sem: &Semantic,
    ctx: &AnalysisContext,
    include_lint: bool,
) -> Value {
    let lint_diags: Vec<Diagnostic> = if include_lint {
        oxabl_lint::lint_file(program, sem, ctx)
    } else {
        Vec::new()
    };

    json!({
        "envelope": ENVELOPE_VERSION,
        "sections": section_versions(),
        "schema_revision": sem.schema_revision.raw(),
        "scopes": scopes_json(sem),
        "symbols": symbols_json(sem),
        "references": references_json(sem),
        "types": types_json(sem),
        "diagnostics": diagnostics_json(sem, &lint_diags),
        // Empty by construction, not by omission: this entry point is handed an
        // already-parsed program and never ran a preprocessor, so there is no
        // preprocessor diagnostic to report. The section is still present so a
        // consumer can index it unconditionally.
        "preproc": Value::Array(Vec::new()),
        "coverage": coverage_json(sem),
        "dependencies": dependencies_json(sem),
    })
}

/// Produce the versioned JSON document, sourcing the `diagnostics` section
/// from a pre-computed [`CollectedDiagnostics`] instead of re-running the lint
/// pass internally. This is the path the CLI `analyze` command uses so its
/// diagnostic set is byte-for-byte the collector's (R7): the non-diagnostic
/// sections (scopes, symbols, references, types) still come from `sem`, but the
/// `diagnostics` array is built from the collector's parse / semantic / lint
/// entries.
///
/// Preprocessor diagnostics stay out of `diagnostics` — they are not findings
/// about the source, they are coverage warnings about the run — but they are no
/// longer *dropped*: they get their own `preproc` section, built from the same
/// `collected` set, so a caller that only reads this document still sees them.
/// The `coverage` section reports [`unjudged_symbol_count`] for the same reason.
/// Both used to be keys the CLI spliced in afterwards, which meant every other
/// caller of this function silently lost them.
pub fn dump_json_with_diagnostics(sem: &Semantic, collected: &CollectedDiagnostics) -> Value {
    json!({
        "envelope": ENVELOPE_VERSION,
        "sections": section_versions(),
        "schema_revision": sem.schema_revision.raw(),
        "scopes": scopes_json(sem),
        "symbols": symbols_json(sem),
        "references": references_json(sem),
        "types": types_json(sem),
        "diagnostics": collected_diagnostics_json(collected),
        "preproc": preproc_json(collected),
        "coverage": coverage_json(sem),
        "dependencies": dependencies_json(sem),
    })
}

/// How many symbols the count-gated lint rules could not fully judge.
///
/// A symbol carrying `SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT` appears
/// inside a statement form the parser recognizes but does not model, so its
/// read/write counts describe only part of what the code does and
/// `unused-variable`, `assigned-but-never-read` and `block-var-used-outside`
/// all decline to fire for it.
///
/// Exposed so a CLI or editor can tell the user that a file went *partly* blind
/// rather than letting it look clean. Nobody reaches for the analyze dump to
/// find a diagnostic they never saw, so listing the flag per-symbol (which the
/// dump does) is not a substitute for a count at the surface where diagnostics
/// are read.
pub fn unjudged_symbol_count(sem: &Semantic) -> usize {
    sem.symbols
        .iter()
        .filter(|(_, s)| {
            s.flags
                .contains(oxabl_semantic::SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT)
        })
        .count()
}

/// Human-oriented text rendering. Compact, not stable across versions — if
/// you need stability, dump to JSON. Used for interactive `oxabl analyze`
/// runs without `--format json`.
pub fn dump_text(program: &[Statement], sem: &Semantic, ctx: &AnalysisContext) -> String {
    use std::fmt::Write;
    let mut out = String::new();
    write_scopes_and_symbols(&mut out, sem);

    let diags = oxabl_lint::lint_file(program, sem, ctx);
    writeln!(
        out,
        "\n=== Diagnostics ({} semantic + {} lint) ===",
        sem.diagnostics.len(),
        diags.len()
    )
    .ok();
    for d in sem.diagnostics.iter().chain(diags.iter()) {
        writeln!(out, "  [{}] {:?} {}", d.code.0, d.severity, d.message).ok();
    }

    // This entry point never ran a preprocessor, so the `preproc` section has no
    // text counterpart here; coverage and dependencies do.
    write_coverage(&mut out, sem);
    write_dependencies(&mut out, sem);
    out
}

/// Text rendering that sources its diagnostics from the shared collector (the
/// CLI `analyze --format text` path). Scopes and symbols come from `sem`; the
/// diagnostics list is the collector's parse / semantic / lint entries.
///
/// The preprocessor and coverage sections are rendered here too — they were
/// JSON-only for as long as the CLI spliced them in, so `--format text` used to
/// be strictly less informative than `--format json` about the *same* run.
pub fn dump_text_with_diagnostics(sem: &Semantic, collected: &CollectedDiagnostics) -> String {
    use std::fmt::Write;
    let mut out = String::new();
    write_scopes_and_symbols(&mut out, sem);

    let diags: Vec<&Diagnostic> = collected
        .all()
        .filter(|c| c.source != DiagnosticSource::Preproc)
        .map(|c| &c.diagnostic)
        .collect();
    writeln!(out, "\n=== Diagnostics ({}) ===", diags.len()).ok();
    for d in diags {
        writeln!(out, "  [{}] {:?} {}", d.code.0, d.severity, d.message).ok();
    }

    let preproc: Vec<&Diagnostic> = collected
        .by_source(DiagnosticSource::Preproc)
        .map(|c| &c.diagnostic)
        .collect();
    writeln!(out, "\n=== Preprocessor ({}) ===", preproc.len()).ok();
    for d in preproc {
        writeln!(out, "  [{}] {:?} {}", d.code.0, d.severity, d.message).ok();
    }

    write_coverage(&mut out, sem);
    write_dependencies(&mut out, sem);
    out
}

/// The `coverage` section's text form: how much of the file the count-gated lint
/// rules could not judge. Always printed, including at zero — unlike the CLI's
/// stderr note, which stays silent at zero because a line that always appears is
/// a line users learn to skip. A *section* is different: a dump reader indexing
/// into it needs it to exist.
fn write_coverage(out: &mut String, sem: &Semantic) {
    use std::fmt::Write;
    writeln!(out, "\n=== Coverage ===").ok();
    writeln!(out, "  unjudged symbols: {}", unjudged_symbol_count(sem)).ok();
}

/// The `dependencies` section's text form: the run's cross-file index state.
///
/// Rendered by **both** text entry points, and from the same JSON the sections
/// carry, so `--format text` cannot become less informative about a run than
/// `--format json` — which is exactly what happened to `preproc` and `coverage`
/// for as long as the CLI spliced them into the finished document.
///
/// Always printed, including when nothing cross-file happened: `index revision: 0`
/// is the fact that no index was attached, and it is what tells a reader that an
/// empty unresolved list means nothing was looked at.
fn write_dependencies(out: &mut String, sem: &Semantic) {
    use std::fmt::Write;
    let deps = dependencies_json(sem);
    let files = deps["files"].as_array().map(Vec::as_slice).unwrap_or(&[]);
    let unresolved = deps["unresolved"]
        .as_array()
        .map(Vec::as_slice)
        .unwrap_or(&[]);

    writeln!(out, "\n=== Dependencies ===").ok();
    writeln!(
        out,
        "  index revision: {}",
        deps["index_revision"].as_u64().unwrap_or(0)
    )
    .ok();
    writeln!(out, "  files consulted ({}):", files.len()).ok();
    for f in files {
        writeln!(
            out,
            "    [{}] {} {}",
            f["file"].as_u64().unwrap_or(0),
            f["via"].as_str().unwrap_or("?"),
            f["name"].as_str().unwrap_or("?"),
        )
        .ok();
    }
    writeln!(out, "  unresolved lookups ({}):", unresolved.len()).ok();
    for u in unresolved {
        let span = match (u["span"]["start"].as_u64(), u["span"]["end"].as_u64()) {
            (Some(s), Some(e)) => format!(" @{s}..{e}"),
            // A name this file does not spell — an ancestor reached through a
            // supertype — has no span here, and an invented one would point at
            // unrelated bytes.
            _ => String::new(),
        };
        writeln!(
            out,
            "    {} {} {}{span}",
            u["via"].as_str().unwrap_or("?"),
            u["name"].as_str().unwrap_or("?"),
            u["reason"].as_str().unwrap_or("?"),
        )
        .ok();
    }
}

fn write_scopes_and_symbols(out: &mut String, sem: &Semantic) {
    use std::fmt::Write;

    writeln!(out, "=== Scopes ({}) ===", sem.scope_tree.len()).ok();
    for (id, s) in sem.scope_tree.iter() {
        writeln!(
            out,
            "  [{:>2}] {:<14} parent={:<3} owner={:>4} bindings={}",
            id.raw(),
            format!("{:?}", s.kind),
            match s.parent {
                Some(p) => p.raw().to_string(),
                None => "—".into(),
            },
            s.owner_node.as_u32(),
            s.bindings.iter().map(|b| b.len()).sum::<usize>(),
        )
        .ok();
    }

    writeln!(out, "\n=== Symbols ({}) ===", sem.symbols.len()).ok();
    for (id, sym) in sem.symbols.iter() {
        // The same display type and origin the JSON rows carry, through the same
        // two helpers — a symbol whose type lives in the inherited-member side map
        // must not read as untyped here while the JSON reports its return type.
        let (ty, ty_source) = symbol_display_type(sem, id, sym);
        writeln!(
            out,
            "  [{:>3}] {:<10} {:<14} scope={:<3} reads={} writes={} ty={}{} origin={}",
            id.raw(),
            format!("{:?}", sym.kind),
            sym.name.as_ref(),
            sym.declared_in.raw(),
            sym.read_count,
            sym.write_count,
            ty.unwrap_or_else(|| "—".into()),
            match ty_source {
                // Only the unusual provenance is spelled out: a declared type is
                // what a reader assumes, and tagging every row with it would be
                // noise on the common line.
                Some("inherited") => "(inherited)",
                Some(_) | None => "",
            },
            symbol_origin(sem, id, sym),
        )
        .ok();
    }
}

// ---------------------------------------------------------------------------
// Section builders
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct ScopeRow {
    id: u32,
    kind: &'static str,
    parent: Option<u32>,
    owner_node: u32,
    bindings: Vec<BindingRow>,
}

#[derive(Serialize)]
struct BindingRow {
    namespace: &'static str,
    name: String,
    symbol: u32,
}

fn scopes_json(sem: &Semantic) -> Value {
    let rows: Vec<ScopeRow> = sem
        .scope_tree
        .iter()
        .map(|(id, s)| ScopeRow {
            id: id.raw(),
            kind: scope_kind_str(s.kind),
            parent: s.parent.map(ScopeId::raw),
            owner_node: s.owner_node.as_u32(),
            bindings: s
                .bindings
                .iter()
                .enumerate()
                .flat_map(|(ns_idx, map)| {
                    let ns = namespace_from_index(ns_idx);
                    map.iter().map(move |(name, sym)| BindingRow {
                        namespace: namespace_str(ns),
                        name: name.as_ref().to_string(),
                        symbol: sym.raw(),
                    })
                })
                .collect(),
        })
        .collect();
    serde_json::to_value(rows).unwrap_or(Value::Null)
}

#[derive(Serialize)]
struct SymbolRow {
    id: u32,
    name: String,
    namespace: &'static str,
    kind: &'static str,
    declared_in: u32,
    declaration: u32,
    read_count: u32,
    write_count: u32,
    flags: Vec<&'static str>,
    data_type: Option<String>,
    /// Where the value in `data_type` was read from. `None` exactly when
    /// `data_type` is `None`; otherwise `"declared"` or `"inherited"` — see
    /// [`symbol_display_type`] for why those are two different places.
    data_type_source: Option<&'static str>,
    /// What kind of thing this row is: a declaration in the file under analysis,
    /// a seeded built-in, a schema-derived synthetic entry, or a cross-file
    /// symbol synthesized from the workspace index.
    origin: &'static str,
    /// The supertypes this symbol's `CLASS` / `INTERFACE` header named, as
    /// written and where written. Absent (rather than an empty array) on every
    /// row that is not a class or interface with a supertype, which is nearly all
    /// of them — a hundred built-in rows do not need the key.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    supertypes: Vec<SupertypeRow>,
}

/// One name a class or interface header listed, plus its span in the file under
/// analysis. The relation is carried explicitly rather than implied by which
/// array the row sits in: an interface's `INHERITS` list is recorded as
/// `implements` in the model, and flattening both into one array with a tag keeps
/// that from reading as a bug in the dump.
#[derive(Serialize)]
struct SupertypeRow {
    relation: &'static str,
    name: String,
    start: u32,
    end: u32,
}

fn symbols_json(sem: &Semantic) -> Value {
    let rows: Vec<SymbolRow> = sem
        .symbols
        .iter()
        .map(|(id, sym)| {
            let (data_type, data_type_source) = symbol_display_type(sem, id, sym);
            SymbolRow {
                id: id.raw(),
                name: sym.name.as_ref().to_string(),
                namespace: namespace_str(sym.namespace),
                kind: symbol_kind_str(sym.kind),
                declared_in: sym.declared_in.raw(),
                declaration: sym.declaration.as_u32(),
                read_count: sym.read_count,
                write_count: sym.write_count,
                flags: symbol_flags_list(sym.flags),
                data_type,
                data_type_source,
                origin: symbol_origin(sem, id, sym),
                supertypes: supertype_rows(sem, id),
            }
        })
        .collect();
    serde_json::to_value(rows).unwrap_or(Value::Null)
}

/// The type to *display* for a symbol, and which file declared it.
///
/// One type channel now. An inherited member's declared type used to be parked in
/// `SymbolTable::inherited_member_type` so that it was observable here without
/// being usable by the rules; the type lives on `Symbol::data_type` like any
/// other, and the rules judge it. What survives is `data_type_source`, which
/// answers a question the type itself cannot: whether the declaration that
/// supplied it is in this file or in another one the index reached.
fn symbol_display_type(
    sem: &Semantic,
    id: SymbolId,
    sym: &oxabl_semantic::Symbol,
) -> (Option<String>, Option<&'static str>) {
    match &sym.data_type {
        None => (None, None),
        Some(ty) => {
            // A synthesized cross-file symbol carries a type only because the
            // declaring file wrote one — there is no local declaration to point
            // at. `symbol_origin` is the single derivation of "which world", so
            // the two fields cannot disagree about a row.
            let source = if sym.declaration == NodeId::DUMMY
                && symbol_origin(sem, id, sym) == "cross_file"
            {
                "inherited"
            } else {
                "declared"
            };
            (Some(render_type(ty)), Some(source))
        }
    }
}

/// Which world a symbol came from, in one string a consumer can branch on.
///
/// `declaration == NodeId::DUMMY` already marks *synthesized* rows, but it lumps
/// three unlike populations together — seeded built-ins, schema-derived fields and
/// default buffers, and cross-file symbols from the workspace index — and a
/// consumer asking "is this resolution cross-file?" needs the third apart from the
/// first two.
fn symbol_origin(sem: &Semantic, id: SymbolId, sym: &oxabl_semantic::Symbol) -> &'static str {
    if sym.declaration != NodeId::DUMMY {
        return "declared";
    }
    // Seeded before any source is read, so it is neither schema- nor
    // index-derived. Checked first because a built-in carries no other marker.
    if matches!(sym.kind, SymbolKind::BuiltIn) {
        return "builtin";
    }
    // The index's own footprints: a resolved literal `RUN` target's file, a
    // `SHARED` consumer's producer file. Either one means the index minted or
    // linked this symbol. An inherited member used to be recognized by its entry
    // in the type side map; with the type promoted onto the symbol, the
    // `SymbolKind` arms below carry that case — nothing else synthesizes a
    // `Function` or `Property`.
    if sem.symbols.program_file(id).is_some() || sem.symbols.shared_producer(id).is_some() {
        return "cross_file";
    }
    match sym.kind {
        // A synthesized type symbol can only have come from the index: the
        // schema declares tables and fields, never classes.
        SymbolKind::Class | SymbolKind::Interface => "cross_file",
        // A synthesized method with no recorded type (a `VOID` inherited method)
        // and a synthesized procedure with no recorded file are still cross-file:
        // nothing else synthesizes into these namespaces.
        SymbolKind::Function | SymbolKind::Property | SymbolKind::Procedure => "cross_file",
        // What the resolve pass synthesizes from a loaded schema.
        SymbolKind::Field | SymbolKind::Buffer | SymbolKind::TempTable => "schema",
        // No pass synthesizes these; a DUMMY declaration on one is a bug
        // somewhere else, and saying so beats guessing a world for it.
        SymbolKind::Variable
        | SymbolKind::Parameter
        | SymbolKind::Stream
        | SymbolKind::Frame
        | SymbolKind::Event
        | SymbolKind::BuiltIn
        | SymbolKind::Dataset
        | SymbolKind::DataSource => "unknown",
    }
}

/// The supertype rows for a class or interface symbol; empty for everything else.
fn supertype_rows(sem: &Semantic, id: SymbolId) -> Vec<SupertypeRow> {
    let Some(supers) = sem.symbols.supertypes(id) else {
        return Vec::new();
    };
    let row = |relation: &'static str, r: &oxabl_semantic::SupertypeRef| SupertypeRow {
        relation,
        // The **as-written** spelling, not the folded identity: this is what the
        // header says, and a reader comparing it against a file on disk needs the
        // casing. The folded spelling is what `dependencies.unresolved` keys on.
        name: r.name.as_written().to_string(),
        start: r.name_span.start,
        end: r.name_span.end,
    };
    supers
        .inherits
        .iter()
        .map(|r| row("inherits", r))
        .chain(supers.implements.iter().map(|r| row("implements", r)))
        .collect()
}

#[derive(Serialize)]
struct ReferenceRow {
    node: u32,
    resolution: ResolutionKind,
    symbol: Option<u32>,
    name: Option<String>,
    reason: Option<&'static str>,
    /// The [`symbol_origin`] of the symbol this reference resolved to — the key
    /// that makes a cross-file resolution distinguishable from a local one
    /// without joining against the `symbols` section. `None` on an unresolved
    /// row, where `reason` is the informative field.
    origin: Option<&'static str>,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
enum ResolutionKind {
    Resolved,
    Unresolved,
}

fn references_json(sem: &Semantic) -> Value {
    let rows: Vec<ReferenceRow> = sem
        .references
        .iter()
        .map(|(nid, r)| match r {
            Resolution::Resolved(sym) => ReferenceRow {
                node: nid.as_u32(),
                resolution: ResolutionKind::Resolved,
                symbol: Some(sym.raw()),
                name: None,
                reason: None,
                origin: Some(symbol_origin(sem, *sym, sem.symbols.get(*sym))),
            },
            Resolution::Unresolved { name, reason } => ReferenceRow {
                node: nid.as_u32(),
                resolution: ResolutionKind::Unresolved,
                symbol: None,
                name: Some(name.as_ref().to_string()),
                reason: Some(unresolved_reason_str(*reason)),
                origin: None,
            },
        })
        .collect();
    serde_json::to_value(rows).unwrap_or(Value::Null)
}

#[derive(Serialize)]
struct TypeRow {
    node: u32,
    r#type: String,
}

fn types_json(sem: &Semantic) -> Value {
    let rows: Vec<TypeRow> = sem
        .types
        .iter()
        .map(|(nid, t)| TypeRow {
            node: nid.as_u32(),
            r#type: render_type(t),
        })
        .collect();
    serde_json::to_value(rows).unwrap_or(Value::Null)
}

#[derive(Serialize)]
struct DiagnosticRow {
    code: String,
    severity: &'static str,
    message: String,
    span: SpanRow,
    source: &'static str,
}

#[derive(Serialize)]
struct SpanRow {
    file: u32,
    start: u32,
    end: u32,
}

fn diagnostics_json(sem: &Semantic, lint_diags: &[Diagnostic]) -> Value {
    let mut rows: Vec<DiagnosticRow> = sem
        .diagnostics
        .iter()
        .map(|d| diag_row(d, "semantic"))
        .collect();
    rows.extend(lint_diags.iter().map(|d| diag_row(d, "lint")));
    serde_json::to_value(rows).unwrap_or(Value::Null)
}

/// Build the `diagnostics` section from the shared collector's output.
///
/// Only the parse / semantic / lint stages feed this section (in that pipeline
/// order); preprocessor diagnostics go to the `preproc` section instead — see
/// [`preproc_json`] for why they are kept apart. Spans are already resolved to
/// root-buffer coordinates by the collector.
fn collected_diagnostics_json(collected: &CollectedDiagnostics) -> Value {
    let rows: Vec<DiagnosticRow> = collected
        .all()
        .filter(|c| c.source != DiagnosticSource::Preproc)
        .map(|c| diag_row(&c.diagnostic, c.source.as_str()))
        .collect();
    serde_json::to_value(rows).unwrap_or(Value::Null)
}

/// Build the `preproc` section: the collector's loud, root-origin preprocessor
/// diagnostics.
///
/// Its own section rather than an extra `source` in `diagnostics`, because a
/// `PREPROC007` unresolvable include is not a finding *about the source* — it
/// says "part of this file was never seen, so treat the rest of this document as
/// incomplete". A consumer deciding whether to trust the dump reads this
/// section; a consumer listing problems reads `diagnostics`. Merging them would
/// force every such consumer to filter.
///
/// Rows use the same shape as `diagnostics`, so one deserializer serves both.
fn preproc_json(collected: &CollectedDiagnostics) -> Value {
    let rows: Vec<DiagnosticRow> = collected
        .by_source(DiagnosticSource::Preproc)
        .map(|c| diag_row(&c.diagnostic, c.source.as_str()))
        .collect();
    serde_json::to_value(rows).unwrap_or(Value::Null)
}

/// Build the `coverage` section: what the analysis could *not* answer.
///
/// An object rather than a bare `unjudged_symbols` scalar so the next coverage
/// fact has a home. The CLI reached the scalar by splicing a key into the
/// finished document, and a second such fact would have meant a second splice —
/// which is the pattern this section exists to end.
fn coverage_json(sem: &Semantic) -> Value {
    json!({ "unjudged_symbols": unjudged_symbol_count(sem) })
}

/// One other file this run consulted, and what linked it.
#[derive(Serialize, PartialEq, Eq, PartialOrd, Ord)]
struct DependencyFileRow {
    /// The index's own file id — **not** a [`FileId`](oxabl_common::FileId), and
    /// not a path. Only the index implementation that minted it can map it back
    /// to one, which is exactly why a diagnostic may not be anchored at it.
    file: u32,
    /// Which question reached this file: `class` (a supertype chain walk),
    /// `program` (a literal `RUN` target), `shared_producer` (the file whose
    /// `DEFINE NEW [GLOBAL] SHARED` a local `DEFINE SHARED` corresponds to).
    via: &'static str,
    /// The name that was looked up, folded — the index's identity for it.
    name: String,
}

/// One cross-file lookup that produced no link.
#[derive(Serialize, PartialEq, Eq, PartialOrd, Ord)]
struct UnresolvedLookupRow {
    /// The question asked. `class` today; a future index question adds a value
    /// here rather than another section.
    via: &'static str,
    name: String,
    /// The same strings the `references` section's `reason` uses, from the same
    /// [`unresolved_reason_str`] — a consumer learns one vocabulary, not two.
    reason: &'static str,
    /// Where the name is written in the file under analysis, when the file
    /// writes it at all. `None` for an *ancestor* reached through a supertype:
    /// that name appears in another file's header, and inventing a span in this
    /// one would point at unrelated bytes.
    span: Option<LookupSpanRow>,
}

/// A span inside the file under analysis. Deliberately not the `diagnostics`
/// section's [`SpanRow`]: that carries a `file` because a diagnostic can be
/// anchored anywhere, while this is always the analysed file — and these offsets
/// are the model's own **virtual** (post-expansion) coordinates, not the
/// root-resolved ones a diagnostic row carries. This section is a dump of index
/// state, not a finding to render.
#[derive(Serialize, PartialEq, Eq, PartialOrd, Ord)]
struct LookupSpanRow {
    start: u32,
    end: u32,
}

/// Build the `dependencies` section: the run's cross-file index state.
///
/// An **object**, for the reason `coverage` is one: the next index fact should be
/// an added key here, not a ninth section.
///
/// Three keys, all always present — a consumer indexes into them unconditionally,
/// and a file with nothing cross-file about it gets empty arrays rather than
/// missing keys:
///
/// * `index_revision` — the generation of the index this result was computed
///   under, `0` when no index was attached at all. That distinction matters
///   before either array is read: with no index, an empty `unresolved` means
///   *nothing was looked at*, not *everything resolved*.
/// * `files` — every other file a lookup reached. This is the conservative
///   dependency edge set: a change to one of these files can change this file's
///   answers.
/// * `unresolved` — every lookup that came back empty, with the reason and, where
///   the analysed file spells the name, its span. This is what makes an absent
///   parent observable: a supertype that resolves mints no symbol of its own, so
///   the resolved and absent cases are otherwise identical in this document.
///
/// Both arrays are **sorted**, because both are built from hash maps and an
/// unstable order would make two dumps of one file differ.
fn dependencies_json(sem: &Semantic) -> Value {
    let mut files: Vec<DependencyFileRow> = Vec::new();
    let mut unresolved: Vec<UnresolvedLookupRow> = Vec::new();

    for (name, lookup) in sem.symbols.class_lookups() {
        match lookup {
            oxabl_semantic::ClassLookup::Linked(file) => files.push(DependencyFileRow {
                file: file.raw(),
                via: "class",
                name: name.as_ref().to_string(),
            }),
            oxabl_semantic::ClassLookup::Absent => unresolved.push(UnresolvedLookupRow {
                via: "class",
                name: name.as_ref().to_string(),
                reason: unresolved_reason_str(UnresolvedReason::AbsentFromWorkspace),
                span: supertype_span(sem, name),
            }),
            oxabl_semantic::ClassLookup::Unusable => unresolved.push(UnresolvedLookupRow {
                via: "class",
                name: name.as_ref().to_string(),
                reason: unresolved_reason_str(UnresolvedReason::PresentButUnusable),
                span: supertype_span(sem, name),
            }),
            oxabl_semantic::ClassLookup::Unknowable => unresolved.push(UnresolvedLookupRow {
                via: "class",
                name: name.as_ref().to_string(),
                reason: unresolved_reason_str(UnresolvedReason::Unknowable),
                span: supertype_span(sem, name),
            }),
        }
    }

    // The two symbol-linked file facts. Walked over the symbol table rather than
    // over a map of their own because that is the only access the model offers,
    // and the population is a handful of symbols per file at most.
    for (id, _) in sem.symbols.iter() {
        if let Some(file) = sem.symbols.program_file(id) {
            files.push(DependencyFileRow {
                file: file.raw(),
                via: "program",
                name: sem.symbols.get(id).name.as_ref().to_string(),
            });
        }
        if let Some(file) = sem.symbols.shared_producer(id) {
            files.push(DependencyFileRow {
                file: file.raw(),
                via: "shared_producer",
                name: sem.symbols.get(id).name.as_ref().to_string(),
            });
        }
    }

    files.sort();
    files.dedup();
    unresolved.sort();
    unresolved.dedup();

    json!({
        "index_revision": sem.index_revision.raw(),
        "files": files,
        "unresolved": unresolved,
    })
}

/// The span of `folded` in a class or interface header in the file under
/// analysis, if any header names it.
///
/// A linear scan over the symbol table, the way `ClassLattice::class_named`
/// recovers a supertype's identity from its name: this runs once per *failed*
/// cross-file class lookup, which is a handful of names at most.
fn supertype_span(
    sem: &Semantic,
    folded: &oxabl_lexer::oxabl_atom::OxablAtom,
) -> Option<LookupSpanRow> {
    sem.symbols.iter().find_map(|(id, _)| {
        let supers = sem.symbols.supertypes(id)?;
        supers
            .inherits
            .iter()
            .chain(&supers.implements)
            .find(|r| r.name.as_atom() == folded)
            .map(|r| LookupSpanRow {
                start: r.name_span.start,
                end: r.name_span.end,
            })
    })
}

fn diag_row(d: &Diagnostic, source: &'static str) -> DiagnosticRow {
    DiagnosticRow {
        code: d.code.0.to_string(),
        severity: severity_str(d.severity),
        message: d.message.clone(),
        span: SpanRow {
            file: d.span.file.raw(),
            start: d.span.span.start,
            end: d.span.span.end,
        },
        source,
    }
}

// ---------------------------------------------------------------------------
// Enum stringification
// ---------------------------------------------------------------------------

fn scope_kind_str(k: ScopeKind) -> &'static str {
    match k {
        ScopeKind::File => "file",
        ScopeKind::Procedure => "procedure",
        ScopeKind::Function => "function",
        ScopeKind::Class => "class",
        ScopeKind::Interface => "interface",
        ScopeKind::Method => "method",
        ScopeKind::PropertyGet => "property_get",
        ScopeKind::PropertySet => "property_set",
        ScopeKind::Constructor => "constructor",
        ScopeKind::Destructor => "destructor",
        ScopeKind::Block => "block",
        ScopeKind::Catch => "catch",
        ScopeKind::Finally => "finally",
        ScopeKind::Trigger => "trigger",
        ScopeKind::Frame => "frame",
        ScopeKind::TempTable => "temp_table",
        ScopeKind::TriggerProcedure => "trigger_procedure",
    }
}

fn namespace_str(ns: NamespaceId) -> &'static str {
    match ns {
        NamespaceId::Values => "values",
        NamespaceId::Buffers => "buffers",
        NamespaceId::Tables => "tables",
        NamespaceId::Types => "types",
        NamespaceId::Procedures => "procedures",
        NamespaceId::Functions => "functions",
        NamespaceId::Streams => "streams",
        NamespaceId::Frames => "frames",
        NamespaceId::Events => "events",
        NamespaceId::WidgetHandles => "widget_handles",
    }
}

fn namespace_from_index(i: usize) -> NamespaceId {
    NamespaceId::ALL[i]
}

fn symbol_kind_str(k: SymbolKind) -> &'static str {
    match k {
        SymbolKind::Variable => "variable",
        SymbolKind::Parameter => "parameter",
        SymbolKind::Property => "property",
        SymbolKind::Field => "field",
        SymbolKind::TempTable => "temp_table",
        SymbolKind::Buffer => "buffer",
        SymbolKind::Stream => "stream",
        SymbolKind::Frame => "frame",
        SymbolKind::Event => "event",
        SymbolKind::Procedure => "procedure",
        SymbolKind::Function => "function",
        SymbolKind::Class => "class",
        SymbolKind::Interface => "interface",
        SymbolKind::BuiltIn => "builtin",
        SymbolKind::Dataset => "dataset",
        SymbolKind::DataSource => "data_source",
    }
}

fn symbol_flags_list(f: oxabl_semantic::SymbolFlags) -> Vec<&'static str> {
    use oxabl_semantic::SymbolFlags as F;
    let mut out = Vec::new();
    if f.contains(F::NO_UNDO) {
        out.push("no_undo");
    }
    if f.contains(F::STATIC) {
        out.push("static");
    }
    if f.contains(F::ABSTRACT) {
        out.push("abstract");
    }
    if f.contains(F::FINAL) {
        out.push("final");
    }
    if f.contains(F::OVERRIDE) {
        out.push("override");
    }
    if f.contains(F::PARAM_INPUT) {
        out.push("param_input");
    }
    if f.contains(F::PARAM_OUTPUT) {
        out.push("param_output");
    }
    if f.contains(F::PARAM_INPUT_OUT) {
        out.push("param_input_output");
    }
    if f.contains(F::PARAM_RETURN) {
        out.push("param_return");
    }
    if f.contains(F::SHARED) {
        out.push("shared");
    }
    if f.contains(F::NEW_SHARED) {
        out.push("new_shared");
    }
    if f.contains(F::NEW_GLOBAL_SHARED) {
        out.push("new_global_shared");
    }
    if f.contains(F::PUBLIC) {
        out.push("public");
    }
    if f.contains(F::PRIVATE) {
        out.push("private");
    }
    if f.contains(F::PROTECTED) {
        out.push("protected");
    }
    if f.contains(F::PACKAGE_PRIVATE) {
        out.push("package_private");
    }
    if f.contains(F::PROTOTYPE) {
        out.push("prototype");
    }
    // Resolve-computed, and the one flag a user is most likely to be hunting
    // for: it is why a count-gated lint rule stayed silent about this symbol.
    // Without it the dump reports every flag except the one that explains the
    // missing diagnostic.
    if f.contains(F::TOUCHED_BY_UNMODELLED_STATEMENT) {
        out.push("touched_by_unmodelled_statement");
    }
    out
}

fn unresolved_reason_str(r: UnresolvedReason) -> &'static str {
    match r {
        UnresolvedReason::NotInScope => "not_in_scope",
        UnresolvedReason::External => "external",
        UnresolvedReason::NoSchema => "no_schema",
        UnresolvedReason::AbsentFromWorkspace => "absent_from_workspace",
        UnresolvedReason::PresentButUnusable => "present_but_unusable",
        UnresolvedReason::Unknowable => "unknowable",
    }
}

fn severity_str(s: Severity) -> &'static str {
    match s {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Info => "info",
        Severity::Hint => "hint",
    }
}

fn render_type(t: &ResolvedType) -> String {
    match t {
        ResolvedType::Primitive(p) => format!("{p:?}").to_lowercase(),
        ResolvedType::Class(sid) => format!("class#{}", sid.raw()),
        ResolvedType::Buffer(sid) => format!("buffer#{}", sid.raw()),
        ResolvedType::Table(rev, tid) => format!("table#{}/{}", rev.raw(), tid.raw()),
        ResolvedType::Array { element, extent } => {
            let ext = extent
                .map(|n| n.to_string())
                .unwrap_or_else(|| "dyn".into());
            format!("array[{ext}] {}", render_type(element))
        }
        ResolvedType::Unknown => "unknown".into(),
        ResolvedType::Error => "error".into(),
    }
}

// Keep SymbolId / NodeId imports live for section types that embed them.
const _: fn() = || {
    let _: SymbolId = SymbolId::new(0);
    let _: NodeId = NodeId::from_u32(0);
};

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_ast::{DataType, Identifier, Span, Statement, StatementKind, TypeSource};
    use oxabl_common::FileId;
    use oxabl_schema::Schema;
    use oxabl_semantic::analyze_file;

    fn ident(n: &str) -> Identifier {
        Identifier {
            span: Span {
                start: 0,
                end: n.len() as u32,
            },
            name: n.into(),
        }
    }

    fn var_decl(n: &str, ty: DataType) -> Statement {
        Statement::new(StatementKind::VariableDeclaration {
            name: ident(n),
            type_source: TypeSource::Explicit(ty),
            initial_value: None,
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }

    fn run_dump(stmts: Vec<Statement>) -> Value {
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        let sem = analyze_file(&stmts, &ctx);
        dump_json(&stmts, &sem, &ctx, true)
    }

    #[test]
    fn envelope_has_expected_sections() {
        let v = run_dump(vec![var_decl("x", DataType::Integer)]);
        assert_eq!(v["envelope"], 1);
        assert!(v["sections"]["scopes"].is_number());
        assert!(v["scopes"].is_array());
        assert!(v["symbols"].is_array());
        assert!(v["references"].is_array());
        assert!(v["types"].is_array());
        assert!(v["diagnostics"].is_array());
    }

    /// R15: the flag that explains a *missing* lint diagnostic has to be
    /// visible somewhere. `symbol_flags_list` is hand-maintained and would not
    /// fail to compile if the entry were omitted — it would just silently ship a
    /// dump reporting every flag except the useful one.
    #[test]
    fn unmodelled_touch_flag_appears_in_the_symbol_dump() {
        let v = run_dump(vec![
            var_decl("x", DataType::Integer),
            Statement::new(StatementKind::Skipped {
                names: vec![ident("x")],
                may_reference_tables: false,
            }),
        ]);
        let x = v["symbols"]
            .as_array()
            .unwrap()
            .iter()
            .find(|s| s.get("name").and_then(Value::as_str) == Some("x"))
            .expect("x in symbols");
        let flags: Vec<&str> = x["flags"]
            .as_array()
            .unwrap()
            .iter()
            .map(|f| f.as_str().unwrap())
            .collect();
        assert!(
            flags.contains(&"touched_by_unmodelled_statement"),
            "got {flags:?}"
        );
        // The counts stay exact — the flag is the only signal.
        assert_eq!(x["read_count"], 0);
        assert_eq!(x["write_count"], 0);
    }

    #[test]
    fn schema_revision_included() {
        let v = run_dump(vec![]);
        assert!(v["schema_revision"].is_number());
    }

    #[test]
    fn builtins_appear_in_symbols() {
        let v = run_dump(vec![]);
        let symbols = v["symbols"].as_array().unwrap();
        let names: Vec<&str> = symbols
            .iter()
            .map(|s| s.get("name").and_then(Value::as_str).unwrap())
            .collect();
        for expected in ["session", "error-status", "self", "super", "this-object"] {
            assert!(names.contains(&expected), "{expected} not in {names:?}");
        }
    }

    #[test]
    fn user_variable_symbol_serialized_with_kind() {
        let v = run_dump(vec![var_decl("x", DataType::Integer)]);
        let x = v["symbols"]
            .as_array()
            .unwrap()
            .iter()
            .find(|s| s["name"] == "x")
            .unwrap();
        assert_eq!(x["kind"], "variable");
        assert_eq!(x["namespace"], "values");
        assert_eq!(x["data_type"], "integer");
    }

    #[test]
    fn dump_is_valid_json_string() {
        let v = run_dump(vec![var_decl("x", DataType::Integer)]);
        let s = serde_json::to_string(&v).unwrap();
        // Round-trip parses cleanly.
        let back: Value = serde_json::from_str(&s).unwrap();
        assert_eq!(back["envelope"], 1);
    }

    #[test]
    fn diagnostics_entry_tagged_with_source() {
        let v = run_dump(vec![var_decl("unused", DataType::Integer)]);
        let diags = v["diagnostics"].as_array().unwrap();
        // Should include LINT0002 unused-variable from the lint pass.
        let lint_sources: Vec<&str> = diags
            .iter()
            .filter_map(|d| d.get("source").and_then(Value::as_str))
            .collect();
        assert!(lint_sources.contains(&"lint"));
    }

    #[test]
    fn dump_text_contains_scopes_and_symbols_headers() {
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        let stmts = vec![var_decl("x", DataType::Integer)];
        let sem = analyze_file(&stmts, &ctx);
        let text = dump_text(&stmts, &sem, &ctx);
        assert!(text.contains("=== Scopes"));
        assert!(text.contains("=== Symbols"));
        assert!(text.contains("=== Diagnostics"));
    }

    #[test]
    fn duplicate_decl_surfaces_in_diagnostics() {
        let v = run_dump(vec![
            var_decl("dup", DataType::Integer),
            var_decl("dup", DataType::Integer),
        ]);
        let diags = v["diagnostics"].as_array().unwrap();
        assert!(diags.iter().any(|d| d["code"] == "SEM0001"));
    }

    // ---- Schema-derived symbols (symbols section v2) ----------------------

    use oxabl_schema::test_support::customer_schema as test_schema;

    /// Kept named for the version that introduced schema-derived rows; the
    /// pinned number moved to 3 when the cross-file keys landed. It is the one
    /// test that asserts a section version literally, on purpose — the schema-
    /// synthesized population is exactly what that version records.
    #[test]
    fn symbols_section_v2_includes_schema_derived_symbols() {
        use oxabl_ast::{BufferTarget, Expression, ExpressionKind, XmlSerializeOptions};

        let schema = test_schema();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        // `Customer.Name` with no DEFINE BUFFER: the qualifier binds a
        // synthesized default buffer and the field a synthesized Field
        // symbol — both must appear in the dump.
        let fa = Expression::with_id(
            NodeId::from_u32(2),
            oxabl_ast::Span::DUMMY,
            ExpressionKind::FieldAccess {
                qualifier: Box::new(Expression::with_id(
                    NodeId::from_u32(3),
                    oxabl_ast::Span::DUMMY,
                    ExpressionKind::Identifier(ident("Customer")),
                )),
                field: ident("Name"),
            },
        );
        let stmts = vec![
            Statement::with_id(
                NodeId::from_u32(4),
                oxabl_ast::Span::DUMMY,
                StatementKind::DefineBuffer {
                    name: ident("bCust"),
                    target: BufferTarget::Table(ident("Customer")),
                    preselect: false,
                    label: None,
                    xml_options: XmlSerializeOptions::default(),
                    is_new_shared: false,
                    is_shared: false,
                    is_new_global_shared: false,
                },
            ),
            Statement::with_id(
                NodeId::from_u32(5),
                oxabl_ast::Span::DUMMY,
                StatementKind::ExpressionStatement(fa),
            ),
        ];
        let sem = analyze_file(&stmts, &ctx);
        let v = dump_json(&stmts, &sem, &ctx, true);

        assert_eq!(v["sections"]["symbols"], 4);
        let symbols = v["symbols"].as_array().unwrap();
        // Synthesized default buffer for `Customer` (kind buffer,
        // declaration = NodeId::DUMMY = u32::MAX).
        let synth_buffer = symbols
            .iter()
            .find(|s| s["name"] == "customer" && s["kind"] == "buffer")
            .expect("synthesized default buffer in dump");
        assert_eq!(synth_buffer["declaration"], u32::MAX);
        // Synthesized field symbol for `Name`, typed from the schema.
        let synth_field = symbols
            .iter()
            .find(|s| s["name"] == "name" && s["kind"] == "field")
            .expect("synthesized field in dump");
        assert_eq!(synth_field["declaration"], u32::MAX);
        assert_eq!(synth_field["data_type"], "character");
    }

    #[test]
    fn every_unresolved_reason_serializes_to_a_distinct_snake_case_string() {
        // The dump's `reason` key is a consumer-visible contract: the two
        // pre-existing strings must not shift, and each new reason needs its
        // own name rather than collapsing into `external`.
        let all = [
            (UnresolvedReason::NotInScope, "not_in_scope"),
            (UnresolvedReason::External, "external"),
            (UnresolvedReason::NoSchema, "no_schema"),
            (
                UnresolvedReason::AbsentFromWorkspace,
                "absent_from_workspace",
            ),
            (UnresolvedReason::PresentButUnusable, "present_but_unusable"),
            (UnresolvedReason::Unknowable, "unknowable"),
        ];
        for (reason, expected) in all {
            assert_eq!(unresolved_reason_str(reason), expected);
        }
        let mut names: Vec<&str> = all.iter().map(|(_, s)| *s).collect();
        names.sort_unstable();
        let count = names.len();
        names.dedup();
        assert_eq!(names.len(), count, "reason strings must be distinct");
    }

    // ---- Cross-file facts (symbols v3, references v2, dependencies 1) -----
    //
    // These run the real `BatchIndex` over an in-memory filesystem rather than a
    // hand-written stub, so what they pin is the envelope every client emits
    // rather than a shape only this test can produce. Fixtures are synthetic ABL.

    use oxabl_index::BatchIndex;
    use oxabl_workspace::InMemoryFileSystem;

    /// The parent class: one public method with a declared return type.
    const CALC_BASE: &str = "CLASS orders.calc-base:\n    METHOD PUBLIC INTEGER calc-total():\n        RETURN 0.\n    END METHOD.\nEND CLASS.";
    /// Where a `/src` include-path entry makes the index look for it: a qualified
    /// name maps onto a relative path by replacing dots with separators.
    const CALC_BASE_PATH: &str = "/src/orders/calc-base.cls";
    /// The child: calls the inherited method, so the chain walk actually runs.
    const CHILD: &str = "CLASS orders.child INHERITS orders.calc-base:\n    METHOD PUBLIC VOID run-it():\n        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n        v-total = calc-total().\n        MESSAGE v-total.\n    END METHOD.\nEND CLASS.";

    /// Analyze `source` against a batch index over `workspace`, rooted at `/src`,
    /// and return both the dump and the model behind it.
    fn dump_with_index(source: &str, workspace: &[(&str, &str)]) -> (Value, String) {
        let mut fs = InMemoryFileSystem::new();
        for (path, contents) in workspace {
            fs.insert(std::path::PathBuf::from(*path), *contents);
        }
        let dirs = [std::path::PathBuf::from("/src")];
        let index = BatchIndex::new(&fs, &dirs);
        let schema = Schema::empty();
        let tokens = oxabl_lexer::tokenize(source);
        let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
        assert!(
            program.errors.is_empty(),
            "fixture must parse cleanly: {:?}",
            program.errors
        );
        let ctx = AnalysisContext::new(FileId::new(1), source, &schema).with_index(&index);
        let sem = analyze_file(&program.statements, &ctx);
        (
            dump_json(&program.statements, &sem, &ctx, true),
            dump_text(&program.statements, &sem, &ctx),
        )
    }

    fn rows(v: &Value, key: &str) -> Vec<Value> {
        v[key].as_array().cloned().unwrap_or_default()
    }

    /// AE1. The inherited member is a symbol **with its return type**, and the
    /// call site is a reference resolving to it — the two facts that made
    /// `symbols` and `references` bump instead of gaining sibling sections.
    #[test]
    fn a_resolved_inherited_member_appears_as_a_symbol_with_its_return_type() {
        let (v, _) = dump_with_index(CHILD, &[(CALC_BASE_PATH, CALC_BASE)]);

        let member = rows(&v, "symbols")
            .into_iter()
            .find(|s| s["name"] == "calc-total")
            .expect("the inherited member is in the symbol table");
        // The type is on `Symbol::data_type`, the same field a local declaration
        // populates, and it reaches the type lattice from there — that is the
        // point of the promotion. `data_type_source` still says which file
        // declared it, which the type alone cannot.
        assert_eq!(member["data_type"], "integer");
        assert_eq!(member["data_type_source"], "inherited");
        assert_eq!(member["origin"], "cross_file");
        assert_eq!(
            member["declaration"],
            u32::MAX,
            "synthesized: no local node"
        );
        assert_eq!(member["kind"], "function");

        let sid = member["id"].as_u64().expect("symbol id");
        let call = rows(&v, "references")
            .into_iter()
            .find(|r| r["symbol"].as_u64() == Some(sid))
            .expect("the call site resolves to the inherited member");
        assert_eq!(call["resolution"], "resolved");
        assert_eq!(
            call["origin"], "cross_file",
            "a cross-file resolution must be distinguishable from a local one"
        );

        // And the class's own header link is on the class row, as written.
        let class = rows(&v, "symbols")
            .into_iter()
            .find(|s| s["name"] == "orders.child")
            .expect("the class itself");
        assert_eq!(class["supertypes"][0]["relation"], "inherits");
        assert_eq!(class["supertypes"][0]["name"], "orders.calc-base");

        // The consulted file is a dependency edge: a change to it can change
        // this file's answers.
        let files = rows(&v, "dependencies").into_iter().collect::<Vec<_>>();
        assert!(files.is_empty(), "dependencies is an object, not an array");
        let consulted = v["dependencies"]["files"].as_array().unwrap();
        assert!(
            consulted
                .iter()
                .any(|f| f["via"] == "class" && f["name"] == "orders.calc-base"),
            "the parent's file must appear as consulted, got {consulted:?}"
        );
        assert!(
            v["dependencies"]["unresolved"]
                .as_array()
                .unwrap()
                .is_empty()
        );
        assert!(v["dependencies"]["index_revision"].as_u64().unwrap() > 0);
    }

    /// A local declaration keeps saying so: `origin` must not turn every row
    /// cross-file the moment an index is attached.
    #[test]
    fn a_locally_declared_symbol_stays_declared_under_an_index() {
        let (v, _) = dump_with_index(CHILD, &[(CALC_BASE_PATH, CALC_BASE)]);
        let local = rows(&v, "symbols")
            .into_iter()
            .find(|s| s["name"] == "v-total")
            .expect("the local variable");
        assert_eq!(local["origin"], "declared");
        assert_eq!(local["data_type"], "integer");
        assert_eq!(local["data_type_source"], "declared");
        let sid = local["id"].as_u64().unwrap();
        let read = rows(&v, "references")
            .into_iter()
            .find(|r| r["symbol"].as_u64() == Some(sid))
            .expect("a reference to the local");
        assert_eq!(read["origin"], "declared");
    }

    /// AE3. The parent no file declares is the case the new section exists for:
    /// a supertype that *resolves* mints no symbol of its own, so without this
    /// row the resolved and absent cases look identical in the document.
    #[test]
    fn an_absent_parent_appears_in_the_dependency_section_with_its_span() {
        // Same child, empty workspace: the index looks on `/src` and finds nothing.
        let (v, _) = dump_with_index(CHILD, &[]);

        let unresolved = v["dependencies"]["unresolved"].as_array().unwrap();
        let parent = unresolved
            .iter()
            .find(|u| u["name"] == "orders.calc-base")
            .unwrap_or_else(|| panic!("absent parent must be reported, got {unresolved:?}"));
        assert_eq!(parent["via"], "class");
        assert_eq!(
            parent["reason"], "absent_from_workspace",
            "an index was attached and it searched, so a miss is a fact about the \
             workspace — as distinct from a file it located and could not read"
        );
        // The span points at the name inside the header — computed from the
        // fixture rather than hard-coded, so it stays true if the fixture moves.
        let start = CHILD.find("orders.calc-base").expect("name in header") as u64;
        assert_eq!(parent["span"]["start"], start);
        assert_eq!(
            parent["span"]["end"],
            start + "orders.calc-base".len() as u64
        );
        // Nothing was linked, so there is no dependency edge to report.
        assert!(v["dependencies"]["files"].as_array().unwrap().is_empty());
        // And the member never resolved: no cross-file symbol was synthesized.
        assert!(
            !rows(&v, "symbols")
                .iter()
                .any(|s| s["name"] == "calc-total"),
            "nothing is synthesized from a parent that does not exist"
        );
    }

    /// AE2. A run-time-computed `RUN` target is *unknowable*, not merely absent —
    /// the distinction R5 requires so a consumer can widen conservatively rather
    /// than reporting a missing program.
    #[test]
    fn a_dynamic_run_target_reports_the_unknowable_reason() {
        let source = "DEFINE VARIABLE v-target AS CHARACTER NO-UNDO.\nv-target = \"post-order.p\".\nRUN VALUE(v-target).\n";
        let (v, _) = dump_with_index(source, &[]);
        let refs = rows(&v, "references");
        let reasons: Vec<&str> = refs.iter().filter_map(|r| r["reason"].as_str()).collect();
        assert!(
            reasons.contains(&"unknowable"),
            "a computed RUN target must be unknowable, got {reasons:?}"
        );
    }

    /// A literal `RUN` target that resolves is a dependency edge too, under its
    /// own `via` — the section reports *which question* reached a file, because a
    /// program dependency and an inheritance dependency invalidate differently.
    #[test]
    fn a_resolved_literal_run_target_is_a_consulted_file() {
        let (v, _) = dump_with_index(
            "RUN post-order.p.\n",
            &[("/src/post-order.p", "MESSAGE \"posted\".\n")],
        );
        let files = v["dependencies"]["files"].as_array().unwrap();
        assert!(
            files
                .iter()
                .any(|f| f["via"] == "program" && f["name"] == "post-order.p"),
            "got {files:?}"
        );
    }

    /// The version map is one helper with two call sites precisely so these
    /// numbers cannot drift; this is the assertion that they are the eight the
    /// envelope documents.
    #[test]
    fn the_section_map_reports_eight_sections_with_the_bumped_versions() {
        let v = run_dump(vec![var_decl("x", DataType::Integer)]);
        let sections = v["sections"].as_object().expect("sections is an object");
        assert_eq!(sections.len(), 8, "got {sections:?}");
        assert_eq!(
            sections["symbols"], 4,
            "bumped again: a cross-file row's `data_type` is populated now"
        );
        assert_eq!(
            sections["references"], 2,
            "bumped for the resolved row's origin"
        );
        assert_eq!(sections["dependencies"], 2, "the new section");
        // The untouched five keep their numbers: a bump is a claim about a
        // section's shape, and claiming one falsely is as bad as missing one.
        for (name, version) in [
            ("scopes", 1),
            ("types", 1),
            ("diagnostics", 1),
            ("preproc", 1),
            ("coverage", 1),
        ] {
            assert_eq!(sections[name], version, "{name} must not have moved");
        }
    }

    /// Empty-but-present, not missing: a consumer indexes into this section
    /// unconditionally, and `index_revision: 0` is the fact that no index was
    /// attached — which is what makes an empty `unresolved` mean "nothing was
    /// looked at" rather than "everything resolved".
    #[test]
    fn a_file_with_no_cross_file_references_still_emits_the_dependency_section() {
        let v = run_dump(vec![var_decl("x", DataType::Integer)]);
        assert!(v["dependencies"].is_object());
        assert_eq!(v["dependencies"]["index_revision"], 0);
        assert!(v["dependencies"]["files"].as_array().unwrap().is_empty());
        assert!(
            v["dependencies"]["unresolved"]
                .as_array()
                .unwrap()
                .is_empty()
        );
    }

    /// Both text siblings render the section. `preproc` and `coverage` were
    /// JSON-only for as long as the CLI spliced them in, which made
    /// `--format text` strictly less informative about the same run; do not
    /// regress that.
    #[test]
    fn both_text_dumps_contain_the_dependency_section() {
        let (_, text) = dump_with_index(CHILD, &[(CALC_BASE_PATH, CALC_BASE)]);
        assert!(text.contains("=== Dependencies ==="), "got:\n{text}");
        assert!(text.contains("class orders.calc-base"), "got:\n{text}");
        // The inherited member's type must show here too, from the same helper
        // the JSON row uses.
        assert!(text.contains("integer(inherited)"), "got:\n{text}");

        let schema = Schema::empty();
        let stmts = vec![var_decl("x", DataType::Integer)];
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        let sem = analyze_file(&stmts, &ctx);
        let collected = CollectedDiagnostics::default();
        let text = dump_text_with_diagnostics(&sem, &collected);
        assert!(text.contains("=== Dependencies ==="), "got:\n{text}");
        assert!(text.contains("index revision: 0"), "got:\n{text}");
    }
}
