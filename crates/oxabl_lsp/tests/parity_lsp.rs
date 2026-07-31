//! Leg 3 of 4: the language server (R19).
//!
//! Drives the salsa queries — `compute_diagnostics` over a [`Buffer`] whose text
//! comes from a [`Rope`], which is the server's own text representation and the
//! reason this leg cannot live in `oxabl_pipeline`. Comparison is against
//! `oxabl_pipeline::fixtures`, the same table the other three legs use.
//!
//! **Byte spans only.** The server's client-facing ranges are derived from the
//! rope under a negotiated position encoding, and that conversion has its own
//! tests. Comparing rendered positions here would let a UTF-16 column bug
//! masquerade as a pipeline divergence, so what is compared is the byte-spanned
//! [`CollectedDiagnostics`](oxabl_analyze::CollectedDiagnostics) the query
//! returns (KTD5).
//!
//! Every fixture is synthetic ABL from the shared table.

use std::path::Path;
use std::sync::Arc;

use lsp_types::PositionEncodingKind;
use oxabl_lsp::db::{AnalysisConfig, AnalysisDatabase, Buffer, SchemaHandle, compute_diagnostics};
use oxabl_lsp::document::Document;
use oxabl_lsp::formatting::compute_formatting_edits;
use oxabl_lsp::position::byte_to_position;
use oxabl_pipeline::fixtures::{self, ExpectedFormat, FIXTURES, ObservedDiagnostic, ParityFixture};
use oxabl_pipeline::{FormatPipeline, PipelineConfig};
use oxabl_workspace::InMemoryFileSystem;
use ropey::Rope;

/// Where this leg roots a fixture's files — the same directory the pipeline and
/// browser legs use, so all three resolve over one set of spellings.
fn root() -> &'static Path {
    Path::new(fixtures::PARITY_ROOT)
}

/// A database configured exactly as the live server is for a project with no
/// `oxabl.toml`: the resolved all-defaults configuration, preprocessing on, and
/// a filesystem for include reads and cross-file lookups.
///
/// The filesystem holds the fixture's sibling files and nothing else. For a
/// single-file fixture that means it is empty — the include row is
/// *unresolvable* on purpose, so the loud `PREPROC007` is the point rather than a
/// successful expansion.
///
/// Nothing is shared with the other legs but the fixture: this database builds its
/// own salsa-backed index, and that two different backends answer identically is
/// the property being asserted. (A `SnapshotIndex` borrows its database and is
/// deliberately not `Sync`, so it could not be shared even if that were wanted.)
fn db_with(config: PipelineConfig, fs: InMemoryFileSystem) -> AnalysisDatabase {
    AnalysisDatabase::new(AnalysisConfig {
        fs: Arc::new(fs),
        pipeline: Arc::new(config),
        preprocess: true,
        ..Default::default()
    })
}

/// The server's text pipeline: source → `Rope` → buffer input. Going through the
/// rope rather than handing the `&str` straight to the buffer is deliberate — it
/// is what the server does on `didOpen`, and a rope round-trip that lost bytes
/// would shift every span this leg compares.
///
/// The buffer carries the fixture's path, as a real `didOpen` does: it is what
/// excludes the analysed file from its own cross-file lookups.
fn buffer(db: &AnalysisDatabase, fixture: &ParityFixture) -> Buffer {
    let rope = Rope::from_str(fixture.source);
    assert_eq!(
        rope.to_string(),
        fixture.source,
        "the rope must round-trip the source"
    );
    Buffer::new(db, rope.to_string(), Some(fixture.root_path(root())))
}

fn observed(fixture: &ParityFixture, config: PipelineConfig) -> Vec<ObservedDiagnostic> {
    observed_over(fixture, config, fixture.filesystem_under(root()))
}

fn observed_over(
    fixture: &ParityFixture,
    config: PipelineConfig,
    fs: InMemoryFileSystem,
) -> Vec<ObservedDiagnostic> {
    let db = db_with(config, fs);
    let buffer = buffer(&db, fixture);
    let schema = SchemaHandle::new(&db, 0);
    let diagnostics = compute_diagnostics(&db, buffer, schema)
        .expect("an uncontended snapshot read is never cancelled");
    fixtures::observed(&diagnostics)
}

fn document(source: &str) -> Document {
    Document {
        rope: Rope::from_str(source),
        version: 1,
    }
}

// ---------------------------------------------------------------------------
// Diagnostics
// ---------------------------------------------------------------------------

/// Every fixture yields exactly the shared table's diagnostic set through the
/// salsa queries — codes, severities, byte spans, sources.
#[test]
fn every_fixture_matches_the_shared_table_through_the_queries() {
    for fixture in FIXTURES {
        fixture.assert_diagnostics(
            "lsp queries",
            observed(fixture, fixture.config_under(root())),
        );
    }
}

/// A clean buffer publishes nothing.
#[test]
fn the_clean_fixture_yields_no_diagnostics() {
    let fixture = fixtures::fixture("clean");
    assert!(
        observed(fixture, fixture.config_under(root())).is_empty(),
        "a clean buffer must produce nothing"
    );
}

/// The recovered set survives the query path: a parse error does not cost the
/// buffer its lint findings.
#[test]
fn a_parse_error_yields_the_same_recovered_set() {
    let fixture = fixtures::fixture("parse_error");
    let observed = observed(fixture, fixture.config_under(root()));
    fixture.assert_diagnostics("lsp queries", observed.clone());
    assert!(
        observed.iter().any(|d| d.code == "PARSE001")
            && observed.iter().any(|d| d.code.starts_with("LINT")),
        "recovery must yield both: {observed:?}"
    );
}

/// The loud unresolvable-include warning is published like any other diagnostic
/// — the server has the include capability, so the gap the browser asserts does
/// not apply here.
#[test]
fn the_loud_include_warning_is_published() {
    let fixture = fixtures::fixture("unresolvable_include");
    let observed = observed(fixture, fixture.config_under(root()));
    fixture.assert_diagnostics("lsp queries", observed.clone());
    assert!(observed.iter().any(|d| d.code == "PREPROC007"));
}

/// Schema is a capability here too: supplied, the rule fires; withheld, the
/// buffer is silent rather than differently diagnosed.
#[test]
fn the_schema_gated_fixture_needs_a_loaded_schema() {
    let fixture = fixtures::fixture("unknown_field");
    fixture.assert_diagnostics(
        "lsp queries",
        observed(fixture, fixture.config_under(root())),
    );
    assert!(
        observed(fixture, fixtures::canonical_config()).is_empty(),
        "with no schema loaded the rule must be inert"
    );
}

/// Recomputing an unchanged buffer returns the same set, so nothing in this leg
/// depends on a cold query graph.
#[test]
fn a_memoized_recompute_returns_the_same_set() {
    for fixture in FIXTURES {
        let db = db_with(
            fixture.config_under(root()),
            fixture.filesystem_under(root()),
        );
        let buffer = buffer(&db, fixture);
        let schema = SchemaHandle::new(&db, 0);
        let first = compute_diagnostics(&db, buffer, schema).unwrap();
        let second = compute_diagnostics(&db, buffer, schema).unwrap();
        assert_eq!(first, second, "fixture `{}`", fixture.name);
        fixture.assert_diagnostics("lsp queries (memoized)", fixtures::observed(&second));
    }
}

/// The non-ASCII fixture's **byte** span comes back from the queries unchanged,
/// and the server's own conversion turns it into a *different* number under
/// UTF-16 — which is the whole reason this leg compares bytes.
///
/// This is the fixture that makes the module doc's caution load-bearing. Every
/// other row is pure ASCII, where a byte column and a UTF-16 column are the same
/// integer, so a client that used one where it meant the other would pass. Here
/// the two disagree, and the assertion is both that the pipeline's byte span is
/// preserved through the rope and that the client-facing position is the
/// encoding-aware one rather than the byte column the CLI prints.
#[test]
fn the_non_ascii_fixture_keeps_bytes_and_renders_utf16_columns() {
    let fixture = fixtures::fixture(fixtures::NON_ASCII_FIXTURE);
    fixture.assert_diagnostics("lsp queries", observed(fixture, fixture.config()));

    let rope = Rope::from_str(fixture.source);
    let start = fixture.diagnostics[0].start as usize;

    let utf16 = byte_to_position(&rope, start, &PositionEncodingKind::UTF16);
    assert_eq!(utf16.line as usize, fixtures::NON_ASCII_LINE - 1);
    assert_eq!(
        utf16.character as usize,
        fixtures::NON_ASCII_CHARACTER_COLUMN,
        "the server must send the negotiated encoding's column, not the byte one"
    );

    // The byte column is what UTF-8 negotiation asks for, and it is a different
    // number — so the two conversions cannot be confused for each other here.
    let utf8 = byte_to_position(&rope, start, &PositionEncodingKind::UTF8);
    assert_eq!(
        utf8.character as usize,
        fixtures::NON_ASCII_BYTE_COLUMN - 1,
        "UTF-8 negotiation asks for the byte column, 0-based"
    );
    assert_ne!(utf8.character, utf16.character);
}

// ---------------------------------------------------------------------------
// Cross-file resolution (R7)
// ---------------------------------------------------------------------------

/// Every cross-file row answers through the salsa queries exactly as the table
/// says, with the siblings supplied and with them withheld.
///
/// This is the leg with a *different index backend*: the server answers from
/// per-file salsa inputs keyed and invalidated individually, where the other three
/// answer from the batch memo. Identical answers from two memoization strategies
/// over the same supplied files is the property R7 asks for — the clients differ
/// in what they cache, not in what they resolve.
#[test]
fn cross_file_fixtures_resolve_through_the_salsa_queries() {
    for fixture in FIXTURES.iter().filter(|f| f.is_cross_file()) {
        let supplied = observed(fixture, fixture.config_under(root()));
        fixture.assert_diagnostics("lsp queries (siblings supplied)", supplied.clone());

        // Same include path, empty filesystem: the files are the only variable.
        let withheld = observed_over(
            fixture,
            fixture.config_under(root()),
            InMemoryFileSystem::new(),
        );
        fixture.assert_diagnostics_without_siblings("lsp queries (siblings withheld)", withheld);

        if fixture.siblings_change_the_answer() {
            assert_ne!(
                fixtures::normalize(supplied),
                fixture.expected_without_siblings(),
                "fixture `{}`: the two halves must differ, or the supplied half \
                 would pass for a server that never looked",
                fixture.name
            );
        }
    }
}

// ---------------------------------------------------------------------------
// Format
// ---------------------------------------------------------------------------

/// The server's formatting decision agrees with the table on all three arms: one
/// whole-document edit for drift, and an empty edit list for both leave-it-alone
/// arms.
#[test]
fn formatting_edits_agree_with_the_shared_table() {
    let encoding = PositionEncodingKind::UTF8;
    for fixture in FIXTURES {
        let pipeline = FormatPipeline::new(fixture.config().style.clone());
        fixture.assert_format("lsp formatting", &pipeline.format(fixture.source));

        let edits = compute_formatting_edits(&document(fixture.source), &pipeline, &encoding);
        match fixture.format {
            ExpectedFormat::Reformatted(expected) => {
                assert_eq!(
                    edits.len(),
                    1,
                    "fixture `{}`: drift is one whole-document edit",
                    fixture.name
                );
                assert_eq!(edits[0].new_text, expected, "fixture `{}`", fixture.name);
            }
            // A refusal and an already-conforming buffer are both "send no
            // edits" — the editor leaves the buffer as the user typed it.
            ExpectedFormat::Unchanged | ExpectedFormat::Refused(_) => {
                assert!(
                    edits.is_empty(),
                    "fixture `{}`: expected no edits, got {edits:?}",
                    fixture.name
                );
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Per-rule severity
// ---------------------------------------------------------------------------

/// The override moves the severity and nothing else, identically to the pipeline
/// and CLI legs.
#[test]
fn a_per_rule_severity_override_changes_only_the_severity() {
    let fixture = fixtures::fixture(fixtures::OVERRIDE_FIXTURE);
    let baseline = observed(fixture, fixture.config_under(root()));
    let overridden = observed(fixture, fixtures::config_with_override());

    let target = overridden
        .iter()
        .find(|d| d.code == fixtures::OVERRIDE_CODE)
        .unwrap_or_else(|| panic!("expected {} after the override", fixtures::OVERRIDE_CODE));
    let before = baseline
        .iter()
        .find(|d| d.code == fixtures::OVERRIDE_CODE)
        .expect("the baseline carries the same code");

    assert_eq!(target.severity, fixtures::OVERRIDE_SEVERITY);
    assert_ne!(before.severity, target.severity);
    assert_eq!((target.start, target.end), (before.start, before.end));
    assert_eq!(target.source, before.source);
    assert_eq!(overridden.len(), baseline.len());
}
