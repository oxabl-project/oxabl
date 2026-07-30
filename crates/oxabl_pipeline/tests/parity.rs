//! Leg 1 of 4: the composed `run` versus the two-phase `expand` + `collect`
//! (R19).
//!
//! This is the CLI-versus-LSP orchestration divergence in miniature. The
//! non-incremental clients call [`LintPipeline::run`]; the language server calls
//! [`LintPipeline::expand`] and [`LintPipeline::collect`] as separate memoized
//! salsa queries. Those are two code paths over one pipeline, and if they ever
//! stop agreeing then every other parity claim in this suite is void — the CLI
//! leg and the LSP leg would each be right about their own path and the product
//! would still be inconsistent.
//!
//! It is also the leg that establishes the *expected* column of the shared
//! table: the other three legs assert against the same
//! [`fixtures::FIXTURES`](oxabl_pipeline::fixtures::FIXTURES) rows this one
//! validates against the pipeline directly.
//!
//! All fixtures are synthetic ABL and live in the shared table, behind
//! `oxabl_pipeline`'s `test-support` feature.

use oxabl_pipeline::fixtures::{
    self, Capability, ExpectedFormat, FIXTURES, ObservedDiagnostic, ParityFixture,
};
use oxabl_pipeline::{
    FormatPipeline, LintPipeline, LintResult, NotFormatted, NotFormattedKind, PipelineConfig,
};
use oxabl_workspace::{FileSystem, InMemoryFileSystem};

/// A filesystem with nothing in it — the include fixture is *unresolvable* on
/// purpose, so no leg needs to plant a file for it.
fn fs() -> InMemoryFileSystem {
    InMemoryFileSystem::new()
}

fn observed(result: &LintResult) -> Vec<ObservedDiagnostic> {
    fixtures::observed(result.diagnostics())
}

fn run(fixture: &ParityFixture, config: &PipelineConfig, fs: &dyn FileSystem) -> LintResult {
    LintPipeline::new(config, fs).run(fixture.source)
}

// ---------------------------------------------------------------------------
// The table itself, through the composed run
// ---------------------------------------------------------------------------

/// Every row of the shared table produces exactly what it claims, code by code,
/// severity by severity, byte span by byte span. Establishes the baseline the
/// other three legs are compared against.
#[test]
fn the_shared_table_matches_the_composed_run() {
    let fs = fs();
    for fixture in FIXTURES {
        let config = fixture.config();
        let result = run(fixture, &config, &fs);
        assert!(
            !result.failed_run(),
            "fixture `{}` must not fail the run: {:?}",
            fixture.name,
            result.failure()
        );
        fixture.assert_diagnostics("pipeline run", observed(&result));
    }
}

/// The format column, through the same handle every client drives.
#[test]
fn the_shared_table_matches_the_format_pipeline() {
    for fixture in FIXTURES {
        let pipeline = FormatPipeline::new(fixture.config().style.clone());
        let outcome = pipeline.format(fixture.source);
        fixture.assert_format("pipeline format", &outcome);

        let (would_change, has_output, refused) = fixture.expected_format_facts();
        assert_eq!(outcome.changed(), would_change, "{}", fixture.name);
        assert_eq!(outcome.output().is_some(), has_output, "{}", fixture.name);
        assert_eq!(
            outcome.not_formatted().is_some(),
            refused,
            "{}",
            fixture.name
        );

        // The *kind* of refusal, not merely that one happened: a bail that
        // regressed into a contained panic is a defect the table must not
        // accept as parity.
        assert_eq!(
            outcome.not_formatted().map(NotFormatted::kind),
            fixture.expected_refusal_kind(),
            "{}",
            fixture.name
        );
    }
}

// ---------------------------------------------------------------------------
// The composed run versus the two phases
// ---------------------------------------------------------------------------

/// The property this leg exists for: `expand` + `collect` and `run` are the same
/// answer on every fixture, including the include one (where the expansion is
/// the interesting part) and the parse-error one (where recovery is).
#[test]
fn two_phase_and_composed_run_agree_on_every_fixture() {
    let fs = fs();
    for fixture in FIXTURES {
        let config = fixture.config();
        let pipeline = LintPipeline::new(&config, &fs);

        let expansion = pipeline.expand(fixture.source);
        let two_phase = pipeline.collect(&expansion);
        let composed = pipeline.run(fixture.source);

        assert_eq!(
            two_phase.diagnostics(),
            composed.diagnostics(),
            "fixture `{}`: the two-phase and composed runs disagree",
            fixture.name
        );
        assert_eq!(
            two_phase.dependency_paths(),
            composed.dependency_paths(),
            "fixture `{}`: dependency paths disagree",
            fixture.name
        );
        assert_eq!(
            two_phase.semantic().is_some(),
            composed.semantic().is_some(),
            "fixture `{}`: model presence disagrees",
            fixture.name
        );
        assert_eq!(
            two_phase.preprocessing_failed(),
            composed.preprocessing_failed(),
            "fixture `{}`: fatal-preprocessing arm disagrees",
            fixture.name
        );

        // And the two-phase path matches the *table*, not merely the composed
        // run — so a table drift cannot be hidden by both paths being wrong
        // together.
        fixture.assert_diagnostics("pipeline two-phase", observed(&two_phase));
    }
}

// ---------------------------------------------------------------------------
// Capability carve-outs, asserted as gaps
// ---------------------------------------------------------------------------

/// Without a schema, the schema-gated fixture is *silent* rather than
/// differently-diagnosed. This is the shape the browser leg asserts, pinned here
/// where the capability can be toggled directly.
#[test]
fn the_schema_gated_fixture_is_inert_without_a_schema() {
    let fs = fs();
    let fixture = FIXTURES
        .iter()
        .find(|f| f.needs_capability(Capability::Schema))
        .expect("the table carries a schema-gated fixture");

    let unloaded = fixtures::canonical_config();
    assert!(!unloaded.schema_loaded, "the canonical config loads no .df");
    let result = run(fixture, &unloaded, &fs);
    assert!(
        observed(&result).is_empty(),
        "fixture `{}` must be inert with no schema, got {:?}",
        fixture.name,
        observed(&result)
    );

    // With the capability supplied it is not inert — otherwise the assertion
    // above would pass for an entirely broken rule.
    assert!(!observed(&run(fixture, &fixture.config(), &fs)).is_empty());
}

/// Without include resolution — i.e. with the preprocessor off, which is exactly
/// the browser's configuration — the include fixture is silent rather than
/// differently-diagnosed.
#[test]
fn the_include_fixture_is_inert_without_include_resolution() {
    let fs = fs();
    let fixture = FIXTURES
        .iter()
        .find(|f| f.needs_capability(Capability::IncludeResolution))
        .expect("the table carries an include fixture");
    let config = fixture.config();

    let without = LintPipeline::new(&config, &fs)
        .with_preprocess(false)
        .run(fixture.source);
    assert!(
        fixtures::observed(without.diagnostics()).is_empty(),
        "fixture `{}` must be inert with preprocessing off, got {:?}",
        fixture.name,
        fixtures::observed(without.diagnostics())
    );
    assert!(without.dependency_paths().is_empty());

    // And loud with the capability present.
    fixture.assert_diagnostics(
        "pipeline run",
        observed(&LintPipeline::new(&config, &fs).run(fixture.source)),
    );
}

/// Every fixture that needs *no* capability answers identically with
/// preprocessing on and off.
///
/// This is what makes the browser leg's comparison legitimate at all: the
/// browser runs with the preprocessor off, so without this property its
/// agreement with the CLI would only hold by luck.
#[test]
fn capability_free_fixtures_are_indifferent_to_preprocessing() {
    let fs = fs();
    for fixture in FIXTURES.iter().filter(|f| f.needs.is_empty()) {
        let config = fixture.config();
        let on = LintPipeline::new(&config, &fs).run(fixture.source);
        let off = LintPipeline::new(&config, &fs)
            .with_preprocess(false)
            .run(fixture.source);
        assert_eq!(
            on.diagnostics(),
            off.diagnostics(),
            "fixture `{}` must not depend on preprocessing",
            fixture.name
        );
    }
}

// ---------------------------------------------------------------------------
// The per-rule severity override
// ---------------------------------------------------------------------------

/// The override changes the severity and nothing else — same code, same byte
/// span, same source. The CLI and LSP legs assert the identical transformation
/// through their own configuration surfaces.
#[test]
fn a_per_rule_severity_override_changes_only_the_severity() {
    let fs = fs();
    let fixture = fixtures::fixture(fixtures::OVERRIDE_FIXTURE);

    let baseline = observed(&run(fixture, &fixture.config(), &fs));
    let overridden = observed(&run(fixture, &fixtures::config_with_override(), &fs));

    assert_eq!(
        overridden.len(),
        baseline.len(),
        "the override must not change the finding count"
    );
    let target = overridden
        .iter()
        .find(|d| d.code == fixtures::OVERRIDE_CODE)
        .unwrap_or_else(|| panic!("expected {} after the override", fixtures::OVERRIDE_CODE));
    assert_eq!(target.severity, fixtures::OVERRIDE_SEVERITY);

    let before = baseline
        .iter()
        .find(|d| d.code == fixtures::OVERRIDE_CODE)
        .expect("the baseline carries the same code");
    assert_ne!(
        before.severity, target.severity,
        "the override must actually move the severity, or this asserts nothing"
    );
    assert_eq!((target.start, target.end), (before.start, before.end));
    assert_eq!(target.source, before.source);
}

// ---------------------------------------------------------------------------
// Table hygiene
// ---------------------------------------------------------------------------

/// The table covers what R19 asks it to cover. A fixture silently dropped from
/// the table would otherwise weaken all four legs at once with nothing failing.
#[test]
fn the_table_covers_every_rule_a_parse_error_a_clean_file_and_format_drift() {
    let codes: Vec<&str> = FIXTURES
        .iter()
        .flat_map(|f| f.diagnostics.iter().map(|d| d.code))
        .collect();
    for rule in [
        "LINT0001", "LINT0002", "LINT0003", "LINT0004", "LINT0005", "LINT0006",
    ] {
        assert!(codes.contains(&rule), "no fixture covers {rule}");
    }
    assert!(
        codes.contains(&"PARSE001"),
        "no fixture covers a parse error"
    );
    assert!(
        codes.contains(&"PREPROC007"),
        "no fixture covers the loud unresolvable-include warning"
    );

    assert!(
        FIXTURES
            .iter()
            .any(|f| f.diagnostics.is_empty() && matches!(f.format, ExpectedFormat::Unchanged)),
        "no fully clean fixture"
    );
    assert!(
        FIXTURES
            .iter()
            .any(|f| matches!(f.format, ExpectedFormat::Reformatted(_))),
        "no format-drift fixture"
    );
    assert!(
        FIXTURES
            .iter()
            .any(|f| f.expected_refusal_kind() == Some(NotFormattedKind::Bail)),
        "no format-refusal fixture the formatter declines on purpose"
    );

    let mut names: Vec<&str> = FIXTURES.iter().map(|f| f.name).collect();
    names.sort_unstable();
    let unique = names.len();
    names.dedup();
    assert_eq!(unique, names.len(), "fixture names must be unique");
}

/// The two ways a client can arrive at "no `oxabl.toml`" are the same value.
///
/// [`fixtures::canonical_config`] resolves — which is what the CLI and the
/// language server do — while the browser constructs [`PipelineConfig::default`]
/// in-process. The parity table is written against the first, so if the second
/// drifted from it, every browser-leg comparison would be asserting a different
/// question and the table would still look right. That drift is exactly the
/// defect the suite caught on its first run, and this is the assertion that keeps
/// the two derivations pinned to each other rather than only to prose.
#[test]
fn the_resolved_and_in_process_defaults_are_one_configuration() {
    let resolved = fixtures::canonical_config();
    let in_process = PipelineConfig::default();

    assert_eq!(
        resolved.lint_severities, in_process.lint_severities,
        "two default severity tables have been reintroduced"
    );
    assert_eq!(
        resolved.style.to_toml().unwrap(),
        in_process.style.to_toml().unwrap(),
        "the default style must not depend on how the config was built"
    );
    assert_eq!(resolved.include_paths, in_process.include_paths);
    assert_eq!(resolved.schema_loaded, in_process.schema_loaded);
    assert!(!in_process.schema_loaded);
}

/// The expected byte spans really do point at the substring they claim to, so a
/// span typo is caught in the table rather than surviving as four legs agreeing
/// on the wrong number.
#[test]
fn expected_spans_are_inside_their_source_and_land_on_real_text() {
    for fixture in FIXTURES {
        for expected in fixture.diagnostics {
            let (start, end) = (expected.start as usize, expected.end as usize);
            assert!(
                end <= fixture.source.len() && start < end,
                "fixture `{}`: {} has an impossible span {start}..{end}",
                fixture.name,
                expected.code
            );
            let text = &fixture.source[start..end];
            assert!(
                !text.trim().is_empty(),
                "fixture `{}`: {} spans only whitespace",
                fixture.name,
                expected.code
            );
        }
    }
}
