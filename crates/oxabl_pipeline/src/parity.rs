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
//! [`fixtures::FIXTURES`](crate::fixtures::FIXTURES) rows this one validates
//! against the pipeline directly.
//!
//! All fixtures are synthetic ABL and live in the shared table. This leg is the
//! one that reaches the table through `#[cfg(test)]` rather than through the
//! `test-support` feature: an integration test under `tests/` is a separate
//! crate, so enabling a feature *on this crate* for it would require a self
//! `[dev-dependencies]` entry — a `path = "."` edge that Cargo tolerates but
//! that release-please's `cargo-workspace` plugin reads as a dependency cycle
//! and refuses the whole release on. The three downstream legs still consume
//! the table the way any external crate must, through the feature.

use std::path::Path;

use oxabl_common::SourceMap;
use oxabl_workspace::{FileSystem, InMemoryFileSystem};

use crate::fixtures::{
    self, Capability, CrossFileEffect, ExpectedFormat, FIXTURES, ObservedDiagnostic, ParityFixture,
};
use crate::{
    FormatPipeline, LintPipeline, LintResult, NotFormatted, NotFormattedKind, PipelineConfig,
    position,
};

/// Where this leg roots a fixture's files, shared with the language-server and
/// browser legs so all three resolve over the same spellings.
fn root() -> &'static Path {
    Path::new(fixtures::PARITY_ROOT)
}

/// The filesystem a fixture runs over: its sibling files, rooted at
/// [`fixtures::PARITY_ROOT`]. Empty for a single-file fixture — the include row is
/// *unresolvable* on purpose, so no leg plants a file for it.
fn fs(fixture: &ParityFixture) -> InMemoryFileSystem {
    fixture.filesystem_under(root())
}

/// A filesystem with nothing in it, for the withheld-siblings half of the
/// cross-file capability pair.
fn empty_fs() -> InMemoryFileSystem {
    InMemoryFileSystem::new()
}

fn observed(result: &LintResult) -> Vec<ObservedDiagnostic> {
    fixtures::observed(result.diagnostics())
}

/// The composed run, through a per-file handle carrying the fixture's own
/// identity — which is how a walk and an editor both drive it, and what keeps a
/// file from resolving a name to its own copy on disk.
fn run(fixture: &ParityFixture, config: &PipelineConfig, fs: &dyn FileSystem) -> LintResult {
    let pipeline = LintPipeline::new(config, fs);
    pipeline
        .with_file(fixture.root_path(root()))
        .run(fixture.source)
}

// ---------------------------------------------------------------------------
// The table itself, through the composed run
// ---------------------------------------------------------------------------

/// Every row of the shared table produces exactly what it claims, code by code,
/// severity by severity, byte span by byte span. Establishes the baseline the
/// other three legs are compared against.
#[test]
fn the_shared_table_matches_the_composed_run() {
    for fixture in FIXTURES {
        let fs = fs(fixture);
        let config = fixture.config_under(root());
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
    for fixture in FIXTURES {
        let fs = fs(fixture);
        let config = fixture.config_under(root());
        let run = LintPipeline::new(&config, &fs);
        let pipeline = run.with_file(fixture.root_path(root()));

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
    let fixture = FIXTURES
        .iter()
        .find(|f| f.needs_capability(Capability::Schema))
        .expect("the table carries a schema-gated fixture");
    let fs = fs(fixture);

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
    let fixture = FIXTURES
        .iter()
        .find(|f| f.needs_capability(Capability::IncludeResolution))
        .expect("the table carries an include fixture");
    let fs = fs(fixture);
    let config = fixture.config_under(root());

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
    for fixture in FIXTURES.iter().filter(|f| f.needs.is_empty()) {
        let fs = fs(fixture);
        let config = fixture.config_under(root());
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
// Cross-file resolution (R7)
// ---------------------------------------------------------------------------

/// Sibling files are not a capability but an *input*: with them supplied the row
/// answers as the table says, and with them withheld it answers as
/// [`ParityFixture::expected_without_siblings`] says — louder, never quieter.
///
/// The second half is what stops a wholly broken resolver from passing the first:
/// a row whose siblings change the answer must actually produce a *different*
/// answer when they are gone. For a row where resolution is silent, unresolvable,
/// or unknowable the two answers are equal by construction, and this asserts that
/// equality rather than pretending a difference exists.
#[test]
fn withholding_a_fixtures_siblings_only_ever_makes_it_louder() {
    for fixture in FIXTURES.iter().filter(|f| f.is_cross_file()) {
        let config = fixture.config_under(root());

        let with = observed(&run(fixture, &config, &fs(fixture)));
        fixture.assert_diagnostics("pipeline run (siblings supplied)", with.clone());

        // Same configuration, same include path, nothing on the filesystem for it
        // to find — so this isolates the sibling files as the only variable.
        let without = observed(&run(fixture, &config, &empty_fs()));
        fixture.assert_diagnostics_without_siblings("pipeline run (siblings withheld)", without);

        let with = fixtures::normalize(with);
        let without = fixture.expected_without_siblings();
        if fixture.siblings_change_the_answer() {
            assert_ne!(
                with, without,
                "fixture `{}` claims a diagnostic-visible resolution, so the two \
                 answers must differ — otherwise the supplied half would pass for \
                 a resolver that never looked",
                fixture.name
            );
            for found in &with {
                assert!(
                    without.contains(found),
                    "fixture `{}`: supplying siblings added {} at {}..{} — attaching \
                     an index must only ever remove a finding (R11)",
                    fixture.name,
                    found.code,
                    found.start,
                    found.end
                );
            }
        } else {
            assert_eq!(
                with, without,
                "fixture `{}` claims no diagnostic-visible resolution, so the \
                 supplied and withheld answers must be identical",
                fixture.name
            );
        }
    }
}

/// The composed and two-phase entry points agree on every cross-file row, and
/// both are re-asserted against the table.
///
/// The whole-table test above already covers this for the supplied half; this one
/// exists for the **withheld** half, where the pipeline's index answers
/// `NotFound` rather than being absent, and for the model and dependency facts
/// the table does not carry.
#[test]
fn the_two_entry_points_agree_on_cross_file_rows_with_and_without_siblings() {
    for fixture in FIXTURES.iter().filter(|f| f.is_cross_file()) {
        let config = fixture.config_under(root());
        for (label, fs) in [
            ("siblings supplied", fixture.filesystem_under(root())),
            ("siblings withheld", empty_fs()),
        ] {
            let run = LintPipeline::new(&config, &fs);
            let pipeline = run.with_file(fixture.root_path(root()));

            let two_phase = pipeline.collect(&pipeline.expand(fixture.source));
            let composed = pipeline.run(fixture.source);

            assert_eq!(
                two_phase.diagnostics(),
                composed.diagnostics(),
                "fixture `{}` ({label}): the two-phase and composed runs disagree",
                fixture.name
            );
            assert_eq!(
                two_phase.semantic().is_some(),
                composed.semantic().is_some(),
                "fixture `{}` ({label}): model presence disagrees",
                fixture.name
            );
            assert_eq!(
                two_phase.dependency_paths(),
                composed.dependency_paths(),
                "fixture `{}` ({label}): dependency paths disagree",
                fixture.name
            );

            // Both paths against the table, not merely against each other.
            let observed = observed(&two_phase);
            if label == "siblings supplied" {
                fixture.assert_diagnostics("pipeline two-phase", observed);
            } else {
                fixture.assert_diagnostics_without_siblings("pipeline two-phase", observed);
            }
        }
    }
}

/// A cross-file row resolves the same way with no file identity at all — the
/// browser's position, where the buffer has no path.
///
/// Without this the browser leg's agreement with the other three would hold only
/// by luck, exactly as `capability_free_fixtures_are_indifferent_to_preprocessing`
/// makes its preprocessing-off comparison legitimate.
#[test]
fn cross_file_rows_do_not_depend_on_the_analysed_files_identity() {
    for fixture in FIXTURES.iter().filter(|f| f.is_cross_file()) {
        let fs = fs(fixture);
        let config = fixture.config_under(root());
        let anonymous = LintPipeline::new(&config, &fs);
        assert_eq!(anonymous.file(), None);
        fixture.assert_diagnostics(
            "pipeline run (no identity)",
            observed(&anonymous.run(fixture.source)),
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
    let fixture = fixtures::fixture(fixtures::OVERRIDE_FIXTURE);
    let fs = fs(fixture);

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

    assert!(
        FIXTURES.iter().any(|f| !f.source.is_ascii()),
        "no fixture exercises non-ASCII source, so byte-versus-character \
         confusion would be invisible to every leg"
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

// ---------------------------------------------------------------------------
// The non-ASCII case
// ---------------------------------------------------------------------------

/// The non-ASCII fixture's byte offsets and character offsets really do disagree,
/// and the shared position helper derives the *byte* column.
///
/// Two claims, both needed. The first is about the fixture: if the source were
/// quietly edited back to ASCII, or the constants drifted onto each other, the
/// row would still pass every other test in the suite while testing nothing —
/// which is the state the table was in before this fixture existed. The second is
/// about [`position`](crate::position), the one derivation the
/// byte-offset clients share: its convention is `SourceMap`'s 1-based byte
/// column, and each client asserts that same number through its own surface.
#[test]
fn the_non_ascii_fixture_distinguishes_bytes_from_characters() {
    let fixture = fixtures::fixture(fixtures::NON_ASCII_FIXTURE);
    let source = fixture.source;
    assert!(
        !source.is_ascii(),
        "the fixture that exercises encoding must not be ASCII"
    );
    assert_ne!(
        fixtures::NON_ASCII_BYTE_COLUMN,
        fixtures::NON_ASCII_CHARACTER_COLUMN + 1,
        "a byte column and a 0-based character column that differ only by the \
         indexing base would make every leg's assertion vacuous"
    );

    let expected = fixture.diagnostics[0];
    let map = SourceMap::new(source);
    let resolved = position::resolve_offsets(&map, expected.start, expected.end);
    assert_eq!(resolved.start.line, fixtures::NON_ASCII_LINE);
    assert_eq!(resolved.start.column, fixtures::NON_ASCII_BYTE_COLUMN);

    // And the span really does cover the identifier, not a neighbouring slice
    // shifted by the multi-byte characters ahead of it.
    assert_eq!(
        &source[expected.start as usize..expected.end as usize],
        "unusedTwo"
    );
}

/// The expected byte spans really do point at the substring they claim to, so a
/// span typo is caught in the table rather than surviving as four legs agreeing
/// on the wrong number.
///
/// Covers the cross-file rows' `Resolved` findings too: those spans are in the
/// same root buffer and are just as easy to mistype.
#[test]
fn expected_spans_are_inside_their_source_and_land_on_real_text() {
    for fixture in FIXTURES {
        let resolved = fixture.resolutions.iter().filter_map(|r| match r.effect {
            CrossFileEffect::Resolved(finding) => Some(finding),
            CrossFileEffect::ResolvedSilently
            | CrossFileEffect::Unresolvable
            | CrossFileEffect::Unknowable => None,
        });
        for expected in fixture.diagnostics.iter().copied().chain(resolved) {
            let (start, end) = (expected.start as usize, expected.end as usize);
            assert!(
                end <= fixture.source.len() && start < end,
                "fixture `{}`: {} has an impossible span {start}..{end}",
                fixture.name,
                expected.code
            );
            // Byte offsets, so a span may not land mid-character. On the
            // non-ASCII fixture this is the difference between a real offset and
            // one a character count produced; stated explicitly because the slice
            // below would otherwise report it as an unexplained panic.
            assert!(
                fixture.source.is_char_boundary(start) && fixture.source.is_char_boundary(end),
                "fixture `{}`: {}'s span {start}..{end} splits a character",
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

/// Cross-file hygiene: files and claims come in pairs, the claimed removals are
/// really removals, the table covers every effect arm, and every path is a
/// neutral relative one.
///
/// A row that planted files and claimed nothing about them would assert that four
/// clients agree without saying on what — the failure mode this whole unit exists
/// to prevent, one level up.
#[test]
fn every_fixture_carrying_siblings_declares_what_they_resolve() {
    for fixture in FIXTURES {
        assert_eq!(
            fixture.is_cross_file(),
            !fixture.resolutions.is_empty(),
            "fixture `{}`: sibling files and declared resolutions must come together",
            fixture.name
        );

        for resolution in fixture.resolutions {
            assert!(
                !resolution.name.is_empty(),
                "fixture `{}`: a resolution must name its reference",
                fixture.name
            );
            // A claimed removal that is also in the row's own expected set would
            // be no removal at all.
            if let CrossFileEffect::Resolved(finding) = resolution.effect {
                assert!(
                    !fixture.expected().iter().any(|d| d.code == finding.code
                        && d.start == finding.start
                        && d.end == finding.end),
                    "fixture `{}`: {} at {}..{} is claimed as removed by resolution \
                     but is also expected with the siblings supplied",
                    fixture.name,
                    finding.code,
                    finding.start,
                    finding.end
                );
            }
        }

        // Relative, and pointing inside the fixture's own root: an absolute path
        // would only be right for the three in-process legs, and a `..` would
        // reach outside the temp directory the CLI leg builds.
        for path in std::iter::once(fixture.root_file)
            .chain(fixture.siblings.iter().map(|sibling| sibling.path))
        {
            let path = Path::new(path);
            assert!(
                path.is_relative(),
                "fixture `{}`: `{}` must be relative to the fixture root",
                fixture.name,
                path.display()
            );
            assert!(
                path.components()
                    .all(|c| matches!(c, std::path::Component::Normal(_))),
                "fixture `{}`: `{}` must not escape the fixture root",
                fixture.name,
                path.display()
            );
        }
    }

    // Every effect arm is exercised, so the four legs' cross-file assertions
    // cover a removal, an agreed silence, an absence, and an unknowable.
    let effects: Vec<CrossFileEffect> = FIXTURES
        .iter()
        .flat_map(|f| f.resolutions.iter().map(|r| r.effect))
        .collect();
    assert!(
        effects
            .iter()
            .any(|e| matches!(e, CrossFileEffect::Resolved(_))),
        "no fixture covers a diagnostic-visible cross-file resolution"
    );
    for arm in [
        CrossFileEffect::ResolvedSilently,
        CrossFileEffect::Unresolvable,
        CrossFileEffect::Unknowable,
    ] {
        assert!(
            effects.contains(&arm),
            "no fixture covers the {arm:?} cross-file effect"
        );
    }
}
