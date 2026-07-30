//! Browser bindings for Oxabl's shared public pipelines.
//!
//! This crate deliberately contains no ABL behavior. It drives
//! [`LintPipeline`] and [`FormatPipeline`] — the same two handles the CLI and the
//! language server drive — and translates the
//! [`LintResult`](oxabl_pipeline::LintResult) and [`FormatOutcome`] they return
//! into a small JSON wire shape suitable for a
//! browser (R11). The CLI, LSP, VS Code extension, and browser therefore share
//! the same lexer, parser, semantic analysis, lint rules, formatter, safe default
//! style, *and* run orchestration.
//!
//! Line/column derivation is not done here either: it comes from
//! [`oxabl_pipeline::position`], the one derivation the CLI's text rendering also
//! uses, so the two cannot drift (R13).
//!
//! # Reduced capability, not divergent behavior
//!
//! The browser MVP has no project filesystem, include path, or schema upload, so
//! the pipeline is configured with [`PipelineConfig::default`] — no include
//! paths, no schema — and preprocessing off. Those capabilities are *absent*
//! rather than emulated: a second, divergent implementation in this layer is
//! exactly what R11 forbids.
//!
//! # `oxabl_pipeline` directly, not through the umbrella
//!
//! The pipeline handles are taken from `oxabl_pipeline` rather than
//! `oxabl::pipeline`, the same way `oxabl_lsp` takes them. The umbrella's
//! re-export does name these handles, so either edge would work; this crate
//! already compiles `oxabl_pipeline` regardless (the umbrella depends on it
//! unconditionally, precisely so the browser bundle can reach it), so the direct
//! edge adds nothing to the payload and one less hop of indirection to read. The
//! remaining `oxabl::` imports are plain type re-exports.

use oxabl::analyze::{CollectedDiagnostic, DiagnosticSource};
use oxabl::common::SourceMap;
use oxabl::workspace::InMemoryFileSystem;
use oxabl_pipeline::{FormatOutcome, FormatPipeline, LintPipeline, PipelineConfig, position};
use serde::Serialize;
use wasm_bindgen::prelude::*;

/// Inline JS that stashes a panic message where JS can read it after the trap.
///
/// **The cross-repo contract is the key `globalThis.__oxablPanicMessage`.** A
/// `globalThis` key rather than an imported website function on purpose: a
/// `--target web` build bakes its import specifiers in at bindgen time, so
/// importing a path from the website would hardcode that site's directory layout
/// into this crate and break the artifact for every other consumer. The stash
/// keeps the artifact self-contained. The consumer clears the key before each
/// call and reads it in its `catch`.
///
/// The channel has to be a call *out* of wasm from inside the panic hook, not a
/// return value: on `panic=abort` the panic aborts to an `unreachable` trap, so
/// the exported function never returns. std runs the registered hook to
/// completion *before* the panic runtime aborts, and an ordinary synchronous
/// wasm→JS call inside the hook returns before that trap by program order on
/// every engine — so the message is stashed by the time the `RuntimeError`
/// surfaces.
///
/// Reading the message back through a second export call would be the wrong
/// shape regardless: it would re-enter an instance already deemed untrustworthy,
/// and the state reset discards the statics anyway.
#[wasm_bindgen(inline_js = "
export function __oxabl_stash_panic(message) {
  try {
    globalThis.__oxablPanicMessage = message;
  } catch (_) {
    // A frozen or exotic global is not worth trapping over inside a panic hook:
    // the website falls back to a fixed no-message diagnostic.
  }
}
")]
extern "C" {
    #[wasm_bindgen(js_name = "__oxabl_stash_panic")]
    fn stash_panic(message: &str);
}

/// Install the panic hook. Runs on instantiation, and — crucially — **re-runs on
/// every recovery**: `__wbg_reset_state` calls `__wbindgen_start()`
/// unconditionally as its last step, after creating the fresh instance and
/// rebinding it. The hook is a static, so it dies with the old instance and this
/// re-run re-arms it, which is why no `reinstall()` export is needed. (Upstream
/// removed `set_on_reinit` in 0.2.118 for exactly that reason.)
#[wasm_bindgen(start)]
pub fn start() {
    std::panic::set_hook(Box::new(|info| {
        stash_panic(&format!("{info}"));
    }));
}

/// An identifier for **this artifact**, not just the crate.
///
/// Returns `<crate version>+<build id>`. The build id is a short git SHA baked
/// in by `build.rs`; see there for why the crate version alone identifies
/// nothing. The website shows this in a crash report so a stale hand-vendored
/// copy of `src/wasm/` is distinguishable from a current one.
#[wasm_bindgen]
pub fn version() -> String {
    format!("{}+{}", env!("CARGO_PKG_VERSION"), env!("OXABL_WASM_BUILD"))
}

/// Panic on purpose, to verify the browser's capture-and-recover path.
///
/// No ABL input reaches a parser panic — all five `unreachable!()` sites are
/// caller-guarded — so without this export every manual browser check of the
/// crash path would be unexecutable. It is behind the `debug-panic` feature,
/// which `scripts/build-wasm.sh` enables only for a local verification build
/// (`--verify`), never for the release artifact.
#[cfg(feature = "debug-panic")]
#[wasm_bindgen]
pub fn debug_panic() {
    panic!("deliberate panic from debug_panic(), for verifying browser recovery");
}

#[derive(Serialize)]
struct AnalyzeResponse {
    diagnostics: Vec<WireDiagnostic>,
}

#[derive(Serialize)]
struct WireDiagnostic {
    source: &'static str,
    severity: &'static str,
    code: &'static str,
    message: String,
    start: WirePosition,
    end: WirePosition,
    help: Option<String>,
}

#[derive(Serialize)]
struct WirePosition {
    byte: u32,
    line: usize,
    column: usize,
}

#[derive(Serialize)]
struct FormatResponse {
    source: String,
    changed: bool,
    error: Option<String>,
}

/// Map one collected diagnostic onto the wire, deriving both endpoints through
/// the shared position helper (R13) rather than reaching for
/// [`SourceMap::lookup`] here.
///
/// The helper's convention is `SourceMap`'s own — 1-based line, 1-based **byte**
/// column — so this is the same number the hand-rolled pair produced, and the
/// same number the CLI's text output prints.
fn diagnostic_to_wire(item: CollectedDiagnostic, source_map: &SourceMap) -> WireDiagnostic {
    let diagnostic = item.diagnostic;
    let resolved = position::resolve_diagnostic(source_map, &diagnostic);

    WireDiagnostic {
        source: diagnostic_source(item.source),
        severity: diagnostic.severity.as_str(),
        code: diagnostic.code.0,
        message: diagnostic.message,
        start: WirePosition::from(resolved.start),
        end: WirePosition::from(resolved.end),
        help: diagnostic.help,
    }
}

impl From<position::Position> for WirePosition {
    fn from(position: position::Position) -> Self {
        WirePosition {
            byte: position.byte,
            line: position.line,
            column: position.column,
        }
    }
}

fn diagnostic_source(source: DiagnosticSource) -> &'static str {
    source.as_str()
}

/// Analyze one in-memory ABL file through the shared [`LintPipeline`] the CLI and
/// the language server also drive.
///
/// Configuration is [`PipelineConfig::default`] over an empty in-memory file
/// system, with preprocessing off: the browser MVP has no project filesystem,
/// include path, or schema upload, so those capabilities are absent rather than
/// emulated here.
#[wasm_bindgen]
pub fn analyze_source(source: &str) -> String {
    let config = PipelineConfig::default();
    let fs = InMemoryFileSystem::new();
    // `run` is the guarded convenience, though its guard is a documented
    // pass-through on wasm32 — under `panic=abort` a panic traps instead of
    // being contained, so the browser's protection is the panic hook plus
    // instance reinitialization, not a failed-run result. A failed run is
    // nonetheless reachable natively, where this crate's unit tests live, and it
    // reports as an empty diagnostic set: the wire shape deliberately gains no
    // `error` field for something the browser can never observe.
    let result = LintPipeline::new(&config, &fs)
        .with_preprocess(false)
        .run(source);

    let source_map = SourceMap::new(source);
    let diagnostics = result
        .into_diagnostics()
        .diagnostics
        .into_iter()
        .map(|diagnostic| diagnostic_to_wire(diagnostic, &source_map))
        .collect();

    serde_json::to_string(&AnalyzeResponse { diagnostics })
        .expect("the browser diagnostic wire shape is always serializable")
}

/// Format one ABL file through the shared [`FormatPipeline`], using the same safe
/// default style as the language server when no `oxabl.toml` is present.
///
/// The style comes out of [`PipelineConfig::default`], the same value
/// [`analyze_source`] configures itself from, rather than being fetched from
/// `StyleGuide` directly. Reaching for the style guide's own default here would
/// agree only *coincidentally*: two entry points of one client would be deriving
/// their configuration from two places, and a change to what a default
/// `PipelineConfig` means would move analysis and leave formatting behind. One
/// derivation, so there is nothing to drift.
///
/// The wire shape has a single `error` field, so a refusal's bail-versus-panic
/// distinction — which [`FormatOutcome`] keeps structural — collapses to its
/// reason text *here*, at the transport boundary, not in the pipeline. On either
/// leave-it-alone arm the original bytes come back untouched with
/// `changed: false`; no arm ever returns partially formatted source.
#[wasm_bindgen]
pub fn format_source(source: &str) -> String {
    let pipeline = FormatPipeline::new(PipelineConfig::default().style);
    let result = match pipeline.format(source) {
        FormatOutcome::Reformatted(formatted) => FormatResponse {
            source: formatted,
            changed: true,
            error: None,
        },
        FormatOutcome::Unchanged => FormatResponse {
            source: source.to_string(),
            changed: false,
            error: None,
        },
        FormatOutcome::DidNotFormat(not_formatted) => FormatResponse {
            source: source.to_string(),
            changed: false,
            error: Some(not_formatted.reason()),
        },
    };

    serde_json::to_string(&result).expect("the browser format wire shape is always serializable")
}

#[cfg(test)]
mod tests {
    use oxabl::common::{Diagnostic, FileSpan, Span};

    use super::*;

    #[test]
    fn version_names_the_artifact_not_just_the_crate() {
        let v = version();
        assert!(!v.is_empty());
        let (crate_version, build) = v
            .split_once('+')
            .expect("version is `<crate version>+<build id>`");
        assert_eq!(crate_version, env!("CARGO_PKG_VERSION"));
        assert!(
            !build.is_empty(),
            "a crash report needs a build identifier: {v}"
        );
        // The crate version alone never moves (this crate is absent from
        // release-please), so it cannot be the whole identifier.
        assert_ne!(v, crate_version);
    }

    /// The panic vehicle must not ship in the release artifact. A build without
    /// the feature has no `debug_panic`, which this asserts by construction: the
    /// call below only compiles when the feature is on.
    #[test]
    fn debug_panic_exists_only_under_its_feature() {
        #[cfg(feature = "debug-panic")]
        {
            let previous = std::panic::take_hook();
            std::panic::set_hook(Box::new(|_| {}));
            let caught = std::panic::catch_unwind(debug_panic);
            std::panic::set_hook(previous);
            assert!(caught.is_err(), "the vehicle must actually panic");
        }
        #[cfg(not(feature = "debug-panic"))]
        {
            // Nothing to call — the export does not exist in a default build,
            // which is the property under test. Kept as an explicit arm so the
            // test is visibly meaningful in both configurations.
        }
    }

    #[test]
    fn analysis_uses_the_shared_lint_pipeline() {
        let response: serde_json::Value = serde_json::from_str(&analyze_source(
            "DEFINE VARIABLE unused AS INTEGER NO-UNDO.",
        ))
        .unwrap();
        let diagnostics = response["diagnostics"].as_array().unwrap();

        assert!(diagnostics.iter().any(|diagnostic| {
            diagnostic["code"] == "LINT0002"
                && diagnostic["source"] == "lint"
                && diagnostic["start"]["line"] == 1
        }));
    }

    #[test]
    fn formatting_uses_the_safe_shared_default() {
        let source = "IF TRUE THEN\nMESSAGE \"hello\".";
        let response: serde_json::Value = serde_json::from_str(&format_source(source)).unwrap();

        assert_eq!(response["error"], serde_json::Value::Null);
        assert_eq!(response["changed"], true);
        assert_eq!(response["source"], "IF TRUE THEN\n    MESSAGE \"hello\".\n");
    }

    #[test]
    fn formatting_bail_keeps_the_original_source() {
        let source = "IF THEN.";
        let response: serde_json::Value = serde_json::from_str(&format_source(source)).unwrap();

        assert_eq!(response["source"], source);
        assert_eq!(response["changed"], false);
        assert!(response["error"].is_string());
    }

    // Already-formatted source takes the pipeline's `Unchanged` arm, which is a
    // separate arm from `Reformatted` — the browser must still get the bytes
    // back, since the wire shape always carries a source.
    #[test]
    fn already_formatted_source_round_trips_unchanged() {
        let source = "IF TRUE THEN\n    MESSAGE \"hello\".\n";
        let response: serde_json::Value = serde_json::from_str(&format_source(source)).unwrap();

        assert_eq!(response["error"], serde_json::Value::Null);
        assert_eq!(response["changed"], false);
        assert_eq!(response["source"], source);
    }

    #[test]
    fn a_clean_source_reports_no_diagnostics() {
        let response: serde_json::Value =
            serde_json::from_str(&analyze_source("MESSAGE \"hello\".\n")).unwrap();

        assert_eq!(
            response["diagnostics"].as_array().map(Vec::len),
            Some(0),
            "got {response}"
        );
    }

    // R13: the wire positions are the shared helper's answer, byte column and
    // all, so the browser and the CLI's text output cannot disagree. Pinned
    // against the helper directly rather than against a hand-computed number.
    #[test]
    fn wire_positions_come_from_the_shared_helper() {
        let source = "/* é */ DEFINE VARIABLE unused AS INTEGER NO-UNDO.\n";
        let response: serde_json::Value = serde_json::from_str(&analyze_source(source)).unwrap();
        let diagnostic = response["diagnostics"]
            .as_array()
            .and_then(|all| all.iter().find(|d| d["code"] == "LINT0002"))
            .unwrap_or_else(|| panic!("expected an unused-variable diagnostic, got {response}"));

        let map = SourceMap::new(source);
        let start = source.find("unused").expect("fixture names the variable") as u32;
        let expected = position::resolve_offset(&map, start);

        assert_eq!(diagnostic["start"]["byte"], expected.byte);
        assert_eq!(diagnostic["start"]["line"], expected.line);
        assert_eq!(diagnostic["start"]["column"], expected.column);
        // A two-byte character precedes the span, so a byte column and a
        // character column differ here — which is what makes this assertion
        // load-bearing rather than tautological.
        assert_eq!(expected.column, start as usize + 1);
    }

    // `help` survives the wire mapping. `LintResult` keeps `labels` and `help`
    // intact, so dropping `help` here would be this layer's bug — and this layer
    // is where it would happen, since the wire struct enumerates its fields.
    //
    // Driven through a synthetic diagnostic rather than real ABL: with
    // preprocessing off (the browser's configuration) the only diagnostic in the
    // workspace that carries `help` is the preprocessor's unresolvable-include
    // warning, which is unreachable from here by construction. Asserting on the
    // mapping directly beats asserting nothing.
    #[test]
    fn diagnostic_help_reaches_the_wire() {
        let source = "DEFINE VARIABLE unused AS INTEGER NO-UNDO.\n";
        let map = SourceMap::new(source);
        let start = source.find("unused").expect("fixture names the variable") as u32;
        let collected = CollectedDiagnostic {
            diagnostic: Diagnostic::warning(
                "LINT9999",
                "synthetic".to_string(),
                FileSpan {
                    file: oxabl_pipeline::ROOT_FILE_ID,
                    span: Span {
                        start,
                        end: start + 6,
                    },
                },
            )
            .with_help("do something about it".to_string()),
            source: DiagnosticSource::Lint,
        };

        let wire = diagnostic_to_wire(collected, &map);
        assert_eq!(wire.help.as_deref(), Some("do something about it"));
        assert_eq!(wire.source, "lint");
        assert_eq!(wire.severity, "warning");
        assert_eq!(wire.start.byte, start);
        assert_eq!(wire.start.line, 1);

        // And it survives serialization, since that is what the browser reads.
        let json = serde_json::to_value(&wire).expect("serializable");
        assert_eq!(json["help"], "do something about it");
    }

    // -----------------------------------------------------------------------
    // Leg 4 of 4: the browser, against the shared parity table (R19)
    // -----------------------------------------------------------------------
    //
    // The `#[wasm_bindgen]` exports are reachable only from inside this crate
    // without a dev-dependency back-edge and host-target shims, which is why
    // this leg lives here rather than in `oxabl_pipeline`. It compares the same
    // `oxabl_pipeline::fixtures` table the pipeline, CLI, and LSP legs use.
    //
    // Byte spans are what is compared: the wire shape carries `start.byte` /
    // `end.byte` beside the derived line/column, and only the bytes are the
    // pipeline's own answer (KTD5).
    //
    // Where the browser is deliberately *less capable* — no schema upload, no
    // include resolution, no `oxabl.toml` — the tests assert the **capability
    // gap** rather than a different diagnostic set. One genuine *defect* is also
    // pinned by name; see `browser_default_severities_diverge_for_two_rules`.
    mod parity {
        use oxabl_pipeline::fixtures::{
            self, Capability, ExpectedFormat, FIXTURES, ObservedDiagnostic, ParityFixture,
        };

        use oxabl_pipeline::{FormatOutcome, FormatPipeline, PipelineConfig};

        use super::super::{analyze_source, format_source};

        /// Every diagnostic `analyze_source` reported, in the shared comparison
        /// form.
        fn observed(source: &str) -> Vec<ObservedDiagnostic> {
            let response: serde_json::Value = serde_json::from_str(&analyze_source(source))
                .expect("the browser wire shape is JSON");
            response["diagnostics"]
                .as_array()
                .expect("diagnostics array")
                .iter()
                .map(|d| {
                    ObservedDiagnostic::from_wire(
                        d["code"].as_str().unwrap(),
                        d["severity"].as_str().unwrap(),
                        d["source"].as_str().unwrap(),
                        d["start"]["byte"].as_u64().unwrap() as u32,
                        d["end"]["byte"].as_u64().unwrap() as u32,
                    )
                })
                .collect()
        }

        fn formatted(source: &str) -> serde_json::Value {
            serde_json::from_str(&format_source(source)).expect("the browser wire shape is JSON")
        }

        /// Fixtures the browser has the capabilities to answer at all.
        fn comparable() -> impl Iterator<Item = &'static ParityFixture> {
            FIXTURES.iter().filter(|f| f.browser_comparable())
        }

        /// Every capability-free fixture yields the shared table's codes, byte
        /// spans, and sources through the `#[wasm_bindgen]` entry point.
        ///
        /// Severity is compared through
        /// [`fixtures::browser_expected`](oxabl_pipeline::fixtures::browser_expected),
        /// which applies the one recorded divergence rather than pretending it
        /// away.
        #[test]
        fn every_browser_capable_fixture_matches_the_shared_table() {
            for fixture in comparable() {
                let observed = fixtures::normalize(observed(fixture.source));
                assert_eq!(
                    observed,
                    fixtures::browser_expected(fixture),
                    "the browser diverged on fixture `{}`",
                    fixture.name
                );
            }
        }

        /// A clean source is clean in the browser too.
        #[test]
        fn the_clean_fixture_yields_no_diagnostics() {
            assert!(observed(fixtures::fixture("clean").source).is_empty());
        }

        /// The recovered set survives: a parse error does not cost the browser
        /// its lint findings either.
        #[test]
        fn a_parse_error_yields_the_same_recovered_set() {
            let fixture = fixtures::fixture("parse_error");
            let observed = fixtures::normalize(observed(fixture.source));
            assert_eq!(observed, fixtures::browser_expected(fixture));
            assert!(
                observed.iter().any(|d| d.code == "PARSE001")
                    && observed.iter().any(|d| d.code.starts_with("LINT")),
                "recovery must yield both: {observed:?}"
            );
        }

        /// Format outcomes agree across the reformat, unchanged, and refusal
        /// arms — the wire shape's three-field rendering of the one
        /// [`FormatOutcome`](oxabl_pipeline::FormatOutcome).
        #[test]
        fn format_outcomes_agree_with_the_shared_table() {
            for fixture in FIXTURES {
                // Formatting takes raw source and a style guide only, so *every*
                // fixture is comparable here — the capability gaps are inputs to
                // the lint pipeline, not the format one.
                let response = formatted(fixture.source);
                match fixture.format {
                    ExpectedFormat::Unchanged => {
                        assert_eq!(response["changed"], false, "{}", fixture.name);
                        assert_eq!(
                            response["error"],
                            serde_json::Value::Null,
                            "{}",
                            fixture.name
                        );
                        assert_eq!(response["source"], fixture.source, "{}", fixture.name);
                    }
                    ExpectedFormat::Reformatted(expected) => {
                        assert_eq!(response["changed"], true, "{}", fixture.name);
                        assert_eq!(
                            response["error"],
                            serde_json::Value::Null,
                            "{}",
                            fixture.name
                        );
                        assert_eq!(response["source"], expected, "{}", fixture.name);
                    }
                    // The browser wire shape collapses the refusal to its reason
                    // text — a genuine capability gap, not a divergence: there
                    // is no discriminant field to compare. What the table's kind
                    // buys here is that the *reason* is the one the shared
                    // pipeline produced for that kind, checked below against an
                    // independent derivation.
                    ExpectedFormat::Refused(_) => {
                        assert_eq!(response["changed"], false, "{}", fixture.name);
                        assert!(
                            response["error"].is_string(),
                            "a refusal states a reason: {}",
                            fixture.name
                        );
                        // No arm ever returns partially formatted bytes.
                        assert_eq!(response["source"], fixture.source, "{}", fixture.name);
                    }
                }
            }
        }

        /// The non-ASCII fixture's byte span reaches the wire, and the
        /// line/column beside it is the **byte** column the shared helper derives.
        ///
        /// Only the bytes enter the full-table comparison, so the wire's derived
        /// pair — the number the playground actually shows a user — is only
        /// covered by a test like this one, on a source where counting characters
        /// instead of bytes gives a different answer.
        #[test]
        fn the_non_ascii_fixture_reaches_the_wire_with_byte_positions() {
            let fixture = fixtures::fixture(fixtures::NON_ASCII_FIXTURE);
            assert!(fixture.browser_comparable());
            let response: serde_json::Value =
                serde_json::from_str(&analyze_source(fixture.source)).unwrap();
            let rendered = &response["diagnostics"][0];

            assert_eq!(rendered["start"]["byte"], fixture.diagnostics[0].start);
            assert_eq!(rendered["end"]["byte"], fixture.diagnostics[0].end);
            assert_eq!(rendered["start"]["line"], fixtures::NON_ASCII_LINE);
            assert_eq!(
                rendered["start"]["column"],
                fixtures::NON_ASCII_BYTE_COLUMN,
                "the browser reports SourceMap's byte column, got {rendered}"
            );
        }

        /// A refusal's `error` text is the shared pipeline's own
        /// [`NotFormatted::reason`](oxabl_pipeline::NotFormatted::reason), not a
        /// message this client assembled.
        ///
        /// The wire shape has one `error` field, so the browser cannot carry the
        /// [`NotFormattedKind`](oxabl_pipeline::NotFormattedKind) the table now
        /// pins — that discriminant is genuinely unavailable here. Comparing the
        /// text against the pipeline's own rendering is what recovers the claim:
        /// a bail that regressed into a contained panic reports a different
        /// reason, and this fails.
        #[test]
        fn a_refusal_reports_the_shared_pipelines_reason() {
            let shared = FormatPipeline::new(PipelineConfig::default().style);
            let mut refusals = 0;
            for fixture in FIXTURES {
                let Some(refusal) = shared.format(fixture.source).not_formatted().cloned() else {
                    continue;
                };
                refusals += 1;
                assert_eq!(
                    formatted(fixture.source)["error"],
                    serde_json::Value::String(refusal.reason()),
                    "{}",
                    fixture.name
                );
            }
            assert!(refusals > 0, "the table must carry a refusal fixture");
        }

        /// The browser's format style is the shared default configuration's,
        /// derived the same way rather than fetched from `StyleGuide` directly.
        ///
        /// The full-table comparison above already fails on any style difference
        /// the drift fixture can *see*. This one is narrower on purpose: it runs
        /// the same source through a `FormatPipeline` built from
        /// `PipelineConfig::default().style` — a second, independent derivation —
        /// and demands byte equality, so a browser that starts configuring
        /// formatting from its own source of truth fails here by name instead of
        /// waiting for a fixture whose bytes happen to disagree.
        #[test]
        fn the_format_style_comes_from_the_shared_default_config() {
            let shared = FormatPipeline::new(PipelineConfig::default().style);
            for fixture in FIXTURES {
                let response = formatted(fixture.source);
                let expected = match shared.format(fixture.source) {
                    FormatOutcome::Reformatted(bytes) => bytes,
                    FormatOutcome::Unchanged | FormatOutcome::DidNotFormat(_) => {
                        fixture.source.to_string()
                    }
                };
                assert_eq!(
                    response["source"], expected,
                    "the browser must format through the shared default style: {}",
                    fixture.name
                );
            }
        }

        // --- Capability gaps, asserted as gaps ------------------------------

        /// Includes are an **unavailable capability**, not a different answer:
        /// the entry point takes source only, so there is nowhere to put a search
        /// path and the preprocessor is off. The include fixture therefore
        /// produces nothing — and the table says so, which is why it is excluded
        /// from the comparison above rather than expected to be clean there.
        #[test]
        fn include_resolution_is_an_unavailable_capability() {
            let fixture = fixtures::fixture("unresolvable_include");
            assert!(
                fixture.needs_capability(Capability::IncludeResolution),
                "the table must record this fixture's capability requirement"
            );
            assert!(
                !fixture.browser_comparable(),
                "a fixture needing a capability the browser lacks is not comparable"
            );
            // The capability is absent, so the loud warning the other three legs
            // assert cannot be produced here at all.
            assert!(
                !observed(fixture.source)
                    .iter()
                    .any(|d| d.code == "PREPROC007"),
                "with no include resolution there is no include to fail to resolve"
            );
        }

        /// A schema is an unavailable capability for the same reason, so
        /// `unknown-table-or-field` is inert in the browser.
        #[test]
        fn a_loaded_schema_is_an_unavailable_capability() {
            let fixture = fixtures::fixture("unknown_field");
            assert!(fixture.needs_capability(Capability::Schema));
            assert!(!fixture.browser_comparable());
            assert!(
                !observed(fixture.source)
                    .iter()
                    .any(|d| d.code == "LINT0003"),
                "the rule is schema-gated and the browser has no schema"
            );
        }

        /// Per-rule severity is also an unavailable capability: there is no
        /// `oxabl.toml` and no second parameter on the entry point, so the
        /// override the other three legs apply has nowhere to enter.
        ///
        /// Asserted as a gap: the browser reports the rule at its *un-overridden*
        /// severity, and no call form exists that would change it.
        #[test]
        fn per_rule_severity_is_an_unavailable_capability() {
            let fixture = fixtures::fixture(fixtures::OVERRIDE_FIXTURE);
            let target = observed(fixture.source)
                .into_iter()
                .find(|d| d.code == fixtures::OVERRIDE_CODE)
                .unwrap_or_else(|| panic!("expected {}", fixtures::OVERRIDE_CODE));

            let un_overridden = fixtures::browser_expected(fixture)
                .into_iter()
                .find(|d| d.code == fixtures::OVERRIDE_CODE)
                .expect("the table carries this code");
            assert_eq!(target.severity, un_overridden.severity);
            assert_ne!(
                target.severity,
                fixtures::OVERRIDE_SEVERITY,
                "if the override's severity were already the default, this would assert nothing"
            );
        }

        // --- The regression this suite already caught once -------------------

        /// The browser reports the **same severity** as every filesystem-backed
        /// client for a rule whose built-in severity differs from its configured
        /// default (R19).
        ///
        /// This is the assertion the parity suite earned. On its first run the
        /// browser returned `error` for `type-mismatch-assignment` and
        /// `unknown-table-or-field` where the CLI and the LSP returned `warning`,
        /// under the same empty environment — two default severity tables, one
        /// materialized by `PipelineConfig::resolve` and one left empty by
        /// `PipelineConfig::default`, disagreeing on exactly those two rules.
        ///
        /// The tables are now one table. This test is narrower than the
        /// full-table comparison above on purpose: it names the specific rule
        /// whose two severities disagree, so re-introducing a second default
        /// table fails here with an obvious message rather than somewhere in a
        /// diff of whole diagnostic sets.
        #[test]
        fn browser_severity_matches_every_other_client() {
            let fixture = fixtures::fixture("type_mismatch");
            let observed = observed(fixture.source);
            let browser = observed
                .iter()
                .find(|d| d.code == "LINT0004")
                .unwrap_or_else(|| panic!("expected LINT0004, got {observed:?}"));
            let shared = fixture
                .expected()
                .into_iter()
                .find(|d| d.code == "LINT0004")
                .expect("the table carries LINT0004");

            assert_eq!(
                browser.severity, shared.severity,
                "the browser must not be a variable in the answer: a second \
                 default severity table has been reintroduced"
            );
            assert_eq!((browser.start, browser.end), (shared.start, shared.end));
            assert_eq!(browser.code, shared.code);
            assert_eq!(browser.source, shared.source);
        }
    }
}
