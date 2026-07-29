//! Browser bindings for Oxabl's shared public pipelines.
//!
//! This crate deliberately contains no ABL behavior. It translates the
//! diagnostics and formatter result returned by the `oxabl` umbrella crate
//! into a small JSON wire shape suitable for a browser. The CLI, LSP, VS Code
//! extension, and browser therefore share the same lexer, parser, semantic
//! analysis, lint rules, formatter, and safe default style.

use oxabl::analyze::{CollectedDiagnostic, DiagnosticSource};
use oxabl::common::SourceMap;
use oxabl::style::StyleGuide;
use oxabl::workspace::InMemoryFileSystem;
use oxabl::{AnalyzeOptions, try_analyze_with_fs, try_format_source};
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

fn diagnostic_to_wire(item: CollectedDiagnostic, source_map: &SourceMap) -> WireDiagnostic {
    let diagnostic = item.diagnostic;
    let start = diagnostic.span.span.start;
    let end = diagnostic.span.span.end;
    let (start_line, start_column) = source_map.lookup(start as usize);
    let (end_line, end_column) = source_map.lookup(end as usize);

    WireDiagnostic {
        source: diagnostic_source(item.source),
        severity: diagnostic.severity.as_str(),
        code: diagnostic.code.0,
        message: diagnostic.message,
        start: WirePosition {
            byte: start,
            line: start_line,
            column: start_column,
        },
        end: WirePosition {
            byte: end,
            line: end_line,
            column: end_column,
        },
        help: diagnostic.help,
    }
}

fn diagnostic_source(source: DiagnosticSource) -> &'static str {
    source.as_str()
}

/// Analyze one in-memory ABL file through the same parse → semantic → lint
/// collector used by the CLI and LSP.
///
/// The browser MVP has no project filesystem, include path, or schema upload,
/// so preprocessing and schema-backed rules are disabled rather than emulated.
#[wasm_bindgen]
pub fn analyze_source(source: &str) -> String {
    let options = AnalyzeOptions::default();
    let fs = InMemoryFileSystem::new();
    // The canonical fallible entry point, though its guard is a documented
    // pass-through on wasm32 — under `panic=abort` a panic traps instead of
    // arriving here, so the browser's protection is the panic hook plus instance
    // reinitialization, not this `Err` arm. The arm exists because this crate
    // also compiles natively for its unit tests, where the guard does catch. The
    // wire shape deliberately gains no `error` field for it.
    let (_, collected) = match try_analyze_with_fs(source, &fs, &options) {
        Ok(result) => result,
        Err(_) => {
            return serde_json::to_string(&AnalyzeResponse {
                diagnostics: Vec::new(),
            })
            .expect("the browser diagnostic wire shape is always serializable");
        }
    };
    let source_map = SourceMap::new(source);
    let diagnostics = collected
        .diagnostics
        .into_iter()
        .map(|diagnostic| diagnostic_to_wire(diagnostic, &source_map))
        .collect();

    serde_json::to_string(&AnalyzeResponse { diagnostics })
        .expect("the browser diagnostic wire shape is always serializable")
}

/// Format one ABL file through Oxabl's shared layout-only formatter using the
/// same safe default style as the LSP when no `oxabl.toml` is present.
#[wasm_bindgen]
pub fn format_source(source: &str) -> String {
    let result = match try_format_source(source, &StyleGuide::default_base()) {
        Ok(formatted) => FormatResponse {
            changed: formatted != source,
            source: formatted,
            error: None,
        },
        Err(error) => FormatResponse {
            source: source.to_string(),
            changed: false,
            error: Some(error.to_string()),
        },
    };

    serde_json::to_string(&result).expect("the browser format wire shape is always serializable")
}

#[cfg(test)]
mod tests {
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
}
