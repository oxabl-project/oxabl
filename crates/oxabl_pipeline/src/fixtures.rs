//! One fixture table, shared by all four cross-client parity legs (R19).
//!
//! # Why this lives in `src/` behind a feature
//!
//! `#[cfg(test)]` is unreachable from another crate, and the parity claim is
//! precisely that four *different crates* agree. So the table has to be a normal
//! module that downstream test targets can `use` — which, ungated, would make
//! test data part of this crate's public API *and* compile it into the
//! `oxabl_wasm` payload, since the umbrella re-exports `oxabl_pipeline`
//! unconditionally so the browser can reach it.
//!
//! The `test-support` feature is therefore **off by default** and enabled only
//! from each downstream leg's `[dev-dependencies]`, the same arrangement
//! `oxabl_schema/test-support` already uses. A plain `cargo build` never
//! compiles this file.
//!
//! This crate's *own* leg ([`crate::parity`]) reaches the table through
//! `#[cfg(test)]` instead, which is why the module is gated on `any(test,
//! feature = "test-support")`. Enabling a feature on this crate for a target
//! under `tests/` would mean a self `path = "."` dev-dependency, and
//! release-please's `cargo-workspace` plugin treats that edge as a dependency
//! cycle and fails the release. Nothing is lost: the cross-*crate* claim is
//! carried by the three legs that genuinely are external crates.
//!
//! # What the table pins, and what it deliberately does not
//!
//! Each fixture carries a synthetic ABL source (CC: every string here is
//! invented), the byte-spanned diagnostics it must produce, the format outcome
//! it must produce, and the *capabilities* it needs. Spans are compared as
//! **byte offsets**, never as rendered line/column: the language server derives
//! positions from its own `Rope` under a negotiated encoding, and conflating
//! that conversion with pipeline output would make an encoding bug look like a
//! parity failure (KTD5).
//!
//! One row ([`NON_ASCII_FIXTURE`]) is deliberately **not** ASCII, so that
//! caution is actually exercised: everywhere else a byte offset and a character
//! offset are the same integer, and a client that confused them would pass. Each
//! leg additionally checks the rendered position it derives *for that row* —
//! which is where a rendered position belongs, since it is per-client output
//! rather than a shared answer.
//!
//! [`Capability`] is the other half of the design. A client that cannot be given
//! a schema, or cannot resolve an include, must not be asserted to produce the
//! same diagnostics as one that can — the strategy's distinction is between
//! *fewer inputs* and *different behavior*. A leg lacking a capability asserts
//! the **gap** instead.
//!
//! # The canonical configuration
//!
//! [`canonical_config`] is what every filesystem-backed client resolves with no
//! `oxabl.toml` present: `WorkspaceConfig::defaults()` run through
//! [`resolve_from_config`]. It agrees with [`PipelineConfig::default`], which is
//! what the browser builds — the two read one severity table now, so the client
//! is not a variable in the answer. `canonical_config` remains the named anchor
//! the table is written against; see its own docs.
//!
//! # Cross-file rows (R7)
//!
//! A fixture may carry [`SiblingFile`]s: further synthetic ABL sources that live
//! *beside* the root one and are what a cross-file name resolves to. They are
//! held as in-memory content with **relative** paths, because each leg roots them
//! somewhere different — three legs load them into an
//! [`InMemoryFileSystem`] under `/parity`, and the CLI leg writes the same
//! relative paths into a temp directory it then passes as `-I`. The comparison
//! is still codes, severities, byte spans, and sources: byte spans in the *root*
//! buffer, so a sibling's own coordinates never enter the claim.
//!
//! **There is deliberately no `Capability::CrossFile`.** The settled decision is
//! that every client resolves over whatever filesystem it is given, so the
//! browser leg is *given one* through an internal seam. What the browser lacks is
//! a JS-exported way to supply files, and that is a scope boundary rather than a
//! parity carve-out — a capability variant here would be exactly the
//! compensating flag that hides the divergence this table exists to expose.
//!
//! Each cross-file row also declares its [`CrossFileResolution`]s: what
//! supplying the siblings actually achieves. Only one arm is visible in the
//! diagnostic channel every client shares — [`CrossFileEffect::Resolved`], the
//! `undefined-symbol` false positive that cross-file resolution *removes* — so a
//! row's `diagnostics` are always the **with-siblings** answer and
//! [`ParityFixture::expected_without_siblings`] adds the removed findings back.
//! That direction matters: attaching an index must never *add* a diagnostic
//! (R11), so withholding the siblings can only ever make a leg louder.

use std::path::{Path, PathBuf};

use oxabl_analyze::{CollectedDiagnostic, CollectedDiagnostics, DiagnosticSource};
use oxabl_common::Severity;
use oxabl_schema::Schema;
use oxabl_workspace::{InMemoryFileSystem, LintConfig, LintSeverity, WorkspaceConfig};

use crate::{
    ConfigOverrides, FormatOutcome, NotFormattedKind, PipelineConfig, resolve_from_config,
};

/// An input a client may or may not be able to supply.
///
/// A fixture naming a capability is only comparable on legs that have it;
/// elsewhere the leg asserts the capability is absent rather than asserting a
/// different diagnostic set.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Capability {
    /// A loaded `.df` schema. The browser has no upload path, so
    /// `unknown-table-or-field` is inert there.
    Schema,
    /// Include resolution (the preprocessor plus a search path). The browser
    /// runs with preprocessing off and no filesystem, so no `PREPROC007` can
    /// ever be produced there.
    IncludeResolution,
}

/// A diagnostic a fixture must produce, in the pipeline's own coordinates.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExpectedDiagnostic {
    pub code: &'static str,
    pub severity: Severity,
    pub source: DiagnosticSource,
    /// Byte offset of the span start in the *root* buffer.
    pub start: u32,
    /// Byte offset of the span end in the *root* buffer.
    pub end: u32,
}

/// The format decision a fixture must produce, without carrying the bytes for
/// the reformat case where the exact output is the formatter's business rather
/// than the parity claim's.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpectedFormat {
    /// Already conforms: no drift, nothing to write.
    Unchanged,
    /// Would be rewritten. The expected bytes travel separately so a formatter
    /// improvement is not a parity failure.
    Reformatted(&'static str),
    /// The formatter declined. A refusal is **not** drift.
    ///
    /// The [`NotFormattedKind`] travels with it because "some refusal happened"
    /// is not the claim worth pinning. The crate keeps the bail-versus-panic
    /// split *structural* precisely so a client can tell "the formatter correctly
    /// declined, given this input" from "oxabl has a bug" — and a table that
    /// accepted any refusal would let a bail regress into a contained
    /// [`InternalPanic`](oxabl_common::InternalPanic) with every leg still green,
    /// losing exactly the distinction the design invested in.
    Refused(NotFormattedKind),
}

/// A further synthetic ABL file living beside a fixture's root source, carried as
/// content rather than as a path into the repository.
///
/// `path` is **relative** to whatever directory a leg roots the fixture at, and
/// must stay so: the pipeline, language-server, and browser legs root at
/// `/parity` in an [`InMemoryFileSystem`] while the CLI leg roots at a temp
/// directory, so an absolute path here would only be right for three of them.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SiblingFile {
    /// Path relative to the fixture's root directory, e.g.
    /// `orders/calc-base.cls`. Neutral and obviously synthetic.
    pub path: &'static str,
    /// The file's synthetic ABL content.
    pub source: &'static str,
}

/// What supplying a fixture's siblings achieves for one cross-file name.
///
/// Four explicit arms rather than a boolean, because "nothing changed" has three
/// genuinely different causes and a leg's assertion is only meaningful if the
/// table says which one it is. No catch-all arm: a fifth kind of cross-file
/// answer must be a compile error at every match site.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CrossFileEffect {
    /// A sibling supplies the name, and the resolution is visible in the shared
    /// diagnostic channel: the finding below fires when the siblings are
    /// **withheld** and must not fire when they are supplied. This is the
    /// subclass-to-parent false positive cross-file resolution removes.
    Resolved(ExpectedDiagnostic),
    /// A sibling supplies the name, but the resolution is not observable in the
    /// diagnostic channel — it lands in the semantic model's reference entries,
    /// which only `analyze`'s envelope renders and no parity leg can see. What
    /// the four legs then assert is an **agreed silence**: supplying the sibling
    /// makes no client louder or quieter, so none of them has invented a finding
    /// out of a cross-file link.
    ResolvedSilently,
    /// No supplied file declares the name. The finding that names it stands with
    /// the siblings supplied exactly as without them — a resolver reaching for a
    /// plausible-looking neighbour instead of declining would show up here.
    Unresolvable,
    /// The reference cannot be known statically at all (a `RUN VALUE(...)`
    /// target). No client resolves it and none reports it: silence either way,
    /// derived from unknowability rather than from absence.
    Unknowable,
}

/// One cross-file name a fixture exercises, and what resolving it does.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CrossFileResolution {
    /// The reference as the fixture's root source writes it, for the assertion
    /// message. Not looked up — the effect below is what is asserted.
    pub name: &'static str,
    /// What supplying the siblings achieves for `name`.
    pub effect: CrossFileEffect,
}

/// One row of the shared table.
pub struct ParityFixture {
    /// Stable identifier, used in assertion messages and to look a row up.
    pub name: &'static str,
    /// Path, relative to the fixture's root directory, that every leg gives the
    /// root source. The CLI leg writes the file there; the pipeline and
    /// language-server legs hand the same spelling over as the analysed file's
    /// identity, so a fixture cannot resolve a name differently on one leg
    /// because that leg called its buffer something else.
    pub root_file: &'static str,
    /// Synthetic ABL source. Invented — nothing here comes from any real
    /// codebase.
    pub source: &'static str,
    /// Further synthetic files beside the root one, which is what a cross-file
    /// name resolves to. Empty for a single-file row.
    pub siblings: &'static [SiblingFile],
    /// What supplying `siblings` achieves. Non-empty exactly when `siblings` is
    /// — a row that plants files and claims nothing about them would assert that
    /// four clients agree without saying on what.
    pub resolutions: &'static [CrossFileResolution],
    /// Every diagnostic the fixture must produce, in byte spans. For a
    /// cross-file row this is the **with-siblings** answer; see
    /// [`Self::expected_without_siblings`].
    pub diagnostics: &'static [ExpectedDiagnostic],
    /// The format decision the fixture must produce.
    pub format: ExpectedFormat,
    /// Capabilities without which this fixture is not comparable.
    pub needs: &'static [Capability],
}

impl ParityFixture {
    /// Whether this fixture needs `capability`.
    pub fn needs_capability(&self, capability: Capability) -> bool {
        self.needs.contains(&capability)
    }

    /// Whether the browser can be asked this question at all.
    ///
    /// A [`Capability`] the browser's *exported* entry points cannot accept is
    /// what makes a fixture incomparable there. Sibling files are **not** such a
    /// capability: the browser resolves over whatever filesystem it is given, and
    /// the browser leg supplies one through an internal seam, so a cross-file row
    /// is fully comparable there.
    pub fn browser_comparable(&self) -> bool {
        self.needs.is_empty()
    }

    /// Whether this row's answer depends on files beside the root source.
    pub fn is_cross_file(&self) -> bool {
        !self.siblings.is_empty()
    }

    /// Whether withholding the siblings changes the diagnostic set — i.e. whether
    /// any resolution is [`CrossFileEffect::Resolved`].
    ///
    /// What the "no siblings supplied is inert, with them supplied is not" pair
    /// keys off: for a row where every effect is silent, unresolvable, or
    /// unknowable, the two answers are *equal* by design and the pair asserts
    /// that instead.
    pub fn siblings_change_the_answer(&self) -> bool {
        self.resolutions
            .iter()
            .any(|r| matches!(r.effect, CrossFileEffect::Resolved(_)))
    }

    /// Where a leg rooted at `root` puts the root source.
    pub fn root_path(&self, root: &Path) -> PathBuf {
        root.join(self.root_file)
    }

    /// This fixture's siblings, loaded into a fresh in-memory filesystem rooted
    /// at `root`.
    ///
    /// The three in-process legs share this loader so none of them can plant a
    /// file the others do not. The CLI leg writes the same relative paths to
    /// disk, since it drives a separate process.
    pub fn filesystem_under(&self, root: &Path) -> InMemoryFileSystem {
        let mut fs = InMemoryFileSystem::new();
        for sibling in self.siblings {
            fs.insert(root.join(sibling.path), sibling.source);
        }
        fs
    }

    /// The configuration this fixture must be run under, on a leg that has
    /// every capability: [`canonical_config`] plus a schema when the fixture
    /// needs one.
    pub fn config(&self) -> PipelineConfig {
        let mut config = canonical_config();
        if self.needs_capability(Capability::Schema) {
            config.schema = schema();
            config.schema_loaded = true;
        }
        config
    }

    /// [`Self::config`] with `root` on the include path, which is what makes a
    /// sibling reachable at all.
    ///
    /// A single-file row gets [`Self::config`] unchanged — no include path, so
    /// every existing row keeps running under exactly the configuration it
    /// always did, and the `{missing.i}` row's include stays unresolvable.
    pub fn config_under(&self, root: &Path) -> PipelineConfig {
        let mut config = self.config();
        if self.is_cross_file() {
            config.include_paths = vec![root.to_path_buf()];
        }
        config
    }

    /// The expected set in comparison form.
    pub fn expected(&self) -> Vec<ObservedDiagnostic> {
        normalize(
            self.diagnostics
                .iter()
                .map(|e| ObservedDiagnostic {
                    code: e.code.to_string(),
                    severity: e.severity,
                    source: e.source,
                    start: e.start,
                    end: e.end,
                })
                .collect(),
        )
    }

    /// The expected set when the siblings are **withheld**: this row's own
    /// diagnostics plus every finding a [`CrossFileEffect::Resolved`] arm says
    /// cross-file resolution removes.
    ///
    /// Strictly a superset of [`Self::expected`], and that direction is the R11
    /// firewall stated as data: attaching an index removes `undefined-symbol`
    /// findings on inherited members and must never add one, so the file-less
    /// answer can only be louder.
    pub fn expected_without_siblings(&self) -> Vec<ObservedDiagnostic> {
        let mut expected = self.expected();
        for resolution in self.resolutions {
            match resolution.effect {
                CrossFileEffect::Resolved(finding) => expected.push(ObservedDiagnostic {
                    code: finding.code.to_string(),
                    severity: finding.severity,
                    source: finding.source,
                    start: finding.start,
                    end: finding.end,
                }),
                // The other three arms are silent by construction — see
                // `CrossFileEffect`. Explicit arms rather than a catch-all so a
                // fifth effect cannot silently default to "changes nothing".
                CrossFileEffect::ResolvedSilently
                | CrossFileEffect::Unresolvable
                | CrossFileEffect::Unknowable => {}
            }
        }
        normalize(expected)
    }

    /// Assert an observed set equals what this row must produce with its siblings
    /// **withheld** — the other half of the cross-file capability idiom.
    pub fn assert_diagnostics_without_siblings(
        &self,
        leg: &str,
        observed: Vec<ObservedDiagnostic>,
    ) {
        assert_eq!(
            normalize(observed),
            self.expected_without_siblings(),
            "{leg} diverged on fixture `{}` with its siblings withheld",
            self.name
        );
    }

    /// Assert an observed set equals the expectation, naming the leg so a
    /// failure says *which* client diverged.
    ///
    /// Takes the observed set unsorted; both sides are normalized here, because
    /// the CLI splits preprocessor diagnostics into a second JSON key and so
    /// cannot preserve pipeline order.
    pub fn assert_diagnostics(&self, leg: &str, observed: Vec<ObservedDiagnostic>) {
        let observed = normalize(observed);
        assert_eq!(
            observed,
            self.expected(),
            "{leg} diverged on fixture `{}`",
            self.name
        );
    }

    /// Assert a [`FormatOutcome`] matches this fixture's expectation.
    pub fn assert_format(&self, leg: &str, outcome: &FormatOutcome) {
        match (self.format, outcome) {
            (ExpectedFormat::Unchanged, FormatOutcome::Unchanged) => {}
            (ExpectedFormat::Reformatted(expected), FormatOutcome::Reformatted(actual)) => {
                assert_eq!(
                    actual, expected,
                    "{leg} produced different bytes on fixture `{}`",
                    self.name
                );
            }
            (ExpectedFormat::Refused(kind), FormatOutcome::DidNotFormat(refusal)) => {
                assert_eq!(
                    refusal.kind(),
                    kind,
                    "{leg} refused fixture `{}` for the wrong reason: {}",
                    self.name,
                    refusal.reason()
                );
            }
            (expected, actual) => panic!(
                "{leg} diverged on fixture `{}`: expected {expected:?}, got {actual:?}",
                self.name
            ),
        }
    }

    /// The format outcome as the three facts every client can observe, for a
    /// leg that never sees a [`FormatOutcome`] (the browser's wire shape, the
    /// CLI's report).
    ///
    /// `(would_change, has_output, refused)`.
    pub fn expected_format_facts(&self) -> (bool, bool, bool) {
        match self.format {
            ExpectedFormat::Unchanged => (false, false, false),
            ExpectedFormat::Reformatted(_) => (true, true, false),
            ExpectedFormat::Refused(_) => (false, false, true),
        }
    }

    /// The kind of refusal this fixture expects, or `None` when it expects the
    /// formatter to produce an answer at all.
    ///
    /// For the legs whose surface is not a [`FormatOutcome`]: the CLI reports a
    /// contained panic under its own `failures` key and a bail nowhere, so it
    /// needs the discriminant even though it never sees the type.
    pub fn expected_refusal_kind(&self) -> Option<NotFormattedKind> {
        match self.format {
            ExpectedFormat::Refused(kind) => Some(kind),
            ExpectedFormat::Unchanged | ExpectedFormat::Reformatted(_) => None,
        }
    }
}

/// A diagnostic as some client actually reported it, reduced to the four facts
/// R19 names plus the byte span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObservedDiagnostic {
    pub start: u32,
    pub end: u32,
    pub code: String,
    pub severity: Severity,
    pub source: DiagnosticSource,
}

impl ObservedDiagnostic {
    /// From the pipeline's own type — the pipeline and language-server legs.
    pub fn from_collected(collected: &CollectedDiagnostic) -> Self {
        ObservedDiagnostic {
            start: collected.diagnostic.span.span.start,
            end: collected.diagnostic.span.span.end,
            code: collected.diagnostic.code.0.to_string(),
            severity: collected.diagnostic.severity,
            source: collected.source,
        }
    }

    /// From a wire/JSON rendering — the CLI and browser legs, whose severity and
    /// source arrive as the lowercase strings [`Severity::as_str`] and
    /// [`DiagnosticSource::as_str`] produce.
    ///
    /// Panics on an unrecognized spelling: a client that invents a new tag has
    /// diverged, and silently mapping it to something plausible would hide
    /// exactly that.
    pub fn from_wire(code: &str, severity: &str, source: &str, start: u32, end: u32) -> Self {
        ObservedDiagnostic {
            start,
            end,
            code: code.to_string(),
            severity: severity_from_str(severity),
            source: source_from_str(source),
        }
    }
}

/// Every diagnostic in a pipeline result, in comparison form.
pub fn observed(diagnostics: &CollectedDiagnostics) -> Vec<ObservedDiagnostic> {
    diagnostics
        .all()
        .map(ObservedDiagnostic::from_collected)
        .collect()
}

/// Sort into a canonical order so two legs that emit the same set in a different
/// order still compare equal.
///
/// Order is a total one over the compared facts. `Severity` and
/// `DiagnosticSource` are deliberately not `Ord` upstream — a severity ordering
/// would invite "worse than" comparisons the diagnostic model does not promise —
/// so their wire spellings are the sort key here.
pub fn normalize(mut observed: Vec<ObservedDiagnostic>) -> Vec<ObservedDiagnostic> {
    observed.sort_by(|a, b| {
        (
            a.start,
            a.end,
            &a.code,
            a.severity.as_str(),
            a.source.as_str(),
        )
            .cmp(&(
                b.start,
                b.end,
                &b.code,
                b.severity.as_str(),
                b.source.as_str(),
            ))
    });
    observed
}

/// Parse the wire spelling of a [`Severity`].
pub fn severity_from_str(severity: &str) -> Severity {
    match severity {
        "error" => Severity::Error,
        "warning" => Severity::Warning,
        "info" => Severity::Info,
        "hint" => Severity::Hint,
        other => panic!("unknown severity spelling `{other}`"),
    }
}

/// Parse the wire spelling of a [`DiagnosticSource`].
pub fn source_from_str(source: &str) -> DiagnosticSource {
    match source {
        "parse" => DiagnosticSource::Parse,
        "preproc" => DiagnosticSource::Preproc,
        "semantic" => DiagnosticSource::Semantic,
        "lint" => DiagnosticSource::Lint,
        other => panic!("unknown diagnostic source spelling `{other}`"),
    }
}

/// The `.df` text every leg loads for the schema-gated fixture — the same
/// `Customer(CustNum, Name)` fixture the rest of the workspace's tests use.
///
/// Exposed as text, not only as a [`Schema`], because the CLI leg has to hand a
/// real file to `--schema`.
pub const CUSTOMER_DF: &str = oxabl_schema::test_support::CUSTOMER_DF;

/// The loaded form of [`CUSTOMER_DF`].
pub fn schema() -> Schema {
    oxabl_schema::test_support::customer_schema()
}

/// The directory the three in-process legs root a fixture's files at, and the
/// anchor [`canonical_config`] resolves against.
///
/// One constant so the pipeline, language-server, and browser legs cannot root a
/// fixture in three places and then compare answers derived from three different
/// include paths. The CLI leg roots at a temp directory instead — it drives a
/// separate process over a real filesystem — which is why sibling paths are
/// relative rather than absolute.
pub const PARITY_ROOT: &str = "/parity";

/// The configuration a filesystem-backed client resolves when there is no
/// `oxabl.toml`: `WorkspaceConfig::defaults()` through [`resolve_from_config`].
///
/// # Its relationship to `PipelineConfig::default()`
///
/// The two are the **same value**, by construction rather than by coincidence:
/// both lower `LintConfig::default()` through `to_severity_map`, and both take
/// `StyleGuide::default_base()`, an empty include path, and no schema. That
/// matters because the filesystem-backed clients resolve while the browser
/// builds a default directly — if the two ever stopped agreeing, one client
/// would be answering a different question about the same source.
///
/// It did once. `PipelineConfig::default()` used to leave its `LintSeverityMap`
/// empty, which is not "the defaults" but "no rule has a configured severity",
/// so `unknown-table-or-field` and `type-mismatch-assignment` came back at the
/// severity `oxabl_lint` constructs them with (`error`) instead of the
/// `[workspace.lint]` default a resolution materializes (`warn`). One table
/// closed that; `browser_expected` and the browser leg's
/// `browser_severity_matches_every_other_client` keep it closed.
///
/// So this function exists as the parity table's **named anchor**, not as a
/// different value: the table's expectations are written against "what a
/// filesystem-backed client resolves with no `oxabl.toml`", and saying that in
/// one place is what lets a leg's setup be read as the client's real default
/// rather than as test tuning.
pub fn canonical_config() -> PipelineConfig {
    let (config, warnings) = resolve_from_config(
        &WorkspaceConfig::defaults(),
        Path::new(PARITY_ROOT),
        &ConfigOverrides::default(),
    );
    assert!(
        warnings.is_empty(),
        "the all-defaults configuration must resolve cleanly: {warnings:?}"
    );
    config
}

// --- A known cross-client divergence, recorded rather than hidden -----------

/// The expected set as the *browser* reports it — identical to
/// [`ParityFixture::expected`], because the browser is not a variable in the
/// answer (R19).
///
/// # Why this function exists at all
///
/// It briefly did more. When the parity suite first ran it found a real
/// divergence: `unknown-table-or-field` and `type-mismatch-assignment` came back
/// as `error` from the browser and `warning` from every filesystem-backed
/// client, under the same empty environment. The cause was two default severity
/// tables — [`PipelineConfig::default`] left its map empty, so each rule kept
/// whatever severity `oxabl_lint` constructs it with, while
/// [`PipelineConfig::resolve`] materialized `[workspace.lint]`'s documented
/// defaults even with no config file present. This function used to apply that
/// delta so the rest of the comparison could still run.
///
/// The two tables are now one table, so the delta is empty and the indirection
/// is kept only as the seam a future capability difference would go through —
/// and as the record of what the suite was worth. If a client ever legitimately
/// needs a different expectation, it belongs here where it must be justified,
/// not scattered through a leg's assertions.
///
/// Only meaningful for a [`ParityFixture::browser_comparable`] fixture.
pub fn browser_expected(fixture: &ParityFixture) -> Vec<ObservedDiagnostic> {
    normalize(fixture.expected())
}

// --- The per-rule severity override case -----------------------------------

/// The fixture the severity-override scenario re-uses.
pub const OVERRIDE_FIXTURE: &str = "unused_variable";
/// The `[workspace.lint]` key for that fixture's rule.
pub const OVERRIDE_RULE: &str = "unused-variable";
/// The diagnostic code that key controls.
pub const OVERRIDE_CODE: &str = "LINT0002";
/// The severity the override selects — deliberately different from the rule's
/// default `warning`, so an override that silently failed to apply would be
/// visible.
pub const OVERRIDE_SEVERITY: Severity = Severity::Info;

/// An `oxabl.toml` applying the override, for the legs that configure through a
/// file on disk.
pub const OVERRIDE_TOML: &str = "[workspace]\nname = \"parity\"\n\
                                 [workspace.lint]\nunused-variable = \"info\"\n";

/// [`canonical_config`] with the override applied, for the legs that configure
/// in-process.
pub fn config_with_override() -> PipelineConfig {
    // Derived through `LintConfig` rather than by poking one entry into a
    // `LintSeverityMap`, so this is the same *kind* of value a resolved config
    // is: `to_severity_map` stays the single lowering.
    let mut lint = LintConfig::default();
    assert!(
        lint.set_by_name(OVERRIDE_RULE, LintSeverity::Info),
        "`{OVERRIDE_RULE}` must be a known rule name"
    );
    PipelineConfig {
        lint_severities: lint.to_severity_map(),
        ..canonical_config()
    }
}

// --- The non-ASCII case -----------------------------------------------------

/// The fixture whose source is *not* pure ASCII, so byte offsets and character
/// offsets disagree.
///
/// Named rather than looked up by string in three places because each leg also
/// asserts its own **rendered** position for it. Rendered positions are
/// deliberately outside the shared comparison — the whole table is byte spans, so
/// that an encoding conversion cannot masquerade as a pipeline divergence — which
/// means the conversions themselves are only covered if each client checks its
/// own, on a source where getting it wrong shows up at all.
pub const NON_ASCII_FIXTURE: &str = "non_ascii_prefix";

/// The 1-based line the non-ASCII fixture's finding sits on.
pub const NON_ASCII_LINE: usize = 2;

/// The 1-based **byte** column of that finding — `SourceMap`'s convention, and
/// therefore what [`position`](crate::position) and every byte-offset client
/// report.
///
/// Deliberately different from [`NON_ASCII_CHARACTER_COLUMN`]: a client that
/// counted characters here would produce this number's neighbour, which is the
/// bug the fixture exists to make visible.
pub const NON_ASCII_BYTE_COLUMN: usize = 33;

/// The 0-based character (and, for this source, UTF-16 code unit) column of the
/// same finding — what a position-encoding-aware client such as the language
/// server must send instead.
pub const NON_ASCII_CHARACTER_COLUMN: usize = 30;

// --- The table --------------------------------------------------------------

/// Every fixture, shared by all four legs.
pub const FIXTURES: &[ParityFixture] = &[
    ParityFixture {
        name: "undefined_symbol",
        root_file: "main.p",
        siblings: &[],
        resolutions: &[],
        source: "MESSAGE undefinedThing.\n",
        diagnostics: &[ExpectedDiagnostic {
            code: "LINT0001",
            severity: Severity::Error,
            source: DiagnosticSource::Lint,
            start: 8,
            end: 22,
        }],
        format: ExpectedFormat::Unchanged,
        needs: &[],
    },
    ParityFixture {
        name: "unused_variable",
        root_file: "main.p",
        siblings: &[],
        resolutions: &[],
        source: "DEFINE VARIABLE unusedVar AS INTEGER NO-UNDO.\n",
        diagnostics: &[ExpectedDiagnostic {
            code: "LINT0002",
            severity: Severity::Warning,
            source: DiagnosticSource::Lint,
            start: 16,
            end: 25,
        }],
        format: ExpectedFormat::Unchanged,
        needs: &[],
    },
    // Needs a loaded schema: with none, `unknown-table-or-field` is inert by
    // design and this source is clean.
    ParityFixture {
        name: "unknown_field",
        root_file: "main.p",
        siblings: &[],
        resolutions: &[],
        source: "FIND FIRST Customer.\nMESSAGE Customer.NoSuchField.\n",
        diagnostics: &[ExpectedDiagnostic {
            code: "LINT0003",
            severity: Severity::Warning,
            source: DiagnosticSource::Lint,
            start: 38,
            end: 49,
        }],
        format: ExpectedFormat::Unchanged,
        needs: &[Capability::Schema],
    },
    // `counter` is written *and* read, so neither LINT0002 (never referenced)
    // nor LINT0006 (written, never read) can fire — the two halves of that split
    // population must not be tripped by a fixture aimed at LINT0004.
    ParityFixture {
        name: "type_mismatch",
        root_file: "main.p",
        siblings: &[],
        resolutions: &[],
        source: "DEFINE VARIABLE counter AS INTEGER NO-UNDO.\ncounter = \"text\".\nMESSAGE counter.\n",
        diagnostics: &[ExpectedDiagnostic {
            code: "LINT0004",
            severity: Severity::Warning,
            source: DiagnosticSource::Lint,
            start: 44,
            end: 51,
        }],
        format: ExpectedFormat::Unchanged,
        needs: &[],
    },
    // Defined inside the DO block, assigned only there, read outside it.
    ParityFixture {
        name: "block_var_used_outside",
        root_file: "main.p",
        siblings: &[],
        resolutions: &[],
        source: "DO:\n    DEFINE VARIABLE tally AS INTEGER NO-UNDO.\n    tally = 1.\nEND.\nMESSAGE tally.\n",
        diagnostics: &[ExpectedDiagnostic {
            code: "LINT0005",
            severity: Severity::Info,
            source: DiagnosticSource::Lint,
            start: 24,
            end: 29,
        }],
        format: ExpectedFormat::Unchanged,
        needs: &[],
    },
    // Written once, never read: LINT0006's half of the split, reported at the
    // write site rather than the declaration.
    ParityFixture {
        name: "assigned_but_never_read",
        root_file: "main.p",
        siblings: &[],
        resolutions: &[],
        source: "DEFINE VARIABLE deadOne AS INTEGER NO-UNDO.\ndeadOne = 7.\n",
        diagnostics: &[ExpectedDiagnostic {
            code: "LINT0006",
            severity: Severity::Warning,
            source: DiagnosticSource::Lint,
            start: 44,
            end: 51,
        }],
        format: ExpectedFormat::Unchanged,
        needs: &[],
    },
    // Error recovery: the parse error is reported *and* the lint pass still runs
    // over the recovered tree. The formatter refuses a parse-dirty file, which is
    // also this table's refusal case — a `Bail`, since declining to format a file
    // that does not parse is correct behavior and not an oxabl defect.
    ParityFixture {
        name: "parse_error",
        root_file: "main.p",
        siblings: &[],
        resolutions: &[],
        source: "DEFINE VARIABLE leftAlone AS INTEGER NO-UNDO.\n@ @ @\nMESSAGE \"after\".\n",
        diagnostics: &[
            ExpectedDiagnostic {
                code: "PARSE001",
                severity: Severity::Error,
                source: DiagnosticSource::Parse,
                start: 46,
                end: 47,
            },
            ExpectedDiagnostic {
                code: "LINT0002",
                severity: Severity::Warning,
                source: DiagnosticSource::Lint,
                start: 16,
                end: 25,
            },
        ],
        format: ExpectedFormat::Refused(NotFormattedKind::Bail),
        needs: &[],
    },
    // Multi-byte text ahead of the finding, on both the preceding line and the
    // finding's own line. Every other fixture is pure ASCII, which made byte
    // offsets and character offsets numerically identical and so left the suite's
    // stated fear — byte-versus-UTF-16 confusion — never actually exercised. Here
    // the two disagree: line 2 begins at byte offset 23 but character offset 19,
    // and the span starts at 1-based byte column 33 where a 1-based character
    // column would say 31 (that is `NON_ASCII_CHARACTER_COLUMN`, which is 0-based,
    // plus one — the two constants keep the conventions their clients use). A client
    // that counted characters where it should count bytes now produces a wrong
    // *byte span* and fails the shared comparison, and the per-client rendered
    // positions are checked where they are each derived.
    ParityFixture {
        name: NON_ASCII_FIXTURE,
        source: "/* café — naïve */\n/* ¡señor! */ DEFINE VARIABLE unusedTwo AS INTEGER NO-UNDO.\n",
        diagnostics: &[ExpectedDiagnostic {
            code: "LINT0002",
            severity: Severity::Warning,
            source: DiagnosticSource::Lint,
            start: 55,
            end: 64,
        }],
        format: ExpectedFormat::Unchanged,
        needs: &[],
    },
    ParityFixture {
        name: "clean",
        root_file: "main.p",
        siblings: &[],
        resolutions: &[],
        source: "MESSAGE \"hello\".\n",
        diagnostics: &[],
        format: ExpectedFormat::Unchanged,
        needs: &[],
    },
    // Lint-clean, format-dirty: the drift channel on its own.
    ParityFixture {
        name: "format_drift",
        root_file: "main.p",
        siblings: &[],
        resolutions: &[],
        source: "DO:\nMESSAGE \"x\".\nEND.\n",
        diagnostics: &[],
        format: ExpectedFormat::Reformatted("DO:\n    MESSAGE \"x\".\nEND.\n"),
        needs: &[],
    },
    // The loud unresolvable-include warning. Needs include resolution, which is
    // to say the preprocessor: with it off the `{missing.i}` is never attempted
    // and the file is clean.
    ParityFixture {
        name: "unresolvable_include",
        root_file: "main.p",
        siblings: &[],
        resolutions: &[],
        source: "{missing.i}\nMESSAGE \"hello\".\n",
        diagnostics: &[ExpectedDiagnostic {
            code: "PREPROC007",
            severity: Severity::Warning,
            source: DiagnosticSource::Preproc,
            start: 0,
            end: 11,
        }],
        format: ExpectedFormat::Unchanged,
        needs: &[Capability::IncludeResolution],
    },
    // --- Cross-file rows (R7) ---------------------------------------------
    //
    // The parent class below is what makes cross-file resolution observable at
    // all: without it the inherited call is an `undefined-symbol` finding — the
    // false positive this line of work removes — so the *removal* is the shared
    // fact all four clients are held to.
    ParityFixture {
        name: "cross_file_inheritance",
        root_file: "orders/child.cls",
        siblings: &[SiblingFile {
            path: "orders/calc-base.cls",
            source: CALC_BASE,
        }],
        resolutions: &[CrossFileResolution {
            name: "calc-total",
            // Withheld, the inherited call is undefined. Supplied, the row is
            // clean — which is why `diagnostics` below is empty.
            effect: CrossFileEffect::Resolved(ExpectedDiagnostic {
                code: "LINT0001",
                severity: Severity::Error,
                source: DiagnosticSource::Lint,
                start: 149,
                end: 159,
            }),
        }],
        source: "CLASS orders.child INHERITS orders.calc-base:\n    \
                 METHOD PUBLIC VOID run-it():\n        \
                 DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n        \
                 v-total = calc-total().\n        \
                 MESSAGE v-total.\n    \
                 END METHOD.\nEND CLASS.\n",
        diagnostics: &[],
        format: ExpectedFormat::Unchanged,
        needs: &[],
    },
    // A `USING` import of a sibling class, then that class as a declared type, a
    // `NEW` target, and a member-call receiver. Every one of those resolves
    // through the index, and none of them is visible in the diagnostic channel:
    // the R11 firewall keeps an index-synthesized class at `ResolvedType::Unknown`
    // precisely so a cross-file type cannot start driving type diagnostics. So
    // the shared claim here is an agreed silence — four clients resolve the
    // import and not one of them invents a finding out of it.
    ParityFixture {
        name: "cross_file_using",
        root_file: "report.p",
        siblings: &[SiblingFile {
            path: "orders/calc-base.cls",
            source: CALC_BASE,
        }],
        resolutions: &[CrossFileResolution {
            name: "orders.calc-base",
            effect: CrossFileEffect::ResolvedSilently,
        }],
        source: "USING orders.calc-base.\n\
                 DEFINE VARIABLE v-calc AS CLASS orders.calc-base NO-UNDO.\n\
                 v-calc = NEW orders.calc-base().\n\
                 MESSAGE v-calc:calc-total().\n",
        diagnostics: &[],
        format: ExpectedFormat::Unchanged,
        needs: &[],
    },
    // A `RUN` whose target is a runtime string. The named file *is* supplied, so
    // a client tempted to guess has something to guess at — and the correct
    // answer is that nobody links it, because the name is not statically
    // knowable. Silence derived from unknowability, on all four legs.
    ParityFixture {
        name: "cross_file_dynamic_run",
        root_file: "dispatch.p",
        siblings: &[SiblingFile {
            path: "init-globals.p",
            source: "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.\n",
        }],
        resolutions: &[CrossFileResolution {
            name: "VALUE(v-program)",
            effect: CrossFileEffect::Unknowable,
        }],
        source: "DEFINE VARIABLE v-program AS CHARACTER NO-UNDO.\n\
                 v-program = \"init-globals.p\".\n\
                 RUN VALUE(v-program).\n",
        diagnostics: &[],
        format: ExpectedFormat::Unchanged,
        needs: &[],
    },
    // A parent no supplied file declares. A sibling *is* supplied — a different
    // class in the same package — so the row distinguishes "the search found
    // nothing" from "the search never ran": the finding must stand, identically,
    // with the siblings supplied and without them.
    ParityFixture {
        name: "cross_file_absent_parent",
        root_file: "orders/orphan.cls",
        siblings: &[SiblingFile {
            path: "orders/calc-base.cls",
            source: CALC_BASE,
        }],
        resolutions: &[CrossFileResolution {
            name: "orders.no-such-base",
            effect: CrossFileEffect::Unresolvable,
        }],
        source: "CLASS orders.orphan INHERITS orders.no-such-base:\n    \
                 METHOD PUBLIC VOID run-it():\n        \
                 DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n        \
                 v-total = calc-total().\n        \
                 MESSAGE v-total.\n    \
                 END METHOD.\nEND CLASS.\n",
        diagnostics: &[ExpectedDiagnostic {
            code: "LINT0001",
            severity: Severity::Error,
            source: DiagnosticSource::Lint,
            start: 153,
            end: 163,
        }],
        format: ExpectedFormat::Unchanged,
        needs: &[],
    },
];

/// The parent class two cross-file rows resolve to: one public method with a
/// return type, and nothing else. Synthetic.
const CALC_BASE: &str = "CLASS orders.calc-base:\n    \
                         METHOD PUBLIC INTEGER calc-total():\n        \
                         RETURN 0.\n    \
                         END METHOD.\nEND CLASS.\n";

/// Look a fixture up by name, panicking on a typo rather than silently skipping.
pub fn fixture(name: &str) -> &'static ParityFixture {
    FIXTURES
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("no parity fixture named `{name}`"))
}
