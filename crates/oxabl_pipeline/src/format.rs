//! The shared format run: one handle that cannot preprocess, one outcome every
//! client derives from (R4, R5, R20).
//!
//! [`FormatPipeline`] is the formatting counterpart of
//! [`LintPipeline`](crate::LintPipeline), and it is deliberately the smaller of
//! the two: formatting is a pure function of raw source and a resolved
//! [`StyleGuide`], so the handle carries nothing else.
//!
//! # Why there is no `preprocess` switch (R4, KTD4)
//!
//! The formatter must see **raw** source. Every span it re-emits is a real byte
//! offset into the bytes on disk; expanded macro text has different offsets, so
//! formatting an expansion would rewrite the wrong bytes — silently, and in the
//! user's file. The lint pipeline needs the opposite (a macro-expanded view), so
//! the temptation is one shared `preprocess` flag plus a comment warning which
//! pipeline must never set it.
//!
//! This type takes the structural route instead: it is constructed from the
//! style guide **alone**, so there is nowhere to put such a flag and no code
//! path that could honor one. That is the enforcement mechanism, not a
//! convention — a future contributor must not "helpfully" add a filesystem, an
//! include path, or a preprocess input back onto this handle. If a caller needs
//! an expansion, it wants [`LintPipeline`](crate::LintPipeline).
//!
//! # Why the guard is one hop, not two (R20, KTD6)
//!
//! Panic containment is not invented here.
//! [`oxabl_formatter::try_format_source`] is already the formatter's whole
//! `tokenize → parse → format` pipeline wrapped in
//! [`oxabl_common::catch_panic`] — the one guard every client shares — so
//! calling it *is* calling that guard, and wrapping it in a second
//! `catch_panic` would only add a layer that can never fire.
//!
//! It must not reach for `oxabl::try_format_source` instead: that is the
//! umbrella's re-export, this crate sits *beneath* the umbrella, and the
//! umbrella's format entry point is re-pointed at this pipeline — so calling it
//! from here is a package cycle Cargo rejects and would be self-recursive even
//! if it linked. Likewise, no `std::panic::catch_unwind` appears here.
//!
//! One platform caveat carries through unchanged: `catch_panic` is a documented
//! pass-through on `wasm32-unknown-unknown`, where a panic traps instead of
//! unwinding. The browser therefore never observes
//! [`NotFormattedKind::InternalPanic`] — its protection is the panic hook plus
//! instance reinitialization, which lives outside this crate.

use oxabl_formatter::{FormatFailure, try_format_source};
use oxabl_style::StyleGuide;

/// The shared formatting decision — the one model every client renders (R5).
///
/// Three states, and the third is the interesting one: see [`NotFormatted`] for
/// why "did not format" is not a bare string. The CLI's per-file report, the
/// browser's `{source, changed, error}` wire struct, and the language server's
/// edits-or-no-edits decision are all derived from this enum rather than
/// re-deciding it.
///
/// On both non-[`Reformatted`](Self::Reformatted) arms the caller keeps the
/// original source bytes: `Unchanged` because there is nothing to write, and
/// `DidNotFormat` because the formatter declined to produce output at all. No
/// arm ever carries partially formatted bytes.
///
/// # Why this is *not* `#[non_exhaustive]`, unlike [`NotFormattedKind`]
///
/// The asymmetry is deliberate, and a reader must not "fix" it. This is the
/// primary result type, and all three clients match it exhaustively. Marking it
/// `#[non_exhaustive]` would force each of them to carry a wildcard arm that is
/// unreachable today — and that wildcard is precisely what would swallow a
/// future variant: adding one would compile everywhere, silently taking the
/// fallback path in every client instead of failing the build. The compile-time
/// gap is the feature. It is the same property the crate reasons about in
/// [`NotFormatted::kind`], where the two named [`FormatFailure`] arms stay
/// spelled out above the wildcard that type's `#[non_exhaustive]` requires.
///
/// [`NotFormattedKind`] is the opposite case: a classification label, not a
/// result, whose set could plausibly grow, and which no client needs to match
/// exhaustively — so it is `#[non_exhaustive]`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormatOutcome {
    /// The formatter ran and produced output byte-identical to the input. This
    /// is distinct from `Reformatted` with equal bytes on purpose: a client that
    /// writes files, or reports drift, needs "nothing to do" as its own answer.
    Unchanged,
    /// The formatter ran and produced different bytes, carried here.
    Reformatted(String),
    /// The formatter produced no output; the input bytes stand.
    DidNotFormat(NotFormatted),
}

impl FormatOutcome {
    /// Whether formatting would change the file — the per-file drift boolean
    /// `format --check` and `check` both report. A refusal is **not** drift.
    pub fn changed(&self) -> bool {
        matches!(self, FormatOutcome::Reformatted(_))
    }

    /// The reformatted bytes, or `None` on either of the leave-it-alone arms.
    pub fn output(&self) -> Option<&str> {
        match self {
            FormatOutcome::Reformatted(formatted) => Some(formatted),
            FormatOutcome::Unchanged | FormatOutcome::DidNotFormat(_) => None,
        }
    }

    /// The refusal, when the formatter produced no output.
    pub fn not_formatted(&self) -> Option<&NotFormatted> {
        match self {
            FormatOutcome::DidNotFormat(not_formatted) => Some(not_formatted),
            FormatOutcome::Unchanged | FormatOutcome::Reformatted(_) => None,
        }
    }
}

/// Why a file was left unformatted, keeping the bail-versus-panic distinction
/// **structural** (R5).
///
/// [`FormatFailure`] separates its `Bail` arm from its `Panic` arm deliberately,
/// so a caller can tell "the formatter correctly refused, given this input"
/// from "oxabl has a bug". Before this type existed that distinction survived
/// only as differing message *text* assembled by the CLI, which meant the
/// language server and the browser had to string-match to recover what
/// `FormatFailure` already models — and any client that reworded the message
/// destroyed it.
///
/// So this type carries the [`FormatFailure`] itself, plus a
/// [`NotFormattedKind`] discriminant a client can branch on without matching a
/// `#[non_exhaustive]` enum from another crate. Nothing is flattened into a
/// `String`; [`Display`](std::fmt::Display) renders the reason for a client
/// that only wants text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NotFormatted {
    failure: FormatFailure,
}

impl NotFormatted {
    /// The underlying failure, for a client that wants the specific
    /// [`FormatBail`](oxabl_formatter::FormatBail) or the
    /// [`InternalPanic`](oxabl_common::InternalPanic) message.
    pub fn failure(&self) -> &FormatFailure {
        &self.failure
    }

    /// Which of the two kinds of refusal this is — the distinction R5 exists to
    /// preserve.
    ///
    /// [`FormatFailure`] is `#[non_exhaustive]`, so an arm this function has not
    /// been taught about classifies as [`NotFormattedKind::InternalPanic`]. That
    /// is the loud default on purpose: an unclassified failure is a wiring gap in
    /// oxabl, and reporting it as an oxabl defect is both the honest answer and
    /// the one someone will notice.
    pub fn kind(&self) -> NotFormattedKind {
        match &self.failure {
            // Both named arms stay explicit above the wildcard. `FormatFailure`
            // is `#[non_exhaustive]`, so a wildcard is unavoidable; keeping
            // these two spelled out is what makes a future arm a visible gap in
            // review — grep for `NotFormattedKind` and the classification is
            // right here.
            FormatFailure::Bail(_) => NotFormattedKind::Bail,
            FormatFailure::Panic(_) => NotFormattedKind::InternalPanic,
            // An arm added upstream and not classified here is an unfinished
            // wiring job, so it fails loud: `InternalPanic` is the answer that
            // makes a client report it as an oxabl defect. Defaulting to `Bail`
            // would blame the input instead and hide the omission behind a
            // plausible-looking result.
            _ => NotFormattedKind::InternalPanic,
        }
    }

    /// Whether this refusal is an oxabl bug rather than a property of the input.
    /// The one question a client asks to decide between "report and move on" and
    /// "report this as a defect".
    pub fn is_internal_panic(&self) -> bool {
        self.kind() == NotFormattedKind::InternalPanic
    }

    /// The human-readable reason, for a client whose surface is text (the CLI's
    /// per-file line, the browser's `error` field).
    pub fn reason(&self) -> String {
        self.failure.to_string()
    }
}

impl std::fmt::Display for NotFormatted {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.failure.fmt(f)
    }
}

impl From<FormatFailure> for NotFormatted {
    fn from(failure: FormatFailure) -> Self {
        NotFormatted { failure }
    }
}

/// Whether a file was left unformatted by the formatter's own choice or by a
/// contained panic.
///
/// `#[non_exhaustive]`: this is a classification label whose set could plausibly
/// grow with the formatter's failure modes, and no client needs to match it
/// exhaustively — [`NotFormatted::is_internal_panic`] answers the one question
/// clients actually ask. See [`FormatOutcome`] for why the result type it rides
/// on deliberately is *not* marked the same way.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum NotFormattedKind {
    /// The formatter deliberately declined — parse errors, a tripped
    /// semantic-preservation guard, or a file that only parses after expansion.
    /// Expected behavior on some inputs, not a defect.
    Bail,
    /// The formatter panicked and the guard contained it. An oxabl bug; the
    /// message is worth reporting verbatim. Also the classification for a
    /// `FormatFailure` arm [`NotFormatted::kind`] has not been taught about,
    /// which is likewise an oxabl bug.
    InternalPanic,
}

/// The shared format run, constructed once from a resolved [`StyleGuide`] and
/// reused across many files or many edits of one buffer (R1).
///
/// The style guide is owned rather than borrowed — unlike
/// [`LintPipeline`](crate::LintPipeline), which borrows a config holding a whole
/// schema — because a `StyleGuide` is a small, plain value and owning it lets a
/// client stand this handle up from a preset in one expression.
///
/// There is no filesystem, no include path, and no preprocess input, and that
/// absence is the enforcement of R4/KTD4: the formatter must see raw source,
/// because formatting expanded macro output would rewrite the wrong bytes. A
/// flag that cannot be passed cannot be passed wrongly.
#[derive(Debug, Clone)]
pub struct FormatPipeline {
    style: StyleGuide,
}

impl FormatPipeline {
    /// A pipeline that formats to `style`.
    pub fn new(style: StyleGuide) -> Self {
        FormatPipeline { style }
    }

    /// The style guide this pipeline formats to.
    pub fn style(&self) -> &StyleGuide {
        &self.style
    }

    /// Format `source` — **raw**, unexpanded bytes — into a [`FormatOutcome`].
    ///
    /// Guarded, so a panic anywhere in `tokenize → parse → format` becomes a
    /// [`NotFormattedKind::InternalPanic`] outcome rather than unwinding into a
    /// caller that may already have rewritten earlier files. The guard is
    /// `try_format_source`'s own [`catch_panic`](oxabl_common::catch_panic),
    /// which is why this does not wrap it a second time — a second layer could
    /// never fire, and reaching for the umbrella's re-export instead would be a
    /// package cycle.
    pub fn format(&self, source: &str) -> FormatOutcome {
        match try_format_source(source, &self.style) {
            // Byte-identical output is its own answer, not a `Reformatted` a
            // caller has to compare for itself.
            Ok(formatted) if formatted == source => FormatOutcome::Unchanged,
            Ok(formatted) => FormatOutcome::Reformatted(formatted),
            Err(failure) => FormatOutcome::DidNotFormat(failure.into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use oxabl_formatter::FormatBail;
    use oxabl_style::KeywordCase;

    use super::*;

    /// Silence the default panic hook's stderr backtrace for the duration of a
    /// deliberately-panicking test, so a green run does not look like a crash.
    fn quietly<T>(f: impl FnOnce() -> T) -> T {
        let previous = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        let out = f();
        std::panic::set_hook(previous);
        out
    }

    fn base() -> FormatPipeline {
        FormatPipeline::new(StyleGuide::default_base())
    }

    #[test]
    fn reindents_a_block_and_reports_the_new_bytes() {
        let outcome = base().format("IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n");

        let formatted = outcome
            .output()
            .unwrap_or_else(|| panic!("expected Reformatted, got {outcome:?}"));
        assert!(
            formatted.contains("    MESSAGE"),
            "body must be indented, got {formatted:?}"
        );
        assert!(outcome.changed());
    }

    // The distinction the CLI's "nothing to do" and `--check`'s drift boolean
    // both rest on: identical bytes are `Unchanged`, never `Reformatted`.
    #[test]
    fn already_formatted_source_is_unchanged_not_reformatted_with_equal_bytes() {
        let pipeline = base();
        let source = "IF TRUE THEN DO:\n    MESSAGE \"hi\".\nEND.\n";
        let outcome = pipeline.format(source);

        assert_eq!(outcome, FormatOutcome::Unchanged, "got {outcome:?}");
        assert!(!outcome.changed());
        assert!(outcome.output().is_none(), "no bytes to write");
    }

    #[test]
    fn formatting_is_idempotent_through_the_handle() {
        let pipeline = base();
        let first = pipeline.format("IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n");
        let formatted = first
            .output()
            .unwrap_or_else(|| panic!("expected Reformatted, got {first:?}"))
            .to_string();

        assert_eq!(
            pipeline.format(&formatted),
            FormatOutcome::Unchanged,
            "the pipeline's own output must be a fixed point"
        );
    }

    // R5, bail half: a refusal carries a reason, no bytes, and the *bail*
    // discriminant. `ParseErrors` is the bail used here rather than
    // `SemanticGuardTripped` — see the note on
    // `semantic_guard_bail_has_no_known_trigger`.
    #[test]
    fn a_bail_carries_the_bail_discriminant_a_reason_and_no_output() {
        let outcome = base().format("@ @ @\n");

        let refusal = outcome
            .not_formatted()
            .unwrap_or_else(|| panic!("expected DidNotFormat, got {outcome:?}"));
        assert_eq!(refusal.kind(), NotFormattedKind::Bail);
        assert!(!refusal.is_internal_panic());
        assert_eq!(
            refusal.failure(),
            &FormatFailure::Bail(FormatBail::ParseErrors)
        );
        assert!(!refusal.reason().is_empty(), "a refusal states a reason");
        assert!(outcome.output().is_none(), "a refusal emits no bytes");
        assert!(!outcome.changed(), "a refusal is not drift");
    }

    // R5, panic half: the same variant, a *different* discriminant, and the
    // panic message intact — so a client never has to string-match the reason to
    // tell an oxabl bug from a correct refusal. Injected through
    // `oxabl_common`'s `test-panics` dev-feature, which arms the `format` site
    // inside `try_format_source`'s guarded region; no real ABL input panics.
    #[test]
    fn a_contained_panic_is_distinguishable_from_a_bail() {
        let source = "/* OXABL-TEST-PANIC:format */\nMESSAGE \"hi\".\n";
        let outcome = quietly(|| base().format(source));

        let refusal = outcome
            .not_formatted()
            .unwrap_or_else(|| panic!("expected DidNotFormat, got {outcome:?}"));
        assert_eq!(refusal.kind(), NotFormattedKind::InternalPanic);
        assert!(refusal.is_internal_panic());
        let panic = match refusal.failure() {
            FormatFailure::Panic(panic) => panic,
            other => panic!("expected a contained panic, got {other:?}"),
        };
        assert!(
            panic.message().contains("injected test panic at format"),
            "the panic message must survive, got {:?}",
            panic.message()
        );
        assert!(outcome.output().is_none(), "a panic emits no bytes");
    }

    // The two kinds are not merely different strings — a client that branches on
    // `kind()` gets different answers for the same `DidNotFormat` variant.
    #[test]
    fn the_two_refusal_kinds_do_not_collapse() {
        let bail = base().format("@ @ @\n");
        let panicked =
            quietly(|| base().format("/* OXABL-TEST-PANIC:format */\nMESSAGE \"hi\".\n"));

        let bail = bail.not_formatted().expect("a bail");
        let panicked = panicked.not_formatted().expect("a contained panic");
        assert_ne!(bail.kind(), panicked.kind());
        assert_ne!(bail, panicked);
    }

    #[test]
    fn the_style_guide_drives_the_output() {
        let source = "if true then do:\n    message \"hi\".\nend.\n";

        // The safe default preserves the author's keywords as written, so the
        // only thing left to fix here is nothing.
        assert_eq!(
            base().format(source),
            FormatOutcome::Unchanged,
            "default_base must not recase keywords"
        );

        let mut recasing = StyleGuide::default_base();
        recasing.keyword_case = KeywordCase::Uppercase;
        let outcome = FormatPipeline::new(recasing).format(source);
        let formatted = outcome
            .output()
            .unwrap_or_else(|| panic!("expected Reformatted, got {outcome:?}"));
        assert!(
            formatted.contains("MESSAGE"),
            "recasing must reach the output, got {formatted:?}"
        );
    }

    // Documents a scenario that cannot be tested rather than faking it: no input
    // in this workspace trips the semantic-preservation guard, so the *specific*
    // `SemanticGuardTripped` bail is unreachable from here. It is classified by
    // the same explicit `FormatFailure::Bail` arm the reachable bails go
    // through, which this pins directly.
    #[test]
    fn semantic_guard_bail_has_no_known_trigger() {
        let refusal: NotFormatted = FormatFailure::Bail(FormatBail::SemanticGuardTripped).into();
        assert_eq!(refusal.kind(), NotFormattedKind::Bail);
        assert!(!refusal.reason().is_empty());
    }

    // The handle is reusable across files (R1) and carries no per-run state.
    #[test]
    fn one_handle_serves_many_files() {
        let pipeline = base();
        assert_eq!(pipeline.style().keyword_case, KeywordCase::Preserve);
        assert!(
            pipeline
                .format("IF TRUE THEN DO:\nMESSAGE \"a\".\nEND.\n")
                .changed()
        );
        assert_eq!(
            pipeline.format("MESSAGE \"b\".\n"),
            FormatOutcome::Unchanged
        );
    }
}
