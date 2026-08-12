//! Panic containment for the malformed-input surface.
//!
//! The lexer and parser document that they may panic on some malformed inputs.
//! Every client used to hand-roll its own `std::panic::catch_unwind` around
//! them, inconsistently and — in the language server's case — not at all.
//! [`catch_panic`] is the one guard those clients share, and the `try_*` entry
//! points in `oxabl_parser`, `oxabl_formatter`, and `oxabl` are built on it.
//!
//! The guard lives here rather than in the `oxabl` umbrella crate because
//! `oxabl` optionally depends on `oxabl_lsp`, so the language server cannot
//! depend back on the umbrella — and the language server is where the guard
//! matters most.

use std::fmt;

/// A panic caught by [`catch_panic`], carrying the panic message.
///
/// This is the "the operation gave up in a way it was not designed to" arm.
/// It is never a diagnostic about the input: recovered parse errors arrive in
/// `Program::errors` and a formatter refusal arrives as a `FormatBail`. An
/// `InternalPanic` means an oxabl bug, and the message is worth reporting
/// verbatim.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InternalPanic {
    message: String,
}

impl InternalPanic {
    /// Build an `InternalPanic` from an already-extracted panic message.
    pub fn new(message: impl Into<String>) -> Self {
        InternalPanic {
            message: message.into(),
        }
    }

    /// The panic message, without the `internal error:` prefix that
    /// [`Display`](fmt::Display) adds.
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for InternalPanic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "internal error: {}", self.message)
    }
}

impl std::error::Error for InternalPanic {}

/// Run `f`, converting a panic into `Err(`[`InternalPanic`]`)`.
///
/// # Contract caveat: a payload this guard cannot describe travels through
///
/// The guard contains a panic whose payload is a `&str` or a `String` — which
/// is every panic oxabl raises itself. A payload of any other type is
/// **re-raised** with [`std::panic::resume_unwind`] instead of being reported
/// as `Err`.
///
/// A guard that cannot say what happened must not claim to have handled it. In
/// this workspace the rule is load-bearing rather than tidy: a cancelled
/// incremental recompute unwinds with a payload of its own type, and a guard
/// that swallowed one would turn an abandoned race into a confident wrong
/// answer — an empty result published as fact. Cancellation therefore travels
/// out of every guard built on this function and is caught at the layer that
/// understands it, which is the only layer that can tell it apart from a bug.
///
/// So a caller must not assume this contains everything. A caller that runs on
/// a cancellable substrate owns a catch of its own, placed *inside* this guard.
///
/// # Platform caveat: inert on `wasm32-unknown-unknown`
///
/// `wasm32-unknown-unknown` builds with `-Cpanic=abort` on stable Rust, where
/// `std::panic::catch_unwind` compiles but can never catch: a panic lowers to
/// an `unreachable` trap. Rather than pretend otherwise, this function is a
/// documented **pass-through** on `target_arch = "wasm32"` — it calls `f`
/// directly and always returns `Ok`. A silent no-op guard is worse than a
/// declared one, so browser consumers must not treat a `try_*` entry point as
/// protection; they need a panic hook plus instance reinitialization.
///
/// # Platform caveat: requires unwinding
///
/// Natively the guarantee is conditional on the unwinding panic strategy. A
/// `panic = "abort"` profile — in this workspace or in a downstream consumer's
/// — reduces every guard built on this function to a pass-through, silently.
/// The workspace declares no `[profile]` section, so the unwinding default
/// applies.
pub fn catch_panic<T, F>(f: F) -> Result<T, InternalPanic>
where
    F: FnOnce() -> T,
{
    #[cfg(target_arch = "wasm32")]
    {
        // Documented pass-through: see the platform caveat above.
        Ok(f())
    }

    #[cfg(not(target_arch = "wasm32"))]
    {
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)).map_err(|payload| {
            match payload_message(payload.as_ref()) {
                Some(message) => InternalPanic::new(message),
                // Not ours to describe, so not ours to contain. See the contract
                // caveat above.
                None => std::panic::resume_unwind(payload),
            }
        })
    }
}

/// The message a panic payload carries, or `None` when it is neither `&str` nor
/// `String` and the guard therefore must not report it as contained.
#[cfg(not(target_arch = "wasm32"))]
fn payload_message(payload: &(dyn std::any::Any + Send)) -> Option<String> {
    if let Some(s) = payload.downcast_ref::<&'static str>() {
        Some((*s).to_string())
    } else {
        payload.downcast_ref::<String>().cloned()
    }
}

/// The comment marker that injects a panic at a guarded call site, for tests.
///
/// No ABL input in this repo panics, so every "the guard contains it" test
/// needs an injected panic. Rather than each client inventing its own hook,
/// guarded entry points call [`panic_if_injected`] with a site name, and a test
/// asks for a panic at that site by putting `OXABL-TEST-PANIC:<site>` in an ABL
/// comment in the source it feeds them:
///
/// ```text
/// /* OXABL-TEST-PANIC:analyze */
/// DEFINE VARIABLE i AS INTEGER NO-UNDO.
/// ```
///
/// Content-triggered rather than a global flag, so the marker targets exactly
/// one file or buffer, needs no cross-process channel, and cannot leak between
/// tests running in parallel.
pub const TEST_PANIC_MARKER: &str = "OXABL-TEST-PANIC";

/// The named panic-injection sites, so a test and a call site cannot disagree
/// about spelling.
pub mod panic_sites {
    /// The shared analyze pipeline (`oxabl::analyze` and its fallible sibling).
    pub const ANALYZE: &str = "analyze";
    /// The shared `format_source` pipeline.
    pub const FORMAT: &str = "format";
    /// The language server's diagnostics query.
    pub const LSP_DIAGNOSTICS: &str = "lsp.diagnostics";
    /// The language server's include-dependency query — a separate site so a
    /// test can prove each guard spans *both* calls, not just the first.
    pub const LSP_DEPENDENCIES: &str = "lsp.dependencies";
    /// The language server's cross-file index extraction. Triggered by a marker
    /// in a *referenced* file rather than in the open buffer, which is what lets
    /// a test prove that a panic while indexing somebody else's file is
    /// contained and reports no answer — instead of committing an empty
    /// diagnostic set for the buffer that named it.
    pub const LSP_INDEX: &str = "lsp.index";
}

/// Panic when `source` carries this `site`'s injection marker.
///
/// A no-op unless `oxabl_common`'s test-only `test-panics` feature is enabled,
/// which only a dev-dependency does — so the marker is inert in any real build.
/// See [`TEST_PANIC_MARKER`].
#[inline]
pub fn panic_if_injected(site: &str, source: &str) {
    #[cfg(feature = "test-panics")]
    if source.contains(&format!("{TEST_PANIC_MARKER}:{site}")) {
        panic!("injected test panic at {site}");
    }

    #[cfg(not(feature = "test-panics"))]
    let _ = (site, source);
}

#[cfg(test)]
mod tests {
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

    #[test]
    fn passes_through_the_closures_value_when_nothing_panics() {
        assert_eq!(catch_panic(|| 7 + 1), Ok(8));
        assert_eq!(catch_panic(|| String::from("ok")).unwrap(), "ok");
    }

    #[test]
    fn catches_a_panic_and_keeps_its_message() {
        let err = quietly(|| catch_panic(|| panic!("boom")).unwrap_err());
        assert_eq!(err.message(), "boom");
        assert!(
            err.to_string().contains("boom"),
            "Display must carry the message, got {err}"
        );
    }

    #[test]
    fn catches_a_formatted_panic_message() {
        let err = quietly(|| catch_panic(|| panic!("boom {}", 42)).unwrap_err());
        assert_eq!(err.message(), "boom 42");
    }

    // A payload the guard cannot describe travels through it intact, rather than
    // being reported as a contained `Err`. This is the mechanism a cancelled
    // recompute relies on to reach the layer that can tell it apart from a bug —
    // proven here with a plain type, so this crate needs no such substrate.
    #[test]
    fn a_non_string_payload_is_re_raised_rather_than_contained() {
        struct NotAString(u32);

        let escaped = quietly(|| {
            std::panic::catch_unwind(|| catch_panic(|| std::panic::panic_any(NotAString(7))))
        });

        let payload = escaped.expect_err("the guard must not contain a payload it cannot describe");
        assert_eq!(
            payload
                .downcast_ref::<NotAString>()
                .expect("the original payload arrives intact")
                .0,
            7,
        );
    }

    #[test]
    fn is_std_error() {
        fn assert_error<E: std::error::Error>(_: &E) {}
        assert_error(&InternalPanic::new("x"));
    }
}
