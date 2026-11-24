//! `weak-crypto` (ABLSEC009), group `security`.
//!
//! Fires where a password or token is protected with a weak method, and where
//! `RANDOM` makes a value that must not be guessed. This is a heuristic rule:
//! it reads the *names* of the values, because stage 1 has no data flow, so it
//! is in the `warning` class and not `error`.
//!
//! # The set
//!
//! | Form | Reported when |
//! |---|---|
//! | `ENCODE(x)`, `MD5-DIGEST(x)`, `SHA1-DIGEST(x)` | `x` mentions a password-like or token-like name |
//! | `MESSAGE-DIGEST("MD5" or "SHA-1", x)` | `x` mentions a password-like or token-like name |
//! | `name = ... RANDOM(...) ...` | `name` looks like a token, nonce, secret or identifier |
//!
//! ```abl
//! cHash = MD5-DIGEST(pcPassword).
//! cToken = STRING(RANDOM(1, 999999)).
//! ```
//!
//! `MD5-DIGEST` and `SHA1-DIGEST` are fast and unsalted: a leaked hash is
//! cracked with a table. `ENCODE` is a one-way scramble that has no salt and is
//! not a modern password hash. `RANDOM` is a pseudo-random generator with a
//! small state: the next value can be predicted from a few outputs.
//!
//! Fixed — a random key for tokens and nonces, and a salted slow hash for
//! passwords:
//!
//! ```abl
//! cToken = BASE64-ENCODE(GENERATE-RANDOM-KEY).
//! rKey = GENERATE-PBE-KEY(pcPassword, rSalt, 100000).
//! ```
//!
//! # Precision
//!
//! The name lists are those of [`Heuristics`](super::names::Heuristics). A
//! name is split into words (`cResetToken` is `c`, `reset`, `token`), so
//! `cPassenger` is not password-like, and `iDice = RANDOM(1, 6)` is not
//! reported. The rule misses a hash of a password that has a neutral name
//! (`cInput`), and it reports a digest of a token that is not secret, such as
//! a cache key named `cTokenCacheKey`. A `RANDOM` that feeds an identifier is
//! reported even where the identifier is only a display number; the name list
//! for identifiers is `id`, `uid`, `guid`, `uuid` and `sid`. Code that embeds
//! the linter can pass its own lists to [`run_with`].
//!
//! # Where the rule looks
//!
//! Calls in modelled expressions, and by lexing, calls in unmodelled
//! statements; `RANDOM` in the value of an assignment, an `ASSIGN` pair or the
//! `INITIAL` value of a variable. A `RANDOM` whose result goes through a
//! procedure parameter to a token is not seen.

use oxabl_ast::{Expression, ExpressionKind, Span, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::calls::{Call, Site, any_expr, last_name, scan};
use super::literal::cover;
use super::names::{Heuristics, defaults};
use super::{ABLSEC009, finding, meta};
use crate::registry::RuleMeta;

/// Kebab-case rule name.
pub const NAME: &str = "weak-crypto";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// Security severity on the 0.0 to 10.0 scale, as a decimal string.
pub const SECURITY_SEVERITY: &str = "8.0";
/// One-line description for listings.
pub const DESCRIPTION: &str = "a password or token is hashed with ENCODE, MD5 or SHA-1, or RANDOM makes a token or identifier";

/// Remediation text, shown with each finding and in rule listings.
pub const HELP: &str = "Make tokens, nonces and identifiers that must not be guessed with \
     `GENERATE-RANDOM-KEY`, not `RANDOM`. Store a password as a salted slow hash, such as \
     `GENERATE-PBE-KEY` with a random salt and many iterations, not with `ENCODE`, `MD5-DIGEST` \
     or `SHA1-DIGEST`. This check reads names, so it can report a value that is not secret.";

/// Registry metadata.
pub const META: RuleMeta = meta(
    ABLSEC009,
    NAME,
    DESCRIPTION,
    DEFAULT_SEVERITY,
    SECURITY_SEVERITY,
    HELP,
);

/// Functions that make a fast, unsalted hash.
const HASHES: [&str; 3] = ["ENCODE", "MD5-DIGEST", "SHA1-DIGEST"];

/// Algorithms of `MESSAGE-DIGEST` that are weak.
const WEAK_ALGORITHMS: [&str; 3] = ["MD5", "SHA-1", "SHA1"];

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    run_with(program, ctx, defaults())
}

/// Entry point with the word lists of `heuristics`.
pub fn run_with(
    program: &[Statement],
    ctx: &AnalysisContext,
    heuristics: &Heuristics,
) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let mut report = |span: Span, message: String| {
        diags.push(finding(
            ctx,
            DEFAULT_SEVERITY,
            ABLSEC009,
            span,
            message,
            HELP,
        ));
    };
    scan(
        ctx,
        program,
        &["ENCODE", "DIGEST"],
        &mut |site| match site {
            Site::Statement(stmt) => match &stmt.kind {
                StatementKind::Assignment { target, value } => {
                    random(stmt, target, value, heuristics, &mut report);
                }
                StatementKind::Assign { assignments } => {
                    for pair in assignments {
                        random(stmt, &pair.target, &pair.value, heuristics, &mut report);
                    }
                }
                _ => {}
            },
            Site::Call(call) => {
                if let Some(what) = weak_hash(&call, heuristics) {
                    report(
                        call.span,
                        format!("`{what}` protects a password or token with a weak, unsalted hash"),
                    );
                }
            }
        },
    );
    diags
}

/// The name to show when `call` hashes a password or token weakly.
fn weak_hash(call: &Call, heuristics: &Heuristics) -> Option<String> {
    let secret = |args: &[super::calls::Arg]| {
        args.iter()
            .flat_map(|a| a.names.iter())
            .any(|n| heuristics.is_secret_or_token(n))
    };
    if let Some(name) = HASHES.iter().find(|h| call.is(h)) {
        return secret(&call.args).then(|| (*name).to_string());
    }
    if call.is("MESSAGE-DIGEST") {
        let weak = call
            .args
            .first()
            .and_then(|a| a.literal.as_deref())
            .is_some_and(|alg| WEAK_ALGORITHMS.iter().any(|w| w.eq_ignore_ascii_case(alg)));
        return (weak && secret(call.args.get(1..).unwrap_or_default()))
            .then(|| "MESSAGE-DIGEST".to_string());
    }
    None
}

/// Report `target = ...RANDOM(...)...` when `target` must not be guessable.
fn random(
    stmt: &Statement,
    target: &Expression,
    value: &Expression,
    heuristics: &Heuristics,
    report: &mut impl FnMut(Span, String),
) {
    let Some(name) = last_name(target) else {
        return;
    };
    if heuristics.is_unguessable(name)
        && any_expr(value, &mut |e| {
            matches!(&e.kind, ExpressionKind::FunctionCall { name, .. }
                if name.name.eq_ignore_ascii_case("RANDOM"))
        })
    {
        report(
            cover(target.span, value.span, stmt.span),
            format!("`RANDOM` is predictable and makes the value of `{name}`"),
        );
    }
}
