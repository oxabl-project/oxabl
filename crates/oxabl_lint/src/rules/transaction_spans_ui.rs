//! `transaction-spans-ui` lint (TX003), group `strict.transactions`.
//!
//! Fires on a statement that waits for the user inside a transaction block:
//! `WAIT-FOR`, `UPDATE`, `SET`, `PROMPT-FOR`, `INSERT`, `CHOOSE`, `READKEY`,
//! `PAUSE` with no time, and `MESSAGE` with `VIEW-AS ALERT-BOX` or a `SET` or
//! `UPDATE` phrase.
//!
//! # Why
//!
//! A transaction holds its locks, and keeps its changes uncommitted, until it
//! ends. A statement that waits for the user makes the transaction last as
//! long as the user takes: other sessions wait for the locked records, and a
//! user who walks away holds them for good.
//!
//! ```abl
//! DO TRANSACTION:
//!     FIND FIRST Customer EXCLUSIVE-LOCK.
//!     UPDATE Customer.Name.
//! END.
//! ```
//!
//! Fixed:
//!
//! ```abl
//! DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
//! UPDATE cName.
//! DO FOR Customer TRANSACTION:
//!     FIND FIRST Customer EXCLUSIVE-LOCK.
//!     Customer.Name = cName.
//! END.
//! ```
//!
//! # What is and is not reported
//!
//! One diagnostic per statement, naming the outermost transaction block
//! around it in its routine. A transaction block is one with `TRANSACTION`, or
//! one the compiler makes a transaction by a documented rule (a procedure,
//! `FOR`, `REPEAT` or `DO ON ERROR` that updates a table of the loaded
//! schema directly); see `docs/design/strict-analysis-open-questions.md`,
//! sections 7.3 and 7.8.
//!
//! Not reported:
//!
//! - `PAUSE n` and `READKEY PAUSE n`, which wait a fixed time at most (a
//!   lock-retry loop sleeps with `PAUSE 1`), and `PAUSE ... BEFORE-HIDE`,
//!   which sets an option;
//! - a plain `MESSAGE`, which does not wait;
//! - a statement in a routine (an `ON ... DO` trigger, an internal procedure)
//!   that a transaction calls: only the routine's own blocks count;
//! - a block that is a transaction block only by an inferred rule (the
//!   routine block of a function, method or trigger), only because of a
//!   temp-table update, or only when no schema is loaded;
//! - in an include checked alone, its main block;
//! - a file the scope analysis cannot judge: a recovered parse, an unexpanded
//!   include, or a file past its work budget.
//!
//! In a session with no user (batch, AppServer) these statements do not wait
//! for input, but they still do not belong in a transaction.
//!
//! # Include files
//!
//! A finding in an include is reported in the include when the include alone
//! shows it, else at the `{...}` site in the including file.

use oxabl_ast::{Span, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_lexer::{Kind, Lexer, Token};
use oxabl_scope::FileScopes;
use oxabl_semantic::AnalysisContext;

use super::TX003;
use super::handle_support::{quote, warning};
use super::lock_support::{describe_block, enclosing_transaction, locate, not_judged};
use super::syntactic::{Visitor, source_of, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "transaction-spans-ui";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.transactions";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str = "A statement that waits for the user inside a transaction";

/// Entry point: analyse the file, then [`check`].
pub fn run(
    program: &[Statement],
    _sem: &oxabl_semantic::Semantic,
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    check(&crate::shared::analyze_scopes(program, ctx), program, ctx)
}

/// The findings over an analysed file.
pub fn check(scopes: &FileScopes, program: &[Statement], ctx: &AnalysisContext) -> Vec<Diagnostic> {
    if not_judged(scopes) {
        return Vec::new();
    }
    let fragment = ctx.source_context.is_include_fragment();
    let mut diags = Vec::new();
    for (span, candidate) in candidates(program) {
        let Some((unit, block)) = locate(scopes, span.start) else {
            continue;
        };
        let Some(tx) = enclosing_transaction(scopes, unit, block, fragment) else {
            continue;
        };
        if !candidate.waits(ctx.source, span) {
            continue;
        }
        diags.push(warning(
            ctx,
            TX003,
            format!(
                "`{}` waits for the user inside the transaction of {}",
                quote(ctx.source, span),
                describe_block(ctx.source, unit, tx),
            ),
            "Get the input before the transaction starts, then update in a short transaction \
             block: while the statement waits, every record the transaction locked stays locked."
                .to_string(),
            span,
        ));
    }
    diags
}

/// A statement that may wait for the user.
enum Candidate {
    /// A statement the parser skips, or head-parses without its options
    /// (`UPDATE`, `SET`, `WAIT-FOR`, `PAUSE`): read from its tokens.
    Skipped,
    /// `MESSAGE`, with `SET` or `UPDATE` targets or none.
    Message { set: bool },
}

impl Candidate {
    /// Whether the statement at `span` waits for the user. Lexes the
    /// statement, so it is asked only inside a transaction.
    fn waits(&self, source: &str, span: Span) -> bool {
        match self {
            Candidate::Skipped => skipped_waits(&tokens(source, span)),
            Candidate::Message { set } => {
                *set || tokens(source, span).iter().any(|t| t.kind == Kind::ViewAs)
            }
        }
    }
}

/// The statements that may wait for the user, in source order.
fn candidates(program: &[Statement]) -> Vec<(Span, Candidate)> {
    struct Candidates(Vec<(Span, Candidate)>);
    impl Visitor for Candidates {
        fn statement(&mut self, stmt: &Statement) -> bool {
            let candidate = match &stmt.kind {
                StatementKind::Skipped { .. }
                | StatementKind::Update { .. }
                | StatementKind::Set { .. }
                | StatementKind::WaitFor { .. }
                | StatementKind::Pause { .. } => Candidate::Skipped,
                StatementKind::Message { set_targets, .. } => Candidate::Message {
                    set: !set_targets.is_empty(),
                },
                _ => return true,
            };
            self.0.push((stmt.span, candidate));
            true
        }
    }
    let mut v = Candidates(Vec::new());
    walk_block(&mut v, program);
    v.0.sort_by_key(|(s, _)| s.start);
    v.0
}

/// The tokens of a statement, without comments.
fn tokens(source: &str, span: Span) -> Vec<Token> {
    let text = source_of(source, span).unwrap_or("");
    Lexer::new(text)
        .take_while(|t| t.kind != Kind::Eof)
        .filter(|t| t.kind != Kind::Comment)
        .collect()
}

/// Whether a statement read from its tokens waits for the user.
fn skipped_waits(toks: &[Token]) -> bool {
    let has = |k: Kind| toks.iter().any(|t| t.kind == k);
    match toks.first().map(|t| t.kind) {
        Some(Kind::WaitFor | Kind::PromptFor | Kind::Set | Kind::Choose) => true,
        // `UPDATE t SET f = v` is the SQL statement.
        Some(Kind::Update) => !has(Kind::Set),
        Some(Kind::Insert) => toks.get(1).is_none_or(|t| t.kind != Kind::Into),
        Some(Kind::Readkey) => !has(Kind::Pause),
        // `PAUSE n` sleeps for a time: lock-retry loops use it.
        Some(Kind::Pause) => {
            !has(Kind::BeforeHide)
                && toks.get(1).is_none_or(|t| {
                    matches!(
                        t.kind,
                        Kind::Period | Kind::NoMessage | Kind::Message | Kind::KwIn
                    )
                })
        }
        _ => false,
    }
}
