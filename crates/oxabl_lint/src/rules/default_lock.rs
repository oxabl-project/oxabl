//! `default-lock` lint (LK001), group `strict.locks`.
//!
//! Fires on a `FIND`, a `FOR` or `PRESELECT` record phrase, a `CAN-FIND` or
//! an `OPEN QUERY` record phrase that reads a database table with no lock
//! phrase.
//!
//! # Why
//!
//! A read with no lock phrase takes `SHARE-LOCK`. Other sessions can then
//! read the record but not change it, and the lock is held until the buffer's
//! scope ends or, in a transaction, until the transaction ends. Most reads
//! only need `NO-LOCK`; a read that changes the record needs `EXCLUSIVE-LOCK`,
//! and a `SHARE-LOCK` that is upgraded later can deadlock with another session
//! that does the same. Writing the lock says which one the code means.
//!
//! ```abl
//! FIND FIRST Customer WHERE Customer.CustNum = 1.
//! DISPLAY Customer.Name.
//! ```
//!
//! Fixed:
//!
//! ```abl
//! FIND FIRST Customer WHERE Customer.CustNum = 1 NO-LOCK.
//! DISPLAY Customer.Name.
//! ```
//!
//! # What is and is not reported
//!
//! One diagnostic per record phrase, on the table name. The message names the
//! default, `SHARE-LOCK`. The help suggests `NO-LOCK` only when nothing changes
//! the record later in the buffer's scope; when a change follows, it suggests
//! `EXCLUSIVE-LOCK`. When a call may change the record (the buffer is passed to
//! it, or is shared), or the buffer's scope is not known, the help suggests no
//! lock. No autofix is offered yet.
//!
//! Not reported:
//!
//! - a temp-table, which has no locks;
//! - a table the loaded schema does not have, or any table when no schema is
//!   loaded: the name may be a temp-table that the file does not show;
//! - a `GET`, which takes the lock its query was opened with;
//! - a file the scope analysis cannot judge: a recovered parse, an unexpanded
//!   include, or a file past its work budget;
//! - a read where text the file does not show stands after the record phrase:
//!   an include the preprocessor did not find, or a `{&name}` that expanded
//!   to nothing (in an include checked alone, anywhere in the read). That text
//!   may be the lock phrase.
//!
//! The `-NL` startup option, which changes the default, is not read.
//!
//! # Include files
//!
//! A finding in an include is reported in the include when the include alone
//! shows it, else at the `{...}` site in the including file.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_scope::{FileScopes, Lock, ReadKind, RecordRead, Table, Written};
use oxabl_semantic::AnalysisContext;

use super::LK001;
use super::handle_support::warning;
use super::lock_support::{not_judged, read_form};

/// Kebab-case rule name.
pub const NAME: &str = "default-lock";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.locks";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str = "A database read with no lock phrase, which takes SHARE-LOCK";

/// Entry point: analyse the file, then [`check`].
pub fn run(
    program: &[Statement],
    _sem: &oxabl_semantic::Semantic,
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    check(&crate::shared::analyze_scopes(program, ctx), program, ctx)
}

/// The findings over an analysed file.
pub fn check(
    scopes: &FileScopes,
    _program: &[Statement],
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    if not_judged(scopes) {
        return Vec::new();
    }
    let mut diags = Vec::new();
    for read in scopes.reads() {
        if read.lock != Lock::Default
            || read.kind == ReadKind::Get
            || !matches!(read.table, Table::Database(_))
        {
            continue;
        }
        diags.push(warning(
            ctx,
            LK001,
            format!(
                "{} reads `{}` with no lock phrase, so it takes SHARE-LOCK, the default",
                read_form(read.kind),
                read.name
            ),
            help(scopes, read),
            read.at,
        ));
    }
    diags
}

fn help(scopes: &FileScopes, read: &RecordRead) -> String {
    let buffer = read.buffer.map(|b| scopes.buffer(b));
    let known = buffer.is_none_or(|b| b.scopes().is_some());
    match read.written {
        Written::No if known => {
            "Nothing changes the record later in its scope: add `NO-LOCK`.".to_string()
        }
        Written::Yes if known => "The record is changed later in its scope: add \
                                  `EXCLUSIVE-LOCK`, the lock the change takes."
            .to_string(),
        _ => "Write the lock phrase the read needs, so that it does not take the default."
            .to_string(),
    }
}
