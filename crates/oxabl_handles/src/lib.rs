//! Lifetimes of dynamic objects in Progress ABL.
//!
//! [`analyze_file`] finds every dynamic object a file creates and follows it
//! through its unit's control-flow graph (`oxabl_flow`): where it is freed,
//! where it escapes to an owner outside the unit, and on which paths it is
//! lost. The `strict.handles` lint rules (HD001-HD005) and procedure summaries
//! read this model and compute nothing again.
//!
//! # Objects
//!
//! An [`Object`] is one create site:
//!
//! * a handle-based object, widget or COM object: `CREATE QUERY`, `BUFFER`,
//!   `TEMP-TABLE`, `DATASET`, `DATA-SOURCE`, `CALL`, `X-DOCUMENT`, `BUTTON`,
//!   `CREATE "Excel.Application"`, `CREATE VALUE(...)`, ... ([`ObjectKind`]);
//! * a persistent procedure: `RUN p PERSISTENT SET h`. Without `SET` no handle
//!   is kept, and the verdict is [`Verdict::Uncaptured`];
//! * a MEMPTR: `SET-SIZE(m) = n`, where `n` is not the literal 0, or
//!   `COPY-LOB ... TO m` for a variable declared `AS MEMPTR`;
//! * a class instance (`NEW`, `DYNAMIC-NEW`). The AVM counts references to
//!   these and frees them, so they get [`Verdict::Collected`] and are not
//!   followed. Reference cycles are out of scope.
//!
//! # Frees, escapes, leaks
//!
//! The analysis follows each object from its create node over every path of
//! the unit's CFG, raised conditions and `FINALLY` copies included. A path
//! keeps the set of the unit's variables that hold the object, so `h2 = h`
//! makes either a way to free it, and overwriting the last holder (a new
//! `CREATE` into it, `h = ?`) loses the object there ([`LeakKind::Overwrite`]).
//! A create in a loop whose object is still live on the next iteration is lost
//! at that create. A statement that raises does not complete, so a create that
//! raises makes nothing and an assignment that raises stores nothing.
//!
//! A path ends when:
//!
//! * a [`Free`] runs on a holder: `DELETE OBJECT`, `DELETE PROCEDURE`,
//!   `DELETE WIDGET`, `RELEASE OBJECT`, `SET-SIZE(m) = 0`, `APPLY "CLOSE"` (a
//!   persistent procedure deletes itself on CLOSE by convention; no other
//!   kind of object is freed by it), or
//!   `DELETE WIDGET-POOL` of the pool the object is in;
//! * the unit exits with the object live. If it escaped on that path, the
//!   object [`Verdict::Escapes`]; if it sits in a widget pool this procedure
//!   deletes, the pool frees it ([`Verdict::PoolFreed`]); otherwise it leaks
//!   ([`LeakKind::Exit`]).
//!
//! An `IF VALID-HANDLE(h) THEN` (also `NOT`, `h <> ?`, `h = ?`) does not go
//! down the branch where `h` is invalid while `h` holds the object: that path
//! cannot happen, so `IF VALID-HANDLE(h) THEN DELETE OBJECT h.` frees on
//! every path.
//!
//! An [`Escape`] is a statement that hands the object to an owner the unit
//! cannot see ([`EscapeKind`]): an `OUTPUT` parameter, `RETURN h`, a class
//! member, a variable of an enclosing unit, a shared variable, a field, an
//! array element, another object's attribute, a parent widget (`FRAME`,
//! `PARENT`), `ADD-SUPER-PROCEDURE`, a constructor argument, an
//! `INPUT-OUTPUT` argument, a `PUBLISH` argument or a `SUBSCRIBE PROCEDURE`. A handle derived
//! from the object (`h:DEFAULT-BUFFER-HANDLE`, `h:GET-BUFFER-HANDLE(n)`) that is returned or
//! stored in such a place hands the object over too ([`EscapeKind::DerivedHandle`]). An `INPUT` argument
//! of `RUN`, a function or a method is passed to a callee. If the file defines
//! the callee and it frees that parameter, the object escapes
//! ([`EscapeKind::FreedByCallee`]). If the file defines it and it does not, or
//! the callee is a built-in, the callee borrows the handle: a use. Any other
//! callee is [`OpaqueReason::Callee`].
//!
//! Each [`Leak`] has its path. A path is *normal* when it raises no condition
//! and takes no `THROW` or `RETURN ERROR`. An object whose every leak is not
//! normal is freed on the success path only; that is the case for `FINALLY`.
//!
//! # Opaque
//!
//! When something the analysis cannot see could free or keep the object, the
//! object gets an [`Opaque`] reason, and a would-be leak becomes
//! [`Verdict::Unknown`], never [`Verdict::Leaks`]: a statement the parser
//! skips that names a holder, a dynamic call with a holder as argument
//! (`RUN VALUE`, `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`), an unexpanded include
//! or a statement left empty by parse recovery while the object lives, or
//! `RUN destroy... IN h`, a MEMPTR passed `OUTPUT`, a handle passed to a callee
//! the file does not define, a delete of an operand that is not the holder
//! itself, or a walk that runs past its work budget (one object, or the file).
//!
//! # Widget pools
//!
//! Handle-based objects and widgets go into a widget pool
//! ([`ObjectKind::uses_widget_pool`]). [`Placement`] says which: a named pool
//! from `IN WIDGET-POOL`, the unnamed pool of an earlier `CREATE WIDGET-POOL`
//! of the unit (it must dominate the create), the main block's unnamed pool
//! for a nested unit, the pool of a class with `USE-WIDGET-POOL`, or
//! [`Placement::Unscoped`]: the caller's pool or the
//! session pool, which this file cannot know (HD005).
//!
//! # Units and includes
//!
//! The work is per unit: only the units that create something are built, one
//! at a time ([`oxabl_flow::UnitBuilder`]). Names resolve against the unit's
//! own definitions, then its enclosing units', then the file level (class
//! members). The file-level facts are one walk of the statements: which units
//! free a name, and which named pools the file creates. An object created in
//! the main block and freed by name in another unit (a cleanup procedure or a
//! trigger) escapes to that unit.
//!
//! Spans are in the text the statements were parsed from. Analyse the
//! expanded text, as `oxabl check` does, and map spans back through the
//! expansion's source map to report at the include or at its use site.
//!
//! The ABL semantics this relies on, with confidence marks, are in
//! `docs/design/strict-analysis-open-questions.md`, section 6.

mod analysis;
mod dump;
mod events;
mod index;
mod model;

pub use model::{
    Escape, EscapeKind, Free, FreeKind, Leak, LeakKind, Object, ObjectKind, Opaque, OpaqueReason,
    Placement, PoolSite, Site, Step, Summary, UnitHandles, Verdict,
};

use oxabl_ast::Statement;
use oxabl_flow::{FlowOptions, UnitBuilder, UnitId};

/// The dynamic objects of one file, by unit.
#[derive(Debug, Clone, Default)]
pub struct FileHandles {
    units: Vec<UnitHandles>,
}

impl FileHandles {
    /// The units that create something, in source pre-order.
    pub fn units(&self) -> &[UnitHandles] {
        &self.units
    }

    /// One unit, if it creates something.
    pub fn unit(&self, id: UnitId) -> Option<&UnitHandles> {
        self.units.iter().find(|u| u.unit == id)
    }

    /// Every object with its unit.
    pub fn objects(&self) -> impl Iterator<Item = (&UnitHandles, &Object)> {
        self.units
            .iter()
            .flat_map(|u| u.objects.iter().map(move |o| (u, o)))
    }

    /// A stable text rendering, for snapshot tests and debugging. `source` is
    /// the text that was analysed; sites show as `@line`.
    pub fn dump(&self, source: &str) -> String {
        dump::dump(self, source)
    }
}

/// Analyse a parsed file. `source` must be the text the statements were
/// parsed from (the expanded text when includes were expanded).
pub fn analyze_file(program: &[Statement], source: &str) -> FileHandles {
    analyze_file_with(program, source, &FlowOptions::default())
}

/// [`analyze_file`] with the flow options (the default error directive).
pub fn analyze_file_with(
    program: &[Statement],
    source: &str,
    options: &FlowOptions,
) -> FileHandles {
    let index = index::FileIndex::build(program, source);
    if index.creating.is_empty() {
        return FileHandles::default();
    }
    let builder = UnitBuilder::new(program, source, options);
    let mut units: Vec<UnitId> = index
        .creating
        .iter()
        .filter_map(|&s| builder.unit_of(s))
        .collect();
    units.sort_unstable();
    units.dedup();
    let budget = analysis::Budget::new();
    let units = units
        .into_iter()
        .map(|u| analysis::analyze_unit(&builder.build(u), &index, source, &budget))
        .filter(|u| !u.objects.is_empty() || !u.pools.is_empty())
        .collect();
    FileHandles { units }
}
