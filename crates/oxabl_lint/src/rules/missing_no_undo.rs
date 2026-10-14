//! `missing-no-undo` lint (TX006), group `strict.transactions`.
//!
//! Fires on a variable, parameter, temp-table or property that is defined
//! without `NO-UNDO`.
//!
//! # Why
//!
//! Without `NO-UNDO`, the AVM keeps undo information for the object, so that an
//! `UNDO` can restore its old value. That costs work on every change inside a
//! transaction, and an `UNDO` silently rolls back state that was only ever
//! scratch data. For a temp-table the cost is per record. Almost no code wants
//! either, and the keyword costs nothing to add.
//!
//! ```abl
//! DEFINE VARIABLE iCount AS INTEGER.
//! DEFINE TEMP-TABLE ttItem
//!     FIELD ItemNum AS INTEGER.
//! ```
//!
//! Fixed:
//!
//! ```abl
//! DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
//! DEFINE TEMP-TABLE ttItem NO-UNDO
//!     FIELD ItemNum AS INTEGER.
//! ```
//!
//! The fix is to add `NO-UNDO` after the type (after the name, for a
//! temp-table). The diagnostic's `help` says so; an autofix is planned and
//! will need no analysis beyond what this rule already does, except for a
//! shared definition (below). Add the keyword by hand wherever the program
//! does depend on `UNDO` restoring a value.
//!
//! # Shared definitions
//!
//! A `NEW SHARED` or `NEW GLOBAL SHARED` definition and each `SHARED`
//! definition that binds to it live in different files. The AVM requires the
//! two to agree, and it is assumed (recalled, not checked against an install;
//! see the open-questions note) that `NO-UNDO` is part of what must agree. Adding
//! `NO-UNDO` on one side alone would then turn a warning into a compile or
//! run-time mismatch. So for a shared definition:
//!
//! - the `help` says to add `NO-UNDO` to the producer and every consumer;
//! - the finding is **not autofixable**: a fix would have to edit every file
//!   that consumes the producer, and this rule sees one file;
//! - the finding is still reported, on the producer and on each consumer.
//!
//! # What is reported
//!
//! | Form | Reported when |
//! |---|---|
//! | `DEFINE VARIABLE`, `DEFINE VAR` | no `NO-UNDO` |
//! | `DEFINE ... PARAMETER name AS/LIKE ...` | no `NO-UNDO`; every direction |
//! | `DEFINE TEMP-TABLE` | no `NO-UNDO` |
//! | `DEFINE PROPERTY` | no `NO-UNDO` and every accessor is an auto accessor (`GET.`, `SET.`) |
//!
//! # What is not, and why
//!
//! - **The `VAR` statement** (`VAR INTEGER x.`) is `NO-UNDO` by definition, so
//!   there is nothing to write.
//! - **Parameters in a signature**: `METHOD`, `CONSTRUCTOR`, `FUNCTION`,
//!   `DEFINE EVENT` and property `SET` parameters, written `(INPUT x AS
//!   INTEGER)`. The syntax has no place for `NO-UNDO`. Only the
//!   `DEFINE ... PARAMETER` statement is reported. The parser stores a
//!   `FUNCTION` signature as statements in the function body, so the rule
//!   tells the two apart by the statement starting with `DEFINE`.
//! - **`TABLE`, `TABLE-HANDLE`, `DATASET`, `DATASET-HANDLE` and `BUFFER`
//!   parameters**, and `DEFINE BUFFER`, `DATASET` and the other non-variable
//!   definitions: they take no `NO-UNDO`.
//! - **A property that is not a plain auto property**: one with a computed
//!   accessor (`GET: ... END GET.` or `SET: ... END SET.`), an `ABSTRACT`
//!   property, and any property declared in an `INTERFACE`. A computed
//!   accessor may keep the value elsewhere, an abstract or interface property
//!   keeps nothing, and the rule does not guess whether `NO-UNDO` applies.
//! - **Class-typed definitions**: variables, parameters and properties whose
//!   type is a class (`AS Foo.Bar`, `AS CLASS Foo`). The rule does not claim
//!   `NO-UNDO` matters for object references. Conditional types
//!   (`&IF ... &THEN INTEGER &ELSE ...`) are also left alone, since the type
//!   is not known.
//! - **`WORK-TABLE` and `WORKFILE`**, which are never undo-able.
//!
//! # Gaps
//!
//! `NO-UNDO` written in an unusual place may not be seen: the parser reads it
//! after the name, after the type, and after the `VIEW-AS`/`SIZE` options of a
//! variable, but not after the clauses of a temp-table. A temp-table
//! `REFERENCE-ONLY` is not distinguished. A property is judged to have storage
//! when either accessor is an auto accessor (`GET.` / `SET.`); the parser
//! stores an empty computed body (`GET: END GET.`) the same way.

use oxabl_ast::{DataType, Identifier, ParameterType, Statement, StatementKind, TypeSource};
use oxabl_common::{Diagnostic, Severity};
use oxabl_lexer::Kind;
use oxabl_semantic::{AnalysisContext, Semantic};

use super::TX006;
use super::syntactic::{Visitor, file_span, leading_tokens, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "missing-no-undo";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.transactions";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "a variable, parameter, temp-table or property is defined without NO-UNDO";

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut v = Definitions {
        ctx,
        diags: Vec::new(),
    };
    walk_block(&mut v, program);
    v.diags
}

struct Definitions<'a> {
    ctx: &'a AnalysisContext<'a>,
    diags: Vec<Diagnostic>,
}

/// Whether the type is one the rule does not judge: a class, or a type that
/// depends on the preprocessor.
fn type_is_exempt(type_source: &TypeSource) -> bool {
    matches!(
        type_source,
        TypeSource::Explicit(DataType::Class(_) | DataType::PreprocIf(_))
    )
}

impl Definitions<'_> {
    fn report(&mut self, name: &Identifier, noun: &str, shared: bool) {
        let help = if shared {
            // The AVM expects the producer and its consumers to match.
            "Add `NO-UNDO` to the `NEW SHARED` producer and to every `SHARED` consumer; \
             changing one side alone makes the definitions disagree."
        } else {
            "Add `NO-UNDO` to the definition."
        };
        self.diags.push(
            Diagnostic::warning(
                TX006,
                format!("{noun} `{}` is defined without NO-UNDO", name.name),
                file_span(self.ctx, name.span),
            )
            .with_help(help.to_string()),
        );
    }

    /// The statement's leading tokens, up to and including the keyword that
    /// names what is being defined.
    fn leading(&self, stmt: &Statement) -> Vec<Kind> {
        leading_tokens(self.ctx.source, stmt.span, 12)
            .into_iter()
            .map(|(t, _)| t.kind)
            .collect()
    }

    fn property(&mut self, stmt: &Statement) {
        let StatementKind::Property {
            name,
            data_type,
            no_undo: false,
            get_body,
            set_body,
            ..
        } = &stmt.kind
        else {
            return;
        };
        if matches!(data_type, DataType::Class(_) | DataType::PreprocIf(_)) {
            return;
        }
        // An auto property has a backing variable. With a computed accessor
        // the rule cannot tell what the property is stored in, so it stays out.
        let mut accessors = [get_body, set_body].into_iter().flatten().peekable();
        if accessors.peek().is_none() || accessors.any(|body| !body.is_empty()) {
            return;
        }
        let kinds = self.leading(stmt);
        let modifiers = kinds.split(|k| *k == Kind::Property).next().unwrap_or(&[]);
        if modifiers.contains(&Kind::Abstract) {
            return;
        }
        self.report(name, "property", false);
    }
}

impl Visitor for Definitions<'_> {
    fn statement(&mut self, stmt: &Statement) -> bool {
        match &stmt.kind {
            // Nothing declared in an interface has storage.
            StatementKind::Interface { .. } => return false,
            StatementKind::VariableDeclaration {
                name,
                type_source,
                no_undo: false,
                is_new_shared,
                is_shared,
                is_new_global_shared,
                ..
            } if !type_is_exempt(type_source) => self.report(
                name,
                "variable",
                *is_new_shared || *is_shared || *is_new_global_shared,
            ),
            StatementKind::DefineTempTable {
                name,
                no_undo: false,
                is_new_shared,
                is_shared,
                is_new_global_shared,
                ..
            } => self.report(
                name,
                "temp-table",
                *is_new_shared || *is_shared || *is_new_global_shared,
            ),
            StatementKind::DefineParameter {
                param_type:
                    ParameterType::Variable {
                        name,
                        type_source,
                        no_undo: false,
                    },
                ..
            } if !type_is_exempt(type_source) => {
                // A signature parameter is stored as the same statement but
                // has no `DEFINE`, and no place to write `NO-UNDO`.
                if self.leading(stmt).first() == Some(&Kind::Define) {
                    self.report(name, "parameter", false);
                }
            }
            StatementKind::Property { .. } => self.property(stmt),
            _ => {}
        }
        true
    }
}
