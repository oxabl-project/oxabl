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
//! temp-table). The diagnostic's `help` says so, and the finding carries the
//! quick fix `Add NO-UNDO` (`docs/design/quick-fixes.md`): an insertion of
//! ` NO-UNDO` after the last token of the type phrase (the data type keyword,
//! or the field of `LIKE`), or after the name of a temp-table. Add the keyword
//! by hand wherever the program does depend on `UNDO` restoring a value.
//!
//! No fix is made for a shared definition (below), for a temp-table whose
//! statement writes `UNDO`, and when the type phrase is not `AS <data type
//! keyword>` or `LIKE <field>` right after the name. The pipeline drops the fix
//! when the text it changes is not the file's own text.
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

use oxabl_ast::{DataType, Identifier, ParameterType, Span, Statement, StatementKind, TypeSource};
use oxabl_common::{Diagnostic, Edit, Fix, Severity};
use oxabl_lexer::{Kind, Token};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::TX006;
use super::syntactic::{Visitor, file_span, leading_tokens, statement_tokens, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "missing-no-undo";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.transactions";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "a variable, parameter, temp-table or property is defined without NO-UNDO";

/// The help of a plain finding.
pub const HELP: &str = "Add `NO-UNDO` to the definition.";
/// The help of a finding on a `SHARED` or `NEW [GLOBAL] SHARED` definition.
pub const SHARED_HELP: &str = "Add `NO-UNDO` to the `NEW SHARED` producer and to every `SHARED` consumer; \
     changing one side alone makes the definitions disagree.";

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

/// The title of the fix.
pub const FIX_TITLE: &str = "Add NO-UNDO";

/// What the fix inserts after its anchor.
const FIX_TEXT: &str = " NO-UNDO";

/// Where a definition's `NO-UNDO` goes.
enum Insertion<'a> {
    /// After the type phrase that follows the name.
    AfterType {
        name: &'a Identifier,
        type_source: &'a TypeSource,
    },
    /// After the name (a temp-table).
    AfterName(&'a Identifier),
}

/// The offset after the last token of the type phrase right after `name`:
/// `AS <data type keyword>` or `LIKE <field>`. `None` when the tokens are not
/// that shape.
fn after_type(tokens: &[Token], name: &Identifier, type_source: &TypeSource) -> Option<usize> {
    let at = tokens
        .iter()
        .position(|t| t.start == name.span.start as usize && t.end == name.span.end as usize)?;
    let keyword = tokens.get(at + 1)?;
    let ty = tokens.get(at + 2)?;
    match type_source {
        TypeSource::Explicit(data_type) => {
            let plain = !matches!(
                data_type,
                DataType::Class(_) | DataType::Generic(_) | DataType::PreprocIf(_)
            );
            (plain && keyword.kind == Kind::KwAs && ty.kind != Kind::Identifier).then_some(ty.end)
        }
        // A qualified field can be several tokens.
        TypeSource::Like { source } => (keyword.kind == Kind::Like
            && ty.start == source.span.start as usize
            && tokens[at + 2..]
                .iter()
                .any(|t| t.end == source.span.end as usize))
        .then_some(source.span.end as usize),
    }
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
    fn report(&mut self, stmt: &Statement, name: &Identifier, noun: &str, fix: Option<Insertion>) {
        // The AVM expects the producer and its consumers to match, so a shared
        // definition has no fix.
        let help = if fix.is_some() { HELP } else { SHARED_HELP };
        let mut diagnostic = Diagnostic::warning(
            TX006,
            format!("{noun} `{}` is defined without NO-UNDO", name.name),
            file_span(self.ctx, name.span),
        )
        .with_help(help.to_string());
        if let Some(fix) = fix.and_then(|insertion| self.fix(stmt, insertion)) {
            diagnostic = diagnostic.with_fix(fix);
        }
        self.diags.push(diagnostic);
    }

    /// The `Add NO-UNDO` fix, or `None` when the place is not certain.
    fn fix(&self, stmt: &Statement, insertion: Insertion) -> Option<Fix> {
        let tokens = statement_tokens(self.ctx.source, stmt.span);
        let at = match insertion {
            Insertion::AfterType { name, type_source } => after_type(&tokens, name, type_source)?,
            Insertion::AfterName(name) => {
                // A header that writes `UNDO` asks for undo.
                if tokens.iter().any(|t| t.kind == Kind::Undo) {
                    return None;
                }
                tokens
                    .iter()
                    .find(|t| {
                        t.start == name.span.start as usize && t.end == name.span.end as usize
                    })?
                    .end
            }
        };
        let at = at as u32;
        Some(Fix::new(
            FIX_TITLE,
            Edit {
                span: file_span(self.ctx, Span { start: at, end: at }),
                new_text: FIX_TEXT.to_string(),
                statement: file_span(self.ctx, stmt.span),
            },
            true,
        ))
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
        let type_source = TypeSource::Explicit(data_type.clone());
        let fix = self.fix(
            stmt,
            Insertion::AfterType {
                name,
                type_source: &type_source,
            },
        );
        let mut diagnostic = Diagnostic::warning(
            TX006,
            format!("property `{}` is defined without NO-UNDO", name.name),
            file_span(self.ctx, name.span),
        )
        .with_help(HELP.to_string());
        if let Some(fix) = fix {
            diagnostic = diagnostic.with_fix(fix);
        }
        self.diags.push(diagnostic);
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
            } if !type_is_exempt(type_source) => {
                let shared = *is_new_shared || *is_shared || *is_new_global_shared;
                let fix = (!shared).then_some(Insertion::AfterType { name, type_source });
                self.report(stmt, name, "variable", fix);
            }
            StatementKind::DefineTempTable {
                name,
                no_undo: false,
                is_new_shared,
                is_shared,
                is_new_global_shared,
                ..
            } => {
                let shared = *is_new_shared || *is_shared || *is_new_global_shared;
                let fix = (!shared).then_some(Insertion::AfterName(name));
                self.report(stmt, name, "temp-table", fix);
            }
            StatementKind::DefineParameter {
                param_type:
                    ParameterType::Variable {
                        name,
                        type_source,
                        no_undo: false,
                        ..
                    },
                ..
            } if !type_is_exempt(type_source) => {
                // A signature parameter is stored as the same statement but
                // has no `DEFINE`, and no place to write `NO-UNDO`.
                if self.leading(stmt).first() == Some(&Kind::Define) {
                    let fix = Insertion::AfterType { name, type_source };
                    self.report(stmt, name, "parameter", Some(fix));
                }
            }
            StatementKind::Property { .. } => self.property(stmt),
            _ => {}
        }
        true
    }
}
