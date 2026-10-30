//! `integrity-bypass` (ABLSEC017), group `security`.
//!
//! Fires where code switches off the checks that keep data consistent, or
//! changes the table of database users. Both are normal in a few places, a
//! load utility or a user-administration program, and a risk everywhere else:
//! a skipped trigger skips the validation and the audit it carries, and a write
//! to `_User` can add a user or change a password.
//!
//! # The set
//!
//! | Form | Reported |
//! |---|---|
//! | `DISABLE TRIGGERS FOR LOAD OF table`, `... FOR DUMP OF table` | the statement |
//! | `CREATE _User`, `DELETE _User` | the statement |
//! | `_User.field = expr`, `ASSIGN _User.field = expr ...` | each assignment |
//!
//! A buffer that is defined `FOR _User` is the same table: `CREATE bUser`,
//! `DELETE bUser` and `bUser.field = expr` are reported too. A read (`FIND`,
//! `FOR EACH`) is not.
//!
//! ```abl
//! DISABLE TRIGGERS FOR LOAD OF Customer.
//! ```
//!
//! Fixed — keep these statements in the programs that need them (a data load,
//! a user-administration tool) and run no other code with triggers off.
//!
//! # The allow-list of programs
//!
//! The rule does not know where it runs. List the programs where the
//! statements are expected as an override of the severity for those paths in
//! `oxabl.toml`:
//!
//! ```toml
//! [[workspace.lint.overrides]]
//! files = ["utils/load/**", "admin/users.p"]
//! ABLSEC017 = "off"
//! ```
//!
//! Every other file keeps the finding.
//!
//! # Where the rule looks
//!
//! `DISABLE TRIGGERS` is not modelled by the parser, so the rule lexes the
//! statement. `CREATE`, `DELETE` and the assignments are modelled. `UPDATE`,
//! `SET`, `IMPORT` and `INSERT` into `_User` are not seen, and neither is a
//! buffer of `_User` that a handle reaches (`CREATE BUFFER ... FOR TABLE
//! "_User"`).

use std::collections::HashSet;

use oxabl_ast::{
    BufferTarget, CreateTarget, Expression, ExpressionKind, Span, Statement, StatementKind,
};
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::calls::{Site, scan};
use super::literal::{StatementText, cover, is_text_read};
use super::{ABLSEC017, finding, meta};
use crate::registry::RuleMeta;
use crate::rules::syntactic::{Visitor, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "integrity-bypass";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// Security severity on the 0.0 to 10.0 scale, as a decimal string.
pub const SECURITY_SEVERITY: &str = "5.5";
/// One-line description for listings.
pub const DESCRIPTION: &str = "DISABLE TRIGGERS, or a write to the _User table";

/// Remediation text, shown with each finding and in rule listings.
pub const HELP: &str = "Keep `DISABLE TRIGGERS` and writes to `_User` in the few programs that need them, \
     such as a data load or a user-administration tool, and list those programs in an \
     `oxabl.toml` override that turns this rule off for their paths. Run no other code with \
     triggers off.";

/// Registry metadata.
pub const META: RuleMeta = meta(
    ABLSEC017,
    NAME,
    DESCRIPTION,
    DEFAULT_SEVERITY,
    SECURITY_SEVERITY,
    HELP,
);

const USER_TABLE: &str = "_User";

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut buffers = UserBuffers::default();
    buffers.names.insert(USER_TABLE.to_ascii_lowercase());
    walk_block(&mut buffers, program);

    let mut diags = Vec::new();
    let mut report = |span: Span, message: String| {
        diags.push(finding(
            ctx,
            DEFAULT_SEVERITY,
            ABLSEC017,
            span,
            message,
            HELP,
        ));
    };
    scan(ctx, program, &["TRIGGERS"], &mut |site| {
        let Site::Statement(stmt) = site else {
            return;
        };
        match &stmt.kind {
            _ if is_text_read(stmt) => {
                if let Some(st) = StatementText::of(ctx.source, stmt)
                    && st.is_word(0, "DISABLE")
                    && st.is_word(1, "TRIGGERS")
                {
                    report(
                        st.span(0..st.len()),
                        "`DISABLE TRIGGERS` skips the validation and audit that triggers carry"
                            .to_string(),
                    );
                }
            }
            StatementKind::Create {
                target: CreateTarget::Name(name),
                ..
            } if buffers.has(&name.name) => {
                report(
                    stmt.span,
                    format!("`CREATE {}` adds a row to the user table", name.name),
                );
            }
            StatementKind::Delete { buffer, .. } if buffers.has(&buffer.name) => {
                report(
                    stmt.span,
                    format!("`DELETE {}` removes a row from the user table", buffer.name),
                );
            }
            StatementKind::Assignment { target, value } => {
                assignment(stmt, target, value, &buffers, &mut report);
            }
            StatementKind::Assign { assignments } => {
                for pair in assignments {
                    assignment(stmt, &pair.target, &pair.value, &buffers, &mut report);
                }
            }
            _ => {}
        }
    });
    diags
}

fn assignment(
    stmt: &Statement,
    target: &Expression,
    value: &Expression,
    buffers: &UserBuffers,
    report: &mut impl FnMut(Span, String),
) {
    if let ExpressionKind::FieldAccess { qualifier, field } = &target.kind
        && let ExpressionKind::Identifier(table) = &qualifier.kind
        && buffers.has(&table.name)
    {
        report(
            cover(target.span, value.span, stmt.span),
            format!(
                "`{}.{}` is written in the user table",
                table.name, field.name
            ),
        );
    }
}

/// `_User` and the buffers that the file defines for it.
#[derive(Default)]
struct UserBuffers {
    names: HashSet<String>,
}

impl UserBuffers {
    fn has(&self, name: &str) -> bool {
        self.names.contains(&name.to_ascii_lowercase())
    }
}

impl Visitor for UserBuffers {
    fn statement(&mut self, stmt: &Statement) -> bool {
        if let StatementKind::DefineBuffer {
            name,
            target: BufferTarget::Table(table),
            ..
        } = &stmt.kind
            && table.name.eq_ignore_ascii_case(USER_TABLE)
        {
            self.names.insert(name.name.to_ascii_lowercase());
        }
        true
    }
}
