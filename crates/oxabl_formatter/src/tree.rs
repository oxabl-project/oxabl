//! Shared AST-shape helpers used by both attachment (U2) and the printer (U4):
//! enumerating a statement's direct child statements, and identifying the type
//! keyword a block's `END` takes under `end_with_type`.

use oxabl_ast::{OnAction, OnKind, Statement, StatementKind};

/// Return the direct child statements of a block-bearing statement in source
/// order, or `None` for a leaf (non-block) statement.
///
/// `Some(vec![])` is a block with an empty body (e.g. `DO: END.`) — distinct
/// from a leaf, because comments inside it are *dangling*, not *interior*, and
/// the printer treats it as a nesting level.
pub(crate) fn block_children(kind: &StatementKind) -> Option<Vec<&Statement>> {
    let mut out: Vec<&Statement> = Vec::new();
    match kind {
        StatementKind::Block(body)
        | StatementKind::Do { body, .. }
        | StatementKind::Repeat { body, .. }
        | StatementKind::ForEach { body, .. }
        | StatementKind::Procedure { body, .. }
        | StatementKind::Function { body, .. }
        | StatementKind::Class { body, .. }
        | StatementKind::Interface { body, .. }
        | StatementKind::Catch { body, .. }
        | StatementKind::Finally { body }
        | StatementKind::Destructor { body } => {
            out.extend(body.iter());
        }
        StatementKind::Method {
            parameters, body, ..
        }
        | StatementKind::Constructor {
            parameters, body, ..
        } => {
            out.extend(parameters.iter());
            out.extend(body.iter());
        }
        StatementKind::Property {
            get_body,
            set_body,
            set_parameters,
            ..
        } => {
            let mut any = false;
            if let Some(b) = get_body {
                any = true;
                out.extend(b.iter());
            }
            out.extend(set_parameters.iter());
            if let Some(b) = set_body {
                any = true;
                out.extend(b.iter());
            }
            if !any {
                return None;
            }
        }
        StatementKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            out.push(then_branch);
            if let Some(e) = else_branch {
                out.push(e);
            }
        }
        StatementKind::Case {
            when_branches,
            otherwise,
            ..
        } => {
            for w in when_branches {
                out.extend(w.body.iter());
            }
            if let Some(o) = otherwise {
                out.extend(o.iter());
            }
        }
        StatementKind::Label { body, .. } => {
            out.push(body);
        }
        StatementKind::On { kind: on_kind } => out.push(on_action(on_kind)?),
        StatementKind::PreprocIf(p) => {
            out.extend(p.then_branch.iter());
            for (_, branch) in &p.elseif_branches {
                out.extend(branch.iter());
            }
            if let Some(e) = &p.else_branch {
                out.extend(e.iter());
            }
        }
        _ => return None,
    }
    // Guarantee source order regardless of field declaration order.
    out.sort_by_key(|s| s.span.start);
    Some(out)
}

/// The trigger-block statement of an `ON` statement, if it has one.
fn on_action(on_kind: &OnKind) -> Option<&Statement> {
    let action = match on_kind {
        OnKind::UiEvent { action, .. } | OnKind::DbEvent { action, .. } => action,
        OnKind::KeyRemap { .. } => return None,
    };
    match action {
        OnAction::Block(stmt) => Some(stmt),
        OnAction::Revert | OnAction::PersistentRun { .. } => None,
    }
}

/// The type keyword a block's closing `END` takes under `end_with_type: true`
/// (R4.2 / KTD4), or `None` for a bare-`END` block (DO / FOR / REPEAT / plain
/// block) or a non-block statement.
///
/// The keyword is returned uppercase; the keyword-case pass (U5) re-cases it to
/// match the resolved `StyleGuide` afterward, so the two stay consistent.
pub(crate) fn typed_end_keyword(kind: &StatementKind) -> Option<&'static str> {
    match kind {
        StatementKind::Procedure { .. } => Some("PROCEDURE"),
        StatementKind::Function { .. } => Some("FUNCTION"),
        StatementKind::Class { .. } => Some("CLASS"),
        StatementKind::Method { .. } => Some("METHOD"),
        StatementKind::Constructor { .. } => Some("CONSTRUCTOR"),
        StatementKind::Destructor { .. } => Some("DESTRUCTOR"),
        StatementKind::Interface { .. } => Some("INTERFACE"),
        StatementKind::Case { .. } => Some("CASE"),
        _ => None,
    }
}
