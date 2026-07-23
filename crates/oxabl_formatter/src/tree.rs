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

/// Is this a *self-delimiting block* — a construct that introduces its own
/// indentation level via a dedicated opener and `END` (or equivalent body
/// delimiters)?
///
/// This is the discriminator for prefix-wrapper indentation: a wrapper
/// (`IF … THEN`, `ELSE`, a label, `ON …`) does not add its own level for a
/// branch that is a self-delimiting block, because the block's own `DO:`/`END`
/// already supplies it (`IF x THEN DO:` is one level, not two). It deliberately
/// **excludes** the wrappers/conditionals themselves (`If`, `Label`, `On`,
/// `PreprocIf`): a `THEN`-nested bare `IF` (`IF a THEN IF b THEN …`) has no
/// block to borrow, so it must still indent (Fable finding 1).
fn is_self_delimiting_block(kind: &StatementKind) -> bool {
    matches!(
        kind,
        StatementKind::Block(_)
            | StatementKind::Do { .. }
            | StatementKind::Repeat { .. }
            | StatementKind::ForEach { .. }
            | StatementKind::Procedure { .. }
            | StatementKind::Function { .. }
            | StatementKind::Class { .. }
            | StatementKind::Interface { .. }
            | StatementKind::Method { .. }
            | StatementKind::Constructor { .. }
            | StatementKind::Destructor { .. }
            | StatementKind::Catch { .. }
            | StatementKind::Finally { .. }
            | StatementKind::Property { .. }
            | StatementKind::Case { .. }
    )
}

/// Depth delta for a prefix-wrapper branch: `0` when the branch borrows an
/// existing level, `1` when it needs its own.
///
/// A self-delimiting block borrows its own level (`IF x THEN DO:`). An
/// **else-position** `IF` borrows too, so an else-if chain
/// (`… ELSE IF y THEN DO:`) stays flush with the opening `IF` rather than
/// stair-stepping. Everything else — a leaf branch on its own line, or a
/// **then-position** nested `IF` — needs `+1`.
fn wrapper_child_delta(child: &Statement, is_else: bool) -> usize {
    if is_self_delimiting_block(&child.kind)
        || (is_else && matches!(child.kind, StatementKind::If { .. }))
    {
        0
    } else {
        1
    }
}

/// Like [`block_children`], but pairs each child with the indentation depth
/// delta it should nest by (used only by the printer's depth walk).
///
/// Every child of an ordinary block nests `+1`. The prefix wrappers `If` /
/// `Label` / `On` instead defer to [`wrapper_child_delta`], so a block branch
/// (or an else-if) does not double-indent while a leaf branch still gets its
/// level. Non-wrapper kinds delegate to [`block_children`], so any block kind
/// added there is automatically covered here with the default `+1`.
pub(crate) fn children_with_deltas(kind: &StatementKind) -> Option<Vec<(&Statement, usize)>> {
    let mut out: Vec<(&Statement, usize)> = Vec::new();
    match kind {
        StatementKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            out.push((then_branch, wrapper_child_delta(then_branch, false)));
            if let Some(e) = else_branch {
                out.push((e, wrapper_child_delta(e, true)));
            }
        }
        StatementKind::Label { body, .. } => {
            out.push((body, wrapper_child_delta(body, false)));
        }
        StatementKind::On { kind: on_kind } => {
            let action = on_action(on_kind)?;
            out.push((action, wrapper_child_delta(action, false)));
        }
        _ => {
            for ch in block_children(kind)? {
                out.push((ch, 1));
            }
        }
    }
    // Guarantee source order regardless of field declaration order.
    out.sort_by_key(|(s, _)| s.span.start);
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
