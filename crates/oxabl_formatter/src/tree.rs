//! Shared AST-shape helpers used by both attachment (U2) and the printer (U4):
//! enumerating a statement's direct child statements, and identifying the type
//! keyword a block's `END` takes under `end_with_type`.

use oxabl_ast::{OnAction, OnKind, Span, Statement, StatementKind};
use oxabl_lexer::{Kind, Token};

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
        | StatementKind::Class { body, .. }
        | StatementKind::Interface { body, .. }
        | StatementKind::Catch { body, .. }
        | StatementKind::Finally { body }
        | StatementKind::Destructor { body } => {
            out.extend(body.iter());
        }
        StatementKind::Method { body, .. } | StatementKind::Constructor { body, .. } => {
            out.extend(body.iter());
        }
        // The parser prepends the signature parameters to a function body.
        // They belong to the header, and a function body defines no parameter.
        StatementKind::Function { body, .. } => {
            out.extend(
                body.iter()
                    .skip_while(|s| matches!(s.kind, StatementKind::DefineParameter { .. })),
            );
        }
        StatementKind::Property {
            get_body, set_body, ..
        } => {
            let mut any = false;
            if let Some(b) = get_body {
                any = true;
                out.extend(b.iter());
            }
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
    // Statements with no source extent own no line, and their `0..0` span would
    // sort them first and make them claim line 0.
    out.retain(|s| s.span != Span::DUMMY);
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

/// Is this a *prefix wrapper* — a statement that prefixes a single branch
/// statement (`IF … THEN`, `ELSE`, a label, `ON …`) but has **no structural
/// closing line of its own**?
///
/// This is the mirror of [`is_self_delimiting_block`]: a real block owns an
/// `END` (or equivalent) that must snap to the block's depth, but a prefix
/// wrapper contributes no closer — its "last line" is just its branch's last
/// physical line, which must keep its normal continuation indent. `PreprocIf`
/// is deliberately **not** a prefix wrapper here: it owns `&ENDIF`, whose line
/// is a genuine closer that must keep snapping (KTD1).
pub(crate) fn is_prefix_wrapper(kind: &StatementKind) -> bool {
    matches!(
        kind,
        StatementKind::If { .. } | StatementKind::Label { .. } | StatementKind::On { .. }
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
/// level. A `CASE` branch body behaves like a wrapper branch: `WHEN … THEN` and
/// `OTHERWISE` prefix their statement, so one that starts on a later line
/// (`starts_own_line`) nests one level past the `WHEN`, unless it is a block
/// that supplies its own level. Other kinds delegate to [`block_children`], so
/// any block kind added there is automatically covered here with the default
/// `+1`.
pub(crate) fn children_with_deltas<'a>(
    kind: &'a StatementKind,
    starts_own_line: &dyn Fn(&Statement) -> bool,
) -> Option<Vec<(&'a Statement, usize)>> {
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
        StatementKind::Case { .. } => {
            for ch in block_children(kind)? {
                let own_line = starts_own_line(ch) && !is_self_delimiting_block(&ch.kind);
                out.push((ch, 1 + usize::from(own_line)));
            }
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

/// A computed `GET` or `SET` accessor of a property, as byte offsets: the first
/// token of its head (an access modifier, or the `GET`/`SET` keyword), its `:`,
/// and its `END`.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Accessor {
    pub head: usize,
    pub colon: usize,
    pub end: usize,
}

/// The computed accessors of a `DEFINE PROPERTY` statement, in source order.
/// An auto accessor (`GET.`) has no body and no entry. The AST keeps no spans
/// for the accessors, so they are read from the tokens of the statement that
/// are outside the accessor bodies.
pub(crate) fn property_accessors(stmt: &Statement, toks: &[Token]) -> Vec<Accessor> {
    let StatementKind::Property {
        get_body, set_body, ..
    } = &stmt.kind
    else {
        return Vec::new();
    };
    let mut bodies: Vec<Span> = get_body
        .iter()
        .chain(set_body.iter())
        .flatten()
        .map(|s| s.span)
        .filter(|s| *s != Span::DUMMY)
        .collect();
    bodies.sort_by_key(|s| s.start);

    let (lo, hi) = (stmt.span.start, stmt.span.end);
    let from = toks.partition_point(|t| t.start < lo);
    let mut code: Vec<&Token> = Vec::new();
    let mut b = 0;
    for t in toks[from..].iter().take_while(|t| t.start < hi) {
        if matches!(t.kind, Kind::Comment | Kind::Eof) {
            continue;
        }
        while b < bodies.len() && bodies[b].end <= t.start {
            b += 1;
        }
        if b < bodies.len() && bodies[b].start <= t.start {
            continue;
        }
        code.push(t);
    }

    let mut out = Vec::new();
    let mut i = 0;
    while i < code.len() {
        let t = code[i];
        let after_end = i > 0 && code[i - 1].kind == Kind::End;
        if matches!(t.kind, Kind::Get | Kind::Set) && !after_end {
            // Skip the parameter list of `GET(...)` / `SET(...)`.
            let mut j = i + 1;
            if code.get(j).is_some_and(|p| p.kind == Kind::LeftParen) {
                let mut depth = 0usize;
                while let Some(p) = code.get(j) {
                    j += 1;
                    match p.kind {
                        Kind::LeftParen => depth += 1,
                        Kind::RightParen => {
                            depth = depth.saturating_sub(1);
                            if depth == 0 {
                                break;
                            }
                        }
                        _ => {}
                    }
                }
            }
            if let Some(colon) = code.get(j).filter(|c| c.kind == Kind::Colon)
                && let Some(k) = (j + 1..code.len()).find(|&k| code[k].kind == Kind::End)
            {
                let head = match i.checked_sub(1).map(|p| code[p]) {
                    Some(m) if is_access_modifier(m.kind) => m.start,
                    _ => t.start,
                };
                out.push(Accessor {
                    head: head as usize,
                    colon: colon.start as usize,
                    end: code[k].start as usize,
                });
                i = k + 1;
                continue;
            }
        }
        i += 1;
    }
    out
}

fn is_access_modifier(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Public
            | Kind::Private
            | Kind::Protected
            | Kind::PackagePrivate
            | Kind::PackageProtected
    )
}

/// The trigger-block statement of an `ON` statement, if it has one.
pub(crate) fn on_action(on_kind: &OnKind) -> Option<&Statement> {
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
