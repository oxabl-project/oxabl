//! Plumbing shared by the syntactic rules (`run-value`, `dynamic-invoke`,
//! `shared-state`, `missing-no-undo`): a pre-order walk over every statement
//! and expression, and helpers for reading the source text of a statement.
//!
//! The semantic-backed rules keep their own walkers because they track scope
//! as they descend. The syntactic rules share this one so each stays a pair of
//! hooks rather than a copy of the whole traversal.
//!
//! Two things are deliberately not visited, because they are not statements in
//! the source: the inline parameter list of a `METHOD`, `CONSTRUCTOR`,
//! `DEFINE EVENT` or computed property `SET`, which the parser stores as
//! `DefineParameter` statements. (A `FUNCTION` signature is stored in the
//! function body, so its parameters *are* visited.)

use oxabl_ast::{
    CreateTarget, Expression, ExpressionKind, OnAction, OnKind, PreprocIf, Span, Statement,
    StatementKind, StreamOperation, SubscribeTarget,
};
use oxabl_common::FileSpan;
use oxabl_lexer::{Kind, Lexer, Token};
use oxabl_semantic::AnalysisContext;

/// Hooks called once per node, parents before children.
pub(super) trait Visitor {
    /// Called for every statement. Return `false` to skip the statement's
    /// children; its own expressions are then not visited either.
    fn statement(&mut self, _stmt: &Statement) -> bool {
        true
    }

    /// Called for every expression, including nested operands and arguments.
    fn expression(&mut self, _expr: &Expression) {}
}

/// Walk `stmts` and everything beneath them.
pub(super) fn walk_block(v: &mut impl Visitor, stmts: &[Statement]) {
    for stmt in stmts {
        walk_statement(v, stmt);
    }
}

fn walk_statement(v: &mut impl Visitor, stmt: &Statement) {
    if !v.statement(stmt) {
        return;
    }
    match &stmt.kind {
        StatementKind::VariableDeclaration { initial_value, .. } => {
            walk_opt(v, initial_value.as_ref());
        }
        StatementKind::DefineTempTable { fields, .. } => {
            for init in fields.iter().filter_map(|f| f.initial_value.as_ref()) {
                for e in init {
                    walk_expression(v, e);
                }
            }
        }
        StatementKind::Property {
            get_body, set_body, ..
        } => {
            for body in [get_body, set_body].into_iter().flatten() {
                walk_block(v, body);
            }
        }
        StatementKind::Procedure { body, .. }
        | StatementKind::Function { body, .. }
        | StatementKind::Class { body, .. }
        | StatementKind::Interface { body, .. }
        | StatementKind::Destructor { body }
        | StatementKind::Finally { body }
        | StatementKind::Catch { body, .. }
        | StatementKind::Block(body)
        | StatementKind::VariableDeclarationList(body)
        | StatementKind::Method { body, .. }
        | StatementKind::Constructor { body, .. } => walk_block(v, body),
        StatementKind::Do {
            from,
            to,
            by,
            while_condition,
            on_actions,
            body,
            ..
        } => {
            for e in [from, to, by, while_condition].into_iter().flatten() {
                walk_expression(v, e);
            }
            for e in on_actions {
                walk_expression(v, e);
            }
            walk_block(v, body);
        }
        StatementKind::Repeat {
            while_condition,
            on_actions,
            body,
        } => {
            walk_opt(v, while_condition.as_ref());
            for e in on_actions {
                walk_expression(v, e);
            }
            walk_block(v, body);
        }
        StatementKind::ForEach {
            where_clause,
            on_actions,
            body,
            ..
        } => {
            walk_opt(v, where_clause.as_ref());
            for e in on_actions {
                walk_expression(v, e);
            }
            walk_block(v, body);
        }
        StatementKind::Find {
            key_value,
            where_clause,
            ..
        } => {
            walk_opt(v, key_value.as_ref());
            walk_opt(v, where_clause.as_ref());
        }
        StatementKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            walk_expression(v, condition);
            walk_statement(v, then_branch);
            if let Some(eb) = else_branch {
                walk_statement(v, eb);
            }
        }
        StatementKind::Case {
            expression,
            when_branches,
            otherwise,
        } => {
            walk_expression(v, expression);
            for wb in when_branches {
                for value in &wb.values {
                    walk_expression(v, value);
                }
                walk_block(v, &wb.body);
            }
            if let Some(o) = otherwise {
                walk_block(v, o);
            }
        }
        StatementKind::Label { body, .. } => walk_statement(v, body),
        StatementKind::On { kind } => match kind {
            OnKind::UiEvent { action, .. } | OnKind::DbEvent { action, .. } => match action {
                OnAction::Block(body) => walk_statement(v, body),
                OnAction::PersistentRun { arguments, .. } => {
                    for a in arguments {
                        walk_expression(v, a);
                    }
                }
                OnAction::Revert => {}
            },
            OnKind::KeyRemap { .. } => {}
        },
        StatementKind::PreprocIf(pif) => {
            walk_preproc_if(v, pif, |v, body: &Vec<Statement>| walk_block(v, body));
        }
        StatementKind::PreprocMessage { expression } => walk_expression(v, expression),
        StatementKind::Assignment { target, value } => {
            walk_expression(v, target);
            walk_expression(v, value);
        }
        StatementKind::Assign { assignments } | StatementKind::BufferCopy { assignments, .. } => {
            for p in assignments {
                walk_expression(v, &p.target);
                walk_expression(v, &p.value);
            }
        }
        StatementKind::ExpressionStatement(e)
        | StatementKind::Throw(e)
        | StatementKind::DeleteObject { target: e, .. } => walk_expression(v, e),
        StatementKind::Return(e) => walk_opt(v, e.as_ref()),
        StatementKind::Display { items, .. } => {
            for item in items {
                walk_expression(v, &item.expression);
                walk_opt(v, item.when_condition.as_ref());
            }
        }
        StatementKind::Message { items, .. }
        | StatementKind::Put { items, .. }
        | StatementKind::Form { items, .. }
        | StatementKind::Enable { items }
        | StatementKind::WaitFor { widgets: items }
        | StatementKind::Disable { items }
        | StatementKind::Hide { items, .. }
        | StatementKind::View { items, .. } => {
            for e in items {
                walk_expression(v, e);
            }
        }
        StatementKind::Export {
            delimiter, items, ..
        }
        | StatementKind::Import {
            delimiter,
            targets: items,
            ..
        } => {
            for e in delimiter.iter().chain(items) {
                walk_expression(v, e);
            }
        }
        StatementKind::Update { targets, options } | StatementKind::Set { targets, options } => {
            for e in targets.iter().chain(options) {
                walk_expression(v, e);
            }
        }
        StatementKind::Pause {
            duration: a,
            message: b,
        }
        | StatementKind::Apply {
            event: a,
            target: b,
        } => {
            for e in a.iter().chain(b) {
                walk_expression(v, e);
            }
        }
        StatementKind::Down { lines, .. } | StatementKind::Up { lines, .. } => {
            if let Some(e) = lines {
                walk_expression(v, e);
            }
        }
        StatementKind::Run {
            target,
            arguments,
            in_handle,
            persistent_handle,
            async_handle,
            event_procedure,
            ..
        } => {
            if let oxabl_ast::RunTarget::Dynamic(e) = target {
                walk_expression(v, e);
            }
            for arg in arguments {
                walk_expression(v, &arg.expression);
            }
            for e in [in_handle, persistent_handle, async_handle, event_procedure]
                .into_iter()
                .flatten()
            {
                walk_expression(v, e);
            }
        }
        StatementKind::Publish {
            event_name,
            from_handle,
            arguments,
        } => {
            walk_expression(v, event_name);
            walk_opt(v, from_handle.as_ref());
            for arg in arguments {
                walk_expression(v, &arg.expression);
            }
        }
        StatementKind::Subscribe {
            subscriber,
            event_name,
            target,
            ..
        } => {
            walk_opt(v, subscriber.as_ref());
            walk_expression(v, event_name);
            if let SubscribeTarget::InHandle(h) = target {
                walk_expression(v, h);
            }
        }
        StatementKind::Unsubscribe {
            subscriber,
            event_name,
            in_handle,
        } => {
            for e in [subscriber, event_name, in_handle].into_iter().flatten() {
                walk_expression(v, e);
            }
        }
        StatementKind::StreamIo { operation, .. } => match operation {
            StreamOperation::From(e)
            | StreamOperation::Through(e)
            | StreamOperation::To { target: e, .. } => walk_expression(v, e),
            StreamOperation::Close => {}
        },
        StatementKind::Create { target, .. } => {
            if let CreateTarget::Handle { widget_pool, .. } = target {
                walk_opt(v, widget_pool.as_ref());
            }
        }
        StatementKind::DefineParameter { .. }
        | StatementKind::DefineBuffer { .. }
        | StatementKind::DefineDataset { .. }
        | StatementKind::DefineDataSource { .. }
        | StatementKind::DefineStream { .. }
        | StatementKind::DefineFrame { .. }
        | StatementKind::DefineWidget { .. }
        | StatementKind::DefineEvent { .. }
        | StatementKind::PreprocDefine { .. }
        | StatementKind::PreprocUndefine { .. }
        | StatementKind::TriggerProcedure { .. }
        | StatementKind::Using { .. }
        | StatementKind::Delete { .. }
        | StatementKind::Release { .. }
        | StatementKind::Validate { .. }
        | StatementKind::BufferCompare { .. }
        | StatementKind::IncludeReference { .. }
        | StatementKind::IncludeArgReference { .. }
        | StatementKind::Leave(_)
        | StatementKind::Next(_)
        | StatementKind::Empty
        | StatementKind::Annotation(_)
        | StatementKind::Skipped { .. } => {}
    }
}

fn walk_opt(v: &mut impl Visitor, e: Option<&Expression>) {
    if let Some(e) = e {
        walk_expression(v, e);
    }
}

fn walk_preproc_if<V: Visitor, T>(v: &mut V, pif: &PreprocIf<T>, walk_branch: impl Fn(&mut V, &T)) {
    walk_expression(v, &pif.condition);
    walk_branch(v, &pif.then_branch);
    for (cond, branch) in &pif.elseif_branches {
        walk_expression(v, cond);
        walk_branch(v, branch);
    }
    if let Some(branch) = &pif.else_branch {
        walk_branch(v, branch);
    }
}

fn walk_expression(v: &mut impl Visitor, expr: &Expression) {
    v.expression(expr);
    match &expr.kind {
        ExpressionKind::Literal(_)
        | ExpressionKind::Identifier(_)
        | ExpressionKind::IncludeReference { .. }
        | ExpressionKind::IncludeArgReference { .. }
        | ExpressionKind::PreprocReference(_) => {}
        ExpressionKind::Add(l, r)
        | ExpressionKind::Minus(l, r)
        | ExpressionKind::Multiply(l, r)
        | ExpressionKind::Divide(l, r)
        | ExpressionKind::Modulo(l, r)
        | ExpressionKind::Equal(l, r)
        | ExpressionKind::NotEqual(l, r)
        | ExpressionKind::LessThan(l, r)
        | ExpressionKind::LessThanOrEqual(l, r)
        | ExpressionKind::GreaterThan(l, r)
        | ExpressionKind::GreaterThanOrEqual(l, r)
        | ExpressionKind::Begins(l, r)
        | ExpressionKind::Matches(l, r)
        | ExpressionKind::Contains(l, r)
        | ExpressionKind::And(l, r)
        | ExpressionKind::Or(l, r)
        | ExpressionKind::ArrayAccess { array: l, index: r } => {
            walk_expression(v, l);
            walk_expression(v, r);
        }
        ExpressionKind::Negate(e)
        | ExpressionKind::Not(e)
        | ExpressionKind::MemberAccess { object: e, .. }
        | ExpressionKind::FieldAccess { qualifier: e, .. } => walk_expression(v, e),
        ExpressionKind::IfThenElse(c, t, e) => {
            walk_expression(v, c);
            walk_expression(v, t);
            walk_expression(v, e);
        }
        ExpressionKind::FunctionCall { arguments, .. } | ExpressionKind::New { arguments, .. } => {
            for a in arguments {
                walk_expression(v, a);
            }
        }
        ExpressionKind::MethodCall {
            object, arguments, ..
        } => {
            walk_expression(v, object);
            for a in arguments {
                walk_expression(v, a);
            }
        }
        ExpressionKind::PreprocIf(pif) => {
            walk_preproc_if(v, pif, |v, e: &Expression| walk_expression(v, e));
        }
        ExpressionKind::CanFind { where_clause, .. } => {
            if let Some(w) = where_clause {
                walk_expression(v, w);
            }
        }
    }
}

/// `span` in the file under analysis.
pub(super) fn file_span(ctx: &AnalysisContext, span: Span) -> FileSpan {
    FileSpan {
        file: ctx.file_id,
        span,
    }
}

/// The source text of `span`, or `None` when the span is empty or does not
/// fall inside `source` (hand-built AST, or a span from another buffer).
pub(super) fn source_of(source: &str, span: Span) -> Option<&str> {
    if span.start >= span.end {
        return None;
    }
    source.get(span.start as usize..span.end as usize)
}

/// The first `n` tokens of the statement at `span`, without a trailing EOF.
///
/// The statement text is lexed lazily, so a long statement costs only the
/// tokens asked for. Empty when the span does not map into `source`.
pub(super) fn leading_tokens(source: &str, span: Span, n: usize) -> Vec<(Token, &str)> {
    let Some(text) = source_of(source, span) else {
        return Vec::new();
    };
    Lexer::new(text)
        .take(n)
        .take_while(|t| t.kind != Kind::Eof)
        .map(|t| {
            let word = &text[t.start..t.end];
            (t, word)
        })
        .collect()
}
