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
    AstPools, CreateTarget, ExprId, ExprList, Expression, ExpressionKind, OnAction, OnKind,
    PreprocIf, Span, Statement, StatementKind, StreamOperation, SubscribeTarget,
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
pub(super) fn walk_block(v: &mut impl Visitor, p: &AstPools, stmts: &[Statement]) {
    for stmt in stmts {
        walk_statement(v, p, stmt);
    }
}

fn walk_statement(v: &mut impl Visitor, p: &AstPools, stmt: &Statement) {
    if !v.statement(stmt) {
        return;
    }
    match &stmt.kind {
        StatementKind::VariableDeclaration { initial_value, .. } => {
            walk_opt(v, p, *initial_value);
        }
        StatementKind::DefineTempTable { fields, .. } => {
            for init in fields.iter().filter_map(|f| f.initial_value) {
                walk_list(v, p, init);
            }
        }
        StatementKind::Property {
            get_body, set_body, ..
        } => {
            for body in [get_body, set_body].into_iter().flatten() {
                walk_block(v, p, body);
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
        | StatementKind::Constructor { body, .. } => walk_block(v, p, body),
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
                walk_expression(v, p, *e);
            }
            walk_list(v, p, *on_actions);
            walk_block(v, p, body);
        }
        StatementKind::Repeat {
            while_condition,
            on_actions,
            body,
        } => {
            walk_opt(v, p, *while_condition);
            walk_list(v, p, *on_actions);
            walk_block(v, p, body);
        }
        StatementKind::ForEach {
            where_clause,
            on_actions,
            body,
            ..
        } => {
            walk_opt(v, p, *where_clause);
            walk_list(v, p, *on_actions);
            walk_block(v, p, body);
        }
        StatementKind::Find {
            key_value,
            where_clause,
            ..
        } => {
            walk_opt(v, p, *key_value);
            walk_opt(v, p, *where_clause);
        }
        StatementKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            walk_expression(v, p, *condition);
            walk_statement(v, p, then_branch);
            if let Some(eb) = else_branch {
                walk_statement(v, p, eb);
            }
        }
        StatementKind::Case {
            expression,
            when_branches,
            otherwise,
        } => {
            walk_expression(v, p, *expression);
            for wb in when_branches {
                walk_list(v, p, wb.values);
                walk_block(v, p, &wb.body);
            }
            if let Some(o) = otherwise {
                walk_block(v, p, o);
            }
        }
        StatementKind::Label { body, .. } => walk_statement(v, p, body),
        StatementKind::On { kind } => match kind {
            OnKind::UiEvent { action, .. } | OnKind::DbEvent { action, .. } => match action {
                OnAction::Block(body) => walk_statement(v, p, body),
                OnAction::PersistentRun {
                    procedure,
                    in_handle,
                    arguments,
                } => {
                    walk_opt(v, p, procedure.expression());
                    walk_opt(v, p, *in_handle);
                    walk_list(v, p, *arguments);
                }
                OnAction::Revert => {}
            },
            OnKind::KeyRemap { .. } => {}
        },
        StatementKind::PreprocIf(pif) => {
            walk_preproc_if(v, p, pif, |v, body: &Vec<Statement>| walk_block(v, p, body));
        }
        StatementKind::PreprocMessage { expression } => walk_expression(v, p, *expression),
        StatementKind::Assignment { target, value } => {
            walk_expression(v, p, *target);
            walk_expression(v, p, *value);
        }
        StatementKind::Assign { assignments } | StatementKind::BufferCopy { assignments, .. } => {
            for pair in assignments {
                walk_expression(v, p, pair.target);
                walk_expression(v, p, pair.value);
            }
        }
        StatementKind::ExpressionStatement(e)
        | StatementKind::Throw(e)
        | StatementKind::ReleaseObject { target: e, .. } => walk_expression(v, p, *e),
        StatementKind::DeleteObject {
            target,
            extra_targets,
            ..
        } => {
            walk_expression(v, p, *target);
            walk_list(v, p, *extra_targets);
        }
        StatementKind::CreateWidgetPool { name, .. }
        | StatementKind::DeleteWidgetPool { name, .. } => walk_opt(v, p, *name),
        StatementKind::Return(e) => walk_opt(v, p, *e),
        StatementKind::Display { items, .. } => {
            for item in items {
                walk_expression(v, p, item.expression);
                walk_opt(v, p, item.when_condition);
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
            walk_list(v, p, *items);
        }
        StatementKind::Export {
            delimiter, items, ..
        }
        | StatementKind::Import {
            delimiter,
            targets: items,
            ..
        } => {
            walk_opt(v, p, *delimiter);
            walk_list(v, p, *items);
        }
        StatementKind::Update {
            targets, options, ..
        }
        | StatementKind::Set {
            targets, options, ..
        }
        | StatementKind::PromptFor {
            targets, options, ..
        } => {
            walk_list(v, p, *targets);
            walk_list(v, p, *options);
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
                walk_expression(v, p, *e);
            }
        }
        StatementKind::Down { lines, .. } | StatementKind::Up { lines, .. } => {
            if let Some(e) = lines {
                walk_expression(v, p, *e);
            }
        }
        StatementKind::Get { query, .. } => walk_expression(v, p, *query),
        StatementKind::CopyLob {
            source,
            target,
            options,
            ..
        } => {
            walk_expression(v, p, *source);
            walk_expression(v, p, *target);
            walk_list(v, p, *options);
        }
        StatementKind::Color { items, .. } => {
            walk_list(v, p, *items);
        }
        StatementKind::Accumulate { expression, breaks } => {
            walk_expression(v, p, *expression);
            walk_list(v, p, *breaks);
        }
        StatementKind::Reposition {
            query, operands, ..
        } => {
            walk_expression(v, p, *query);
            walk_list(v, p, *operands);
        }
        StatementKind::Close {
            target,
            status,
            handle,
            ..
        } => {
            for e in std::iter::once(target).chain(status).chain(handle) {
                walk_expression(v, p, *e);
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
                walk_expression(v, p, *e);
            }
            for arg in arguments {
                walk_expression(v, p, arg.expression);
            }
            for e in [in_handle, persistent_handle, async_handle, event_procedure]
                .into_iter()
                .flatten()
            {
                walk_expression(v, p, *e);
            }
        }
        StatementKind::Publish {
            event_name,
            from_handle,
            arguments,
        } => {
            walk_expression(v, p, *event_name);
            walk_opt(v, p, *from_handle);
            for arg in arguments {
                walk_expression(v, p, arg.expression);
            }
        }
        StatementKind::Subscribe {
            subscriber,
            event_name,
            target,
            ..
        } => {
            walk_opt(v, p, *subscriber);
            walk_expression(v, p, *event_name);
            if let SubscribeTarget::InHandle(h) = target {
                walk_expression(v, p, *h);
            }
        }
        StatementKind::Unsubscribe {
            subscriber,
            event_name,
            in_handle,
        } => {
            for e in [subscriber, event_name, in_handle].into_iter().flatten() {
                walk_expression(v, p, *e);
            }
        }
        StatementKind::StreamIo { operation, .. } => match operation {
            StreamOperation::From(target) | StreamOperation::To { target, .. } => {
                walk_opt(v, p, target.expression());
            }
            StreamOperation::Through(e) => walk_expression(v, p, *e),
            StreamOperation::Close => {}
        },
        StatementKind::Create { target, .. } => {
            if let CreateTarget::Handle {
                handle,
                widget_pool,
                class,
                assignments,
                triggers,
                connect,
                for_table,
                ..
            } = target
            {
                walk_expression(v, p, *handle);
                walk_opt(v, p, *for_table);
                walk_opt(v, p, *widget_pool);
                walk_opt(v, p, *class);
                walk_opt(v, p, connect.as_ref().and_then(|c| c.file));
                for a in assignments {
                    walk_expression(v, p, a.value);
                }
                for t in triggers {
                    match &t.action {
                        OnAction::Block(body) => walk_statement(v, p, body),
                        OnAction::PersistentRun {
                            procedure,
                            in_handle,
                            arguments,
                        } => {
                            walk_opt(v, p, procedure.expression());
                            walk_opt(v, p, *in_handle);
                            walk_list(v, p, *arguments);
                        }
                        OnAction::Revert => {}
                    }
                }
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
        | StatementKind::Clear { .. }
        | StatementKind::Annotation(_)
        | StatementKind::Skipped { .. } => {}
    }
}

fn walk_opt(v: &mut impl Visitor, p: &AstPools, e: Option<ExprId>) {
    if let Some(e) = e {
        walk_expression(v, p, e);
    }
}

fn walk_list(v: &mut impl Visitor, p: &AstPools, list: ExprList) {
    for &e in p.exprs(list) {
        walk_expression(v, p, e);
    }
}

fn walk_preproc_if<V: Visitor, T>(
    v: &mut V,
    p: &AstPools,
    pif: &PreprocIf<T>,
    walk_branch: impl Fn(&mut V, &T),
) {
    walk_expression(v, p, pif.condition);
    walk_branch(v, &pif.then_branch);
    for &(cond, ref branch) in &pif.elseif_branches {
        walk_expression(v, p, cond);
        walk_branch(v, branch);
    }
    if let Some(branch) = &pif.else_branch {
        walk_branch(v, branch);
    }
}

fn walk_expression(v: &mut impl Visitor, p: &AstPools, id: ExprId) {
    let expr = p.expr(id);
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
            walk_expression(v, p, *l);
            walk_expression(v, p, *r);
        }
        ExpressionKind::Negate(e)
        | ExpressionKind::Not(e)
        | ExpressionKind::MemberAccess { object: e, .. }
        | ExpressionKind::FieldAccess { qualifier: e, .. } => walk_expression(v, p, *e),
        ExpressionKind::IfThenElse(c, t, e) => {
            walk_expression(v, p, *c);
            walk_expression(v, p, *t);
            walk_expression(v, p, *e);
        }
        ExpressionKind::FunctionCall { arguments, .. } | ExpressionKind::New { arguments, .. } => {
            walk_list(v, p, *arguments);
        }
        ExpressionKind::MethodCall {
            object, arguments, ..
        } => {
            walk_expression(v, p, *object);
            walk_list(v, p, *arguments);
        }
        ExpressionKind::PreprocIf(pif) => {
            walk_preproc_if(v, p, pif, |v, e: &ExprId| walk_expression(v, p, *e));
        }
        ExpressionKind::CanFind { where_clause, .. } => {
            if let Some(w) = where_clause {
                walk_expression(v, p, *w);
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
            let word = t.slice(text);
            (t, word)
        })
        .collect()
}
