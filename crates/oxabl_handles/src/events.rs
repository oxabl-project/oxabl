//! What each statement does to dynamic objects.
//!
//! This is the only module that reads the statement and expression shapes
//! that create, free, store or hand on a handle. The analysis consumes the
//! [`Effect`]s, so supporting a new AST form is a change here.

use oxabl_ast::{
    CreateTarget, CreateTargetKind, DeleteObjectKind, Expression, ExpressionKind as EK, Identifier,
    Literal, ParameterDirection, Span, Statement, StatementKind as SK,
};
use oxabl_lexer::{Kind, Lexer};
use smallvec::SmallVec;

use crate::model::{EscapeKind, FreeKind, ObjectKind, OpaqueReason};

/// Where a value is stored.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Place<'a> {
    /// A plain variable, by its name as written.
    Var(&'a str),
    /// A place the analysis does not follow; storing a handle there is an
    /// escape of this kind.
    Other(EscapeKind),
}

/// The widget pool a create names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum PoolRef {
    /// No `IN WIDGET-POOL`.
    Default,
    /// `IN WIDGET-POOL name`: the name folded to lowercase (a string literal's
    /// value, else the expression's text).
    Named(String),
}

/// One thing a statement does, in execution order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Effect<'a> {
    /// A new object; `into` is `None` when no handle is kept
    /// (`RUN p PERSISTENT` without `SET`).
    Create {
        kind: ObjectKind,
        into: Option<Place<'a>>,
        pool: PoolRef,
        /// `NO-ERROR`: the create may fail and make nothing.
        no_error: bool,
    },
    /// A class instance (`NEW`, `DYNAMIC-NEW`).
    Collected,
    Free {
        var: &'a str,
        kind: FreeKind,
    },
    /// `to = from`; `from` is `None` for any value that is not a plain
    /// variable, which only overwrites `to`.
    Assign {
        to: Place<'a>,
        from: Option<&'a str>,
    },
    Escape {
        var: &'a str,
        kind: EscapeKind,
    },
    Opaque {
        var: &'a str,
        reason: OpaqueReason,
    },
    /// A statement the parser skips, with the names it passed over.
    Skipped(&'a [Identifier]),
    /// `recv:ADD-BUFFER(var)` or `recv:ADD-BUFFER(var:DEFAULT-BUFFER-HANDLE)`:
    /// an escape when `recv` holds a dataset.
    Attach {
        recv: &'a str,
        var: &'a str,
    },
    /// `var = x:FIRST-PROCEDURE` (or `LAST-PROCEDURE`, `NEXT-SIBLING`,
    /// `PREV-SIBLING`): `var` walks a procedure list.
    ProcedureWalk {
        var: &'a str,
    },
    CreatePool {
        name: Option<String>,
        persistent: bool,
    },
    /// `DELETE WIDGET-POOL [name]`; `None` deletes the most recent unnamed
    /// pool.
    DeletePool {
        name: Option<String>,
    },
}

pub(crate) type Effects<'a> = SmallVec<[Effect<'a>; 2]>;

/// The effects of one statement, not counting nested statements.
pub(crate) fn effects<'a>(s: &'a Statement, src: &str) -> Effects<'a> {
    let mut out = Effects::new();
    match &s.kind {
        SK::Create {
            target:
                CreateTarget::Handle {
                    kind,
                    handle,
                    widget_pool,
                    class,
                    assignments,
                    for_table,
                    ..
                },
            no_error,
        } => {
            for e in [class, for_table, widget_pool].into_iter().flatten() {
                scan(e, src, &mut out);
            }
            for a in assignments {
                scan(&a.value, src, &mut out);
            }
            let pool = match widget_pool {
                Some(p) => PoolRef::Named(pool_name(p, src)),
                None => PoolRef::Default,
            };
            out.push(Effect::Create {
                kind: object_kind(*kind),
                into: place_of(handle),
                pool,
                no_error: *no_error,
            });
            // `ASSIGN FRAME = f` or `PARENT = w`: the parent widget owns it.
            if let Some(var) = var_of(handle)
                && assignments
                    .iter()
                    .any(|a| is_parent_attribute(&a.attribute.name))
            {
                out.push(Effect::Escape {
                    var,
                    kind: EscapeKind::WidgetParent,
                });
            }
            // Any other attribute set to a handle stores it in the new widget.
            for a in assignments {
                if let Some(var) = var_of(&a.value)
                    && !is_parent_attribute(&a.attribute.name)
                {
                    out.push(Effect::Assign {
                        to: Place::Other(EscapeKind::Attribute),
                        from: Some(var),
                    });
                }
            }
        }
        SK::CreateWidgetPool {
            name, persistent, ..
        } => out.push(Effect::CreatePool {
            name: name.as_ref().map(|n| pool_name(n, src)),
            persistent: *persistent,
        }),
        SK::DeleteWidgetPool { name, .. } => out.push(Effect::DeletePool {
            name: name.as_ref().map(|n| pool_name(n, src)),
        }),
        SK::DeleteObject {
            kind,
            target,
            extra_targets,
            ..
        } => {
            let kind = match kind {
                DeleteObjectKind::Procedure => FreeKind::DeleteProcedure,
                DeleteObjectKind::Widget => FreeKind::DeleteWidget,
                _ => FreeKind::DeleteObject,
            };
            for t in std::iter::once(target).chain(extra_targets) {
                scan(t, src, &mut out);
                if let Some(var) = var_of(t) {
                    out.push(Effect::Free { var, kind });
                }
            }
        }
        SK::ReleaseObject { target, .. } => {
            if let Some(var) = var_of(target) {
                out.push(Effect::Free {
                    var,
                    kind: FreeKind::ReleaseObject,
                });
            }
        }
        // By convention a persistent procedure deletes itself on CLOSE.
        SK::Apply {
            event,
            target: Some(target),
        } => {
            if let (EK::Literal(Literal::String(e)), Some(var)) = (&event.kind, var_of(target))
                && e.value.eq_ignore_ascii_case("CLOSE")
            {
                out.push(Effect::Free {
                    var,
                    kind: FreeKind::ApplyClose,
                });
            }
        }
        SK::Run {
            target,
            arguments,
            in_handle,
            persistent,
            persistent_handle,
            no_error,
            ..
        } => {
            let dynamic = matches!(target, oxabl_ast::RunTarget::Dynamic(_));
            if let Some(h) = in_handle {
                scan(h, src, &mut out);
                if let (oxabl_ast::RunTarget::Literal { name, .. }, Some(var)) = (target, var_of(h))
                    && (name.eq_ignore_ascii_case("destroy")
                        || name.eq_ignore_ascii_case("destroyObject"))
                {
                    out.push(Effect::Opaque {
                        var,
                        reason: OpaqueReason::DestroyCall,
                    });
                }
            }
            for a in arguments {
                scan(&a.expression, src, &mut out);
                let Some(var) = var_of(&a.expression) else {
                    continue;
                };
                if dynamic {
                    out.push(Effect::Opaque {
                        var,
                        reason: OpaqueReason::DynamicCall,
                    });
                }
                match a.direction {
                    ParameterDirection::Input => {}
                    ParameterDirection::InputOutput => {
                        out.push(Effect::Escape {
                            var,
                            kind: EscapeKind::InputOutputArgument,
                        });
                        out.push(Effect::Assign {
                            to: Place::Var(var),
                            from: None,
                        });
                    }
                    ParameterDirection::Output | ParameterDirection::Return => {
                        out.push(Effect::Assign {
                            to: Place::Var(var),
                            from: None,
                        })
                    }
                }
            }
            if *persistent {
                out.push(Effect::Create {
                    kind: ObjectKind::PersistentProcedure,
                    into: persistent_handle.as_ref().and_then(place_of),
                    pool: PoolRef::Default,
                    no_error: *no_error,
                });
            }
        }
        SK::Assignment { target, value } => assignment(target, value, src, &mut out),
        SK::Assign { assignments } => {
            for pair in assignments {
                assignment(&pair.target, &pair.value, src, &mut out);
            }
        }
        SK::ExpressionStatement(e) => scan(e, src, &mut out),
        SK::If { condition, .. } => scan(condition, src, &mut out),
        SK::Return(Some(e)) => {
            scan(e, src, &mut out);
            if let Some(var) = var_of(e) {
                out.push(Effect::Escape {
                    var,
                    kind: EscapeKind::Return,
                });
            }
        }
        SK::Publish { arguments, .. } => {
            for a in arguments {
                scan(&a.expression, src, &mut out);
                if let Some(var) = var_of(&a.expression) {
                    out.push(Effect::Escape {
                        var,
                        kind: EscapeKind::Published,
                    });
                }
            }
        }
        SK::Subscribe {
            subscriber: Some(e),
            ..
        } => {
            if let Some(var) = var_of(e) {
                out.push(Effect::Escape {
                    var,
                    kind: EscapeKind::Subscriber,
                });
            }
        }
        SK::Skipped { names, .. } if !names.is_empty() => out.push(Effect::Skipped(names)),
        _ => {}
    }
    out
}

/// Whether a statement the CFG says may raise cannot: an assignment of a
/// plain variable or literal to a plain variable, or `RETURN` of one.
pub(crate) fn cannot_raise(s: &Statement) -> bool {
    let simple = |e: &Expression| var_of(e).is_some() || matches!(e.kind, EK::Literal(_));
    match &s.kind {
        SK::Assignment { target, value } => var_of(target).is_some() && simple(value),
        SK::Assign { assignments } => assignments
            .iter()
            .all(|p| var_of(&p.target).is_some() && simple(&p.value)),
        SK::Return(value) => value.as_ref().is_none_or(simple),
        _ => false,
    }
}

/// Whether `s` creates something this crate tracks, without building effects.
pub(crate) fn creates(effects: &Effects) -> bool {
    effects
        .iter()
        .any(|e| matches!(e, Effect::Create { .. } | Effect::Collected))
}

fn assignment<'a>(target: &'a Expression, value: &'a Expression, src: &str, out: &mut Effects<'a>) {
    if let EK::FunctionCall { name, arguments } = &target.kind
        && name.name.eq_ignore_ascii_case("SET-SIZE")
    {
        scan(value, src, out);
        let Some(m) = arguments.first() else {
            return;
        };
        if is_zero(value) {
            if let Some(var) = var_of(m) {
                out.push(Effect::Free {
                    var,
                    kind: FreeKind::SetSizeZero,
                });
            }
        } else if let Some(into) = place_of(m) {
            out.push(Effect::Create {
                kind: ObjectKind::Memptr,
                into: Some(into),
                pool: PoolRef::Default,
                no_error: false,
            });
        }
        return;
    }
    scan(value, src, out);
    if let EK::MemberAccess { object, member } = &target.kind
        && is_parent_attribute(&member.name)
    {
        // The value is the parent, which keeps its own owner.
        if let Some(var) = var_of(object) {
            out.push(Effect::Escape {
                var,
                kind: EscapeKind::WidgetParent,
            });
        }
        return;
    }
    if let (Some(var), EK::MemberAccess { member, .. }) = (var_of(target), &value.kind)
        && [
            "FIRST-PROCEDURE",
            "LAST-PROCEDURE",
            "NEXT-SIBLING",
            "PREV-SIBLING",
        ]
        .iter()
        .any(|m| member.name.eq_ignore_ascii_case(m))
    {
        out.push(Effect::ProcedureWalk { var });
    }
    if let Some(to) = place_of(target) {
        out.push(Effect::Assign {
            to,
            from: var_of(value),
        });
    }
}

/// Calls inside an expression that keep, free or hide a handle argument.
fn scan<'a>(e: &'a Expression, src: &str, out: &mut Effects<'a>) {
    match &e.kind {
        EK::FunctionCall { name, arguments } => {
            let n = name.name.as_str();
            if n.eq_ignore_ascii_case("DYNAMIC-NEW") {
                out.push(Effect::Collected);
                opaque_args(arguments.iter().skip(1), out);
            } else if n.eq_ignore_ascii_case("DYNAMIC-FUNCTION") {
                // `DYNAMIC-FUNCTION("f" IN h, ...)`: `h` is where the function
                // runs, not an argument.
                let skip = if dynamic_function_in(arguments, src) {
                    2
                } else {
                    1
                };
                opaque_args(arguments.iter().skip(skip), out);
            } else if n.eq_ignore_ascii_case("DYNAMIC-INVOKE") {
                opaque_args(arguments.iter().skip(2), out);
            }
            for a in arguments {
                scan(a, src, out);
            }
        }
        EK::MethodCall {
            object,
            method,
            arguments,
        } => {
            scan(object, src, out);
            for a in arguments {
                scan(a, src, out);
            }
            if method.name.eq_ignore_ascii_case("ADD-BUFFER")
                && let Some(recv) = var_of(object)
            {
                for a in arguments {
                    let var = match &a.kind {
                        EK::MemberAccess { object, member }
                            if member.name.eq_ignore_ascii_case("DEFAULT-BUFFER-HANDLE") =>
                        {
                            var_of(object)
                        }
                        _ => var_of(a),
                    };
                    if let Some(var) = var {
                        out.push(Effect::Attach { recv, var });
                    }
                }
            }
            if method.name.eq_ignore_ascii_case("ADD-SUPER-PROCEDURE") {
                for a in arguments {
                    if let Some(var) = var_of(a) {
                        out.push(Effect::Escape {
                            var,
                            kind: EscapeKind::SuperProcedure,
                        });
                    }
                }
            }
        }
        EK::New { arguments, .. } => {
            for a in arguments {
                scan(a, src, out);
            }
            out.push(Effect::Collected);
            for a in arguments {
                if let Some(var) = var_of(a) {
                    out.push(Effect::Escape {
                        var,
                        kind: EscapeKind::ConstructorArgument,
                    });
                }
            }
        }
        EK::Add(a, b)
        | EK::Minus(a, b)
        | EK::Multiply(a, b)
        | EK::Divide(a, b)
        | EK::Modulo(a, b)
        | EK::Equal(a, b)
        | EK::NotEqual(a, b)
        | EK::LessThan(a, b)
        | EK::LessThanOrEqual(a, b)
        | EK::GreaterThan(a, b)
        | EK::GreaterThanOrEqual(a, b)
        | EK::Begins(a, b)
        | EK::Matches(a, b)
        | EK::Contains(a, b)
        | EK::And(a, b)
        | EK::Or(a, b) => {
            scan(a, src, out);
            scan(b, src, out);
        }
        EK::Negate(a) | EK::Not(a) => scan(a, src, out),
        EK::IfThenElse(c, a, b) => {
            scan(c, src, out);
            scan(a, src, out);
            scan(b, src, out);
        }
        EK::MemberAccess { object, .. } => scan(object, src, out),
        EK::ArrayAccess { array, index } => {
            scan(array, src, out);
            scan(index, src, out);
        }
        EK::FieldAccess { qualifier, .. } => scan(qualifier, src, out),
        EK::PreprocIf(p) => {
            scan(&p.then_branch, src, out);
            for (_, b) in &p.elseif_branches {
                scan(b, src, out);
            }
            if let Some(b) = &p.else_branch {
                scan(b, src, out);
            }
        }
        _ => {}
    }
}

fn opaque_args<'a>(args: impl Iterator<Item = &'a Expression>, out: &mut Effects<'a>) {
    for a in args {
        if let Some(var) = var_of(a) {
            out.push(Effect::Opaque {
                var,
                reason: OpaqueReason::DynamicCall,
            });
        }
    }
}

/// Whether `IN` separates the first two arguments of a `DYNAMIC-FUNCTION`.
fn dynamic_function_in(args: &[Expression], src: &str) -> bool {
    let (Some(a), Some(b)) = (args.first(), args.get(1)) else {
        return false;
    };
    let gap = Span {
        start: a.span.end,
        end: b.span.start,
    };
    tokens(src, gap).any(|k| k == Kind::KwIn)
}

/// The variable named by a plain identifier expression.
pub(crate) fn var_of(e: &Expression) -> Option<&str> {
    match &e.kind {
        EK::Identifier(id) if !id.name.contains(['.', ':']) => Some(&id.name),
        _ => None,
    }
}

/// Where an assignment target stores its value.
fn place_of(e: &Expression) -> Option<Place<'_>> {
    match &e.kind {
        EK::Identifier(id) => Some(place_of_name(&id.name)),
        EK::ArrayAccess { .. } => Some(Place::Other(EscapeKind::ArrayElement)),
        EK::FieldAccess { .. } => Some(Place::Other(EscapeKind::Field)),
        EK::MemberAccess { .. } => Some(Place::Other(EscapeKind::Attribute)),
        _ => None,
    }
}

fn place_of_name(name: &str) -> Place<'_> {
    if name.contains(':') {
        Place::Other(EscapeKind::Attribute)
    } else if name.contains('.') {
        Place::Other(EscapeKind::Field)
    } else {
        Place::Var(name)
    }
}

fn is_zero(e: &Expression) -> bool {
    matches!(&e.kind, EK::Literal(Literal::Integer(i)) if i.value == 0)
}

fn object_kind(k: CreateTargetKind) -> ObjectKind {
    match k {
        CreateTargetKind::Dataset => ObjectKind::Dataset,
        CreateTargetKind::DataSource => ObjectKind::DataSource,
        CreateTargetKind::TempTable => ObjectKind::TempTable,
        CreateTargetKind::Buffer => ObjectKind::Buffer,
        CreateTargetKind::Query => ObjectKind::Query,
        CreateTargetKind::Widget => ObjectKind::Widget,
        CreateTargetKind::Call => ObjectKind::Call,
        CreateTargetKind::XDocument => ObjectKind::XDocument,
        CreateTargetKind::XNodeRef => ObjectKind::XNodeRef,
        CreateTargetKind::SaxReader => ObjectKind::SaxReader,
        CreateTargetKind::SaxWriter => ObjectKind::SaxWriter,
        CreateTargetKind::SaxAttributes => ObjectKind::SaxAttributes,
        CreateTargetKind::Socket => ObjectKind::Socket,
        CreateTargetKind::ServerSocket => ObjectKind::ServerSocket,
        CreateTargetKind::Server => ObjectKind::Server,
        CreateTargetKind::SoapHeader => ObjectKind::SoapHeader,
        CreateTargetKind::SoapHeaderEntryRef => ObjectKind::SoapHeaderEntryRef,
        CreateTargetKind::ClientPrincipal => ObjectKind::ClientPrincipal,
        CreateTargetKind::Automation => ObjectKind::Automation,
        _ => ObjectKind::Dynamic,
    }
}

/// `FRAME` or `PARENT`: setting it puts a widget under a parent that owns it.
fn is_parent_attribute(name: &str) -> bool {
    name.eq_ignore_ascii_case("FRAME") || name.eq_ignore_ascii_case("PARENT")
}

fn pool_name(e: &Expression, src: &str) -> String {
    match &e.kind {
        EK::Literal(Literal::String(s)) => s.value.to_ascii_lowercase(),
        _ => slice(src, e.span).trim().to_ascii_lowercase(),
    }
}

/// What an `IF` condition says about a holder of a live object, so that no
/// path with the object live takes one of its edges.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Guard<'a> {
    /// The holder the condition tests; `None` for a test of
    /// `ERROR-STATUS:ERROR` alone.
    pub(crate) var: Option<&'a str>,
    /// The value of the condition for every path where the holder is valid:
    /// the other edge is never taken while the object lives.
    pub(crate) live: Option<bool>,
    /// The same, right after a `NO-ERROR` create that made the object, where
    /// `ERROR-STATUS:ERROR` is false too.
    pub(crate) created: Option<bool>,
}

pub(crate) type Guards<'a> = SmallVec<[Guard<'a>; 1]>;

/// The guards of an `IF` condition: one for each holder a test names, and one
/// for the error status.
pub(crate) fn guards(cond: &Expression) -> Guards<'_> {
    let mut vars: SmallVec<[Option<&str>; 2]> = SmallVec::new();
    let mut error = false;
    collect_tested(cond, &mut vars, &mut error);
    if error {
        vars.push(None);
    }
    vars.into_iter()
        .filter_map(|var| {
            let live = var.and_then(|v| eval(cond, Some(v), false));
            let created = eval(cond, var, true);
            (live.is_some() || created.is_some()).then_some(Guard { var, live, created })
        })
        .collect()
}

fn collect_tested<'a>(
    e: &'a Expression,
    vars: &mut SmallVec<[Option<&'a str>; 2]>,
    error: &mut bool,
) {
    match &e.kind {
        EK::Not(a) => collect_tested(a, vars, error),
        EK::And(a, b) | EK::Or(a, b) => {
            collect_tested(a, vars, error);
            collect_tested(b, vars, error);
        }
        _ => {
            if is_error_status(e) {
                *error = true;
            } else if let Some(v) = tested_var(e)
                && !vars.contains(&Some(v))
            {
                vars.push(Some(v));
            }
        }
    }
}

/// The holder a validity or `?` comparison names.
fn tested_var(e: &Expression) -> Option<&str> {
    match &e.kind {
        EK::FunctionCall { name, arguments }
            if is_validity_test(&name.name) && arguments.len() == 1 =>
        {
            var_of(&arguments[0])
        }
        EK::Equal(a, b) | EK::NotEqual(a, b) => match (&a.kind, &b.kind) {
            (_, EK::Literal(Literal::Unknown(_))) => var_of(a),
            (EK::Literal(Literal::Unknown(_)), _) => var_of(b),
            _ => None,
        },
        _ => None,
    }
}

fn is_validity_test(name: &str) -> bool {
    name.eq_ignore_ascii_case("VALID-HANDLE") || name.eq_ignore_ascii_case("VALID-OBJECT")
}

fn is_error_status(e: &Expression) -> bool {
    match &e.kind {
        EK::Identifier(id) => id.name.eq_ignore_ascii_case("ERROR-STATUS:ERROR"),
        EK::MemberAccess { object, member } => {
            member.name.eq_ignore_ascii_case("ERROR")
                && matches!(&object.kind, EK::Identifier(id)
                    if id.name.eq_ignore_ascii_case("ERROR-STATUS"))
        }
        _ => false,
    }
}

/// The value of `cond` when `var` holds a live object (valid, not unknown) and,
/// if `error_clear`, `ERROR-STATUS:ERROR` is false; `None` when it varies.
fn eval(cond: &Expression, var: Option<&str>, error_clear: bool) -> Option<bool> {
    match &cond.kind {
        EK::Not(a) => eval(a, var, error_clear).map(|v| !v),
        EK::And(a, b) => match (eval(a, var, error_clear), eval(b, var, error_clear)) {
            (Some(false), _) | (_, Some(false)) => Some(false),
            (Some(true), Some(true)) => Some(true),
            _ => None,
        },
        EK::Or(a, b) => match (eval(a, var, error_clear), eval(b, var, error_clear)) {
            (Some(true), _) | (_, Some(true)) => Some(true),
            (Some(false), Some(false)) => Some(false),
            _ => None,
        },
        _ if is_error_status(cond) => error_clear.then_some(false),
        _ => {
            let v = var?;
            if tested_var(cond)? != v {
                return None;
            }
            // Valid and not unknown: a validity test and `<> ?` hold.
            Some(!matches!(cond.kind, EK::Equal(..)))
        }
    }
}

fn slice(src: &str, span: Span) -> &str {
    src.get(span.start as usize..span.end as usize)
        .unwrap_or("")
}

fn tokens(src: &str, span: Span) -> impl Iterator<Item = Kind> + '_ {
    Lexer::new(slice(src, span))
        .map(|t| t.kind)
        .filter(|k| !matches!(k, Kind::Comment))
        .take_while(|k| *k != Kind::Eof)
}
