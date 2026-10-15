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
        /// A MEMPTR a `COPY-LOB` fills: not an object of its own where an
        /// earlier create of the same variable dominates it.
        weak: bool,
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
    /// `var` is an `OUTPUT` or `INPUT-OUTPUT` argument: it is overwritten, but
    /// for a MEMPTR the callee may fill the block it already points to.
    OutputArgument {
        var: &'a str,
    },
    /// `to = h:ATTR`, `to = h:METHOD(...)`: a handle derived from the holder
    /// `from`. It is an escape of the holder only where `to` outlives the unit.
    DerivedAssign {
        to: Place<'a>,
        from: &'a str,
    },
    /// A statement the parser skips, with the names it passed over.
    Skipped(&'a [Identifier]),
    /// `var` is the `pos`-th argument of a call, passed `INPUT`.
    Pass {
        var: &'a str,
        callee: Callee<'a>,
        pos: usize,
    },
    /// `COPY-LOB ... TO to`: a MEMPTR target is allocated by the copy. The
    /// index turns it into a [`Effect::Create`] when `to` is a MEMPTR.
    CopyLob {
        to: Option<&'a str>,
        no_error: bool,
    },
    /// `recv:ADD-BUFFER(var)` or `recv:ADD-BUFFER(var:DEFAULT-BUFFER-HANDLE)`:
    /// an escape when `recv` holds a dataset, a use when it holds a query.
    /// `recv` is `None` when the receiver is not a variable.
    Attach {
        recv: Option<&'a str>,
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

/// What a call runs, as far as the statement shows.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Callee<'a> {
    /// `RUN name` or `RUN name IN THIS-PROCEDURE`.
    Procedure(&'a str),
    Function(&'a str),
    /// `obj:Method(...)`: the last link of the chain, and what it is called on.
    Method {
        name: &'a str,
        recv: Receiver<'a>,
    },
    /// `RUN name IN h`, or a call the file cannot resolve by name.
    External,
}

/// What a method is called on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Receiver<'a> {
    /// A plain name: a variable, a static object or a system handle.
    Name(&'a str),
    /// An attribute or method chain from this name: `h:A:M(...)`.
    Chain(&'a str),
    /// A chain whose last link is a handle attribute: `BUFFER b:HANDLE`.
    Handle,
    /// Any other expression: `CAST(...)`, a class name, ...
    Other,
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
                weak: false,
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
                free_operand(t, kind, &mut out);
            }
        }
        SK::ReleaseObject { target, .. } => {
            scan(target, src, &mut out);
            free_operand(target, FreeKind::ReleaseObject, &mut out);
        }
        // By convention a persistent procedure deletes itself on CLOSE.
        SK::Apply {
            event: Some(event),
            target: Some(target),
        } => {
            if let (EK::Literal(Literal::String(e)), Some(var)) = (&event.kind, identity_of(target))
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
                    && (is_destroy_name(name) || is_dispatch_destroy(name, arguments))
                {
                    out.push(Effect::Opaque {
                        var,
                        reason: OpaqueReason::DestroyCall,
                    });
                }
            }
            let callee = match (target, in_handle) {
                (_, Some(h)) if !is_this_procedure(h) => Callee::External,
                (oxabl_ast::RunTarget::Literal { name, .. }, _) => Callee::Procedure(name),
                _ => Callee::External,
            };
            for (pos, a) in arguments.iter().enumerate() {
                scan(&a.expression, src, &mut out);
                let Some(var) = identity_of(&a.expression) else {
                    continue;
                };
                if dynamic {
                    out.push(Effect::Opaque {
                        var,
                        reason: OpaqueReason::DynamicCall,
                    });
                }
                match a.direction {
                    // A table or dataset argument by value is a copy the callee
                    // cannot free; the parser drops that keyword.
                    ParameterDirection::Input
                        if !dynamic && !copied_argument(src, a.expression.span) =>
                    {
                        out.push(Effect::Pass { var, callee, pos })
                    }
                    ParameterDirection::Input => {}
                    ParameterDirection::InputOutput => {
                        out.push(Effect::Escape {
                            var,
                            kind: EscapeKind::InputOutputArgument,
                        });
                        out.push(Effect::OutputArgument { var });
                    }
                    ParameterDirection::Output | ParameterDirection::Return => {
                        out.push(Effect::OutputArgument { var })
                    }
                }
            }
            if *persistent {
                out.push(Effect::Create {
                    kind: ObjectKind::PersistentProcedure,
                    into: persistent_handle.as_ref().and_then(place_of),
                    pool: PoolRef::Default,
                    no_error: *no_error,
                    weak: false,
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
            if let Some(var) = identity_of(e) {
                out.push(Effect::Escape {
                    var,
                    kind: EscapeKind::Return,
                });
            } else if let Some(var) = derived_root(e) {
                out.push(Effect::Escape {
                    var,
                    kind: EscapeKind::DerivedHandle,
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
        SK::Skipped { names, .. } if !names.is_empty() => match copy_lob(s.span, names, src) {
            Some((to, no_error)) => out.push(Effect::CopyLob { to, no_error }),
            None => out.push(Effect::Skipped(names)),
        },
        // An event of a waited-on object can run code that frees it
        // (`ON CLOSE OF THIS-PROCEDURE DO: DELETE PROCEDURE ...`).
        SK::WaitFor { widgets } => {
            for var in widgets.iter().filter_map(var_of) {
                out.push(Effect::Opaque {
                    var,
                    reason: OpaqueReason::SkippedStatement,
                });
            }
        }
        _ => {}
    }
    // The names a head-parsed statement's skipped tail passed over.
    if let Some(tail) = &s.skipped_tail
        && !tail.names.is_empty()
    {
        out.push(Effect::Skipped(&tail.names));
    }
    out
}

/// Whether a statement the CFG says may raise cannot: an assignment of a
/// plain variable or literal to a plain variable, `RETURN` of one, a test
/// (`IF`, `CASE`, `DO WHILE`, `REPEAT WHILE`) of variables and literals only,
/// or a `DO i = a TO b [BY c]` header whose bounds are variables or literals.
pub(crate) fn cannot_raise(s: &Statement) -> bool {
    let simple = |e: &Expression| identity_of(e).is_some() || matches!(e.kind, EK::Literal(_));
    match &s.kind {
        SK::If { condition, .. } => plain_condition(condition),
        SK::Case {
            expression,
            when_branches,
            ..
        } => simple(expression) && when_branches.iter().all(|w| w.values.iter().all(&simple)),
        SK::Do {
            loop_var: None,
            from: None,
            to: None,
            by: None,
            while_condition: Some(c),
            ..
        }
        | SK::Repeat {
            while_condition: Some(c),
            ..
        } => plain_condition(c),
        SK::Do {
            loop_var: Some(_),
            from: Some(from),
            to: Some(to),
            by,
            while_condition,
            ..
        } => {
            simple(from)
                && simple(to)
                && by.as_ref().is_none_or(simple)
                && while_condition.as_ref().is_none_or(plain_condition)
        }
        SK::Assignment { target, value } => var_of(target).is_some() && simple(value),
        SK::Assign { assignments } => assignments
            .iter()
            .all(|p| var_of(&p.target).is_some() && simple(&p.value)),
        SK::Return(value) => value.as_ref().is_none_or(simple),
        _ => false,
    }
}

/// A condition of variables, literals, comparisons, `AND`, `OR` and `NOT`, plus
/// `VALID-HANDLE(v)`, `VALID-OBJECT(v)` and `ERROR-STATUS:ERROR`, which read a
/// value without raising: no other call, attribute, method, arithmetic or
/// field.
fn plain_condition(e: &Expression) -> bool {
    match &e.kind {
        EK::Literal(_) => true,
        EK::Identifier(_) => var_of(e).is_some() || is_error_status(e),
        EK::MemberAccess { .. } => is_error_status(e),
        EK::FunctionCall { name, arguments } => {
            is_validity_test(&name.name)
                && arguments.len() == 1
                && identity_of(&arguments[0]).is_some()
        }
        EK::Not(a) => plain_condition(a),
        EK::And(a, b)
        | EK::Or(a, b)
        | EK::Equal(a, b)
        | EK::NotEqual(a, b)
        | EK::LessThan(a, b)
        | EK::LessThanOrEqual(a, b)
        | EK::GreaterThan(a, b)
        | EK::GreaterThanOrEqual(a, b) => plain_condition(a) && plain_condition(b),
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
                weak: false,
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
        match derived_root(value) {
            Some(from) => out.push(Effect::DerivedAssign { to, from }),
            None => out.push(Effect::Assign {
                to,
                from: identity_of(value),
            }),
        }
    }
}

/// `RUN destroy`, `destroyObject`, `DestroyThis`: a user procedure whose name
/// starts with `destroy`.
fn is_destroy_name(name: &str) -> bool {
    name.get(..7)
        .is_some_and(|p| p.eq_ignore_ascii_case("destroy"))
}

/// `RUN dispatch IN h ('destroy')`, the ADM form.
fn is_dispatch_destroy(name: &str, arguments: &[oxabl_ast::RunArgument]) -> bool {
    name.eq_ignore_ascii_case("dispatch")
        && arguments.first().is_some_and(|a| {
            matches!(&a.expression.kind, EK::Literal(Literal::String(s))
                if s.value.eq_ignore_ascii_case("destroy"))
        })
}

/// The holder at the root of a chain whose last link yields a handle:
/// `h:DEFAULT-BUFFER-HANDLE`, `h:HANDLE`, `h:GET-BUFFER-HANDLE(n)`, ...
fn derived_root(e: &Expression) -> Option<&str> {
    if identity_of(e).is_some() {
        return None;
    }
    let (root, last) = match &e.kind {
        EK::Identifier(id) => split_chain(&id.name)?,
        EK::FunctionCall { name, .. } => split_chain(&name.name)?,
        EK::MemberAccess { object, member } => (chain_start(object)?, member.name.as_str()),
        EK::MethodCall { object, method, .. } => (chain_start(object)?, method.name.as_str()),
        _ => return None,
    };
    yields_handle(last).then_some(root)
}

/// `h:A:B` is the root `h` and the last link `B`.
fn split_chain(name: &str) -> Option<(&str, &str)> {
    let (root, rest) = name.split_once(':')?;
    let last = rest.rsplit(':').next()?;
    (!root.is_empty() && !root.contains('.')).then_some((root, last))
}

fn chain_start(e: &Expression) -> Option<&str> {
    match &e.kind {
        EK::Identifier(id) => split_chain(&id.name).map(|(r, _)| r).or_else(|| var_of(e)),
        EK::FunctionCall { name, .. } => split_chain(&name.name).map(|(r, _)| r),
        EK::MemberAccess { object, .. } | EK::MethodCall { object, .. } => chain_start(object),
        _ => None,
    }
}

/// Attributes and methods whose value is a handle of the object they are read
/// from.
fn yields_handle(member: &str) -> bool {
    let m = member.as_bytes();
    m.windows(6).any(|w| w.eq_ignore_ascii_case(b"HANDLE"))
        || ["QUERY", "DATASET", "PARENT", "FRAME", "WINDOW"]
            .iter()
            .any(|n| member.eq_ignore_ascii_case(n))
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
            } else if let Some(callee) = function_callee(n) {
                pass_args(arguments, callee, out);
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
            if method.name.eq_ignore_ascii_case("ADD-BUFFER") {
                let recv = var_of(object);
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
            } else {
                let callee = Callee::Method {
                    name: &method.name,
                    recv: receiver(object),
                };
                pass_args(arguments, callee, out);
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

/// What a call by this name runs; `None` for a built-in function.
fn function_callee(name: &str) -> Option<Callee<'_>> {
    if let Some((head, last)) = name.rsplit_once(':') {
        let recv = match split_chain(head) {
            Some((_, last)) if is_handle_attribute(last) => Receiver::Handle,
            Some((root, _)) => Receiver::Chain(root),
            None if !head.is_empty() && !head.contains('.') => Receiver::Name(head),
            None => Receiver::Other,
        };
        Some(Callee::Method { name: last, recv })
    } else if name.contains('.') {
        Some(Callee::External)
    } else if is_builtin_function(name) {
        None
    } else {
        Some(Callee::Function(name))
    }
}

/// What the object of a method call is.
fn receiver(object: &Expression) -> Receiver<'_> {
    if let Some(name) = var_of(object) {
        return Receiver::Name(name);
    }
    let last = match &object.kind {
        EK::Identifier(id) => split_chain(&id.name).map(|(_, last)| last),
        EK::MemberAccess { member, .. } => Some(member.name.as_str()),
        _ => None,
    };
    if last.is_some_and(is_handle_attribute) {
        return Receiver::Handle;
    }
    chain_start(object).map_or(Receiver::Other, Receiver::Chain)
}

/// `HANDLE`, `DEFAULT-BUFFER-HANDLE`, `TABLE-HANDLE`, ...: the value is a
/// handle.
fn is_handle_attribute(name: &str) -> bool {
    let b = name.as_bytes();
    b.len() >= 6 && b[b.len() - 6..].eq_ignore_ascii_case(b"HANDLE")
}

/// A system handle other than `THIS-OBJECT` and `SUPER`, whose methods are
/// built in.
pub(crate) fn is_system_handle(name: &str) -> bool {
    let mut buf = [0u8; 24];
    let Some(folded) = buf.get_mut(..name.len()) else {
        return false;
    };
    folded.copy_from_slice(name.as_bytes());
    folded.make_ascii_lowercase();
    matches!(
        &*folded,
        b"self"
            | b"session"
            | b"error-status"
            | b"error-stat"
            | b"error-statu"
            | b"this-procedure"
            | b"source-procedure"
            | b"target-procedure"
            | b"current-window"
            | b"active-window"
            | b"default-window"
            | b"active-form"
            | b"focus"
            | b"clipboard"
            | b"file-info"
            | b"file-information"
            | b"last-event"
            | b"log-manager"
            | b"dslog-manager"
            | b"web-context"
            | b"com-self"
            | b"debugger"
            | b"profiler"
            | b"compiler"
            | b"codebase-locator"
            | b"color-table"
            | b"font-table"
            | b"rcode-info"
            | b"rcode-information"
            | b"audit-control"
            | b"audit-policy"
            | b"security-policy"
            | b"codepage-table"
    )
}

fn is_builtin_function(name: &str) -> bool {
    let mut buf = [0u8; 64];
    let Some(folded) = buf.get_mut(..name.len()) else {
        return false;
    };
    folded.copy_from_slice(name.as_bytes());
    folded.make_ascii_lowercase();
    std::str::from_utf8(folded).is_ok_and(oxabl_lexer::is_builtin_function)
}

/// Whether `name` is a method of a built-in handle type, which borrows its
/// handle arguments: `SET-BUFFERS`, `ATTACH-DATA-SOURCE`, `FILL`, ...
pub(crate) fn is_builtin_method(name: &str) -> bool {
    name.contains('-')
        || [
            "FILL",
            "READ",
            "WRITE",
            "CONNECT",
            "DISCONNECT",
            "INVOKE",
            "SAVE",
            "LOAD",
            "PARSE",
            "SELECT",
        ]
        .iter()
        .any(|m| name.eq_ignore_ascii_case(m))
}

fn pass_args<'a>(args: &'a [Expression], callee: Callee<'a>, out: &mut Effects<'a>) {
    for (pos, a) in args.iter().enumerate() {
        if let Some(var) = identity_of(a) {
            out.push(Effect::Pass { var, callee, pos });
        }
    }
}

/// Whether the argument is a `TABLE`, `TABLE-HANDLE`, `DATASET` or
/// `DATASET-HANDLE` passed by value: the keyword before it is one of those, and
/// no `BY-REFERENCE` or `BIND` follows it.
fn copied_argument(src: &str, span: Span) -> bool {
    let before = slice(
        src,
        Span {
            start: 0,
            end: span.start,
        },
    )
    .trim_end();
    let word = before
        .rsplit(|c: char| c.is_whitespace() || c == '(' || c == ',')
        .next();
    let table = word.is_some_and(|w| {
        ["TABLE", "TABLE-HANDLE", "DATASET", "DATASET-HANDLE"]
            .iter()
            .any(|k| w.eq_ignore_ascii_case(k))
    });
    let after = src.get(span.end as usize..).unwrap_or("");
    let option = after.split([',', ')', '.']).next().unwrap_or("");
    table
        && !option
            .split_whitespace()
            .any(|w| w.eq_ignore_ascii_case("BY-REFERENCE") || w.eq_ignore_ascii_case("BIND"))
}

fn is_this_procedure(e: &Expression) -> bool {
    matches!(&e.kind, EK::Identifier(id) if id.name.eq_ignore_ascii_case("THIS-PROCEDURE"))
}

/// A `DELETE` or `RELEASE` operand: a free of the variable it is the same
/// object as, else opaque for every variable it mentions.
fn free_operand<'a>(t: &'a Expression, kind: FreeKind, out: &mut Effects<'a>) {
    if let Some(var) = identity_of(t) {
        out.push(Effect::Free { var, kind });
        return;
    }
    let mut names = SmallVec::<[&str; 4]>::new();
    mentions(t, &mut names);
    for var in names {
        out.push(Effect::Opaque {
            var,
            reason: OpaqueReason::UnmappedFree,
        });
    }
}

/// The variables an expression names, by the root of each chain.
fn mentions<'a>(e: &'a Expression, out: &mut SmallVec<[&'a str; 4]>) {
    let root = |name: &'a str| name.split([':', '.']).next().filter(|r| !r.is_empty());
    match &e.kind {
        EK::Identifier(id) => out.extend(root(&id.name)),
        EK::FunctionCall { name, arguments } => {
            if name.name.contains(':') {
                out.extend(root(&name.name));
            }
            for a in arguments {
                mentions(a, out);
            }
        }
        EK::MethodCall {
            object, arguments, ..
        } => {
            mentions(object, out);
            for a in arguments {
                mentions(a, out);
            }
        }
        EK::MemberAccess { object, .. } => mentions(object, out),
        EK::ArrayAccess { array, index } => {
            mentions(array, out);
            mentions(index, out);
        }
        EK::FieldAccess { qualifier, .. } => mentions(qualifier, out),
        EK::Add(a, b) | EK::Minus(a, b) | EK::Multiply(a, b) | EK::Divide(a, b) => {
            mentions(a, out);
            mentions(b, out);
        }
        EK::Negate(a) | EK::Not(a) => mentions(a, out),
        _ => {}
    }
}

/// `COPY-LOB ... TO target`: the statement's target when it is a plain
/// variable (not `TO FILE`), and whether it has `NO-ERROR`. `None` for any
/// other skipped statement. The parser skips the statement, so its tokens are
/// read again here.
fn copy_lob<'a>(span: Span, names: &'a [Identifier], src: &str) -> Option<(Option<&'a str>, bool)> {
    let text = slice(src, span);
    let toks: SmallVec<[_; 16]> = Lexer::new(text)
        .filter(|t| !matches!(t.kind, Kind::Comment))
        .take_while(|t| t.kind != Kind::Eof)
        .collect();
    if toks.first()?.kind != Kind::CopyLob {
        return None;
    }
    let no_error = toks.iter().any(|t| t.kind == Kind::NoError);
    let target = toks
        .iter()
        .position(|t| t.kind == Kind::To)
        .and_then(|i| toks.get(i + 1))
        .map(|t| &text[t.start..t.end])
        .filter(|w| {
            w.starts_with(|c: char| c.is_ascii_alphabetic())
                && !w.contains([':', '.'])
                && !w.eq_ignore_ascii_case("FILE")
        })
        .and_then(|w| {
            names
                .iter()
                .map(|n| n.name.as_str())
                .find(|n| n.eq_ignore_ascii_case(w))
        });
    Some((target, no_error))
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

/// The variable an expression is the same object as: `h`, or `h:HANDLE`.
pub(crate) fn identity_of(e: &Expression) -> Option<&str> {
    match &e.kind {
        EK::Identifier(id) => match id.name.split_once(':') {
            None => var_of(e),
            Some((root, rest))
                if rest.eq_ignore_ascii_case("HANDLE")
                    && !root.is_empty()
                    && !root.contains('.') =>
            {
                Some(root)
            }
            _ => None,
        },
        EK::MemberAccess { object, member } if member.name.eq_ignore_ascii_case("HANDLE") => {
            var_of(object)
        }
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

/// Whether the statement at `span` runs a procedure, calls a function or
/// method, reads an attribute, or touches a database record: where a raised
/// condition most likely comes from.
pub(crate) fn calls_something(src: &str, span: Span) -> bool {
    let text = slice(src, span);
    let mut prev: Option<(Kind, bool)> = None;
    let (mut lead, mut second_is_name) = (Kind::Eof, false);
    // A `:` is a member access unless it ends the text, where it opens a block.
    let mut colon = false;
    for (n, t) in Lexer::new(text)
        .filter(|t| !matches!(t.kind, Kind::Comment))
        .take_while(|t| t.kind != Kind::Eof)
        .enumerate()
    {
        if colon {
            return true;
        }
        let word = &text[t.start..t.end];
        match n {
            0 => lead = t.kind,
            1 => second_is_name = t.kind == Kind::Identifier,
            _ => {}
        }
        colon = t.kind == Kind::Colon;
        if t.kind == Kind::Identifier && word.contains(':') {
            return true;
        }
        if t.kind == Kind::LeftParen
            && let Some((k, true)) = prev
            && !matches!(
                k,
                Kind::KwIf
                    | Kind::KwWhile
                    | Kind::KwElse
                    | Kind::KwReturn
                    | Kind::And
                    | Kind::Or
                    | Kind::Not
                    | Kind::Then
                    | Kind::When
                    | Kind::Case
                    | Kind::Do
            )
        {
            return true;
        }
        prev = Some((t.kind, word.starts_with(|c: char| c.is_ascii_alphabetic())));
    }
    match lead {
        Kind::Run
        | Kind::Find
        | Kind::KwFor
        | Kind::Update
        | Kind::Insert
        | Kind::Validate
        | Kind::Get => true,
        Kind::Create | Kind::Delete => second_is_name,
        _ => false,
    }
}
