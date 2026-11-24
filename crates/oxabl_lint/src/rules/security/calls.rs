//! A common view of the calls in a program, for the rules that match a call by
//! its name and read its arguments.
//!
//! [`scan`] walks a program and hands each statement and each call to a
//! callback. A call is a function call, a method call (`obj:Name(...)`) or a
//! `NEW` of a class. Calls in modelled expressions come from the tree. Calls in
//! the statements the parser does not model, or models only in part
//! ([`is_text_read`]), come from lexing the statement text, as the stage-1
//! rules already do; a `NEW` inside such a statement is not seen.
//!
//! An [`Arg`] says what a rule needs to know about one argument without a
//! second look at the tree or the tokens: is it constant, what string does it
//! spell, which names and calls does it mention.

use oxabl_ast::{Expression, ExpressionKind, Literal, Span, Statement};
use oxabl_lexer::Kind;
use oxabl_semantic::AnalysisContext;

use super::literal::{StatementText, cover, is_constant, is_text_read, mentions};
use crate::rules::syntactic::{Visitor, walk_block};

/// One argument of a [`Call`].
#[derive(Debug, Clone)]
pub struct Arg {
    pub span: Span,
    /// Literal, or built only from literals. A `NEW` whose arguments are all
    /// constant counts, since the value it builds cannot change.
    pub constant: bool,
    /// The text, when the argument is a string literal or a `+` of string
    /// literals.
    pub literal: Option<String>,
    /// Names the argument mentions: variables, fields and members.
    pub names: Vec<String>,
    /// Functions and methods the argument calls, and classes it instantiates.
    pub calls: Vec<String>,
}

/// How a call is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallKind {
    Function,
    Method,
    New,
}

/// A function call, method call or `NEW`.
#[derive(Debug, Clone)]
pub struct Call {
    /// The function or method name, or the last segment of the class name for
    /// a `NEW`.
    pub name: String,
    /// The last name in the receiver of a method call (`oResp` for
    /// `THIS-OBJECT:oResp:Write(x)`).
    pub receiver: Option<String>,
    pub args: Vec<Arg>,
    /// From the name to the closing parenthesis.
    pub span: Span,
    pub kind: CallKind,
}

impl Call {
    /// Whether the call is named `name`, ignoring case.
    pub fn is(&self, name: &str) -> bool {
        self.name.eq_ignore_ascii_case(name)
    }

    /// Whether the receiver is named `name`, ignoring case.
    pub fn receiver_is(&self, name: &str) -> bool {
        self.receiver
            .as_deref()
            .is_some_and(|r| r.eq_ignore_ascii_case(name))
    }

    /// Whether any argument is not constant.
    pub fn has_dynamic_arg(&self) -> bool {
        self.args.iter().any(|a| !a.constant)
    }
}

/// What [`scan`] reports.
pub enum Site<'a> {
    Statement(&'a Statement),
    Call(Call),
}

/// Call `f` for every statement and every call in `program`.
///
/// `needles` is a cheap filter for skipped statements: one is lexed only when
/// its text holds one of the words, ignoring case.
pub fn scan(
    ctx: &AnalysisContext,
    program: &[Statement],
    needles: &[&str],
    f: &mut dyn FnMut(Site),
) {
    let mut scanner = Scanner { ctx, needles, f };
    walk_block(&mut scanner, program);
}

struct Scanner<'a, 'f> {
    ctx: &'a AnalysisContext<'a>,
    needles: &'a [&'a str],
    f: &'f mut dyn FnMut(Site),
}

impl Visitor for Scanner<'_, '_> {
    fn statement(&mut self, stmt: &Statement) -> bool {
        (self.f)(Site::Statement(stmt));
        if !is_text_read(stmt) {
            return true;
        }
        if mentions(self.ctx.source, stmt, self.needles)
            && let Some(st) = StatementText::of(self.ctx.source, stmt)
        {
            for call in token_calls(&st) {
                (self.f)(Site::Call(call));
            }
        }
        false
    }

    fn expression(&mut self, expr: &Expression) {
        if let Some(call) = expression_call(expr) {
            (self.f)(Site::Call(call));
        }
    }
}

/// The call that `expr` is, if it is one.
pub fn expression_call(expr: &Expression) -> Option<Call> {
    match &expr.kind {
        ExpressionKind::FunctionCall { name, arguments } => Some(Call {
            name: name.name.clone(),
            receiver: None,
            args: arguments.iter().map(arg_of).collect(),
            span: expr.span,
            kind: CallKind::Function,
        }),
        ExpressionKind::MethodCall {
            object,
            method,
            arguments,
        } => Some(Call {
            name: method.name.clone(),
            receiver: last_name(object).map(str::to_string),
            args: arguments.iter().map(arg_of).collect(),
            span: cover(method.span, expr.span, expr.span),
            kind: CallKind::Method,
        }),
        ExpressionKind::New {
            class_name,
            arguments,
            ..
        } => Some(Call {
            name: class_name
                .rsplit('.')
                .next()
                .unwrap_or(class_name)
                .to_string(),
            receiver: None,
            args: arguments.iter().map(arg_of).collect(),
            span: expr.span,
            kind: CallKind::New,
        }),
        _ => None,
    }
}

/// The argument view of `expr`.
pub fn arg_of(expr: &Expression) -> Arg {
    let mut names = Vec::new();
    let mut calls = Vec::new();
    collect(expr, &mut names, &mut calls);
    Arg {
        span: expr.span,
        constant: constant(expr),
        literal: string_literal(expr),
        names,
        calls,
    }
}

/// Like [`is_constant`], and a `NEW` whose arguments are all constant is too.
pub fn constant(expr: &Expression) -> bool {
    match &expr.kind {
        ExpressionKind::New { arguments, .. } => arguments.iter().all(constant),
        _ => is_constant(expr),
    }
}

/// The text of a string literal or a `+` of string literals.
pub fn string_literal(expr: &Expression) -> Option<String> {
    match &expr.kind {
        ExpressionKind::Literal(Literal::String(s)) => Some(s.value.clone()),
        ExpressionKind::Add(l, r) => Some(string_literal(l)? + &string_literal(r)?),
        _ => None,
    }
}

/// The last name that an expression spells: a variable, a field, a member, or
/// the callee of a call.
pub fn last_name(expr: &Expression) -> Option<&str> {
    match &expr.kind {
        ExpressionKind::Identifier(id) => Some(&id.name),
        ExpressionKind::MemberAccess { member, .. } => Some(&member.name),
        ExpressionKind::FieldAccess { field, .. } => Some(&field.name),
        ExpressionKind::MethodCall { method, .. } => Some(&method.name),
        ExpressionKind::FunctionCall { name, .. } => Some(&name.name),
        ExpressionKind::ArrayAccess { array, .. } => last_name(array),
        ExpressionKind::New { class_name, .. } => class_name.rsplit('.').next(),
        _ => None,
    }
}

fn children<'a>(expr: &'a Expression, f: &mut dyn FnMut(&'a Expression)) {
    match &expr.kind {
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
            f(l);
            f(r);
        }
        ExpressionKind::Negate(e)
        | ExpressionKind::Not(e)
        | ExpressionKind::MemberAccess { object: e, .. }
        | ExpressionKind::FieldAccess { qualifier: e, .. } => f(e),
        ExpressionKind::IfThenElse(c, t, e) => {
            f(c);
            f(t);
            f(e);
        }
        ExpressionKind::FunctionCall { arguments, .. } | ExpressionKind::New { arguments, .. } => {
            arguments.iter().for_each(f)
        }
        ExpressionKind::MethodCall {
            object, arguments, ..
        } => {
            f(object);
            arguments.iter().for_each(f);
        }
        ExpressionKind::CanFind { where_clause, .. } => {
            if let Some(w) = where_clause {
                f(w);
            }
        }
        ExpressionKind::Literal(_)
        | ExpressionKind::Identifier(_)
        | ExpressionKind::IncludeReference { .. }
        | ExpressionKind::IncludeArgReference { .. }
        | ExpressionKind::PreprocReference(_)
        | ExpressionKind::PreprocIf(_) => {}
    }
}

fn collect(expr: &Expression, names: &mut Vec<String>, calls: &mut Vec<String>) {
    match &expr.kind {
        ExpressionKind::Identifier(id) => names.push(id.name.clone()),
        ExpressionKind::MemberAccess { member, .. } => names.push(member.name.clone()),
        ExpressionKind::FieldAccess { field, .. } => names.push(field.name.clone()),
        ExpressionKind::FunctionCall { name, .. } => calls.push(name.name.clone()),
        ExpressionKind::MethodCall { method, .. } => calls.push(method.name.clone()),
        ExpressionKind::New { class_name, .. } => {
            calls.push(
                class_name
                    .rsplit('.')
                    .next()
                    .unwrap_or(class_name)
                    .to_string(),
            );
        }
        _ => {}
    }
    children(expr, &mut |child| collect(child, names, calls));
}

/// Whether `pred` holds for `expr` or any expression beneath it.
pub fn any_expr(expr: &Expression, pred: &mut dyn FnMut(&Expression) -> bool) -> bool {
    if pred(expr) {
        return true;
    }
    let mut found = false;
    children(expr, &mut |child| {
        if !found {
            found = any_expr(child, pred);
        }
    });
    found
}

/// The calls in a skipped statement: each word followed by `(`.
pub fn token_calls(st: &StatementText) -> Vec<Call> {
    let mut out = Vec::new();
    for i in 0..st.len() {
        if !st.is(i + 1, Kind::LeftParen) || st.is(i, Kind::RightParen) {
            continue;
        }
        let Some(close) = st.close_of(i + 1) else {
            continue;
        };
        let is_method = i >= 2 && st.is(i - 1, Kind::Colon);
        out.push(Call {
            name: st.word(i).to_string(),
            receiver: is_method
                .then(|| st.word(i - 2).to_string())
                .filter(|r| r.chars().all(|c| c.is_alphanumeric() || "-_.".contains(c))),
            args: st
                .call_args(i + 1)
                .into_iter()
                .map(|range| token_arg(st, range))
                .collect(),
            span: st.span(i..close + 1),
            kind: if is_method {
                CallKind::Method
            } else {
                CallKind::Function
            },
        });
    }
    out
}

/// The text of the string literal token at `i`, without its quotes.
pub fn token_string(st: &StatementText, i: usize) -> String {
    let word = st.word(i);
    let inner = word.get(1..word.len().saturating_sub(1)).unwrap_or("");
    inner.to_string()
}

fn token_arg(st: &StatementText, range: std::ops::Range<usize>) -> Arg {
    let mut names = Vec::new();
    let mut calls = Vec::new();
    let mut text = String::new();
    let mut only_strings = !range.is_empty();
    for i in range.clone() {
        match st.token(i).kind {
            Kind::StringLiteral => text.push_str(&token_string(st, i)),
            Kind::Plus => {}
            Kind::Add if st.word(i) == "+" => {}
            Kind::LeftParen | Kind::RightParen => only_strings = false,
            kind => {
                only_strings = false;
                if st.is(i + 1, Kind::LeftParen) {
                    calls.push(st.word(i).to_string());
                } else if kind == Kind::Identifier {
                    names.push(st.word(i).to_string());
                }
            }
        }
    }
    Arg {
        span: st.span(range.clone()),
        constant: st.is_constant(range),
        literal: only_strings.then_some(text),
        names,
        calls,
    }
}
