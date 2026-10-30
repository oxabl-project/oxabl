//! `client-identity` (ABLSEC010), group `security`.
//!
//! Fires where a decision reads a user, company or role that the client sent
//! with the request. The client can send any value it likes, so a check of
//! that value decides nothing: the caller names itself an administrator, or
//! asks for another company's records.
//!
//! ```abl
//! IF get-value("role") = "admin" THEN RUN purge-orders.
//! FOR EACH Order NO-LOCK WHERE Order.Company = get-field("company"):
//! ```
//!
//! Fixed — take the identity from the session that authenticated the caller:
//!
//! ```abl
//! hPrincipal = SESSION:CURRENT-REQUEST-INFO:GetClientPrincipal().
//! IF CAN-DO(hPrincipal:ROLES, "admin") THEN RUN purge-orders.
//! ```
//!
//! # Identity
//!
//! A request value is an identity when the key it is read by has a word of
//! `identity-keys` (`[workspace.lint.security]`; default `user`, `username`,
//! `userid`, `login`, `company`, `tenant`, `role`, `roles`, `owner`,
//! `admin`): `get-value("user")`, `get-field("company-id")`,
//! `poRequest:GetPathParameter("tenant")`, and `o:GetCharacter("userId")` on
//! an object that holds request input. `identity-sources` names project
//! functions whose result is the client's claim. The value is followed with
//! the taint analysis (in every mode), through variables, fields and the
//! routines of the file. A comparison, a number conversion or a guard does
//! not clean it: the checked value is still the client's. A logical value
//! made from it holds it too: `l = role = "admin"` and then `IF l`, or a
//! function of the file that returns `p = "admin"` for its parameter. A
//! parameter is no identity of its own; a call puts back what its argument
//! holds. A validator that `protects = ["identity"]` cleans it.
//!
//! # Decisions
//!
//! The condition of `IF` (the statement and the expression), the selector
//! and the `WHEN` values of `CASE` (`CASE TRUE: WHEN c = "admin"`), the
//! condition of `DO WHILE` and `REPEAT WHILE`, the `WHERE` of `FOR EACH`,
//! `FIND` and `CAN-FIND`, the key of `FIND`, the `WHEN` of a `DISPLAY` item,
//! and an argument of an `auth-routines` routine
//! (`[workspace.lint.security.reach]`). So is every `WHERE`, `WHILE` and
//! `WHEN` that the parser does not put in a node: of a joined record phrase,
//! of `FOR EACH ... WHILE`, of `PRESELECT` and `OPEN QUERY`, of an `ASSIGN`
//! pair; and the condition of an `IF` expression in a statement the parser
//! does not model. So is the `WHEN` and the `VALIDATE(condition, message)`
//! of a format phrase of `UPDATE`, `SET`, `DISPLAY`, `ENABLE` and `FORM`. `statement_decision` and `expression_decision` list every
//! kind of the AST, so a new kind does not build until it is a decision or
//! named as none. A value that is only displayed or stored is not reported.
//!
//! # One finding per routine and origin
//!
//! A value read once usually decides many statements of its routine: every
//! `FIND` and `FOR EACH` of the company the caller named. The bug is the
//! read, so the rule reports once per routine and source: at the first
//! decision that uses it, with the number of decisions in the message and the
//! others (at most [`MAX_RELATED`]) as related locations. A decision added or
//! removed below the first does not move the finding, so a baseline keeps it.
//!
//! # Mode
//!
//! The rule asks where a value comes from, so it runs only in taint mode
//! (`mode = "taint"`, or `ABLSEC010 = "taint"` in `modes`). In sink mode it
//! is silent: turning on the `security` group in sink mode does not add it to
//! a baseline.
//!
//! # Not judged
//!
//! A work budget bounds the analysis of each routine, and a number of rounds
//! ([`super::taint::ROUNDS`]) bounds the summaries of a file. A decision whose
//! value the analysis did not finish may read an identity, so neither bound
//! may hide it: the routine gets one finding per reason, at its first such
//! decision, with the number of such decisions. It says "authorization could
//! not be judged: analysis budget exceeded" when its routine, or one whose
//! value reaches it, ran past the budget, and "authorization could not be
//! judged: the values of this file did not settle in the analysis" when the
//! summaries of the file did not stop changing in the rounds.
//!
//! A decision on a value the analysis cannot see (an unexpanded reference, a
//! unit where an include did not arrive) is not judged either, and says
//! "authorization could not be judged: the analysis cannot see a value it
//! reads".
//!
//! # What is not reported
//!
//! The identity of the session (`USERID`, the client principal), a value of
//! another program (a shared variable, a parameter of a routine that code
//! outside the file calls, unless it is read by an identity key: the identity
//! is followed inside one file), and any decision in a file whose parse
//! recovered.

use oxabl_ast::{Expression, ExpressionKind, RunTarget, Span, Statement, StatementKind};
use oxabl_common::{Diagnostic, Label, SecurityMode, Severity};
use oxabl_lexer::Kind;
use oxabl_semantic::{AnalysisContext, Semantic};

use super::literal::{StatementText, ends_with_word, leading_word};
use super::reach::{names_routine, unit_at};
use super::taint::{Gate, SinkKind, Unfinished};
use super::{ABLSEC010, finding, meta};
use crate::registry::RuleMeta;
use crate::rules::syntactic::{Visitor, file_span, walk_block};
use crate::shared::RunShared;

/// Kebab-case rule name.
pub const NAME: &str = "client-identity";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// Security severity on the 0.0 to 10.0 scale, as a decimal string.
pub const SECURITY_SEVERITY: &str = "8.0";
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "A decision reads a user, company or role that the client sent with the request";

/// Remediation text, shown with each finding and in rule listings.
pub const HELP: &str = "Take the user, company and role from the session that authenticated the \
     caller (the client principal), never from a request parameter, header or body.";

/// Remediation text of a routine past the work budget.
pub const UNJUDGED_HELP: &str = "The routine is too large for the analysis to follow its values. \
     Check by hand that its decisions take the user, company and role from the session, or split \
     it into smaller routines.";

/// Remediation text of a decision on a value the analysis cannot see.
pub const UNSEEN_HELP: &str = "A value that this decision reads comes from text the analysis \
     cannot read: a preprocessor reference that did not expand, or an include that did not \
     arrive (check the include paths). Check by hand that the decisions of this routine take the \
     user, company and role from the session.";

/// Remediation text of a routine in a file whose summaries did not settle.
pub const UNSETTLED_HELP: &str = "The values of this file pass through more routines, one after \
     another, than the analysis follows. Check by hand that the decisions of this routine take \
     the user, company and role from the session.";

/// Registry metadata.
pub const META: RuleMeta = meta(
    ABLSEC010,
    NAME,
    DESCRIPTION,
    DEFAULT_SEVERITY,
    SECURITY_SEVERITY,
    HELP,
);

/// The most further decisions a finding lists as related locations.
pub const MAX_RELATED: usize = 20;

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    check(program, ctx, &RunShared::default())
}

/// [`run`] with the facts of a registry run.
pub fn check(program: &[Statement], ctx: &AnalysisContext, shared: &RunShared) -> Vec<Diagnostic> {
    if ctx.lint_severities.security().mode_for(ABLSEC010, NAME) != SecurityMode::Taint {
        return Vec::new();
    }
    let mut v = Decisions {
        gate: Gate::new(ABLSEC010, NAME, program, ctx, shared),
        source: ctx.source,
        auth: &ctx.lint_severities.security().reach.auth_routines,
        reported: Vec::new(),
        hits: Vec::new(),
    };
    walk_block(&mut v, program);
    let flow = shared.flow(program, ctx);
    let mut hits = v.hits;
    hits.sort_by_key(|h| h.span.start);
    let mut groups: Vec<(Group, Vec<Hit>)> = Vec::new();
    for h in hits {
        let unit = unit_at(flow, h.span.start).map(|(u, _)| u);
        let origin = match (h.judgement.unjudged, &h.judgement.origin) {
            (Some(why), _) => format!("unjudged {why:?}"),
            (None, Some((span, _))) => format!("{}:{}", span.start, span.end),
            (None, None) => h.judgement.chain.clone(),
        };
        let key = (unit, origin);
        match groups.iter_mut().find(|(k, _)| *k == key) {
            Some((_, list)) => list.push(h),
            None => groups.push((key, vec![h])),
        }
    }
    groups
        .into_iter()
        .map(|(_, list)| report(ctx, list))
        .collect()
}

/// A routine (its unit, when one holds the decision) and an origin.
type Group = (Option<usize>, String);

/// One decision an identity reaches.
struct Hit {
    span: Span,
    word: String,
    judgement: super::taint::Judgement,
}

/// The finding of one routine and origin: at its first decision.
fn report(ctx: &AnalysisContext, mut list: Vec<Hit>) -> Diagnostic {
    let count = list.len();
    let rest: Vec<Label> = list
        .iter()
        .skip(1)
        .take(MAX_RELATED)
        .map(|h| Label {
            span: file_span(ctx, h.span),
            message: if h.judgement.unjudged.is_some() {
                format!("`{}` was not judged either", h.word)
            } else {
                format!("`{}` decides on the same identity", h.word)
            },
        })
        .collect();
    let first = list.swap_remove(0);
    let decisions = if count == 1 {
        "1 decision".to_string()
    } else {
        format!("{count} decisions")
    };
    if let Some(why) = first.judgement.unjudged {
        let (reason, help) = match why {
            Unfinished::Budget => ("analysis budget exceeded", UNJUDGED_HELP),
            Unfinished::Unsettled => (
                "the values of this file did not settle in the analysis",
                UNSETTLED_HELP,
            ),
            Unfinished::Unseen => ("the analysis cannot see a value it reads", UNSEEN_HELP),
        };
        let mut d = finding(
            ctx,
            DEFAULT_SEVERITY,
            ABLSEC010,
            first.span,
            format!(
                "`{}`: authorization could not be judged: {reason}; \
                 {decisions} of this routine {} not judged",
                first.word,
                if count == 1 { "was" } else { "were" }
            ),
            help,
        );
        d.related = (!rest.is_empty()).then(|| Box::new(rest));
        return d;
    }
    let origin = first
        .judgement
        .origin
        .as_ref()
        .map_or("the request", |(_, what)| what.as_str());
    let d = finding(
        ctx,
        DEFAULT_SEVERITY,
        ABLSEC010,
        first.span,
        format!(
            "`{}` decides on an identity the client supplied: {}; {decisions} of this routine \
             use {origin}",
            first.word, first.judgement.chain
        ),
        HELP,
    );
    let mut d = first.judgement.attach(d);
    d.related = (!rest.is_empty()).then(|| Box::new(rest));
    d
}

struct Decisions<'a> {
    gate: Gate<'a>,
    source: &'a str,
    auth: &'a [String],
    /// The operands of the decisions found: a `CAN-FIND` inside one is not
    /// counted again.
    reported: Vec<Span>,
    hits: Vec<Hit>,
}

impl Decisions<'_> {
    /// Note the decision whose keyword is `word` at `at` when an identity
    /// reaches one of `operands`.
    fn decide(&mut self, at: u32, word: &str, operands: &[Span]) {
        let inside = |s: &Span| s.start <= at && at < s.end;
        if self.reported.iter().any(inside) {
            return;
        }
        let Some(j) = self.gate.judge_decision(operands, SinkKind::Identity) else {
            return;
        };
        self.reported.extend_from_slice(operands);
        self.hits.push(Hit {
            span: Span {
                start: at,
                end: at + word.len() as u32,
            },
            word: word.to_string(),
            judgement: j,
        });
    }

    fn is_auth(&self, name: &str) -> bool {
        self.auth.iter().any(|p| names_routine(p, &[name]))
    }

    /// The `WHERE`, `WHILE`, `WHEN`, `IF` and `VALIDATE` conditions of a
    /// statement that its node does not hold: in the ranges the parser did not
    /// model, or anywhere in a statement it skipped whole.
    fn unmodelled(&mut self, s: &Statement) {
        let ranges: Vec<Span> = match (&s.kind, &s.skipped_tail) {
            (StatementKind::Skipped { .. }, _) => vec![s.span],
            (_, Some(tail)) => tail.unmodelled.clone(),
            _ => return,
        };
        let source = self.source;
        let Some(word) = leading_word(source, s) else {
            return;
        };
        for range in ranges {
            let Some(st) = StatementText::over(source, range) else {
                continue;
            };
            let before = source.get(..range.start as usize).unwrap_or_default();
            let mut operands = Vec::new();
            if CONDITIONS.iter().any(|w| ends_with_word(before, w)) {
                operands.push(st.span(0..clause_end(&st, 0)));
            }
            let mut depth = 0usize;
            for i in 0..st.len() {
                match st.token(i).kind {
                    Kind::LeftParen => depth += 1,
                    Kind::RightParen => depth = depth.saturating_sub(1),
                    _ if depth == 0 && CONDITIONS.iter().any(|w| st.is_word(i, w)) => {
                        operands.push(st.span(i + 1..clause_end(&st, i + 1)));
                    }
                    // The condition of a `VALIDATE(condition, message)`
                    // phrase, not of the `VALIDATE` method of a handle.
                    _ if st.is_word(i, "VALIDATE")
                        && st.is(i + 1, Kind::LeftParen)
                        && !(i > 0 && st.is(i - 1, Kind::Colon)) =>
                    {
                        if let Some(first) = st.call_args(i + 1).into_iter().next() {
                            operands.push(st.span(first));
                        }
                    }
                    // The condition of an `IF` expression, at any depth.
                    _ if st.is_word(i, "IF") => {
                        if let Some(then) = (i + 1..st.len()).find(|&j| st.is_word(j, "THEN")) {
                            operands.push(st.span(i + 1..then));
                        }
                    }
                    _ => {}
                }
            }
            for op in operands.into_iter().filter(|op| op.start < op.end) {
                self.decide(s.span.start, &word, &[op]);
            }
        }
    }
}

impl Decisions<'_> {
    /// The `WHEN` and `VALIDATE(condition, message)` options of the format
    /// phrases of a frame statement, which the parser puts in its node
    /// (`UPDATE x VALIDATE(...)`, `ENABLE x WHEN c`). The ranges it did not
    /// model are read by [`Self::unmodelled`]; the `WHEN` of a `DISPLAY` item
    /// is a decision of the statement.
    fn format_phrases(&mut self, s: &Statement) {
        use StatementKind as S;
        let (exprs, when): (Vec<Span>, bool) = match &s.kind {
            S::Update { options, .. } | S::Set { options, .. } => {
                (options.iter().map(|e| e.span).collect(), true)
            }
            S::Enable { items } | S::Form { items, .. } => {
                (items.iter().map(|e| e.span).collect(), true)
            }
            S::Display { .. } => (Vec::new(), false),
            _ => return,
        };
        let source = self.source;
        let (Some(st), Some(word)) = (StatementText::of(source, s), leading_word(source, s)) else {
            return;
        };
        let skipped = s
            .skipped_tail
            .as_ref()
            .map_or(&[][..], |t| &t.unmodelled[..]);
        let mut operands = Vec::new();
        let mut depth = 0usize;
        for i in 0..st.len() {
            let at = st.span(i..i + 1).start;
            if skipped.iter().any(|r| r.start <= at && at < r.end) {
                continue;
            }
            match st.token(i).kind {
                Kind::LeftParen => depth += 1,
                Kind::RightParen => depth = depth.saturating_sub(1),
                _ if depth == 0 && when && st.is_word(i, "WHEN") && i + 1 < st.len() => {
                    // The expression the parser read after `WHEN`, else the
                    // clause.
                    let next = st.span(i + 1..i + 2).start;
                    operands.push(
                        exprs
                            .iter()
                            .copied()
                            .find(|e| e.start == next)
                            .unwrap_or_else(|| st.span(i + 1..clause_end(&st, i + 1))),
                    );
                }
                _ if st.is_word(i, "VALIDATE")
                    && st.is(i + 1, Kind::LeftParen)
                    && !(i > 0 && st.is(i - 1, Kind::Colon)) =>
                {
                    if let Some(first) = st.call_args(i + 1).into_iter().next() {
                        operands.push(st.span(first));
                    }
                }
                _ => {}
            }
        }
        for op in operands.into_iter().filter(|op| op.start < op.end) {
            self.decide(s.span.start, &word, &[op]);
        }
    }
}

/// The words that start a condition.
const CONDITIONS: [&str; 3] = ["WHERE", "WHILE", "WHEN"];

/// Where the clause that starts at `from` ends: at the next condition, sort,
/// or joined record phrase at the top level, or at the end.
fn clause_end(st: &StatementText, from: usize) -> usize {
    let mut depth = 0usize;
    for i in from..st.len() {
        match st.token(i).kind {
            Kind::LeftParen => depth += 1,
            Kind::RightParen => depth = depth.saturating_sub(1),
            Kind::Comma if depth == 0 => return i,
            _ if depth == 0
                && ["WHERE", "WHILE", "WHEN", "BY", "BREAK"]
                    .iter()
                    .any(|w| st.is_word(i, w)) =>
            {
                return i;
            }
            _ => {}
        }
    }
    st.len()
}

/// The decision a statement makes: its keyword and the operands that decide
/// it. Every kind is listed, so a new kind does not build until it is a
/// decision or named as none.
fn statement_decision(
    s: &Statement,
    auth: &dyn Fn(&str) -> bool,
) -> Option<(&'static str, Vec<Span>)> {
    use StatementKind as S;
    let decision = |word, spans: Vec<Span>| (!spans.is_empty()).then_some((word, spans));
    match &s.kind {
        S::If { condition, .. } => decision("IF", vec![condition.span]),
        // The selector and each `WHEN` value: `CASE TRUE: WHEN c = "admin"`
        // decides in its values.
        S::Case {
            expression,
            when_branches,
            ..
        } => decision(
            "CASE",
            std::iter::once(expression.span)
                .chain(
                    when_branches
                        .iter()
                        .flat_map(|w| w.values.iter().map(|v| v.span)),
                )
                .collect(),
        ),
        // A loop's bounds are not a decision.
        S::Do {
            while_condition, ..
        } => decision("DO", while_condition.iter().map(|w| w.span).collect()),
        S::Repeat {
            while_condition, ..
        } => decision("REPEAT", while_condition.iter().map(|w| w.span).collect()),
        S::ForEach { where_clause, .. } => {
            decision("FOR", where_clause.iter().map(|w| w.span).collect())
        }
        // `FIND Customer cKey` selects by its key value as a `WHERE` does.
        S::Find {
            key_value,
            where_clause,
            ..
        } => decision(
            "FIND",
            key_value
                .iter()
                .chain(where_clause)
                .map(|e| e.span)
                .collect(),
        ),
        // `DISPLAY x WHEN c`: the condition decides whether `x` is shown.
        S::Display { items, .. } => decision(
            "DISPLAY",
            items
                .iter()
                .filter_map(|i| i.when_condition.as_ref().map(|w| w.span))
                .collect(),
        ),
        S::Run {
            target: RunTarget::Literal { name, .. },
            arguments,
            ..
        } if auth(name) => decision("RUN", arguments.iter().map(|a| a.expression.span).collect()),
        // Read from its tokens (`Decisions::unmodelled`).
        S::Skipped { .. } => None,
        // No condition: values, targets, definitions, blocks, events and the
        // preprocessor, which decides at compile time. A `WHEN`, `WHERE` or
        // `WHILE` the parser does not hold is read from the tokens.
        S::Run { .. }
        | S::VariableDeclaration { .. }
        | S::VariableDeclarationList(_)
        | S::Assignment { .. }
        | S::ExpressionStatement(_)
        | S::Block(_)
        | S::Procedure { .. }
        | S::DefineParameter { .. }
        | S::IncludeReference { .. }
        | S::IncludeArgReference { .. }
        | S::Message { .. }
        | S::Put { .. }
        | S::Export { .. }
        | S::Import { .. }
        | S::Pause { .. }
        | S::Down { .. }
        | S::Form { .. }
        | S::Update { .. }
        | S::Set { .. }
        | S::Apply { .. }
        | S::WaitFor { .. }
        | S::Enable { .. }
        | S::Disable { .. }
        | S::Hide { .. }
        | S::View { .. }
        | S::Up { .. }
        | S::DefineTempTable { .. }
        | S::DefineBuffer { .. }
        | S::Catch { .. }
        | S::Finally { .. }
        | S::Throw(_)
        | S::Assign { .. }
        | S::Function { .. }
        | S::Class { .. }
        | S::Method { .. }
        | S::Property { .. }
        | S::Constructor { .. }
        | S::Destructor { .. }
        | S::Interface { .. }
        | S::Using { .. }
        | S::Create { .. }
        | S::Delete { .. }
        | S::DeleteObject { .. }
        | S::DeleteWidgetPool { .. }
        | S::CreateWidgetPool { .. }
        | S::Release { .. }
        | S::ReleaseObject { .. }
        | S::Validate { .. }
        | S::BufferCopy { .. }
        | S::BufferCompare { .. }
        | S::PreprocIf(_)
        | S::PreprocDefine { .. }
        | S::PreprocUndefine { .. }
        | S::PreprocMessage { .. }
        | S::DefineStream { .. }
        | S::DefineFrame { .. }
        | S::DefineWidget { .. }
        | S::DefineDataset { .. }
        | S::DefineDataSource { .. }
        | S::Publish { .. }
        | S::Subscribe { .. }
        | S::Unsubscribe { .. }
        | S::DefineEvent { .. }
        | S::StreamIo { .. }
        | S::On { .. }
        | S::TriggerProcedure { .. }
        | S::Leave(_)
        | S::Next(_)
        | S::Return(_)
        | S::Empty
        | S::Annotation(_)
        | S::Label { .. } => None,
    }
}

/// The decision an expression makes: its keyword and the operands that
/// decide it. Every kind is listed, as in [`statement_decision`].
fn expression_decision(e: &Expression, auth: &dyn Fn(&str) -> bool) -> Option<(String, Vec<Span>)> {
    use ExpressionKind as E;
    match &e.kind {
        E::CanFind {
            where_clause: Some(w),
            ..
        } => Some(("CAN-FIND".to_string(), vec![w.span])),
        // `IF c THEN "purge.p" ELSE "noop.p"` picks what runs.
        E::IfThenElse(c, ..) => Some(("IF".to_string(), vec![c.span])),
        E::FunctionCall { name, arguments } if auth(&name.name) => Some((
            name.name.clone(),
            arguments.iter().map(|a| a.span).collect(),
        )),
        // Values and operators, which a decision reads; the preprocessor
        // decides at compile time.
        E::Literal(_)
        | E::Identifier(_)
        | E::Add(..)
        | E::Minus(..)
        | E::Multiply(..)
        | E::Divide(..)
        | E::Modulo(..)
        | E::Equal(..)
        | E::NotEqual(..)
        | E::LessThan(..)
        | E::LessThanOrEqual(..)
        | E::GreaterThan(..)
        | E::GreaterThanOrEqual(..)
        | E::Begins(..)
        | E::Matches(..)
        | E::Contains(..)
        | E::And(..)
        | E::Or(..)
        | E::Negate(_)
        | E::Not(_)
        | E::FunctionCall { .. }
        | E::MemberAccess { .. }
        | E::MethodCall { .. }
        | E::ArrayAccess { .. }
        | E::FieldAccess { .. }
        | E::IncludeReference { .. }
        | E::IncludeArgReference { .. }
        | E::PreprocReference(_)
        | E::PreprocIf(_)
        | E::New { .. }
        | E::CanFind { .. } => None,
    }
}

impl Visitor for Decisions<'_> {
    fn statement(&mut self, s: &Statement) -> bool {
        let auth = |name: &str| self.is_auth(name);
        if let Some((word, spans)) = statement_decision(s, &auth) {
            self.decide(s.span.start, word, &spans);
        }
        self.unmodelled(s);
        self.format_phrases(s);
        true
    }

    fn expression(&mut self, e: &Expression) {
        let auth = |name: &str| self.is_auth(name);
        if let Some((word, spans)) = expression_decision(e, &auth) {
            self.decide(e.span.start, &word, &spans);
        }
    }
}
