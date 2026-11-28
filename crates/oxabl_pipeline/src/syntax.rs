//! Syntax ranges for editor folding and selection expansion.
//!
//! The input is the raw buffer. Every span therefore maps directly to the
//! source, including both branches of a preprocessor conditional. Include
//! references stay references: their expanded text never enters these ranges.

use oxabl_ast::{
    AstPools, CreateTarget, DataType, ExprId, ExprList, Expression, ExpressionKind, OnAction,
    OnKind, ParameterType, PreprocIf, Span, Statement, StatementKind, StreamOperation,
    SubscribeTarget, TypeSource,
};
use oxabl_common::{InternalPanic, catch_panic, panic_if_injected, panic_sites};
use oxabl_lexer::{Kind, Token};

use crate::RawUnit;

/// The LSP folding-range kind, without a dependency on a client protocol.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FoldKind {
    Comment,
    Region,
}

/// One source range that an editor can fold.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Fold {
    pub span: Span,
    pub kind: Option<FoldKind>,
}

/// The most ranges in one selection chain. This keeps its JSON below the
/// recursion limit of an editor client.
const MAX_SELECTION_RANGES: usize = 32;

/// The syntax facts that folding and selection requests share for one buffer.
#[derive(Debug)]
pub struct DocumentSyntax {
    folds: Vec<Fold>,
    statements: Vec<Span>,
    expressions: Vec<Span>,
    /// Each token, comments included, and its rank at a token boundary.
    words: Vec<(Span, Boundary)>,
    file: Span,
}

/// Lex and parse `source`, then collect all editor ranges in one walk.
///
/// Recovered statements and expressions remain available after parse errors.
/// `Err` means a contained internal panic, not an empty syntax tree.
pub fn document_syntax(source: &str) -> Result<DocumentSyntax, InternalPanic> {
    catch_panic(|| {
        panic_if_injected(panic_sites::SYNTAX, source);
        DocumentSyntax::new(source)
    })
}

/// [`document_syntax`] for a caller that holds the [`RawUnit`] of the text:
/// the text is not lexed or parsed again. The answer and the panic guard are
/// those of `document_syntax(unit.text())`.
pub fn document_syntax_of(unit: &RawUnit) -> Result<DocumentSyntax, InternalPanic> {
    catch_panic(|| {
        panic_if_injected(panic_sites::SYNTAX, unit.text());
        DocumentSyntax::of_parsed(unit.text(), unit.tokens(), unit.program())
    })
}

impl DocumentSyntax {
    fn new(source: &str) -> Self {
        let tokens = oxabl_lexer::tokenize(source);
        let program = oxabl_parser::Parser::new(&tokens, source)
            .without_comments()
            .parse_program();
        DocumentSyntax::of_parsed(source, &tokens, &program)
    }

    fn of_parsed(source: &str, tokens: &[Token], program: &oxabl_parser::Program) -> Self {
        let mut builder = Builder {
            source,
            tokens,
            breaks: line_breaks(source),
            folds: Vec::new(),
            statements: Vec::new(),
            expressions: Vec::new(),
        };
        walk_statements(&mut builder, &program.pools, &program.statements);
        builder.comments_and_markers();
        builder.preproc_branches();
        let folds = builder.one_fold_per_line();
        sort_levels(&mut builder.statements);
        sort_levels(&mut builder.expressions);
        let words = tokens
            .iter()
            .filter(|token| token.kind != Kind::Eof)
            .map(token_span)
            .filter(|span| valid_span(source, *span))
            .map(|span| (span, Boundary::of(&source[span.start as usize..])))
            .collect();
        DocumentSyntax {
            folds,
            statements: builder.statements,
            expressions: builder.expressions,
            words,
            file: Span {
                start: 0,
                end: source.len() as u32,
            },
        }
    }

    /// Folding ranges in source order. No two start on the same line.
    pub fn folds(&self) -> &[Fold] {
        &self.folds
    }

    /// At most `limit` folding ranges in source order. When there are more, the
    /// outermost ranges stay, so every part of the file keeps its top folds.
    pub fn folds_within(&self, limit: usize) -> Vec<Fold> {
        if self.folds.len() <= limit {
            return self.folds.clone();
        }
        // The nesting depth of each fold: the folds that contain it.
        let mut open: Vec<u32> = Vec::new();
        let mut ranked: Vec<(usize, usize)> = Vec::with_capacity(self.folds.len());
        for (at, fold) in self.folds.iter().enumerate() {
            while open.last().is_some_and(|end| *end <= fold.span.start) {
                open.pop();
            }
            ranked.push((open.len(), at));
            open.push(fold.span.end);
        }
        ranked.sort_unstable();
        let mut kept: Vec<usize> = ranked[..limit].iter().map(|(_, at)| *at).collect();
        kept.sort_unstable();
        kept.into_iter().map(|at| self.folds[at]).collect()
    }

    /// The chain at `offset`, from the innermost range to the whole file.
    /// Equal ranges occur once, and every range contains the previous one.
    pub fn selection(&self, offset: u32) -> Vec<Span> {
        let mut out = Vec::new();
        if let Some(word) = self.word_at(offset) {
            push_level(&mut out, word);
        }
        for span in self
            .expressions
            .iter()
            .chain(&self.statements)
            .filter(|span| contains_offset(**span, offset))
        {
            push_level(&mut out, *span);
        }
        push_level(&mut out, self.file);
        if out.len() > MAX_SELECTION_RANGES {
            let file = out.pop().expect("the file ends every chain");
            out.truncate(MAX_SELECTION_RANGES - 1);
            out.push(file);
        }
        out
    }

    /// The token at `offset`. Between two tokens, a word wins over a closing
    /// bracket, which wins over other punctuation, and a word or a closing
    /// bracket on the left wins a tie. So a cursor at the end of a name selects the name, and a cursor
    /// after `)` selects the call that it closes.
    fn word_at(&self, offset: u32) -> Option<Span> {
        let at = self.words.partition_point(|(span, _)| span.start <= offset);
        let right = at
            .checked_sub(1)
            .map(|i| self.words[i])
            .filter(|(span, _)| contains_offset(*span, offset));
        let left = at
            .checked_sub(2)
            .map(|i| self.words[i])
            .filter(|(span, _)| span.end == offset);
        match (left, right) {
            (Some((left, l)), Some((right, r)))
                if right.start == offset && l != Boundary::Other && l >= r =>
            {
                Some(left)
            }
            _ => right.or(left).map(|(span, _)| span),
        }
    }
}

/// How strongly a token claims a cursor at its edge.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Boundary {
    Other,
    /// `)` or `]`: the end of a call, a group or a subscript.
    Close,
    /// A name, a keyword, a literal or a reference.
    Word,
}

impl Boundary {
    /// The rank of the token that starts `text`.
    fn of(text: &str) -> Self {
        match text.chars().next() {
            Some(c) if c.is_alphanumeric() || matches!(c, '_' | '"' | '\'' | '{' | '&' | '?') => {
                Boundary::Word
            }
            Some(')' | ']') => Boundary::Close,
            _ => Boundary::Other,
        }
    }
}

fn sort_levels(spans: &mut Vec<Span>) {
    spans.sort_by_key(|span| (span.end - span.start, span.start, span.end));
    spans.dedup();
}

fn push_level(out: &mut Vec<Span>, span: Span) {
    if out.last().is_some_and(|previous| *previous == span) {
        return;
    }
    if out.last().is_none_or(|previous| contains(span, *previous)) {
        out.push(span);
    }
}

fn contains(outer: Span, inner: Span) -> bool {
    outer.start <= inner.start && inner.end <= outer.end
}

fn contains_offset(span: Span, offset: u32) -> bool {
    span.start <= offset && offset <= span.end
}

fn valid_span(source: &str, span: Span) -> bool {
    span.start < span.end
        && span.end as usize <= source.len()
        && source.is_char_boundary(span.start as usize)
        && source.is_char_boundary(span.end as usize)
}

struct Builder<'a> {
    source: &'a str,
    tokens: &'a [Token],
    /// The last byte of each line break (`\n`, `\r\n` or a lone `\r`), in order.
    breaks: Vec<u32>,
    folds: Vec<Fold>,
    statements: Vec<Span>,
    expressions: Vec<Span>,
}

impl<'a> Builder<'a> {
    fn line_of(&self, offset: u32) -> usize {
        self.breaks.partition_point(|at| *at < offset)
    }

    /// The offset where the line of `offset` starts.
    fn line_start(&self, offset: u32) -> u32 {
        match self.line_of(offset) {
            0 => 0,
            line => self.breaks[line - 1] + 1,
        }
    }

    /// The offset where the line of `offset` ends, before its line break.
    fn line_end(&self, offset: u32) -> usize {
        self.breaks
            .get(self.line_of(offset))
            .map_or(self.source.len(), |at| *at as usize)
    }

    /// Folds in source order, with one fold for each start line: the
    /// innermost. A client folds by line and keeps only one range per line,
    /// so `IF a THEN DO:` folds its `DO` block, not the whole `IF ... ELSE`.
    /// A comment never takes the line from another fold: in
    /// `FOR EACH t: /* two\n lines */`, the `FOR` block folds.
    fn one_fold_per_line(&mut self) -> Vec<Fold> {
        let mut folds = std::mem::take(&mut self.folds);
        folds.sort_by_key(|fold| (fold.span.start, std::cmp::Reverse(fold.span.end)));
        folds.dedup_by_key(|fold| fold.span);
        let mut kept: Vec<Fold> = Vec::with_capacity(folds.len());
        for fold in folds {
            match kept.last_mut() {
                Some(last) if self.line_of(last.span.start) == self.line_of(fold.span.start) => {
                    let rank = |fold: &Fold| {
                        (
                            fold.kind == Some(FoldKind::Comment),
                            self.line_of(fold.span.end),
                        )
                    };
                    if rank(&fold) <= rank(last) {
                        *last = fold;
                    }
                }
                _ => kept.push(fold),
            }
        }
        kept
    }

    fn push_fold(&mut self, span: Span, kind: Option<FoldKind>) {
        let mut span = trim_trailing_space(self.source, span);
        if !valid_span(self.source, span) || self.line_of(span.start) == self.line_of(span.end) {
            return;
        }
        // A client folds whole lines: when code follows the range on its last
        // line, the range stops on the line before, so that code stays visible.
        if self.code_follows(span.end) {
            let line_start = self.line_start(span.end);
            span = trim_trailing_space(
                self.source,
                Span {
                    start: span.start,
                    end: line_start.max(span.start),
                },
            );
        }
        if !valid_span(self.source, span) || self.line_of(span.start) == self.line_of(span.end) {
            return;
        }
        self.folds.push(Fold { span, kind });
    }

    /// Whether a token other than a comment that ends on the same line starts
    /// at or after `offset` on its line.
    fn code_follows(&self, offset: u32) -> bool {
        let line_end = self.line_end(offset);
        let from = self.tokens.partition_point(|token| token.start < offset);
        self.tokens[from..]
            .iter()
            .take_while(|token| (token.start as usize) < line_end && token.kind != Kind::Eof)
            .any(|token| {
                // A `//` comment token holds its line break, so test its last
                // byte, not its end.
                token.kind != Kind::Comment
                    || self.line_of(token.end.saturating_sub(1)) != self.line_of(offset)
            })
    }

    fn single_line(&self, span: Span) -> bool {
        let span = trim_trailing_space(self.source, span);
        self.line_of(span.start) == self.line_of(span.end)
    }

    fn tokens_in(&self, span: Span) -> &'a [Token] {
        let lo = self
            .tokens
            .partition_point(|token| token.start < span.start);
        let n = self.tokens[lo..].partition_point(|token| token.start < span.end);
        &self.tokens[lo..lo + n]
    }

    fn is_define(&self, statement: &Statement) -> bool {
        self.tokens_in(statement.span)
            .iter()
            .find(|token| token.kind != Kind::Comment)
            .is_some_and(|token| token.kind == Kind::Define)
    }

    /// A run starts at a one-line `DEFINE`: a run that started at a longer
    /// one would share its first line with that statement's own fold.
    fn define_runs(&mut self, statements: &[Statement]) {
        let mut run: Option<(u32, u32, usize)> = None;
        for statement in statements {
            if self.is_define(statement) {
                match run.as_mut() {
                    Some((_, end, count)) => {
                        *end = statement.span.end;
                        *count += 1;
                    }
                    None if self.single_line(statement.span) => {
                        run = Some((statement.span.start, statement.span.end, 1));
                    }
                    None => {}
                }
            } else {
                self.define_run(run.take());
            }
        }
        self.define_run(run);
    }

    fn define_run(&mut self, run: Option<(u32, u32, usize)>) {
        if let Some((start, end, count)) = run
            && count > 1
        {
            self.push_fold(Span { start, end }, None);
        }
    }

    /// Each `&IF`, `&ELSEIF` and `&ELSE` branch, from the tokens, so a
    /// conditional that the parser did not read as a statement still folds.
    /// A branch ends before the directive that closes it.
    fn preproc_branches(&mut self) {
        // The start of the open branch of each open `&IF`, innermost last.
        let mut open: Vec<u32> = Vec::new();
        for token in self.tokens {
            let start = token.start;
            match token.kind {
                Kind::PreprocIf => open.push(start),
                Kind::PreprocElseif | Kind::PreprocElse => {
                    if let Some(branch) = open.last_mut() {
                        let previous = std::mem::replace(branch, start);
                        self.push_fold(
                            Span {
                                start: previous,
                                end: start,
                            },
                            Some(FoldKind::Region),
                        );
                    }
                }
                Kind::PreprocEndif => {
                    if let Some(previous) = open.pop() {
                        self.push_fold(
                            Span {
                                start: previous,
                                end: start,
                            },
                            Some(FoldKind::Region),
                        );
                    }
                }
                _ => {}
            }
        }
    }

    /// `span` without its last line when that line starts with the `END` that
    /// closes the statement, so the `END` stays visible in a folded block.
    fn without_end_line(&self, span: Span) -> Span {
        let tokens = self.tokens_in(span);
        let Some(last) = tokens.iter().rposition(|token| token.kind != Kind::Comment) else {
            return span;
        };
        let line_start = self.line_start(tokens[last].start);
        if line_start <= span.start {
            return span;
        }
        let first_on_line = tokens[..=last]
            .iter()
            .rev()
            .take_while(|token| token.start >= line_start)
            .filter(|token| token.kind != Kind::Comment)
            .last();
        if first_on_line.is_some_and(|token| token.kind == Kind::End) {
            Span {
                start: span.start,
                end: line_start,
            }
        } else {
            span
        }
    }

    fn comments_and_markers(&mut self) {
        let mut open = Vec::new();
        for token in self.tokens {
            if token.kind != Kind::Comment {
                continue;
            }
            let span = token_span(token);
            let Some(text) = self.source.get(token.range()) else {
                continue;
            };
            if text.starts_with("/*") {
                self.push_fold(span, Some(FoldKind::Comment));
                continue;
            }
            let marker = analyze_marker(text).or_else(|| region_marker(text));
            match marker {
                Some((Marker::Start, family)) => open.push((span.start, family)),
                Some((Marker::End, family)) => {
                    if let Some(at) = open
                        .iter()
                        .rposition(|(_, open_family)| *open_family == family)
                    {
                        let (start, _) = open.remove(at);
                        self.push_fold(
                            Span {
                                start,
                                end: span.end,
                            },
                            Some(FoldKind::Region),
                        );
                    }
                }
                None => {}
            }
        }
    }
}

impl Visitor for Builder<'_> {
    fn statement_list(&mut self, statements: &[Statement]) {
        self.define_runs(statements);
    }

    fn statement(&mut self, statement: &Statement) {
        if valid_span(self.source, statement.span) {
            self.statements.push(statement.span);
        }
        // The branches of a conditional fold on their own (`preproc_branches`).
        if !matches!(statement.kind, StatementKind::PreprocIf(_)) {
            let span = self.without_end_line(statement.span);
            self.push_fold(span, None);
        }
    }

    fn expression(&mut self, expression: &Expression) {
        if valid_span(self.source, expression.span) {
            self.expressions.push(expression.span);
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum MarkerFamily {
    Analyze,
    Region,
}

enum Marker {
    Start,
    End,
}

fn analyze_marker(text: &str) -> Option<(Marker, MarkerFamily)> {
    let text = text.trim_start();
    directive(text, "&ANALYZE-SUSPEND")
        .then_some((Marker::Start, MarkerFamily::Analyze))
        .or_else(|| {
            directive(text, "&ANALYZE-RESUME").then_some((Marker::End, MarkerFamily::Analyze))
        })
}

fn region_marker(text: &str) -> Option<(Marker, MarkerFamily)> {
    let text = text.strip_prefix("//")?.trim_start();
    directive(text, "#region")
        .then_some((Marker::Start, MarkerFamily::Region))
        .or_else(|| directive(text, "#endregion").then_some((Marker::End, MarkerFamily::Region)))
}

fn directive(text: &str, wanted: &str) -> bool {
    text.get(..wanted.len())
        .is_some_and(|head| head.eq_ignore_ascii_case(wanted))
        && text[wanted.len()..]
            .chars()
            .next()
            .is_none_or(char::is_whitespace)
}

/// The last byte of each line break, as LSP counts them: `\n`, `\r\n` (its
/// `\n`) and a lone `\r`.
fn line_breaks(source: &str) -> Vec<u32> {
    let bytes = source.as_bytes();
    bytes
        .iter()
        .enumerate()
        .filter(|(at, byte)| {
            **byte == b'\n' || (**byte == b'\r' && bytes.get(at + 1) != Some(&b'\n'))
        })
        .map(|(at, _)| at as u32)
        .collect()
}

fn trim_trailing_space(source: &str, mut span: Span) -> Span {
    while span.end > span.start
        && source
            .as_bytes()
            .get(span.end as usize - 1)
            .is_some_and(u8::is_ascii_whitespace)
    {
        span.end -= 1;
    }
    span
}

fn token_span(token: &Token) -> Span {
    Span {
        start: token.start,
        end: token.end,
    }
}

trait Visitor {
    fn statement_list(&mut self, _statements: &[Statement]) {}
    fn statement(&mut self, statement: &Statement);
    fn expression(&mut self, expression: &Expression);
}

fn walk_statements(v: &mut impl Visitor, p: &AstPools, statements: &[Statement]) {
    v.statement_list(statements);
    for statement in statements {
        walk_statement(v, p, statement);
    }
}

fn walk_opt(v: &mut impl Visitor, p: &AstPools, expression: Option<ExprId>) {
    if let Some(expression) = expression {
        walk_expression(v, p, expression);
    }
}

fn walk_list(v: &mut impl Visitor, p: &AstPools, list: ExprList) {
    for &expression in p.exprs(list) {
        walk_expression(v, p, expression);
    }
}

fn walk_statement(v: &mut impl Visitor, p: &AstPools, statement: &Statement) {
    v.statement(statement);
    match &statement.kind {
        StatementKind::VariableDeclaration {
            initial_value,
            type_source,
            ..
        } => {
            walk_type_source(v, p, type_source);
            walk_opt(v, p, *initial_value);
        }
        StatementKind::VariableDeclarationList(body) => walk_statements(v, p, body),
        StatementKind::DefineTempTable { fields, .. } => {
            for field in fields {
                walk_type_source(v, p, &field.type_source);
                if let Some(init) = field.initial_value {
                    walk_list(v, p, init);
                }
            }
        }
        StatementKind::Property {
            data_type,
            get_body,
            set_body,
            set_parameters,
            ..
        } => {
            walk_data_type(v, p, data_type);
            walk_statements(v, p, set_parameters);
            for body in [get_body, set_body].into_iter().flatten() {
                walk_statements(v, p, body);
            }
        }
        StatementKind::Procedure { body, .. }
        | StatementKind::Class { body, .. }
        | StatementKind::Interface { body, .. }
        | StatementKind::Destructor { body }
        | StatementKind::Finally { body }
        | StatementKind::Catch { body, .. }
        | StatementKind::Block(body) => walk_statements(v, p, body),
        StatementKind::Function {
            return_type,
            body,
            prototype_parameters,
            ..
        } => {
            walk_data_type(v, p, return_type);
            walk_statements(v, p, prototype_parameters);
            walk_statements(v, p, body);
        }
        StatementKind::Method {
            return_type,
            parameters,
            body,
            ..
        } => {
            if let Some(data_type) = return_type {
                walk_data_type(v, p, data_type);
            }
            walk_statements(v, p, parameters);
            walk_statements(v, p, body);
        }
        StatementKind::Constructor {
            parameters, body, ..
        } => {
            walk_statements(v, p, parameters);
            walk_statements(v, p, body);
        }
        StatementKind::DefineEvent { parameters, .. } => walk_statements(v, p, parameters),
        StatementKind::DefineParameter { param_type, .. } => {
            if let ParameterType::Variable { type_source, .. } = param_type {
                walk_type_source(v, p, type_source);
            }
        }
        StatementKind::Do {
            from,
            to,
            by,
            while_condition,
            on_actions,
            body,
            ..
        } => {
            for expression in [from, to, by, while_condition].into_iter().flatten() {
                walk_expression(v, p, *expression);
            }
            walk_list(v, p, *on_actions);
            walk_statements(v, p, body);
        }
        StatementKind::Repeat {
            while_condition,
            on_actions,
            body,
        } => {
            walk_opt(v, p, *while_condition);
            walk_list(v, p, *on_actions);
            walk_statements(v, p, body);
        }
        StatementKind::ForEach {
            where_clause,
            on_actions,
            body,
            ..
        } => {
            walk_opt(v, p, *where_clause);
            walk_list(v, p, *on_actions);
            walk_statements(v, p, body);
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
            if let Some(else_branch) = else_branch {
                walk_statement(v, p, else_branch);
            }
        }
        StatementKind::Case {
            expression,
            when_branches,
            otherwise,
        } => {
            walk_expression(v, p, *expression);
            for branch in when_branches {
                walk_list(v, p, branch.values);
                walk_statements(v, p, &branch.body);
            }
            if let Some(otherwise) = otherwise {
                walk_statements(v, p, otherwise);
            }
        }
        StatementKind::Label { body, .. } => walk_statement(v, p, body),
        StatementKind::On { kind } => match kind {
            OnKind::UiEvent { action, .. } | OnKind::DbEvent { action, .. } => {
                walk_on_action(v, p, action)
            }
            OnKind::KeyRemap { .. } => {}
        },
        StatementKind::PreprocIf(preproc) => {
            walk_expression(v, p, preproc.condition);
            walk_statements(v, p, &preproc.then_branch);
            for (condition, branch) in &preproc.elseif_branches {
                walk_expression(v, p, *condition);
                walk_statements(v, p, branch);
            }
            if let Some(branch) = &preproc.else_branch {
                walk_statements(v, p, branch);
            }
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
        StatementKind::ExpressionStatement(expression)
        | StatementKind::Throw(expression)
        | StatementKind::ReleaseObject {
            target: expression, ..
        } => walk_expression(v, p, *expression),
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
        StatementKind::Return(expression) => walk_opt(v, p, *expression),
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
        } => {
            walk_list(v, p, *targets);
            walk_list(v, p, *options);
        }
        StatementKind::PromptFor {
            targets,
            options,
            fields,
            ..
        } => {
            walk_list(v, p, *targets);
            walk_list(v, p, *options);
            for field in fields {
                walk_type_source(v, p, &field.type_source);
            }
        }
        StatementKind::Get { query, .. } => walk_expression(v, p, *query),
        StatementKind::Close {
            target,
            status,
            handle,
            ..
        } => {
            walk_expression(v, p, *target);
            walk_opt(v, p, *status);
            walk_opt(v, p, *handle);
        }
        StatementKind::Reposition {
            query, operands, ..
        } => {
            walk_expression(v, p, *query);
            walk_list(v, p, *operands);
        }
        StatementKind::Accumulate {
            expression, breaks, ..
        } => {
            walk_expression(v, p, *expression);
            walk_list(v, p, *breaks);
        }
        StatementKind::Color { items, .. } => {
            walk_list(v, p, *items);
        }
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
        StatementKind::Clear { .. } => {}
        StatementKind::Pause {
            duration: first,
            message: second,
        }
        | StatementKind::Apply {
            event: first,
            target: second,
        } => {
            for expression in first.iter().chain(second) {
                walk_expression(v, p, *expression);
            }
        }
        StatementKind::Down { lines, .. } | StatementKind::Up { lines, .. } => {
            walk_opt(v, p, *lines);
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
            if let oxabl_ast::RunTarget::Dynamic(expression) = target {
                walk_expression(v, p, *expression);
            }
            for argument in arguments {
                walk_expression(v, p, argument.expression);
            }
            for expression in [in_handle, persistent_handle, async_handle, event_procedure]
                .into_iter()
                .flatten()
            {
                walk_expression(v, p, *expression);
            }
        }
        StatementKind::Publish {
            event_name,
            from_handle,
            arguments,
        } => {
            walk_expression(v, p, *event_name);
            walk_opt(v, p, *from_handle);
            for argument in arguments {
                walk_expression(v, p, argument.expression);
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
            if let SubscribeTarget::InHandle(handle) = target {
                walk_expression(v, p, *handle);
            }
        }
        StatementKind::Unsubscribe {
            subscriber,
            event_name,
            in_handle,
        } => {
            for expression in [subscriber, event_name, in_handle].into_iter().flatten() {
                walk_expression(v, p, *expression);
            }
        }
        StatementKind::StreamIo { operation, .. } => match operation {
            StreamOperation::From(target) | StreamOperation::To { target, .. } => {
                walk_opt(v, p, target.expression());
            }
            StreamOperation::Through(expression) => walk_expression(v, p, *expression),
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
                walk_opt(v, p, connect.as_ref().and_then(|connect| connect.file));
                for assignment in assignments {
                    walk_expression(v, p, assignment.value);
                }
                for trigger in triggers {
                    walk_on_action(v, p, &trigger.action);
                }
            }
        }
        StatementKind::DefineFrame { fields, .. } => {
            for field in fields {
                walk_type_source(v, p, &field.type_source);
            }
        }
        StatementKind::TriggerProcedure {
            new_value,
            old_value_param,
            ..
        } => {
            for parameter in [new_value, old_value_param].into_iter().flatten() {
                walk_data_type(v, p, &parameter.data_type);
            }
        }
        StatementKind::DefineBuffer { .. }
        | StatementKind::DefineDataset { .. }
        | StatementKind::DefineDataSource { .. }
        | StatementKind::DefineStream { .. }
        | StatementKind::DefineWidget { .. }
        | StatementKind::PreprocDefine { .. }
        | StatementKind::PreprocUndefine { .. }
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

fn walk_on_action(v: &mut impl Visitor, p: &AstPools, action: &OnAction) {
    match action {
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

fn walk_type_source(v: &mut impl Visitor, p: &AstPools, source: &TypeSource) {
    if let TypeSource::Explicit(data_type) = source {
        walk_data_type(v, p, data_type);
    }
}

fn walk_data_type(v: &mut impl Visitor, p: &AstPools, data_type: &DataType) {
    if let DataType::PreprocIf(preproc) = data_type {
        walk_expression(v, p, preproc.condition);
        walk_data_type(v, p, &preproc.then_branch);
        for (condition, branch) in &preproc.elseif_branches {
            walk_expression(v, p, *condition);
            walk_data_type(v, p, branch);
        }
        if let Some(branch) = &preproc.else_branch {
            walk_data_type(v, p, branch);
        }
    }
}

fn walk_expression(v: &mut impl Visitor, p: &AstPools, id: ExprId) {
    let expression = p.expr(id);
    v.expression(expression);
    match &expression.kind {
        ExpressionKind::Literal(_)
        | ExpressionKind::Identifier(_)
        | ExpressionKind::IncludeReference { .. }
        | ExpressionKind::IncludeArgReference { .. }
        | ExpressionKind::PreprocReference(_) => {}
        ExpressionKind::Add(left, right)
        | ExpressionKind::Minus(left, right)
        | ExpressionKind::Multiply(left, right)
        | ExpressionKind::Divide(left, right)
        | ExpressionKind::Modulo(left, right)
        | ExpressionKind::Equal(left, right)
        | ExpressionKind::NotEqual(left, right)
        | ExpressionKind::LessThan(left, right)
        | ExpressionKind::LessThanOrEqual(left, right)
        | ExpressionKind::GreaterThan(left, right)
        | ExpressionKind::GreaterThanOrEqual(left, right)
        | ExpressionKind::Begins(left, right)
        | ExpressionKind::Matches(left, right)
        | ExpressionKind::Contains(left, right)
        | ExpressionKind::And(left, right)
        | ExpressionKind::Or(left, right)
        | ExpressionKind::ArrayAccess {
            array: left,
            index: right,
        } => {
            walk_expression(v, p, *left);
            walk_expression(v, p, *right);
        }
        ExpressionKind::Negate(inner)
        | ExpressionKind::Not(inner)
        | ExpressionKind::MemberAccess { object: inner, .. }
        | ExpressionKind::FieldAccess {
            qualifier: inner, ..
        } => walk_expression(v, p, *inner),
        ExpressionKind::IfThenElse(condition, then_expression, else_expression) => {
            walk_expression(v, p, *condition);
            walk_expression(v, p, *then_expression);
            walk_expression(v, p, *else_expression);
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
        ExpressionKind::PreprocIf(id) => {
            walk_expression_preproc(v, p, p.preproc_if(*id));
        }
        ExpressionKind::CanFind { where_clause, .. } => {
            walk_opt(v, p, *where_clause);
        }
    }
}

fn walk_expression_preproc(v: &mut impl Visitor, p: &AstPools, preproc: &PreprocIf<ExprId>) {
    walk_expression(v, p, preproc.condition);
    walk_expression(v, p, preproc.then_branch);
    for (condition, branch) in &preproc.elseif_branches {
        walk_expression(v, p, *condition);
        walk_expression(v, p, *branch);
    }
    if let Some(branch) = &preproc.else_branch {
        walk_expression(v, p, *branch);
    }
}
