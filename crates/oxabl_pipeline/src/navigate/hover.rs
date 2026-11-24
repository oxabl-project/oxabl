//! Hover: what the name or keyword at a position is, as short Markdown.
//!
//! A name is resolved as go to definition resolves it ([`Navigation::innermost`]),
//! so a hover and a definition never disagree about what a name means. The text
//! comes from the declaration as the parser read it: the expanded unit for a
//! local name, the class file for an inherited member, the program for a
//! procedure run through a handle, the loaded schema for a table or field, and
//! the documentation index for a built-in function, a statement keyword or a
//! system handle.
//!
//! A name that does not resolve, or that resolves to more than one thing, has
//! no hover.

use std::path::Path;

use oxabl_ast::{
    AccessModifier, BufferTarget, DataType, HandleParamKind, ParameterType, Span, Statement,
    StatementKind, TempTableField, TypeSource,
};
use oxabl_common::VirtualSpan;
use oxabl_lexer::{Kind, Token, tokenize};
use oxabl_semantic::{
    IndexedFileId, Resolution, ResolvedType, SiteTarget, SymbolId, SymbolKind, UnresolvedReason,
};

use oxabl_workspace::is_include_fragment;

mod like;

use super::{Navigation, Parsed, Target, search};
use crate::keyword_docs::{DocKind, FUNCTION_ABBREVIATIONS, KEYWORD_DOCS, KeywordDoc};
use crate::outline::{access_name, children, data_type_name, direction_name};
use crate::schema_hover::{code, field_markdown, inline, plain, table_markdown};

/// The answer to a hover request.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Hover {
    /// Markdown.
    pub contents: String,
    /// The bytes of the hovered word in the hovered file, when the file writes
    /// it there (not when an include argument or a macro supplied it).
    pub span: Option<Span>,
}

/// Words that may stand between `DEFINE` and what it defines, and between a
/// parameter's direction and `PARAMETER`.
const DEFINE_MODIFIERS: &[&str] = &[
    "NEW",
    "GLOBAL",
    "SHARED",
    "PRIVATE",
    "PROTECTED",
    "PUBLIC",
    "PACKAGE-PRIVATE",
    "PACKAGE-PROTECTED",
    "STATIC",
    "ABSTRACT",
    "OVERRIDE",
    "SERIALIZABLE",
    "NON-SERIALIZABLE",
    "INPUT",
    "OUTPUT",
    "INPUT-OUTPUT",
    "RETURN",
];

/// How many tokens before the hovered one a statement may start for its head
/// to reach the hovered token.
const MAX_HEAD: usize = 6;

/// A signature longer than this, in characters, puts one parameter on a line.
const SIGNATURE_WIDTH: usize = 80;

impl Navigation<'_, '_> {
    /// What the name or keyword at byte `offset` of `file` is. `file` is the
    /// analysed file or an include its expansion read. `None` when there is
    /// nothing to say: no name there, a name that does not resolve, or one
    /// that resolves to several things.
    pub fn hover(&self, file: &Path, offset: u32) -> Option<Hover> {
        oxabl_common::hold_if_injected(oxabl_common::panic_sites::HOVER, &self.expanded.text);
        let file = search::normalize_lexically(file);
        if self.include_at(&file, offset).is_some() {
            return None;
        }
        let ids = self.file_ids(&file);
        // A cursor just after a name, as a click at its end gives, still means it.
        for at in [Some(offset), offset.checked_sub(1)].into_iter().flatten() {
            let Some(virt) = ids
                .iter()
                .find_map(|id| self.expanded.expanded_offset(*id, at))
            else {
                continue;
            };
            if let Some(answer) = self.hover_at(virt) {
                return answer.map(|contents| Hover {
                    contents,
                    span: self.word_span(&file, virt),
                });
            }
        }
        None
    }

    /// `None` when nothing at expanded offset `virt` has an answer,
    /// `Some(None)` when what is there has no hover.
    fn hover_at(&self, virt: u32) -> Option<Option<String>> {
        let answer = self.innermost(
            virt,
            &|sym| self.symbol_hover(sym),
            &|target, span| self.site_hover(target, span, virt),
            &Option::is_some,
            || None,
        );
        if answer.is_some() {
            return answer;
        }
        self.statement_hover(virt).map(Some)
    }

    fn site_hover(
        &self,
        target: &SiteTarget,
        span: VirtualSpan,
        virt: u32,
    ) -> Option<Option<String>> {
        let references = &self.semantic.references;
        match target {
            SiteTarget::Reference(node) => match references.get(*node)? {
                Resolution::Resolved(sym) => Some(self.symbol_hover(*sym)),
                Resolution::Unresolved {
                    name,
                    reason: UnresolvedReason::External,
                } => Some(self.builtin_function_hover(name.as_ref(), span, virt)),
                Resolution::Unresolved { .. } => Some(None),
            },
            SiteTarget::Run(node) => match references.get(*node)? {
                Resolution::Resolved(sym) if self.semantic.symbols.program_file(*sym).is_some() => {
                    Some(self.symbol_hover(*sym))
                }
                _ if let Some(targets) = self.handle_call_targets(span) => {
                    Some(self.handle_call_hover(&targets))
                }
                Resolution::Resolved(sym) => Some(self.symbol_hover(*sym)),
                Resolution::Unresolved { .. } => Some(None),
            },
            SiteTarget::Using(node) => match references.get(*node)? {
                Resolution::Resolved(sym) => Some(self.symbol_hover(*sym)),
                Resolution::Unresolved { .. } => Some(None),
            },
            SiteTarget::Type(resolution) => match resolution {
                Resolution::Resolved(sym) => Some(self.symbol_hover(*sym)),
                Resolution::Unresolved { .. } => Some(None),
            },
            SiteTarget::Symbol(sym) => Some(self.symbol_hover(*sym)),
            SiteTarget::Table(table) => Some(self.table_hover(*table)),
            SiteTarget::SchemaField(table) => Some(
                self.text(span)
                    .and_then(|name| self.field_hover(*table, name)),
            ),
        }
    }

    fn symbol_hover(&self, sym: SymbolId) -> Option<String> {
        let symbols = &self.semantic.symbols;
        let symbol = symbols.get(sym);
        if let Some(file) = symbols.program_file(sym) {
            return self.program_hover(file);
        }
        if let Some(file) = symbols.class_file(sym) {
            return self.class_hover(file);
        }
        if let Some(file) = symbols.member_file(sym) {
            return self.member_hover(file, symbol.kind, symbol.name.as_ref());
        }
        if let Some(table) = symbols.field_table(sym) {
            return self.field_hover(table, symbol.name.as_ref());
        }
        if let Some(table) = self.table_of(symbol) {
            return self.table_hover(table);
        }
        if symbol.kind == SymbolKind::BuiltIn {
            return find_doc(DocKind::SystemHandle, symbol.name.as_ref()).map(doc_markdown);
        }
        if symbol.declaration == oxabl_ast::NodeId::DUMMY {
            return None;
        }
        let parsed = self.parsed();
        let source = Source::new(&self.expanded.text, &parsed.tokens);
        let found = find_declaration(&parsed.program.statements, symbol.name_span)?;
        let mut out = source.declaration(&found)?;
        if found.field.is_none()
            && symbol.kind == SymbolKind::TempTable
            && let Some(shape) = self.temp_table_shape(found.statement, Some(sym))
        {
            out.push_str("\n\n");
            out.push_str(&shape);
        }
        if let Some(note) = self.like_note(&found, symbol.data_type.as_ref()) {
            out.push_str("\n\n");
            out.push_str(&note);
        }
        if let Some(overloads) = symbols.method_overloads(sym)
            && overloads.len() > 1
        {
            out.push_str(&format!("\n\n{} overloads", overloads.len()));
        }
        Some(out)
    }

    /// The type a `LIKE` declaration takes, when the analysis knows it: the
    /// declared symbol's type, or the type of what the name after `LIKE`
    /// resolved to.
    fn like_note(&self, found: &Declaration, data_type: Option<&ResolvedType>) -> Option<String> {
        let like = match found.statement_kind_type_source()? {
            TypeSource::Like { source } => source,
            TypeSource::Explicit(_) => return None,
        };
        let known = |t: &&ResolvedType| !matches!(t, ResolvedType::Unknown | ResolvedType::Error);
        let schema = &self.run.config().schema;
        let shown = match data_type.filter(known) {
            Some(data_type) => data_type
                .display_abl(&self.semantic.symbols, schema)
                .to_string(),
            None => self.like_source_type(like.span)?,
        };
        Some(format!(
            "{} is {}.",
            inline(&format!("LIKE {}", like.name)),
            inline(&shown)
        ))
    }

    /// The type of the field or variable that the name at `span` (after a
    /// `LIKE`) resolved to.
    fn like_source_type(&self, span: Span) -> Option<String> {
        let symbols = &self.semantic.symbols;
        let schema = &self.run.config().schema;
        let mut sites: Vec<_> = symbols
            .sites()
            .iter()
            .filter(|site| span.start <= site.span.start && site.span.end <= span.end)
            .collect();
        // The field is the last part of the name.
        sites.sort_by_key(|site| std::cmp::Reverse(site.span.start));
        sites.into_iter().find_map(|site| {
            let data_type = match &site.target {
                SiteTarget::SchemaField(table) => {
                    let table = schema.get_by_id(*table)?;
                    match table.resolve_field_by_name(self.text(site.span)?) {
                        oxabl_schema::FieldResolution::Unique(field) => {
                            ResolvedType::from_schema_field(field)
                        }
                        _ => return None,
                    }
                }
                SiteTarget::Reference(node) => match self.semantic.references.get(*node)? {
                    Resolution::Resolved(sym) => symbols.get(*sym).data_type.clone()?,
                    Resolution::Unresolved { .. } => return None,
                },
                SiteTarget::Symbol(sym) => symbols.get(*sym).data_type.clone()?,
                _ => return None,
            };
            (!matches!(data_type, ResolvedType::Unknown | ResolvedType::Error))
                .then(|| data_type.display_abl(symbols, schema).to_string())
        })
    }

    /// A built-in function, when the cursor is on its name.
    ///
    /// The name must be written as a call (`name(`). The analysis leaves a
    /// name unresolved when it cannot see where it is declared (an include
    /// analysed alone, a class whose ancestor oxabl did not read), so a bare
    /// `interval` may be a variable or a property. There, a call may also be a
    /// user function of the includer or an inherited method (`Compare(...)`),
    /// so only a reserved name, which no routine can have, is a built-in.
    fn builtin_function_hover(&self, name: &str, span: VirtualSpan, virt: u32) -> Option<String> {
        let token = self.token_at(virt)?;
        if token.start != span.start as usize {
            return None;
        }
        let tokens = &self.parsed().tokens;
        let at = tokens.partition_point(|t| t.start < token.start);
        let called = tokens[at + 1..]
            .iter()
            .find(|t| t.kind != Kind::Comment)
            .is_some_and(|t| t.kind == Kind::LeftParen);
        if !called {
            return None;
        }
        let doc = find_doc(DocKind::Function, full_function_name(name))?;
        if !doc.reserved && self.may_hide_routines(virt) {
            return None;
        }
        Some(doc_markdown(doc))
    }

    /// Whether a routine that this unit cannot see may have the name at
    /// `virt`: the unit is an include analysed alone, or `virt` is in a class
    /// that inherits another class than `Progress.Lang.Object`.
    fn may_hide_routines(&self, virt: u32) -> bool {
        if self.run.file().is_some_and(is_include_fragment) {
            return true;
        }
        self.parsed().program.statements.iter().any(|statement| {
            let StatementKind::Class {
                inherits: Some(parent),
                ..
            } = &statement.kind
            else {
                return false;
            };
            statement.span.start <= virt
                && virt < statement.span.end
                && !parent.name.eq_ignore_ascii_case("Progress.Lang.Object")
        })
    }

    /// The statement whose head holds the token at `virt`.
    fn statement_hover(&self, virt: u32) -> Option<String> {
        let parsed = self.parsed();
        let source = Source::new(&self.expanded.text, &parsed.tokens);
        let words: Vec<&Token> = parsed
            .tokens
            .iter()
            .filter(|t| t.kind != Kind::Comment)
            .collect();
        let at = words
            .iter()
            .position(|t| t.start <= virt as usize && (virt as usize) < t.end)?;
        let starts = statement_starts(&parsed.program.statements);
        for first in (at.saturating_sub(MAX_HEAD)..=at).rev() {
            if starts.binary_search(&(words[first].start as u32)).is_ok() {
                let (doc, end) = longest_statement(&source, &words[first..])?;
                return (at < first + end).then(|| doc_markdown(doc));
            }
        }
        None
    }

    /// The token of the expanded unit that holds `virt`.
    fn token_at(&self, virt: u32) -> Option<&Token> {
        let tokens = &self.parsed().tokens;
        let at = tokens.partition_point(|t| t.end <= virt as usize);
        tokens
            .get(at)
            .filter(|t| t.start <= virt as usize && t.kind != Kind::Comment)
    }

    /// The bytes in `file` of the token at `virt`.
    fn word_span(&self, file: &Path, virt: u32) -> Option<Span> {
        let token = self.token_at(virt)?;
        let span = VirtualSpan::new(token.start as u32, token.end as u32);
        let Target { path, span } = self.place(span)?;
        (path == file).then_some(span).flatten()
    }

    fn program_hover(&self, file: IndexedFileId) -> Option<String> {
        let path = self.path_of(file)?;
        Some(code(&format!("RUN {}", self.shown_path(&path))))
    }

    /// `path` relative to the include path entry that holds it.
    fn shown_path(&self, path: &Path) -> String {
        let config = self.run.config();
        let entries = config.include_paths_from(self.search_base.as_deref());
        entries
            .iter()
            .filter_map(|entry| path.strip_prefix(search::normalize_lexically(entry)).ok())
            .min_by_key(|relative| relative.components().count())
            .unwrap_or(path)
            .to_string_lossy()
            .replace('\\', "/")
    }

    fn class_hover(&self, file: IndexedFileId) -> Option<String> {
        let path = self.path_of(file)?;
        let text = self.run.file_system().read(&path).ok()?;
        let parsed = Parsed::of(&text);
        parsed
            .program
            .statements
            .iter()
            .find_map(class_header)
            .map(|header| code(&header))
    }

    /// Each declaration of member `name` in the class file `file`.
    fn member_hover(&self, file: IndexedFileId, kind: SymbolKind, name: &str) -> Option<String> {
        let path = self.path_of(file)?;
        let text = self.run.file_system().read(&path).ok()?;
        let parsed = Parsed::of(&text);
        let source = Source::new(&text, &parsed.tokens);
        let class =
            parsed
                .program
                .statements
                .iter()
                .find_map(|statement| match &statement.kind {
                    StatementKind::Class { name, body, .. }
                    | StatementKind::Interface { name, body, .. } => Some((name, body)),
                    _ => None,
                })?;
        let (class_name, body) = class;
        let lines: Vec<String> = body
            .iter()
            .filter(|member| member_named(member, kind, name))
            .filter_map(|member| {
                let mut text = source.declaration(&Declaration {
                    statement: member,
                    routine: None,
                    field: None,
                })?;
                if let Some(shape) = self.temp_table_shape(member, None) {
                    text.push_str("\n\n");
                    text.push_str(&shape);
                }
                Some(text)
            })
            .collect();
        if lines.is_empty() {
            return None;
        }
        Some(format!(
            "{}\n\nDeclared in {}.",
            lines.join("\n\n"),
            inline(&class_name.name)
        ))
    }

    /// The procedure that `RUN p IN h` runs, when the handle runs one program
    /// and its text declares `p` once.
    fn handle_call_hover(&self, targets: &[Target]) -> Option<String> {
        let [
            Target {
                path,
                span: Some(span),
            },
        ] = targets
        else {
            return None;
        };
        let text = self.run.file_system().read(path).ok()?;
        let parsed = Parsed::of(&text);
        let source = Source::new(&text, &parsed.tokens);
        let found = find_declaration(
            &parsed.program.statements,
            VirtualSpan::new(span.start, span.end),
        )?;
        let signature = source.declaration(&found)?;
        Some(format!(
            "{signature}\n\nIn {}.",
            inline(&self.shown_path(path))
        ))
    }

    fn table_hover(&self, table: oxabl_schema::TableId) -> Option<String> {
        let table = self.run.config().schema.get_by_id(table)?;
        Some(table_markdown(table))
    }

    fn field_hover(&self, table: oxabl_schema::TableId, name: &str) -> Option<String> {
        let table = self.run.config().schema.get_by_id(table)?;
        match table.resolve_field_by_name(name) {
            oxabl_schema::FieldResolution::Unique(field) => Some(field_markdown(table, field)),
            _ => None,
        }
    }
}

/// A declaration found in a parsed text: the statement, the procedure,
/// function or method that holds it, and the temp-table field it names.
pub(super) struct Declaration<'a> {
    statement: &'a Statement,
    routine: Option<&'a Statement>,
    field: Option<&'a TempTableField>,
}

impl Declaration<'_> {
    /// Whether an `&IF` chooses the declared type.
    fn chosen_type(&self) -> bool {
        let property = match &self.statement.kind {
            StatementKind::Property { data_type, .. } => Some(data_type),
            _ => None,
        };
        let explicit = match self.statement_kind_type_source() {
            Some(TypeSource::Explicit(data_type)) => Some(data_type),
            _ => property,
        };
        matches!(explicit, Some(DataType::PreprocIf(_)))
    }

    fn statement_kind_type_source(&self) -> Option<&TypeSource> {
        if let Some(field) = self.field {
            return Some(&field.type_source);
        }
        match &self.statement.kind {
            StatementKind::VariableDeclaration { type_source, .. } => Some(type_source),
            StatementKind::DefineParameter {
                param_type: ParameterType::Variable { type_source, .. },
                ..
            } => Some(type_source),
            _ => None,
        }
    }
}

/// The statement that declares the name at `name`, found by the span of the
/// declared name.
fn find_declaration(statements: &[Statement], name: VirtualSpan) -> Option<Declaration<'_>> {
    let span = Span {
        start: name.start,
        end: name.end,
    };
    let mut stack: Vec<(&[Statement], Option<&Statement>)> = vec![(statements, None)];
    while let Some((list, routine)) = stack.pop() {
        for statement in list {
            if statement.span.start > span.start || span.end > statement.span.end {
                continue;
            }
            if let Some(found) = declares(statement, span, routine) {
                return Some(found);
            }
            let inner = match &statement.kind {
                StatementKind::Procedure { .. }
                | StatementKind::Function { .. }
                | StatementKind::Method { .. }
                | StatementKind::Constructor { .. }
                | StatementKind::Property { .. }
                | StatementKind::DefineEvent { .. } => Some(statement),
                _ => routine,
            };
            if let StatementKind::VariableDeclarationList(list) = &statement.kind {
                stack.push((list, inner));
            }
            children(statement, |list| stack.push((list, inner)));
        }
    }
    None
}

/// `statement` as the declaration of the name at `span`, when it declares it.
fn declares<'a>(
    statement: &'a Statement,
    span: Span,
    routine: Option<&'a Statement>,
) -> Option<Declaration<'a>> {
    let found = |field| Declaration {
        statement,
        routine,
        field,
    };
    let name = match &statement.kind {
        StatementKind::VariableDeclaration { name, .. }
        | StatementKind::Procedure { name, .. }
        | StatementKind::Function { name, .. }
        | StatementKind::Method { name, .. }
        | StatementKind::Property { name, .. }
        | StatementKind::DefineBuffer { name, .. }
        | StatementKind::DefineDataset { name, .. }
        | StatementKind::DefineDataSource { name, .. }
        | StatementKind::DefineEvent { name, .. }
        | StatementKind::DefineStream { name }
        | StatementKind::DefineFrame { name, .. }
        | StatementKind::DefineWidget { name }
        | StatementKind::Class { name, .. }
        | StatementKind::Interface { name, .. } => name,
        StatementKind::DefineParameter { param_type, .. } => match param_type {
            ParameterType::Variable { name, .. }
            | ParameterType::Handle { name, .. }
            | ParameterType::Buffer { name, .. } => name,
        },
        StatementKind::DefineTempTable { name, fields, .. } => {
            if let Some(field) = fields.iter().find(|field| field.name.span == span) {
                return Some(found(Some(field)));
            }
            name
        }
        _ => return None,
    };
    (name.span == span).then(|| found(None))
}

/// Whether class member `member` declares `name` as a symbol of `kind`.
fn member_named(member: &Statement, kind: SymbolKind, name: &str) -> bool {
    let declared = match (&member.kind, kind) {
        (StatementKind::Method { name, .. }, SymbolKind::Function) => name,
        (StatementKind::Property { name, .. }, SymbolKind::Property) => name,
        (StatementKind::VariableDeclaration { name, .. }, SymbolKind::Variable) => name,
        (StatementKind::DefineEvent { name, .. }, SymbolKind::Event) => name,
        (StatementKind::DefineBuffer { name, .. }, SymbolKind::Buffer) => name,
        (StatementKind::DefineTempTable { name, .. }, SymbolKind::TempTable) => name,
        (StatementKind::DefineDataset { name, .. }, SymbolKind::Dataset) => name,
        _ => return false,
    };
    declared.name.eq_ignore_ascii_case(name)
}

/// `CLASS a.B INHERITS a.Base IMPLEMENTS a.I`, or the `INTERFACE` header.
fn class_header(statement: &Statement) -> Option<String> {
    match &statement.kind {
        StatementKind::Class {
            name,
            inherits,
            implements,
            is_abstract,
            is_final,
            ..
        } => {
            let mut header = format!("CLASS {}", name.name);
            if let Some(parent) = inherits {
                header.push_str(&format!(" INHERITS {}", parent.name));
            }
            if !implements.is_empty() {
                let names: Vec<&str> = implements.iter().map(|i| i.name.as_str()).collect();
                header.push_str(&format!(" IMPLEMENTS {}", names.join(", ")));
            }
            if *is_abstract {
                header.push_str(" ABSTRACT");
            }
            if *is_final {
                header.push_str(" FINAL");
            }
            Some(header)
        }
        StatementKind::Interface { name, inherits, .. } => {
            let mut header = format!("INTERFACE {}", name.name);
            if !inherits.is_empty() {
                let names: Vec<&str> = inherits.iter().map(|i| i.name.as_str()).collect();
                header.push_str(&format!(" INHERITS {}", names.join(", ")));
            }
            Some(header)
        }
        _ => None,
    }
}

/// The start of every statement, sorted.
fn statement_starts(statements: &[Statement]) -> Vec<u32> {
    let mut starts = Vec::new();
    let mut stack = vec![statements];
    while let Some(list) = stack.pop() {
        for statement in list {
            starts.push(statement.span.start);
            if let StatementKind::VariableDeclarationList(list) = &statement.kind {
                stack.push(list);
            }
            children(statement, |list| stack.push(list));
        }
    }
    starts.sort_unstable();
    starts
}

/// The documented statement with the most words that `words` start with, and
/// how many tokens its head takes.
fn longest_statement<'d>(source: &Source, words: &[&Token]) -> Option<(&'d KeywordDoc, usize)> {
    let first = words.first()?;
    let spells = |token: &Token, word: &str| word_matches(token, source.token_text(token), word);
    let mut best: Option<(&KeywordDoc, usize)> = None;
    for doc in KEYWORD_DOCS
        .iter()
        .filter(|doc| doc.kind == DocKind::Statement && spells(first, doc.words[0]))
    {
        let mut at = 1;
        let mut matched = true;
        for word in &doc.words[1..] {
            while let Some(token) = words.get(at)
                && !spells(token, word)
                && (token.kind == Kind::Comma || DEFINE_MODIFIERS.iter().any(|m| spells(token, m)))
            {
                at += 1;
            }
            match words.get(at) {
                Some(token) if spells(token, word) => at += 1,
                _ => {
                    matched = false;
                    break;
                }
            }
        }
        if matched && best.is_none_or(|(kept, _)| doc.words.len() > kept.words.len()) {
            best = Some((doc, at));
        }
    }
    best
}

/// Whether `token`, written as `text`, spells `word`: the same text, or the
/// same keyword through an abbreviation (`DEF` for `DEFINE`).
fn word_matches(token: &Token, text: &str, word: &str) -> bool {
    if text.eq_ignore_ascii_case(word) {
        return true;
    }
    if token.kind == Kind::Identifier {
        return false;
    }
    let lexed = tokenize(word);
    matches!(lexed.first(), Some(first) if first.kind == token.kind && first.end == word.len())
}

/// The full name of a built-in function written as `name`, which may be an
/// abbreviation.
fn full_function_name(name: &str) -> &str {
    let lower = name.to_ascii_lowercase();
    match FUNCTION_ABBREVIATIONS.binary_search_by(|(abbrev, _)| (*abbrev).cmp(lower.as_str())) {
        Ok(at) => FUNCTION_ABBREVIATIONS[at].1,
        Err(_) => name,
    }
}

fn find_doc(kind: DocKind, name: &str) -> Option<&'static KeywordDoc> {
    KEYWORD_DOCS.iter().find(|doc| {
        doc.kind == kind && doc.words.len() == 1 && doc.words[0].eq_ignore_ascii_case(name)
    })
}

fn doc_markdown(doc: &KeywordDoc) -> String {
    let what = match doc.kind {
        DocKind::Statement => "An ABL statement.",
        DocKind::Function => "An ABL built-in function.",
        DocKind::SystemHandle => "An ABL system handle.",
    };
    format!(
        "**{}**\n\n{what} [Progress documentation]({})",
        plain(doc.title),
        doc.url
    )
}

/// A parsed text, for rendering the declarations in it.
struct Source<'t> {
    text: &'t str,
    tokens: &'t [Token],
}

impl<'t> Source<'t> {
    fn new(text: &'t str, tokens: &'t [Token]) -> Self {
        Source { text, tokens }
    }

    fn token_text(&self, token: &Token) -> &'t str {
        self.text.get(token.start..token.end).unwrap_or_default()
    }

    fn slice(&self, span: Span) -> &'t str {
        self.text
            .get(span.start as usize..span.end as usize)
            .unwrap_or_default()
    }

    /// The tokens that start in `from..to`, comments left out.
    fn tokens_in(&self, from: u32, to: u32) -> impl Iterator<Item = (usize, &'t Token)> {
        let first = self.tokens.partition_point(|t| t.start < from as usize);
        self.tokens[first..]
            .iter()
            .enumerate()
            .take_while(move |(_, t)| t.start < to as usize)
            .filter(|(_, t)| t.kind != Kind::Comment)
            .map(move |(n, t)| (first + n, t))
    }

    /// The end of the options after a declared name: the first `GET`, `SET` or
    /// `:` of a property, else `to`.
    fn options_end(&self, from: u32, to: u32) -> u32 {
        self.tokens_in(from, to)
            .find(|(_, t)| matches!(t.kind, Kind::Get | Kind::Set | Kind::Colon))
            .map_or(to, |(_, t)| t.start as u32)
    }

    /// ` EXTENT n` as written in `from..to`.
    fn extent(&self, from: u32, to: u32) -> Option<String> {
        let mut tokens = self.tokens_in(from, to).map(|(_, t)| t);
        tokens.find(|t| t.kind == Kind::Extent)?;
        match tokens.next() {
            Some(t) if matches!(t.kind, Kind::IntegerLiteral | Kind::Preprop) => {
                Some(format!(" EXTENT {}", self.token_text(t)))
            }
            _ => Some(" EXTENT".to_string()),
        }
    }

    /// ` INITIAL v` as written in `from..to`: one constant, or a bracketed list.
    fn initial(&self, from: u32, to: u32) -> Option<String> {
        let mut tokens = self.tokens_in(from, to).map(|(_, t)| t);
        tokens.find(|t| t.kind == Kind::Initial)?;
        let first = tokens.next()?;
        let end = match first.kind {
            Kind::LeftBracket => {
                let mut depth = 1;
                let mut end = first.end;
                for t in tokens.by_ref() {
                    end = t.end;
                    match t.kind {
                        Kind::LeftBracket => depth += 1,
                        Kind::RightBracket => depth -= 1,
                        _ => {}
                    }
                    if depth == 0 {
                        break;
                    }
                }
                end
            }
            Kind::Minus => tokens.next().map_or(first.end, |t| t.end),
            _ => first.end,
        };
        let written = self.text.get(first.start..end)?;
        Some(format!(
            " INITIAL {}",
            written.split_whitespace().collect::<Vec<_>>().join(" ")
        ))
    }

    /// ` AS INTEGER` or ` LIKE Customer.Name`; empty for a type that an
    /// `&IF` chooses, which has no one name.
    fn type_text(type_source: &TypeSource) -> String {
        match type_source {
            TypeSource::Explicit(data_type) => type_name(data_type)
                .map(|name| format!(" AS {name}"))
                .unwrap_or_default(),
            TypeSource::Like { source } => format!(" LIKE {}", source.name),
        }
    }

    /// The hover of a declaration: a code block, and a line on where it is.
    fn declaration(&self, found: &Declaration) -> Option<String> {
        let statement = found.statement;
        let span = statement.span;
        if let (Some(field), StatementKind::DefineTempTable { name, .. }) =
            (found.field, &statement.kind)
        {
            let options = (field.name.span.end, field.span.end.max(field.name.span.end));
            let mut line = format!(
                "FIELD {}{}",
                field.name.name,
                Self::type_text(&field.type_source)
            );
            line.push_str(&self.extent(options.0, options.1).unwrap_or_default());
            line.push_str(&self.initial(options.0, options.1).unwrap_or_default());
            return Some(format!(
                "{}\n\nField of temp-table {}.",
                code(&line),
                inline(&name.name)
            ));
        }
        let text = match &statement.kind {
            StatementKind::VariableDeclaration {
                name,
                type_source,
                no_undo,
                extent,
                access,
                is_static,
                is_new_shared,
                is_shared,
                is_new_global_shared,
                ..
            } => {
                let mut line = String::from("DEFINE");
                if *is_new_global_shared {
                    line.push_str(" NEW GLOBAL SHARED");
                } else if *is_new_shared {
                    line.push_str(" NEW SHARED");
                } else if *is_shared {
                    line.push_str(" SHARED");
                }
                push_access(&mut line, *access, *is_static);
                line.push_str(&format!(
                    " VARIABLE {}{}",
                    name.name,
                    Self::type_text(type_source)
                ));
                let written = self.extent(name.span.end, span.end).or(match extent {
                    Some(0) => Some(" EXTENT".to_string()),
                    Some(n) => Some(format!(" EXTENT {n}")),
                    None => None,
                });
                line.push_str(&written.unwrap_or_default());
                line.push_str(&self.initial(name.span.end, span.end).unwrap_or_else(|| {
                    initial_value_text(self, &statement.kind).unwrap_or_default()
                }));
                if *no_undo {
                    line.push_str(" NO-UNDO");
                }
                code(&line)
            }
            StatementKind::DefineParameter {
                direction,
                param_type,
            } => {
                let inline_form = matches!(
                    found.routine.map(|r| &r.kind),
                    Some(
                        StatementKind::Function { .. }
                            | StatementKind::Method { .. }
                            | StatementKind::Constructor { .. }
                            | StatementKind::Property { .. }
                            | StatementKind::DefineEvent { .. }
                    )
                );
                let mut line = if inline_form {
                    self.parameter(statement)?
                } else {
                    let direction = direction_name(direction);
                    match param_type {
                        ParameterType::Buffer { name, target } => {
                            format!("DEFINE PARAMETER BUFFER {} FOR {}", name.name, target.name)
                        }
                        ParameterType::Handle { name, .. } => format!(
                            "DEFINE {direction} PARAMETER {}{}",
                            self.parameter_tail(param_type),
                            self.passing(name.span.end, span.end)
                        ),
                        ParameterType::Variable { name, .. } => format!(
                            "DEFINE {direction} PARAMETER {}{}",
                            self.parameter_tail(param_type),
                            self.extent(name.span.end, span.end).unwrap_or_default()
                        ),
                    }
                };
                if let ParameterType::Variable { no_undo: true, .. } = param_type {
                    line.push_str(" NO-UNDO");
                }
                let mut out = code(&line);
                if let Some(owner) = found.routine.and_then(routine_label) {
                    out.push_str(&format!("\n\nParameter of {}.", inline(&owner)));
                }
                out
            }
            StatementKind::Property {
                access,
                is_static,
                name,
                data_type,
                no_undo,
                ..
            } => {
                let mut line = String::from("DEFINE");
                push_access(&mut line, Some(*access), *is_static);
                line.push_str(&format!(" PROPERTY {}", name.name));
                if let Some(type_name) = type_name(data_type) {
                    line.push_str(&format!(" AS {type_name}"));
                }
                let end = self.options_end(name.span.end, span.end);
                line.push_str(&self.extent(name.span.end, end).unwrap_or_default());
                line.push_str(&self.initial(name.span.end, end).unwrap_or_default());
                if *no_undo {
                    line.push_str(" NO-UNDO");
                }
                line.push_str(&self.accessors(statement));
                code(&line)
            }
            StatementKind::Procedure { .. }
            | StatementKind::Function { .. }
            | StatementKind::Method { .. } => code(&self.signature(statement)?),
            StatementKind::DefineTempTable {
                name,
                like_table,
                no_undo,
                ..
            } => {
                let mut line = format!("DEFINE TEMP-TABLE {}", name.name);
                if let Some(like) = like_table {
                    line.push_str(&format!(" LIKE {}", like.name));
                }
                if *no_undo {
                    line.push_str(" NO-UNDO");
                }
                code(&line)
            }
            StatementKind::DefineBuffer { name, target, .. } => code(&match target {
                BufferTarget::Table(table) => {
                    format!("DEFINE BUFFER {} FOR {}", name.name, table.name)
                }
                BufferTarget::TempTable(table) => {
                    format!("DEFINE BUFFER {} FOR TEMP-TABLE {}", name.name, table.name)
                }
            }),
            StatementKind::DefineDataset { name, .. } => {
                code(&format!("DEFINE DATASET {}", name.name))
            }
            StatementKind::DefineDataSource { name, .. } => {
                code(&format!("DEFINE DATA-SOURCE {}", name.name))
            }
            StatementKind::DefineEvent { name, .. } => code(&format!("DEFINE EVENT {}", name.name)),
            StatementKind::DefineStream { name } => code(&format!("DEFINE STREAM {}", name.name)),
            StatementKind::DefineFrame { name, .. } => code(&format!("DEFINE FRAME {}", name.name)),
            StatementKind::Class { .. } | StatementKind::Interface { .. } => {
                code(&class_header(statement)?)
            }
            _ => return None,
        };
        if found.chosen_type() {
            return Some(format!("{text}\n\nAn `&IF` chooses the type."));
        }
        Some(text)
    }

    /// `PROCEDURE p (INPUT a AS CHARACTER)`, `FUNCTION f RETURNS INTEGER (...)`
    /// or `METHOD PUBLIC VOID m (...)`.
    fn signature(&self, statement: &Statement) -> Option<String> {
        let (head, parameters): (String, Vec<&Statement>) = match &statement.kind {
            StatementKind::Procedure { name, body, .. } => {
                (format!("PROCEDURE {}", name.name), parameters_of(body))
            }
            StatementKind::Function {
                name,
                return_type,
                body,
                prototype_parameters,
            } => {
                let list = if prototype_parameters.is_empty() {
                    body
                } else {
                    prototype_parameters
                };
                let mut head = format!("FUNCTION {}", name.name);
                if let Some(returns) = type_name(return_type) {
                    head.push_str(&format!(" RETURNS {returns}"));
                }
                (head, parameters_of(list))
            }
            StatementKind::Method {
                access,
                is_static,
                is_abstract,
                is_override,
                return_type,
                name,
                parameters,
                ..
            } => {
                let mut head = String::from("METHOD");
                push_access(&mut head, Some(*access), *is_static);
                if *is_abstract {
                    head.push_str(" ABSTRACT");
                }
                if *is_override {
                    head.push_str(" OVERRIDE");
                }
                if self.method_is_final(statement) {
                    head.push_str(" FINAL");
                }
                match return_type {
                    None => head.push_str(" VOID"),
                    Some(data_type) => {
                        if let Some(returns) = type_name(data_type) {
                            head.push(' ');
                            head.push_str(&returns);
                        }
                    }
                }
                head.push_str(&format!(" {}", name.name));
                (head, parameters_of(parameters))
            }
            _ => return None,
        };
        let parameters: Vec<String> = parameters
            .into_iter()
            .filter_map(|p| self.parameter(p))
            .collect();
        let tail = self.prototype_tail(statement);
        let is_method = matches!(statement.kind, StatementKind::Method { .. });
        if parameters.is_empty() {
            let parens = if is_method { " ()" } else { "" };
            return Some(format!("{head}{parens}{tail}"));
        }
        let one_line = format!("{head} ({}){tail}", parameters.join(", "));
        if one_line.chars().count() <= SIGNATURE_WIDTH {
            return Some(one_line);
        }
        Some(format!(
            "{head} (\n    {}){tail}",
            parameters.join(",\n    ")
        ))
    }

    /// What a declaration without an ABL body writes after its parameters,
    /// as written: ` IN SUPER`, ` FORWARD`, ` MAP TO x IN h`,
    /// ` EXTERNAL "lib.so" CDECL`. Empty for a definition with a body.
    fn prototype_tail(&self, statement: &Statement) -> String {
        let (name, prototype) = match &statement.kind {
            StatementKind::Procedure {
                name,
                is_prototype,
                is_external,
                ..
            } => (name, *is_prototype || *is_external),
            StatementKind::Function { name, body, .. } => (name, body.is_empty()),
            _ => return String::new(),
        };
        if !prototype {
            return String::new();
        }
        let mut depth = 0usize;
        let mut start = None;
        let mut end = statement.span.end;
        for (_, token) in self.tokens_in(name.span.end, statement.span.end) {
            match token.kind {
                Kind::LeftParen => depth += 1,
                Kind::RightParen => depth = depth.saturating_sub(1),
                Kind::Colon | Kind::Period if depth == 0 => {
                    end = token.start as u32;
                    break;
                }
                _ => {}
            }
            let text = self.token_text(token);
            let opens = token.kind == Kind::External
                || token.kind == Kind::KwIn
                || ["FORWARD", "MAP"]
                    .iter()
                    .any(|w| text.eq_ignore_ascii_case(w));
            if depth == 0 && start.is_none() && opens {
                start = Some(token.start as u32);
            }
        }
        match start {
            Some(start) if start < end => {
                let written = self.slice(Span { start, end });
                format!(
                    " {}",
                    written.split_whitespace().collect::<Vec<_>>().join(" ")
                )
            }
            _ => String::new(),
        }
    }

    /// Whether the header of a `METHOD` writes `FINAL`.
    fn method_is_final(&self, statement: &Statement) -> bool {
        let StatementKind::Method { name, .. } = &statement.kind else {
            return false;
        };
        self.tokens_in(statement.span.start, name.span.start)
            .any(|(_, t)| t.kind == Kind::Final)
    }

    /// ` GET. PRIVATE SET.`: the accessors of a property, each with the access
    /// modifier written before it.
    fn accessors(&self, statement: &Statement) -> String {
        let StatementKind::Property {
            name,
            get_body,
            set_body,
            ..
        } = &statement.kind
        else {
            return String::new();
        };
        let mut out = String::new();
        let mut from = self.options_end(name.span.end, statement.span.end);
        for (body, kind, word) in [(get_body, Kind::Get, "GET"), (set_body, Kind::Set, "SET")] {
            let Some(body) = body else {
                continue;
            };
            let tokens: Vec<(usize, &Token)> = self.tokens_in(from, statement.span.end).collect();
            let Some(at) = tokens.iter().position(|(_, t)| t.kind == kind) else {
                continue;
            };
            out.push(' ');
            if at > 0
                && matches!(
                    tokens[at - 1].1.kind,
                    Kind::Private
                        | Kind::Protected
                        | Kind::Public
                        | Kind::PackagePrivate
                        | Kind::PackageProtected
                )
            {
                out.push_str(&self.token_text(tokens[at - 1].1).to_ascii_uppercase());
                out.push(' ');
            }
            out.push_str(word);
            out.push('.');
            // The next accessor starts after this one's body.
            from = body
                .last()
                .map_or(tokens[at].1.end as u32, |last| last.span.end);
        }
        out
    }

    /// A parameter as a signature writes it: `INPUT a AS CHARACTER`.
    fn parameter(&self, statement: &Statement) -> Option<String> {
        let StatementKind::DefineParameter {
            direction,
            param_type,
        } = &statement.kind
        else {
            return None;
        };
        if let ParameterType::Buffer { name, target } = param_type {
            return Some(format!("BUFFER {} FOR {}", name.name, target.name));
        }
        let mut text = format!(
            "{} {}",
            direction_name(direction),
            self.parameter_tail(param_type)
        );
        match param_type {
            ParameterType::Variable { name, .. } => text.push_str(
                &self
                    .extent(name.span.end, statement.span.end)
                    .unwrap_or_default(),
            ),
            ParameterType::Handle { name, .. } => {
                text.push_str(&self.passing(name.span.end, statement.span.end));
            }
            ParameterType::Buffer { .. } => {}
        }
        Some(text)
    }

    /// What follows the direction: `a AS CHARACTER`, `TABLE FOR tt`.
    fn parameter_tail(&self, param_type: &ParameterType) -> String {
        match param_type {
            ParameterType::Variable {
                name, type_source, ..
            } => {
                let type_text = Self::type_text(type_source);
                // The parser names an unnamed prototype parameter `$N`.
                if name.name.starts_with('$') {
                    type_text.trim_start().to_string()
                } else {
                    format!("{}{type_text}", name.name)
                }
            }
            ParameterType::Handle { kind, name, .. } => match kind {
                HandleParamKind::Table => format!("TABLE FOR {}", name.name),
                HandleParamKind::TableHandle => format!("TABLE-HANDLE {}", name.name),
                HandleParamKind::Dataset => format!("DATASET FOR {}", name.name),
                HandleParamKind::DatasetHandle => format!("DATASET-HANDLE {}", name.name),
            },
            ParameterType::Buffer { name, target } => {
                format!("BUFFER {} FOR {}", name.name, target.name)
            }
        }
    }
}

impl Source<'_> {
    /// ` APPEND BIND BY-VALUE BY-REFERENCE` as written after a table or
    /// dataset parameter's name, in `from..to`.
    fn passing(&self, from: u32, to: u32) -> String {
        let mut out = String::new();
        for (_, token) in self.tokens_in(from, to) {
            if matches!(
                token.kind,
                Kind::Append | Kind::Bind | Kind::ByValue | Kind::ByReference
            ) {
                out.push(' ');
                out.push_str(&self.token_text(token).to_ascii_uppercase());
            }
        }
        out
    }
}

/// The `INITIAL` expression the parser kept, as written.
fn initial_value_text(source: &Source, kind: &StatementKind) -> Option<String> {
    let StatementKind::VariableDeclaration {
        initial_value: Some(value),
        ..
    } = kind
    else {
        return None;
    };
    let written = source.slice(value.span);
    (!written.is_empty()).then(|| {
        format!(
            " INITIAL {}",
            written.split_whitespace().collect::<Vec<_>>().join(" ")
        )
    })
}

/// The parameter definitions that lead a routine's statement list.
fn parameters_of(list: &[Statement]) -> Vec<&Statement> {
    list.iter()
        .filter(|s| matches!(s.kind, StatementKind::DefineParameter { .. }))
        .collect()
}

/// `PROCEDURE p`, `FUNCTION f` or `METHOD m`, for the routine that owns a
/// parameter.
fn routine_label(routine: &Statement) -> Option<String> {
    match &routine.kind {
        StatementKind::Procedure { name, .. } => Some(format!("PROCEDURE {}", name.name)),
        StatementKind::Function { name, .. } => Some(format!("FUNCTION {}", name.name)),
        StatementKind::Method { name, .. } => Some(format!("METHOD {}", name.name)),
        StatementKind::Property { name, .. } => Some(format!("PROPERTY {}", name.name)),
        StatementKind::DefineEvent { name, .. } => Some(format!("EVENT {}", name.name)),
        StatementKind::Constructor { .. } => Some("CONSTRUCTOR".to_string()),
        _ => None,
    }
}

fn push_access(line: &mut String, access: Option<AccessModifier>, is_static: bool) {
    if let Some(access) = access {
        line.push(' ');
        line.push_str(access_name(access));
    }
    if is_static {
        line.push_str(" STATIC");
    }
}

/// The type as ABL writes it; `None` for a type an `&IF` chooses.
fn type_name(data_type: &DataType) -> Option<String> {
    data_type_name(data_type)
}
