//! The call fill: the arguments a call item inserts after its name, one
//! snippet placeholder for each parameter. See `docs/design/call-fill.md`.
//!
//! The parameters come from the resolution of signature help, never from a
//! reading of their own: [`Navigation::declared_callee`] and
//! [`Navigation::builtin_callees`] for the items whose declaration the base
//! holds, and [`Navigation::callees_at`] for a call that the editor asks for
//! after it inserted the name ([`fill_call`]). A callee that signature help
//! does not resolve gets no fill.

use std::collections::HashMap;
use std::ops::Range;
use std::path::Path;

use oxabl_lexer::Kind;
use oxabl_semantic::{ParamMode, SymbolId, SymbolKind};
use oxabl_style::KeywordCase;

use super::base::Mapped;
use super::case::Case;
use super::context::Context;
use super::{CompletionBase, CompletionItem, Engine, ItemData, ItemKind};
use crate::keyword_docs::KEYWORD_DOCS;
use crate::navigate::{ArgumentKeyword, Callee};
use crate::{Navigation, close_open_call};

/// `oxabl.completion.callArguments`: what a call item inserts after its name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CallArguments {
    /// One placeholder for each parameter.
    #[default]
    Fill,
    /// The parentheses, with the cursor in them.
    Parentheses,
    /// The name only.
    None,
}

impl CallArguments {
    /// The value of the setting; `None` for a value it does not know.
    pub fn parse(value: &str) -> Option<CallArguments> {
        match value {
            "fill" => Some(CallArguments::Fill),
            "parentheses" => Some(CallArguments::Parentheses),
            "none" => Some(CallArguments::None),
            _ => None,
        }
    }
}

/// What the client does after it inserts an item.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ItemAction {
    #[default]
    None,
    /// Show signature help: the item opened a call.
    ParameterHints,
    /// Ask the server for the fill of the call whose name the item wrote
    /// (`oxabl/callArguments`): the base cannot give it.
    FillCall,
}

/// How the call is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Form {
    /// `RUN name (args).`
    Run,
    /// `name(args)`.
    Function,
}

/// Where the parameters of an item come from.
enum Source {
    Declared(SymbolId),
    Builtin(&'static str),
    /// Only an analysis of the call can tell: the editor asks after the
    /// insert.
    Later,
}

impl Engine<'_, '_, '_, '_> {
    /// Add the call fill of `item` to its insert text, or the action that
    /// asks for it later.
    pub(super) fn fill(&self, item: &mut CompletionItem) {
        let mode = self.request.call_arguments;
        if mode == CallArguments::None || !self.request.snippets || item.snippet {
            return;
        }
        let form = match self.found.context {
            Context::Run => Form::Run,
            Context::Values { .. } | Context::Where => Form::Function,
            _ => return,
        };
        let text = self.request.text;
        let rest = rest_of_line(text, self.found.word_end as usize);
        // The call already has its arguments.
        if rest.trim_start().starts_with('(') {
            return;
        }
        let source = match (item.kind, &item.data, form) {
            (ItemKind::Function, ItemData::Symbol(sym), _) => Source::Declared(SymbolId::new(*sym)),
            (ItemKind::Function, ItemData::Doc(at), Form::Function) => {
                match KEYWORD_DOCS.get(*at as usize) {
                    Some(doc) => Source::Builtin(doc.words[0]),
                    None => return,
                }
            }
            // A `PROCEDURE` that only the text shows, and a program.
            (ItemKind::Function, ItemData::None, Form::Run)
            | (ItemKind::File, ItemData::Path(_), Form::Run) => Source::Later,
            // A method gets its fill from `Navigation::callees_at` through
            // `Source::Later`, or from the declarations its item names, when
            // member completion offers it.
            _ => return,
        };
        let callees = match source {
            Source::Later => None,
            Source::Declared(sym) => match self.declared(sym) {
                Some(Ok(callee)) => Some(vec![callee]),
                Some(Err(())) => None,
                None => return,
            },
            Source::Builtin(name) => match self.based() {
                Some((base, _)) => match base.navigation.builtin_callees(name) {
                    Some(callees) => Some(callees),
                    None => return,
                },
                None => return,
            },
        };
        let Some(callees) = callees else {
            item.action = ItemAction::FillCall;
            return;
        };
        let period = form == Form::Run && rest.trim().is_empty();
        let Some((suffix, action)) = render(&callees, form, mode, self.case, period) else {
            return;
        };
        item.insert = format!("{}{suffix}", escape(&item.insert));
        item.snippet = true;
        item.action = action;
    }

    /// The declaration of `sym` in the base. `Some(Err(()))` when the base
    /// may hold an older text of it, or has no table of its declarations:
    /// the editor asks after the insert. `None` when signature help would
    /// not read it either.
    fn declared(&self, sym: SymbolId) -> Option<Result<Callee, ()>> {
        let (base, mapped) = self.based()?;
        let Some(callees) = base.callees else {
            return Some(Err(()));
        };
        let (callee, span) = callees.0.get(&sym.raw())?.as_ref()?;
        if base.stale || touches_change(base, mapped, span.clone()) {
            return Some(Err(()));
        }
        Some(Ok(callee.clone()))
    }
}

/// The declarations of the internal procedures and user functions of one
/// analysis, as signature help reads them, by the raw id of their symbol,
/// each with the bytes of its statement in the expanded text. `None` for a
/// declaration that signature help does not read. Made once for each base,
/// off the request path: it parses the expanded text again.
#[derive(Debug, Default)]
pub struct DeclaredCallees(HashMap<u32, Option<(Callee, Range<u32>)>>);

impl DeclaredCallees {
    pub fn of(navigation: &Navigation<'_, '_>) -> DeclaredCallees {
        let symbols = &navigation.model.semantic.symbols;
        DeclaredCallees(
            symbols
                .iter()
                .filter(|(sym, symbol)| {
                    matches!(symbol.kind, SymbolKind::Procedure | SymbolKind::Function)
                        && !symbols.is_method(*sym)
                })
                .map(|(sym, _)| (sym.raw(), navigation.declared_callee(sym)))
                .collect(),
        )
    }
}

/// Whether the current text changed the bytes of the declaration at
/// `span` (expanded bytes of the base).
fn touches_change(base: &CompletionBase<'_, '_, '_>, mapped: &Mapped, span: Range<u32>) -> bool {
    if mapped.changed.is_none() {
        return false;
    }
    let expanded = &base.navigation.expanded;
    let (Some((start_file, start)), Some((end_file, end))) = (
        expanded.resolve_offset(span.start),
        expanded.resolve_offset(span.end.saturating_sub(1)),
    ) else {
        return true;
    };
    if start_file != end_file {
        return true;
    }
    mapped.touches_changed(
        start_file,
        oxabl_ast::Span {
            start,
            end: end + 1,
        },
    )
}

/// The text after the name, and what the client does next. `None` when
/// the item inserts its name only.
fn render(
    callees: &[Callee],
    form: Form,
    mode: CallArguments,
    case: Case,
    period: bool,
) -> Option<(String, ItemAction)> {
    let open = match form {
        Form::Run => " (",
        Form::Function => "(",
    };
    let end = if period { "." } else { "" };
    let parentheses = || Some((format!("{open}$0){end}"), ItemAction::ParameterHints));
    let [callee] = callees else {
        // Several declarations: the arguments pick one, so signature help
        // shows them all.
        return match callees {
            [] => None,
            _ => parentheses(),
        };
    };
    if callee.parameters.is_empty() {
        return match form {
            Form::Run if period => Some((".$0".to_string(), ItemAction::None)),
            Form::Run => None,
            // A built-in function whose syntax has no parentheses (`TODAY`).
            Form::Function if !callee.label.contains('(') => None,
            Form::Function => Some(("()$0".to_string(), ItemAction::None)),
        };
    }
    let required: Vec<_> = callee.parameters.iter().filter(|p| !p.optional).collect();
    if mode == CallArguments::Parentheses || required.is_empty() {
        return parentheses();
    }
    let mut text = String::from(open);
    for (at, param) in required.iter().enumerate() {
        if at > 0 {
            text.push_str(", ");
        }
        match param.mode {
            Some(ParamMode::Output) => text.push_str(&case.apply("OUTPUT ")),
            Some(ParamMode::InputOutput) => text.push_str(&case.apply("INPUT-OUTPUT ")),
            Some(ParamMode::Input) | None => {}
        }
        let keyword = match param.keyword {
            ArgumentKeyword::None => "",
            ArgumentKeyword::Buffer => "BUFFER ",
            ArgumentKeyword::Table => "TABLE ",
            ArgumentKeyword::TableHandle => "TABLE-HANDLE ",
            ArgumentKeyword::Dataset => "DATASET ",
            ArgumentKeyword::DatasetHandle => "DATASET-HANDLE ",
        };
        text.push_str(&case.apply(keyword));
        text.push_str(&format!("${{{}:{}}}", at + 1, escape(&param.name)));
    }
    text.push(')');
    text.push_str(end);
    text.push_str("$0");
    Some((text, ItemAction::ParameterHints))
}

/// `text` with the characters a snippet gives a meaning escaped.
fn escape(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for c in text.chars() {
        if matches!(c, '\\' | '$' | '}') {
            out.push('\\');
        }
        out.push(c);
    }
    out
}

/// The text from byte `at` to the end of its line.
fn rest_of_line(text: &str, at: usize) -> &str {
    let rest = text.get(at..).unwrap_or_default();
    rest.find(['\n', '\r']).map_or(rest, |end| &rest[..end])
}

/// A text with the call after a name opened, for the analysis that
/// [`fill_call`] reads.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpenedCall {
    /// The text with `()` after the name, and a period at the end of the line
    /// when the statement needs one to parse.
    pub text: String,
    /// The byte inside the parentheses.
    pub at: u32,
}

/// `text` with a call opened after the name that ends at byte `offset`.
/// `None` when no name ends there, or the call already has its parentheses.
pub fn open_call(text: &str, offset: u32) -> Option<OpenedCall> {
    let offset = offset as usize;
    if !text.is_char_boundary(offset) || offset == 0 {
        return None;
    }
    let name_end = text[..offset]
        .chars()
        .next_back()
        .is_some_and(|c| c.is_alphanumeric() || matches!(c, '-' | '_' | '#' | '$' | '%' | '&'));
    if !name_end || rest_of_line(text, offset).trim_start().starts_with('(') {
        return None;
    }
    let open = if after_run(text, offset) { " (" } else { "(" };
    let mut opened = String::with_capacity(text.len() + 4);
    opened.push_str(&text[..offset]);
    opened.push_str(open);
    opened.push(')');
    opened.push_str(&text[offset..]);
    let at = (offset + open.len()) as u32;
    let text = close_open_call(&opened, at).unwrap_or(opened);
    Some(OpenedCall { text, at })
}

/// Whether the name that ends at byte `offset` is the target of a `RUN`.
fn after_run(text: &str, offset: usize) -> bool {
    let tokens = oxabl_lexer::tokenize(&text[..offset]);
    let mut words = tokens
        .iter()
        .rev()
        .filter(|t| !matches!(t.kind, Kind::Comment | Kind::Eof));
    // The name is at most a path of a few parts: `dir/sub/prog.p`.
    for _ in 0..16 {
        let Some(token) = words.next() else {
            return false;
        };
        match token.kind {
            Kind::Run => return true,
            Kind::Slash | Kind::Period | Kind::Identifier => {}
            kind if super::case::is_keyword(kind) => {}
            _ => return false,
        }
        if token.kind == Kind::Period && text[token.end..].starts_with(char::is_whitespace) {
            return false;
        }
    }
    false
}

/// The fill of the call that [`open_call`] opened, from `navigation`, the
/// analysis of `opened.text`, and whether the client then shows signature
/// help. `text` and `offset` are the document before the call was opened.
/// `None` when signature help does not resolve the call, or the fill writes
/// nothing.
pub fn fill_call(
    navigation: &Navigation<'_, '_>,
    file: &Path,
    text: &str,
    offset: u32,
    opened: &OpenedCall,
    keyword_case: KeywordCase,
    mode: CallArguments,
) -> Option<(String, bool)> {
    if mode == CallArguments::None {
        return None;
    }
    let call = navigation.callees_at(file, opened.at)?;
    let tokens = oxabl_lexer::tokenize(text);
    let before = tokens.partition_point(|t| t.end <= offset as usize);
    let case = Case::of(keyword_case, "", text, &tokens, before);
    let form = if call.run { Form::Run } else { Form::Function };
    let period = call.run && rest_of_line(text, offset as usize).trim().is_empty();
    render(&call.callees, form, mode, case, period)
        .map(|(text, action)| (text, action == ItemAction::ParameterHints))
}
