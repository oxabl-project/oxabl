//! Code completion: the items for the cursor in a document. See
//! `docs/design/completion.md`.
//!
//! The context comes from the tokens of the current text ([`context`]); the
//! names, scopes and types come from a base, one analysis of the compilation
//! unit, which can be of an older text ([`base`]). The engine needs no salsa:
//! the daemon keeps the bases and hands one in.
//!
//! Completion fails closed. When oxabl does not know the context at the
//! cursor, the answer is `None`. A list that can miss items says so with
//! [`CompletionList::incomplete`].

mod base;
mod case;
mod context;
mod keywords;
mod names;
mod rank;
mod snippets;

use std::path::Path;

use oxabl_lexer::{Kind, Token};
use oxabl_semantic::{ScopeId, ScopeKind};
use oxabl_style::KeywordCase;

use crate::Navigation;
use crate::keyword_docs::KEYWORD_DOCS;
use base::Mapped;
use case::Case;
use context::{Context, Found};
use keywords::{Keyword, Place};
use names::Wanted;
use rank::{Quality, nearness};

/// The most items one answer holds. A longer list is cut after ranking and is
/// incomplete.
pub const LIMIT: usize = 200;

/// One completion request: the document as the editor holds it, and the
/// cursor.
pub struct CompletionRequest<'r> {
    /// The current text of the document.
    pub text: &'r str,
    /// The byte of the cursor in `text`.
    pub offset: u32,
    /// The document's path, as the base's expansion names it.
    pub path: &'r Path,
    /// `keyword_case` of the file's `[workspace.style]`.
    pub keyword_case: KeywordCase,
    /// Whether the client takes snippet items.
    pub snippets: bool,
    /// The character the person typed that made the client ask, when it
    /// asked for that reason.
    pub trigger: Option<char>,
}

/// The analysis completion reads, with the text of the document it was made
/// from.
pub struct CompletionBase<'b, 's, 'a> {
    pub navigation: &'b Navigation<'s, 'a>,
    /// The text of the document that the analysis read.
    pub text: &'b str,
    /// The document is an include analysed alone: the names of its includer
    /// are not known.
    pub fragment: bool,
    /// The base is older than the text of a file it read other than the
    /// document (the root of an include, or another include): its names can
    /// be out of date.
    pub stale: bool,
}

/// The answer to a [`CompletionRequest`]. Offsets are bytes of the current
/// text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompletionList {
    /// The list can miss items, or change as the person types: the client
    /// asks again at each key.
    pub incomplete: bool,
    pub items: Vec<CompletionItem>,
    /// The start of the word an item replaces.
    pub word_start: u32,
    /// The end of the word after the cursor.
    pub word_end: u32,
    /// The cursor.
    pub cursor: u32,
}

/// One item of a [`CompletionList`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompletionItem {
    pub label: String,
    pub kind: ItemKind,
    pub detail: Option<String>,
    /// Orders the items: see `docs/design/completion.md`, "Match, rank and
    /// limit".
    pub sort_text: String,
    /// The text that replaces the word.
    pub insert: String,
    /// `insert` is in snippet syntax.
    pub snippet: bool,
    /// What `completionItem/resolve` reads.
    pub data: ItemData,
}

/// What an item is, for its icon.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemKind {
    Keyword,
    Snippet,
    Variable,
    Property,
    Method,
    Function,
    Struct,
    Field,
    Class,
    Interface,
    Enum,
    EnumMember,
    Event,
    Module,
    File,
    Folder,
    Constant,
}

/// What resolve reads for an item.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ItemData {
    None,
    /// An entry of the keyword documentation index.
    Doc(u16),
    /// A symbol of the base the item came from, by its raw id.
    Symbol(u32),
}

/// An item before it is ranked.
struct Candidate {
    label: String,
    kind: ItemKind,
    detail: Option<String>,
    insert: String,
    snippet: bool,
    data: ItemData,
    fits: bool,
    nearness: u8,
}

impl Candidate {
    fn keyword(keyword: &Keyword, case: Case) -> Candidate {
        let label = case.apply(keyword.word);
        Candidate {
            insert: label.clone(),
            label,
            kind: ItemKind::Keyword,
            detail: keyword.title.map(str::to_string),
            snippet: false,
            data: keyword_doc(keyword).map_or(ItemData::None, ItemData::Doc),
            fits: false,
            nearness: nearness::KEYWORD,
        }
    }
}

/// The entry of the documentation index that a keyword item resolves to.
fn keyword_doc(keyword: &Keyword) -> Option<u16> {
    let title = keyword.title?;
    KEYWORD_DOCS
        .iter()
        .position(|doc| doc.title == title)
        .map(|at| at as u16)
}

/// The documentation of entry `doc` of the keyword documentation index, as
/// hover shows it.
pub fn keyword_documentation(doc: u16) -> Option<String> {
    KEYWORD_DOCS
        .get(doc as usize)
        .map(crate::navigate::hover::doc_markdown)
}

/// The documentation of symbol `symbol` (raw id) of the base `navigation`
/// reads, as the hover of its declaration shows it.
pub fn symbol_documentation(navigation: &Navigation<'_, '_>, symbol: u32) -> Option<String> {
    if symbol as usize >= navigation.model.semantic.symbols.len() {
        return None;
    }
    navigation.symbol_hover(oxabl_semantic::SymbolId::new(symbol))
}

/// The items for `request`. `base` is the analysis of the document, `None`
/// when none could be made. `None` when oxabl does not know what the cursor
/// needs.
pub fn complete(
    request: &CompletionRequest<'_>,
    base: Option<&CompletionBase<'_, '_, '_>>,
) -> Option<CompletionList> {
    let text = request.text;
    let tokens = oxabl_lexer::tokenize(text);
    let cursor = request.offset.min(text.len() as u32);
    let found = context::find(text, &tokens, cursor)?;
    if let Some(trigger) = request.trigger
        && !found.context.takes_trigger(trigger)
    {
        return None;
    }
    let word = &text[found.word_start as usize..cursor as usize];
    let case = Case::of(request.keyword_case, word, text, &tokens, found.before);
    let mapped = match base {
        Some(base) => Some(base::map(
            base,
            request.path,
            text,
            &tokens,
            cursor,
            found.window_start,
        )?),
        None => None,
    };
    let engine = Engine {
        request,
        base,
        mapped: mapped.as_ref(),
        tokens: &tokens,
        found: &found,
        case,
    };
    let (candidates, incomplete) = engine.collect()?;
    Some(engine.finish(candidates, incomplete, word, cursor))
}

/// What makes the items of one context.
type Collector = fn(&Engine<'_, '_, '_, '_>, &Context) -> Option<(Vec<Candidate>, bool)>;

/// The collector of `context`. `None` when no collector answers it yet: the
/// answer is `null`, and no base is needed.
fn collector(context: &Context) -> Option<Collector> {
    Some(match context {
        Context::StatementStart => |engine, _| engine.statement_start(),
        Context::DefineKeywords { .. } => |engine, context| match context {
            Context::DefineKeywords { modifiers } => engine.define_keywords(modifiers),
            _ => None,
        },
        Context::Values { .. } | Context::Where => |engine, _| engine.names(Wanted::Values),
        Context::Records(_) => |engine, context| match context {
            Context::Records(after) => engine.names(Wanted::Records(*after)),
            _ => None,
        },
        // The other contexts have no collector yet.
        _ => return None,
    })
}

/// Whether `request` can have an answer, so the caller needs a base for it.
/// False when the text at the cursor asks for nothing completion answers, or
/// when the trigger character does not start the context: then the answer is
/// `null` whatever the base, and the caller makes none.
pub fn wants_base(request: &CompletionRequest<'_>) -> bool {
    let text = request.text;
    let tokens = oxabl_lexer::tokenize(text);
    let cursor = request.offset.min(text.len() as u32);
    context::find(text, &tokens, cursor).is_some_and(|found| {
        request
            .trigger
            .is_none_or(|trigger| found.context.takes_trigger(trigger))
            && collector(&found.context).is_some()
    })
}

/// One request's state, shared by the collectors.
struct Engine<'e, 'b, 's, 'a> {
    request: &'e CompletionRequest<'e>,
    base: Option<&'e CompletionBase<'b, 's, 'a>>,
    mapped: Option<&'e Mapped>,
    tokens: &'e [Token],
    found: &'e Found,
    case: Case,
}

impl Engine<'_, '_, '_, '_> {
    /// The candidates of the context, and whether the list is incomplete.
    /// `None` when the context has no answer.
    fn collect(&self) -> Option<(Vec<Candidate>, bool)> {
        if self.found.reaches_start && !self.window_may_start_here() {
            return None;
        }
        collector(&self.found.context)?(self, &self.found.context)
    }

    /// Whether a window that reaches the start of the document starts a
    /// statement there. In a file that is its own root it does. In an include,
    /// the statement can have begun in the includer: in a selected root, the
    /// window starts here only when the expanded text before the include ends
    /// a statement; an include analysed alone cannot tell.
    fn window_may_start_here(&self) -> bool {
        let Some((base, mapped)) = self.based() else {
            return !is_include(self.request.path);
        };
        let navigation = base.navigation;
        let own_root = navigation.root_path().as_deref()
            == Some(oxabl_index::search::normalize_lexically(self.request.path).as_path());
        if own_root {
            return !base.fragment;
        }
        let text = navigation.expanded.text.as_bytes();
        mapped
            .ids
            .iter()
            .flat_map(|id| navigation.expanded.expanded_offsets(*id, 0))
            .all(|start| {
                let before = &text[..(start as usize).min(text.len())];
                let end = before
                    .iter()
                    .rposition(|b| !b.is_ascii_whitespace())
                    .map(|at| before[at]);
                matches!(end, None | Some(b'.' | b':'))
            })
    }

    /// The base and the cursor in it: both are needed by every collector
    /// that reads names or scopes.
    fn based(&self) -> Option<(&CompletionBase<'_, '_, '_>, &Mapped)> {
        Some((self.base?, self.mapped?))
    }

    /// Whether the list misses items whatever the collector: a stale base, an
    /// include analysed alone, or a parse error before the window.
    fn base_incomplete(&self) -> bool {
        let Some((base, mapped)) = self.based() else {
            return true;
        };
        mapped.stale
            || base.stale
            || base.fragment
            || base.navigation.model.recovered.is_some_and(|recovered| {
                mapped
                    .window_places
                    .first()
                    .is_none_or(|window| recovered.start < *window)
            })
    }

    /// The statement start: the statement keywords of the place, and the
    /// snippets.
    fn statement_start(&self) -> Option<(Vec<Candidate>, bool)> {
        let place = self.place()?;
        let (names, incomplete) = self.names(Wanted::StatementStart)?;
        let mut items: Vec<Candidate> = keywords::statement_keywords(place)
            .iter()
            .map(|keyword| Candidate::keyword(keyword, self.case))
            .chain(names)
            .collect();
        if self.request.snippets {
            items.extend(
                snippets::SNIPPETS
                    .iter()
                    .filter(|snippet| snippet.place.allows(place))
                    .map(|snippet| Candidate {
                        label: snippet.prefix.to_string(),
                        kind: ItemKind::Snippet,
                        detail: Some(snippet.description.to_string()),
                        insert: self.case.render_snippet(snippet.body),
                        snippet: true,
                        data: ItemData::None,
                        fits: false,
                        nearness: nearness::KEYWORD,
                    }),
            );
        }
        Some((items, incomplete))
    }

    /// The words after `DEFINE` and `modifiers`.
    fn define_keywords(&self, modifiers: &[Kind]) -> Option<(Vec<Candidate>, bool)> {
        let place = self.place()?;
        let items = keywords::define_keywords(place, modifiers)?
            .iter()
            .map(|keyword| Candidate::keyword(keyword, self.case))
            .collect();
        Some((items, self.base_incomplete()))
    }

    /// The scope at the cursor in the base.
    fn scope(&self) -> Option<ScopeId> {
        let (base, mapped) = self.based()?;
        Some(base::scope_at(base, *mapped.places.first()?))
    }

    /// Where the statement at the cursor starts, from the scope that holds
    /// it. `None` after the type of a class file, at its file scope.
    fn place(&self) -> Option<Place> {
        let place = self.scope_place()?;
        // The branch of `IF ... THEN` or `CASE ... WHEN ... THEN` is not a
        // statement of the file scope.
        Some(
            if self.found.after_branch && place == Place::ProcedureFile {
                Place::Routine
            } else {
                place
            },
        )
    }

    fn scope_place(&self) -> Option<Place> {
        let (base, _) = self.based()?;
        let tree = &base.navigation.model.semantic.scope_tree;
        let scope = self.scope()?;
        let kind = tree.get(scope).kind;
        // A block is a statement of the routine that holds it.
        let routine = tree
            .ancestors(scope)
            .map(|id| tree.get(id).kind)
            .find(|kind| !kind.is_var_transparent() && *kind != ScopeKind::Trigger);
        Some(match kind {
            ScopeKind::File => {
                if !is_class_file(self.request.path) {
                    Place::ProcedureFile
                } else {
                    match self.class_header_kind() {
                        None => Place::ClassHeader,
                        Some(Kind::KwEnum) => Place::EnumBody,
                        Some(_) => return None,
                    }
                }
            }
            ScopeKind::Class => Place::ClassBody,
            ScopeKind::Interface => Place::InterfaceBody,
            _ => match routine {
                Some(ScopeKind::Function) => Place::Function,
                Some(
                    ScopeKind::Method
                    | ScopeKind::Constructor
                    | ScopeKind::Destructor
                    | ScopeKind::PropertyGet
                    | ScopeKind::PropertySet
                    | ScopeKind::Class,
                ) => Place::Member,
                _ => Place::Routine,
            },
        })
    }

    /// The keyword of the `CLASS`, `INTERFACE` or `ENUM` statement before the
    /// window, in the current text.
    fn class_header_kind(&self) -> Option<Kind> {
        let tokens = &self.tokens[..self.found.before];
        let mut previous = None;
        for token in tokens {
            if token.kind == Kind::Comment {
                continue;
            }
            if matches!(token.kind, Kind::Class | Kind::Interface | Kind::KwEnum)
                && previous.is_none_or(|kind| kind == Kind::Period)
            {
                return Some(token.kind);
            }
            previous = Some(token.kind);
        }
        None
    }

    /// The items that match the word, ranked and cut at [`LIMIT`].
    fn finish(
        &self,
        candidates: Vec<Candidate>,
        incomplete: bool,
        word: &str,
        cursor: u32,
    ) -> CompletionList {
        let mut ranked: Vec<(String, Candidate)> = candidates
            .into_iter()
            .filter_map(|candidate| {
                let quality = rank::quality(word, &candidate.label)?;
                let quality = if word.is_empty() {
                    Quality::Other
                } else {
                    quality
                };
                let sort = rank::sort_text(
                    candidate.fits,
                    quality,
                    candidate.nearness,
                    &candidate.label,
                );
                Some((sort, candidate))
            })
            .collect();
        ranked.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.label.cmp(&b.1.label)));
        let cut = ranked.len() > LIMIT;
        ranked.truncate(LIMIT);
        CompletionList {
            incomplete: incomplete || cut,
            items: ranked
                .into_iter()
                .map(|(sort_text, candidate)| CompletionItem {
                    label: candidate.label,
                    kind: candidate.kind,
                    detail: candidate.detail,
                    sort_text,
                    insert: candidate.insert,
                    snippet: candidate.snippet,
                    data: candidate.data,
                })
                .collect(),
            word_start: self.found.word_start,
            word_end: self.found.word_end,
            cursor,
        }
    }
}

fn is_include(path: &Path) -> bool {
    oxabl_workspace::is_include_fragment(path)
}

fn is_class_file(path: &Path) -> bool {
    path.extension()
        .is_some_and(|ext| ext.eq_ignore_ascii_case("cls"))
}
