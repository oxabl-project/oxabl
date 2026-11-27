//! A parameter list that holds an include, read as the declaration writes it.
//!
//! Many code bases start each parameter list with an include that declares a
//! block of shared parameters. The analysis expands the include, so it knows
//! the parameters. Signature help and hover show the include as one entry,
//! `{inc/std-params.i} (2 parameters)`, never every parameter it declares.
//!
//! A file that is read as text holds the include as a reference, which the
//! parser skips in a parameter list. [`Loaded::view`] expands such a file, and
//! [`Folding::entries`] groups the parameters of the expansion by the include
//! that brought them in. See `docs/design/signature-help.md`.

use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use oxabl_analyze::ExpandedFile;
use oxabl_ast::{ParameterDirection, Span, Statement, StatementKind};
use oxabl_common::VirtualSpan;
use oxabl_lexer::{Kind, Token};

use super::hover::Source;
use super::{Navigation, Parsed};
use crate::{Expansion, LintPipeline};

/// One entry of a parameter list: a parameter that the declaration writes, or
/// the parameters that one include declares.
pub(super) enum Entry<'a> {
    Param(&'a Statement),
    Include(Group<'a>),
}

/// The parameters that one include reference declares.
pub(super) struct Group<'a> {
    /// The `{...}` reference, in the bytes of the file that writes it.
    pub site: Span,
    /// The reference as written, on one line.
    pub text: String,
    /// Every parameter the include declares, in order.
    pub params: Vec<&'a Statement>,
}

impl Group<'_> {
    /// The parameters a call passes arguments for: not `RETURN`.
    pub fn count(&self) -> usize {
        self.params
            .iter()
            .filter(|statement| {
                !matches!(
                    statement.kind,
                    StatementKind::DefineParameter {
                        direction: ParameterDirection::Return,
                        ..
                    }
                )
            })
            .count()
    }

    /// `{inc/std-params.i} (2 parameters)`.
    pub fn label(&self) -> String {
        let count = self.count();
        let noun = if count == 1 {
            "parameter"
        } else {
            "parameters"
        };
        format!("{} ({count} {noun})", self.text)
    }
}

/// An include expansion that the file writes itself.
struct Region {
    /// The text it became in the expanded text.
    expanded: std::ops::Range<u32>,
    /// Its `{...}` reference; `None` in the text of a `{&name}` value.
    site: Option<Span>,
    /// Its place in the list of expansions.
    index: usize,
}

/// What a [`Folding`] keeps from one use to the next. It holds no borrow, so
/// a [`Navigation`] can own one.
#[derive(Default)]
pub(super) struct FoldingCache {
    built: OnceCell<Built>,
    /// The text of the file that was expanded, as written.
    root_text: OnceCell<Option<Arc<str>>>,
    /// Whether the include at a site (by start) is read whole, per site.
    whole: RefCell<HashMap<u32, bool>>,
}

struct Built {
    regions: Vec<Region>,
    /// The enclosing expansion and the text of each expansion.
    tree: Vec<(Option<usize>, Option<oxabl_common::FileId>)>,
}

/// The way from the expanded parameters of a declaration to the entries that
/// the declaration writes.
pub(super) struct Folding<'t> {
    expanded: &'t ExpandedFile,
    cache: &'t FoldingCache,
    /// Reads the text of the file that was expanded, as written.
    root: Box<dyn Fn() -> Option<Arc<str>> + 't>,
}

impl<'t> Folding<'t> {
    pub fn new(
        expanded: &'t ExpandedFile,
        cache: &'t FoldingCache,
        root: impl Fn() -> Option<Arc<str>> + 't,
    ) -> Self {
        Folding {
            expanded,
            cache,
            root: Box::new(root),
        }
    }

    fn built(&self) -> &Built {
        self.cache.built.get_or_init(|| {
            let expansions = self.expanded.include_expansions();
            let regions = expansions
                .iter()
                .enumerate()
                .filter(|(_, expansion)| expansion.parent.is_none())
                .map(|(index, expansion)| Region {
                    expanded: expansion.expanded.start..expansion.expanded.end,
                    site: expansion.site,
                    index,
                })
                .collect();
            let tree = expansions.iter().map(|e| (e.parent, e.file)).collect();
            Built { regions, tree }
        })
    }

    fn root_text(&self) -> Option<&Arc<str>> {
        self.cache.root_text.get_or_init(|| (self.root)()).as_ref()
    }

    /// `parameters`, the parameter statements of one declaration in the
    /// expanded text, as the declaration writes them. `None` when the
    /// declaration is not known whole: an include that is not one plain
    /// `{file}` reference, or that holds a `{&name}` reference or an `&IF`.
    pub fn entries<'a>(&self, parameters: Vec<&'a Statement>) -> Option<Vec<Entry<'a>>> {
        let mut out: Vec<Entry<'a>> = Vec::new();
        for statement in parameters {
            let at = statement.span.start;
            // The text of a `{&name}` value has no reference of its own: its
            // parameters are listed one by one.
            let Some((region, site)) = self
                .built()
                .regions
                .iter()
                .find(|r| r.expanded.contains(&at))
                .and_then(|region| Some((region, region.site?)))
            else {
                out.push(Entry::Param(statement));
                continue;
            };
            if let Some(Entry::Include(group)) = out.last_mut()
                && group.site == site
            {
                group.params.push(statement);
                continue;
            }
            let text = self.reference(site)?;
            if !self.read_whole(region) {
                return None;
            }
            out.push(Entry::Include(Group {
                site,
                text,
                params: vec![statement],
            }));
        }
        Some(out)
    }

    /// The include reference at `site`, on one line. `None` when the bytes
    /// there are not one `{file}` reference.
    fn reference(&self, site: Span) -> Option<String> {
        let text = self
            .root_text()?
            .get(site.start as usize..site.end as usize)?;
        let tokens = oxabl_lexer::tokenize(text);
        let mut words = tokens.iter().filter(|t| t.kind != Kind::Eof);
        let first = words.next()?;
        if first.kind != Kind::IncludeReference
            || words.next().is_some()
            || (first.start, first.end) != (0, text.len())
        {
            return None;
        }
        Some(text.split_whitespace().collect::<Vec<_>>().join(" "))
    }

    /// Whether the text of the include of `region`, and of the includes below
    /// it, is the same for every compile: no `{&name}` reference and no `&IF`.
    fn read_whole(&self, region: &Region) -> bool {
        let key = region.site.map_or(0, |site| site.start);
        if let Some(known) = self.cache.whole.borrow().get(&key) {
            return *known;
        }
        let tree = &self.built().tree;
        let whole = (0..tree.len())
            .filter(|&at| below(tree, at, region.index))
            .all(|at| {
                let Some(file) = tree[at].1 else {
                    return true;
                };
                let Some(text) = self.expanded.source_text(file) else {
                    return false;
                };
                let tokens: Vec<Token> = oxabl_lexer::tokenize(text);
                !Source::new(text, &tokens).may_expand(0, u32::MAX, &[], false)
            });
        self.cache.whole.borrow_mut().insert(key, whole);
        whole
    }
}

/// Whether expansion `at` is `ancestor` or is written in it, at any depth.
fn below(
    tree: &[(Option<usize>, Option<oxabl_common::FileId>)],
    mut at: usize,
    ancestor: usize,
) -> bool {
    // The tree is finite: a parent is an earlier expansion.
    loop {
        if at == ancestor {
            return true;
        }
        match tree[at].0 {
            Some(parent) if parent < at => at = parent,
            _ => return false,
        }
    }
}

/// A file read for its declarations.
pub(super) struct Loaded {
    pub text: Arc<str>,
    pub parsed: Parsed,
    unfolded: OnceCell<Option<Unfolded>>,
}

/// A file with its includes expanded, and the expanded text parsed.
struct Unfolded {
    expansion: Expansion,
    parsed: Parsed,
    cache: FoldingCache,
}

impl Unfolded {
    fn expanded(&self) -> &ExpandedFile {
        self.expansion
            .expanded()
            .expect("an unfolded file has an expansion")
    }
}

impl Loaded {
    pub fn new(text: Arc<str>, parsed: Parsed) -> Loaded {
        Loaded {
            text,
            parsed,
            unfolded: OnceCell::new(),
        }
    }

    /// The declarations of the file at `path`. With `expand` set, the file
    /// is read as its expansion shows it, and its parameter lists group the
    /// parameters of each include; `None` when the expansion is not whole: an
    /// include that did not arrive, or text that does not parse. Without it,
    /// the file is read as written.
    pub fn view(&self, nav: &Navigation, path: &Path, expand: bool) -> Option<View<'_>> {
        if !expand {
            return Some(View {
                text: &self.text,
                tokens: &self.parsed.tokens,
                statements: &self.parsed.program.statements,
                expanded: None,
                folding: None,
            });
        }
        let unfolded = self
            .unfolded
            .get_or_init(|| unfold(nav, path, &self.text))
            .as_ref()?;
        let expanded = unfolded.expanded();
        let root = Arc::clone(&self.text);
        Some(View {
            text: &expanded.text,
            tokens: &unfolded.parsed.tokens,
            statements: &unfolded.parsed.program.statements,
            expanded: Some(expanded),
            folding: Some(Folding::new(expanded, &unfolded.cache, move || {
                Some(Arc::clone(&root))
            })),
        })
    }
}

fn unfold(nav: &Navigation, path: &Path, text: &str) -> Option<Unfolded> {
    let pipeline = LintPipeline::new(nav.run.config(), nav.run.file_system());
    let expansion = pipeline.with_file(path).expand(text);
    let parsed = {
        let expanded = expansion.expanded()?;
        if expanded.lost_include_text() {
            return None;
        }
        Parsed::of(&expanded.text)
    };
    parsed.program.errors.is_empty().then_some(Unfolded {
        expansion,
        parsed,
        cache: FoldingCache::default(),
    })
}

/// A file's declarations, as written or as the expansion shows them.
pub(super) struct View<'l> {
    text: &'l str,
    tokens: &'l [Token],
    pub statements: &'l [Statement],
    expanded: Option<&'l ExpandedFile>,
    folding: Option<Folding<'l>>,
}

impl View<'_> {
    pub fn source(&self) -> Source<'_> {
        let source = Source::new(self.text, self.tokens);
        match &self.folding {
            Some(folding) => source.folded(folding),
            None => source,
        }
    }

    /// `span`, in the bytes of the file as written, in this view's text.
    pub fn locate(&self, span: Span) -> Option<VirtualSpan> {
        match self.expanded {
            None => Some(VirtualSpan::new(span.start, span.end)),
            Some(expanded) => {
                let start = expanded.expanded_offset(expanded.root_file(), span.start)?;
                Some(VirtualSpan::new(start, start + (span.end - span.start)))
            }
        }
    }

    /// Whether the file itself wrote the statement that starts at `at` of this
    /// view's text, and no include.
    pub fn written_here(&self, at: u32) -> bool {
        self.expanded.is_none_or(|expanded| {
            expanded
                .resolve_root_span(Span {
                    start: at,
                    end: at + 1,
                })
                .is_some()
        })
    }
}

impl Navigation<'_, '_> {
    /// The folding of the analysed unit. The text of the unit is read when an
    /// include of a parameter list needs it.
    pub(super) fn folding(&self) -> Folding<'_> {
        Folding::new(self.expanded, &self.folding_cache, || {
            let path = self.root_path()?;
            self.run.file_system().read(&path).ok()
        })
    }
}

/// The bytes of a class that hold no member: the bodies of its methods,
/// constructors, destructor and property accessors.
pub(super) fn bodies_of(body: &[Statement]) -> Vec<(u32, u32)> {
    body.iter()
        .flat_map(|member| -> Vec<(u32, u32)> {
            let from_first = |body: &[Statement]| {
                body.first()
                    .map(|first| (first.span.start, member.span.end))
            };
            match &member.kind {
                StatementKind::Method { body, .. }
                | StatementKind::Constructor { body, .. }
                | StatementKind::Destructor { body } => from_first(body).into_iter().collect(),
                StatementKind::Property {
                    get_body, set_body, ..
                } => [get_body, set_body]
                    .into_iter()
                    .flatten()
                    .filter_map(|body| Some((body.first()?.span.start, body.last()?.span.end)))
                    .collect(),
                _ => Vec::new(),
            }
        })
        .collect()
}

/// The include references of a class, outside the bodies of its members.
pub(super) struct ClassIncludes {
    /// Those in the parameter list of a method or constructor.
    pub in_parameters: Vec<Span>,
    /// Those anywhere else: they may declare members.
    pub elsewhere: Vec<Span>,
}

/// The include references that `class` writes, read from its text as written.
pub(super) fn class_includes(source: &Source, class: &Statement) -> ClassIncludes {
    let (StatementKind::Class { body, .. } | StatementKind::Interface { body, .. }) = &class.kind
    else {
        return ClassIncludes {
            in_parameters: Vec::new(),
            elsewhere: source.includes_in(class.span.start, class.span.end, &[]),
        };
    };
    let mut skip = bodies_of(body);
    let mut in_parameters = Vec::new();
    for member in body {
        if !matches!(
            member.kind,
            StatementKind::Method { .. } | StatementKind::Constructor { .. }
        ) {
            continue;
        }
        if let Some((open, close)) = source.parameter_list(member) {
            in_parameters.extend(source.includes_in(open, close, &[]));
            skip.push((open, close));
        }
    }
    ClassIncludes {
        in_parameters,
        elsewhere: source.includes_in(class.span.start, class.span.end, &skip),
    }
}
