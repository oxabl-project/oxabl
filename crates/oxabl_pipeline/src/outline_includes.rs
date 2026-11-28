//! The document outline with the declarations of include files.
//!
//! [`outline_with_includes`] starts from the raw outline of the buffer
//! ([`outline`]) and puts the raw outline of each include the preprocessor
//! expanded under the `Include` symbol that references it. An include that
//! did not arrive keeps its symbol, with a marker in the detail. See
//! `docs/design/document-outline.md`.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use oxabl_analyze::{ExpandedFile, IncludeExpansion};
use oxabl_ast::Span;
use oxabl_common::{FileId, FileSpan, InternalPanic};

use crate::outline::{
    MAX_DEPTH, OutlineKind, OutlineSymbol, outline, outline_of, outline_recovered,
};
use crate::{Expansion, RawUnit};

/// The most symbols that the includes of one buffer add. An include that
/// would pass it is a marked symbol with no children.
pub const MAX_INCLUDED_SYMBOLS: usize = 5_000;

/// Markers, in the detail of an `Include` symbol that shows no declarations.
pub mod marker {
    /// No file the reference names was found.
    pub const NOT_FOUND: &str = "not found";
    /// The file was found and could not be read.
    pub const UNREADABLE: &str = "cannot be read";
    /// The file is encrypted source.
    pub const ENCRYPTED: &str = "encrypted";
    /// The reference is in an `&IF` branch that the preprocessor decided is not
    /// compiled.
    pub const NOT_COMPILED: &str = "not compiled";
    /// The reference is in an `&IF` branch that the preprocessor skipped
    /// without the facts to decide it, so the compiler may expand it.
    pub const MAY_NOT_BE_COMPILED: &str = "may not be compiled";
    /// The preprocessor did not expand the reference (a cycle, the depth limit,
    /// a failed expansion), or the expansion has no text to read.
    pub const NOT_EXPANDED: &str = "not expanded";
    /// The declarations of this include alone would pass
    /// [`super::MAX_INCLUDED_SYMBOLS`].
    pub const TOO_LARGE: &str = "too large";
    /// The includes before this one used the symbols that the outline allows.
    pub const LIMIT_REACHED: &str = "outline limit reached";
    /// Shown after the path of an include that the parser could not read to the
    /// end, so the symbols can be fewer than the file declares.
    pub const PARTIAL: &str = "partial";
    /// The preprocessor expanded the include, and the outline of its text
    /// failed (a contained internal panic).
    pub const CANNOT_OUTLINE: &str = "cannot be outlined";
    /// Shown after the path of an include that sits inside a statement and
    /// shows no symbol: it can add fields, terms or other parts that the
    /// outline does not list.
    pub const FRAGMENT: &str = "inside a statement";
}

/// A file whose text some symbols of an [`IncludeOutline`] are in.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncludeFile {
    pub path: PathBuf,
    /// The text the preprocessor read, which the spans of its symbols are in.
    pub text: Arc<str>,
}

/// One symbol of an outline that holds include declarations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OutlineNode {
    pub name: String,
    pub detail: Option<String>,
    pub kind: OutlineKind,
    /// A byte range of the file `file` names.
    pub span: Span,
    pub selection_span: Span,
    /// The file the spans are in, an index into [`IncludeOutline::files`].
    /// `None`: the buffer.
    pub file: Option<usize>,
    /// For an `Include` symbol that shows the declarations of a file: the file,
    /// an index into [`IncludeOutline::files`].
    pub opens: Option<usize>,
    pub children: Vec<OutlineNode>,
}

impl OutlineNode {
    /// `symbol` and what it holds, with their spans in the file `file`.
    pub fn from_symbol(symbol: OutlineSymbol, file: Option<usize>) -> Self {
        OutlineNode {
            name: symbol.name,
            detail: symbol.detail,
            kind: symbol.kind,
            span: symbol.span,
            selection_span: symbol.selection_span,
            file,
            opens: None,
            children: symbol
                .children
                .into_iter()
                .map(|child| OutlineNode::from_symbol(child, file))
                .collect(),
        }
    }
}

/// The outline of a buffer, with the symbols that its includes hold.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct IncludeOutline {
    pub symbols: Vec<OutlineNode>,
    pub files: Vec<IncludeFile>,
}

impl IncludeOutline {
    /// The outline of `source` alone, as [`outline`] makes it.
    pub fn raw(source: &str) -> Result<Self, InternalPanic> {
        Ok(IncludeOutline {
            symbols: outline(source)?
                .into_iter()
                .map(|symbol| OutlineNode::from_symbol(symbol, None))
                .collect(),
            files: Vec::new(),
        })
    }
}

/// The outline of `source` with the declarations of the includes that
/// `expansion` expanded. Every text it outlines is the text that the
/// preprocessor read, so a file that changes meanwhile cannot mismatch.
///
/// - A `{file.i}` statement that the expansion did expand holds the symbols of
///   the file, as a raw outline of that file, and its `opens` names the file.
///   A nested include holds its own symbols the same way. The root's own
///   symbols are as [`outline`] gives them.
/// - A reference that did not arrive keeps its symbol with a `marker` as its
///   detail and no children. A reference with no symbol in the raw outline (in
///   the middle of a statement, or written through a `{&name}` value) gets one.
/// - A symbol of an include in text that the preprocessor decided an `&IF` did
///   not compile is left out. Text of a branch it only guessed stays.
/// - An include whose raw outline needed error recovery shows `partial`. A
///   reference with no symbol of its own whose include shows no symbol shows
///   `inside a statement`: it stays, never silently missing.
///
/// `Err` means that the outline of `source` is unknown, as for [`outline`].
pub fn outline_with_includes(
    source: &str,
    expansion: &Expansion,
) -> Result<IncludeOutline, InternalPanic> {
    with_includes(source, outline(source)?, expansion)
}

/// [`outline_with_includes`] for a caller that holds the [`RawUnit`] of the
/// root's text: the root is not lexed or parsed again. The include files are
/// outlined as before.
pub fn outline_with_includes_of(
    unit: &RawUnit,
    expansion: &Expansion,
) -> Result<IncludeOutline, InternalPanic> {
    with_includes(unit.text(), outline_of(unit)?, expansion)
}

fn with_includes(
    source: &str,
    root: Vec<OutlineSymbol>,
    expansion: &Expansion,
) -> Result<IncludeOutline, InternalPanic> {
    let mut symbols: Vec<OutlineNode> = root
        .into_iter()
        .map(|symbol| OutlineNode::from_symbol(symbol, None))
        .collect();
    let Some(expanded) = expansion.expanded() else {
        mark_includes(&mut symbols, marker::NOT_EXPANDED);
        return Ok(IncludeOutline {
            symbols,
            files: Vec::new(),
        });
    };
    let exps = expanded.include_expansions();
    let mut by_parent: HashMap<Option<usize>, Vec<usize>> = HashMap::new();
    for (index, exp) in exps.iter().enumerate() {
        by_parent.entry(exp.parent).or_default().push(index);
    }
    let mut builder = Builder {
        expanded,
        exps,
        by_parent,
        source,
        files: Vec::new(),
        exp_file: HashMap::new(),
        raw: HashMap::new(),
        not_taken: HashMap::new(),
        budget: MAX_INCLUDED_SYMBOLS,
    };
    builder.resolve(&mut symbols, Owner::Root);
    Ok(IncludeOutline {
        symbols: limit_depth(symbols),
        files: builder.files,
    })
}

#[derive(Clone, Copy)]
enum Owner {
    Root,
    Expansion(usize),
}

/// A raw outline of one text of an include file, with whether the parser had
/// to recover. `None`: it could not be made.
type RawOutline = Option<(Vec<OutlineNode>, bool)>;

struct Builder<'a> {
    expanded: &'a ExpandedFile,
    exps: Vec<IncludeExpansion<'a>>,
    /// The expansions each expansion (`None`: the root) wrote itself.
    by_parent: HashMap<Option<usize>, Vec<usize>>,
    source: &'a str,
    files: Vec<IncludeFile>,
    /// The index in `files` of each expansion that was read.
    exp_file: HashMap<usize, usize>,
    /// The raw outline of each entry of `files`, made once for all the
    /// expansions that read that text.
    raw: HashMap<usize, RawOutline>,
    /// Sorted, non-overlapping text that the preprocessor did not take, by the
    /// preprocessor's id of the expansion.
    not_taken: HashMap<FileId, Vec<Span>>,
    budget: usize,
}

impl Builder<'_> {
    /// The index in `files` of the text the symbols of `owner` are in.
    fn owner_file_index(&self, owner: Owner) -> Option<usize> {
        match owner {
            Owner::Root => None,
            Owner::Expansion(index) => self.exp_file.get(&index).copied(),
        }
    }

    /// The id of expansion `index`, also when it kept no text of its own.
    fn id_of(&self, index: usize) -> Option<FileId> {
        self.exps[index]
            .file
            .or_else(|| self.expanded.expansion_ids().get(index).copied().flatten())
    }

    /// The text that the preprocessor read for expansion `index`.
    fn text_of(&self, index: usize) -> Option<Arc<str>> {
        let id = self.id_of(index)?;
        self.expanded.source_text(id).map(Arc::clone)
    }

    fn owner_text(&self, owner: Owner) -> Option<Arc<str>> {
        match owner {
            Owner::Root => Some(Arc::from(self.source)),
            Owner::Expansion(index) => self.text_of(index),
        }
    }

    /// Fill each `Include` symbol of `nodes`, the symbols of the owner, and
    /// give a symbol to each expansion and each report of the owner that has
    /// none.
    fn resolve(&mut self, nodes: &mut Vec<OutlineNode>, owner: Owner) {
        let key = match owner {
            Owner::Root => None,
            Owner::Expansion(index) => Some(index),
        };
        let candidates = self.by_parent.get(&key).cloned().unwrap_or_default();
        let mut used = vec![false; candidates.len()];
        self.fill(nodes, owner, &candidates, &mut used);
        let text = self.owner_text(owner);
        let file = self.owner_file_index(owner);
        for (position, &index) in candidates.iter().enumerate() {
            if used[position] {
                continue;
            }
            let site = self.exps[index].site.or_else(|| match owner {
                // An include that a `{&name}` value writes: the reference
                // that the root wrote is its site.
                Owner::Root => self
                    .expanded
                    .resolve_include_site(self.exps[index].expanded),
                Owner::Expansion(_) => None,
            });
            let mut node = match (site, &text) {
                (Some(site), Some(text)) => include_symbol(text, site),
                _ => None,
            }
            .unwrap_or_else(|| substituted_symbol(self.exps[index].path));
            match self.expansion_nodes(index) {
                Ok((opened, children)) => {
                    node.opens = Some(opened);
                    if self.partial(opened) {
                        node.detail = Some(marker::PARTIAL.to_string());
                    } else if children.is_empty() {
                        // The text sits inside a statement (the fields of a
                        // temp-table, a `WHERE` term): the raw outline of the
                        // file alone cannot tell what it adds there.
                        node.detail = Some(marker::FRAGMENT.to_string());
                    }
                    node.children = children;
                }
                Err(reason) => node.detail = Some(reason.to_string()),
            }
            node.file = file;
            self.place(nodes, node);
        }
        self.mark_unclaimed(nodes, owner, text.as_deref(), file);
    }

    /// Put `node` where its span says. A node with no site of its own goes last.
    fn place(&self, nodes: &mut Vec<OutlineNode>, node: OutlineNode) {
        if node.span.end == 0 {
            nodes.push(node);
        } else {
            insert_by_span(nodes, node);
        }
    }

    fn partial(&self, file: usize) -> bool {
        matches!(self.raw.get(&file), Some(Some((_, true))))
    }

    fn fill(
        &mut self,
        nodes: &mut [OutlineNode],
        owner: Owner,
        candidates: &[usize],
        used: &mut [bool],
    ) {
        for node in nodes {
            if node.kind != OutlineKind::Include {
                self.fill(&mut node.children, owner, candidates, used);
                continue;
            }
            let found = candidates.iter().enumerate().find(|&(position, &index)| {
                !used[position]
                    && self.exps[index].site.is_some_and(|site| {
                        node.span.start <= site.start && site.end <= node.span.end
                    })
            });
            match found {
                Some((position, &index)) => {
                    used[position] = true;
                    match self.expansion_nodes(index) {
                        Ok((opened, children)) => {
                            node.opens = Some(opened);
                            if self.partial(opened) {
                                node.detail = Some(marker::PARTIAL.to_string());
                            }
                            node.children = children;
                        }
                        Err(reason) => node.detail = Some(reason.to_string()),
                    }
                }
                None => node.detail = Some(self.why_missing(owner, node.span).to_string()),
            }
        }
    }

    /// Give a symbol with a marker to each reference that the preprocessor
    /// reported for `owner` and that no `Include` symbol of the owner covers:
    /// a reference in the middle of a statement, or written through a
    /// `{&name}` value.
    fn mark_unclaimed(
        &self,
        nodes: &mut Vec<OutlineNode>,
        owner: Owner,
        text: Option<&str>,
        file: Option<usize>,
    ) {
        let expanded = self.expanded;
        let reports = expanded
            .unresolved_includes()
            .iter()
            .chain(expanded.nested_unresolved_includes())
            .map(|found| (&found.site, found.via.as_slice()))
            .chain(
                expanded
                    .unreadable_includes()
                    .iter()
                    .map(|found| (&found.site, found.via.as_slice())),
            )
            .chain(
                expanded
                    .skipped_includes()
                    .iter()
                    .map(|found| (&found.site, found.via.as_slice())),
            );
        let mut covered: Vec<Span> = Vec::new();
        collect_include_spans(nodes, file, &mut covered);
        for (site, via) in reports {
            if !self.belongs(owner, site, via) || covered.iter().any(|c| overlaps(*c, site.span)) {
                continue;
            }
            covered.push(site.span);
            let mut node = text
                .and_then(|text| include_symbol(text, site.span))
                .unwrap_or_else(|| substituted_symbol(Path::new("{...}")));
            node.detail = Some(self.why_missing(owner, site.span).to_string());
            node.file = file;
            self.place(nodes, node);
        }
    }

    /// The symbols of expansion `index` and the index of its file in `files`,
    /// or the marker that says why it shows none.
    fn expansion_nodes(&mut self, index: usize) -> Result<(usize, Vec<OutlineNode>), &'static str> {
        let text = self.text_of(index).ok_or(marker::NOT_EXPANDED)?;
        if self.budget == 0 {
            return Err(marker::LIMIT_REACHED);
        }
        let path = self.exps[index].path.to_path_buf();
        let file = self.file_index(&path, &text);
        self.exp_file.insert(index, file);
        let mut nodes = self.raw_nodes(file)?;
        if let Some(id) = self.id_of(index) {
            let expanded = self.expanded;
            let spans = self
                .not_taken
                .entry(id)
                .or_insert_with(|| expanded.not_taken_regions(id));
            if !spans.is_empty() {
                nodes = without_skipped(nodes, spans);
            }
        }
        let cost = count(&nodes);
        if cost > MAX_INCLUDED_SYMBOLS {
            return Err(marker::TOO_LARGE);
        }
        if cost > self.budget {
            return Err(marker::LIMIT_REACHED);
        }
        self.budget -= cost;
        self.resolve(&mut nodes, Owner::Expansion(index));
        Ok((file, nodes))
    }

    /// The index in `files` of this text of `path`.
    fn file_index(&mut self, path: &Path, text: &Arc<str>) -> usize {
        if let Some(index) = self
            .files
            .iter()
            .position(|file| file.path == path && file.text == *text)
        {
            return index;
        }
        self.files.push(IncludeFile {
            path: path.to_path_buf(),
            text: Arc::clone(text),
        });
        self.files.len() - 1
    }

    fn raw_nodes(&mut self, file: usize) -> Result<Vec<OutlineNode>, &'static str> {
        let text = Arc::clone(&self.files[file].text);
        let cached = self.raw.entry(file).or_insert_with(|| {
            outline_recovered(&text).ok().map(|(symbols, recovered)| {
                let nodes = symbols
                    .into_iter()
                    .map(|symbol| OutlineNode::from_symbol(symbol, Some(file)))
                    .collect();
                (nodes, recovered)
            })
        });
        match cached {
            Some((nodes, _)) => Ok(nodes.clone()),
            None => Err(marker::CANNOT_OUTLINE),
        }
    }

    /// The `{...}` sites that lead from the root to expansion `index`,
    /// outermost first. `None` when one of them has no site of its own.
    fn chain(&self, index: usize) -> Option<Vec<Span>> {
        let mut sites = Vec::new();
        let mut at = Some(index);
        while let Some(current) = at {
            sites.push(self.exps[current].site?);
            at = self.exps[current].parent;
        }
        sites.reverse();
        Some(sites)
    }

    /// Whether a report of the preprocessor, at `site` in the text of the file
    /// that wrote it and below the include sites `via`, is about the text of
    /// `owner`. An expansion that has no file id of its own is found by the
    /// include sites above it.
    fn belongs(&self, owner: Owner, site: &FileSpan, via: &[FileSpan]) -> bool {
        match owner {
            Owner::Root => via.is_empty() && site.file == self.expanded.root_file(),
            Owner::Expansion(index) => match self.chain(index) {
                Some(chain) => {
                    via.len() == chain.len()
                        && via
                            .iter()
                            .zip(&chain)
                            .all(|(site, span)| site.span == *span)
                }
                None => self.id_of(index) == Some(site.file),
            },
        }
    }

    /// Why the `{...}` reference at `span` in the text of `owner` has no
    /// expansion. Anything the preprocessor did not report is "not expanded".
    fn why_missing(&self, owner: Owner, span: Span) -> &'static str {
        let expanded = self.expanded;
        if expanded
            .unresolved_includes()
            .iter()
            .chain(expanded.nested_unresolved_includes())
            .any(|found| {
                overlaps(found.site.span, span) && self.belongs(owner, &found.site, &found.via)
            })
        {
            return marker::NOT_FOUND;
        }
        if let Some(unreadable) = expanded.unreadable_includes().iter().find(|found| {
            overlaps(found.site.span, span) && self.belongs(owner, &found.site, &found.via)
        }) {
            return if unreadable.encrypted {
                marker::ENCRYPTED
            } else {
                marker::UNREADABLE
            };
        }
        if let Some(found) = expanded.skipped_includes().iter().find(|found| {
            overlaps(found.site.span, span) && self.belongs(owner, &found.site, &found.via)
        }) {
            let spans = expanded.not_taken_regions(found.site.file);
            return if inside(&spans, found.site.span.start) {
                marker::NOT_COMPILED
            } else {
                marker::MAY_NOT_BE_COMPILED
            };
        }
        marker::NOT_EXPANDED
    }
}

/// The spans of the `Include` symbols in `nodes` that are in text `file`.
fn collect_include_spans(nodes: &[OutlineNode], file: Option<usize>, out: &mut Vec<Span>) {
    for node in nodes {
        if node.kind == OutlineKind::Include && node.file == file {
            out.push(node.span);
        }
        collect_include_spans(&node.children, file, out);
    }
}

fn inside(spans: &[Span], offset: u32) -> bool {
    let after = spans.partition_point(|span| span.start <= offset);
    after > 0 && offset < spans[after - 1].end
}

/// `nodes` without the symbols in `spans`. An `Include` symbol stays, because
/// its marker tells the reader that the reference is not compiled.
fn without_skipped(nodes: Vec<OutlineNode>, spans: &[Span]) -> Vec<OutlineNode> {
    nodes
        .into_iter()
        .filter_map(|mut node| {
            if node.kind != OutlineKind::Include && inside(spans, node.selection_span.start) {
                return None;
            }
            node.children = without_skipped(std::mem::take(&mut node.children), spans);
            Some(node)
        })
        .collect()
}

fn overlaps(a: Span, b: Span) -> bool {
    a.start < b.end && b.start < a.end
}

fn count(nodes: &[OutlineNode]) -> usize {
    nodes.iter().map(|node| 1 + count(&node.children)).sum()
}

fn mark_includes(nodes: &mut [OutlineNode], detail: &str) {
    for node in nodes {
        if node.kind == OutlineKind::Include {
            node.detail = Some(detail.to_string());
        }
        mark_includes(&mut node.children, detail);
    }
}

/// An `Include` symbol for the reference at `site` in `text`, named as the
/// raw outline names one: the file as written. A reference through a `{&name}`
/// value is named as written, with its braces.
fn include_symbol(text: &str, site: Span) -> Option<OutlineNode> {
    let written = text.get(site.start as usize..site.end as usize)?;
    let inner = written.strip_prefix('{')?.strip_suffix('}')?;
    let (name, selection) = if inner.starts_with('&') {
        (written.to_string(), site)
    } else {
        let file = inner.split_whitespace().next()?;
        let selection = written.find(file).map_or(site, |at| {
            let start = site.start + at as u32;
            Span {
                start,
                end: start + file.len() as u32,
            }
        });
        (file.to_string(), selection)
    };
    Some(OutlineNode {
        name,
        detail: None,
        kind: OutlineKind::Include,
        span: site,
        selection_span: selection,
        file: None,
        opens: None,
        children: Vec::new(),
    })
}

/// An `Include` symbol for an include that has no reference in the text of its
/// owner: it is named by its file, and its span is empty.
fn substituted_symbol(path: &Path) -> OutlineNode {
    let name = path.file_name().map_or_else(
        || path.display().to_string(),
        |name| name.to_string_lossy().into_owned(),
    );
    OutlineNode {
        name,
        detail: None,
        kind: OutlineKind::Include,
        span: Span { start: 0, end: 0 },
        selection_span: Span { start: 0, end: 0 },
        file: None,
        opens: None,
        children: Vec::new(),
    }
}

/// Put `node` in the innermost symbol of its file whose span holds it, before
/// the first later symbol of that file.
fn insert_by_span(nodes: &mut Vec<OutlineNode>, node: OutlineNode) {
    let container = nodes.iter().position(|other| {
        other.file == node.file
            && other.kind != OutlineKind::Include
            && other.span.start <= node.span.start
            && node.span.end <= other.span.end
    });
    match container {
        Some(index) => insert_by_span(&mut nodes[index].children, node),
        None => {
            let at = nodes
                .iter()
                .position(|other| other.file == node.file && other.span.start > node.span.start)
                .unwrap_or(nodes.len());
            nodes.insert(at, node);
        }
    }
}

/// Keep at most [`MAX_DEPTH`] symbols above any symbol, as [`outline`] does: a
/// symbol that holds symbols and has that many above it gives no symbol of its
/// own, and what it holds goes to the symbol above.
fn limit_depth(nodes: Vec<OutlineNode>) -> Vec<OutlineNode> {
    fn at_depth(nodes: Vec<OutlineNode>, depth: usize) -> Vec<OutlineNode> {
        let mut out = Vec::with_capacity(nodes.len());
        for mut node in nodes {
            if depth < MAX_DEPTH {
                node.children = at_depth(std::mem::take(&mut node.children), depth + 1);
                out.push(node);
                continue;
            }
            // Pre-order, without recursion: the input can be far deeper.
            let mut pending = vec![node];
            while let Some(mut next) = pending.pop() {
                if next.children.is_empty() {
                    out.push(next);
                } else {
                    let children = std::mem::take(&mut next.children);
                    pending.extend(children.into_iter().rev());
                }
            }
        }
        out
    }
    at_depth(nodes, 0)
}
