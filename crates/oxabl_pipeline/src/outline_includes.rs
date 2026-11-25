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

use crate::Expansion;
use crate::outline::{MAX_DEPTH, OutlineKind, OutlineSymbol, outline};

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
    /// The reference is in an `&IF` branch that is not compiled.
    pub const NOT_COMPILED: &str = "not compiled";
    /// The preprocessor did not expand the reference (a cycle, the depth limit,
    /// a failed expansion), or the expansion has no text to read.
    pub const NOT_EXPANDED: &str = "not expanded";
    /// The declarations of the include would pass [`super::MAX_INCLUDED_SYMBOLS`].
    pub const TOO_LARGE: &str = "too large";
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
/// `expansion` expanded. `sources` holds the text of every include the
/// expansion read.
///
/// - A `{file.i}` statement that the expansion did expand holds the symbols of
///   the file, as a raw outline of that file, and its `opens` names the file.
///   A nested include holds its own symbols the same way. The root's own
///   symbols are as [`outline`] gives them.
/// - A reference that did not arrive keeps its symbol with a `marker` as its
///   detail and no children.
/// - A symbol of an include in text that an `&IF` did not compile is left out.
/// - An include that the expansion read at a place with no `Include` symbol
///   (a reference in the middle of a statement) gets a symbol of its own when
///   it holds a declaration.
///
/// `Err` means that the outline of `source` is unknown, as for [`outline`].
pub fn outline_with_includes(
    source: &str,
    expansion: &Expansion,
    sources: &HashMap<PathBuf, Arc<str>>,
) -> Result<IncludeOutline, InternalPanic> {
    let mut symbols: Vec<OutlineNode> = outline(source)?
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
        sources,
        source,
        files: Vec::new(),
        file_of: HashMap::new(),
        raw: HashMap::new(),
        skipped: skipped_by_file(expanded),
        skipped_ids: skipped_ids(expanded),
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

struct Builder<'a> {
    expanded: &'a ExpandedFile,
    exps: Vec<IncludeExpansion<'a>>,
    /// The expansions each expansion (`None`: the root) wrote itself.
    by_parent: HashMap<Option<usize>, Vec<usize>>,
    sources: &'a HashMap<PathBuf, Arc<str>>,
    source: &'a str,
    files: Vec<IncludeFile>,
    file_of: HashMap<PathBuf, usize>,
    /// The raw outline of each include file, made once for all its expansions.
    /// `None`: it could not be made.
    raw: HashMap<PathBuf, Option<Vec<OutlineNode>>>,
    /// Sorted, non-overlapping text that an `&IF` did not compile, by the
    /// preprocessor's id of the expansion.
    skipped: HashMap<FileId, Vec<Span>>,
    /// The id of an expansion that kept no text of its own, so has none in
    /// `exps`, by the include sites above it. Known only when it has skipped text.
    skipped_ids: HashMap<Vec<(u32, u32)>, FileId>,
    budget: usize,
}

impl Builder<'_> {
    /// The index in `files` of the text the symbols of `owner` are in.
    fn owner_file_index(&self, owner: Owner) -> Option<usize> {
        match owner {
            Owner::Root => None,
            Owner::Expansion(index) => self.file_of.get(self.exps[index].path).copied(),
        }
    }

    fn owner_text(&self, owner: Owner) -> Option<&str> {
        match owner {
            Owner::Root => Some(self.source),
            Owner::Expansion(index) => self
                .sources
                .get(self.exps[index].path)
                .map(|text| text.as_ref()),
        }
    }

    /// Fill each `Include` symbol of `nodes`, the symbols of the owner, and
    /// give a symbol to each expansion of the owner that has none.
    fn resolve(&mut self, nodes: &mut Vec<OutlineNode>, owner: Owner) {
        let key = match owner {
            Owner::Root => None,
            Owner::Expansion(index) => Some(index),
        };
        let candidates = self.by_parent.get(&key).cloned().unwrap_or_default();
        let mut used = vec![false; candidates.len()];
        self.fill(nodes, owner, &candidates, &mut used);
        for (position, &index) in candidates.iter().enumerate() {
            if used[position] {
                continue;
            }
            let Some(site) = self.exps[index].site else {
                continue;
            };
            let Some(text) = self.owner_text(owner) else {
                continue;
            };
            let Some(mut node) = include_symbol(text, site) else {
                continue;
            };
            match self.expansion_nodes(index) {
                Ok((file, children)) if !children.is_empty() => {
                    node.opens = Some(file);
                    node.children = children;
                }
                Ok(_) => continue,
                Err(reason) => node.detail = Some(reason.to_string()),
            }
            node.file = self.owner_file_index(owner);
            insert_by_span(nodes, node);
        }
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
                        Ok((file, children)) => {
                            node.opens = Some(file);
                            node.children = children;
                        }
                        Err(reason) => node.detail = Some(reason.to_string()),
                    }
                }
                None => node.detail = Some(self.why_missing(owner, node.span).to_string()),
            }
        }
    }

    /// The symbols of expansion `index` and the index of its file in `files`,
    /// or the marker that says why it shows none.
    fn expansion_nodes(&mut self, index: usize) -> Result<(usize, Vec<OutlineNode>), &'static str> {
        let path = self.exps[index].path.to_path_buf();
        let text = self
            .sources
            .get(&path)
            .cloned()
            .ok_or(marker::NOT_EXPANDED)?;
        let file = self.file_index(&path, &text);
        let mut nodes = self.raw_nodes(&path, &text, file)?;
        if let Some(spans) = self.skipped_in(index) {
            nodes = without_skipped(nodes, spans);
        }
        let cost = count(&nodes);
        if cost > self.budget {
            return Err(marker::TOO_LARGE);
        }
        self.budget -= cost;
        self.resolve(&mut nodes, Owner::Expansion(index));
        Ok((file, nodes))
    }

    fn file_index(&mut self, path: &Path, text: &Arc<str>) -> usize {
        if let Some(&index) = self.file_of.get(path) {
            return index;
        }
        self.files.push(IncludeFile {
            path: path.to_path_buf(),
            text: Arc::clone(text),
        });
        let index = self.files.len() - 1;
        self.file_of.insert(path.to_path_buf(), index);
        index
    }

    fn raw_nodes(
        &mut self,
        path: &Path,
        text: &str,
        file: usize,
    ) -> Result<Vec<OutlineNode>, &'static str> {
        let cached = self.raw.entry(path.to_path_buf()).or_insert_with(|| {
            outline(text).ok().map(|symbols| {
                symbols
                    .into_iter()
                    .map(|symbol| OutlineNode::from_symbol(symbol, Some(file)))
                    .collect()
            })
        });
        cached.clone().ok_or(marker::NOT_EXPANDED)
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

    /// The text of expansion `index` that an `&IF` did not compile.
    fn skipped_in(&self, index: usize) -> Option<&Vec<Span>> {
        let id = self.exps[index].file.or_else(|| {
            let chain = self.chain(index)?;
            let key: Vec<(u32, u32)> = chain.iter().map(|span| (span.start, span.end)).collect();
            self.skipped_ids.get(&key).copied()
        })?;
        self.skipped.get(&id)
    }

    /// Whether a report of the preprocessor, at `site` in the text of the file
    /// that wrote it and below the include sites `via`, is about the
    /// reference at `span` in the text of `owner`. An expansion that has no
    /// file id of its own is found by the include sites above it.
    fn is_here(&self, owner: Owner, span: Span, site: &FileSpan, via: &[FileSpan]) -> bool {
        if !overlaps(site.span, span) {
            return false;
        }
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
                None => self.exps[index].file == Some(site.file),
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
            .any(|found| self.is_here(owner, span, &found.site, &found.via))
        {
            return marker::NOT_FOUND;
        }
        if let Some(unreadable) = expanded
            .unreadable_includes()
            .iter()
            .find(|found| self.is_here(owner, span, &found.site, &found.via))
        {
            return if unreadable.encrypted {
                marker::ENCRYPTED
            } else {
                marker::UNREADABLE
            };
        }
        if expanded
            .skipped_includes()
            .iter()
            .any(|found| self.is_here(owner, span, &found.site, &found.via))
        {
            return marker::NOT_COMPILED;
        }
        marker::NOT_EXPANDED
    }
}

/// The id of each expansion that has skipped text, by the include sites above
/// it, outermost first.
fn skipped_ids(expanded: &ExpandedFile) -> HashMap<Vec<(u32, u32)>, FileId> {
    expanded
        .skipped_text()
        .iter()
        .filter(|skipped| !skipped.via.is_empty())
        .map(|skipped| {
            let key = skipped
                .via
                .iter()
                .map(|site| (site.span.start, site.span.end))
                .collect();
            (key, skipped.site.file)
        })
        .collect()
}

/// The text of each expansion that an `&IF` did not compile, sorted, with
/// overlaps merged.
fn skipped_by_file(expanded: &ExpandedFile) -> HashMap<FileId, Vec<Span>> {
    let mut map: HashMap<FileId, Vec<Span>> = HashMap::new();
    for skipped in expanded.skipped_text() {
        map.entry(skipped.site.file)
            .or_default()
            .push(skipped.site.span);
    }
    for spans in map.values_mut() {
        spans.sort_by_key(|span| span.start);
        let mut merged: Vec<Span> = Vec::with_capacity(spans.len());
        for span in spans.iter() {
            match merged.last_mut() {
                Some(last) if span.start <= last.end => last.end = last.end.max(span.end),
                _ => merged.push(*span),
            }
        }
        *spans = merged;
    }
    map
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
/// raw outline names one: the file as written.
fn include_symbol(text: &str, site: Span) -> Option<OutlineNode> {
    let written = text.get(site.start as usize..site.end as usize)?;
    let inner = written.strip_prefix('{')?.strip_suffix('}')?;
    let file = inner.split_whitespace().next()?;
    let selection = written.find(file).map_or(site, |at| {
        let start = site.start + at as u32;
        Span {
            start,
            end: start + file.len() as u32,
        }
    });
    Some(OutlineNode {
        name: file.to_string(),
        detail: None,
        kind: OutlineKind::Include,
        span: site,
        selection_span: selection,
        file: None,
        opens: None,
        children: Vec::new(),
    })
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
