use std::path::PathBuf;
use std::sync::Arc;

use oxabl_ast::Span;
use oxabl_common::{Diagnostic, FileId, FileSpan};

use crate::PreprocVarTable;

/// A node in the virtual file expansion tree.
///
/// The preprocessor never materializes the expanded source as a single
/// contiguous string during processing. Instead it builds a tree of `SpanNode`s
/// that reference slices of real source files. The only materialization happens
/// via [`PreprocessedFile::to_text`], called once per file to feed the lexer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpanNode {
    /// A contiguous slice of a real source file.
    Chunk { file: FileId, start: u32, end: u32 },
    /// An expanded include site.
    Include {
        /// The span in the *parent* file where `{file.i}` appeared.
        site: FileSpan,
        /// The resolved path of the included file, when this node is a real
        /// `{file.i}` expansion.
        ///
        /// `None` for a `{&var}` or `{N}` argument substitution. Those share this
        /// variant because they splice foreign text into the parent the same way,
        /// but their content is synthetic and has no file on disk. A consumer
        /// asking "which files does this one include" must therefore filter on
        /// this field rather than counting `Include` nodes.
        path: Option<PathBuf>,
        /// The content of the included file, itself a span tree.
        children: Vec<SpanNode>,
    },
}

impl SpanNode {
    /// Total logical length (in bytes) of the text this node represents.
    fn logical_len(&self) -> u32 {
        match self {
            SpanNode::Chunk { start, end, .. } => end - start,
            SpanNode::Include { children, .. } => children.iter().map(|c| c.logical_len()).sum(),
        }
    }
}

/// An include reference that named no file the preprocessor could locate.
///
/// Kept as data, not only as a PREPROC007 diagnostic, because a dependency edge
/// set has to be able to say "there is an edge here I could not follow". A
/// missing edge that nobody mentions is the failure mode that ends trust in an
/// impact answer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedInclude {
    /// The include as written, after `{&var}` pre-expansion.
    pub name: String,
    /// The `{...}` site, in the coordinates of the file that wrote it.
    pub site: FileSpan,
    /// The enclosing include sites that led here, outermost first.
    ///
    /// Empty when the reference was written in the file being processed. For a
    /// *nested* reference the first entry is the site in that file — the only
    /// coordinate a consumer analysing it can render, since [`Self::site`]
    /// belongs to an include's buffer rather than to the root's bytes.
    pub via: Vec<FileSpan>,
    /// The reference's `{` never closes, so the name is only a best reading of
    /// the text after it. Whether the file exists says nothing about what the
    /// reference would have included.
    pub unterminated: bool,
}

/// An include that resolved to a path whose content could not be read.
///
/// The file exists, so editing it changes the including unit, but its own
/// includes are unknown. Kept as data, like [`UnresolvedInclude`], so a dependency
/// edge set can keep the edge and say what is missing behind it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnreadableInclude {
    /// The include as written, after `{&var}` pre-expansion.
    pub name: String,
    /// The path the include resolved to.
    pub path: PathBuf,
    /// The `{...}` site, in the coordinates of the file that wrote it.
    pub site: FileSpan,
    /// The enclosing include sites that led here, outermost first. Empty when the
    /// reference was written in the file being processed.
    pub via: Vec<FileSpan>,
}

/// The directive that opens a conditional branch.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BranchDirective {
    If,
    ElseIf,
    Else,
}

/// One enclosing conditional branch of a [`SkippedInclude`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SkippedBranch {
    pub directive: BranchDirective,
    /// The condition as written, between `&IF`/`&ELSEIF` and `&THEN`, before any
    /// substitution. An `&ELSE` branch carries the last condition of its chain,
    /// the one that decided it was not reached by the chain before it.
    pub condition: String,
}

/// An include reference written inside a branch that was not taken.
///
/// Kept as data, never expanded, so a build tool that keys a cache on the include
/// set can see includes a different set of defines would reach. Only references
/// inside a file the preprocessor did read are listed; a branch never reached
/// because an enclosing one was not taken is listed too, with every enclosing
/// condition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SkippedInclude {
    /// The include name as written, with any `{...}` reference inside it left as
    /// text.
    pub name: String,
    /// The `{...}` site, in the coordinates of the file that wrote it.
    pub site: FileSpan,
    /// The enclosing include sites that led here, outermost first. Empty when the
    /// reference was written in the file being processed.
    pub via: Vec<FileSpan>,
    /// The enclosing conditional branches, outermost first. The innermost is the
    /// branch that was not taken; the outer ones may have been.
    pub branches: Vec<SkippedBranch>,
    /// The name holds a `{&name}` or `{N}` reference, so the file it would name
    /// depends on values the preprocessor did not substitute.
    pub computed_name: bool,
}

/// Why the preprocessor lost sight of text it was asked to expand.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum TextLossKind {
    /// An include was dropped because the chain reached the depth limit.
    IncludeDepthLimit,
    /// An `&IF` was never closed, so the rest of its file was dropped or kept
    /// on the strength of a branch that has no end.
    UnclosedConditional,
    /// An `&IF` was decided on a predefined name that had no value.
    UnknownPredefinedName,
    /// An `&IF` was decided on a function or shape the evaluator does not model.
    UnmodelledCondition,
}

/// A place where the preprocessor dropped text, or chose between branches without
/// the facts to choose. Kept as data so a dependency edge set can say its
/// absence of an edge proves nothing for this file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextLoss {
    pub kind: TextLossKind,
    /// The directive or reference at fault, in the coordinates of the file that
    /// wrote it.
    pub site: FileSpan,
}

/// The preprocessed representation of a source file.
///
/// Contains the virtual span tree, the preprocessor variable state after
/// processing, and the set of all transitively included files (for change
/// tracking / Salsa invalidation).
#[derive(Debug, Clone)]
pub struct PreprocessedFile {
    /// The root span tree (may be deeply nested for include-heavy files).
    pub tree: Vec<SpanNode>,
    /// Preprocessor variable state after processing this file.
    pub vars: PreprocVarTable,
    /// All include files transitively referenced (for change tracking).
    pub dependencies: Vec<FileId>,
    /// Every include reference, at any depth, that resolved to no file.
    pub unresolved_includes: Vec<UnresolvedInclude>,
    /// Every include reference, at any depth, that resolved to a path whose
    /// content could not be read.
    pub unreadable_includes: Vec<UnreadableInclude>,
    /// Every include reference, at any depth, written inside a branch that was not
    /// taken. Never expanded.
    pub skipped_includes: Vec<SkippedInclude>,
    /// Every place, at any depth, where text was dropped or a branch was chosen
    /// without the facts to choose it.
    pub text_losses: Vec<TextLoss>,
    /// Non-fatal diagnostics accumulated during preprocessing.
    ///
    /// Fatal diagnostics bail out with `Err` from [`crate::Preprocessor::process`].
    /// Anything here is an issue the preprocessor recovered from (e.g. an
    /// include file that could not be resolved) — callers should surface these
    /// to the user so the root cause is visible, even when parsing succeeds.
    pub diagnostics: Vec<Diagnostic>,
    /// Cache of source texts keyed by FileId, needed for `to_text()` and
    /// `resolve()`. Populated during preprocessing.
    sources: Vec<(FileId, Arc<str>)>,
}

impl PreprocessedFile {
    pub(crate) fn new(
        tree: Vec<SpanNode>,
        vars: PreprocVarTable,
        dependencies: Vec<FileId>,
        unresolved_includes: Vec<UnresolvedInclude>,
        sources: Vec<(FileId, Arc<str>)>,
        diagnostics: Vec<Diagnostic>,
    ) -> Self {
        PreprocessedFile {
            tree,
            vars,
            dependencies,
            unresolved_includes,
            unreadable_includes: Vec::new(),
            skipped_includes: Vec::new(),
            text_losses: Vec::new(),
            diagnostics,
            sources,
        }
    }

    /// Map a virtual (logical) byte offset back to its real source location.
    ///
    /// Walks the span tree with a running cumulative offset. O(tree depth)
    /// for typical files; O(n) in the worst case for flat trees with many
    /// chunks.
    pub fn resolve(&self, virtual_offset: u32) -> FileSpan {
        let mut remaining = virtual_offset;
        if let Some(fs) = resolve_in_nodes(&self.tree, &mut remaining) {
            return fs;
        }
        // Fallback: offset is past the end of the tree
        FileSpan {
            file: FileId::UNKNOWN,
            span: Span {
                start: virtual_offset,
                end: virtual_offset,
            },
        }
    }

    /// Produce the logical source text for lexing.
    ///
    /// This is the *only* place a `String` is materialized — called once per
    /// file per preprocessing run. The key benefit of the tree is position
    /// resolution without re-scanning.
    pub fn to_text(&self) -> Arc<str> {
        let mut buf = String::new();
        self.append_text(&self.tree, &mut buf);
        Arc::from(buf)
    }

    fn append_text(&self, nodes: &[SpanNode], buf: &mut String) {
        for node in nodes {
            match node {
                SpanNode::Chunk { file, start, end } => {
                    if let Some(source) = self.get_source(*file) {
                        let s = *start as usize;
                        let e = *end as usize;
                        if e <= source.len() {
                            buf.push_str(&source[s..e]);
                        }
                    }
                }
                SpanNode::Include { children, .. } => {
                    self.append_text(children, buf);
                }
            }
        }
    }

    fn get_source(&self, file: FileId) -> Option<&str> {
        // Ids are handed out in the order the sources are pushed, so the source
        // is usually at its id's offset from the first. The scan covers a table
        // built in another order.
        let first = self.sources.first()?.0;
        let guess = file.raw().checked_sub(first.raw()).map(|n| n as usize);
        if let Some((id, src)) = guess.and_then(|n| self.sources.get(n))
            && *id == file
        {
            return Some(src);
        }
        self.sources
            .iter()
            .find(|(id, _)| *id == file)
            .map(|(_, src)| &**src)
    }
}

/// Walk nodes to resolve a virtual offset to a real `FileSpan`.
fn resolve_in_nodes(nodes: &[SpanNode], remaining: &mut u32) -> Option<FileSpan> {
    for node in nodes {
        let len = node.logical_len();
        if *remaining < len {
            match node {
                SpanNode::Chunk { file, start, .. } => {
                    let real_offset = start + *remaining;
                    return Some(FileSpan {
                        file: *file,
                        span: Span {
                            start: real_offset,
                            end: real_offset,
                        },
                    });
                }
                SpanNode::Include { children, .. } => {
                    return resolve_in_nodes(children, remaining);
                }
            }
        }
        *remaining -= len;
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_sources(pairs: &[(FileId, &str)]) -> Vec<(FileId, Arc<str>)> {
        pairs.iter().map(|(id, s)| (*id, Arc::from(*s))).collect()
    }

    #[test]
    fn single_chunk_to_text() {
        let file = FileId::new(1);
        let source = "DEFINE VARIABLE x AS INTEGER.";
        let pf = PreprocessedFile::new(
            vec![SpanNode::Chunk {
                file,
                start: 0,
                end: source.len() as u32,
            }],
            PreprocVarTable::new(),
            vec![],
            Vec::new(),
            make_sources(&[(file, source)]),
            vec![],
        );
        assert_eq!(&*pf.to_text(), source);
    }

    #[test]
    fn include_to_text() {
        let parent_id = FileId::new(1);
        let child_id = FileId::new(2);
        let parent_src = "BEFORE {inc.i} AFTER";
        let child_src = "INCLUDED";

        // Tree: "BEFORE " + include(INCLUDED) + " AFTER"
        let tree = vec![
            SpanNode::Chunk {
                file: parent_id,
                start: 0,
                end: 7, // "BEFORE "
            },
            SpanNode::Include {
                site: FileSpan {
                    file: parent_id,
                    span: Span { start: 7, end: 14 }, // "{inc.i}"
                },
                path: Some(PathBuf::from("/proj/inc.i")),
                children: vec![SpanNode::Chunk {
                    file: child_id,
                    start: 0,
                    end: 8, // "INCLUDED"
                }],
            },
            SpanNode::Chunk {
                file: parent_id,
                start: 14,
                end: 20, // " AFTER"
            },
        ];

        let pf = PreprocessedFile::new(
            tree,
            PreprocVarTable::new(),
            vec![child_id],
            Vec::new(),
            make_sources(&[(parent_id, parent_src), (child_id, child_src)]),
            vec![],
        );

        assert_eq!(&*pf.to_text(), "BEFORE INCLUDED AFTER");
    }

    #[test]
    fn resolve_in_single_chunk() {
        let file = FileId::new(1);
        let source = "HELLO WORLD";
        let pf = PreprocessedFile::new(
            vec![SpanNode::Chunk {
                file,
                start: 0,
                end: 11,
            }],
            PreprocVarTable::new(),
            vec![],
            Vec::new(),
            make_sources(&[(file, source)]),
            vec![],
        );

        let resolved = pf.resolve(6);
        assert_eq!(resolved.file, file);
        assert_eq!(resolved.span.start, 6); // "W" in "WORLD"
    }

    #[test]
    fn resolve_into_include() {
        let parent_id = FileId::new(1);
        let child_id = FileId::new(2);

        // Logical: "AB" (parent 0..2) + "CD" (child 0..2) + "EF" (parent 10..12)
        let tree = vec![
            SpanNode::Chunk {
                file: parent_id,
                start: 0,
                end: 2,
            },
            SpanNode::Include {
                site: FileSpan {
                    file: parent_id,
                    span: Span { start: 2, end: 10 },
                },
                path: Some(PathBuf::from("/proj/inc.i")),
                children: vec![SpanNode::Chunk {
                    file: child_id,
                    start: 0,
                    end: 2,
                }],
            },
            SpanNode::Chunk {
                file: parent_id,
                start: 10,
                end: 12,
            },
        ];

        let pf = PreprocessedFile::new(
            tree,
            PreprocVarTable::new(),
            vec![child_id],
            Vec::new(),
            make_sources(&[(parent_id, "AB--------EF"), (child_id, "CD")]),
            vec![],
        );

        // Offset 0 → parent byte 0 ("A")
        let r0 = pf.resolve(0);
        assert_eq!(r0.file, parent_id);
        assert_eq!(r0.span.start, 0);

        // Offset 2 → child byte 0 ("C")
        let r2 = pf.resolve(2);
        assert_eq!(r2.file, child_id);
        assert_eq!(r2.span.start, 0);

        // Offset 3 → child byte 1 ("D")
        let r3 = pf.resolve(3);
        assert_eq!(r3.file, child_id);
        assert_eq!(r3.span.start, 1);

        // Offset 4 → parent byte 10 ("E")
        let r4 = pf.resolve(4);
        assert_eq!(r4.file, parent_id);
        assert_eq!(r4.span.start, 10);
    }

    #[test]
    fn resolve_past_end_returns_unknown() {
        let pf = PreprocessedFile::new(
            vec![SpanNode::Chunk {
                file: FileId::new(1),
                start: 0,
                end: 5,
            }],
            PreprocVarTable::new(),
            vec![],
            Vec::new(),
            make_sources(&[(FileId::new(1), "HELLO")]),
            vec![],
        );

        let r = pf.resolve(100);
        assert_eq!(r.file, FileId::UNKNOWN);
    }

    #[test]
    fn logical_len_nested() {
        let node = SpanNode::Include {
            site: FileSpan {
                file: FileId::new(1),
                span: Span { start: 0, end: 10 },
            },
            path: Some(PathBuf::from("/proj/inc.i")),
            children: vec![
                SpanNode::Chunk {
                    file: FileId::new(2),
                    start: 0,
                    end: 5,
                },
                SpanNode::Chunk {
                    file: FileId::new(2),
                    start: 10,
                    end: 15,
                },
            ],
        };
        assert_eq!(node.logical_len(), 10);
    }
}
