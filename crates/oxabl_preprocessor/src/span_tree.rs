use std::sync::Arc;

use oxabl_ast::Span;
use oxabl_common::{FileId, FileSpan};

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
    /// Cache of source texts keyed by FileId, needed for `to_text()` and
    /// `resolve()`. Populated during preprocessing.
    sources: Vec<(FileId, Arc<str>)>,
}

impl PreprocessedFile {
    pub(crate) fn new(
        tree: Vec<SpanNode>,
        vars: PreprocVarTable,
        dependencies: Vec<FileId>,
        sources: Vec<(FileId, Arc<str>)>,
    ) -> Self {
        PreprocessedFile {
            tree,
            vars,
            dependencies,
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
            make_sources(&[(file, source)]),
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
            make_sources(&[(parent_id, parent_src), (child_id, child_src)]),
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
            make_sources(&[(file, source)]),
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
            make_sources(&[(parent_id, "AB--------EF"), (child_id, "CD")]),
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
            make_sources(&[(FileId::new(1), "HELLO")]),
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
