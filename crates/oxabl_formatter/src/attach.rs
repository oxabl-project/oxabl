//! Comment attachment (R3): classify every entry of [`Program::comments`] as
//! **leading**, **trailing**, **dangling**, or **interior** against the parsed
//! tree, into a per-run [`CommentMap`] keyed by [`NodeId`], losing no comment.
//!
//! Attachment is advisory metadata computed once per `format()` call; it never
//! mutates the shared AST (R3.2, KTD1). The classifier walks the tree in source
//! order (KTD2), advancing a cursor into the *already sorted* comment table, and
//! decides each comment by line coincidence against the surrounding node spans
//! (KTD3 — trailing-period ownership and the `//`-vs-`&` span-end asymmetry both
//! resolve through the node span, since a node's span already owns its trailing
//! `.` and the classifier keys off `comment.span.start`, never its end).

use std::collections::HashMap;

use oxabl_ast::{Comment, NodeId, Span, Statement};
use oxabl_common::SourceMap;
use oxabl_parser::Program;

use crate::tree::block_children;

/// The comments attached to a single node, split by position (R3.1).
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NodeComments {
    /// Comments on their own line(s) before the node.
    pub leading: Vec<Comment>,
    /// Comments after the node on the same line (owns `END. /* done */`).
    pub trailing: Vec<Comment>,
    /// Comments inside an otherwise-empty block body.
    pub dangling: Vec<Comment>,
    /// Comments strictly inside a single (leaf) node's span, e.g.
    /// `DEFINE VARIABLE x /* counter */ AS INTEGER.` — left in place verbatim by
    /// the printer, never re-placed.
    pub interior: Vec<Comment>,
}

/// Per-`format()`-run comment classification, keyed by [`NodeId`] (KTD1).
///
/// Built once and discarded when the call returns, so the shared AST and its
/// NodeId side-tables are never touched (R3.2).
#[derive(Debug, Default)]
pub struct CommentMap {
    nodes: HashMap<NodeId, NodeComments>,
    /// Comments after the last top-level statement, with no following node to
    /// lead — a stable terminal bucket so a file-trailing comment is never
    /// dropped (R3.4). Keyed conceptually to the `Program` root.
    file_trailing: Vec<Comment>,
}

impl CommentMap {
    /// Comments attached to `id`, if any.
    pub fn get(&self, id: NodeId) -> Option<&NodeComments> {
        self.nodes.get(&id)
    }

    /// The file-trailing bucket (comments after the last top-level statement).
    pub fn file_trailing(&self) -> &[Comment] {
        &self.file_trailing
    }

    /// Iterate every node's comments (used by the printer to place own-line
    /// comments at the right depth).
    pub(crate) fn iter_nodes(&self) -> impl Iterator<Item = (NodeId, &NodeComments)> {
        self.nodes.iter().map(|(&id, nc)| (id, nc))
    }

    /// Total comments held across every bucket — used by the no-loss/no-dup
    /// invariant check (R3.4).
    pub fn total(&self) -> usize {
        self.file_trailing.len()
            + self
                .nodes
                .values()
                .map(|n| n.leading.len() + n.trailing.len() + n.dangling.len() + n.interior.len())
                .sum::<usize>()
    }

    fn entry(&mut self, id: NodeId) -> &mut NodeComments {
        self.nodes.entry(id).or_default()
    }
}

/// Context for the block currently being descended into.
#[derive(Clone, Copy)]
struct Owner {
    id: NodeId,
    /// 1-based line of the block's opening token (its `span.start`).
    start_line: usize,
    /// Byte offset just past the block's full span; bounds comment consumption.
    end_offset: u32,
}

struct Attacher<'a> {
    comments: &'a [Comment],
    cursor: usize,
    sm: &'a SourceMap,
    map: CommentMap,
}

impl Attacher<'_> {
    fn line_of(&self, offset: u32) -> usize {
        self.sm.lookup(offset as usize).0
    }

    /// Line of a span's last content byte (not the exclusive end).
    fn end_line(&self, span: Span) -> usize {
        self.line_of(span.end.saturating_sub(1))
    }

    fn peek_start(&self) -> Option<u32> {
        self.comments.get(self.cursor).map(|c| c.span.start)
    }

    /// Classify a single gap comment against its surrounding context.
    fn classify(
        &mut self,
        c: Comment,
        prev: Option<&Statement>,
        following: Option<&Statement>,
        owner: Option<Owner>,
    ) {
        let c_line = self.line_of(c.span.start);

        // Trailing: same line as the preceding node's end. This is where
        // trailing-period ownership is enforced — the node span already includes
        // its `.`, so `END. /* done */` trails the ended node (R3.3).
        if let Some(p) = prev
            && c_line == self.end_line(p.span)
        {
            self.map.entry(p.id).trailing.push(c);
            return;
        }

        // A comment on the block opener's own line, before any child, trails the
        // block node itself.
        if prev.is_none()
            && let Some(o) = owner
            && c_line == o.start_line
        {
            self.map.entry(o.id).trailing.push(c);
            return;
        }

        // Leading: on its own line before the following node.
        if let Some(f) = following {
            self.map.entry(f.id).leading.push(c);
            return;
        }

        // No following node in this body: a dangling comment inside the block,
        // or (at the top level) a file-trailing comment. Either way, never lost
        // (R3.4).
        if let Some(o) = owner {
            self.map.entry(o.id).dangling.push(c);
        } else {
            self.map.file_trailing.push(c);
        }
    }

    /// Attach every comment falling within `owner`'s body (or the whole file
    /// when `owner` is `None`), recursing into nested blocks.
    fn attach_body(&mut self, siblings: &[&Statement], owner: Option<Owner>) {
        let mut prev: Option<&Statement> = None;

        for &s in siblings {
            // Comments before this sibling begins — leading/trailing/dangling.
            while let Some(start) = self.peek_start() {
                if start >= s.span.start {
                    break;
                }
                let c = self.comments[self.cursor];
                self.cursor += 1;
                self.classify(c, prev, Some(s), owner);
            }

            // Comments inside this sibling.
            match block_children(&s.kind) {
                Some(children) => {
                    // Block: descend. Header/footer/dangling comments resolve
                    // against the children and the block owner.
                    let ctx = Owner {
                        id: s.id,
                        start_line: self.line_of(s.span.start),
                        end_offset: s.span.end,
                    };
                    self.attach_body(&children, Some(ctx));
                }
                None => {
                    // Leaf: any comment strictly inside its span is interior and
                    // is emitted verbatim in place by the printer (Fable
                    // finding 3). The inside-span test fires before the gap
                    // logic by construction — we consume these before moving to
                    // the next sibling's gap.
                    while let Some(start) = self.peek_start() {
                        if start >= s.span.end {
                            break;
                        }
                        let c = self.comments[self.cursor];
                        self.cursor += 1;
                        self.map.entry(s.id).interior.push(c);
                    }
                }
            }

            prev = Some(s);
        }

        // Comments after the last sibling, up to the owner's end (or EOF at the
        // top level): footer/dangling/file-trailing.
        let bound = owner.map(|o| o.end_offset).unwrap_or(u32::MAX);
        while let Some(start) = self.peek_start() {
            if start >= bound {
                break;
            }
            let c = self.comments[self.cursor];
            self.cursor += 1;
            self.classify(c, prev, None, owner);
        }
    }
}

/// Build the [`CommentMap`] for a parsed program against its source.
///
/// `program.comments` is already sorted by `span.start` (Slice 2 invariant, §13),
/// so a single source-order tree walk with a monotonic cursor classifies every
/// comment in `O(n)` after the walk, `O(n log n)` including the tree traversal.
pub fn attach(program: &Program, sm: &SourceMap) -> CommentMap {
    let mut attacher = Attacher {
        comments: &program.comments,
        cursor: 0,
        sm,
        map: CommentMap::default(),
    };
    let top: Vec<&Statement> = program.statements.iter().collect();
    attacher.attach_body(&top, None);
    // Defensive: any comment past the last statement's bound lands file-trailing
    // (attach_body already handles this via the top-level trailing loop, but a
    // comment beyond u32 bounds would be impossible; nothing to drain here).
    debug_assert_eq!(
        attacher.map.total(),
        program.comments.len(),
        "comment attachment lost or duplicated a comment"
    );
    attacher.map
}
