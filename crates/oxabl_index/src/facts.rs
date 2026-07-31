//! The per-file fact record: everything the index answers with, and nothing
//! else.
//!
//! [`index_file`](crate::index_file) parses a referenced file, runs the declare
//! pass over it, and projects the result down to one of these. Projecting rather
//! than retaining a whole [`Semantic`](oxabl_semantic::Semantic) per indexed
//! file is what keeps a cache's memory proportional to the *facts* — a handful
//! of names per file — instead of to the workspace: a `Semantic` carries a scope
//! tree, a symbol arena, a reference map and a type map for every node, none of
//! which any of the four index queries can answer with.
//!
//! The descriptors are pre-`Arc`ed in exactly the shapes
//! [`WorkspaceIndex`](oxabl_semantic::WorkspaceIndex) hands back, so answering a
//! repeated query is a refcount bump rather than a deep clone — property 3 of
//! the seam (keys and answers are cheap, because a lookup lands on the language
//! server's per-keystroke path).

use std::sync::Arc;

use oxabl_semantic::{ClassDescriptor, IndexName, IndexedFileId, MemberDescriptor};

/// One class or interface a file declares, together with the members it
/// declares *itself*.
///
/// Ancestors are deliberately not folded in — the consumer walks
/// [`ClassDescriptor::inherits`] and asks again per level, which is what
/// preserves per-class early cutoff for an incremental cache.
///
/// `Clone` is two refcount bumps, which is what lets a memo hand out an answer
/// by key without copying the member list.
#[derive(Debug, Clone)]
pub struct ClassFacts {
    /// The answer to [`WorkspaceIndex::class`](oxabl_semantic::WorkspaceIndex::class).
    pub descriptor: Arc<ClassDescriptor>,
    /// The answer to
    /// [`WorkspaceIndex::class_members`](oxabl_semantic::WorkspaceIndex::class_members),
    /// in declaration order.
    pub members: Arc<[MemberDescriptor]>,
}

/// Everything one indexed file contributes to the index's four answers.
#[derive(Debug)]
pub struct FileFacts {
    /// The id this file was indexed under. Every answer derived from these
    /// facts reports it, so the index's own id space is minted in exactly one
    /// place.
    pub file: IndexedFileId,
    /// Whether the file yielded a usable tree.
    ///
    /// `false` means the parse recovered at least one error, so the facts below
    /// are empty *because the file is broken* rather than because it declares
    /// nothing. Callers need the distinction: a broken file is knowably
    /// unusable and answers `NotFound`, while a file that genuinely declares
    /// nothing answers `NotFound` for the same query but is not a signal that
    /// the workspace is misconfigured. Keeping it as a fact on the record — not
    /// an `Option<FileFacts>` — is what lets a memo store one entry per path
    /// either way and never re-read a file it already knows is broken.
    pub parsed: bool,
    /// Classes and interfaces declared at file level, in declaration order.
    pub classes: Vec<ClassFacts>,
    /// Internal procedure names (`PROCEDURE foo:`).
    pub procedures: Vec<IndexName>,
    /// User-defined function names declared at file level. A method is *not* a
    /// function here even though the declare pass gives both
    /// [`SymbolKind::Function`](oxabl_semantic::SymbolKind::Function): a method
    /// is reached through its class and is reported in
    /// [`ClassFacts::members`].
    pub functions: Vec<IndexName>,
    /// Names this file *produces* via `DEFINE NEW [GLOBAL] SHARED`.
    ///
    /// A plain `DEFINE SHARED` is a consumer, not a producer, and never appears
    /// here — linking a consumer to another consumer would invent a producer
    /// that does not exist.
    pub shared_definitions: Vec<IndexName>,
}

impl FileFacts {
    /// The record for a file that was located but could not be used — an
    /// unreadable file, or one whose parse recovered errors.
    ///
    /// Not an error path: a broken file is *knowably* unusable, which the seam
    /// spells `NotFound` (property 1). The id is still assigned so a memo can
    /// remember the verdict per path.
    pub fn unparseable(file: IndexedFileId) -> Self {
        FileFacts {
            file,
            parsed: false,
            classes: Vec::new(),
            procedures: Vec::new(),
            functions: Vec::new(),
            shared_definitions: Vec::new(),
        }
    }

    /// The facts for `name`, if this file declares it.
    ///
    /// A linear scan: an ABL file declares one class in practice (the file name
    /// *is* the class name), so a hash map per file would cost more to build
    /// than the scan it replaces.
    pub fn class(&self, name: &IndexName) -> Option<&ClassFacts> {
        self.classes.iter().find(|c| c.descriptor.name == *name)
    }

    /// Whether this file's `DEFINE NEW [GLOBAL] SHARED` definitions produce
    /// `name`.
    pub fn defines_shared(&self, name: &IndexName) -> bool {
        self.shared_definitions.contains(name)
    }
}
