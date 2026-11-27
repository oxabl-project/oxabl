//! Flat expression arena (`ExprId`, `ExprList`, `ExprPool`) and combined
//! [`AstPools`].
//!
//! All [`Expression`] nodes of a parsed file live contiguously in one `Vec`,
//! referenced by 4-byte [`ExprId`] handles. Argument and item lists live in a
//! companion `Vec<ExprId>`, referenced by 8-byte [`ExprList`] ranges. This
//! eliminates per-expression `Box<Expression>` allocations and per-call
//! `Vec<Expression>` allocations without introducing lifetimes into the AST.

use std::fmt;
use std::num::NonZeroU32;
use std::ops::{Index, IndexMut};

use crate::{Expression, Identifier, NameId, NamePool};

/// Compact 4-byte handle into an [`ExprPool`].
///
/// Backed by [`NonZeroU32`] so `Option<ExprId>` is also 4 bytes.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExprId(NonZeroU32);

impl ExprId {
    /// Reserved sentinel for hand-constructed placeholder nodes.
    pub const DUMMY: ExprId = ExprId(NonZeroU32::MAX);

    #[inline]
    pub(crate) fn from_index(index: usize) -> Self {
        let raw = u32::try_from(index)
            .ok()
            .and_then(|n| n.checked_add(1))
            .and_then(NonZeroU32::new);
        match raw {
            Some(nz) if nz != NonZeroU32::MAX => ExprId(nz),
            _ => panic!("ExprPool capacity exceeded (index {index})"),
        }
    }

    /// Zero-based index inside the owning [`ExprPool`].
    #[inline]
    pub const fn index(self) -> usize {
        (self.0.get() - 1) as usize
    }

    /// Raw non-zero representation for diagnostics and debugging.
    #[inline]
    pub const fn as_u32(self) -> u32 {
        self.0.get()
    }
}

impl fmt::Debug for ExprId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if *self == Self::DUMMY {
            f.write_str("ExprId(DUMMY)")
        } else {
            write!(f, "ExprId({})", self.index())
        }
    }
}

/// Contiguous slice of [`ExprId`]s stored inside an [`ExprPool`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct ExprList {
    start: u32,
    len: u32,
}

impl ExprList {
    /// An empty expression list (occupies zero slots in the pool).
    pub const EMPTY: ExprList = ExprList { start: 0, len: 0 };

    /// Number of expressions in this list.
    #[inline]
    pub const fn len(self) -> usize {
        self.len as usize
    }

    /// Whether this list contains zero expressions.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.len == 0
    }
}

#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<ExprId>() == 4);
#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<Option<ExprId>>() == 4);
#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<ExprList>() == 8);

/// Flat, contiguous storage for every [`Expression`] and expression list in one
/// parsed file.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ExprPool {
    nodes: Vec<Expression>,
    lists: Vec<ExprId>,
}

impl ExprPool {
    /// Construct an empty pool with zero allocations.
    pub const fn new() -> Self {
        Self {
            nodes: Vec::new(),
            lists: Vec::new(),
        }
    }

    /// Pre-allocate backing buffers for roughly `node_cap` expressions and
    /// `list_cap` list entries.
    pub fn with_capacity(node_cap: usize, list_cap: usize) -> Self {
        Self {
            nodes: Vec::with_capacity(node_cap),
            lists: Vec::with_capacity(list_cap),
        }
    }

    /// Store `expr` in the pool and return its [`ExprId`].
    #[inline]
    pub fn push(&mut self, expr: Expression) -> ExprId {
        let id = ExprId::from_index(self.nodes.len());
        self.nodes.push(expr);
        id
    }

    /// Store a slice of [`ExprId`]s contiguously and return its [`ExprList`].
    #[inline]
    pub fn push_list(&mut self, items: &[ExprId]) -> ExprList {
        if items.is_empty() {
            return ExprList::EMPTY;
        }
        let start = u32::try_from(self.lists.len())
            .unwrap_or_else(|_| panic!("ExprPool list buffer exceeds u32::MAX"));
        let len = u32::try_from(items.len())
            .unwrap_or_else(|_| panic!("ExprList length exceeds u32::MAX"));
        self.lists.extend_from_slice(items);
        ExprList { start, len }
    }

    /// Look up `id`, returning `None` if out of bounds or [`ExprId::DUMMY`].
    #[inline]
    pub fn try_get(&self, id: ExprId) -> Option<&Expression> {
        self.nodes.get(id.index())
    }

    /// Look up `id`. Panics if `id` does not belong to this pool.
    #[inline]
    pub fn get(&self, id: ExprId) -> &Expression {
        match self.try_get(id) {
            Some(expr) => expr,
            None => panic!(
                "invalid {id:?} for ExprPool of length {} \
                 (did you pass the owning Program's AstPools?)",
                self.nodes.len()
            ),
        }
    }

    /// Mutably look up `id` (used by the parser to stamp spans).
    #[inline]
    pub fn get_mut(&mut self, id: ExprId) -> &mut Expression {
        let len = self.nodes.len();
        match self.nodes.get_mut(id.index()) {
            Some(expr) => expr,
            None => panic!("invalid {id:?} for ExprPool of length {len}"),
        }
    }

    /// Borrow the slice of [`ExprId`]s represented by `list`.
    #[inline]
    pub fn list(&self, list: ExprList) -> &[ExprId] {
        if list.len == 0 {
            return &[];
        }
        let start = list.start as usize;
        let end = start + list.len as usize;
        &self.lists[start..end]
    }

    /// Number of stored expressions.
    #[inline]
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    /// Whether the pool contains no expressions.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }
}

impl Index<ExprId> for ExprPool {
    type Output = Expression;

    #[inline]
    fn index(&self, id: ExprId) -> &Self::Output {
        self.get(id)
    }
}

impl IndexMut<ExprId> for ExprPool {
    #[inline]
    fn index_mut(&mut self, id: ExprId) -> &mut Self::Output {
        self.get_mut(id)
    }
}

impl Index<ExprList> for ExprPool {
    type Output = [ExprId];

    #[inline]
    fn index(&self, list: ExprList) -> &Self::Output {
        self.list(list)
    }
}

/// Owned expression and name pools for one parsed AST.
///
/// Carried by `oxabl_parser::Program` and borrowed by downstream passes.
/// `'static + Clone + Send + Sync` — introduces no lifetime parameters into
/// Salsa queries or downstream types.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct AstPools {
    pub names: NamePool,
    pub exprs: ExprPool,
}

impl AstPools {
    /// Construct empty pools with zero heap allocations.
    pub const fn new() -> Self {
        Self {
            names: NamePool::new(),
            exprs: ExprPool::new(),
        }
    }

    /// Resolve an interned [`NameId`] to its authored text.
    #[inline]
    pub fn name(&self, id: NameId) -> &str {
        self.names.get(id)
    }

    /// Resolve an [`Identifier`]'s interned name to its authored text.
    #[inline]
    pub fn ident(&self, ident: Identifier) -> &str {
        self.names.get(ident.name)
    }

    /// Resolve an [`ExprId`] to its [`Expression`] node.
    #[inline]
    pub fn expr(&self, id: ExprId) -> &Expression {
        self.exprs.get(id)
    }

    /// Resolve an [`ExprList`] to its slice of [`ExprId`] handles.
    #[inline]
    pub fn exprs(&self, list: ExprList) -> &[ExprId] {
        self.exprs.list(list)
    }
}
