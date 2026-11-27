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

use crate::{Expression, Identifier, NameId, NamePool, PreprocIf, TypeArguments};

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

/// Defines a 4-byte handle into one of the side pools of an [`ExprPool`].
///
/// The payloads own a `Vec`, so they live beside the nodes and keep
/// [`Expression`] free of drop glue.
macro_rules! side_id {
    ($(#[$doc:meta])* $name:ident, $pool:literal) => {
        $(#[$doc])*
        #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $name(NonZeroU32);

        impl $name {
            #[inline]
            pub(crate) fn from_index(index: usize) -> Self {
                let raw = u32::try_from(index)
                    .ok()
                    .and_then(|n| n.checked_add(1))
                    .and_then(NonZeroU32::new);
                match raw {
                    Some(nz) => $name(nz),
                    None => panic!(concat!($pool, " capacity exceeded (index {})"), index),
                }
            }

            /// Zero-based index inside the owning pool.
            #[inline]
            pub const fn index(self) -> usize {
                (self.0.get() - 1) as usize
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, concat!(stringify!($name), "({})"), self.index())
            }
        }
    };
}

side_id! {
    /// Handle to the [`PreprocIf`] of a mid-expression `&IF` in an [`ExprPool`].
    PreprocIfId, "ExprPool preproc-if"
}

side_id! {
    /// Handle to the [`TypeArguments`] of a generic `NEW` in an [`ExprPool`].
    TypeArgsId, "ExprPool type-argument"
}

#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<ExprId>() == 4);
#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<Option<TypeArgsId>>() == 4);
#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<PreprocIfId>() == 4);
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
    preproc_ifs: Vec<PreprocIf<ExprId>>,
    type_args: Vec<TypeArguments>,
}

impl ExprPool {
    /// Construct an empty pool with zero allocations.
    pub const fn new() -> Self {
        Self {
            nodes: Vec::new(),
            lists: Vec::new(),
            preproc_ifs: Vec::new(),
            type_args: Vec::new(),
        }
    }

    /// Pre-allocate backing buffers for roughly `node_cap` expressions and
    /// `list_cap` list entries.
    pub fn with_capacity(node_cap: usize, list_cap: usize) -> Self {
        Self {
            nodes: Vec::with_capacity(node_cap),
            lists: Vec::with_capacity(list_cap),
            preproc_ifs: Vec::new(),
            type_args: Vec::new(),
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

    /// Store the conditional of a mid-expression `&IF`.
    #[inline]
    pub fn push_preproc_if(&mut self, preproc: PreprocIf<ExprId>) -> PreprocIfId {
        let id = PreprocIfId::from_index(self.preproc_ifs.len());
        self.preproc_ifs.push(preproc);
        id
    }

    /// Look up a stored `&IF`. Panics if `id` does not belong to this pool.
    #[inline]
    pub fn preproc_if(&self, id: PreprocIfId) -> &PreprocIf<ExprId> {
        match self.preproc_ifs.get(id.index()) {
            Some(preproc) => preproc,
            None => panic!(
                "invalid {id:?} for ExprPool of {} preproc-ifs",
                self.preproc_ifs.len()
            ),
        }
    }

    /// Store the type arguments of a generic `NEW`.
    #[inline]
    pub fn push_type_args(&mut self, arguments: TypeArguments) -> TypeArgsId {
        let id = TypeArgsId::from_index(self.type_args.len());
        self.type_args.push(arguments);
        id
    }

    /// Look up stored type arguments. Panics if `id` does not belong to this pool.
    #[inline]
    pub fn type_args(&self, id: TypeArgsId) -> &TypeArguments {
        match self.type_args.get(id.index()) {
            Some(arguments) => arguments,
            None => panic!(
                "invalid {id:?} for ExprPool of {} type-argument lists",
                self.type_args.len()
            ),
        }
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

    /// Resolve a [`PreprocIfId`] to its mid-expression `&IF`.
    #[inline]
    pub fn preproc_if(&self, id: PreprocIfId) -> &PreprocIf<ExprId> {
        self.exprs.preproc_if(id)
    }

    /// Resolve a [`TypeArgsId`] to its type arguments.
    #[inline]
    pub fn type_args(&self, id: TypeArgsId) -> &TypeArguments {
        self.exprs.type_args(id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ExpressionKind, Literal, Span, UnknownLiteral};

    fn expr() -> Expression {
        Expression::new(ExpressionKind::Literal(Literal::Unknown(UnknownLiteral {
            span: Span::DUMMY,
        })))
    }

    fn pool_of(count: usize) -> (ExprPool, Vec<ExprId>) {
        let mut pool = ExprPool::new();
        let ids = (0..count).map(|_| pool.push(expr())).collect();
        (pool, ids)
    }

    #[test]
    fn try_get_gives_the_node_of_a_valid_handle() {
        let (pool, ids) = pool_of(2);
        assert_eq!(pool.try_get(ids[0]), Some(&expr()));
        assert_eq!(pool.try_get(ids[1]), Some(&expr()));
    }

    #[test]
    fn try_get_gives_none_for_dummy() {
        let (pool, _) = pool_of(1);
        assert_eq!(pool.try_get(ExprId::DUMMY), None);
        assert_eq!(ExprPool::new().try_get(ExprId::DUMMY), None);
    }

    #[test]
    fn try_get_gives_none_for_an_out_of_range_handle() {
        let (pool, _) = pool_of(1);
        let (_, other) = pool_of(3);
        assert_eq!(pool.try_get(other[1]), None);
        assert_eq!(pool.try_get(other[2]), None);
    }

    #[test]
    #[should_panic(expected = "invalid ExprId(DUMMY) for ExprPool of length 1")]
    fn get_panics_on_dummy() {
        let (pool, _) = pool_of(1);
        let _ = pool.get(ExprId::DUMMY);
    }

    #[test]
    #[should_panic(expected = "invalid ExprId(2) for ExprPool of length 1")]
    fn get_panics_on_an_out_of_range_handle() {
        let (pool, _) = pool_of(1);
        let (_, other) = pool_of(3);
        let _ = pool.get(other[2]);
    }

    #[test]
    #[should_panic(expected = "invalid ExprId(DUMMY) for ExprPool of length 1")]
    fn get_mut_panics_on_dummy() {
        let (mut pool, _) = pool_of(1);
        let _ = pool.get_mut(ExprId::DUMMY);
    }

    #[test]
    #[should_panic(expected = "invalid ExprId(DUMMY)")]
    fn index_panics_on_dummy() {
        let (pool, _) = pool_of(1);
        let _ = &pool[ExprId::DUMMY];
    }

    #[test]
    fn side_pools_read_back_what_was_pushed() {
        let (mut pool, ids) = pool_of(2);
        let first = pool.push_preproc_if(PreprocIf {
            condition: ids[0],
            then_branch: ids[1],
            elseif_branches: vec![(ids[1], ids[0])],
            else_branch: Some(ids[0]),
        });
        let second = pool.push_preproc_if(PreprocIf {
            condition: ids[1],
            then_branch: ids[0],
            elseif_branches: Vec::new(),
            else_branch: None,
        });
        assert_eq!(pool.preproc_if(first).then_branch, ids[1]);
        assert_eq!(pool.preproc_if(first).elseif_branches.len(), 1);
        assert_eq!(pool.preproc_if(second).condition, ids[1]);
        assert_eq!(pool.preproc_if(second).else_branch, None);

        let args = pool.push_type_args(TypeArguments::default());
        assert!(pool.type_args(args).0.is_empty());
    }

    #[test]
    #[should_panic(expected = "invalid PreprocIfId(0) for ExprPool of 0 preproc-ifs")]
    fn preproc_if_panics_on_a_foreign_handle() {
        let (mut other, ids) = pool_of(1);
        let id = other.push_preproc_if(PreprocIf {
            condition: ids[0],
            then_branch: ids[0],
            elseif_branches: Vec::new(),
            else_branch: None,
        });
        let _ = ExprPool::new().preproc_if(id);
    }

    #[test]
    fn empty_list_reads_as_an_empty_slice() {
        let (pool, _) = pool_of(2);
        assert!(ExprList::EMPTY.is_empty());
        assert_eq!(ExprList::EMPTY.len(), 0);
        assert!(pool.list(ExprList::EMPTY).is_empty());
        assert!(ExprPool::new().list(ExprList::EMPTY).is_empty());
    }

    #[test]
    fn pushing_an_empty_list_gives_empty() {
        let (mut pool, ids) = pool_of(2);
        pool.push_list(&ids);
        assert_eq!(pool.push_list(&[]), ExprList::EMPTY);
    }

    #[test]
    fn a_pushed_list_reads_back_in_order() {
        let (mut pool, ids) = pool_of(3);
        let first = pool.push_list(&[ids[2], ids[0]]);
        let second = pool.push_list(&[ids[1]]);
        assert_eq!(pool.list(first), &[ids[2], ids[0]]);
        assert_eq!(pool.list(second), &[ids[1]]);
        assert_eq!(first.len(), 2);
        assert!(!first.is_empty());
    }
}
