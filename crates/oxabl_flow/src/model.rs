//! The data model: units, ABL blocks, CFG nodes and edges.

use oxabl_ast::{NodeId, Span};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

macro_rules! index_type {
    ($(#[$doc:meta])* $name:ident) => {
        $(#[$doc])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(pub(crate) u32);

        impl $name {
            /// The raw index, for side tables sized to the owning collection.
            #[inline]
            pub const fn index(self) -> usize {
                self.0 as usize
            }

            /// The id at a raw index, e.g. a member of a [`BitSet`](crate::dataflow::BitSet)
            /// of nodes. Not checked against any collection.
            #[inline]
            pub const fn from_index(index: usize) -> Self {
                $name(index as u32)
            }
        }
    };
}

index_type!(
    /// A node of one [`Cfg`].
    NodeIx
);
index_type!(
    /// An ABL block of one [`Cfg`]. `BlockId(0)` is always the routine block.
    BlockId
);
index_type!(
    /// A unit of one [`FileFlow`](crate::FileFlow), in source pre-order.
    /// `UnitId(0)` is always the file's main block.
    UnitId
);

impl BlockId {
    /// The routine block every unit has.
    pub const ROUTINE: BlockId = BlockId(0);
}

/// What kind of scope-owning unit a [`Cfg`] covers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnitKind {
    /// The main block of a procedure file (`.p`, `.w`, `.i`), or the file
    /// level of a class file, which has no executable code.
    Main,
    /// The main block of a `TRIGGER PROCEDURE` file.
    DbTrigger,
    /// `PROCEDURE name: ... END.`
    Procedure,
    /// `FUNCTION name ...: ... END.` (forward declarations are not units).
    Function,
    /// `METHOD ...: ... END.` (abstract methods are not units).
    Method,
    /// `CONSTRUCTOR ...: ... END.`
    Constructor,
    /// `DESTRUCTOR ...: ... END.`
    Destructor,
    /// A computed `GET` accessor of a property.
    PropertyGet,
    /// A computed `SET` accessor of a property.
    PropertySet,
    /// The block of an `ON event ... DO: ... END.` trigger.
    Trigger,
}

impl UnitKind {
    /// Lowercase name used in dumps.
    pub fn as_str(self) -> &'static str {
        match self {
            UnitKind::Main => "main",
            UnitKind::DbTrigger => "db-trigger",
            UnitKind::Procedure => "procedure",
            UnitKind::Function => "function",
            UnitKind::Method => "method",
            UnitKind::Constructor => "constructor",
            UnitKind::Destructor => "destructor",
            UnitKind::PropertyGet => "property-get",
            UnitKind::PropertySet => "property-set",
            UnitKind::Trigger => "trigger",
        }
    }
}

/// A scope-owning unit: the code one [`Cfg`] covers.
#[derive(Debug, Clone)]
pub struct Unit {
    pub kind: UnitKind,
    /// Procedure, function, method or property name, as written.
    pub name: Option<String>,
    /// The statement that defines the unit; [`NodeId::PROGRAM`] for the main
    /// block. Property accessors share their property's id, told apart by
    /// `kind`. This is also the owner node of the matching semantic scope.
    pub stmt: NodeId,
    pub span: Span,
    /// The unit this one is nested in, lexically.
    pub parent: Option<UnitId>,
}

/// Which records a `FOR` block reads, from its first record phrase.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ForQualifier {
    Each,
    First,
    Last,
    Next,
    Prev,
    /// `FOR table ...` with no qualifier.
    Unqualified,
}

/// The kind of an ABL block.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockKind {
    /// The block of a whole unit.
    Routine(UnitKind),
    Do,
    Repeat,
    For(ForQualifier),
    Catch,
    Finally,
}

impl BlockKind {
    /// Lowercase name used in dumps.
    pub fn as_str(self) -> &'static str {
        match self {
            BlockKind::Routine(k) => k.as_str(),
            BlockKind::Do => "do",
            BlockKind::Repeat => "repeat",
            BlockKind::For(ForQualifier::Each) => "for-each",
            BlockKind::For(ForQualifier::First) => "for-first",
            BlockKind::For(ForQualifier::Last) => "for-last",
            BlockKind::For(ForQualifier::Next) => "for-next",
            BlockKind::For(ForQualifier::Prev) => "for-prev",
            BlockKind::For(ForQualifier::Unqualified) => "for",
            BlockKind::Catch => "catch",
            BlockKind::Finally => "finally",
        }
    }
}

/// An ABL condition a block can handle with an `ON ... UNDO` phrase.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Condition {
    Error,
    Endkey,
    Stop,
    Quit,
}

impl Condition {
    /// All conditions, in [`Block::handling`] index order.
    pub const ALL: [Condition; 4] = [
        Condition::Error,
        Condition::Endkey,
        Condition::Stop,
        Condition::Quit,
    ];

    /// Lowercase name used in dumps.
    pub fn as_str(self) -> &'static str {
        match self {
            Condition::Error => "error",
            Condition::Endkey => "endkey",
            Condition::Stop => "stop",
            Condition::Quit => "quit",
        }
    }
}

/// What a `RETURN` action hands back.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReturnKind {
    /// `RETURN [value]`.
    Normal,
    /// `RETURN ERROR [value]`: raises ERROR in the caller.
    Error,
    /// `RETURN NO-APPLY`.
    NoApply,
}

/// The action after an `UNDO`, in an `ON` phrase or an `UNDO` statement.
/// Labels are as written; `None` means the block the phrase or statement
/// undoes.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Action {
    Retry(Option<String>),
    Leave(Option<String>),
    Next(Option<String>),
    Return(ReturnKind),
    Throw,
}

/// Where a block's handling of a condition comes from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Origin {
    /// An explicit `ON ... UNDO` phrase on the block header.
    Phrase,
    /// ABL's implicit default for the block kind.
    Default,
    /// `ROUTINE-LEVEL ON ERROR UNDO, THROW` changed the default.
    RoutineLevel,
    /// `BLOCK-LEVEL ON ERROR UNDO, THROW` changed the default.
    BlockLevel,
}

/// How a block handles one condition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Handling {
    /// `UNDO label`, as written; `None` undoes this block.
    pub undo: Option<String>,
    pub action: Action,
    pub origin: Origin,
}

/// A file-level directive that changes the default `ON ERROR` action.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ErrorDirective {
    /// `ROUTINE-LEVEL ON ERROR UNDO, THROW`: routine blocks throw.
    RoutineLevel,
    /// `BLOCK-LEVEL ON ERROR UNDO, THROW`: routine blocks and every block with
    /// default error handling throw.
    BlockLevel,
}

/// What a `RETRY` on a block does at run time.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RetryBehavior {
    /// The block holds an input-blocking statement, so it really retries.
    Retry,
    /// Infinite-loop protection: an iterating block goes to its next iteration.
    Next,
    /// Infinite-loop protection: a non-iterating block is left.
    Leave,
}

/// An ABL block with its properties.
///
/// The defaults applied when the header has no phrase, per kind:
///
/// | block | ERROR | ENDKEY | STOP, QUIT |
/// |---|---|---|---|
/// | routine, `REPEAT`, `FOR`, `DO TRANSACTION` | `UNDO, RETRY` | `UNDO, LEAVE` | none |
/// | plain `DO` | none | none | none |
/// | `CATCH`, `FINALLY` | `UNDO, THROW` past the associated block | none | none |
///
/// `ROUTINE-LEVEL ON ERROR UNDO, THROW` makes routine blocks (not
/// destructors) throw; `BLOCK-LEVEL` also makes `REPEAT`, `FOR` and
/// `DO TRANSACTION` throw. "None" means the condition passes to the enclosing
/// block; an unhandled STOP or QUIT leaves the unit.
#[derive(Debug, Clone)]
pub struct Block {
    pub kind: BlockKind,
    /// The statement that opens the block (the unit statement for the routine
    /// block). A label wrapper is not the block statement; its name is in
    /// `label`.
    pub stmt: NodeId,
    pub span: Span,
    pub parent: Option<BlockId>,
    pub label: Option<String>,
    /// `REPEAT`, `FOR`, and `DO` with `WHILE` or `TO`.
    pub iterating: bool,
    /// The `TRANSACTION` keyword is on the header.
    pub transaction: bool,
    /// `STOP-AFTER` is on the header. Every statement in the block, and in the
    /// blocks nested in it, may then raise STOP.
    pub stop_after: bool,
    /// For a `FOR` block, the buffers of its record phrases (scoped to the
    /// block unless used outside it); for `DO FOR` / `REPEAT FOR`, the buffers
    /// strongly scoped to the block. As written.
    pub buffers: Vec<String>,
    /// The block (or a block nested in it) has an input-blocking statement
    /// (`UPDATE`, `SET`, `PROMPT-FOR`, `INSERT`, `WAIT-FOR`, `READKEY`,
    /// `CHOOSE`, `MESSAGE ... UPDATE`), so `RETRY` is not converted. These
    /// statements, `IMPORT` and `PAUSE` raise ENDKEY.
    pub input_blocking: bool,
    /// Indexed by [`Condition`] order; `None` passes the condition on.
    pub handling: [Option<Handling>; 4],
    /// `CATCH` blocks associated with this block, in source order.
    pub catches: SmallVec<[BlockId; 1]>,
    /// The `FINALLY` block associated with this block.
    pub finally: Option<BlockId>,
    /// For a `CATCH` block, the caught type as written.
    pub catch_type: Option<String>,
}

impl Block {
    /// How this block handles `condition`, or `None` when it passes it on.
    pub fn handling(&self, condition: Condition) -> Option<&Handling> {
        self.handling[condition as usize].as_ref()
    }

    /// What `RETRY` on this block does.
    pub fn retry_becomes(&self) -> RetryBehavior {
        if self.input_blocking {
            RetryBehavior::Retry
        } else if self.iterating {
            RetryBehavior::Next
        } else {
            RetryBehavior::Leave
        }
    }

    /// Whether this `CATCH` block catches every error object (its type is
    /// `Progress.Lang.Error`).
    pub fn catches_all_errors(&self) -> bool {
        self.catch_type.as_deref().is_some_and(|t| {
            t.eq_ignore_ascii_case("Progress.Lang.Error") || t.eq_ignore_ascii_case("Error")
        })
    }

    /// Whether this `CATCH` block may catch `condition`. ERROR is caught by
    /// any non-stop type (the error type is not resolved); STOP only by the
    /// `Progress.Lang.Stop` family.
    pub fn may_catch(&self, condition: Condition) -> bool {
        let Some(t) = self.catch_type.as_deref() else {
            return false;
        };
        match condition {
            Condition::Error => !is_stop_type(t),
            Condition::Stop => is_stop_type(t),
            Condition::Endkey | Condition::Quit => false,
        }
    }

    /// Whether this `CATCH` block catches `condition` whatever is raised.
    pub fn catches_all(&self, condition: Condition) -> bool {
        match condition {
            Condition::Error => self.catches_all_errors(),
            Condition::Stop => self.catch_type.as_deref().is_some_and(|t| {
                t.eq_ignore_ascii_case("Progress.Lang.Stop") || t.eq_ignore_ascii_case("Stop")
            }),
            Condition::Endkey | Condition::Quit => false,
        }
    }
}

fn is_stop_type(t: &str) -> bool {
    let last = t.rsplit('.').next().unwrap_or(t);
    [
        "Stop",
        "StopError",
        "StopAfter",
        "UserInterrupt",
        "LockConflict",
    ]
    .iter()
    .any(|s| last.eq_ignore_ascii_case(s))
}

/// How control leaves a unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExitKind {
    /// Normal completion: the end of the routine, `RETURN`, or a `LEAVE` of
    /// the routine block.
    Return,
    /// ERROR raised in the caller: an unhandled error, `UNDO, THROW` out of
    /// the routine, or `RETURN ERROR`.
    Error,
    /// An unhandled STOP condition.
    Stop,
    /// An unhandled QUIT condition.
    Quit,
}

impl ExitKind {
    /// Lowercase name used in dumps.
    pub fn as_str(self) -> &'static str {
        match self {
            ExitKind::Return => "return",
            ExitKind::Error => "error",
            ExitKind::Stop => "stop",
            ExitKind::Quit => "quit",
        }
    }
}

/// What a node stands for. Every node also has a [`Node::block`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeKind {
    /// Unit entry.
    Entry,
    /// Unit exit.
    Exit(ExitKind),
    /// One statement executes. For `IF`, `CASE` and `&IF` it is the test, and
    /// the branches leave on [`EdgeKind::True`] and the like.
    Stmt,
    /// The block header executes: on entry, and for an iterating block on each
    /// iteration (the `WHILE`/`TO` test or the record fetch).
    Head,
    /// End of the block body (end of one iteration). A `NEXT` arrives here.
    End,
    /// The point after the given block, where `LEAVE` and loop exhaustion
    /// arrive. The node itself is in the enclosing block.
    After(BlockId),
    /// The block undoes and handles a raised condition: the edges go to its
    /// `CATCH` blocks and to the `ON` phrase action.
    Handler(Condition),
}

/// Why a node is not fully modelled. Consumers decide whether that matters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OpaqueReason {
    /// A statement the parser recognizes but skips (`PUT`, `UPDATE`, ...).
    /// Modelled as "may raise, then falls through".
    SkippedStatement,
    /// An include reference that was not expanded.
    UnexpandedInclude,
    /// `LEAVE`/`NEXT`/`UNDO` names a label no enclosing block has; the jump
    /// was given its unlabelled target.
    UnresolvedLabel,
    /// A statement the parser left empty whose tokens are not a known
    /// control statement (often parse recovery). Modelled as a no-op.
    UnrecognizedStatement,
    /// A `CATCH` or `FINALLY` outside a block body. Not modelled.
    StrayHandlerBlock,
}

impl OpaqueReason {
    /// Kebab-case name used in dumps and reports.
    pub fn as_str(self) -> &'static str {
        match self {
            OpaqueReason::SkippedStatement => "skipped-statement",
            OpaqueReason::UnexpandedInclude => "unexpanded-include",
            OpaqueReason::UnresolvedLabel => "unresolved-label",
            OpaqueReason::UnrecognizedStatement => "unrecognized-statement",
            OpaqueReason::StrayHandlerBlock => "stray-handler-block",
        }
    }
}

/// A CFG node.
#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    /// The innermost ABL block the node is in.
    pub block: BlockId,
    /// The AST statement: the statement for [`NodeKind::Stmt`], the block
    /// statement for `Head`/`End`/`After`/`Handler`, the unit statement for
    /// `Entry`.
    pub stmt: Option<NodeId>,
    pub span: Span,
    pub opaque: Option<OpaqueReason>,
    /// The block an `UNDO` statement undoes: the one it names, else the
    /// closest with the error property (a `CATCH`'s owner, for an `UNDO` in a
    /// `CATCH`). `None` for every other node; see [`Cfg::undone_block`].
    pub undoes: Option<BlockId>,
}

/// Why control moves along an edge.
///
/// A jump that runs `FINALLY` blocks on its way goes through a copy of each,
/// and every hop of that path keeps the jump's kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EdgeKind {
    /// Sequential flow.
    Normal,
    /// `IF` condition true.
    True,
    /// `IF` condition false (also when there is no `ELSE`).
    False,
    /// The n-th `WHEN` of a `CASE` (0-based).
    When(u32),
    /// `CASE` `OTHERWISE`, or no `WHEN` matched.
    Otherwise,
    /// One branch of a statement-level `&IF` (chosen at compile time).
    Preproc,
    /// Block header to body: the first statement, or the next iteration.
    LoopBody,
    /// Block header to after the block: the test failed or no record. Also
    /// the end of the only iteration of a `FOR FIRST` or `FOR LAST`.
    LoopDone,
    /// End of an iteration back to the header.
    LoopBack,
    /// `LEAVE`, or an `UNDO, LEAVE` action, or an implicit leave.
    Leave,
    /// `NEXT`, or an `UNDO, NEXT` action.
    Next,
    /// An `UNDO, RETRY` action that really retries.
    Retry,
    /// `RETURN`, or an `UNDO, RETURN` action.
    Return,
    /// `RETURN ERROR`, or an `UNDO, RETURN ERROR` action.
    ReturnError,
    /// `THROW`, `UNDO, THROW`, or an `ON ERROR UNDO, THROW` action.
    Throw,
    /// The source may raise this condition: an implicit error, ENDKEY from an
    /// input statement, the `STOP`/`QUIT` statements, or a STOP that a
    /// `STOP-AFTER` block or a call, lock wait or `WAIT-FOR` can bring.
    Raise(Condition),
    /// A handler hands the condition to a `CATCH` block.
    Catch,
}

/// A CFG edge.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Edge {
    pub from: NodeIx,
    pub to: NodeIx,
    pub kind: EdgeKind,
}

/// The control-flow graph of one unit.
///
/// Nodes are statement-level. A `FINALLY` body appears once per distinct
/// continuation (normal end, each `LEAVE` target, error propagation, ...), so a
/// statement can have several nodes; see [`Cfg::nodes_of`].
#[derive(Debug, Clone)]
pub struct Cfg {
    pub(crate) id: UnitId,
    pub(crate) unit: Unit,
    pub(crate) nodes: Vec<Node>,
    pub(crate) edges: Vec<Edge>,
    pub(crate) succ: Vec<SmallVec<[u32; 2]>>,
    pub(crate) pred: Vec<SmallVec<[u32; 2]>>,
    pub(crate) blocks: Vec<Block>,
    pub(crate) entry: NodeIx,
    pub(crate) exits: [Option<NodeIx>; 4],
    pub(crate) stmt_nodes: FxHashMap<NodeId, SmallVec<[NodeIx; 1]>>,
    pub(crate) stmt_block: FxHashMap<NodeId, BlockId>,
}
