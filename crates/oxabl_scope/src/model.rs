//! The data model: buffers and their scopes, transaction blocks, record reads.

use oxabl_ast::{NodeId, Span};
use oxabl_flow::{BlockId, BlockKind, UnitId, UnitKind};

/// A buffer of one file, an index into [`FileScopes::buffers`](crate::FileScopes::buffers).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BufferId(pub(crate) u32);

impl BufferId {
    /// The raw index.
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

/// How a buffer comes to exist.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BufferKind {
    /// The default buffer of a database table or temp-table.
    Default,
    /// `DEFINE BUFFER b FOR t`.
    Defined,
    /// `DEFINE NEW SHARED BUFFER` or `NEW GLOBAL SHARED`: scoped to the main
    /// block of the defining procedure.
    NewShared,
    /// `DEFINE SHARED BUFFER`: the producer's buffer.
    Shared,
    /// `DEFINE PARAMETER BUFFER`: the caller's buffer.
    Parameter,
    /// A buffer of a database trigger (`TRIGGER PROCEDURE FOR WRITE OF t NEW
    /// BUFFER n`, `ON WRITE OF t ...`), the table's own buffer included.
    Trigger,
}

impl BufferKind {
    /// Lowercase name used in dumps.
    pub fn as_str(self) -> &'static str {
        match self {
            BufferKind::Default => "default",
            BufferKind::Defined => "defined",
            BufferKind::NewShared => "new-shared",
            BufferKind::Shared => "shared",
            BufferKind::Parameter => "parameter",
            BufferKind::Trigger => "trigger",
        }
    }
}

/// The table behind a buffer.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Table {
    /// A table of the loaded schema.
    Database(String),
    /// A temp-table of the file.
    TempTable { name: String, no_undo: bool },
    /// Neither: the schema is not loaded, or does not have the table. Taken
    /// to be a database table when no schema is loaded.
    Unknown(String),
}

impl Table {
    /// The table name, as written at its definition.
    pub fn name(&self) -> &str {
        match self {
            Table::Database(n) | Table::Unknown(n) => n,
            Table::TempTable { name, .. } => name,
        }
    }

    /// Whether the table is a temp-table.
    pub fn is_temp_table(&self) -> bool {
        matches!(self, Table::TempTable { .. })
    }
}

/// How a scope comes about.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeKind {
    /// `DO FOR b` or `REPEAT FOR b`.
    Strong,
    /// A record phrase of a `FOR` block (or a `PRESELECT` phrase).
    Weak,
    /// Free references, scoped to the nearest block with record scoping.
    Free,
    /// Free references whose scope encloses another scope of the buffer
    /// (absorbed), or that come from another unit than the buffer's owner.
    Raised,
}

impl ScopeKind {
    /// Lowercase name used in dumps.
    pub fn as_str(self) -> &'static str {
        match self {
            ScopeKind::Strong => "strong",
            ScopeKind::Weak => "weak",
            ScopeKind::Free => "free",
            ScopeKind::Raised => "raised",
        }
    }
}

/// One scope of a buffer: the block whose lifetime is the record's.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    pub unit: UnitId,
    pub block: BlockId,
    pub kind: ScopeKind,
    /// The reference that holds the scope at this block: the `DO FOR` or
    /// `FOR` phrase, else the first free reference whose own scope is this
    /// block. For a raised scope, the reference that raised it.
    pub at: Span,
    /// The other scopes it absorbed (raised weak scopes, deeper free scopes).
    pub absorbed: Vec<Span>,
    /// References that belong to the scope.
    pub references: u32,
}

/// Why an answer is Unknown. The model never guesses (the plan's opaque rule).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnknownReason {
    /// Two scopes of the buffer conflict (a compile error as far as known):
    /// the reading of the references is wrong somewhere.
    Conflict,
    /// An unqualified field matches two buffers.
    AmbiguousField,
    /// An include that was not expanded may reference anything.
    UnexpandedInclude,
    /// A statement the parser skips, of a form the model does not know,
    /// names a database buffer.
    SkippedStatement,
    /// A dynamic buffer method may update a record the compiler cannot see.
    DynamicBuffer,
    /// No schema is loaded, so an unqualified name may be a field.
    NoSchema,
    /// The schema does not have the table, so its fields are not known.
    UnknownTable,
    /// A `GET` without a lock phrase takes the lock its query was opened
    /// with, and the file opens it nowhere before.
    QueryLock,
    /// A call the file does not show gets the buffer (a `BUFFER b`
    /// argument, or a shared buffer) and may change its record.
    Callee,
    /// A write to the buffer in another routine of the scope: whether it runs
    /// after the read depends on when the routine is called.
    UnitOrder,
    /// The parser recovered from an error, so statements may be missing.
    RecoveredParse,
    /// The file has more references than the work budget allows.
    TooComplex,
}

impl UnknownReason {
    /// Kebab-case name used in dumps and reports.
    pub fn as_str(self) -> &'static str {
        match self {
            UnknownReason::Conflict => "conflict",
            UnknownReason::AmbiguousField => "ambiguous-field",
            UnknownReason::UnexpandedInclude => "unexpanded-include",
            UnknownReason::SkippedStatement => "skipped-statement",
            UnknownReason::DynamicBuffer => "dynamic-buffer",
            UnknownReason::NoSchema => "no-schema",
            UnknownReason::UnknownTable => "unknown-table",
            UnknownReason::QueryLock => "query-lock",
            UnknownReason::Callee => "callee",
            UnknownReason::UnitOrder => "unit-order",
            UnknownReason::RecoveredParse => "recovered-parse",
            UnknownReason::TooComplex => "too-complex",
        }
    }
}

/// An Unknown reason with the place that caused it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Unknown {
    pub reason: UnknownReason,
    pub at: Span,
}

/// A buffer and its scopes.
#[derive(Debug, Clone)]
pub struct Buffer {
    pub id: BufferId,
    /// The name as defined (a default buffer: as first written).
    pub name: String,
    pub kind: BufferKind,
    pub table: Table,
    /// The unit that owns the buffer: the defining unit, or the main block
    /// for a default buffer of a database table.
    pub owner: UnitId,
    /// The definition, if the file has one.
    pub defined: Option<Span>,
    /// The scopes the references give, in source order. The answer only when
    /// [`unknown`](Self::unknown) is empty; see [`Buffer::scopes`].
    pub seen: Vec<Scope>,
    /// Why the scopes are not known. Empty when they are.
    pub unknown: Vec<Unknown>,
    /// References to the buffer, all kinds.
    pub references: u32,
}

impl Buffer {
    /// The scopes, or `None` when they are Unknown or belong to another
    /// procedure ([`Buffer::is_external`]).
    pub fn scopes(&self) -> Option<&[Scope]> {
        (self.unknown.is_empty() && !self.is_external()).then_some(self.seen.as_slice())
    }

    /// The record and its scope belong to the caller or producer: a buffer
    /// parameter, a trigger buffer or a `SHARED` buffer.
    pub fn is_external(&self) -> bool {
        matches!(
            self.kind,
            BufferKind::Shared | BufferKind::Parameter | BufferKind::Trigger
        )
    }
}

/// What makes a block a transaction block implicitly.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UpdateKind {
    Create,
    Delete,
    /// An assignment to a field.
    Assign,
    /// `BUFFER-COPY ... TO b`.
    BufferCopy,
    /// `UPDATE`, `SET`, `INSERT` or `IMPORT` naming a field or buffer.
    Input,
    /// `FIND`, `FOR`, `GET` with `EXCLUSIVE-LOCK`.
    ExclusiveRead,
}

impl UpdateKind {
    /// Lowercase name used in dumps.
    pub fn as_str(self) -> &'static str {
        match self {
            UpdateKind::Create => "create",
            UpdateKind::Delete => "delete",
            UpdateKind::Assign => "assign",
            UpdateKind::BufferCopy => "buffer-copy",
            UpdateKind::Input => "input",
            UpdateKind::ExclusiveRead => "exclusive-lock",
        }
    }
}

/// A database update that a block holds directly.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Update {
    pub kind: UpdateKind,
    pub at: Span,
    pub buffer: Option<BufferId>,
}

/// A database update with the unit and block whose statement holds it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockUpdate {
    pub unit: UnitId,
    pub block: BlockId,
    pub update: Update,
}

/// Something that may update the database, with the unit and block whose
/// statement holds it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockUnknown {
    pub unit: UnitId,
    pub block: BlockId,
    pub unknown: Unknown,
}

/// Whether a block is a transaction block, and why.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TxKind {
    /// Not a transaction block.
    None,
    /// The `TRANSACTION` keyword.
    Explicit,
    /// The first database update or `EXCLUSIVE-LOCK` read the block holds
    /// directly.
    Implicit(Update),
    /// Not a transaction block as far as the model sees, but something it
    /// cannot see might make it one.
    Unknown(Unknown),
}

impl TxKind {
    /// Whether the block is a transaction block (explicit or implicit).
    pub fn is_transaction(&self) -> bool {
        matches!(self, TxKind::Explicit | TxKind::Implicit(_))
    }
}

/// The transaction facts of one block.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Transaction {
    pub kind: TxKind,
    /// The closest enclosing transaction block of the same unit: this block
    /// is then a sub-transaction when it is a transaction block.
    pub within: Option<BlockId>,
}

/// One ABL block with its scopes and transaction facts.
#[derive(Debug, Clone)]
pub struct BlockScopes {
    pub id: BlockId,
    pub kind: BlockKind,
    pub stmt: NodeId,
    pub span: Span,
    pub parent: Option<BlockId>,
    pub label: Option<String>,
    /// The block may be a transaction block: a routine, `FOR`, `REPEAT`, or
    /// a `DO` with `TRANSACTION` or an `ON ERROR`/`ON ENDKEY` phrase.
    pub transaction_capable: bool,
    pub transaction: Transaction,
    /// The buffers scoped to this block (known scopes only), in id order.
    pub buffers: Vec<BufferId>,
}

/// One unit with its blocks.
#[derive(Debug, Clone)]
pub struct UnitScopes {
    pub unit: UnitId,
    pub kind: UnitKind,
    pub name: Option<String>,
    pub span: Span,
    pub parent: Option<UnitId>,
    /// Every block of the unit, indexed by [`BlockId`].
    pub blocks: Vec<BlockScopes>,
}

impl UnitScopes {
    /// One block.
    pub fn block(&self, id: BlockId) -> &BlockScopes {
        &self.blocks[id.index()]
    }
}

/// A lock mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LockMode {
    NoLock,
    ShareLock,
    ExclusiveLock,
}

impl LockMode {
    /// Lowercase name used in dumps.
    pub fn as_str(self) -> &'static str {
        match self {
            LockMode::NoLock => "no-lock",
            LockMode::ShareLock => "share-lock",
            LockMode::ExclusiveLock => "exclusive-lock",
        }
    }
}

/// The lock a record read takes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Lock {
    /// Written in the statement.
    Explicit(LockMode),
    /// No lock phrase: `SHARE-LOCK`.
    Default,
    /// A temp-table record: no lock.
    TempTable,
    /// The phrase cannot be read.
    Unknown(UnknownReason),
}

impl Lock {
    /// The lock the read takes, when known; `None` for a temp-table.
    pub fn effective(&self) -> Option<LockMode> {
        match self {
            Lock::Explicit(m) => Some(*m),
            Lock::Default => Some(LockMode::ShareLock),
            Lock::TempTable | Lock::Unknown(_) => None,
        }
    }
}

/// The statement form of a record read.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReadKind {
    Find,
    /// A record phrase of a `FOR` block.
    For,
    /// A `PRESELECT` phrase of a `DO` or `REPEAT` header.
    Preselect,
    CanFind,
    /// A record phrase of `OPEN QUERY`.
    OpenQuery,
    /// `GET`, for each buffer of the query. Without a lock phrase it takes
    /// the lock of the `OPEN QUERY` before it.
    Get,
}

impl ReadKind {
    /// Lowercase name used in dumps.
    pub fn as_str(self) -> &'static str {
        match self {
            ReadKind::Find => "find",
            ReadKind::For => "for",
            ReadKind::Preselect => "preselect",
            ReadKind::CanFind => "can-find",
            ReadKind::OpenQuery => "open-query",
            ReadKind::Get => "get",
        }
    }
}

/// One record read and its lock.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordRead {
    pub unit: UnitId,
    /// The block the statement is in (for a `FOR` phrase, the `FOR` block).
    pub block: BlockId,
    pub stmt: NodeId,
    pub kind: ReadKind,
    /// The buffer name as written.
    pub name: String,
    /// The buffer read into; `None` when it does not resolve. A `CAN-FIND`
    /// names the table, and reads with a buffer of its own.
    pub buffer: Option<BufferId>,
    /// The table read: the buffer's, or for a `CAN-FIND` the table its name
    /// resolves to.
    pub table: Table,
    pub at: Span,
    pub lock: Lock,
    /// Whether the record may be changed after the read in the same scope:
    /// later in the text, or anywhere in an iterating block of the scope that
    /// holds the read. A `SHARE-LOCK` is then upgraded, and `NO-LOCK` is no
    /// safe fix.
    pub written: Written,
}

/// Whether a write to a record read's buffer may run after the read, in its
/// scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Written {
    No,
    Yes,
    /// A call may write it ([`UnknownReason::Callee`]), a statement the model
    /// does not read may ([`UnknownReason::SkippedStatement`]), or another
    /// routine of the scope writes it ([`UnknownReason::UnitOrder`]).
    Unknown(UnknownReason),
}
