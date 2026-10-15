//! The data model: effects, index choices, units and their calls.

use oxabl_ast::Span;
use oxabl_flow::{BlockId, UnitId, UnitKind};
use oxabl_index::calls::{CallKind, CallTarget};
use oxabl_scope::{Lock, ReadKind, Table};

/// What an effect does to a table.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Op {
    Read,
    Create,
    Update,
    Delete,
}

impl Op {
    /// Lowercase name used in dumps.
    pub fn as_str(self) -> &'static str {
        match self {
            Op::Read => "read",
            Op::Create => "create",
            Op::Update => "update",
            Op::Delete => "delete",
        }
    }

    /// Whether the operation writes the database.
    pub fn writes(self) -> bool {
        self != Op::Read
    }
}

/// How often a statement runs relative to the record loops around it
/// (section 8.3): once, or once per row of `depth` nested record loops of
/// its unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Shape {
    Once,
    /// Per row of `depth` (1 or more) record loops; nested when 2 or more.
    PerRow {
        depth: u8,
    },
}

impl Shape {
    /// A shape from a record-loop depth.
    pub fn from_depth(depth: u8) -> Shape {
        match depth {
            0 => Shape::Once,
            d => Shape::PerRow { depth: d },
        }
    }

    /// The record-loop depth (0 for once).
    pub fn depth(self) -> u8 {
        match self {
            Shape::Once => 0,
            Shape::PerRow { depth } => depth,
        }
    }

    /// Text used in dumps: `once`, `per-row`, `nested(2)`.
    pub fn render(self) -> String {
        match self {
            Shape::Once => "once".to_string(),
            Shape::PerRow { depth: 1 } => "per-row".to_string(),
            Shape::PerRow { depth } => format!("nested({depth})"),
        }
    }
}

/// Why the model picked an index (section 8.2).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IndexReason {
    /// `USE-INDEX` names it.
    UseIndex,
    /// A `CONTAINS` on a field of this word index.
    Contains,
    /// `FIND t constant`: the primary index.
    KeyValue,
    /// Every component of this unique index has an active equality match.
    UniqueEquality,
    /// The most active equality matches on leading components.
    Equality,
    /// No equality match; an active range match on the first component.
    Range,
    /// No match; the most `BY` fields in index order.
    Sort,
    /// No match: the primary index.
    Primary,
}

impl IndexReason {
    /// Kebab-case name used in dumps and reports.
    pub fn as_str(self) -> &'static str {
        match self {
            IndexReason::UseIndex => "use-index",
            IndexReason::Contains => "contains",
            IndexReason::KeyValue => "key-value",
            IndexReason::UniqueEquality => "unique-equality",
            IndexReason::Equality => "equality",
            IndexReason::Range => "range",
            IndexReason::Sort => "sort",
            IndexReason::Primary => "primary",
        }
    }
}

/// How a tie between candidate indexes was broken.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TieBreak {
    /// One candidate only.
    None,
    /// Equal equality counts: an active range match on the next component.
    Range,
    /// Still equal: the most `BY` fields in index order.
    Sort,
    /// Still equal: the primary index is among them.
    Primary,
    /// Still equal: the first by name.
    Alphabetical,
}

impl TieBreak {
    /// Kebab-case name used in dumps.
    pub fn as_str(self) -> &'static str {
        match self {
            TieBreak::None => "none",
            TieBreak::Range => "range",
            TieBreak::Sort => "sort",
            TieBreak::Primary => "primary",
            TieBreak::Alphabetical => "alphabetical",
        }
    }
}

/// The index the model expects the compiler to use for a search.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexChoice {
    /// The index name, as defined.
    pub name: String,
    pub reason: IndexReason,
    pub tie: TieBreak,
    /// Leading components with an active equality match.
    pub equality: u8,
    /// The component after them has an active range match.
    pub range: bool,
    /// No bracket: the search reads the whole index (XREF `WHOLE-INDEX`).
    pub whole: bool,
}

/// Why the model does not name an index.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IndexUnknown {
    /// No schema is loaded.
    NoSchema,
    /// The schema (or the file, for a temp-table) does not define the table.
    UnknownTable,
    /// `USE-INDEX` names an index the table does not have.
    UnknownIndex,
    /// The compiler may combine several indexes (`OR`, or `AND` over full
    /// equality matches of several indexes) in a `FOR`, `PRESELECT` or
    /// `OPEN QUERY`.
    MultiIndex,
    /// `OF` names a table with no unique index the two tables share.
    OfJoin,
    /// Text the preprocessor did not expand is in the phrase.
    Unexpanded,
    /// The phrase could not be read.
    Unreadable,
    /// A `CONTAINS` on a field with no word index.
    NoWordIndex,
}

impl IndexUnknown {
    /// Kebab-case name used in dumps and reports.
    pub fn as_str(self) -> &'static str {
        match self {
            IndexUnknown::NoSchema => "no-schema",
            IndexUnknown::UnknownTable => "unknown-table",
            IndexUnknown::UnknownIndex => "unknown-index",
            IndexUnknown::MultiIndex => "multi-index",
            IndexUnknown::OfJoin => "of-join",
            IndexUnknown::Unexpanded => "unexpanded",
            IndexUnknown::Unreadable => "unreadable",
            IndexUnknown::NoWordIndex => "no-word-index",
        }
    }
}

/// How a search finds its records.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Access {
    Index(IndexChoice),
    /// `TABLE-SCAN`: no index.
    TableScan,
    /// `WHERE ROWID(b) = ...` or `RECID(b) = ...`: no index.
    Rowid,
    Unknown(IndexUnknown),
}

/// One database or temp-table effect of a statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Effect {
    pub unit: UnitId,
    /// The innermost block of the statement (for a `FOR` phrase, the `FOR`
    /// block).
    pub block: BlockId,
    /// The buffer name in the statement, or the field for an assignment.
    pub at: Span,
    /// The statement the effect is in (for a write, the same as `at`).
    pub statement: Span,
    pub table: Table,
    pub op: Op,
    /// The statement form of a read; `None` for a write.
    pub read: Option<ReadKind>,
    /// The lock of a read; `None` for a write.
    pub lock: Option<Lock>,
    pub shape: Shape,
    /// The innermost record loop of the unit the statement runs in once per
    /// row, when [`shape`](Self::shape) is per-row.
    pub per_row_of: Option<BlockId>,
    /// How a search finds its records. `None` for a write, a `GET`, and
    /// `FIND CURRENT`, which do not search.
    pub access: Option<Access>,
}

/// Why a unit's effects may be incomplete.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EffectUnknown {
    /// A dynamic query or buffer reads or writes tables the compiler does not
    /// see (`CREATE QUERY`, `QUERY-PREPARE`, `h:FIND-FIRST(...)`,
    /// `h:BUFFER-CREATE()`).
    Dynamic,
    /// A statement the parser skips names a database buffer.
    SkippedStatement,
    /// A write to a table the schema does not have.
    UnknownTable,
    /// No schema is loaded: a name may be a field.
    NoSchema,
    /// An include was not expanded.
    UnexpandedInclude,
    /// The parse recovered from an error.
    RecoveredParse,
    /// Past the scope analysis's work budget.
    TooComplex,
}

impl EffectUnknown {
    /// Kebab-case name used in dumps and reports.
    pub fn as_str(self) -> &'static str {
        match self {
            EffectUnknown::Dynamic => "dynamic",
            EffectUnknown::SkippedStatement => "skipped-statement",
            EffectUnknown::UnknownTable => "unknown-table",
            EffectUnknown::NoSchema => "no-schema",
            EffectUnknown::UnexpandedInclude => "unexpanded-include",
            EffectUnknown::RecoveredParse => "recovered-parse",
            EffectUnknown::TooComplex => "too-complex",
        }
    }
}

/// A call of one unit, with the shape it runs in.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnitCall {
    pub at: Span,
    pub block: BlockId,
    pub kind: CallKind,
    pub target: CallTarget,
    pub shape: Shape,
}

/// The direct effects of one unit.
#[derive(Debug, Clone)]
pub struct UnitEffects {
    pub unit: UnitId,
    pub kind: UnitKind,
    pub name: Option<String>,
    pub span: Span,
    pub parent: Option<UnitId>,
    /// Indexes into [`FileEffects::effects`](crate::FileEffects::effects), in source order.
    pub effects: Vec<u32>,
    pub calls: Vec<UnitCall>,
    /// Why the effects may be incomplete, with a site; one per reason.
    pub unknown: Vec<(EffectUnknown, Span)>,
}
