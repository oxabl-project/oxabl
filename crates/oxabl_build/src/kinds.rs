//! The codes the store keeps for edges, observations and degraded analyses.

/// What an edge of the graph means. The numbers are stored.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum EdgeKind {
    DirectInclude = 1,
    TransitiveInclude = 2,
    Class = 3,
    Program = 4,
    SharedProducer = 5,
    ConditionalInclude = 6,
    ConditionalClass = 7,
    ConditionalProgram = 8,
}

impl EdgeKind {
    pub fn from_code(code: u8) -> Option<Self> {
        Some(match code {
            1 => EdgeKind::DirectInclude,
            2 => EdgeKind::TransitiveInclude,
            3 => EdgeKind::Class,
            4 => EdgeKind::Program,
            5 => EdgeKind::SharedProducer,
            6 => EdgeKind::ConditionalInclude,
            7 => EdgeKind::ConditionalClass,
            8 => EdgeKind::ConditionalProgram,
            _ => return None,
        })
    }

    pub fn code(self) -> u8 {
        self as u8
    }

    /// Whether the compiler reads the target when it compiles the source: the
    /// edges that make up a closure.
    pub fn is_compile_time(self) -> bool {
        matches!(
            self,
            EdgeKind::DirectInclude
                | EdgeKind::TransitiveInclude
                | EdgeKind::Class
                | EdgeKind::ConditionalInclude
                | EdgeKind::ConditionalClass
        )
    }

    /// Whether the target is a class source, which is analysed as a node of its
    /// own.
    pub fn is_class(self) -> bool {
        matches!(self, EdgeKind::Class | EdgeKind::ConditionalClass)
    }

    /// Whether the stored edge carries the class name that reached the target.
    pub fn carries_name(self) -> bool {
        self.is_class()
    }
}

/// What an analysis observed about a path. The numbers are stored.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum ObservationKind {
    Read = 1,
    Absent = 2,
    Listed = 3,
}

impl ObservationKind {
    pub fn from_code(code: u8) -> Option<Self> {
        Some(match code {
            1 => ObservationKind::Read,
            2 => ObservationKind::Absent,
            3 => ObservationKind::Listed,
            _ => return None,
        })
    }

    pub fn code(self) -> u8 {
        self as u8
    }
}

/// Why a reference did not resolve. The numbers are stored.
pub mod unresolved_reason {
    pub const ABSENT_FROM_WORKSPACE: u8 = 1;
    pub const PRESENT_BUT_UNUSABLE: u8 = 2;
    pub const UNKNOWABLE: u8 = 3;
    pub const EXTERNAL: u8 = 4;
    pub const COMPUTED_NAME: u8 = 5;
}

/// Why an analysis may be missing edges. The numbers are stored.
pub mod degraded_reason {
    pub const PARSE_RECOVERED: u8 = 1;
    pub const UNRESOLVED_INCLUDE: u8 = 2;
    pub const UNREADABLE_INCLUDE: u8 = 3;
    pub const PREPROCESSOR_LOSS: u8 = 4;
    pub const UNMODELLED_STATEMENT: u8 = 5;
    pub const ANALYSIS_FAILED: u8 = 6;
}
