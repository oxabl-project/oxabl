//! The lifetime model: objects, their sites, leaks and verdicts.

use oxabl_ast::{NodeId, Span};
use oxabl_flow::{EdgeKind, ExitKind, UnitId, UnitKind};

/// What a create site makes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum ObjectKind {
    Query,
    Buffer,
    TempTable,
    Dataset,
    DataSource,
    Call,
    XDocument,
    XNodeRef,
    SaxReader,
    SaxWriter,
    SaxAttributes,
    Socket,
    ServerSocket,
    Server,
    SoapHeader,
    SoapHeaderEntryRef,
    ClientPrincipal,
    /// Any widget: `CREATE BUTTON`, `FRAME`, `WINDOW`, ...
    Widget,
    /// `CREATE VALUE(expr) h`: the type is computed at run time.
    Dynamic,
    /// A COM object: `CREATE "Excel.Application" ch`.
    Automation,
    /// `RUN p PERSISTENT [SET h]`.
    PersistentProcedure,
    /// `SET-SIZE(m) = n` with `n` not a literal 0.
    Memptr,
    /// `NEW` or `DYNAMIC-NEW`: reference-counted and garbage-collected.
    ClassInstance,
}

impl ObjectKind {
    /// Kebab-case name used in dumps and reports.
    pub fn as_str(self) -> &'static str {
        match self {
            ObjectKind::Query => "query",
            ObjectKind::Buffer => "buffer",
            ObjectKind::TempTable => "temp-table",
            ObjectKind::Dataset => "dataset",
            ObjectKind::DataSource => "data-source",
            ObjectKind::Call => "call",
            ObjectKind::XDocument => "x-document",
            ObjectKind::XNodeRef => "x-noderef",
            ObjectKind::SaxReader => "sax-reader",
            ObjectKind::SaxWriter => "sax-writer",
            ObjectKind::SaxAttributes => "sax-attributes",
            ObjectKind::Socket => "socket",
            ObjectKind::ServerSocket => "server-socket",
            ObjectKind::Server => "server",
            ObjectKind::SoapHeader => "soap-header",
            ObjectKind::SoapHeaderEntryRef => "soap-header-entryref",
            ObjectKind::ClientPrincipal => "client-principal",
            ObjectKind::Widget => "widget",
            ObjectKind::Dynamic => "dynamic",
            ObjectKind::Automation => "automation",
            ObjectKind::PersistentProcedure => "persistent-procedure",
            ObjectKind::Memptr => "memptr",
            ObjectKind::ClassInstance => "class-instance",
        }
    }

    /// Whether the object goes into a widget pool when it is created: every
    /// handle-based object and widget. Not procedures, MEMPTRs, COM objects,
    /// class instances, server and socket objects, or a `CREATE VALUE` whose
    /// type is not known.
    pub fn uses_widget_pool(self) -> bool {
        !matches!(
            self,
            ObjectKind::Automation
                | ObjectKind::Dynamic
                | ObjectKind::Server
                | ObjectKind::Socket
                | ObjectKind::ServerSocket
                | ObjectKind::PersistentProcedure
                | ObjectKind::Memptr
                | ObjectKind::ClassInstance
        )
    }

    /// Whether the AVM frees the object when nothing refers to it any more.
    pub fn is_collected(self) -> bool {
        self == ObjectKind::ClassInstance
    }
}

/// A statement of the analysed text. `span` is in the text the statements
/// were parsed from (the expanded text when includes were expanded).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Site {
    pub stmt: NodeId,
    pub span: Span,
}

/// How a statement frees an object.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum FreeKind {
    DeleteObject,
    DeleteProcedure,
    DeleteWidget,
    /// `RELEASE OBJECT` of a COM object.
    ReleaseObject,
    /// `SET-SIZE(m) = 0`.
    SetSizeZero,
    /// `APPLY "CLOSE" TO hProc`: by convention a persistent procedure deletes
    /// itself on CLOSE. Only a persistent procedure is freed this way.
    ApplyClose,
    /// `DELETE WIDGET-POOL` of the pool the object is in.
    DeletePool,
}

impl FreeKind {
    pub fn as_str(self) -> &'static str {
        match self {
            FreeKind::DeleteObject => "delete-object",
            FreeKind::DeleteProcedure => "delete-procedure",
            FreeKind::DeleteWidget => "delete-widget",
            FreeKind::ReleaseObject => "release-object",
            FreeKind::SetSizeZero => "set-size-0",
            FreeKind::ApplyClose => "apply-close",
            FreeKind::DeletePool => "delete-widget-pool",
        }
    }
}

/// A statement that frees the object on some path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Free {
    pub kind: FreeKind,
    pub site: Site,
    /// The statement is in a `FINALLY` block, so it runs on every exit of the
    /// `FINALLY`'s block.
    pub in_finally: bool,
}

/// Where an object goes when its owner is not this unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum EscapeKind {
    /// Assigned to an `OUTPUT`, `INPUT-OUTPUT` or `RETURN` parameter.
    OutputParameter,
    /// `RETURN h` from a function, method or procedure.
    Return,
    /// Stored in a data member or property of the class.
    Member,
    /// Stored in a variable of an enclosing unit (a main-block variable used
    /// from an internal procedure, function or trigger).
    OuterVariable,
    /// Stored in a `SHARED` or `GLOBAL` variable.
    Shared,
    /// Stored in a name that no definition of the file declares.
    Unresolved,
    /// Stored in a temp-table or database field.
    Field,
    /// Stored in an array element or an `EXTENT` variable.
    ArrayElement,
    /// Stored in an attribute or property of another object.
    Attribute,
    /// `ADD-SUPER-PROCEDURE(h)`.
    SuperProcedure,
    /// Passed to a constructor (`NEW C(h)`), which may keep it.
    ConstructorArgument,
    /// Passed `INPUT-OUTPUT` to a `RUN`.
    InputOutputArgument,
    /// Passed to the subscribers of a `PUBLISH`.
    Published,
    /// Registered as a subscriber (`SUBSCRIBE PROCEDURE h`).
    Subscriber,
    /// Added to a dataset this unit creates: `hDs:ADD-BUFFER(h)` or
    /// `hDs:ADD-BUFFER(h:DEFAULT-BUFFER-HANDLE)`. The dataset is taken as the
    /// owner.
    DatasetMember,
    /// In a named widget pool that this file does not create.
    ForeignPool,
    /// In a `PERSISTENT` named pool that this file deletes somewhere.
    PersistentPool,
    /// Held at the end of the main block by a variable that another unit of
    /// the file frees.
    FreedByOtherUnit,
    /// A widget put under a parent widget (`ASSIGN FRAME = f`, `h:PARENT =
    /// w`): deleting the parent deletes it.
    WidgetParent,
    /// A handle derived from the object (`h:DEFAULT-BUFFER-HANDLE`,
    /// `h:GET-BUFFER-HANDLE(n)`) is returned or stored where it outlives the
    /// unit; the root object is taken as handed over too. Low confidence.
    DerivedHandle,
    /// Passed `INPUT` to a procedure, function or method of this file that
    /// frees its parameter.
    FreedByCallee,
    /// Passed `INPUT` to a procedure, function or method of this file that
    /// keeps its parameter: stores it where it outlives the call, returns it,
    /// or hands it on in one of the other ways listed here.
    KeptByCallee,
}

impl EscapeKind {
    pub fn as_str(self) -> &'static str {
        match self {
            EscapeKind::OutputParameter => "output-parameter",
            EscapeKind::Return => "return",
            EscapeKind::Member => "member",
            EscapeKind::OuterVariable => "outer-variable",
            EscapeKind::Shared => "shared",
            EscapeKind::Unresolved => "unresolved",
            EscapeKind::Field => "field",
            EscapeKind::ArrayElement => "array-element",
            EscapeKind::Attribute => "attribute",
            EscapeKind::SuperProcedure => "super-procedure",
            EscapeKind::ConstructorArgument => "constructor-argument",
            EscapeKind::InputOutputArgument => "input-output-argument",
            EscapeKind::Published => "published",
            EscapeKind::Subscriber => "subscriber",
            EscapeKind::DatasetMember => "dataset-member",
            EscapeKind::ForeignPool => "foreign-pool",
            EscapeKind::PersistentPool => "persistent-pool",
            EscapeKind::FreedByOtherUnit => "freed-by-other-unit",
            EscapeKind::WidgetParent => "widget-parent",
            EscapeKind::DerivedHandle => "derived-handle",
            EscapeKind::FreedByCallee => "freed-by-callee",
            EscapeKind::KeptByCallee => "kept-by-callee",
        }
    }
}

/// A statement where the object leaves this unit's ownership on some path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Escape {
    pub kind: EscapeKind,
    pub site: Site,
    /// For a variable that outlives the unit (member, outer, shared): some
    /// other unit of the file frees a variable of that name. A class-level
    /// check reads this: a member no unit of the class frees is a leak of the
    /// class.
    pub freed_elsewhere: bool,
}

/// Why part of an object's lifetime is not modelled.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum OpaqueReason {
    /// A statement the parser skips names a variable that holds the object.
    SkippedStatement,
    /// An include reference that was not expanded runs while the object lives.
    UnexpandedInclude,
    /// A statement the parser left empty (often parse recovery) runs while the
    /// object lives.
    UnrecognizedStatement,
    /// A `CATCH` or `FINALLY` outside a block body runs while the object lives.
    StrayHandlerBlock,
    /// The object is an argument of a dynamic call: `RUN VALUE(...)`,
    /// `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`, `DYNAMIC-NEW`.
    DynamicCall,
    /// No path from the create reaches an exit or a free (dead code or an
    /// endless loop).
    NoExitPath,
    /// The walk of the object's paths hit its size limit.
    TooComplex,
    /// A persistent procedure lives while the unit deletes procedures found by
    /// walking a procedure list (`FIRST-PROCEDURE`, `NEXT-SIBLING`), which may
    /// include it.
    ProcedureSweep,
    /// `RUN destroy... IN h` (a name starting with `destroy`, or ADM's
    /// `RUN dispatch IN h ('destroy')`): by a common convention the
    /// procedure deletes itself there, but nothing in this file shows it.
    DestroyCall,
    /// The object is a MEMPTR passed as an `OUTPUT` or `INPUT-OUTPUT`
    /// argument: whether that replaces the pointer or fills the block is not
    /// known.
    OutputArgument,
    /// The object is an `INPUT` argument of a call the file does not define
    /// (`RUN x.p`, `RUN p IN h`, a function or method of another file): the
    /// callee may delete or keep it.
    Callee,
    /// A `DELETE` or `RELEASE` operand that names a holder but is not the
    /// holder itself (`DELETE OBJECT WIDGET-HANDLE(STRING(h))`).
    UnmappedFree,
    /// The object is added with `ADD-BUFFER` to an object this unit does not
    /// create: a dataset would own it, a query would not.
    AddBuffer,
}

impl OpaqueReason {
    pub fn as_str(self) -> &'static str {
        match self {
            OpaqueReason::SkippedStatement => "skipped-statement",
            OpaqueReason::UnexpandedInclude => "unexpanded-include",
            OpaqueReason::UnrecognizedStatement => "unrecognized-statement",
            OpaqueReason::StrayHandlerBlock => "stray-handler-block",
            OpaqueReason::DynamicCall => "dynamic-call",
            OpaqueReason::NoExitPath => "no-exit-path",
            OpaqueReason::TooComplex => "too-complex",
            OpaqueReason::ProcedureSweep => "procedure-sweep",
            OpaqueReason::DestroyCall => "destroy-call",
            OpaqueReason::OutputArgument => "output-argument",
            OpaqueReason::Callee => "callee",
            OpaqueReason::UnmappedFree => "unmapped-free",
            OpaqueReason::AddBuffer => "add-buffer",
        }
    }
}

/// An opaque statement on a path where the object lives.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Opaque {
    pub reason: OpaqueReason,
    pub site: Site,
}

/// Where a leaking path loses the object.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LeakKind {
    /// The unit ends, by this exit, with the object live and owned here.
    Exit(ExitKind),
    /// The last variable holding the object is assigned something else (a new
    /// `CREATE` into it, `h = ?`, ...). In a loop this is a create whose object
    /// from the previous iteration is still live.
    Overwrite,
}

/// One step of a leaking path: a node and the edge that leaves it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Step {
    pub span: Span,
    pub edge: EdgeKind,
}

/// A path on which the object is lost.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Leak {
    pub kind: LeakKind,
    /// The overwriting statement, or the last statement before the exit.
    pub at: Span,
    /// The path raises no condition: no ERROR, ENDKEY, STOP or QUIT, no
    /// `THROW` and no `RETURN ERROR`. A leak only on other paths is the
    /// "delete on the success path only" case.
    pub normal: bool,
    /// The first transfer that raises a condition, for a path that is not
    /// normal: its edge kind and the node it leaves.
    pub via: Option<Step>,
    /// The first transfer that raises out of a `RUN`, a call, a method or
    /// attribute, or a database statement, on some path that loses the object
    /// here. `via` may be a plainer statement on another path.
    pub via_call: Option<Step>,
    /// From the create to the loss, without the synthetic entry, end and exit
    /// nodes.
    pub path: Vec<Step>,
}

/// The widget pool an object is created in.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Placement {
    /// The kind never goes in a widget pool.
    NotPooled,
    /// The unnamed pool an earlier `CREATE WIDGET-POOL` of this unit made; it
    /// is deleted when this unit ends.
    UnitPool,
    /// The unnamed pool the file's main block creates; it is deleted when the
    /// procedure (or, for a persistent procedure, its instance) ends.
    FilePool,
    /// The pool of a class with `USE-WIDGET-POOL`; it is deleted with the
    /// instance.
    ClassPool,
    /// A named pool this file creates, by its name as written.
    NamedPool { name: String, persistent: bool },
    /// A named pool this file does not create.
    ForeignPool { name: String },
    /// No pool of this file: the caller's most recent unnamed pool, else the
    /// session pool. Not knowable from this file.
    Unscoped,
}

impl Placement {
    /// The pool is deleted when this procedure ends, so it frees what is left
    /// in it.
    pub fn freed_with_procedure(&self) -> bool {
        matches!(
            self,
            Placement::UnitPool
                | Placement::FilePool
                | Placement::ClassPool
                | Placement::NamedPool {
                    persistent: false,
                    ..
                }
        )
    }
}

/// The lifetime verdict of one object.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Verdict {
    /// Freed on every path that leaves the unit.
    Freed,
    /// Every path frees it, or ends with it in a widget pool this procedure
    /// deletes.
    PoolFreed,
    /// Every path frees it or hands it to an owner outside the unit.
    Escapes,
    /// Some path loses it: see [`Object::leaks`].
    Leaks,
    /// Some path would lose it, but an opaque construct may free or hand it
    /// on: see [`Object::opaque`]. Never report as a leak.
    Unknown,
    /// A class instance: reference-counted, not a lifetime concern.
    Collected,
    /// `RUN p PERSISTENT` without `SET`: no handle is kept here. Usually a
    /// singleton or a self-managing super procedure.
    Uncaptured,
}

impl Verdict {
    pub fn as_str(self) -> &'static str {
        match self {
            Verdict::Freed => "freed",
            Verdict::PoolFreed => "pool-freed",
            Verdict::Escapes => "escapes",
            Verdict::Leaks => "leaks",
            Verdict::Unknown => "unknown",
            Verdict::Collected => "collected",
            Verdict::Uncaptured => "uncaptured",
        }
    }
}

/// One dynamic object: a create site and its lifetime in the unit.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Object {
    pub kind: ObjectKind,
    pub create: Site,
    /// The variable the create stores the handle in, as written.
    pub holder: Option<String>,
    pub placement: Placement,
    pub frees: Vec<Free>,
    pub escapes: Vec<Escape>,
    pub leaks: Vec<Leak>,
    pub opaque: Vec<Opaque>,
    pub verdict: Verdict,
}

impl Object {
    /// Some leak happens without a raised condition.
    pub fn leaks_on_normal_path(&self) -> bool {
        self.verdict == Verdict::Leaks && self.leaks.iter().any(|l| l.normal)
    }

    /// The object leaks, but only where a condition is raised: the frees run
    /// on the success path only.
    pub fn leaks_on_abnormal_path_only(&self) -> bool {
        self.verdict == Verdict::Leaks && self.leaks.iter().all(|l| !l.normal)
    }
}

/// A `CREATE WIDGET-POOL` statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PoolSite {
    pub site: Site,
    /// The pool name as written; `None` for an unnamed pool.
    pub name: Option<String>,
    pub persistent: bool,
}

/// The dynamic objects of one unit.
#[derive(Debug, Clone)]
pub struct UnitHandles {
    pub unit: UnitId,
    pub kind: UnitKind,
    pub name: Option<String>,
    pub span: Span,
    /// In source order of their create sites.
    pub objects: Vec<Object>,
    pub pools: Vec<PoolSite>,
}

/// Counts for a procedure summary.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Summary {
    /// Objects created, class instances excluded.
    pub created: usize,
    /// Freed on every path, by a delete or by a pool.
    pub freed: usize,
    pub escaped: usize,
    pub leaked: usize,
    pub unknown: usize,
    /// `RUN PERSISTENT` without `SET`.
    pub uncaptured: usize,
}

impl UnitHandles {
    pub fn summary(&self) -> Summary {
        let mut s = Summary::default();
        for o in &self.objects {
            if o.kind.is_collected() {
                continue;
            }
            s.created += 1;
            match o.verdict {
                Verdict::Freed | Verdict::PoolFreed => s.freed += 1,
                Verdict::Escapes => s.escaped += 1,
                Verdict::Leaks => s.leaked += 1,
                Verdict::Unknown => s.unknown += 1,
                Verdict::Uncaptured => s.uncaptured += 1,
                Verdict::Collected => {}
            }
        }
        s
    }
}
