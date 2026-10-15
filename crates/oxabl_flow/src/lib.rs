//! Control-flow graphs for Progress ABL, with ABL block semantics.
//!
//! [`build_file`] turns a parsed file into a [`FileFlow`]: one [`Cfg`] per
//! scope-owning unit (the main block, each internal procedure, function,
//! method, constructor, destructor, property accessor and `ON ... DO` trigger).
//! `CATCH` and `FINALLY` are blocks inside their unit's CFG, because control
//! reaches them from the code around them.
//!
//! # Model
//!
//! * **Nodes** are statement-level. A [`NodeKind::Stmt`] node refers to its AST
//!   statement by `NodeId` and carries its span. Blocks add
//!   synthetic nodes: a header ([`NodeKind::Head`]), the end of an iteration
//!   ([`NodeKind::End`]), the point after the block ([`NodeKind::After`]) and
//!   one handler per condition the block handles ([`NodeKind::Handler`]).
//! * **Blocks** ([`Block`]) carry their ABL properties: kind, label, whether
//!   they iterate, `TRANSACTION`, and the effective `ON ERROR` / `ENDKEY` /
//!   `STOP` / `QUIT` handling, with ABL's defaults and the `ROUTINE-LEVEL` and
//!   `BLOCK-LEVEL` directives applied.
//! * **Edges** ([`EdgeKind`]) say why control moves: fallthrough, branches,
//!   loop edges, `LEAVE`/`NEXT`, `RETURN`, `RETURN ERROR`, `THROW`, and a
//!   [`EdgeKind::Raise`] edge from every statement that may raise a condition
//!   to the handler of the block that handles it.
//! * **`FINALLY`** runs on every exit of its block, error exits included. Each
//!   distinct continuation gets its own copy of the `FINALLY` body, so a path
//!   through a copy goes on to the place its jump was going. A statement in a
//!   `FINALLY` therefore has one node per copy ([`Cfg::nodes_of`]).
//! * **Opaque** nodes ([`OpaqueReason`]) mark what is not modelled: statements
//!   the parser skips are "may raise, then fall through"; nothing is guessed.
//!
//! The AST does not keep block-header phrases or the `UNDO`, `QUIT` and `STOP`
//! statements, so the builder reads them from the lexer's tokens over each
//! statement's span. That is why [`build_file`] takes the source text.
//!
//! # Conditions
//!
//! A statement may raise ERROR unless it has `NO-ERROR`, and ENDKEY when it
//! reads input. QUIT comes only from the `QUIT` statement. STOP comes from:
//!
//! * the `STOP` statement;
//! * every statement under `STOP-AFTER`, the header of an iterating block
//!   included. An unhandled one leaves the unit;
//! * `RUN`, a `FIND` or `FOR` that takes a lock (no `NO-LOCK`), and `WAIT-FOR`,
//!   but only where a block or `CATCH` of the unit handles STOP. An unhandled
//!   STOP from these leaves the unit, and that path is not modelled.
//!
//! Not modelled: STOP from a function or method call, a user interrupt, or a
//! lock wait inside `CREATE`, `UPDATE` and the like.
//!
//! # `UNDO`
//!
//! An `UNDO` statement undoes the block it names, else the closest block with
//! the error property. In a `CATCH` that is the block the `CATCH` belongs to.
//! The node records it ([`Node::undoes`]). The action then runs as an `ON`
//! phrase action would. `THROW` raises ERROR in the block that encloses the
//! undone block, so the undone block's own `CATCH` blocks never see it.
//! Without an action, `UNDO` does the undone block's `ON ERROR` action.
//!
//! Where ABL's behaviour is uncertain, the model's choice is written down with
//! a confidence mark in `docs/design/strict-analysis-open-questions.md`, and
//! each has a fixture under `tests/fixtures`.
//!
//! # Queries
//!
//! Graph access, reachability, path search and block exits are methods on
//! [`Cfg`]. [`dominators`] has dominator and post-dominator trees, and
//! [`dataflow`] a worklist solver for forward and backward analyses.
//! [`FileFlow::dump`] renders a stable text form for snapshot tests.
//!
//! A question about one block needs one unit: [`UnitBuilder`] says which unit
//! holds a statement and builds that unit alone.

mod build;
pub mod dataflow;
pub mod dominators;
mod dump;
mod model;
mod query;
mod tokens;

pub use build::{FlowOptions, UnitBuilder, build_file, build_file_tokens, build_file_with};
pub use model::{
    Action, Block, BlockId, BlockKind, Cfg, Condition, Edge, EdgeKind, ErrorDirective, ExitKind,
    ForQualifier, Handling, Node, NodeIx, NodeKind, OpaqueReason, Origin, RetryBehavior,
    ReturnKind, Unit, UnitId, UnitKind,
};

use oxabl_ast::NodeId;

/// The CFGs of one file.
#[derive(Debug, Clone)]
pub struct FileFlow {
    units: Vec<Cfg>,
    directive: Option<ErrorDirective>,
}

impl FileFlow {
    /// Every unit, in source pre-order; the main block is first.
    pub fn units(&self) -> &[Cfg] {
        &self.units
    }

    /// One unit.
    pub fn unit(&self, id: UnitId) -> &Cfg {
        &self.units[id.index()]
    }

    /// The error directive in effect for the file.
    pub fn directive(&self) -> Option<ErrorDirective> {
        self.directive
    }

    /// The units a statement defines (two for a property with both
    /// accessors).
    pub fn units_defined_by(&self, stmt: NodeId) -> impl Iterator<Item = &Cfg> {
        self.units.iter().filter(move |c| c.unit().stmt == stmt)
    }

    /// The unit whose CFG has nodes for `stmt`, with those nodes.
    pub fn find_stmt(&self, stmt: NodeId) -> Option<(&Cfg, &[NodeIx])> {
        self.units.iter().find_map(|c| {
            let nodes = c.nodes_of(stmt);
            (!nodes.is_empty()).then_some((c, nodes))
        })
    }

    /// A stable text rendering of every CFG, for snapshot tests and debugging.
    pub fn dump(&self, source: &str) -> String {
        dump::dump(self, source)
    }
}
