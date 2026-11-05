//! The oxabl build tool: the persisted graph store, the incremental refresh and
//! the planner behind `oxabl build`.
//!
//! [`plan::plan`] refreshes the dependency graph of a workspace from its files,
//! reading only what changed, and says which compile units a change reaches.
//! [`store`] holds the graph in SQLite, [`refresh`] brings it up to date,
//! [`graph`] is its in-memory form, and [`config`] lowers `oxabl.toml`.

pub mod config;
pub mod encode;
pub mod error;
pub mod graph;
pub mod kinds;
pub mod lock;
pub mod paths;
pub mod plan;
pub mod refresh;
pub mod store;
