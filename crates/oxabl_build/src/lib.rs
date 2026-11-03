//! The oxabl build tool: the persisted graph store, the incremental refresh and
//! the planner behind `oxabl build`.
//!
//! [`plan::plan`] refreshes the dependency graph of a workspace from its files,
//! reading only what changed, and says which compile units a change reaches.
//! [`store`] holds the graph in SQLite, [`refresh`] brings it up to date,
//! [`graph`] is its in-memory form, and [`config`] lowers `oxabl.toml`.
//!
//! [`worker`] runs the AVM worker pool and speaks the worker protocol.
//! [`key`] holds the compile environment and its identity, and the compile key
//! of each unit with the rules that withhold it; [`paths`] holds the canonical
//! paths, the unit PROPATH and the compile name.

pub mod cache;
pub mod config;
pub mod encode;
pub mod error;
pub mod events;
pub mod graph;
pub mod key;
pub mod kinds;
pub mod lock;
pub mod output;
pub mod paths;
pub mod plan;
pub mod refresh;
pub mod run;
pub mod schedule;
pub mod store;
pub mod worker;
