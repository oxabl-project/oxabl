//! Output directories: layout, ownership, stages, promotion and pruning.

pub mod check;
pub mod layout;
pub mod ownership;
pub mod promote;
pub mod prune;
pub mod stage;

pub use check::{OutputState, check};
pub use layout::{Expected, OutputLayout};
pub use stage::Stage;
