//! Output directories: layout, ownership, stages, promotion and pruning.

pub mod check;
pub mod ignore;
pub mod in_place;
pub mod layout;
pub mod ownership;
pub mod promote;
pub mod prune;
pub mod stage;

pub use check::{OutputState, check};
pub use layout::{Expected, OutputLayout, Rcode};
pub use stage::Stage;
