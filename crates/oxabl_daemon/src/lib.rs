//! The oxabl session core: one salsa instance per workspace root, serving several
//! clients at once.
//!
//! # Why this crate exists
//!
//! Two clients on one workspace used to mean two indexes. An editor's language
//! client held a salsa instance and its memoized facts; a second client on the same
//! tree would parse all of it again and hold all of it again. The purpose here is
//! resource sharing — one cache instead of one per client — not a value layer. The
//! daemon gates nothing: it withholds nothing from the editor that it serves to any
//! other client.
//!
//! # Where it sits
//!
//! Above the shared run, and it is the **only** crate that may know about salsa.
//! `oxabl_index` and `oxabl_pipeline` are forbidden a salsa edge, because the
//! umbrella re-exports the pipeline unconditionally and the browser bundle builds
//! through the umbrella. So incremental recomputation and cancellation live here,
//! and every question they answer is still the shared pipeline's.
//!
//! # What lives here
//!
//! - [`db`] — the salsa substrate: one input per open buffer, one per workspace file
//!   a lookup has reached, the two-phase expansion and diagnostics queries, and the
//!   `WorkspaceIndex` implementation that memoizes the shared seam per file.
//! - [`session`] — one [`Session`](session::Session) per workspace root, held in a
//!   map, plus the four disciplines that decide whether a completed background
//!   computation is worth publishing. Those disciplines are the load-bearing part:
//!   two copies of them would drift, and drifting means publishing a stale answer.
//! - [`dispatch`] — one table for every method served, so an LSP method and an
//!   `oxabl/*` method register the same way and share one panic-containment wrapper.
//! - [`server`] — the message loop, transport-agnostic so a second transport is a
//!   second caller rather than a second loop.
//!
//! # Cancellation is not failure
//!
//! `salsa::Cancelled` travels as a panic payload in this workspace. A guard around
//! a query would turn a cancelled recompute into a reported error and freeze a
//! client on stale results, so the queries carry their own
//! `salsa::Cancelled::catch` and the panic guards sit strictly outside it. A
//! cancelled request re-arms; a genuine panic fails one request and is never
//! retried.

pub mod db;
pub mod dispatch;
pub mod handshake;
#[cfg(unix)]
pub mod listener;
pub mod methods;
pub mod registry;
pub mod server;
pub mod session;

pub use dispatch::{ClientContext, Dispatch, MethodError, MethodResult};
pub use handshake::{default_dispatch, register_handshake};
#[cfg(unix)]
pub use listener::{Listener, Stopper, connection_over};
pub use methods::register_methods;
pub use registry::{Discovery, discover, register, socket_path_for, unregister};
pub use server::{serve, serve_stdio, serve_with_first};
pub use session::{
    Analysis, CompletedWork, Disposition, Session, SessionHost, Sessions, analyze_guarded, dispose,
};

/// This build's version, reported at the handshake so a human reading a contract
/// mismatch can see which side is stale.
pub const DAEMON_VERSION: &str = env!("CARGO_PKG_VERSION");
