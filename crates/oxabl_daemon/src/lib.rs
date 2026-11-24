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
//! - [`session`] — one [`Session`] per workspace root, held in a
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
pub mod idle;
#[cfg(unix)]
pub mod listener;
pub mod methods;
pub mod registry;
pub mod server;
pub mod session;

pub use dispatch::{ClientContext, Dispatch, MethodError, MethodResult};
pub use handshake::{default_dispatch, register_handshake};
pub use idle::{Activity, Busy, DEFAULT_IDLE_TIMEOUT, IDLE_TIMEOUT_VARIABLE, parse_idle_timeout};
#[cfg(unix)]
pub use listener::{Listener, Stopper, connection_over};
pub use methods::register_methods;
pub use registry::{
    DaemonState, Discovery, daemon_state, discover, register, registration_path_for,
    socket_path_for, stop_daemon, unregister,
};
pub use server::{serve, serve_stdio, serve_with_first};
pub use session::{
    Analysis, CompletedWork, Disposition, Session, SessionHost, Sessions, analyze_guarded,
    canonical_path, canonical_root, dispose,
};

/// This crate's version.
pub const DAEMON_VERSION: &str = env!("CARGO_PKG_VERSION");

static BUILD_VERSION: std::sync::OnceLock<&'static str> = std::sync::OnceLock::new();

/// Name the build of the running binary, before any daemon or client starts.
///
/// A daemon also runs the code of the crates above this one (the language server,
/// the binary), so the identity a registration carries is the binary's version.
/// The first call wins.
pub fn set_build_version(version: &'static str) {
    let _ = BUILD_VERSION.set(version);
}

/// The build a daemon registers and a client compares: the version given to
/// [`set_build_version`], or this crate's name and version when none was given.
/// The fallback is not a binary version, so no client orders it against one.
pub fn build_version() -> &'static str {
    BUILD_VERSION
        .get()
        .copied()
        .unwrap_or(concat!("oxabl_daemon ", env!("CARGO_PKG_VERSION")))
}

/// Who built the running executable: a hash of its bytes and when it was written.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuildIdentity {
    /// Sixteen hex digits of a BLAKE3 hash of the executable, or empty when it could
    /// not be read.
    pub id: String,
    /// When the executable was written, in milliseconds since the Unix epoch, or 0.
    pub modified_ms: u64,
}

/// The identity of the executable this process runs, read once.
///
/// A version number does not tell two development builds apart, so a daemon
/// registers this and a client compares it with its own. The bytes come from the
/// running executable itself (`/proc/self/exe` on Linux), not from the path it had
/// at start, so a binary replaced on disk after start does not change the answer.
pub fn build_identity() -> &'static BuildIdentity {
    static IDENTITY: std::sync::OnceLock<BuildIdentity> = std::sync::OnceLock::new();
    IDENTITY.get_or_init(|| {
        read_build_identity().unwrap_or(BuildIdentity {
            id: String::new(),
            modified_ms: 0,
        })
    })
}

fn read_build_identity() -> std::io::Result<BuildIdentity> {
    use std::io::Read;
    let mut file = if cfg!(target_os = "linux") {
        std::fs::File::open("/proc/self/exe")?
    } else {
        std::fs::File::open(std::env::current_exe()?)?
    };
    let modified_ms = file
        .metadata()?
        .modified()?
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |since| u64::try_from(since.as_millis()).unwrap_or(0));
    let mut hasher = blake3::Hasher::new();
    let mut buffer = vec![0u8; 1 << 20];
    loop {
        let read = file.read(&mut buffer)?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
    }
    let hash = hasher.finalize();
    Ok(BuildIdentity {
        id: hash.to_hex()[..16].to_string(),
        modified_ms,
    })
}

/// Whether the executable of process `pid` was replaced or removed on disk after it
/// started. Only Linux says so (`/proc/<pid>/exe` ends in ` (deleted)`); elsewhere
/// the answer is `false`, and the registered identity is the only evidence.
pub fn executable_replaced(pid: u32) -> bool {
    #[cfg(target_os = "linux")]
    {
        use std::os::unix::ffi::OsStrExt;
        std::fs::read_link(format!("/proc/{pid}/exe"))
            .is_ok_and(|target| target.as_os_str().as_bytes().ends_with(b" (deleted)"))
    }
    #[cfg(not(target_os = "linux"))]
    {
        let _ = pid;
        false
    }
}

#[cfg(test)]
mod build_identity_tests {
    #[test]
    fn the_running_executable_has_an_identity() {
        let identity = super::build_identity();
        assert_eq!(identity.id.len(), 16);
        assert!(identity.id.bytes().all(|byte| byte.is_ascii_hexdigit()));
        assert_ne!(identity.modified_ms, 0);
    }
}
