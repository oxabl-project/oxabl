//! Discovery: how a client finds a running daemon for a workspace root, and how a
//! dead one stops being found (R9, KTD20).
//!
//! # A registration file, not a port
//!
//! One file per workspace root, under the XDG cache directory, recording the
//! daemon's pid, its socket path, and the contract it speaks. No port to allocate
//! and no registry service to keep alive.
//!
//! Filesystem permissions do the access control, and they are applied rather than
//! assumed: [`ensure_registration_dir`] makes the leaf directory 0700 and repairs
//! one an earlier build left at 0755, the registration is opened 0600, and the
//! socket is bound inside that directory. The directory is the load-bearing
//! control, because it leaves no window in which the socket exists and is
//! reachable. The mode on each file is the backstop.
//!
//! The unit of access is a uid, not a person. Two humans sharing one account share
//! one daemon; that is the same trust boundary every other file in the cache
//! directory has.
//!
//! The location and the file's shape live in
//! [`oxabl_daemon_protocol`](oxabl_daemon_protocol::Registration), because both the
//! daemon that writes it and every client that reads it must agree — and a client
//! must not pull the whole analysis stack in to read a small JSON file.
//!
//! # A dead registration is an absent one
//!
//! A daemon that crashed leaves its file behind. A client that trusted it would
//! connect to a socket nobody is listening on and wait, which is the one failure a
//! discovery mechanism must not have. So a registration whose pid is not alive is
//! treated as absent and replaced. Checking liveness rather than trying the socket
//! is deliberate: a stale socket file can accept a connection that is then never
//! answered, and no timeout is short enough to be right for both a cold start and a
//! busy pass.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

#[cfg(unix)]
use std::os::unix::fs::{DirBuilderExt, MetadataExt, OpenOptionsExt, PermissionsExt};

use oxabl_daemon_protocol::{
    CONTRACT_VERSION, Registration, registration_dir, registration_path, temp_dir_fallback_in_use,
};

/// Create the registration directory, owned by this user and reachable by nobody
/// else.
///
/// The access control this module claims is a directory mode, so it has to be
/// applied where the directory is made — and in one place, because two call sites
/// spelling the intent twice is how one of them drifts.
///
/// Both steps are needed. `DirBuilder::mode` sets the mode at creation, leaving no
/// window in which the directory exists and is reachable. It does nothing for a
/// directory that already exists, and a developer who ran an earlier build already
/// has a 0755 one — so the mode is also set unconditionally afterwards.
///
/// Only the leaf is tightened. A 0700 leaf already blocks traversal, so the
/// intermediate `oxabl` directory can keep the umask default.
pub fn ensure_registration_dir() -> io::Result<PathBuf> {
    let dir = registration_dir();

    #[cfg(unix)]
    {
        fs::DirBuilder::new()
            .recursive(true)
            .mode(0o700)
            .create(&dir)
            .or_else(|error| match error.kind() {
                io::ErrorKind::AlreadyExists => Ok(()),
                _ => Err(error),
            })?;
        fs::set_permissions(&dir, fs::Permissions::from_mode(0o700))?;
        verify_owned_when_shared(&dir)?;
    }
    #[cfg(not(unix))]
    fs::create_dir_all(&dir)?;

    Ok(dir)
}

/// Refuse a registration directory in a shared parent that is not ours.
///
/// With neither `XDG_CACHE_HOME` nor `HOME` set, the directory lands under the
/// system temp directory — a world-writable parent, where another user can create
/// `oxabl/daemon` first and `create_dir_all` will adopt it, symlink included. That
/// is an integrity problem rather than a confidentiality one, and a mode cannot fix
/// it: the directory is already someone else's.
///
/// Checked only on that branch. Under a home directory the parent is already the
/// user's own.
#[cfg(unix)]
fn verify_owned_when_shared(dir: &Path) -> io::Result<()> {
    if !temp_dir_fallback_in_use() {
        return Ok(());
    }
    let metadata = fs::metadata(dir)?;
    // SAFETY: `getuid` reads the calling process's real user id. It takes no
    // arguments, touches no memory, and cannot fail.
    let uid = unsafe { libc_getuid() };
    if metadata.uid() != uid || metadata.mode() & 0o777 != 0o700 {
        return Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            format!(
                "refusing to use {}: it is in a shared directory and is not a \
                 private directory owned by this user. Set XDG_CACHE_HOME or HOME.",
                dir.display()
            ),
        ));
    }
    Ok(())
}

/// What a client found when it looked for a daemon.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Discovery {
    /// A daemon is running and speaks a contract this build can talk to.
    Live(Registration),
    /// A daemon is running and speaks a different contract. Reported rather than
    /// connected to, so the mismatch is named before a request is attempted.
    VersionMismatch(Registration),
    /// No daemon is registered, or the registration belongs to a dead process.
    Absent,
}

/// Look for a daemon serving `workspace_root`.
///
/// An unreadable or unparsable registration is [`Discovery::Absent`] rather than an
/// error: a corrupt file is indistinguishable from no file for every purpose a
/// caller has, and the next daemon to start replaces it.
pub fn discover(workspace_root: &Path) -> Discovery {
    let path = registration_path(workspace_root);
    let Ok(contents) = fs::read_to_string(&path) else {
        return Discovery::Absent;
    };
    let Ok(registration) = serde_json::from_str::<Registration>(&contents) else {
        return Discovery::Absent;
    };
    if !process_is_alive(registration.pid) {
        return Discovery::Absent;
    }
    if registration.contract_version != CONTRACT_VERSION {
        return Discovery::VersionMismatch(registration);
    }
    Discovery::Live(registration)
}

/// Write the registration for a daemon now serving `workspace_root`.
///
/// Replaces whatever was there. A client only ever reaches this point after finding
/// the previous registration absent or dead, and two daemons racing to serve one
/// root is a race the socket bind settles — the loser fails to bind and exits, so
/// the last writer here is the one that owns the socket.
///
/// Written to a temporary file and renamed, so a client reading concurrently sees
/// either the old registration or the new one and never a half-written file that
/// parses as neither.
///
/// The staging name carries the writer's pid. `rename` is atomic, but the write
/// into the staging file is not — so a staging path shared between two writers let
/// one of them rename a body the other was still writing, which is the torn read
/// this design exists to prevent. A per-writer name makes the guarantee real.
pub fn register(workspace_root: &Path, socket_path: &Path, pid: u32) -> io::Result<PathBuf> {
    let path = registration_path(workspace_root);
    ensure_registration_dir()?;
    let registration = Registration {
        pid,
        socket_path: socket_path.to_string_lossy().into_owned(),
        contract_version: CONTRACT_VERSION,
        workspace_root: workspace_root.to_string_lossy().into_owned(),
    };
    let body = serde_json::to_string_pretty(&registration)
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?;

    let staged = path.with_extension(format!("json.{}.tmp", std::process::id()));
    write_private(&staged, body.as_bytes())?;
    // Renaming onto the destination replaces it atomically; the staging file is
    // removed by the rename itself, so a failure before this point is what leaves
    // one behind.
    match fs::rename(&staged, &path) {
        Ok(()) => Ok(path),
        Err(error) => {
            let _ = fs::remove_file(&staged);
            Err(error)
        }
    }
}

/// Write `body` to `path`, readable by this user only.
///
/// `fs::write` cannot set a mode, so the file would exist world-readable for the
/// moment before any chmod. Opening with the mode closes that window. `create_new`
/// makes a colliding staging path an error rather than a silent overwrite — which
/// is safe precisely because the name above carries the writer's pid.
fn write_private(path: &Path, body: &[u8]) -> io::Result<()> {
    use std::io::Write;

    let mut options = fs::OpenOptions::new();
    options.write(true).create_new(true);
    #[cfg(unix)]
    options.mode(0o600);

    // A staging file left by a previous run that died between write and rename
    // would otherwise block every later registration.
    if path.exists() {
        fs::remove_file(path)?;
    }
    let mut file = options.open(path)?;
    file.write_all(body)?;
    file.sync_all()
}

/// Remove the registration for `workspace_root`, on an orderly shutdown.
///
/// Best-effort: a missing file is success, because the goal is "no live-looking
/// registration remains" and a file that is already gone satisfies it.
pub fn unregister(workspace_root: &Path) -> io::Result<()> {
    match fs::remove_file(registration_path(workspace_root)) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error),
    }
}

/// The socket path a daemon for `workspace_root` listens on.
///
/// Beside the registration rather than in a temp directory, so the two share a
/// lifetime and a permission model: both sit in the 0700 directory
/// [`ensure_registration_dir`] guarantees, so whoever can read the registration can
/// reach the socket and nobody else can traverse to either.
///
/// Built by swapping the extension explicitly rather than with
/// `Path::with_extension`. A flattened root contains dots of its own, so
/// `with_extension` was correct only because the registration always ends in
/// `.json` — an invariant spread across three functions and stated by none of
/// them. The naming rule also budgets the socket path against `sun_path`, so the
/// two spellings must stay in step.
pub fn socket_path_for(workspace_root: &Path) -> PathBuf {
    let registration = registration_path(workspace_root);
    let mut name = registration
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    debug_assert!(name.ends_with(".json"));
    name.truncate(name.len() - ".json".len());
    name.push_str(".sock");
    registration.with_file_name(name)
}

/// Whether a process with `pid` exists.
///
/// `kill(pid, 0)` is the portable question on Unix: it performs the permission check
/// and the existence check and sends nothing. An `EPERM` answer means the process
/// exists and belongs to somebody else, which is still "alive" — and still a reason
/// not to steal its registration.
#[cfg(unix)]
fn process_is_alive(pid: u32) -> bool {
    if pid == 0 {
        return false;
    }
    // SAFETY: `kill` with signal 0 sends no signal. It only reports whether the
    // process exists and whether we could signal it, and cannot affect it.
    let result = unsafe { libc_kill(pid as i32, 0) };
    if result == 0 {
        return true;
    }
    // `EPERM` (1) means it exists and is not ours.
    std::io::Error::last_os_error().raw_os_error() == Some(1)
}

/// Windows named pipes are deferred; the desktop app targets Linux first. Reporting
/// every registration as dead would make a client start a second daemon on every
/// launch, so this refuses to guess and says nothing is alive — which at least keeps
/// the failure to "no daemon found" rather than "connect to a socket nobody holds".
#[cfg(not(unix))]
fn process_is_alive(_pid: u32) -> bool {
    false
}

#[cfg(unix)]
unsafe extern "C" {
    #[link_name = "kill"]
    fn libc_kill(pid: i32, sig: i32) -> i32;

    #[link_name = "getuid"]
    fn libc_getuid() -> u32;
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Run `body` with the registration directory pointed at a fresh temporary
    /// directory, so the tests never touch a developer's real cache.
    ///
    /// `XDG_CACHE_HOME` is process-wide, so this serialises: the guard holds a lock
    /// for as long as the override is in place. Mutating the environment is
    /// otherwise a race against every other test in the binary.
    fn with_cache_home<T>(body: impl FnOnce(&Path) -> T) -> T {
        static LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
        let _guard = LOCK.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
        let cache = tempfile::tempdir().expect("a temporary cache directory");
        let previous = std::env::var_os("XDG_CACHE_HOME");
        // SAFETY: the lock above makes this the only thread mutating the
        // environment for the duration.
        unsafe { std::env::set_var("XDG_CACHE_HOME", cache.path()) };
        let out = body(cache.path());
        unsafe {
            match previous {
                Some(value) => std::env::set_var("XDG_CACHE_HOME", value),
                None => std::env::remove_var("XDG_CACHE_HOME"),
            }
        }
        out
    }

    // The access control is a mode, so it is asserted as one. A directory an
    // earlier build left world-readable is repaired rather than accepted, which is
    // the case `create_dir_all` alone silently gets wrong.
    #[cfg(unix)]
    #[test]
    fn the_registration_directory_and_file_are_private() {
        with_cache_home(|cache| {
            let dir = cache.join("oxabl").join("daemon");
            fs::create_dir_all(&dir).expect("a pre-existing directory");
            fs::set_permissions(&dir, fs::Permissions::from_mode(0o755))
                .expect("leave it as an earlier build would");

            let root = Path::new("/proj/private");
            let path = register(root, &socket_path_for(root), std::process::id())
                .expect("registration writes");

            assert_eq!(
                fs::metadata(&dir).expect("the directory").mode() & 0o777,
                0o700,
                "an existing loose directory must be repaired, not accepted"
            );
            assert_eq!(
                fs::metadata(&path).expect("the registration").mode() & 0o777,
                0o600,
            );
        });
    }

    // Two registrations for one root must not share a staging path. The rename is
    // atomic; the write into the staging file is not, so a shared name lets one
    // writer publish a body the other is still writing.
    #[test]
    fn a_staging_path_is_per_writer() {
        with_cache_home(|_| {
            let root = Path::new("/proj/staged");
            let path = register(root, &socket_path_for(root), std::process::id())
                .expect("registration writes");
            let staged = path.with_extension(format!("json.{}.tmp", std::process::id()));

            assert!(!staged.exists(), "the rename consumes the staging file");
            assert!(
                staged.file_name().expect("a name")
                    != path.with_extension("json.tmp").file_name().expect("a name"),
                "the staging name must carry the writer's identity"
            );
        });
    }

    #[test]
    fn nothing_registered_is_absent() {
        with_cache_home(|_| {
            assert_eq!(discover(Path::new("/proj/never-seen")), Discovery::Absent);
        });
    }

    #[test]
    fn a_live_registration_is_found() {
        with_cache_home(|_| {
            let root = Path::new("/proj/alpha");
            let socket = socket_path_for(root);
            // This process is alive by construction, which is the point.
            register(root, &socket, std::process::id()).expect("registration writes");

            match discover(root) {
                Discovery::Live(registration) => {
                    assert_eq!(registration.pid, std::process::id());
                    assert_eq!(registration.contract_version, CONTRACT_VERSION);
                    assert_eq!(registration.workspace_root, "/proj/alpha");
                    assert!(registration.socket_path.ends_with(".sock"));
                }
                other => panic!("expected a live registration, got {other:?}"),
            }
        });
    }

    /// The rule that keeps a crashed daemon from stranding a client: a registration
    /// whose pid is dead is absent, so the client starts a daemon instead of
    /// connecting to a socket nobody holds.
    #[test]
    fn a_registration_whose_pid_is_dead_is_absent() {
        with_cache_home(|_| {
            let root = Path::new("/proj/crashed");
            // A pid that cannot be running: pid 1 is init, and this is well past any
            // plausible live pid on a test machine.
            register(root, &socket_path_for(root), u32::MAX - 1).expect("registration writes");
            assert_eq!(discover(root), Discovery::Absent);
        });
    }

    #[test]
    fn a_mismatched_contract_is_reported_rather_than_connected_to() {
        with_cache_home(|_| {
            let root = Path::new("/proj/older");
            let path = registration_path(root);
            fs::create_dir_all(path.parent().expect("a parent")).expect("the directory");
            let stale = Registration {
                pid: std::process::id(),
                socket_path: socket_path_for(root).to_string_lossy().into_owned(),
                contract_version: CONTRACT_VERSION + 7,
                workspace_root: root.to_string_lossy().into_owned(),
            };
            fs::write(&path, serde_json::to_string(&stale).expect("json")).expect("write");

            match discover(root) {
                Discovery::VersionMismatch(found) => {
                    assert_eq!(found.contract_version, CONTRACT_VERSION + 7);
                }
                other => panic!("expected a version mismatch, got {other:?}"),
            }
        });
    }

    #[test]
    fn a_corrupt_registration_is_absent_rather_than_an_error() {
        with_cache_home(|_| {
            let root = Path::new("/proj/corrupt");
            let path = registration_path(root);
            fs::create_dir_all(path.parent().expect("a parent")).expect("the directory");
            fs::write(&path, "this is not json").expect("write");
            assert_eq!(discover(root), Discovery::Absent);
        });
    }

    #[test]
    fn two_roots_produce_two_registrations() {
        with_cache_home(|_| {
            let alpha = Path::new("/proj/alpha");
            let beta = Path::new("/proj/beta");
            register(alpha, &socket_path_for(alpha), std::process::id()).expect("alpha");
            register(beta, &socket_path_for(beta), std::process::id()).expect("beta");

            assert_ne!(registration_path(alpha), registration_path(beta));
            assert_ne!(socket_path_for(alpha), socket_path_for(beta));
            assert!(matches!(discover(alpha), Discovery::Live(_)));
            assert!(matches!(discover(beta), Discovery::Live(_)));
        });
    }

    #[test]
    fn registering_replaces_a_previous_registration() {
        with_cache_home(|_| {
            let root = Path::new("/proj/replaced");
            register(root, Path::new("/tmp/old.sock"), u32::MAX - 1).expect("first");
            register(root, Path::new("/tmp/new.sock"), std::process::id()).expect("second");

            match discover(root) {
                Discovery::Live(found) => assert_eq!(found.socket_path, "/tmp/new.sock"),
                other => panic!("expected the replacement, got {other:?}"),
            }
        });
    }

    #[test]
    fn unregistering_a_missing_registration_is_success() {
        with_cache_home(|_| {
            assert!(unregister(Path::new("/proj/never-registered")).is_ok());
        });
    }

    #[test]
    fn unregistering_removes_the_registration() {
        with_cache_home(|_| {
            let root = Path::new("/proj/leaving");
            register(root, &socket_path_for(root), std::process::id()).expect("registration");
            unregister(root).expect("removal");
            assert_eq!(discover(root), Discovery::Absent);
        });
    }
}
