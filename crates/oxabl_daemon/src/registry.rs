//! Discovery: how a client finds a running daemon for a workspace root, and how a
//! dead one stops being found (R9, KTD20).
//!
//! # A registration file, not a port
//!
//! One file per workspace root, under the XDG cache directory, recording the
//! daemon's pid, its socket path, and the contract it speaks. No port to allocate
//! and no registry service to keep alive.
//!
//! Filesystem permissions do the access control, and they are created rather than
//! assumed or repaired: [`ensure_registration_dir`] makes the leaf directory 0700
//! at the moment it comes into existence, the registration is opened 0600, and the
//! socket is bound inside that directory. The directory is the load-bearing
//! control, because it leaves no window in which the socket exists and is
//! reachable. The mode on each file is the backstop.
//!
//! A directory that already exists is **verified and then used, or refused** — it
//! is never modified into shape. `mkdir` applies `mode & ~umask`, so a create at
//! 0700 cannot yield something wider; a wider one was therefore made by something
//! that is not this daemon, and a chmod would only have hidden that. The old
//! unconditional `chmod` is gone for that reason: applied to a leaf an attacker had
//! replaced with a symlink, it tightened *their* directory and then served the
//! socket out of it.
//!
//! # Why `rustix` here, rather than the hand-rolled `extern "C"` block below
//!
//! Every other syscall in this module is declared by hand. The directory setup is
//! not, and the exception is deliberate (KTD7): it is the one path where owning
//! errno mapping and descriptor lifetimes by hand is exactly where the bug lives.
//! `rustix` takes `AsFd` and returns `OwnedFd`, so no descriptor can be closed
//! twice or used after close, and on Linux it exposes `openat2` with
//! `RESOLVE_NO_SYMLINKS`, which refuses a symlink at *any* component instead of
//! only the last one. There is no way to ask for that through a single hand-written
//! declaration.
//!
//! # The residual window, stated plainly
//!
//! There is no `bindat`. The socket is created by path, and the permission set
//! after the bind re-resolves that path. "Does not follow a symlink at any point"
//! is therefore load-bearing on the 0700 directory this module verifies, not on the
//! bind: an attacker who cannot traverse into the directory cannot plant anything
//! for the bind to resolve through. Treat the post-bind `chmod` as a backstop
//! inside an already-private directory rather than as an independent guarantee.
//! Opening the registration and the lock file works the same way, and for the same
//! reason. Do not read the absolute wording as licence to add path-based work
//! outside that directory.
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
use std::ffi::OsStr;
#[cfg(unix)]
use std::os::fd::OwnedFd;
#[cfg(unix)]
use std::os::unix::fs::OpenOptionsExt;

#[cfg(unix)]
use rustix::fs::{Mode, OFlags};
#[cfg(unix)]
use rustix::io::Errno;

use oxabl_daemon_protocol::{CONTRACT_VERSION, Registration, registration_dir, registration_path};

/// How many trailing components of [`registration_dir`] this daemon creates, and
/// therefore owns the safety of. Everything above them is the prefix the user named
/// through `XDG_CACHE_HOME`, `HOME`, or the temp directory.
///
/// Coupled to the shape `registration_dir` returns (`<base>/oxabl/daemon`). If that
/// grows a level, this has to grow with it, or a component the daemon creates would
/// be created by path.
#[cfg(unix)]
const OWNED_COMPONENTS: usize = 2;

/// The sticky bit: on a directory, only the owner of an entry may remove it.
#[cfg(unix)]
const STICKY: u32 = 0o1000;

/// Create the registration directory, owned by this user and reachable by nobody
/// else.
///
/// The access control this module claims is a directory mode, so it is applied
/// where the directory is made — and in one place, because two call sites spelling
/// the intent twice is how one of them drifts.
///
/// One step, not two: the mode is set by `mkdir` itself, so there is no window in
/// which the directory exists and is reachable. There is deliberately no second
/// step that fixes up a directory that already exists. `mkdir` applies
/// `mode & ~umask`, so a create at 0700 cannot produce something wider — a wider
/// directory was made by something else, and the only safe answer is to refuse it.
/// The previous unconditional `chmod` did the opposite: on a leaf replaced by a
/// symlink it tightened the attacker's directory and carried on.
///
/// Only the leaf is 0700. A 0700 leaf already blocks traversal, so the intermediate
/// `oxabl` directory is left at 0755 — but it is still verified, because an
/// intermediate somebody else owns lets them rename the leaf out from under us.
///
/// One consequence of taking the mode from `mkdir` and never fixing it up: a umask
/// that strips owner bits (`0177` and the like) makes the created leaf narrower than
/// 0700, and the next call refuses it. That is a loud, named failure with the mode in
/// the message, which is the right way for an unusable umask to surface.
pub fn ensure_registration_dir() -> io::Result<PathBuf> {
    let dir = registration_dir();

    #[cfg(unix)]
    {
        // Dropped on return. Descriptor-relative work is what makes the walk
        // race-free; the per-file opens inside the verified directory are still by
        // path, for the reason the module doc gives.
        let _verified = create_and_verify_dir(&dir)?;
    }
    #[cfg(not(unix))]
    fs::create_dir_all(&dir)?;

    Ok(dir)
}

/// Walk down to the registration directory, creating what is missing and verifying
/// what is not, and return a descriptor for the leaf.
///
/// Descriptor-relative on purpose. A single `create_dir_all` resolves the whole path
/// again on every call, so a symlink planted at *any* component is followed — and
/// the intermediate component is the one an attacker can win, because it is created
/// with the umask default rather than 0700 and it often does not exist yet. Each
/// component is therefore created below the descriptor of its parent and then opened
/// refusing symlinks, so nothing above the leaf can be substituted between the
/// create and the use.
///
/// The prefix above [`OWNED_COMPONENTS`] is resolved by path, symlinks included. It
/// is the location the user named, and refusing a symlink there would refuse
/// legitimate setups — a `~/.cache` pointing at another volume, or a symlinked
/// `/home`. It is not trusted blindly: the descriptor it resolves to is verified
/// before anything is created inside it.
#[cfg(unix)]
fn create_and_verify_dir(dir: &Path) -> io::Result<OwnedFd> {
    let prefix = dir.ancestors().nth(OWNED_COMPONENTS).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "refusing to use {}: it is not the <base>/oxabl/daemon shape the \
                 daemon knows how to create safely. Set XDG_CACHE_HOME or HOME.",
                dir.display()
            ),
        )
    })?;
    let owned = dir.strip_prefix(prefix).map_err(|error| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("refusing to use {}: {error}", dir.display()),
        )
    })?;
    // A bare relative destination leaves the prefix empty; the current directory is
    // what it means.
    let base = if prefix.as_os_str().is_empty() {
        Path::new(".")
    } else {
        prefix
    };

    fs::create_dir_all(base)?;
    let mut parent = open_dir_by_path(base)?;
    verify_usable_parent(&parent, base)?;

    let mut walked = base.to_path_buf();
    let depth = owned.components().count();
    for (index, component) in owned.components().enumerate() {
        let std::path::Component::Normal(name) = component else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!(
                    "refusing to use {}: {:?} is not a plain directory name.",
                    dir.display(),
                    component.as_os_str()
                ),
            ));
        };
        walked.push(name);
        let leaf = index + 1 == depth;
        // The leaf is private from birth. An intermediate is 0755 rather than 0700
        // because a 0700 leaf already blocks traversal, and rather than 0777 because
        // `mkdir` only ever narrows: asking for 0777 under an empty umask would
        // create a world-writable directory that the check below then refuses — the
        // daemon failing on a directory it made itself.
        let mode = if leaf { 0o700 } else { 0o755 };

        let existed = match rustix::fs::mkdirat(&parent, name, Mode::from_bits_truncate(mode)) {
            Ok(()) => false,
            Err(Errno::EXIST) => true,
            Err(errno) => return Err(refuse(&walked, "it could not be created", errno)),
        };
        parent = open_dir_below(&parent, name, &walked)?;

        if leaf && existed {
            // The one case where the mode was not set by us. Verify it or refuse it;
            // do not repair it.
            verify_private_leaf(&parent, &walked)?;
        } else {
            verify_usable_parent(&parent, &walked)?;
        }
    }

    Ok(parent)
}

/// Open a directory by path, for the prefix the user named.
#[cfg(unix)]
fn open_dir_by_path(path: &Path) -> io::Result<OwnedFd> {
    rustix::fs::open(
        path,
        OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
        Mode::empty(),
    )
    .map_err(|errno| refuse(path, "it could not be opened as a directory", errno))
}

/// Open `name` directly below `parent`, refusing a symlink and refusing anything
/// that is not a directory.
///
/// On Linux the kernel does the refusing: `openat2` with `RESOLVE_NO_SYMLINKS`
/// rejects a symlink at any component, and `RESOLVE_BENEATH` rejects an escape
/// upwards. `openat2` is not everywhere, though — it needs Linux 5.6, and a seccomp
/// profile that predates it answers `EPERM` rather than passing it through — so the
/// fallback is `openat` with `O_NOFOLLOW`. For the single, plain component this
/// function is given, `O_NOFOLLOW` refuses exactly the same thing; the kernel call
/// is preferred because it keeps that equivalence from being an argument the next
/// reader has to re-derive.
#[cfg(unix)]
fn open_dir_below(parent: &OwnedFd, name: &OsStr, path: &Path) -> io::Result<OwnedFd> {
    // `O_NONBLOCK` so a leaf replaced by a FIFO cannot park this thread waiting for
    // a writer. `O_DIRECTORY` should reject it first; this costs nothing and does
    // not depend on that ordering.
    let flags =
        OFlags::RDONLY | OFlags::DIRECTORY | OFlags::NOFOLLOW | OFlags::CLOEXEC | OFlags::NONBLOCK;

    #[cfg(target_os = "linux")]
    let opened = match rustix::fs::openat2(
        parent,
        name,
        flags,
        Mode::empty(),
        rustix::fs::ResolveFlags::NO_SYMLINKS | rustix::fs::ResolveFlags::BENEATH,
    ) {
        Err(Errno::NOSYS) | Err(Errno::PERM) => {
            rustix::fs::openat(parent, name, flags, Mode::empty())
        }
        other => other,
    };
    #[cfg(not(target_os = "linux"))]
    let opened = rustix::fs::openat(parent, name, flags, Mode::empty());

    opened.map_err(|errno| {
        let why = match errno {
            Errno::LOOP => "it is a symbolic link, and the daemon will not follow one on this path",
            Errno::NOTDIR => "it exists and is not a directory",
            Errno::XDEV => "it resolves outside its parent directory",
            _ => "it could not be opened as a directory",
        };
        refuse(path, why, errno)
    })
}

/// Verify a directory the leaf will live under: ours, or a shared directory nobody
/// can play games in.
///
/// Being a directory is already settled by `O_DIRECTORY` on the open above, so the
/// two questions left are who owns it and who may unlink out of it.
///
/// A world-writable parent without the sticky bit is a hard failure. Without the
/// sticky bit any local user can rename or unlink our directory and put their own
/// there, so a correct mode on the leaf would prove nothing. `/tmp` is the case this
/// admits: root-owned and sticky, which is exactly the configuration that makes the
/// temp-directory fallback usable at all.
#[cfg(unix)]
fn verify_usable_parent(dir: &OwnedFd, path: &Path) -> io::Result<()> {
    let stat =
        rustix::fs::fstat(dir).map_err(|errno| refuse(path, "it could not be inspected", errno))?;
    let mode = permission_bits(&stat);

    if mode & 0o002 != 0 && mode & STICKY == 0 {
        return Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            format!(
                "refusing to use {}: it is world-writable and has no sticky bit, so \
                 any local user could replace the daemon's directory. Set \
                 XDG_CACHE_HOME or HOME to a directory you own.",
                path.display()
            ),
        ));
    }

    let uid = rustix::process::getuid().as_raw();
    let shared_and_protected = stat.st_uid == 0 && mode & STICKY != 0;
    if stat.st_uid != uid && !shared_and_protected {
        return Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            format!(
                "refusing to use {}: it is owned by uid {}, not by this user (uid \
                 {uid}) and not by root with the sticky bit. Set XDG_CACHE_HOME or \
                 HOME to a directory you own.",
                path.display(),
                stat.st_uid
            ),
        ));
    }
    Ok(())
}

/// Verify a leaf directory that already existed: this user's, and already 0700.
///
/// Refused rather than repaired (KTD8). The daemon only ever creates this directory
/// at 0700, so one that is not 0700 is not one the daemon made, and a `chmod` would
/// turn "somebody else's directory" into "somebody else's directory that now looks
/// like ours".
#[cfg(unix)]
fn verify_private_leaf(dir: &OwnedFd, path: &Path) -> io::Result<()> {
    let stat =
        rustix::fs::fstat(dir).map_err(|errno| refuse(path, "it could not be inspected", errno))?;
    let mode = permission_bits(&stat) & 0o777;
    let uid = rustix::process::getuid().as_raw();
    if stat.st_uid != uid || mode != 0o700 {
        return Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            format!(
                "refusing to use {}: it already exists with mode {mode:04o} owned by \
                 uid {}, and the daemon requires mode 0700 owned by this user (uid \
                 {uid}). It is refused rather than changed, because the daemon never \
                 creates it any other way. Remove it, or set XDG_CACHE_HOME to a \
                 directory you own.",
                path.display(),
                stat.st_uid
            ),
        ));
    }
    Ok(())
}

/// The permission, set-id and sticky bits of a `stat`, with the file type dropped.
///
/// `st_mode` is a `u32` on every platform this compiles for, so no widening is
/// needed and clippy is right to reject one.
#[cfg(unix)]
fn permission_bits(stat: &rustix::fs::Stat) -> u32 {
    stat.st_mode & 0o7777
}

/// Refusal with the path named, the reason given, and the underlying errno kept so
/// the kind survives for a caller that matches on it.
#[cfg(unix)]
fn refuse(path: &Path, why: &str, errno: Errno) -> io::Error {
    let source = io::Error::from(errno);
    io::Error::new(
        source.kind(),
        format!("refusing to use {}: {why} ({source})", path.display()),
    )
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
    // Two signals, and a registration is live only when both agree.
    //
    // The lock is the authoritative one: the kernel drops it when the holder dies
    // by any means, so no dead daemon can look alive and a recycled pid cannot
    // resurrect one. The pid check is secondary — it costs one syscall, and it
    // still catches a registration written by a build that predates the lock.
    //
    // Keeping both matters because `flock` is unreliable on older NFS servers, and
    // a home directory on NFS is plausible. Where the lock cannot be trusted this
    // degrades to the previous behaviour rather than to something worse.
    if !a_daemon_holds_the_lock(workspace_root) || !process_is_alive(registration.pid) {
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
/// root is settled before either reaches this point: [`acquire_root_lock`] admits
/// exactly one, and the loser exits without binding or writing.
///
/// The bind does **not** settle that race and never did. `Listener::bind` unlinks
/// a stale socket path before binding, so `bind` cannot return `EADDRINUSE` and two
/// racing daemons would both have succeeded.
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

/// Take the root lock and write the registration, returning the guard.
///
/// The pairing a real daemon has: [`Listener::bind`](crate::Listener::bind) holds
/// the lock for its whole life, and the registration means nothing once the lock is
/// gone. A caller that writes a registration without holding the lock produces one
/// [`discover`] correctly reports as dead.
///
/// Returns `None` when a daemon already holds the root.
pub fn register_locked(
    workspace_root: &Path,
    socket_path: &Path,
    pid: u32,
) -> io::Result<Option<(PathBuf, RootLock)>> {
    let Some(lock) = acquire_root_lock(workspace_root)? else {
        return Ok(None);
    };
    let path = register(workspace_root, socket_path, pid)?;
    Ok(Some((path, lock)))
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
/// and the existence check and sends nothing.
///
/// **Secondary to the lock**, and no longer load-bearing on its own. It answers
/// "does some process have this number", which stopped being the same question as
/// "is this daemon running" the moment a pid could be recycled. `EPERM` is counted
/// as alive because a process owned by another user is a process; under the lock
/// that arm can no longer pin a dead registration on its own, since the lock has
/// to agree.
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

    #[link_name = "flock"]
    fn libc_flock(fd: i32, operation: i32) -> i32;
}

#[cfg(unix)]
const LOCK_EX: i32 = 2;
#[cfg(unix)]
const LOCK_NB: i32 = 4;

/// The lock file that says a daemon owns this root.
///
/// Never unlinked. Deleting it would let two daemons hold locks on two different
/// inodes for one root, which is the race the lock exists to prevent — so the file
/// outlives every daemon and costs one empty inode per workspace.
pub fn lock_path_for(workspace_root: &Path) -> PathBuf {
    let registration = registration_path(workspace_root);
    let mut name = registration
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    debug_assert!(name.ends_with(".json"));
    name.truncate(name.len() - ".json".len());
    name.push_str(".lock");
    registration.with_file_name(name)
}

/// Exclusive ownership of one workspace root, held for a daemon's lifetime.
///
/// The lock is what makes "a daemon serves this root" a fact rather than an
/// inference. The kernel releases it when the holding process dies **by any
/// means**, including `SIGKILL`, where no cleanup code runs — so liveness stops
/// depending on a pid that the operating system is free to hand to somebody else.
#[derive(Debug)]
pub struct RootLock {
    /// Held, never read. Closing this descriptor is what releases the lock, so the
    /// value exists purely for its `Drop`.
    #[cfg(unix)]
    #[allow(dead_code)]
    file: fs::File,
}

/// Take the lock for `workspace_root`, or report that a daemon already holds it.
///
/// Non-blocking on purpose: a second daemon must fail immediately and exit, not
/// queue behind the first.
pub fn acquire_root_lock(workspace_root: &Path) -> io::Result<Option<RootLock>> {
    ensure_registration_dir()?;
    let path = lock_path_for(workspace_root);

    #[cfg(unix)]
    {
        let file = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(false)
            .mode(0o600)
            .open(&path)?;
        // SAFETY: `flock` takes a valid descriptor this function owns and an
        // operation constant. It mutates no memory and the descriptor outlives the
        // call.
        let taken =
            unsafe { libc_flock(std::os::fd::AsRawFd::as_raw_fd(&file), LOCK_EX | LOCK_NB) };
        if taken == 0 {
            return Ok(Some(RootLock { file }));
        }
        let error = io::Error::last_os_error();
        // `EWOULDBLOCK`/`EAGAIN` is the answer we asked for: somebody holds it.
        match error.raw_os_error() {
            Some(11) | Some(35) => Ok(None),
            _ => Err(error),
        }
    }
    #[cfg(not(unix))]
    {
        let _ = path;
        Ok(Some(RootLock {}))
    }
}

/// Whether a daemon currently holds the lock for `workspace_root`.
///
/// Takes the lock and drops it again, so the answer is "somebody else holds it"
/// rather than "I hold it". Returns a plain `bool` and releases inside, because a
/// caller that accidentally held this would block the next daemon start.
fn a_daemon_holds_the_lock(workspace_root: &Path) -> bool {
    match acquire_root_lock(workspace_root) {
        // We took it, so nobody held it.
        Ok(Some(lock)) => {
            drop(lock);
            false
        }
        Ok(None) => true,
        // The lock file could not be opened at all. Say nothing rather than
        // claiming a daemon is absent; the pid check below still applies.
        Err(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // The modes these tests assert are read through the Unix extensions; the
    // production path reads them off a descriptor instead.
    #[cfg(unix)]
    use std::os::unix::fs::{MetadataExt, PermissionsExt};

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

    // The access control is a mode, so it is asserted as one. A directory the
    // daemon creates itself is 0700 from the moment it exists, and the
    // registration inside it is 0600.
    #[cfg(unix)]
    #[test]
    fn the_registration_directory_and_file_are_private() {
        with_cache_home(|cache| {
            let dir = cache.join("oxabl").join("daemon");
            let root = Path::new("/proj/private");
            let path = register(root, &socket_path_for(root), std::process::id())
                .expect("registration writes");

            assert_eq!(
                fs::metadata(&dir).expect("the directory").mode() & 0o777,
                0o700,
                "the directory must be created private, not widened and then tightened"
            );
            assert_eq!(
                fs::metadata(&path).expect("the registration").mode() & 0o777,
                0o600,
            );
        });
    }

    // A directory that is not already the mode this module claims is refused, not
    // repaired. Repairing it would mean chmod-ing a directory whose provenance is
    // unknown, and `mkdir` at 0700 can never produce a wider one — so a wider one
    // was made by something else.
    #[cfg(unix)]
    #[test]
    fn a_pre_existing_directory_with_a_wider_mode_is_refused() {
        with_cache_home(|cache| {
            let dir = cache.join("oxabl").join("daemon");
            fs::create_dir_all(&dir).expect("a pre-existing directory");
            fs::set_permissions(&dir, fs::Permissions::from_mode(0o755))
                .expect("leave it as an earlier build would");

            let root = Path::new("/proj/private");
            let error = register(root, &socket_path_for(root), std::process::id())
                .expect_err("a loose directory must be refused");

            assert_eq!(
                error.kind(),
                io::ErrorKind::PermissionDenied,
                "refusal is a permission failure, not a generic one"
            );
            assert!(
                error.to_string().contains(&dir.display().to_string()),
                "the message must name the directory it refused: {error}"
            );
            assert_eq!(
                fs::metadata(&dir).expect("the directory").mode() & 0o777,
                0o755,
                "a refused directory must not be modified"
            );
        });
    }

    // The hole this closes: a symlink planted at the leaf made the old repair step
    // chmod 0700 onto whatever the link pointed at, and then made the daemon serve
    // its socket out of somebody else's directory.
    #[cfg(unix)]
    #[test]
    fn a_leaf_that_is_a_symlink_is_refused_and_its_target_is_untouched() {
        with_cache_home(|cache| {
            let target = cache.join("elsewhere");
            fs::create_dir_all(&target).expect("the link target");
            fs::set_permissions(&target, fs::Permissions::from_mode(0o755))
                .expect("a target mode to watch");
            let middle = cache.join("oxabl");
            fs::create_dir_all(&middle).expect("the intermediate directory");
            std::os::unix::fs::symlink(&target, middle.join("daemon")).expect("the planted link");

            let root = Path::new("/proj/hijacked");
            let error = register(root, &socket_path_for(root), std::process::id())
                .expect_err("a symlinked leaf must be refused");

            assert!(
                error.to_string().contains("daemon"),
                "the message must name what it refused: {error}"
            );
            assert_eq!(
                fs::metadata(&target).expect("the target").mode() & 0o777,
                0o755,
                "the link target must not be chmod-ed"
            );
            assert_eq!(
                fs::read_dir(&target).expect("the target").count(),
                0,
                "nothing may be written through the link"
            );
        });
    }

    // A directory that is already what the daemon would have created is used as it
    // is. The point of refusing a wrong one is not to refuse every existing one.
    #[cfg(unix)]
    #[test]
    fn a_pre_existing_private_directory_is_accepted_unchanged() {
        with_cache_home(|cache| {
            let dir = cache.join("oxabl").join("daemon");
            fs::create_dir_all(&dir).expect("a pre-existing directory");
            fs::set_permissions(&dir, fs::Permissions::from_mode(0o700)).expect("private already");
            let before = fs::metadata(&dir).expect("the directory");

            let root = Path::new("/proj/existing");
            register(root, &socket_path_for(root), std::process::id())
                .expect("registration writes");

            let after = fs::metadata(&dir).expect("the directory");
            assert_eq!(after.mode() & 0o777, 0o700);
            assert_eq!(
                after.ino(),
                before.ino(),
                "the directory must be used, not replaced"
            );
        });
    }

    // An intermediate component is the one an attacker can win: it is created with
    // the umask default rather than 0700, and it often does not exist yet. A create
    // of the whole chain by path would follow a link planted there.
    #[cfg(unix)]
    #[test]
    fn an_intermediate_component_that_is_a_symlink_is_refused() {
        with_cache_home(|cache| {
            let target = cache.join("elsewhere");
            fs::create_dir_all(&target).expect("the link target");
            std::os::unix::fs::symlink(&target, cache.join("oxabl")).expect("the planted link");

            let root = Path::new("/proj/redirected");
            let error = register(root, &socket_path_for(root), std::process::id())
                .expect_err("a symlinked intermediate must be refused");

            assert!(
                error.to_string().contains("oxabl"),
                "the message must name the component it refused: {error}"
            );
            assert_eq!(
                fs::read_dir(&target).expect("the target").count(),
                0,
                "nothing below a refused component may be created"
            );
        });
    }

    // `mkdir` reports `EEXIST` for any kind of entry, so the open that follows has
    // to be the one that insists on a directory.
    #[cfg(unix)]
    #[test]
    fn a_leaf_that_is_a_regular_file_is_refused() {
        with_cache_home(|cache| {
            let middle = cache.join("oxabl");
            fs::create_dir_all(&middle).expect("the intermediate directory");
            fs::write(middle.join("daemon"), b"not a directory").expect("the planted file");

            let root = Path::new("/proj/filed");
            let error = register(root, &socket_path_for(root), std::process::id())
                .expect_err("a leaf that is a file must be refused");
            assert!(
                error.to_string().contains("not a directory"),
                "the message must say what it found: {error}"
            );
        });
    }

    // A FIFO is the same refusal with a second hazard: opening one can block until a
    // writer appears, which would hang the daemon rather than fail it.
    #[cfg(unix)]
    #[test]
    fn a_leaf_that_is_a_fifo_is_refused_without_blocking() {
        with_cache_home(|cache| {
            let middle = cache.join("oxabl");
            fs::create_dir_all(&middle).expect("the intermediate directory");
            rustix::fs::mknodat(
                rustix::fs::CWD,
                middle.join("daemon"),
                rustix::fs::FileType::Fifo,
                Mode::from_bits_truncate(0o600),
                0,
            )
            .expect("the planted fifo");

            let root = Path::new("/proj/piped");
            let error = register(root, &socket_path_for(root), std::process::id())
                .expect_err("a leaf that is a fifo must be refused");
            assert!(
                error.to_string().contains("not a directory"),
                "the message must say what it found: {error}"
            );
        });
    }

    // Without the sticky bit, a world-writable parent lets any local user rename our
    // directory away and put their own there, so a correct mode on the leaf would
    // prove nothing.
    #[cfg(unix)]
    #[test]
    fn a_world_writable_parent_without_the_sticky_bit_is_refused() {
        with_cache_home(|cache| {
            fs::set_permissions(cache, fs::Permissions::from_mode(0o777))
                .expect("a shared parent with no sticky bit");

            let root = Path::new("/proj/exposed");
            let error = register(root, &socket_path_for(root), std::process::id())
                .expect_err("a world-writable parent must be refused");

            assert_eq!(error.kind(), io::ErrorKind::PermissionDenied);
            assert!(
                error.to_string().contains("sticky"),
                "the message must name the missing protection: {error}"
            );
            // Restore, so the temporary directory can be cleaned up.
            fs::set_permissions(cache, fs::Permissions::from_mode(0o700)).expect("restore");
        });
    }

    // The temp-directory fallback exists for a headless process with neither
    // `XDG_CACHE_HOME` nor `HOME`, and it lands under a world-writable sticky
    // directory. That combination has to stay usable, or the refusals above would
    // have made the fallback dead. The stand-in is owned by this user rather than by
    // root, because a test cannot create a root-owned directory; the sticky arm of
    // the ownership rule is unreachable from here.
    #[cfg(unix)]
    #[test]
    fn a_world_writable_sticky_parent_is_usable() {
        with_cache_home(|cache| {
            let parent = cache.join("sticky");
            fs::create_dir(&parent).expect("a stand-in for /tmp");
            fs::set_permissions(&parent, fs::Permissions::from_mode(0o1777))
                .expect("world-writable and sticky");
            let dir = parent.join("oxabl").join("daemon");

            create_and_verify_dir(&dir).expect("a sticky shared parent is usable");
            assert_eq!(
                fs::metadata(&dir).expect("the directory").mode() & 0o777,
                0o700,
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
            // Alive by construction: this process holds the lock and its pid is
            // its own, which is what a real daemon has.
            let (_path, _lock) = register_locked(root, &socket, std::process::id())
                .expect("registration writes")
                .expect("no daemon holds this root");

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
            // Created the way the daemon creates it. A hand-made directory is a
            // wrong-mode directory, which the daemon now refuses outright.
            ensure_registration_dir().expect("the directory");
            let stale = Registration {
                pid: std::process::id(),
                socket_path: socket_path_for(root).to_string_lossy().into_owned(),
                contract_version: CONTRACT_VERSION + 7,
                workspace_root: root.to_string_lossy().into_owned(),
            };
            fs::write(&path, serde_json::to_string(&stale).expect("json")).expect("write");
            let _lock = acquire_root_lock(root)
                .expect("lock the root")
                .expect("nothing holds it");

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
            ensure_registration_dir().expect("the directory");
            fs::write(&path, "this is not json").expect("write");
            assert_eq!(discover(root), Discovery::Absent);
        });
    }

    #[test]
    fn two_roots_produce_two_registrations() {
        with_cache_home(|_| {
            let alpha = Path::new("/proj/alpha");
            let beta = Path::new("/proj/beta");
            let (_a, _alpha_lock) =
                register_locked(alpha, &socket_path_for(alpha), std::process::id())
                    .expect("alpha")
                    .expect("nothing holds alpha");
            let (_b, _beta_lock) =
                register_locked(beta, &socket_path_for(beta), std::process::id())
                    .expect("beta")
                    .expect("nothing holds beta");

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
            let (_path, _lock) =
                register_locked(root, Path::new("/tmp/new.sock"), std::process::id())
                    .expect("second")
                    .expect("nothing holds it");

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
