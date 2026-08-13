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
//! # Why liveness is a connect, and never a lock query (KTD6)
//!
//! A daemon that crashed leaves its file behind. A client that trusted it would
//! connect to a socket nobody is listening on and wait, which is the one failure a
//! discovery mechanism must not have. So the question "is a daemon there" is asked by
//! **connecting to the socket**, and it is never asked by taking the lock.
//!
//! Taking the lock to answer it is what this module used to do, and it broke the
//! thing it was asking about: the probe held the lock for the moment between acquire
//! and release, and a daemon starting in that moment got `EWOULDBLOCK` and refused
//! itself with "a daemon already serves this workspace" when nothing did. A client
//! poll loop asks every 20ms, so the window is hit in practice, not in theory. Nor is
//! there a non-disturbing version to reach for: `flock` locks and `fcntl` record locks
//! are separate lock spaces on local Linux filesystems (`fcntl_locking(2)`: "Since
//! Linux 2.0, there is no interaction between the types of lock placed by flock(2)
//! and fcntl()"), so a lock *query* cannot see an `flock` at all — it would answer
//! "nobody holds it" while a daemon held it.
//!
//! An earlier version of this doc argued the opposite, that trying the socket is the
//! unreliable half, because a stale socket can accept a connection nobody ever
//! answers and no timeout is right for both a cold start and a busy pass. That
//! objection is real and it is answered by never making the connect decisive on its
//! own:
//!
//! * A daemon that is alive but wedged reads as **saturated** — a full backlog or a
//!   timed-out connect is a live server, and the answer is "do not start a second
//!   one", so no timeout has to be tuned.
//! * "Crashed" is proved by an **acquirable lock**, not by a timeout expiring. The
//!   daemon holds the lock for its whole lifetime ([`RootLock`] is a field of
//!   `Listener`), and the kernel releases it however the process dies — so a lock
//!   that can be taken plus a socket that refuses a connection is proof that no live
//!   owner exists. The lock stays, but only as the start-serialisation mutex.
//!
//! The start path owes itself one more probe: another process can take the lock,
//! start a daemon, and be serving by the time we look, so the socket is probed again
//! *after* the lock is held and a socket that answers is refused rather than
//! unlinked. `tmux` sequences connect, lock, reconnect for the same reason, and
//! `sccache` probes by connecting only.
//!
//! The read path validates the registration directory the same way the write path
//! does, and refuses a registration that names a socket outside it. Discovery used to
//! reach [`ensure_registration_dir`] by accident, through the lock the probe took; a
//! connect-based probe touches no lock, so the check is made on purpose, by a walk
//! that inspects and never creates. Without it a client
//! resolving the shared fallback location would read a registration another local
//! user planted, connect to that user's socket, hand over buffer contents and take
//! fabricated diagnostics back.

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

/// Whether a walk down to the registration directory may create what is missing.
///
/// The write path creates; the read path must not. A client that discovers a daemon
/// answers a question, and answering it by making a directory would mean every
/// `oxabl` invocation in a tree with no daemon left one behind — and, worse, that the
/// only ownership check on the registration lived on the daemon's write path.
#[cfg(unix)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Missing {
    /// Create each owned component that is not there yet.
    Create,
    /// Refuse: report the missing component and change nothing.
    Refuse,
}

/// Verify the registration directory without creating or modifying any part of it,
/// and return a descriptor for the leaf.
///
/// The check discovery makes before it reads a registration (R25). Same walk, same
/// ownership and mode rules, no `mkdir` and no `chmod`: a directory that is not there
/// is `NotFound`, which for a caller means "no daemon has ever registered here".
#[cfg(unix)]
fn verify_existing_dir(dir: &Path) -> io::Result<OwnedFd> {
    walk_registration_dir(dir, Missing::Refuse)
}

/// Walk down to the registration directory, creating what is missing and verifying
/// what is not, and return a descriptor for the leaf.
///
#[cfg(unix)]
fn create_and_verify_dir(dir: &Path) -> io::Result<OwnedFd> {
    walk_registration_dir(dir, Missing::Create)
}

/// The walk both of the above are, with one difference: whether a missing component
/// is created or refused.
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
fn walk_registration_dir(dir: &Path, missing: Missing) -> io::Result<OwnedFd> {
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

    if missing == Missing::Create {
        fs::create_dir_all(base)?;
    }
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

        let existed = match missing {
            // Nothing is created on the read path, so whatever is there was made by
            // somebody else and goes through the stricter arm below.
            Missing::Refuse => true,
            Missing::Create => {
                match rustix::fs::mkdirat(&parent, name, Mode::from_bits_truncate(mode)) {
                    Ok(()) => false,
                    Err(Errno::EXIST) => true,
                    Err(errno) => return Err(refuse(&walked, "it could not be created", errno)),
                }
            }
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
    /// No daemon is registered, or nothing answers the socket the registration names.
    Absent,
}

/// The state a registration and a connect attempt together report (R13).
///
/// Finer than [`Discovery`], because a caller deciding whether to *start* a daemon
/// needs a distinction a caller deciding whether to *connect* does not: a daemon that
/// crashed and left artifacts behind, one that is mid-start, and one that is serving
/// are three different situations, and a refusal that names the wrong one sends its
/// reader looking for a daemon that is not there.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DaemonState {
    /// The socket accepted a connection.
    Running(Registration),
    /// The socket is bound and its backlog is full, or the connect timed out. A busy
    /// server is a **live** server: reading this as dead is what starts a second
    /// daemon on a workspace one already owns.
    Saturated(Registration),
    /// A registration exists, nothing answers its socket, and the process that wrote
    /// it is still there: a daemon between taking the lock and serving.
    Starting(Registration),
    /// A registration exists, nothing answers its socket, and its process is gone.
    /// The artifacts are debris — but only the lock proves it, so cleaning up is the
    /// start path's job and not a client's.
    Crashed(Registration),
    /// The connect failed for a reason that says nothing about the owner (a
    /// descriptor limit, a permission the socket's mode should not have had).
    /// Reported apart from `Absent`, because "no daemon" is the one answer that
    /// starts a second one.
    Undecided(Registration),
    /// Nothing is registered, or what is registered cannot be trusted.
    Absent,
}

/// Look for a daemon serving `workspace_root`.
///
/// An unreadable, unparsable or untrustworthy registration is [`Discovery::Absent`]
/// rather than an error: a corrupt file is indistinguishable from no file for every
/// purpose a caller has, and the next daemon to start replaces it.
///
/// Liveness is a connect and never a lock — see the module doc for why a lock query
/// cannot answer this and a lock *acquire* breaks the daemon it is asking about.
pub fn discover(workspace_root: &Path) -> Discovery {
    match daemon_state(workspace_root) {
        // Answering, busy, or unreadable-for-unrelated-reasons: all three mean a
        // client must not start a second daemon.
        DaemonState::Running(registration)
        | DaemonState::Saturated(registration)
        | DaemonState::Undecided(registration) => {
            if registration.contract_version != CONTRACT_VERSION {
                Discovery::VersionMismatch(registration)
            } else {
                Discovery::Live(registration)
            }
        }
        // Mid-start and crashed are both "nothing to connect to yet". The caller
        // starts a daemon, and the lock — not this answer — decides which of the two
        // it turns out to have been.
        DaemonState::Starting(_) | DaemonState::Crashed(_) | DaemonState::Absent => {
            Discovery::Absent
        }
    }
}

/// The state machine behind [`discover`]: read the registration, then connect.
pub fn daemon_state(workspace_root: &Path) -> DaemonState {
    let Some(registration) = read_registration(workspace_root) else {
        return DaemonState::Absent;
    };

    #[cfg(unix)]
    {
        match probe_socket(Path::new(&registration.socket_path)) {
            Liveness::Answering => DaemonState::Running(registration),
            Liveness::Saturated => DaemonState::Saturated(registration),
            Liveness::Undecided => DaemonState::Undecided(registration),
            // Nothing is listening. Whether that is a crash or a start in progress
            // is the one place the pid still earns its syscall — it decides which
            // sentence a refusal prints, and nothing else.
            Liveness::Gone | Liveness::Unanswered => {
                if process_is_alive(registration.pid) {
                    DaemonState::Starting(registration)
                } else {
                    DaemonState::Crashed(registration)
                }
            }
        }
    }
    // Windows named pipes are deferred and `Listener` is Unix-only, so there is no
    // socket to probe and no daemon to find. Deliberate degradation: a client
    // reports no daemon rather than guessing one is there.
    #[cfg(not(unix))]
    {
        let _ = &registration;
        DaemonState::Absent
    }
}

/// Read the registration for `workspace_root`, or report that there is nothing worth
/// connecting to.
///
/// Three refusals before the contents are believed, all of them on the read path
/// because the read path is where a client acts on somebody else's file (R25):
///
/// 1. The registration directory is verified — this user's, 0700, no symlink at any
///    component — and **not** created or repaired. Discovery used to reach that check
///    by accident, through the lock the old probe took.
/// 2. The pid is range-checked, so no value from disk reaches `kill`.
/// 3. The socket path must resolve inside the verified directory. A registration that
///    names a socket somewhere else is a planted one, and connecting to it would hand
///    a local attacker the buffers this client is about to send.
fn read_registration(workspace_root: &Path) -> Option<Registration> {
    let dir = registration_dir();

    #[cfg(unix)]
    if let Err(error) = verify_existing_dir(&dir) {
        // A directory that is not there is the ordinary case — no daemon has ever
        // run for this cache home — and saying so on every poll would bury the
        // refusals that matter.
        if error.kind() != io::ErrorKind::NotFound {
            eprintln!("oxabl: ignoring any daemon registration: {error}");
        }
        return None;
    }

    let path = registration_path(workspace_root);
    let registration =
        serde_json::from_str::<Registration>(&fs::read_to_string(&path).ok()?).ok()?;

    if checked_pid(registration.pid).is_none() {
        eprintln!(
            "oxabl: refusing the daemon registration {}: {} is not a process id. \
             Remove the file.",
            path.display(),
            registration.pid
        );
        return None;
    }

    let socket = Path::new(&registration.socket_path);
    if socket.parent() != Some(dir.as_path()) {
        eprintln!(
            "oxabl: refusing the daemon registration {}: it names the socket {}, \
             which is not inside {}. Not connecting to it. Remove the file, or set \
             XDG_CACHE_HOME to a directory you own.",
            path.display(),
            socket.display(),
            dir.display()
        );
        return None;
    }

    Some(registration)
}

/// What a connect to a registered socket said about its owner (KTD6).
#[cfg(unix)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Liveness {
    /// The connection was accepted, or queued for a listener that exists.
    Answering,
    /// A live listener that could not take it now.
    Saturated,
    /// There is no socket at the path.
    Gone,
    /// The socket file is there and nobody is listening on it.
    Unanswered,
    /// The attempt failed for a reason that is about this process, not the owner.
    Undecided,
}

/// Ask the socket itself whether a daemon is behind it.
///
/// Non-blocking, and that is the whole reason this is not `UnixStream::connect`: a
/// listener whose backlog is full makes a blocking connect wait, so the probe that
/// exists to avoid stalling a client would stall it. Non-blocking turns that case
/// into `EAGAIN`, which is a *statement about the server* — alive and busy.
///
/// The connection is dropped immediately. It reaches the daemon as a real accepted
/// client that disconnects at once, which is why `accept_loop_with` reaps finished
/// client threads on every pass.
#[cfg(unix)]
fn probe_socket(path: &Path) -> Liveness {
    let Ok(address) = rustix::net::SocketAddrUnix::new(path) else {
        // Too long for `sun_path`, or holding a NUL. Nothing could have bound it.
        return Liveness::Gone;
    };
    let socket = match rustix::net::socket_with(
        rustix::net::AddressFamily::UNIX,
        rustix::net::SocketType::STREAM,
        rustix::net::SocketFlags::CLOEXEC | rustix::net::SocketFlags::NONBLOCK,
        None,
    ) {
        Ok(socket) => socket,
        // We could not even make a socket, which says nothing about the daemon.
        Err(_) => return Liveness::Undecided,
    };
    match rustix::net::connect(&socket, &address) {
        Ok(()) => Liveness::Answering,
        Err(errno) => classify_connect_failure(errno),
    }
}

/// Read a failed connect as a statement about the owner of the socket.
///
/// The arms are the whole correctness of the probe, so they are separate from the
/// syscall and named one by one.
#[cfg(unix)]
fn classify_connect_failure(errno: Errno) -> Liveness {
    match errno {
        // Nothing at the path at all.
        Errno::NOENT => Liveness::Gone,
        // The one arm that means "stale artifact": the file exists, and the kernel
        // has no listener to hand the connection to.
        Errno::CONNREFUSED => Liveness::Unanswered,
        // `EAGAIN` (`EWOULDBLOCK` is the same value) is a full accept queue and
        // `ETIMEDOUT` is a server too slow to answer — both are live servers.
        // `EINPROGRESS` means the connection is being set up, which needs a listener
        // to be set up with, and `EINTR` left it in progress too.
        Errno::AGAIN | Errno::TIMEDOUT | Errno::INPROGRESS | Errno::INTR => Liveness::Saturated,
        // Everything else — `EACCES`, `EMFILE`, `ENOMEM` — is about this process.
        // Reported as undecided, never as absent: absent is what starts a rival
        // daemon, and a descriptor limit is no reason to do that.
        _ => Liveness::Undecided,
    }
}

/// Whether something is listening on `socket_path` right now.
///
/// The re-probe the start path owes itself (KTD6). Between a client's connect and
/// its lock another process can take the lock, start a daemon and begin serving, so
/// holding the lock is not on its own a licence to unlink the socket file — the
/// unlink would leave a live daemon unreachable.
///
/// Deliberately generous: anything but a clear "nobody is there" counts as
/// answering, because the caller is about to delete a file and the conservative
/// direction for a delete is to leave it alone.
#[cfg(unix)]
pub fn socket_is_answering(socket_path: &Path) -> bool {
    match probe_socket(socket_path) {
        Liveness::Answering | Liveness::Saturated | Liveness::Undecided => true,
        Liveness::Gone | Liveness::Unanswered => false,
    }
}

/// Remove a socket file whose owner is gone, and refuse to remove anything else.
///
/// Cleanup is a delete, which makes it the operation worth steering, so the unlink is
/// fenced three ways: the path must sit directly inside the verified registration
/// directory, the entry is looked up **relative to that directory's descriptor**
/// rather than by path again, and it must be a socket — not a file, not a directory,
/// and not a symlink to somebody's data.
///
/// A path that is already gone is success: the goal is "no stale socket remains".
#[cfg(unix)]
pub fn remove_stale_socket(socket_path: &Path) -> io::Result<()> {
    let dir = registration_dir();
    let name = match (socket_path.parent(), socket_path.file_name()) {
        (Some(parent), Some(name)) if parent == dir.as_path() => name,
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!(
                    "refusing to remove {}: only a socket directly inside {} is the \
                     daemon's to clean up.",
                    socket_path.display(),
                    dir.display()
                ),
            ));
        }
    };
    let parent = verify_existing_dir(&dir)?;

    let stat = match rustix::fs::statat(&parent, name, rustix::fs::AtFlags::SYMLINK_NOFOLLOW) {
        Ok(stat) => stat,
        Err(Errno::NOENT) => return Ok(()),
        Err(errno) => return Err(refuse(socket_path, "it could not be inspected", errno)),
    };
    if rustix::fs::FileType::from_raw_mode(stat.st_mode) != rustix::fs::FileType::Socket {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "refusing to remove {}: it is not a socket, and the daemon only \
                 cleans up a socket left by a daemon that died. Move it aside.",
                socket_path.display()
            ),
        ));
    }
    rustix::fs::unlinkat(&parent, name, rustix::fs::AtFlags::empty())
        .map_err(|errno| refuse(socket_path, "it could not be removed", errno))
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
/// The pairing a real daemon has: [`Listener::bind`](crate::Listener::bind) holds the
/// lock for its whole life, so exactly one daemon per root reaches the bind. The lock
/// is not what [`discover`] reads — a registration is live because its socket answers
/// — but it is what stops two daemons from both having a socket to answer with.
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

/// A process id read from disk, or `None` when the number cannot be one (R15).
///
/// `kill` overloads the sign, and a `u32` from a JSON file can carry values no pid
/// ever has. `0` targets our own process group; a negative value targets a process
/// group; and `u32::MAX as i32` is `-1`, which targets *every* process this user may
/// signal and therefore succeeds — so an unchecked `u32::MAX` reported a daemon alive
/// unconditionally. The range check is what keeps the sign out of the syscall: the
/// value must be strictly positive and must fit the signed type `kill` actually
/// takes.
fn checked_pid(pid: u32) -> Option<i32> {
    match i32::try_from(pid) {
        Ok(pid) if pid > 0 => Some(pid),
        _ => None,
    }
}

/// Whether a process with `pid` exists.
///
/// `kill(pid, 0)` is the portable question on Unix: it performs the permission check
/// and the existence check and sends nothing.
///
/// **Never the liveness answer.** Liveness is the connect (KTD6); this decides only
/// whether a registration nobody answers belongs to a daemon that is still starting
/// or to one that crashed, which is a difference in the sentence a refusal prints.
/// `EPERM` counts as alive because a process owned by another user is a process.
#[cfg(unix)]
fn process_is_alive(pid: u32) -> bool {
    let Some(pid) = checked_pid(pid) else {
        return false;
    };
    // SAFETY: `kill` with signal 0 sends no signal. It only reports whether the
    // process exists and whether we could signal it, and cannot affect it. `pid` is
    // range-checked strictly positive above, so this cannot address a process group.
    let result = unsafe { libc_kill(pid, 0) };
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

    /// Bind the socket a real daemon for `root` would bind, so a probe of its
    /// registration finds something listening.
    ///
    /// A listener with nothing accepting still answers: the kernel completes the
    /// connection into the backlog, which is exactly the state a daemon is in between
    /// `bind` and its first `accept`.
    #[cfg(unix)]
    fn serving(root: &Path) -> std::os::unix::net::UnixListener {
        ensure_registration_dir().expect("the registration directory");
        let socket = socket_path_for(root);
        let _ = fs::remove_file(&socket);
        std::os::unix::net::UnixListener::bind(&socket).expect("a listening socket")
    }

    #[test]
    fn a_live_registration_is_found() {
        with_cache_home(|_| {
            let root = Path::new("/proj/alpha");
            let socket = socket_path_for(root);
            // Alive by construction: something is listening on the socket the
            // registration names, which is what a real daemon has.
            let _listening = serving(root);
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
    /// whose socket nobody answers is absent, so the client starts a daemon instead of
    /// connecting to a socket nobody holds.
    #[test]
    fn a_registration_nobody_answers_is_absent() {
        with_cache_home(|_| {
            let root = Path::new("/proj/crashed");
            register(root, &socket_path_for(root), std::process::id())
                .expect("registration writes");
            assert_eq!(
                discover(root),
                Discovery::Absent,
                "a registration with no listener behind it is not a daemon"
            );
        });
    }

    /// A number from a file is not a pid until it has been checked (R15).
    ///
    /// `kill` overloads the sign, and `u32::MAX as i32` is `-1`: unchecked, it asks
    /// "may I signal every process I own", which succeeds — so the worst possible
    /// value reported a daemon alive. The old dead-pid sentinel `u32::MAX - 1` passed
    /// only by luck, because it wraps to `-2` and process group 2 happens not to
    /// exist on this machine.
    #[cfg(unix)]
    #[test]
    fn a_number_that_cannot_be_a_process_id_is_never_reported_alive() {
        for pid in [0, u32::MAX, u32::MAX - 1, i32::MAX as u32 + 1] {
            assert!(
                checked_pid(pid).is_none(),
                "{pid} is not a process id and must be refused before `kill` sees it"
            );
            assert!(
                !process_is_alive(pid),
                "{pid} must never be reported alive: it addresses a process group, \
                 not a process"
            );
        }
        assert!(
            process_is_alive(std::process::id()),
            "this process is alive, so the check must not refuse every value"
        );
    }

    /// The range check runs before the registration is believed, so a pid no process
    /// can have makes the whole file untrustworthy — even with a live socket behind
    /// it.
    #[cfg(unix)]
    #[test]
    fn a_registration_whose_process_id_is_out_of_range_is_absent() {
        with_cache_home(|_| {
            let root = Path::new("/proj/impossible-pid");
            let _listening = serving(root);
            register(root, &socket_path_for(root), u32::MAX).expect("registration writes");

            assert_eq!(
                discover(root),
                Discovery::Absent,
                "a pid that cannot exist makes the registration untrustworthy"
            );
        });
    }

    /// A socket path outside the verified directory is a planted registration: a local
    /// attacker's socket, which a client would otherwise hand its buffers to.
    #[cfg(unix)]
    #[test]
    fn a_registration_naming_a_socket_outside_the_directory_is_refused() {
        with_cache_home(|_| {
            let elsewhere = tempfile::tempdir().expect("somebody else's directory");
            let planted = elsewhere.path().join("theirs.sock");
            let listening = std::os::unix::net::UnixListener::bind(&planted)
                .expect("the attacker's listening socket");
            listening
                .set_nonblocking(true)
                .expect("so the accept below cannot block");

            let root = Path::new("/proj/planted");
            register(root, &planted, std::process::id()).expect("the planted registration");

            assert_eq!(
                discover(root),
                Discovery::Absent,
                "a socket outside the verified directory must not be trusted"
            );
            assert_eq!(
                listening.accept().map(|_| ()).map_err(|e| e.kind()),
                Err(io::ErrorKind::WouldBlock),
                "the refusal must come before any connection is attempted"
            );
        });
    }

    /// The stale-artifact arm: a socket file whose daemon is gone refuses the
    /// connection, which is the one connect outcome that means "debris".
    #[cfg(unix)]
    #[test]
    fn a_crashed_daemons_socket_is_unanswered_and_then_removed() {
        with_cache_home(|_| {
            let root = Path::new("/proj/left-behind");
            let socket = socket_path_for(root);
            // Dropping a `UnixListener` closes it and leaves the file, which is what a
            // killed daemon leaves behind.
            drop(serving(root));
            assert!(socket.exists(), "the socket file outlives its listener");
            assert_eq!(
                probe_socket(&socket),
                Liveness::Unanswered,
                "a socket with no listener refuses the connection"
            );

            register(root, &socket, std::process::id()).expect("the crashed registration");
            assert_eq!(discover(root), Discovery::Absent);

            remove_stale_socket(&socket).expect("debris is removable");
            assert!(!socket.exists(), "the stale socket is cleaned up");
            assert_eq!(
                probe_socket(&socket),
                Liveness::Gone,
                "and then there is nothing at the path at all"
            );
        });
    }

    /// Cleanup is a delete, so it must not be steerable. Anything that is not a
    /// socket is refused and left alone, however it got there.
    #[cfg(unix)]
    #[test]
    fn stale_socket_cleanup_refuses_anything_that_is_not_a_socket() {
        with_cache_home(|_| {
            let root = Path::new("/proj/not-a-socket");
            ensure_registration_dir().expect("the directory");
            let socket = socket_path_for(root);
            fs::write(&socket, b"precious").expect("a regular file where the socket goes");

            let error = remove_stale_socket(&socket).expect_err("a file is not debris");
            assert!(
                error.to_string().contains("not a socket"),
                "the message must say what it refused: {error}"
            );
            assert_eq!(
                fs::read(&socket).expect("still there"),
                b"precious",
                "a refused path must not be removed"
            );
        });
    }

    /// And a path outside the verified directory is refused before it is even
    /// inspected, so a planted registration cannot aim the cleanup at other data.
    #[cfg(unix)]
    #[test]
    fn stale_socket_cleanup_refuses_a_path_outside_the_registration_directory() {
        with_cache_home(|_| {
            let elsewhere = tempfile::tempdir().expect("somebody else's directory");
            let target = elsewhere.path().join("theirs.sock");
            let _listening = std::os::unix::net::UnixListener::bind(&target).expect("their socket");

            let error =
                remove_stale_socket(&target).expect_err("a foreign path is not ours to remove");
            assert_eq!(error.kind(), io::ErrorKind::InvalidInput);
            assert!(target.exists(), "a refused path must not be removed");
        });
    }

    /// A full accept queue is a **live** server. Reading it as dead is the classic
    /// failure in this pattern: the client respawns onto a workspace a daemon owns.
    #[cfg(unix)]
    #[test]
    fn a_saturated_daemon_is_reported_running_rather_than_respawned() {
        with_cache_home(|_| {
            let root = Path::new("/proj/busy");
            ensure_registration_dir().expect("the directory");
            let socket = socket_path_for(root);

            let listening = rustix::net::socket_with(
                rustix::net::AddressFamily::UNIX,
                rustix::net::SocketType::STREAM,
                rustix::net::SocketFlags::CLOEXEC,
                None,
            )
            .expect("a socket");
            let address = rustix::net::SocketAddrUnix::new(&socket).expect("an address");
            rustix::net::bind(&listening, &address).expect("bind");
            // The shortest queue the kernel will give, so it fills in a few connects.
            rustix::net::listen(&listening, 1).expect("listen");
            register(root, &socket, std::process::id()).expect("registration writes");

            // Each probe leaves a queued connection behind, so probing is also how the
            // queue is filled. Nothing accepts, so it never drains.
            let mut saturated = false;
            for _ in 0..256 {
                match probe_socket(&socket) {
                    Liveness::Answering => continue,
                    Liveness::Saturated => {
                        saturated = true;
                        break;
                    }
                    other => panic!("a bound socket must not report {other:?}"),
                }
            }
            assert!(saturated, "the accept queue must have filled up");
            assert!(
                matches!(discover(root), Discovery::Live(_)),
                "a busy daemon is a running daemon"
            );
        });
    }

    /// The inverted arm this unit removed: the old probe returned `false` on an
    /// unopenable lock file, and the caller's `||` short-circuited, so a **live**
    /// daemon was reported absent — the opposite of what the comment claimed. Nothing
    /// on the read path opens the lock now, so the answer comes from the socket.
    #[cfg(unix)]
    #[test]
    fn a_lock_file_that_cannot_be_opened_does_not_hide_a_live_daemon() {
        with_cache_home(|_| {
            let root = Path::new("/proj/locked-out");
            let _listening = serving(root);
            register(root, &socket_path_for(root), std::process::id())
                .expect("registration writes");

            let lock = lock_path_for(root);
            fs::write(&lock, b"").expect("the lock file");
            fs::set_permissions(&lock, fs::Permissions::from_mode(0o000))
                .expect("a lock nothing can open");

            assert!(
                matches!(discover(root), Discovery::Live(_)),
                "an unopenable lock file must not turn a live daemon into an absent one"
            );

            fs::set_permissions(&lock, fs::Permissions::from_mode(0o600)).expect("restore");
        });
    }

    /// The read path answers a question; it must not make a directory to answer it
    /// (R25). Creating one here is also what used to be the *only* place a client
    /// checked the directory's ownership, which is why the check is now explicit.
    #[cfg(unix)]
    #[test]
    fn discovery_creates_nothing() {
        with_cache_home(|cache| {
            assert_eq!(discover(Path::new("/proj/nothing-here")), Discovery::Absent);
            assert!(
                !cache.join("oxabl").exists(),
                "a client that finds no daemon must leave no directory behind"
            );
        });
    }

    /// A registration directory this user did not make at 0700 is refused on the read
    /// path too, and refused without touching it.
    ///
    /// The wrong-*mode* case is the testable half. A directory owned by another user
    /// cannot be created without privileges, so that arm of the same check is
    /// unreachable from a test — stated rather than faked.
    #[cfg(unix)]
    #[test]
    fn a_registration_directory_with_a_wider_mode_is_refused_on_the_read_path() {
        with_cache_home(|cache| {
            let root = Path::new("/proj/loose-directory");
            let dir = cache.join("oxabl").join("daemon");
            let _listening = serving(root);
            register(root, &socket_path_for(root), std::process::id())
                .expect("registration writes");
            // Loosened after the fact, the way something that is not this daemon
            // would have left it.
            fs::set_permissions(&dir, fs::Permissions::from_mode(0o755)).expect("widen it");

            assert_eq!(
                discover(root),
                Discovery::Absent,
                "a directory the daemon would not have created is not trusted to hold \
                 a registration"
            );
            assert_eq!(
                fs::metadata(&dir).expect("the directory").mode() & 0o777,
                0o755,
                "a refused directory must not be repaired by a reader"
            );

            fs::set_permissions(&dir, fs::Permissions::from_mode(0o700)).expect("restore");
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
            // A daemon of the wrong contract is still a daemon: it answers.
            let _listening = serving(root);

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
            let _alpha_listening = serving(alpha);
            let _beta_listening = serving(beta);
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

    /// A registration is replaced rather than merged, so the crashed daemon's pid does
    /// not survive into the live one's file.
    ///
    /// Both spellings name the same socket, because the socket has to live inside the
    /// verified registration directory — a registration naming anything else is
    /// refused. The pid is what tells the two apart.
    #[cfg(unix)]
    #[test]
    fn registering_replaces_a_previous_registration() {
        with_cache_home(|_| {
            let root = Path::new("/proj/replaced");
            let socket = socket_path_for(root);
            register(root, &socket, 999_999).expect("first");

            let _listening = serving(root);
            let (_path, _lock) = register_locked(root, &socket, std::process::id())
                .expect("second")
                .expect("nothing holds it");

            match discover(root) {
                Discovery::Live(found) => {
                    assert_eq!(found.pid, std::process::id());
                    assert_eq!(found.socket_path, socket.to_string_lossy());
                }
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
