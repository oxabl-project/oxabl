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
//! * A daemon that is alive but wedged still reads as **live**, and that is the
//!   answer the caller needs: a wedged accept loop leaves a bound socket, so the
//!   kernel completes the connection into the backlog and the probe reports
//!   `Answering` until the backlog fills, and `Saturated` after it does. Both mean
//!   "do not start a second one", so no timeout has to be tuned. What the probe does
//!   *not* do is distinguish wedged from working — a hung daemon reads as a running
//!   one, and it is the handshake, not discovery, that would notice.
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
/// 0700, and the run that created it is the run that refuses it. That is a loud, named
/// failure with the mode in the message, which is the right way for an unusable umask
/// to surface — and it is why the leaf is verified whether it was found or just made.
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
/// The prefix above [`OWNED_COMPONENTS`] is resolved by path, symlinks included, and
/// on the create path it is also *made* by path, by a `create_dir_all` that runs
/// before any descriptor is opened. Both are deliberate: the prefix is the location
/// the user named, and refusing — or refusing to create through — a symlink there
/// would refuse legitimate setups, a `~/.cache` pointing at another volume or a
/// symlinked `/home`. So the prefix is not verified before it is created; it is
/// verified after, on the descriptor the path resolved to, and nothing is created
/// *inside* it until that check has passed.
///
/// What that costs is bounded by what the prefix is. Creating a directory through a
/// symlink a local attacker planted would put the prefix somewhere we did not choose
/// — but the leaf inside it is then created 0700 below a verified descriptor, and a
/// prefix whose owner or mode is wrong is refused before the leaf is touched. The
/// branch where a hostile prefix is plausible at all is the shared-temp fallback, and
/// there `create_dir_all` creates nothing: its base is the temp directory, which
/// already exists.
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

        match missing {
            // Nothing is created on the read path: whatever is there was made by
            // somebody else, and the checks below are the same either way.
            Missing::Refuse => {}
            Missing::Create => {
                match rustix::fs::mkdirat(&parent, name, Mode::from_bits_truncate(mode)) {
                    Ok(()) | Err(Errno::EXIST) => {}
                    Err(errno) => return Err(refuse(&walked, "it could not be created", errno)),
                }
            }
        }
        parent = open_dir_below(&parent, name, &walked)?;

        // The leaf is checked strictly whether we just created it or found it. Asking
        // only when it already existed made a create the one path that never checked:
        // `mkdirat` applies `mode & ~umask`, so a umask stripping owner-execute turns
        // the 0700 request into a 0600 directory the owner cannot traverse. The
        // parent-grade check accepts that — it is neither world-writable nor foreign —
        // so the creating run carried on, the lock-file open below it failed with a
        // bare `EACCES`, and every later run refused the directory with the named
        // message the first run should have printed.
        if leaf {
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
///
/// # Why a group-writable parent is accepted rather than refused
///
/// The mask is `0o002`, so `drwxrwx--- user user` passes. That is a residual, and it is
/// accepted knowingly: a member of the group can rename the leaf out from under the
/// daemon exactly as a world-writable parent's users could. It is accepted because the
/// check cannot tell the two apart. On every distribution that uses per-user private
/// groups, `umask 002` produces a group-writable home directory whose group has one
/// member — the owner — and `st_gid` does not say whether a group is private or
/// shared. Widening the mask would therefore refuse an ordinary desktop account to
/// close a hole only a shared-group account has. A user in the second position closes
/// it by pointing `XDG_CACHE_HOME` at a directory whose group is not shared; the leaf's
/// own 0700 still keeps the group out of the socket and the registration.
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

/// Verify the leaf directory: this user's, and exactly 0700.
///
/// Applied to every leaf, one this call just created as much as one it found. A
/// directory that is wider was made by something that is not this daemon; one that is
/// narrower is a directory the owner cannot traverse, which `mkdirat` produces from a
/// 0700 request under a umask that strips owner bits. Both are refused here, so the
/// failure is named once with the mode in it rather than surfacing later as an
/// `EACCES` from opening the lock file.
///
/// Refused rather than repaired (KTD8). A `chmod` would turn "somebody else's
/// directory" into "somebody else's directory that now looks like ours", and on a leaf
/// replaced by a symlink it would tighten the attacker's directory and carry on.
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
                "refusing to use {}: it has mode {mode:04o} owned by uid {}, and the \
                 daemon requires mode 0700 owned by this user (uid {uid}). It is \
                 refused rather than changed, because the daemon never creates it any \
                 other way. Remove it, or set XDG_CACHE_HOME to a directory you own — \
                 and if the mode is narrower than 0700, loosen a umask that is \
                 stripping the owner bits the daemon needs.",
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
    /// The question could not be answered, and the reason says nothing about whether a
    /// daemon is there: the registration could not be read for a reason that is not
    /// "it is not there", or the probe could not be made at all. Carries that reason,
    /// so a client that eventually gives up names the cause instead of a timeout.
    ///
    /// **Not** a synonym for either neighbour. Reported as `Absent` it starts a rival
    /// daemon on a workspace that may already have one; reported as `Live` it sends the
    /// client into a `connect` that fails, and the editor's server dies without any
    /// daemon ever being started. The only correct response is to keep polling, so
    /// this variant exists to say exactly that.
    Undecided(String),
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
    /// A daemon is mid-start: the registration on disk is not yet the registration the
    /// daemon that will serve this root is going to write.
    ///
    /// Two shapes reach here. The plain one is a registration whose socket nobody
    /// answers and whose writer is still alive — a daemon between taking the lock and
    /// serving. The other is a socket that *does* answer while the pid in the
    /// registration is gone, which is a stale body being read next to a fresh daemon's
    /// socket: a killed daemon's file still on disk, and its replacement bound but not
    /// yet registered. Both mean "keep polling", and neither may be reported as
    /// `Running`, because the registration a client would act on — its contract
    /// version above all — belongs to the dead process rather than to the one
    /// answering.
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
    /// The registration could not be read for a reason that is not "there is none":
    /// a directory this daemon would not have created, or an unreadable file. Carries
    /// the refusal, because it is the sentence a client should print when it gives up.
    Unreadable(String),
    /// Nothing is registered, or what is registered cannot be trusted.
    Absent,
}

/// Look for a daemon serving `workspace_root`.
///
/// An unparsable or untrustworthy registration is [`Discovery::Absent`] rather than an
/// error: a corrupt file is indistinguishable from no file for every purpose a caller
/// has, and the next daemon to start replaces it. A registration that could not be
/// *read* is different, and it is [`Discovery::Undecided`] — see that variant for why
/// collapsing the two starts a second daemon over a permission problem.
///
/// Liveness is a connect and never a lock — see the module doc for why a lock query
/// cannot answer this and a lock *acquire* breaks the daemon it is asking about.
pub fn discover(workspace_root: &Path) -> Discovery {
    classify(daemon_state(workspace_root))
}

/// The mapping from the finer state to the answer a client acts on.
///
/// Split from [`discover`] because it is the part that decides whether a client starts
/// a rival daemon, connects, or waits — and a mapping that needs a real daemon, a real
/// socket and a real descriptor limit to exercise is a mapping that goes untested.
/// Given a state, it takes no syscall to check.
fn classify(state: DaemonState) -> Discovery {
    match state {
        // Answering or busy: both mean a client must not start a second daemon, and
        // both mean the registration in hand belongs to the process answering.
        DaemonState::Running(registration) | DaemonState::Saturated(registration) => {
            if registration.contract_version != CONTRACT_VERSION {
                Discovery::VersionMismatch(registration)
            } else {
                Discovery::Live(registration)
            }
        }
        // A probe that never happened is not a daemon that is not there. Reported as
        // live it used to send the client into a `connect` that fails — the socket may
        // be as stale as the registration — so it keeps polling instead.
        DaemonState::Undecided(registration) => Discovery::Undecided(format!(
            "the daemon registered at {} could not be probed on this attempt",
            registration.socket_path
        )),
        DaemonState::Unreadable(reason) => Discovery::Undecided(reason),
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
    let registration = match read_registration(workspace_root) {
        RegistrationRead::Found(registration) => registration,
        RegistrationRead::Refused(reason) => return DaemonState::Unreadable(reason),
        RegistrationRead::Absent => return DaemonState::Absent,
    };

    #[cfg(unix)]
    {
        // A socket that answers proves something is listening. It does not prove that
        // the something wrote the registration just read, and the two come apart in an
        // ordinary upgrade: a `SIGKILL`ed daemon leaves a file naming a dead pid and an
        // older contract, its replacement binds the socket *before* it registers, and a
        // poll landing in that window would pair the new socket with the old body. That
        // pairing reported `Running`, which a client turns into a contract mismatch and
        // exits on — a live daemon of the right version killing the editor's server. So
        // a registration whose writer is gone is never `Running`, whatever the socket
        // says: it is a start in progress, and the client polls until the fresh
        // registration lands.
        let answered = probe_socket(Path::new(&registration.socket_path));
        match answered {
            Liveness::Answering | Liveness::Saturated if !process_is_alive(registration.pid) => {
                DaemonState::Starting(registration)
            }
            Liveness::Answering => DaemonState::Running(registration),
            Liveness::Saturated => DaemonState::Saturated(registration),
            Liveness::Undecided(_) => DaemonState::Undecided(registration),
            // Nothing is listening. Whether that is a crash or a start in progress
            // is decided by the same pid question as above — it chooses which sentence
            // a refusal prints, and nothing else.
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

/// What the read path found on disk.
///
/// `Absent` and `Refused` are kept apart because only one of them may start a daemon.
/// "There is no registration" is the ordinary cold start; "the registration is there
/// and something stopped us reading it" is a permission or resource problem, and a
/// client that reads it as absent spawns a rival daemon that will hit the same problem.
enum RegistrationRead {
    /// A registration that passed every check below.
    Found(Registration),
    /// Nothing is registered, or what is registered is not trustworthy: a corrupt
    /// body, an impossible pid, a socket outside the verified directory.
    Absent,
    /// The registration could not be read, for a reason that is not "it is not there".
    /// Carries the refusal, for a caller that has to explain why it gave up.
    Refused(String),
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
///
/// Refusals 2 and 3 are `Absent`: the file is there and is not to be believed, and the
/// next daemon to start replaces it. A directory refusal, and any read error that is
/// not `NotFound`, are `Refused` instead — they say nothing about whether a daemon
/// exists, and answering "absent" to them starts a second daemon over a mode bit.
fn read_registration(workspace_root: &Path) -> RegistrationRead {
    let dir = registration_dir();

    #[cfg(unix)]
    if let Err(error) = verify_existing_dir(&dir) {
        // A directory that is not there is the ordinary case — no daemon has ever
        // run for this cache home — and saying so on every poll would bury the
        // refusals that matter.
        if error.kind() == io::ErrorKind::NotFound {
            return RegistrationRead::Absent;
        }
        let refusal = format!("ignoring any daemon registration: {error}");
        report_once(&refusal);
        return RegistrationRead::Refused(refusal);
    }

    let path = registration_path(workspace_root);
    let body = match fs::read_to_string(&path) {
        Ok(body) => body,
        Err(error) if error.kind() == io::ErrorKind::NotFound => {
            return RegistrationRead::Absent;
        }
        // A descriptor limit, a permission the 0600 mode should have granted, an I/O
        // error: none of them is evidence that no daemon is running.
        Err(error) => {
            let refusal = format!(
                "the daemon registration {} could not be read: {error}",
                path.display()
            );
            report_once(&refusal);
            return RegistrationRead::Refused(refusal);
        }
    };
    // A body that does not parse is a file no caller can use, and it is replaced by
    // the next daemon to start — so it is absent rather than undecided.
    let Ok(registration) = serde_json::from_str::<Registration>(&body) else {
        return RegistrationRead::Absent;
    };

    if checked_pid(registration.pid).is_none() {
        report_once(&format!(
            "refusing the daemon registration {}: {} is not a process id. Remove the \
             file.",
            path.display(),
            registration.pid
        ));
        return RegistrationRead::Absent;
    }

    let socket = Path::new(&registration.socket_path);
    if socket.parent() != Some(dir.as_path()) {
        report_once(&format!(
            "refusing the daemon registration {}: it names the socket {}, which is not \
             inside {}. Not connecting to it. Remove the file, or set XDG_CACHE_HOME to \
             a directory you own.",
            path.display(),
            socket.display(),
            dir.display()
        ));
        return RegistrationRead::Absent;
    }

    RegistrationRead::Found(registration)
}

/// Print a refusal to stderr the first time this process has it to say.
///
/// The read path is polled, not called: a client asks up to a hundred times at 20ms
/// while it waits for a daemon, so an unconditional `eprintln!` turns one persistently
/// refused directory into a hundred near-identical lines in two seconds, and buries the
/// one line that mattered. Latching keys on the whole message, so a *different* refusal
/// still prints — the noise this suppresses is repetition, not detail.
///
/// The set is bounded by the number of distinct refusals a process can produce, which
/// is bounded by the workspace roots it looks at.
///
/// Returns whether it printed, so the latch itself is a property a test can assert
/// rather than something to be read off stderr.
fn report_once(message: &str) -> bool {
    use std::collections::HashSet;
    use std::sync::{Mutex, OnceLock};

    static REPORTED: OnceLock<Mutex<HashSet<String>>> = OnceLock::new();
    let reported = REPORTED.get_or_init(|| Mutex::new(HashSet::new()));
    let mut reported = reported
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    if reported.insert(message.to_owned()) {
        eprintln!("oxabl: {message}");
        return true;
    }
    false
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
    /// The attempt failed for a reason that is about this process, not the owner. The
    /// errno is carried because it is the only thing a caller can tell the user: the
    /// refusal it prints has to name a descriptor limit as a descriptor limit rather
    /// than as a claim about the socket.
    Undecided(Errno),
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
        // We could not even make a socket, which says nothing about the daemon — and
        // in particular says nothing about whether anything exists at `path`.
        Err(errno) => return Liveness::Undecided(errno),
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
        _ => Liveness::Undecided(errno),
    }
}

/// What the start path found at a registered socket path, for the one decision that
/// needs more than "leave it alone or not".
#[cfg(unix)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SocketOwner {
    /// Something is listening, or is live enough to be listening. Refuse; do not
    /// unlink.
    Answering,
    /// Nothing is listening on a path that may or may not exist. The file, if there is
    /// one, is a dead daemon's debris.
    Debris,
    /// The probe never reached a verdict, because making the socket or the connect
    /// failed for a reason about this process. Leave the path alone, and say so with
    /// the errno rather than claiming anything about what is there.
    Unprobeable(Errno),
}

/// Ask what owns `socket_path` before the start path decides whether to unlink it.
///
/// The re-probe the start path owes itself (KTD6). Between a client's connect and
/// its lock another process can take the lock, start a daemon and begin serving, so
/// holding the lock is not on its own a licence to unlink the socket file — the
/// unlink would leave a live daemon unreachable.
///
/// Three answers rather than a boolean, and that is the fix for a real refusal. The
/// old `socket_is_answering` folded "could not probe" in with "is answering", which is
/// the right generosity for the *delete* — the conservative direction for a delete is
/// to leave the file alone — but it made the caller's refusal a lie: `probe_socket`
/// reports `Undecided` when socket *creation* fails, before any connect, so a low
/// `RLIMIT_NOFILE` made the daemon refuse to start with "something is already listening
/// on this path" for a path that may not exist at all. The generosity is kept, in
/// [`SocketOwner::Unprobeable`]; only the sentence is now the one the caller can
/// defend.
#[cfg(unix)]
pub fn socket_owner(socket_path: &Path) -> SocketOwner {
    match probe_socket(socket_path) {
        Liveness::Answering | Liveness::Saturated => SocketOwner::Answering,
        Liveness::Gone | Liveness::Unanswered => SocketOwner::Debris,
        Liveness::Undecided(errno) => SocketOwner::Unprobeable(errno),
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
/// **Call this under the root lock.** Every caller in this crate does, and
/// [`sweep_stale_staging`] relies on it: it removes this process's own staging file
/// without checking whether a writer still has it open, which is only correct while one
/// process has at most one same-root registration in flight. A caller outside the lock
/// must serialise same-root writers itself.
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
/// Cleaning up after a writer that died is a separate job, and a pid-agnostic one:
/// see [`sweep_stale_staging`].
///
/// # Why a path that is not UTF-8 is refused rather than converted (R14)
///
/// Both paths used to be published through `to_string_lossy`. The bind and the lock
/// use the real bytes, so under a non-UTF-8 `XDG_CACHE_HOME` the daemon listened on
/// one path and advertised another: every client then reported connecting to a socket
/// that was never named that, and no restart could fix it, because the mangling is
/// deterministic. `Registration` carries `String`, so the two choices are refuse at
/// this boundary or change the wire to carry bytes. It refuses: the failure is named
/// once, at the moment the daemon would otherwise publish a lie, and the alternative
/// buys a shape change to the protocol for a locale nobody has asked for. A user in
/// that position gets a message naming the path and the fix.
pub fn register(workspace_root: &Path, socket_path: &Path, pid: u32) -> io::Result<PathBuf> {
    let path = registration_path(workspace_root);
    ensure_registration_dir()?;
    let registration = Registration {
        pid,
        socket_path: advertisable(socket_path, "the daemon socket")?,
        contract_version: CONTRACT_VERSION,
        workspace_root: advertisable(workspace_root, "the workspace root")?,
    };
    let body = serde_json::to_string_pretty(&registration)
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?;

    sweep_stale_staging(&path);
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

/// A path as a client will read it, or a refusal naming what could not be published.
///
/// See [`register`] for why a lossy conversion is not on the menu.
fn advertisable(path: &Path, what: &str) -> io::Result<String> {
    path.to_str().map(str::to_owned).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "refusing to register {} for {what}: the path is not valid UTF-8, and a \
                 registration carries text — publishing a converted path would \
                 advertise a socket nobody bound. Set XDG_CACHE_HOME, and name the \
                 workspace, with paths that are valid UTF-8.",
                path.display()
            ),
        )
    })
}

/// Remove staging files for `path` that no writer will ever finish.
///
/// Cleanup that does not depend on who is doing it. The sweep used to be a
/// remove-if-exists on *this* writer's staging path, so a daemon killed between the
/// write and the rename left a `…json.<its pid>.tmp` that no later run — running under
/// a different pid — would ever look at. One dead start therefore leaked one file per
/// workspace root, permanently, in a directory the daemon otherwise keeps to three
/// entries.
///
/// A live writer's staging file is left alone. Removing one would not corrupt anything
/// published — `rename` is what publishes, and the other writer's open descriptor
/// keeps writing to an unlinked inode — but its rename would then fail with `ENOENT`
/// and a daemon that was starting normally would report an error it did not earn. So
/// the pid in the name is read for exactly one question: is that process still there.
/// Two cases sweep: a pid no live process has, and this process's own, whose leftover
/// belongs to an earlier registration by this same daemon.
///
/// # The invariant the own-pid arm depends on
///
/// This process's own staging file is removed **unconditionally**, without asking
/// whether a writer is using it, and that is only safe because one process never has
/// two registrations for one root in flight at once: [`register`] is reached under the
/// root lock — through [`register_locked`], or from `Listener::bind`, which holds the
/// lock for the listener's whole life — and the lock admits one holder per root. Two
/// concurrent `register` calls for one root inside one process would break it: the
/// second sweep would delete the first's staging file, and the first's `rename` would
/// then fail with `ENOENT` and refuse a start that was going to succeed.
///
/// `register` is `pub`, so that invariant is stated here rather than assumed. A caller
/// that wants to register outside the lock must serialise same-root writers itself, or
/// this arm has to learn to leave a file whose writer is *this* process alone — which
/// costs a second piece of per-writer identity in the name, because a pid is no longer
/// enough to tell "mine, finished" from "mine, in progress".
///
/// Best-effort, and deliberately not fatal. The goal is that debris does not
/// accumulate; a debris file that could not be removed must not stop the registration
/// this call is here to write. Pid reuse can hand a stale pid to an unrelated live
/// process, which leaves one file behind until the next sweep after that process
/// exits — a leak the old code had unconditionally.
fn sweep_stale_staging(path: &Path) {
    let (Some(dir), Some(name)) = (path.parent(), path.file_name()) else {
        return;
    };
    // `<stem>.json` + `.` + `<pid>` + `.tmp`, which is what `register` stages under.
    let prefix = format!("{}.", name.to_string_lossy());
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let candidate = entry.file_name();
        let Some(pid) = candidate
            .to_str()
            .and_then(|candidate| candidate.strip_prefix(&prefix))
            .and_then(|rest| rest.strip_suffix(".tmp"))
            .and_then(|pid| pid.parse::<u32>().ok())
        else {
            continue;
        };
        if pid != std::process::id() && process_is_alive(pid) {
            continue;
        }
        let _ = fs::remove_file(dir.join(&candidate));
    }
}

/// Write `body` to `path`, readable by this user only.
///
/// `fs::write` cannot set a mode, so the file would exist world-readable for the
/// moment before any chmod. Opening with the mode closes that window. `create_new`
/// makes a colliding staging path an error rather than a silent overwrite — which is
/// safe precisely because the name above carries the writer's pid, and because
/// [`sweep_stale_staging`] has already removed this writer's own leftover.
fn write_private(path: &Path, body: &[u8]) -> io::Result<()> {
    use std::io::Write;

    let mut options = fs::OpenOptions::new();
    options.write(true).create_new(true);
    #[cfg(unix)]
    options.mode(0o600);

    let mut file = options.open(path)?;
    file.write_all(body)?;
    file.sync_all()
}

/// Take the root lock and write the registration, returning the guard.
///
/// The pairing a real daemon has: [`Listener::bind`](crate::Listener::bind) holds the
/// lock for its whole life, so exactly one daemon per root reaches the bind. The lock
/// is not what [`discover`] reads — a registration is live because its socket answers
/// *and* the process that wrote it is still there — but it is what stops two daemons
/// from both having a socket to answer with.
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
pub fn socket_path_for(workspace_root: &Path) -> io::Result<PathBuf> {
    sibling_of_registration(&registration_path(workspace_root), ".sock")
}

/// The path beside a registration that ends in `extension` instead of `.json`.
///
/// The one place the `.json` invariant is checked, and it is checked rather than
/// asserted (R14). The two accessors used to read the name, `debug_assert!` that it
/// ended in `.json`, and then subtract five bytes from its length. A name that does
/// not end in `.json` therefore panicked on the subtraction in a debug build — in code
/// whose `unwrap_or_default` was chosen to avoid panicking — and in a release build,
/// where the workspace leaves overflow checks off, did something worse than panic: the
/// difference wrapped to `usize::MAX`, `String::truncate` treats a length past the end
/// as a no-op, and the accessor returned the bare `.sock` and `.lock` names. Every
/// workspace root would have shared one socket and one lock, silently, which is the
/// duplication the lock exists to prevent. `strip_suffix` cannot underflow: the
/// invariant either holds or it is a named error the caller can report.
///
/// Unreachable through [`registration_path`], which builds the name with a `.json`
/// suffix. That is the argument for checking it here rather than trusting it: the
/// invariant lives in another crate, and a `debug_assert` does not hold it in the
/// build that ships.
fn sibling_of_registration(registration: &Path, extension: &str) -> io::Result<PathBuf> {
    let name = registration
        .file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_default();
    let stem = name.strip_suffix(".json").ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "refusing to derive the daemon's {extension} path from {}: a \
                 registration file name must end in .json, and this one is {name:?}. \
                 Set XDG_CACHE_HOME to a directory you own and start again.",
                registration.display()
            ),
        )
    })?;
    Ok(registration.with_file_name(format!("{stem}{extension}")))
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
pub fn lock_path_for(workspace_root: &Path) -> io::Result<PathBuf> {
    sibling_of_registration(&registration_path(workspace_root), ".lock")
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
    let path = lock_path_for(workspace_root)?;

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

    /// The socket path for `root`, for the tests that are not about the naming rule.
    ///
    /// The accessor reports a registration name it cannot derive a sibling from (R14),
    /// and every caller here supplies a name it can, so the check is unwrapped once
    /// rather than at twenty call sites.
    fn socket_for(root: &Path) -> PathBuf {
        socket_path_for(root).expect("a socket path")
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
            let path =
                register(root, &socket_for(root), std::process::id()).expect("registration writes");

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
            let error = register(root, &socket_for(root), std::process::id())
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
            let error = register(root, &socket_for(root), std::process::id())
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
            register(root, &socket_for(root), std::process::id()).expect("registration writes");

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
            let error = register(root, &socket_for(root), std::process::id())
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
            let error = register(root, &socket_for(root), std::process::id())
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
            let error = register(root, &socket_for(root), std::process::id())
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
            let error = register(root, &socket_for(root), std::process::id())
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

    // The leaf is verified whether it was found or just made. A umask that strips
    // owner-execute turns the 0700 request into a 0600 directory the owner cannot
    // traverse, and the parent-grade check accepted that: the creating run carried on
    // into an `EACCES` from the lock-file open, and every run after it refused the
    // directory with the message the first run should have printed.
    //
    // The intermediate is made first, under the ordinary umask, so the umask below bites
    // on the leaf alone — otherwise the walk fails one component earlier, on a
    // `mkdirat` into a directory it cannot traverse, and this arm is never reached.
    //
    // `umask` is process-global, so this runs under the same lock the environment
    // mutations do, and restores it before asserting.
    #[cfg(unix)]
    #[test]
    fn a_leaf_created_under_a_umask_that_strips_owner_bits_is_refused_on_the_creating_run() {
        with_cache_home(|cache| {
            fs::create_dir_all(cache.join("oxabl")).expect("the intermediate directory");
            let dir = cache.join("oxabl").join("daemon");

            let previous = rustix::process::umask(Mode::from_bits_truncate(0o177));
            let outcome = create_and_verify_dir(&dir);
            rustix::process::umask(previous);

            let error = outcome
                .expect_err("a leaf the owner cannot traverse must be refused when it is made");
            assert_eq!(
                error.kind(),
                io::ErrorKind::PermissionDenied,
                "a directory the daemon cannot use is a permission failure: {error}"
            );
            assert!(
                error.to_string().contains("0600") && error.to_string().contains("0700"),
                "the message must name the mode it got and the mode it needs: {error}"
            );
            assert_eq!(
                fs::metadata(&dir).expect("the directory").mode() & 0o777,
                0o600,
                "and it must be refused rather than repaired"
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
            let path =
                register(root, &socket_for(root), std::process::id()).expect("registration writes");
            let staged = path.with_extension(format!("json.{}.tmp", std::process::id()));

            assert!(!staged.exists(), "the rename consumes the staging file");
            assert!(
                staged.file_name().expect("a name")
                    != path.with_extension("json.tmp").file_name().expect("a name"),
                "the staging name must carry the writer's identity"
            );
        });
    }

    /// A staging file is debris the moment its writer is gone, whoever that was. The
    /// old cleanup only ever removed *this* process's staging path, so a daemon killed
    /// between the write and the rename left a file every later run — running under a
    /// different pid — walked straight past.
    #[cfg(unix)]
    #[test]
    fn a_staging_file_left_by_a_dead_process_is_swept_by_a_later_run() {
        with_cache_home(|_| {
            let root = Path::new("/proj/swept");
            let other_root = Path::new("/proj/not-swept");
            ensure_registration_dir().expect("the directory");
            let path = registration_path(root);

            let staging = |registration: &Path, pid: u32| {
                registration.with_extension(format!("json.{pid}.tmp"))
            };
            // A pid in range that no process has: the sweep must read it as gone.
            let dead = staging(&path, i32::MAX as u32);
            // A pid that is unmistakably alive and is not this process. Its writer
            // would rename this file, and removing it would fail a start that was
            // going to succeed.
            let live_pid = rustix::process::getppid()
                .expect("a parent process")
                .as_raw_nonzero()
                .get() as u32;
            let live = staging(&path, live_pid);
            // Another root's staging file is another registration's business.
            let foreign = staging(&registration_path(other_root), i32::MAX as u32);
            for debris in [&dead, &live, &foreign] {
                fs::write(debris, b"{}").expect("a staging file to leave behind");
            }

            register(root, &socket_for(root), std::process::id()).expect("registration writes");

            assert!(
                !dead.exists(),
                "a staging file whose writer is gone must be swept, whatever pid it \
                 names: {}",
                dead.display()
            );
            assert!(
                live.exists(),
                "a live writer's staging file must be left alone, or its rename fails \
                 and a starting daemon reports an error it did not earn"
            );
            assert!(
                foreign.exists(),
                "the sweep is scoped to the registration being written"
            );
        });
    }

    /// And this process's own leftover is swept too, so a rename that failed earlier in
    /// the same run cannot block every later registration through `create_new`.
    #[test]
    fn a_staging_file_left_by_this_process_does_not_block_a_later_registration() {
        with_cache_home(|_| {
            let root = Path::new("/proj/own-leftover");
            ensure_registration_dir().expect("the directory");
            let staged =
                registration_path(root).with_extension(format!("json.{}.tmp", std::process::id()));
            fs::write(&staged, b"half a body").expect("our own leftover");

            register(root, &socket_for(root), std::process::id())
                .expect("a leftover of our own must not block the registration");
            assert!(!staged.exists(), "the rename consumes the staging file");
        });
    }

    /// A path that is not UTF-8 is refused rather than published in a converted form
    /// (R14). The bind uses the real bytes, so a converted registration would advertise
    /// a socket nobody bound — and would do it identically on every restart.
    #[cfg(unix)]
    #[test]
    fn a_path_that_is_not_utf8_is_refused_rather_than_converted() {
        use std::os::unix::ffi::OsStrExt;

        with_cache_home(|_| {
            let root = PathBuf::from(OsStr::from_bytes(b"/proj/not-utf8-\xff"));
            let socket = socket_for(&root);

            let error = register(&root, &socket, std::process::id())
                .expect_err("a path that cannot be published must be refused");
            assert_eq!(error.kind(), io::ErrorKind::InvalidInput);
            assert!(
                error.to_string().contains("UTF-8"),
                "the message must name what it refused and why: {error}"
            );
            assert!(
                !registration_path(&root).exists(),
                "nothing may be published when the paths cannot be published faithfully"
            );
        });
    }

    /// The formerly `debug_assert`-guarded subtraction. A name that does not end in
    /// `.json` used to underflow `name.len() - 5` and panic in a release build — in the
    /// two accessors that chose `unwrap_or_default` precisely to avoid panicking.
    #[test]
    fn a_registration_name_that_is_not_json_is_an_error_rather_than_a_panic() {
        for name in ["registration", "registration.jsonx", ".json.bak", ""] {
            let error = sibling_of_registration(&PathBuf::from(name), ".sock")
                .expect_err("a name that is not a registration cannot name a socket");
            assert_eq!(
                error.kind(),
                io::ErrorKind::InvalidInput,
                "{name:?} must be refused as input, not panic on a length subtraction"
            );
        }
        assert_eq!(
            sibling_of_registration(Path::new("/c/oxabl/daemon/%proj.json"), ".lock")
                .expect("a real registration name"),
            PathBuf::from("/c/oxabl/daemon/%proj.lock"),
            "the ordinary name still yields its sibling"
        );
    }

    /// A base directory variable that is not absolute is ignored, and the next
    /// candidate is used (R16). Resolved against the working directory instead, the
    /// registration, the lock and the socket would differ per process — two daemons on
    /// one root, and a client connecting to a path that means nothing where it stands.
    ///
    /// Inside `with_cache_home` for its lock: `HOME` decides the answer once
    /// `XDG_CACHE_HOME` is refused, and mutating either is a race against every other
    /// test in this binary.
    #[test]
    fn a_relative_base_directory_variable_is_ignored() {
        with_cache_home(|_| {
            let home = tempfile::tempdir().expect("a stand-in home");
            let previous_home = std::env::var_os("HOME");
            // SAFETY: `with_cache_home` holds the environment lock for this closure.
            unsafe {
                std::env::set_var("XDG_CACHE_HOME", "relative-cache");
                std::env::set_var("HOME", home.path());
            }

            let dir = registration_dir();
            assert_eq!(
                dir,
                home.path().join(".cache").join("oxabl").join("daemon"),
                "a relative XDG_CACHE_HOME must be ignored, not joined to the working \
                 directory"
            );

            // With neither variable usable, the temp-directory fallback is what is
            // left — the same branch an unset variable takes.
            unsafe { std::env::set_var("HOME", "relative-home") };
            let dir = registration_dir();
            assert!(
                dir.is_absolute() && dir.starts_with(std::env::temp_dir()),
                "with no usable variable the fallback must be the temp directory, got \
                 {dir:?}"
            );
            assert!(
                oxabl_daemon_protocol::temp_dir_fallback_in_use(),
                "and the caller that checks ownership for that branch must be told it \
                 is the branch in use"
            );

            unsafe {
                match previous_home {
                    Some(value) => std::env::set_var("HOME", value),
                    None => std::env::remove_var("HOME"),
                }
            }
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
        let socket = socket_for(root);
        let _ = fs::remove_file(&socket);
        std::os::unix::net::UnixListener::bind(&socket).expect("a listening socket")
    }

    #[test]
    fn a_live_registration_is_found() {
        with_cache_home(|_| {
            let root = Path::new("/proj/alpha");
            let socket = socket_for(root);
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
            register(root, &socket_for(root), std::process::id()).expect("registration writes");
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
            register(root, &socket_for(root), u32::MAX).expect("registration writes");

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
            let socket = socket_for(root);
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
            let socket = socket_for(root);
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
            let socket = socket_for(root);

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

    /// An answering socket says something is listening. It does not say that the
    /// something wrote the registration beside it, and pairing the two is a regression
    /// that kills an editor's language server on an ordinary upgrade: daemon A is
    /// `SIGKILL`ed leaving a file with a dead pid and an older contract, oxabl is
    /// upgraded, daemon B binds the socket and has not registered yet, and a poll in
    /// that window read the new socket with A's body — `Running`, then
    /// `VersionMismatch`, then the client exits.
    #[cfg(unix)]
    #[test]
    fn an_answering_socket_does_not_lend_its_registration_to_a_dead_process() {
        with_cache_home(|_| {
            let root = Path::new("/proj/upgraded");
            ensure_registration_dir().expect("the directory");
            // A pid in range that no process has: the killed daemon's.
            let dead_pid = i32::MAX as u32;
            assert!(
                !process_is_alive(dead_pid),
                "the test needs a pid nothing owns"
            );
            let stale = Registration {
                pid: dead_pid,
                socket_path: socket_for(root).to_string_lossy().into_owned(),
                contract_version: CONTRACT_VERSION + 7,
                workspace_root: root.to_string_lossy().into_owned(),
            };
            fs::write(
                registration_path(root),
                serde_json::to_string(&stale).expect("json"),
            )
            .expect("the killed daemon's registration");
            // The replacement, bound and not yet registered.
            let _listening = serving(root);

            assert_eq!(
                daemon_state(root),
                DaemonState::Starting(stale),
                "an answering socket beside a dead writer's registration is a start in \
                 progress, not a running daemon"
            );
            assert_eq!(
                discover(root),
                Discovery::Absent,
                "and the client must keep going rather than report the dead daemon's \
                 contract as the live one's"
            );
        });
    }

    /// A registration that could not be *read* is not a registration that is not
    /// there. Answering `Absent` to it starts a rival daemon, which is the one answer a
    /// permission or resource failure must never produce.
    ///
    /// Driven with a directory where the registration file goes: `read_to_string` then
    /// fails `EISDIR`, which is an error and is not `NotFound`, on any uid.
    #[cfg(unix)]
    #[test]
    fn a_registration_that_cannot_be_read_is_undecided_rather_than_absent() {
        with_cache_home(|_| {
            let root = Path::new("/proj/unreadable");
            ensure_registration_dir().expect("the directory");
            fs::create_dir(registration_path(root)).expect("something that is not a file");

            match daemon_state(root) {
                DaemonState::Unreadable(reason) => assert!(
                    reason.contains("could not be read"),
                    "the state must carry the cause: {reason}"
                ),
                other => panic!("expected an unreadable registration, got {other:?}"),
            }
            match discover(root) {
                Discovery::Undecided(reason) => assert!(
                    reason.contains(&registration_path(root).display().to_string()),
                    "the reason a client prints must name the file it could not read: \
                     {reason}"
                ),
                other => panic!("a read failure must not be reported as {other:?}"),
            }
        });
    }

    /// The mapping every client's next move depends on, exercised as the pure function
    /// it is. Reaching `Undecided` for real needs a descriptor limit, which a test
    /// cannot impose on a threaded binary without breaking every other test in it.
    ///
    /// The arm that matters is the middle one. `Undecided` used to map to `Live`, so a
    /// client whose probe failed returned a registration it had never reached, and the
    /// `connect` that followed failed `ECONNREFUSED` — the editor's server died and no
    /// daemon was ever started.
    #[test]
    fn a_state_that_proves_nothing_is_undecided_rather_than_live_or_absent() {
        let registration = Registration {
            pid: std::process::id(),
            socket_path: "/c/oxabl/daemon/%proj.sock".to_string(),
            contract_version: CONTRACT_VERSION,
            workspace_root: "/proj".to_string(),
        };

        assert!(matches!(
            classify(DaemonState::Running(registration.clone())),
            Discovery::Live(_)
        ));
        assert!(matches!(
            classify(DaemonState::Saturated(registration.clone())),
            Discovery::Live(_)
        ));
        match classify(DaemonState::Undecided(registration.clone())) {
            Discovery::Undecided(reason) => assert!(
                reason.contains(&registration.socket_path),
                "the reason must name the socket it could not probe: {reason}"
            ),
            other => panic!(
                "a probe that never happened must not be reported as {other:?}: absent \
                 starts a rival daemon and live sends the client into a failing connect"
            ),
        }
        assert_eq!(
            classify(DaemonState::Unreadable("the cause".to_string())),
            Discovery::Undecided("the cause".to_string()),
            "an unreadable registration carries its cause out to the client"
        );
        for absent in [
            DaemonState::Starting(registration.clone()),
            DaemonState::Crashed(registration),
            DaemonState::Absent,
        ] {
            assert_eq!(
                classify(absent.clone()),
                Discovery::Absent,
                "{absent:?} is the caller's cue to start a daemon"
            );
        }
    }

    /// A refusal prints once, however often the read path is polled. A client asks up
    /// to a hundred times at 20ms, and the author had already suppressed exactly this
    /// for the `NotFound` case — the unconditional branch beside it was the defect.
    #[test]
    fn a_refusal_is_printed_once_per_process_however_often_it_is_polled() {
        let refusal = format!(
            "a refusal only this test produces: {:?}",
            std::time::SystemTime::now()
        );
        assert!(report_once(&refusal), "the first one is worth printing");
        for _ in 0..100 {
            assert!(
                !report_once(&refusal),
                "a poll loop must not print a hundred copies of one refusal"
            );
        }
        assert!(
            report_once(&format!("{refusal} and another")),
            "a different refusal is still printed: the latch suppresses repetition, \
             not detail"
        );
    }

    /// What the start path needs from a probe before it unlinks anything: three
    /// answers, not two. The `Unprobeable` arm needs a failure to *make* a socket —
    /// a descriptor limit — so it is not reachable from a test; what is testable is
    /// that a live socket and a dead one are told apart, which is the pair the delete
    /// decision turns on.
    #[cfg(unix)]
    #[test]
    fn a_socket_with_no_listener_is_debris_and_a_live_one_answers() {
        with_cache_home(|_| {
            let root = Path::new("/proj/owner");
            let socket = socket_for(root);
            ensure_registration_dir().expect("the directory");

            assert_eq!(
                socket_owner(&socket),
                SocketOwner::Debris,
                "nothing at the path is nothing to preserve"
            );
            let listening = serving(root);
            assert_eq!(
                socket_owner(&socket),
                SocketOwner::Answering,
                "a bound socket must be refused rather than unlinked"
            );
            drop(listening);
            assert_eq!(
                socket_owner(&socket),
                SocketOwner::Debris,
                "and a file its listener left behind is debris again"
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
            register(root, &socket_for(root), std::process::id()).expect("registration writes");

            let lock = lock_path_for(root).expect("a lock path");
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

    /// The read path answers a question; it must not make a directory, a lock file or
    /// anything else to answer it (R25). Creating one here is also what used to be the
    /// *only* place a client checked the directory's ownership, which is why the check
    /// is now explicit.
    ///
    /// Asserted as "the base location is unchanged" rather than as a list of names,
    /// because the failure this guards against is a probe with *any* side effect: the
    /// old lock-based liveness check created the directory and an empty lock file per
    /// root, so every `oxabl` invocation in a tree with no daemon left artifacts.
    #[cfg(unix)]
    #[test]
    fn discovery_creates_nothing() {
        with_cache_home(|cache| {
            let root = Path::new("/proj/nothing-here");
            assert_eq!(discover(root), Discovery::Absent);
            // The finer state machine is a second entry point into the same read path.
            assert_eq!(daemon_state(root), DaemonState::Absent);
            assert_eq!(
                fs::read_dir(cache)
                    .expect("the base location")
                    .flatten()
                    .map(|entry| entry.file_name())
                    .collect::<Vec<_>>(),
                Vec::<std::ffi::OsString>::new(),
                "a client that finds no daemon must leave the base location untouched: \
                 no directory, no lock file, nothing"
            );
            // Named individually too, so a failure says which artifact came back.
            for artifact in [
                registration_dir(),
                registration_path(root),
                lock_path_for(root).expect("a lock path"),
                socket_for(root),
            ] {
                assert!(
                    !artifact.exists(),
                    "a probe must not create {}",
                    artifact.display()
                );
            }
        });
    }

    /// A registration directory this user did not make at 0700 is refused on the read
    /// path too, and refused without touching it.
    ///
    /// Refused, and reported as [`Discovery::Undecided`] rather than as `Absent`. The
    /// registration is not trusted either way — that is the property this test is for —
    /// but `Absent` is the answer that makes a client *start a daemon*, and a directory
    /// mode is no reason to start a second daemon: the new one walks the same directory
    /// and refuses it identically. Undecided keeps the client polling and hands it the
    /// sentence to print when it gives up.
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
            register(root, &socket_for(root), std::process::id()).expect("registration writes");
            // Loosened after the fact, the way something that is not this daemon
            // would have left it.
            fs::set_permissions(&dir, fs::Permissions::from_mode(0o755)).expect("widen it");

            match discover(root) {
                Discovery::Undecided(reason) => assert!(
                    reason.contains(&dir.display().to_string()),
                    "a directory the daemon would not have created is not trusted to \
                     hold a registration, and the refusal must name it: {reason}"
                ),
                other => panic!(
                    "a refused directory must not be reported as {other:?}: absent \
                     starts a rival daemon that will refuse the same directory"
                ),
            }
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
                socket_path: socket_for(root).to_string_lossy().into_owned(),
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
            let (_a, _alpha_lock) = register_locked(alpha, &socket_for(alpha), std::process::id())
                .expect("alpha")
                .expect("nothing holds alpha");
            let (_b, _beta_lock) = register_locked(beta, &socket_for(beta), std::process::id())
                .expect("beta")
                .expect("nothing holds beta");

            assert_ne!(registration_path(alpha), registration_path(beta));
            assert_ne!(socket_for(alpha), socket_for(beta));
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
            let socket = socket_for(root);
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
            register(root, &socket_for(root), std::process::id()).expect("registration");
            unregister(root).expect("removal");
            assert_eq!(discover(root), Discovery::Absent);
        });
    }
}
