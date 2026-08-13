//! One session per workspace root, and what a session shares between clients
//! (R6, KTD21).

use std::path::{Path, PathBuf};

use oxabl_daemon::Sessions;

#[test]
fn two_workspace_roots_get_two_sessions() {
    let mut sessions = Sessions::new();
    sessions.for_root("/proj/alpha");
    sessions.for_root("/proj/beta");

    assert_eq!(sessions.len(), 2);
    assert_eq!(
        sessions.roots(),
        vec![Path::new("/proj/alpha"), Path::new("/proj/beta")]
    );
}

/// The property the daemon exists for: two clients on one root hold one session
/// between them, not one each.
#[test]
fn two_clients_on_one_root_share_one_session() {
    let mut sessions = Sessions::new();
    sessions.for_root("/proj").attach(true);
    sessions.for_root("/proj").attach(false);

    assert_eq!(sessions.len(), 1, "one root is one session");
    let session = sessions.get("/proj").expect("the session exists");
    assert_eq!(session.clients(), 2);
    assert_eq!(
        session.editor_clients(),
        1,
        "only the editor contributes unsaved buffers"
    );
}

/// Two spellings of one root are one session. Without normalising, a client that
/// said `/proj/.` would index the workspace a second time and hold it a second time
/// — the exact cost the daemon exists to avoid.
#[test]
fn two_spellings_of_one_root_are_one_session() {
    let mut sessions = Sessions::new();
    sessions.for_root("/proj").attach(false);
    sessions.for_root("/proj/./sub/..").attach(false);

    assert_eq!(sessions.len(), 1, "got {:?}", sessions.roots());
    assert_eq!(sessions.get("/proj").expect("the session").clients(), 2);
}

#[test]
fn two_sessions_do_not_share_cached_facts() {
    let mut sessions = Sessions::new();
    sessions
        .for_root("/proj/alpha")
        .set_buffer("main.p", "MESSAGE \"alpha\".\n".to_string(), None);

    assert_eq!(
        sessions.for_root("/proj/beta").open_buffers(),
        0,
        "a buffer opened in one session must not appear in another"
    );
    assert_eq!(sessions.for_root("/proj/alpha").open_buffers(), 1);
}

#[test]
fn a_detaching_client_leaves_the_session_for_the_other_one() {
    let mut sessions = Sessions::new();
    let session = sessions.for_root("/proj");
    session.attach(true);
    session.attach(false);
    session.detach(true);

    assert_eq!(session.clients(), 1);
    assert_eq!(session.editor_clients(), 0);
    assert_eq!(
        sessions.len(),
        1,
        "the session survives one client leaving — that is the point of sharing it"
    );
}

/// A double detach cannot wrap the counts into a very large number of imaginary
/// clients, which would make an answer claim a working tree nobody is editing.
#[test]
fn detaching_more_clients_than_attached_saturates() {
    let mut sessions = Sessions::new();
    let session = sessions.for_root("/proj");
    session.attach(true);
    session.detach(true);
    session.detach(true);

    assert_eq!(session.clients(), 0);
    assert_eq!(session.editor_clients(), 0);
}

#[test]
fn a_buffer_can_be_replaced_and_closed() {
    let mut sessions = Sessions::new();
    let session = sessions.for_root("/proj");
    let path = Some(PathBuf::from("/proj/main.p"));

    let first = session.set_buffer("main.p", "MESSAGE \"one\".\n".to_string(), path.clone());
    let second = session.set_buffer("main.p", "MESSAGE \"two\".\n".to_string(), path);
    assert!(
        first == second,
        "an edit sets the existing input rather than minting a second one"
    );
    assert_eq!(session.buffer_keys(), vec!["main.p".to_string()]);

    session.close_buffer("main.p");
    assert!(session.buffer("main.p").is_none());
    assert_eq!(session.open_buffers(), 0);
}

/// Installing a configuration bumps the generation, which is what lets a completed
/// computation be recognised as computed under rules the user has replaced.
#[test]
fn installing_a_configuration_bumps_the_generation() {
    let mut sessions = Sessions::new();
    let session = sessions.for_root("/proj");
    assert_eq!(session.config_generation(), 0);

    session.install_config(oxabl_pipeline::PipelineConfig::default());
    assert_eq!(session.config_generation(), 1);

    session.install_config(oxabl_pipeline::PipelineConfig::default());
    assert_eq!(session.config_generation(), 2);
}

// The socket a real listener binds is reachable by this uid and nobody else. The
// directory is the load-bearing control, so both are asserted: a 0600 socket inside
// a 0755 directory would still be a weaker guarantee than the docs claim.
#[cfg(unix)]
#[test]
fn a_bound_socket_and_its_directory_are_private() {
    use std::os::unix::fs::PermissionsExt;

    with_cache_home(|cache| {
        let root = tempfile::tempdir().expect("a workspace root");
        let listener = oxabl_daemon::Listener::bind(root.path()).expect("bind a listener");
        let socket = listener.socket_path().to_path_buf();

        let mode = |path: &Path| {
            std::fs::metadata(path)
                .expect("the path exists")
                .permissions()
                .mode()
                & 0o777
        };
        assert_eq!(mode(&socket), 0o600, "the socket is private");
        assert_eq!(
            mode(&cache.join("oxabl").join("daemon")),
            0o700,
            "the directory nobody else can traverse is the real control"
        );
    });
}

/// A second daemon for one root is refused while the first holds it.
///
/// The bind never settled this: `Listener::bind` unlinks a stale socket path
/// first, so `bind` cannot return `EADDRINUSE` and both racers would have
/// succeeded. The lock is what admits exactly one.
#[cfg(unix)]
#[test]
fn a_second_daemon_for_one_root_is_refused() {
    with_cache_home(|_| {
        let root = tempfile::tempdir().expect("a workspace root");
        let first = oxabl_daemon::Listener::bind(root.path()).expect("the first daemon binds");

        let second = oxabl_daemon::Listener::bind(root.path());
        let error = match second {
            Ok(_) => panic!("a second daemon on one root must be refused"),
            Err(error) => error,
        };
        assert_eq!(error.kind(), std::io::ErrorKind::AddrInUse);

        // And the refusal costs the winner nothing. A loser that cleaned up on its way
        // out used to delete the winner's registration and socket, leaving a live
        // daemon nothing could reach — which is what turned a startup race into a
        // persistent fault.
        assert!(first.socket_path().exists(), "the winner keeps its socket");
        assert!(
            matches!(
                oxabl_daemon::registry::discover(root.path()),
                oxabl_daemon::registry::Discovery::Live(_)
            ),
            "the winner keeps its registration"
        );

        // Releasing it lets the next daemon start.
        drop(first);
        let next = oxabl_daemon::Listener::bind(root.path()).expect("the root is free again");
        drop(next);
    });
}

/// A registration nothing serves is dead, however alive its pid looks.
///
/// This is the recycled-pid case: after a crash the number can be handed to any
/// unrelated process, and a client that trusted it would connect to a socket
/// nobody holds and wait. Here the pid is this very process, so a pid check passes
/// and only the socket can tell the truth.
#[cfg(unix)]
#[test]
fn a_registration_without_the_lock_is_absent_even_with_a_live_pid() {
    with_cache_home(|_| {
        let root = Path::new("/proj/recycled-pid");

        let socket = oxabl_daemon::registry::socket_path_for(root).expect("a socket path");
        oxabl_daemon::registry::register(root, &socket, std::process::id())
            .expect("write a registration nobody holds");

        assert_eq!(
            oxabl_daemon::registry::discover(root),
            oxabl_daemon::registry::Discovery::Absent,
            "a live pid is not a live daemon"
        );
    });
}

/// Two clients probing one root at the same time: exactly one daemon ends up serving
/// it, and the loser's refusal names the state it actually found.
#[cfg(unix)]
#[test]
fn two_clients_probing_one_root_start_exactly_one_daemon() {
    use std::sync::mpsc;

    with_cache_home(|_| {
        let root = tempfile::tempdir().expect("a workspace root");
        let (report, results) = mpsc::channel();

        let starters: Vec<_> = (0..2)
            .map(|_| {
                let path = root.path().to_path_buf();
                let report = report.clone();
                std::thread::spawn(move || {
                    // What a client does: probe, and start a daemon only if nothing is
                    // there. Both may well see nothing — the lock is what settles it.
                    let _ = oxabl_daemon::registry::discover(&path);
                    let outcome = oxabl_daemon::Listener::bind(&path);
                    let started = outcome.is_ok();
                    report
                        .send(outcome.err().map(|error| error.to_string()))
                        .expect("report the outcome");
                    // Hold the listener until both have tried, so the winner cannot
                    // release the root and let the loser succeed too.
                    std::thread::sleep(std::time::Duration::from_millis(50));
                    started
                })
            })
            .collect();
        drop(report);

        let outcomes: Vec<_> = results.iter().collect();
        let started = starters
            .into_iter()
            .map(|handle| handle.join().expect("a starter finishes"))
            .filter(|started| *started)
            .count();

        assert_eq!(started, 1, "exactly one daemon may serve one root");
        let refusal = outcomes
            .iter()
            .flatten()
            .next()
            .expect("the loser reports why");
        assert!(
            refusal.contains("already serves")
                || refusal.contains("is starting")
                || refusal.contains("start lock"),
            "the refusal must name the state it found, got {refusal}"
        );
    });
}

/// A crashed daemon leaves a socket file behind. The next daemon finds it refuses
/// connections, cleans it up, and binds — which is the whole point of proving death
/// by an acquirable lock plus an unanswered connect.
#[cfg(unix)]
#[test]
fn a_crashed_daemons_leftover_socket_is_cleaned_up_by_the_next_daemon() {
    with_cache_home(|_| {
        let root = tempfile::tempdir().expect("a workspace root");
        let socket = oxabl_daemon::registry::socket_path_for(root.path()).expect("a socket path");
        oxabl_daemon::registry::ensure_registration_dir().expect("the directory");
        // Bound and then closed without unlinking: exactly what `SIGKILL` leaves.
        drop(std::os::unix::net::UnixListener::bind(&socket).expect("a socket to abandon"));
        oxabl_daemon::registry::register(root.path(), &socket, std::process::id())
            .expect("the crashed daemon's registration");
        assert!(socket.exists(), "the debris is there to be cleaned up");

        let listener = oxabl_daemon::Listener::bind(root.path())
            .expect("a crashed daemon's socket must not block the next one");
        assert_eq!(listener.socket_path(), socket);
        assert!(
            matches!(
                oxabl_daemon::registry::discover(root.path()),
                oxabl_daemon::registry::Discovery::Live(_)
            ),
            "and the replacement is discoverable"
        );
    });
}

/// The cleanup is a delete, so it refuses to be aimed at anything but a socket. A
/// regular file at the socket path fails the start loudly instead of being removed.
#[cfg(unix)]
#[test]
fn a_regular_file_at_the_socket_path_is_refused_rather_than_removed() {
    with_cache_home(|_| {
        let root = tempfile::tempdir().expect("a workspace root");
        let socket = oxabl_daemon::registry::socket_path_for(root.path()).expect("a socket path");
        oxabl_daemon::registry::ensure_registration_dir().expect("the directory");
        std::fs::write(&socket, b"precious").expect("a file where the socket goes");

        let Err(error) = oxabl_daemon::Listener::bind(root.path()) else {
            panic!("a file that is not a socket must not be deleted");
        };
        assert!(
            error.to_string().contains("not a socket"),
            "the message must say what it refused: {error}"
        );
        assert_eq!(
            std::fs::read(&socket).expect("still there"),
            b"precious",
            "the daemon must not have removed it"
        );
    });
}

/// A start that fails after the socket exists leaves no socket behind.
///
/// The socket file is created by `bind` and removed by `Drop for Listener`, so a
/// failure between the two used to leak it: the guard was constructed last, so an
/// early return skipped its `Drop` entirely. The leak was sticky, too — the next start
/// finds a socket that answers nothing, and the daemon refuses to unlink a socket it
/// cannot prove is debris.
///
/// Driven through the one step after the bind that can still fail: a workspace root
/// whose bytes are not UTF-8 cannot be published in a registration, so `register`
/// refuses it. Nothing has to be faked to reach the window.
#[cfg(unix)]
#[test]
fn a_start_that_fails_after_the_bind_leaves_no_socket_behind() {
    use std::ffi::OsStr;
    use std::os::unix::ffi::OsStrExt;

    with_cache_home(|_| {
        let root = PathBuf::from(OsStr::from_bytes(b"/proj/not-utf8-\xff"));
        let socket = oxabl_daemon::registry::socket_path_for(&root).expect("a socket path");

        let error = oxabl_daemon::Listener::bind(&root)
            .err()
            .expect("a root that cannot be registered must not start a daemon");
        assert!(
            error.to_string().contains("UTF-8"),
            "the refusal must name the reason: {error}"
        );
        assert!(
            !socket.exists(),
            "a failed start must not leave {} behind for the next one to trip over",
            socket.display()
        );
        assert!(
            !oxabl_daemon_protocol::registration_path(&root).exists(),
            "and it must not leave a registration either"
        );
    });
}

/// Probing whether a daemon is running must not disturb the lock a starting daemon
/// needs (R12, KTD6).
///
/// The reported bug, and the one the in-process double-bind test above cannot see:
/// that one binds twice and never runs a probe, so the probe window is untested. A
/// probe that takes the lock — even to release it one syscall later — makes a
/// concurrent `bind` fail with `EWOULDBLOCK`, and the daemon then reports that
/// another daemon serves the root when nothing does.
#[cfg(unix)]
#[test]
fn a_client_probe_does_not_prevent_a_daemon_from_starting() {
    use std::sync::Arc;
    use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};

    with_cache_home(|_| {
        let root = tempfile::tempdir().expect("a workspace root");
        let path = root.path().to_path_buf();
        let probing = Arc::new(AtomicBool::new(true));
        let probes = Arc::new(AtomicU32::new(0));

        let prober = {
            let probing = Arc::clone(&probing);
            let probes = Arc::clone(&probes);
            let path = path.clone();
            std::thread::spawn(move || {
                while probing.load(Ordering::SeqCst) {
                    let _ = oxabl_daemon::registry::discover(&path);
                    probes.fetch_add(1, Ordering::SeqCst);
                }
            })
        };

        let mut refusals = Vec::new();
        for _ in 0..200 {
            // A registration on disk is what makes every probe reach the lock: a
            // client with nothing registered answers "absent" without looking. A
            // crashed daemon leaves exactly this behind.
            let socket = oxabl_daemon::registry::socket_path_for(&path).expect("a socket path");
            oxabl_daemon::registry::register(&path, &socket, 999_999)
                .expect("a stale registration");
            match oxabl_daemon::Listener::bind(&path) {
                Ok(listener) => drop(listener),
                Err(error) => refusals.push(error.to_string()),
            }
        }
        probing.store(false, Ordering::SeqCst);
        prober.join().expect("the probe thread finishes");

        assert!(
            probes.load(Ordering::SeqCst) > 0,
            "the probe loop must actually have run, or this test proves nothing"
        );
        assert!(
            refusals.is_empty(),
            "a probe must not refuse a starting daemon, got {refusals:?}"
        );
    });
}

/// The lock-held arm with nothing registered: the refusal names the lock file, and
/// does not invent a pid it cannot know.
///
/// Reached deterministically by holding the lock without binding. Until now this arm and
/// the one below were reachable only through a real start race, which decides for itself
/// which sentence gets printed — so a refusal that named the wrong state would not have
/// failed anything.
#[cfg(unix)]
#[test]
fn a_held_start_lock_with_no_registration_names_the_lock_rather_than_a_daemon() {
    with_cache_home(|_| {
        let root = tempfile::tempdir().expect("a workspace root");
        while_the_root_lock_is_held(root.path(), || {
            let error = oxabl_daemon::Listener::bind(root.path())
                .err()
                .expect("a root another holder owns must be refused");

            assert_eq!(error.kind(), std::io::ErrorKind::AddrInUse);
            let message = error.to_string();
            assert!(
                message.contains("has not registered a daemon yet"),
                "the refusal must report the state it found, not a daemon: {message}"
            );
            assert!(
                message.contains(
                    &oxabl_daemon::registry::lock_path_for(root.path())
                        .expect("a lock path")
                        .display()
                        .to_string()
                ),
                "and it must name the file to remove if nothing is really starting: \
                 {message}"
            );
        });
    });
}

/// The lock-held arm with a registration whose writer is alive: a daemon between the
/// lock and its socket. The refusal must say "wait", and must name that pid — telling a
/// user to remove a lock file a live daemon holds is the wrong instruction.
#[cfg(unix)]
#[test]
fn a_held_start_lock_with_a_live_registration_reports_a_daemon_that_is_starting() {
    with_cache_home(|_| {
        let root = tempfile::tempdir().expect("a workspace root");
        let socket = oxabl_daemon::registry::socket_path_for(root.path()).expect("a socket path");
        while_the_root_lock_is_held(root.path(), || {
            // What a daemon has written by the time it holds the lock and has not yet
            // bound: a registration, no socket. The pid is this process, which is alive.
            oxabl_daemon::registry::register(root.path(), &socket, std::process::id())
                .expect("the starting daemon's registration");
            assert!(!socket.exists(), "and nothing is listening yet");

            let error = oxabl_daemon::Listener::bind(root.path())
                .err()
                .expect("a root another holder owns must be refused");

            assert_eq!(error.kind(), std::io::ErrorKind::AddrInUse);
            let message = error.to_string();
            assert!(
                message.contains("is starting"),
                "the refusal must name a start in progress: {message}"
            );
            assert!(
                message.contains(&std::process::id().to_string()),
                "and must name the pid to wait for: {message}"
            );
        });
    });
}

/// Run `body` while another thread holds the root lock for `root`.
///
/// Another thread rather than this one, because that is what the situation being tested
/// is: a separate starting daemon. `flock` grants per open file description, so a lock
/// taken here would conflict with `bind`'s too — but a background holder keeps the test
/// honest about which code is under test, and the lock is released before it returns.
#[cfg(unix)]
fn while_the_root_lock_is_held(root: &Path, body: impl FnOnce()) {
    use std::sync::mpsc;

    let (held, holding) = mpsc::channel();
    let (release, released) = mpsc::channel();
    std::thread::scope(|scope| {
        scope.spawn(move || {
            let lock = oxabl_daemon::registry::acquire_root_lock(root)
                .expect("the lock file opens")
                .expect("nothing else holds this root");
            held.send(()).expect("report that the lock is held");
            let _ = released.recv();
            drop(lock);
        });
        holding
            .recv()
            .expect("the lock is held before the test runs");
        body();
        let _ = release.send(());
    });
}

/// Run `body` with the registration directory pointed at a private temporary one.
///
/// `XDG_CACHE_HOME` is process-wide and these tests run on threads of one binary,
/// so the lock is what keeps one test's cache directory from becoming another's.
#[cfg(unix)]
fn with_cache_home<T>(body: impl FnOnce(&Path) -> T) -> T {
    static LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
    let _guard = LOCK.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
    let cache = tempfile::tempdir().expect("a cache directory");
    let previous = std::env::var_os("XDG_CACHE_HOME");
    // SAFETY: the lock above makes this the only thread mutating the environment.
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
