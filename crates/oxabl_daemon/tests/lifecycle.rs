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

/// A registration written without the lock is dead, however alive its pid looks.
///
/// This is the recycled-pid case: after a crash the number can be handed to any
/// unrelated process, and a client that trusted it would connect to a socket
/// nobody holds and wait. Here the pid is this very process, so the pid check
/// passes and only the lock can tell the truth.
#[cfg(unix)]
#[test]
fn a_registration_without_the_lock_is_absent_even_with_a_live_pid() {
    with_cache_home(|_| {
        let root = Path::new("/proj/recycled-pid");

        let socket = oxabl_daemon::registry::socket_path_for(root);
        oxabl_daemon::registry::register(root, &socket, std::process::id())
            .expect("write a registration nobody holds");

        assert_eq!(
            oxabl_daemon::registry::discover(root),
            oxabl_daemon::registry::Discovery::Absent,
            "a live pid is not a live daemon"
        );
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
