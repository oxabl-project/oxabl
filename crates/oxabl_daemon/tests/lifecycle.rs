//! One session per workspace root, and what a session shares between clients
//! (R6, KTD21).

use std::path::{Path, PathBuf};

use oxabl_daemon::{ClientContext, Dispatch, MethodError, SessionHost, Sessions, default_dispatch};
use oxabl_daemon_protocol::{ClientKind, HandshakeRequest, method};

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

/// A handshake for `root`, spelled exactly as a client would spell it.
fn handshake(
    dispatch: &Dispatch,
    host: &SessionHost,
    root: &str,
) -> Result<serde_json::Value, MethodError> {
    dispatch.call(
        host,
        &mut ClientContext::default(),
        method::HANDSHAKE,
        serde_json::to_value(HandshakeRequest::new(ClientKind::Desktop, root)).unwrap(),
    )
}

/// One root reached through a symlink is one session with the root reached
/// directly (R2).
///
/// Lexical normalisation cannot see this: the two spellings share no components,
/// so the daemon would index one tree twice and hold it twice — the exact cost it
/// exists to avoid.
#[cfg(unix)]
#[test]
fn a_symlinked_root_and_the_real_root_are_one_session() {
    let real = tempfile::tempdir().expect("a workspace root");
    let elsewhere = tempfile::tempdir().expect("a directory to hold the link");
    let link = elsewhere.path().join("workspace-link");
    std::os::unix::fs::symlink(real.path(), &link).expect("a symlink to the root");

    let dispatch = default_dispatch();
    let host = SessionHost::new();
    handshake(&dispatch, &host, &real.path().to_string_lossy()).expect("the real root is accepted");
    handshake(&dispatch, &host, &link.to_string_lossy()).expect("the symlinked root is accepted");

    assert_eq!(
        host.with(|sessions| sessions.len()),
        1,
        "a symlink to a root names that root, got {:?}",
        host.with(|sessions| sessions
            .roots()
            .into_iter()
            .map(Path::to_path_buf)
            .collect::<Vec<_>>())
    );
}

/// A trailing separator is a spelling, not a second workspace (R2).
#[test]
fn a_trailing_separator_names_the_same_session() {
    let root = tempfile::tempdir().expect("a workspace root");
    let dispatch = default_dispatch();
    let host = SessionHost::new();

    handshake(&dispatch, &host, &root.path().to_string_lossy()).expect("the root is accepted");
    handshake(&dispatch, &host, &format!("{}/", root.path().display()))
        .expect("the trailing separator is accepted");

    assert_eq!(host.with(|sessions| sessions.len()), 1);
}

/// A relative root and its absolute equivalent are one session (R2).
#[test]
fn a_relative_root_and_its_absolute_equivalent_are_one_session() {
    let parent = tempfile::tempdir().expect("a directory to work from");
    let root = parent.path().join("workspace");
    std::fs::create_dir(&root).expect("a workspace root");

    let dispatch = default_dispatch();
    let host = SessionHost::new();
    handshake(&dispatch, &host, &root.to_string_lossy()).expect("the absolute root is accepted");
    with_current_dir(parent.path(), || {
        handshake(&dispatch, &host, "workspace").expect("the relative root is accepted")
    });

    assert_eq!(host.with(|sessions| sessions.len()), 1);
}

/// A root that is not on disk is refused, and the refusal names it (R2).
///
/// Keying on the raw spelling instead would give the client a session whose every
/// later answer describes a tree that does not exist.
#[test]
fn a_root_that_does_not_exist_is_refused_and_names_the_path() {
    let dispatch = default_dispatch();
    let host = SessionHost::new();

    let error = handshake(&dispatch, &host, "/proj/absent")
        .expect_err("a root that is not on disk is refused");

    assert!(
        error.message.contains("/proj/absent"),
        "the refusal must name the path, got {}",
        error.message
    );
    assert_eq!(
        host.with(|sessions| sessions.len()),
        0,
        "a refused handshake creates no session"
    );
}

/// A daemon bound to one root refuses a client that names another (R26).
///
/// The refusal names both roots, because the client can only fix this by knowing
/// which daemon it reached.
#[test]
fn a_root_other_than_the_bound_one_is_refused_and_creates_no_session() {
    let served = tempfile::tempdir().expect("the root this daemon serves");
    let other = tempfile::tempdir().expect("an unrelated tree");

    let dispatch = default_dispatch();
    let host = SessionHost::new();
    host.bind_root(served.path());

    let error = handshake(&dispatch, &host, &other.path().to_string_lossy())
        .expect_err("a foreign root is refused");

    let served = std::fs::canonicalize(served.path()).expect("the served root resolves");
    let other = std::fs::canonicalize(other.path()).expect("the other root resolves");
    assert!(
        error
            .message
            .contains(&served.to_string_lossy().to_string())
            && error.message.contains(&other.to_string_lossy().to_string()),
        "the refusal must name both roots, got {}",
        error.message
    );
    assert_eq!(
        host.with(|sessions| sessions.len()),
        0,
        "a refused handshake indexes nothing"
    );
}

/// The bound root is a workspace, not a spelling: the same tree reached through a
/// symlink is the daemon's own root and is accepted (R26).
#[cfg(unix)]
#[test]
fn another_spelling_of_the_bound_root_is_accepted() {
    let served = tempfile::tempdir().expect("the root this daemon serves");
    let elsewhere = tempfile::tempdir().expect("a directory to hold the link");
    let link = elsewhere.path().join("workspace-link");
    std::os::unix::fs::symlink(served.path(), &link).expect("a symlink to the root");

    let dispatch = default_dispatch();
    let host = SessionHost::new();
    host.bind_root(&link);

    handshake(&dispatch, &host, &served.path().to_string_lossy())
        .expect("the bound root under another spelling is accepted");
    assert_eq!(host.with(|sessions| sessions.len()), 1);
}

/// Run `body` with the process working directory at `directory`.
///
/// The working directory is process-wide and these tests run on threads of one
/// binary, so the lock is what keeps one test's directory from becoming another's.
/// No other test here depends on it: every path they use is absolute.
fn with_current_dir<T>(directory: &Path, body: impl FnOnce() -> T) -> T {
    static LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
    let _guard = LOCK.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
    let previous = std::env::current_dir().expect("a working directory");
    std::env::set_current_dir(directory).expect("the working directory moves");
    let out = body();
    std::env::set_current_dir(previous).expect("the working directory is restored");
    out
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
