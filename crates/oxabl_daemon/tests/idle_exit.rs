//! A daemon with an idle timeout stops serving only when no client is connected and no
//! background work runs. Over a real Unix socket, with the default method table.

#![cfg(unix)]

use std::os::unix::net::UnixStream;
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use lsp_server::{Message, Request, RequestId, Response};
use oxabl_daemon::{
    ClientContext, Discovery, Listener, SessionHost, connection_over, default_dispatch, discover,
};
use oxabl_daemon_protocol::{
    ClientKind, FreshnessResponse, HandshakeRequest, IndexState, Sourced, method,
};

const BASE_VARIABLES: [&str; 3] = ["XDG_RUNTIME_DIR", "XDG_CACHE_HOME", "HOME"];

/// Run `body` with every base-directory variable pointed at one private temporary
/// directory. The environment is process-wide, so this serialises the tests.
fn with_base_dir<T>(body: impl FnOnce() -> T) -> T {
    static LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
    let _guard = LOCK.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
    let base = tempfile::tempdir().expect("a temporary base directory");
    let previous: Vec<_> = BASE_VARIABLES
        .iter()
        .map(|name| (*name, std::env::var_os(name)))
        .collect();
    // SAFETY: the lock above makes this the only thread mutating the environment.
    unsafe {
        for name in BASE_VARIABLES {
            std::env::set_var(name, base.path());
        }
    }
    let out = body();
    unsafe {
        for (name, value) in previous {
            match value {
                Some(value) => std::env::set_var(name, value),
                None => std::env::remove_var(name),
            }
        }
    }
    out
}

/// A workspace whose pass takes a while: many procedures over one include path.
fn large_workspace() -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("a workspace root");
    std::fs::write(
        dir.path().join("oxabl.toml"),
        "[workspace]\nname = \"idle\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
    )
    .expect("the configuration");
    std::fs::write(
        dir.path().join("shared.i"),
        "DEFINE VARIABLE shared AS INTEGER NO-UNDO.\n",
    )
    .expect("the include");
    let body: String = (0..40)
        .map(|n| {
            format!(
                "PROCEDURE step{n}:\n  DEFINE VARIABLE v{n} AS INTEGER NO-UNDO.\n  \
                 v{n} = shared + {n}.\n  IF v{n} > 10 THEN MESSAGE v{n}.\nEND PROCEDURE.\n"
            )
        })
        .collect();
    for n in 0..400 {
        std::fs::write(
            dir.path().join(format!("unit{n}.p")),
            format!("{{shared.i}}\n{body}"),
        )
        .expect("a procedure");
    }
    dir
}

fn call(
    connection: &lsp_server::Connection,
    id: i32,
    method: &str,
    params: serde_json::Value,
) -> Response {
    connection
        .sender
        .send(Message::Request(Request {
            id: RequestId::from(id),
            method: method.to_string(),
            params,
        }))
        .expect("the request is sent");
    loop {
        match connection
            .receiver
            .recv_timeout(Duration::from_secs(30))
            .expect("a response arrives")
        {
            Message::Response(response) if response.id == RequestId::from(id) => return response,
            _ => continue,
        }
    }
}

fn freshness_state(value: serde_json::Value) -> IndexState {
    match serde_json::from_value::<Sourced<FreshnessResponse>>(value).expect("a freshness answer") {
        Sourced::Available { value } => value.freshness.state,
        Sourced::Unavailable { reason } => panic!("the daemon refused freshness: {reason}"),
    }
}

/// A client asks for freshness, which starts a workspace pass in the background, and
/// leaves at once. The daemon must not stop before the pass has ended: it is the work
/// the next client would otherwise do again.
#[test]
fn a_workspace_pass_keeps_an_idle_daemon_until_the_pass_ends() {
    with_base_dir(|| {
        let workspace = large_workspace();
        let root = workspace.path();
        let mut listener = Listener::bind(root).expect("the socket binds");
        listener.set_idle_timeout(Some(Duration::from_millis(50)));
        let socket = listener.socket_path().to_path_buf();
        let host = Arc::new(SessionHost::new());
        let dispatch = Arc::new(default_dispatch());
        let (ended, end) = std::sync::mpsc::channel();
        let serving = {
            let host = Arc::clone(&host);
            std::thread::spawn(move || {
                let _ = listener.accept_loop(dispatch, host);
                // The registration goes with the listener, as at a daemon's exit.
                drop(listener);
                let _ = ended.send(());
            })
        };

        let (connection, threads) =
            connection_over(UnixStream::connect(&socket).expect("the daemon listens"));
        let handshake = call(
            &connection,
            1,
            method::HANDSHAKE,
            serde_json::to_value(HandshakeRequest::new(
                ClientKind::Desktop,
                root.to_string_lossy(),
            ))
            .unwrap(),
        );
        assert!(handshake.error.is_none(), "{:?}", handshake.error);
        let first = call(&connection, 2, method::FRESHNESS, serde_json::json!({}));
        assert!(
            matches!(
                freshness_state(first.result.expect("a result")),
                IndexState::Indexing { .. }
            ),
            "the first question starts the pass"
        );
        drop(connection);
        threads.shutdown();

        end.recv_timeout(Duration::from_secs(120))
            .expect("the daemon stops once the pass has ended");
        let _ = serving.join();

        let dispatch = default_dispatch();
        let mut context = ClientContext::default();
        dispatch
            .call(
                &host,
                &mut context,
                method::HANDSHAKE,
                serde_json::to_value(HandshakeRequest::new(
                    ClientKind::Desktop,
                    root.to_string_lossy(),
                ))
                .unwrap(),
            )
            .expect("the handshake answers");
        let after = dispatch
            .call(
                &host,
                &mut context,
                method::FRESHNESS,
                serde_json::json!({}),
            )
            .expect("freshness answers");
        assert_eq!(
            freshness_state(after),
            IndexState::Ready,
            "the daemon stopped while its workspace pass still ran"
        );
        assert_eq!(discover(root), Discovery::Absent);
        assert_unregistered(root);
    });
}

/// The registration, the socket and the lock file are all gone.
fn assert_unregistered(root: &Path) {
    for artifact in [
        oxabl_daemon::registration_path_for(root).expect("a registration path"),
        oxabl_daemon::socket_path_for(root).expect("a socket path"),
        oxabl_daemon::registry::lock_path_for(root).expect("a lock path"),
    ] {
        assert!(
            !artifact.exists(),
            "the stopped daemon left {} behind",
            artifact.display()
        );
    }
}

/// Daemons that exit remove the lock file while they hold it. A daemon that opened the
/// file just before must not then hold a lock of its own beside the next daemon's.
#[test]
fn a_removed_lock_file_never_lets_two_holders_share_one_root() {
    use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};

    with_base_dir(|| {
        let workspace = tempfile::tempdir().expect("a workspace root");
        let root = workspace.path().to_path_buf();
        let held = Arc::new(AtomicBool::new(false));
        let taken = Arc::new(AtomicU32::new(0));
        let holders: Vec<_> = (0..4)
            .map(|_| {
                let (root, held, taken) = (root.clone(), Arc::clone(&held), Arc::clone(&taken));
                std::thread::spawn(move || {
                    for _ in 0..500 {
                        let Some(lock) = oxabl_daemon::registry::acquire_root_lock(&root)
                            .expect("the lock file opens")
                        else {
                            continue;
                        };
                        assert!(
                            !held.swap(true, Ordering::SeqCst),
                            "two holders held the lock of one root at once"
                        );
                        taken.fetch_add(1, Ordering::SeqCst);
                        std::thread::yield_now();
                        held.store(false, Ordering::SeqCst);
                        lock.remove();
                    }
                })
            })
            .collect();
        for holder in holders {
            holder.join().expect("no two holders at once");
        }
        assert!(taken.load(Ordering::SeqCst) > 0, "the lock was taken");
    });
}

/// A daemon of `root` on its own thread. `ended` receives a message when its accept
/// loop returns.
struct Serving {
    socket: std::path::PathBuf,
    stopper: oxabl_daemon::Stopper,
    ended: std::sync::mpsc::Receiver<()>,
    thread: std::thread::JoinHandle<()>,
    host: Arc<SessionHost>,
}

fn serve(root: &Path, idle_timeout: Option<Duration>) -> Serving {
    let mut listener = Listener::bind(root).expect("the socket binds");
    listener.set_idle_timeout(idle_timeout);
    let socket = listener.socket_path().to_path_buf();
    let stopper = listener.stopper();
    let host = Arc::new(SessionHost::new());
    let serving_host = Arc::clone(&host);
    let dispatch = Arc::new(default_dispatch());
    let (ended, ended_rx) = std::sync::mpsc::channel();
    let thread = std::thread::spawn(move || {
        let _ = listener.accept_loop(dispatch, serving_host);
        drop(listener);
        let _ = ended.send(());
    });
    Serving {
        socket,
        stopper,
        ended: ended_rx,
        thread,
        host,
    }
}

/// Wait until the daemon has bound its root and has no client. A change of the root
/// before that is a change of the directory the daemon starts for.
fn settled(daemon: &Serving, root: &Path) {
    let (connection, threads) = connect(daemon, root);
    drop(connection);
    threads.shutdown();
    let deadline = std::time::Instant::now() + Duration::from_secs(5);
    while daemon.host.activity().clients() != 0 {
        assert!(
            std::time::Instant::now() < deadline,
            "the client of the daemon never left"
        );
        std::thread::sleep(Duration::from_millis(10));
    }
}

/// A client that completed the handshake for `root`.
fn connect(
    daemon: &Serving,
    root: &Path,
) -> (
    lsp_server::Connection,
    oxabl_daemon::listener::ClientThreads,
) {
    let (connection, threads) =
        connection_over(UnixStream::connect(&daemon.socket).expect("the daemon listens"));
    let handshake = call(
        &connection,
        1,
        method::HANDSHAKE,
        serde_json::to_value(HandshakeRequest::new(
            ClientKind::Desktop,
            root.to_string_lossy(),
        ))
        .unwrap(),
    );
    assert!(handshake.error.is_none(), "{:?}", handshake.error);
    (connection, threads)
}

/// A root that is deleted ends the daemon within a few seconds, with a client still
/// connected: an editor whose folder is gone has nothing left to ask.
#[test]
fn a_removed_workspace_root_ends_the_daemon_with_a_client_connected() {
    with_base_dir(|| {
        let workspace = tempfile::tempdir().expect("a workspace root");
        let root = workspace.path().join("project");
        std::fs::create_dir(&root).expect("the root");
        let daemon = serve(&root, None);
        let (connection, threads) = connect(&daemon, &root);
        assert!(
            daemon.ended.try_recv().is_err(),
            "the daemon serves while its root exists"
        );

        std::fs::remove_dir_all(&root).expect("the root is removed");
        daemon
            .ended
            .recv_timeout(Duration::from_secs(10))
            .expect("the daemon ends after its root is removed");
        daemon.thread.join().expect("the loop ends cleanly");
        drop(connection);
        threads.shutdown();
        let left: Vec<_> = std::fs::read_dir(daemon.socket.parent().expect("a directory"))
            .expect("the registration directory")
            .map(|entry| entry.expect("an entry").file_name())
            .collect();
        assert!(left.is_empty(), "the daemon left {left:?} behind");
    });
}

/// A root that was removed and made again is a different directory, and the open
/// directory shows it: it has no link left. The path alone names a directory that
/// looks like a replacement, which a daemon with a client does not act on.
#[test]
fn a_root_removed_and_made_again_ends_the_daemon_with_a_client_connected() {
    with_base_dir(|| {
        let workspace = tempfile::tempdir().expect("a workspace root");
        let root = workspace.path().join("project");
        std::fs::create_dir(&root).expect("the root");
        let daemon = serve(&root, None);
        let (connection, threads) = connect(&daemon, &root);

        std::fs::remove_dir(&root).expect("the root is removed");
        std::fs::create_dir(&root).expect("the root is made again");
        daemon
            .ended
            .recv_timeout(Duration::from_secs(10))
            .expect("the daemon ends after its root was removed");
        daemon.thread.join().expect("the loop ends cleanly");
        drop(connection);
        threads.shutdown();
    });
}

/// A root that moved away, with nothing at its path, is gone.
#[test]
fn a_moved_workspace_root_ends_the_daemon_with_a_client_connected() {
    with_base_dir(|| {
        let workspace = tempfile::tempdir().expect("a workspace root");
        let root = workspace.path().join("project");
        std::fs::create_dir(&root).expect("the root");
        let daemon = serve(&root, None);
        let (connection, threads) = connect(&daemon, &root);

        std::fs::rename(&root, workspace.path().join("aside")).expect("the root moves away");
        daemon
            .ended
            .recv_timeout(Duration::from_secs(10))
            .expect("the daemon ends after its root moved away");
        daemon.thread.join().expect("the loop ends cleanly");
        drop(connection);
        threads.shutdown();
    });
}

/// Replace the root of `workspace` by another directory with the same name.
fn replace_root(workspace: &Path, root: &Path) {
    // The new directory exists before the old one goes, so no inode is reused.
    let other = workspace.join("other");
    std::fs::create_dir(&other).expect("the other directory");
    std::fs::rename(root, workspace.join("aside")).expect("the root moves away");
    std::fs::rename(&other, root).expect("another directory takes its name");
}

/// With no client, a root whose path names another directory ends the daemon: a
/// restart costs little.
#[test]
fn a_replaced_workspace_root_ends_a_daemon_without_a_client() {
    with_base_dir(|| {
        let workspace = tempfile::tempdir().expect("a workspace root");
        let root = workspace.path().join("project");
        std::fs::create_dir(&root).expect("the root");
        let daemon = serve(&root, None);
        settled(&daemon, &root);

        replace_root(workspace.path(), &root);
        daemon
            .ended
            .recv_timeout(Duration::from_secs(10))
            .expect("the daemon ends after its root is replaced");
        daemon.thread.join().expect("the loop ends cleanly");
    });
}

/// A changed device or inode number alone does not end a daemon that has a client:
/// some file systems do not keep these numbers. The daemon ends when the client leaves.
#[test]
fn a_replaced_workspace_root_keeps_a_daemon_that_has_a_client() {
    with_base_dir(|| {
        let workspace = tempfile::tempdir().expect("a workspace root");
        let root = workspace.path().join("project");
        std::fs::create_dir(&root).expect("the root");
        let daemon = serve(&root, None);
        let (connection, threads) = connect(&daemon, &root);

        replace_root(workspace.path(), &root);
        assert!(
            daemon.ended.recv_timeout(Duration::from_secs(4)).is_err(),
            "the daemon ended with a client connected after only its identity changed"
        );
        let reply = call(&connection, 2, method::FRESHNESS, serde_json::json!({}));
        assert!(
            reply.result.is_some() || reply.error.is_some(),
            "the daemon still serves its client"
        );

        drop(connection);
        threads.shutdown();
        daemon
            .ended
            .recv_timeout(Duration::from_secs(10))
            .expect("the daemon ends when its last client has left");
        daemon.thread.join().expect("the loop ends cleanly");
    });
}

/// The idle exit must not need the socket file. A daemon whose runtime directory was
/// deleted (a test that cleaned up) used to decide to exit and then wait for a
/// connection that could not reach it, for ever.
#[test]
fn an_idle_daemon_exits_after_its_socket_file_was_removed() {
    with_base_dir(|| {
        let workspace = tempfile::tempdir().expect("a workspace root");
        let daemon = serve(workspace.path(), Some(Duration::from_millis(500)));
        std::fs::remove_file(&daemon.socket).expect("the socket file is removed");
        daemon
            .ended
            .recv_timeout(Duration::from_secs(10))
            .expect("the idle daemon exits without its socket file");
        daemon.thread.join().expect("the loop ends cleanly");
    });
}

/// A root that exists and can be read keeps its daemon.
#[test]
fn a_root_that_exists_keeps_its_daemon() {
    with_base_dir(|| {
        let workspace = tempfile::tempdir().expect("a workspace root");
        let daemon = serve(workspace.path(), None);
        assert!(
            daemon.ended.recv_timeout(Duration::from_secs(3)).is_err(),
            "the daemon exited while its root exists"
        );
        daemon.stopper.stop();
        daemon
            .ended
            .recv_timeout(Duration::from_secs(10))
            .expect("the daemon stops when asked");
        daemon.thread.join().expect("the loop ends cleanly");
    });
}

/// Gives a directory its permissions back when it drops, so the test can clean up.
struct Restore(std::path::PathBuf, std::fs::Permissions);

impl Drop for Restore {
    fn drop(&mut self) {
        let _ = std::fs::set_permissions(&self.0, self.1.clone());
    }
}

/// A root that cannot be checked is not a root that is gone. With the parent of the
/// root at mode 000, `stat` of the root fails with a permission error for more than
/// two check intervals. The daemon keeps running, and serves when the mode is back.
#[test]
fn a_root_that_cannot_be_checked_keeps_its_daemon() {
    use std::os::unix::fs::PermissionsExt;
    with_base_dir(|| {
        let workspace = tempfile::tempdir().expect("a workspace root");
        let parent = workspace.path().join("parent");
        let root = parent.join("project");
        std::fs::create_dir_all(&root).expect("the root");
        let daemon = serve(&root, None);
        settled(&daemon, &root);

        let _restore = Restore(
            parent.clone(),
            std::fs::metadata(&parent)
                .expect("the parent")
                .permissions(),
        );
        std::fs::set_permissions(&parent, std::fs::Permissions::from_mode(0o000))
            .expect("the parent is locked");
        assert!(
            std::fs::metadata(&root).is_err(),
            "`stat` of the root succeeds although its parent has mode 000 (a process \
             with root rights?), so this test would prove nothing"
        );
        assert!(
            daemon.ended.recv_timeout(Duration::from_secs(3)).is_err(),
            "the daemon exited because its root could not be checked"
        );

        std::fs::set_permissions(&parent, std::fs::Permissions::from_mode(0o755))
            .expect("the parent is open again");
        let (connection, threads) = connect(&daemon, &root);
        drop(connection);
        threads.shutdown();
        assert!(
            daemon.ended.try_recv().is_err(),
            "the daemon still serves after the check failed"
        );
        daemon.stopper.stop();
        daemon
            .ended
            .recv_timeout(Duration::from_secs(10))
            .expect("the daemon stops when asked");
        daemon.thread.join().expect("the loop ends cleanly");
    });
}
