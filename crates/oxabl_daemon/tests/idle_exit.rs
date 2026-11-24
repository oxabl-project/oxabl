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
