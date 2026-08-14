//! Two clients, one daemon, one session (R6, R9, R11).
//!
//! Over a real Unix socket, because the claim is about the transport as much as the
//! session map: a second client that gets its own session has not shared an index,
//! however tidy the map looks in a unit test.
//!
//! The tests include abrupt disconnects because socket teardown is part of the
//! transport contract. A disconnected client must not leave either peer's framing
//! thread blocked on a socket read.

#![cfg(unix)]

use std::os::unix::net::UnixStream;
use std::path::Path;
use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering};
use std::time::{Duration, Instant};

use lsp_server::{Message, Request, RequestId, Response};
use oxabl_daemon::dispatch::Dispatch;
use oxabl_daemon::{
    Discovery, Listener, SessionHost, connection_over, discover, register_handshake,
};
use oxabl_daemon_protocol::{CONTRACT_VERSION, ClientKind, HandshakeRequest, HandshakeResponse};
use serde_json::{Value, json};

/// Point the registration directory at a fresh temporary directory for the duration.
///
/// `XDG_CACHE_HOME` is process-wide, so this serialises every test in the file
/// behind one lock. Without that they race each other's registrations.
fn with_cache_home<T>(body: impl FnOnce() -> T) -> T {
    static LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
    let _guard = LOCK.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
    let cache = tempfile::tempdir().expect("a temporary cache directory");
    let previous = std::env::var_os("XDG_CACHE_HOME");
    // SAFETY: the lock above makes this the only thread mutating the environment.
    unsafe { std::env::set_var("XDG_CACHE_HOME", cache.path()) };
    let out = body();
    unsafe {
        match previous {
            Some(value) => std::env::set_var("XDG_CACHE_HOME", value),
            None => std::env::remove_var("XDG_CACHE_HOME"),
        }
    }
    out
}

/// The method surface these tests drive: a handshake that shares sessions, a counter
/// of live sessions, and a deliberately slow method.
///
/// The slow one sleeps **without holding the session lock**, which is the discipline
/// every real query handler follows — take the lock to write or to clone a snapshot,
/// then release it. A handler that slept under the lock would serialise the daemon,
/// and the concurrency test below is what would catch it.
fn dispatch(slow_calls: Arc<AtomicU32>) -> Arc<Dispatch> {
    let mut dispatch = Dispatch::new();
    register_handshake(&mut dispatch);
    dispatch.register("oxabl/sessionCount", |host: &SessionHost, _, _| {
        Ok(json!(host.with(|sessions| sessions.len())))
    });
    dispatch.register("oxabl/slow", move |_host: &SessionHost, _, _| {
        slow_calls.fetch_add(1, Ordering::SeqCst);
        std::thread::sleep(Duration::from_millis(300));
        Ok(json!("slow done"))
    });
    dispatch.register("oxabl/boom", |_, _, _| panic!("deliberate"));
    Arc::new(dispatch)
}

/// A connected client, with the request id it will use next.
struct Client {
    connection: lsp_server::Connection,
    threads: Option<oxabl_daemon::listener::ClientThreads>,
    next_id: i32,
}

impl Client {
    fn connect(socket: &Path) -> Self {
        let stream = UnixStream::connect(socket).expect("the daemon is listening");
        let (connection, threads) = connection_over(stream);
        Client {
            connection,
            threads: Some(threads),
            next_id: 1,
        }
    }

    /// Send a request and wait for its response.
    fn call(&mut self, method: &str, params: Value) -> Response {
        let id = RequestId::from(self.next_id);
        self.next_id += 1;
        self.connection
            .sender
            .send(Message::Request(Request {
                id: id.clone(),
                method: method.to_string(),
                params,
            }))
            .expect("the request is sent");
        loop {
            match self
                .connection
                .receiver
                .recv_timeout(Duration::from_secs(10))
                .expect("a response arrives")
            {
                Message::Response(response) if response.id == id => return response,
                _ => continue,
            }
        }
    }

    /// Send a request without waiting, for the concurrency test.
    fn send(&mut self, method: &str, params: Value) -> RequestId {
        let id = RequestId::from(self.next_id);
        self.next_id += 1;
        self.connection
            .sender
            .send(Message::Request(Request {
                id: id.clone(),
                method: method.to_string(),
                params,
            }))
            .expect("the request is sent");
        id
    }

    fn await_response(&self, id: &RequestId) -> Response {
        loop {
            match self
                .connection
                .receiver
                .recv_timeout(Duration::from_secs(10))
                .expect("a response arrives")
            {
                Message::Response(response) if response.id == *id => return response,
                _ => continue,
            }
        }
    }

    /// Disconnect without an orderly shutdown, the way a crashing client does.
    fn abandon(mut self) {
        drop(self.connection);
        if let Some(threads) = self.threads.take() {
            threads.shutdown();
        }
    }
}

/// Stand a daemon up on a temporary root and run `body` against its socket.
fn with_daemon<T>(root: &str, body: impl FnOnce(&Path, Arc<AtomicU32>) -> T) -> T {
    with_cache_home(|| {
        let slow_calls = Arc::new(AtomicU32::new(0));
        let listener = Listener::bind(root).expect("the socket binds");
        let socket = listener.socket_path().to_path_buf();
        let dispatch = dispatch(Arc::clone(&slow_calls));
        let host = Arc::new(SessionHost::new());
        let stopper = listener.stopper();

        let serving = std::thread::spawn(move || {
            let _ = listener.accept_loop(dispatch, host);
        });

        let out = body(&socket, slow_calls);

        stopper.stop();
        let _ = serving.join();
        out
    })
}

#[test]
fn the_daemon_registers_and_accepts_a_connection() {
    with_daemon("/proj/accepts", |socket, _| {
        match discover(Path::new("/proj/accepts")) {
            Discovery::Live(registration) => {
                assert_eq!(registration.pid, std::process::id());
                assert_eq!(registration.socket_path, socket.to_string_lossy());
                assert_eq!(registration.contract_version, CONTRACT_VERSION);
            }
            other => panic!("the daemon must be discoverable, got {other:?}"),
        }

        let mut client = Client::connect(socket);
        let response = client.call("oxabl/sessionCount", Value::Null);
        assert_eq!(response.result, Some(json!(0)));
        client.abandon();
    });
}

/// The property the daemon exists for.
#[test]
fn two_clients_connect_concurrently_and_share_one_session() {
    with_daemon("/proj/shared", |socket, _| {
        let root = "/proj/shared";
        let mut editor = Client::connect(socket);
        let mut desktop = Client::connect(socket);

        let first = editor.call(
            "oxabl/handshake",
            serde_json::to_value(HandshakeRequest::new(ClientKind::Editor, root)).unwrap(),
        );
        let first: HandshakeResponse = serde_json::from_value(first.result.expect("a result"))
            .expect("the handshake response deserialises");
        assert_eq!(first.connected_clients, 1);

        let second = desktop.call(
            "oxabl/handshake",
            serde_json::to_value(HandshakeRequest::new(ClientKind::Desktop, root)).unwrap(),
        );
        let second: HandshakeResponse = serde_json::from_value(second.result.expect("a result"))
            .expect("the handshake response deserialises");
        assert_eq!(
            second.connected_clients, 2,
            "the second client joins the first client's session"
        );
        assert_eq!(second.daemon_version, oxabl_daemon::DAEMON_VERSION);

        // One session for one root, whichever client asks.
        assert_eq!(
            editor.call("oxabl/sessionCount", Value::Null).result,
            Some(json!(1)),
            "two clients on one root must hold one session between them"
        );

        editor.abandon();
        desktop.abandon();
    });
}

#[test]
fn two_workspace_roots_produce_two_sessions() {
    with_daemon("/proj/multi", |socket, _| {
        let mut client = Client::connect(socket);
        for root in ["/proj/multi/alpha", "/proj/multi/beta"] {
            client.call(
                "oxabl/handshake",
                serde_json::to_value(HandshakeRequest::new(ClientKind::Desktop, root)).unwrap(),
            );
        }
        assert_eq!(
            client.call("oxabl/sessionCount", Value::Null).result,
            Some(json!(2))
        );
        client.abandon();
    });
}

/// A slow response to one client must not stall the other.
///
/// The slow handler sleeps without holding the session lock, which is the discipline
/// every query handler follows. If a handler queried under the lock, the fast client
/// below would wait out the slow one and this test would fail — which is the point.
#[test]
fn a_slow_response_to_one_client_does_not_stall_the_other() {
    with_daemon("/proj/concurrent", |socket, slow_calls| {
        let mut slow = Client::connect(socket);
        let mut fast = Client::connect(socket);

        let slow_id = slow.send("oxabl/slow", Value::Null);
        // Wait until the slow handler is genuinely running, so the timing below is
        // measuring concurrency rather than a race to start.
        let deadline = Instant::now() + Duration::from_secs(5);
        while slow_calls.load(Ordering::SeqCst) == 0 && Instant::now() < deadline {
            std::thread::sleep(Duration::from_millis(5));
        }
        assert_eq!(
            slow_calls.load(Ordering::SeqCst),
            1,
            "the slow call started"
        );

        let started = Instant::now();
        let response = fast.call("oxabl/sessionCount", Value::Null);
        let elapsed = started.elapsed();
        assert_eq!(response.result, Some(json!(0)));
        assert!(
            elapsed < Duration::from_millis(250),
            "the fast client waited {elapsed:?} on the slow one's answer"
        );

        // And the slow one still gets its answer.
        assert_eq!(
            slow.await_response(&slow_id).result,
            Some(json!("slow done"))
        );

        slow.abandon();
        fast.abandon();
    });
}

/// A client disconnecting mid-request affects neither the other client nor the
/// session.
#[test]
fn a_client_disconnecting_mid_request_leaves_the_other_working() {
    with_daemon("/proj/leaving", |socket, slow_calls| {
        let root = "/proj/leaving";
        let mut staying = Client::connect(socket);
        staying.call(
            "oxabl/handshake",
            serde_json::to_value(HandshakeRequest::new(ClientKind::Desktop, root)).unwrap(),
        );

        let mut leaving = Client::connect(socket);
        leaving.send("oxabl/slow", Value::Null);
        let deadline = Instant::now() + Duration::from_secs(5);
        while slow_calls.load(Ordering::SeqCst) == 0 && Instant::now() < deadline {
            std::thread::sleep(Duration::from_millis(5));
        }
        // Gone before its response can be delivered.
        leaving.abandon();

        assert_eq!(
            staying.call("oxabl/sessionCount", Value::Null).result,
            Some(json!(1)),
            "the session survives a client vanishing mid-request"
        );
        staying.abandon();
    });
}

/// A panic serving one client leaves the other client's session working.
#[test]
fn a_panic_serving_one_client_leaves_the_other_working() {
    with_daemon("/proj/panics", |socket, _| {
        let root = "/proj/panics";
        let mut unlucky = Client::connect(socket);
        let mut other = Client::connect(socket);
        other.call(
            "oxabl/handshake",
            serde_json::to_value(HandshakeRequest::new(ClientKind::Editor, root)).unwrap(),
        );

        let previous = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        let response = unlucky.call("oxabl/boom", Value::Null);
        std::panic::set_hook(previous);

        let error = response.error.expect("the panicking request is reported");
        assert!(error.message.contains("deliberate"), "got {error:?}");

        assert_eq!(
            other.call("oxabl/sessionCount", Value::Null).result,
            Some(json!(1)),
            "the other client's session must be untouched"
        );
        // And the client whose request panicked is still connected and serving.
        assert_eq!(
            unlucky.call("oxabl/sessionCount", Value::Null).result,
            Some(json!(1)),
            "one failed request must not close the connection"
        );

        unlucky.abandon();
        other.abandon();
    });
}

/// A client at a mismatched contract version is refused, and the refusal names both
/// versions (R11).
#[test]
fn a_mismatched_contract_version_is_refused_naming_both_versions() {
    with_daemon("/proj/mismatch", |socket, _| {
        let mut client = Client::connect(socket);
        let stale = HandshakeRequest {
            contract_version: CONTRACT_VERSION + 9,
            client: ClientKind::Desktop,
            workspace_root: "/proj/mismatch".to_string(),
        };
        let response = client.call("oxabl/handshake", serde_json::to_value(&stale).unwrap());

        let error = response.error.expect("the handshake is refused");
        assert!(
            error.message.contains(&(CONTRACT_VERSION + 9).to_string())
                && error.message.contains(&CONTRACT_VERSION.to_string()),
            "the refusal must name both versions, got {}",
            error.message
        );
        // And no session was created for the refused client.
        assert_eq!(
            client.call("oxabl/sessionCount", Value::Null).result,
            Some(json!(0)),
            "a refused client must not have been given a session"
        );
        client.abandon();
    });
}

/// A second daemon on one root is refused rather than stealing the socket, which
/// would leave the running daemon unreachable and two indexes over one workspace.
#[test]
fn a_second_daemon_on_one_root_is_refused() {
    with_daemon("/proj/single", |_, _| {
        let Err(error) = Listener::bind("/proj/single") else {
            panic!("a second daemon on one root must be refused");
        };
        assert_eq!(error.kind(), std::io::ErrorKind::AddrInUse);
        assert!(
            error.to_string().contains(&std::process::id().to_string()),
            "the refusal should name the daemon that holds it, got {error}"
        );
    });
}

/// An orderly exit leaves no live-looking registration, so the next client starts a
/// daemon rather than connecting to a socket nobody holds.
#[test]
fn an_orderly_shutdown_removes_the_registration() {
    with_cache_home(|| {
        let root = Path::new("/proj/tidy");
        {
            let listener = Listener::bind(root).expect("the socket binds");
            assert!(matches!(discover(root), Discovery::Live(_)));
            assert!(listener.socket_path().exists());
        }
        assert_eq!(
            discover(root),
            Discovery::Absent,
            "dropping the listener must clear the registration"
        );
    });
}

// ---- A query is read-only with respect to shared session state (R1) ---------
//
// These drive the dispatch in-process rather than over the socket: the claim is
// about what a handler writes into the shared session, and a socket between the
// caller and that session would only hide it.

/// A workspace whose own `oxabl.toml` puts the include search path at the root,
/// while an editor client resolved a different one for itself — the subdirectory.
///
/// The two configurations have to be genuinely different, or a query that
/// overwrote the session's configuration would install the same bytes and the
/// tests would pass by coincidence.
struct SharedRoot {
    dir: tempfile::TempDir,
}

/// The buffer the editor client holds open. It resolves under the editor's include
/// path and under no other.
const EDITOR_BUFFER: &str = "{shared.i}\nMESSAGE shared.\n";

/// The same buffer after one keystroke, to force a recompute that cannot be served
/// from the memo.
const EDITOR_BUFFER_EDITED: &str = "{shared.i}\nMESSAGE shared.\nMESSAGE \"typed\".\n";

impl SharedRoot {
    fn new() -> Self {
        let dir = tempfile::tempdir().expect("a workspace root");
        std::fs::write(
            dir.path().join("oxabl.toml"),
            "[workspace]\nname = \"shared\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
        )
        .unwrap();
        std::fs::create_dir(dir.path().join("inc")).unwrap();
        std::fs::write(
            dir.path().join("inc").join("shared.i"),
            "DEFINE VARIABLE shared AS INTEGER NO-UNDO.\n",
        )
        .unwrap();
        std::fs::write(
            dir.path().join("base.i"),
            "DEFINE VARIABLE fromBase AS INTEGER NO-UNDO.\n",
        )
        .unwrap();
        std::fs::write(dir.path().join("direct.p"), "{base.i}\nMESSAGE fromBase.\n").unwrap();
        std::fs::write(dir.path().join("main.p"), EDITOR_BUFFER).unwrap();
        SharedRoot { dir }
    }

    fn path(&self) -> &Path {
        self.dir.path()
    }

    fn main_path(&self) -> std::path::PathBuf {
        self.path().join("main.p")
    }

    /// The configuration the editor client resolved for itself.
    fn editor_config(&self) -> oxabl_pipeline::PipelineConfig {
        oxabl_pipeline::PipelineConfig {
            include_paths: vec![self.path().join("inc")],
            ..oxabl_pipeline::PipelineConfig::default()
        }
    }
}

/// Attach an in-process client to `root` and return its context.
fn attach(
    dispatch: &oxabl_daemon::Dispatch,
    host: &SessionHost,
    root: &Path,
    kind: ClientKind,
) -> oxabl_daemon::ClientContext {
    let mut context = oxabl_daemon::ClientContext::default();
    dispatch
        .call(
            host,
            &mut context,
            oxabl_daemon_protocol::method::HANDSHAKE,
            serde_json::to_value(HandshakeRequest::new(kind, root.to_string_lossy())).unwrap(),
        )
        .expect("the handshake is accepted");
    context
}

/// Ask `oxabl/impact` about one file.
fn impact_on(
    dispatch: &oxabl_daemon::Dispatch,
    host: &SessionHost,
    context: &mut oxabl_daemon::ClientContext,
    file: &Path,
) -> oxabl_daemon_protocol::ImpactResponse {
    let params = serde_json::to_value(oxabl_daemon_protocol::ImpactRequest {
        subject: oxabl_daemon_protocol::Subject::File {
            path: file.to_string_lossy().into_owned(),
        },
    })
    .unwrap();
    let value = dispatch
        .call(host, context, oxabl_daemon_protocol::method::IMPACT, params)
        .expect("impact answers");
    available(serde_json::from_value(value).unwrap())
}

/// Unwrap an answer a cross-file method may refuse (R22). Every root here is
/// configured, so a refusal is a test failure that names its own cause.
fn available<T>(answer: oxabl_daemon_protocol::Sourced<T>) -> T {
    match answer {
        oxabl_daemon_protocol::Sourced::Available { value } => value,
        oxabl_daemon_protocol::Sourced::Unavailable { reason } => {
            panic!("the daemon refused the question: {reason}")
        }
    }
}

/// Ask `oxabl/freshness`.
fn freshness_of(
    dispatch: &oxabl_daemon::Dispatch,
    host: &SessionHost,
    context: &mut oxabl_daemon::ClientContext,
) -> oxabl_daemon_protocol::FreshnessResponse {
    let value = dispatch
        .call(
            host,
            context,
            oxabl_daemon_protocol::method::FRESHNESS,
            serde_json::json!({}),
        )
        .expect("freshness answers");
    available(serde_json::from_value(value).unwrap())
}

/// Open (or re-type) the editor's buffer, returning its salsa input.
fn type_buffer(host: &SessionHost, workspace: &SharedRoot, text: &str) -> oxabl_daemon::db::Buffer {
    host.with(|sessions| {
        sessions.for_root(workspace.path()).set_buffer(
            "main.p",
            text.to_string(),
            Some(workspace.main_path()),
        )
    })
}

/// Whether the buffer's diagnostics report an include the preprocessor could not
/// find — the observable difference between the two configurations.
fn include_unresolved(host: &SessionHost, root: &Path, buffer: oxabl_daemon::db::Buffer) -> bool {
    host.with(|sessions| {
        let session = sessions.for_root(root);
        let schema = session.schema_handle();
        let snapshot = session.database().clone();
        oxabl_daemon::db::compute_diagnostics(&snapshot, buffer, schema)
            .expect("nothing is writing, so the read completes")
            .all()
            .any(|carried| carried.diagnostic.code.0 == "PREPROC007")
    })
}

/// A read query must leave the configuration another client resolved in place.
///
/// The recompute after the query is the point. The configuration is a plain field,
/// not a salsa input, so a query that overwrote it corrupts nothing that is already
/// memoized — the first wrong answer is the next one computed, which here is the
/// one after a keystroke.
#[test]
fn a_query_leaves_another_clients_configuration_installed() {
    let workspace = SharedRoot::new();
    let dispatch = oxabl_daemon::default_dispatch();
    let host = SessionHost::new();

    let mut editor = attach(&dispatch, &host, workspace.path(), ClientKind::Editor);
    host.with(|sessions| {
        sessions
            .for_root(workspace.path())
            .install_config(workspace.editor_config())
    });
    let buffer = type_buffer(&host, &workspace, EDITOR_BUFFER);
    assert!(
        !include_unresolved(&host, workspace.path(), buffer),
        "the editor's own include path resolves the include"
    );

    let mut desktop = attach(&dispatch, &host, workspace.path(), ClientKind::Desktop);
    impact_on(
        &dispatch,
        &host,
        &mut desktop,
        &workspace.path().join("base.i"),
    );

    let edited = type_buffer(&host, &workspace, EDITOR_BUFFER_EDITED);
    assert!(
        !include_unresolved(&host, workspace.path(), edited),
        "the editor's next recompute must still run under the configuration it resolved"
    );

    // And nothing about the editor's own session moved under it.
    let _ = freshness_of(&dispatch, &host, &mut editor);
}

/// A read query must not bump the configuration generation.
///
/// The generation is a gate: an in-flight worker discards its result when the
/// generation it started under is gone. A query that bumped it would throw away
/// work nobody asked to redo and force a republish.
#[test]
fn a_query_does_not_bump_the_configuration_generation() {
    let workspace = SharedRoot::new();
    let dispatch = oxabl_daemon::default_dispatch();
    let host = SessionHost::new();

    host.with(|sessions| {
        sessions
            .for_root(workspace.path())
            .install_config(workspace.editor_config())
    });
    let before = host.with(|sessions| sessions.for_root(workspace.path()).config_generation());

    let mut desktop = attach(&dispatch, &host, workspace.path(), ClientKind::Desktop);
    impact_on(
        &dispatch,
        &host,
        &mut desktop,
        &workspace.path().join("base.i"),
    );

    let after = host.with(|sessions| sessions.for_root(workspace.path()).config_generation());
    assert_eq!(
        before, after,
        "a read query must not look like a configuration change"
    );
}

/// A read query must leave the built workspace standing.
///
/// Dropping it is a configuration change's job. A query that dropped it would make
/// the next client's freshness answer report indexing on a workspace that is built.
///
/// Paired with the configuration-change test below, which asserts the drop *does*
/// happen there: together they place the drop where it belongs. This half holds
/// whichever way the query is written — a pass that dropped the workspace put its
/// own back on the next line — so it pins the invariant rather than a difference.
#[test]
fn a_query_leaves_the_built_workspace_in_place() {
    let workspace = SharedRoot::new();
    let dispatch = oxabl_daemon::default_dispatch();
    let host = SessionHost::new();

    let mut desktop = attach(&dispatch, &host, workspace.path(), ClientKind::Desktop);
    impact_on(
        &dispatch,
        &host,
        &mut desktop,
        &workspace.path().join("base.i"),
    );

    let answer = freshness_of(&dispatch, &host, &mut desktop);
    assert!(
        !matches!(
            answer.freshness.state,
            oxabl_daemon_protocol::IndexState::Indexing { .. }
        ),
        "the query built a workspace and must have left it installed, got {:?}",
        answer.freshness.state
    );
}

/// A genuine configuration change still installs, still bumps the generation, and
/// still drops the workspace — this narrowed the write rather than removing it.
#[test]
fn a_configuration_change_still_installs_and_still_invalidates() {
    let workspace = SharedRoot::new();
    let dispatch = oxabl_daemon::default_dispatch();
    let host = SessionHost::new();

    let mut desktop = attach(&dispatch, &host, workspace.path(), ClientKind::Desktop);
    impact_on(
        &dispatch,
        &host,
        &mut desktop,
        &workspace.path().join("base.i"),
    );
    let before = host.with(|sessions| sessions.for_root(workspace.path()).config_generation());

    host.with(|sessions| {
        sessions
            .for_root(workspace.path())
            .install_config(workspace.editor_config())
    });

    let (generation, installed) = host.with(|sessions| {
        let session = sessions.for_root(workspace.path());
        (
            session.config_generation(),
            session.database().config().pipeline.include_paths.clone(),
        )
    });
    assert_eq!(generation, before + 1, "the change bumped the generation");
    assert_eq!(
        installed,
        vec![workspace.path().join("inc")],
        "the change installed the configuration it carried"
    );

    let answer = freshness_of(&dispatch, &host, &mut desktop);
    assert!(
        matches!(
            answer.freshness.state,
            oxabl_daemon_protocol::IndexState::Indexing { .. }
        ),
        "the change dropped the workspace, so the next answer is indexing again, got {:?}",
        answer.freshness.state
    );
}

/// The same question gets the same answer whether or not another client attached
/// first — a second client shares the session, it does not change what it reports.
#[test]
fn impact_answers_the_same_with_or_without_an_earlier_client() {
    let workspace = SharedRoot::new();
    let subject = workspace.path().join("base.i");

    let alone = {
        let dispatch = oxabl_daemon::default_dispatch();
        let host = SessionHost::new();
        let mut desktop = attach(&dispatch, &host, workspace.path(), ClientKind::Desktop);
        impact_on(&dispatch, &host, &mut desktop, &subject)
    };

    let shared = {
        let dispatch = oxabl_daemon::default_dispatch();
        let host = SessionHost::new();
        let _editor = attach(&dispatch, &host, workspace.path(), ClientKind::Editor);
        host.with(|sessions| {
            sessions
                .for_root(workspace.path())
                .install_config(workspace.editor_config())
        });
        type_buffer(&host, &workspace, EDITOR_BUFFER);
        let mut desktop = attach(&dispatch, &host, workspace.path(), ClientKind::Desktop);
        impact_on(&dispatch, &host, &mut desktop, &subject)
    };

    assert_eq!(alone.direct_reference_count, shared.direct_reference_count);
    assert_eq!(alone.groups, shared.groups);
    assert_eq!(alone.rebuild_set, shared.rebuild_set);
}
