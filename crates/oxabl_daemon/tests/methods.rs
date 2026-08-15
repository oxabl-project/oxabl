//! Contract tests for the public `oxabl/*` method surface (U8).

use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use oxabl_daemon::{ClientContext, Dispatch, SessionHost, analyze_guarded, default_dispatch};
use oxabl_daemon_protocol::{
    ClientKind, FreshnessRequest, FreshnessResponse, HandshakeRequest, ImpactRequest,
    ImpactResponse, IndexState, Provenance, ReindexRequest, ReindexResponse, Sourced,
    StalenessCause, Subject, SymbolSearchRequest, SymbolSearchResponse, method,
};
use serde::Serialize;
use serde::de::DeserializeOwned;

struct Fixture {
    root: tempfile::TempDir,
    base: PathBuf,
    direct: PathBuf,
    overlay: PathBuf,
}

impl Fixture {
    fn new() -> Self {
        Fixture::build(Some(
            "[workspace]\nname = \"methods\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
        ))
    }

    /// The same sources with no `oxabl.toml` at all, so nothing resolves an
    /// include path (R22).
    fn without_configuration() -> Self {
        Fixture::build(None)
    }

    /// The same sources under a configuration that exists and names no include
    /// path — the second way a workspace ends up with nothing to resolve against.
    fn with_configuration(config: &str) -> Self {
        Fixture::build(Some(config))
    }

    fn build(config: Option<&str>) -> Self {
        let root = tempfile::tempdir().expect("a workspace");
        if let Some(config) = config {
            fs::write(root.path().join("oxabl.toml"), config).unwrap();
        }
        let base = root.path().join("base.i");
        let mid = root.path().join("mid.i");
        let direct = root.path().join("direct.p");
        let transitive = root.path().join("transitive.p");
        let overlay = root.path().join("overlay.p");
        fs::write(&base, "DEFINE VARIABLE fromBase AS INTEGER.\n").unwrap();
        fs::write(&mid, "{base.i}\n").unwrap();
        fs::write(&direct, "{base.i}\nMESSAGE fromBase.\n").unwrap();
        fs::write(&transitive, "{mid.i}\nMESSAGE fromBase.\n").unwrap();
        fs::write(&overlay, "MESSAGE \"disk\".\n").unwrap();
        fs::write(
            root.path().join("symbols.p"),
            "FUNCTION Calculate RETURNS INTEGER (): RETURN 1. END FUNCTION.\n",
        )
        .unwrap();
        fs::write(root.path().join("missing-user.p"), "{Missing.i}\n").unwrap();
        Fixture {
            root,
            base,
            direct,
            overlay,
        }
    }

    fn root(&self) -> &Path {
        self.root.path()
    }
}

fn handshake(
    dispatch: &Dispatch,
    host: &SessionHost,
    root: &Path,
    kind: ClientKind,
) -> ClientContext {
    let mut context = ClientContext::default();
    let _: serde_json::Value = call(
        dispatch,
        host,
        &mut context,
        method::HANDSHAKE,
        &HandshakeRequest::new(kind, root.to_string_lossy()),
    );
    context
}

fn call<P: Serialize, R: DeserializeOwned>(
    dispatch: &Dispatch,
    host: &SessionHost,
    context: &mut ClientContext,
    method: &str,
    params: &P,
) -> R {
    let value = dispatch
        .call(host, context, method, serde_json::to_value(params).unwrap())
        .unwrap_or_else(|error| panic!("{method} failed: {error}"));
    serde_json::from_value(value).unwrap()
}

fn reindex_answer(
    dispatch: &Dispatch,
    host: &SessionHost,
    context: &mut ClientContext,
) -> Sourced<ReindexResponse> {
    call(dispatch, host, context, method::REINDEX, &ReindexRequest {})
}

fn reindex(
    dispatch: &Dispatch,
    host: &SessionHost,
    context: &mut ClientContext,
) -> ReindexResponse {
    expect_available(reindex_answer(dispatch, host, context))
}

/// The value a cross-file method answered with, or a failure naming its refusal.
///
/// Every test that expects an answer goes through this, so a test that starts
/// getting refused fails saying why rather than failing to deserialize (R22).
fn expect_available<T>(answer: Sourced<T>) -> T {
    match answer {
        Sourced::Available { value } => value,
        Sourced::Unavailable { reason } => panic!("the daemon refused the question: {reason}"),
    }
}

fn impact_answer(
    dispatch: &Dispatch,
    host: &SessionHost,
    context: &mut ClientContext,
    subject: Subject,
) -> Sourced<ImpactResponse> {
    call(
        dispatch,
        host,
        context,
        method::IMPACT,
        &ImpactRequest { subject },
    )
}

fn impact(
    dispatch: &Dispatch,
    host: &SessionHost,
    context: &mut ClientContext,
    subject: Subject,
) -> ImpactResponse {
    expect_available(impact_answer(dispatch, host, context, subject))
}

fn freshness_answer(
    dispatch: &Dispatch,
    host: &SessionHost,
    context: &mut ClientContext,
) -> Sourced<FreshnessResponse> {
    call(
        dispatch,
        host,
        context,
        method::FRESHNESS,
        &FreshnessRequest {},
    )
}

fn freshness_response(
    dispatch: &Dispatch,
    host: &SessionHost,
    context: &mut ClientContext,
) -> FreshnessResponse {
    expect_available(freshness_answer(dispatch, host, context))
}

#[test]
fn impact_groups_causes_and_keeps_the_rebuild_set_distinct() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    reindex(&dispatch, &host, &mut client);

    let answer = impact(
        &dispatch,
        &host,
        &mut client,
        Subject::File {
            path: fixture.base.to_string_lossy().into_owned(),
        },
    );

    assert_eq!(answer.direct_reference_count, 2);
    assert_eq!(answer.groups.len(), 2);
    assert!(answer.groups.iter().any(|group| {
        group.cause == oxabl_daemon_protocol::Cause::DirectInclude
            && group
                .files
                .iter()
                .any(|row| row.path == fixture.direct.to_string_lossy())
    }));
    assert!(
        answer
            .groups
            .iter()
            .any(|group| { group.cause == oxabl_daemon_protocol::Cause::TransitiveInclude })
    );
    assert!(
        answer
            .rebuild_set
            .contains(&fixture.base.to_string_lossy().into_owned()),
        "the changed file is part of the rebuild set"
    );
    assert!(answer.rebuild_set.len() > answer.direct_reference_count as usize);
}

#[test]
fn unresolved_rows_keep_their_reason_and_do_not_enter_resolved_counts() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    reindex(&dispatch, &host, &mut client);

    let answer = impact(
        &dispatch,
        &host,
        &mut client,
        Subject::File {
            path: fixture
                .root()
                .join("Missing.i")
                .to_string_lossy()
                .into_owned(),
        },
    );
    assert_eq!(answer.direct_reference_count, 0);
    assert!(answer.groups.is_empty());
    assert_eq!(answer.unresolved.len(), 1);
    assert_eq!(answer.unresolved[0].reason, "absent_from_workspace");
}

#[test]
fn an_editor_overlay_changes_edges_and_stamps_the_working_tree() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let _editor = handshake(&dispatch, &host, fixture.root(), ClientKind::Editor);
    let mut desktop = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    host.with(|sessions| {
        sessions.for_root(fixture.root()).set_buffer(
            "overlay.p",
            "{base.i}\nMESSAGE fromBase.\n".to_string(),
            Some(fixture.overlay.clone()),
        );
    });

    reindex(&dispatch, &host, &mut desktop);
    let answer = impact(
        &dispatch,
        &host,
        &mut desktop,
        Subject::File {
            path: fixture.base.to_string_lossy().into_owned(),
        },
    );
    assert!(answer.groups.iter().any(|group| {
        group
            .files
            .iter()
            .any(|row| row.path == fixture.overlay.to_string_lossy())
    }));
    assert_eq!(
        answer.provenance,
        Provenance::WorkingTree {
            editor_clients: 1,
            unsaved_buffers: 1,
        }
    );
}

/// The overlay is still applied when the client reached the workspace through a
/// symlink (R2).
///
/// The failure this pins is silent and worse than a wrong path: the pass keys the
/// unsaved text by the client's spelling while discovery yields the canonical one,
/// so no key matches, every file is read from disk, and the answer still stamps
/// `WorkingTree`. Here the disk copy of `overlay.p` includes nothing, so the edge
/// below exists only if the editor's unsaved text was the text that was read.
#[cfg(unix)]
#[test]
fn an_editor_overlay_survives_a_symlinked_workspace_root() {
    let fixture = Fixture::new();
    let elsewhere = tempfile::tempdir().expect("a directory to hold the link");
    let link = elsewhere.path().join("workspace-link");
    std::os::unix::fs::symlink(fixture.root(), &link).expect("a symlink to the root");

    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let _editor = handshake(&dispatch, &host, &link, ClientKind::Editor);
    let mut desktop = handshake(&dispatch, &host, &link, ClientKind::Desktop);
    assert_eq!(
        host.with(|sessions| sessions.len()),
        1,
        "both clients name one root"
    );

    // The session is keyed by the canonical root; the buffer path is spelled the
    // way the client that opened it spells its own workspace.
    let canonical = fs::canonicalize(fixture.root()).expect("the root resolves");
    host.with(|sessions| {
        sessions.for_root(&canonical).set_buffer(
            "overlay.p",
            "{base.i}\nMESSAGE fromBase.\n".to_string(),
            Some(link.join("overlay.p")),
        );
    });

    reindex(&dispatch, &host, &mut desktop);
    let answer = impact(
        &dispatch,
        &host,
        &mut desktop,
        Subject::File {
            path: canonical.join("base.i").to_string_lossy().into_owned(),
        },
    );
    assert!(
        answer.groups.iter().any(|group| {
            group
                .files
                .iter()
                .any(|row| row.path.ends_with("overlay.p"))
        }),
        "the unsaved include edge is missing, so the pass read the disk copy"
    );
    assert_eq!(
        answer.provenance,
        Provenance::WorkingTree {
            editor_clients: 1,
            unsaved_buffers: 1,
        }
    );
}

#[test]
fn no_editor_means_disk_and_every_answer_names_the_schema_revision() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    reindex(&dispatch, &host, &mut client);
    let answer = impact(
        &dispatch,
        &host,
        &mut client,
        Subject::File {
            path: fixture.base.to_string_lossy().into_owned(),
        },
    );

    assert_eq!(answer.provenance, Provenance::Disk);
    assert_eq!(answer.schema.revision, 0);
    assert!(!answer.schema.loaded);
}

#[test]
fn reindex_replaces_a_stale_graph_and_marks_the_next_answer_fresh() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    reindex(&dispatch, &host, &mut client);
    fs::write(&fixture.direct, "MESSAGE \"changed and longer\".\n").unwrap();

    let stale = freshness_response(&dispatch, &host, &mut client);
    assert!(matches!(stale.freshness.state, IndexState::Stale { .. }));
    let rebuilt = reindex(&dispatch, &host, &mut client);
    assert_eq!(rebuilt.freshness.state, IndexState::Ready);
    let answer = impact(
        &dispatch,
        &host,
        &mut client,
        Subject::File {
            path: fixture.base.to_string_lossy().into_owned(),
        },
    );
    assert_eq!(answer.freshness.state, IndexState::Ready);
    assert_eq!(answer.direct_reference_count, 1);
}

#[test]
fn changing_a_shared_include_marks_the_graph_stale() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    reindex(&dispatch, &host, &mut client);
    fs::write(
        &fixture.base,
        "DEFINE VARIABLE fromBase AS INTEGER. /* changed */\n",
    )
    .unwrap();

    let freshness = freshness_response(&dispatch, &host, &mut client);
    assert!(matches!(
        freshness.freshness.state,
        IndexState::Stale { changed_files: 1 }
    ));
}

#[test]
fn editor_and_desktop_clients_receive_the_same_impact_facts() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut editor = handshake(&dispatch, &host, fixture.root(), ClientKind::Editor);
    let mut desktop = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    reindex(&dispatch, &host, &mut desktop);
    let subject = Subject::File {
        path: fixture.base.to_string_lossy().into_owned(),
    };
    let from_editor = impact(&dispatch, &host, &mut editor, subject.clone());
    let from_desktop = impact(&dispatch, &host, &mut desktop, subject);

    assert_eq!(from_editor.groups, from_desktop.groups);
    assert_eq!(from_editor.unresolved, from_desktop.unresolved);
    assert_eq!(from_editor.rebuild_set, from_desktop.rebuild_set);
    assert_eq!(from_editor.provenance, from_desktop.provenance);
}

#[test]
fn symbol_search_finds_a_declaration_and_an_absence_is_an_empty_answer() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    reindex(&dispatch, &host, &mut client);

    let found: SymbolSearchResponse = call(
        &dispatch,
        &host,
        &mut client,
        method::SYMBOL_SEARCH,
        &SymbolSearchRequest {
            query: "calc".to_string(),
            limit: 20,
        },
    );
    assert_eq!(found.total_matches, 1);
    assert_eq!(found.symbols[0].name, "Calculate");

    let absent: SymbolSearchResponse = call(
        &dispatch,
        &host,
        &mut client,
        method::SYMBOL_SEARCH,
        &SymbolSearchRequest {
            query: "not-present".to_string(),
            limit: 20,
        },
    );
    assert_eq!(absent.total_matches, 0);
    assert!(absent.symbols.is_empty());
}

#[test]
fn a_query_before_handshake_is_refused() {
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let error = dispatch
        .call(
            &host,
            &mut ClientContext::default(),
            method::FRESHNESS,
            serde_json::to_value(FreshnessRequest {}).unwrap(),
        )
        .expect_err("the session is not known before a handshake");
    assert_eq!(error.code, -32600);
}

// A method that takes no arguments must accept every spelling of "no arguments"
// (R20). A JSON-RPC caller may omit `params` altogether, and the transport hands
// an omitted member over as null — which a struct deserializer refuses unless the
// handler substitutes for it.
#[test]
fn a_no_argument_method_accepts_omitted_null_or_empty_params() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);

    // The omitted case, taken from a real request rather than asserted about: a
    // message with no `params` member is what the transport reads as null.
    let request: serde_json::Value =
        serde_json::from_str(r#"{"jsonrpc":"2.0","id":1,"method":"oxabl/freshness"}"#).unwrap();
    let omitted = request
        .get("params")
        .cloned()
        .unwrap_or(serde_json::Value::Null);

    for params in [omitted, serde_json::Value::Null, serde_json::json!({})] {
        for name in [method::FRESHNESS, method::REINDEX] {
            dispatch
                .call(&host, &mut client, name, params.clone())
                .unwrap_or_else(|error| panic!("{name} with params {params}: {error}"));
        }
    }
}

// A request must return within a bounded number of workspace-pass attempts even
// while an open buffer changes continuously (R4), and it must say so by type
// rather than by an empty answer (R6).
#[test]
fn a_continuously_changing_buffer_answers_within_the_attempt_cap() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let _editor = handshake(&dispatch, &host, fixture.root(), ClientKind::Editor);
    let mut desktop = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);

    // A pass over the bare fixture finishes in well under a millisecond, so one
    // can slip whole between two keystrokes and land current by luck. Widening
    // the workspace makes every attempt long enough that the typist is certain to
    // be seen — the test then measures the bound rather than a coin toss.
    for index in 0..200 {
        fs::write(
            fixture.root().join(format!("typed-{index}.p")),
            "{base.i}\nMESSAGE fromBase.\n",
        )
        .unwrap();
    }

    let typist = Mutator::typist(&host, fixture.root(), &fixture.overlay);
    let answer = reindex(&dispatch, &host, &mut desktop);
    drop(typist);

    match answer.freshness.state {
        IndexState::Superseded { cause, attempts } => {
            assert_eq!(
                cause,
                StalenessCause::BuffersMoved,
                "the label names the buffers, not a generic failure"
            );
            assert!(
                (1..=4).contains(&attempts),
                "the request answered after {attempts} passes, outside the cap"
            );
        }
        other => panic!("a pass the buffers moved under answered as {other:?}"),
    }
    // Distinguishable by type, not by an empty collection: the answer still
    // carries the graph the completed pass built.
    assert!(
        answer.freshness.indexed_files > 0,
        "the answer is populated"
    );
    assert!(answer.graph_bytes > 0, "the answer is populated");

    // Exhausting the cap leaves no progress slot claimed, so a following request
    // still starts a pass — and with the typist stopped it lands cleanly.
    let quiet = reindex(&dispatch, &host, &mut desktop);
    assert_eq!(quiet.freshness.state, IndexState::Ready);
}

// The bound must not turn an ordinary pass into a stale one: with nothing moving
// under it, one pass installs and answers `Ready`.
#[test]
fn a_quiet_pass_still_installs_and_reports_ready() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let _editor = handshake(&dispatch, &host, fixture.root(), ClientKind::Editor);
    let mut desktop = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    host.with(|sessions| {
        sessions.for_root(fixture.root()).set_buffer(
            "overlay.p",
            "{base.i}\nMESSAGE fromBase.\n".to_string(),
            Some(fixture.overlay.clone()),
        );
    });

    let answer = reindex(&dispatch, &host, &mut desktop);
    assert_eq!(answer.freshness.state, IndexState::Ready);

    // Installed, not merely returned: the next query answers from the same pass
    // rather than running another.
    let repeat = impact(
        &dispatch,
        &host,
        &mut desktop,
        Subject::File {
            path: fixture.base.to_string_lossy().into_owned(),
        },
    );
    assert_eq!(repeat.freshness.state, IndexState::Ready);
}

/// Keeps changing one piece of session state for as long as it is held (R3, R4).
///
/// One mover for every kind of change — a typed buffer, a reloaded schema, a
/// replaced configuration — because the three differ only in the line they run,
/// and two copies of this thread machinery drift apart the first time one of them
/// is fixed.
///
/// A tight loop rather than a timer: the pass holds no session lock while it runs,
/// so a sleeping mutator can leave a whole pass unobserved and a request that then
/// answered `Ready` would pass by luck rather than by the property under test.
struct Mutator {
    stop: Arc<AtomicBool>,
    thread: Option<std::thread::JoinHandle<()>>,
}

impl Mutator {
    fn start(
        host: &SessionHost,
        root: &Path,
        mut change: impl FnMut(&mut oxabl_daemon::Session) + Send + 'static,
    ) -> Self {
        let stop = Arc::new(AtomicBool::new(false));
        let thread = {
            let (host, stop, root) = (host.clone(), stop.clone(), root.to_path_buf());
            std::thread::spawn(move || {
                while !stop.load(Ordering::Relaxed) {
                    host.with(|sessions| change(sessions.for_root(&root)));
                }
            })
        };
        Mutator {
            stop,
            thread: Some(thread),
        }
    }

    /// Keeps one open buffer changing, the way a user typing into it does.
    fn typist(host: &SessionHost, root: &Path, path: &Path) -> Self {
        let path = path.to_path_buf();
        let mut keystroke = 0u64;
        Mutator::start(host, root, move |session| {
            keystroke += 1;
            session.set_buffer(
                "overlay.p",
                format!("MESSAGE \"typing {keystroke}\".\n"),
                Some(path.clone()),
            );
        })
    }

    fn schema(host: &SessionHost, root: &Path) -> Self {
        Mutator::start(host, root, |session| session.bump_schema())
    }

    fn configuration(host: &SessionHost, root: &Path) -> Self {
        Mutator::start(host, root, |session| {
            session.install_config(oxabl_pipeline::PipelineConfig::default())
        })
    }
}

impl Drop for Mutator {
    fn drop(&mut self) {
        self.stop.store(true, Ordering::Relaxed);
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
    }
}

/// Make one pass long enough that a change landing mid-pass is certain to be
/// seen. A pass over the bare fixture finishes in well under a millisecond.
fn widen(fixture: &Fixture) {
    for index in 0..200 {
        fs::write(
            fixture.root().join(format!("widen-{index}.p")),
            "{base.i}\nMESSAGE fromBase.\n",
        )
        .unwrap();
    }
}

fn freshness(dispatch: &Dispatch, host: &SessionHost, client: &mut ClientContext) -> IndexState {
    let answer = freshness_response(dispatch, host, client);
    answer.freshness.state
}

fn wait_for_ready(dispatch: &Dispatch, host: &SessionHost, client: &mut ClientContext) {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(30);
    while freshness(dispatch, host, client) != IndexState::Ready {
        assert!(
            std::time::Instant::now() < deadline,
            "the pass never landed once the session settled"
        );
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
}

/// A schema change that lands while a pass runs must not be overwritten by that
/// pass's result (R3).
///
/// Driven through the freshness-started pass, which never spends a final attempt:
/// every pass it runs is superseded and installs nothing, so a poll that ever
/// answered `Ready` would be reporting a graph built under a schema the session
/// has already replaced.
#[test]
fn a_schema_change_during_a_pass_is_not_installed_by_it() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    widen(&fixture);

    let mutator = Mutator::schema(&host, fixture.root());
    let until = std::time::Instant::now() + std::time::Duration::from_millis(500);
    while std::time::Instant::now() < until {
        let state = freshness(&dispatch, &host, &mut client);
        assert_ne!(
            state,
            IndexState::Ready,
            "a pass the schema moved under reported itself current"
        );
    }
    drop(mutator);

    // The refusal is scoped to the change, not a permanent one: once the schema
    // settles, the next pass installs.
    wait_for_ready(&dispatch, &host, &mut client);
}

/// The same for a configuration change (R3). The buffer generation cannot catch
/// this one either: no text moved, only the rules did.
#[test]
fn a_configuration_change_during_a_pass_is_not_installed_by_it() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    widen(&fixture);

    let mutator = Mutator::configuration(&host, fixture.root());
    let until = std::time::Instant::now() + std::time::Duration::from_millis(500);
    while std::time::Instant::now() < until {
        let state = freshness(&dispatch, &host, &mut client);
        assert_ne!(
            state,
            IndexState::Ready,
            "a pass the configuration moved under reported itself current"
        );
    }
    drop(mutator);

    wait_for_ready(&dispatch, &host, &mut client);
}

/// A schema changed on every attempt spends the same bounded budget as a moving
/// buffer, and answers with a result labelled for what actually moved (R3, R4,
/// R6).
#[test]
fn a_schema_changed_on_every_attempt_answers_within_the_attempt_cap() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    widen(&fixture);

    let mutator = Mutator::schema(&host, fixture.root());
    let answer = reindex(&dispatch, &host, &mut client);
    drop(mutator);

    match answer.freshness.state {
        IndexState::Superseded { cause, attempts } => {
            assert_eq!(
                cause,
                StalenessCause::SchemaChanged,
                "the label names the schema, not the buffers"
            );
            assert!(
                (1..=4).contains(&attempts),
                "the request answered after {attempts} passes, outside the cap"
            );
        }
        other => panic!("a pass the schema moved under answered as {other:?}"),
    }
    assert!(
        answer.freshness.indexed_files > 0,
        "the answer is populated"
    );

    let quiet = reindex(&dispatch, &host, &mut client);
    assert_eq!(quiet.freshness.state, IndexState::Ready);
}

/// A schema change with no pass in flight still invalidates the stored pass, so
/// the mid-pass check adds a case rather than replacing the ordinary one.
#[test]
fn a_schema_change_with_no_pass_in_flight_still_invalidates() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    assert_eq!(
        reindex(&dispatch, &host, &mut client).freshness.state,
        IndexState::Ready
    );

    host.with(|sessions| sessions.for_root(fixture.root()).bump_schema());
    assert_ne!(
        freshness(&dispatch, &host, &mut client),
        IndexState::Ready,
        "the stored pass survived a schema change"
    );
    wait_for_ready(&dispatch, &host, &mut client);
}

#[test]
fn freshness_starts_the_first_workspace_pass() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);

    let first = freshness_response(&dispatch, &host, &mut client);
    assert!(matches!(first.freshness.state, IndexState::Indexing { .. }));

    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    loop {
        let current = freshness_response(&dispatch, &host, &mut client);
        if current.freshness.state == IndexState::Ready {
            break;
        }
        assert!(
            std::time::Instant::now() < deadline,
            "the pass did not finish"
        );
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
}

// ---------------------------------------------------------------------------
// A workspace that resolves no include path (R22, R23, KTD13)
// ---------------------------------------------------------------------------

/// The remedy every refusal has to name, so a test fails when the message stops
/// telling the reader what to do about it.
const REMEDY: &str = "[workspace.sources]";

fn refusal(answer: &Sourced<impl std::fmt::Debug>) -> &str {
    match answer {
        Sourced::Available { value } => {
            panic!("the daemon answered instead of refusing: {value:?}")
        }
        Sourced::Unavailable { reason } => reason,
    }
}

#[test]
fn impact_is_refused_when_no_include_configuration_resolves() {
    let fixture = Fixture::without_configuration();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);

    let answer = impact_answer(
        &dispatch,
        &host,
        &mut client,
        Subject::File {
            path: fixture.base.to_string_lossy().into_owned(),
        },
    );

    let reason = refusal(&answer);
    assert!(
        reason.contains("no include path resolved"),
        "the refusal names the cause: {reason}"
    );
    assert!(
        reason.contains(REMEDY) && reason.contains("oxabl.toml"),
        "the refusal names the remedy: {reason}"
    );
}

#[test]
fn freshness_is_refused_rather_than_reporting_ready_without_a_configuration() {
    let fixture = Fixture::without_configuration();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);

    // Twice: the first call is the one that would otherwise start a pass, and the
    // second is the one that would otherwise report `Ready` once it finished.
    for _ in 0..2 {
        let answer = freshness_answer(&dispatch, &host, &mut client);
        let reason = refusal(&answer);
        assert!(reason.contains(REMEDY), "the refusal names the remedy");
    }
}

/// `oxabl/reindex` refuses in the same words as the two methods that read the
/// graph it builds (R22).
///
/// Ungated, it answered `Ready` over a populated file count for a graph no include
/// could reach — so a client that reindexed and then asked for freshness was told
/// the workspace was current and then told the question could not be answered.
#[test]
fn reindex_is_refused_when_no_include_configuration_resolves() {
    let fixture = Fixture::without_configuration();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);

    let rebuilt = reindex_answer(&dispatch, &host, &mut client);
    let asked = freshness_answer(&dispatch, &host, &mut client);
    assert_eq!(
        refusal(&rebuilt),
        refusal(&asked),
        "reindex and freshness must refuse the same workspace in the same words"
    );
    assert!(
        refusal(&rebuilt).contains(REMEDY),
        "the refusal names the remedy: {}",
        refusal(&rebuilt)
    );
}

#[test]
fn a_configuration_naming_no_include_path_is_refused_the_same_way() {
    let unconfigured = Fixture::without_configuration();
    let no_sources = Fixture::with_configuration("[workspace]\nname = \"no-sources\"\n");
    let dispatch = default_dispatch();
    let host = SessionHost::new();

    let mut absent = handshake(&dispatch, &host, unconfigured.root(), ClientKind::Desktop);
    let mut empty = handshake(&dispatch, &host, no_sources.root(), ClientKind::Desktop);

    let from_absent = freshness_answer(&dispatch, &host, &mut absent);
    let from_empty = freshness_answer(&dispatch, &host, &mut empty);
    assert_eq!(
        refusal(&from_absent),
        refusal(&from_empty),
        "a configuration that resolves nothing is the same situation as none"
    );
}

#[test]
fn a_refusal_is_distinguishable_from_a_genuinely_empty_answer() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    reindex(&dispatch, &host, &mut client);

    // A configured workspace whose file nothing includes. The answer is empty and
    // it is still an answer — the two must not share a shape (R22).
    let answer = impact_answer(
        &dispatch,
        &host,
        &mut client,
        Subject::File {
            path: fixture
                .root()
                .join("symbols.p")
                .to_string_lossy()
                .into_owned(),
        },
    );
    let value = expect_available(answer);
    assert!(value.groups.is_empty());
    assert_eq!(value.direct_reference_count, 0);
    assert_eq!(value.freshness.state, IndexState::Ready);
}

#[test]
fn symbol_search_still_answers_without_an_include_configuration() {
    let fixture = Fixture::without_configuration();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);

    // Its rows come from each file's own semantic model and from the schema, not
    // from the dependency graph, so a missing include configuration leaves them
    // populated. Refusing here would remove an answer that still works (R23).
    let found: SymbolSearchResponse = call(
        &dispatch,
        &host,
        &mut client,
        method::SYMBOL_SEARCH,
        &SymbolSearchRequest {
            query: "calc".to_string(),
            limit: 20,
        },
    );
    assert_eq!(found.total_matches, 1);
    assert_eq!(found.symbols[0].name, "Calculate");

    let files: SymbolSearchResponse = call(
        &dispatch,
        &host,
        &mut client,
        method::SYMBOL_SEARCH,
        &SymbolSearchRequest {
            query: "direct".to_string(),
            limit: 20,
        },
    );
    assert!(
        files.symbols.iter().any(|row| row.name == "direct"),
        "the file rows survive an unconfigured workspace"
    );
}

#[test]
fn single_file_analysis_still_runs_without_an_include_configuration() {
    let fixture = Fixture::without_configuration();
    let host = SessionHost::new();
    let key = fixture.direct.to_string_lossy().into_owned();
    host.with(|sessions| {
        sessions.for_root(fixture.root()).set_buffer(
            &key,
            "MESSAGE fromBase.\n".to_string(),
            Some(fixture.direct.clone()),
        );
    });

    let (snapshot, buffer, schema) = host.with(|sessions| {
        let session = sessions.get(fixture.root()).expect("the session exists");
        (
            session.database().clone(),
            session.buffer(&key).expect("the buffer is open"),
            session.schema_handle(),
        )
    });
    let analysis = analyze_guarded(&snapshot, buffer, schema, &key);
    assert!(
        analysis.diagnostics.is_some(),
        "a single file is analysed without any include configuration (R23)"
    );
    assert!(!analysis.panicked);
}

#[test]
fn a_configured_workspace_is_unaffected_by_the_refusal() {
    let fixture = Fixture::new();
    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut client = handshake(&dispatch, &host, fixture.root(), ClientKind::Desktop);
    reindex(&dispatch, &host, &mut client);

    let answer = impact(
        &dispatch,
        &host,
        &mut client,
        Subject::File {
            path: fixture.base.to_string_lossy().into_owned(),
        },
    );
    assert_eq!(answer.direct_reference_count, 2);
    assert_eq!(
        freshness_response(&dispatch, &host, &mut client)
            .freshness
            .state,
        IndexState::Ready
    );
}
