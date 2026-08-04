//! Contract tests for the public `oxabl/*` method surface (U8).

use std::fs;
use std::path::{Path, PathBuf};

use oxabl_daemon::{ClientContext, Dispatch, SessionHost, default_dispatch};
use oxabl_daemon_protocol::{
    ClientKind, FreshnessRequest, FreshnessResponse, HandshakeRequest, ImpactRequest,
    ImpactResponse, IndexState, Provenance, ReindexRequest, ReindexResponse, Subject,
    SymbolSearchRequest, SymbolSearchResponse, method,
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
        let root = tempfile::tempdir().expect("a workspace");
        fs::write(
            root.path().join("oxabl.toml"),
            "[workspace]\nname = \"methods\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
        )
        .unwrap();
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

fn reindex(
    dispatch: &Dispatch,
    host: &SessionHost,
    context: &mut ClientContext,
) -> ReindexResponse {
    call(dispatch, host, context, method::REINDEX, &ReindexRequest {})
}

fn impact(
    dispatch: &Dispatch,
    host: &SessionHost,
    context: &mut ClientContext,
    subject: Subject,
) -> ImpactResponse {
    call(
        dispatch,
        host,
        context,
        method::IMPACT,
        &ImpactRequest { subject },
    )
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

    let stale: FreshnessResponse = call(
        &dispatch,
        &host,
        &mut client,
        method::FRESHNESS,
        &FreshnessRequest {},
    );
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

    let freshness: FreshnessResponse = call(
        &dispatch,
        &host,
        &mut client,
        method::FRESHNESS,
        &FreshnessRequest {},
    );
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
