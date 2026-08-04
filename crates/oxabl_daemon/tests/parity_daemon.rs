//! Daemon leg of the shared cross-client parity table (R5, R8).
//!
//! Each observation enters through a daemon [`Session`], then crosses a JSON
//! round trip before comparison. The test therefore pins the daemon's session
//! configuration and transport spellings, not another direct pipeline call that
//! happens to live in this crate.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use oxabl_daemon::db::AnalysisConfig;
use oxabl_daemon::{SessionHost, analyze_guarded};
use oxabl_pipeline::fixtures::{self, FIXTURES, ObservedDiagnostic, ObservedEdges};
use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_workspace::InMemoryFileSystem;
use serde::{Deserialize, Serialize};

fn root() -> &'static Path {
    Path::new(fixtures::PARITY_ROOT)
}

#[derive(Serialize, Deserialize)]
struct WireDiagnostic {
    code: String,
    severity: String,
    source: String,
    start: u32,
    end: u32,
}

#[derive(Serialize, Deserialize)]
struct WireEdges {
    edges: Vec<(String, String)>,
    unresolved: Vec<(String, String, String)>,
}

fn configured_host(
    config: PipelineConfig,
    fs: InMemoryFileSystem,
    key: &str,
    source: &str,
    path: PathBuf,
) -> SessionHost {
    let host = SessionHost::new();
    host.with(|sessions| {
        let session = sessions.for_root(root());
        session.install_analysis_config(AnalysisConfig {
            fs: Arc::new(fs),
            pipeline: Arc::new(config),
            preprocess: true,
            ..Default::default()
        });
        session.set_buffer(key, source.to_string(), Some(path));
    });
    host
}

fn diagnostics_over(
    fixture: &fixtures::ParityFixture,
    config: PipelineConfig,
    fs: InMemoryFileSystem,
) -> Vec<ObservedDiagnostic> {
    let key = fixture.root_path(root()).to_string_lossy().into_owned();
    let host = configured_host(config, fs, &key, fixture.source, fixture.root_path(root()));
    let (snapshot, buffer, schema) = host.with(|sessions| {
        let session = sessions.get(root()).expect("the fixture session exists");
        (
            session.database().clone(),
            session.buffer(&key).expect("the fixture buffer is open"),
            session.schema_handle(),
        )
    });
    let collected = analyze_guarded(&snapshot, buffer, schema, &key)
        .diagnostics
        .expect("a parity fixture returns diagnostics normally");
    let rows: Vec<WireDiagnostic> = collected
        .all()
        .map(|row| WireDiagnostic {
            code: row.diagnostic.code.0.to_string(),
            severity: row.diagnostic.severity.as_str().to_string(),
            source: row.source.as_str().to_string(),
            start: row.diagnostic.span.span.start,
            end: row.diagnostic.span.span.end,
        })
        .collect();
    let body = serde_json::to_vec(&rows).expect("diagnostics serialize for transport");
    serde_json::from_slice::<Vec<WireDiagnostic>>(&body)
        .expect("diagnostics deserialize from transport")
        .into_iter()
        .map(|row| {
            ObservedDiagnostic::from_wire(&row.code, &row.severity, &row.source, row.start, row.end)
        })
        .collect()
}

fn edges_over(
    fixture: &fixtures::ParityFixture,
    config: PipelineConfig,
    fs: InMemoryFileSystem,
) -> ObservedEdges {
    let key = fixture.root_path(root()).to_string_lossy().into_owned();
    let host = configured_host(config, fs, &key, fixture.source, fixture.root_path(root()));
    let (config, source, path) = host.with(|sessions| {
        let session = sessions.get(root()).expect("the fixture session exists");
        let buffer = session.buffer(&key).expect("the fixture buffer is open");
        (
            session.database().config().clone(),
            buffer.text(session.database()).clone(),
            buffer
                .path(session.database())
                .clone()
                .expect("the fixture has a path"),
        )
    });
    let run = LintPipeline::new(&config.pipeline, config.fs.as_ref());
    let pipeline = run.with_file(path);
    let expansion = pipeline.expand(&source);
    let result = pipeline.collect(&expansion);
    let set = pipeline
        .edges_of(&expansion, &result)
        .expect("a parity fixture produces an edge set");
    let wire = WireEdges {
        edges: set
            .edges()
            .iter()
            .map(|edge| {
                (
                    edge.kind.as_str().to_string(),
                    edge.target.key().to_string(),
                )
            })
            .collect(),
        unresolved: set
            .unresolved()
            .iter()
            .map(|row| {
                (
                    row.kind.as_str().to_string(),
                    row.name.clone(),
                    oxabl_analyze::unresolved_reason_str(row.reason).to_string(),
                )
            })
            .collect(),
    };
    let body = serde_json::to_vec(&wire).expect("edges serialize for transport");
    let wire: WireEdges = serde_json::from_slice(&body).expect("edges deserialize from transport");
    ObservedEdges::from_wire(wire.edges, wire.unresolved, root())
}

#[test]
fn every_fixture_matches_the_shared_diagnostics_through_a_daemon_session() {
    for fixture in FIXTURES {
        fixture.assert_diagnostics(
            "daemon wire",
            diagnostics_over(
                fixture,
                fixture.config_under(root()),
                fixture.filesystem_under(root()),
            ),
        );
    }
}

#[test]
fn every_claimed_edge_set_matches_through_a_daemon_session() {
    for fixture in FIXTURES.iter().filter(|fixture| fixture.asserts_edges()) {
        let config = fixture.config_under(root());
        fixture.assert_edges(
            "daemon wire",
            &edges_over(fixture, config.clone(), fixture.filesystem_under(root())),
        );
        if fixture.is_cross_file() {
            fixture.assert_edges_without_siblings(
                "daemon wire",
                &edges_over(fixture, config, InMemoryFileSystem::new()),
            );
        }
    }
}

#[test]
fn the_daemon_supplies_every_capability_claimed_by_the_fixture_table() {
    let supplied = [
        fixtures::Capability::Schema,
        fixtures::Capability::IncludeResolution,
    ];
    for fixture in FIXTURES {
        for capability in fixture.needs {
            assert!(
                supplied.contains(capability),
                "fixture `{}` claims an unsupported daemon capability",
                fixture.name
            );
        }
    }
}
