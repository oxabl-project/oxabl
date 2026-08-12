//! Daemon diagnostics-cycle and request-routing perf gates (U8, R14).
//!
//! Two gates over a synthetic real-disk fixture (`resources/lsp_bench.p` +
//! includes), driven through the real salsa substrate and shared collector:
//!
//! - **WARM** — steady-state single-edit full cycle (`set_text` + recompute).
//!   The CodSpeed gate enforces ≤50ms p95. This is the interactivity bar.
//! - **COLD** — a first computation on a fresh database (cold memo).
//!
//! Both emit a `finished in N` line for manual eyeballing. Thresholds are
//! enforced by CodSpeed CI; the bench itself just runs the pipeline.
//!
//! **Fixture calibration (recorded).** The synthetic fixture
//! (`resources/lsp_bench.p`, ~3.4k expanded lines, two includes) stands in for a
//! large real-world ABL file kept outside the repo. Measured raw full-cycle on
//! this dev host: WARM p95 ≈ 2.6ms, COLD ≈ 2.7ms, against the spike's ~15ms
//! real-file baseline — i.e. the synthetic under-stresses the pipeline by ~6×
//! (well within an order of magnitude, and roughly ~19× under the 50ms bar).
//! The delta is recorded here so a future fixture materially easier than the
//! real file — which could let the real file regress past 50ms behind a green
//! gate — is caught: if this bench drops far below the ~ms range, re-grow the
//! fixture toward the ~15ms band before trusting the gate.
//!
//! **Not a release configuration.** This crate's dev-dependencies enable
//! `oxabl_common/test-panics`, and dev-dependency features apply to benches too,
//! so the `panic_if_injected` marker scan is armed while these run. It is a
//! substring check rather than a hot path, but the numbers are not bit-identical
//! to a release build — recorded here so a future reader does not chase the gap.

use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Instant;

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use oxabl_daemon::db::{
    AnalysisConfig, AnalysisDatabase, Buffer, SchemaHandle, compute_diagnostics,
};
use oxabl_pipeline::PipelineConfig;
use oxabl_workspace::RealFileSystem;
use salsa::Setter;

fn resources_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("resources")
}

fn fixture_source() -> String {
    std::fs::read_to_string(resources_dir().join("lsp_bench.p")).unwrap()
}

fn bench_config() -> AnalysisConfig {
    // Configuration is resolved once and held behind an `Arc`, exactly as the
    // live server holds it: the measured cycle must contain no config resolution.
    AnalysisConfig {
        fs: Arc::new(RealFileSystem),
        pipeline: Arc::new(PipelineConfig {
            include_paths: vec![resources_dir()],
            ..PipelineConfig::default()
        }),
        preprocess: true,
        // The registry is empty and stays empty: the fixture references no
        // other file, so the measured cycle pays for the index handle and
        // nothing more.
        ..Default::default()
    }
}

fn daemon_cycle_benchmarks(c: &mut Criterion) {
    let source = fixture_source();
    let mut group = c.benchmark_group("daemon_cycle");

    // ---- WARM: steady-state single-edit full cycle -----------------------
    {
        let mut db = AnalysisDatabase::new(bench_config());
        let buffer = Buffer::new(&db, source.clone(), None);
        let schema = SchemaHandle::new(&db, 0);
        // Warm the memo so we measure a steady-state edit, not a cold open.
        let _ = compute_diagnostics(&db, buffer, schema);

        // Two variants force a real recompute each iteration (a genuine edit).
        let a = source.clone();
        let b = format!("{source}\nmessage \"edit\".\n");
        let mut toggle = false;

        let start = Instant::now();
        group.bench_function("warm_single_edit_cycle", |bencher| {
            bencher.iter(|| {
                let text = if toggle { a.clone() } else { b.clone() };
                toggle = !toggle;
                buffer.set_text(&mut db).to(text);
                black_box(compute_diagnostics(&db, buffer, schema));
            });
        });
        eprintln!("warm_single_edit_cycle finished in {:?}", start.elapsed());
    }

    // ---- COLD: first computation on a fresh database ---------------------
    {
        let start = Instant::now();
        group.bench_function("cold_open", |bencher| {
            bencher.iter_batched(
                || {
                    let db = AnalysisDatabase::new(bench_config());
                    let buffer = Buffer::new(&db, source.clone(), None);
                    let schema = SchemaHandle::new(&db, 0);
                    (db, buffer, schema)
                },
                |(db, buffer, schema)| {
                    black_box(compute_diagnostics(&db, buffer, schema));
                },
                criterion::BatchSize::SmallInput,
            );
        });
        eprintln!("cold_open finished in {:?}", start.elapsed());
    }

    group.finish();
}

/// The routing hop, which is this crate's own new cost.
///
/// The cycle gates above measure the substrate, and it is unchanged — but a
/// request now travels through a dispatch table and serde before it reaches a
/// handler, and nothing measured that. Timed in process: `Dispatch::call` covers
/// routing, the handler, and both serde directions, without the socket and
/// thread-scheduling variance that would make this a poor CodSpeed gate.
///
/// The full socket round trip is deliberately not benchmarked. It is Unix-only,
/// needs the cache directory redirected, and its variance would swamp the signal.
fn daemon_request_benchmarks(c: &mut Criterion) {
    use oxabl_daemon::{ClientContext, SessionHost, default_dispatch};
    use oxabl_daemon_protocol::{ClientKind, HandshakeRequest, method};

    let mut group = c.benchmark_group("daemon_request");
    let dispatch = default_dispatch();
    let host = SessionHost::new();

    let start = Instant::now();
    group.bench_function("handshake_round_trip", |bencher| {
        bencher.iter_batched(
            || {
                let params = serde_json::to_value(HandshakeRequest::new(
                    ClientKind::Desktop,
                    "/proj/bench".to_string(),
                ))
                .expect("the request serialises");
                (ClientContext::default(), params)
            },
            |(mut context, params)| {
                // Unwrapped rather than discarded: a benchmark that quietly times
                // an error path reports a number that means nothing.
                let response = dispatch.call(&host, &mut context, method::HANDSHAKE, params);
                black_box(response.expect("the handshake succeeds"));
            },
            criterion::BatchSize::SmallInput,
        );
    });
    eprintln!("handshake_round_trip finished in {:?}", start.elapsed());

    group.finish();
}

criterion_group!(benches, daemon_cycle_benchmarks, daemon_request_benchmarks);
criterion_main!(benches);
