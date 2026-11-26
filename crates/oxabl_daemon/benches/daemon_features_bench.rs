//! The editor features on one revision of a buffer, and the workspace symbol
//! search.
//!
//! - `daemon_cycle/features_on_one_revision`: an edit of `resources/lsp_bench.p`
//!   and then, on that revision, what an editor asks for: the diagnostics, the
//!   semantic tokens, the database inlay hints, the document outline and the
//!   document links (a navigation request over the buffer's text). The
//!   includes are in memory, so no system call is measured; the older
//!   `daemon_cycle/*` benches read them from disk.
//! - `workspace_symbols/query`: one `oxabl/symbolSearch` request on the
//!   generated `ci` workspace (`oxabl_bench::generate`), after the workspace
//!   pass. The pass reads the files from disk, outside the measured part; the
//!   search reads no file.
//!
//! Each bench reports the allocations of one iteration (see `oxabl_bench::bench`).

use std::collections::HashMap;
use std::sync::Arc;

use criterion::{Criterion, criterion_group, criterion_main};
use oxabl_bench::alloc::Counting;
use oxabl_bench::generate::{Preset, generate};
use oxabl_bench::{bench, fixtures};
use oxabl_daemon::db::{
    AnalysisConfig, AnalysisDatabase, Buffer, HintSource, NavigationQuery, SchemaHandle,
    compute_diagnostics, compute_navigation, compute_semantic_tokens,
};
use oxabl_daemon::{ClientContext, SessionHost, default_dispatch};
use oxabl_daemon_protocol::{
    ClientKind, HandshakeRequest, SymbolSearchRequest, SymbolSearchResponse, method,
};
use oxabl_pipeline::{HintKinds, outline};
use salsa::Setter;

#[global_allocator]
static ALLOCATOR: Counting = Counting;

fn bench_features(c: &mut Criterion) {
    let (fs, pipeline, path, source) = fixtures::lsp_bench();
    let config = AnalysisConfig {
        fs: Arc::new(fs),
        pipeline: Arc::new(pipeline),
        preprocess: true,
        ..Default::default()
    };
    let mut db = AnalysisDatabase::new(config.clone());
    let buffer = Buffer::new(&db, source.clone(), Some(path.clone()));
    let schema = SchemaHandle::new(&db, 0);

    let edited = format!("{source}\nmessage \"edit\".\n");
    let mut toggle = false;
    let mut cycle = move || {
        let text = if toggle { &source } else { &edited };
        toggle = !toggle;
        buffer.set_text(&mut db).to(text.clone());
        let diagnostics = compute_diagnostics(&db, buffer, schema)
            .expect("not cancelled")
            .diagnostics
            .len();
        let tokens = compute_semantic_tokens(&db, buffer, schema, None)
            .expect("not cancelled")
            .expect("the buffer preprocesses")
            .tokens
            .len();
        let hints = HintSource {
            config: config.clone(),
            file: path.clone(),
            text: Arc::from(text.as_str()),
            context: None,
        }
        .hints(HintKinds::ALL)
        .0
        .len();
        let symbols = outline(text).expect("the outline does not panic").len();
        let overlay = HashMap::from([(path.clone(), Arc::<str>::from(text.as_str()))]);
        let links = compute_navigation(&db, schema, &path, overlay, &NavigationQuery::Links)
            .expect("not cancelled")
            .expect("the buffer preprocesses")
            .links
            .len();
        (diagnostics, tokens, hints, symbols, links)
    };
    let (diagnostics, tokens, hints, symbols, links) = cycle();
    eprintln!(
        "{diagnostics} diagnostics, {tokens} tokens, {hints} hints, {symbols} symbols, {links} links"
    );
    assert_eq!(links, 2, "the two includes of lsp_bench.p");
    assert!(tokens > 1_000, "{tokens} semantic tokens");
    assert!(symbols > 50, "{symbols} outline symbols");

    let mut group = c.benchmark_group("daemon_cycle");
    group.sample_size(10);
    bench(
        &mut group,
        "daemon_cycle",
        "features_on_one_revision",
        cycle,
    );
    group.finish();
}

fn bench_workspace_symbols(c: &mut Criterion) {
    let dir = std::path::Path::new(env!("CARGO_TARGET_TMPDIR")).join("workspace-symbols-ci");
    if dir.exists() {
        std::fs::remove_dir_all(&dir).expect("remove the old workspace");
    }
    generate(&Preset::Ci.knobs())
        .write(&dir)
        .expect("write the workspace");

    let dispatch = default_dispatch();
    let host = SessionHost::new();
    let mut context = ClientContext::default();
    let params = serde_json::to_value(HandshakeRequest::new(
        ClientKind::Desktop,
        dir.to_string_lossy(),
    ))
    .expect("the request serialises");
    dispatch
        .call(&host, &mut context, method::HANDSHAKE, params)
        .expect("the handshake succeeds");

    let request = serde_json::to_value(SymbolSearchRequest {
        query: "p".to_string(),
        limit: 256,
    })
    .expect("the request serialises");
    let mut search = || {
        let answer = dispatch
            .call(&host, &mut context, method::SYMBOL_SEARCH, request.clone())
            .expect("the search succeeds");
        serde_json::from_value::<SymbolSearchResponse>(answer).expect("a search response")
    };
    // The first search runs the workspace pass. One letter, as at the first
    // keystroke: most symbols match.
    let first = search();
    assert_eq!(first.symbols.len(), 256);
    assert!(first.total_matches > 500, "{} matches", first.total_matches);

    let mut group = c.benchmark_group("workspace_symbols");
    group.sample_size(10);
    bench(&mut group, "workspace_symbols", "query", || {
        search().symbols.len()
    });
    group.finish();
}

criterion_group!(benches, bench_features, bench_workspace_symbols);
criterion_main!(benches);
