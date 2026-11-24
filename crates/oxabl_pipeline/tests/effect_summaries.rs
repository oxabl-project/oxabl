use oxabl_pipeline::{LintPipeline, PipelineConfig, WorkspaceEffects};
use oxabl_schema::test_support::customer_schema;
use oxabl_workspace::InMemoryFileSystem;
use std::path::{Path, PathBuf};

fn config() -> PipelineConfig {
    let mut config = PipelineConfig {
        schema: customer_schema(),
        schema_loaded: true,
        include_paths: vec!["/src".into()],
        ..PipelineConfig::default()
    };
    config
        .lint_severities
        .base_mut()
        .enable_group("strict.transactions");
    config
}

fn findings(files: &[(&str, &str)], source: &str) -> Vec<u32> {
    let mut fs = InMemoryFileSystem::new();
    for (name, text) in files {
        fs.insert(PathBuf::from("/src").join(name), *text);
    }
    let config = config();
    let run = LintPipeline::new(&config, &fs);
    let result = run.with_file("/src/main.p").run(source);
    assert!(!result.failed_run());
    result
        .all()
        .filter(|d| d.diagnostic.code.0 == "TX004")
        .map(|d| d.diagnostic.span.span.start)
        .collect()
}

#[test]
fn complete_read_only_transitive_callee_is_silent() {
    assert!(
        findings(
            &[("a.p", "RUN b.p."), ("b.p", "FIND FIRST Customer NO-LOCK.")],
            "DO TRANSACTION:\nRUN a.p.\nEND."
        )
        .is_empty()
    );
}

#[test]
fn writes_partial_and_missing_summaries_keep_findings() {
    let src = "DO TRANSACTION:\nRUN a.p.\nEND.";
    for files in [
        vec![("a.p", "RUN b.p."), ("b.p", "CREATE Customer.")],
        vec![("a.p", "RUN VALUE(\"b.p\").")],
        vec![("a.p", "{missing.i}")],
        vec![("a.p", "RUN b.p."), ("b.p", "DEFINE VARIABLE broken AS .")],
        vec![],
    ] {
        assert_eq!(findings(&files, src), [16], "{files:?}");
    }
}

#[test]
fn persistent_procedure_uses_the_named_unit() {
    let files = [(
        "library.p",
        "CREATE Customer.\nPROCEDURE read-only:\nFIND FIRST Customer NO-LOCK.\nEND PROCEDURE.",
    )];
    let src = "DEFINE VARIABLE h AS HANDLE NO-UNDO.\nRUN library.p PERSISTENT SET h.\nDO TRANSACTION:\nRUN read-only IN h.\nEND.";
    assert!(findings(&files, src).is_empty());
}

#[test]
fn recursive_graph_reaches_a_fixed_point() {
    let src = "DO TRANSACTION:\nRUN a.p.\nEND.";
    assert!(findings(&[("a.p", "RUN b.p."), ("b.p", "RUN a.p.")], src).is_empty());
    assert_eq!(
        findings(
            &[("a.p", "RUN b.p."), ("b.p", "RUN a.p.\nCREATE Customer.")],
            src
        ),
        [16]
    );
}

#[test]
fn root_buffer_overrides_disk_when_a_callee_calls_back() {
    let config = config();
    let mut fs = InMemoryFileSystem::new();
    fs.insert("/src/main.p".into(), "CREATE Customer.");
    fs.insert("/src/a.p".into(), "RUN main.p.");
    let run = LintPipeline::new(&config, &fs);
    let source = "DO TRANSACTION:\nRUN a.p.\nEND.";
    let result = run.with_file("/src/main.p").run(source);
    assert!(!result.all().any(|d| d.diagnostic.code.0 == "TX004"));
}

#[test]
fn batch_caches_once_and_default_never_reads_seeded_files() {
    struct CountFs {
        reads: std::sync::atomic::AtomicUsize,
        inner: InMemoryFileSystem,
    }
    impl oxabl_workspace::FileSystem for CountFs {
        fn read(&self, p: &Path) -> std::io::Result<std::sync::Arc<str>> {
            self.reads
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            self.inner.read(p)
        }
        fn exists(&self, p: &Path) -> bool {
            self.inner.exists(p)
        }
    }
    let mut inner = InMemoryFileSystem::new();
    inner.insert("/src/other.p".into(), "FIND FIRST Customer NO-LOCK.");
    let fs = CountFs {
        reads: Default::default(),
        inner,
    };
    let known = vec![PathBuf::from("/src/other.p")];
    let mut cfg = config();
    cfg.lint_severities
        .base_mut()
        .disable_group("strict.transactions");
    let off = LintPipeline::new(&cfg, &fs).with_known_files(&known);
    off.with_file("/src/main.p").run("MESSAGE \"hi\".");
    assert_eq!(fs.reads.load(std::sync::atomic::Ordering::Relaxed), 0);
    cfg.lint_severities
        .base_mut()
        .enable_group("strict.transactions");
    let on = LintPipeline::new(&cfg, &fs).with_known_files(&known);
    on.with_file("/src/main.p")
        .run("DO TRANSACTION: RUN other.p. END.");
    let count = fs.reads.load(std::sync::atomic::Ordering::Relaxed);
    on.with_file("/src/main.p")
        .run("DO TRANSACTION: RUN other.p. END.");
    assert_eq!(fs.reads.load(std::sync::atomic::Ordering::Relaxed), count);
}

#[test]
fn an_edited_root_does_not_reuse_call_offsets_from_old_text() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert("/src/a.p".into(), "MESSAGE \"hi\".");
    fs.insert("/src/b.p".into(), "CREATE Customer.");
    let config = config();
    let run = LintPipeline::new(&config, &fs);
    let handle = run.with_file("/src/main.p");
    assert!(
        !handle
            .run("DO TRANSACTION: RUN a.p. END.")
            .all()
            .any(|d| d.diagnostic.code.0 == "TX004")
    );
    assert!(
        handle
            .run("DO TRANSACTION: RUN b.p. END.")
            .all()
            .any(|d| d.diagnostic.code.0 == "TX004")
    );
}

#[test]
fn per_directory_opt_in_enables_the_provider() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert("/src/a.p".into(), "FIND FIRST Customer NO-LOCK.");
    let mut config = config();
    config.lint_severities = Default::default();
    let mut layer = oxabl_common::LintLayer::new();
    layer.enable_group("strict.transactions");
    assert!(
        config
            .lint_severities
            .add_scope(Path::new("/src"), &["enabled".into()], layer)
            .is_empty()
    );
    let run = LintPipeline::new(&config, &fs);
    let handle = run.with_file("/src/enabled/main.p");
    assert!(handle.callee_summaries_enabled());
    assert!(
        !handle
            .run("DO TRANSACTION: RUN a.p. END.")
            .all()
            .any(|d| d.diagnostic.code.0 == "TX004")
    );
    assert!(
        !run.with_file("/src/other/main.p")
            .callee_summaries_enabled()
    );
}

/// Programs three calls deep behind each root, with shared callees, a cycle,
/// a persistent library and a missing program, so later frontiers find
/// programs that earlier ones also reach.
fn call_tree() -> (InMemoryFileSystem, Vec<PathBuf>) {
    let mut fs = InMemoryFileSystem::new();
    let mut known = Vec::new();
    for k in 0..12 {
        let root = PathBuf::from(format!("/src/r{k:02}.p"));
        fs.insert(
            root.clone(),
            format!(
                "DO TRANSACTION:\nRUN m{}.p.\nRUN m{}.p.\nEND.\nRUN lib.p PERSISTENT.\nRUN q{k}.p.",
                k % 5,
                (k * 7) % 5
            ),
        );
        known.push(root);
    }
    for m in 0..5 {
        fs.insert(
            PathBuf::from(format!("/src/m{m}.p")),
            format!(
                "RUN l{}.p.\nRUN l{}.p.\nFIND FIRST Customer NO-LOCK.",
                m % 3,
                (m + 1) % 3
            ),
        );
    }
    fs.insert(
        "/src/l0.p".into(),
        "RUN l1.p.\nFIND FIRST Customer NO-LOCK.",
    );
    fs.insert("/src/l1.p".into(), "RUN l0.p.\nRUN read-only IN h.");
    fs.insert("/src/l2.p".into(), "CREATE Customer.\nRUN deep.p.");
    fs.insert("/src/deep.p".into(), "{missing.i}\nRUN l0.p.");
    fs.insert(
        "/src/lib.p".into(),
        "PROCEDURE read-only:\nFIND FIRST Customer NO-LOCK.\nEND PROCEDURE.",
    );
    known.push("/src/l2.p".into());
    known.push("/src/notes.i".into());
    (fs, known)
}

#[test]
fn the_thread_count_changes_no_summary() {
    let (fs, known) = call_tree();
    let config = config();
    let run = LintPipeline::new(&config, &fs);
    let mut paths = known.clone();
    paths.extend(["m0", "m4", "l0", "l1", "deep", "lib"].map(|p| format!("/src/{p}.p").into()));
    let report = |effects: WorkspaceEffects| {
        let files: Vec<_> = paths
            .iter()
            .map(|p| {
                effects
                    .file(p)
                    .map(|f| (f.summaries().to_vec(), f.effect_lenses()))
            })
            .collect();
        let any = effects.file(&known[0]).unwrap();
        let names: Vec<&str> = (0..).map_while(|n| any.callee_name(n)).collect();
        format!("{files:?}\n{names:?}")
    };
    let serial = report(run.build_workspace_effect_summaries(&known));
    assert!(serial.contains("/src/deep.p::"), "{serial}");
    assert!(
        serial.contains("Partial") || serial.contains("partial"),
        "{serial}"
    );
    for threads in [1, 2, 3, 8, 64] {
        assert_eq!(
            report(run.build_workspace_effect_summaries_parallel(&known, threads)),
            serial,
            "{threads} threads"
        );
    }
}
