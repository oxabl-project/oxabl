//! Outcomes of the edge set that need more than the in-memory parity table can
//! express: an include that resolves but cannot be read, a program that `RUN`s
//! itself, and the degraded files a workspace pass reports.

use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use oxabl_pipeline::{
    Completeness, DegradedReason, EdgeKind, EdgeTarget, LintPipeline, PipelineConfig, ReverseGraph,
};
use oxabl_semantic::UnresolvedReason;
use oxabl_workspace::{FileSystem, InMemoryFileSystem, RealFileSystem};

/// An in-memory filesystem in which chosen paths exist and cannot be read.
struct Unreadable {
    inner: InMemoryFileSystem,
    unreadable: Vec<PathBuf>,
}

impl FileSystem for Unreadable {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        if self.unreadable.iter().any(|p| p == path) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "stream did not contain valid UTF-8",
            ));
        }
        self.inner.read(path)
    }

    fn exists(&self, path: &Path) -> bool {
        self.unreadable.iter().any(|p| p == path) || self.inner.exists(path)
    }
}

fn config(root: &str) -> PipelineConfig {
    PipelineConfig {
        include_paths: vec![PathBuf::from(root)],
        ..PipelineConfig::default()
    }
}

#[test]
fn an_unreadable_include_stays_an_edge_and_leaves_a_row() {
    let mut inner = InMemoryFileSystem::new();
    inner.insert(PathBuf::from("/proj/ok.i"), "MESSAGE 1.\n");
    let fs = Unreadable {
        inner,
        unreadable: vec![PathBuf::from("/proj/latin.i")],
    };
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/main.p");

    let edges = pipeline.edge_set("{ok.i}\n{latin.i}\n").expect("edge set");

    let includes: Vec<_> = edges
        .of_kind(EdgeKind::DirectInclude)
        .map(|e| e.target.key().to_string())
        .collect();
    assert_eq!(includes, ["/proj/latin.i", "/proj/ok.i"]);
    let rows = edges.unresolved();
    assert_eq!(rows.len(), 1, "got {rows:?}");
    assert_eq!(rows[0].kind, EdgeKind::DirectInclude);
    assert_eq!(rows[0].name, "latin.i");
    assert_eq!(rows[0].reason, UnresolvedReason::PresentButUnusable);
    assert!(rows[0].span.is_some());
    assert_eq!(
        edges.completeness(),
        &Completeness::Degraded {
            reasons: vec![DegradedReason::UnreadableInclude]
        }
    );
}

#[test]
fn a_nested_unreadable_include_is_a_transitive_edge_with_a_row() {
    let mut inner = InMemoryFileSystem::new();
    inner.insert(PathBuf::from("/proj/mid.i"), "{latin.i}\n");
    let fs = Unreadable {
        inner,
        unreadable: vec![PathBuf::from("/proj/latin.i")],
    };
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/main.p");

    let edges = pipeline.edge_set("{mid.i}\n").expect("edge set");

    let transitive: Vec<_> = edges
        .of_kind(EdgeKind::TransitiveInclude)
        .map(|e| e.target.key().to_string())
        .collect();
    assert_eq!(transitive, ["/proj/latin.i"]);
    let rows = edges.unresolved();
    assert_eq!(rows.len(), 1, "got {rows:?}");
    assert_eq!(rows[0].kind, EdgeKind::TransitiveInclude);
    assert_eq!(rows[0].reason, UnresolvedReason::PresentButUnusable);
    assert_eq!(rows[0].span, None);
}

#[test]
fn a_non_utf8_include_on_a_real_filesystem_is_an_edge_and_a_row() {
    let dir = tempfile::tempdir().expect("temp dir");
    std::fs::write(dir.path().join("latin.i"), b"MESSAGE \"caf\xe9\".\n").expect("write");
    let main = dir.path().join("main.p");
    let config = PipelineConfig {
        include_paths: vec![dir.path().to_path_buf()],
        ..PipelineConfig::default()
    };
    let fs = RealFileSystem;
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file(main);

    let edges = pipeline.edge_set("{latin.i}\n").expect("edge set");

    assert_eq!(edges.of_kind(EdgeKind::DirectInclude).count(), 1);
    assert_eq!(edges.unresolved().len(), 1);
    assert_eq!(
        edges.unresolved()[0].reason,
        UnresolvedReason::PresentButUnusable
    );
}

#[test]
fn an_unreadable_include_is_a_dependency_path() {
    let fs = Unreadable {
        inner: InMemoryFileSystem::new(),
        unreadable: vec![PathBuf::from("/proj/latin.i")],
    };
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/main.p");

    let expansion = pipeline.expand("{latin.i}\n{gone.i}\n");

    assert_eq!(
        expansion.dependency_paths(),
        [PathBuf::from("/proj/latin.i")],
        "an existing file that cannot be read is watched; a missing one is not"
    );
}

#[test]
fn a_nested_row_names_the_root_site_of_its_chain() {
    let mut inner = InMemoryFileSystem::new();
    inner.insert(PathBuf::from("/proj/mid.i"), "{latin.i}\n{gone.i}\n");
    let fs = Unreadable {
        inner,
        unreadable: vec![PathBuf::from("/proj/latin.i")],
    };
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/main.p");
    let source = "MESSAGE 1.\n{mid.i}\n";
    let start = source.find("{mid.i}").expect("site") as u32;

    let edges = pipeline.edge_set(source).expect("edge set");

    let rows = edges.unresolved();
    assert_eq!(rows.len(), 2, "got {rows:?}");
    for row in rows {
        assert_eq!(row.span, None, "{row:?}");
        let site = row.include_site.expect("the chain's root site");
        assert_eq!(
            (site.start, site.end),
            (start, start + "{mid.i}".len() as u32),
            "{row:?}"
        );
    }
}

#[test]
fn the_include_depth_limit_degrades_the_set() {
    let mut inner = InMemoryFileSystem::new();
    for n in 0..80 {
        inner.insert(
            PathBuf::from(format!("/proj/d{n}.i")),
            format!("{{d{}.i}}\n", n + 1),
        );
    }
    let config = config("/proj");
    let run = LintPipeline::new(&config, &inner);
    let pipeline = run.with_file("/proj/main.p");

    let edges = pipeline.edge_set("{d0.i}\n").expect("edge set");

    assert_eq!(
        edges.completeness().reasons(),
        [DegradedReason::PreprocessorLoss]
    );
}

#[test]
fn an_unmodelled_statement_naming_a_class_is_an_edge_or_degrades_the_set() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(
        PathBuf::from("/proj/orders/util.cls"),
        "CLASS orders.util:\n  METHOD PUBLIC STATIC INTEGER two():\n    RETURN 2.\n  END METHOD.\nEND CLASS.\n",
    );
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/main.p");

    let found = pipeline
        .edge_set("PUT UNFORMATTED orders.util:two() SKIP.\n")
        .expect("edge set");
    assert_eq!(found.of_kind(EdgeKind::ClassReference).count(), 1);
    assert!(found.completeness().is_complete());

    let lost = pipeline
        .edge_set("EXPORT orders.nothere:two().\n")
        .expect("edge set");
    assert_eq!(lost.of_kind(EdgeKind::ClassReference).count(), 0);
    assert_eq!(
        lost.completeness().reasons(),
        [DegradedReason::UnmodelledStatement]
    );
}

#[test]
fn line_number_references_in_a_large_file_do_not_cost_the_file_each() {
    let mut source = String::new();
    for n in 0..60_000 {
        source.push_str(&format!("MESSAGE {{&LINE-NUMBER}} {n}.\n"));
    }
    let fs = InMemoryFileSystem::new();
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/big.p");

    let started = std::time::Instant::now();
    let expansion = pipeline.expand(&source);
    let elapsed = started.elapsed();

    assert!(expansion.expanded().is_some());
    assert!(
        elapsed < std::time::Duration::from_secs(5),
        "60k references took {elapsed:?}"
    );
}

#[test]
fn a_program_that_runs_itself_depends_on_nothing_and_leaves_no_row() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/proj/loop.p"), "RUN loop.p.\n");
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/loop.p");

    let edges = pipeline.edge_set("RUN loop.p.\n").expect("edge set");

    assert!(edges.edges().is_empty(), "got {:?}", edges.edges());
    assert!(
        edges.unresolved().is_empty(),
        "got {:?}",
        edges.unresolved()
    );
}

#[test]
fn an_unparsable_program_target_is_an_edge_to_the_located_file() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(
        PathBuf::from("/proj/sub.p"),
        "&IF DEFINED(x) = 0 &THEN\nDO:\n&ENDIF\n  {nothere.i}\n  MESSAGE 1.\nEND.\n",
    );
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/run.p");

    let edges = pipeline.edge_set("RUN sub.p.\n").expect("edge set");

    let targets: Vec<_> = edges.of_kind(EdgeKind::ProgramReference).collect();
    assert_eq!(targets.len(), 1);
    let EdgeTarget::IndexedFile { file, .. } = targets[0].target else {
        panic!("a program edge names an indexed file");
    };
    assert_eq!(run.indexed_path(file), Some(PathBuf::from("/proj/sub.p")));
    assert_eq!(edges.unresolved().len(), 1);
    assert_eq!(
        edges.unresolved()[0].reason,
        UnresolvedReason::PresentButUnusable
    );
    assert!(edges.completeness().is_complete());
}

#[test]
fn an_unparsable_class_target_is_an_edge_to_the_located_file() {
    let mut fs = InMemoryFileSystem::new();
    // Inheriting is what records a class lookup: it happens when an inherited
    // name is looked up, so the child calls one.
    fs.insert(
        PathBuf::from("/proj/orders/base.cls"),
        "{header.i}\nEND CLASS.\n",
    );
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/orders/child.cls");

    let edges = pipeline
        .edge_set("CLASS orders.child INHERITS orders.base:\n  METHOD PUBLIC VOID go():\n    MESSAGE calc-total().\n  END METHOD.\nEND CLASS.\n")
        .expect("edge set");

    let targets: Vec<_> = edges.of_kind(EdgeKind::ClassReference).collect();
    assert_eq!(targets.len(), 1, "got {:?}", edges.edges());
    assert_eq!(edges.unresolved().len(), 1);
    assert_eq!(
        edges.unresolved()[0].reason,
        UnresolvedReason::PresentButUnusable
    );
}

#[test]
fn the_reverse_graph_reports_degraded_files_beside_unanalysed_ones() {
    let mut fs = InMemoryFileSystem::new();
    let clean = PathBuf::from("/proj/clean.p");
    let broken = PathBuf::from("/proj/broken.p");
    let gappy = PathBuf::from("/proj/gappy.p");
    fs.insert(clean.clone(), "MESSAGE 1.\n");
    fs.insert(broken.clone(), "@ @ @\nMESSAGE 1.\n");
    fs.insert(gappy.clone(), "{nowhere.i}\n");
    let files = [clean, broken.clone(), gappy.clone()];
    let config = config("/proj");
    let pipeline = LintPipeline::new(&config, &fs).with_known_files(&files);

    let graph = ReverseGraph::build(&pipeline, &files);

    assert!(graph.unanalysed().is_empty());
    let degraded: Vec<_> = graph
        .degraded()
        .iter()
        .map(|d| (d.file.clone(), d.reasons.clone()))
        .collect();
    assert_eq!(
        degraded,
        [
            (broken, vec![DegradedReason::ParseRecovered]),
            (gappy, vec![DegradedReason::UnresolvedInclude]),
        ]
    );
}

/// The edge set for `source` beside a program `sub.p` and an include that
/// defines the procedure `from-include`.
fn run_set(source: &str) -> oxabl_pipeline::DependencyEdges {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/proj/sub.p"), "MESSAGE 1.\n");
    fs.insert(
        PathBuf::from("/proj/procs.i"),
        "PROCEDURE from-include:\n  MESSAGE 2.\nEND PROCEDURE.\n",
    );
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/main.p");
    pipeline.edge_set(source).expect("edge set")
}

#[test]
fn an_unclosed_generic_bracket_degrades_the_set_but_keeps_later_edges() {
    let edges = run_set("DEFINE VARIABLE l AS List<pkg.Thing NO-UNDO.\nRUN sub.p.\n");

    assert_eq!(edges.of_kind(EdgeKind::ProgramReference).count(), 1);
    assert!(
        edges
            .completeness()
            .reasons()
            .contains(&DegradedReason::ParseRecovered),
        "got {:?}",
        edges.completeness()
    );
    assert!(
        edges.unresolved().iter().all(|row| row.name != "no-undo"),
        "got {:?}",
        edges.unresolved()
    );

    let edges = run_set("DEFINE VARIABLE l AS List<pkg.> NO-UNDO.\nRUN sub.p.\n");
    assert_eq!(edges.of_kind(EdgeKind::ProgramReference).count(), 1);
}

#[test]
fn an_include_inside_a_define_comment_that_spans_lines_is_no_edge() {
    let edges =
        run_set("&GLOBAL-DEFINE x 5 /* retired:\n{procs.i}\n*/\nDISPLAY {&x}.\nRUN sub.p.\n");

    assert_eq!(edges.of_kind(EdgeKind::DirectInclude).count(), 0);
    assert_eq!(edges.of_kind(EdgeKind::ProgramReference).count(), 1);
    assert!(
        edges.unresolved().is_empty(),
        "got {:?}",
        edges.unresolved()
    );
    assert_eq!(edges.completeness(), &Completeness::Complete);
}

#[test]
fn a_condition_that_an_operand_settles_does_not_degrade_the_set() {
    for condition in [
        "SEARCH(\"a.i\") = ? OR TRUE",
        "(TRUE OR SEARCH(\"a.i\") = ?) OR SEARCH(\"b.i\") = ?",
        "SEARCH(\"a.i\") = ? AND FALSE",
    ] {
        let edges = run_set(&format!(
            "&IF {condition} &THEN\n{{procs.i}}\n&ENDIF\nRUN sub.p.\n"
        ));
        assert_eq!(edges.completeness(), &Completeness::Complete, "{condition}");
    }
}

#[test]
fn a_condition_that_the_avm_rejects_degrades_the_set_whatever_settles_it() {
    for condition in ["TRUE OR 1 +", "TRUE OR \"5\" = 5"] {
        let edges = run_set(&format!(
            "&IF {condition} &THEN\n{{procs.i}}\n&ENDIF\nRUN sub.p.\n"
        ));
        assert_eq!(
            edges.completeness().reasons(),
            [DegradedReason::PreprocessorLoss],
            "{condition}"
        );
    }
}

#[test]
fn a_run_through_a_handle_is_neither_an_edge_nor_a_row() {
    // `sub` is a real file beside the handle calls, so a lookup by name would
    // link it.
    let edges = run_set(
        "DEFINE VARIABLE h AS HANDLE NO-UNDO.\n\
         RUN sub IN h.\n\
         RUN sub.p IN h.\n\
         RUN sub IN THIS-PROCEDURE.\n\
         RUN sub IN SUPER.\n\
         RUN sub IN TARGET-PROCEDURE.\n\
         RUN sub IN SOURCE-PROCEDURE.\n\
         RUN sub IN h:CURRENT-WINDOW.\n\
         RUN VALUE(\"sub\") IN h.\n",
    );
    assert!(edges.edges().is_empty(), "got {:?}", edges.edges());
    assert!(
        edges.unresolved().is_empty(),
        "got {:?}",
        edges.unresolved()
    );
}

#[test]
fn a_run_of_a_procedure_in_the_same_unit_is_neither_an_edge_nor_a_row() {
    let edges = run_set(
        "{procs.i}\n\
         RUN local-proc.\n\
         RUN from-include.\n\
         RUN later-proc.\n\
         PROCEDURE local-proc:\n  MESSAGE 1.\nEND PROCEDURE.\n\
         PROCEDURE later-proc:\n  MESSAGE 3.\nEND PROCEDURE.\n",
    );
    assert!(
        edges.of_kind(EdgeKind::ProgramReference).next().is_none(),
        "got {:?}",
        edges.edges()
    );
    assert!(
        edges.unresolved().is_empty(),
        "got {:?}",
        edges.unresolved()
    );
}

#[test]
fn a_bare_run_name_nothing_supplies_has_no_row() {
    let edges =
        run_set("RUN local-proc.\nRUN sub.\nRUN mystery.\nPROCEDURE local-proc:\nEND PROCEDURE.\n");
    assert!(
        edges.unresolved().is_empty(),
        "got {:?}",
        edges.unresolved()
    );
    assert_eq!(edges.of_kind(EdgeKind::ProgramReference).count(), 1);
}

#[test]
fn an_extensionless_run_path_nothing_supplies_keeps_its_row() {
    let edges = run_set("RUN lib/mystery.\nRUN lib/other.p.\n");
    let rows: Vec<_> = edges
        .unresolved()
        .iter()
        .map(|row| (row.name.as_str(), row.reason))
        .collect();
    assert_eq!(
        rows,
        [
            ("lib/mystery", UnresolvedReason::External),
            ("lib/other.p", UnresolvedReason::AbsentFromWorkspace),
        ]
    );
}

/// The paths the program edges of `source` land on, under `extensions`.
fn run_targets(source: &str, extensions: &[&str]) -> Vec<PathBuf> {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/proj/both.p"), "MESSAGE 1.\n");
    fs.insert(PathBuf::from("/proj/both.w"), "MESSAGE 2.\n");
    fs.insert(PathBuf::from("/proj/only.pp"), "MESSAGE 3.\n");
    let config = PipelineConfig {
        run_extensions: extensions.iter().map(|e| e.to_string()).collect(),
        ..config("/proj")
    };
    let run = LintPipeline::new(&config, &fs);
    let edges = run
        .with_file("/proj/main.p")
        .edge_set(source)
        .expect("edge set");
    edges
        .of_kind(EdgeKind::ProgramReference)
        .map(|edge| match edge.target {
            EdgeTarget::IndexedFile { file, .. } => run.indexed_path(file).expect("indexed path"),
            _ => panic!("a program edge names an indexed file"),
        })
        .collect()
}

#[test]
fn the_extension_order_for_a_bare_run_name_comes_from_the_configuration() {
    assert_eq!(
        run_targets("RUN both.\n", &["p", "w"]),
        [PathBuf::from("/proj/both.p")]
    );
    assert_eq!(
        run_targets("RUN both.\n", &["w", "p"]),
        [PathBuf::from("/proj/both.w")]
    );
}

#[test]
fn an_extension_outside_the_configured_list_is_not_tried() {
    assert!(run_targets("RUN only.\n", &["p", "w"]).is_empty());
    assert_eq!(
        run_targets("RUN only.\n", &["pp"]),
        [PathBuf::from("/proj/only.pp")]
    );
    assert!(run_targets("RUN both.\n", &[]).is_empty());
}

/// The class edges of `/proj/Child.cls` when `pkg.Mid` picks its parent with an
/// `&IF` over `{&OPSYS}`.
fn directive_built_parent_edges(opsys: &str) -> Vec<String> {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(
        PathBuf::from("/proj/pkg/Mid.cls"),
        "&IF \"{&OPSYS}\" = \"WIN32\" &THEN\n\
         CLASS pkg.Mid INHERITS pkg.WinBase:\n\
         &ELSE\n\
         CLASS pkg.Mid INHERITS pkg.Base:\n\
         &ENDIF\n\
         END CLASS.\n",
    );
    for base in ["Base", "WinBase"] {
        fs.insert(
            PathBuf::from(format!("/proj/pkg/{base}.cls")),
            format!("CLASS pkg.{base}:\nEND CLASS.\n"),
        );
    }
    let mut config = config("/proj");
    config.preprocessor.opsys = opsys.to_string();
    let run = LintPipeline::new(&config, &fs);
    let edges = run
        .with_file("/proj/Child.cls")
        .edge_set(
            "CLASS Child INHERITS pkg.Mid:\n\
             METHOD PUBLIC VOID m():\n  Hello().\nEND METHOD.\nEND CLASS.\n",
        )
        .expect("edge set");
    edges
        .of_kind(EdgeKind::ClassReference)
        .map(|edge| edge.target.key().to_string())
        .collect()
}

#[test]
fn a_directive_built_class_follows_the_configured_opsys() {
    assert_eq!(
        directive_built_parent_edges("UNIX"),
        ["pkg.base", "pkg.mid"]
    );
    assert_eq!(
        directive_built_parent_edges("WIN32"),
        ["pkg.mid", "pkg.winbase"]
    );
}

#[test]
fn a_dotted_run_extension_in_the_configuration_still_links_a_bare_run() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/proj/both.p"), "MESSAGE 1.\n");
    let workspace = oxabl_workspace::WorkspaceConfig::from_toml(
        "[workspace]\nname = \"p\"\n\
         [workspace.sources]\ninclude_paths = [\"/proj\"]\nrun_extensions = [\".P\", \"\"]\n",
    )
    .expect("config");
    let (config, warnings) = oxabl_pipeline::resolve_from_config(
        &workspace,
        Path::new("/proj"),
        &oxabl_pipeline::ConfigOverrides::default(),
    );
    assert_eq!(warnings.len(), 1, "got {warnings:?}");
    let run = LintPipeline::new(&config, &fs);
    let edges = run
        .with_file("/proj/main.p")
        .edge_set("RUN both.\n")
        .expect("edge set");
    assert_eq!(edges.of_kind(EdgeKind::ProgramReference).count(), 1);
}

#[test]
fn a_run_extension_list_set_directly_is_normalized_by_the_lookup() {
    for extensions in [&[".p"][..], &["P"][..], &[" .P "][..]] {
        assert_eq!(
            run_targets("RUN Both.\n", extensions),
            [PathBuf::from("/proj/both.p")],
            "extensions {extensions:?}"
        );
    }
}

#[test]
fn an_unreadable_include_through_a_define_is_the_roots_own_direct_include() {
    let mut inner = InMemoryFileSystem::new();
    inner.insert(PathBuf::from("/proj/ok.i"), "MESSAGE 1.\n");
    let fs = Unreadable {
        inner,
        unreadable: vec![PathBuf::from("/proj/latin.i")],
    };
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let source = "&SCOPED-DEFINE INC {latin.i}\n{&INC}\n";
    let edges = run
        .with_file("/proj/main.p")
        .edge_set(source)
        .expect("edge set");

    let site = source.find("{&INC}").unwrap() as u32;
    let expected = Some(oxabl_ast::Span {
        start: site,
        end: site + 6,
    });
    let direct: Vec<_> = edges.of_kind(EdgeKind::DirectInclude).collect();
    assert_eq!(direct.len(), 1, "got {:?}", edges.edges());
    assert_eq!((direct[0].span, direct[0].include_site), (expected, None));
    assert!(edges.of_kind(EdgeKind::TransitiveInclude).next().is_none());
    let rows = edges.unresolved();
    assert_eq!(rows.len(), 1, "got {rows:?}");
    assert_eq!(rows[0].kind, EdgeKind::DirectInclude);
    assert_eq!((rows[0].span, rows[0].include_site), (expected, None));
}

#[test]
fn a_transitive_include_names_the_site_of_the_include_that_pulled_it_in() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/proj/a.i"), "{b.i}\n");
    fs.insert(PathBuf::from("/proj/b.i"), "{c.i}\n");
    fs.insert(PathBuf::from("/proj/c.i"), "MESSAGE 1.\n");
    fs.insert(PathBuf::from("/proj/d.i"), "MESSAGE 2.\n");
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let source = "MESSAGE 0.\n{a.i}\n{d.i}\n";
    let edges = run
        .with_file("/proj/main.p")
        .edge_set(source)
        .expect("edge set");

    let a = source.find("{a.i}").unwrap() as u32;
    let sites: Vec<_> = edges
        .of_kind(EdgeKind::TransitiveInclude)
        .map(|edge| {
            (
                edge.target.key().to_string(),
                edge.span,
                edge.include_site.map(|site| (site.start, site.end)),
            )
        })
        .collect();
    assert_eq!(
        sites,
        [
            ("/proj/b.i".to_string(), None, Some((a, a + 5))),
            ("/proj/c.i".to_string(), None, Some((a, a + 5))),
        ]
    );
}

// Both enums grow with the analysis, so a consumer must carry a fallback arm. The
// denied lint turns that arm unreachable, and so a failure, if the attribute that
// requires it is ever dropped.
#[test]
fn completeness_and_degraded_reasons_are_non_exhaustive() {
    #[deny(unreachable_patterns)]
    fn trusted(completeness: &Completeness) -> bool {
        match completeness {
            Completeness::Complete => true,
            Completeness::Degraded { .. } => false,
            _ => false,
        }
    }
    #[deny(unreachable_patterns)]
    fn tag(reason: &DegradedReason) -> &'static str {
        match reason {
            DegradedReason::ParseRecovered => "parse",
            DegradedReason::UnresolvedInclude => "include",
            _ => "other",
        }
    }
    assert!(trusted(&Completeness::Complete));
    assert_eq!(tag(&DegradedReason::ParseRecovered), "parse");
}

/// The class edges and the unresolved rows of `/proj/<file>` over `siblings`, as
/// `(kind, key)` pairs and `(name, reason)` pairs.
fn class_rows(
    file: &str,
    source: &str,
    siblings: &[(&str, &str)],
) -> (Vec<String>, Vec<(String, UnresolvedReason)>) {
    let mut fs = InMemoryFileSystem::new();
    for (path, text) in siblings {
        fs.insert(PathBuf::from(format!("/proj/{path}")), *text);
    }
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let edges = run
        .with_file(format!("/proj/{file}"))
        .edge_set(source)
        .expect("edge set");
    let targets = edges
        .of_kind(EdgeKind::ClassReference)
        .map(|edge| edge.target.key().to_string())
        .collect();
    let rows = edges
        .unresolved()
        .iter()
        .filter(|row| row.kind == EdgeKind::ClassReference)
        .map(|row| (row.name.to_string(), row.reason))
        .collect();
    (targets, rows)
}

const GONE_ROW: (&str, UnresolvedReason) = ("lib.gone", UnresolvedReason::AbsentFromWorkspace);

fn assert_gone_row(rows: &[(String, UnresolvedReason)]) {
    assert!(
        rows.iter()
            .any(|(name, reason)| (name.as_str(), *reason) == GONE_ROW),
        "got {rows:?}"
    );
}

#[test]
fn an_explicit_using_beside_a_shipped_wildcard_keeps_a_header_miss() {
    let (_, rows) = class_rows(
        "A.cls",
        "USING Progress.Lang.*.\nUSING lib.Gone.\nCLASS A INHERITS Gone:\nEND CLASS.\n",
        &[],
    );
    assert_gone_row(&rows);
}

#[test]
fn an_explicit_using_beside_a_shipped_wildcard_keeps_a_type_site_miss() {
    let (_, rows) = class_rows(
        "p.p",
        "USING Progress.Lang.*.\nUSING lib.Gone.\n\
         DEFINE VARIABLE g AS Gone NO-UNDO.\ng = NEW Gone().\n",
        &[],
    );
    assert_gone_row(&rows);
}

#[test]
fn an_explicit_using_beside_a_shipped_wildcard_keeps_a_parents_header_miss() {
    let (targets, rows) = class_rows(
        "C.cls",
        "CLASS C INHERITS pkg.Mid:\nMETHOD PUBLIC VOID m():\n  Hello().\nEND METHOD.\nEND CLASS.\n",
        &[(
            "pkg/Mid.cls",
            "USING OpenEdge.Core.*.\nUSING lib.Gone.\nCLASS pkg.Mid INHERITS Gone:\nEND CLASS.\n",
        )],
    );
    assert_eq!(targets, ["pkg.mid"]);
    assert_gone_row(&rows);
}

#[test]
fn an_explicit_using_of_a_shipped_class_beside_a_wildcard_records_nothing() {
    let (_, rows) = class_rows(
        "A.cls",
        "USING lib.*.\nUSING Progress.Lang.Object.\nCLASS A INHERITS Object:\nEND CLASS.\n",
        &[],
    );
    assert!(rows.is_empty(), "got {rows:?}");
}

/// The unresolved rows of a child of `pkg.Mid`, whose source is `mid`.
/// `usings.i` imports `lib.*`, `usingx.i` imports `lib.Base`, and `lib.Base` and
/// `lib.AppError` exist.
fn rows_under_a_parent(mid: &str) -> Vec<(String, UnresolvedReason)> {
    let (_, rows) = class_rows(
        "Child.cls",
        "CLASS Child INHERITS pkg.Mid:\nMETHOD PUBLIC VOID m():\n  Hello().\nEND METHOD.\nEND CLASS.\n",
        &[
            ("pkg/Mid.cls", mid),
            ("usings.i", "USING lib.*.\n"),
            ("usingx.i", "USING lib.Base.\n"),
            ("lib/Base.cls", "CLASS lib.Base:\nEND CLASS.\n"),
            ("lib/AppError.cls", "CLASS lib.AppError:\nEND CLASS.\n"),
        ],
    );
    rows
}

/// [`rows_under_a_parent`] for a parent whose header comes after `head` and an
/// include.
fn rows_under_a_parent_with_include_after(head: &str) -> Vec<(String, UnresolvedReason)> {
    rows_under_a_parent(&format!(
        "{head}{{usings.i}}\nCLASS pkg.Mid INHERITS Base:\nEND CLASS.\n"
    ))
}

#[test]
fn a_using_of_class_or_interface_does_not_hide_a_later_include() {
    for head in [
        "",
        "USING Progress.Lang.Object.\n",
        "USING Progress.Lang.Class.\n",
        "USING Progress.Reflect.Interface.\n",
    ] {
        let rows = rows_under_a_parent_with_include_after(head);
        assert!(
            rows.iter()
                .any(|row| row == &("base".to_string(), UnresolvedReason::Unknowable)),
            "head {head:?}: got {rows:?}"
        );
    }
}

fn has_row(rows: &[(String, UnresolvedReason)], name: &str, reason: UnresolvedReason) -> bool {
    rows.iter().any(|(n, r)| (n.as_str(), *r) == (name, reason))
}

#[test]
fn a_shipped_import_beside_an_unseen_one_does_not_answer_for_a_parents_header() {
    let missed: Vec<_> = [
        ("", "usings.i", "Base", "base"),
        ("USING Progress.Lang.*.\n", "usings.i", "Base", "base"),
        ("USING Progress.Lang.*.\n", "usingx.i", "Base", "base"),
        ("USING OpenEdge.Core.*.\n", "usingx.i", "Base", "base"),
        ("", "usings.i", "AppError", "apperror"),
    ]
    .into_iter()
    .filter(|(head, include, parent, row)| {
        let rows = rows_under_a_parent(&format!(
            "{head}{{{include}}}\nCLASS pkg.Mid INHERITS {parent}:\nEND CLASS.\n"
        ));
        !has_row(&rows, row, UnresolvedReason::Unknowable)
    })
    .collect();
    assert!(missed.is_empty(), "no Unknowable row for {missed:?}");
}

#[test]
fn a_shipped_name_a_parent_wrote_in_full_stays_quiet_beside_an_unseen_import() {
    for (head, parent) in [
        ("", "Progress.Lang.AppError"),
        ("USING Progress.Lang.AppError.\n", "AppError"),
    ] {
        let rows = rows_under_a_parent(&format!(
            "{head}{{usings.i}}\nCLASS pkg.Mid INHERITS {parent}:\nEND CLASS.\n"
        ));
        assert!(rows.is_empty(), "{head:?} INHERITS {parent}: got {rows:?}");
    }
}

#[test]
fn a_function_prototypes_parameter_types_are_class_sites() {
    for tail in ["FORWARD", "IN SUPER", "IN h"] {
        let found = format!("FUNCTION g RETURNS LOGICAL (INPUT p AS pkg.Thing) {tail}.\n");
        let (targets, rows) = class_rows(
            "p.p",
            &found,
            &[("pkg/Thing.cls", "CLASS pkg.Thing:\nEND CLASS.\n")],
        );
        assert_eq!(targets, ["pkg.thing"], "{tail}: got {rows:?}");

        let lost = format!("FUNCTION g RETURNS LOGICAL (INPUT p AS pkg.Gone) {tail}.\n");
        let (_, rows) = class_rows("p.p", &lost, &[]);
        assert!(
            has_row(&rows, "pkg.gone", UnresolvedReason::AbsentFromWorkspace),
            "{tail}: got {rows:?}"
        );
    }
}

/// The class edges and unresolved rows of `source`, a file beside `pkg.Thing`.
fn generic_rows(source: &str) -> (Vec<String>, Vec<(String, UnresolvedReason)>) {
    class_rows(
        "p.p",
        source,
        &[("pkg/Thing.cls", "CLASS pkg.Thing:\nEND CLASS.\n")],
    )
}

#[test]
fn a_generic_types_arguments_are_class_sites_at_a_declaration() {
    let (targets, rows) = generic_rows(
        "USING Progress.Collections.*.\n\
         DEFINE VARIABLE l AS List<pkg.Thing> NO-UNDO.\n",
    );
    assert_eq!(targets, ["pkg.thing"], "got {rows:?}");
    assert!(
        rows.is_empty(),
        "the shipped outer class gives no row: {rows:?}"
    );

    let (_, rows) =
        generic_rows("DEFINE VARIABLE l AS Progress.Collections.List<pkg.Gone> NO-UNDO.\n");
    assert!(
        has_row(&rows, "pkg.gone", UnresolvedReason::AbsentFromWorkspace),
        "got {rows:?}"
    );
}

#[test]
fn a_generic_types_arguments_are_class_sites_at_new() {
    let (targets, _) = generic_rows(
        "USING Progress.Collections.*.\n\
         DEFINE VARIABLE o AS Progress.Lang.Object NO-UNDO.\n\
         o = NEW List<pkg.Thing>().\n",
    );
    assert_eq!(targets, ["pkg.thing"]);

    let (_, rows) = generic_rows(
        "DEFINE VARIABLE o AS Progress.Lang.Object NO-UNDO.\n\
         o = NEW Progress.Collections.List<pkg.Gone>().\n",
    );
    assert!(
        has_row(&rows, "pkg.gone", UnresolvedReason::AbsentFromWorkspace),
        "got {rows:?}"
    );
}

#[test]
fn a_generic_types_arguments_follow_the_files_imports() {
    let (targets, rows) = generic_rows(
        "USING pkg.*.\n\
         DEFINE VARIABLE m AS Progress.Collections.HashMap<Thing, Gone> NO-UNDO.\n",
    );
    assert_eq!(targets, ["pkg.thing"]);
    assert!(
        has_row(&rows, "gone", UnresolvedReason::AbsentFromWorkspace),
        "got {rows:?}"
    );

    let (targets, rows) = generic_rows(
        "USING Progress.Collections.*.\n\
         USING Progress.Lang.*.\n\
         DEFINE VARIABLE m AS HashMap<Object, Progress.Lang.Object> NO-UNDO.\n",
    );
    assert!(
        targets.is_empty() && rows.is_empty(),
        "{targets:?} {rows:?}"
    );
}

#[test]
fn a_generic_new_in_an_unmodelled_statement_names_its_arguments() {
    for statement in [
        "EXPORT NEW Progress.Collections.List<{}>().",
        "PUT UNFORMATTED NEW Progress.Collections.List<{}>():ToString() SKIP.",
    ] {
        let (targets, _) = generic_rows(&statement.replace("{}", "pkg.Thing"));
        assert_eq!(targets, ["pkg.thing"], "{statement}");

        let (_, rows) = generic_rows(&statement.replace("{}", "pkg.Gone"));
        assert!(
            has_row(&rows, "pkg.gone", UnresolvedReason::AbsentFromWorkspace),
            "{statement}: got {rows:?}"
        );
    }
}

#[test]
fn a_nested_type_argument_is_a_class_site() {
    let (targets, rows) = generic_rows(
        "DEFINE VARIABLE m AS Progress.Collections.HashMap<pkg.Thing, \
         Progress.Collections.List<pkg.Gone>> NO-UNDO.\n",
    );
    assert_eq!(targets, ["pkg.thing"]);
    assert!(
        has_row(&rows, "pkg.gone", UnresolvedReason::AbsentFromWorkspace),
        "got {rows:?}"
    );
}

#[test]
fn a_generic_types_arguments_are_class_sites_at_every_declared_type() {
    for source in [
        "DEFINE INPUT PARAMETER p AS Progress.Collections.List<pkg.Gone> NO-UNDO.\n",
        "FUNCTION g RETURNS Progress.Collections.List<pkg.Gone> ():\n  RETURN ?.\nEND FUNCTION.\n",
        "FUNCTION g RETURNS LOGICAL (INPUT p AS Progress.Collections.List<pkg.Gone>) FORWARD.\n",
        "DEFINE TEMP-TABLE tt NO-UNDO\n  FIELD f AS CLASS Progress.Collections.List<pkg.Gone>.\n",
    ] {
        let (_, rows) = generic_rows(source);
        assert!(
            has_row(&rows, "pkg.gone", UnresolvedReason::AbsentFromWorkspace),
            "{source:?}: got {rows:?}"
        );
    }
    let (_, rows) = class_rows(
        "A.cls",
        "CLASS A:\n  METHOD PUBLIC VOID m(INPUT p AS Progress.Collections.List<pkg.Gone>):\n  END METHOD.\n\
         \n  DEFINE PUBLIC PROPERTY q AS Progress.Collections.List<pkg.Gone> NO-UNDO GET. SET.\nEND CLASS.\n",
        &[],
    );
    assert!(
        has_row(&rows, "pkg.gone", UnresolvedReason::AbsentFromWorkspace),
        "got {rows:?}"
    );
}

// The parser cannot read a generic type in these positions, so the set is
// degraded; if it learns to, the argument must be an edge or a row instead.
#[test]
fn a_generic_type_in_a_position_the_parser_skips_is_never_silent() {
    for source in [
        "DEFINE VARIABLE o AS Progress.Lang.Object NO-UNDO.\n\
         o = CAST(o, Progress.Collections.List<pkg.Gone>).\n",
        "CLASS A IMPLEMENTS Progress.Collections.IComparable<pkg.Gone>:\nEND CLASS.\n",
    ] {
        let mut fs = InMemoryFileSystem::new();
        fs.insert(
            PathBuf::from("/proj/pkg/Thing.cls"),
            "CLASS pkg.Thing:\nEND CLASS.\n",
        );
        let config = config("/proj");
        let run = LintPipeline::new(&config, &fs);
        let edges = run
            .with_file("/proj/p.p")
            .edge_set(source)
            .expect("edge set");
        let named = edges.unresolved().iter().any(|row| row.name == "pkg.gone");
        assert!(
            named || !edges.completeness().is_complete(),
            "{source:?}: complete with no row"
        );
    }
}

#[test]
fn a_class_in_a_block_headers_on_phrase_action_is_a_class_site() {
    for header in [
        "DO ON ERROR UNDO, RETURN ERROR NEW pkg.Thing()",
        "DO ON ERROR UNDO, THROW NEW pkg.Thing()",
        "REPEAT ON ERROR UNDO, RETURN ERROR NEW pkg.Thing()",
        "FOR EACH Customer NO-LOCK ON ERROR UNDO, RETURN ERROR NEW pkg.Thing()",
    ] {
        let source = format!("{header}:\n    LEAVE.\nEND.\n");
        let (targets, rows) = class_rows(
            "p.p",
            &source,
            &[("pkg/Thing.cls", "CLASS pkg.Thing:\nEND CLASS.\n")],
        );
        assert_eq!(targets, ["pkg.thing"], "{header}: got {rows:?}");

        let lost = source.replace("pkg.Thing", "pkg.Gone");
        let (_, rows) = class_rows("p.p", &lost, &[]);
        assert!(
            has_row(&rows, "pkg.gone", UnresolvedReason::AbsentFromWorkspace),
            "{header}: got {rows:?}"
        );
    }
}

#[test]
fn an_indexed_class_names_its_parent_through_its_own_using() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(
        PathBuf::from("/proj/pkg/Base.cls"),
        "CLASS pkg.Base:\n  METHOD PUBLIC VOID Hello():\n  END METHOD.\nEND CLASS.\n",
    );
    fs.insert(
        PathBuf::from("/proj/pkg/Mid.cls"),
        "USING pkg.*.\nCLASS pkg.Mid INHERITS Base:\nEND CLASS.\n",
    );
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/user.p");

    let edges = pipeline
        .edge_set("DEFINE VARIABLE o AS pkg.Mid NO-UNDO.\no = NEW pkg.Mid().\no:Hello().\n")
        .expect("edge set");

    let classes: Vec<_> = edges
        .of_kind(EdgeKind::ClassReference)
        .map(|e| e.target.key().to_string())
        .collect();
    assert_eq!(classes, ["pkg.base", "pkg.mid"]);
    assert!(
        edges.unresolved().is_empty(),
        "got {:?}",
        edges.unresolved()
    );
}
