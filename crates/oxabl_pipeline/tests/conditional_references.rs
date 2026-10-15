//! Class and program names written in `&IF` branches the preprocessor did not
//! take, and what an include in such a branch would read.

use std::path::PathBuf;

use oxabl_pipeline::{
    DependencyEdges, EdgeKind, EdgeTarget, LintPipeline, PipelineConfig, ReferenceSyntax,
};
use oxabl_workspace::InMemoryFileSystem;

fn fs(files: &[(&str, &str)]) -> InMemoryFileSystem {
    let mut fs = InMemoryFileSystem::new();
    for (path, text) in files {
        fs.insert(PathBuf::from(format!("/proj/{path}")), *text);
    }
    fs
}

fn config() -> PipelineConfig {
    PipelineConfig {
        include_paths: vec![PathBuf::from("/proj")],
        ..PipelineConfig::default()
    }
}

/// The edge set of `source` as `/proj/main.p`, and the path behind each
/// conditional reference's target.
fn edges(
    source: &str,
    files: &[(&str, &str)],
    closure: bool,
) -> (DependencyEdges, Vec<Option<PathBuf>>) {
    let fs = fs(files);
    let config = config();
    let run = LintPipeline::new(&config, &fs).with_conditional_include_closure(closure);
    let edges = run
        .with_file("/proj/main.p")
        .edge_set(source)
        .expect("edge set");
    let paths = edges
        .conditional_references()
        .iter()
        .map(|r| match &r.target {
            Some(EdgeTarget::IndexedFile { file, .. }) => run.indexed_path(*file),
            _ => None,
        })
        .collect();
    (edges, paths)
}

const CACHE: (&str, &str) = (
    "app/Cache.cls",
    "CLASS app.Cache:\n  DEFINE PUBLIC STATIC PROPERTY company AS CHARACTER NO-UNDO GET. SET.\nEND CLASS.\n",
);

#[test]
fn a_class_named_in_a_not_taken_branch_is_a_conditional_reference_not_an_edge() {
    let source = "&IF DEFINED(nope) &THEN\nDEFINE VARIABLE o AS app.Cache NO-UNDO.\no = NEW app.Cache().\n&ENDIF\n";
    let (edges, paths) = edges(source, &[CACHE], false);

    assert!(edges.edges().is_empty(), "{:?}", edges.edges());
    assert!(edges.completeness().is_complete());
    let got: Vec<_> = edges
        .conditional_references()
        .iter()
        .map(|r| (r.kind, r.syntax, r.name.as_str()))
        .collect();
    assert_eq!(
        got,
        [
            (
                EdgeKind::ClassReference,
                ReferenceSyntax::TypeName,
                "app.Cache"
            ),
            (EdgeKind::ClassReference, ReferenceSyntax::New, "app.Cache"),
        ]
    );
    assert!(
        paths
            .iter()
            .all(|p| p.as_deref() == Some(std::path::Path::new("/proj/app/Cache.cls")))
    );
    let first = &edges.conditional_references()[0];
    let span = first.span.expect("a site in the root");
    assert_eq!(&source[span.start as usize..span.end as usize], "app.Cache");
    assert_eq!(first.include_site, None);
    assert_eq!(first.branches.len(), 1);
    assert_eq!(first.branches[0].condition, "DEFINED(nope)");
}

#[test]
fn a_short_static_access_resolves_through_the_files_imports() {
    let source = "USING app.*.\n&IF FALSE &THEN\nMESSAGE Cache:company.\n&ENDIF\n";
    let (edges, paths) = edges(source, &[CACHE], false);
    let [reference] = edges.conditional_references() else {
        panic!("got {:?}", edges.conditional_references());
    };
    assert_eq!(reference.syntax, ReferenceSyntax::StaticAccess);
    assert_eq!(reference.name, "Cache");
    assert_eq!(
        paths[0].as_deref(),
        Some(std::path::Path::new("/proj/app/Cache.cls"))
    );
}

#[test]
fn a_not_taken_branch_in_a_taken_include_is_placed_by_the_include_site() {
    let source = "{glob.i}\n";
    let (edges, paths) = edges(
        source,
        &[
            CACHE,
            (
                "glob.i",
                "&IF FALSE &THEN\nMESSAGE app.Cache:company.\n&ENDIF\n",
            ),
        ],
        false,
    );
    let [reference] = edges.conditional_references() else {
        panic!("got {:?}", edges.conditional_references());
    };
    assert_eq!(reference.span, None);
    let site = reference.include_site.expect("an include site");
    assert_eq!(&source[site.start as usize..site.end as usize], "{glob.i}");
    assert!(paths[0].is_some());
}

#[test]
fn an_unresolved_name_is_kept_only_where_its_syntax_names_a_class_or_file() {
    let source = "&IF FALSE &THEN\n\
                  o = NEW app.Missing().\n\
                  MESSAGE hb:NAME.\n\
                  DEFINE VARIABLE v AS Unknown NO-UNDO.\n\
                  RUN missing.p.\n\
                  RUN internal-proc.\n\
                  &ENDIF\n";
    let (edges, paths) = edges(source, &[], false);
    let got: Vec<_> = edges
        .conditional_references()
        .iter()
        .map(|r| (r.syntax, r.name.as_str()))
        .collect();
    assert_eq!(
        got,
        [
            (ReferenceSyntax::New, "app.Missing"),
            (ReferenceSyntax::Run, "missing.p"),
        ]
    );
    assert!(paths.iter().all(Option::is_none));
}

#[test]
fn a_run_target_in_a_not_taken_branch_resolves_to_its_program() {
    let source = "&IF FALSE &THEN\nRUN sub/work.p.\nRUN work2.\n&ENDIF\n";
    let (edges, paths) = edges(
        source,
        &[("sub/work.p", "MESSAGE 1.\n"), ("work2.p", "MESSAGE 2.\n")],
        false,
    );
    let got: Vec<_> = edges
        .conditional_references()
        .iter()
        .map(|r| (r.kind, r.name.as_str()))
        .collect();
    assert_eq!(
        got,
        [
            (EdgeKind::ProgramReference, "sub/work.p"),
            (EdgeKind::ProgramReference, "work2"),
        ]
    );
    assert_eq!(
        paths,
        [
            Some(PathBuf::from("/proj/sub/work.p")),
            Some(PathBuf::from("/proj/work2.p"))
        ]
    );
}

#[test]
fn taken_text_makes_no_conditional_reference() {
    let source = "&IF TRUE &THEN\nRUN sub/work.p.\n&ENDIF\n";
    let (edges, _) = edges(source, &[("sub/work.p", "MESSAGE 1.\n")], false);
    assert!(edges.conditional_references().is_empty());
    assert_eq!(edges.of_kind(EdgeKind::ProgramReference).count(), 1);
}

const CLOSURE_FILES: &[(&str, &str)] = &[
    ("cond.i", "{inner.i}\n&IF FALSE &THEN\n{deeper.i}\n&ENDIF\n"),
    ("inner.i", "{leaf.i}\n"),
    ("leaf.i", "MESSAGE 1.\n"),
    ("deeper.i", "{cond.i}\n{gone.i}\n"),
];

#[test]
fn the_closure_of_a_conditional_include_is_off_by_default() {
    let (edges, _) = edges("&IF FALSE &THEN\n{cond.i}\n&ENDIF\n", CLOSURE_FILES, false);
    assert_eq!(edges.conditional_includes().len(), 1);
    assert!(edges.conditional_include_closure().is_empty());
}

#[test]
fn the_closure_lists_what_each_conditional_target_reads_once_through_a_cycle() {
    let (edges, _) = edges("&IF FALSE &THEN\n{cond.i}\n&ENDIF\n", CLOSURE_FILES, true);
    assert!(edges.edges().is_empty());
    let got: Vec<_> = edges
        .conditional_include_closure()
        .iter()
        .map(|c| {
            (
                c.name.as_str(),
                c.via.as_deref(),
                c.path.clone(),
                c.reaches.clone(),
                c.unresolved.clone(),
                c.complete,
            )
        })
        .collect();
    assert_eq!(
        got,
        [
            (
                "cond.i",
                None,
                Some(PathBuf::from("/proj/cond.i")),
                vec![
                    PathBuf::from("/proj/inner.i"),
                    PathBuf::from("/proj/leaf.i")
                ],
                vec![],
                true,
            ),
            (
                "deeper.i",
                Some("cond.i"),
                Some(PathBuf::from("/proj/deeper.i")),
                vec![
                    PathBuf::from("/proj/cond.i"),
                    PathBuf::from("/proj/inner.i"),
                    PathBuf::from("/proj/leaf.i"),
                ],
                vec!["gone.i".to_string()],
                false,
            ),
        ]
    );
}

#[test]
fn a_computed_or_missing_conditional_include_has_an_incomplete_closure() {
    let (edges, _) = edges(
        "&IF FALSE &THEN\n{{&dir}/x.i}\n{nowhere.i}\n&ENDIF\n",
        &[],
        true,
    );
    let got: Vec<_> = edges
        .conditional_include_closure()
        .iter()
        .map(|c| (c.name.as_str(), c.path.is_some(), c.complete))
        .collect();
    assert_eq!(
        got,
        [("nowhere.i", false, false), ("{&dir}/x.i", false, false)]
    );
}

#[test]
fn the_closure_lists_the_names_a_conditional_target_writes() {
    let files = [
        CACHE,
        ("app/sub.p", "MESSAGE 1.\n"),
        ("a.i", "&IF FALSE &THEN\n{b.i}\n&ENDIF\n"),
        (
            "b.i",
            "MESSAGE app.Cache:company.\nRUN app/sub.p.\nNEW app.Missing().\n&IF FALSE &THEN\nDEFINE VARIABLE o AS CLASS app.Gone NO-UNDO.\nRUN app/sub.p.\n&ENDIF\n",
        ),
    ];
    let fs = fs(&files);
    let config = config();
    let run = LintPipeline::new(&config, &fs).with_conditional_include_closure(true);
    let edges = run
        .with_file("/proj/main.p")
        .edge_set("{a.i}\n")
        .expect("edge set");

    assert_eq!(edges.of_kind(EdgeKind::ClassReference).count(), 0);
    let [entry] = edges.conditional_include_closure() else {
        panic!("one entry: {:?}", edges.conditional_include_closure());
    };
    assert_eq!(entry.name, "b.i");
    let got: Vec<_> = entry
        .references
        .iter()
        .map(|r| {
            let path = match &r.target {
                Some(EdgeTarget::IndexedFile { file, .. }) => run.indexed_path(*file),
                _ => None,
            };
            (r.kind, r.conditional, r.name.as_str(), path)
        })
        .collect();
    assert_eq!(
        got,
        [
            (
                EdgeKind::ClassReference,
                false,
                "app.cache",
                Some(PathBuf::from("/proj/app/Cache.cls"))
            ),
            (EdgeKind::ClassReference, false, "app.missing", None),
            (EdgeKind::ClassReference, true, "app.Gone", None),
            (
                EdgeKind::ProgramReference,
                false,
                "app/sub.p",
                Some(PathBuf::from("/proj/app/sub.p"))
            ),
            (
                EdgeKind::ProgramReference,
                true,
                "app/sub.p",
                Some(PathBuf::from("/proj/app/sub.p"))
            ),
        ]
    );
}
