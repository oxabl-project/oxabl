//! `Referent::search`: the other files of a workspace that can name a
//! referent, in the order a client lists them.

use std::path::{Path, PathBuf};

use oxabl_pipeline::{LintPipeline, PipelineConfig, Referent, ReverseGraph};
use oxabl_workspace::InMemoryFileSystem;

const CLASS: &str = "/proj/app/Big.cls";

const CLASS_TEXT: &str = "CLASS app.Big:\n    METHOD PUBLIC INTEGER m(INPUT a AS INTEGER):\n        RETURN a.\n    END METHOD.\nEND CLASS.\n";

const CALLER: &str = "DEFINE VARIABLE o AS CLASS app.Big NO-UNDO.\no = NEW app.Big().\n";

/// Callers in an order that is neither the order of the paths nor the order
/// of their bytes: `-` is below `/` as a byte and above it as a component.
const CALLERS: &[&str] = &[
    "/proj/z.p",
    "/proj/app-old/c.p",
    "/proj/p10.p",
    "/proj/app.p",
    "/proj/p2.p",
    "/proj/app/c.p",
    "/proj/a.p",
];

fn paths(found: &[PathBuf]) -> Vec<&str> {
    found.iter().map(|path| path.to_str().unwrap()).collect()
}

fn setup() -> (ReverseGraph, Referent) {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from(CLASS), CLASS_TEXT);
    let mut units = vec![PathBuf::from(CLASS)];
    for caller in CALLERS {
        fs.insert(PathBuf::from(caller), CALLER);
        units.push(PathBuf::from(caller));
    }
    let config = PipelineConfig {
        include_paths: vec![PathBuf::from("/proj")],
        ..PipelineConfig::default()
    };
    let run = LintPipeline::new(&config, &fs).with_known_files(&units);
    let graph = ReverseGraph::build(&run, &units);
    let handle = run.with_file(CLASS);
    let expansion = handle.expand(CLASS_TEXT);
    let file_of = |_| None;
    let navigation = handle
        .navigation(&expansion, &file_of)
        .expect("the class preprocesses");
    let at = CLASS_TEXT.find("Big:").unwrap();
    let referent = navigation
        .referent_at(Path::new(CLASS), u32::try_from(at).unwrap())
        .expect("the class name is a referent");
    (graph, referent)
}

// The declaring file comes first, then the callers in the order of their
// paths, without the file that asked.
#[test]
fn a_search_lists_the_declaring_file_and_then_the_callers_in_path_order() {
    let (graph, referent) = setup();
    let found = referent.search(&graph, Path::new("/proj/p2.p"), usize::MAX);
    assert_eq!(
        paths(&found.units),
        [
            CLASS,
            "/proj/a.p",
            "/proj/app/c.p",
            "/proj/app-old/c.p",
            "/proj/app.p",
            "/proj/p10.p",
            "/proj/z.p",
        ]
    );
    assert!(found.gaps.is_empty(), "{:?}", found.gaps);
}

// A cut at the limit keeps the declaring file and the first callers, and says
// how many files it left out.
#[test]
fn a_search_cut_at_the_limit_keeps_the_first_files() {
    let (graph, referent) = setup();
    let found = referent.search(&graph, Path::new("/proj/p2.p"), 3);
    assert_eq!(paths(&found.units), [CLASS, "/proj/a.p", "/proj/app/c.p"]);
    assert_eq!(found.gaps.len(), 1, "{:?}", found.gaps);
    assert!(
        found.gaps[0].reason.contains("3 of the 7"),
        "{:?}",
        found.gaps
    );
}
