//! A `~{` in an include argument that no `}` closes makes the edge set
//! degraded where the include uses the argument: OpenEdge 12.8.9 stops with
//! error 374 (unmatched braces). A closed one is a nested reference, and the
//! file it names is a dependency.

use std::fs;

use oxabl_pipeline::{Completeness, ConfigOverrides, DegradedReason, LintPipeline, PipelineConfig};
use oxabl_workspace::RealFileSystem;

fn completeness(source: &str) -> Completeness {
    let tmp = tempfile::TempDir::new().unwrap();
    let root = tmp.path();
    fs::write(root.join("r.i"), "RUN {1}.\nMESSAGE \"{2}\".\n").unwrap();
    fs::write(root.join("main.p"), source).unwrap();
    fs::write(
        root.join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
    )
    .unwrap();
    let (config, warnings) =
        PipelineConfig::resolve(&root.join("main.p"), &ConfigOverrides::default());
    assert!(warnings.is_empty(), "{warnings:?}");
    let fs = RealFileSystem;
    let run = LintPipeline::new(&config, &fs);
    let edges = run
        .with_file(root.join("main.p"))
        .edge_set(source)
        .expect("edge set");
    edges.completeness().clone()
}

#[test]
fn an_unclosed_escaped_open_brace_in_an_argument_degrades_the_edge_set() {
    assert_eq!(
        completeness("{r.i one.p a~{b}\n"),
        Completeness::Degraded {
            reasons: vec![DegradedReason::PreprocessorLoss]
        }
    );
}

#[test]
fn an_escaped_closing_brace_in_an_argument_does_not() {
    assert_eq!(completeness("{r.i one.p a~}b}\n"), Completeness::Complete);
}

#[test]
fn a_nested_reference_to_a_missing_file_is_an_unresolved_include() {
    assert_eq!(
        completeness("{r.i one.p a~{gone.i~}}\n"),
        Completeness::Degraded {
            reasons: vec![DegradedReason::UnresolvedInclude]
        }
    );
}
