//! `[workspace.preprocessor]` reaching the edge set: the configured environment
//! decides which include an `&IF` names.

use std::path::{Path, PathBuf};

use oxabl_pipeline::{
    ConfigOverrides, DegradedReason, EdgeKind, LintPipeline, resolve_from_config,
};
use oxabl_workspace::{InMemoryFileSystem, WorkspaceConfig};

const SOURCE: &str = "&IF \"{&OPSYS}\" = \"UNIX\" &THEN\n{unix.i}\n&ELSE\n{win.i}\n&ENDIF\n\
                      &IF {&PROVERSION} >= 12 &THEN\n{new.i}\n&ELSE\n{old.i}\n&ENDIF\n";

fn includes(table: &str) -> (Vec<String>, Vec<String>) {
    let toml = format!("[workspace]\nname = \"p\"\n{table}");
    let workspace = WorkspaceConfig::from_toml(&toml).expect("config parses");
    let (mut config, warnings) =
        resolve_from_config(&workspace, Path::new("/proj"), &ConfigOverrides::default());
    assert!(warnings.is_empty(), "{warnings:?}");
    config.include_paths = vec![PathBuf::from("/proj")];

    let mut fs = InMemoryFileSystem::new();
    for name in ["unix.i", "win.i", "new.i", "old.i"] {
        fs.insert(PathBuf::from(format!("/proj/{name}")), "/* x */\n");
    }
    let base = LintPipeline::new(&config, &fs);
    let pipeline = base.with_file("/proj/main.p");
    let expansion = pipeline.expand(SOURCE);
    let result = pipeline.collect(&expansion);

    let edges = pipeline.edges_of(&expansion, &result).expect("edge set");
    let mut named: Vec<String> = edges
        .of_kind(EdgeKind::DirectInclude)
        .map(|edge| edge.target.key().rsplit('/').next().unwrap().to_string())
        .collect();
    named.sort();
    let mut warned: Vec<String> = result
        .all()
        .filter(|d| d.diagnostic.code.0 == "PREPROC009")
        .map(|d| d.diagnostic.message.clone())
        .collect();
    warned.sort();
    (named, warned)
}

#[test]
fn defaults_take_the_unix_branch_and_flag_the_unset_version() {
    let (named, warned) = includes("");
    assert_eq!(named, ["old.i", "unix.i"]);
    assert_eq!(warned.len(), 1);
    assert!(warned[0].contains("{&PROVERSION}"), "{warned:?}");
}

#[test]
fn the_table_overrides_every_value_it_names() {
    let (named, warned) =
        includes("[workspace.preprocessor]\nopsys = \"WIN32\"\nproversion = \"12.8\"\n");
    assert_eq!(named, ["new.i", "win.i"]);
    assert!(warned.is_empty(), "{warned:?}");
}

#[test]
fn file_name_is_the_path_the_client_analysed_the_file_under() {
    let config = oxabl_pipeline::PipelineConfig {
        include_paths: vec![PathBuf::from("/proj")],
        ..Default::default()
    };
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/proj/frame.i"), "/* x */\n");
    let base = LintPipeline::new(&config, &fs);
    let source = "&IF \"{&FILE-NAME}\" MATCHES \"*.w\" &THEN\n{frame.i}\n&ENDIF\n";

    for (file, expected) in [("/proj/main.w", 1), ("/proj/main.p", 0)] {
        let pipeline = base.with_file(file);
        let edges = pipeline.edge_set(source).expect("edge set");
        assert_eq!(
            edges.of_kind(EdgeKind::DirectInclude).count(),
            expected,
            "{file}"
        );
        assert!(edges.unresolved().is_empty(), "{file}");
    }
}

/// The includes named, the loud warning codes and the completeness reasons of `source`.
fn analyse(table: &str, source: &str) -> (Vec<String>, Vec<String>, Vec<DegradedReason>) {
    let toml = format!("[workspace]\nname = \"p\"\n{table}");
    let workspace = WorkspaceConfig::from_toml(&toml).expect("config parses");
    let (mut config, _) =
        resolve_from_config(&workspace, Path::new("/proj"), &ConfigOverrides::default());
    config.include_paths = vec![PathBuf::from("/proj")];
    let mut fs = InMemoryFileSystem::new();
    for name in ["a.i", "b.i", "c.i"] {
        fs.insert(PathBuf::from(format!("/proj/{name}")), "/* x */\n");
    }
    let base = LintPipeline::new(&config, &fs);
    let pipeline = base.with_file("/proj/main.p");
    let expansion = pipeline.expand(source);
    let result = pipeline.collect(&expansion);
    let edges = pipeline.edges_of(&expansion, &result).expect("edge set");
    let mut named: Vec<String> = edges
        .of_kind(EdgeKind::DirectInclude)
        .map(|edge| edge.target.key().rsplit('/').next().unwrap().to_string())
        .collect();
    named.sort();
    let mut codes: Vec<String> = result
        .all()
        .map(|d| d.diagnostic.code.0.to_string())
        .filter(|code| code.starts_with("PREPROC"))
        .collect();
    codes.sort();
    (named, codes, edges.completeness().reasons().to_vec())
}

#[test]
fn the_opsys_function_reads_the_configured_environment() {
    let source = "&IF OPSYS = \"UNIX\" &THEN {a.i} &ELSE {b.i} &ENDIF\n";
    let (named, codes, reasons) = analyse("", source);
    assert_eq!(named, ["a.i"]);
    assert!(
        codes.is_empty() && reasons.is_empty(),
        "{codes:?} {reasons:?}"
    );

    let (named, codes, reasons) = analyse("[workspace.preprocessor]\nopsys = \"WIN32\"\n", source);
    assert_eq!(named, ["b.i"]);
    assert!(
        codes.is_empty() && reasons.is_empty(),
        "{codes:?} {reasons:?}"
    );
}

#[test]
fn the_proversion_function_reads_the_configured_version_or_degrades() {
    let source = "&IF PROVERSION >= \"12\" &THEN {a.i} &ELSE {b.i} &ENDIF\n";
    let (named, codes, reasons) =
        analyse("[workspace.preprocessor]\nproversion = \"12.8\"\n", source);
    assert_eq!(named, ["a.i"]);
    assert!(
        codes.is_empty() && reasons.is_empty(),
        "{codes:?} {reasons:?}"
    );

    let (_, codes, reasons) = analyse("", source);
    assert_eq!(codes, ["PREPROC009"]);
    assert_eq!(reasons, [DegradedReason::PreprocessorLoss]);
}

#[test]
fn defined_inside_logical_follows_the_defines() {
    let source = "&IF NOT LOGICAL(DEFINED(page-flow)) &THEN {a.i} &ELSE {b.i} &ENDIF\n";
    let (named, ..) = analyse("", source);
    assert_eq!(named, ["a.i"]);
    let defined = format!("&GLOBAL-DEFINE page-flow yes\n{source}");
    let (named, codes, reasons) = analyse("", &defined);
    assert_eq!(named, ["b.i"]);
    assert!(
        codes.is_empty() && reasons.is_empty(),
        "{codes:?} {reasons:?}"
    );
}

#[test]
fn a_function_the_evaluator_does_not_model_degrades_the_set() {
    for condition in [
        "SEARCH(\"a.i\") <> ?",
        "KEYWORD(\"def\") = \"DEFINE\"",
        "SOMETHING = 1",
    ] {
        let source = format!("&IF {condition} &THEN {{a.i}} &ELSE {{b.i}} &ENDIF\n");
        let (_, codes, reasons) = analyse("", &source);
        assert_eq!(codes, ["PREPROC010"], "{condition}");
        assert_eq!(reasons, [DegradedReason::PreprocessorLoss], "{condition}");
    }
}
