//! `[workspace.preprocessor]` reaching the edge set: the configured environment
//! decides which include an `&IF` names.

use std::path::{Path, PathBuf};

use oxabl_pipeline::{ConfigOverrides, EdgeKind, LintPipeline, resolve_from_config};
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
