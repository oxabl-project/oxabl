//! The inputs of the benches: the files of `resources/`, the 1 MB mixed file,
//! and a generated workspace in an in-memory file system.

use std::fmt::Write;
use std::path::{Path, PathBuf};

use oxabl_pipeline::PipelineConfig;
use oxabl_schema::SchemaLoader;
use oxabl_workspace::InMemoryFileSystem;

use crate::generate::Workspace;

/// The `resources/` directory of the repository.
pub fn resources_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../resources")
}

/// A file of `resources/`.
pub fn resource(name: &str) -> String {
    let path = resources_dir().join(name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

/// The parser fixtures that parse with no error, in the order of the large
/// mixed file.
pub const MIXED_PARTS: [&str; 10] = [
    "bench_parser_expressions.abl",
    "bench_parser_declarations.abl",
    "bench_parser_control_flow.abl",
    "bench_parser_oo_abl.abl",
    "bench_parser_procs_funcs.abl",
    "bench_parser_datasets.abl",
    "bench_parser_compiler_forms.abl",
    "bench_parser_type_spellings.abl",
    "bench_parser_name_forms.abl",
    "bench_parser_skipped_forms.abl",
];

/// The [`MIXED_PARTS`] repeated 40 times: 1,048,400 bytes.
pub fn large_mixed() -> String {
    let parts: Vec<String> = MIXED_PARTS.iter().map(|name| resource(name)).collect();
    let mut s = String::new();
    for _ in 0..40 {
        for part in &parts {
            s.push_str(part);
        }
    }
    s
}

/// The directory of `resources/lsp_bench.p` in [`lsp_bench`]'s file system.
pub const LSP_BENCH_DIR: &str = "/bench/resources";

/// `resources/lsp_bench.p` and its two includes in memory, with a
/// configuration that searches their directory.
pub fn lsp_bench() -> (InMemoryFileSystem, PipelineConfig, PathBuf, String) {
    let mut fs = InMemoryFileSystem::new();
    let dir = PathBuf::from(LSP_BENCH_DIR);
    for name in ["lsp_bench_defs.i", "lsp_bench_utils.i"] {
        fs.insert(dir.join(name), resource(name));
    }
    let source = resource("lsp_bench.p");
    let path = dir.join("lsp_bench.p");
    fs.insert(path.clone(), source.clone());
    let config = PipelineConfig {
        include_paths: vec![dir],
        ..PipelineConfig::default()
    };
    (fs, config, path, source)
}

/// `workspace` in memory below `dir`, with the configuration its `oxabl.toml`
/// says: include paths `src` and `.`, and its schema loaded.
pub fn in_memory(workspace: &Workspace, dir: &Path) -> (InMemoryFileSystem, PipelineConfig) {
    let mut fs = InMemoryFileSystem::new();
    for file in &workspace.files {
        fs.insert(dir.join(&file.path), file.text.clone());
    }
    let df = dir.join("schema/app.df");
    let (schema, problems) = SchemaLoader::load_files(std::slice::from_ref(&df), &fs);
    assert!(
        problems.is_empty(),
        "the generated schema loads: {problems:?}"
    );
    let config = PipelineConfig {
        include_paths: vec![dir.join("src"), dir.to_path_buf()],
        schema,
        schema_loaded: true,
        ..PipelineConfig::default()
    };
    (fs, config)
}

/// A root that includes `/rep/hot.i` in each of `procedures` procedures, and
/// the file system with the include: `findings` unused variables, so each
/// expansion states the same findings again and the fold of include findings
/// has one row for each and `procedures` expansions in it.
pub fn repeated_include(
    procedures: usize,
    findings: usize,
) -> (InMemoryFileSystem, PipelineConfig, PathBuf, String) {
    let dir = PathBuf::from("/rep");
    let mut fs = InMemoryFileSystem::new();
    let include: String = (0..findings)
        .map(|i| format!("DEFINE VARIABLE hot-unused-{i} AS INTEGER NO-UNDO.\n"))
        .collect();
    fs.insert(dir.join("hot.i"), include);
    let source: String = (0..procedures)
        .map(|p| format!("PROCEDURE rp{p}:\n{{hot.i}}\nEND PROCEDURE.\n"))
        .collect();
    let path = dir.join("root.p");
    fs.insert(path.clone(), source.clone());
    let config = PipelineConfig {
        include_paths: vec![dir],
        ..PipelineConfig::default()
    };
    (fs, config, path, source)
}

/// A tree for the scan of `SHARED` producers: `files` files of six kinds in
/// turn. Written `NEW SHARED` definitions, one with a `TRIGGERS` phrase, a file
/// with nothing but an include that holds the definition, consumers with an
/// include, classes with an include, and files the scan does not read.
pub fn shared_producers(files: usize) -> (InMemoryFileSystem, PipelineConfig, Vec<PathBuf>) {
    let dir = PathBuf::from("/sp");
    let mut fs = InMemoryFileSystem::new();
    fs.insert(
        dir.join("defs.i"),
        "DEFINE NEW SHARED VARIABLE sp-included AS INTEGER NO-UNDO.\n\
         DEFINE NEW SHARED TEMP-TABLE sp-included-tt NO-UNDO FIELD f AS INTEGER.\n",
    );
    fs.insert(
        dir.join("plain.i"),
        "DEFINE VARIABLE sp-note AS CHARACTER NO-UNDO.\n",
    );
    let body: String = (0..20)
        .map(|n| {
            format!(
                "PROCEDURE sp-run{n}:\n    DEFINE VARIABLE lv AS INTEGER NO-UNDO.\n    \
                 DO lv = 1 TO {n}:\n        MESSAGE lv.\n    END.\nEND PROCEDURE.\n"
            )
        })
        .collect();
    let mut paths = Vec::new();
    for i in 0..files {
        let (name, text) = match i % 6 {
            0 => (
                format!("w{i}.p"),
                format!(
                    "DEFINE NEW SHARED VARIABLE sp-v{i} AS INTEGER NO-UNDO.\n\
                     DEFINE NEW SHARED TEMP-TABLE sp-t{i} NO-UNDO FIELD f AS INTEGER.\n\
                     DEFINE VARIABLE sp-own AS INTEGER NO-UNDO.\n{body}"
                ),
            ),
            1 => (
                format!("t{i}.p"),
                format!(
                    "DEFINE NEW SHARED VARIABLE sp-trig{i} AS CHARACTER NO-UNDO\n    \
                     VIEW-AS FILL-IN\n    TRIGGERS:\n        ON LEAVE DO:\n            \
                     DEFINE VARIABLE sp-inner AS INTEGER NO-UNDO.\n        END.\n    \
                     END TRIGGERS.\n{body}"
                ),
            ),
            2 => (format!("i{i}.p"), "{defs.i}\n".to_string()),
            3 => (
                format!("c{i}.p"),
                format!(
                    "DEFINE SHARED VARIABLE sp-v{} AS INTEGER NO-UNDO.\n{{plain.i}}\n{body}",
                    i - 3
                ),
            ),
            4 => (
                format!("k{i}.cls"),
                format!(
                    "CLASS sp.K{i}:\n{{plain.i}}\n    METHOD PUBLIC VOID run():\n    END METHOD.\nEND CLASS.\n"
                ),
            ),
            _ => (format!("q{i}.p"), body.clone()),
        };
        let path = dir.join(name);
        fs.insert(path.clone(), text);
        paths.push(path);
    }
    let config = PipelineConfig {
        include_paths: vec![dir],
        ..PipelineConfig::default()
    };
    (fs, config, paths)
}

/// A procedure file of `procedures` internal procedures, each with blocks
/// nested `depth` deep: many scopes for the semantic passes.
pub fn nested_blocks(procedures: usize, depth: usize) -> String {
    let mut s = String::from("DEFINE VARIABLE g-total AS INTEGER NO-UNDO.\n");
    for p in 0..procedures {
        writeln!(
            s,
            "PROCEDURE np{p}:\n    DEFINE VARIABLE lv-{p} AS INTEGER NO-UNDO."
        )
        .unwrap();
        for d in 0..depth {
            let pad = "    ".repeat(d + 1);
            writeln!(
                s,
                "{pad}DO WHILE lv-{p} < {d}:\n{pad}    DEFINE VARIABLE lv-{p}-{d} AS INTEGER NO-UNDO.\n{pad}    lv-{p}-{d} = lv-{p} + g-total."
            )
            .unwrap();
        }
        for d in (0..depth).rev() {
            let pad = "    ".repeat(d + 1);
            writeln!(s, "{pad}    lv-{p} = lv-{p} + 1.\n{pad}END.").unwrap();
        }
        writeln!(s, "    RUN np{}.\nEND PROCEDURE.", (p + 1) % procedures).unwrap();
    }
    s
}
