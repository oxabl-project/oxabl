//! edge_corpus — build the dependency edge set of every ABL root under a tree and
//! report counts: roots, edges, and the include references written in branches
//! that were not taken.
//!
//! Every `.p`, `.w`, `.cls` and `.v` file is a root; `.i` files are only reached
//! through one. Each root is expanded the way `oxabl check` expands it (the
//! tree's `oxabl.toml`, preprocessing on).
//!
//! # Usage
//! ```text
//! OXABL_EDGE_CORPUS=<dir> cargo run -p oxabl --release --example edge_corpus
//! ```

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use oxabl_pipeline::{BranchKind, ConfigOverrides, EdgeKind, LintPipeline, PipelineConfig};
use oxabl_workspace::RealFileSystem;
use walkdir::WalkDir;

const ROOT_EXTENSIONS: &[&str] = &["p", "w", "cls", "v"];

#[derive(Default)]
struct Report {
    roots: usize,
    failed: usize,
    with_direct_include: usize,
    with_conditional: usize,
    conditional: usize,
    conditional_in_root: usize,
    conditional_computed: usize,
    conditional_else_or_elseif: usize,
    distinct_names: BTreeSet<String>,
    max_per_root: usize,
}

fn discover(root: &Path) -> Vec<PathBuf> {
    WalkDir::new(root)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .map(|e| std::path::absolute(e.path()).unwrap_or_else(|_| e.into_path()))
        .filter(|p| {
            p.extension()
                .and_then(|e| e.to_str())
                .is_some_and(|e| ROOT_EXTENSIONS.contains(&e.to_ascii_lowercase().as_str()))
        })
        .collect()
}

fn main() {
    let Some(root) = std::env::var_os("OXABL_EDGE_CORPUS").map(PathBuf::from) else {
        eprintln!("set OXABL_EDGE_CORPUS to a directory");
        std::process::exit(2);
    };
    let files = discover(&root);
    if files.is_empty() {
        eprintln!("no ABL roots under {}", root.display());
        std::process::exit(2);
    }
    let (config, warnings) = PipelineConfig::resolve(&root, &ConfigOverrides::default());
    for w in &warnings {
        eprintln!("warning: {w}");
    }
    let fs = RealFileSystem;
    let pipeline = LintPipeline::new(&config, &fs).with_preprocess(true);

    let mut r = Report::default();
    for file in &files {
        let Ok(bytes) = std::fs::read(file) else {
            r.failed += 1;
            continue;
        };
        let source = String::from_utf8_lossy(&bytes);
        let Ok(edges) = pipeline.with_file(file).edge_set(&source) else {
            r.failed += 1;
            continue;
        };
        r.roots += 1;
        if edges.of_kind(EdgeKind::DirectInclude).next().is_some() {
            r.with_direct_include += 1;
        }
        let conditional = edges.conditional_includes();
        if conditional.is_empty() {
            continue;
        }
        r.with_conditional += 1;
        r.conditional += conditional.len();
        r.max_per_root = r.max_per_root.max(conditional.len());
        for c in conditional {
            r.conditional_in_root += usize::from(c.span.is_some());
            r.conditional_computed += usize::from(c.computed_name);
            r.conditional_else_or_elseif +=
                usize::from(c.branches.last().is_some_and(|b| b.kind != BranchKind::If));
            r.distinct_names.insert(c.name.to_ascii_lowercase());
        }
    }

    println!("roots analysed:                  {}", r.roots);
    println!("roots that failed:               {}", r.failed);
    println!("roots with a direct include:     {}", r.with_direct_include);
    println!("roots with a conditional include: {}", r.with_conditional);
    println!("conditional include references:  {}", r.conditional);
    println!("  written in the root itself:    {}", r.conditional_in_root);
    println!(
        "  with a computed name:          {}",
        r.conditional_computed
    );
    println!(
        "  in an ELSEIF or ELSE branch:   {}",
        r.conditional_else_or_elseif
    );
    println!(
        "  distinct names:                {}",
        r.distinct_names.len()
    );
    println!("  most in one root:              {}", r.max_per_root);
}
