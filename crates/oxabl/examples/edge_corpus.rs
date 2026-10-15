//! edge_corpus — build the dependency edge set of every ABL root under a tree and
//! report counts: roots, degraded roots, the include references and the class and
//! program names written in branches that were not taken, and, when asked, what
//! the conditional includes would read.
//!
//! Every `.p`, `.w`, `.cls` and `.v` file is a root; `.i` files are only reached
//! through one. Each root is expanded the way `oxabl check` expands it (the
//! tree's `oxabl.toml`, preprocessing on).
//!
//! `OXABL_EDGE_CLOSURE=1` turns on the closure of the conditional includes.
//! `OXABL_ROOT_RELATIVE=.,rf` replaces the tree's root-relative include paths.
//!
//! # Usage
//! ```text
//! OXABL_EDGE_CORPUS=<dir> cargo run -p oxabl --release --example edge_corpus
//! ```

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use std::time::Instant;

use oxabl_pipeline::{
    BranchKind, Completeness, ConfigOverrides, EdgeKind, EdgeTarget, LintPipeline, PipelineConfig,
};
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
    degraded: BTreeMap<&'static str, usize>,
    with_reference: usize,
    references: usize,
    references_resolved: usize,
    references_in_root: usize,
    references_by_syntax: BTreeMap<&'static str, (usize, usize)>,
    reference_targets: BTreeSet<PathBuf>,
    /// (root, target file) pairs a conditional reference adds beyond the edges.
    reference_new_pairs: usize,
    max_references_per_root: usize,
    closure_entries: usize,
    closure_resolved: usize,
    closure_incomplete: usize,
    closure_nested: usize,
    closure_files: BTreeSet<PathBuf>,
    /// (root, file) pairs the closure adds beyond the root's include edges.
    closure_new_pairs: usize,
    closure_references: usize,
    closure_references_resolved: usize,
    closure_references_conditional: usize,
    closure_reference_targets: BTreeSet<PathBuf>,
    /// (root, file) pairs the closure's references add beyond the edges and the
    /// conditional references.
    closure_reference_new_pairs: usize,
    class_edges: usize,
    program_edges: usize,
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
    let mut config = config;
    if let Some(list) = std::env::var_os("OXABL_ROOT_RELATIVE") {
        config.root_relative_include_paths = list
            .to_string_lossy()
            .split(',')
            .filter(|entry| !entry.is_empty())
            .map(PathBuf::from)
            .collect();
    }
    let closure = std::env::var_os("OXABL_EDGE_CLOSURE").is_some_and(|v| v == "1");
    let fs = RealFileSystem;
    let pipeline = LintPipeline::new(&config, &fs)
        .with_preprocess(true)
        .with_conditional_include_closure(closure);

    let started = Instant::now();
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
        r.class_edges += edges.of_kind(EdgeKind::ClassReference).count();
        r.program_edges += edges.of_kind(EdgeKind::ProgramReference).count();
        if edges.of_kind(EdgeKind::DirectInclude).next().is_some() {
            r.with_direct_include += 1;
        }
        if let Completeness::Degraded { reasons } = edges.completeness() {
            *r.degraded.entry("any").or_default() += 1;
            for reason in reasons {
                *r.degraded.entry(reason.as_str()).or_default() += 1;
            }
        }
        let edge_files: BTreeSet<PathBuf> = edges
            .edges()
            .iter()
            .filter_map(|e| match &e.target {
                EdgeTarget::IncludePath(path) => Some(path.clone()),
                EdgeTarget::IndexedFile { file, .. } => pipeline.indexed_path(*file),
                EdgeTarget::SchemaTable(_) => None,
            })
            .collect();
        let references = edges.conditional_references();
        if !references.is_empty() {
            r.with_reference += 1;
        }
        r.references += references.len();
        r.max_references_per_root = r.max_references_per_root.max(references.len());
        let mut new_targets = BTreeSet::new();
        for reference in references {
            let target = match &reference.target {
                Some(EdgeTarget::IndexedFile { file, .. }) => pipeline.indexed_path(*file),
                _ => None,
            };
            let counts = r
                .references_by_syntax
                .entry(reference.syntax.as_str())
                .or_default();
            counts.0 += 1;
            r.references_in_root += usize::from(reference.span.is_some());
            if let Some(target) = target {
                counts.1 += 1;
                r.references_resolved += 1;
                if !edge_files.contains(&target) {
                    new_targets.insert(target.clone());
                }
                r.reference_targets.insert(target);
            }
        }
        r.reference_new_pairs += new_targets.len();
        let reference_files: BTreeSet<PathBuf> = references
            .iter()
            .filter_map(|reference| match &reference.target {
                Some(EdgeTarget::IndexedFile { file, .. }) => pipeline.indexed_path(*file),
                _ => None,
            })
            .collect();
        let mut new_reference_files = BTreeSet::new();
        let mut new_files = BTreeSet::new();
        for entry in edges.conditional_include_closure() {
            r.closure_entries += 1;
            r.closure_resolved += usize::from(entry.path.is_some());
            r.closure_incomplete += usize::from(!entry.complete);
            r.closure_nested += usize::from(entry.via.is_some());
            for reference in &entry.references {
                r.closure_references += 1;
                r.closure_references_conditional += usize::from(reference.conditional);
                let Some(EdgeTarget::IndexedFile { file, .. }) = &reference.target else {
                    continue;
                };
                r.closure_references_resolved += 1;
                if let Some(target) = pipeline.indexed_path(*file) {
                    if !edge_files.contains(&target) && !reference_files.contains(&target) {
                        new_reference_files.insert(target.clone());
                    }
                    r.closure_reference_targets.insert(target);
                }
            }
            for path in entry.path.iter().chain(&entry.reaches) {
                r.closure_files.insert(path.clone());
                if !edge_files.contains(path) {
                    new_files.insert(path.clone());
                }
            }
        }
        r.closure_new_pairs += new_files.len();
        r.closure_reference_new_pairs += new_reference_files.len();
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

    let elapsed = started.elapsed();
    println!(
        "wall time:                       {:.2}s",
        elapsed.as_secs_f64()
    );
    println!("roots analysed:                  {}", r.roots);
    println!("class edges:                     {}", r.class_edges);
    println!("program edges:                   {}", r.program_edges);
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
    for (reason, count) in &r.degraded {
        println!("degraded roots ({reason}): {count}");
    }
    println!("roots with a conditional reference: {}", r.with_reference);
    println!("conditional references:          {}", r.references);
    println!("  resolved to a file:            {}", r.references_resolved);
    println!("  written in the root itself:    {}", r.references_in_root);
    for (syntax, (all, resolved)) in &r.references_by_syntax {
        println!("  {syntax}: {all} ({resolved} resolved)");
    }
    println!(
        "  distinct resolved files:       {}",
        r.reference_targets.len()
    );
    println!("  root-file pairs beyond edges:  {}", r.reference_new_pairs);
    println!(
        "  most in one root:              {}",
        r.max_references_per_root
    );
    if closure {
        println!("conditional include closure entries: {}", r.closure_entries);
        println!("  resolved:                      {}", r.closure_resolved);
        println!("  incomplete:                    {}", r.closure_incomplete);
        println!("  found in a target's branches:  {}", r.closure_nested);
        println!("  distinct files:                {}", r.closure_files.len());
        println!("  root-file pairs beyond edges:  {}", r.closure_new_pairs);
        println!("  references:                    {}", r.closure_references);
        println!(
            "    resolved to a file:          {}",
            r.closure_references_resolved
        );
        println!(
            "    in a target's branches:      {}",
            r.closure_references_conditional
        );
        println!(
            "    distinct resolved files:     {}",
            r.closure_reference_targets.len()
        );
        println!(
            "    root-file pairs beyond edges and conditional references: {}",
            r.closure_reference_new_pairs
        );
    }
}
