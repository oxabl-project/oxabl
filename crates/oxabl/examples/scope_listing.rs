//! scope_listing — compare `oxabl_scope` with the OpenEdge compiler's
//! `COMPILE ... LISTING` output: the plan's M3 exit gate.
//!
//! For every `.p`, `.w` and `.cls` file under a source tree, read the listing
//! the compiler wrote for it, parse its block table (`oxabl_scope::listing`),
//! run `oxabl_scope` on the file (expanded as `oxabl check` expands it, with the
//! tree's `oxabl.toml` and schema), and compare block by block: the blocks
//! both sides have, the `Tran` column against the model's transaction blocks,
//! and the `Buffers:` lines against the model's buffer scopes. Every
//! disagreement is printed with both sides; a block the model answers Unknown
//! is counted apart, not as a disagreement.
//!
//! `scripts/gen-scope-listings.sh` writes the listings on a licensed install.
//!
//! # Usage
//! ```text
//! OXABL_SCOPE_LISTING_SRC=<source dir> [OXABL_SCOPE_LISTING_DIR=<listing dir>] \
//!   cargo run -p oxabl --release --example scope_listing
//! ```
//! The listing of `<src>/a/b.p` is `<listing dir>/a/b.p.lst`, else
//! `<listing dir>/a/b.lst`; the listing dir defaults to the source dir. A
//! source without a listing is skipped and counted.

use std::collections::HashMap;
use std::panic::AssertUnwindSafe;
use std::path::{Path, PathBuf};

use oxabl_ast::Span;
use oxabl_pipeline::{ConfigOverrides, LintPipeline, PipelineConfig};
use oxabl_scope::ScopeOptions;
use oxabl_scope::listing::{self, Comparison, Disagreement, ListingBlock, ModelBlock};
use oxabl_workspace::RealFileSystem;
use walkdir::WalkDir;

const EXTENSIONS: &[&str] = &["p", "w", "cls"];

fn main() {
    let Some(src) = std::env::var_os("OXABL_SCOPE_LISTING_SRC").map(PathBuf::from) else {
        eprintln!("set OXABL_SCOPE_LISTING_SRC to the source directory");
        std::process::exit(2);
    };
    let src = std::path::absolute(&src).unwrap_or(src);
    let lst_dir = std::env::var_os("OXABL_SCOPE_LISTING_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| src.clone());
    let (config, warnings) = PipelineConfig::resolve(&src, &ConfigOverrides::default());
    for w in &warnings {
        eprintln!("warning: {w}");
    }
    let schema = config.schema_loaded.then_some(&config.schema);
    let fs = RealFileSystem;
    let pipeline = LintPipeline::new(&config, &fs).with_preprocess(true);

    let mut total = Comparison::default();
    let (mut files, mut without_listing, mut failed, mut disagreeing) = (0, 0, 0, 0);
    for file in discover(&src) {
        let rel = file.strip_prefix(&src).unwrap_or(&file).to_path_buf();
        let Some(lst) = listing_for(&lst_dir, &rel) else {
            without_listing += 1;
            continue;
        };
        files += 1;
        let (Ok(source), Ok(lst_text)) = (read(&file), read(&lst)) else {
            failed += 1;
            eprintln!("{}: unreadable", rel.display());
            continue;
        };
        let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
            compare_file(&pipeline, schema, &file, &rel, &source, &lst_text)
        }));
        let Ok(Some((c, recovered))) = result else {
            failed += 1;
            eprintln!("{}: could not be analysed", rel.display());
            continue;
        };
        total.add(&c);
        println!(
            "{}: blocks listing {} model {} matched {}; tran agree {}/{} (outermost reading {}), unknown {}; buffers agree {}/{}, unknown {}{}",
            rel.display(),
            c.listing_blocks,
            c.model_blocks,
            c.matched,
            c.tran_agree,
            c.matched - c.tran_unknown,
            c.tran_outermost_agree,
            c.tran_unknown,
            c.buffers_agree,
            c.matched - c.buffers_unknown,
            c.buffers_unknown,
            if recovered { "; parse recovered" } else { "" }
        );
        if !c.disagreements.is_empty() {
            disagreeing += 1;
        }
        for d in &c.disagreements {
            println!("  {}", describe(d));
        }
    }
    println!();
    println!("files compared        {files}");
    println!("without a listing     {without_listing}");
    println!("failed                {failed}");
    println!("files that disagree   {disagreeing}");
    println!(
        "blocks                listing {}, model {}, matched {}, type agrees {}",
        total.listing_blocks, total.model_blocks, total.matched, total.kind_agree
    );
    println!(
        "transaction flag      agree {} of {} known ({} unknown); outermost reading agrees {}",
        total.tran_agree,
        total.matched - total.tran_unknown,
        total.tran_unknown,
        total.tran_outermost_agree
    );
    println!(
        "buffer scopes         agree {} of {} known ({} unknown)",
        total.buffers_agree,
        total.matched - total.buffers_unknown,
        total.buffers_unknown
    );
}

fn read(path: &Path) -> std::io::Result<String> {
    std::fs::read(path).map(|b| String::from_utf8_lossy(&b).into_owned())
}

fn discover(root: &Path) -> Vec<PathBuf> {
    if root.is_file() {
        return vec![root.to_path_buf()];
    }
    let mut files: Vec<PathBuf> = WalkDir::new(root)
        .follow_links(false)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .filter(|e| {
            e.path()
                .extension()
                .and_then(|x| x.to_str())
                .is_some_and(|x| EXTENSIONS.iter().any(|k| k.eq_ignore_ascii_case(x)))
        })
        .map(|e| e.into_path())
        .collect();
    files.sort();
    files
}

fn listing_for(dir: &Path, rel: &Path) -> Option<PathBuf> {
    let mut with_ext = dir.join(rel).into_os_string();
    with_ext.push(".lst");
    [PathBuf::from(with_ext), dir.join(rel).with_extension("lst")]
        .into_iter()
        .find(|p| p.is_file())
}

/// Run the model on one file and compare it with its listing. Returns the
/// comparison and whether the parse was recovered.
fn compare_file(
    pipeline: &LintPipeline,
    schema: Option<&oxabl_schema::Schema>,
    file: &Path,
    rel: &Path,
    source: &str,
    lst_text: &str,
) -> Option<(Comparison, bool)> {
    let expansion = pipeline.with_file(file).expand(source);
    let expanded = expansion.expanded()?;
    let text = expanded.text.as_str();
    let tokens = oxabl_lexer::tokenize(text);
    let program = oxabl_parser::Parser::new(&tokens, text).parse_program();
    let recovered = !program.errors.is_empty();
    let options = ScopeOptions {
        parse_recovered: recovered,
        elided: expanded.elided_offsets(),
        elided_includes: expanded.elided_include_offsets(),
        ..ScopeOptions::default()
    };
    let scopes = oxabl_scope::analyze_tokens(&program.statements, text, &tokens, schema, &options);
    let root_name = rel.display().to_string();
    let root_lines = Lines::new(source);
    let mut include_lines: HashMap<PathBuf, Option<Lines>> = HashMap::new();
    let place = |span: Span| -> Option<(String, u32)> {
        if let Some(s) = expanded.resolve_root_span(span) {
            return Some((root_name.clone(), root_lines.line(s.start)));
        }
        let (path, real, _) = expanded.resolve_include_span(span)?;
        let lines = include_lines
            .entry(path.to_path_buf())
            .or_insert_with(|| read(path).ok().map(|t| Lines::new(&t)))
            .as_ref()?;
        Some((path.display().to_string(), lines.line(real.start)))
    };
    let model = listing::model_blocks(&scopes, place, &root_name);
    Some((
        listing::compare(&listing::parse(lst_text), &model),
        recovered,
    ))
}

fn describe(d: &Disagreement) -> String {
    fn l(b: &ListingBlock) -> String {
        format!(
            "listing {}:{} {} tran={} buffers=[{}]",
            b.file,
            b.line,
            b.kind,
            if b.transaction { "yes" } else { "no" },
            b.buffers.join(",")
        )
    }
    fn m(b: &ModelBlock) -> String {
        format!(
            "model {}:{} {} tran={} buffers={}",
            b.file,
            b.line,
            b.kind,
            match b.transaction {
                Some(true) => "yes",
                Some(false) => "no",
                None => "unknown",
            },
            b.buffers
                .as_ref()
                .map_or("unknown".to_string(), |v| format!("[{}]", v.join(",")))
        )
    }
    match d {
        Disagreement::ListingOnly(b) => format!("only in the listing: {}", l(b)),
        Disagreement::ModelOnly(b) => format!("only in the model: {}", m(b)),
        Disagreement::Kind(a, b) => format!("block type: {} | {}", l(a), m(b)),
        Disagreement::Transaction(a, b) => format!("tran: {} | {}", l(a), m(b)),
        Disagreement::Buffers(a, b) => format!("buffers: {} | {}", l(a), m(b)),
    }
}

struct Lines(Vec<u32>);

impl Lines {
    fn new(src: &str) -> Self {
        let mut v = vec![0];
        v.extend(
            src.bytes()
                .enumerate()
                .filter(|(_, b)| *b == b'\n')
                .map(|(i, _)| i as u32 + 1),
        );
        Lines(v)
    }

    fn line(&self, at: u32) -> u32 {
        self.0.partition_point(|&s| s <= at) as u32
    }
}
