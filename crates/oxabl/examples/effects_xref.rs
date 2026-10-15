//! effects_xref — compare the index `oxabl_effects` expects for each search
//! with the OpenEdge compiler's `COMPILE ... XREF` output: the plan's M4 exit
//! gate ("DB002 agrees with XREF `WHOLE-INDEX` entries").
//!
//! For every `.p`, `.w` and `.cls` file under a source tree, read the XREF file
//! the compiler wrote for it, keep its `SEARCH` rows (`oxabl_effects::xref`),
//! run `oxabl_effects` on the file (expanded as `oxabl check` expands it, with
//! the tree's `oxabl.toml` and schema), and compare place by place (file,
//! line, table): the index name and `WHOLE-INDEX`. Every disagreement is
//! printed with both sides; a search the model answers Unknown is counted
//! apart, not as a disagreement.
//!
//! `scripts/gen-xref.sh` writes the XREF files on a licensed install.
//!
//! # Usage
//! ```text
//! OXABL_EFFECTS_XREF_SRC=<source dir> [OXABL_EFFECTS_XREF_DIR=<xref dir>] \
//!   [OXABL_EFFECTS_XREF_LINE=statement|phrase] \
//!   cargo run -p oxabl --release --example effects_xref
//! ```
//! The XREF file of `<src>/a/b.p` is `<xref dir>/a/b.p.xref`, else
//! `<xref dir>/a/b.xref`; the XREF dir defaults to the source dir. A source
//! without one is skipped and counted. A search is placed on the line its
//! statement starts on (`statement`, the default), or the line of its buffer
//! name (`phrase`): which line XREF reports for a multi-line statement is not
//! known on this host, and the harness can try both.

use std::collections::HashMap;
use std::panic::AssertUnwindSafe;
use std::path::{Path, PathBuf};

use oxabl_ast::Span;
use oxabl_effects::EffectOptions;
use oxabl_effects::xref::{self, Comparison, ModelSearch};
use oxabl_index::calls::FileCalls;
use oxabl_pipeline::{ConfigOverrides, LintPipeline, PipelineConfig};
use oxabl_scope::ScopeOptions;
use oxabl_workspace::RealFileSystem;
use walkdir::WalkDir;

const EXTENSIONS: &[&str] = &["p", "w", "cls"];

fn main() {
    let Some(src) = std::env::var_os("OXABL_EFFECTS_XREF_SRC").map(PathBuf::from) else {
        eprintln!("set OXABL_EFFECTS_XREF_SRC to the source directory");
        std::process::exit(2);
    };
    let src = std::path::absolute(&src).unwrap_or(src);
    let xref_dir = std::env::var_os("OXABL_EFFECTS_XREF_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| src.clone());
    let by_phrase = std::env::var("OXABL_EFFECTS_XREF_LINE").is_ok_and(|v| v == "phrase");
    let (config, warnings) = PipelineConfig::resolve(&src, &ConfigOverrides::default());
    for w in &warnings {
        eprintln!("warning: {w}");
    }
    let schema = config.schema_loaded.then_some(&config.schema);
    let fs = RealFileSystem;
    let pipeline = LintPipeline::new(&config, &fs).with_preprocess(true);

    let mut total = Comparison::default();
    let (mut files, mut without, mut failed, mut disagreeing) = (0, 0, 0, 0);
    for file in discover(&src) {
        let rel = file.strip_prefix(&src).unwrap_or(&file).to_path_buf();
        let Some(xref_path) = xref_for(&xref_dir, &rel) else {
            without += 1;
            continue;
        };
        files += 1;
        let (Ok(source), Ok(xref_text)) = (read(&file), read(&xref_path)) else {
            failed += 1;
            eprintln!("{}: unreadable", rel.display());
            continue;
        };
        let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
            model_searches(&pipeline, schema, &file, &rel, &source, by_phrase)
        }));
        let Ok(Some((model, recovered))) = result else {
            failed += 1;
            eprintln!("{}: could not be analysed", rel.display());
            continue;
        };
        let rows = xref::parse(&xref_text);
        let c = xref::compare(&model, &xref::searches(&rows));
        total.add(&c);
        println!(
            "{}: places {} matched {} agree {} whole-index agree {}, unknown {}, model only {}, xref only {}{}",
            rel.display(),
            c.places,
            c.matched,
            c.agree,
            c.whole_agree,
            c.unknown,
            c.model_only,
            c.xref_only,
            if recovered { "; parse recovered" } else { "" }
        );
        if !c.disagreements.is_empty() {
            disagreeing += 1;
        }
        for d in &c.disagreements {
            println!(
                "  {}:{} {}: model [{}] | xref [{}]{}",
                d.file,
                d.line,
                d.table,
                d.model.join(", "),
                d.xref.join(", "),
                if d.whole_agrees {
                    " (whole-index agrees)"
                } else {
                    ""
                }
            );
        }
    }
    let known = total.matched - total.unknown;
    println!();
    println!("files compared        {files}");
    println!("without an XREF file  {without}");
    println!("failed                {failed}");
    println!("files that disagree   {disagreeing}");
    println!(
        "places                {}, matched {}, model unknown {}",
        total.places, total.matched, total.unknown
    );
    println!("index agrees          {} of {known} known", total.agree);
    println!(
        "WHOLE-INDEX agrees    {} of {known} known; xref only {}, model only {}",
        total.whole_agree, total.whole_missed, total.whole_extra
    );
    println!(
        "unmatched             model only {} (rowid {}), xref only {}",
        total.model_only, total.rowid, total.xref_only
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

fn xref_for(dir: &Path, rel: &Path) -> Option<PathBuf> {
    let mut with_ext = dir.join(rel).into_os_string();
    with_ext.push(".xref");
    [
        PathBuf::from(with_ext),
        dir.join(rel).with_extension("xref"),
    ]
    .into_iter()
    .find(|p| p.is_file())
}

/// Run the model on one file: every search, placed in the file (or include)
/// and line it comes from. Returns the searches and whether the parse was
/// recovered.
fn model_searches(
    pipeline: &LintPipeline,
    schema: Option<&oxabl_schema::Schema>,
    file: &Path,
    rel: &Path,
    source: &str,
    by_phrase: bool,
) -> Option<(Vec<ModelSearch>, bool)> {
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
    let flow = oxabl_flow::build_file_with(&program.statements, text, &options.flow);
    let scopes =
        oxabl_scope::analyze_flow(&program.statements, text, &tokens, &flow, schema, &options);
    let calls = FileCalls::extract(&program.statements, text, &tokens);
    let effects = oxabl_effects::analyze_file(
        &program.statements,
        text,
        &tokens,
        &flow,
        &scopes,
        &calls,
        schema,
        &EffectOptions {
            elided: expanded.elided_offsets(),
        },
    );
    let root_name = rel.display().to_string();
    let root_lines = Lines::new(source);
    let mut include_lines: HashMap<PathBuf, Option<Lines>> = HashMap::new();
    let mut place = |span: Span| -> Option<(String, u32)> {
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
    let mut out = Vec::new();
    for e in effects.effects() {
        let Some(access) = &e.access else {
            continue;
        };
        let at = if by_phrase {
            e.at
        } else {
            Span {
                start: e.statement.start,
                end: e.statement.start + 1,
            }
        };
        let Some((file, line)) = place(at) else {
            continue;
        };
        out.push(ModelSearch {
            file,
            line,
            table: e.table.name().to_string(),
            access: access.clone(),
        });
    }
    Some((out, recovered))
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
