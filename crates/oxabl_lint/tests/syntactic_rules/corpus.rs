//! Corpus runner for the four rules. Ignored by default.
//!
//! ```text
//! OXABL_CORPUS=/path/to/abl/tree \
//!     cargo test --release -p oxabl_lint --test syntactic_rules -- --ignored --nocapture
//! ```
//!
//! Every `.p`, `.w` and `.cls` file under the root is preprocessed (the root is
//! the only include path), parsed, analyzed and run through each rule. The
//! report is counts per rule, each finding counted once at its origin: a
//! definition in an include file is one finding however many files include it.
//! Set `OXABL_CORPUS_SAMPLE=<n>` to also print about
//! `n` evenly spaced findings per rule, with the offending line, for checking by
//! hand. Nothing is written to disk.

use std::collections::{BTreeMap, HashSet};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::{Path, PathBuf};
use std::time::Instant;

use oxabl_common::FileId;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_preprocessor::Preprocessor;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};
use oxabl_workspace::RealFileSystem;

use super::RULES;

fn collect(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect(&path, out);
        } else if path
            .extension()
            .and_then(|e| e.to_str())
            .is_some_and(|e| ["p", "w", "cls"].iter().any(|x| e.eq_ignore_ascii_case(x)))
        {
            out.push(path);
        }
    }
}

struct Finding {
    path: PathBuf,
    line: usize,
    text: String,
    message: String,
}

#[test]
#[ignore = "needs OXABL_CORPUS"]
fn corpus_counts() {
    let Some(root) = std::env::var_os("OXABL_CORPUS") else {
        return;
    };
    let root = PathBuf::from(root);
    let sample: usize = std::env::var("OXABL_CORPUS_SAMPLE")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);

    let mut files = Vec::new();
    collect(&root, &mut files);
    files.sort();

    let fs = RealFileSystem;
    let include_paths = [root.clone()];
    let preprocessor = Preprocessor::new(&fs, &include_paths);
    let schema = Schema::empty();

    let mut seen: Vec<HashSet<u64>> = RULES.iter().map(|_| HashSet::new()).collect();
    let mut by_message: Vec<BTreeMap<String, usize>> =
        RULES.iter().map(|_| BTreeMap::new()).collect();
    let mut counts = [0usize; RULES.len()];
    let mut files_hit = [0usize; RULES.len()];
    let mut findings: Vec<Vec<Finding>> = RULES.iter().map(|_| Vec::new()).collect();
    let (mut skipped, mut recovered) = (0usize, 0usize);
    let started = Instant::now();

    for path in &files {
        let Ok(bytes) = std::fs::read(path) else {
            skipped += 1;
            continue;
        };
        let source = String::from_utf8_lossy(&bytes);
        let Ok(expanded) = preprocessor.process(FileId::new(1), &source) else {
            skipped += 1;
            continue;
        };
        let text = expanded.to_text();
        let tokens = tokenize(&text);
        let program = Parser::new(&tokens, &text).parse_program();
        if !program.errors.is_empty() {
            recovered += 1;
        }
        let ctx = AnalysisContext::new(FileId::new(1), &text, &schema);
        let sem = analyze_file(&program.statements, &ctx);
        for (i, (_, rule)) in RULES.iter().enumerate() {
            let mut diags = rule(&program.statements, &sem, &ctx);
            // Include ids are per run, so an include finding is keyed by its
            // offset in the include plus the text it points at.
            diags.retain(|d| {
                let start = d.span.span.start as usize;
                let origin = expanded.resolve(d.span.span.start);
                let mut h = DefaultHasher::new();
                if origin.file == FileId::new(1) {
                    path.hash(&mut h);
                }
                origin.span.start.hash(&mut h);
                d.message.hash(&mut h);
                text.get(start..(start + 120).min(text.len()))
                    .unwrap_or("")
                    .hash(&mut h);
                seen[i].insert(h.finish())
            });
            if diags.is_empty() {
                continue;
            }
            for d in &diags {
                // The message with its quoted name dropped: a breakdown by form.
                let form = d
                    .message
                    .split('`')
                    .enumerate()
                    .filter(|(n, _)| n % 2 == 0)
                    .map(|(_, p)| p)
                    .collect::<String>();
                *by_message[i].entry(form).or_default() += 1;
            }
            counts[i] += diags.len();
            files_hit[i] += 1;
            if sample > 0 {
                for d in diags {
                    let at = (d.span.span.start as usize).min(text.len());
                    let line = text[..at].matches('\n').count() + 1;
                    findings[i].push(Finding {
                        path: path.strip_prefix(&root).unwrap_or(path).to_path_buf(),
                        line,
                        text: text.lines().nth(line - 1).unwrap_or("").trim().to_string(),
                        message: d.message,
                    });
                }
            }
        }
    }

    println!(
        "{} files, {skipped} unreadable, {recovered} with parse errors, {:.1?}",
        files.len(),
        started.elapsed()
    );
    for (i, (code, _)) in RULES.iter().enumerate() {
        println!("{code}: {} findings in {} files", counts[i], files_hit[i]);
    }
    for (i, (code, _)) in RULES.iter().enumerate() {
        for (form, n) in &by_message[i] {
            println!("{code}   {n:>6}  {form}");
        }
    }
    for (i, (code, _)) in RULES.iter().enumerate() {
        let all = &findings[i];
        if all.is_empty() {
            continue;
        }
        println!("--- {code} sample ---");
        let step = (all.len() / sample.max(1)).max(1);
        for f in all.iter().step_by(step).take(sample) {
            println!(
                "{}:{}: {} | {}",
                f.path.display(),
                f.line,
                f.message,
                f.text
            );
        }
    }
}
