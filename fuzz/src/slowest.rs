//! Times every stage on each file given and prints the slowest. Run it over a
//! corpus to find the inputs that cost the most, which a coverage-guided run
//! does not report:
//!
//!   cargo +nightly build --release --bin slowest
//!   target/release/slowest [-n 10] fuzz/corpus/lint/*

use std::path::PathBuf;
use std::time::Instant;

use oxabl_common::FileId;
use oxabl_formatter::try_format_source;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_preprocessor::Preprocessor;
use oxabl_style::StyleGuide;
use oxabl_workspace::{FileSystem, InMemoryFileSystem};

fn main() {
    let mut top = 10;
    let mut files = Vec::new();
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        if arg == "-n" {
            top = args.next().and_then(|n| n.parse().ok()).expect("-n takes a number");
        } else {
            files.push(arg);
        }
    }

    let config = PipelineConfig::default();
    let style = StyleGuide::default_base();
    let include_paths = vec![PathBuf::from("/inc")];
    let mut rows: Vec<(&str, f64, usize, String)> = Vec::new();
    for file in &files {
        let Ok(bytes) = std::fs::read(file) else { continue };
        let source = String::from_utf8_lossy(&bytes).into_owned();
        let time = |stage: &'static str, rows: &mut Vec<(&str, f64, usize, String)>, run: &dyn Fn()| {
            let start = Instant::now();
            run();
            rows.push((stage, start.elapsed().as_secs_f64() * 1000.0, source.len(), file.clone()));
        };
        time("lex", &mut rows, &|| {
            tokenize(&source);
        });
        time("preprocess+parse", &mut rows, &|| {
            let fs = InMemoryFileSystem::new();
            let pp = Preprocessor::new(&fs as &dyn FileSystem, &include_paths);
            if let Ok(expanded) = pp.process(FileId::new(1), &source) {
                let text = expanded.to_text();
                let tokens = tokenize(&text);
                Parser::new(&tokens, &text).parse_program();
            }
        });
        time("lint", &mut rows, &|| {
            let fs = InMemoryFileSystem::new();
            let run = LintPipeline::new(&config, &fs);
            run.with_file("/work/fuzz.p").run(&source);
        });
        time("format", &mut rows, &|| {
            let _ = try_format_source(&source, &style);
        });
    }
    for stage in ["lex", "preprocess+parse", "lint", "format"] {
        let mut stage_rows: Vec<_> = rows.iter().filter(|r| r.0 == stage).collect();
        stage_rows.sort_by(|a, b| b.1.total_cmp(&a.1));
        println!("== {stage}");
        for (_, ms, size, file) in stage_rows.into_iter().take(top) {
            println!("{ms:10.2} ms {size:8} bytes  {file}");
        }
    }
}
