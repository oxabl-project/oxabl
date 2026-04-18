use std::path::PathBuf;

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use oxabl_common::FileId;
use oxabl_schema::{SchemaLoader, parse_df};
use oxabl_workspace::InMemoryFileSystem;

const SP2K: &str = include_str!("../fixtures/sp2k.df");

fn parse_sp2k(c: &mut Criterion) {
    c.bench_function("parse_df:sp2k", |b| {
        b.iter(|| {
            let out = parse_df(black_box(SP2K), FileId::new(1));
            black_box(out);
        });
    });
}

fn load_sp2k(c: &mut Criterion) {
    c.bench_function("load_files:sp2k", |b| {
        b.iter(|| {
            let mut fs = InMemoryFileSystem::new();
            fs.insert(PathBuf::from("/sp2k.df"), SP2K.to_string());
            let (schema, diags) = SchemaLoader::load_files(&[PathBuf::from("/sp2k.df")], &fs);
            black_box(schema);
            black_box(diags);
        });
    });
}

fn load_large_merged(c: &mut Criterion) {
    // Synthesise a ~5 MB merged fixture by concatenating sp2k several times
    // with distinct table names. Approximates the "5 MB merged .df" target
    // from the plan's Phase 2 section.
    let mut corpus = String::new();
    let target_size = 5 * 1024 * 1024;
    let mut i = 0;
    while corpus.len() < target_size {
        for line in SP2K.lines() {
            // Namespace table names so merges don't collapse into one table.
            let rewritten = line
                .replace("\"Benefits\"", &format!("\"Benefits{i}\""))
                .replace("\"Customer\"", &format!("\"Customer{i}\""))
                .replace("\"Order\"", &format!("\"Order{i}\""))
                .replace("\"Order-Line\"", &format!("\"Order-Line{i}\""))
                .replace("\"Warehouse\"", &format!("\"Warehouse{i}\""));
            corpus.push_str(&rewritten);
            corpus.push('\n');
        }
        i += 1;
    }
    c.bench_function("load_files:5mb_merged", |b| {
        b.iter(|| {
            let out = parse_df(black_box(&corpus), FileId::new(1));
            black_box(out);
        });
    });
}

criterion_group!(benches, parse_sp2k, load_sp2k, load_large_merged);
criterion_main!(benches);
