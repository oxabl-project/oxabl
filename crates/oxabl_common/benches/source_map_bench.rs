use criterion::{Criterion, Throughput, black_box, criterion_group, criterion_main};
use oxabl_common::SourceMap;
use std::fs;
use std::path::Path;

/// Load the large fixture file from workspace resources/.
fn load_fixture() -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("resources")
        .join("bench_keywords.abl");
    fs::read_to_string(path).unwrap()
}

fn source_map_benchmarks(c: &mut Criterion) {
    let source = load_fixture();
    let mut group = c.benchmark_group("source_map");

    // Benchmark SourceMap construction (single-pass newline scan)
    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function("construction", |b| {
        b.iter(|| SourceMap::new(black_box(&source)))
    });

    // Benchmark lookups with pre-built map at various offsets
    let map = SourceMap::new(&source);
    let len = source.len();
    let offsets: Vec<usize> = vec![
        0,                     // beginning of file
        len / 4,               // 25%
        len / 2,               // middle
        len * 3 / 4,           // 75%
        len.saturating_sub(1), // end of file
    ];
    group.throughput(Throughput::Elements(offsets.len() as u64));
    group.bench_function("lookup", |b| {
        b.iter(|| {
            for &offset in &offsets {
                black_box(map.lookup(offset));
            }
        })
    });

    group.finish();
}

criterion_group!(benches, source_map_benchmarks);
criterion_main!(benches);
