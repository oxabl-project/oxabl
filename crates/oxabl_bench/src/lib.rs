//! End-to-end and memory benchmarks of oxabl, and a generator of large
//! synthetic ABL workspaces. Not published.
//!
//! - [`generate`]: a deterministic synthetic workspace (`gen-workspace` is its
//!   command line).
//! - [`fixtures`]: the inputs of the benches.
//! - [`alloc`]: a counting global allocator.
//! - [`bench`]: a Criterion bench that also reports the allocations of one
//!   iteration.

pub mod alloc;
pub mod fixtures;
pub mod generate;

use std::io::Write;

use criterion::BenchmarkGroup;
use criterion::measurement::WallTime;

/// The variable that names a file for the memory rows of the benches. Each row
/// is one JSON object on its own line, appended.
pub const MEMORY_FILE_VARIABLE: &str = "OXABL_BENCH_MEMORY";

/// Bench `f` as `<group>/<name>`, after one run of it under
/// [`alloc::measure`]. The allocations, bytes and peak of that run go to
/// stderr, and to the file that [`MEMORY_FILE_VARIABLE`] names.
///
/// The binary must install [`alloc::Counting`] as its global allocator.
pub fn bench<T>(
    group: &mut BenchmarkGroup<'_, WallTime>,
    group_name: &str,
    name: &str,
    mut f: impl FnMut() -> T,
) {
    let (value, usage) = alloc::measure(&mut f);
    drop(value);
    report(&format!("{group_name}/{name}"), usage);
    group.bench_function(name, |b| b.iter(|| std::hint::black_box(f())));
}

/// Print one memory row, and append it to the file of
/// [`MEMORY_FILE_VARIABLE`] when it is set.
pub fn report(id: &str, usage: alloc::Usage) {
    eprintln!(
        "memory {id}: {} allocations, {} bytes requested, peak {} bytes",
        usage.allocations, usage.bytes, usage.peak
    );
    if let Some(path) = std::env::var_os(MEMORY_FILE_VARIABLE) {
        let row = format!(
            "{{\"bench\":\"{id}\",\"allocations\":{},\"bytes\":{},\"peak\":{}}}\n",
            usage.allocations, usage.bytes, usage.peak
        );
        let written = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&path)
            .and_then(|mut file| file.write_all(row.as_bytes()));
        if let Err(e) = written {
            panic!(
                "cannot write {}: {e}",
                std::path::Path::new(&path).display()
            );
        }
    }
}
