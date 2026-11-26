//! The two parts of a references search that grow with the size of the
//! workspace and with the number of lenses of a file, on a synthetic
//! workspace in memory: a class `app.Big` with [`MEMBERS`] methods, and
//! [`UNITS`] programs that name it.
//!
//! - `references/search_every_lens_of_a_class`: [`Referent::search`] for the
//!   class and each of its members over the workspace graph, as one batch of
//!   lenses asks it. Each search finds every program, so the cost must grow
//!   in step with the number of files, not with its square.
//! - `references/sites_of_a_unit_for_every_lens`: [`Navigation::references_with`]
//!   for the class and each member in one program that calls each member
//!   [`REPEATS`] times, with
//!   one [`ReferenceMemo`], as a search analyses a file for every lens of a
//!   batch. The sites of the program are resolved once, not once for each
//!   referent.

use std::fmt::Write;
use std::hint::black_box;
use std::path::{Path, PathBuf};

use criterion::{Criterion, criterion_group, criterion_main};
use oxabl_pipeline::{
    LintPipeline, Navigation, PipelineConfig, ReferenceMemo, Referent, ReverseGraph,
};
use oxabl_workspace::InMemoryFileSystem;

/// The methods of the class: about the lenses of a large class.
const MEMBERS: usize = 70;

/// The programs that name the class: about the most that a search analyses.
const UNITS: usize = 2_000;

const CLASS: &str = "/proj/app/Big.cls";

fn class_file() -> String {
    let mut s = String::from("CLASS app.Big:\n");
    for k in 0..MEMBERS {
        writeln!(s, "    METHOD PUBLIC INTEGER m{k}(INPUT a AS INTEGER):").unwrap();
        writeln!(s, "        RETURN a + {k}.").unwrap();
        writeln!(s, "    END METHOD.").unwrap();
    }
    s.push_str("END CLASS.\n");
    s
}

/// How many times the program of `references/sites_of_a_unit_for_every_lens`
/// calls each method: a few hundred sites, as a large program has.
const REPEATS: usize = 8;

/// A program that names the class and calls `calls` of its methods
/// `repeats` times.
fn caller_file(calls: usize, repeats: usize) -> String {
    let mut s = String::from(
        "DEFINE VARIABLE o AS CLASS app.Big NO-UNDO.\n\
         DEFINE VARIABLE n AS INTEGER NO-UNDO.\n\
         o = NEW app.Big().\n",
    );
    for _ in 0..repeats {
        for k in 0..calls {
            writeln!(s, "n = o:m{k}(n).").unwrap();
        }
    }
    s
}

struct Workspace {
    fs: InMemoryFileSystem,
    config: PipelineConfig,
    units: Vec<PathBuf>,
}

fn workspace() -> Workspace {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from(CLASS), class_file());
    let mut units = vec![PathBuf::from(CLASS)];
    for n in 0..UNITS {
        let path = PathBuf::from(format!("/proj/p{n}.p"));
        // The first program calls every method; the others call a few.
        let text = if n == 0 {
            caller_file(MEMBERS, REPEATS)
        } else {
            caller_file(3, 1)
        };
        fs.insert(path.clone(), text);
        units.push(path);
    }
    let config = PipelineConfig {
        include_paths: vec![PathBuf::from("/proj")],
        ..PipelineConfig::default()
    };
    Workspace { fs, config, units }
}

/// The class and each of its methods, as the lenses of the class name them,
/// from the analysis of the class file.
fn class_referents(ws: &Workspace) -> Vec<Referent> {
    let source = class_file();
    let run = LintPipeline::new(&ws.config, &ws.fs).with_known_files(&ws.units);
    let handle = run.with_file(CLASS);
    let expansion = handle.expand(&source);
    let file_of = |_| None;
    let navigation = handle
        .navigation(&expansion, &file_of)
        .expect("the class preprocesses");
    let class = Path::new(CLASS);
    let mut offsets = vec![source.find("Big:").unwrap()];
    offsets.extend((0..MEMBERS).map(|k| source.find(&format!(" m{k}(")).unwrap() + 1));
    offsets
        .into_iter()
        .map(|at| {
            navigation
                .referent_at(class, u32::try_from(at).unwrap())
                .expect("each lens names a referent")
        })
        .collect()
}

fn bench_search(c: &mut Criterion) {
    let ws = workspace();
    let referents = class_referents(&ws);
    let run = LintPipeline::new(&ws.config, &ws.fs).with_known_files(&ws.units);
    let graph = ReverseGraph::build(&run, &ws.units);
    let class = Path::new(CLASS);
    let found = referents[0].search(&graph, class, usize::MAX);
    assert_eq!(found.units.len(), UNITS, "every program names the class");
    let found = referents[1].search(&graph, class, usize::MAX);
    assert_eq!(found.units.len(), UNITS, "every program can call a method");

    let mut group = c.benchmark_group("references");
    group.sample_size(10);
    group.bench_function("search_every_lens_of_a_class", |b| {
        b.iter(|| {
            for referent in &referents {
                black_box(referent.search(&graph, class, usize::MAX));
            }
        });
    });
    group.finish();
}

/// The uses that a program gives for each referent, with one memo.
fn uses_of_each(navigation: &Navigation<'_, '_>, referents: &[Referent]) -> Vec<usize> {
    let mut memo = ReferenceMemo::default();
    referents
        .iter()
        .map(|referent| {
            navigation
                .references_with(referent, &mut memo)
                .occurrences
                .len()
        })
        .collect()
}

fn bench_sites(c: &mut Criterion) {
    let ws = workspace();
    let referents = class_referents(&ws);
    let path = PathBuf::from("/proj/p0.p");
    let source = caller_file(MEMBERS, REPEATS);
    let run = LintPipeline::new(&ws.config, &ws.fs).with_known_files(&ws.units);
    let handle = run.with_file(&path);
    let expansion = handle.expand(&source);
    let file_of = |_| None;
    let navigation = handle
        .navigation(&expansion, &file_of)
        .expect("the program preprocesses");
    let uses = uses_of_each(&navigation, &referents);
    // The class twice (the variable and `NEW`), each method at each call.
    assert_eq!(uses[0], 2, "{uses:?}");
    assert!(uses[1..].iter().all(|&n| n == REPEATS), "{uses:?}");

    let mut group = c.benchmark_group("references");
    group.bench_function("sites_of_a_unit_for_every_lens", |b| {
        b.iter(|| black_box(uses_of_each(&navigation, &referents)));
    });
    group.finish();
}

criterion_group!(benches, bench_search, bench_sites);
criterion_main!(benches);
