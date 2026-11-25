//! The generated workspace is deterministic, has the shape its knobs ask for,
//! and every root of it preprocesses and parses with no finding.

use std::collections::BTreeMap;
use std::path::Path;

use oxabl_analyze::DiagnosticSource;
use oxabl_bench::fixtures::in_memory;
use oxabl_bench::generate::{Knobs, Preset, generate};
use oxabl_pipeline::LintPipeline;

/// Every feature of the ERP preset, at a size a test can analyse.
fn small_erp() -> Knobs {
    Knobs {
        programs: 120,
        classes: 40,
        lib_includes: 80,
        fan_out_programs: 2,
        fan_out: 40,
        chains: 2,
        chain_lines: 80,
        chain_programs: 4,
        root_lines: (30, 300),
        hot_lines: 60,
        lib_lines: (20, 60),
        tables: 50,
        ..Preset::Erp.knobs()
    }
}

/// The findings of each kind over every root, as `code -> count`.
fn findings(knobs: &Knobs) -> BTreeMap<String, usize> {
    let workspace = generate(knobs);
    let dir = Path::new("/gen");
    let (fs, config) = in_memory(&workspace, dir);
    let run = LintPipeline::new(&config, &fs);
    let mut found = BTreeMap::new();
    for root in workspace.roots() {
        let path = dir.join(&root.path);
        let result = run.with_file(&path).run(&root.text);
        assert!(
            result.failure().is_none(),
            "{}: {:?}",
            root.path,
            result.failure()
        );
        for source in [
            DiagnosticSource::Parse,
            DiagnosticSource::Preproc,
            DiagnosticSource::Semantic,
            DiagnosticSource::Lint,
        ] {
            for d in result.by_source(source) {
                let code = format!("{}:{:?}", source.as_str(), d.diagnostic.code);
                if !found.contains_key(&code) {
                    eprintln!("{}: {code} {}", root.path, d.diagnostic.message);
                }
                *found.entry(code).or_insert(0) += 1;
            }
        }
    }
    found
}

#[test]
fn the_same_knobs_give_the_same_files() {
    let knobs = Preset::Ci.knobs();
    assert_eq!(generate(&knobs), generate(&knobs));
    let other = Knobs {
        seed: knobs.seed + 1,
        ..knobs.clone()
    };
    assert_ne!(generate(&knobs), generate(&other));
}

#[test]
fn the_ci_preset_has_its_shape() {
    let s = generate(&Preset::Ci.knobs()).stats();
    assert_eq!(s.roots, 300);
    assert_eq!(s.max_depth, 4);
    assert!(s.top_fan_in[0] > 200, "{:?}", s.top_fan_in);
    assert!(s.max_fan_out >= 30, "{}", s.max_fan_out);
}

#[test]
fn the_erp_preset_has_its_shape() {
    let s = generate(&Preset::Erp.knobs()).stats();
    assert!((13_000..=15_000).contains(&s.files), "{} files", s.files);
    assert_eq!(s.roots, 7_000);
    assert!(s.includes_over_1000_roots >= 5, "{s:?}");
    assert!(s.top_fan_in[0] > 3_000, "{s:?}");
    assert!(s.max_fan_out >= 300, "{s:?}");
    assert_eq!(s.max_depth, 10);
    assert!(s.roots_over_30000_lines >= 100, "{s:?}");
}

#[test]
fn every_root_parses_with_no_finding() {
    for (name, knobs) in [("ci", Preset::Ci.knobs()), ("small erp", small_erp())] {
        let found = findings(&knobs);
        assert!(found.is_empty(), "{name}: {found:?}");
    }
}
