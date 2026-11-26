//! The generated workspace is deterministic, has the shape its knobs ask for,
//! and every root of it preprocesses and parses with no finding.

use std::collections::BTreeMap;
use std::path::Path;

use oxabl_analyze::DiagnosticSource;
use oxabl_bench::fixtures::in_memory;
use oxabl_bench::generate::{Dist, Hot, Knobs, Preset, generate};
use oxabl_pipeline::{FormatPipeline, LintPipeline};

/// Every feature of the ERP preset, at a size a test can analyse.
fn small_erp() -> Knobs {
    let erp = Preset::Erp.knobs();
    Knobs {
        programs: 120,
        classes: 40,
        lib_includes: 80,
        chains: 2,
        chain_lines: 80,
        chain_programs: 4,
        root_lines: Dist::Quantiles {
            min: 20,
            p50: 60,
            p90: 200,
            p99: 400,
            max: 600,
        },
        hot: erp
            .hot
            .iter()
            .map(|h| Hot {
                lines: (h.lines / 20).max(20),
                ..*h
            })
            .collect(),
        lib_lines: Dist::Quantiles {
            min: 5,
            p50: 30,
            p90: 80,
            p99: 150,
            max: 200,
        },
        preprocessor_every: 12,
        ref_every: 6,
        shared_producers: 6,
        shared_consumers: 4,
        tables: 50,
        ..erp
    }
}

/// The findings of each kind over every root, as `code -> count`, and
/// `format:drift` for each root or include that `oxabl format` would change.
fn findings(knobs: &Knobs) -> BTreeMap<String, usize> {
    let workspace = generate(knobs);
    let dir = Path::new("/gen");
    let (fs, config) = in_memory(&workspace, dir);
    let run = LintPipeline::new(&config, &fs);
    let format = FormatPipeline::new(config.style.clone());
    let mut found = BTreeMap::new();
    for file in workspace
        .files
        .iter()
        .filter(|f| f.root || f.path.ends_with(".i"))
    {
        let outcome = format.format(&file.text);
        assert!(outcome.not_formatted().is_none(), "{} formats", file.path);
        if outcome.changed() {
            if !found.contains_key("format:drift") {
                eprintln!("{}: drifts", file.path);
            }
            *found.entry("format:drift".to_string()).or_insert(0) += 1;
        }
    }
    for root in workspace.roots() {
        let path = dir.join(&root.path);
        let result = run.with_file(&path).run(&root.text);
        assert!(
            result.failure().is_none(),
            "{}: {:?}",
            root.path,
            result.failure()
        );
        // The findings in an include, as the root's run folds them
        // (`include_origin`, which `check --include-findings` reports).
        for d in result.include_origin() {
            let code = format!("include:{}:{:?}", d.source.as_str(), d.diagnostic.code);
            *found.entry(code).or_insert(0) += 1;
        }
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

/// FNV-1a over the path and the text of every file, in order.
fn fingerprint(knobs: &Knobs) -> u64 {
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    for file in &generate(knobs).files {
        for byte in file
            .path
            .bytes()
            .chain([0])
            .chain(file.text.bytes())
            .chain([0])
        {
            hash = (hash ^ u64::from(byte)).wrapping_mul(0x0100_0000_01b3);
        }
    }
    hash
}

/// The CodSpeed benches read the `ci` workspace and compare by name, so a
/// change of one byte of it reads as a change of the code. A change of the
/// generator that must change this workspace updates the number on purpose.
#[test]
fn the_ci_workspace_does_not_change() {
    assert_eq!(
        fingerprint(&Preset::Ci.knobs()),
        7_321_140_975_358_348_576,
        "the ci workspace changed"
    );
}

#[test]
fn the_ci_preset_has_its_shape() {
    let s = generate(&Preset::Ci.knobs()).stats();
    assert_eq!(s.roots, 300);
    assert_eq!(s.max_depth, 4);
    assert!(s.top_fan_in[0] > 200, "{:?}", s.top_fan_in);
    assert!(s.max_fan_out >= 30, "{}", s.max_fan_out);
}

/// The numbers measured on a large real workspace (counts only), with the
/// band that the preset must stay inside.
#[test]
fn the_erp_preset_has_the_shape_of_a_large_real_workspace() {
    let s = generate(&Preset::Erp.knobs()).stats();
    let within = |what: &str, got: usize, want: usize, percent: usize| {
        let (lo, hi) = (want * (100 - percent) / 100, want * (100 + percent) / 100);
        assert!(
            (lo..=hi).contains(&got),
            "{what}: {got}, want {want} +-{percent}%"
        );
    };
    assert_eq!(s.roots, 10_386);
    within("files", s.files, 14_800, 5);
    within("bytes", s.bytes, 212_000_000, 8);
    within("lines", s.lines, 5_380_000, 8);
    within("lines per root, p50", s.root_lines.p50, 178, 15);
    within("lines per root, p90", s.root_lines.p90, 718, 15);
    within("lines per root, p99", s.root_lines.p99, 3_151, 15);
    within("direct includes, p90", s.direct_includes.p90, 7, 30);
    within("direct includes, p99", s.direct_includes.p99, 11, 30);
    within("closure, p50", s.closure.p50, 24, 30);
    within("expanded lines, mean", s.expanded_lines.mean, 3_418, 10);
    assert!(s.depth.max <= 6 && s.depth.p99 >= 5, "{:?}", s.depth);
    within("largest direct fan-in", s.top_direct_fan_in[0], 4_925, 15);
    assert!(
        s.direct_fan_in.p50 <= 2 && s.direct_fan_in.p90 <= 8,
        "{s:?}"
    );
    let c = &s.counts;
    within("&SCOPED-DEFINE", c.scoped_define, 4_231, 20);
    within("&IF", c.if_directive, 2_901, 15);
    within("{&name}", c.name_refs, 40_428, 15);
    within("temp-tables", c.temp_tables, 18_886, 15);
    within("datasets", c.datasets, 233, 25);
    within("classes", c.classes, 2_176, 1);
    within("NEW SHARED", c.new_shared, 2_000, 15);
    within("FIND", c.find, 93_653, 15);
    within("FOR EACH", c.for_each, 30_471, 15);
    within("RUN", c.run, 71_900, 15);
    within("NEW class", c.new_object, 8_525, 20);
    within("methods", c.methods, 10_752, 15);
}

#[test]
fn the_launchers_are_nothing_but_includes() {
    let workspace = generate(&Preset::Erp.knobs());
    let launchers: Vec<_> = workspace
        .roots()
        .filter(|f| f.text.starts_with("/* Launcher"))
        .collect();
    assert!(launchers.len() > 200, "{} launchers", launchers.len());
    for launcher in launchers.iter().take(50) {
        assert!(
            launcher
                .text
                .lines()
                .all(|l| l.starts_with("/*") || l.starts_with('{')),
            "{}: {}",
            launcher.path,
            launcher.text
        );
        assert!(!launcher.includes.is_empty(), "{}", launcher.path);
    }
    let reads: usize = launchers.iter().map(|f| f.includes.len()).sum();
    assert!(reads / launchers.len() >= 8, "{} reads", reads);
}

#[test]
fn every_shared_definition_has_a_producer() {
    let workspace = generate(&small_erp());
    let defined = |access: &str| -> std::collections::BTreeSet<String> {
        workspace
            .roots()
            .flat_map(|f| f.text.lines())
            .filter_map(|l| l.strip_prefix(&format!("DEFINE {access} VARIABLE ")))
            .map(|l| l.split(' ').next().unwrap().to_string())
            .collect()
    };
    let produced = defined("NEW SHARED");
    let consumed = defined("SHARED");
    assert!(!produced.is_empty() && !consumed.is_empty());
    let missing: Vec<_> = consumed.difference(&produced).collect();
    assert!(missing.is_empty(), "no producer of {missing:?}");
}

#[test]
fn no_root_reads_an_include_twice() {
    let workspace = generate(&small_erp());
    for root in workspace.roots() {
        let mut seen = std::collections::BTreeSet::new();
        let mut stack = root.includes.clone();
        while let Some(id) = stack.pop() {
            assert!(
                seen.insert(id),
                "{} reads {} twice",
                root.path,
                workspace.files[id].path
            );
            stack.extend(&workspace.files[id].includes);
        }
    }
}

/// The hot includes of `knobs` that the roots read, counted once for each root.
fn hot_reads(knobs: &Knobs) -> usize {
    let workspace = generate(knobs);
    workspace
        .roots()
        .map(|root| {
            root.includes
                .iter()
                .filter(|&&id| workspace.files[id].path.starts_with("inc/hot/"))
                .count()
        })
        .sum()
}

#[test]
fn each_root_reports_the_findings_of_the_hot_includes_it_reads() {
    let knobs = Knobs {
        include_findings: 100,
        ..Preset::Ci.knobs()
    };
    let reads = hot_reads(&knobs);
    assert!(reads > 400, "{reads} reads of a hot include");
    let found = findings(&knobs);
    let expected = BTreeMap::from([(
        "include:lint:DiagnosticCode(\"LINT0002\")".to_string(),
        100 * reads,
    )]);
    assert_eq!(found, expected);
}

#[test]
fn every_root_parses_with_no_finding_and_no_drift() {
    for (name, knobs) in [("ci", Preset::Ci.knobs()), ("small erp", small_erp())] {
        let found = findings(&knobs);
        assert!(found.is_empty(), "{name}: {found:?}");
    }
}
