//! The cost of each transaction rule over one synthetic program set.
//!
//! Every case lints each root of the set with one of TX001 to TX004 on, and
//! `none` with none of them on. TX004 reads the workspace effect summaries of
//! its callees, so its case builds them too, on the calling thread. The count
//! of a case is then the work itself, and the gap to `none` is the cost of the
//! rule. A TX004 that costs far more than TX001 to TX003 shows as a gap
//! between the cases.

use std::hint::black_box;
use std::path::PathBuf;
use std::sync::Arc;

use criterion::{Criterion, criterion_group, criterion_main};
use oxabl_common::{LintSeverityMap, Severity};
use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_schema::test_support::customer_schema;
use oxabl_workspace::InMemoryFileSystem;

const ROOTS: usize = 48;
const MIDDLE: usize = 8;
const LEAVES: usize = 4;

const COMMON: &str = r#"
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE total AS DECIMAL NO-UNDO.
DEFINE VARIABLE answer AS CHARACTER NO-UNDO.
"#;

/// A root with a finding for each of the four rules: an update in a procedure
/// with no `TRANSACTION` (TX001), an update in the main block (TX002), a wait
/// for the user inside a transaction (TX003) and calls inside it (TX004).
fn root(k: usize) -> String {
    format!(
        "{{inc/common.i}}\n\
         DO TRANSACTION:\n\
             FIND FIRST Customer WHERE Customer.CustNum = {k} EXCLUSIVE-LOCK NO-ERROR.\n\
             IF AVAILABLE Customer THEN Customer.Name = \"r{k}\".\n\
             UPDATE answer.\n\
             RUN mid/m{}.p.\n\
             RUN mid/m{}.p.\n\
         END.\n\
         FOR EACH Customer EXCLUSIVE-LOCK WHERE Customer.CustNum > {k}:\n\
             Customer.Name = \"x\".\n\
         END.\n\
         PROCEDURE update-{k}:\n\
             FIND FIRST Customer WHERE Customer.CustNum = {k} EXCLUSIVE-LOCK NO-ERROR.\n\
             IF AVAILABLE Customer THEN Customer.Name = \"u{k}\".\n\
         END PROCEDURE.\n\
         PROCEDURE local-{k}:\n\
             DO i = 1 TO 3:\n\
                 RUN show IN THIS-PROCEDURE.\n\
             END.\n\
         END PROCEDURE.\n\
         PROCEDURE show:\n\
             MESSAGE total.\n\
         END PROCEDURE.\n\
         FIND FIRST Customer WHERE Customer.CustNum = 0 EXCLUSIVE-LOCK NO-ERROR.\n\
         IF AVAILABLE Customer THEN Customer.Name = \"main\".\n",
        k % MIDDLE,
        (k * 5 + 1) % MIDDLE,
    )
}

fn middle(m: usize) -> String {
    format!(
        "{{inc/common.i}}\n\
         FOR EACH Customer NO-LOCK WHERE Customer.CustNum > {m}:\n\
             i = i + 1.\n\
         END.\n\
         RUN lib/l{}.p.\n\
         RUN lib/l{}.p.\n",
        m % LEAVES,
        (m + 1) % LEAVES
    )
}

fn leaf(l: usize) -> String {
    let body = if l == 0 {
        "CREATE Customer.\nCustomer.Name = \"new\".\n".to_string()
    } else {
        format!(
            "RUN lib/l{}.p.\nFIND FIRST Customer NO-LOCK NO-ERROR.\n",
            l - 1
        )
    };
    format!("{{inc/common.i}}\n{body}")
}

struct Set {
    fs: InMemoryFileSystem,
    roots: Vec<(PathBuf, String)>,
}

fn program_set() -> Set {
    let mut fs = InMemoryFileSystem::new();
    let mut put = |path: &str, text: String| fs.insert(PathBuf::from(format!("/ws/{path}")), text);
    put("inc/common.i", COMMON.to_string());
    for m in 0..MIDDLE {
        put(&format!("mid/m{m}.p"), middle(m));
    }
    for l in 0..LEAVES {
        put(&format!("lib/l{l}.p"), leaf(l));
    }
    let roots = (0..ROOTS)
        .map(|k| {
            let path = format!("app/r{k:02}.p");
            let text = root(k);
            put(&path, text.clone());
            (PathBuf::from(format!("/ws/{path}")), text)
        })
        .collect();
    Set { fs, roots }
}

/// A configuration with the one rule `code` on, or none of the four.
fn config(code: Option<&str>) -> PipelineConfig {
    let mut severities = LintSeverityMap::new();
    for off in ["TX001", "TX002", "TX003", "TX004"] {
        severities.set(off, None);
    }
    if let Some(code) = code {
        severities.set(code, Some(Severity::Warning));
    }
    PipelineConfig {
        schema: customer_schema(),
        schema_loaded: true,
        include_paths: vec![PathBuf::from("/ws")],
        lint_severities: severities,
        ..PipelineConfig::default()
    }
}

/// Lint every root. Returns the count of findings with `code`.
fn lint_roots(set: &Set, config: &PipelineConfig, code: &str, summaries: bool) -> usize {
    let run = LintPipeline::new(config, &set.fs);
    let paths: Vec<PathBuf> = set.roots.iter().map(|(path, _)| path.clone()).collect();
    let run = if summaries {
        let effects = Arc::new(run.build_workspace_effect_summaries(&paths));
        run.with_effect_summaries(effects)
    } else {
        run
    };
    set.roots
        .iter()
        .map(|(path, text)| {
            let result = run.with_file(path).run(text);
            assert!(result.failure().is_none());
            result.all().filter(|d| d.diagnostic.code.0 == code).count()
        })
        .sum()
}

fn bench_rules(c: &mut Criterion) {
    let set = program_set();
    let mut group = c.benchmark_group("tx_rules");
    group.sample_size(50);

    // A case that finds nothing measures nothing.
    for code in ["TX001", "TX002", "TX003", "TX004"] {
        let config = config(Some(code));
        let found = lint_roots(&set, &config, code, code == "TX004");
        assert!(found >= ROOTS, "{code} found {found}");
        group.bench_function(code, |b| {
            b.iter(|| black_box(lint_roots(&set, &config, code, code == "TX004")))
        });
    }
    let none = config(None);
    group.bench_function("none", |b| {
        b.iter(|| black_box(lint_roots(&set, &none, "TX001", false)))
    });

    group.finish();
}

criterion_group!(benches, bench_rules);
criterion_main!(benches);
