//! Findings that sit in an include, kept with the compilation unit that
//! produced them. Synthetic roots and includes only.

use std::path::PathBuf;

use oxabl_pipeline::{IncludeOriginDiagnostic, LintPipeline, LintResult, PipelineConfig};
use oxabl_workspace::InMemoryFileSystem;

fn fs_of(files: &[(&str, &str)]) -> InMemoryFileSystem {
    let mut fs = InMemoryFileSystem::new();
    for (path, content) in files {
        fs.insert(PathBuf::from(path), *content);
    }
    fs
}

fn run(files: &[(&str, &str)], root: &str) -> LintResult {
    let fs = fs_of(files);
    let config = PipelineConfig {
        include_paths: vec![PathBuf::from("/w")],
        ..PipelineConfig::default()
    };
    LintPipeline::new(&config, &fs).run(root)
}

fn messages(result: &LintResult) -> Vec<&str> {
    result
        .include_origin()
        .iter()
        .map(|d| d.diagnostic.message.as_str())
        .collect()
}

/// The text a finding's span covers in its own include file.
fn covered<'a>(d: &IncludeOriginDiagnostic, content: &'a str) -> &'a str {
    let span = d.diagnostic.span.span;
    &content[span.start as usize..span.end as usize]
}

const DECL: &str = "DEFINE VARIABLE used-v AS INTEGER NO-UNDO.\n\
                    DEFINE VARIABLE spare-v AS INTEGER NO-UNDO.\n";

#[test]
fn unused_include_declaration_points_at_the_physical_include() {
    let result = run(&[("/w/decl.i", DECL)], "{decl.i}\nMESSAGE used-v.\n");

    assert_eq!(messages(&result), ["unused variable `spare-v`"]);
    let found = &result.include_origin()[0];
    assert_eq!(found.path, PathBuf::from("/w/decl.i"));
    assert_eq!(found.diagnostic.code.0, "LINT0002");
    assert_eq!(covered(found, DECL), "spare-v");
    assert_eq!(found.expansions.len(), 1);
    let expansion = &found.expansions[0];
    assert_eq!((expansion.occurrence, expansion.occurrences), (1, 1));
    assert_eq!(expansion.chain.len(), 1);
    assert_eq!(expansion.chain[0].path, PathBuf::from("/w/decl.i"));
    let site = expansion.chain[0]
        .site
        .expect("a written include has a site");
    assert_eq!((site.start, site.end), (0, 8));
}

#[test]
fn include_declaration_read_by_the_root_is_not_reported() {
    let result = run(
        &[("/w/decl.i", DECL)],
        "{decl.i}\nMESSAGE used-v spare-v.\n",
    );
    assert!(messages(&result).is_empty(), "{:?}", messages(&result));
}

#[test]
fn the_same_include_is_judged_per_root() {
    let files = [("/w/decl.i", DECL)];
    let reads = run(&files, "{decl.i}\nMESSAGE used-v spare-v.\n");
    let ignores = run(&files, "{decl.i}\nMESSAGE used-v.\n");
    assert!(reads.include_origin().is_empty());
    assert_eq!(ignores.include_origin().len(), 1);
}

#[test]
fn root_origin_findings_are_unchanged_and_not_duplicated() {
    let root = "{decl.i}\nDEFINE VARIABLE own-v AS INTEGER NO-UNDO.\nMESSAGE used-v.\n";
    let result = run(&[("/w/decl.i", DECL)], root);

    let root_found: Vec<_> = result
        .all()
        .map(|d| (d.diagnostic.code.0, d.diagnostic.message.as_str()))
        .collect();
    assert_eq!(root_found, [("LINT0002", "unused variable `own-v`")]);
    assert_eq!(messages(&result), ["unused variable `spare-v`"]);
}

#[test]
fn nested_include_keeps_its_chain_and_physical_location() {
    let inner = "DEFINE VARIABLE deep-v AS INTEGER NO-UNDO.\n";
    let outer = "/* outer */\n{inner.i}\n";
    let result = run(
        &[("/w/outer.i", outer), ("/w/inner.i", inner)],
        "\n{outer.i}\n",
    );

    let found = &result.include_origin()[0];
    assert_eq!(messages(&result), ["unused variable `deep-v`"]);
    assert_eq!(found.path, PathBuf::from("/w/inner.i"));
    assert_eq!(covered(found, inner), "deep-v");
    let chain = &found.expansions[0].chain;
    let paths: Vec<_> = chain.iter().map(|step| step.path.clone()).collect();
    assert_eq!(
        paths,
        [PathBuf::from("/w/outer.i"), PathBuf::from("/w/inner.i")]
    );
    // Each site is in the bytes of the file that wrote the reference.
    let root_site = chain[0].site.unwrap();
    assert_eq!(
        &"\n{outer.i}\n"[root_site.start as usize..root_site.end as usize],
        "{outer.i}"
    );
    let outer_site = chain[1].site.unwrap();
    assert_eq!(
        &outer[outer_site.start as usize..outer_site.end as usize],
        "{inner.i}"
    );
}

#[test]
fn the_result_is_deterministic() {
    let files = [("/w/decl.i", DECL), ("/w/o.i", "{decl.i}\n")];
    let first = run(&files, "{o.i}\n{decl.i}\n");
    let second = run(&files, "{o.i}\n{decl.i}\n");
    assert_eq!(first.include_origin(), second.include_origin());
}

#[test]
fn identical_findings_of_repeated_expansions_are_one_row_listing_each() {
    let decl = "DEFINE VARIABLE v AS INTEGER NO-UNDO.\n";
    let result = run(
        &[("/w/d.i", decl)],
        "PROCEDURE a:\n{d.i}\nEND PROCEDURE.\nPROCEDURE b:\n{d.i}\nEND PROCEDURE.\n",
    );

    assert_eq!(messages(&result), ["unused variable `v`"]);
    let occurrences: Vec<_> = result.include_origin()[0]
        .expansions
        .iter()
        .map(|e| (e.occurrence, e.occurrences))
        .collect();
    assert_eq!(occurrences, [(1, 2), (2, 2)]);
}

#[test]
fn distinct_findings_of_repeated_expansions_stay_distinct() {
    let decl = "DEFINE VARIABLE v AS INTEGER NO-UNDO.\n";
    let result = run(&[("/w/d.i", decl)], "{d.i}\n{d.i}\n");

    let mut found = messages(&result);
    found.sort_unstable();
    assert_eq!(
        found,
        [
            "`v` is already declared in this scope",
            "unused variable `v`"
        ]
    );
    // The second expansion is the one that redeclares.
    let redeclared = result
        .include_origin()
        .iter()
        .find(|d| d.diagnostic.message.contains("already declared"))
        .unwrap();
    assert_eq!(redeclared.expansions[0].occurrence, 2);
}

#[test]
fn a_finding_in_substituted_text_has_no_physical_location_and_is_not_returned() {
    let result = run(
        &[("/w/arg.i", "DEFINE VARIABLE {&n} AS INTEGER NO-UNDO.\n")],
        "{arg.i &n=spare-v}\n",
    );
    assert!(result.include_origin().is_empty());
    assert!(result.all().next().is_none());
}
