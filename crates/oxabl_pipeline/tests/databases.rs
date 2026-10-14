//! The databases an `&IF` can see reaching the edge set: `DBTYPE`, `CONNECTED`
//! and `LDBNAME` decide a branch from the schema files or `[workspace.preprocessor]`.

use std::fs;
use std::path::Path;

use oxabl_pipeline::{
    Completeness, ConfigOverrides, DegradedReason, EdgeKind, LintPipeline, PipelineConfig,
};
use oxabl_workspace::RealFileSystem;

const DF: &str = "ADD TABLE \"rate_a\"\nADD FIELD \"id\" OF \"rate_a\" AS integer\n\
                  ADD TABLE \"rate_b\"\nADD FIELD \"id\" OF \"rate_b\" AS integer\n";

const SOURCE: &str = "&IF DBTYPE(\"ledger\") NE ? &THEN\nDEFINE BUFFER b FOR rate_a.\n\
                      &ELSE\nDEFINE BUFFER b FOR rate_b.\n&ENDIF\nFIND FIRST b NO-LOCK.\n";

/// The schema tables the edge set names and its completeness, for a workspace
/// whose `oxabl.toml` holds `table`, with `ledger.df` in `schema/`.
fn tables(table: &str) -> (Vec<String>, Completeness) {
    tables_of(SOURCE, "ledger.df", table)
}

/// Like [`tables`], for `source` and a schema file named `df_name`. The
/// `[workspace.schema]` table in `table` has to name that file.
fn tables_of(source: &str, df_name: &str, table: &str) -> (Vec<String>, Completeness) {
    let tmp = tempfile::TempDir::new().unwrap();
    let root: &Path = tmp.path();
    fs::create_dir_all(root.join("schema")).unwrap();
    fs::write(root.join("schema").join(df_name), DF).unwrap();
    fs::write(root.join("main.p"), source).unwrap();
    fs::write(
        root.join("oxabl.toml"),
        format!("[workspace]\nname = \"p\"\n{table}"),
    )
    .unwrap();

    let (config, warnings) =
        PipelineConfig::resolve(&root.join("main.p"), &ConfigOverrides::default());
    assert!(warnings.is_empty(), "{warnings:?}");
    let fs = RealFileSystem;
    let run = LintPipeline::new(&config, &fs);
    let edges = run
        .with_file(root.join("main.p"))
        .edge_set(source)
        .expect("edge set");
    let mut named: Vec<String> = edges
        .of_kind(EdgeKind::SchemaTable)
        .map(|edge| edge.target.key().to_string())
        .collect();
    named.sort();
    (named, edges.completeness().clone())
}

const SCHEMA: &str = "[workspace.schema]\nfiles = [\"schema/ledger.df\"]\n";

#[test]
fn a_schema_file_connects_the_database_it_is_named_after() {
    let (named, completeness) = tables(SCHEMA);
    assert_eq!(named, ["rate_a"]);
    assert_eq!(completeness, Completeness::Complete);
}

#[test]
fn a_listed_database_replaces_the_schema_file_names() {
    let (named, completeness) = tables(&format!(
        "{SCHEMA}[workspace.preprocessor]\ndatabases = [\"other\"]\n"
    ));
    assert_eq!(named, ["rate_b"]);
    assert_eq!(completeness, Completeness::Complete);

    let (named, completeness) = tables(&format!(
        "{SCHEMA}[workspace.preprocessor]\ndatabases = [\"LEDGER\"]\n"
    ));
    assert_eq!(named, ["rate_a"]);
    assert_eq!(completeness, Completeness::Complete);
}

#[test]
fn without_any_database_information_the_branch_is_a_guess() {
    let (_, completeness) = tables("");
    assert_eq!(
        completeness,
        Completeness::Degraded {
            reasons: vec![DegradedReason::PreprocessorLoss]
        }
    );
}

const VERSIONED_SCHEMA: &str = "[workspace.schema]\nfiles = [\"schema/ledger_v12.df\"]\n";

fn preprocessor_loss() -> Completeness {
    Completeness::Degraded {
        reasons: vec![DegradedReason::PreprocessorLoss],
    }
}

/// A schema file name is not always the logical name (a versioned dump, for
/// one), so a name that no file is called is not known to be absent.
#[test]
fn a_name_that_no_schema_file_has_is_a_guess_not_an_absent_database() {
    let (_, completeness) = tables_of(SOURCE, "ledger_v12.df", VERSIONED_SCHEMA);
    assert_eq!(completeness, preprocessor_loss());

    let source = SOURCE.replace("DBTYPE(\"ledger\") NE ?", "CONNECTED(\"ledger\")");
    let (_, completeness) = tables_of(&source, "ledger_v12.df", VERSIONED_SCHEMA);
    assert_eq!(completeness, preprocessor_loss());
}

#[test]
fn a_name_that_a_schema_file_has_is_answered() {
    let source = SOURCE.replace("\"ledger\"", "\"ledger_v12\"");
    let (named, completeness) = tables_of(&source, "ledger_v12.df", VERSIONED_SCHEMA);
    assert_eq!(named, ["rate_a"]);
    assert_eq!(completeness, Completeness::Complete);
}

#[test]
fn a_stated_list_makes_an_unlisted_name_absent_again() {
    let (named, completeness) = tables_of(
        SOURCE,
        "ledger_v12.df",
        &format!("{VERSIONED_SCHEMA}[workspace.preprocessor]\ndatabases = [\"other\"]\n"),
    );
    assert_eq!(named, ["rate_b"]);
    assert_eq!(completeness, Completeness::Complete);
}

#[test]
fn dictdb_is_the_first_connected_database() {
    let source = "&IF CONNECTED(\"DICTDB\") &THEN\nDEFINE BUFFER b FOR rate_a.\n\
                  &ELSE\nDEFINE BUFFER b FOR rate_b.\n&ENDIF\nFIND FIRST b NO-LOCK.\n";

    // From the schema file names.
    let (named, completeness) = tables_of(source, "ledger_v12.df", VERSIONED_SCHEMA);
    assert_eq!(named, ["rate_a"]);
    assert_eq!(completeness, Completeness::Complete);

    // From a stated list, which may be empty.
    let stated =
        |list: &str| format!("{VERSIONED_SCHEMA}[workspace.preprocessor]\ndatabases = {list}\n");
    let (named, completeness) = tables_of(source, "ledger_v12.df", &stated("[\"main\"]"));
    assert_eq!(named, ["rate_a"]);
    assert_eq!(completeness, Completeness::Complete);
    let (named, completeness) = tables_of(source, "ledger_v12.df", &stated("[]"));
    assert_eq!(named, ["rate_b"]);
    assert_eq!(completeness, Completeness::Complete);
}
