//! The predefined names oxabl analysed with must equal the worker's values.

use oxabl_workspace::PreprocessorConfig;

use super::environment::Facts;

/// Why the worker's environment cannot back the analysis.
#[derive(Debug, PartialEq, Eq)]
pub enum Disagreement {
    /// `[workspace.preprocessor].proversion` is not set.
    ProversionUnset,
    /// One difference per entry, each naming both sides.
    Differences(Vec<String>),
}

fn differ(name: &str, configured: &str, reported: &str, out: &mut Vec<String>) {
    if configured != reported {
        out.push(format!(
            "{name}: [workspace.preprocessor] says `{configured}`, the worker reports `{reported}`"
        ));
    }
}

/// Compares `[workspace.preprocessor]` with the worker. `schema_names` are the
/// logical names the `[workspace.schema]` files stand for, used when
/// `databases` is not set.
pub fn check(
    facts: &Facts,
    preprocessor: &PreprocessorConfig,
    schema_names: &[String],
) -> Result<(), Disagreement> {
    let Some(proversion) = &preprocessor.proversion else {
        return Err(Disagreement::ProversionUnset);
    };
    let mut out = Vec::new();
    differ("opsys", &preprocessor.opsys, facts.get("opsys"), &mut out);
    differ(
        "window-system",
        &preprocessor.window_system,
        facts.get("window_system"),
        &mut out,
    );
    let batch = if preprocessor.batch_mode { "yes" } else { "no" };
    differ("batch-mode", batch, facts.get("batch_mode"), &mut out);
    differ(
        "process-architecture",
        &preprocessor.process_architecture.to_string(),
        facts.get("process_architecture"),
        &mut out,
    );
    differ("proversion", proversion, facts.get("proversion"), &mut out);

    let reported: Vec<&str> = facts.databases.iter().map(|d| d.ldb.as_str()).collect();
    match &preprocessor.databases {
        Some(listed) => {
            let same = listed.len() == reported.len()
                && listed
                    .iter()
                    .zip(&reported)
                    .all(|(a, b)| a.eq_ignore_ascii_case(b));
            if !same {
                out.push(format!(
                    "databases: [workspace.preprocessor] says [{}], the worker connects [{}]",
                    listed.join(", "),
                    reported.join(", ")
                ));
            }
        }
        None => {
            let mut inferred: Vec<String> = schema_names
                .iter()
                .map(|n| n.to_ascii_lowercase())
                .collect();
            let mut actual: Vec<String> = reported.iter().map(|n| n.to_ascii_lowercase()).collect();
            inferred.sort();
            inferred.dedup();
            actual.sort();
            actual.dedup();
            if inferred != actual {
                out.push(format!(
                    "databases: [workspace.schema] stands for [{}], the worker connects [{}]",
                    inferred.join(", "),
                    reported.join(", ")
                ));
            }
        }
    }
    for (name, dbtype) in &preprocessor.database_types {
        match facts.databases.iter().find(|d| d.ldb.eq_ignore_ascii_case(name)) {
            Some(database) if database.dbtype.eq_ignore_ascii_case(dbtype) => {}
            Some(database) => out.push(format!(
                "database-types.{name}: [workspace.preprocessor] says `{dbtype}`, the worker reports `{}`",
                database.dbtype
            )),
            None => out.push(format!(
                "database-types.{name}: the worker connects no database `{name}`"
            )),
        }
    }
    for database in &facts.databases {
        let named = preprocessor
            .database_types
            .keys()
            .any(|name| name.eq_ignore_ascii_case(&database.ldb));
        if !named && !database.dbtype.eq_ignore_ascii_case("PROGRESS") {
            out.push(format!(
                "database-types.{}: not set, and the worker reports `{}` instead of PROGRESS",
                database.ldb, database.dbtype
            ));
        }
    }
    if out.is_empty() {
        Ok(())
    } else {
        Err(Disagreement::Differences(out))
    }
}
