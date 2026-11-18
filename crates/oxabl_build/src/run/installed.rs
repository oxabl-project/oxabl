//! The manifest of the r-code beside the sources, around a build: the intents
//! and the removal of r-code that no unit expects before the first install,
//! and the whole manifest again at the end.

use std::collections::{BTreeMap, HashSet};
use std::path::PathBuf;

use rustc_hash::{FxHashMap, FxHashSet};

use super::shown;
use crate::config::BuildSettings;
use crate::error::BuildError;
use crate::output::OutputLayout;
use crate::output::in_place::rcode_path;
use crate::output::manifest::Manifest;
use crate::output::orphans::{self, Swept};
use crate::plan::Plan;
use crate::refresh::Outcome;
use crate::store::{OutputRow, Results};

/// The manifest during a build, and the r-code paths it keeps.
pub(super) struct Installed {
    manifest: Manifest,
    /// The r-code path beside each unit that compiles, to its unit.
    expected: FxHashMap<String, u32>,
}

/// The paths that a sweep never removes as an emptied directory.
fn kept_directories(settings: &BuildSettings, layout: &OutputLayout) -> HashSet<PathBuf> {
    let root = settings.root.as_path();
    let mut keep: HashSet<PathBuf> = layout
        .directories()
        .iter()
        .map(|directory| root.join(directory))
        .collect();
    keep.insert(root.to_path_buf());
    keep.insert(settings.state_dir.clone());
    keep.insert(settings.store_dir.clone());
    keep
}

fn infrastructure(what: &str, error: impl std::fmt::Display) -> BuildError {
    BuildError::Infrastructure(format!("{what}: {error}"))
}

/// Stops the build when a file that runs could not be deleted, and adds the
/// files that stay to the notes.
fn settle(swept: Swept, notes: &mut Vec<String>) -> Result<(), BuildError> {
    if !swept.failed.is_empty() {
        return Err(BuildError::Infrastructure(format!(
            "cannot delete r-code that oxabl installed and no unit expects: {}; the runtime would run it",
            shown(&swept.failed)
        )));
    }
    for kept in swept.kept {
        notes.push(format!("kept {kept}; delete it if nothing needs it"));
    }
    Ok(())
}

/// Before the first delete or rename of a build: appends an intent for the
/// r-code beside each unit to build, then deletes the temporary files of a
/// stopped install, the r-code in the manifest that no unit expects, and the
/// r-code of the store that no unit expects at its path (the place of the
/// other mode, or of another output directory).
pub(super) fn begin(
    settings: &BuildSettings,
    layout: &OutputLayout,
    plan: &Plan,
    outcome: &Outcome,
    results: &Results,
    compiling: &FxHashSet<u32>,
    notes: &mut Vec<String>,
) -> Result<Installed, BuildError> {
    let root = settings.root.as_path();
    let mut manifest = Manifest::read(&settings.state_dir)
        .map_err(|error| infrastructure("cannot read the manifest of the r-code", error))?;
    if manifest.unreadable > 0 {
        notes.push(format!(
            "{} lines of {} are not readable and were left out",
            manifest.unreadable,
            settings
                .state_dir
                .join(crate::output::manifest::FILE)
                .display()
        ));
    }
    let expected: FxHashMap<String, u32> = match layout.beside_source() {
        true => compiling
            .iter()
            .map(|unit| (outcome.name(*unit), *unit))
            .filter(|(path, _)| !path.starts_with(['$', '/']))
            .map(|(path, unit)| (rcode_path(path), unit))
            .collect(),
        false => FxHashMap::default(),
    };
    let intents: Vec<String> = match layout.beside_source() {
        true => plan
            .rebuild
            .iter()
            .map(|entry| rcode_path(&entry.path))
            .filter(|path| expected.contains_key(path))
            .collect(),
        false => Vec::new(),
    };
    manifest
        .intend(intents.iter().map(String::as_str))
        .map_err(|error| infrastructure("cannot write the manifest of the r-code", error))?;

    let keep = kept_directories(settings, layout);
    settle(
        orphans::remove_temporaries(root, manifest.lines.keys().map(String::as_str)),
        notes,
    )?;
    let expected_paths: HashSet<String> = expected.keys().cloned().collect();
    let (swept, lines) = orphans::sweep_manifest(root, &manifest, &expected_paths, &keep);
    settle(swept, notes)?;
    manifest.lines = lines;

    settle(
        orphans::sweep_rows(
            root,
            stale_rcode(layout, plan, outcome, results, compiling),
            &keep,
        ),
        notes,
    )?;
    Ok(Installed { manifest, expected })
}

/// The r-code rows of the store at a path that no unit expects now. A unit
/// that compiles and whose output names the plan does not know keeps its
/// rows.
fn stale_rcode<'a>(
    layout: &OutputLayout,
    plan: &Plan,
    outcome: &Outcome,
    results: &'a Results,
    compiling: &FxHashSet<u32>,
) -> Vec<&'a OutputRow> {
    let keys = plan.keys.as_ref();
    let mut expected: HashSet<String> = HashSet::new();
    let mut unknown: FxHashSet<u32> = FxHashSet::default();
    for unit in compiling {
        let names = keys
            .and_then(|keys| keys.get(outcome.name(*unit)))
            .and_then(|key| key.compile_name.as_deref().zip(key.output_name.as_deref()));
        match names {
            Some((name, output)) => {
                expected.extend(
                    layout
                        .expected(name, output)
                        .into_iter()
                        .map(|file| file.dest),
                );
            }
            None => {
                unknown.insert(*unit);
            }
        }
    }
    let mut stale: Vec<&OutputRow> = results
        .outputs
        .iter()
        .filter(|(unit, _)| !unknown.contains(unit))
        .flat_map(|(_, rows)| rows)
        .filter(|row| row.path.ends_with(".r") && !expected.contains(&row.path))
        .collect();
    stale.sort_by(|a, b| a.path.cmp(&b.path));
    stale
}

impl Installed {
    /// Writes the manifest again: the r-code beside each unit that compiles,
    /// with the hash of its last record, or an intent when the unit has no
    /// r-code now or the build did not reach it.
    pub(super) fn finish(
        mut self,
        stored: &FxHashMap<u32, Vec<OutputRow>>,
        installed: &FxHashMap<u32, Vec<OutputRow>>,
    ) -> Result<(), BuildError> {
        let mut lines = BTreeMap::new();
        for (path, unit) in &self.expected {
            let rows = installed.get(unit).or_else(|| stored.get(unit));
            let row = rows.and_then(|rows| rows.iter().find(|row| row.path == *path));
            match row {
                Some(row) => {
                    lines.insert(path.clone(), Some((row.hash, row.size)));
                }
                None if self.manifest.lines.contains_key(path) => {
                    lines.insert(path.clone(), None);
                }
                None => {}
            }
        }
        self.manifest
            .replace(lines)
            .map_err(|error| infrastructure("cannot write the manifest of the r-code", error))
    }
}
