use std::path::PathBuf;

use oxabl_pipeline::{ConfigOverrides, configured_schema_files, resolve_from_config_deferred};
use oxabl_workspace::ObservingFileSystem;
use rustc_hash::{FxHashMap, FxHashSet};

use super::RefreshError;
use super::assemble::{Parts, assemble};
use super::binary::executable_hash;
use super::files::{Current, Files, Job};
use super::identity::analysis_identity;
use super::intern::Interner;
use super::listing::Listings;
use super::outcome::{Delta, Outcome};
use super::probe::now_ns;
use super::settle::{Analyses, Settle};
use super::snapshot::SnapshotFs;
use super::walk::walk_units;
use crate::config::BuildWorkspace;
use crate::paths::PathMap;
use crate::store::{Analysis, FileRow, Loaded, Reading, tracked};

/// What a caller needs from a refresh besides the store changes.
#[derive(Debug, Clone, Copy, Default)]
pub struct Request {
    /// Load the graph even when nothing changed.
    pub need_graph: bool,
    /// Load the graph and the analyses, and report the content of every
    /// tracked path: what the compile keys are made from.
    pub need_keys: bool,
}

/// Bring the graph up to date with the working tree. `reading` is the store
/// snapshot to start from; `None` starts from nothing. Nothing is written: the
/// outcome holds the changes for the caller to commit.
pub fn run(
    workspace: &BuildWorkspace,
    reading: Option<&Reading<'_>>,
    request: Request,
) -> Result<Outcome, RefreshError> {
    let settings = &workspace.settings;
    let config = &workspace.config;
    let observed_ns = now_ns();
    let paths = PathMap::new(&settings.root, settings.dlc.as_deref());
    let loaded = match reading {
        Some(reading) => reading.load()?,
        None => Loaded::empty(),
    };
    let generation = loaded.generation + 1;
    let overrides = ConfigOverrides::default();
    let (mut pipeline, config_warnings) =
        resolve_from_config_deferred(config, &settings.root, &overrides);
    let (schema_files, schema_warnings) =
        configured_schema_files(config, &settings.root, &overrides);
    let mut warnings: Vec<String> = config_warnings
        .iter()
        .chain(&schema_warnings)
        .map(ToString::to_string)
        .collect();
    let (binary, binary_row) = executable_hash(loaded.binary.as_ref(), observed_ns);
    let identity = analysis_identity(&binary, config, settings, &schema_files);
    let identity_changed = loaded.identity != Some(identity);

    let interner = Interner::from_names(loaded.paths);
    let mut old_rows: FxHashMap<u32, FileRow> = loaded.files.into_iter().collect();
    let old_units: FxHashSet<u32> = old_rows
        .iter()
        .filter(|(_, row)| row.tracked_by & tracked::UNIT != 0)
        .map(|(id, _)| *id)
        .collect();

    let units = walk_units(settings, &paths, &pipeline.root_extensions)?;
    let unit_ids: Vec<u32> = units
        .iter()
        .map(|u| interner.intern(&u.canonical))
        .collect();
    let unit_set: FxHashSet<u32> = unit_ids.iter().copied().collect();
    let mut removed_units: Vec<u32> = old_units.difference(&unit_set).copied().collect();
    removed_units.sort_unstable();
    let new_units: FxHashSet<u32> = unit_set.difference(&old_units).copied().collect();

    let snapshot = SnapshotFs::new(pipeline.decoding);
    let others: Vec<(u32, PathBuf)> = old_rows
        .keys()
        .filter(|id| !unit_set.contains(id))
        .map(|id| {
            let name = interner.name_of(*id).unwrap_or_default();
            (*id, paths.absolute(&name))
        })
        .collect();
    let jobs: Vec<Job<'_>> = unit_ids
        .iter()
        .zip(&units)
        .map(|(id, unit)| Job {
            id: *id,
            path: &unit.path,
            stat: Some(unit.probe),
        })
        .chain(others.iter().map(|(id, path)| Job {
            id: *id,
            path,
            stat: None,
        }))
        .collect();
    let mut files = Files::new(observed_ns, generation);
    files.probe_all(jobs, &mut old_rows, &snapshot);
    let orphan_rows: Vec<u32> = old_rows.keys().copied().collect();

    let mut listings = Listings::check(
        loaded.listings,
        |id| paths.absolute(&interner.name_of(id).unwrap_or_default()),
        observed_ns,
        generation,
    );

    let analysed: FxHashSet<u32> = loaded.analysis_heads.iter().map(|(id, _, _)| *id).collect();
    let units_lack_analysis = unit_ids.iter().any(|id| !analysed.contains(id));
    let unchanged = files.changed.is_empty()
        && listings.changed.is_empty()
        && !identity_changed
        && removed_units.is_empty()
        && !units_lack_analysis;
    let skipped = unchanged && !request.need_graph && !request.need_keys;

    let fs = ObservingFileSystem::new(snapshot);
    let mut analyses = Analyses::default();
    if !skipped {
        let mut old: FxHashMap<u32, Analysis> = FxHashMap::default();
        if let Some(reading) = reading {
            for (id, stored) in reading.analyses()? {
                old.insert(id, stored.decode()?);
            }
        }
        let affected: FxHashSet<u32> = files
            .changed
            .iter()
            .chain(&listings.changed)
            .copied()
            .collect();
        let stale: Vec<u32> = old
            .iter()
            .filter(|(id, analysis)| {
                identity_changed
                    || !matches!(
                        files.get(**id).map(|t| t.current),
                        Some(Current::Present { hash, .. }) if hash == analysis.content_hash
                    )
                    || analysis
                        .observations
                        .iter()
                        .any(|o| affected.contains(&o.path))
            })
            .map(|(id, _)| *id)
            .collect();
        analyses = Analyses::new(old);
        for id in stale {
            analyses.invalidate(id);
        }
        let pending: Vec<u32> = unit_ids
            .iter()
            .copied()
            .filter(|id| analyses.get(*id).is_none())
            .collect();
        if !pending.is_empty() {
            let schema_warnings = pipeline.load_schema(config, &settings.root, &overrides);
            warnings.extend(schema_warnings.iter().map(ToString::to_string));
        }
        Settle {
            config: &pipeline,
            fs: &fs,
            paths: &paths,
            interner: &interner,
            files: &mut files,
            listings: &mut listings,
            generation,
        }
        .run(&mut analyses, pending)?;
    }

    let mut outcome = assemble(Parts {
        generation,
        loaded_generation: loaded.generation,
        identity: identity_changed.then_some(identity),
        binary: binary_row,
        unit_ids,
        orphan_rows,
        interner,
        files,
        listings,
        analyses,
        delta: Delta {
            new_units,
            removed_units,
            ..Delta::default()
        },
        skipped,
        need_keys: request.need_keys,
        need_generations: request.need_graph,
    });
    outcome.warnings = warnings;
    Ok(outcome)
}
