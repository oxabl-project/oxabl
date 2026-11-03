//! Compile keys for every unit of a plan.

use std::collections::{BTreeMap, BTreeSet};

use rustc_hash::{FxHashMap, FxHashSet};

use super::closure;
use super::compile_key::{Algorithm, ClosureFile, EntryDigest, Material, compile_key, components};
use super::digest::{FileDigest, sha256_bytes, sha256_hex};
use super::environment::Environment;
use super::keyability::{ExternalPackage, Resolved, Rules};
use super::layout::{Layout, lay_out};
use super::options::CompileSettings;
use crate::config::BuildWorkspace;
use crate::paths::PathMap;
use crate::paths::propath::{OUT, Placement, Search, canonical_entry, library_name};
use crate::refresh::{Content, Outcome, parallel};
use crate::store::Components;
use crate::worker::{Pool, PoolError, program};

/// The key of one unit, or why it has none.
#[derive(Debug, Clone, Default)]
pub struct UnitKey {
    /// The name the worker compiles; `None` for a skipped unit.
    pub compile_name: Option<String>,
    pub key: Option<[u8; 32]>,
    /// The digest of each section of the key, when there is a key.
    pub components: Option<Components>,
    /// Why there is no key: the rules K1 to K9, or the reason the unit is
    /// skipped. Empty when there is a key.
    pub unkeyable: Vec<String>,
    pub skipped: bool,
    /// The index of the unit PROPATH in [`Keyed::propaths`].
    pub propath: Option<usize>,
}

/// The keys of all units of a plan.
#[derive(Debug, Default)]
pub struct Keyed {
    /// By position in `Outcome::units`.
    pub units: Vec<UnitKey>,
    /// The distinct unit PROPATHs, each entry with its digest.
    pub propaths: Vec<Vec<(String, String)>>,
    pub environment: Option<blake3::Hash>,
    /// The readable record of the environment, as the store keeps it.
    pub environment_record: Option<String>,
    pub warnings: Vec<String>,
}

fn config_error(message: String) -> PoolError {
    PoolError::Config(message)
}

fn from_worker(digest: &FileDigest) -> EntryDigest {
    match digest {
        FileDigest::File { sha256, .. } => EntryDigest::Library(sha256.clone()),
        FileDigest::Dir => EntryDigest::Dir,
        FileDigest::Absent => EntryDigest::Absent,
    }
}

/// The path to ask a worker about for the PROPATH `entry`, when the host
/// cannot answer: a library, an entry under `$DLC`, or a file under the root.
fn worker_path(
    paths: &PathMap,
    libraries: &BTreeMap<String, String>,
    entry: &str,
) -> Result<Option<String>, Resolved> {
    if let Some(name) = library_name(entry) {
        return Ok(libraries.get(name).cloned());
    }
    if entry.starts_with('$') {
        return Ok(Some(entry.to_string()));
    }
    match std::fs::metadata(paths.absolute(entry)) {
        Ok(meta) if meta.is_dir() => Err(Resolved::Known(EntryDigest::Dir)),
        Ok(_) => Ok(Some(entry.to_string())),
        Err(_) => Err(Resolved::Known(EntryDigest::Absent)),
    }
}

/// The digests of the PROPATH entries and the worker copies of the `$DLC`
/// closure files, as one worker reports them and every other worker agrees.
struct Hashed {
    entries: FxHashMap<String, Resolved>,
    dlc_files: FxHashMap<String, FileDigest>,
}

fn hash_with_workers(
    pool: &mut Pool,
    workspace: &BuildWorkspace,
    environment: &Environment,
    paths: &PathMap,
    entries: &BTreeSet<&String>,
    dlc_files: &BTreeSet<String>,
) -> Result<Hashed, PoolError> {
    let libraries = &workspace.config.build.libraries;
    let mut resolved: FxHashMap<String, Resolved> = FxHashMap::default();
    for library in &environment.libraries {
        resolved.insert(
            library.entry.clone(),
            Resolved::Known(from_worker(&library.digest)),
        );
    }
    let mut asks: Vec<(String, String)> = Vec::new();
    for entry in entries
        .iter()
        .filter(|entry| **entry != OUT && !entry.starts_with('/'))
    {
        if resolved.contains_key(*entry) {
            continue;
        }
        match worker_path(paths, libraries, entry) {
            Ok(Some(path)) => asks.push(((*entry).clone(), path)),
            Ok(None) => {
                resolved.insert((*entry).clone(), Resolved::Known(EntryDigest::Absent));
            }
            Err(known) => {
                resolved.insert((*entry).clone(), known);
            }
        }
    }
    resolved.insert(OUT.to_string(), Resolved::Known(EntryDigest::Dir));
    let wanted: Vec<String> = dlc_files
        .iter()
        .cloned()
        .chain(asks.iter().map(|(_, path)| path.clone()))
        .collect();
    let answers: FxHashMap<String, FileDigest> = match wanted.is_empty() {
        true => FxHashMap::default(),
        false => pool.hash_dlc_files(&wanted)?.into_iter().collect(),
    };
    for (entry, path) in asks {
        let digest = &answers[&path];
        let is_library = library_name(&entry).is_some();
        resolved.insert(
            entry,
            match (digest, is_library) {
                (FileDigest::Dir, true) => Resolved::Unhashable,
                _ => Resolved::Known(from_worker(digest)),
            },
        );
    }
    let dlc_files = dlc_files
        .iter()
        .map(|path| (path.clone(), answers[path].clone()))
        .collect();
    Ok(Hashed {
        entries: resolved,
        dlc_files,
    })
}

/// The `$DLC/...` files the worker and the host mirror disagree about, and the
/// worker digest of those they agree about.
fn compare_mirror(
    paths: &PathMap,
    dlc_files: &FxHashMap<String, FileDigest>,
) -> (FxHashSet<String>, FxHashMap<String, [u8; 32]>) {
    let list: Vec<(&String, &FileDigest)> = dlc_files.iter().collect();
    let checked = parallel::map(&list, |(path, worker)| {
        let host = std::fs::read(paths.absolute(path))
            .ok()
            .map(|b| sha256_hex(&b));
        match (worker, host) {
            (FileDigest::File { sha256, .. }, Some(host)) if *sha256 == host => {
                sha256_bytes(sha256).ok_or(())
            }
            _ => Err(()),
        }
    });
    let mut mismatch = FxHashSet::default();
    let mut agreed = FxHashMap::default();
    for ((path, _), outcome) in list.iter().zip(checked) {
        match outcome {
            Ok(bytes) => {
                agreed.insert((*path).clone(), bytes);
            }
            Err(()) => {
                mismatch.insert((*path).clone());
            }
        }
    }
    (mismatch, agreed)
}

/// Every class name a workspace class goes by, lower-cased, with the spelling
/// and the file.
fn workspace_classes(outcome: &Outcome, layout: &Layout) -> FxHashMap<String, (String, String)> {
    let mut names: FxHashMap<String, (String, String)> = FxHashMap::default();
    for (id, placed) in outcome.units.iter().zip(&layout.units) {
        if let Some(declared) = &placed.declared {
            names
                .entry(declared.to_ascii_lowercase())
                .or_insert_with(|| (declared.clone(), outcome.name(*id).to_string()));
        }
    }
    for (_, analysis) in outcome.analyses.iter() {
        for edge in analysis.edges.iter().filter(|edge| edge.kind.is_class()) {
            if let Some(name) = &edge.name {
                names
                    .entry(name.to_ascii_lowercase())
                    .or_insert_with(|| (name.clone(), outcome.name(edge.target).to_string()));
            }
        }
    }
    names
}

/// Computes the key of every unit of `outcome`, or says why it has none. The
/// workers of `pool` hash the library and `$DLC` files only they can see.
pub fn compute(
    workspace: &BuildWorkspace,
    outcome: &Outcome,
    environment: &Environment,
    pool: &mut Pool,
) -> Result<Keyed, PoolError> {
    let settings = &workspace.settings;
    let config = &workspace.config;
    let sources = &config.workspace.sources;
    let build = &config.build;
    let paths = PathMap::new(&settings.root, settings.dlc.as_deref());

    let include = sources
        .include_paths
        .iter()
        .map(|entry| canonical_entry(&paths, &build.libraries, entry))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|message| config_error(format!("[workspace.sources].include_paths: {message}")))?;
    let defaults = environment
        .libraries
        .iter()
        .map(|l| l.entry.clone())
        .collect();
    let search = Search::new(
        sources.root_relative_include_paths.clone(),
        include,
        defaults,
    );
    let layout = lay_out(outcome, &paths, &search);

    let active: Vec<usize> = (0..outcome.units.len())
        .filter(|at| !matches!(layout.units[*at].placement, Placement::Skipped(_)))
        .collect();
    let active_ids: Vec<u32> = active.iter().map(|at| outcome.units[*at]).collect();
    let closures = closure::of_units(outcome, &active_ids);

    let dlc_files: BTreeSet<String> = closures
        .iter()
        .flatten()
        .map(|id| outcome.name(*id))
        .filter(|name| name.starts_with("$DLC/"))
        .map(str::to_string)
        .collect();
    let used: BTreeSet<&String> = active
        .iter()
        .filter_map(|at| layout.units[*at].propath)
        .collect::<BTreeSet<_>>()
        .into_iter()
        .flat_map(|id| &layout.propaths[id])
        .collect();
    let hashed = hash_with_workers(pool, workspace, environment, &paths, &used, &dlc_files)?;
    let (mirror_mismatch, dlc_sha) = compare_mirror(&paths, &hashed.dlc_files);

    let class_units = outcome
        .units
        .iter()
        .zip(&layout.units)
        .filter(|(_, placed)| placed.class)
        .map(|(id, _)| *id);
    let class_sources = closure::class_sources(outcome, class_units);
    let external = ExternalPackage::lower(&build.external_packages);
    let rules = Rules {
        outcome,
        external: &external,
        environment_unknown: &environment.unknown,
        digests: &hashed.entries,
        mirror_mismatch: &mirror_mismatch,
        class_sources: &class_sources,
        several_propaths: layout.propaths.len() > 1,
    };
    let positions: Vec<usize> = (0..active.len()).collect();
    let mut verdicts = parallel::map(&positions, |k| {
        let placed = &layout.units[active[*k]];
        let propath = &layout.propaths[placed.propath.expect("an active unit has a PROPATH")];
        rules.check(active_ids[*k], &closures[*k], propath)
    });

    let mut warnings = Vec::new();
    let classes = workspace_classes(outcome, &layout);
    let mut conflicts: BTreeMap<String, (String, String, usize)> = BTreeMap::new();
    let mut without_key = 0;
    for (k, verdict) in verdicts.iter_mut().enumerate() {
        let placed = &layout.units[active[k]];
        if placed.class && placed.declared.is_none() {
            let path = outcome.name(active_ids[k]);
            verdict.add_reason(4, format!("computed-name: class statement in {path}"));
        }
        let accepted = std::mem::take(&mut verdict.accepted);
        for name in &accepted {
            if let Some((spelling, file)) = classes.get(&name.name.to_ascii_lowercase()) {
                verdict.add_conflict(spelling);
                let slot = conflicts
                    .entry(spelling.clone())
                    .or_insert_with(|| (file.clone(), name.entry.clone(), 0));
                slot.2 += 1;
            }
        }
        verdict.accepted = accepted;
    }
    for (name, (file, entry, units)) in &conflicts {
        warnings.push(format!(
            "class {name} is accepted as external ({entry}) but {file} also has that name; {units} units that use it are compiled without a key"
        ));
    }
    let settings_lowered = CompileSettings::lower(build);
    let identity = environment.identity();
    let worker = program::hash();
    let propath_digests: Vec<Vec<(String, EntryDigest)>> = layout
        .propaths
        .iter()
        .map(|propath| {
            propath
                .iter()
                .map(|entry| match hashed.entries.get(entry) {
                    Some(Resolved::Known(digest)) => (entry.clone(), digest.clone()),
                    _ => (entry.clone(), EntryDigest::Absent),
                })
                .collect()
        })
        .collect();

    let keys = parallel::map(&positions, |k| {
        if !verdicts[*k].is_keyable() {
            return None;
        }
        let at = active[*k];
        let unit = active_ids[*k];
        let Placement::Compiles { name } = &layout.units[at].placement else {
            return None;
        };
        let Content::Hash(unit_content) = outcome.content_of(unit) else {
            return None;
        };
        let mut files = Vec::with_capacity(closures[*k].len());
        for id in &closures[*k] {
            let path = outcome.name(*id);
            let (algorithm, hash) = match dlc_sha.get(path) {
                Some(sha) => (Algorithm::Sha256, *sha),
                None => match outcome.content_of(*id) {
                    Content::Hash(hash) => (Algorithm::Blake3, hash),
                    _ => return None,
                },
            };
            files.push(ClosureFile {
                path,
                algorithm,
                hash,
            });
        }
        let propath = &propath_digests[layout.units[at].propath?];
        let material = Material {
            unit_path: outcome.name(unit),
            compile_name: name,
            unit_content,
            closure: &files,
            propath,
            environment: identity,
            settings: &settings_lowered,
            worker,
        };
        Some((compile_key(&material), components(&material)))
    });

    let mut units: Vec<UnitKey> = layout
        .units
        .iter()
        .map(|placed| match &placed.placement {
            Placement::Skipped(reason) => UnitKey {
                unkeyable: vec![reason.clone()],
                skipped: true,
                propath: placed.propath,
                ..UnitKey::default()
            },
            Placement::Compiles { name } => UnitKey {
                compile_name: Some(name.clone()),
                propath: placed.propath,
                ..UnitKey::default()
            },
        })
        .collect();
    for ((k, verdict), key) in verdicts.into_iter().enumerate().zip(keys) {
        let unit = &mut units[active[k]];
        let reasons = verdict.into_reasons();
        match key {
            Some((key, parts)) if reasons.is_empty() => {
                unit.key = Some(*key.as_bytes());
                unit.components = Some(parts);
            }
            _ => {
                without_key += usize::from(reasons.iter().any(|r| r == "per-unit-propath-classes"));
                unit.unkeyable = match reasons.is_empty() {
                    true => vec!["unreadable: the unit's own bytes".to_string()],
                    false => reasons,
                };
            }
        }
    }
    if without_key > 0 {
        warnings.push(format!(
            "{without_key} units that use classes are compiled without a key: the units have different PROPATHs"
        ));
    }
    let propaths = propath_digests
        .iter()
        .map(|propath| propath.iter().map(|(e, d)| (e.clone(), d.text())).collect())
        .collect();
    Ok(Keyed {
        units,
        propaths,
        environment: Some(identity),
        environment_record: Some(super::record::record(environment)),
        warnings,
    })
}
