//! Compile keys for every unit of a plan.

use std::collections::{BTreeMap, BTreeSet};

use rustc_hash::{FxHashMap, FxHashSet};

use super::closure;
use super::compile_key::{
    Algorithm, ClosureFile, ClosureSource, EntryDigest, Material, compile_key, components,
};
use super::digest::{FileDigest, sha256_bytes, sha256_hex};
use super::environment::Environment;
use super::keyability::{ExternalPackage, Resolved, Rules, Verdict};
use super::layout::{Layout, Placed, lay_out, search};
use super::options::CompileSettings;
use super::resolution::{ClassResolution, ResolutionSummary, Resolver, Units};
use crate::config::BuildWorkspace;
use crate::graph::Scratch;
use crate::observe::class_listing;
use crate::paths::propath::{OUT, Placement, is_class_unit, library_name};
use crate::paths::{Declared, PathMap};
use crate::refresh::{Content, Observed, Outcome, parallel};
use crate::store::Components;
use crate::worker::{Pool, PoolError, program};

/// The key of one unit, or why it has none.
#[derive(Debug, Clone, Default)]
pub struct UnitKey {
    /// The name the worker compiles; `None` for a skipped unit.
    pub compile_name: Option<String>,
    /// The name of the unit's output files ([`Placement::Compiles`]).
    pub output_name: Option<String>,
    pub key: Option<[u8; 32]>,
    /// The digest of each section of the key, when there is a key.
    pub components: Option<Components>,
    /// Why there is no key: the rules K1 to K11, or the reason the unit is
    /// skipped. Empty when there is a key.
    pub unkeyable: Vec<String>,
    pub skipped: bool,
    /// The index of the unit PROPATH in [`Keyed::propaths`].
    pub propath: Option<usize>,
    /// The unit has no key from its source only because oxabl cannot read its
    /// closure, so the closure its compiler reports can give it one.
    pub observable: bool,
    /// The key holds the closure observed at the last compile.
    pub observed: bool,
    /// With `observed`: the files of that closure, the listed classes and
    /// their closures included.
    pub observed_closure: Vec<u32>,
    /// How the class names the unit reaches resolve under every PROPATH id;
    /// only when K9 applies to it.
    pub class_resolution: Option<ClassResolution>,
}

/// The class sources that a compile of a unit that oxabl cannot read could
/// use, on one unit PROPATH.
#[derive(Debug, Clone, Default)]
pub struct ClassPart {
    /// Their canonical paths, sorted.
    pub listing: Vec<String>,
    /// The listed classes and the closures oxabl found for them.
    pub files: Vec<u32>,
    /// Why a listed class cannot be keyed: then no unit on this PROPATH gets
    /// a key from an observed closure.
    pub reasons: Vec<String>,
}

/// What every key of a plan shares, so that a key can also be made after a
/// compile, from the closure the compiler reported.
#[derive(Debug, Clone)]
pub struct KeyBase {
    pub environment: blake3::Hash,
    pub settings: CompileSettings,
    pub worker: blake3::Hash,
    /// Each distinct unit PROPATH, each entry with its digest.
    pub propaths: Vec<Vec<(String, EntryDigest)>>,
}

impl KeyBase {
    /// The key of a unit and the digest of each of its sections. `closure`
    /// is sorted by path bytes.
    pub fn key(
        &self,
        unit: (&str, &str, [u8; 32]),
        closure: &[ClosureFile<'_>],
        source: ClosureSource,
        propath: usize,
    ) -> ([u8; 32], Components) {
        let (unit_path, compile_name, unit_content) = unit;
        let material = Material {
            unit_path,
            compile_name,
            unit_content,
            closure,
            source,
            propath: &self.propaths[propath],
            environment: self.environment,
            settings: &self.settings,
            worker: self.worker,
        };
        (*compile_key(&material).as_bytes(), components(&material))
    }
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
    /// The SHA-256 of each `$DLC/...` closure file as the workers hashed it.
    pub dlc: FxHashMap<String, [u8; 32]>,
    pub base: Option<KeyBase>,
    /// The class part of each unit PROPATH that a unit oxabl cannot read uses.
    pub classes: FxHashMap<usize, ClassPart>,
    /// The counts of the class resolutions; only with more than one PROPATH id.
    pub class_resolution: Option<ResolutionSummary>,
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
        if let Some(declared) = placed.class_name() {
            names
                .entry(declared.to_ascii_lowercase())
                .or_insert_with(|| (declared.to_string(), outcome.name(*id).to_string()));
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
    let build = &config.build;
    let paths = PathMap::new(&settings.root, settings.dlc.as_deref());

    let defaults = environment
        .libraries
        .iter()
        .map(|l| l.entry.clone())
        .collect();
    let search = search(workspace, &paths, defaults).map_err(config_error)?;
    let layout = lay_out(outcome, &paths, &search);

    let active: Vec<usize> = (0..outcome.units.len())
        .filter(|at| !matches!(layout.units[*at].placement, Placement::Skipped(_)))
        .collect();
    let active_ids: Vec<u32> = active.iter().map(|at| outcome.units[*at]).collect();
    let closures = closure::of_units(outcome, &active_ids);

    let observed = outcome.observed.values().flat_map(|o| &o.files);
    let dlc_files: BTreeSet<String> = closures
        .iter()
        .flatten()
        .chain(observed)
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
    let (mut mirror_mismatch, mut dlc_sha) = compare_mirror(&paths, &hashed.dlc_files);

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
    };
    let positions: Vec<usize> = (0..active.len()).collect();
    let mut verdicts = parallel::map(&positions, |k| {
        let placed = &layout.units[active[*k]];
        let propath = &layout.propaths[placed.propath.expect("an active unit has a PROPATH")];
        rules.check(active_ids[*k], &closures[*k], propath)
    });
    let units = resolver_units(&layout, (&active, &active_ids), &closures, &class_sources);
    let mut resolver = Resolver::new(&paths, &layout.propaths, units);
    if search.has_root_relative() {
        let written = class_units_with_written_names(&layout, (&active, &active_ids));
        misread_classes(
            outcome,
            (&active_ids, &closures),
            (&class_sources, &written),
            &mut resolver,
            &mut verdicts,
        );
        inherit_misread(
            (&active_ids, &closures),
            &class_sources,
            &resolver,
            &mut verdicts,
        );
    }

    let resolutions = resolve(
        outcome,
        &mut resolver,
        &layout,
        (&active, &active_ids),
        &closures,
        &mut verdicts,
    );

    let mut warnings = Vec::new();
    let classes = workspace_classes(outcome, &layout);
    let mut conflicts: BTreeMap<String, (String, String, usize)> = BTreeMap::new();
    let mut without_key = 0;
    let mut names: BTreeSet<String> = BTreeSet::new();
    let mut rcode: BTreeSet<String> = BTreeSet::new();
    let mut rcode_units = 0;
    for (k, verdict) in verdicts.iter_mut().enumerate() {
        let placed = &layout.units[active[k]];
        if placed.class {
            let path = outcome.name(active_ids[k]);
            // The analysis searches from the class's own directory when its
            // text does not give its name, so a derived package root
            // elsewhere would search other root-relative entries.
            let derived_here = placed
                .derived
                .as_deref()
                .is_some_and(|name| !(search.has_root_relative() && name.contains('.')));
            match placed.declared {
                Declared::Name(_) => {}
                _ if derived_here => {}
                Declared::Conditional => {
                    verdict.add_reason(4, format!("conditional-name: class statement in {path}"));
                }
                Declared::Absent => {
                    verdict.add_reason(4, format!("computed-name: class statement in {path}"));
                }
            }
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
    let wanted: BTreeSet<usize> = (0..active.len())
        .filter(|k| is_observable(&verdicts[*k], &layout.units[active[*k]]))
        .filter_map(|k| layout.units[active[k]].propath)
        .collect();
    let mut class_parts = class_parts(outcome, &paths, &layout, &rules, &classes, &wanted);
    if search.has_root_relative() {
        misread_listed(outcome, &mut class_parts, &mut resolver);
    }
    rcode_listed(outcome, &mut class_parts, &mut resolver);
    let unhashed: BTreeSet<String> = class_parts
        .values()
        .flat_map(|part| &part.files)
        .map(|id| outcome.name(*id))
        .filter(|name| name.starts_with("$DLC/") && !dlc_files.contains(*name))
        .map(str::to_string)
        .collect();
    if !unhashed.is_empty() {
        let wanted: Vec<String> = unhashed.into_iter().collect();
        let answers: FxHashMap<String, FileDigest> =
            pool.hash_dlc_files(&wanted)?.into_iter().collect();
        let (mismatch, agreed) = compare_mirror(&paths, &answers);
        mirror_mismatch.extend(mismatch);
        dlc_sha.extend(agreed);
    }
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
    let base = KeyBase {
        environment: environment.identity(),
        settings: CompileSettings::lower(build),
        worker: program::key_digest(),
        propaths: propath_digests,
    };

    let keys = parallel::map(&positions, |k| {
        let verdict = &verdicts[*k];
        let at = active[*k];
        let unit = active_ids[*k];
        let Placement::Compiles { name, .. } = &layout.units[at].placement else {
            return Err(Vec::new());
        };
        let Content::Hash(unit_content) = outcome.content_of(unit) else {
            return Err(Vec::new());
        };
        let propath = layout.units[at].propath.ok_or_else(Vec::new)?;
        let mut ids = Vec::new();
        let (files, source) = if verdict.is_keyable() {
            let mut files = Vec::with_capacity(closures[*k].len());
            for id in &closures[*k] {
                let path = outcome.name(*id);
                let (algorithm, hash) = match dlc_sha.get(path) {
                    Some(sha) => (Algorithm::Sha256, *sha),
                    None => match outcome.content_of(*id) {
                        Content::Hash(hash) => (Algorithm::Blake3, hash),
                        _ => return Err(Vec::new()),
                    },
                };
                files.push(ClosureFile {
                    path,
                    algorithm,
                    hash,
                });
            }
            (files, ClosureSource::Parsed)
        } else if is_observable(verdict, &layout.units[at]) {
            let observed = outcome.observed.get(&unit).ok_or_else(Vec::new)?;
            let part = class_parts.get(&propath).ok_or_else(Vec::new)?;
            let (files, closure) = observed_files(
                outcome,
                (unit, observed),
                part,
                &paths,
                &dlc_sha,
                &mirror_mismatch,
            )?;
            ids = closure;
            (files, ClosureSource::Observed)
        } else {
            return Err(Vec::new());
        };
        let unit = (outcome.name(unit), name.as_str(), unit_content);
        Ok((base.key(unit, &files, source, propath), source, ids))
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
            Placement::Compiles { name, output } => UnitKey {
                compile_name: Some(name.clone()),
                output_name: Some(output.clone()),
                propath: placed.propath,
                ..UnitKey::default()
            },
        })
        .collect();
    for (((k, verdict), key), resolution) in
        verdicts.into_iter().enumerate().zip(keys).zip(resolutions)
    {
        let unit = &mut units[active[k]];
        unit.class_resolution = resolution;
        unit.observable = is_observable(&verdict, &layout.units[active[k]]);
        let reasons = verdict.into_reasons();
        match key {
            Ok(((key, parts), source, closure)) => {
                unit.key = Some(key);
                unit.components = Some(parts);
                unit.observed = source == ClosureSource::Observed;
                unit.observed_closure = closure;
            }
            Err(extra) => {
                let differing: Vec<&str> = reasons
                    .iter()
                    .filter_map(|r| r.strip_prefix("per-unit-propath-classes: "))
                    .collect();
                without_key += usize::from(!differing.is_empty());
                names.extend(differing.into_iter().map(str::to_string));
                let bound: Vec<&str> = reasons
                    .iter()
                    .filter_map(|r| r.strip_prefix("class-rcode: "))
                    .collect();
                rcode_units += usize::from(!bound.is_empty());
                rcode.extend(bound.into_iter().map(str::to_string));
                unit.unkeyable = match reasons.is_empty() {
                    true => vec!["unreadable: the unit's own bytes".to_string()],
                    false => reasons.into_iter().chain(extra).collect(),
                };
            }
        }
    }
    if without_key > 0 {
        let shown: Vec<&str> = names.iter().take(3).map(String::as_str).collect();
        let more = match names.len() > shown.len() {
            true => format!(" and {} more", names.len() - shown.len()),
            false => String::new(),
        };
        warnings.push(format!(
            "{without_key} units that use classes are compiled without a key: their PROPATHs resolve a name to different files ({}{more})",
            shown.join("; ")
        ));
    }
    if !rcode.is_empty() {
        let shown: Vec<&str> = rcode.iter().take(3).map(String::as_str).collect();
        let more = match rcode.len() > shown.len() {
            true => format!(" and {} more", rcode.len() - shown.len()),
            false => String::new(),
        };
        warnings.push(format!(
            "{rcode_units} units that use classes are compiled without a key: the compiler binds class r-code that oxabl does not hash ({}{more})",
            shown.join(", ")
        ));
    }
    let class_resolution = (layout.propaths.len() > 1).then(|| summarise(&units));
    let propaths = base
        .propaths
        .iter()
        .map(|propath| propath.iter().map(|(e, d)| (e.clone(), d.text())).collect())
        .collect();
    Ok(Keyed {
        units,
        propaths,
        environment: Some(base.environment),
        environment_record: Some(super::record::record(environment)),
        warnings,
        dlc: dlc_sha,
        base: Some(base),
        classes: class_parts,
        class_resolution,
    })
}

/// What the resolver needs to know about the units that compile: their
/// PROPATH ids and closures, and the class units whose r-code `$OUT` holds.
fn resolver_units<'a>(
    layout: &Layout,
    (active, active_ids): (&[usize], &[u32]),
    closures: &'a [Vec<u32>],
    class_sources: &'a FxHashSet<u32>,
) -> Units<'a> {
    let mut units = Units {
        propath: FxHashMap::default(),
        closure: FxHashMap::default(),
        out: FxHashMap::default(),
        compiles: FxHashSet::default(),
        class_sources,
    };
    for ((at, id), closure) in active.iter().zip(active_ids).zip(closures) {
        let placed = &layout.units[*at];
        if let Some(propath) = placed.propath {
            units.propath.insert(*id, propath);
        }
        units.closure.insert(*id, closure.as_slice());
        let Placement::Compiles { output, .. } = &placed.placement else {
            continue;
        };
        let Some(stem) = output.strip_suffix(".cls").filter(|_| placed.class) else {
            continue;
        };
        units.out.insert(stem.to_string(), *id);
        units.compiles.insert(*id);
    }
    units
}

/// The class resolution of each active unit that is a class or reaches one,
/// by position in `active`, with its K9 and K11 reasons added to its
/// verdict. Kept for the plan only with more than one PROPATH id.
fn resolve(
    outcome: &Outcome,
    resolver: &mut Resolver<'_>,
    layout: &Layout,
    (active, active_ids): (&[usize], &[u32]),
    closures: &[Vec<u32>],
    verdicts: &mut [Verdict],
) -> Vec<Option<ClassResolution>> {
    let several = layout.propaths.len() > 1;
    let mut resolutions = Vec::with_capacity(active.len());
    for (k, (unit, closure)) in active_ids.iter().zip(closures).enumerate() {
        let class_sources = resolver.units().class_sources;
        let reaches_class =
            class_sources.contains(unit) || closure.iter().any(|node| class_sources.contains(node));
        if !reaches_class || layout.units[active[k]].propath.is_none() {
            resolutions.push(None);
            continue;
        }
        let resolution = resolver.unit(outcome, *unit);
        for reason in resolution.rcode_reasons() {
            verdicts[k].add_reason(11, reason);
        }
        for reason in resolution.propath_reasons() {
            verdicts[k].add_reason(9, reason);
        }
        resolutions.push(several.then_some(resolution));
    }
    resolutions
}

/// The class units whose analysis searched what their own compile does.
fn class_units_with_written_names(
    layout: &Layout,
    (active, active_ids): (&[usize], &[u32]),
) -> FxHashSet<u32> {
    active
        .iter()
        .zip(active_ids)
        .filter(|(at, _)| {
            let placed = &layout.units[**at];
            placed.class
                && (placed.declared.name().is_some()
                    || placed
                        .derived
                        .as_deref()
                        .is_some_and(|name| !name.contains('.')))
        })
        .map(|(_, id)| *id)
        .collect()
}

/// K10: a class source in a unit's closure that its compile does not read
/// from `$OUT` is compiled in memory under the unit's PROPATH, while the
/// analysis searched from the class's own package root. Each name of the
/// class that the unit's PROPATH resolves to another file than the analysis
/// did makes the unit unkeyable. A class unit whose analysis did not search
/// what its compile does (`written`) is searched on the unit's PROPATH too.
fn misread_classes(
    outcome: &Outcome,
    (active_ids, closures): (&[u32], &[Vec<u32>]),
    (class_sources, written): (&FxHashSet<u32>, &FxHashSet<u32>),
    resolver: &mut Resolver<'_>,
    verdicts: &mut [Verdict],
) {
    for (k, verdict) in verdicts.iter_mut().enumerate() {
        let unit = active_ids[k];
        let Some(propath) = resolver.units().propath.get(&unit).copied() else {
            continue;
        };
        for source in closures[k]
            .iter()
            .filter(|node| class_sources.contains(node))
        {
            if written.contains(source) && resolver.units().binds_out(*source) {
                continue;
            }
            for misread in resolver.misread(outcome, *source, propath).iter() {
                verdict.add_reason(10, misread.reason());
            }
        }
    }
}

/// K10 of a class unit that a user reads from `$OUT`: the r-code there holds
/// what the class's own compile read in place of the analysed file, and the
/// user's key does not hold it.
fn inherit_misread(
    (active_ids, closures): (&[u32], &[Vec<u32>]),
    class_sources: &FxHashSet<u32>,
    resolver: &Resolver<'_>,
    verdicts: &mut [Verdict],
) {
    let own: FxHashMap<u32, Vec<String>> = active_ids
        .iter()
        .zip(verdicts.iter())
        .filter(|(id, verdict)| {
            resolver.units().compiles.contains(id) && !verdict.reasons(10).is_empty()
        })
        .map(|(id, verdict)| (*id, verdict.reasons(10).to_vec()))
        .collect();
    if own.is_empty() {
        return;
    }
    for (k, verdict) in verdicts.iter_mut().enumerate() {
        for source in closures[k]
            .iter()
            .filter(|node| class_sources.contains(node))
        {
            if !resolver.units().binds_out(*source) {
                continue;
            }
            for reason in own.get(source).into_iter().flatten() {
                verdict.add_reason(10, reason.clone());
            }
        }
    }
}

/// K10 for the classes listed in the class part of each PROPATH id. The
/// units that use a class part compile after every class unit, so a class
/// unit that compiles is read from `$OUT`.
fn misread_listed(
    outcome: &Outcome,
    parts: &mut FxHashMap<usize, ClassPart>,
    resolver: &mut Resolver<'_>,
) {
    let ids: FxHashMap<&str, u32> = outcome
        .names
        .iter()
        .enumerate()
        .map(|(id, name)| (name.as_str(), id as u32))
        .collect();
    for (propath, part) in parts.iter_mut() {
        for class in &part.listing {
            let Some(id) = ids.get(class.as_str()).copied() else {
                continue;
            };
            if resolver.units().compiles.contains(&id) {
                continue;
            }
            for misread in resolver.misread(outcome, id, *propath).iter() {
                part.reasons
                    .push(format!("observed-class: {class}: {}", misread.reason()));
            }
        }
    }
}

/// K11 for the classes listed in the class part of each PROPATH id: a
/// listed class that is not a unit that compiles, and whose r-code the
/// PROPATH finds before its source.
fn rcode_listed(
    outcome: &Outcome,
    parts: &mut FxHashMap<usize, ClassPart>,
    resolver: &mut Resolver<'_>,
) {
    let ids: FxHashMap<&str, u32> = outcome
        .names
        .iter()
        .enumerate()
        .map(|(id, name)| (name.as_str(), id as u32))
        .collect();
    for (propath, part) in parts.iter_mut() {
        for class in &part.listing {
            let id = ids.get(class.as_str()).copied();
            if id.is_some_and(|id| resolver.units().compiles.contains(&id)) {
                continue;
            }
            if let Some(file) = resolver.rcode_before(*propath, class) {
                part.reasons
                    .push(format!("observed-class: {class}: class-rcode: {file}"));
            }
        }
    }
}

/// The counts over the units that have a class resolution.
fn summarise(units: &[UnitKey]) -> ResolutionSummary {
    let mut summary = ResolutionSummary::default();
    for unit in units {
        let Some(resolution) = &unit.class_resolution else {
            continue;
        };
        summary.units += 1;
        if resolution.uniform {
            summary.uniform += 1;
        }
        if unit.key.is_some() {
            summary.released += 1;
        }
    }
    summary
}

/// The class part of each unit PROPATH of `wanted`.
fn class_parts(
    outcome: &Outcome,
    paths: &PathMap,
    layout: &Layout,
    rules: &Rules<'_>,
    classes: &FxHashMap<String, (String, String)>,
    wanted: &BTreeSet<usize>,
) -> FxHashMap<usize, ClassPart> {
    if wanted.is_empty() {
        return FxHashMap::default();
    }
    let ids: FxHashMap<&str, u32> = outcome
        .names
        .iter()
        .enumerate()
        .map(|(id, name)| (name.as_str(), id as u32))
        .collect();
    let class_units: Vec<&str> = outcome
        .units
        .iter()
        .map(|unit| outcome.name(*unit))
        .filter(|name| is_class_unit(name))
        .collect();
    let context = PartContext {
        outcome,
        paths,
        rules,
        classes,
        ids: &ids,
        class_units: &class_units,
    };
    wanted
        .iter()
        .map(|propath| (*propath, context.part(&layout.propaths[*propath])))
        .collect()
}

/// What the class part of one PROPATH is made from.
struct PartContext<'a> {
    outcome: &'a Outcome,
    paths: &'a PathMap,
    rules: &'a Rules<'a>,
    classes: &'a FxHashMap<String, (String, String)>,
    ids: &'a FxHashMap<&'a str, u32>,
    class_units: &'a [&'a str],
}

impl PartContext<'_> {
    /// Every class the compile could read, each judged by the rules of the
    /// key as if it were the unit: what it reads must be in the key too.
    fn part(&self, propath: &[String]) -> ClassPart {
        let outcome = self.outcome;
        let listing = match class_listing(self.paths, propath, self.class_units.iter().copied()) {
            Ok(listing) => listing,
            Err(why) => {
                return ClassPart {
                    reasons: vec![format!("observed-classes-unlisted: {why}")],
                    ..ClassPart::default()
                };
            }
        };
        let checked = parallel::map_with(
            &listing,
            || Scratch::new(outcome.names.len()),
            |scratch, class| {
                let Some(id) = self
                    .ids
                    .get(class.as_str())
                    .copied()
                    .filter(|id| outcome.analyses.get(*id).is_some())
                else {
                    return Err(vec![format!("observed-class-unanalysed: {class}")]);
                };
                let closure = outcome.after.closure(id, scratch);
                let verdict = self.rules.check(id, &closure, propath);
                let mut reasons: Vec<String> = verdict
                    .accepted
                    .iter()
                    .filter_map(|name| self.classes.get(&name.name.to_ascii_lowercase()))
                    .map(|(spelling, _)| format!("class-conflict: {spelling}"))
                    .collect();
                if !verdict.is_keyable() {
                    reasons.extend(verdict.into_reasons());
                }
                match reasons.is_empty() {
                    true => Ok(std::iter::once(id).chain(closure).collect::<Vec<u32>>()),
                    false => Err(reasons
                        .into_iter()
                        .map(|reason| format!("observed-class: {class}: {reason}"))
                        .collect()),
                }
            },
        );
        let mut part = ClassPart {
            listing,
            ..ClassPart::default()
        };
        for result in checked {
            match result {
                Ok(files) => part.files.extend(files),
                Err(reasons) => part.reasons.extend(reasons),
            }
        }
        part.files.sort_unstable();
        part.files.dedup();
        part
    }
}

/// Whether a closure the compiler reported can key the unit: no rule applies
/// but those that say oxabl cannot read the unit, and the unit is not a class:
/// the closure of a class unit is the input of every user of the class.
fn is_observable(verdict: &Verdict, placed: &Placed) -> bool {
    verdict.is_observable() && !placed.class
}

/// The files of an observed closure with their hashes now, sorted by path
/// bytes, and their ids; or why the closure does not hold now. The closure is
/// the files the compiler read and the class part of the unit PROPATH. Each
/// file must still be there and readable, each path that was absent must
/// still be absent (a file there would shadow an include), the classes
/// listed now must be the ones listed at the compile, and neither the unit nor
/// a file may look encrypted.
fn observed_files<'a>(
    outcome: &'a Outcome,
    (unit, observed): (u32, &Observed),
    part: &ClassPart,
    paths: &PathMap,
    dlc_sha: &FxHashMap<String, [u8; 32]>,
    mirror_mismatch: &FxHashSet<String>,
) -> Result<(Vec<ClosureFile<'a>>, Vec<u32>), Vec<String>> {
    if !part.reasons.is_empty() {
        return Err(part.reasons.clone());
    }
    let mut reasons = Vec::new();
    for id in std::iter::once(&unit).chain(&observed.files) {
        let path = outcome.name(*id);
        if !path.starts_with('$') && crate::observe::looks_encrypted(&paths.absolute(path)) {
            reasons.push(format!("observed-encrypted: {path}"));
        }
    }
    let then: BTreeSet<&str> = observed
        .classes
        .iter()
        .map(|id| outcome.name(*id))
        .collect();
    let now: BTreeSet<&str> = part.listing.iter().map(String::as_str).collect();
    for added in now.difference(&then) {
        reasons.push(format!("observed-classes-changed: +{added}"));
    }
    for removed in then.difference(&now) {
        reasons.push(format!("observed-classes-changed: -{removed}"));
    }
    let mut ids: Vec<u32> = observed.files.iter().chain(&part.files).copied().collect();
    ids.sort_unstable();
    ids.dedup();
    let mut files = Vec::with_capacity(ids.len());
    for id in &ids {
        let path = outcome.name(*id);
        if path.starts_with("$DLC/") {
            match dlc_sha.get(path) {
                Some(sha) if !mirror_mismatch.contains(path) => files.push(ClosureFile {
                    path,
                    algorithm: Algorithm::Sha256,
                    hash: *sha,
                }),
                _ => reasons.push(format!("observed-unreadable: {path}")),
            }
            continue;
        }
        match outcome.content_of(*id) {
            Content::Hash(hash) if !path.is_empty() && !path.starts_with('/') => {
                files.push(ClosureFile {
                    path,
                    algorithm: Algorithm::Blake3,
                    hash,
                })
            }
            Content::Absent => reasons.push(format!("observed-missing: {path}")),
            _ => reasons.push(format!("observed-unreadable: {path}")),
        }
    }
    for id in &observed.absent {
        if outcome.content_of(*id) != Content::Absent {
            reasons.push(format!("observed-shadowed: {}", outcome.name(*id)));
        }
    }
    if !reasons.is_empty() {
        return Err(reasons);
    }
    files.sort_unstable_by(|a, b| a.path.cmp(b.path));
    Ok((files, ids))
}
