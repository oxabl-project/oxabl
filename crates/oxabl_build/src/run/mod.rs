//! `oxabl build`: plan, compile in dependency order, install, record.

mod complete;
mod context;
mod coordinate;
mod fetch;
mod finish;
mod pass;
mod prepare;
mod record;
mod release;
pub mod report;
pub mod request;
mod shared;
mod slot;
mod types;
mod upload;

use std::collections::HashSet;
use std::sync::Mutex;
use std::sync::atomic::AtomicU64;
use std::time::Instant;

use oxabl_common::catch_panic;
use rustc_hash::{FxHashMap, FxHashSet};

use self::context::{Ctx, FetchPlan, Imaging, State, Verify};
use self::pass::PassEnd;
use self::prepare::{Prepared, prepare};
use self::record::Recorder;
pub use self::report::{BuildReport, Setup, Summary, UnitReport, UnitStatus};
pub use self::request::BuildRequest;
use self::types::UnitInfo;
use self::upload::Upload;
use crate::cache::{ArtifactPath, CompileKey, FetchRequest, NotStoredReason, StoreOutcome};
use crate::config::BuildWorkspace;
use crate::error::BuildError;
use crate::events::Event;
use crate::graph::Scratch;
use crate::key::compute::{Keyed, UnitKey};
use crate::observe::{Observer, Places};
use crate::output::{OutputLayout, prune, stage};
use crate::paths::PathMap;
use crate::paths::propath::{OUT, library_name};
use crate::plan::Plan;
use crate::refresh::Outcome;
use crate::store::StoredEnvironment;
use crate::verify::reads::{Reads, trim_slash};
use crate::verify::rehash::Rehasher;
use crate::verify::release::{Releaser, Settings};
use crate::worker::compile::Limits;

/// Compiles what the plan says must compile. The report says how it went; an
/// error is a build that could not run at all.
pub fn build(
    workspace: &BuildWorkspace,
    request: BuildRequest<'_>,
) -> Result<BuildReport, BuildError> {
    let started_at = Instant::now();
    let started_ns = crate::refresh::now_ns();
    let options = compile_options(&workspace.config.build)?;
    let prepared = prepare(workspace, &request)?;
    let BuildRequest {
        fail_fast,
        cancel,
        sink,
        cache,
        ..
    } = request;
    let Prepared {
        spec,
        layout,
        committed,
        started,
        plan,
        mut notes,
        mut setup,
        _locks,
    } = prepared;
    let crate::plan::run::Committed {
        outcome,
        results,
        store,
        ..
    } = committed;
    let crate::worker::Started {
        mut pool,
        environment,
        ..
    } = started;
    let settings = &workspace.settings;
    let root = settings.root.as_path();
    let paths = PathMap::new(root, settings.dlc.as_deref());
    let keys = plan.keys.as_ref().expect("a build computes keys");

    let pruning = Instant::now();
    let mut recorder = Recorder::new(store, root);
    let identity = environment.identity();
    let environment_record = keys.keyed.environment_record.clone().unwrap_or_default();
    recorder.batch().environment = Some(StoredEnvironment {
        identity: *identity.as_bytes(),
        record: environment_record,
        at: crate::plan::time::now_rfc3339(),
    });
    recorder.batch().refreshed = plan.restat.clone();
    let id_of: FxHashMap<&str, u32> = outcome
        .units
        .iter()
        .map(|unit| (outcome.name(*unit), *unit))
        .collect();
    let compiling: FxHashSet<u32> = outcome
        .units
        .iter()
        .copied()
        .filter(|unit| !plan.skipped.iter().any(|path| path == outcome.name(*unit)))
        .collect();
    recorder.batch().deletes = results
        .by_unit
        .keys()
        .filter(|unit| !compiling.contains(unit))
        .copied()
        .collect();
    let owned: HashSet<String> = results
        .outputs
        .iter()
        .filter(|(unit, _)| compiling.contains(unit))
        .flat_map(|(_, rows)| rows.iter().map(|row| row.path.clone()))
        .collect();
    prune::prune(root, &layout.directories(), &owned);
    if let Err(error) = recorder.flush() {
        pool.shutdown();
        return Err(error);
    }
    setup.prune_ms = ms(pruning);

    let mut infos: FxHashMap<u32, UnitInfo> = FxHashMap::default();
    for entry in &plan.rebuild {
        let unit = id_of[entry.path.as_str()];
        let key = keys.get(&entry.path).expect("every unit has a key state");
        infos.insert(unit, info(&layout, &entry.path, key));
    }
    let propaths: Vec<Vec<String>> = keys
        .keyed
        .propaths
        .iter()
        .map(|propath| {
            propath
                .iter()
                .map(|(entry, _)| worker_entry(entry, &layout, &workspace.config.build.libraries))
                .collect()
        })
        .collect();
    let weights = results
        .by_unit
        .iter()
        .map(|(unit, row)| (*unit, row.duration_ms.max(1)))
        .collect();
    let verifying = verification(workspace, &layout, cache.writes(), &mut notes);
    let baseline: Vec<(String, blake3::Hash)> = environment
        .databases
        .iter()
        .map(|database| (database.info.ldb.clone(), database.schema.identity()))
        .collect();
    let imaging = (verifying && !baseline.is_empty()).then(|| Imaging {
        databases: baseline.iter().map(|(ldb, _)| ldb.clone()).collect(),
        timeout: spec.unit_timeout,
    });
    let verify = verifying.then(|| Verify {
        rehasher: Rehasher::new(root, &outcome, &keys.keyed.dlc, spec.unit_timeout),
        reads: Reads::new(
            &outcome.names,
            environment.facts.get("dlc"),
            &spec.worker_root,
            &layout.rcode,
        ),
    });
    let observer = observer(
        &plan,
        &outcome,
        &layout,
        &paths,
        &keys.keyed,
        Facts {
            worker_dlc: environment.facts.get("dlc"),
            worker_root: &spec.worker_root,
            started_ns,
        },
        &mut notes,
    );
    let fetch = fetch_plan(&infos, &settings.state_dir, &spec.build_id);
    let ctx = Ctx {
        root,
        outcome: &outcome,
        infos,
        propaths,
        options,
        xref: private_xref(verifying, &layout),
        observed_xref: private_xref(verifying || observer.is_some(), &layout),
        debug_list: layout.debug_list.is_some(),
        listing: layout.listing.is_some(),
        limits: Limits {
            unit_timeout: spec.unit_timeout,
            cancel_grace: workspace.config.build.workers.cancel_grace.0,
        },
        cancel: &cancel,
        cache: &*cache,
        fetch,
        verify,
        observer,
        user_xref: layout.xref.is_some(),
        imaging,
        fail_fast,
        generation: plan.generation,
        weights,
        lost: Mutex::new(FxHashMap::default()),
        next_request: AtomicU64::new(1),
    };
    let mut state = State {
        recorder,
        sink,
        upload: verifying.then(|| Upload::start(cache.uploader())),
        release: verifying.then(|| {
            let verify = &workspace.config.build.verify;
            Releaser::new(
                baseline,
                Settings {
                    units: verify.batch_units.max(1) as usize,
                    interval: verify.batch_interval.0,
                },
            )
        }),
        units: FxHashMap::default(),
        failed: FxHashSet::default(),
        notes,
        fatal: None,
        scratch: Scratch::new(outcome.names.len()),
    };
    let setup_ms = started_at.elapsed().as_millis() as u64;
    state.emit(&Event::BuildStarted {
        build: &spec.build_id,
        units: outcome.units.len(),
        rebuild: plan.rebuild.len(),
        workers: pool.len(),
    });

    let mut targets: Vec<u32> = ctx.infos.keys().copied().collect();
    targets.sort_unstable();
    let mut end = PassEnd::Complete;
    if !targets.is_empty() {
        end = pass::run(&ctx, &mut state, &mut pool, &targets, true);
        if end == PassEnd::Complete {
            let mut retry: Vec<u32> = state
                .failed
                .iter()
                .copied()
                .filter(|unit| state.units.get(unit).is_some_and(|s| !s.lost))
                .collect();
            retry.sort_unstable();
            if !retry.is_empty() {
                end = pass::run(&ctx, &mut state, &mut pool, &retry, false);
            }
        }
    }

    release::finish(&mut state, &mut pool, end);
    let State {
        recorder,
        mut sink,
        upload,
        release,
        units,
        mut notes,
        fatal,
        ..
    } = state;
    let uploaded = upload.map(Upload::finish).unwrap_or_default();
    if uploaded.broken {
        notes.push("the cache failed while taking a set; no more sets are offered".to_string());
    }
    notes.extend(catch_panic(|| cache.notes()).unwrap_or_default());
    let cache_report = cache_report(
        cache.label(),
        verifying,
        uploaded.pushed,
        &uploaded.outcomes,
        release,
        &mut notes,
    );
    let closed = recorder.close();
    pool.shutdown();
    stage::remove_stale(&settings.state_dir);
    if let Some(error) = fatal {
        return Err(error);
    }
    closed?;
    let mut report = finish::report(
        &plan,
        &id_of,
        units,
        end,
        notes,
        finish::Facts {
            build: spec.build_id.clone(),
            environment: identity.to_hex().to_string(),
            setup_ms,
            setup,
            elapsed_ms: started_at.elapsed().as_millis() as u64,
        },
    );
    report.cache = cache_report;
    if let Some(sink) = &mut sink {
        let summary = &report.summary;
        sink(&Event::Summary {
            compiled: summary.compiled,
            fetched: summary.fetched,
            failed: summary.failed,
            skipped: summary.skipped,
            exit: report.exit,
        });
    }
    Ok(report)
}

pub(crate) fn ms(since: Instant) -> u64 {
    since.elapsed().as_millis() as u64
}

/// The `[build.compile]` pairs for the worker. The worker writes `languages`
/// and `options` into source text, so they hold only plain words.
fn compile_options(
    build: &oxabl_workspace::BuildConfig,
) -> Result<Vec<(String, String)>, BuildError> {
    let pairs = crate::key::options::CompileSettings::lower(build);
    let mut options = Vec::new();
    for (name, value) in pairs.pairs() {
        let plain = value
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || "-_:,.".contains(c));
        if !plain {
            return Err(BuildError::Usage(format!(
                "[build.compile] {name} = \"{value}\" may hold only letters, digits and `-_:,.`"
            )));
        }
        options.push((name.to_string(), value.clone()));
    }
    Ok(options)
}

fn info(layout: &OutputLayout, path: &str, key: &UnitKey) -> UnitInfo {
    let name = key
        .compile_name
        .clone()
        .expect("a compiling unit has a name");
    UnitInfo {
        path: path.to_string(),
        expected: layout.expected(&name),
        name,
        propath: key.propath.expect("a compiling unit has a PROPATH"),
        key: key.key,
        components: key.components,
        observable: key.observable,
        observed: key.observed,
        observed_closure: key.observed_closure.clone(),
    }
}

/// A PROPATH entry as the worker sees it.
fn worker_entry(
    entry: &str,
    layout: &OutputLayout,
    libraries: &std::collections::BTreeMap<String, String>,
) -> String {
    if entry == OUT {
        return layout.rcode.clone();
    }
    match library_name(entry).and_then(|name| libraries.get(name)) {
        Some(path) => path.clone(),
        None => entry.to_string(),
    }
}

/// Whether this build verifies its units: a cache stores sets, the read check
/// is on and the compiler writes the `XREF-XML` that the check reads.
fn verification(
    workspace: &BuildWorkspace,
    layout: &OutputLayout,
    writes: bool,
    notes: &mut Vec<String>,
) -> bool {
    if !writes {
        return false;
    }
    if matches!(layout.xref, Some((_, oxabl_workspace::XrefFormat::Text))) {
        notes.push(
            "no cache entries are written: [build.outputs].xref_format = \"text\" gives no compiler-resolved file names to check"
                .to_string(),
        );
        return false;
    }
    if !workspace.config.build.verify.reads {
        notes.push(
            "no cache entries are written: [build.verify].reads = false turns the read check off"
                .to_string(),
        );
        return false;
    }
    true
}

/// The XREF mode of a request: the user's, or the private `XREF-XML` when
/// something reads it and the user asked for none.
fn private_xref(read: bool, layout: &OutputLayout) -> &'static str {
    match (read, layout.xref.is_some()) {
        (true, false) => "private",
        _ => layout.xref_mode(),
    }
}

/// The closure that a unit's key holds: the one the compiler reported at the
/// last compile, or the one the analysis found.
pub(crate) fn closure_of(
    outcome: &Outcome,
    info: &UnitInfo,
    unit: u32,
    scratch: &mut Scratch,
) -> Vec<u32> {
    match info.observed {
        true => info.observed_closure.clone(),
        false => outcome.after.closure(unit, scratch),
    }
}

/// Where the workers are, and when the build started.
struct Facts<'a> {
    worker_dlc: &'a str,
    worker_root: &'a str,
    started_ns: i64,
}

/// What observes the closure of the units that oxabl cannot read, when the
/// plan compiles one. A text XREF gives no file names the compiler resolved.
fn observer<'a>(
    plan: &Plan,
    outcome: &'a Outcome,
    layout: &'a OutputLayout,
    paths: &'a PathMap,
    keyed: &'a Keyed,
    facts: Facts<'a>,
    notes: &mut Vec<String>,
) -> Option<Observer<'a>> {
    let keys = plan.keys.as_ref()?;
    let observable = plan
        .rebuild
        .iter()
        .filter(|entry| keys.get(&entry.path).is_some_and(|key| key.observable))
        .count();
    if observable == 0 {
        return None;
    }
    if matches!(layout.xref, Some((_, oxabl_workspace::XrefFormat::Text))) {
        notes.push(format!(
            "{observable} units that oxabl cannot read compile at each build: [build.outputs].xref_format = \"text\" gives no file names that the compiler resolved"
        ));
        return None;
    }
    Some(Observer {
        places: Places {
            paths,
            worker_dlc: trim_slash(facts.worker_dlc),
            worker_root: trim_slash(facts.worker_root),
            rcode: &layout.rcode,
        },
        outcome,
        base: keyed.base.as_ref()?,
        classes: &keyed.classes,
        class_units: outcome
            .units
            .iter()
            .map(|unit| outcome.name(*unit))
            .filter(|name| crate::paths::propath::is_class_unit(name))
            .collect(),
        ids: outcome
            .names
            .iter()
            .enumerate()
            .map(|(id, name)| (name.as_str(), id as u32))
            .collect(),
        started_ns: facts.started_ns,
    })
}

/// The lookups of the first pass: every keyable unit of the rebuild set.
fn fetch_plan(
    infos: &FxHashMap<u32, UnitInfo>,
    state_dir: &std::path::Path,
    build_id: &str,
) -> Option<FetchPlan> {
    let mut keyed: Vec<(&u32, &UnitInfo)> = infos
        .iter()
        .filter(|(_, info)| info.key.is_some())
        .collect();
    if keyed.is_empty() {
        return None;
    }
    keyed.sort_by(|a, b| a.1.path.cmp(&b.1.path));
    let mut requests = Vec::with_capacity(keyed.len());
    let mut unit_of = FxHashMap::default();
    for (unit, info) in keyed {
        let key = CompileKey(info.key.expect("filtered on a key"));
        requests.push(FetchRequest {
            key,
            expect: info
                .expected
                .iter()
                .map(|expected| ArtifactPath(expected.staged.clone()))
                .collect(),
        });
        unit_of.insert(key, *unit);
    }
    Some(FetchPlan {
        requests,
        unit_of,
        stage: state_dir.join("stage").join(build_id).join("fetch"),
    })
}

/// What the caches did, and the notes about what verification kept out.
fn cache_report(
    label: &str,
    writes: bool,
    pushed: usize,
    outcomes: &[(CompileKey, StoreOutcome)],
    release: Option<Releaser>,
    notes: &mut Vec<String>,
) -> report::CacheReport {
    let mut cache = report::CacheReport {
        label: label.to_string(),
        writes,
        offered: pushed,
        ..Default::default()
    };
    let mut refused: Vec<(NotStoredReason, usize)> = Vec::new();
    for (_, outcome) in outcomes {
        match outcome {
            StoreOutcome::Stored => cache.stored += 1,
            StoreOutcome::AlreadyPresent => cache.already_present += 1,
            StoreOutcome::NotStored(reason) => {
                match refused.iter_mut().find(|(r, _)| r == reason) {
                    Some((_, count)) => *count += 1,
                    None => refused.push((*reason, 1)),
                }
            }
        }
    }
    cache.not_stored = refused;
    if let Some(release) = release {
        let dropped = release.dropped;
        cache.changed_inputs = dropped.inputs.len();
        cache.schema_dropped = dropped.schema.len();
        if let Some((path, file)) = dropped.inputs.first() {
            notes.push(format!(
                "{} units are not cached because a file changed while they compiled, for example {file} for {path}",
                dropped.inputs.len()
            ));
        }
        if let Some(reason) = dropped.schema_reason {
            notes.push(format!(
                "{reason}: {} units compiled after it are not cached",
                dropped.schema.len()
            ));
        }
    }
    cache
}
