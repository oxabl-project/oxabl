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
pub use self::report::{BuildReport, Summary, UnitReport, UnitStatus};
pub use self::request::BuildRequest;
use self::types::UnitInfo;
use crate::cache::{ArtifactPath, CompileKey, FetchRequest, NotStoredReason, StoreOutcome};
use crate::config::BuildWorkspace;
use crate::error::BuildError;
use crate::events::Event;
use crate::graph::Scratch;
use crate::key::compute::UnitKey;
use crate::output::{OutputLayout, prune, stage};
use crate::paths::propath::{OUT, library_name};
use crate::store::StoredEnvironment;
use crate::verify::reads::Reads;
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
    let keys = plan.keys.as_ref().expect("a build computes keys");

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
        user_xref: layout.xref.is_some(),
    });
    let fetch = fetch_plan(&infos, &settings.state_dir, &spec.build_id);
    let ctx = Ctx {
        root,
        outcome: &outcome,
        infos,
        propaths,
        options,
        xref: match (verifying, layout.xref.is_some()) {
            (true, false) => "private",
            _ => layout.xref_mode(),
        },
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
        uploader: verifying.then(|| cache.uploader()),
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
        pushed: 0,
        uploader_broken: false,
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
        uploader,
        release,
        pushed,
        units,
        mut notes,
        fatal,
        ..
    } = state;
    let outcomes = uploader
        .and_then(|uploader| catch_panic(|| uploader.finish()).ok())
        .unwrap_or_default();
    let cache_report = cache_report(
        cache.label(),
        verifying,
        pushed,
        &outcomes,
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
