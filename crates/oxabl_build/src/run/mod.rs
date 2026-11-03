//! `oxabl build`: plan, compile in dependency order, install, record.

mod context;
mod finish;
mod pass;
mod prepare;
mod record;
pub mod report;
pub mod request;
mod shared;
mod slot;
mod types;

use std::collections::HashSet;
use std::sync::Mutex;
use std::sync::atomic::AtomicU64;
use std::time::Instant;

use rustc_hash::{FxHashMap, FxHashSet};

use self::context::{Ctx, State};
use self::pass::PassEnd;
use self::prepare::{Prepared, prepare};
use self::record::Recorder;
pub use self::report::{BuildReport, Summary, UnitReport, UnitStatus};
pub use self::request::BuildRequest;
use self::types::UnitInfo;
use crate::config::BuildWorkspace;
use crate::error::BuildError;
use crate::events::Event;
use crate::graph::Scratch;
use crate::key::compute::UnitKey;
use crate::output::{OutputLayout, prune, stage};
use crate::paths::propath::{OUT, library_name};
use crate::store::StoredEnvironment;
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
        notes,
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
    let upload = cache.uploader();
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
    let ctx = Ctx {
        root,
        outcome: &outcome,
        infos,
        propaths,
        options,
        xref: layout.xref_mode(),
        debug_list: layout.debug_list.is_some(),
        listing: layout.listing.is_some(),
        limits: Limits {
            unit_timeout: spec.unit_timeout,
            cancel_grace: workspace.config.build.workers.cancel_grace.0,
        },
        cancel: &cancel,
        fail_fast,
        generation: plan.generation,
        weights,
        lost: Mutex::new(FxHashMap::default()),
        next_request: AtomicU64::new(1),
    };
    let mut state = State {
        recorder,
        sink,
        uploader: Some(upload),
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
        end = pass::run(&ctx, &mut state, &mut pool, &targets);
        if end == PassEnd::Complete {
            let mut retry: Vec<u32> = state
                .failed
                .iter()
                .copied()
                .filter(|unit| state.units.get(unit).is_some_and(|s| !s.lost))
                .collect();
            retry.sort_unstable();
            if !retry.is_empty() {
                end = pass::run(&ctx, &mut state, &mut pool, &retry);
            }
        }
    }

    let State {
        recorder,
        mut sink,
        uploader,
        units,
        notes,
        fatal,
        ..
    } = state;
    let offered = uploader
        .map(|uploader| uploader.finish())
        .unwrap_or_default();
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
    report.offered = offered.len();
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
