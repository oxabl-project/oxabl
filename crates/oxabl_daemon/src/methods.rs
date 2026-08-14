//! The public `oxabl/*` query surface (R7, R8, R19, R20).

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Instant;

use oxabl_analyze::unresolved_reason_str;
use oxabl_ast::NodeId;
use oxabl_daemon_protocol::{
    AffectedFile, AffectedGroup, ByteSpan, Cause, Freshness, FreshnessRequest, FreshnessResponse,
    ImpactRequest, ImpactResponse, IndexState, Provenance, ReindexRequest, ReindexResponse,
    SchemaIdentity, Sourced, StalenessCause, Subject, SymbolKind, SymbolRow, SymbolSearchRequest,
    SymbolSearchResponse, method,
};
use oxabl_pipeline::{EdgeKind, Expansion, LintPipeline, LintResult, ReverseGraph};
use oxabl_semantic::{SymbolFlags, SymbolKind as SemanticSymbolKind};
use oxabl_workspace::{FileSystem, RealFileSystem, discover_path};

use crate::dispatch::{ClientContext, Dispatch, MethodError};
use crate::session::{
    FileStamp, SessionHost, SupersededPass, WorkspaceProgress, WorkspaceSnapshot,
};

/// Register every non-LSP method. No handler checks the client kind: the daemon
/// exposes the same capability to the editor and desktop clients (KTD5).
pub fn register_methods(dispatch: &mut Dispatch) {
    dispatch.register(method::IMPACT, impact);
    dispatch.register(method::SYMBOL_SEARCH, symbol_search);
    dispatch.register(method::FRESHNESS, freshness);
    dispatch.register(method::REINDEX, reindex);
}

/// Read the params of a method that takes no arguments (R20).
///
/// JSON-RPC lets a caller omit `params` entirely, and the transport reads an
/// omitted member as null. A struct deserializer rejects null, so a well-formed
/// request would be answered with `invalid params` for saying nothing where there
/// was nothing to say. Substituting an empty object first accepts all three
/// spellings — omitted, null, and `{}`.
///
/// Done here rather than by making the request types unit structs:
/// `deserialize_unit_struct` accepts only null and *rejects* `{}`, which is the
/// shape every existing caller sends. That would move the defect rather than fix
/// it.
fn no_argument_params<T: serde::de::DeserializeOwned>(
    params: serde_json::Value,
) -> Result<T, MethodError> {
    let params = match params {
        serde_json::Value::Null => serde_json::Value::Object(serde_json::Map::new()),
        given => given,
    };
    serde_json::from_value(params).map_err(MethodError::invalid_params)
}

fn impact(
    host: &SessionHost,
    context: &mut ClientContext,
    params: serde_json::Value,
) -> Result<serde_json::Value, MethodError> {
    let started = Instant::now();
    let request: ImpactRequest =
        serde_json::from_value(params).map_err(MethodError::invalid_params)?;
    let root = context.workspace_root()?.to_path_buf();
    let workspace = ensure_workspace(host, &root, false)?;
    let subject = pipeline_subject(&request.subject);
    let dependents = workspace.graph.dependents(&subject);

    let groups = dependents
        .kinds()
        .into_iter()
        .map(|kind| AffectedGroup {
            cause: cause(kind),
            files: dependents
                .of_kind(kind)
                .map(|row| AffectedFile {
                    path: row.file.to_string_lossy().into_owned(),
                    span: row.span.map(span),
                })
                .collect(),
        })
        .collect();
    let unresolved = dependents
        .unresolved()
        .iter()
        .map(|row| oxabl_daemon_protocol::UnresolvedReference {
            file: row.file.to_string_lossy().into_owned(),
            cause: cause(row.reference.kind),
            name: row.reference.name.clone(),
            reason: unresolved_reason_str(row.reference.reason).to_string(),
            span: row.reference.span.map(span),
        })
        .collect();
    let rebuild_set = workspace
        .graph
        .rebuild_set(&subject)
        .into_iter()
        .map(|path| path.to_string_lossy().into_owned())
        .collect();
    let (provenance, freshness) = session_stamps(host, &root, &workspace);
    let response = ImpactResponse {
        subject: request.subject,
        groups,
        unresolved,
        direct_reference_count: dependents.files().len() as u32,
        rebuild_set,
        provenance,
        schema: schema_identity(&workspace),
        freshness,
        estimated_build_seconds: Sourced::unavailable("no build daemon supplies this value"),
        query_millis: started.elapsed().as_millis() as u64,
    };
    serde_json::to_value(response).map_err(MethodError::internal)
}

fn symbol_search(
    host: &SessionHost,
    context: &mut ClientContext,
    params: serde_json::Value,
) -> Result<serde_json::Value, MethodError> {
    let request: SymbolSearchRequest =
        serde_json::from_value(params).map_err(MethodError::invalid_params)?;
    let workspace = ensure_workspace(host, context.workspace_root()?, false)?;
    let needle = request.query.to_ascii_lowercase();
    let mut symbols: Vec<SymbolRow> = workspace
        .symbols
        .iter()
        .filter(|row| row.name.to_ascii_lowercase().contains(&needle))
        .cloned()
        .collect();
    symbols.sort_by(|left, right| {
        let left_prefix = !left.name.to_ascii_lowercase().starts_with(&needle);
        let right_prefix = !right.name.to_ascii_lowercase().starts_with(&needle);
        (
            left_prefix,
            left.name.to_ascii_lowercase(),
            left.id.as_str(),
        )
            .cmp(&(
                right_prefix,
                right.name.to_ascii_lowercase(),
                right.id.as_str(),
            ))
    });
    let total_matches = symbols.len() as u32;
    symbols.truncate(request.limit as usize);
    serde_json::to_value(SymbolSearchResponse {
        symbols,
        total_matches,
    })
    .map_err(MethodError::internal)
}

fn freshness(
    host: &SessionHost,
    context: &mut ClientContext,
    params: serde_json::Value,
) -> Result<serde_json::Value, MethodError> {
    let _: FreshnessRequest = no_argument_params(params)?;
    let root = context.workspace_root()?;
    // Reading the state and claiming the pass are one step. Split in two, a second
    // call that arrives before the first spawned thread claims anything reads
    // `None` as well and spawns again — and the loser then exists only to wait for
    // a result it discards.
    let (state, claimed) = host.with(|sessions| {
        let session = sessions.for_root(root);
        let state = session
            .workspace()
            .map(|workspace| {
                let provenance = provenance(session);
                let freshness = workspace_freshness(&workspace);
                (workspace, provenance, freshness)
            })
            .map(EitherFreshness::Ready)
            .or_else(|| session.workspace_progress().map(EitherFreshness::Indexing));
        let claimed = if state.is_none() {
            session.claim_workspace_pass()
        } else {
            None
        };
        (state, claimed)
    });

    // Starting the pass on first query is the feature; racing to start it is not.
    // This call already answers immediately either way — the `None` arm below
    // reports `Indexing` rather than blocking.
    if let Some(progress) = claimed {
        let host = host.clone();
        let root = root.to_path_buf();
        std::thread::Builder::new()
            .name("oxabl-workspace-pass".to_string())
            .spawn(move || {
                if let Err(error) = run_claimed_workspace_pass(&host, &root, progress) {
                    eprintln!("oxabl daemon: workspace pass failed: {error}");
                }
            })
            .map_err(MethodError::internal)?;
    }

    let response = match state {
        Some(EitherFreshness::Ready((workspace, provenance, freshness))) => FreshnessResponse {
            freshness,
            schema: schema_identity(&workspace),
            provenance,
        },
        Some(EitherFreshness::Indexing(progress)) => {
            let (indexed, total) = progress.values();
            FreshnessResponse {
                freshness: Freshness {
                    state: IndexState::Indexing { indexed, total },
                    indexed_files: indexed,
                    unanalysed_files: 0,
                    unresolved_ratio: 0.0,
                    unnameable_edges: 0,
                    last_pass_millis: Sourced::unavailable("no workspace pass has completed"),
                },
                schema: SchemaIdentity {
                    revision: 0,
                    table_count: 0,
                    loaded: false,
                },
                provenance: Provenance::Disk,
            }
        }
        None => FreshnessResponse {
            freshness: Freshness {
                state: IndexState::Indexing {
                    indexed: 0,
                    total: 0,
                },
                indexed_files: 0,
                unanalysed_files: 0,
                unresolved_ratio: 0.0,
                unnameable_edges: 0,
                last_pass_millis: Sourced::unavailable("no workspace pass has completed"),
            },
            schema: SchemaIdentity {
                revision: 0,
                table_count: 0,
                loaded: false,
            },
            provenance: Provenance::Disk,
        },
    };
    serde_json::to_value(response).map_err(MethodError::internal)
}

enum EitherFreshness {
    Ready((WorkspaceSnapshot, Provenance, Freshness)),
    Indexing(WorkspaceProgress),
}

fn reindex(
    host: &SessionHost,
    context: &mut ClientContext,
    params: serde_json::Value,
) -> Result<serde_json::Value, MethodError> {
    let _: ReindexRequest = no_argument_params(params)?;
    let workspace = ensure_workspace(host, context.workspace_root()?, true)?;
    serde_json::to_value(ReindexResponse {
        freshness: workspace_freshness(&workspace),
        pass_millis: workspace.pass_millis,
        graph_bytes: workspace.graph_bytes,
    })
    .map_err(MethodError::internal)
}

/// Return a graph at the current buffer generation. Disk changes never cause an
/// automatic rebuild; they only make the result stale until `oxabl/reindex`.
///
/// # Why this terminates
///
/// A pass whose buffers moved under it installs nothing and the loop runs
/// another. Typing bumps the buffer generation on every keystroke, so an
/// unbounded loop rescans the whole tree for as long as the user types and the
/// request never returns (R4). The bound is
/// [`MAX_WORKSPACE_PASS_ATTEMPTS`] passes: on the last one the pass installs and
/// answers whatever it built, labelled [`StalenessCause::BuffersMoved`], so the
/// caller gets a populated answer that says out loud it is behind (R6).
///
/// Waiting on another caller's pass is not an attempt. That caller carries its
/// own budget and installs when it is spent, so the wait ends on its result
/// rather than on a race this one could lose forever.
fn ensure_workspace(
    host: &SessionHost,
    root: &Path,
    force: bool,
) -> Result<WorkspaceSnapshot, MethodError> {
    let mut attempts: u32 = 0;
    loop {
        let attempts_before = attempts;
        let mut parked = false;
        let prepared = host.with(|sessions| {
            let session = sessions.for_root(root);
            if !force
                && let Some(workspace) = session.workspace()
                && workspace.buffer_generation == session.buffer_generation()
            {
                // The snapshot travels out of the critical section that checked it.
                // Re-reading it afterwards was a second lock acquisition guarded by
                // an `expect` on a condition the first one had already left behind.
                return WorkspacePreparation::Ready(workspace);
            }
            if let Some(running) = session.workspace_progress() {
                return WorkspacePreparation::Wait(running);
            }
            let progress = session.begin_workspace_pass();
            WorkspacePreparation::Build(
                session.root().to_path_buf(),
                session.buffer_overlay(),
                session.buffer_generation(),
                progress,
            )
        });

        match prepared {
            WorkspacePreparation::Ready(workspace) => return Ok(workspace),
            WorkspacePreparation::Wait(running) => {
                // Parked on the running pass rather than polling for it. The loop
                // still re-checks on wake: a spurious wake, and a pass that
                // installed nothing because the buffers moved under it, both need
                // one.
                running.wait_until_finished(WORKSPACE_WAIT_TIMEOUT);
                parked = true;
            }
            WorkspacePreparation::Build(root, overlay, generation, progress) => {
                attempts += 1;
                let attempt = PassAttempt {
                    number: attempts,
                    is_final: attempts >= MAX_WORKSPACE_PASS_ATTEMPTS,
                };
                if let Some(workspace) =
                    run_claimed_pass(host, &root, overlay, generation, progress, attempt)?
                {
                    return Ok(workspace);
                }
            }
        }

        // Forward progress, asserted rather than read off the loop's shape. Every
        // iteration must either return, spend an attempt, or park on a pass
        // somebody else owns; one that did none of those spins. The captured
        // learning from the parser's sync-token loop is that this assertion belongs
        // at the tail of any loop claiming to advance, because the shape is exactly
        // what stops being true when a branch is added later.
        debug_assert!(
            attempts > attempts_before || parked,
            "a workspace-pass iteration neither ran a pass nor waited on one"
        );
        debug_assert!(
            attempts <= MAX_WORKSPACE_PASS_ATTEMPTS,
            "the retry ran {attempts} passes against a cap of {MAX_WORKSPACE_PASS_ATTEMPTS}"
        );
    }
}

/// How many passes one request may run before it answers with the pass it has.
///
/// A constant rather than a deadline, so the bound is the same on a loaded CI
/// runner as on a developer's machine and a test can assert it. Four, because a
/// request that has already rebuilt the workspace three times has lost the race
/// against a typist and a fourth will not win it either — while a caller that
/// merely collided with one stray keystroke still gets a current answer. It also
/// bounds the worst case at four passes of latency, which is the real cost this
/// number buys.
const MAX_WORKSPACE_PASS_ATTEMPTS: u32 = 4;

/// Where one pass sits in its caller's bounded sequence.
#[derive(Clone, Copy)]
struct PassAttempt {
    number: u32,
    /// The caller has no attempt left after this one, so a superseded result is
    /// installed and labelled rather than thrown away (R4, R6).
    is_final: bool,
}

/// Run one pass against a progress slot this caller already claimed, and signal
/// every waiter when it ends.
///
/// Whoever claims the slot owns finishing it. Both claim sites route through here
/// so there is one signal point rather than one per caller, and `Ok(None)` means
/// the pass completed but was superseded before it landed, so nothing was
/// installed and the caller has an attempt left to spend.
fn run_claimed_pass(
    host: &SessionHost,
    root: &Path,
    overlay: HashMap<PathBuf, Arc<str>>,
    generation: u64,
    progress: WorkspaceProgress,
    attempt: PassAttempt,
) -> Result<Option<WorkspaceSnapshot>, MethodError> {
    let outcome = match build_workspace(root, overlay, generation, &progress) {
        Ok(mut workspace) => {
            progress.complete();
            host.with(|sessions| {
                let session = sessions.for_root(root);
                match superseding_cause(session, generation) {
                    // Superseded with attempts still to spend: install nothing and
                    // let the caller's loop run another pass.
                    Some(_) if !attempt.is_final => {
                        session.clear_workspace_progress();
                        return Ok(None);
                    }
                    // Superseded on the last attempt. This installs and answers
                    // rather than looping again or failing: an error and an empty
                    // answer both lose the graph the pass did build, and an empty
                    // answer additionally reads as all-clear. The label is what
                    // makes it honest, and it clears the progress slot, so the next
                    // request starts a pass of its own.
                    Some(cause) => {
                        workspace.superseded = Some(SupersededPass {
                            cause,
                            attempts: attempt.number,
                        });
                    }
                    None => {}
                }
                // The configuration this pass resolved stays local to the pass. It
                // already reached the pipeline that built the snapshot, so nothing
                // here reads it, and installing it would let one client's query
                // replace the configuration another client resolved — silently,
                // because the memoized diagnostics stay valid across the write and
                // the next recompute is the first to use the wrong rules.
                session.install_workspace(workspace.clone());
                Ok(Some(workspace))
            })
        }
        Err(error) => {
            host.with(|sessions| {
                if let Some(session) = sessions.get_mut(root) {
                    session.clear_workspace_progress();
                }
            });
            Err(error)
        }
    };

    // One signal, covering all three outcomes. A waiter not woken on the failure
    // path would hang, which is worse than the polling this replaced — so there is
    // exactly one place it can be forgotten.
    progress.finish();
    outcome
}

/// Why a completed pass is already out of date, if it is.
///
/// One place, so every reason a pass may be superseded is compared together and
/// named the same way, rather than a second discard-and-loop growing beside the
/// first. The session state read here is state the daemon itself changed while
/// the pass ran, which is why none of it can be recovered from the file stamps.
fn superseding_cause(session: &crate::Session, generation: u64) -> Option<StalenessCause> {
    (session.buffer_generation() != generation).then_some(StalenessCause::BuffersMoved)
}

/// Run a pass claimed by a caller that has no overlay in hand, reading the
/// session's own state for it.
fn run_claimed_workspace_pass(
    host: &SessionHost,
    root: &Path,
    progress: WorkspaceProgress,
) -> Result<Option<WorkspaceSnapshot>, MethodError> {
    let (owned_root, overlay, generation) = host.with(|sessions| {
        let session = sessions.for_root(root);
        (
            session.root().to_path_buf(),
            session.buffer_overlay(),
            session.buffer_generation(),
        )
    });
    // Never the final attempt. This pass is started by a freshness poll and
    // nobody is blocked on its result, so a superseded one is dropped rather than
    // installed: the client's next poll claims a fresh pass, and the attempt cap
    // belongs to a request that has to answer.
    run_claimed_pass(
        host,
        &owned_root,
        overlay,
        generation,
        progress,
        PassAttempt {
            number: 1,
            is_final: false,
        },
    )
}

/// How long a waiter parks before re-checking on its own.
///
/// A backstop, not the mechanism. The signal is what wakes a waiter; this only
/// bounds the damage if one is ever missed, turning a hung daemon into a slow
/// re-check.
const WORKSPACE_WAIT_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(5);

enum WorkspacePreparation {
    /// A current snapshot, carried out of the critical section that checked it.
    Ready(WorkspaceSnapshot),
    /// Another pass is already running; wait on its completion signal.
    Wait(WorkspaceProgress),
    Build(PathBuf, HashMap<PathBuf, Arc<str>>, u64, WorkspaceProgress),
}

fn build_workspace(
    root: &Path,
    overlay: HashMap<PathBuf, Arc<str>>,
    buffer_generation: u64,
    progress: &WorkspaceProgress,
) -> Result<WorkspaceSnapshot, MethodError> {
    let started = Instant::now();
    let files = discover_path(root).map_err(MethodError::internal)?;
    progress.set_total(files.len());
    let (config, _warnings) =
        oxabl_pipeline::PipelineConfig::resolve(root, &oxabl_pipeline::ConfigOverrides::default());
    let fs = OverlayFileSystem { overlay };
    let pipeline = LintPipeline::new(&config, &fs).with_known_files(&files);
    let mut symbols = file_symbols(&files);
    let graph = ReverseGraph::build_with(&pipeline, &files, |path, expansion, result| {
        collect_symbols(path, expansion, result, &fs, &mut symbols);
        progress.advance();
    });
    symbols.extend(table_symbols(&config));
    symbols.sort_by(|left, right| left.id.cmp(&right.id));
    symbols.dedup_by(|left, right| left.id == right.id);

    let tracked_files = graph.tracked_files();
    let graph_bytes = graph.estimated_heap_bytes() as u64;
    Ok(WorkspaceSnapshot {
        graph: Arc::new(graph),
        symbols: Arc::new(symbols),
        files: Arc::new(tracked_files.into_iter().map(FileStamp::capture).collect()),
        config: Arc::new(config),
        buffer_generation,
        pass_millis: started.elapsed().as_millis() as u64,
        graph_bytes,
        // Set by whoever installs it, which is the only place that can know
        // whether the session moved while this ran.
        superseded: None,
    })
}

struct OverlayFileSystem {
    overlay: HashMap<PathBuf, Arc<str>>,
}

impl FileSystem for OverlayFileSystem {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        self.overlay
            .get(path)
            .cloned()
            .map(Ok)
            .unwrap_or_else(|| RealFileSystem.read(path))
    }

    fn exists(&self, path: &Path) -> bool {
        self.overlay.contains_key(path) || RealFileSystem.exists(path)
    }
}

fn collect_symbols(
    path: &Path,
    expansion: &Expansion,
    result: &LintResult,
    fs: &dyn FileSystem,
    rows: &mut Vec<SymbolRow>,
) {
    let (Some(expanded), Some(semantic), Ok(source)) =
        (expansion.expanded(), result.semantic(), fs.read(path))
    else {
        return;
    };
    for (id, symbol) in semantic.symbols.iter() {
        if symbol.declaration == NodeId::DUMMY {
            continue;
        }
        let kind = match symbol.kind {
            SemanticSymbolKind::Class => SymbolKind::Class,
            SemanticSymbolKind::Interface => SymbolKind::Interface,
            SemanticSymbolKind::Procedure => SymbolKind::Procedure,
            SemanticSymbolKind::Function => SymbolKind::Function,
            SemanticSymbolKind::Variable
                if symbol.flags.intersects(
                    SymbolFlags::SHARED | SymbolFlags::NEW_SHARED | SymbolFlags::NEW_GLOBAL_SHARED,
                ) =>
            {
                SymbolKind::SharedVariable
            }
            _ => continue,
        };
        let Some(root_span) = expanded.resolve_root_span(oxabl_ast::Span {
            start: symbol.name_span.start,
            end: symbol.name_span.end,
        }) else {
            continue;
        };
        let name = source
            .get(root_span.start as usize..root_span.end as usize)
            .unwrap_or(symbol.name.as_ref())
            .to_string();
        rows.push(SymbolRow {
            id: format!("{}:{}:{}", path.display(), root_span.start, id.raw()),
            name,
            kind,
            file: Some(path.to_string_lossy().into_owned()),
            span: Some(span(root_span)),
            subject: Subject::File {
                path: path.to_string_lossy().into_owned(),
            },
        });
    }
}

fn file_symbols(files: &[PathBuf]) -> Vec<SymbolRow> {
    files
        .iter()
        .map(|path| SymbolRow {
            id: format!("file:{}", path.display()),
            name: path
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .into_owned(),
            kind: SymbolKind::File,
            file: Some(path.to_string_lossy().into_owned()),
            span: None,
            subject: Subject::File {
                path: path.to_string_lossy().into_owned(),
            },
        })
        .collect()
}

fn table_symbols(config: &oxabl_pipeline::PipelineConfig) -> Vec<SymbolRow> {
    config
        .schema
        .tables()
        .map(|(_, table)| {
            let name = table.name.to_string();
            SymbolRow {
                id: format!("table:{name}"),
                name: name.clone(),
                kind: SymbolKind::Table,
                file: None,
                span: None,
                subject: Subject::Table { name },
            }
        })
        .collect()
}

fn session_stamps(
    host: &SessionHost,
    root: &Path,
    workspace: &WorkspaceSnapshot,
) -> (Provenance, Freshness) {
    // `for_root` rather than an `expect` on `get`. The session does exist —
    // a handshake created it and nothing removes one — but that is an argument
    // from the absence of eviction code, not from a type or a lock, and it would
    // stop holding the day sessions are reclaimed. Reaching for the session the
    // ordinary way costs nothing and cannot become a contained panic in a log.
    host.with(|sessions| {
        let session = sessions.for_root(root);
        (provenance(session), workspace_freshness(workspace))
    })
}

fn provenance(session: &crate::Session) -> Provenance {
    if session.editor_clients() == 0 {
        Provenance::Disk
    } else {
        Provenance::WorkingTree {
            editor_clients: session.editor_clients(),
            unsaved_buffers: session.open_buffers(),
        }
    }
}

/// Report how current a snapshot is, preferring what the daemon knows about the
/// pass over what the files say.
///
/// The two are not alternatives with the same evidence. A pass superseded because
/// the buffers moved under it leaves every stamped file untouched on disk, so the
/// stamp-derived state for it is `Ready` — a populated answer, unflagged, reading
/// as all-clear while it describes source the editor has already moved past
/// (R6). The stamps cannot see that, and no amount of file metadata could: the
/// state that moved was the daemon's own. So the carried cause wins outright, and
/// the stamp count is not folded in beside it — a superseded answer that also
/// named a file count would invite the reader to treat the file count as the
/// whole story.
fn workspace_freshness(workspace: &WorkspaceSnapshot) -> Freshness {
    Freshness {
        state: match workspace.superseded {
            Some(superseded) => IndexState::Superseded {
                cause: superseded.cause,
                attempts: superseded.attempts,
            },
            // The stamp sweep runs only here. It cannot change the answer above,
            // and it is a `stat` per tracked file on a path an editor polls.
            None => {
                let changed_files =
                    workspace.files.iter().filter(|file| file.changed()).count() as u32;
                if changed_files == 0 {
                    IndexState::Ready
                } else {
                    IndexState::Stale { changed_files }
                }
            }
        },
        indexed_files: workspace.graph.file_count() as u32,
        unanalysed_files: workspace.graph.unanalysed().len() as u32,
        unresolved_ratio: workspace.graph.unresolved_ratio(),
        unnameable_edges: workspace.graph.unnameable().len() as u32,
        last_pass_millis: Sourced::Available {
            value: workspace.pass_millis,
        },
    }
}

fn schema_identity(workspace: &WorkspaceSnapshot) -> SchemaIdentity {
    SchemaIdentity {
        revision: workspace.config.schema.revision().raw(),
        table_count: workspace.config.schema.len() as u32,
        loaded: workspace.config.schema_loaded,
    }
}

fn pipeline_subject(subject: &Subject) -> oxabl_pipeline::Subject {
    match subject {
        Subject::File { path } => oxabl_pipeline::Subject::file(path),
        Subject::Table { name } => oxabl_pipeline::Subject::table(name),
    }
}

fn cause(kind: EdgeKind) -> Cause {
    match kind {
        EdgeKind::DirectInclude => Cause::DirectInclude,
        EdgeKind::TransitiveInclude => Cause::TransitiveInclude,
        EdgeKind::SchemaTable => Cause::SchemaTable,
        EdgeKind::ClassReference => Cause::Class,
        EdgeKind::ProgramReference => Cause::Program,
        EdgeKind::SharedProducer => Cause::SharedProducer,
    }
}

fn span(value: oxabl_ast::Span) -> ByteSpan {
    ByteSpan {
        start: value.start,
        end: value.end,
    }
}
