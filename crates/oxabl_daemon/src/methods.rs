//! The public `oxabl/*` query surface (R7, R8, R19, R20, R22, R23).
//!
//! ## Why a cross-file question is refused rather than answered emptily
//!
//! An include reference is resolved against the include paths a configuration
//! names. With none, every include in the workspace fails to resolve, the reverse
//! graph has no edges to hold, and an impact query answers "nothing depends on
//! this" — populated, unflagged, and `Ready`. That answer is not merely
//! incomplete. It is the same shape as the true answer for a file nothing
//! references, so a caller cannot tell the two apart, and the one thing this
//! product must never do is under-report a blast radius while looking confident.
//! There is also nowhere to look for the missing files, so the daemon cannot
//! narrow the gap: the answer is fabricated rather than partial. So the
//! graph-dependent methods refuse, and the refusal names the remedy (R22, KTD13).
//!
//! ## Why only the graph-dependent methods
//!
//! The refusal covers `oxabl/impact`, `oxabl/freshness`, and `oxabl/reindex` —
//! every method whose answer is a report on the dependency graph. `oxabl/reindex`
//! belongs with the other two even though it builds rather than reads: its whole
//! response is the graph's freshness and size, so an unconfigured workspace makes
//! it report `Ready` over a file count that no include contributed to. A client
//! that reindexed and then asked for freshness would be told the workspace is
//! current and then told the question cannot be answered, which is worse than
//! either answer alone.
//!
//! `oxabl/symbolSearch` funnels through the same workspace pass, but its rows come
//! from each file's own semantic model and from the schema, both of which are
//! populated without a single include path; single-file analysis is likewise
//! unaffected (R23). Refusing those would remove an answer that still works, which
//! R22 does not ask for.

use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

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
    AddedFileWalk, FileStamp, SessionGenerations, SessionHost, SupersededPass, WorkspaceProgress,
    WorkspaceSnapshot,
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
    // Checked before the pass, not after it: a pass run here would resolve no
    // include and be discarded, so the refusal would cost a full tree scan to
    // reach.
    if let Some(reason) = cross_file_refusal(&root) {
        return serde_json::to_value(Sourced::<ImpactResponse>::unavailable(reason))
            .map_err(MethodError::internal);
    }
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
    serde_json::to_value(Sourced::Available { value: response }).map_err(MethodError::internal)
}

/// Why this workspace cannot answer a cross-file question, if it cannot (R22).
///
/// ## Why this reads the configuration itself rather than a completed pass
///
/// Both callers refuse *before* running a pass. A pass over an unconfigured
/// workspace scans the whole tree to build a graph with no edges in it, and the
/// answer is thrown away — so reading the configuration first is what keeps a
/// refusal cheap on a path an editor polls.
///
/// [`PipelineConfig::resolve_style_only`] rather than
/// [`PipelineConfig::resolve`](oxabl_pipeline::PipelineConfig::resolve) for the
/// same reason, and only because the question here is exclusively about include
/// paths: both resolvers derive `include_paths` identically, and skipping the
/// schema step means this check opens no `.df` file. Nothing downstream of this
/// function reads `schema` or `schema_loaded`, which is the misuse that
/// resolver's documentation warns about.
fn cross_file_refusal(root: &Path) -> Option<String> {
    let (config, _warnings) = oxabl_pipeline::PipelineConfig::resolve_style_only(
        root,
        &oxabl_pipeline::ConfigOverrides::default(),
    );
    config
        .include_paths
        .is_empty()
        .then(|| NO_INCLUDE_CONFIGURATION.to_string())
}

/// What a refused cross-file question says: what was refused, why, and the remedy.
///
/// One constant, so `oxabl/impact`, `oxabl/freshness`, and `oxabl/reindex` refuse
/// in identical words — a client that renders the reason shows the same remedy
/// whichever method it asked.
const NO_INCLUDE_CONFIGURATION: &str = "no include path resolved for this workspace, so a \
     cross-file answer would come from a dependency graph nothing could populate; name include \
     paths under [workspace.sources] in an oxabl.toml at the workspace root";

fn symbol_search(
    host: &SessionHost,
    context: &mut ClientContext,
    params: serde_json::Value,
) -> Result<serde_json::Value, MethodError> {
    let request: SymbolSearchRequest =
        serde_json::from_value(params).map_err(MethodError::invalid_params)?;
    let workspace = ensure_workspace(host, context.workspace_root()?, false)?;

    let (symbols, total_matches) =
        select_symbols(&workspace.symbols, &request.query, request.limit as usize);

    serde_json::to_value(SymbolSearchResponse {
        symbols,
        total_matches,
    })
    .map_err(MethodError::internal)
}

/// The best `limit` matches for `query`, and how many matched in total (R19).
///
/// Bounded in the work it does, not only in what it returns. A type-ahead query sends
/// a character at a time, so the previous shape — lowercase every name into a fresh
/// `String`, clone every matching row, sort the whole matched set, then throw all but
/// the first few away — charged the caller for the entire symbol table on every
/// keystroke to render fifty rows. Three things changed and none of them is the sort
/// algorithm: the fold stopped allocating, the selection keeps `limit` candidates
/// instead of all of them, and the clone happens after the selection rather than
/// before it.
///
/// **Truncation is reported, never implied.** The returned count is over every match,
/// counted before any bound applies, so a client can say "showing 50 of 900" rather
/// than showing 50 and implying that is all there is. It would have been cheaper to
/// count only what was kept, and it would have made a truncated answer indistinguishable
/// from a complete one — the same failure as an empty result that reads as all-clear.
///
/// Ranking is unchanged: prefix matches order ahead of substring matches, then by
/// folded name, then by id.
fn select_symbols(symbols: &[SymbolRow], query: &str, limit: usize) -> (Vec<SymbolRow>, u32) {
    // Folded once, for the whole request. Every comparison below reads these bytes
    // and folds the candidate a byte at a time, so no row allocates.
    let needle = query.to_ascii_lowercase();
    let needle = needle.as_bytes();

    // The worst-ranked candidate sits at the top, so the heap sheds the row that
    // would not have survived the truncation anyway.
    let mut best: BinaryHeap<Ranked<'_>> = BinaryHeap::with_capacity(limit.min(1024) + 1);
    let mut total_matches: u32 = 0;

    for row in symbols {
        if !contains_folded(&row.name, needle) {
            continue;
        }
        total_matches = total_matches.saturating_add(1);
        if limit == 0 {
            continue;
        }
        best.push(Ranked {
            not_prefix: !starts_with_folded(&row.name, needle),
            row,
        });
        if best.len() > limit {
            best.pop();
        }
    }

    // Ascending, so the best-ranked row is first. Only the rows that survived
    // selection are cloned; the rest were compared in place.
    let selected = best
        .into_sorted_vec()
        .into_iter()
        .map(|ranked| ranked.row.clone())
        .collect();
    (selected, total_matches)
}

/// A matching row and its rank, ordered worst-first so a heap can shed the worst.
///
/// Holds a borrow rather than a clone: the whole point of selecting before cloning is
/// that a single-character query over a large symbol table must not copy every row it
/// happens to match on its way to returning fifty.
struct Ranked<'a> {
    /// `false` for a prefix match, so prefix matches order ahead of substring ones —
    /// the same key the previous implementation built, kept so ranking is unchanged.
    not_prefix: bool,
    row: &'a SymbolRow,
}

impl Ord for Ranked<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.not_prefix
            .cmp(&other.not_prefix)
            .then_with(|| folded_cmp(&self.row.name, &other.row.name))
            .then_with(|| self.row.id.as_str().cmp(other.row.id.as_str()))
    }
}

impl PartialOrd for Ranked<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Ranked<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == std::cmp::Ordering::Equal
    }
}

impl Eq for Ranked<'_> {}

/// Whether `haystack` contains `needle`, which is already ASCII-lowercase.
///
/// Folds a byte at a time rather than lowercasing the haystack into a `String`. The
/// allocation this replaces was per row per request, and it happened three more times
/// per comparison inside the sort — so a type-ahead query over a large symbol table
/// spent most of its time in the allocator rather than in the match.
///
/// ASCII folding, deliberately: it is exactly what `to_ascii_lowercase` did, so no
/// match changes. Progress is an ASCII-cased language and this crate's guidance is
/// that case-insensitive matching folds bytes rather than allocating.
fn contains_folded(haystack: &str, needle: &[u8]) -> bool {
    if needle.is_empty() {
        return true;
    }
    let haystack = haystack.as_bytes();
    if needle.len() > haystack.len() {
        return false;
    }
    haystack
        .windows(needle.len())
        .any(|window| folded_eq(window, needle))
}

/// Whether `haystack` starts with `needle`, which is already ASCII-lowercase.
fn starts_with_folded(haystack: &str, needle: &[u8]) -> bool {
    let haystack = haystack.as_bytes();
    haystack.len() >= needle.len() && folded_eq(&haystack[..needle.len()], needle)
}

fn folded_eq(left: &[u8], right: &[u8]) -> bool {
    left.eq_ignore_ascii_case(right)
}

/// Order two names as their ASCII-lowercase forms would order.
///
/// Byte-for-byte equivalent to comparing `to_ascii_lowercase()` of each, because
/// lowercasing maps each ASCII byte independently and leaves every other byte alone.
fn folded_cmp(left: &str, right: &str) -> std::cmp::Ordering {
    let (left, right) = (left.as_bytes(), right.as_bytes());
    for (left, right) in left.iter().zip(right.iter()) {
        match left.to_ascii_lowercase().cmp(&right.to_ascii_lowercase()) {
            std::cmp::Ordering::Equal => {}
            other => return other,
        }
    }
    left.len().cmp(&right.len())
}

fn freshness(
    host: &SessionHost,
    context: &mut ClientContext,
    params: serde_json::Value,
) -> Result<serde_json::Value, MethodError> {
    let _: FreshnessRequest = no_argument_params(params)?;
    let root = context.workspace_root()?;
    // Refused before the pass is claimed, so an unconfigured workspace does not
    // rescan its tree once per poll to report on a graph it cannot populate.
    if let Some(reason) = cross_file_refusal(root) {
        return serde_json::to_value(Sourced::<FreshnessResponse>::unavailable(reason))
            .map_err(MethodError::internal);
    }
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
    serde_json::to_value(Sourced::Available { value: response }).map_err(MethodError::internal)
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
    let root = context.workspace_root()?.to_path_buf();
    // Refused before the pass, like its two siblings. A reindex over a workspace
    // that resolves no include path would scan the whole tree to build a graph with
    // no edges, then report that graph as `Ready` over a populated file count — the
    // confident-but-fabricated answer R22 exists to remove.
    if let Some(reason) = cross_file_refusal(&root) {
        return serde_json::to_value(Sourced::<ReindexResponse>::unavailable(reason))
            .map_err(MethodError::internal);
    }
    let workspace = ensure_workspace(host, &root, true)?;
    serde_json::to_value(Sourced::Available {
        value: ReindexResponse {
            freshness: workspace_freshness(&workspace),
            pass_millis: workspace.pass_millis,
            graph_bytes: workspace.graph_bytes,
        },
    })
    .map_err(MethodError::internal)
}

/// Return a graph at the current buffer generation. Disk changes never cause an
/// automatic rebuild; they only make the result stale until `oxabl/reindex`.
///
/// # Why this terminates
///
/// A pass the session moved under installs nothing and the loop runs another —
/// see [`superseding_cause`] for what counts as moving. Typing bumps the buffer
/// generation on every keystroke, so an unbounded loop rescans the whole tree for
/// as long as the user types and the request never returns (R4).
///
/// Two budgets bound it, both of [`MAX_WORKSPACE_PASS_ATTEMPTS`]. Passes this
/// request ran: on the last one [`run_claimed_pass`] cannot answer `Ok(None)` at
/// all — the only arm that returns it is guarded by `!attempt.is_final` — so the
/// final attempt either installs a snapshot and returns it or fails. That is
/// structural, and it is the whole guarantee; the `debug_assert!` at the tail of
/// the loop is a debug-build backstop that catches a later branch breaking the
/// shape, not the thing that stops the loop, because it compiles out in release.
/// The installed snapshot carries the cause that superseded it, so the caller gets
/// a populated answer that says out loud it is behind (R6).
///
/// Times this request parked on somebody else's pass. A park spends no attempt,
/// which is right when the pass being waited on will install — but a pass started
/// by a freshness poll never spends a final attempt, so it discards every
/// superseded result and frees the slot immediately. A client polling in a loop
/// can therefore keep winning the claim race while this request parks, and the
/// attempt budget is never touched. So parks are counted too, and a request that
/// has spent them stops waiting and claims the next pass itself, which puts it
/// back on the bounded path above.
fn ensure_workspace(
    host: &SessionHost,
    root: &Path,
    force: bool,
) -> Result<WorkspaceSnapshot, MethodError> {
    let mut attempts: u32 = 0;
    let mut parks: u32 = 0;
    loop {
        let attempts_before = attempts;
        let mut parked = false;
        let may_park = parks < MAX_WORKSPACE_PASS_ATTEMPTS;
        let prepared = host.with(|sessions| {
            let session = sessions.for_root(root);
            if !force
                && let Some(workspace) = session.workspace()
                && workspace.generations == session.generations()
            {
                // The snapshot travels out of the critical section that checked it.
                // Re-reading it afterwards was a second lock acquisition guarded by
                // an `expect` on a condition the first one had already left behind.
                return WorkspacePreparation::Ready(workspace);
            }
            if may_park && let Some(running) = session.workspace_progress() {
                return WorkspacePreparation::Wait(running);
            }
            // Out of parks: claim the slot from under the running pass rather than
            // wait for a result this request has already watched not arrive. The
            // displaced pass still finishes, still signals its own waiters, and
            // installs or discards as it would have; what it no longer does is keep
            // this request waiting on a slot a poller can re-win forever.
            let progress = session.begin_workspace_pass();
            WorkspacePreparation::Build(
                session.root().to_path_buf(),
                session.buffer_overlay(),
                session.generations(),
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
                parks += 1;
            }
            WorkspacePreparation::Build(root, overlay, generations, progress) => {
                attempts += 1;
                let attempt = PassAttempt {
                    number: attempts,
                    is_final: attempts >= MAX_WORKSPACE_PASS_ATTEMPTS,
                };
                if let Some(workspace) =
                    run_claimed_pass(host, &root, overlay, generations, progress, attempt)?
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
        debug_assert!(
            parks <= MAX_WORKSPACE_PASS_ATTEMPTS,
            "the retry parked {parks} times against a cap of {MAX_WORKSPACE_PASS_ATTEMPTS}"
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
///
/// Owning the slot is not the same as still holding it. Every exit here hands its
/// own `progress` to the session so the retire is identity-checked: this pass may
/// have been displaced while it ran, and clearing then would null the slot of the
/// pass that displaced it, leaving a later caller to read "no pass running" and
/// start a third scan of the same tree. `progress.finish()` needs no such check —
/// it signals through this pass's own `Arc`, so it reaches this pass's waiters and
/// nobody else's.
fn run_claimed_pass(
    host: &SessionHost,
    root: &Path,
    overlay: HashMap<PathBuf, Arc<str>>,
    generations: SessionGenerations,
    progress: WorkspaceProgress,
    attempt: PassAttempt,
) -> Result<Option<WorkspaceSnapshot>, MethodError> {
    let outcome = match build_workspace(root, overlay, generations, &progress) {
        Ok(mut workspace) => {
            progress.complete();
            host.with(|sessions| {
                let session = sessions.for_root(root);
                match superseding_cause(session, generations) {
                    // Superseded with attempts still to spend: install nothing and
                    // let the caller's loop run another pass.
                    Some(_) if !attempt.is_final => {
                        session.clear_workspace_progress(&progress);
                        return Ok(None);
                    }
                    // Superseded on the last attempt. This installs and answers
                    // rather than looping again or failing: an error and an empty
                    // answer both lose the graph the pass did build, and an empty
                    // answer additionally reads as all-clear. The label is what
                    // makes it honest. It frees the progress slot too — but only if
                    // this pass still holds it, since a pass displaced while it ran
                    // would otherwise free the slot of the pass that displaced it.
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
                session.install_workspace(workspace.clone(), &progress);
                Ok(Some(workspace))
            })
        }
        Err(error) => {
            host.with(|sessions| {
                if let Some(session) = sessions.get_mut(root) {
                    session.clear_workspace_progress(&progress);
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
/// the pass ran, which is why none of it can be recovered from the file stamps:
/// the stamps cover the files this pass tracked, and neither the schema source
/// nor the configuration file need be among them — a schema loaded from a `.df`
/// outside the workspace leaves every stamp clean while changing what the pass
/// would have resolved (R3).
///
/// The order is the order of remedies, not of likelihood. When several moved
/// together the caller is told about the one it cannot wait out: a buffer settles
/// when the user stops typing, while a replaced schema or configuration stays
/// replaced until the workspace is indexed under it.
fn superseding_cause(
    session: &crate::Session,
    claimed: SessionGenerations,
) -> Option<StalenessCause> {
    let current = session.generations();
    if current.schema != claimed.schema {
        return Some(StalenessCause::SchemaChanged);
    }
    if current.config != claimed.config {
        return Some(StalenessCause::ConfigurationChanged);
    }
    if current.buffers != claimed.buffers {
        return Some(StalenessCause::BuffersMoved);
    }
    None
}

/// Run a pass claimed by a caller that has no overlay in hand, reading the
/// session's own state for it.
fn run_claimed_workspace_pass(
    host: &SessionHost,
    root: &Path,
    progress: WorkspaceProgress,
) -> Result<Option<WorkspaceSnapshot>, MethodError> {
    let (owned_root, overlay, generations) = host.with(|sessions| {
        let session = sessions.for_root(root);
        (
            session.root().to_path_buf(),
            session.buffer_overlay(),
            session.generations(),
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
        generations,
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
    Build(
        PathBuf,
        HashMap<PathBuf, Arc<str>>,
        SessionGenerations,
        WorkspaceProgress,
    ),
}

fn build_workspace(
    root: &Path,
    overlay: HashMap<PathBuf, Arc<str>>,
    generations: SessionGenerations,
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
        generations,
        pass_millis: started.elapsed().as_millis() as u64,
        graph_bytes,
        root: root.to_path_buf(),
        // The count discovery produced, captured before the pipeline widened it with
        // include targets from outside the tree.
        discovered_files: files.len(),
        added_files: Arc::new(Mutex::new(AddedFileWalk::starting_from(files.len()))),
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
/// The two are not alternatives with the same evidence. A superseded pass leaves
/// every stamped file untouched on disk — the buffers, the schema revision, and
/// the installed configuration all live in the session, not in the tracked files —
/// so the stamp-derived state for it is `Ready`: a populated answer, unflagged,
/// reading as all-clear while it describes source or rules the session has already
/// moved past (R3, R6). The stamps cannot see that, and no amount of file metadata
/// could: the state that moved was the daemon's own. So the carried cause wins
/// outright, and the stamp count is not folded in beside it — a superseded answer
/// that also named a file count would invite the reader to treat the file count as
/// the whole story.
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
                if changed_files > 0 {
                    IndexState::Stale { changed_files }
                } else {
                    // Every stamp is clean, which is exactly when a file the pass
                    // never saw is the only thing left that could make the workspace
                    // stale — it is in no stamp, so no stamp can report it.
                    match added_since_pass(workspace) {
                        AddedFiles::None => IndexState::Ready,
                        AddedFiles::Some(count) => IndexState::Stale {
                            changed_files: count,
                        },
                        // Uncountable is treated as changed, the same way a stamp
                        // whose file cannot be read reports changed: a workspace this
                        // daemon cannot enumerate is one it cannot call `Ready`.
                        AddedFiles::Uncountable => IndexState::Stale { changed_files: 1 },
                    }
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

/// How long the added-file walk is allowed to be reused before it runs again.
///
/// Short enough that a file added in an editor shows up on the next poll or the one
/// after, long enough that a client polling at an interactive rate walks the tree
/// once rather than on every poll. A constant rather than a tuning knob: nothing in
/// the wire contract exposes it, and a knob would have to be explained.
const ADDED_FILE_WALK_INTERVAL: Duration = Duration::from_millis(2_000);

/// What a walk of the workspace says about files the pass never saw.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AddedFiles {
    /// The workspace holds what it held when the pass ran.
    None,
    /// It holds a different number of files. The count is the difference.
    Some(u32),
    /// The workspace could not be walked, so nothing can be concluded.
    Uncountable,
}

/// Whether the workspace has gained (or lost) files since the pass walked it (R5).
///
/// Counts rather than compares sets. The set comparison would be exact, but it would
/// hold a second copy of every path in the workspace for the lifetime of the snapshot
/// to detect a condition whose remedy — rebuild — is the same whichever file moved.
/// A count catches every addition and every removal; what it misses is a
/// simultaneous add and remove of equal size between two polls, and that case is
/// caught by the stamps, because the removed file was stamped and now reads as
/// changed.
///
/// Rate-limited, and the limit is not an optimisation. The gate above admits this
/// walk when every stamp is clean, which is the *idle* state an editor polls through
/// continuously — so without the limit the common case would be a full directory walk
/// per poll, and the gate would be admitting the walk on exactly the path it was
/// supposed to protect.
fn added_since_pass(workspace: &WorkspaceSnapshot) -> AddedFiles {
    let mut walk = workspace
        .added_files
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());

    let counted = match walk.last_ran {
        Some(last) if last.elapsed() < ADDED_FILE_WALK_INTERVAL => walk.last_count,
        _ => {
            let Ok(files) = discover_path(&workspace.root) else {
                // Left un-timestamped on purpose, so the next poll retries rather
                // than reusing an answer this one never got.
                return AddedFiles::Uncountable;
            };
            walk.last_ran = Some(Instant::now());
            walk.last_count = files.len();
            walk.walks += 1;
            files.len()
        }
    };

    match counted.abs_diff(workspace.discovered_files) {
        0 => AddedFiles::None,
        difference => AddedFiles::Some(difference.min(u32::MAX as usize) as u32),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::Sessions;

    /// A workspace with one file, enough to build a real snapshot against.
    fn workspace_root() -> tempfile::TempDir {
        let root = tempfile::tempdir().expect("a workspace");
        std::fs::write(root.path().join("only.p"), "MESSAGE \"only\".\n").expect("a source file");
        root
    }

    /// The selection the previous implementation performed, kept as the oracle.
    ///
    /// Lowercases into fresh `String`s, clones every match, sorts the whole set, then
    /// truncates — exactly what shipped. Kept so "ranking is unchanged" is proved
    /// against the code it replaced rather than against a restatement of it.
    fn select_symbols_by_sorting(
        symbols: &[SymbolRow],
        query: &str,
        limit: usize,
    ) -> (Vec<SymbolRow>, u32) {
        let needle = query.to_ascii_lowercase();
        let mut matched: Vec<SymbolRow> = symbols
            .iter()
            .filter(|row| row.name.to_ascii_lowercase().contains(&needle))
            .cloned()
            .collect();
        matched.sort_by(|left, right| {
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
        let total = matched.len() as u32;
        matched.truncate(limit);
        (matched, total)
    }

    fn symbol(id: &str, name: &str) -> SymbolRow {
        SymbolRow {
            id: id.to_owned(),
            name: name.to_owned(),
            kind: SymbolKind::SharedVariable,
            file: None,
            span: None,
            subject: Subject::Table {
                name: name.to_owned(),
            },
        }
    }

    /// Names chosen to exercise every tie the ranking has to break: prefix against
    /// substring, one folded name against another, and two rows whose folded names are
    /// equal so only the id can separate them.
    fn mixed_case_symbols() -> Vec<SymbolRow> {
        [
            ("v:CustomerName", "CustomerName"),
            ("v:customername", "customername"),
            ("v:CUSTOMER", "CUSTOMER"),
            ("v:OrderCustomer", "OrderCustomer"),
            ("v:custom", "custom"),
            ("v:xCustomerY", "xCustomerY"),
            ("v:unrelated", "unrelated"),
            ("v:Cust", "Cust"),
            ("v:cUsTomerZ", "cUsTomerZ"),
            ("v:zzz", "zzz"),
        ]
        .into_iter()
        .map(|(id, name)| symbol(id, name))
        .collect()
    }

    /// The bounded selection returns exactly what sorting-then-truncating returned,
    /// across every query and every limit the fixture can produce.
    #[test]
    fn the_bounded_selection_matches_the_sort_it_replaced() {
        let symbols = mixed_case_symbols();

        for query in ["", "c", "cust", "CUSTOMER", "customerz", "Z", "nothing"] {
            for limit in [0, 1, 2, 3, 5, 10, 50] {
                assert_eq!(
                    select_symbols(&symbols, query, limit),
                    select_symbols_by_sorting(&symbols, query, limit),
                    "query {query:?} at limit {limit} must rank and bound identically"
                );
            }
        }
    }

    /// A bounded answer says how much it left out, so a short list is never mistaken
    /// for a complete one.
    ///
    /// The cheap version of this selection would count only the rows it kept, and then
    /// a truncated list and a complete list of the same length would be indistinguishable
    /// — the same failure as an empty result that reads as all-clear. The count is taken
    /// over every match, before the bound applies.
    #[test]
    fn a_truncated_answer_reports_how_many_it_left_out() {
        let symbols = mixed_case_symbols();

        let (rows, total) = select_symbols(&symbols, "cust", 2);
        assert_eq!(rows.len(), 2, "the caller asked for two");
        assert!(
            total > rows.len() as u32,
            "the total must exceed the window, or truncation is invisible: {total}"
        );
        assert_eq!(
            total,
            select_symbols(&symbols, "cust", usize::MAX).1,
            "the reported total must be the whole match set, not the window"
        );
    }

    /// A complete answer is distinguishable from a truncated one by the same field.
    #[test]
    fn an_untruncated_answer_reports_a_total_equal_to_what_it_returned() {
        let symbols = mixed_case_symbols();
        let (rows, total) = select_symbols(&symbols, "unrelated", 10);
        assert_eq!(rows.len(), 1);
        assert_eq!(total, 1);
    }

    /// An empty query matches everything, and is bounded by the limit rather than
    /// sorting the whole table — while still reporting the whole table's size.
    #[test]
    fn an_empty_query_is_bounded_by_the_limit() {
        let symbols = mixed_case_symbols();
        let (rows, total) = select_symbols(&symbols, "", 3);

        assert_eq!(rows.len(), 3);
        assert_eq!(total, symbols.len() as u32);
    }

    /// Prefix matches still order ahead of substring matches.
    #[test]
    fn prefix_matches_still_rank_ahead_of_substring_matches() {
        let symbols = mixed_case_symbols();
        let (rows, _) = select_symbols(&symbols, "customer", 3);

        assert!(
            rows.iter()
                .all(|row| row.name.to_ascii_lowercase().starts_with("customer")),
            "the top rows must be the prefix matches, got {:?}",
            rows.iter().map(|row| &row.name).collect::<Vec<_>>()
        );
    }

    /// Names differing only in case are one match, and the fold that decides so does
    /// not allocate per row.
    #[test]
    fn matching_is_case_insensitive_in_both_directions() {
        let symbols = vec![symbol("v:a", "AlphaBeta"), symbol("v:b", "alphabeta")];

        assert_eq!(select_symbols(&symbols, "ALPHABETA", 10).1, 2);
        assert_eq!(select_symbols(&symbols, "phab", 10).1, 2);
        assert_eq!(select_symbols(&symbols, "AlphaBeta", 10).1, 2);
    }

    /// The folding helpers agree with the allocating forms they replaced, including at
    /// the edges a windowed search gets wrong: an empty needle, a needle longer than
    /// the name, and a needle that is the whole name.
    #[test]
    fn the_folding_helpers_agree_with_the_allocating_forms() {
        let names = ["", "a", "Alpha", "alphabet", "ALPHABET", "xAlphaY"];
        let needles = ["", "a", "alpha", "alphabet", "alphabetical", "y"];

        for name in names {
            for needle in needles {
                let lowered = name.to_ascii_lowercase();
                assert_eq!(
                    contains_folded(name, needle.as_bytes()),
                    lowered.contains(needle),
                    "contains: {name:?} / {needle:?}"
                );
                assert_eq!(
                    starts_with_folded(name, needle.as_bytes()),
                    lowered.starts_with(needle),
                    "starts_with: {name:?} / {needle:?}"
                );
                assert_eq!(
                    folded_cmp(name, needle),
                    lowered.cmp(&needle.to_ascii_lowercase()),
                    "cmp: {name:?} / {needle:?}"
                );
            }
        }
    }

    /// A workspace whose graph tracks more files than discovery finds.
    ///
    /// The include target sits *outside* the discovered tree, which is what makes the
    /// tracked set larger than the discovered one — the exact shape the added-file
    /// count has to survive.
    fn workspace_with_an_outside_include() -> (tempfile::TempDir, tempfile::TempDir) {
        let outside = tempfile::tempdir().expect("a directory outside the workspace");
        std::fs::write(
            outside.path().join("shared.i"),
            "DEFINE VARIABLE fromShared AS INTEGER.\n",
        )
        .expect("an include target outside the tree");

        let root = tempfile::tempdir().expect("a workspace");
        std::fs::write(
            root.path().join("oxabl.toml"),
            format!(
                "[workspace]\nname = \"unit\"\n[workspace.sources]\ninclude_paths = [{:?}]\n",
                outside.path()
            ),
        )
        .expect("a configuration naming the outside directory");
        std::fs::write(
            root.path().join("only.p"),
            "{shared.i}\nMESSAGE fromShared.\n",
        )
        .expect("a source file that includes it");
        (root, outside)
    }

    /// A pass's snapshot, with the rate limit on the added-file walk expired.
    ///
    /// Expired rather than slept through: the interval is a product value measured in
    /// seconds, and a test that waited it out would trade determinism for two seconds
    /// per assertion. Clearing the timestamp is exactly what elapsing it does.
    fn expire_added_file_walk(workspace: &WorkspaceSnapshot) {
        let mut walk = workspace
            .added_files
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        walk.last_ran = None;
    }

    /// How many directory walks this snapshot has run.
    fn walks(workspace: &WorkspaceSnapshot) -> u64 {
        workspace
            .added_files
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .walks
    }

    /// A pass over `root`, claimed and completed against a quiet session.
    fn pass(root: &Path) -> WorkspaceSnapshot {
        let host = SessionHost::new();
        let generations = host.with(|sessions| sessions.for_root(root).generations());
        build_workspace(
            root,
            HashMap::new(),
            generations,
            &WorkspaceProgress::default(),
        )
        .expect("a pass over the fixture")
    }

    /// A workspace that gained a file is stale, and no stamp could ever say so (R5).
    ///
    /// Freshness was computed entirely from stamps taken over the files the *previous*
    /// pass saw. A file created since then is in no stamp, so every stamp stayed clean
    /// and a workspace that had grown reported `Ready` — populated, unflagged, and
    /// confidently wrong. This fails on every filesystem, every time, and both
    /// pre-existing staleness tests missed it because both mutate an existing file's
    /// length.
    #[test]
    fn a_file_added_after_the_pass_is_reported_stale() {
        let root = workspace_root();
        let root = root.path();
        let workspace = pass(root);

        assert_eq!(
            workspace_freshness(&workspace).state,
            IndexState::Ready,
            "the workspace starts current"
        );

        std::fs::write(root.join("newcomer.p"), "MESSAGE \"added\".\n").expect("a new file");
        expire_added_file_walk(&workspace);

        match workspace_freshness(&workspace).state {
            IndexState::Stale { changed_files } => assert_eq!(
                changed_files, 1,
                "one file was added, and the count must say so rather than be empty"
            ),
            other => panic!("a workspace that gained a file is not current, got {other:?}"),
        }
    }

    /// The walk is rate-limited, because the condition that admits it is the idle
    /// steady state rather than a rare one.
    ///
    /// Without the limit an editor polling freshness would walk the whole tree on every
    /// poll: the gate runs the walk only when every stamp is clean, and every stamp
    /// clean is precisely what an untouched workspace looks like. The gate is still the
    /// right condition — an added file only matters when nothing else already marks the
    /// workspace stale — but it bounds nothing on its own.
    #[test]
    fn repeated_freshness_polls_perform_at_most_one_directory_walk() {
        let root = workspace_root();
        let workspace = pass(root.path());

        for _ in 0..8 {
            assert_eq!(workspace_freshness(&workspace).state, IndexState::Ready);
        }

        assert_eq!(
            walks(&workspace),
            1,
            "eight polls in quick succession must walk the tree once"
        );
    }

    /// A workspace already known to be stale is not walked: the answer is settled, and
    /// a directory walk could only confirm it more expensively.
    #[test]
    fn the_directory_walk_does_not_run_when_a_stamp_is_already_dirty() {
        let root = workspace_root();
        let root = root.path();
        let workspace = pass(root);

        std::fs::write(root.join("only.p"), "MESSAGE \"changed and longer\".\n")
            .expect("a changed file");

        assert!(matches!(
            workspace_freshness(&workspace).state,
            IndexState::Stale { .. }
        ));
        assert_eq!(
            walks(&workspace),
            0,
            "a workspace already known to be stale must not pay for a directory walk"
        );
    }

    /// A workspace that uses include files is `Ready` when nothing changed.
    ///
    /// The trap in the added-file check: the graph tracks more files than discovery
    /// found, because include targets can live outside the discovered set. Compared
    /// against the *tracked* count a fresh walk would come up short on every workspace
    /// that uses includes and report `Stale` permanently — a false positive worse than
    /// the missed detection this check exists to fix.
    #[test]
    fn a_workspace_that_uses_includes_is_ready_when_nothing_changed() {
        let (root, _outside) = workspace_with_an_outside_include();
        let workspace = pass(root.path());

        assert!(
            workspace.files.len() > workspace.discovered_files,
            "the fixture must track more files than discovery found, or this proves \
             nothing"
        );
        assert_eq!(workspace_freshness(&workspace).state, IndexState::Ready);
    }

    /// A snapshot installed while superseded must not be served by every later
    /// query (R3).
    ///
    /// The pass that lands on the final attempt is labelled, which the cap tests
    /// pin — but it is also *stored*, and [`ensure_workspace`] decides on every
    /// non-forcing query whether the stored one is still current. Judged by the
    /// buffer counter alone that comparison matched forever after a schema change:
    /// no buffer moved, so the snapshot kept looking current and the workspace was
    /// never rebuilt under the schema that replaced it — the answer served
    /// indefinitely under rules the session had already dropped.
    ///
    /// Built here rather than raced through a mutator thread on purpose. A thread
    /// that keeps bumping the schema also keeps clearing the stored snapshot, so
    /// the state this defect needs — a stale snapshot stored and then left alone —
    /// is the one ordering such a test cannot pin.
    #[test]
    fn a_stored_snapshot_behind_the_schema_is_rebuilt_by_a_non_forcing_query() {
        let root = workspace_root();
        let root = root.path();
        let host = SessionHost::new();

        let generations = host.with(|sessions| sessions.for_root(root).generations());
        let mut stale = build_workspace(
            root,
            HashMap::new(),
            generations,
            &WorkspaceProgress::default(),
        )
        .expect("a first pass");
        stale.superseded = Some(SupersededPass {
            cause: StalenessCause::SchemaChanged,
            attempts: MAX_WORKSPACE_PASS_ATTEMPTS,
        });
        // The order a superseded install produces: the schema moved while the pass
        // ran, and the pass landed after it with the generations it was claimed at.
        host.with(|sessions| {
            let session = sessions.for_root(root);
            session.bump_schema();
            session.install_workspace(stale, &WorkspaceProgress::default());
        });

        let answer = ensure_workspace(&host, root, false).expect("the query answers");
        assert!(
            answer.superseded.is_none(),
            "the stored snapshot was served again under a schema the session had replaced"
        );
        assert_eq!(
            answer.generations,
            host.with(|sessions: &mut Sessions| sessions.for_root(root).generations()),
            "the rebuilt snapshot is judged against the state the session holds now"
        );
    }

    /// A displaced pass installs its graph but must not retire the slot that
    /// replaced it (R4).
    ///
    /// A caller out of parks claims the slot from under a running pass on purpose,
    /// and the displaced pass keeps going to completion. Installing is right — the
    /// graph is real work. Freeing the slot is not: the replacement is still
    /// scanning, and the next caller would read "no pass running" and start a
    /// second recursive scan concurrent with it, which is the race the slot exists
    /// to prevent.
    #[test]
    fn a_displaced_pass_installs_without_retiring_the_replacement_slot() {
        let root = workspace_root();
        let root = root.path();
        let host = SessionHost::new();

        let (displaced, replacement, generations) = host.with(|sessions| {
            let session = sessions.for_root(root);
            let displaced = session.begin_workspace_pass();
            // The caller that ran out of parks claims the slot from under it.
            let replacement = session.begin_workspace_pass();
            (displaced, replacement, session.generations())
        });

        let workspace = build_workspace(root, HashMap::new(), generations, &displaced)
            .expect("the displaced pass still completes");
        host.with(|sessions| {
            sessions
                .for_root(root)
                .install_workspace(workspace, &displaced);
        });

        host.with(|sessions| {
            let session = sessions.for_root(root);
            assert!(
                session
                    .workspace_progress()
                    .is_some_and(|held| held.is_same_pass(&replacement)),
                "the displaced pass retired the slot the replacement holds"
            );
            assert!(
                session.workspace().is_some(),
                "the displaced pass threw away the graph it had already built"
            );
        });
    }

    /// A caller parked on somebody else's pass must return even when the progress
    /// slot is re-claimed under it forever (R4).
    ///
    /// A park spends no attempt, which is right when the pass being waited on will
    /// install. But a pass a freshness poll starts never spends a final attempt:
    /// it discards a superseded result and frees the slot at once, so a client
    /// polling in a loop keeps re-winning the claim race. The waiter's budget is
    /// never touched, and the request is bounded in passes run while unbounded in
    /// wall clock.
    ///
    /// The competitor here re-occupies the slot the moment it is free and signals
    /// each occupancy finished, which is the poller's behaviour with the timing
    /// taken out: every park wakes at once and finds the slot taken again. Before
    /// parks were counted this loop had no exit, so the assertion is that the call
    /// returns at all.
    #[test]
    fn a_parked_caller_stops_waiting_once_its_parks_are_spent() {
        use std::sync::atomic::{AtomicBool, Ordering};

        let root = workspace_root();
        let root = root.path().to_path_buf();
        let host = SessionHost::new();

        // Occupied before the call under test, so it is certain to park rather than
        // win the slot outright on its first look.
        host.with(|sessions| sessions.for_root(&root).claim_workspace_pass())
            .expect("the slot is free to begin with")
            .finish();

        let stop = Arc::new(AtomicBool::new(false));
        let competitor = {
            let (host, root, stop) = (host.clone(), root.clone(), Arc::clone(&stop));
            std::thread::spawn(move || {
                while !stop.load(Ordering::Relaxed) {
                    host.with(|sessions| {
                        if let Some(progress) = sessions.for_root(&root).claim_workspace_pass() {
                            progress.finish();
                        }
                    });
                }
            })
        };

        // Off this thread, so a request that never returns fails the test rather
        // than hanging it.
        let (sender, receiver) = std::sync::mpsc::channel();
        let parked = {
            let (host, root) = (host.clone(), root.clone());
            std::thread::spawn(move || {
                let _ = sender.send(ensure_workspace(&host, &root, false).map(|_| ()));
            })
        };

        let answered = receiver.recv_timeout(std::time::Duration::from_secs(60));
        stop.store(true, Ordering::Relaxed);
        let _ = competitor.join();
        answered
            .expect("a caller parked behind a re-claimed slot never returned")
            .expect("the pass it finally ran succeeded");
        let _ = parked.join();
    }
}
