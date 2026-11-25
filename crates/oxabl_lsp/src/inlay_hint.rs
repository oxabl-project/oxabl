//! `textDocument/inlayHint`: the database facts that ABL code does not show
//! and the strict database rules read, at the code they are about. The lock
//! a read takes with no lock phrase, the index oxabl expects a search to use,
//! and where a raised buffer scope or an implicit transaction starts and
//! ends. The parameter-name hints before the arguments of a call are in the
//! same answer. See `docs/design/inlay-hints.md` and
//! `docs/design/signature-help.md`.
//!
//! A file shows hints when `diagnostics` is on in `[workspace.features]`. The
//! index kind needs no rule; each other kind shows only where its rule (from
//! `strict.locks`, `strict.transactions` or `strict.access`) is on. An
//! include with a selected context shows the hints of that root. The client turns
//! each kind off on its own with the `inlayHints` object of its
//! initialization options or of `workspace/didChangeConfiguration`
//! (`settings.oxabl.inlayHints`).
//!
//! One worker thread answers the requests in order and sends each answer
//! itself. The database hints hold no database snapshot, so an edit never
//! waits for them and no edit of another file cancels them. The parameter-name
//! hints read other files, so the worker takes a snapshot for them: a write
//! waits until it is dropped, and a write cancels the analysis that holds it.
//! The analysis then runs again on a new snapshot while the text of the file
//! is the same, and a few times at most. The hints of the whole file are kept
//! per file until its text, its include context, the configuration, the
//! schema, a file on disk or the kinds change (the parameter-name hints: any
//! open buffer or file on disk); a request renders only the hints in its
//! range. A request whose file changed before the worker reached it, or while
//! it ran, answers `ContentModified`: the client asks again for the new text.

use std::collections::HashMap;
use std::ops::RangeInclusive;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use lsp_server::{ErrorCode, Message, Request, RequestId, Response};
use lsp_types::{
    InlayHint, InlayHintKind, InlayHintLabel, InlayHintParams, InlayHintTooltip,
    PositionEncodingKind, Uri,
};
use oxabl_daemon::db::{HintSource, compute_parameter_hints};
use oxabl_pipeline::HintKinds;
use serde_json::Value;

use crate::Server;
use crate::position::{byte_to_position, position_to_byte};
use oxabl_daemon::SessionHost;

/// What the client said about inlay hints.
pub(crate) struct Options {
    /// The client takes `textDocument/inlayHint`.
    pub supported: bool,
    /// The client takes `workspace/inlayHint/refresh`.
    pub refresh: bool,
    /// The kinds the client wants.
    pub kinds: HintKinds,
    /// The client wants the parameter-name hints. Off until it says so.
    pub parameters: bool,
    /// Refresh requests sent, for their ids.
    refreshes: u64,
    shared: Arc<Shared>,
    /// The canonical path of each open file, resolved once when it opens:
    /// resolving is a file system call, and an edit asks for it on every key.
    canonical: HashMap<Uri, PathBuf>,
    /// The worker's queue, once started.
    jobs: Option<crossbeam_channel::Sender<Job>>,
}

impl Options {
    pub(crate) fn from_client(
        caps: &lsp_types::ClientCapabilities,
        initialization: Option<&Value>,
    ) -> Self {
        Self {
            supported: caps
                .text_document
                .as_ref()
                .is_some_and(|c| c.inlay_hint.is_some()),
            refresh: caps
                .workspace
                .as_ref()
                .and_then(|c| c.inlay_hint.as_ref())
                .and_then(|c| c.refresh_support)
                .unwrap_or(false),
            kinds: initialization
                .and_then(|o| o.get("inlayHints"))
                .map_or(HintKinds::ALL, |v| kinds_from(v, HintKinds::ALL)),
            parameters: initialization
                .and_then(|o| o.get("inlayHints"))
                .is_some_and(|v| parameters_from(v, false)),
            refreshes: 0,
            shared: Arc::default(),
            canonical: HashMap::new(),
            jobs: None,
        }
    }
}

/// The kinds an `inlayHints` settings object turns on. A key that is absent
/// or not a boolean keeps its value in `current`.
pub(crate) fn kinds_from(value: &Value, current: HintKinds) -> HintKinds {
    let flag = |key: &str, now: bool| value.get(key).and_then(Value::as_bool).unwrap_or(now);
    HintKinds {
        locks: flag("locks", current.locks),
        index: flag("index", current.index),
        scope: flag("scope", current.scope),
        transactions: flag("transactions", current.transactions),
    }
}

/// Whether an `inlayHints` settings object turns the parameter-name hints on.
/// A key that is absent or not a boolean keeps `current`.
pub(crate) fn parameters_from(value: &Value, current: bool) -> bool {
    value
        .get("parameters")
        .and_then(Value::as_bool)
        .unwrap_or(current)
}

impl Server<'_> {
    /// Apply `workspace/didChangeConfiguration`. Settings without an
    /// `inlayHints` object change nothing.
    pub(crate) fn handle_configuration_change(&mut self, params: Value) {
        let settings = &params["settings"];
        let Some(value) = settings
            .get("oxabl")
            .and_then(|s| s.get("inlayHints"))
            .or_else(|| settings.get("inlayHints"))
        else {
            return;
        };
        let kinds = kinds_from(value, self.inlay.kinds);
        let parameters = parameters_from(value, self.inlay.parameters);
        if kinds != self.inlay.kinds || parameters != self.inlay.parameters {
            self.inlay.kinds = kinds;
            self.inlay.parameters = parameters;
            self.refresh_inlay_hints();
        }
    }

    /// Ask the client to ask again: the hints of a file may change without an
    /// edit to it (the configuration, the schema, an include).
    pub(crate) fn refresh_inlay_hints(&mut self) {
        self.refresh_semantic_tokens();
        if self.inlay.supported && self.inlay.refresh {
            self.inlay.refreshes += 1;
            let id = format!("oxabl-inlay-refresh-{}", self.inlay.refreshes);
            let _ = self.connection.sender.send(Message::Request(Request::new(
                id.into(),
                "workspace/inlayHint/refresh".into(),
                Value::Null,
            )));
        }
    }

    pub(crate) fn handle_inlay_hint(&mut self, req: Request) -> anyhow::Result<()> {
        let params: InlayHintParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => {
                return self.respond(Response::new_err(
                    req.id,
                    ErrorCode::InvalidParams as i32,
                    error.to_string(),
                ));
            }
        };
        let uri = params.text_document.uri;
        let kinds = self.inlay.kinds;
        let path = crate::uri_to_path(&uri).filter(|path| {
            oxabl_workspace::is_root_file(path) || oxabl_workspace::is_include_fragment(path)
        });
        let none = || Response::new_ok(req.id.clone(), Vec::<InlayHint>::new());
        let parameters = self.inlay.parameters;
        let (Some(path), true, false) = (path, self.inlay.supported, kinds.none() && !parameters)
        else {
            return self.respond(none());
        };
        let Some(doc) = self.documents.get(&uri) else {
            return self.respond(none());
        };
        let root = self.contexts.selected(&uri).cloned();
        let root_version = root.as_ref().and_then(|r| self.inlay.shared.version_of(r));
        let shared = Arc::clone(&self.inlay.shared);
        let version = doc.version;
        let Some((config, generations, overlay, parameter_work)) = self.host.with(|sessions| {
            let session = sessions.get(&self.session_root)?;
            let generations = (session.config_generation(), session.schema_revision());
            let parameter_work = parameters.then(|| {
                let key = ParameterKey {
                    version,
                    root: root.clone(),
                    root_version,
                    config: generations.0,
                    schema: generations.1,
                    buffers: session.buffer_generation(),
                    disk: shared.disk.load(Ordering::SeqCst),
                };
                let kept = lock(&shared.parameters)
                    .get(&uri)
                    .filter(|(kept, _)| *kept == key)
                    .map(|(_, hints)| Arc::clone(hints));
                match kept {
                    Some(hints) => ParameterWork::Ready(hints),
                    None => ParameterWork::Compute(Box::new(ParameterSource {
                        key,
                        host: self.host.clone(),
                        session_root: self.session_root.clone(),
                        root: root
                            .clone()
                            .unwrap_or_else(|| oxabl_daemon::canonical_path(&path)),
                        file: oxabl_daemon::canonical_path(&path),
                        overlay: session.buffer_overlay(),
                    })),
                }
            });
            Some((
                session.database().config().clone(),
                generations,
                root.as_ref().map(|_| session.buffer_overlay()),
                parameter_work,
            ))
        }) else {
            return self.respond(none());
        };
        let rope = doc.rope.clone();
        let encoding = self.encoding.clone();
        let from = position_to_byte(&rope, params.range.start, &encoding) as u32;
        let to = position_to_byte(&rope, params.range.end, &encoding) as u32;
        let key = Key {
            text: 0,
            context: root.clone(),
            config: generations.0,
            schema: generations.1,
            disk: self.inlay.shared.disk.load(Ordering::SeqCst),
            kinds,
        };
        let job = Job {
            id: req.id.clone(),
            uri,
            version: doc.version,
            root_version,
            source: HintSource {
                config,
                file: path,
                text: Arc::from(doc.text()),
                context: root.zip(overlay),
            },
            key,
            parameters: parameter_work,
            range: from..=to,
            rope,
            encoding,
        };
        match self.inlay.worker(&self.connection.sender) {
            Ok(worker) => {
                if worker.send(job).is_ok() {
                    return Ok(());
                }
                self.inlay.jobs = None;
                self.respond(Response::new_err(
                    req.id,
                    ErrorCode::InternalError as i32,
                    "oxabl-lsp: the inlay hint worker stopped".to_string(),
                ))
            }
            Err(error) => self.respond(Response::new_err(
                req.id,
                ErrorCode::InternalError as i32,
                format!("oxabl-lsp: cannot start the inlay hint worker: {error}"),
            )),
        }
    }

    /// Record the version of an open file, or forget a closed one.
    pub(crate) fn note_inlay_version(&mut self, uri: &Uri, version: Option<i32>) {
        let path = match version {
            Some(_) => self.inlay_path(uri),
            None => self.inlay.canonical.remove(uri),
        };
        if let Some(path) = path {
            self.schedule_inlay_readers(uri, &path);
            let mut paths = self
                .inlay
                .shared
                .paths
                .lock()
                .unwrap_or_else(|e| e.into_inner());
            match version {
                Some(version) => paths.insert(path, version),
                None => paths.remove(&path),
            };
        }
        let mut versions = self
            .inlay
            .shared
            .versions
            .lock()
            .unwrap_or_else(|e| e.into_inner());
        match version {
            Some(version) => {
                versions.insert(uri.clone(), version);
            }
            None => {
                versions.remove(uri);
                self.inlay
                    .shared
                    .cache
                    .lock()
                    .unwrap_or_else(|e| e.into_inner())
                    .remove(uri);
                lock(&self.inlay.shared.parameters).remove(uri);
            }
        }
    }

    /// The canonical path of an open file, resolved on its first use.
    fn inlay_path(&mut self, uri: &Uri) -> Option<PathBuf> {
        if let Some(path) = self.inlay.canonical.get(uri) {
            return Some(path.clone());
        }
        let path = oxabl_daemon::canonical_path(&crate::uri_to_path(uri)?);
        self.inlay.canonical.insert(uri.clone(), path.clone());
        Some(path)
    }

    /// Schedule the debounced analysis of each include whose cached hints
    /// were read in a root that reads `uri`'s file (a nested or sibling
    /// include, or the root). Its due analysis then asks the client again
    /// (see `fire_due_debounces`), as an edit of the root does.
    fn schedule_inlay_readers(&mut self, uri: &Uri, path: &std::path::Path) {
        let readers: Vec<Uri> = self
            .inlay
            .shared
            .cache
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .iter()
            .filter(|(reader, c)| {
                *reader != uri && c.key.context.is_some() && c.read.iter().any(|p| p == path)
            })
            .map(|(reader, _)| reader.clone())
            .collect();
        let now = std::time::Instant::now();
        for reader in readers {
            if self.contexts.selected(&reader).is_some() {
                self.debouncer.schedule(reader, now);
            }
        }
    }

    /// A file on disk changed: an include may now expand to other text.
    pub(crate) fn inlay_disk_changed(&mut self) {
        self.inlay.shared.disk.fetch_add(1, Ordering::SeqCst);
        self.refresh_inlay_hints();
    }
}

/// What a file's hints were computed from: its text, its selected include
/// context, the configuration and schema generations, the generation of
/// files on disk, and the kinds. In a context, the cache entry also holds
/// the files the root's analysis read and the open-buffer text of them.
#[derive(Clone, PartialEq, Eq)]
struct Key {
    text: u64,
    context: Option<PathBuf>,
    config: u64,
    schema: u32,
    disk: u64,
    kinds: HintKinds,
}

/// The hints of a whole file.
type Hints = Arc<Vec<oxabl_pipeline::InlayHint>>;

/// What a file's parameter-name hints were computed from. They read the
/// signatures in other files, so any change of an open buffer or of a file on
/// disk makes them stale.
#[derive(Clone, PartialEq, Eq)]
struct ParameterKey {
    version: i32,
    root: Option<PathBuf>,
    root_version: Option<i32>,
    config: u64,
    schema: u32,
    buffers: u64,
    disk: u64,
}

/// The parameter-name hints of one request: kept from an earlier one, or
/// computed over a snapshot of the session.
enum ParameterWork {
    Ready(Hints),
    Compute(Box<ParameterSource>),
}

struct ParameterSource {
    key: ParameterKey,
    /// The session's snapshot is taken when the worker reaches the request,
    /// not before: a snapshot that waits in the queue would make a write wait
    /// for it.
    host: SessionHost,
    session_root: PathBuf,
    /// The file that is analysed: the selected context, or the file itself.
    root: PathBuf,
    /// The file the hints are for.
    file: PathBuf,
    overlay: HashMap<PathBuf, Arc<str>>,
}

/// How many times the analysis of the parameter-name hints runs for one
/// request, when writes to other files cancel it.
const MAX_ATTEMPTS: u32 = 4;

fn lock<T>(mutex: &Mutex<T>) -> std::sync::MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(|e| e.into_inner())
}

/// One file's cached hints.
struct Cached {
    key: Key,
    /// The files a context's analysis read (canonical), and the fingerprint
    /// of their open-buffer text then.
    read: Vec<PathBuf>,
    overlay: u64,
    hints: Hints,
}

impl Shared {
    fn version_of(&self, path: &std::path::Path) -> Option<i32> {
        self.paths
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .get(path)
            .copied()
    }
}

/// One request, queued for the worker.
struct Job {
    id: RequestId,
    uri: Uri,
    version: i32,
    /// The version of the selected root, when it is open.
    root_version: Option<i32>,
    source: HintSource,
    key: Key,
    /// The parameter-name hints, when the client wants them.
    parameters: Option<ParameterWork>,
    range: RangeInclusive<u32>,
    rope: ropey::Rope,
    encoding: PositionEncodingKind,
}

/// What the worker and the server's main loop share.
#[derive(Default)]
pub(crate) struct Shared {
    /// The version of each open file, as the main loop last saw it.
    versions: Mutex<HashMap<Uri, i32>>,
    /// The version of each open file, by its canonical path.
    paths: Mutex<HashMap<PathBuf, i32>>,
    /// The last hints of each open file, for the whole file.
    cache: Mutex<HashMap<Uri, Cached>>,
    /// The last parameter-name hints of each open file, for the whole file.
    parameters: Mutex<HashMap<Uri, (ParameterKey, Hints)>>,
    /// Bumped on each watched-file change.
    disk: AtomicU64,
}

impl Options {
    /// The queue of the one worker thread, started on first use. One worker
    /// bounds the work to one analysis at a time; a request whose file has
    /// changed since is answered without one.
    fn worker(
        &mut self,
        sender: &crossbeam_channel::Sender<Message>,
    ) -> std::io::Result<crossbeam_channel::Sender<Job>> {
        if let Some(jobs) = &self.jobs {
            return Ok(jobs.clone());
        }
        let (jobs, queue) = crossbeam_channel::unbounded::<Job>();
        let shared = Arc::clone(&self.shared);
        let sender = sender.clone();
        oxabl_common::stack::builder()
            .name("oxabl-inlay-hints".to_string())
            .spawn(move || {
                for job in queue {
                    let response = answer(&shared, job);
                    let _ = sender.send(Message::Response(response));
                }
            })?;
        self.jobs = Some(jobs.clone());
        Ok(jobs)
    }
}

fn answer(shared: &Shared, job: Job) -> Response {
    let current = shared
        .versions
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .get(&job.uri)
        .copied();
    // The file, or the root it is read in, changed after the request: the
    // client asks again for the new text (a refresh follows the debounced
    // analysis of a context), so no analysis is spent on the old one.
    let root_now = job
        .source
        .context
        .as_ref()
        .and_then(|(root, _)| shared.version_of(root));
    if current != Some(job.version) || root_now != job.root_version {
        return Response::new_err(
            job.id,
            ErrorCode::ContentModified as i32,
            "oxabl-lsp: an edit replaced this text".to_string(),
        );
    }
    let cancelled = std::cell::Cell::new(false);
    let computed = oxabl_common::catch_panic(|| {
        if !job.source.enabled() {
            return Vec::new();
        }
        let mut hints: Vec<oxabl_pipeline::InlayHint> = if job.key.kinds.none() {
            Vec::new()
        } else {
            database_hints(shared, &job)
                .iter()
                .filter(|h| job.range.contains(&h.at))
                .cloned()
                .collect()
        };
        if let Some(work) = &job.parameters {
            let mut attempts = 0;
            let kept = loop {
                attempts += 1;
                let kept = parameter_hints(shared, &job, work);
                // A write to another file cancels the analysis. The text of
                // this file is the same, and the client does not ask again
                // after `ContentModified`, so it runs again.
                let same = lock(&shared.versions).get(&job.uri) == Some(&job.version);
                // An edit that came while the analysis ran leaves hints of
                // the old text, even when the analysis finished.
                if !same {
                    break None;
                }
                if kept.is_some() || attempts == MAX_ATTEMPTS {
                    break kept;
                }
            };
            match kept {
                Some(kept) => {
                    hints.extend(kept.iter().filter(|h| job.range.contains(&h.at)).cloned());
                    hints.sort_by_key(|h| h.at);
                }
                None => cancelled.set(true),
            }
        }
        hints
    });
    if cancelled.get() {
        return Response::new_err(
            job.id,
            ErrorCode::ContentModified as i32,
            "oxabl-lsp: an edit replaced this analysis".to_string(),
        );
    }
    match computed {
        Ok(hints) => Response::new_ok(job.id, render(&hints, &job.rope, &job.encoding)),
        Err(panic) => Response::new_err(
            job.id,
            ErrorCode::InternalError as i32,
            format!("oxabl-lsp: the inlay hints failed: {panic}"),
        ),
    }
}

/// The database hints of the whole file, from the cache when its text,
/// context and settings are those of the last request.
fn database_hints(shared: &Shared, job: &Job) -> Hints {
    let key = Key {
        text: hash(&job.source.text),
        ..job.key.clone()
    };
    let cached = lock(&shared.cache)
        .get(&job.uri)
        .filter(|c| c.key == key && c.overlay == job.source.overlay_fingerprint(&c.read))
        .map(|c| Arc::clone(&c.hints));
    cached.unwrap_or_else(|| {
        let (hints, read) = job.source.hints(key.kinds);
        let read: Vec<PathBuf> = read
            .iter()
            .map(|p| oxabl_daemon::canonical_path(p))
            .collect();
        let hints = Arc::new(hints);
        let overlay = job.source.overlay_fingerprint(&read);
        lock(&shared.cache).insert(
            job.uri.clone(),
            Cached {
                key,
                read,
                overlay,
                hints: Arc::clone(&hints),
            },
        );
        hints
    })
}

/// The parameter-name hints of the whole file. `None` when an edit cancelled
/// the analysis. A file whose analysis fails has no hints, which is only a
/// missing decoration.
fn parameter_hints(shared: &Shared, job: &Job, work: &ParameterWork) -> Option<Hints> {
    let uri = &job.uri;
    let source = match work {
        ParameterWork::Ready(hints) => return Some(Arc::clone(hints)),
        ParameterWork::Compute(source) => source,
    };
    let (snapshot, schema) = source.host.with(|sessions| {
        let session = sessions.get(&source.session_root)?;
        Some((session.database().clone(), session.schema_handle()))
    })?;
    oxabl_common::hold_if_injected(
        oxabl_common::panic_sites::LSP_PARAMETER_HINTS,
        &job.source.text,
    );
    let hints = compute_parameter_hints(
        &snapshot,
        schema,
        &source.root,
        &source.file,
        source.overlay.clone(),
    )?;
    let hints = Arc::new(hints.unwrap_or_default());
    lock(&shared.parameters).insert(uri.clone(), (source.key.clone(), Arc::clone(&hints)));
    Some(hints)
}

fn hash(text: &str) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    text.hash(&mut hasher);
    hasher.finish()
}

fn render(
    hints: &[oxabl_pipeline::InlayHint],
    rope: &ropey::Rope,
    encoding: &PositionEncodingKind,
) -> Vec<InlayHint> {
    hints
        .iter()
        .map(|h| InlayHint {
            position: byte_to_position(rope, h.at as usize, encoding),
            label: InlayHintLabel::String(h.label.clone()),
            kind: (h.kind == oxabl_pipeline::InlayKind::Parameter)
                .then_some(InlayHintKind::PARAMETER),
            text_edits: None,
            tooltip: Some(InlayHintTooltip::String(h.tooltip.clone())),
            padding_left: Some(h.after),
            padding_right: Some(!h.after),
            data: None,
        })
        .collect()
}
