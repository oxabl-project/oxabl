//! `textDocument/inlayHint`: the database facts that ABL code does not show
//! and the strict database rules read, at the code they are about. The lock
//! a read takes with no lock phrase, the index oxabl expects a search to use,
//! and where a raised buffer scope or an implicit transaction starts and
//! ends. See `docs/design/inlay-hints.md`.
//!
//! A file shows hints only when its configuration enables a rule of
//! `strict.locks`, `strict.transactions` or `strict.access`, and each kind
//! only where its rule is on. An include with a selected context shows the
//! hints of that root. The client turns
//! each kind off on its own with the `inlayHints` object of its
//! initialization options or of `workspace/didChangeConfiguration`
//! (`settings.oxabl.inlayHints`).
//!
//! One worker thread answers the requests in order and sends each answer
//! itself. It holds no database snapshot, so an edit never waits for it and
//! no edit of another file cancels it. The hints of the whole file are kept
//! per file until its text, its include context, the configuration, the
//! schema, a file on disk or the kinds change; a request renders only the
//! hints in its range. A request whose file changed before the worker reached
//! it answers `ContentModified` with no analysis: the client asks again for
//! the new text.

use std::collections::HashMap;
use std::ops::RangeInclusive;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use lsp_server::{ErrorCode, Message, Request, RequestId, Response};
use lsp_types::{
    InlayHint, InlayHintLabel, InlayHintParams, InlayHintTooltip, PositionEncodingKind, Uri,
};
use oxabl_daemon::db::HintSource;
use oxabl_pipeline::HintKinds;
use serde_json::Value;

use crate::Server;
use crate::position::{byte_to_position, position_to_byte};

/// What the client said about inlay hints.
pub(crate) struct Options {
    /// The client takes `textDocument/inlayHint`.
    pub supported: bool,
    /// The client takes `workspace/inlayHint/refresh`.
    pub refresh: bool,
    /// The kinds the client wants.
    pub kinds: HintKinds,
    /// Refresh requests sent, for their ids.
    refreshes: u64,
    shared: Arc<Shared>,
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
            refreshes: 0,
            shared: Arc::default(),
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
        if kinds != self.inlay.kinds {
            self.inlay.kinds = kinds;
            self.refresh_inlay_hints();
        }
    }

    /// Ask the client to ask again: the hints of a file may change without an
    /// edit to it (the configuration, the schema, an include).
    pub(crate) fn refresh_inlay_hints(&mut self) {
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
        let (Some(path), true, false) = (path, self.inlay.supported, kinds.none()) else {
            return self.respond(none());
        };
        let Some(doc) = self.documents.get(&uri) else {
            return self.respond(none());
        };
        let root = self.contexts.selected(&uri).cloned();
        let Some((config, generations, overlay)) = self.host.with(|sessions| {
            let session = sessions.get(&self.session_root)?;
            Some((
                session.database().config().clone(),
                (session.config_generation(), session.schema_revision()),
                root.as_ref()
                    .map(|_| (session.buffer_generation(), session.buffer_overlay())),
            ))
        }) else {
            return self.respond(none());
        };
        let rope = doc.rope.clone();
        let encoding = self.encoding.clone();
        let from = position_to_byte(&rope, params.range.start, &encoding) as u32;
        let to = position_to_byte(&rope, params.range.end, &encoding) as u32;
        let (buffers, overlay) = match overlay {
            Some((buffers, overlay)) => (buffers, Some(overlay)),
            None => (0, None),
        };
        let key = Key {
            text: 0,
            context: root.clone(),
            buffers,
            config: generations.0,
            schema: generations.1,
            disk: self.inlay.shared.disk.load(Ordering::SeqCst),
            kinds,
        };
        let job = Job {
            id: req.id.clone(),
            uri,
            version: doc.version,
            source: HintSource {
                config,
                file: path,
                text: Arc::from(doc.text()),
                context: root.zip(overlay),
            },
            key,
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
/// context (and, for one, the generation of the open buffers it reads), the
/// configuration and schema generations, the generation of files on disk,
/// and the kinds.
#[derive(Clone, PartialEq, Eq)]
struct Key {
    text: u64,
    context: Option<PathBuf>,
    buffers: u64,
    config: u64,
    schema: u32,
    disk: u64,
    kinds: HintKinds,
}

/// The hints of a whole file.
type Hints = Arc<Vec<oxabl_pipeline::InlayHint>>;

/// One request, queued for the worker.
struct Job {
    id: RequestId,
    uri: Uri,
    version: i32,
    source: HintSource,
    key: Key,
    range: RangeInclusive<u32>,
    rope: ropey::Rope,
    encoding: PositionEncodingKind,
}

/// What the worker and the server's main loop share.
#[derive(Default)]
pub(crate) struct Shared {
    /// The version of each open file, as the main loop last saw it.
    versions: Mutex<HashMap<Uri, i32>>,
    /// The last hints of each open file, for the whole file.
    cache: Mutex<HashMap<Uri, (Key, Hints)>>,
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
    // The file changed after the request: the client asks again for the new
    // text, so no analysis is spent on the old one.
    if current != Some(job.version) {
        return Response::new_err(
            job.id,
            ErrorCode::ContentModified as i32,
            "oxabl-lsp: an edit replaced this text".to_string(),
        );
    }
    let computed = oxabl_common::catch_panic(|| {
        if !job.source.enabled() {
            return Vec::new();
        }
        let key = Key {
            text: hash(&job.source.text),
            ..job.key.clone()
        };
        let cached = shared
            .cache
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .get(&job.uri)
            .filter(|(k, _)| *k == key)
            .map(|(_, hints)| Arc::clone(hints));
        let hints = cached.unwrap_or_else(|| {
            let hints = Arc::new(job.source.hints(key.kinds));
            shared
                .cache
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .insert(job.uri.clone(), (key, Arc::clone(&hints)));
            hints
        });
        hints
            .iter()
            .filter(|h| job.range.contains(&h.at))
            .cloned()
            .collect()
    });
    match computed {
        Ok(hints) => Response::new_ok(job.id, render(&hints, &job.rope, &job.encoding)),
        Err(panic) => Response::new_err(
            job.id,
            ErrorCode::InternalError as i32,
            format!("oxabl-lsp: the inlay hints failed: {panic}"),
        ),
    }
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
            kind: None,
            text_edits: None,
            tooltip: Some(InlayHintTooltip::String(h.tooltip.clone())),
            padding_left: Some(h.after),
            padding_right: Some(!h.after),
            data: None,
        })
        .collect()
}
