//! `textDocument/semanticTokens/full`, `/full/delta` and `/range`: a token for
//! each name that oxabl resolves, so an editor colours it by what it is. See
//! `docs/design/semantic-tokens.md`.
//!
//! One worker thread answers the requests in order. A request waits for no
//! lock and holds no database snapshot while it waits: the worker takes the
//! snapshot after it has checked that the document still has the version of
//! the request, and checks again before it answers. A request whose document
//! changed answers `ContentModified` (-32801), so the client asks again, and
//! the server never sends the tokens of one version for another. An edit that
//! comes while the analysis runs cancels it through the database.
//!
//! A name that oxabl does not resolve has no token. When the analysis cannot
//! run (the preprocessing failed) the answer holds no token, which leaves the
//! colours to the grammar of the client.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use lsp_server::{ErrorCode, Message, Request, RequestId, Response};
use lsp_types::{
    PositionEncodingKind, Range, SemanticToken, SemanticTokenType, SemanticTokens,
    SemanticTokensDelta, SemanticTokensDeltaParams, SemanticTokensEdit,
    SemanticTokensFullDeltaResult, SemanticTokensFullOptions, SemanticTokensLegend,
    SemanticTokensOptions, SemanticTokensParams, SemanticTokensRangeParams,
    SemanticTokensRangeResult, SemanticTokensServerCapabilities, Uri,
};
use oxabl_daemon::db::{NavigationQuery, compute_navigation, compute_semantic_tokens};
use oxabl_daemon::{SessionHost, canonical_path};
use oxabl_pipeline::{SemanticKind, SemanticModifiers};
use ropey::Rope;
use serde_json::Value;

use crate::navigation::reads_unsaved_include;
use crate::position::position_to_byte;
use crate::{Server, uri_to_path};

/// What the client said about semantic tokens, and what the user chose.
pub(crate) struct Options {
    /// The client takes `textDocument/semanticTokens`.
    pub supported: bool,
    /// The client takes `workspace/semanticTokens/refresh`.
    refresh: bool,
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
        let options = Options {
            supported: caps
                .text_document
                .as_ref()
                .is_some_and(|c| c.semantic_tokens.is_some()),
            refresh: caps
                .workspace
                .as_ref()
                .and_then(|c| c.semantic_tokens.as_ref())
                .and_then(|c| c.refresh_support)
                .unwrap_or(false),
            refreshes: 0,
            shared: Arc::default(),
            jobs: None,
        };
        if let Some(enabled) = initialization
            .and_then(|o| o.get("semanticTokens"))
            .and_then(enabled_in)
        {
            options.shared.enabled.store(enabled, Ordering::SeqCst);
        }
        options
    }
}

/// The `enable` flag of a `semanticTokens` settings value: a boolean, or an
/// object with a boolean `enable`.
fn enabled_in(value: &Value) -> Option<bool> {
    value
        .as_bool()
        .or_else(|| value.get("enable").and_then(Value::as_bool))
}

/// The capability to advertise to a client that supports semantic tokens.
pub(crate) fn capability() -> SemanticTokensServerCapabilities {
    SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
        work_done_progress_options: Default::default(),
        legend: SemanticTokensLegend {
            token_types: SemanticKind::ALL
                .iter()
                .map(|kind| SemanticTokenType::new(kind.name()))
                .collect(),
            token_modifiers: SemanticModifiers::NAMES
                .iter()
                .map(|name| lsp_types::SemanticTokenModifier::new(name))
                .collect(),
        },
        range: Some(true),
        full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
    })
}

/// What the worker and the server's main loop share.
struct Shared {
    /// The version of each open file, as the main loop last saw it.
    versions: Mutex<HashMap<Uri, i32>>,
    /// The tokens last sent for the whole of each open file, with their id.
    previous: Mutex<HashMap<Uri, Previous>>,
    /// The number of the next result id.
    next_result: AtomicU64,
    /// Whether the user wants the tokens.
    enabled: AtomicBool,
}

impl Default for Shared {
    fn default() -> Self {
        Shared {
            versions: Mutex::default(),
            previous: Mutex::default(),
            next_result: AtomicU64::new(1),
            enabled: AtomicBool::new(true),
        }
    }
}

struct Previous {
    result_id: String,
    data: Arc<Vec<SemanticToken>>,
}

impl Shared {
    fn version_of(&self, uri: &Uri) -> Option<i32> {
        lock(&self.versions).get(uri).copied()
    }
}

fn lock<T>(mutex: &Mutex<T>) -> std::sync::MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(|e| e.into_inner())
}

/// What a request asks for.
enum Ask {
    Full,
    Delta { previous: String },
    Range(Range),
}

/// One request, queued for the worker.
struct Job {
    id: RequestId,
    ask: Ask,
    uri: Uri,
    /// The version of the document at the request.
    version: i32,
    /// The canonical path of the file.
    path: PathBuf,
    /// The analysed root: the file itself, or the context of an include.
    root: PathBuf,
    /// The key of the open buffer of the file when it is its own root.
    buffer_key: Option<String>,
    rope: Rope,
    encoding: PositionEncodingKind,
    host: SessionHost,
    session_root: PathBuf,
}

impl Server<'_> {
    pub(crate) fn handle_semantic_tokens_full(&mut self, req: Request) -> anyhow::Result<()> {
        match serde_json::from_value::<SemanticTokensParams>(req.params) {
            Ok(params) => self.queue_semantic_tokens(req.id, params.text_document.uri, Ask::Full),
            Err(error) => self.respond(invalid(req.id, &error.to_string())),
        }
    }

    pub(crate) fn handle_semantic_tokens_delta(&mut self, req: Request) -> anyhow::Result<()> {
        match serde_json::from_value::<SemanticTokensDeltaParams>(req.params) {
            Ok(params) => self.queue_semantic_tokens(
                req.id,
                params.text_document.uri,
                Ask::Delta {
                    previous: params.previous_result_id,
                },
            ),
            Err(error) => self.respond(invalid(req.id, &error.to_string())),
        }
    }

    pub(crate) fn handle_semantic_tokens_range(&mut self, req: Request) -> anyhow::Result<()> {
        match serde_json::from_value::<SemanticTokensRangeParams>(req.params) {
            Ok(params) => self.queue_semantic_tokens(
                req.id,
                params.text_document.uri,
                Ask::Range(params.range),
            ),
            Err(error) => self.respond(invalid(req.id, &error.to_string())),
        }
    }

    fn queue_semantic_tokens(&mut self, id: RequestId, uri: Uri, ask: Ask) -> anyhow::Result<()> {
        let Some(path) = uri_to_path(&uri) else {
            return self.respond(invalid(id, &format!("{} is not a file", uri.as_str())));
        };
        let Some(document) = self.documents.get(&uri) else {
            return self.respond(invalid(id, &format!("{} is not open", uri.as_str())));
        };
        let path = canonical_path(&path);
        let root = self
            .contexts
            .selected(&uri)
            .cloned()
            .unwrap_or_else(|| path.clone());
        let job = Job {
            id: id.clone(),
            ask,
            version: document.version,
            rope: document.rope.clone(),
            buffer_key: (root == path).then(|| uri.as_str().to_string()),
            uri,
            path,
            root,
            encoding: self.encoding.clone(),
            host: self.host.clone(),
            session_root: self.session_root.clone(),
        };
        match self.semantic.worker(&self.connection.sender) {
            Ok(worker) => {
                if worker.send(job).is_ok() {
                    return Ok(());
                }
                self.semantic.jobs = None;
                self.respond(Response::new_err(
                    id,
                    ErrorCode::InternalError as i32,
                    "oxabl-lsp: the semantic token worker stopped".to_string(),
                ))
            }
            Err(error) => self.respond(Response::new_err(
                id,
                ErrorCode::InternalError as i32,
                format!("oxabl-lsp: cannot start the semantic token worker: {error}"),
            )),
        }
    }

    /// Record the version of an open file, or forget a closed one.
    pub(crate) fn note_semantic_version(&mut self, uri: &Uri, version: Option<i32>) {
        let mut versions = lock(&self.semantic.shared.versions);
        match version {
            Some(version) => {
                versions.insert(uri.clone(), version);
            }
            None => {
                versions.remove(uri);
                lock(&self.semantic.shared.previous).remove(uri);
            }
        }
    }

    /// Apply the `semanticTokens` setting of `workspace/didChangeConfiguration`
    /// and ask the client for the tokens again when it changed.
    pub(crate) fn handle_semantic_tokens_settings(&mut self, params: &Value) {
        let settings = &params["settings"];
        let Some(enabled) = settings
            .get("oxabl")
            .and_then(|s| s.get("semanticTokens"))
            .or_else(|| settings.get("semanticTokens"))
            .and_then(enabled_in)
        else {
            return;
        };
        if self.semantic.shared.enabled.swap(enabled, Ordering::SeqCst) != enabled {
            self.refresh_semantic_tokens();
        }
    }

    /// Ask the client to ask again: the tokens of a file may change without an
    /// edit to it (the configuration, the schema, an include).
    pub(crate) fn refresh_semantic_tokens(&mut self) {
        if self.semantic.supported && self.semantic.refresh {
            self.semantic.refreshes += 1;
            let id = format!("oxabl-semantic-refresh-{}", self.semantic.refreshes);
            let _ = self.connection.sender.send(Message::Request(Request::new(
                id.into(),
                "workspace/semanticTokens/refresh".into(),
                Value::Null,
            )));
        }
    }
}

impl Options {
    /// The queue of the one worker thread, started on first use.
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
            .name("oxabl-semantic-tokens".to_string())
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

fn invalid(id: RequestId, message: &str) -> Response {
    Response::new_err(
        id,
        ErrorCode::InvalidParams as i32,
        format!("oxabl-lsp: {message}"),
    )
}

fn modified(id: RequestId) -> Response {
    Response::new_err(
        id,
        ErrorCode::ContentModified as i32,
        "oxabl-lsp: an edit replaced this text".to_string(),
    )
}

/// How often a cancelled analysis of an unchanged text runs again.
const MAX_RETRIES: u32 = 8;

fn answer(shared: &Shared, job: Job) -> Response {
    // The text is read only to look for a test marker.
    let marked = if oxabl_common::INJECTION_ENABLED {
        job.rope.to_string()
    } else {
        String::new()
    };
    oxabl_common::hold_if_injected(oxabl_common::panic_sites::SEMANTIC_TOKENS, &marked);
    // The document changed after the request: the client asks again for the
    // new text, so no analysis is spent on the old one.
    if shared.version_of(&job.uri) != Some(job.version) {
        return modified(job.id);
    }
    let mut retries = 0;
    let data = loop {
        let computed = oxabl_common::catch_panic(|| {
            oxabl_common::panic_if_injected(oxabl_common::panic_sites::SEMANTIC_TOKENS, &marked);
            if !shared.enabled.load(Ordering::SeqCst) {
                return Computed::Tokens(Vec::new());
            }
            tokens_of(&job, &marked)
        });
        match computed {
            Ok(Computed::Tokens(data)) => break data,
            // A write to another file cancels every snapshot. The text of this
            // file is the same, and the client does not ask again for it, so
            // the analysis runs again on a new snapshot.
            Ok(Computed::Cancelled)
                if retries < MAX_RETRIES && shared.version_of(&job.uri) == Some(job.version) =>
            {
                retries += 1;
            }
            Ok(Computed::Cancelled | Computed::Gone) => return modified(job.id),
            Err(panic) => {
                return Response::new_err(
                    job.id,
                    ErrorCode::InternalError as i32,
                    format!("oxabl-lsp: the semantic tokens failed: {panic}"),
                );
            }
        }
    };
    oxabl_common::hold_if_injected(oxabl_common::panic_sites::SEMANTIC_ANSWER, &marked);
    // An edit that came while the analysis ran, before the cancellation
    // reached it, leaves tokens of the old text.
    if shared.version_of(&job.uri) != Some(job.version) {
        return modified(job.id);
    }
    let value = match job.ask {
        Ask::Range(_) => serde_json::to_value(SemanticTokensRangeResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        })),
        Ask::Full | Ask::Delta { .. } => {
            let result_id = shared
                .next_result
                .fetch_add(1, Ordering::SeqCst)
                .to_string();
            let data = Arc::new(data);
            let previous = lock(&shared.previous).insert(
                job.uri.clone(),
                Previous {
                    result_id: result_id.clone(),
                    data: Arc::clone(&data),
                },
            );
            match (&job.ask, previous) {
                (Ask::Delta { previous: asked }, Some(known)) if known.result_id == *asked => {
                    serde_json::to_value(SemanticTokensFullDeltaResult::TokensDelta(
                        SemanticTokensDelta {
                            result_id: Some(result_id),
                            edits: edits(&known.data, &data),
                        },
                    ))
                }
                _ => serde_json::to_value(SemanticTokens {
                    result_id: Some(result_id),
                    data: data.to_vec(),
                }),
            }
        }
    };
    match value {
        Ok(value) => Response::new_ok(job.id, value),
        Err(error) => Response::new_err(
            job.id,
            ErrorCode::InternalError as i32,
            format!("oxabl-lsp: cannot render the semantic tokens: {error}"),
        ),
    }
}

/// How the analysis of one job ended.
enum Computed {
    /// The tokens, delta-encoded. None when the analysis cannot run.
    Tokens(Vec<SemanticToken>),
    /// The database cancelled the analysis: a write came while it ran.
    Cancelled,
    /// The session of the job no longer exists.
    Gone,
}

fn tokens_of(job: &Job, marked: &str) -> Computed {
    let rope = &job.rope;
    let range = match &job.ask {
        Ask::Range(range) => Some((
            position_to_byte(rope, range.start, &job.encoding) as u32,
            position_to_byte(rope, range.end, &job.encoding) as u32,
        )),
        _ => None,
    };
    let Some((snapshot, schema, overlay, buffer)) = job.host.with(|sessions| {
        let session = sessions.get(&job.session_root)?;
        Some((
            session.database().clone(),
            session.schema_handle(),
            session.buffer_overlay(),
            job.buffer_key
                .as_deref()
                .and_then(|key| session.buffer(key)),
        ))
    }) else {
        return Computed::Gone;
    };
    oxabl_common::hold_if_injected(oxabl_common::panic_sites::SEMANTIC_SNAPSHOT, marked);
    let computed = match buffer {
        Some(buffer) if !reads_unsaved_include(&snapshot, buffer, &job.root, &overlay) => {
            compute_semantic_tokens(&snapshot, buffer, schema, range)
        }
        _ => compute_navigation(
            &snapshot,
            schema,
            &job.root,
            overlay,
            &NavigationQuery::SemanticTokens {
                file: job.path.clone(),
                range,
            },
        ),
    };
    let Some(answer) = computed else {
        return Computed::Cancelled;
    };
    let Ok(answer) = answer else {
        return Computed::Tokens(Vec::new());
    };
    let Some(text) = answer.texts.into_values().next() else {
        return Computed::Tokens(Vec::new());
    };
    let rope = Rope::from_str(&text);
    Computed::Tokens(encode(&answer.tokens, &rope, &text, &job.encoding))
}

/// The tokens as the protocol sends them: each relative to the one before.
/// The tokens are sorted and none overlaps. A token that spans lines is left
/// out. A column is counted from the start of the line, or from the token
/// before on the same line, so a long line costs one pass.
fn encode(
    tokens: &[oxabl_pipeline::SemanticToken],
    rope: &Rope,
    text: &str,
    encoding: &PositionEncodingKind,
) -> Vec<SemanticToken> {
    let utf8 = *encoding == PositionEncodingKind::UTF8;
    let units = |part: &str| -> u32 {
        if utf8 {
            part.len() as u32
        } else {
            part.chars().map(|c| c.len_utf16() as u32).sum()
        }
    };
    let mut data = Vec::with_capacity(tokens.len());
    // The line of the last token and the byte where it starts.
    let mut last: Option<(usize, usize)> = None;
    for token in tokens {
        let (start, end) = (token.span.start as usize, token.span.end as usize);
        let Some(name) = text.get(start..end).filter(|name| !name.is_empty()) else {
            continue;
        };
        if name.bytes().any(|b| b == b'\n' || b == b'\r') {
            continue;
        }
        let same_line = |from: usize| {
            from <= start
                && text
                    .get(from..start)
                    .is_some_and(|gap| !gap.bytes().any(|b| b == b'\n' || b == b'\r'))
        };
        let (line, delta_line, delta_start) = match last {
            Some((line, from)) if same_line(from) => (line, 0, units(&text[from..start])),
            _ => {
                let line = rope.byte_to_line(start);
                let before = last.map_or(0, |(line, _)| line);
                if line < before {
                    continue;
                }
                let from = rope.line_to_byte(line);
                (line, (line - before) as u32, units(&text[from..start]))
            }
        };
        data.push(SemanticToken {
            delta_line,
            delta_start,
            length: units(name),
            token_type: token.kind.index(),
            token_modifiers_bitset: token.modifiers.bits(),
        });
        last = Some((line, start));
    }
    data
}

/// The one edit that turns `old` into `new`: the tokens between their common
/// head and tail. `start` and `delete_count` count the integers of the
/// protocol, five for each token.
fn edits(old: &[SemanticToken], new: &[SemanticToken]) -> Vec<SemanticTokensEdit> {
    let head = old.iter().zip(new).take_while(|(a, b)| a == b).count();
    let tail = old[head..]
        .iter()
        .rev()
        .zip(new[head..].iter().rev())
        .take_while(|(a, b)| a == b)
        .count();
    let removed = old.len() - head - tail;
    let added = &new[head..new.len() - tail];
    if removed == 0 && added.is_empty() {
        return Vec::new();
    }
    vec![SemanticTokensEdit {
        start: (head * 5) as u32,
        delete_count: (removed * 5) as u32,
        data: (!added.is_empty()).then(|| added.to_vec()),
    }]
}
