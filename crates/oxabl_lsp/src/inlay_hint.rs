//! `textDocument/inlayHint`: the database facts that ABL code does not show
//! and the strict database rules read, at the code they are about. The lock
//! a read takes with no lock phrase, the index oxabl expects a search to use,
//! and where a raised buffer scope or an implicit transaction starts and
//! ends. See `docs/design/inlay-hints.md`.
//!
//! A file shows hints only when its configuration enables a rule of
//! `strict.locks`, `strict.transactions` or `strict.access`. The client turns
//! each kind off on its own with the `inlayHints` object of its
//! initialization options or of `workspace/didChangeConfiguration`
//! (`settings.oxabl.inlayHints`).
//!
//! The answer is computed on a worker thread and sent from there. The worker
//! reads the memoized expansion on a snapshot and drops the snapshot before
//! the analysis, so an edit never waits for it. The hints of the whole file
//! are kept per file until its text, configuration or schema changes; a
//! request renders only the hints in its range. A request that an edit
//! overtakes answers `ContentModified`, never an empty list.

use std::collections::HashMap;
use std::ops::RangeInclusive;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use lsp_server::{ErrorCode, Message, Request, Response};
use lsp_types::{
    InlayHint, InlayHintLabel, InlayHintParams, InlayHintTooltip, PositionEncodingKind, Uri,
};
use oxabl_daemon::SessionHost;
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
    cache: Cache,
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
            cache: Cache::default(),
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
        let analysed = crate::uri_to_path(&uri).is_some_and(|path| {
            oxabl_workspace::is_root_file(&path) || oxabl_workspace::is_include_fragment(&path)
        });
        let none = || Response::new_ok(req.id.clone(), Vec::<InlayHint>::new());
        if !self.inlay.supported || kinds.none() || !analysed {
            return self.respond(none());
        }
        let Some(doc) = self.documents.get(&uri) else {
            return self.respond(none());
        };
        let Some((buffer, snapshot, generations)) = self.host.with(|sessions| {
            let session = sessions.get(&self.session_root)?;
            Some((
                session.buffer(uri.as_str())?,
                session.database().clone(),
                (
                    session.buffer_generation(),
                    session.config_generation(),
                    session.schema_revision(),
                ),
            ))
        }) else {
            return self.respond(none());
        };
        let rope = doc.rope.clone();
        let encoding = self.encoding.clone();
        let from = position_to_byte(&rope, params.range.start, &encoding) as u32;
        let to = position_to_byte(&rope, params.range.end, &encoding) as u32;
        let work = Work {
            uri,
            buffer,
            kinds,
            generations,
            range: from..=to,
            host: self.host.clone(),
            root: self.session_root.clone(),
            cache: Arc::clone(&self.inlay.cache),
        };
        let sender = self.connection.sender.clone();
        let id = req.id;
        let spawned = oxabl_common::stack::builder()
            .name("oxabl-inlay-hints".to_string())
            .spawn({
                let sender = sender.clone();
                let id = id.clone();
                move || {
                    let response = match work.answer(snapshot) {
                        Ok(hints) => Response::new_ok(id, render(&hints, &rope, &encoding)),
                        Err((code, message)) => Response::new_err(id, code as i32, message),
                    };
                    let _ = sender.send(Message::Response(response));
                }
            });
        if let Err(error) = spawned {
            return self.respond(Response::new_err(
                id,
                ErrorCode::InternalError as i32,
                format!("oxabl-lsp: cannot start the inlay hints: {error}"),
            ));
        }
        Ok(())
    }

    /// Forget the hints of a closed file.
    pub(crate) fn forget_inlay_hints(&mut self, uri: &Uri) {
        self.inlay
            .cache
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .remove(uri);
    }
}

/// The last hints of each open file, with what they were computed from: the
/// expansion's fingerprint, the configuration and schema generations, and the
/// kinds. A request for another range of the same text costs a filter.
type Cache = Arc<Mutex<HashMap<Uri, (CacheKey, Arc<Vec<oxabl_pipeline::InlayHint>>)>>>;
type CacheKey = (u64, u64, u32, HintKinds);

/// One request, answered on a worker thread.
struct Work {
    uri: Uri,
    buffer: oxabl_daemon::db::Buffer,
    kinds: HintKinds,
    /// The buffer, configuration and schema generations the request saw.
    generations: (u64, u64, u32),
    range: RangeInclusive<u32>,
    host: SessionHost,
    root: PathBuf,
    cache: Cache,
}

impl Work {
    /// The hints in the range. The snapshot is dropped before the analysis,
    /// so a keystroke never waits for it.
    fn answer(
        self,
        snapshot: oxabl_daemon::db::AnalysisDatabase,
    ) -> Result<Vec<oxabl_pipeline::InlayHint>, (ErrorCode, String)> {
        let modified = || {
            (
                ErrorCode::ContentModified,
                "oxabl-lsp: an edit replaced this analysis".to_string(),
            )
        };
        let input = oxabl_common::catch_panic(|| {
            oxabl_daemon::db::database_hint_input(&snapshot, self.buffer)
        });
        drop(snapshot);
        let input = match input {
            Ok(Some(Some(input))) => input,
            Ok(Some(None)) => return Ok(Vec::new()),
            Ok(None) => return Err(modified()),
            Err(panic) => return Err(failed(panic)),
        };
        let (_, config, schema) = self.generations;
        let key = (input.fingerprint(), config, schema, self.kinds);
        let cached = self
            .cache
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .get(&self.uri)
            .filter(|(k, _)| *k == key)
            .map(|(_, hints)| Arc::clone(hints));
        let hints = match cached {
            Some(hints) => hints,
            None => {
                let hints = Arc::new(
                    oxabl_common::catch_panic(|| input.hints(self.kinds)).map_err(failed)?,
                );
                self.cache
                    .lock()
                    .unwrap_or_else(|e| e.into_inner())
                    .insert(self.uri.clone(), (key, Arc::clone(&hints)));
                hints
            }
        };
        // The rope the positions are rendered on is the request's.
        let now = self.host.with(|sessions| {
            sessions.get(&self.root).map(|s| {
                (
                    s.buffer_generation(),
                    s.config_generation(),
                    s.schema_revision(),
                )
            })
        });
        if now != Some(self.generations) {
            return Err(modified());
        }
        Ok(hints
            .iter()
            .filter(|h| self.range.contains(&h.at))
            .cloned()
            .collect())
    }
}

fn failed(panic: impl std::fmt::Display) -> (ErrorCode, String) {
    (
        ErrorCode::InternalError,
        format!("oxabl-lsp: the inlay hints failed: {panic}"),
    )
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
