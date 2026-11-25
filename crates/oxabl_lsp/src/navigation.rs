//! `textDocument/definition`, `textDocument/hover`, `textDocument/documentHighlight` and
//! `textDocument/documentLink`.
//!
//! Each analyses one compilation unit on a worker thread with the open buffers
//! over the disk ([`compute_navigation`]) and render its byte spans in the
//! negotiated position encoding. A highlight of an open file that is its own
//! root, and that reads no include with unsaved changes, is answered from the
//! cached analysis ([`compute_highlights`]). The highlights of one document run
//! one at a time, and a request that a newer one replaced while it waited is
//! cancelled. An open include with a selected compilation context is analysed
//! in that root for go to definition, hover and highlights; otherwise an include
//! is analysed alone. A failed analysis answers with an error, never with an
//! empty answer that reads as "no definition".
//!
//! Hover requests run one at a time ([`HoverGate`]). A hover that a newer one
//! replaced before it started, or that the client cancelled with
//! `$/cancelRequest`, answers `RequestCanceled` and computes nothing: an editor
//! asks for a hover at each pause of the pointer, and only the last one counts.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, PoisonError};

use lsp_server::{ErrorCode, Message, Request, RequestId, Response};
use lsp_types::{
    DocumentHighlight, DocumentHighlightKind, DocumentHighlightParams, DocumentLink,
    DocumentLinkParams, GotoDefinitionParams, Hover, HoverContents, HoverParams, Location,
    MarkupContent, MarkupKind, PositionEncodingKind, Range,
};
use oxabl_daemon::canonical_path;
use oxabl_daemon::db::{
    AnalysisDatabase, Buffer, NavigationAnswer, NavigationQuery, buffer_dependencies,
    compute_highlights, compute_navigation,
};
use ropey::Rope;

use crate::position::{byte_to_position, position_to_byte};
use crate::{Server, path_to_uri, uri_to_path};

/// The highlight requests of each open document.
pub(crate) type HighlightTurns = HashMap<lsp_types::Uri, Arc<HighlightTurn>>;

/// The newest highlight request of a document, and the lock its analysis holds.
#[derive(Default)]
pub(crate) struct HighlightTurn {
    latest: AtomicU64,
    running: Mutex<()>,
}

/// How a navigation request waits for the others of its kind.
#[derive(Default)]
struct Gate {
    /// The open buffer whose cached analysis a highlight query uses.
    buffer: Option<String>,
    /// The highlight turn of the document, and this request's number in it.
    turn: Option<(Arc<HighlightTurn>, u64)>,
    /// The hover ticket, when the request is a hover.
    ticket: Option<HoverTicket>,
}

impl Server<'_> {
    pub(crate) fn handle_definition(&mut self, req: Request) -> anyhow::Result<()> {
        let params: GotoDefinitionParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let position = params.text_document_position_params;
        let uri = position.text_document.uri;
        let Some(path) = uri_to_path(&uri) else {
            return self.respond(invalid(req.id, &format!("{} is not a file", uri.as_str())));
        };
        let Some(document) = self.documents.get(&uri) else {
            return self.respond(invalid(req.id, &format!("{} is not open", uri.as_str())));
        };
        let offset = position_to_byte(&document.rope, position.position, &self.encoding) as u32;
        let path = canonical_path(&path);
        let root = self
            .contexts
            .selected(&uri)
            .cloned()
            .unwrap_or_else(|| path.clone());
        let query = NavigationQuery::Definition { file: path, offset };
        self.spawn_navigation(req.id, root, None, None, query, |answer, encoding| {
            serde_json::to_value(locations(answer, encoding))
        })
    }

    pub(crate) fn handle_hover(&mut self, req: Request) -> anyhow::Result<()> {
        let params: HoverParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let position = params.text_document_position_params;
        let uri = position.text_document.uri;
        let Some(path) = uri_to_path(&uri) else {
            return self.respond(invalid(req.id, &format!("{} is not a file", uri.as_str())));
        };
        let Some(document) = self.documents.get(&uri) else {
            return self.respond(invalid(req.id, &format!("{} is not open", uri.as_str())));
        };
        let offset = position_to_byte(&document.rope, position.position, &self.encoding) as u32;
        let path = canonical_path(&path);
        let root = self
            .contexts
            .selected(&uri)
            .cloned()
            .unwrap_or_else(|| path.clone());
        let buffer = self.open_buffer(&root);
        let query = NavigationQuery::Hover {
            file: path,
            offset,
            buffer,
        };
        let gate = Gate {
            ticket: Some(self.hovers.ticket(&req.id)),
            ..Gate::default()
        };
        self.spawn_gated(req.id, root, query, gate, |answer, encoding| {
            serde_json::to_value(hover(answer, encoding))
        })
    }

    /// The session buffer of the open document at `path`.
    fn open_buffer(&self, path: &Path) -> Option<oxabl_daemon::db::Buffer> {
        let uri = self
            .documents
            .uris()
            .find(|uri| uri_to_path(uri).is_some_and(|p| canonical_path(&p) == path))?
            .clone();
        self.host
            .with(|sessions| sessions.get(&self.session_root)?.buffer(uri.as_str()))
    }

    pub(crate) fn handle_document_highlight(&mut self, req: Request) -> anyhow::Result<()> {
        let params: DocumentHighlightParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let position = params.text_document_position_params;
        let uri = position.text_document.uri;
        let Some(path) = uri_to_path(&uri) else {
            return self.respond(invalid(req.id, &format!("{} is not a file", uri.as_str())));
        };
        let Some(document) = self.documents.get(&uri) else {
            return self.respond(invalid(req.id, &format!("{} is not open", uri.as_str())));
        };
        let offset = position_to_byte(&document.rope, position.position, &self.encoding) as u32;
        let path = canonical_path(&path);
        let root = self
            .contexts
            .selected(&uri)
            .cloned()
            .unwrap_or_else(|| path.clone());
        // The open file as its own root is answered from the analysis that
        // diagnostics share; an include analysed in a selected root is not.
        let own_buffer = (root == path).then(|| uri.as_str().to_string());
        let turn = self.highlight_turns.entry(uri).or_default().clone();
        let mine = turn.latest.fetch_add(1, Ordering::SeqCst) + 1;
        let query = NavigationQuery::Highlights { file: path, offset };
        self.spawn_navigation(
            req.id,
            root,
            own_buffer,
            Some((turn, mine)),
            query,
            |answer, encoding| serde_json::to_value(highlights(answer, encoding)),
        )
    }

    pub(crate) fn handle_document_link(&mut self, req: Request) -> anyhow::Result<()> {
        let params: DocumentLinkParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let uri = params.text_document.uri;
        let Some(path) = uri_to_path(&uri) else {
            return self.respond(invalid(req.id, &format!("{} is not a file", uri.as_str())));
        };
        let root = canonical_path(&path);
        self.spawn_navigation(
            req.id,
            root,
            None,
            None,
            NavigationQuery::Links,
            |answer, encoding| serde_json::to_value(links(answer, encoding)),
        )
    }

    /// Answer a navigation request on a worker thread. The response goes
    /// straight to the client. With a `buffer` key, a highlight query uses the
    /// open buffer's cached analysis. With a `turn`, the request waits for the
    /// one before it and is cancelled when a newer one came meanwhile.
    fn spawn_navigation(
        &mut self,
        id: RequestId,
        root: PathBuf,
        buffer: Option<String>,
        turn: Option<(Arc<HighlightTurn>, u64)>,
        query: NavigationQuery,
        render: Render,
    ) -> anyhow::Result<()> {
        let gate = Gate {
            buffer,
            turn,
            ticket: None,
        };
        self.spawn_gated(id, root, query, gate, render)
    }

    /// [`Self::spawn_navigation`] with every way a request can be ordered
    /// against the others of its kind.
    fn spawn_gated(
        &mut self,
        id: RequestId,
        root: PathBuf,
        query: NavigationQuery,
        gate: Gate,
        render: Render,
    ) -> anyhow::Result<()> {
        let Gate {
            buffer,
            turn,
            ticket,
        } = gate;
        let Some((snapshot, schema, overlay, buffer)) = self.host.with(|sessions| {
            let session = sessions.get(&self.session_root)?;
            Some((
                session.database().clone(),
                session.schema_handle(),
                session.buffer_overlay(),
                buffer.and_then(|key| session.buffer(&key)),
            ))
        }) else {
            return self.respond(Response::new_err(
                id,
                ErrorCode::RequestFailed as i32,
                "oxabl-lsp: no session".to_string(),
            ));
        };
        let encoding = self.encoding.clone();
        let sender = self.connection.sender.clone();
        let spawned = oxabl_common::stack::builder()
            .name("oxabl-navigation".to_string())
            .spawn({
                let sender = sender.clone();
                let id = id.clone();
                move || {
                    let _running_hover = match &ticket {
                        Some(ticket) => match ticket.start() {
                            Some(guard) => Some(guard),
                            None => {
                                let _ = sender.send(Message::Response(Response::new_err(
                                    id,
                                    ErrorCode::RequestCanceled as i32,
                                    "oxabl-lsp: a newer hover or the client cancelled this one"
                                        .to_string(),
                                )));
                                return;
                            }
                        },
                        None => None,
                    };
                    let _running_highlight = turn.as_ref().map(|(turn, _)| {
                        turn.running.lock().unwrap_or_else(PoisonError::into_inner)
                    });
                    if let Some((turn, mine)) = &turn
                        && turn.latest.load(Ordering::SeqCst) != *mine
                    {
                        let _ = sender.send(Message::Response(Response::new_err(
                            id,
                            ErrorCode::ContentModified as i32,
                            "oxabl-lsp: a newer highlight replaced this one".to_string(),
                        )));
                        return;
                    }
                    let answered = oxabl_common::catch_panic(|| {
                        let answer = match (&query, buffer) {
                            (NavigationQuery::Highlights { offset, .. }, Some(buffer))
                                if !reads_unsaved_include(&snapshot, buffer, &root, &overlay) =>
                            {
                                compute_highlights(&snapshot, buffer, schema, *offset)
                            }
                            _ => compute_navigation(&snapshot, schema, &root, overlay, &query),
                        };
                        answer.map(|answer| answer.map(|answer| render(answer, &encoding)))
                    });
                    let response = match answered {
                        Ok(Some(Ok(Ok(value)))) => Response::new_ok(id, value),
                        Ok(Some(Ok(Err(error)))) => Response::new_err(
                            id,
                            ErrorCode::InternalError as i32,
                            format!("oxabl-lsp: cannot render the answer: {error}"),
                        ),
                        Ok(Some(Err(reason))) => Response::new_err(
                            id,
                            ErrorCode::RequestFailed as i32,
                            format!("oxabl-lsp: {} gives no answer: {reason}", root.display()),
                        ),
                        Ok(None) => Response::new_err(
                            id,
                            ErrorCode::ContentModified as i32,
                            "oxabl-lsp: an edit replaced this analysis".to_string(),
                        ),
                        Err(panic) => Response::new_err(
                            id,
                            ErrorCode::InternalError as i32,
                            format!("oxabl-lsp: the analysis failed: {panic}"),
                        ),
                    };
                    let _ = sender.send(Message::Response(response));
                }
            });
        if let Err(error) = spawned {
            return self.respond(Response::new_err(
                id,
                ErrorCode::InternalError as i32,
                format!("oxabl-lsp: cannot start the analysis: {error}"),
            ));
        }
        Ok(())
    }
}

type Render = fn(NavigationAnswer, &PositionEncodingKind) -> serde_json::Result<serde_json::Value>;

/// Runs hover requests one at a time and drops the ones nobody waits for.
#[derive(Clone, Default)]
pub(crate) struct HoverGate {
    inner: Arc<GateInner>,
}

#[derive(Default)]
struct GateInner {
    /// The number of the newest hover request.
    latest: AtomicU64,
    /// Each hover request not yet started, and whether the client cancelled it.
    pending: Mutex<HashMap<RequestId, bool>>,
    /// Held while a hover computes.
    running: Mutex<()>,
}

/// One hover request's place in the [`HoverGate`].
pub(crate) struct HoverTicket {
    inner: Arc<GateInner>,
    id: RequestId,
    number: u64,
    /// Whether [`HoverTicket::start`] ran.
    started: std::sync::atomic::AtomicBool,
}

impl HoverGate {
    pub(crate) fn ticket(&self, id: &RequestId) -> HoverTicket {
        let number = self.inner.latest.fetch_add(1, Ordering::SeqCst) + 1;
        lock(&self.inner.pending).insert(id.clone(), false);
        HoverTicket {
            inner: Arc::clone(&self.inner),
            id: id.clone(),
            number,
            started: std::sync::atomic::AtomicBool::new(false),
        }
    }

    /// `$/cancelRequest` for `id`. Only a hover not yet started is affected.
    pub(crate) fn cancel(&self, id: &RequestId) {
        if let Some(cancelled) = lock(&self.inner.pending).get_mut(id) {
            *cancelled = true;
        }
    }
}

impl HoverTicket {
    /// Wait for the hovers before this one, then the guard to compute under;
    /// `None` when a newer hover came or the client cancelled this one.
    fn start(&self) -> Option<std::sync::MutexGuard<'_, ()>> {
        let guard = lock(&self.inner.running);
        self.started.store(true, Ordering::SeqCst);
        let cancelled = lock(&self.inner.pending).remove(&self.id).unwrap_or(false);
        let newest = self.inner.latest.load(Ordering::SeqCst) == self.number;
        (newest && !cancelled).then_some(guard)
    }
}

impl Drop for HoverTicket {
    /// A ticket that never started (no session, no thread) leaves no trace: it
    /// is no longer pending, and no longer the newest, so it cancels no hover
    /// queued before it.
    fn drop(&mut self) {
        if self.started.load(Ordering::SeqCst) {
            return;
        }
        lock(&self.inner.pending).remove(&self.id);
        let _ = self.inner.latest.compare_exchange(
            self.number,
            self.number - 1,
            Ordering::SeqCst,
            Ordering::SeqCst,
        );
    }
}

fn lock<T>(mutex: &Mutex<T>) -> std::sync::MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(|e| e.into_inner())
}

/// Whether the expansion of `buffer` may read an include whose open buffer
/// differs from the file, which the cached analysis does not see. True when
/// that cannot be told, and when another open document is not on disk: a new
/// include is not among the dependencies, yet the expansion may read it.
pub(crate) fn reads_unsaved_include(
    snapshot: &AnalysisDatabase,
    buffer: Buffer,
    root: &Path,
    overlay: &HashMap<PathBuf, Arc<str>>,
) -> bool {
    unsaved_among(buffer_dependencies(snapshot, buffer), root, overlay)
}

/// [`reads_unsaved_include`] for the dependencies of the expansion, or `None`
/// when they could not be read.
pub(crate) fn unsaved_among(
    dependencies: Option<Vec<PathBuf>>,
    root: &Path,
    overlay: &HashMap<PathBuf, Arc<str>>,
) -> bool {
    let Some(dependencies) = dependencies else {
        return true;
    };
    let open: HashMap<PathBuf, &Arc<str>> = overlay
        .iter()
        .map(|(path, text)| (canonical_path(path), text))
        .filter(|(path, _)| path != root)
        .collect();
    open.keys().any(|path| !path.exists())
        || dependencies.iter().any(|path| {
            open.get(&canonical_path(path)).is_some_and(|text| {
                std::fs::read_to_string(path).map_or(true, |saved| saved != text.as_ref())
            })
        })
}

fn invalid(id: RequestId, message: &str) -> Response {
    Response::new_err(
        id,
        ErrorCode::InvalidParams as i32,
        format!("oxabl-lsp: {message}"),
    )
}

/// Each target as a location. `None` (JSON `null`) when there is none.
fn locations(answer: NavigationAnswer, encoding: &PositionEncodingKind) -> Option<Vec<Location>> {
    let mut ropes = Ropes::new(&answer.texts);
    let locations: Vec<Location> = answer
        .definitions
        .iter()
        .filter_map(|target| {
            let uri = path_to_uri(&canonical_path(&target.path))?;
            let range = match target.span {
                Some(span) => ropes.range(&target.path, (span.start, span.end), encoding)?,
                None => Range::default(),
            };
            Some(Location { uri, range })
        })
        .collect();
    (!locations.is_empty()).then_some(locations)
}

/// The hover as Markdown. `None` (JSON `null`) when there is none.
fn hover(answer: NavigationAnswer, encoding: &PositionEncodingKind) -> Option<Hover> {
    let (path, hover) = answer.hover.as_ref()?;
    let mut ropes = Ropes::new(&answer.texts);
    let range = hover
        .span
        .and_then(|span| ropes.range(path, (span.start, span.end), encoding));
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: hover.contents.clone(),
        }),
        range,
    })
}

/// Each use of the name, read or write. `None` (JSON `null`) when there is none.
fn highlights(
    answer: NavigationAnswer,
    encoding: &PositionEncodingKind,
) -> Option<Vec<DocumentHighlight>> {
    let file = answer.texts.keys().next().cloned()?;
    let mut ropes = Ropes::new(&answer.texts);
    let found: Vec<DocumentHighlight> = answer
        .highlights
        .iter()
        .filter_map(|use_| {
            Some(DocumentHighlight {
                range: ropes.range(&file, (use_.span.start, use_.span.end), encoding)?,
                kind: Some(if use_.write {
                    DocumentHighlightKind::WRITE
                } else {
                    DocumentHighlightKind::READ
                }),
            })
        })
        .collect();
    (!found.is_empty()).then_some(found)
}

fn links(answer: NavigationAnswer, encoding: &PositionEncodingKind) -> Vec<DocumentLink> {
    let Some(root) = answer.texts.keys().next().cloned() else {
        return Vec::new();
    };
    let mut ropes = Ropes::new(&answer.texts);
    answer
        .links
        .iter()
        .filter_map(|link| {
            Some(DocumentLink {
                range: ropes.range(&root, (link.span.start, link.span.end), encoding)?,
                target: Some(path_to_uri(&canonical_path(&link.target))?),
                tooltip: None,
                data: None,
            })
        })
        .collect()
}

/// The text of each answered file as a rope, built once.
struct Ropes<'t> {
    texts: &'t HashMap<PathBuf, std::sync::Arc<str>>,
    ropes: HashMap<PathBuf, Rope>,
}

impl<'t> Ropes<'t> {
    fn new(texts: &'t HashMap<PathBuf, std::sync::Arc<str>>) -> Self {
        Ropes {
            texts,
            ropes: HashMap::new(),
        }
    }

    fn range(
        &mut self,
        path: &Path,
        (start, end): (u32, u32),
        encoding: &PositionEncodingKind,
    ) -> Option<Range> {
        if !self.ropes.contains_key(path) {
            let text = self.texts.get(path)?;
            self.ropes.insert(path.to_path_buf(), Rope::from_str(text));
        }
        let rope = self.ropes.get(path)?;
        Some(Range::new(
            byte_to_position(rope, start as usize, encoding),
            byte_to_position(rope, end as usize, encoding),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_ticket_that_never_starts_does_not_replace_an_older_one() {
        let gate = HoverGate::default();
        let older = gate.ticket(&RequestId::from(1));
        let failed = gate.ticket(&RequestId::from(2));
        drop(failed);
        assert!(older.start().is_some(), "the older hover must still run");
        assert!(lock(&gate.inner.pending).is_empty());
    }

    #[test]
    fn a_newer_ticket_and_a_cancel_stop_a_waiting_one() {
        let gate = HoverGate::default();
        let replaced = gate.ticket(&RequestId::from(1));
        let cancelled = gate.ticket(&RequestId::from(2));
        let newest = gate.ticket(&RequestId::from(3));
        gate.cancel(&RequestId::from(2));
        assert!(replaced.start().is_none());
        assert!(cancelled.start().is_none());
        assert!(newest.start().is_some());
    }
}
