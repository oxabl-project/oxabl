//! `textDocument/definition`, `textDocument/hover` and
//! `textDocument/documentLink`.
//!
//! Each analyses one compilation unit on a worker thread with the open buffers
//! over the disk ([`compute_navigation`]) and render its byte spans in the
//! negotiated position encoding. An open include with a selected compilation
//! context is analysed in that root for go to definition and hover; otherwise
//! an include is analysed alone. A failed analysis answers with an error, never with an
//! empty answer that reads as "no definition".
//!
//! Hover requests run one at a time ([`HoverGate`]). A hover that a newer one
//! replaced before it started, or that the client cancelled with
//! `$/cancelRequest`, answers `RequestCanceled` and computes nothing: an editor
//! asks for a hover at each pause of the pointer, and only the last one counts.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use lsp_server::{ErrorCode, Message, Request, RequestId, Response};
use lsp_types::{
    DocumentLink, DocumentLinkParams, GotoDefinitionParams, Hover, HoverContents, HoverParams,
    Location, MarkupContent, MarkupKind, PositionEncodingKind, Range,
};
use oxabl_daemon::canonical_path;
use oxabl_daemon::db::{NavigationAnswer, NavigationQuery, compute_navigation};
use ropey::Rope;

use crate::position::{byte_to_position, position_to_byte};
use crate::{Server, path_to_uri, uri_to_path};

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
        self.spawn_navigation(req.id, root, query, |answer, encoding| {
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
        let query = NavigationQuery::Hover { file: path, offset };
        let ticket = self.hovers.ticket(&req.id);
        self.spawn_gated(req.id, root, query, Some(ticket), |answer, encoding| {
            serde_json::to_value(hover(answer, encoding))
        })
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
        self.spawn_navigation(req.id, root, NavigationQuery::Links, |answer, encoding| {
            serde_json::to_value(links(answer, encoding))
        })
    }

    /// Answer a navigation request on a worker thread. The response goes
    /// straight to the client.
    fn spawn_navigation(
        &mut self,
        id: RequestId,
        root: PathBuf,
        query: NavigationQuery,
        render: Render,
    ) -> anyhow::Result<()> {
        self.spawn_gated(id, root, query, None, render)
    }

    /// [`Self::spawn_navigation`], behind `ticket` when there is one.
    fn spawn_gated(
        &mut self,
        id: RequestId,
        root: PathBuf,
        query: NavigationQuery,
        ticket: Option<HoverTicket>,
        render: Render,
    ) -> anyhow::Result<()> {
        let Some((snapshot, schema, overlay)) = self.host.with(|sessions| {
            let session = sessions.get(&self.session_root)?;
            Some((
                session.database().clone(),
                session.schema_handle(),
                session.buffer_overlay(),
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
                    let _running = match &ticket {
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
                    let answered = oxabl_common::catch_panic(|| {
                        compute_navigation(&snapshot, schema, &root, overlay, &query)
                            .map(|answer| answer.map(|answer| render(answer, &encoding)))
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
}

impl HoverGate {
    pub(crate) fn ticket(&self, id: &RequestId) -> HoverTicket {
        let number = self.inner.latest.fetch_add(1, Ordering::SeqCst) + 1;
        lock(&self.inner.pending).insert(id.clone(), false);
        HoverTicket {
            inner: Arc::clone(&self.inner),
            id: id.clone(),
            number,
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
        let cancelled = lock(&self.inner.pending).remove(&self.id).unwrap_or(false);
        let newest = self.inner.latest.load(Ordering::SeqCst) == self.number;
        (newest && !cancelled).then_some(guard)
    }
}

fn lock<T>(mutex: &Mutex<T>) -> std::sync::MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(|e| e.into_inner())
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
