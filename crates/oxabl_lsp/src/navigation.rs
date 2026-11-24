//! `textDocument/definition`, `textDocument/documentHighlight` and
//! `textDocument/documentLink`.
//!
//! Each analyses one compilation unit on a worker thread with the open buffers
//! over the disk ([`compute_navigation`]) and render its byte spans in the
//! negotiated position encoding. A highlight of an open file that is its own
//! root, and that reads no include with unsaved changes, is answered from the
//! cached analysis ([`compute_highlights`]). The highlights of one document run
//! one at a time, and a request that a newer one replaced while it waited is
//! cancelled. An open include with a selected compilation context is analysed
//! in that root; otherwise an include is analysed alone. A failed analysis
//! answers with an error, never with an empty answer that reads as "no
//! definition".

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, PoisonError};

use lsp_server::{ErrorCode, Message, Request, RequestId, Response};
use lsp_types::{
    DocumentHighlight, DocumentHighlightKind, DocumentHighlightParams, DocumentLink,
    DocumentLinkParams, GotoDefinitionParams, Location, PositionEncodingKind, Range,
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
        render: fn(
            NavigationAnswer,
            &PositionEncodingKind,
        ) -> serde_json::Result<serde_json::Value>,
    ) -> anyhow::Result<()> {
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
                    let _running = turn.as_ref().map(|(turn, _)| {
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

/// Whether the expansion of `buffer` may read an include whose open buffer
/// differs from the file, which the cached analysis does not see. True when
/// that cannot be told, and when another open document is not on disk: a new
/// include is not among the dependencies, yet the expansion may read it.
fn reads_unsaved_include(
    snapshot: &AnalysisDatabase,
    buffer: Buffer,
    root: &Path,
    overlay: &HashMap<PathBuf, Arc<str>>,
) -> bool {
    let Some(dependencies) = buffer_dependencies(snapshot, buffer) else {
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
