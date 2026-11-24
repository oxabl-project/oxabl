//! `textDocument/references` and the call hierarchy
//! (`textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`,
//! `callHierarchy/outgoingCalls`). See `docs/design/references.md`.
//!
//! Each request runs on a worker thread ([`answer_references`]): it can wait
//! for a workspace pass. An answer that can be short is never shown as a
//! complete one. A references answer comes with a `window/showMessage`
//! warning that names each gap. A call hierarchy answer holds an item named
//! `incomplete` that names them, first in the list, and the same warning.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use lsp_server::{ErrorCode, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::Notification as _;
use lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams,
    Location, MessageType, PositionEncodingKind, Range, ReferenceParams, SymbolKind,
};
use oxabl_daemon::canonical_path;
use oxabl_daemon::references::{
    ReferenceAnswer, ReferenceError, ReferenceQuery, answer_references,
};
use oxabl_pipeline::{CallItem, CallItemKind, Span};
use ropey::Rope;
use serde::{Deserialize, Serialize};

use crate::position::{byte_to_position, position_to_byte};
use crate::{Server, path_to_uri, uri_to_path};

/// What a call hierarchy item carries back to the server, in `data`.
#[derive(Serialize, Deserialize)]
struct ItemData {
    root: PathBuf,
    path: PathBuf,
    name: String,
    kind: String,
    span: [u32; 2],
    selection: [u32; 2],
    declared: bool,
    unresolved: Option<String>,
}

/// The `data` of the item that says an answer is incomplete.
#[derive(Serialize, Deserialize)]
struct MarkerData {
    incomplete: bool,
}

const KINDS: &[(CallItemKind, &str)] = &[
    (CallItemKind::Program, "program"),
    (CallItemKind::Class, "class"),
    (CallItemKind::Procedure, "procedure"),
    (CallItemKind::Function, "function"),
    (CallItemKind::Method, "method"),
    (CallItemKind::Constructor, "constructor"),
    (CallItemKind::Destructor, "destructor"),
    (CallItemKind::Property, "property"),
    (CallItemKind::Trigger, "trigger"),
    (CallItemKind::External, "external"),
    (CallItemKind::Unresolved, "unresolved"),
    (CallItemKind::Other, "other"),
];

fn kind_name(kind: CallItemKind) -> &'static str {
    KINDS
        .iter()
        .find(|(known, _)| *known == kind)
        .map_or("other", |(_, name)| name)
}

fn symbol_kind(kind: CallItemKind) -> SymbolKind {
    match kind {
        CallItemKind::Program => SymbolKind::FILE,
        CallItemKind::Class => SymbolKind::CLASS,
        CallItemKind::Procedure | CallItemKind::Function | CallItemKind::External => {
            SymbolKind::FUNCTION
        }
        CallItemKind::Method | CallItemKind::Destructor => SymbolKind::METHOD,
        CallItemKind::Constructor => SymbolKind::CONSTRUCTOR,
        CallItemKind::Property => SymbolKind::PROPERTY,
        CallItemKind::Trigger => SymbolKind::EVENT,
        CallItemKind::Unresolved => SymbolKind::NULL,
        CallItemKind::Other => SymbolKind::OBJECT,
    }
}

fn to_data(item: &CallItem) -> ItemData {
    ItemData {
        root: item.root.clone(),
        path: item.path.clone(),
        name: item.name.clone(),
        kind: kind_name(item.kind).to_string(),
        span: [item.span.start, item.span.end],
        selection: [item.selection.start, item.selection.end],
        declared: item.declared,
        unresolved: item.unresolved.clone(),
    }
}

fn from_data(data: ItemData) -> Option<CallItem> {
    let kind = KINDS
        .iter()
        .find(|(_, name)| *name == data.kind)
        .map(|(kind, _)| *kind)?;
    Some(CallItem {
        name: data.name,
        kind,
        detail: None,
        path: data.path,
        span: Span {
            start: data.span[0],
            end: data.span[1],
        },
        selection: Span {
            start: data.selection[0],
            end: data.selection[1],
        },
        root: data.root,
        declared: data.declared,
        unresolved: data.unresolved,
    })
}

/// What a call hierarchy request's item is.
enum Asked {
    Item(CallItem),
    /// The item that says an answer is incomplete: it has no calls.
    Marker,
}

fn asked(item: &CallHierarchyItem) -> Result<Asked, String> {
    let data = item
        .data
        .clone()
        .ok_or_else(|| "the item has no data: it does not come from this server".to_string())?;
    if serde_json::from_value::<MarkerData>(data.clone()).is_ok_and(|marker| marker.incomplete) {
        return Ok(Asked::Marker);
    }
    let data: ItemData = serde_json::from_value(data)
        .map_err(|error| format!("the item's data is not this server's: {error}"))?;
    from_data(data)
        .map(Asked::Item)
        .ok_or_else(|| "the item's data names no known kind".to_string())
}

impl Server<'_> {
    pub(crate) fn handle_references(&mut self, req: Request) -> anyhow::Result<()> {
        let params: ReferenceParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let position = params.text_document_position;
        let Some((root, file, offset)) = self.cursor(&req.id, &position)? else {
            return Ok(());
        };
        let query = ReferenceQuery::References {
            file,
            offset,
            include_declaration: params.context.include_declaration,
        };
        self.spawn_references(req.id, root, query, "references", |answer, encoding| {
            serde_json::to_value(locations(answer, encoding))
        })
    }

    pub(crate) fn handle_prepare_call_hierarchy(&mut self, req: Request) -> anyhow::Result<()> {
        let params: CallHierarchyPrepareParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let position = params.text_document_position_params;
        let Some((root, file, offset)) = self.cursor(&req.id, &position)? else {
            return Ok(());
        };
        let query = ReferenceQuery::Prepare { file, offset };
        self.spawn_references(
            req.id,
            root,
            query,
            "the call hierarchy",
            |answer, encoding| {
                let mut ropes = Ropes::new(&answer.texts);
                let items: Vec<CallHierarchyItem> = answer
                    .items
                    .iter()
                    .filter_map(|item| render_item(item, &mut ropes, encoding))
                    .collect();
                serde_json::to_value((!items.is_empty()).then_some(items))
            },
        )
    }

    pub(crate) fn handle_incoming_calls(&mut self, req: Request) -> anyhow::Result<()> {
        let params: CallHierarchyIncomingCallsParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let item = match asked(&params.item) {
            Ok(Asked::Item(item)) => item,
            Ok(Asked::Marker) => {
                return self.respond(Response::new_ok(req.id, Vec::<()>::new()));
            }
            Err(error) => return self.respond(invalid(req.id, &error)),
        };
        let shown = params.item;
        let root = canonical_path(&item.root);
        let query = ReferenceQuery::Incoming(item);
        self.spawn_references(
            req.id,
            root,
            query,
            "the incoming calls",
            move |answer, encoding| {
                let mut ropes = Ropes::new(&answer.texts);
                let mut calls: Vec<CallHierarchyIncomingCall> = Vec::new();
                if !answer.gaps.is_empty() {
                    calls.push(CallHierarchyIncomingCall {
                        from: marker(&shown, &answer.gaps),
                        from_ranges: Vec::new(),
                    });
                }
                for call in &answer.incoming {
                    let Some(from) = render_item(&call.from, &mut ropes, encoding) else {
                        continue;
                    };
                    let from_ranges = call
                        .spans
                        .iter()
                        .filter_map(|span| ropes.range(&call.from.path, *span, encoding))
                        .collect();
                    calls.push(CallHierarchyIncomingCall { from, from_ranges });
                }
                serde_json::to_value(calls)
            },
        )
    }

    pub(crate) fn handle_outgoing_calls(&mut self, req: Request) -> anyhow::Result<()> {
        let params: CallHierarchyOutgoingCallsParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let item = match asked(&params.item) {
            Ok(Asked::Item(item)) => item,
            Ok(Asked::Marker) => {
                return self.respond(Response::new_ok(req.id, Vec::<()>::new()));
            }
            Err(error) => return self.respond(invalid(req.id, &error)),
        };
        let shown = params.item;
        let caller = item.path.clone();
        let root = canonical_path(&item.root);
        let query = ReferenceQuery::Outgoing(item);
        self.spawn_references(
            req.id,
            root,
            query,
            "the outgoing calls",
            move |answer, encoding| {
                let mut ropes = Ropes::new(&answer.texts);
                let mut calls: Vec<CallHierarchyOutgoingCall> = Vec::new();
                if !answer.gaps.is_empty() {
                    calls.push(CallHierarchyOutgoingCall {
                        to: marker(&shown, &answer.gaps),
                        from_ranges: Vec::new(),
                    });
                }
                for call in &answer.outgoing {
                    let Some(to) = render_item(&call.to, &mut ropes, encoding) else {
                        continue;
                    };
                    let from_ranges = call
                        .spans
                        .iter()
                        .filter_map(|span| ropes.range(&caller, *span, encoding))
                        .collect();
                    calls.push(CallHierarchyOutgoingCall { to, from_ranges });
                }
                serde_json::to_value(calls)
            },
        )
    }

    /// The unit to analyse, the file and the byte offset of a request's
    /// cursor. `None` when the request was already answered with an error.
    fn cursor(
        &mut self,
        id: &RequestId,
        position: &lsp_types::TextDocumentPositionParams,
    ) -> anyhow::Result<Option<(PathBuf, PathBuf, u32)>> {
        let uri = &position.text_document.uri;
        let Some(path) = uri_to_path(uri) else {
            self.respond(invalid(
                id.clone(),
                &format!("{} is not a file", uri.as_str()),
            ))?;
            return Ok(None);
        };
        let Some(document) = self.documents.get(uri) else {
            self.respond(invalid(
                id.clone(),
                &format!("{} is not open", uri.as_str()),
            ))?;
            return Ok(None);
        };
        let offset = position_to_byte(&document.rope, position.position, &self.encoding) as u32;
        let path = canonical_path(&path);
        let root = self
            .contexts
            .selected(uri)
            .cloned()
            .unwrap_or_else(|| path.clone());
        Ok(Some((root, path, offset)))
    }

    /// Answer a references or call hierarchy request on a worker thread. The
    /// response, and a warning when the answer can be incomplete, go straight
    /// to the client.
    fn spawn_references(
        &mut self,
        id: RequestId,
        root: PathBuf,
        query: ReferenceQuery,
        what: &'static str,
        render: impl FnOnce(
            ReferenceAnswer,
            &PositionEncodingKind,
        ) -> serde_json::Result<serde_json::Value>
        + Send
        + 'static,
    ) -> anyhow::Result<()> {
        let host = self.host.clone();
        let session_root = self.session_root.clone();
        let encoding = self.encoding.clone();
        let sender = self.connection.sender.clone();
        let spawned = oxabl_common::stack::builder()
            .name("oxabl-references".to_string())
            .spawn({
                let sender = sender.clone();
                let id = id.clone();
                move || {
                    let answered = oxabl_common::catch_panic(|| {
                        answer_references(&host, &session_root, &root, &query)
                    });
                    let response = match answered {
                        Ok(Ok(answer)) => {
                            if !answer.gaps.is_empty() {
                                warn(&sender, what, &answer.gaps);
                            }
                            match render(answer, &encoding) {
                                Ok(value) => Response::new_ok(id, value),
                                Err(error) => Response::new_err(
                                    id,
                                    ErrorCode::InternalError as i32,
                                    format!("oxabl-lsp: cannot render the answer: {error}"),
                                ),
                            }
                        }
                        Ok(Err(ReferenceError::Failed(reason))) => Response::new_err(
                            id,
                            ErrorCode::RequestFailed as i32,
                            format!("oxabl-lsp: {} gives no answer: {reason}", root.display()),
                        ),
                        Ok(Err(ReferenceError::Modified)) => Response::new_err(
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

/// Tell the user, and the log, that an answer can be incomplete and why.
fn warn(sender: &crossbeam_channel::Sender<Message>, what: &str, gaps: &[String]) {
    let message = format!("oxabl: {what} may be incomplete: {}", gaps.join("; "));
    for method in [
        lsp_types::notification::ShowMessage::METHOD,
        lsp_types::notification::LogMessage::METHOD,
    ] {
        let _ = sender.send(Message::Notification(Notification {
            method: method.to_string(),
            params: serde_json::json!({
                "type": MessageType::WARNING,
                "message": message,
            }),
        }));
    }
}

/// The item that says the answer for `shown` is incomplete, and why.
fn marker(shown: &CallHierarchyItem, gaps: &[String]) -> CallHierarchyItem {
    CallHierarchyItem {
        name: "incomplete".to_string(),
        kind: SymbolKind::NULL,
        tags: None,
        detail: Some(gaps.join("; ")),
        uri: shown.uri.clone(),
        range: shown.range,
        selection_range: shown.selection_range,
        data: Some(serde_json::json!({ "incomplete": true })),
    }
}

fn invalid(id: RequestId, message: &str) -> Response {
    Response::new_err(
        id,
        ErrorCode::InvalidParams as i32,
        format!("oxabl-lsp: {message}"),
    )
}

fn locations(answer: ReferenceAnswer, encoding: &PositionEncodingKind) -> Vec<Location> {
    let mut ropes = Ropes::new(&answer.texts);
    answer
        .locations
        .iter()
        .filter_map(|target| {
            let uri = path_to_uri(&canonical_path(&target.path))?;
            let range = ropes.range(&target.path, target.span?, encoding)?;
            Some(Location { uri, range })
        })
        .collect()
}

fn render_item(
    item: &CallItem,
    ropes: &mut Ropes<'_>,
    encoding: &PositionEncodingKind,
) -> Option<CallHierarchyItem> {
    let uri = path_to_uri(&canonical_path(&item.path))?;
    let (range, selection_range) = if item.span.end == 0 {
        (Range::default(), Range::default())
    } else {
        (
            ropes.range(&item.path, item.span, encoding)?,
            ropes.range(&item.path, item.selection, encoding)?,
        )
    };
    Some(CallHierarchyItem {
        name: item.name.clone(),
        kind: symbol_kind(item.kind),
        tags: None,
        detail: item.detail.clone(),
        uri,
        range,
        selection_range,
        data: serde_json::to_value(to_data(item)).ok(),
    })
}

/// The text of each answered file as a rope, built once.
struct Ropes<'t> {
    texts: &'t HashMap<PathBuf, Arc<str>>,
    ropes: HashMap<PathBuf, Rope>,
}

impl<'t> Ropes<'t> {
    fn new(texts: &'t HashMap<PathBuf, Arc<str>>) -> Self {
        Ropes {
            texts,
            ropes: HashMap::new(),
        }
    }

    fn range(
        &mut self,
        path: &std::path::Path,
        span: Span,
        encoding: &PositionEncodingKind,
    ) -> Option<Range> {
        if !self.ropes.contains_key(path) {
            let text = self.texts.get(path)?;
            self.ropes.insert(path.to_path_buf(), Rope::from_str(text));
        }
        let rope = self.ropes.get(path)?;
        Some(Range::new(
            byte_to_position(rope, span.start as usize, encoding),
            byte_to_position(rope, span.end as usize, encoding),
        ))
    }
}
