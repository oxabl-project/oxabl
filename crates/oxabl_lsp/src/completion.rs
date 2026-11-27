//! `textDocument/completion` and `completionItem/resolve`. See
//! `docs/design/completion.md`.
//!
//! The daemon computes the items on a worker thread
//! ([`oxabl_daemon::completion`]); this module negotiates what the client
//! takes, orders the requests of each document, and renders the items in the
//! negotiated position encoding.
//!
//! The requests of one document run one at a time ([`HoverGate`]). A request
//! that a newer one replaced before it started, or that the client cancelled
//! with `$/cancelRequest`, answers `RequestCanceled` and computes nothing. A
//! request holds no database snapshot, so an edit never waits for it. A
//! failed analysis answers an error, never an empty list.

use std::collections::HashMap;

use lsp_server::{ErrorCode, Message, Request, RequestId, Response};
use lsp_types::{
    Command, CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams,
    CompletionTextEdit, Documentation, InsertReplaceEdit, InsertTextFormat, MarkupContent,
    MarkupKind, PositionEncodingKind, Range, TextEdit, Uri,
};
use oxabl_daemon::canonical_path;
use oxabl_daemon::completion::{
    BaseKey, CompletionQuery, ResolveQuery, compute_completion, compute_resolve,
};
use oxabl_pipeline::completion::{CompletionList, ItemData, ItemKind};
use ropey::Rope;
use serde_json::{Value, json};

use crate::navigation::HoverGate;
use crate::position::{byte_to_position, position_to_byte};
use crate::{Server, uri_to_path};

/// The characters after which a client asks for completion by itself.
pub(crate) const TRIGGER_CHARACTERS: [&str; 5] = [":", ".", "{", "&", "/"];

/// The capability the server advertises to every client.
pub(crate) fn capability() -> CompletionOptions {
    CompletionOptions {
        resolve_provider: Some(true),
        trigger_characters: Some(TRIGGER_CHARACTERS.iter().map(|c| c.to_string()).collect()),
        ..Default::default()
    }
}

/// What the client takes in a completion item.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct Options {
    /// `snippetSupport`: snippet items.
    pub snippets: bool,
    /// `insertReplaceSupport`: an `InsertReplaceEdit`.
    pub insert_replace: bool,
}

impl Options {
    pub(crate) fn from_client(caps: &lsp_types::ClientCapabilities) -> Self {
        let item = caps
            .text_document
            .as_ref()
            .and_then(|c| c.completion.as_ref())
            .and_then(|c| c.completion_item.as_ref());
        Options {
            snippets: item.and_then(|i| i.snippet_support).unwrap_or(false),
            insert_replace: item.and_then(|i| i.insert_replace_support).unwrap_or(false),
        }
    }
}

/// The completion requests of each open document, one at a time.
pub(crate) type Gates = HashMap<Uri, HoverGate>;

impl Server<'_> {
    pub(crate) fn handle_completion(&mut self, req: Request) -> anyhow::Result<()> {
        let params: CompletionParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let position = params.text_document_position;
        let uri = position.text_document.uri;
        let Some(path) = uri_to_path(&uri) else {
            return self.respond(invalid(req.id, &format!("{} is not a file", uri.as_str())));
        };
        let Some(document) = self.documents.get(&uri) else {
            return self.respond(invalid(req.id, &format!("{} is not open", uri.as_str())));
        };
        let rope = document.rope.clone();
        let offset = position_to_byte(&rope, position.position, &self.encoding) as u32;
        let path = canonical_path(&path);
        let root = self
            .contexts
            .selected(&uri)
            .cloned()
            .filter(|root| *root != path);
        let ticket = self
            .completions
            .entry(uri.clone())
            .or_default()
            .ticket(&req.id);
        let options = self.completion;
        let trigger = params
            .context
            .and_then(|context| context.trigger_character)
            .and_then(|trigger| trigger.chars().next());
        let text: std::sync::Arc<str> = std::sync::Arc::from(rope.to_string());
        // No database snapshot: the request reads copied inputs only, so it
        // never makes an edit wait (see `oxabl_daemon::completion`).
        let Some((bases, query)) = self.host.with(|sessions| {
            let session = sessions.get(&self.session_root)?;
            let query = CompletionQuery {
                key: BaseKey {
                    document: uri.as_str().to_string(),
                    root: root.clone(),
                },
                path: path.clone(),
                text,
                offset,
                snippets: options.snippets,
                trigger,
                overlay: if root.is_some() {
                    session.buffer_overlay()
                } else {
                    HashMap::new()
                },
                config: session.database().config().clone(),
                generation: (session.config_generation(), session.schema_revision()),
            };
            Some((session.completion_bases(), query))
        }) else {
            return self.respond(Response::new_err(
                req.id,
                ErrorCode::RequestFailed as i32,
                "oxabl-lsp: no session".to_string(),
            ));
        };
        let encoding = self.encoding.clone();
        let sender = self.connection.sender.clone();
        let id = req.id.clone();
        let spawned = oxabl_common::stack::builder()
            .name("oxabl-completion".to_string())
            .spawn(move || {
                let Some(_running) = ticket.start() else {
                    let _ = sender.send(Message::Response(Response::new_err(
                        id,
                        ErrorCode::RequestCanceled as i32,
                        "oxabl-lsp: a newer completion or the client cancelled this one"
                            .to_string(),
                    )));
                    return;
                };
                let answered = oxabl_common::catch_panic(|| compute_completion(&bases, &query));
                let response = match answered {
                    Ok(Ok(answer)) => {
                        let value = match answer.list {
                            Some(list) => serde_json::to_value(render(
                                list,
                                answer.base,
                                &rope,
                                &encoding,
                                options,
                            )),
                            None => Ok(Value::Null),
                        };
                        match value {
                            Ok(value) => Response::new_ok(id, value),
                            Err(error) => Response::new_err(
                                id,
                                ErrorCode::InternalError as i32,
                                format!("oxabl-lsp: cannot render the completion: {error}"),
                            ),
                        }
                    }
                    Ok(Err(reason)) => Response::new_err(
                        id,
                        ErrorCode::RequestFailed as i32,
                        format!("oxabl-lsp: no completion: {reason}"),
                    ),
                    Err(panic) => Response::new_err(
                        id,
                        ErrorCode::InternalError as i32,
                        format!("oxabl-lsp: the completion failed: {panic}"),
                    ),
                };
                let _ = sender.send(Message::Response(response));
            });
        if let Err(error) = spawned {
            return self.respond(Response::new_err(
                req.id,
                ErrorCode::InternalError as i32,
                format!("oxabl-lsp: cannot start the completion: {error}"),
            ));
        }
        Ok(())
    }

    pub(crate) fn handle_completion_resolve(&mut self, req: Request) -> anyhow::Result<()> {
        let mut item: CompletionItem = match serde_json::from_value(req.params) {
            Ok(item) => item,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let Some(query) = item.data.as_ref().and_then(resolve_query) else {
            return self.respond(Response::new_ok(req.id, item));
        };
        let Some(bases) = self
            .host
            .with(|sessions| Some(sessions.get(&self.session_root)?.completion_bases()))
        else {
            return self.respond(Response::new_ok(req.id, item));
        };
        let sender = self.connection.sender.clone();
        let id = req.id.clone();
        let spawned = oxabl_common::stack::builder()
            .name("oxabl-completion-resolve".to_string())
            .spawn(move || {
                let documentation = oxabl_common::catch_panic(|| compute_resolve(&bases, query));
                let response = match documentation {
                    Ok(documentation) => {
                        if let Some(value) = documentation {
                            item.documentation =
                                Some(Documentation::MarkupContent(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value,
                                }));
                        }
                        Response::new_ok(id, item)
                    }
                    Err(panic) => Response::new_err(
                        id,
                        ErrorCode::InternalError as i32,
                        format!("oxabl-lsp: the resolve failed: {panic}"),
                    ),
                };
                let _ = sender.send(Message::Response(response));
            });
        if let Err(error) = spawned {
            return self.respond(Response::new_err(
                req.id,
                ErrorCode::InternalError as i32,
                format!("oxabl-lsp: cannot start the resolve: {error}"),
            ));
        }
        Ok(())
    }

    /// `$/cancelRequest` for `id`: a completion that has not started.
    pub(crate) fn cancel_completion(&self, id: &RequestId) {
        for gate in self.completions.values() {
            gate.cancel(id);
        }
    }
}

/// What resolve reads from an item's `data`.
fn resolve_query(data: &Value) -> Option<ResolveQuery> {
    if let Some(doc) = data.get("doc").and_then(Value::as_u64) {
        return Some(ResolveQuery::Doc(u16::try_from(doc).ok()?));
    }
    if let Some(path) = data.get("path").and_then(Value::as_str) {
        let entry = data.get("entry").and_then(Value::as_str)?;
        return Some(ResolveQuery::Path {
            path: path.to_string(),
            entry: entry.into(),
        });
    }
    let number = |key: &str| {
        data.get(key)
            .and_then(Value::as_u64)
            .and_then(|n| u32::try_from(n).ok())
    };
    let base = data.get("base").and_then(Value::as_u64)?;
    if let Some(member) = data.get("member").and_then(Value::as_str) {
        return Some(ResolveQuery::Member {
            base,
            class: data.get("class").and_then(Value::as_str)?.to_string(),
            kind: data.get("kind").and_then(Value::as_str)?.to_string(),
            name: member.to_string(),
        });
    }
    if let Some(table) = number("table") {
        return Some(match number("field") {
            Some(field) => ResolveQuery::Field { base, table, field },
            None => ResolveQuery::Table { base, table },
        });
    }
    Some(ResolveQuery::Symbol {
        base,
        symbol: number("symbol")?,
    })
}

fn render(
    list: CompletionList,
    base: u64,
    rope: &Rope,
    encoding: &PositionEncodingKind,
    options: Options,
) -> lsp_types::CompletionList {
    let position = |at: u32| byte_to_position(rope, at as usize, encoding);
    let start = position(list.word_start);
    let insert = Range::new(start, position(list.cursor));
    let replace = Range::new(start, position(list.word_end.max(list.cursor)));
    let items = list
        .items
        .into_iter()
        .map(|item| {
            let edit = if options.insert_replace {
                CompletionTextEdit::InsertAndReplace(InsertReplaceEdit {
                    new_text: item.insert,
                    insert,
                    replace,
                })
            } else {
                CompletionTextEdit::Edit(TextEdit {
                    range: insert,
                    new_text: item.insert,
                })
            };
            let data = match item.data {
                ItemData::None => None,
                ItemData::Doc(doc) => Some(json!({ "doc": doc })),
                ItemData::Symbol(symbol) => Some(json!({ "base": base, "symbol": symbol })),
                ItemData::Table(table) => Some(json!({ "base": base, "table": table })),
                ItemData::Field { table, field } => {
                    Some(json!({ "base": base, "table": table, "field": field }))
                }
                ItemData::Path(data) => {
                    Some(json!({ "path": data.path, "entry": data.entry.to_string_lossy() }))
                }
                ItemData::Member(data) => Some(json!({
                    "base": base,
                    "class": data.class,
                    "kind": data.kind,
                    "member": data.name,
                })),
            };
            // A directory asks for the list of its entries.
            let command = (item.kind == ItemKind::Folder).then(|| Command {
                title: "Suggest".to_string(),
                command: "editor.action.triggerSuggest".to_string(),
                arguments: None,
            });
            CompletionItem {
                filter_text: Some(item.label.clone()),
                label: item.label,
                kind: Some(kind(item.kind)),
                detail: item.detail,
                sort_text: Some(item.sort_text),
                text_edit: Some(edit),
                insert_text_format: item.snippet.then_some(InsertTextFormat::SNIPPET),
                data,
                command,
                ..Default::default()
            }
        })
        .collect();
    lsp_types::CompletionList {
        is_incomplete: list.incomplete,
        items,
    }
}

fn kind(kind: ItemKind) -> CompletionItemKind {
    match kind {
        ItemKind::Keyword => CompletionItemKind::KEYWORD,
        ItemKind::Snippet => CompletionItemKind::SNIPPET,
        ItemKind::Variable => CompletionItemKind::VARIABLE,
        ItemKind::Property => CompletionItemKind::PROPERTY,
        ItemKind::Method => CompletionItemKind::METHOD,
        ItemKind::Function => CompletionItemKind::FUNCTION,
        ItemKind::Struct => CompletionItemKind::STRUCT,
        ItemKind::Field => CompletionItemKind::FIELD,
        ItemKind::Class => CompletionItemKind::CLASS,
        ItemKind::Interface => CompletionItemKind::INTERFACE,
        ItemKind::Enum => CompletionItemKind::ENUM,
        ItemKind::EnumMember => CompletionItemKind::ENUM_MEMBER,
        ItemKind::Event => CompletionItemKind::EVENT,
        ItemKind::Module => CompletionItemKind::MODULE,
        ItemKind::File => CompletionItemKind::FILE,
        ItemKind::Folder => CompletionItemKind::FOLDER,
        ItemKind::Constant => CompletionItemKind::CONSTANT,
    }
}

fn invalid(id: RequestId, message: &str) -> Response {
    Response::new_err(
        id,
        ErrorCode::InvalidParams as i32,
        format!("oxabl-lsp: {message}"),
    )
}
