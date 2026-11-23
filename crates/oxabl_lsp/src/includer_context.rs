//! The compilation context of an open include file.
//!
//! An include has no meaning of its own: what it declares is used or unused,
//! defined or not, only in a root that expands it. By default an open `.i` is
//! analysed as a fragment. The client can list the roots that expand it
//! (`oxabl/includerContexts`) and select one (`oxabl/selectIncluderContext`).
//! Then the include's diagnostics are the findings of that root's expanded
//! compilation unit that sit in the include, with the open buffers over the disk.
//! The server never selects a root by itself.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use lsp_server::{ErrorCode, Message, Request, RequestId, Response};
use lsp_types::Uri;
use oxabl_daemon::db::{ContextDiagnostics, compute_context_diagnostics};
use oxabl_daemon::methods::includer_contexts;
use oxabl_daemon::{CompletedWork, Disposition, canonical_path, dispose};
use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::diagnostics::context_to_lsp_diagnostics;
use crate::{Server, path_to_uri, uri_to_path};

pub(crate) const LIST_METHOD: &str = "oxabl/includerContexts";
pub(crate) const SELECT_METHOD: &str = "oxabl/selectIncluderContext";

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct ListParams {
    text_document: lsp_types::TextDocumentIdentifier,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct SelectParams {
    text_document: lsp_types::TextDocumentIdentifier,
    /// The root to analyse the include in, or `null` for fragment mode.
    root: Option<PathBuf>,
}

/// One root the client can choose.
#[derive(Serialize)]
struct ContextRow {
    root: PathBuf,
    uri: Option<String>,
    /// Whether the root writes the `{...}` for this include itself.
    direct: bool,
}

/// The selected roots and the work in flight for them.
#[derive(Default)]
pub(crate) struct Contexts {
    /// The selected root of each open include, by canonical path.
    selected: HashMap<Uri, PathBuf>,
    /// A count of selection requests per include. A selection that completes
    /// after a later one was asked for is refused, never applied.
    requests: HashMap<Uri, u64>,
}

impl Contexts {
    pub(crate) fn selected(&self, uri: &Uri) -> Option<&PathBuf> {
        self.selected.get(uri)
    }

    pub(crate) fn has_selection(&self, uri: &Uri) -> bool {
        self.selected.contains_key(uri)
    }

    pub(crate) fn forget(&mut self, uri: &Uri) {
        self.selected.remove(uri);
        self.requests.remove(uri);
    }

    /// The includes whose selected root is `root`.
    pub(crate) fn users_of(&self, root: &Path) -> Vec<Uri> {
        let root = canonical_path(root);
        self.selected
            .iter()
            .filter(|(_, selected)| **selected == root)
            .map(|(uri, _)| uri.clone())
            .collect()
    }
}

/// What a context worker sends back to the main loop.
pub(crate) enum ContextEvent {
    /// A selection was checked against the includer list.
    Checked {
        id: RequestId,
        uri: Uri,
        request: u64,
        /// The root, or why it cannot be selected.
        outcome: Result<PathBuf, String>,
    },
    /// The diagnostics of an include in a selected root.
    Computed(ContextResult),
}

pub(crate) struct ContextResult {
    uri: Uri,
    version: i32,
    root: PathBuf,
    /// `None` when the read was cancelled or panicked.
    outcome: Option<ContextDiagnostics>,
    panicked: bool,
    config_generation: u64,
}

/// The path of an open include file, or why the request does not apply to it.
fn include_path(uri: &Uri) -> Result<PathBuf, String> {
    let path = uri_to_path(uri).ok_or_else(|| format!("{} is not a file", uri.as_str()))?;
    if !oxabl_workspace::is_include_fragment(&path) {
        return Err(format!("{} is not an include file", path.display()));
    }
    Ok(canonical_path(&path))
}

impl Server<'_> {
    /// `oxabl/includerContexts`: the roots that expand an include, the selected
    /// one, and why the list can be incomplete.
    pub(crate) fn handle_includer_contexts(&mut self, req: Request) -> anyhow::Result<()> {
        let params: ListParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, error.to_string())),
        };
        let uri = params.text_document.uri;
        let include = match include_path(&uri) {
            Ok(path) => path,
            Err(error) => return self.respond(invalid(req.id, error)),
        };
        let selected = self.contexts.selected(&uri).cloned();
        let host = self.host.clone();
        let root = self.session_root.clone();
        let sender = self.connection.sender.clone();
        let id = req.id;
        spawn_worker(sender.clone(), id.clone(), move || {
            let response = match includer_contexts(&host, &root, &include) {
                Err(error) => failed(id, error.to_string()),
                Ok(Err(reason)) => Response::new_ok(
                    id,
                    json!({
                        "selected": selected,
                        "contexts": [],
                        "complete": false,
                        "reason": reason,
                    }),
                ),
                Ok(Ok(found)) => {
                    let reason = if !found.gaps.is_empty() {
                        Some(format!(
                            "the list can be incomplete: {}",
                            found.gaps.join("; ")
                        ))
                    } else if found.roots.is_empty() {
                        Some("no root file in the workspace includes this file".to_string())
                    } else {
                        None
                    };
                    let rows: Vec<ContextRow> = found
                        .roots
                        .into_iter()
                        .map(|(root, direct)| ContextRow {
                            uri: path_to_uri(&root).map(|uri| uri.as_str().to_string()),
                            root,
                            direct,
                        })
                        .collect();
                    Response::new_ok(
                        id,
                        json!({
                            "selected": selected,
                            "contexts": rows,
                            "complete": found.gaps.is_empty(),
                            "reason": reason,
                        }),
                    )
                }
            };
            let _ = sender.send(Message::Response(response));
        });
        Ok(())
    }

    /// `oxabl/selectIncluderContext`: analyse an include in one root, or as a
    /// fragment again. A root is applied only after the includer list confirms
    /// it expands the include.
    pub(crate) fn handle_select_includer_context(&mut self, req: Request) -> anyhow::Result<()> {
        let params: SelectParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, error.to_string())),
        };
        let uri = params.text_document.uri;
        let include = match include_path(&uri) {
            Ok(path) => path,
            Err(error) => return self.respond(invalid(req.id, error)),
        };
        let request = {
            let count = self.contexts.requests.entry(uri.clone()).or_default();
            *count += 1;
            *count
        };
        let Some(wanted) = params.root else {
            self.contexts.selected.remove(&uri);
            self.debouncer.schedule(uri, std::time::Instant::now());
            return self.respond(Response::new_ok(req.id, json!({ "selected": null })));
        };
        let wanted = canonical_path(&wanted);
        let host = self.host.clone();
        let root = self.session_root.clone();
        let events = self.context_events.clone();
        spawn_worker(self.connection.sender.clone(), req.id.clone(), move || {
            let outcome = match includer_contexts(&host, &root, &include) {
                Err(error) => Err(error.to_string()),
                Ok(Err(reason)) => Err(reason),
                Ok(Ok(found)) if found.roots.iter().any(|(root, _)| *root == wanted) => Ok(wanted),
                Ok(Ok(_)) => Err(format!(
                    "{} does not include this file in the workspace index",
                    wanted.display()
                )),
            };
            let _ = events.send(ContextEvent::Checked {
                id: req.id,
                uri,
                request,
                outcome,
            });
        });
        Ok(())
    }

    pub(crate) fn handle_context_event(&mut self, event: ContextEvent) -> anyhow::Result<()> {
        match event {
            ContextEvent::Checked {
                id,
                uri,
                request,
                outcome,
            } => {
                if self.contexts.requests.get(&uri) != Some(&request) {
                    return self.respond(Response::new_err(
                        id,
                        ErrorCode::ContentModified as i32,
                        "oxabl-lsp: a later selection replaced this one".to_string(),
                    ));
                }
                if self.documents.get(&uri).is_none() {
                    return self.respond(invalid(id, "the include is not open".to_string()));
                }
                match outcome {
                    Ok(root) => {
                        self.contexts.selected.insert(uri.clone(), root.clone());
                        self.debouncer.schedule(uri, std::time::Instant::now());
                        self.respond(Response::new_ok(id, json!({ "selected": root })))
                    }
                    Err(reason) => self.respond(invalid(id, reason)),
                }
            }
            ContextEvent::Computed(result) => {
                self.handle_context_result(result);
                Ok(())
            }
        }
    }

    /// Start the analysis of `uri` in its selected root on a worker.
    pub(crate) fn spawn_context_analysis(&mut self, uri: Uri, version: i32, root: PathBuf) {
        let Some(include) = uri_to_path(&uri) else {
            return;
        };
        let Some((snapshot, schema, overlay, config_generation)) = self.host.with(|sessions| {
            let session = sessions.get(&self.session_root)?;
            Some((
                session.database().clone(),
                session.schema_handle(),
                session.buffer_overlay(),
                session.config_generation(),
            ))
        }) else {
            return;
        };
        let events = self.context_events.clone();
        oxabl_common::stack::builder()
            .name("oxabl-context-analysis".to_string())
            .spawn(move || {
                let computed = oxabl_common::catch_panic(|| {
                    compute_context_diagnostics(&snapshot, schema, &root, &include, overlay)
                });
                let (outcome, panicked) = match computed {
                    Ok(outcome) => (outcome, false),
                    Err(panic) => {
                        eprintln!(
                            "oxabl: context analysis panicked for {}: {panic}",
                            include.display()
                        );
                        (None, true)
                    }
                };
                let _ = events.send(ContextEvent::Computed(ContextResult {
                    uri,
                    version,
                    root,
                    outcome,
                    panicked,
                    config_generation,
                }));
            })
            .expect("failed to spawn a context analysis thread");
    }

    fn handle_context_result(&mut self, res: ContextResult) {
        // A result for a root that is no longer selected is no answer at all: the
        // change of selection already scheduled its own computation.
        if self.contexts.selected(&res.uri) != Some(&res.root) {
            return;
        }
        let work = CompletedWork {
            read_version: res.version,
            current_version: self.documents.get(&res.uri).map(|doc| doc.version),
            read_generation: res.config_generation,
            current_generation: self.config_generation(),
            has_diagnostics: res.outcome.is_some(),
            panicked: res.panicked,
        };
        match dispose(work) {
            Disposition::Drop => {}
            Disposition::Retry => {
                self.debouncer.schedule(res.uri, std::time::Instant::now());
            }
            Disposition::Publish => {
                let Some(outcome) = res.outcome else {
                    return;
                };
                self.record_dependencies(&res.uri, Some(outcome.dependencies.clone()));
                let Some(doc) = self.documents.get(&res.uri) else {
                    return;
                };
                let diagnostics =
                    context_to_lsp_diagnostics(&outcome, &res.root, &doc.rope, &self.encoding);
                let regions = outcome
                    .inactive_regions
                    .iter()
                    .map(|span| (span.start, span.end));
                self.publish_inactive_regions(&res.uri, res.version, regions);
                self.publish(res.uri, diagnostics, Some(res.version));
            }
        }
    }

    /// Schedule the includes analysed in `root`, after `root` changed.
    pub(crate) fn schedule_context_users(&mut self, root: &Path) {
        let now = std::time::Instant::now();
        for uri in self.contexts.users_of(root) {
            self.debouncer.schedule(uri, now);
        }
    }
}

fn invalid(id: RequestId, message: String) -> Response {
    Response::new_err(
        id,
        ErrorCode::InvalidParams as i32,
        format!("oxabl-lsp: {message}"),
    )
}

fn failed(id: RequestId, message: String) -> Response {
    Response::new_err(
        id,
        ErrorCode::RequestFailed as i32,
        format!("oxabl-lsp: {message}"),
    )
}

/// Run `work` on a worker thread. A worker that cannot start, or panics,
/// answers the request with an error rather than leaving it open.
fn spawn_worker(
    sender: crossbeam_channel::Sender<Message>,
    id: RequestId,
    work: impl FnOnce() + Send + 'static,
) {
    let guard_sender = sender.clone();
    let guard_id = id.clone();
    let spawned = oxabl_common::stack::builder()
        .name("oxabl-includer-context".to_string())
        .spawn(move || {
            if let Err(panic) = oxabl_common::catch_panic(work) {
                let _ = guard_sender.send(Message::Response(Response::new_err(
                    guard_id,
                    ErrorCode::InternalError as i32,
                    format!("oxabl-lsp: the includer context request failed: {panic}"),
                )));
            }
        });
    if let Err(error) = spawned {
        let _ = sender.send(Message::Response(Response::new_err(
            id,
            ErrorCode::InternalError as i32,
            format!("oxabl-lsp: cannot start the includer context request: {error}"),
        )));
    }
}
