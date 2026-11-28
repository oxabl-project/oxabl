//! `textDocument/documentSymbol` with the declarations of include files.
//!
//! The server expands the buffer with the open buffers over the disk and puts
//! the outline of each include under its `{file.i}` symbol
//! ([`oxabl_pipeline::outline_with_includes`]). The work runs on a worker
//! thread, because it reads files. The setting `outline.includes` turns it
//! off, and then the outline is the raw outline of the buffer alone. See
//! `docs/design/document-outline.md`.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::Ordering;

use lsp_server::{ErrorCode, Message, RequestId, Response};
use lsp_types::Uri;
use oxabl_daemon::canonical_path;
use oxabl_daemon::db::AnalysisConfig;
use oxabl_pipeline::{
    IncludeOutline, LintPipeline, outline_with_includes, outline_with_includes_of,
};
use oxabl_workspace::FileSystem;
use serde_json::Value;

use crate::document_symbol::{self, Computed};
use crate::{Server, uri_to_path};

/// The `outline` setting.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Settings {
    /// Whether the outline holds the declarations of include files. On unless
    /// the client turns it off.
    pub(crate) includes: bool,
}

impl Default for Settings {
    fn default() -> Self {
        Settings { includes: true }
    }
}

impl Settings {
    /// The `outline` object of the initialization options.
    pub(crate) fn from_init(initialization: Option<&Value>) -> Self {
        let mut settings = Settings::default();
        if let Some(includes) = initialization.and_then(includes_in) {
            settings.includes = includes;
        }
        settings
    }
}

/// The `includes` flag of the `outline` object of `root`.
fn includes_in(root: &Value) -> Option<bool> {
    root.get("outline")?.get("includes")?.as_bool()
}

/// Open buffers over a file system, keyed by canonical path.
struct OverlayFs {
    overlay: HashMap<PathBuf, Arc<str>>,
    inner: Arc<dyn FileSystem>,
}

impl FileSystem for OverlayFs {
    fn read(&self, path: &Path) -> Result<Arc<str>, std::io::Error> {
        match self
            .overlay
            .get(path)
            .or_else(|| self.overlay.get(&canonical_path(path)))
        {
            Some(text) => Ok(Arc::clone(text)),
            None => self.inner.read(path),
        }
    }

    fn exists(&self, path: &Path) -> bool {
        self.overlay.contains_key(path) || self.inner.exists(path)
    }
}

/// The outline of `text`, the content of the file at `path`, with its includes.
/// `None` when `stop` said that nobody needs the answer.
/// `unit` is the raw unit of `text` when the session has it.
struct ComputeSource<'a> {
    text: &'a str,
    uri: &'a Uri,
    units: &'a crate::RawUnits,
    path: &'a Path,
}

fn compute(
    source: ComputeSource<'_>,
    config: &AnalysisConfig,
    overlay: HashMap<PathBuf, Arc<str>>,
    stop: &dyn Fn() -> bool,
) -> Option<Result<IncludeOutline, String>> {
    let ComputeSource {
        text,
        uri,
        units,
        path,
    } = source;
    oxabl_common::hold_if_injected(oxabl_common::panic_sites::OUTLINE_INCLUDES, text);
    let fs = OverlayFs {
        overlay,
        inner: Arc::clone(&config.fs),
    };
    let computed = oxabl_common::catch_panic(|| {
        if stop() {
            return None;
        }
        let unit = units.of(uri, text);
        if stop() {
            return None;
        }
        let run = LintPipeline::new(&config.pipeline, &fs)
            .with_preprocess(config.preprocess)
            .with_branch_tracking(true);
        let expansion = run.with_file(path).expand(text);
        if stop() {
            return None;
        }
        Some(match unit.as_deref() {
            Some(unit) => outline_with_includes_of(unit, &expansion),
            None => outline_with_includes(text, &expansion),
        })
    });
    match computed {
        Ok(None) => None,
        Ok(Some(outline)) => Some(outline.map_err(|panic| panic.to_string())),
        Err(panic) => Some(Err(panic.to_string())),
    }
}

impl Server<'_> {
    /// Apply the `outline` setting of `workspace/didChangeConfiguration`.
    pub(crate) fn handle_outline_settings(&mut self, params: &Value) {
        let settings = &params["settings"];
        let includes = settings
            .get("oxabl")
            .and_then(includes_in)
            .or_else(|| includes_in(settings));
        if let Some(includes) = includes
            && self.outline_settings.includes != includes
        {
            self.outline_settings.includes = includes;
            self.outlines.invalidate_includes();
        }
    }

    /// `textDocument/documentSymbol` for an open buffer, with the symbols of
    /// its includes. The answer comes from a worker thread. Returns `false`
    /// when the request cannot be served this way, and the caller then answers
    /// with the raw outline.
    pub(crate) fn handle_document_symbol_includes(&self, id: &RequestId, uri: &Uri) -> bool {
        let uri = uri.clone();
        let Some(path) = uri_to_path(&uri).map(|path| canonical_path(&path)) else {
            return false;
        };
        let Some(doc) = self.documents.get(&uri) else {
            return false;
        };
        let Some((config, overlay)) = self.host.with(|sessions| {
            let session = sessions.get(&self.session_root)?;
            Some((
                session.database().config().clone(),
                session.buffer_overlay(),
            ))
        }) else {
            return false;
        };
        let slot = self.outlines.include_slot(&uri, doc.version);
        let requests = self.outlines.requests.clone();
        let cancelled = requests.register(id);
        let text = doc.text();
        let rope = doc.rope.clone();
        let encoding = self.encoding.clone();
        let hierarchical = self.hierarchical_symbols;
        let base = self
            .workspace_root
            .clone()
            .map(|root| canonical_path(&root));
        let sender = self.connection.sender.clone();
        let units = self.raw_units();
        let spawned = oxabl_common::stack::builder()
            .name("oxabl-outline".to_string())
            .spawn({
                let id = id.clone();
                move || {
                    let computed = slot.get_or_compute(&cancelled, |stop| {
                        compute(
                            ComputeSource {
                                text: &text,
                                uri: &uri,
                                units: &units,
                                path: &path,
                            },
                            &config,
                            overlay,
                            stop,
                        )
                    });
                    let response = match computed {
                        Computed::Stopped if cancelled.load(Ordering::SeqCst) => Response::new_err(
                            id.clone(),
                            ErrorCode::RequestCanceled as i32,
                            "oxabl-lsp: the outline request was cancelled".to_string(),
                        ),
                        Computed::Stopped => Response::new_err(
                            id.clone(),
                            ErrorCode::ContentModified as i32,
                            "oxabl-lsp: an edit replaced this outline".to_string(),
                        ),
                        Computed::Done(outline) => {
                            match outline.and_then(|outline| {
                                oxabl_common::catch_panic(|| {
                                    document_symbol::to_response_with_includes(
                                        &outline,
                                        &uri,
                                        &rope,
                                        &encoding,
                                        hierarchical,
                                        base.as_deref(),
                                    )
                                })
                                .map_err(|panic| panic.to_string())
                            }) {
                                Ok(answer) => Response::new_ok(id.clone(), answer),
                                Err(panic) => Response::new_err(
                                    id.clone(),
                                    ErrorCode::RequestFailed as i32,
                                    format!("oxabl-lsp: the outline failed: {panic}"),
                                ),
                            }
                        }
                    };
                    requests.finish(&id);
                    let _ = sender.send(Message::Response(response));
                }
            });
        if let Err(error) = spawned {
            self.outlines.requests.finish(id);
            let _ = self.respond(Response::new_err(
                id.clone(),
                ErrorCode::InternalError as i32,
                format!("oxabl-lsp: cannot start the outline: {error}"),
            ));
        }
        true
    }
}
