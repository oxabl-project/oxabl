//! Ready-only code lenses. Graph construction runs on daemon snapshots in workers.

use std::collections::HashMap;

use lsp_server::{ErrorCode, Request, Response};
use lsp_types::{CodeLens, CodeLensParams, Command, PositionEncodingKind, Range, Uri};

use crate::{Server, position::byte_to_position};

pub(crate) struct Options {
    pub encoding: PositionEncodingKind,
    pub supported: bool,
    pub dynamic: bool,
    pub refresh: bool,
    /// The user turned the reference-count lenses on.
    pub reference_lenses: bool,
}

impl Options {
    pub(crate) fn from_client(
        caps: &lsp_types::ClientCapabilities,
        encoding: PositionEncodingKind,
        initialization: Option<&serde_json::Value>,
    ) -> Self {
        let lens = caps
            .text_document
            .as_ref()
            .and_then(|c| c.code_lens.as_ref());
        Self {
            encoding,
            supported: lens.is_some(),
            dynamic: lens.and_then(|c| c.dynamic_registration).unwrap_or(false),
            refresh: caps
                .workspace
                .as_ref()
                .and_then(|c| c.code_lens.as_ref())
                .and_then(|c| c.refresh_support)
                .unwrap_or(false),
            reference_lenses: initialization
                .and_then(|o| o.get("referenceLenses"))
                .and_then(crate::reference_lens::enabled_in)
                .unwrap_or(false),
        }
    }
}

pub(crate) struct LensCache {
    epoch: u64,
    requests: u64,
    generations: Option<(u64, u64, u32)>,
    options: Options,
    registered: Vec<String>,
    registered_resolve: bool,
    /// A static client was told at the handshake that lenses resolve. Without
    /// that, it never resolves one, so a lens of a later setting has no count.
    resolve_advertised: bool,
    // An absent result marks an in-flight request, avoiding duplicate workers.
    ready: HashMap<Uri, Option<Vec<CodeLens>>>,
}

pub(crate) struct LensResult {
    epoch: u64,
    uri: Uri,
    lenses: Option<Vec<CodeLens>>,
}

impl LensCache {
    pub(crate) fn new(options: Options) -> Self {
        let resolve_advertised = !options.dynamic && options.reference_lenses;
        Self {
            epoch: 0,
            requests: 0,
            generations: None,
            options,
            registered: Vec::new(),
            registered_resolve: false,
            resolve_advertised,
            ready: HashMap::new(),
        }
    }

    pub(crate) fn supported(&self) -> bool {
        self.options.supported
    }

    pub(crate) fn refresh_supported(&self) -> bool {
        self.options.refresh
    }

    pub(crate) fn reference_lenses(&self) -> bool {
        self.options.reference_lenses && (self.options.dynamic || self.resolve_advertised)
    }

    /// Set the reference-lens choice; returns the one it replaced.
    pub(crate) fn set_reference_lenses(&mut self, enabled: bool) -> bool {
        std::mem::replace(&mut self.options.reference_lenses, enabled)
    }

    pub(crate) fn request_id(&mut self) -> lsp_server::RequestId {
        self.requests += 1;
        lsp_server::RequestId::from(format!("oxabl-lenses-{}", self.requests))
    }

    pub(crate) fn invalidate(&mut self) {
        self.epoch += 1;
        self.ready.clear();
    }

    pub(crate) fn complete(&mut self, result: LensResult) -> bool {
        if result.epoch == self.epoch {
            if let Some(lenses) = result.lenses {
                self.ready.insert(result.uri, Some(lenses));
                return self.options.refresh;
            } else {
                self.ready.remove(&result.uri);
            }
        }
        false
    }
}

impl Server<'_> {
    fn lens_enabled(&self, uri: &Uri) -> bool {
        let Some(path) = crate::uri_to_path(uri) else {
            return false;
        };
        if !lens_file(&path) {
            return false;
        }
        self.host.with(|sessions| {
            sessions.get(&self.session_root).is_some_and(|session| {
                session
                    .database()
                    .config()
                    .lint_pipeline()
                    .with_file(&path)
                    .effect_lenses_enabled()
            })
        })
    }

    pub(crate) fn update_lens_registration(&mut self) {
        if !self.lenses.options.dynamic {
            return;
        }
        let mut files: Vec<String> = self
            .documents
            .uris()
            .filter(|uri| self.lens_enabled(uri) || self.reference_lenses_apply(uri))
            .filter_map(crate::uri_to_path)
            .map(|p| p.to_string_lossy().into_owned())
            .collect();
        files.sort();
        let resolve = self.reference_lenses_on();
        if files == self.lenses.registered && resolve == self.lenses.registered_resolve {
            return;
        }
        if !self.lenses.registered.is_empty() {
            let _ = self.connection.sender.send(lsp_server::Message::Request(Request::new(
                self.lenses.request_id(),
                "client/unregisterCapability".into(),
                serde_json::json!({"unregisterations": [{"id": "oxabl-lenses", "method": "textDocument/codeLens"}]}),
            )));
        }
        if !files.is_empty() {
            let selector: Vec<_> = files
                .iter()
                .map(|path| serde_json::json!({"scheme": "file", "pattern": path}))
                .collect();
            let _ = self.connection.sender.send(lsp_server::Message::Request(Request::new(
                self.lenses.request_id(),
                "client/registerCapability".into(),
                serde_json::json!({"registrations": [{"id": "oxabl-lenses", "method": "textDocument/codeLens",
                    "registerOptions": {"documentSelector": selector, "resolveProvider": resolve}}]}),
            )));
        }
        self.lenses.registered = files;
        self.lenses.registered_resolve = resolve;
    }

    pub(crate) fn handle_code_lens(
        &mut self,
        req: Request,
        tx: &crossbeam_channel::Sender<LensResult>,
    ) -> anyhow::Result<()> {
        let params: CodeLensParams = match serde_json::from_value(req.params) {
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
        if !self.lenses.options.supported {
            return self.respond(Response::new_ok(req.id, Vec::<CodeLens>::new()));
        }
        let mut lenses = if self.lens_enabled(&uri) {
            self.effect_lenses(&uri, tx)
        } else {
            Vec::new()
        };
        lenses.extend(self.reference_lens_ranges(&uri));
        self.respond(Response::new_ok(req.id, lenses))
    }

    /// The effect lenses that are ready; the first request starts the worker
    /// that makes them.
    fn effect_lenses(
        &mut self,
        uri: &Uri,
        tx: &crossbeam_channel::Sender<LensResult>,
    ) -> Vec<CodeLens> {
        let uri = uri.clone();
        let generations = self.host.with(|sessions| {
            sessions.get(&self.session_root).map(|s| {
                (
                    s.buffer_generation(),
                    s.config_generation(),
                    s.schema_revision(),
                )
            })
        });
        if generations != self.lenses.generations {
            self.lenses.invalidate();
            self.lenses.generations = generations;
        }
        if let Some(ready) = self.lenses.ready.get(&uri) {
            return ready.clone().unwrap_or_default();
        }
        if let Some(doc) = self.documents.get(&uri)
            && let Some((buffer, snapshot, schema)) = self.host.with(|sessions| {
                let session = sessions.get(&self.session_root)?;
                Some((
                    session.buffer(uri.as_str())?,
                    session.database().clone(),
                    session.schema_handle(),
                ))
            })
        {
            let epoch = self.lenses.epoch;
            let rope = doc.rope.clone();
            let encoding = self.encoding.clone();
            let tx = tx.clone();
            self.lenses.ready.insert(uri.clone(), None);
            oxabl_common::stack::builder()
                .spawn(move || {
                    let lenses = oxabl_common::catch_panic(|| {
                        oxabl_daemon::db::compute_effect_lenses(&snapshot, buffer, schema).map(
                            |lenses| {
                                lenses
                                    .into_iter()
                                    .map(|(at, title)| {
                                        let position =
                                            byte_to_position(&rope, at as usize, &encoding);
                                        CodeLens {
                                            range: Range::new(position, position),
                                            command: Some(Command {
                                                title,
                                                command: String::new(),
                                                arguments: None,
                                            }),
                                            data: None,
                                        }
                                    })
                                    .collect()
                            },
                        )
                    })
                    .ok()
                    .flatten();
                    let _ = tx.send(LensResult { epoch, uri, lenses });
                })
                .expect("failed to spawn an analysis thread");
        }
        Vec::new()
    }
}

/// A file that can have lenses: a root file or an include.
pub(crate) fn lens_file(path: &std::path::Path) -> bool {
    oxabl_workspace::is_root_file(path) || oxabl_workspace::is_include_fragment(path)
}
