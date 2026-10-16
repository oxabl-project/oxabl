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
}

impl Options {
    pub(crate) fn from_client(
        caps: &lsp_types::ClientCapabilities,
        encoding: PositionEncodingKind,
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
        }
    }
}

pub(crate) struct LensCache {
    epoch: u64,
    requests: u64,
    generations: Option<(u64, u64, u32)>,
    options: Options,
    registered: Vec<String>,
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
        Self {
            epoch: 0,
            requests: 0,
            generations: None,
            options,
            registered: Vec::new(),
            ready: HashMap::new(),
        }
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
        if !oxabl_workspace::is_root_file(&path) && !oxabl_workspace::is_include_fragment(&path) {
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
            .filter(|uri| self.lens_enabled(uri))
            .filter_map(crate::uri_to_path)
            .map(|p| p.to_string_lossy().into_owned())
            .collect();
        files.sort();
        if files == self.lenses.registered {
            return;
        }
        if !self.lenses.registered.is_empty() {
            let _ = self.connection.sender.send(lsp_server::Message::Request(Request::new(
                self.lenses.request_id(),
                "client/unregisterCapability".into(),
                serde_json::json!({"unregisterations": [{"id": "oxabl-effects", "method": "textDocument/codeLens"}]}),
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
                serde_json::json!({"registrations": [{"id": "oxabl-effects", "method": "textDocument/codeLens",
                    "registerOptions": {"documentSelector": selector, "resolveProvider": false}}]}),
            )));
        }
        self.lenses.registered = files;
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
        if !self.lenses.options.supported || !self.lens_enabled(&uri) {
            return self.respond(Response::new_ok(req.id, Vec::<CodeLens>::new()));
        }
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
            return self.respond(Response::new_ok(req.id, ready.clone().unwrap_or_default()));
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
            std::thread::spawn(move || {
                let lenses = oxabl_common::catch_panic(|| {
                    oxabl_daemon::db::compute_effect_lenses(&snapshot, buffer, schema).map(
                        |lenses| {
                            lenses
                                .into_iter()
                                .map(|(at, title)| {
                                    let position = byte_to_position(&rope, at as usize, &encoding);
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
            });
        }
        self.respond(Response::new_ok(req.id, Vec::<CodeLens>::new()))
    }
}
