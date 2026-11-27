//! `oxabl/callArguments`: the call fill of an item whose parameters the
//! completion base cannot give (a program after `RUN`). The client asks after
//! it inserted the name, and inserts the answer as a snippet at the cursor.
//! See `docs/design/call-fill.md`.
//!
//! The setting `completion.callArguments` is read here too, from the
//! initialization options and from `workspace/didChangeConfiguration`.

use lsp_server::{ErrorCode, Request, RequestId, Response};
use lsp_types::{Position, Uri};
use oxabl_daemon::canonical_path;
use oxabl_daemon::db::NavigationQuery;
use oxabl_pipeline::completion::CallArguments;
use serde::Deserialize;
use serde_json::{Value, json};

use crate::position::position_to_byte;
use crate::{Server, uri_to_path};

/// The request a client sends after it inserted the name of a call item
/// whose command is [`COMMAND`].
pub(crate) const METHOD: &str = "oxabl/callArguments";

/// The client's command that asks [`METHOD`]. The server names it on an item
/// only when the client said it has it (`completion.fillCommand`).
pub(crate) const COMMAND: &str = "oxabl.fillCall";

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct Params {
    text_document: Document,
    position: Position,
}

#[derive(Deserialize)]
struct Document {
    uri: Uri,
    version: i32,
}

/// The `completion` object of the initialization options or the settings:
/// the value of `callArguments`, when it is one the server knows.
pub(crate) fn call_arguments_in(root: &Value) -> Option<CallArguments> {
    root.get("completion")?
        .get("callArguments")?
        .as_str()
        .and_then(CallArguments::parse)
}

/// Whether the initialization options say that the client has [`COMMAND`].
pub(crate) fn fill_command_in(root: &Value) -> bool {
    root.get("completion")
        .and_then(|completion| completion.get("fillCommand"))
        .and_then(Value::as_bool)
        .unwrap_or(false)
}

impl Server<'_> {
    pub(crate) fn handle_call_arguments(&mut self, req: Request) -> anyhow::Result<()> {
        let params: Params = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let uri = params.text_document.uri;
        let Some(path) = uri_to_path(&uri) else {
            return self.respond(invalid(req.id, &format!("{} is not a file", uri.as_str())));
        };
        let Some(document) = self.documents.get(&uri) else {
            return self.respond(invalid(req.id, &format!("{} is not open", uri.as_str())));
        };
        // The text changed after the client inserted the name: the fill would
        // be for another text.
        if document.version != params.text_document.version {
            return self.respond(Response::new_ok(req.id, Value::Null));
        }
        let offset = position_to_byte(&document.rope, params.position, &self.encoding) as u32;
        let path = canonical_path(&path);
        let root = self
            .contexts
            .selected(&uri)
            .cloned()
            .unwrap_or_else(|| path.clone());
        let query = NavigationQuery::CallFill {
            file: path,
            offset,
            arguments: self.completion.call_arguments,
        };
        self.spawn_navigation(req.id, root, None, None, query, |answer, _| {
            Ok(match answer.call_fill {
                Some((snippet, hints)) => json!({ "snippet": snippet, "parameterHints": hints }),
                None => Value::Null,
            })
        })
    }

    /// Apply `completion.callArguments` of `workspace/didChangeConfiguration`.
    pub(crate) fn handle_completion_settings(&mut self, params: &Value) {
        let settings = &params["settings"];
        let value = settings
            .get("oxabl")
            .and_then(call_arguments_in)
            .or_else(|| call_arguments_in(settings));
        if let Some(value) = value {
            self.completion.call_arguments = value;
        }
    }
}

fn invalid(id: RequestId, message: &str) -> Response {
    Response::new_err(
        id,
        ErrorCode::InvalidParams as i32,
        format!("oxabl-lsp: {message}"),
    )
}
