//! `workspace/symbol`: the daemon's workspace symbol search rendered as LSP
//! symbol information.
//!
//! The search runs on a worker thread, since it waits for a workspace pass. A
//! failed or panicked search answers with an error, never with an empty list
//! that reads as "no such symbol".

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use lsp_server::{ErrorCode, Message, Request, RequestId, Response};
use lsp_types::{
    Location, PositionEncodingKind, Range, SymbolInformation, SymbolKind, WorkspaceSymbolParams,
};
use oxabl_daemon::SessionHost;
use oxabl_daemon::methods::search_symbols_located;
use oxabl_daemon_protocol::{SymbolKind as RowKind, SymbolRow};
use ropey::Rope;

use crate::position::byte_to_position;

/// The most symbols one answer carries. The client filters further as the user
/// types, so the first rows by rank are the useful ones.
pub(crate) const RESULT_LIMIT: usize = 256;

fn symbol_kind(kind: RowKind) -> SymbolKind {
    match kind {
        RowKind::File => SymbolKind::FILE,
        RowKind::Class => SymbolKind::CLASS,
        RowKind::Interface => SymbolKind::INTERFACE,
        RowKind::Procedure | RowKind::Function => SymbolKind::FUNCTION,
        RowKind::Table => SymbolKind::STRUCT,
        RowKind::SharedVariable => SymbolKind::VARIABLE,
    }
}

/// Answer a `workspace/symbol` request on a worker thread. The response goes
/// straight to the client.
pub(crate) fn spawn(
    req: Request,
    host: SessionHost,
    root: PathBuf,
    encoding: PositionEncodingKind,
    sender: crossbeam_channel::Sender<Message>,
) {
    let id = req.id.clone();
    let params = match serde_json::from_value::<WorkspaceSymbolParams>(req.params) {
        Ok(params) => params,
        Err(error) => {
            let _ = sender.send(Message::Response(Response::new_err(
                id,
                ErrorCode::InvalidParams as i32,
                format!("oxabl-lsp: invalid workspace/symbol params: {error}"),
            )));
            return;
        }
    };
    let spawned = oxabl_common::stack::builder()
        .name("oxabl-workspace-symbol".to_string())
        .spawn({
            let sender = sender.clone();
            let id = id.clone();
            move || {
                let response = answer(id, &host, &root, &params.query, &encoding);
                let _ = sender.send(Message::Response(response));
            }
        });
    if let Err(error) = spawned {
        let _ = sender.send(Message::Response(Response::new_err(
            id,
            ErrorCode::InternalError as i32,
            format!("oxabl-lsp: cannot start the symbol search: {error}"),
        )));
    }
}

fn answer(
    id: RequestId,
    host: &SessionHost,
    root: &Path,
    query: &str,
    encoding: &PositionEncodingKind,
) -> Response {
    let searched = oxabl_common::catch_panic(|| {
        search_symbols_located(host, root, query, RESULT_LIMIT)
            .map(|found| to_response(found.symbols, encoding))
    });
    match searched {
        Ok(Ok(symbols)) => Response::new_ok(id, symbols),
        Ok(Err(error)) => Response::new_err(
            id,
            ErrorCode::RequestFailed as i32,
            format!("oxabl-lsp: the workspace symbol search failed: {error}"),
        ),
        Err(panic) => Response::new_err(
            id,
            ErrorCode::InternalError as i32,
            format!("oxabl-lsp: the workspace symbol search failed: {panic}"),
        ),
    }
}

/// Render the rows that have a declaring file. A row without one (a schema
/// table) has no location to give, so it is left out.
pub(crate) fn to_response(
    rows: Vec<(SymbolRow, Option<Arc<str>>)>,
    encoding: &PositionEncodingKind,
) -> Vec<SymbolInformation> {
    let mut ropes: HashMap<String, Rope> = HashMap::new();
    rows.into_iter()
        .filter_map(|(row, text)| {
            let file = row.file.as_ref()?;
            let uri = crate::path_to_uri(Path::new(file))?;
            let range = match (row.span, text) {
                (Some(span), Some(text)) => {
                    let rope = ropes
                        .entry(file.clone())
                        .or_insert_with(|| Rope::from_str(&text));
                    Range::new(
                        byte_to_position(rope, span.start as usize, encoding),
                        byte_to_position(rope, span.end as usize, encoding),
                    )
                }
                // A span the file can no longer be read for has no honest place.
                (Some(_), None) => return None,
                // A file row names the whole file.
                (None, _) => Range::default(),
            };
            #[allow(deprecated)] // lsp-types makes the field mandatory
            let information = SymbolInformation {
                name: row.name.clone(),
                kind: symbol_kind(row.kind),
                tags: None,
                deprecated: None,
                location: Location { uri, range },
                container_name: container_name(file),
            };
            Some(information)
        })
        .collect()
}

/// The declaring file's name, so two symbols with one name in different files
/// can be told apart in the picker.
fn container_name(file: &str) -> Option<String> {
    Path::new(file)
        .file_name()
        .map(|name| name.to_string_lossy().into_owned())
}
