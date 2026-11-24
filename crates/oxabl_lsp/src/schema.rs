//! `oxabl/schema` and `oxabl/schemaTable`: the configured `.df` files as a
//! tree of databases, tables, fields, indexes and sequences, for a schema
//! browser, and one table of one database, for a detail view.
//!
//! Both read the `oxabl.toml` and the `.df` files at each request, so the
//! answer follows the disk; a client asks again when a `.df` or `oxabl.toml`
//! changes. `oxabl/schemaTable` reads only the file it is asked about (and the
//! earlier files when that file adds to a table it does not declare). Each node has the range of
//! its name in its `.df`, in the negotiated position encoding.

use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::Ordering;

use lsp_server::{ErrorCode, Message, Request, RequestId, Response};
use lsp_types::PositionEncodingKind;
use oxabl_daemon::canonical_path;
use oxabl_pipeline::{
    SchemaDatabase, SchemaTable, SchemaView, TextPosition, TextRange, schema_view_table,
    schema_view_until,
};
use ropey::Rope;
use serde::Deserialize;
use serde_json::{Value, json};

use crate::position::byte_to_position;
use crate::{Server, path_to_uri, uri_to_path};

pub(crate) const METHOD: &str = "oxabl/schema";
pub(crate) const TABLE_METHOD: &str = "oxabl/schemaTable";

#[derive(Deserialize)]
struct TableParams {
    /// The `uri` of a database in the `oxabl/schema` answer.
    database: String,
    table: String,
}

/// What a request asks for, decided on the main loop.
enum Query {
    Tree,
    Table { database: PathBuf, table: String },
}

impl Server<'_> {
    pub(crate) fn handle_schema(&mut self, req: Request) -> anyhow::Result<()> {
        self.spawn_schema(req.id, Query::Tree)
    }

    pub(crate) fn handle_schema_table(&mut self, req: Request) -> anyhow::Result<()> {
        let params: TableParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let database = match params.database.parse().ok().as_ref().and_then(uri_to_path) {
            Some(path) => path,
            None => {
                return self.respond(invalid(
                    req.id,
                    &format!("{} is not a file URI", params.database),
                ));
            }
        };
        self.spawn_schema(
            req.id,
            Query::Table {
                database,
                table: params.table,
            },
        )
    }

    /// Answer on a worker thread: the request reads files. A tree request
    /// that a newer one replaced stops between two files and answers
    /// `ContentModified`.
    fn spawn_schema(&mut self, id: RequestId, query: Query) -> anyhow::Result<()> {
        let anchor = self.workspace_anchor.clone();
        let encoding = self.encoding.clone();
        let sender = self.connection.sender.clone();
        let latest = Arc::clone(&self.schema_requests);
        let generation = match query {
            Query::Tree => latest.fetch_add(1, Ordering::SeqCst) + 1,
            Query::Table { .. } => 0,
        };
        let spawned = oxabl_common::stack::builder()
            .name("oxabl-schema".to_string())
            .spawn({
                let sender = sender.clone();
                let id = id.clone();
                move || {
                    let stop = || generation != 0 && latest.load(Ordering::SeqCst) != generation;
                    let answered = oxabl_common::catch_panic(|| {
                        answer(anchor.as_deref(), &query, &encoding, &stop)
                    });
                    let response = match answered {
                        Ok(Ok(Some(value))) => Response::new_ok(id, value),
                        Ok(Ok(None)) => Response::new_err(
                            id,
                            ErrorCode::ContentModified as i32,
                            format!("oxabl-lsp: a newer {METHOD} request replaced this one"),
                        ),
                        Ok(Err(failure)) => {
                            Response::new_err(id, failure.code as i32, failure.message)
                        }
                        Err(panic) => Response::new_err(
                            id,
                            ErrorCode::InternalError as i32,
                            format!("oxabl-lsp: the schema could not be read: {panic}"),
                        ),
                    };
                    let _ = sender.send(Message::Response(response));
                }
            });
        if let Err(error) = spawned {
            return self.respond(Response::new_err(
                id,
                ErrorCode::InternalError as i32,
                format!("oxabl-lsp: cannot start the schema read: {error}"),
            ));
        }
        Ok(())
    }
}

fn invalid(id: RequestId, message: &str) -> Response {
    Response::new_err(
        id,
        ErrorCode::InvalidParams as i32,
        format!("oxabl-lsp: invalid {TABLE_METHOD} params: {message}"),
    )
}

/// Why a request has no answer.
struct Failure {
    code: ErrorCode,
    message: String,
}

impl Failure {
    fn render(error: serde_json::Error) -> Self {
        Failure {
            code: ErrorCode::InternalError,
            message: format!("oxabl-lsp: cannot render the schema: {error}"),
        }
    }
}

/// The answer; `None` when `stop` said so.
fn answer(
    anchor: Option<&Path>,
    query: &Query,
    encoding: &PositionEncodingKind,
    stop: &dyn Fn() -> bool,
) -> Result<Option<Value>, Failure> {
    let Some(anchor) = anchor else {
        let reason = "the language server has no workspace yet: open a folder or an ABL file.";
        return match query {
            Query::Tree => Ok(Some(json!({
                "config": null, "reason": reason, "warnings": [], "databases": [], "directories": [],
            }))),
            Query::Table { .. } => Err(Failure {
                code: ErrorCode::InvalidParams,
                message: format!("oxabl-lsp: {reason}"),
            }),
        };
    };
    match query {
        // Checked again before the answer is built: rendering a large schema
        // costs more than reading it.
        Query::Tree => match schema_view_until(anchor, stop) {
            Some(view) if !stop() => tree(view, encoding).map(Some),
            _ => Ok(None),
        },
        Query::Table { database, table } => match schema_view_table(anchor, database, table) {
            Ok(Some((database, table))) => one_table(&database, table, encoding).map(Some),
            Ok(None) => Ok(Some(Value::Null)),
            Err(message) => Err(Failure {
                code: ErrorCode::InvalidParams,
                message: format!("oxabl-lsp: {message}"),
            }),
        },
    }
}

fn tree(mut view: SchemaView, encoding: &PositionEncodingKind) -> Result<Value, Failure> {
    let mut databases = Vec::with_capacity(view.databases.len());
    for database in &mut view.databases {
        let rope = Rope::from_str(&database.text);
        database.place(|span| range(&rope, span.start, span.end, encoding));
        databases.push(database_value(database)?);
    }
    view.databases.clear();
    let mut value = serde_json::to_value(&view).map_err(Failure::render)?;
    value["databases"] = Value::Array(databases);
    Ok(value)
}

/// The database with its `uri`, which a client hands back to
/// `oxabl/schemaTable`. `null` for a path a URI cannot name.
fn database_value(database: &SchemaDatabase) -> Result<Value, Failure> {
    let mut value = serde_json::to_value(database).map_err(Failure::render)?;
    value["uri"] = json!(uri_of(&database.path));
    Ok(value)
}

fn uri_of(path: &Path) -> Option<String> {
    path_to_uri(&canonical_path(path)).map(|uri| uri.as_str().to_string())
}

fn one_table(
    database: &SchemaDatabase,
    mut table: SchemaTable,
    encoding: &PositionEncodingKind,
) -> Result<Value, Failure> {
    let rope = Rope::from_str(&database.text);
    table.place(&mut |span| range(&rope, span.start, span.end, encoding));
    Ok(json!({
        "database": {
            "name": database.name,
            "path": database.path.to_string_lossy(),
            "uri": uri_of(&database.path),
            "complete": database.complete,
        },
        "table": serde_json::to_value(&table).map_err(Failure::render)?,
    }))
}

fn range(rope: &Rope, start: u32, end: u32, encoding: &PositionEncodingKind) -> TextRange {
    let position = |offset: u32| {
        let at = byte_to_position(rope, offset as usize, encoding);
        TextPosition {
            line: at.line,
            character: at.character,
        }
    };
    TextRange {
        start: position(start),
        end: position(end),
    }
}
