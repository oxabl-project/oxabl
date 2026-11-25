//! Reference-count code lenses: "N references" above each procedure, function,
//! method, class, interface and property, and "used by N files" above a
//! program. `textDocument/codeLens` gives only the ranges; `codeLens/resolve`
//! runs the search that find references runs (`docs/design/references.md`) and
//! counts its locations. The effect lenses (`code_lens.rs`) are a different
//! kind with their own configuration.

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use lsp_server::{ErrorCode, Message, Request, Response};
use lsp_types::{CodeLens, Command, Location, Position, PositionEncodingKind, Range, Uri};
use oxabl_daemon::canonical_path;
use oxabl_daemon::references::{
    ReferenceAnswer, ReferenceError, ReferenceQuery, answer_references,
};
use oxabl_pipeline::{OutlineKind, OutlineSymbol};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::code_lens::lens_file;
use crate::position::byte_to_position;
use crate::references::{locations, with_dropped};
use crate::{Server, uri_to_path};

/// The client command a resolved lens runs. Its arguments are the `uri` and
/// `position` that anchor the list, and the `locations` the count counts, so
/// the list is the count and costs no second search.
pub(crate) const COMMAND: &str = "oxabl.showReferences";

/// The longest gap text a title shows, in characters.
const REASON_LIMIT: usize = 60;

/// The most titles kept; the cache is emptied when it grows past this.
const CACHE_LIMIT: usize = 4096;

/// The generations of the session a title was made at: buffers, configuration
/// and schema.
type Generations = (u64, u64, u32);

/// What tells two resolves of one lens apart: the file, the unit that asked,
/// the kind, the name's byte and the version of the text.
#[derive(Hash, PartialEq, Eq)]
struct CacheKey {
    uri: String,
    root: PathBuf,
    kind: Kind,
    offset: u32,
    version: i32,
}

/// The titles of resolved lenses, shared with the resolve workers. A title is
/// used again only at the session generations it was made at, so an edit of
/// any open file, a change of the configuration or schema, or a change of a
/// file on disk (which empties the cache) makes a new search.
#[derive(Clone, Default)]
pub(crate) struct ResolveCache {
    titles: Arc<Mutex<HashMap<CacheKey, (Generations, Title)>>>,
}

impl ResolveCache {
    fn lock(&self) -> std::sync::MutexGuard<'_, HashMap<CacheKey, (Generations, Title)>> {
        self.titles
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn get(&self, key: &CacheKey, now: Generations) -> Option<Title> {
        self.lock()
            .get(key)
            .filter(|(made, _)| *made == now)
            .map(|(_, title)| title.clone())
    }

    fn put(&self, key: CacheKey, made: Generations, title: Title) {
        let mut titles = self.lock();
        if titles.len() >= CACHE_LIMIT {
            titles.clear();
        }
        titles.insert(key, (made, title));
    }

    pub(crate) fn clear(&self) {
        self.lock().clear();
    }
}

/// The `enable` flag of a `referenceLenses` settings value: a boolean, or an
/// object with a boolean `enable`.
pub(crate) fn enabled_in(value: &Value) -> Option<bool> {
    value
        .as_bool()
        .or_else(|| value.get("enable").and_then(Value::as_bool))
}

/// What a lens carries from `textDocument/codeLens` to `codeLens/resolve`.
#[derive(Serialize, Deserialize)]
struct LensData {
    /// A marker, so data of another feature is never read as this one's.
    oxabl: String,
    kind: Kind,
    uri: String,
    /// The version of the text the offsets are measured on.
    version: i32,
    /// The byte of the name, or `0` for a program.
    offset: u32,
    position: Position,
}

const DATA_TAG: &str = "references";

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Hash)]
#[serde(rename_all = "lowercase")]
enum Kind {
    Program,
    Symbol,
}

impl Server<'_> {
    pub(crate) fn reference_lenses_on(&self) -> bool {
        self.lenses.supported() && self.lenses.reference_lenses()
    }

    /// Whether reference lenses are on and `uri` is a file that can have them.
    pub(crate) fn reference_lenses_apply(&self, uri: &Uri) -> bool {
        self.reference_lenses_on() && uri_to_path(uri).is_some_and(|path| lens_file(&path))
    }

    /// Apply the `referenceLenses` setting of `workspace/didChangeConfiguration`
    /// and ask the client for the lenses again when it changed.
    pub(crate) fn handle_reference_lens_settings(&mut self, params: &Value) {
        let settings = &params["settings"];
        let Some(enabled) = settings
            .get("oxabl")
            .and_then(|s| s.get("referenceLenses"))
            .or_else(|| settings.get("referenceLenses"))
            .and_then(enabled_in)
        else {
            return;
        };
        if self.lenses.set_reference_lenses(enabled) == enabled {
            return;
        }
        self.update_lens_registration();
        if self.lenses.supported() && self.lenses.refresh_supported() {
            let _ = self.connection.sender.send(Message::Request(Request::new(
                self.lenses.request_id(),
                "workspace/codeLens/refresh".into(),
                Value::Null,
            )));
        }
    }

    /// The lenses of `uri`, without a count: the outline of the open text.
    pub(crate) fn reference_lens_ranges(&mut self, uri: &Uri) -> Vec<CodeLens> {
        if !self.reference_lenses_apply(uri) {
            return Vec::new();
        }
        let Some(path) = uri_to_path(uri) else {
            return Vec::new();
        };
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };
        let outline = self
            .outlines
            .get_or_compute(uri, doc.version, || oxabl_pipeline::outline(&doc.text()));
        // An outline that failed is unknown, not empty: no lens.
        let Ok(symbols) = outline else {
            return Vec::new();
        };
        let mut lenses = Vec::new();
        let at = |offset: u32| byte_to_position(&doc.rope, offset as usize, &self.encoding);
        let lens = |kind, start: u32, name: u32| {
            let position = at(start);
            let named = at(name);
            CodeLens {
                range: Range::new(position, position),
                command: None,
                data: serde_json::to_value(LensData {
                    oxabl: DATA_TAG.to_string(),
                    kind,
                    uri: uri.as_str().to_string(),
                    version: doc.version,
                    offset: name,
                    position: named,
                })
                .ok(),
            }
        };
        if is_program(&path) {
            lenses.push(lens(Kind::Program, 0, 0));
        }
        let mut stack: Vec<&OutlineSymbol> = symbols.iter().rev().collect();
        while let Some(symbol) = stack.pop() {
            if lensed(symbol.kind) {
                lenses.push(lens(
                    Kind::Symbol,
                    symbol.span.start,
                    symbol.selection_span.start,
                ));
            }
            stack.extend(symbol.children.iter().rev());
        }
        lenses
    }

    /// `codeLens/resolve`: count the references of a reference lens on a
    /// worker thread. A lens of another kind comes back as it is.
    pub(crate) fn handle_code_lens_resolve(&mut self, req: Request) -> anyhow::Result<()> {
        let lens: CodeLens = match serde_json::from_value(req.params) {
            Ok(lens) => lens,
            Err(error) => {
                return self.respond(Response::new_err(
                    req.id,
                    ErrorCode::InvalidParams as i32,
                    format!("oxabl-lsp: invalid codeLens: {error}"),
                ));
            }
        };
        let data = lens
            .data
            .clone()
            .and_then(|data| serde_json::from_value::<LensData>(data).ok())
            .filter(|data| data.oxabl == DATA_TAG);
        let Some(data) = data else {
            return self.respond(Response::new_ok(req.id, lens));
        };
        let Ok(uri) = data.uri.parse::<Uri>() else {
            return self.respond(Response::new_err(
                req.id,
                ErrorCode::InvalidParams as i32,
                "oxabl-lsp: the lens names no file".to_string(),
            ));
        };
        let Some(path) = uri_to_path(&uri).map(|path| canonical_path(&path)) else {
            return self.respond(Response::new_err(
                req.id,
                ErrorCode::InvalidParams as i32,
                format!("oxabl-lsp: {} is not a file", data.uri),
            ));
        };
        // Offsets of other text mean another place: the client asks again.
        if self
            .documents
            .get(&uri)
            .is_none_or(|doc| doc.version != data.version)
        {
            return self.respond(Response::new_err(
                req.id,
                ErrorCode::ContentModified as i32,
                "oxabl-lsp: the text changed since the lens was made".to_string(),
            ));
        }
        let (root, query) = match data.kind {
            Kind::Program => (path.clone(), ReferenceQuery::Program),
            Kind::Symbol => (
                self.contexts
                    .selected(&uri)
                    .cloned()
                    .unwrap_or_else(|| path.clone()),
                ReferenceQuery::References {
                    file: path.clone(),
                    offset: data.offset,
                    include_declaration: false,
                },
            ),
        };
        let generations: Option<Generations> = self.host.with(|sessions| {
            sessions.get(&self.session_root).map(|session| {
                (
                    session.buffer_generation(),
                    session.config_generation(),
                    session.schema_revision(),
                )
            })
        });
        let key = CacheKey {
            uri: data.uri.clone(),
            root: root.clone(),
            kind: data.kind,
            offset: data.offset,
            version: data.version,
        };
        if let Some(title) = generations.and_then(|now| self.reference_cache.get(&key, now)) {
            return self.respond(Response::new_ok(req.id, resolved(lens, &data, title)));
        }
        // The text, to hold the search in a test that asks for it.
        let hold_text = oxabl_common::INJECTION_ENABLED
            .then(|| self.documents.get(&uri).map(|doc| doc.text()))
            .flatten();
        let cache = self.reference_cache.clone();
        let in_flight = self.references.clone();
        let cancel = in_flight.start(&req.id);
        let host = self.host.clone();
        let session_root = self.session_root.clone();
        let encoding = self.encoding.clone();
        let sender = self.connection.sender.clone();
        let id = req.id.clone();
        let spawned = oxabl_common::stack::builder()
            .name("oxabl-reference-lens".to_string())
            .spawn({
                let sender = sender.clone();
                let in_flight = in_flight.clone();
                let id = id.clone();
                move || {
                    if let Some(text) = &hold_text {
                        oxabl_common::hold_if_injected(
                            oxabl_common::panic_sites::LSP_REFERENCE_LENS,
                            text,
                        );
                    }
                    let answered = oxabl_common::catch_panic(|| {
                        answer_references(&host, &session_root, &root, &query, &cancel)
                    });
                    in_flight.finish(&id);
                    let response = match answered {
                        Ok(Ok(answer)) => {
                            crate::references::log_standing(&sender, &in_flight, "references and calls", &answer.gaps);
                            let volatile = answer.volatile;
                            let title = title(answer, data.kind, &encoding);
                            if !volatile
                                && title.opens
                                && let Some(made) = generations
                            {
                                cache.put(key, made, title.clone());
                            }
                            Response::new_ok(id, resolved(lens, &data, title))
                        }
                        Ok(Err(ReferenceError::Cancelled)) => Response::new_err(
                            id,
                            ErrorCode::RequestCanceled as i32,
                            "oxabl-lsp: the request was cancelled".to_string(),
                        ),
                        Ok(Err(ReferenceError::Modified)) => Response::new_err(
                            id,
                            ErrorCode::ContentModified as i32,
                            "oxabl-lsp: an edit replaced this analysis".to_string(),
                        ),
                        Ok(Err(ReferenceError::Failed(reason))) => Response::new_ok(
                            id,
                            resolved(
                                lens,
                                &data,
                                unknown(&format!("the file gives no answer: {reason}")),
                            ),
                        ),
                        Err(panic) => Response::new_ok(
                            id,
                            resolved(
                                lens,
                                &data,
                                unknown(&format!("the analysis failed: {panic}")),
                            ),
                        ),
                    };
                    let _ = sender.send(Message::Response(response));
                }
            });
        if let Err(error) = spawned {
            self.references.finish(&id);
            return self.respond(Response::new_err(
                id,
                ErrorCode::InternalError as i32,
                format!("oxabl-lsp: cannot start the analysis: {error}"),
            ));
        }
        Ok(())
    }
}

/// A title, whether it opens the references, and the places it counts.
#[derive(Clone)]
struct Title {
    text: String,
    opens: bool,
    locations: Vec<Location>,
}

fn resolved(mut lens: CodeLens, data: &LensData, title: Title) -> CodeLens {
    lens.command = Some(Command {
        title: title.text,
        command: if title.opens {
            COMMAND.to_string()
        } else {
            String::new()
        },
        arguments: title.opens.then(|| {
            vec![
                Value::String(data.uri.clone()),
                serde_json::json!(data.position),
                serde_json::json!(title.locations),
            ]
        }),
    });
    lens
}

/// A count that is not known, so there is none to show.
fn unknown(reason: &str) -> Title {
    Title {
        text: format!("references not known: {}", shorten(reason)),
        opens: false,
        locations: Vec::new(),
    }
}

/// The title for an answer: an exact count, or a count that says it can be
/// short. A gap of the referent marks a count, and so does a standing gap that
/// is a property of the referent's kind (a call through a handle). Another
/// standing gap holds for every request of the workspace and is only logged.
fn title(answer: ReferenceAnswer, kind: Kind, encoding: &PositionEncodingKind) -> Title {
    if !answer.resolved {
        return unknown("oxabl does not resolve this name");
    }
    let gaps = answer.gaps.clone();
    let wanted = answer.locations.len();
    let files: HashSet<PathBuf> = answer
        .locations
        .iter()
        .map(|target| canonical_path(&target.path))
        .collect();
    let places = locations(answer, encoding);
    let shown = places.len();
    let reasons: Vec<String> = with_dropped(&gaps, wanted - shown)
        .iter()
        .filter_map(|gap| gap.count_reason().map(str::to_string))
        .collect();
    let incomplete = !reasons.is_empty();
    let plus = if incomplete { "+" } else { "" };
    let mut text = format!(
        "{shown}{plus} reference{}",
        if shown == 1 && !incomplete { "" } else { "s" }
    );
    if kind == Kind::Program {
        text += &format!(
            " · used by {}{plus} file{}",
            files.len(),
            if files.len() == 1 && !incomplete {
                ""
            } else {
                "s"
            }
        );
    }
    if incomplete {
        text += &format!(" · incomplete: {}", shorten(&reasons[0]));
        if reasons.len() > 1 {
            text += &format!(" (+{} more)", reasons.len() - 1);
        }
    }
    Title {
        text,
        opens: true,
        locations: places,
    }
}

fn shorten(reason: &str) -> String {
    let reason = reason.lines().next().unwrap_or_default().trim();
    if reason.chars().count() <= REASON_LIMIT {
        return reason.to_string();
    }
    let cut: String = reason.chars().take(REASON_LIMIT).collect();
    format!("{}…", cut.trim_end())
}

fn lensed(kind: OutlineKind) -> bool {
    matches!(
        kind,
        OutlineKind::Class
            | OutlineKind::Interface
            | OutlineKind::Procedure
            | OutlineKind::Function
            | OutlineKind::Method
            | OutlineKind::Property
    )
}

/// A root file that is a program: not a class, not an include.
fn is_program(path: &std::path::Path) -> bool {
    oxabl_workspace::is_root_file(path)
        && !oxabl_workspace::is_include_fragment(path)
        && !path
            .extension()
            .is_some_and(|ext| ext.eq_ignore_ascii_case("cls"))
}
