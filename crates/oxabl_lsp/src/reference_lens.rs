//! Reference-count code lenses: "N references" above each procedure, function,
//! method, class, interface and property, and "used by N files" above a
//! program. `textDocument/codeLens` gives only the ranges; `codeLens/resolve`
//! runs the search that find references runs (`docs/design/references.md`) and
//! counts its locations. The effect lenses (`code_lens.rs`) are a different
//! kind with their own configuration.

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use lsp_server::{ErrorCode, Message, Request, Response};
use lsp_types::{CodeLens, Command, Location, Position, PositionEncodingKind, Range, Uri};
use oxabl_daemon::canonical_path;
use oxabl_daemon::hierarchy::may_override;
use oxabl_daemon::references::{
    Pass, ReferenceAnswer, ReferenceError, ReferenceQuery, answer_references,
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

/// What a count shows for a member that overrides or implements another.
const SUPERTYPE_SHORT: &str = "calls through a supertype are not counted";

/// The longest gap text a title shows, in characters.
const REASON_LIMIT: usize = 60;

/// The most titles kept.
const CACHE_LIMIT: usize = 4096;

/// The most places the kept titles hold together: each title holds every
/// place of its answer.
const LOCATION_LIMIT: usize = 100_000;

/// The generations of the session a title was made at: buffers, configuration
/// and schema.
type Generations = (u64, u64, u32);

/// What tells two resolves of one lens apart: the file, the unit that asked,
/// the kind, the name's byte and the version of the text.
#[derive(Clone, Hash, PartialEq, Eq)]
struct CacheKey {
    uri: String,
    root: PathBuf,
    kind: Kind,
    offset: u32,
    version: i32,
}

/// A kept title, and the workspace pass its search read.
struct Entry {
    title: Title,
    pass: Option<Pass>,
}

#[derive(Default)]
struct Titles {
    /// The generations every entry was made at. An entry of other generations
    /// is dead, so there are none.
    generations: Option<Generations>,
    entries: HashMap<CacheKey, Entry>,
    /// The places the entries hold together.
    locations: usize,
}

impl Titles {
    fn clear(&mut self) {
        self.entries.clear();
        self.locations = 0;
    }
}

/// The titles of resolved lenses, shared with the resolve workers. A title is
/// used again only at the session generations it was made at, so an edit of
/// any open file, a change of the configuration or schema, or a change of a
/// file on disk (which empties the cache) makes a new search. A title that
/// read a workspace pass is used only while that pass is the current one.
#[derive(Clone, Default)]
pub(crate) struct ResolveCache {
    titles: Arc<Mutex<Titles>>,
    /// Moved by every [`clear`](Self::clear), so an answer that a clear
    /// overtook is not kept.
    epoch: Arc<AtomicU64>,
}

impl ResolveCache {
    fn lock(&self) -> std::sync::MutexGuard<'_, Titles> {
        self.titles
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    /// The epoch to give [`put`](Self::put) for an answer that starts now.
    fn epoch(&self) -> u64 {
        self.epoch.load(Ordering::SeqCst)
    }

    /// The kept title of `key` at the generations `now`, and the pass it read.
    /// The entries of older generations are dead and go.
    fn get(&self, key: &CacheKey, now: Generations) -> Option<(Title, Option<Pass>)> {
        let mut titles = self.lock();
        if titles.generations != Some(now) {
            titles.clear();
            titles.generations = Some(now);
        }
        titles
            .entries
            .get(key)
            .map(|entry| (entry.title.clone(), entry.pass.clone()))
    }

    /// Keep a title that an answer made at `made`, unless a clear came after
    /// `epoch`, the session has moved on, or the title is too big. The other
    /// versions of the same file are dead and go.
    fn put(&self, key: CacheKey, made: Generations, epoch: u64, title: Title, pass: Option<Pass>) {
        let mut titles = self.lock();
        if epoch != self.epoch() || titles.generations != Some(made) {
            return;
        }
        let places = title.locations.len();
        if places > LOCATION_LIMIT {
            return;
        }
        let dead: Vec<CacheKey> = titles
            .entries
            .keys()
            .filter(|held| held.uri == key.uri && held.version != key.version)
            .cloned()
            .collect();
        for held in dead {
            if let Some(entry) = titles.entries.remove(&held) {
                titles.locations -= entry.title.locations.len();
            }
        }
        if let Some(entry) = titles.entries.remove(&key) {
            titles.locations -= entry.title.locations.len();
        }
        if titles.entries.len() >= CACHE_LIMIT || titles.locations + places > LOCATION_LIMIT {
            titles.clear();
        }
        titles.locations += places;
        titles.entries.insert(key, Entry { title, pass });
    }

    pub(crate) fn clear(&self) {
        let mut titles = self.lock();
        self.epoch.fetch_add(1, Ordering::SeqCst);
        titles.clear();
    }
}

/// How many debounce windows the lenses wait after a change before the client
/// is asked for them again: a burst of saves or edits asks once.
const REFRESH_WINDOWS: u32 = 5;

/// The debounced `workspace/codeLens/refresh` after a change that can make a
/// count old.
pub(crate) struct Refresh {
    window: std::time::Duration,
    /// The files whose lenses the client asked for.
    files: HashSet<Uri>,
    due: Option<std::time::Instant>,
}

impl Refresh {
    pub(crate) fn new(debounce_window: std::time::Duration) -> Self {
        Refresh {
            window: debounce_window * REFRESH_WINDOWS,
            files: HashSet::new(),
            due: None,
        }
    }

    pub(crate) fn due(&self) -> Option<std::time::Instant> {
        self.due
    }

    pub(crate) fn forget(&mut self, uri: &Uri) {
        self.files.remove(uri);
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

    /// Ask the client for the lenses again once changes stop. A count and its
    /// list of places come from the other files too, and the client asks again
    /// for a file only when that file changes.
    pub(crate) fn schedule_lens_refresh(&mut self) {
        if self.reference_lenses_on() && self.lenses.refresh_supported() {
            self.lens_refresh.due = Some(std::time::Instant::now() + self.lens_refresh.window);
        }
    }

    /// [`schedule_lens_refresh`](Self::schedule_lens_refresh) for an edit of
    /// `uri`, when the lenses of another file were asked for: the client asks
    /// again for the file that changed by itself.
    pub(crate) fn schedule_lens_refresh_for_edit(&mut self, uri: &Uri) {
        if self.lens_refresh.files.iter().any(|other| other != uri) {
            self.schedule_lens_refresh();
        }
    }

    /// Send the refresh when it is due.
    pub(crate) fn fire_lens_refresh(&mut self) {
        let Some(due) = self.lens_refresh.due else {
            return;
        };
        if due > std::time::Instant::now() {
            return;
        }
        self.lens_refresh.due = None;
        if self.reference_lenses_on() && self.lenses.refresh_supported() {
            let _ = self.connection.sender.send(Message::Request(Request::new(
                self.lenses.request_id(),
                "workspace/codeLens/refresh".into(),
                Value::Null,
            )));
        }
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
        self.lens_refresh.files.insert(uri.clone());
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
        let epoch = self.reference_cache.epoch();
        let kept = generations.and_then(|now| self.reference_cache.get(&key, now));
        let kept = match kept {
            // No other file is read: nothing can have made it old.
            Some((title, None)) => {
                return self.respond(Response::new_ok(req.id, resolved(lens, &data, title)));
            }
            kept => kept,
        };
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
                    // A kept title that read a workspace pass holds while the
                    // pass is the one that find references reads now.
                    if let Some((title, Some(pass))) = &kept
                        && pass.is_current(&host, &session_root, &cancel)
                    {
                        in_flight.finish(&id);
                        let response = Response::new_ok(id, resolved(lens, &data, title.clone()));
                        let _ = sender.send(Message::Response(response));
                        return;
                    }
                    if let Some(text) = &hold_text {
                        oxabl_common::hold_if_injected(
                            oxabl_common::panic_sites::LSP_REFERENCE_LENS,
                            text,
                        );
                    }
                    let answered = oxabl_common::catch_panic(|| {
                        answer_references(&host, &session_root, &root, &query, &cancel)
                    });
                    if let (Some(text), Ok(Ok(_))) = (&hold_text, &answered) {
                        oxabl_common::hold_if_injected(
                            oxabl_common::panic_sites::LSP_REFERENCE_LENS_ANSWER,
                            text,
                        );
                    }
                    // A member that overrides or implements another is also
                    // called through its supertype, which the search does not
                    // read. Any failure to tell counts as an override.
                    let overrides = match &answered {
                        Ok(Ok(answer)) if answer.member && data.kind == Kind::Symbol => {
                            match oxabl_common::catch_panic(|| {
                                may_override(&host, &session_root, &root, &path, data.offset)
                            }) {
                                Ok(Ok(overrides)) => Ok(overrides),
                                Ok(Err(
                                    error @ (ReferenceError::Cancelled | ReferenceError::Modified),
                                )) => Err(error),
                                _ => Ok(true),
                            }
                        }
                        _ => Ok(false),
                    };
                    in_flight.finish(&id);
                    let answered = match (answered, overrides) {
                        (Ok(Ok(answer)), Ok(overrides)) => Ok(Ok((answer, overrides))),
                        (Ok(Ok(_)), Err(error)) => Ok(Err(error)),
                        (Ok(Err(error)), _) => Ok(Err(error)),
                        (Err(panic), _) => Err(panic),
                    };
                    let response = match answered {
                        Ok(Ok((answer, overrides))) => {
                            crate::references::log_standing(
                                &sender,
                                &in_flight,
                                "references and calls",
                                &answer.gaps,
                            );
                            let volatile = answer.volatile;
                            let pass = answer.pass.clone();
                            let title = title(answer, data.kind, overrides, &encoding);
                            if !volatile
                                && title.opens
                                && let Some(made) = generations
                            {
                                cache.put(key, made, epoch, title.clone(), pass);
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
/// short. A gap of the referent marks a count, and so does a call of the
/// workspace that no search follows and that can reach it
/// ([`ReferenceAnswer::count_reasons`]). A standing gap holds for every request
/// of the workspace and is only logged.
fn title(
    answer: ReferenceAnswer,
    kind: Kind,
    overrides: bool,
    encoding: &PositionEncodingKind,
) -> Title {
    if !answer.resolved {
        return unknown("oxabl does not resolve this name");
    }
    let gaps = answer.gaps.clone();
    let count_reasons = answer.count_reasons.clone();
    let wanted = answer.locations.len();
    let files: HashSet<PathBuf> = answer
        .locations
        .iter()
        .map(|target| canonical_path(&target.path))
        .collect();
    let places = locations(answer, encoding);
    let shown = places.len();
    // The gaps of this answer come first: they can date or cut the search.
    let mut reasons: Vec<String> = with_dropped(&gaps, wanted - shown)
        .iter()
        .filter(|gap| !gap.standing)
        .map(|gap| gap.reason.clone())
        .collect();
    reasons.extend(count_reasons);
    if overrides {
        reasons.push(SUPERTYPE_SHORT.to_string());
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    const NOW: Generations = (1, 1, 1);

    fn key(uri: &str, version: i32, offset: u32) -> CacheKey {
        CacheKey {
            uri: uri.to_string(),
            root: PathBuf::from(uri),
            kind: Kind::Symbol,
            offset,
            version,
        }
    }

    fn title_of(places: usize) -> Title {
        let place = Location::new(
            "file:///a.p".parse().unwrap(),
            Range::new(Position::new(0, 0), Position::new(0, 1)),
        );
        Title {
            text: format!("{places} references"),
            opens: true,
            locations: vec![place; places],
        }
    }

    fn held(cache: &ResolveCache) -> (usize, usize) {
        let titles = cache.lock();
        (titles.entries.len(), titles.locations)
    }

    /// A cache synced to `NOW`, as the first resolve of a session leaves it.
    fn cache() -> ResolveCache {
        let cache = ResolveCache::default();
        cache.get(&key("file:///a.p", 1, 0), NOW);
        cache
    }

    #[test]
    fn a_title_is_kept_for_the_generations_it_was_made_at() {
        let cache = cache();
        let epoch = cache.epoch();
        cache.put(key("file:///a.p", 1, 0), NOW, epoch, title_of(2), None);
        assert!(cache.get(&key("file:///a.p", 1, 0), NOW).is_some());
        assert!(cache.get(&key("file:///a.p", 1, 0), (2, 1, 1)).is_none());
        // The entries of the old generations are gone, not only unused.
        assert_eq!(held(&cache), (0, 0));
    }

    #[test]
    fn a_clear_after_the_search_began_keeps_no_title() {
        let cache = cache();
        let epoch = cache.epoch();
        cache.clear();
        cache.put(key("file:///a.p", 1, 0), NOW, epoch, title_of(1), None);
        assert_eq!(held(&cache), (0, 0));
        // A search that began after the clear is kept.
        cache.put(
            key("file:///a.p", 1, 0),
            NOW,
            cache.epoch(),
            title_of(1),
            None,
        );
        assert_eq!(held(&cache), (1, 1));
    }

    #[test]
    fn a_title_of_older_generations_is_not_kept() {
        let cache = cache();
        cache.put(
            key("file:///a.p", 1, 0),
            (0, 1, 1),
            cache.epoch(),
            title_of(1),
            None,
        );
        assert_eq!(held(&cache), (0, 0));
    }

    #[test]
    fn the_titles_of_other_versions_of_a_file_are_dropped() {
        let cache = cache();
        for version in 1..=3 {
            cache.put(
                key("file:///a.p", version, 0),
                NOW,
                cache.epoch(),
                title_of(4),
                None,
            );
            cache.put(
                key("file:///a.p", version, 9),
                NOW,
                cache.epoch(),
                title_of(4),
                None,
            );
        }
        cache.put(
            key("file:///b.p", 1, 0),
            NOW,
            cache.epoch(),
            title_of(4),
            None,
        );
        // Version 3 of `a.p` (two lenses) and the one of `b.p`.
        assert_eq!(held(&cache), (3, 12));
        assert!(cache.get(&key("file:///a.p", 1, 0), NOW).is_none());
        assert!(cache.get(&key("file:///a.p", 3, 9), NOW).is_some());
    }

    #[test]
    fn the_places_that_the_titles_hold_are_bounded() {
        let cache = cache();
        let big = LOCATION_LIMIT / 2 + 1;
        cache.put(
            key("file:///a.p", 1, 0),
            NOW,
            cache.epoch(),
            title_of(big),
            None,
        );
        cache.put(
            key("file:///b.p", 1, 0),
            NOW,
            cache.epoch(),
            title_of(big),
            None,
        );
        // The second does not fit beside the first: the cache starts again.
        let (entries, places) = held(&cache);
        assert!(places <= LOCATION_LIMIT, "{places} places held");
        assert_eq!(entries, 1);
        // A title of more places than the limit is not kept.
        cache.put(
            key("file:///c.p", 1, 0),
            NOW,
            cache.epoch(),
            title_of(LOCATION_LIMIT + 1),
            None,
        );
        assert!(cache.get(&key("file:///c.p", 1, 0), NOW).is_none());
    }
}
