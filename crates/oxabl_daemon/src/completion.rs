//! The bases of code completion: for each open document and include context,
//! the newest analysis that completion reads. See `docs/design/completion.md`.
//!
//! A completion request never waits for an analysis when a base exists. It
//! reads the newest base, which can be of an older text (stale), and when it
//! is stale it starts one refresh in the background for the next request. The
//! first request of a document makes its base on the request's thread.
//!
//! No base is made under a database snapshot. Salsa checks for cancellation
//! only at query boundaries, and an analysis has none inside it, so a snapshot
//! held through one makes the next edit of any buffer wait until it ends. A
//! base is made from copied inputs instead (the text, the configuration, the
//! open buffers) with the pipeline's own run index, and a request reads it
//! with no snapshot either.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};
use std::time::{Duration, Instant};

use oxabl_pipeline::completion::{
    CompletionBase, CompletionList, CompletionRequest, DirListing, complete, keyword_documentation,
    path_documentation, sorted_listing, symbol_documentation, table_documentation, wants_base,
};
use oxabl_pipeline::{Expansion, LintPipeline, Navigation, NavigationModel};
use oxabl_workspace::{DirEntry, FileSystem};

use crate::db::{AnalysisConfig, OverlayFs, spelled_as_expanded};
use crate::session::canonical_path;

/// The hold-marker site of a refresh, for the tests that show a request does
/// not wait for it.
pub const REFRESH_SITE: &str = "completion.refresh";

/// Which base: a document, and the root it is analysed in when that is not
/// the document itself.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BaseKey {
    /// The session's key of the document (its URI for an editor).
    pub document: String,
    /// The selected root of an include, `None` for a document analysed as
    /// its own root.
    pub root: Option<PathBuf>,
}

/// One analysis that completion reads.
pub struct Base {
    id: u64,
    /// The text of the document that the analysis read.
    text: Arc<str>,
    /// The root that was analysed, as the expansion names it.
    root: PathBuf,
    /// The text of the root, for a base made in a selected root context.
    root_text: Option<Arc<str>>,
    /// The open buffers the analysis read over the disk (root contexts).
    overlay: HashMap<PathBuf, Arc<str>>,
    model: Arc<NavigationModel>,
    expansion: Arc<Expansion>,
    /// The configuration the analysis ran under.
    config: AnalysisConfig,
    generation: (u64, u32),
}

/// The newest bases of each document, and the refresh that runs for it.
#[derive(Default)]
struct Slot {
    newest: Option<Arc<Base>>,
    /// The base before the newest, which a resolve of an older list reads.
    previous: Option<Arc<Base>>,
    /// The id of the base that the running refresh makes, its claim token.
    refreshing: Option<u64>,
}

/// How long a directory listing of `RUN` and `{` is kept. A request comes at
/// each key, and the same directory is listed for each; a file created or
/// deleted shows in the list after this.
const LISTING_TTL: Duration = Duration::from_secs(3);

/// The most directory listings kept.
const MAX_LISTINGS: usize = 256;

/// The directory listings of the recent requests, each kept for
/// [`LISTING_TTL`].
#[derive(Default)]
struct Listings(Mutex<HashMap<PathBuf, Listed>>);

/// When a directory was listed, and its entries (`None`: it cannot be listed).
type Listed = (Instant, Option<Arc<[DirEntry]>>);

/// A [`DirListing`] that reads [`Listings`] before the file system.
struct CachedListing<'c> {
    fs: &'c dyn FileSystem,
    cache: &'c Listings,
}

impl DirListing for CachedListing<'_> {
    fn list(&self, dir: &Path) -> Option<Arc<[DirEntry]>> {
        let now = Instant::now();
        {
            let cache = self.cache.0.lock().unwrap_or_else(|e| e.into_inner());
            if let Some((at, listing)) = cache.get(dir)
                && now.duration_since(*at) < LISTING_TTL
            {
                return listing.clone();
            }
        }
        // Listed with no lock held: a slow directory does not stop the others.
        let listing = sorted_listing(self.fs, dir);
        let mut cache = self.cache.0.lock().unwrap_or_else(|e| e.into_inner());
        if cache.len() >= MAX_LISTINGS {
            cache.retain(|_, (at, _)| now.duration_since(*at) < LISTING_TTL);
            if cache.len() >= MAX_LISTINGS {
                cache.clear();
            }
        }
        cache.insert(dir.to_path_buf(), (now, listing.clone()));
        listing
    }
}

/// The completion bases of one session.
#[derive(Default)]
pub struct Bases {
    slots: Mutex<HashMap<BaseKey, Slot>>,
    listings: Listings,
    /// Numbers the bases in the order their analysis started.
    sequence: AtomicU64,
}

impl Bases {
    fn slots(&self) -> MutexGuard<'_, HashMap<BaseKey, Slot>> {
        self.slots.lock().unwrap_or_else(|e| e.into_inner())
    }

    fn next_id(&self) -> u64 {
        self.sequence.fetch_add(1, Ordering::SeqCst) + 1
    }

    /// Drop the bases of a closed document. A refresh that ends later keeps
    /// nothing.
    pub fn forget(&self, document: &str) {
        self.slots().retain(|key, _| key.document != document);
    }

    /// Drop every base and listing: the configuration or the schema changed.
    pub fn clear(&self) {
        self.slots().clear();
        self.listings
            .0
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .clear();
    }

    /// The newest base of `key` that was made under `generation`.
    fn newest(&self, key: &BaseKey, generation: (u64, u32)) -> Option<Arc<Base>> {
        self.slots()
            .get(key)?
            .newest
            .clone()
            .filter(|base| base.generation == generation)
    }

    /// Keep `base`, made on the request's thread, as the newest of `key`. The
    /// bases of the document in another root context go: only the selected
    /// context asks.
    fn store_made(&self, key: &BaseKey, base: Arc<Base>) {
        let mut slots = self.slots();
        slots.retain(|other, _| other.document != key.document || other == key);
        let slot = slots.entry(key.clone()).or_default();
        Self::keep(slot, base);
    }

    /// Keep `base`, made by a refresh, unless the slot of `key` is gone (the
    /// document closed, or the configuration changed) or holds a newer one.
    fn store_refreshed(&self, key: &BaseKey, base: Arc<Base>) {
        if let Some(slot) = self.slots().get_mut(key) {
            Self::keep(slot, base);
        }
    }

    fn keep(slot: &mut Slot, base: Arc<Base>) {
        match &slot.newest {
            Some(newest) if newest.id > base.id => {}
            _ => slot.previous = slot.newest.replace(base),
        }
    }

    /// Claim the refresh of `key`, and the id of the base it makes, which is
    /// the claim's token. `None` when one runs or the slot is gone.
    fn claim_refresh(&self, key: &BaseKey) -> Option<u64> {
        let mut slots = self.slots();
        let slot = slots.get_mut(key)?;
        if slot.refreshing.is_some() {
            return None;
        }
        let token = self.next_id();
        slot.refreshing = Some(token);
        Some(token)
    }

    /// End the refresh that holds `token`. The slot of a document that closed
    /// and opened again belongs to a newer claim, which this leaves running.
    fn end_refresh(&self, key: &BaseKey, token: u64) {
        if let Some(slot) = self.slots().get_mut(key)
            && slot.refreshing == Some(token)
        {
            slot.refreshing = None;
        }
    }

    /// The base numbered `id`, while it is one of the two newest of its
    /// document.
    fn by_id(&self, id: u64) -> Option<Arc<Base>> {
        self.slots().values().find_map(|slot| {
            [&slot.newest, &slot.previous]
                .into_iter()
                .flatten()
                .find(|base| base.id == id)
                .cloned()
        })
    }
}

/// One completion request, as the client's thread reads it. It holds no
/// database handle.
pub struct CompletionQuery {
    pub key: BaseKey,
    /// The document's path, canonical.
    pub path: PathBuf,
    /// The text of the document, as the editor holds it.
    pub text: Arc<str>,
    /// The byte of the cursor in `text`.
    pub offset: u32,
    /// The client takes snippet items.
    pub snippets: bool,
    /// The character the person typed that made the client ask.
    pub trigger: Option<char>,
    /// The open buffers, by canonical path, for a document in a selected root.
    pub overlay: HashMap<PathBuf, Arc<str>>,
    /// The session's configuration.
    pub config: AnalysisConfig,
    /// The session's configuration generation and schema revision.
    pub generation: (u64, u32),
}

/// The answer to a [`CompletionQuery`]: the list, or `None` when oxabl does
/// not know the context, and the number of the base it read (0 for none).
pub struct CompletionAnswer {
    pub list: Option<CompletionList>,
    pub base: u64,
}

/// Answer `query`. `Err` when the root of an include cannot be read.
/// Unguarded: run it inside a panic guard.
pub fn compute_completion(
    bases: &Arc<Bases>,
    query: &CompletionQuery,
) -> Result<CompletionAnswer, String> {
    fn request<'q>(
        query: &'q CompletionQuery,
        path: &'q Path,
        listing: &'q dyn DirListing,
    ) -> CompletionRequest<'q> {
        CompletionRequest {
            text: &query.text,
            offset: query.offset,
            path,
            keyword_case: query.config.pipeline.style.keyword_case,
            snippets: query.snippets,
            trigger: query.trigger,
            config: &query.config.pipeline,
            listing,
        }
    }
    let listing = CachedListing {
        fs: &*query.config.fs,
        cache: &bases.listings,
    };
    let listing: &dyn DirListing = &listing;
    // A position that reads no analysis needs no base: a `.` that ends a
    // statement must not start one.
    if !wants_base(&request(query, &query.path, listing)) {
        return Ok(CompletionAnswer {
            list: complete(&request(query, &query.path, listing), None),
            base: 0,
        });
    }
    let found = match &query.key.root {
        None => own_base(bases, query),
        Some(root) => context_base(bases, query, root)?,
    };
    let Some((base, stale)) = found else {
        let list = match query.key.root {
            // The root no longer reads the include: no list.
            Some(_) => None,
            None => complete(&request(query, &query.path, listing), None),
        };
        return Ok(CompletionAnswer { list, base: 0 });
    };
    let document = match &query.key.root {
        None => base.root.clone(),
        Some(_) => spelled_as_expanded(&base.expansion, &base.root, &query.path),
    };
    let list = with_base(&base, |navigation| {
        let fragment = query.key.root.is_none() && oxabl_workspace::is_include_fragment(&document);
        let completion = CompletionBase {
            navigation,
            text: &base.text,
            fragment,
            stale,
        };
        complete(&request(query, &document, listing), Some(&completion))
    })
    .flatten();
    Ok(CompletionAnswer {
        list,
        base: base.id,
    })
}

/// The base of a document that is its own root: the newest one, made now
/// when there is none. A stale one starts a refresh. `None` when the
/// preprocessing of the document failed.
fn own_base(bases: &Arc<Bases>, query: &CompletionQuery) -> Option<(Arc<Base>, bool)> {
    if let Some(base) = bases.newest(&query.key, query.generation) {
        if base.text != query.text
            && let Some(token) = bases.claim_refresh(&query.key)
        {
            let config = query.config.clone();
            let path = query.path.clone();
            let text = Arc::clone(&query.text);
            let generation = query.generation;
            refresh(
                bases,
                &query.key,
                token,
                Arc::clone(&query.text),
                move |id| make_own_base(&config, &path, text, id, generation),
            );
        }
        return Some((base, false));
    }
    let base = Arc::new(make_own_base(
        &query.config,
        &query.path,
        Arc::clone(&query.text),
        bases.next_id(),
        query.generation,
    )?);
    bases.store_made(&query.key, Arc::clone(&base));
    Some((base, false))
}

/// The analysis of `text`, the document at `path` as its own root. Its
/// includes are read from disk, as diagnostics read them.
fn make_own_base(
    config: &AnalysisConfig,
    path: &Path,
    text: Arc<str>,
    id: u64,
    generation: (u64, u32),
) -> Option<Base> {
    let run = config.lint_pipeline().with_define_record(true);
    let handle = run.with_file(path);
    let expansion = handle.expand(&text);
    let model = handle.navigation_model(&expansion)?;
    Some(Base {
        id,
        text,
        root: path.to_path_buf(),
        root_text: None,
        overlay: HashMap::new(),
        model: Arc::new(model),
        expansion: Arc::new(expansion),
        config: config.clone(),
        generation,
    })
}

/// Make a fresh base, numbered `token`, in the background with `make` and
/// keep it.
fn refresh(
    bases: &Arc<Bases>,
    key: &BaseKey,
    token: u64,
    text: Arc<str>,
    make: impl FnOnce(u64) -> Option<Base> + Send + 'static,
) {
    let spawned = oxabl_common::stack::builder()
        .name("oxabl-completion-refresh".to_string())
        .spawn({
            let bases = Arc::clone(bases);
            let key = key.clone();
            move || {
                oxabl_common::hold_if_injected(REFRESH_SITE, &text);
                if let Ok(Some(base)) = oxabl_common::catch_panic(|| make(token)) {
                    bases.store_refreshed(&key, Arc::new(base));
                }
                bases.end_refresh(&key, token);
            }
        });
    if spawned.is_err() {
        bases.end_refresh(key, token);
    }
}

/// The base of an include in the selected root `root`, and whether it is
/// older than a file it read. `Ok(None)` when no base can be made: the
/// preprocessing of the root failed, or it no longer reads the include, and
/// then the answer is `null`.
///
/// A base whose root or other open includes changed is still read, marked
/// stale, while a refresh makes the new one: only the preprocessing runs on
/// the request's thread, to see that the root still reads the include.
fn context_base(
    bases: &Arc<Bases>,
    query: &CompletionQuery,
    root: &Path,
) -> Result<Option<(Arc<Base>, bool)>, String> {
    let fs = OverlayFs {
        overlay: query.overlay.clone(),
        inner: Arc::clone(&query.config.fs),
    };
    let root_text = fs
        .read(root)
        .map_err(|error| format!("cannot read {}: {error}", root.display()))?;
    if let Some(base) = bases.newest(&query.key, query.generation) {
        let others_changed = base.root_text.as_deref() != Some(&*root_text)
            || includes_changed(&base, &query.overlay, &query.path);
        if others_changed && !still_reads(&query.config, &fs, root, &root_text, &query.path) {
            return Ok(None);
        }
        if (others_changed || base.text != query.text)
            && let Some(token) = bases.claim_refresh(&query.key)
        {
            let config = query.config.clone();
            let path = query.path.clone();
            let root = root.to_path_buf();
            let generation = query.generation;
            refresh(
                bases,
                &query.key,
                token,
                Arc::clone(&query.text),
                move |id| make_context_base(&config, &path, &root, fs, id, generation),
            );
        }
        return Ok(Some((base, others_changed)));
    }
    let made = make_context_base(
        &query.config,
        &query.path,
        root,
        fs,
        bases.next_id(),
        query.generation,
    );
    let Some(base) = made else {
        return Ok(None);
    };
    let base = Arc::new(base);
    bases.store_made(&query.key, Arc::clone(&base));
    Ok(Some((base, false)))
}

/// Whether an include of `base` other than the document is open now with
/// text other than the analysis read.
fn includes_changed(base: &Base, overlay: &HashMap<PathBuf, Arc<str>>, document: &Path) -> bool {
    let document = canonical_path(document);
    base.expansion
        .dependency_paths()
        .iter()
        .map(|path| canonical_path(path))
        .filter(|path| *path != document)
        .any(|path| base.overlay.get(&path) != overlay.get(&path))
}

/// Whether `root_text` still reads the include at `path`: the preprocessing
/// alone, with no analysis.
fn still_reads(
    config: &AnalysisConfig,
    fs: &OverlayFs,
    root: &Path,
    root_text: &str,
    path: &Path,
) -> bool {
    let run = LintPipeline::new(&config.pipeline, fs).with_preprocess(config.preprocess);
    let expansion = run.with_file(root).expand(root_text);
    reads(&expansion, path)
}

fn reads(expansion: &Expansion, path: &Path) -> bool {
    let include = canonical_path(path);
    !expansion.preprocessing_failed()
        && expansion
            .dependency_paths()
            .iter()
            .any(|path| canonical_path(path) == include)
}

fn make_context_base(
    config: &AnalysisConfig,
    path: &Path,
    root: &Path,
    fs: OverlayFs,
    id: u64,
    generation: (u64, u32),
) -> Option<Base> {
    let root_text = fs.read(root).ok()?;
    let run = LintPipeline::new(&config.pipeline, &fs)
        .with_preprocess(config.preprocess)
        .with_branch_tracking(true)
        .with_define_record(true);
    let handle = run.with_file(root);
    let expansion = handle.expand(&root_text);
    if !reads(&expansion, path) {
        return None;
    }
    let spelled = spelled_as_expanded(&expansion, root, path);
    let text = fs.read(&spelled).ok()?;
    let model = handle.navigation_model(&expansion)?;
    drop(handle);
    drop(run);
    Some(Base {
        id,
        text,
        root: root.to_path_buf(),
        root_text: Some(root_text),
        overlay: fs.overlay,
        model: Arc::new(model),
        expansion: Arc::new(expansion),
        config: config.clone(),
        generation,
    })
}

/// Run `answer` over a [`Navigation`] of `base`, with the pipeline's own run
/// index. `None` when the base's expansion has no text.
fn with_base<T>(base: &Base, answer: impl FnOnce(&Navigation<'_, '_>) -> T) -> Option<T> {
    let fs = OverlayFs {
        overlay: base.overlay.clone(),
        inner: Arc::clone(&base.config.fs),
    };
    let run = LintPipeline::new(&base.config.pipeline, &fs).with_preprocess(base.config.preprocess);
    let handle = run.with_file(&base.root);
    let file_of = |_| None;
    let navigation = handle.navigation_with(&base.expansion, Arc::clone(&base.model), &file_of)?;
    Some(answer(&navigation))
}

/// What `completionItem/resolve` asks for one item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveQuery {
    /// An entry of the keyword documentation index.
    Doc(u16),
    /// A symbol of base `base`.
    Symbol { base: u64, symbol: u32 },
    /// A table of the schema of base `base`, by its raw id.
    Table { base: u64, table: u32 },
    /// Field `field` of a table of the schema of base `base`.
    Field { base: u64, table: u32, field: u32 },
    /// A program or include file, as the item writes it, under an include
    /// path entry.
    Path { path: String, entry: PathBuf },
}

/// The Markdown documentation of an item. `None` when there is none, or the
/// base of a symbol or a table is no longer kept. Unguarded: run it inside a
/// panic guard.
pub fn compute_resolve(bases: &Bases, query: ResolveQuery) -> Option<String> {
    match query {
        ResolveQuery::Doc(doc) => keyword_documentation(doc),
        ResolveQuery::Symbol { base, symbol } => {
            let base = bases.by_id(base)?;
            with_base(&base, |navigation| symbol_documentation(navigation, symbol)).flatten()
        }
        ResolveQuery::Table { base, table } => {
            let base = bases.by_id(base)?;
            table_documentation(&base.config.pipeline.schema, table, None)
        }
        ResolveQuery::Field { base, table, field } => {
            let base = bases.by_id(base)?;
            table_documentation(&base.config.pipeline.schema, table, Some(field))
        }
        ResolveQuery::Path { path, entry } => Some(path_documentation(&path, &entry)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key() -> BaseKey {
        BaseKey {
            document: "file:///a.p".to_string(),
            root: None,
        }
    }

    fn open(bases: &Bases) {
        bases.slots().insert(key(), Slot::default());
    }

    #[test]
    fn a_refresh_of_a_closed_document_leaves_the_new_claim_running() {
        let bases = Bases::default();
        open(&bases);
        let old = bases.claim_refresh(&key()).expect("the first claim");
        assert_eq!(bases.claim_refresh(&key()), None, "one refresh at a time");
        bases.forget(&key().document);
        assert_eq!(bases.claim_refresh(&key()), None, "no slot after a close");
        open(&bases);
        let new = bases
            .claim_refresh(&key())
            .expect("a claim of the new slot");
        bases.end_refresh(&key(), old);
        assert_eq!(
            bases.claim_refresh(&key()),
            None,
            "the old refresh ended the new one's claim"
        );
        bases.end_refresh(&key(), new);
        assert!(bases.claim_refresh(&key()).is_some());
    }
}
