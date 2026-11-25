//! The bases of code completion: for each open document and include context,
//! the newest analysis that completion reads. See `docs/design/completion.md`.
//!
//! A completion request never waits for an analysis when a base exists. It
//! reads the newest base, which can be of an older text (stale), and when it
//! is stale it starts one refresh in the background for the next request. The
//! first request of a document makes its base on the request's thread.
//!
//! A fresh base of an open file that is its own root is the memoized
//! [`navigation_model`](crate::db) query, which semantic tokens and highlights
//! also read, so it costs no memory of its own. A base of an include in a
//! selected root context is made from the root with the open buffers over the
//! disk, as go to definition makes it.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};

use oxabl_pipeline::completion::{
    CompletionBase, CompletionList, CompletionRequest, complete, keyword_documentation,
    symbol_documentation,
};
use oxabl_pipeline::{Expansion, LintPipeline, Navigation, NavigationModel};
use oxabl_semantic::IndexedFileId;
use oxabl_workspace::FileSystem;

use crate::db::{
    AnalysisDatabase, Buffer, OverlayFs, SchemaHandle, SnapshotIndex, expanded_text,
    navigation_model, spelled_as_expanded,
};
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
    config_generation: u64,
    schema_revision: u32,
}

/// The newest bases of each document, and the refresh that runs for it.
#[derive(Default)]
struct Slot {
    newest: Option<Arc<Base>>,
    /// The base before the newest, which a resolve of an older list reads.
    previous: Option<Arc<Base>>,
    refreshing: bool,
}

/// The completion bases of one session.
#[derive(Default)]
pub struct Bases {
    slots: Mutex<HashMap<BaseKey, Slot>>,
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

    /// Drop the bases of a closed document.
    pub fn forget(&self, document: &str) {
        self.slots().retain(|key, _| key.document != document);
    }

    /// Drop every base: the configuration or the schema changed.
    pub fn clear(&self) {
        self.slots().clear();
    }

    /// The newest base of `key` that was made under `generation`.
    fn newest(&self, key: &BaseKey, generation: (u64, u32)) -> Option<Arc<Base>> {
        self.slots()
            .get(key)?
            .newest
            .clone()
            .filter(|base| (base.config_generation, base.schema_revision) == generation)
    }

    /// Keep `base` as the newest of `key`, unless a newer one is kept.
    fn store(&self, key: &BaseKey, base: Arc<Base>) {
        let mut slots = self.slots();
        let slot = slots.entry(key.clone()).or_default();
        match &slot.newest {
            Some(newest) if newest.id > base.id => {}
            _ => {
                slot.previous = slot.newest.replace(base);
            }
        }
    }

    /// Claim the refresh of `key`; false when one runs.
    fn claim_refresh(&self, key: &BaseKey) -> bool {
        let mut slots = self.slots();
        let slot = slots.entry(key.clone()).or_default();
        !std::mem::replace(&mut slot.refreshing, true)
    }

    fn end_refresh(&self, key: &BaseKey) {
        if let Some(slot) = self.slots().get_mut(key) {
            slot.refreshing = false;
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

/// One completion request, as the client's thread reads it.
pub struct CompletionQuery {
    pub key: BaseKey,
    /// The document's path, canonical.
    pub path: PathBuf,
    /// The open buffer of the document.
    pub buffer: Buffer,
    /// The byte of the cursor in the buffer's text.
    pub offset: u32,
    /// The client takes snippet items.
    pub snippets: bool,
    /// The open buffers, by path, for a document in a selected root.
    pub overlay: HashMap<PathBuf, Arc<str>>,
    /// The session's configuration generation.
    pub config_generation: u64,
}

/// The answer to a [`CompletionQuery`]: the list, or `None` when oxabl does
/// not know the context, and the number of the base it read (0 for none).
pub struct CompletionAnswer {
    pub list: Option<CompletionList>,
    pub base: u64,
}

/// Answer `query` on a worker snapshot. `None` when a concurrent write
/// cancelled the read; `Err` when the analysis failed in a way that gives no
/// answer.
pub fn compute_completion(
    snapshot: &AnalysisDatabase,
    schema: SchemaHandle,
    bases: &Arc<Bases>,
    query: &CompletionQuery,
) -> Option<Result<CompletionAnswer, String>> {
    salsa::Cancelled::catch(std::panic::AssertUnwindSafe(|| {
        let generation = (query.config_generation, *schema.revision(snapshot));
        let text: Arc<str> = Arc::from(query.buffer.text(snapshot).as_str());
        let base = match &query.key.root {
            None => own_base(snapshot, schema, bases, query, generation, &text),
            Some(root) => context_base(snapshot, schema, bases, query, generation, root, &text)?,
        };
        let Some(base) = base else {
            let list = answer(snapshot, None, query, &text, &query.path);
            return Ok(CompletionAnswer { list, base: 0 });
        };
        let document = match &query.key.root {
            None => base.root.clone(),
            Some(_) => spelled_as_expanded(&base.expansion, &base.root, &query.path),
        };
        let list = with_base(snapshot, &base, |navigation| {
            let fragment =
                query.key.root.is_none() && oxabl_workspace::is_include_fragment(&document);
            let completion = CompletionBase {
                navigation,
                text: &base.text,
                fragment,
            };
            answer(snapshot, Some(&completion), query, &text, &document)
        })
        .flatten();
        Ok(CompletionAnswer {
            list,
            base: base.id,
        })
    }))
    .ok()
}

fn answer(
    snapshot: &AnalysisDatabase,
    base: Option<&CompletionBase<'_, '_, '_>>,
    query: &CompletionQuery,
    text: &str,
    document: &Path,
) -> Option<CompletionList> {
    let request = CompletionRequest {
        text,
        offset: query.offset,
        path: document,
        keyword_case: snapshot.config().pipeline.style.keyword_case,
        snippets: query.snippets,
    };
    complete(&request, base)
}

/// The base of a document that is its own root: the newest one, made now
/// when there is none. A stale one starts a refresh. `None` when the
/// preprocessing of the document failed.
fn own_base(
    snapshot: &AnalysisDatabase,
    schema: SchemaHandle,
    bases: &Arc<Bases>,
    query: &CompletionQuery,
    generation: (u64, u32),
    text: &Arc<str>,
) -> Option<Arc<Base>> {
    if let Some(base) = bases.newest(&query.key, generation) {
        if base.text != *text && bases.claim_refresh(&query.key) {
            let db = snapshot.clone();
            let buffer = query.buffer;
            refresh(bases, &query.key, Arc::clone(text), move |id| {
                salsa::Cancelled::catch(std::panic::AssertUnwindSafe(|| {
                    make_own_base(&db, schema, buffer, id, generation)
                }))
                .ok()
                .flatten()
            });
        }
        return Some(base);
    }
    let base = Arc::new(make_own_base(
        snapshot,
        schema,
        query.buffer,
        bases.next_id(),
        generation,
    )?);
    bases.store(&query.key, Arc::clone(&base));
    Some(base)
}

fn make_own_base(
    db: &AnalysisDatabase,
    schema: SchemaHandle,
    buffer: Buffer,
    id: u64,
    (config_generation, schema_revision): (u64, u32),
) -> Option<Base> {
    let root = buffer.path(db).clone()?;
    let text: Arc<str> = Arc::from(buffer.text(db).as_str());
    let model = navigation_model(db, buffer, schema)?;
    let expansion = Arc::new(expanded_text(db, buffer));
    Some(Base {
        id,
        text,
        root,
        root_text: None,
        overlay: HashMap::new(),
        model,
        expansion,
        config_generation,
        schema_revision,
    })
}

/// Make a fresh base in the background with `make` and keep it.
fn refresh(
    bases: &Arc<Bases>,
    key: &BaseKey,
    text: Arc<str>,
    make: impl FnOnce(u64) -> Option<Base> + Send + 'static,
) {
    let id = bases.next_id();
    let spawned = oxabl_common::stack::builder()
        .name("oxabl-completion-refresh".to_string())
        .spawn({
            let bases = Arc::clone(bases);
            let key = key.clone();
            move || {
                oxabl_common::hold_if_injected(REFRESH_SITE, &text);
                if let Ok(Some(base)) = oxabl_common::catch_panic(|| make(id)) {
                    bases.store(&key, Arc::new(base));
                }
                bases.end_refresh(&key);
            }
        });
    if spawned.is_err() {
        bases.end_refresh(key);
    }
}

/// The base of an include in the selected root `root`. `Ok(None)` when no
/// base can be made: the root cannot be read, its preprocessing failed, or it
/// no longer reads the include, and then the answer is `null`.
fn context_base(
    snapshot: &AnalysisDatabase,
    schema: SchemaHandle,
    bases: &Arc<Bases>,
    query: &CompletionQuery,
    generation: (u64, u32),
    root: &Path,
    text: &Arc<str>,
) -> Result<Option<Arc<Base>>, String> {
    let config = snapshot.config();
    let fs = OverlayFs {
        overlay: query.overlay.clone(),
        inner: Arc::clone(&config.fs),
    };
    let root_text = fs
        .read(root)
        .map_err(|error| format!("cannot read {}: {error}", root.display()))?;
    if let Some(base) = bases.newest(&query.key, generation)
        && base.root_text.as_deref() == Some(&*root_text)
    {
        // Only the include changed: the root still reads it where it did.
        if base.text != *text && bases.claim_refresh(&query.key) {
            let db = snapshot.clone();
            let path = query.path.clone();
            let root = root.to_path_buf();
            refresh(bases, &query.key, Arc::clone(text), move |id| {
                salsa::Cancelled::catch(std::panic::AssertUnwindSafe(|| {
                    make_context_base(&db, schema, &path, &root, &fs, id, generation)
                }))
                .ok()
                .and_then(Result::ok)
                .flatten()
            });
        }
        return Ok(Some(base));
    }
    let made = make_context_base(
        snapshot,
        schema,
        &query.path,
        root,
        &fs,
        bases.next_id(),
        generation,
    )?;
    let Some(base) = made else {
        return Ok(None);
    };
    let base = Arc::new(base);
    bases.store(&query.key, Arc::clone(&base));
    Ok(Some(base))
}

fn make_context_base(
    snapshot: &AnalysisDatabase,
    schema: SchemaHandle,
    path: &Path,
    root: &Path,
    fs: &OverlayFs,
    id: u64,
    (config_generation, schema_revision): (u64, u32),
) -> Result<Option<Base>, String> {
    let _revision = schema.revision(snapshot);
    let config = snapshot.config();
    let Ok(root_text) = fs.read(root) else {
        return Ok(None);
    };
    let run = LintPipeline::new(&config.pipeline, fs).with_preprocess(config.preprocess);
    let handle = run.with_file(root);
    let expansion = handle.expand(&root_text);
    if expansion.preprocessing_failed() {
        return Ok(None);
    }
    let include = canonical_path(path);
    if !expansion
        .dependency_paths()
        .iter()
        .any(|path| canonical_path(path) == include)
    {
        return Ok(None);
    }
    let spelled = spelled_as_expanded(&expansion, root, path);
    let text = fs
        .read(&spelled)
        .map_err(|error| format!("cannot read {}: {error}", spelled.display()))?;
    salsa::Database::unwind_if_revision_cancelled(snapshot);
    let index = SnapshotIndex::new(snapshot, Some(root), expansion.search_base());
    let Some(model) = handle.with_index(&index).navigation_model(&expansion) else {
        return Ok(None);
    };
    Ok(Some(Base {
        id,
        text,
        root: root.to_path_buf(),
        root_text: Some(root_text),
        overlay: fs.overlay.clone(),
        model: Arc::new(model),
        expansion: Arc::new(expansion),
        config_generation,
        schema_revision,
    }))
}

/// Run `answer` over a [`Navigation`] of `base`. `None` when the base's
/// expansion has no text (its preprocessing failed).
fn with_base<T>(
    snapshot: &AnalysisDatabase,
    base: &Base,
    answer: impl FnOnce(&Navigation<'_, '_>) -> T,
) -> Option<T> {
    let config = snapshot.config();
    let fs = OverlayFs {
        overlay: base.overlay.clone(),
        inner: Arc::clone(&config.fs),
    };
    let run = LintPipeline::new(&config.pipeline, &fs).with_preprocess(config.preprocess);
    let index = SnapshotIndex::new(snapshot, Some(&base.root), base.expansion.search_base());
    let handle = run.with_file(&base.root).with_index(&index);
    let file_of = |id: IndexedFileId| {
        config
            .index
            .registered()
            .into_iter()
            .find(|(_, known)| *known == id)
            .map(|(file, _)| file.path(snapshot).clone())
    };
    let navigation = handle.navigation_with(&base.expansion, Arc::clone(&base.model), &file_of)?;
    Some(answer(&navigation))
}

/// What `completionItem/resolve` asks for one item.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolveQuery {
    /// An entry of the keyword documentation index.
    Doc(u16),
    /// A symbol of base `base`.
    Symbol { base: u64, symbol: u32 },
}

/// The Markdown documentation of an item. `None` when there is none, or the
/// base of a symbol is no longer kept, or a write cancelled the read.
pub fn compute_resolve(
    snapshot: &AnalysisDatabase,
    bases: &Bases,
    query: ResolveQuery,
) -> Option<String> {
    match query {
        ResolveQuery::Doc(doc) => keyword_documentation(doc),
        ResolveQuery::Symbol { base, symbol } => {
            let base = bases.by_id(base)?;
            salsa::Cancelled::catch(std::panic::AssertUnwindSafe(|| {
                with_base(snapshot, &base, |navigation| {
                    symbol_documentation(navigation, symbol)
                })
                .flatten()
            }))
            .ok()
            .flatten()
        }
    }
}
