//! The parts of a workspace pass, kept so that the next pass analyses only the
//! files that changed.
//!
//! A pass analyses each file of the workspace once. The part of the reverse
//! graph that a file gives and its symbol rows depend only on the text of the
//! file and on the files that its analysis read or looked for: its include
//! files at any depth and the class and program files whose facts it took from
//! the index. The pass reads through an [`ObservingFileSystem`], so it knows
//! these files for each analysis, and through [`Stamping`], so it knows the
//! content of each file that it read.
//!
//! The next pass compares each of those files with what the pass read (the
//! content hash, after a `stat` that shows a change). It analyses again only a
//! file that is new or that read or looked for a file that changed, and joins
//! the kept parts of the other files into a new graph. The graph is the graph
//! of a full pass over the same text.
//!
//! Two inputs of an analysis are not files, and a change of either makes a
//! full pass (fail closed):
//!
//! - the configuration and the schema (the session generations, and
//!   [`ConfigKey`], what the pass read of the configuration that it resolved);
//! - the map of `SHARED` producers, which every file can read. A pass builds it
//!   on the first question of any analysis, as a pass that scans every file up
//!   front would, and a pass in which no file asks never builds it. When the
//!   earlier pass built it, the next pass scans the changed files again, and
//!   when the map is not the same, the pass is full.
//!
//! # The table of paths
//!
//! Each input is a path with a record of what the pass read there. The records
//! are kept for the paths that a kept file reads, and no others: after each
//! pass the table is cut down to those paths. A record of a path that no kept
//! file reads would go stale, as nothing checks it, and a later file that read
//! the path again would trust it. The table is shared between passes while no
//! path comes or goes, and the records are shared one by one.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use oxabl_common::LintSeverityMap;
use oxabl_daemon_protocol::SymbolRow;
use oxabl_index::{DeferredProducers, SharedProducers};
use oxabl_pipeline::{Expansion, FilePart, LintPipeline, LintResult, PipelineConfig, ReverseGraph};
use oxabl_semantic::IndexName;
use oxabl_workspace::{
    DirEntry, EncodedFileSystem, FeaturesConfig, FileSystem, Observations, ObservingFileSystem,
    PreprocessorConfig, SourceDecoding,
};

use crate::session::{FileStamp, WorkspaceProgress};

/// Open buffers by workspace path.
pub(crate) type Overlay = HashMap<PathBuf, Arc<str>>;

/// The content hash of each open buffer by workspace path.
pub(crate) type OverlayHashes = HashMap<PathBuf, blake3::Hash>;

/// The hashes of `overlay`.
pub(crate) fn overlay_hashes(overlay: &Overlay) -> OverlayHashes {
    overlay
        .iter()
        .map(|(path, text)| (path.clone(), blake3::hash(text.as_bytes())))
        .collect()
}

/// The open buffers over the disk. A case-insensitive lookup and a listing
/// read the disk, as `oxabl check` does.
pub(crate) struct OverlayFileSystem {
    pub overlay: Overlay,
    pub disk: EncodedFileSystem,
}

impl FileSystem for OverlayFileSystem {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        self.overlay
            .get(path)
            .cloned()
            .map(Ok)
            .unwrap_or_else(|| self.disk.read(path))
    }

    fn exists(&self, path: &Path) -> bool {
        self.overlay.contains_key(path) || self.disk.exists(path)
    }

    fn find_ignoring_case(&self, dir: &Path, relative: &Path) -> Vec<PathBuf> {
        self.disk.find_ignoring_case(dir, relative)
    }

    fn find_ignoring_case_listing(
        &self,
        dir: &Path,
        relative: &Path,
        listed: &mut Vec<PathBuf>,
    ) -> Vec<PathBuf> {
        self.disk.find_ignoring_case_listing(dir, relative, listed)
    }

    fn list_dir(&self, dir: &Path) -> Option<Vec<DirEntry>> {
        self.disk.list_dir(dir)
    }
}

/// What a pass read of one file: the hash of the text, and the disk stamp
/// taken before the read (`None` when the text came from a buffer).
#[derive(Clone)]
struct ReadStamp {
    /// `None` when the read failed for another reason than a missing file:
    /// such a file counts as changed at each pass, so the files that read it
    /// are analysed again until it reads.
    hash: Option<blake3::Hash>,
    disk: Option<FileStamp>,
}

/// An [`OverlayFileSystem`] that keeps a [`ReadStamp`] of the first read of
/// each path, and the stamp of each directory when it is first listed.
pub(crate) struct Stamping {
    fs: OverlayFileSystem,
    seen: Mutex<HashMap<PathBuf, ReadStamp>>,
    listed: Mutex<HashMap<PathBuf, FileStamp>>,
}

/// What a pass took from the file system, taken out of [`Stamping`].
struct Seen {
    read: HashMap<PathBuf, ReadStamp>,
    listed: HashMap<PathBuf, FileStamp>,
}

impl Stamping {
    pub fn new(fs: OverlayFileSystem) -> Self {
        Stamping {
            fs,
            seen: Mutex::new(HashMap::new()),
            listed: Mutex::new(HashMap::new()),
        }
    }

    fn take(&self) -> Seen {
        Seen {
            read: lock_take(&self.seen),
            listed: lock_take(&self.listed),
        }
    }

    fn seen(&self) -> std::sync::MutexGuard<'_, HashMap<PathBuf, ReadStamp>> {
        self.seen
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    /// Keep the stamp of `dir` the first time it is listed.
    fn stamp_listed(&self, dir: &Path, stamp: FileStamp) {
        self.listed
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .entry(dir.to_path_buf())
            .or_insert(stamp);
    }
}

fn lock_take<T: Default>(mutex: &Mutex<T>) -> T {
    std::mem::take(&mut *mutex.lock().unwrap_or_else(|p| p.into_inner()))
}

impl FileSystem for Stamping {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        if self.seen().contains_key(path) {
            return self.fs.read(path);
        }
        // The stamp before the read: a write after it changes the stamp, so
        // the next pass reads the file again.
        let disk = (!self.fs.overlay.contains_key(path)).then(|| FileStamp::capture(path.into()));
        let read = self.fs.read(path);
        let hash = match &read {
            Ok(text) => Some(blake3::hash(text.as_bytes())),
            Err(error)
                if matches!(
                    error.kind(),
                    io::ErrorKind::NotFound | io::ErrorKind::NotADirectory
                ) =>
            {
                return read;
            }
            Err(_) => None,
        };
        self.seen()
            .entry(path.to_path_buf())
            .or_insert(ReadStamp { hash, disk });
        read
    }

    fn exists(&self, path: &Path) -> bool {
        self.fs.exists(path)
    }

    fn find_ignoring_case(&self, dir: &Path, relative: &Path) -> Vec<PathBuf> {
        self.find_ignoring_case_listing(dir, relative, &mut Vec::new())
    }

    /// The stamp of `dir` is taken before the listing. A directory below it
    /// is stamped as soon as the listing returns, so a file that lands there
    /// in between is not seen.
    fn find_ignoring_case_listing(
        &self,
        dir: &Path,
        relative: &Path,
        listed: &mut Vec<PathBuf>,
    ) -> Vec<PathBuf> {
        self.stamp_listed(dir, FileStamp::capture(dir.to_path_buf()));
        let start = listed.len();
        let found = self.fs.find_ignoring_case_listing(dir, relative, listed);
        for below in &listed[start..] {
            self.stamp_listed(below, FileStamp::capture(below.clone()));
        }
        found
    }

    fn list_dir(&self, dir: &Path) -> Option<Vec<DirEntry>> {
        self.stamp_listed(dir, FileStamp::capture(dir.to_path_buf()));
        self.fs.list_dir(dir)
    }
}

/// The file system of a pass.
pub(crate) type PassFileSystem = ObservingFileSystem<Stamping>;

/// The file system a pass reads through.
pub(crate) fn pass_file_system(overlay: Overlay, disk: EncodedFileSystem) -> PassFileSystem {
    ObservingFileSystem::new(Stamping::new(OverlayFileSystem { overlay, disk }))
}

/// What the analyses of a pass took from one path.
#[derive(Clone, Default)]
struct Input {
    /// `Some(Some(_))`: the text was read. `Some(None)`: only the existence
    /// of the file was used.
    read: Option<Option<ReadStamp>>,
    /// The path was looked for and was not there.
    absent: bool,
    /// The directory was listed. `Some(None)` when no stamp was taken: such a
    /// directory counts as changed at each pass.
    listed: Option<Option<FileStamp>>,
}

/// What one file gives to a pass.
pub(crate) struct KeptFile {
    part: FilePart,
    /// The row of the file itself, and the rows of what it declares.
    symbols: Vec<Arc<SymbolRow>>,
}

/// A kept file, and what it depends on. Cheap to clone.
#[derive(Clone)]
struct Entry {
    file: Arc<KeptFile>,
    /// The inputs of its analysis and of its `SHARED` scan, as ids of
    /// [`PathTable`].
    inputs: Arc<[u32]>,
    /// The `SHARED` names the file produces; `Some` when the pass scanned it.
    shared: Option<Arc<[IndexName]>>,
}

/// Every path an analysis read or looked for, by id. Paths are kept as the
/// analysis spelled them, as the buffers are matched by that spelling.
#[derive(Clone, Default)]
struct PathTable {
    paths: Vec<PathBuf>,
    ids: HashMap<PathBuf, u32>,
}

impl PathTable {
    fn id(&mut self, path: &Path) -> (u32, bool) {
        if let Some(&id) = self.ids.get(path) {
            return (id, false);
        }
        let id = u32::try_from(self.paths.len()).expect("more than u32::MAX input paths");
        self.paths.push(path.to_path_buf());
        self.ids.insert(path.to_path_buf(), id);
        (id, true)
    }
}

/// The parts of a completed pass, and what each part depends on.
pub(crate) struct KeptPass {
    /// What the pass read of the configuration it resolved.
    config: ConfigKey,
    table: Arc<PathTable>,
    /// The record of each path of `table`, by id.
    inputs: Vec<Arc<Input>>,
    /// The discovered files, in the order of discovery.
    files: Vec<PathBuf>,
    entries: HashMap<PathBuf, Entry>,
    /// Whether the `SHARED` producer map was built, and so each entry has its
    /// `shared` names. When it was not, no analysis read it.
    scanned: bool,
    /// The rows of the schema tables, which only the configuration changes.
    tables: Arc<[Arc<SymbolRow>]>,
    /// The graph that the entries were joined into; `None` until the pass
    /// that made this one has joined them. The next pass starts from it.
    graph: Option<Arc<ReverseGraph>>,
}

/// What a join of the parts of a pass starts from: the graph of the earlier
/// pass, and the parts that changed.
struct JoinBase<'b> {
    graph: &'b ReverseGraph,
    /// The parts of the earlier pass that this pass analysed again, and those
    /// of the files that are gone.
    removed: Vec<&'b FilePart>,
    /// The files that this pass analysed.
    again: &'b [PathBuf],
}

/// A completed pass: the graph and the symbol rows, and the parts to keep.
pub(crate) struct Built {
    pub graph: Arc<ReverseGraph>,
    pub symbols: Arc<Vec<Arc<SymbolRow>>>,
    pub kept: Arc<KeptPass>,
    /// How many files the pass analysed.
    pub analysed: usize,
}

/// Everything in a [`PipelineConfig`] that an analysis reads. Two
/// configurations with the same key give the same parts.
///
/// The fields are named one by one, so a new field of the configuration is a
/// decision here, and no text of a `Debug` impl stands for a field. The
/// formatter style is left out: only the formatter reads it.
#[derive(Clone, PartialEq)]
pub(crate) struct ConfigKey {
    include_paths: Vec<PathBuf>,
    root_relative_include_paths: Vec<PathBuf>,
    lint_severities: LintSeverityMap,
    preprocessor: PreprocessorConfig,
    run_extensions: Vec<String>,
    case_insensitive_lookup: bool,
    decoding: SourceDecoding,
    root_extensions: Vec<String>,
    features: FeaturesConfig,
    schema_loaded: bool,
    schema: blake3::Hash,
}

impl ConfigKey {
    pub(crate) fn of(config: &PipelineConfig) -> Self {
        ConfigKey {
            include_paths: config.include_paths.clone(),
            root_relative_include_paths: config.root_relative_include_paths.clone(),
            lint_severities: config.lint_severities.clone(),
            preprocessor: config.preprocessor.clone(),
            run_extensions: config.run_extensions.clone(),
            case_insensitive_lookup: config.case_insensitive_lookup,
            decoding: config.decoding,
            root_extensions: config.root_extensions.clone(),
            features: config.features,
            schema_loaded: config.schema_loaded,
            schema: schema_hash(config),
        }
    }
}

/// A hash of the schema (`Schema::hash_content`). A schema
/// that was parsed and one that was read from the cache give the same hash.
fn schema_hash(config: &PipelineConfig) -> blake3::Hash {
    let mut hashing = BufferedBlake3::new();
    config.schema.hash_content(&mut hashing);
    hashing.digest()
}

/// A [`std::hash::Hasher`] that feeds BLAKE3 the bytes it is given.
///
/// `Hash` writes a field at a time. BLAKE3 takes a block at a time, and a write
/// of a few bytes to it costs more than the bytes do, so the writes wait in a
/// buffer and go in blocks. The digest is that of the bytes in one write.
struct BufferedBlake3 {
    hasher: blake3::Hasher,
    pending: Vec<u8>,
}

impl BufferedBlake3 {
    const BLOCK: usize = 16 * 1024;

    fn new() -> Self {
        BufferedBlake3 {
            hasher: blake3::Hasher::new(),
            pending: Vec::with_capacity(Self::BLOCK),
        }
    }

    fn digest(&self) -> blake3::Hash {
        let mut hasher = self.hasher.clone();
        hasher.update(&self.pending);
        hasher.finalize()
    }
}

impl std::hash::Hasher for BufferedBlake3 {
    fn write(&mut self, bytes: &[u8]) {
        if self.pending.len() + bytes.len() > Self::BLOCK {
            self.hasher.update(&self.pending);
            self.pending.clear();
        }
        if bytes.len() > Self::BLOCK {
            self.hasher.update(bytes);
        } else {
            self.pending.extend_from_slice(bytes);
        }
    }

    fn finish(&self) -> u64 {
        u64::from_le_bytes(self.digest().as_bytes()[..8].try_into().expect("32 bytes"))
    }
}

/// The inspection of each analysis: the symbol rows of a file.
pub(crate) type Collect<'c> =
    &'c (dyn Fn(&Path, &Expansion, &LintResult, &dyn FileSystem) -> Vec<SymbolRow> + Sync);

/// What a pass needs besides its files.
pub(crate) struct PassContext<'c> {
    pub config: &'c PipelineConfig,
    pub fs: &'c PassFileSystem,
    pub jobs: usize,
    pub collect: Collect<'c>,
    pub progress: &'c WorkspaceProgress,
}

/// What each file's `SHARED` scan found and read, kept by the scan of a map
/// that an analysis asked for.
type ScanSink = Mutex<HashMap<PathBuf, (Vec<IndexName>, Observations)>>;

/// The `SHARED` names each file of a list produces, with what each scan read.
type Scans = Vec<(Vec<IndexName>, Observations)>;

/// The `SHARED` producers an analysis reads.
enum Producers<'p> {
    /// A map that was built, from the kept scans.
    Known(&'p SharedProducers),
    /// A map built on the first question of any analysis, and never when none
    /// asks.
    Lazy(&'p DeferredProducers),
}

impl PassContext<'_> {
    /// The `SHARED` names each of `files` produces, with what each scan
    /// observed. The same answer as [`SharedProducers::build_parallel`].
    fn scan_shared(&self, files: &[PathBuf]) -> Scans {
        let run = LintPipeline::new(self.config, self.fs);
        let includes = run.index_includes();
        parallel(files, self.jobs, |path| scan_observed(path, &includes))
    }

    /// A map of the `SHARED` producers of `files` that its first question
    /// builds, and the sink that then holds what each scan read.
    fn lazy_producers(&self, files: &[PathBuf]) -> (DeferredProducers, Arc<ScanSink>) {
        let sink = Arc::new(ScanSink::default());
        let kept = Arc::clone(&sink);
        let producers = DeferredProducers::with_scan(files, self.jobs, move |path, includes| {
            let (names, observed) = scan_observed(path, includes);
            kept.lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner())
                .insert(path.to_path_buf(), (names.clone(), observed));
            names
        });
        (producers, sink)
    }

    /// Analyse `files` with the producers of `producers`.
    fn analyse(
        &self,
        files: &[PathBuf],
        producers: &Producers<'_>,
    ) -> Vec<(FilePart, Vec<SymbolRow>, Observations)> {
        let fs: &dyn FileSystem = self.fs;
        ReverseGraph::analyse_parts(
            files,
            self.jobs,
            || {
                let run = LintPipeline::new(self.config, self.fs);
                match producers {
                    Producers::Known(map) => run.with_shared_producers(map),
                    Producers::Lazy(map) => run.with_deferred_producers(map),
                }
            },
            |path, expansion, result| {
                let rows = (self.collect)(path, expansion, result, fs);
                if oxabl_common::INJECTION_ENABLED
                    && let Ok(text) = fs.read(path)
                {
                    oxabl_common::hold_if_injected(oxabl_common::panic_sites::PASS_FILE, &text);
                }
                self.progress.advance();
                rows
            },
        )
        .into_iter()
        .map(|analysed| {
            (
                analysed.part,
                analysed.inspected.unwrap_or_default(),
                analysed
                    .observations
                    .map(Arc::unwrap_or_clone)
                    .unwrap_or_default(),
            )
        })
        .collect()
    }
}

/// The `SHARED` names of one file, with what the scan read.
fn scan_observed(
    path: &Path,
    includes: &oxabl_index::IncludeContext<'_>,
) -> (Vec<IndexName>, Observations) {
    let mut names = Vec::new();
    let observed = includes.fs.observe(&mut || {
        names = oxabl_index::definitions_in(path, includes);
    });
    (
        names,
        observed.map(Arc::unwrap_or_clone).unwrap_or_default(),
    )
}

/// Run `work` over `files` on `jobs` threads; the answers in `files` order.
fn parallel<T: Send>(files: &[PathBuf], jobs: usize, work: impl Fn(&Path) -> T + Sync) -> Vec<T> {
    let jobs = jobs.clamp(1, files.len().max(1));
    if jobs == 1 {
        return files.iter().map(|path| work(path)).collect();
    }
    let next = AtomicUsize::new(0);
    let mut done: Vec<(usize, T)> = std::thread::scope(|scope| {
        let workers: Vec<_> = (0..jobs)
            .map(|_| {
                let (next, work) = (&next, &work);
                oxabl_common::stack::builder()
                    .spawn_scoped(scope, move || {
                        let mut mine = Vec::new();
                        loop {
                            let at = next.fetch_add(1, Ordering::Relaxed);
                            let Some(path) = files.get(at) else { break };
                            mine.push((at, work(path)));
                        }
                        mine
                    })
                    .expect("failed to spawn an analysis thread")
            })
            .collect();
        workers
            .into_iter()
            .flat_map(|worker| match worker.join() {
                Ok(mine) => mine,
                Err(panic) => std::panic::resume_unwind(panic),
            })
            .collect()
    });
    done.sort_unstable_by_key(|(at, _)| *at);
    done.into_iter().map(|(_, value)| value).collect()
}

/// Analyse every file of `files`.
pub(crate) fn full_pass(context: &PassContext<'_>, files: Vec<PathBuf>) -> Built {
    context.progress.set_total(files.len());
    let (producers, sink) = context.lazy_producers(&files);
    let analysed = context.analyse(&files, &Producers::Lazy(&producers));
    let seen = context.fs.inner().take();
    let mut kept = KeptPass::empty(ConfigKey::of(context.config));
    kept.tables = table_symbols(context.config)
        .into_iter()
        .map(Arc::new)
        .collect();
    kept.install(&files, analysed, None, &seen);
    kept.files = files;
    kept.take_lazy_scans(&sink, &seen);
    let analysed = kept.files.len();
    kept.finish(context.config, analysed, None)
}

impl KeptPass {
    fn empty(config: ConfigKey) -> Self {
        KeptPass {
            config,
            table: Arc::new(PathTable::default()),
            inputs: Vec::new(),
            files: Vec::new(),
            entries: HashMap::new(),
            scanned: false,
            tables: Arc::from([]),
            graph: None,
        }
    }

    /// Whether a pass over `config` can start from this one.
    pub fn same_config(&self, config: &PipelineConfig) -> bool {
        self.config == ConfigKey::of(config)
    }

    /// Estimated heap bytes of the kept parts and inputs.
    pub fn estimated_heap_bytes(&self) -> usize {
        let path = |p: &PathBuf| std::mem::size_of::<PathBuf>() + p.as_os_str().len();
        self.table.paths.iter().map(path).sum::<usize>() * 2
            + self.inputs.len() * std::mem::size_of::<Input>()
            + self.files.iter().map(path).sum::<usize>()
            + self
                .entries
                .iter()
                .map(|(file, entry)| {
                    path(file)
                        + std::mem::size_of::<Entry>()
                        + entry.file.part.estimated_heap_bytes()
                        + entry.inputs.len() * 4
                        + entry
                            .shared
                            .as_ref()
                            .map_or(0, |names| names.len() * std::mem::size_of::<IndexName>())
                        + entry
                            .file
                            .symbols
                            .iter()
                            .map(|row| {
                                std::mem::size_of::<SymbolRow>()
                                    + row.id.len()
                                    + row.name.len()
                                    + row.file.as_ref().map_or(0, String::len) * 2
                            })
                            .sum::<usize>()
                })
                .sum::<usize>()
    }

    /// The tables agree, and every id of an entry is in them. Not a check of
    /// the cut: see [`compact`](Self::compact).
    fn assert_consistent(&self) {
        assert_eq!(self.table.paths.len(), self.table.ids.len());
        assert_eq!(self.table.paths.len(), self.inputs.len());
        assert!(self.scanned || self.entries.values().all(|e| e.shared.is_none()));
    }

    /// The id of `path`, added when it is new.
    fn id(&mut self, path: &Path) -> u32 {
        if let Some(&id) = self.table.ids.get(path) {
            return id;
        }
        let (id, _) = Arc::make_mut(&mut self.table).id(path);
        self.inputs.push(Arc::new(Input::default()));
        id
    }

    /// The record of `id`, to change.
    fn input_mut(&mut self, id: u32) -> &mut Input {
        Arc::make_mut(&mut self.inputs[id as usize])
    }

    /// Record what the analysis of `file` observed, and give its input ids.
    ///
    /// A record that is kept stays: a path that two analyses read differently
    /// (a write during the pass) keeps the first hash, and the next pass
    /// analyses every file that read it again. The records of the paths that
    /// changed before this pass were cleared by the caller, and the records of
    /// the paths that no kept file reads were cut: see
    /// [`compact`](Self::compact).
    fn absorb<'o>(
        &mut self,
        file: &Path,
        observed: impl IntoIterator<Item = &'o Observations>,
        seen: &Seen,
    ) -> Vec<u32> {
        let mut ids = Vec::new();
        let read = |kept: &mut KeptPass, ids: &mut Vec<u32>, path: &Path| {
            let id = kept.id(path);
            ids.push(id);
            let record = match (&kept.inputs[id as usize].read, seen.read.get(path)) {
                (Some(Some(_)), _) | (Some(None), None) => return,
                (_, Some(stamp)) => Some(Some(stamp.clone())),
                (None, None) => Some(None),
            };
            kept.input_mut(id).read = record;
        };
        // The file itself, also when an analysis did not observe its read.
        read(self, &mut ids, file);
        for observations in observed {
            for path in observations.read() {
                read(self, &mut ids, path);
            }
            for path in observations.absent() {
                let id = self.id(path);
                ids.push(id);
                if !self.inputs[id as usize].absent {
                    self.input_mut(id).absent = true;
                }
            }
            for path in observations.listed() {
                let id = self.id(path);
                ids.push(id);
                if self.inputs[id as usize].listed.is_none() {
                    // The stamp from when the directory was listed.
                    self.input_mut(id).listed = Some(seen.listed.get(path).cloned());
                }
            }
        }
        ids.sort_unstable();
        ids.dedup();
        ids
    }

    /// Keep what the scan of the `SHARED` producers read, when an analysis
    /// built the map: the names of each file, and its scan inputs in the
    /// inputs of its entry.
    fn take_lazy_scans(&mut self, sink: &ScanSink, seen: &Seen) {
        let scans = std::mem::take(&mut *sink.lock().unwrap_or_else(|p| p.into_inner()));
        if scans.is_empty() {
            return;
        }
        self.scanned = true;
        let files = self.files.clone();
        for path in files {
            let (names, observed) = scans.get(&path).expect("the scan covers every file");
            let scan_ids = self.absorb(&path, [observed], seen);
            let entry = self
                .entries
                .get_mut(&path)
                .expect("every file has an entry");
            let mut ids = entry.inputs.to_vec();
            ids.extend(scan_ids);
            ids.sort_unstable();
            ids.dedup();
            entry.inputs = ids.into();
            entry.shared = Some(names.as_slice().into());
        }
    }

    /// Cut the table to the ids that an entry reads, and number them again.
    ///
    /// The records of the other paths are dropped: nothing checks them, so a
    /// later file that read such a path again would take the hash of text that
    /// is gone as the hash of what it read. Dropping them also bounds the
    /// table by the inputs of the kept files, and not by every path that an
    /// edit read in the whole session.
    fn compact(&mut self) {
        let count = self.table.paths.len();
        let mut live = vec![false; count];
        for entry in self.entries.values() {
            for &id in entry.inputs.iter() {
                assert!(
                    (id as usize) < count,
                    "an entry reads an id outside the table"
                );
                live[id as usize] = true;
            }
        }
        if live.iter().all(|&is_live| is_live) {
            self.assert_consistent();
            return;
        }
        let mut renumbered = vec![u32::MAX; count];
        let mut table = PathTable::default();
        let mut inputs = Vec::new();
        for (id, _) in live.iter().enumerate().filter(|(_, is_live)| **is_live) {
            let (new, _) = table.id(&self.table.paths[id]);
            renumbered[id] = new;
            inputs.push(Arc::clone(&self.inputs[id]));
        }
        for entry in self.entries.values_mut() {
            let ids: Vec<u32> = entry
                .inputs
                .iter()
                .map(|&id| renumbered[id as usize])
                .collect();
            entry.inputs = ids.into();
        }
        self.table = Arc::new(table);
        self.inputs = inputs;
        self.assert_consistent();
    }

    /// A pass that starts from this one over `files`: the entries of the files
    /// that are still there, and a table cut to what they read.
    fn start(&self, files: &[PathBuf]) -> KeptPass {
        let current: HashSet<&PathBuf> = files.iter().collect();
        let mut kept = KeptPass {
            config: self.config.clone(),
            table: Arc::clone(&self.table),
            inputs: self.inputs.clone(),
            files: Vec::new(),
            entries: self
                .entries
                .iter()
                .filter(|(path, _)| current.contains(path))
                .map(|(path, entry)| (path.clone(), entry.clone()))
                .collect(),
            scanned: self.scanned,
            tables: Arc::clone(&self.tables),
            graph: None,
        };
        kept.compact();
        kept
    }

    /// Join the kept parts into the graph and the symbol rows.
    fn finish(
        mut self,
        config: &PipelineConfig,
        analysed: usize,
        base: Option<JoinBase<'_>>,
    ) -> Built {
        self.compact();
        let fs = config.file_system();
        let run = LintPipeline::new(config, &fs);
        let parts: Vec<&FilePart> = self
            .files
            .iter()
            .map(|path| &self.entries[path].file.part)
            .collect();
        let graph = match base {
            Some(base) => {
                let added: Vec<&FilePart> = base
                    .again
                    .iter()
                    .map(|path| &self.entries[path].file.part)
                    .collect();
                ReverseGraph::updated(base.graph, &run, &self.files, &parts, &base.removed, &added)
            }
            None => ReverseGraph::from_parts(&run, &self.files, parts.iter().copied()),
        };
        drop(parts);
        let graph = Arc::new(graph);
        self.graph = Some(Arc::clone(&graph));
        let mut symbols: Vec<Arc<SymbolRow>> = self
            .files
            .iter()
            .flat_map(|path| self.entries[path].file.symbols.iter().cloned())
            .chain(self.tables.iter().cloned())
            .collect();
        symbols.sort_by(|left, right| left.id.cmp(&right.id));
        symbols.dedup_by(|left, right| left.id == right.id);
        Built {
            graph,
            symbols: Arc::new(symbols),
            kept: Arc::new(self),
            analysed,
        }
    }

    /// Whether every path whose text or existence differs between the
    /// buffers `from`, which the pass read through, and the buffers `now`
    /// reads the same now as the pass read it. A path that the pass read from
    /// a buffer that is closed now counts as different, as its disk text is
    /// not read here.
    pub fn reads_same(&self, from: &OverlayHashes, now: &Overlay) -> bool {
        let moved = from
            .keys()
            .filter(|path| !now.contains_key(*path))
            .map(|path| (path, None))
            .chain(now.iter().filter_map(|(path, text)| {
                let hash = blake3::hash(text.as_bytes());
                (from.get(path) != Some(&hash)).then_some((path, Some(hash)))
            }));
        for (path, hash) in moved {
            let Some(&id) = self.table.ids.get(path) else {
                continue;
            };
            let input = &self.inputs[id as usize];
            let Some(hash) = hash else {
                return false;
            };
            let read_other = matches!(&input.read, Some(Some(stamp)) if stamp.hash != Some(hash));
            if read_other || input.absent {
                return false;
            }
        }
        true
    }

    /// How many paths that the pass looked for and did not find are on disk
    /// now.
    pub fn appeared_paths(&self) -> u32 {
        let appeared = self
            .inputs
            .iter()
            .zip(&self.table.paths)
            .filter(|(input, path)| input.absent && path.exists())
            .count();
        u32::try_from(appeared).unwrap_or(u32::MAX)
    }

    /// The ids whose file is not as the pass read it. The table holds only the
    /// ids that an entry reads.
    fn changed_inputs(&mut self, overlay: &Overlay, disk: &EncodedFileSystem) -> Vec<bool> {
        let exists = |path: &Path| overlay.contains_key(path) || disk.exists(path);
        let mut changed = vec![false; self.inputs.len()];
        let mut touched = Vec::new();
        for (id, moved) in changed.iter_mut().enumerate() {
            let path = &self.table.paths[id];
            let input = &self.inputs[id];
            *moved = match &input.read {
                Some(Some(stamp)) => match read_since(path, stamp, overlay, disk) {
                    ReadSince::Same => false,
                    ReadSince::Touched(now) => {
                        touched.push((id, now));
                        false
                    }
                    ReadSince::Moved => true,
                },
                Some(None) => !exists(path),
                None => false,
            };
            *moved |= input.absent && exists(path);
            *moved |= match &input.listed {
                Some(Some(stamp)) => stamp.changed(),
                Some(None) => true,
                None => false,
            };
        }
        // A file that was written with the same text reads from its new stamp.
        for (id, now) in touched {
            if let Some(Some(stamp)) = &mut self.input_mut(id as u32).read {
                stamp.disk = Some(now);
            }
        }
        changed
    }

    /// Clear the record of each path that changed: the analyses of this pass
    /// record it again.
    fn clear_moved(&mut self, changed: &[bool]) {
        for (id, _) in changed.iter().enumerate().filter(|(_, moved)| **moved) {
            self.inputs[id] = Arc::new(Input::default());
        }
    }

    /// The files to analyse: the new ones and the ones that read a path that
    /// changed.
    fn to_analyse(&self, files: &[PathBuf], changed: &[bool]) -> Vec<PathBuf> {
        files
            .iter()
            .filter(|path| {
                self.entries
                    .get(*path)
                    .is_none_or(|entry| entry.inputs.iter().any(|&id| changed[id as usize]))
            })
            .cloned()
            .collect()
    }

    /// Keep the files of `again` that were analysed, with `scans` (the scan of
    /// each) when the `SHARED` producers were known.
    fn install(
        &mut self,
        again: &[PathBuf],
        analysed: Vec<(FilePart, Vec<SymbolRow>, Observations)>,
        scans: Option<Scans>,
        seen: &Seen,
    ) {
        let mut scans = scans.map(Vec::into_iter);
        for (path, (part, symbols, observed)) in again.iter().zip(analysed) {
            let scan = scans.as_mut().and_then(Iterator::next);
            let (ids, shared) = match &scan {
                Some((names, scan_observed)) => (
                    self.absorb(path, [&observed, scan_observed], seen),
                    Some(Arc::from(names.as_slice())),
                ),
                None => (self.absorb(path, [&observed], seen), None),
            };
            let symbols = file_symbols(std::slice::from_ref(path))
                .into_iter()
                .chain(symbols)
                .map(Arc::new)
                .collect();
            self.entries.insert(
                path.clone(),
                Entry {
                    file: Arc::new(KeptFile { part, symbols }),
                    inputs: ids.into(),
                    shared,
                },
            );
        }
    }
}

/// How the text at a path compares with what a pass read there.
enum ReadSince {
    Same,
    /// The same text, written since: the stamp to read from now on.
    Touched(FileStamp),
    Moved,
}

fn read_since(
    path: &Path,
    stamp: &ReadStamp,
    overlay: &Overlay,
    disk: &EncodedFileSystem,
) -> ReadSince {
    let moved = |text: &str| Some(blake3::hash(text.as_bytes())) != stamp.hash;
    match overlay.get(path) {
        _ if stamp.hash.is_none() => ReadSince::Moved,
        Some(text) if moved(text) => ReadSince::Moved,
        Some(_) => ReadSince::Same,
        None if stamp.disk.as_ref().is_some_and(|disk| !disk.changed()) => ReadSince::Same,
        None => {
            let now = FileStamp::capture(path.to_path_buf());
            match disk.read(path) {
                Ok(text) if !moved(&text) => ReadSince::Touched(now),
                _ => ReadSince::Moved,
            }
        }
    }
}

/// What [`incremental_pass`] gives.
pub(crate) enum Refreshed {
    /// No file changed: the graph and the symbol rows of the earlier pass
    /// stay right. The kept parts now hold the newer stamps and buffers.
    Same(Arc<KeptPass>),
    /// The changed files were analysed again.
    Built(Built),
    /// The `SHARED` producers are not the same: the pass must be full.
    Full,
}

/// Start from `previous` and analyse only what changed.
pub(crate) fn incremental_pass(
    context: &PassContext<'_>,
    previous: &KeptPass,
    files: Vec<PathBuf>,
    overlay: &Overlay,
    disk: &EncodedFileSystem,
) -> Refreshed {
    let mut kept = previous.start(&files);
    let changed = kept.changed_inputs(overlay, disk);
    let again = kept.to_analyse(&files, &changed);
    if again.is_empty() && files == previous.files {
        kept.files = files;
        kept.graph = previous.graph.clone();
        return Refreshed::Same(Arc::new(kept));
    }
    context.progress.set_total(again.len());
    let (analysed, scans, sink) = if previous.scanned {
        let Some((producers, scans)) = known_producers(context, previous, &files, &again) else {
            return Refreshed::Full;
        };
        let analysed = context.analyse(&again, &Producers::Known(&producers));
        (analysed, Some(scans), None)
    } else {
        // No analysis of the earlier pass read the map, so no kept part
        // depends on it.
        let (producers, sink) = context.lazy_producers(&files);
        let analysed = context.analyse(&again, &Producers::Lazy(&producers));
        (analysed, None, Some(sink))
    };
    kept.clear_moved(&changed);
    let seen = context.fs.inner().take();
    kept.install(&again, analysed, scans, &seen);
    kept.files = files;
    if let Some(sink) = sink {
        kept.take_lazy_scans(&sink, &seen);
    }
    let analysed_again: HashSet<&PathBuf> = again.iter().collect();
    let removed: Vec<&FilePart> = previous
        .entries
        .iter()
        .filter(|(path, _)| !kept.entries.contains_key(*path) || analysed_again.contains(path))
        .map(|(_, entry)| &entry.file.part)
        .collect();
    let base = previous.graph.as_deref().map(|graph| JoinBase {
        graph,
        removed,
        again: &again,
    });
    Refreshed::Built(kept.finish(context.config, again.len(), base))
}

/// The map of `SHARED` producers over `files` after a scan of `again`, with
/// the scans; `None` when it is not the map of `previous`.
fn known_producers(
    context: &PassContext<'_>,
    previous: &KeptPass,
    files: &[PathBuf],
    again: &[PathBuf],
) -> Option<(SharedProducers, Scans)> {
    let scans = context.scan_shared(again);
    let mut names: HashMap<&PathBuf, &[IndexName]> = HashMap::new();
    for path in files {
        if let Some(shared) = previous.entries.get(path).and_then(|e| e.shared.as_deref()) {
            names.insert(path, shared);
        }
    }
    for (path, (scanned, _)) in again.iter().zip(&scans) {
        names.insert(path, scanned);
    }
    let shared_of = |path: &PathBuf| previous.entries[path].shared.as_deref().unwrap_or(&[]);
    let before = producer_map(previous.files.iter().map(|path| (path, shared_of(path))));
    let after = producer_map(files.iter().map(|path| (path, names[path])));
    if before != after {
        return None;
    }
    let producers = SharedProducers::from_definitions(
        files
            .iter()
            .map(|path| (path.clone(), names[path].to_vec())),
    );
    Some((producers, scans))
}

/// Each `SHARED` name with the files that produce it.
fn producer_map<'p>(
    files: impl Iterator<Item = (&'p PathBuf, &'p [IndexName])>,
) -> BTreeMap<String, Vec<&'p PathBuf>> {
    let mut map: BTreeMap<String, Vec<&PathBuf>> = BTreeMap::new();
    for (path, names) in files {
        for name in names {
            map.entry(name.as_str().to_string()).or_default().push(path);
        }
    }
    for paths in map.values_mut() {
        paths.sort();
        paths.dedup();
    }
    map
}

pub(crate) fn file_symbols(files: &[PathBuf]) -> Vec<SymbolRow> {
    use oxabl_daemon_protocol::{Subject, SymbolKind};
    files
        .iter()
        .map(|path| SymbolRow {
            id: format!("file:{}", path.display()),
            name: path
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .into_owned(),
            kind: SymbolKind::File,
            file: Some(path.to_string_lossy().into_owned()),
            span: None,
            subject: Subject::File {
                path: path.to_string_lossy().into_owned(),
            },
        })
        .collect()
}

pub(crate) fn table_symbols(config: &PipelineConfig) -> Vec<SymbolRow> {
    use oxabl_daemon_protocol::{Subject, SymbolKind};
    config
        .schema
        .tables()
        .map(|(_, table)| {
            let name = table.name.to_string();
            SymbolRow {
                id: format!("table:{name}"),
                name: name.clone(),
                kind: SymbolKind::Table,
                file: None,
                span: None,
                subject: Subject::Table { name },
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::hash::Hasher;

    // The digest does not depend on how the bytes are cut into writes: small
    // writes, a write over a block, and a write that ends on a block edge.
    #[test]
    fn the_buffered_hash_is_the_hash_of_the_bytes() {
        let bytes: Vec<u8> = (0..200_000u32).map(|n| (n % 251) as u8).collect();
        let expected = blake3::hash(&bytes);
        let cuts: [&[usize]; 4] = [
            &[1],
            &[7, 100, 5_000],
            &[BufferedBlake3::BLOCK - 1, 1, BufferedBlake3::BLOCK, 60_000],
            &[BufferedBlake3::BLOCK + 1, 3],
        ];
        for sizes in cuts {
            let mut hashing = BufferedBlake3::new();
            let mut rest = bytes.as_slice();
            for &size in sizes.iter().cycle() {
                if rest.is_empty() {
                    break;
                }
                let (now, later) = rest.split_at(size.min(rest.len()));
                hashing.write(now);
                rest = later;
            }
            assert_eq!(hashing.digest(), expected, "cuts {sizes:?}");
        }
    }
}
