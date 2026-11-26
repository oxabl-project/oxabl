//! The parts of a workspace pass, kept so that the next pass analyses only the
//! files that changed.
//!
//! A pass analyses each file of the workspace once. The part of the reverse
//! graph that a file gives, its symbol rows and the `SHARED` names it produces
//! depend only on the text of the file and on the files that its analysis read
//! or looked for: its include files at any depth and the class and program
//! files whose facts it took from the index. The pass reads through an
//! [`ObservingFileSystem`], so it knows these files for each analysis, and
//! through [`Stamping`], so it knows the content of each file that it read.
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
//! - the configuration and the schema (the session generations, and a
//!   fingerprint of the configuration that the pass resolved);
//! - the map of `SHARED` producers, which every file can read. The scan of a
//!   changed file runs first; when the map is not the same, the pass is full.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use oxabl_daemon_protocol::SymbolRow;
use oxabl_index::SharedProducers;
use oxabl_pipeline::{Expansion, FilePart, LintPipeline, LintResult, PipelineConfig, ReverseGraph};
use oxabl_semantic::IndexName;
use oxabl_workspace::{EncodedFileSystem, FileSystem, Observations, ObservingFileSystem};

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

/// The open buffers over the disk.
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
/// each path.
pub(crate) struct Stamping {
    fs: OverlayFileSystem,
    seen: Mutex<HashMap<PathBuf, ReadStamp>>,
}

impl Stamping {
    pub fn new(fs: OverlayFileSystem) -> Self {
        Stamping {
            fs,
            seen: Mutex::new(HashMap::new()),
        }
    }

    fn seen(&self) -> std::sync::MutexGuard<'_, HashMap<PathBuf, ReadStamp>> {
        self.seen
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }
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
    /// The directory was listed; its stamp then.
    listed: Option<FileStamp>,
}

/// What one file gives to a pass.
pub(crate) struct KeptFile {
    part: FilePart,
    symbols: Vec<SymbolRow>,
    /// The `SHARED` names the file produces.
    shared: Vec<IndexName>,
    /// The inputs of its analysis and of its `SHARED` scan, as ids of
    /// [`KeptPass::paths`].
    inputs: Box<[u32]>,
}

/// The parts of a completed pass, and what each part depends on.
pub(crate) struct KeptPass {
    /// The configuration the pass resolved; see [`config_fingerprint`].
    fingerprint: blake3::Hash,
    /// Every path an analysis read or looked for, by id. Paths are kept as
    /// the analysis spelled them, as the buffers are matched by that spelling.
    paths: Vec<PathBuf>,
    ids: HashMap<PathBuf, u32>,
    inputs: Vec<Input>,
    /// The discovered files, in the order of discovery.
    files: Vec<PathBuf>,
    entries: HashMap<PathBuf, Arc<KeptFile>>,
}

/// A completed pass: the graph and the symbol rows, and the parts to keep.
pub(crate) struct Built {
    pub graph: Arc<ReverseGraph>,
    pub symbols: Arc<Vec<SymbolRow>>,
    pub kept: Arc<KeptPass>,
    /// How many files the pass analysed.
    pub analysed: usize,
}

/// A fingerprint of everything in `config` that an analysis reads. Two
/// configurations with the same fingerprint give the same parts.
pub(crate) fn config_fingerprint(config: &PipelineConfig) -> blake3::Hash {
    struct Hashing(blake3::Hasher);
    impl std::fmt::Write for Hashing {
        fn write_str(&mut self, s: &str) -> std::fmt::Result {
            self.0.update(s.as_bytes());
            Ok(())
        }
    }
    let mut hashing = Hashing(blake3::Hasher::new());
    let _ = std::fmt::Write::write_fmt(&mut hashing, format_args!("{config:?}"));
    hashing.0.finalize()
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

impl PassContext<'_> {
    /// The `SHARED` names each of `files` produces, with what each scan
    /// observed. The same answer as [`SharedProducers::build_parallel`].
    fn scan_shared(&self, files: &[PathBuf]) -> Vec<(Vec<IndexName>, Observations)> {
        let run = LintPipeline::new(self.config, self.fs);
        let includes = run.index_includes();
        parallel(files, self.jobs, |path| {
            let mut names = Vec::new();
            let observed = self.fs.observe(&mut || {
                names = oxabl_index::definitions_in(path, &includes);
            });
            (
                names,
                observed.map(Arc::unwrap_or_clone).unwrap_or_default(),
            )
        })
    }

    /// Analyse `files` with the producers of `producers`.
    fn analyse(
        &self,
        files: &[PathBuf],
        producers: &SharedProducers,
    ) -> Vec<(FilePart, Vec<SymbolRow>, Observations)> {
        let fs: &dyn FileSystem = self.fs;
        ReverseGraph::analyse_parts(
            files,
            self.jobs,
            || LintPipeline::new(self.config, self.fs).with_shared_producers(producers),
            |path, expansion, result| {
                let rows = (self.collect)(path, expansion, result, fs);
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
    let scanned = context.scan_shared(&files);
    let producers = SharedProducers::from_definitions(
        files
            .iter()
            .zip(&scanned)
            .map(|(path, (names, _))| (path.clone(), names.clone())),
    );
    let analysed = context.analyse(&files, &producers);
    let mut kept = KeptPass {
        fingerprint: config_fingerprint(context.config),
        paths: Vec::new(),
        ids: HashMap::new(),
        inputs: Vec::new(),
        files: Vec::new(),
        entries: HashMap::new(),
    };
    let seen = std::mem::take(&mut *context.fs.inner().seen());
    for (path, ((part, symbols, observed), (shared, scan_observed))) in
        files.iter().zip(analysed.into_iter().zip(scanned))
    {
        let inputs = kept.absorb(path, [&observed, &scan_observed], &seen);
        kept.entries.insert(
            path.clone(),
            Arc::new(KeptFile {
                part,
                symbols,
                shared,
                inputs,
            }),
        );
    }
    let analysed = files.len();
    kept.files = files;
    kept.finish(context.config, analysed)
}

impl KeptPass {
    /// Whether a pass over `config` can start from this one.
    pub fn same_config(&self, config: &PipelineConfig) -> bool {
        self.fingerprint == config_fingerprint(config)
    }

    /// Estimated heap bytes of the kept parts and inputs.
    pub fn estimated_heap_bytes(&self) -> usize {
        let path = |p: &PathBuf| std::mem::size_of::<PathBuf>() + p.as_os_str().len();
        self.paths.iter().map(path).sum::<usize>() * 2
            + self.inputs.capacity() * std::mem::size_of::<Input>()
            + self.files.iter().map(path).sum::<usize>()
            + self
                .entries
                .iter()
                .map(|(file, kept)| {
                    path(file)
                        + std::mem::size_of::<KeptFile>()
                        + kept.part.estimated_heap_bytes()
                        + kept.inputs.len() * 4
                        + kept.shared.len() * std::mem::size_of::<IndexName>()
                        + kept
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

    /// The id of `path`, added when it is new.
    fn id(&mut self, path: &Path) -> u32 {
        if let Some(&id) = self.ids.get(path) {
            return id;
        }
        let id = self.paths.len() as u32;
        self.paths.push(path.to_path_buf());
        self.ids.insert(path.to_path_buf(), id);
        self.inputs.push(Input::default());
        id
    }

    /// Record what the analysis of `file` observed, and give its input ids.
    ///
    /// Records are only added to: a text hash that is kept stays, so a path
    /// that two analyses read differently (a write during the pass) keeps the
    /// first hash, and the next pass analyses every file that read it again.
    /// The caller clears the record of a path that changed before this pass.
    fn absorb<'o>(
        &mut self,
        file: &Path,
        observed: impl IntoIterator<Item = &'o Observations>,
        seen: &HashMap<PathBuf, ReadStamp>,
    ) -> Box<[u32]> {
        let mut ids = Vec::new();
        let read = |kept: &mut KeptPass, ids: &mut Vec<u32>, path: &Path| {
            let id = kept.id(path);
            ids.push(id);
            let input = &mut kept.inputs[id as usize];
            match (&input.read, seen.get(path)) {
                (Some(Some(_)), _) => {}
                (_, Some(stamp)) => input.read = Some(Some(stamp.clone())),
                (None, None) => input.read = Some(None),
                (Some(None), None) => {}
            }
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
                self.inputs[id as usize].absent = true;
            }
            for path in observations.listed() {
                let id = self.id(path);
                ids.push(id);
                let input = &mut self.inputs[id as usize];
                if input.listed.is_none() {
                    input.listed = Some(FileStamp::capture(path.clone()));
                }
            }
        }
        ids.sort_unstable();
        ids.dedup();
        ids.into_boxed_slice()
    }

    /// Join the kept parts into the graph and the symbol rows.
    fn finish(self, config: &PipelineConfig, analysed: usize) -> Built {
        let fs = config.file_system();
        let run = LintPipeline::new(config, &fs);
        let graph = ReverseGraph::from_parts(
            &run,
            &self.files,
            self.files.iter().map(|path| &self.entries[path].part),
        );
        let mut symbols = file_symbols(&self.files);
        for path in &self.files {
            symbols.extend(self.entries[path].symbols.iter().cloned());
        }
        symbols.extend(table_symbols(config));
        symbols.sort_by(|left, right| left.id.cmp(&right.id));
        symbols.dedup_by(|left, right| left.id == right.id);
        Built {
            graph: Arc::new(graph),
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
            let Some(&id) = self.ids.get(path) else {
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

    /// The ids whose file is not as the pass read it.
    fn changed_inputs(&mut self, overlay: &Overlay, disk: &EncodedFileSystem) -> Vec<bool> {
        let mut live = vec![false; self.paths.len()];
        for kept in self.entries.values() {
            for &id in kept.inputs.iter() {
                live[id as usize] = true;
            }
        }
        let exists = |path: &Path| overlay.contains_key(path) || disk.exists(path);
        let mut changed = vec![false; self.paths.len()];
        for (id, is_live) in live.into_iter().enumerate() {
            if !is_live {
                continue;
            }
            let path = &self.paths[id];
            let input = &mut self.inputs[id];
            let mut moved = false;
            match &mut input.read {
                Some(Some(stamp)) => {
                    moved = match overlay.get(path) {
                        _ if stamp.hash.is_none() => true,
                        Some(text) => Some(blake3::hash(text.as_bytes())) != stamp.hash,
                        None if stamp.disk.as_ref().is_some_and(|disk| !disk.changed()) => false,
                        None => {
                            let now = FileStamp::capture(path.clone());
                            match disk.read(path) {
                                Ok(text) if Some(blake3::hash(text.as_bytes())) == stamp.hash => {
                                    stamp.disk = Some(now);
                                    false
                                }
                                _ => true,
                            }
                        }
                    }
                }
                Some(None) => moved = !exists(path),
                None => {}
            }
            if input.absent && exists(path) {
                moved = true;
            }
            if input.listed.as_ref().is_some_and(FileStamp::changed) {
                moved = true;
            }
            changed[id] = moved;
        }
        changed
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
    let mut kept = KeptPass {
        fingerprint: previous.fingerprint,
        paths: previous.paths.clone(),
        ids: previous.ids.clone(),
        inputs: previous.inputs.clone(),
        files: Vec::new(),
        entries: HashMap::new(),
    };
    // The entries of the files that are still there; the inputs of a removed
    // file are not checked.
    let current: HashSet<&PathBuf> = files.iter().collect();
    kept.entries = previous
        .entries
        .iter()
        .filter(|(path, _)| current.contains(path))
        .map(|(path, entry)| (path.clone(), Arc::clone(entry)))
        .collect();
    let changed = kept.changed_inputs(overlay, disk);
    let again: Vec<PathBuf> = files
        .iter()
        .filter(|path| {
            kept.entries
                .get(*path)
                .is_none_or(|entry| entry.inputs.iter().any(|&id| changed[id as usize]))
        })
        .cloned()
        .collect();
    if again.is_empty() && files == previous.files {
        kept.files = files;
        return Refreshed::Same(Arc::new(kept));
    }

    let scanned = context.scan_shared(&again);
    let mut shared: HashMap<&PathBuf, &[IndexName]> = kept
        .entries
        .iter()
        .map(|(path, entry)| (path, entry.shared.as_slice()))
        .collect();
    for (path, (names, _)) in again.iter().zip(&scanned) {
        shared.insert(path, names);
    }
    let before = producer_map(
        previous
            .files
            .iter()
            .map(|path| (path, previous.entries[path].shared.as_slice())),
    );
    let after = producer_map(files.iter().map(|path| (path, shared[path])));
    if before != after {
        return Refreshed::Full;
    }
    let producers = SharedProducers::from_definitions(
        files
            .iter()
            .map(|path| (path.clone(), shared[path].to_vec())),
    );
    context.progress.set_total(again.len());
    let analysed = context.analyse(&again, &producers);

    // A path that changed is recorded again from this pass's reads.
    for (id, moved) in changed.iter().enumerate() {
        if *moved {
            kept.inputs[id] = Input::default();
        }
    }
    let seen = std::mem::take(&mut *context.fs.inner().seen());
    for (path, ((part, symbols, observed), (shared, scan_observed))) in
        again.iter().zip(analysed.into_iter().zip(scanned))
    {
        let inputs = kept.absorb(path, [&observed, &scan_observed], &seen);
        kept.entries.insert(
            path.clone(),
            Arc::new(KeptFile {
                part,
                symbols,
                shared,
                inputs,
            }),
        );
    }
    kept.files = files;
    Refreshed::Built(kept.finish(context.config, again.len()))
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
