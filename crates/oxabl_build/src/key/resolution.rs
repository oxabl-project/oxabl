//! How each class name in a unit's closure, and each include inside a class
//! subtree, resolves under every PROPATH id of the build. The uniform
//! resolution is reported only; [`Resolver::misread`] gives K10.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Component, Path};
use std::rc::Rc;

use oxabl_index::search::normalize_lexically;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::paths::PathMap;
use crate::paths::propath::{OUT, library_name};
use crate::refresh::Outcome;

/// A condition a named edge fails. U1 to U3 are the conditions of a uniform
/// edge; `case` and `name` fail closed until the AVM's search is measured.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Failed {
    /// Another PROPATH id resolves the name to another file, or a library
    /// entry comes before the file.
    U1,
    /// A workspace entry holds r-code for the class name.
    U2,
    /// The unit's own PROPATH does not resolve the name to the analysed file.
    U3,
    /// A file matches the name only when case is ignored.
    Case,
    /// The written name is unknown, or does not match the analysed file.
    Name,
}

impl Failed {
    pub fn as_str(self) -> &'static str {
        match self {
            Failed::U1 => "U1",
            Failed::U2 => "U2",
            Failed::U3 => "U3",
            Failed::Case => "case",
            Failed::Name => "name",
        }
    }
}

/// One named edge that is not uniform.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Differing {
    /// `class` or `include`.
    pub kind: &'static str,
    /// The class name, or the include name as written.
    pub name: String,
    /// The file that writes the name.
    pub via: String,
    /// The analysed file and every other file the name can resolve to.
    pub files: Vec<String>,
    pub failed: Vec<Failed>,
}

/// A named edge of a class source that a compile searching another unit's
/// PROPATH does not resolve to the file the analysis did.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Misread {
    /// The class name, or the include name as written; empty when unknown.
    pub name: String,
    /// The class source that writes the name.
    pub via: String,
    /// The file the analysis resolved the name to.
    pub analysed: String,
    /// The file the PROPATH finds first; `None` when it finds none, or
    /// when the name cannot be searched.
    pub found: Option<String>,
}

impl Misread {
    /// The K10 reason.
    pub fn reason(&self) -> String {
        let name = match self.name.is_empty() {
            true => "<unknown name>",
            false => self.name.as_str(),
        };
        let found = self.found.as_deref().unwrap_or("nothing");
        format!(
            "class-propath: {name} in {} finds {found}, analysed {}",
            self.via, self.analysed
        )
    }
}

/// Whether every named edge a unit reaches is uniform, and the ones that are
/// not.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ClassResolution {
    pub uniform: bool,
    pub differing: Vec<Differing>,
}

/// The counts over the units that have a [`ClassResolution`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ResolutionSummary {
    pub units: usize,
    pub uniform: usize,
    /// Uniform units whose only reason is K9.
    pub released: usize,
}

/// What one PROPATH id finds for a name.
#[derive(Debug, Default)]
struct Hit {
    /// The first file that matches the name exactly.
    file: Option<String>,
    /// Files that match only when case is ignored, in the entries up to the
    /// one that holds `file`.
    variants: Vec<String>,
    /// A library entry before `file`: it may hold the name.
    library: Option<String>,
}

/// What every PROPATH id says about one name and its analysed file.
#[derive(Debug, Default)]
struct Global {
    files: BTreeSet<String>,
    failed: BTreeSet<Failed>,
}

/// The name an edge was written with, as a path below a PROPATH entry.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Rel {
    /// A relative path; `exact_only` when it leaves its entry, so that only an
    /// exact match is looked for.
    Path { rel: String, exact_only: bool },
    /// An absolute name: no PROPATH search.
    Fixed,
    /// The name cannot be turned into a path.
    Unknown,
}

/// Emulates the PROPATH search over the working tree, with every directory
/// listing and every answer kept for the rest of the plan.
pub struct Resolver<'a> {
    paths: &'a PathMap,
    propaths: &'a [Vec<String>],
    /// The workspace entries of every PROPATH id, `$OUT` excluded.
    workspace_entries: Vec<String>,
    listings: FxHashMap<std::path::PathBuf, Option<Rc<Vec<String>>>>,
    found: FxHashMap<(String, String, bool), Rc<Vec<String>>>,
    hits: FxHashMap<(usize, String, bool), Rc<Hit>>,
    globals: FxHashMap<(bool, Rel, u32), Rc<Global>>,
    by_source: FxHashMap<(u32, usize), Rc<Vec<Differing>>>,
    misread: FxHashMap<(u32, usize), Rc<Vec<Misread>>>,
}

fn is_workspace_entry(entry: &str) -> bool {
    !entry.starts_with(['$', '/'])
}

fn join(entry: &str, rel: &str) -> String {
    match entry {
        "." => rel.to_string(),
        entry => format!("{entry}/{rel}"),
    }
}

fn include_rel(name: &str) -> Rel {
    let name = name.replace('\\', "/");
    if name.is_empty() {
        return Rel::Unknown;
    }
    if name.starts_with('/') || name.contains(':') {
        return Rel::Fixed;
    }
    let normal = normalize_lexically(Path::new(&name));
    let exact_only = matches!(normal.components().next(), Some(Component::ParentDir));
    let rel = normal.to_string_lossy().into_owned();
    match rel.is_empty() {
        true => Rel::Unknown,
        false => Rel::Path { rel, exact_only },
    }
}

/// The path of the class `name` below a package root, spelled as the analysed
/// file `target` spells it, and the name with that spelling. `None` when the
/// file does not end with the name's package path.
fn class_rel(name: &str, target: &str) -> Option<(String, String)> {
    let (stem, extension) = target.rsplit_once('.')?;
    let parts: Vec<&str> = name.split('.').collect();
    let components: Vec<&str> = stem.split('/').collect();
    if components.len() < parts.len() {
        return None;
    }
    let tail = &components[components.len() - parts.len()..];
    let same = tail
        .iter()
        .zip(&parts)
        .all(|(component, part)| component.eq_ignore_ascii_case(part));
    same.then(|| (format!("{}.{extension}", tail.join("/")), tail.join(".")))
}

/// The name of an edge as a path below a PROPATH entry, and the name to show.
fn named_rel(class: bool, name: Option<&str>, target: &str) -> (Rel, String) {
    match (class, name) {
        (_, None) => (Rel::Unknown, String::new()),
        (true, Some(name)) => match class_rel(name, target) {
            Some((rel, spelled)) => (
                Rel::Path {
                    rel,
                    exact_only: false,
                },
                spelled,
            ),
            None => (Rel::Unknown, name.to_string()),
        },
        (false, Some(name)) => (include_rel(name), name.to_string()),
    }
}

impl<'a> Resolver<'a> {
    pub fn new(paths: &'a PathMap, propaths: &'a [Vec<String>]) -> Self {
        let mut workspace_entries: Vec<String> = Vec::new();
        for entry in propaths.iter().flatten() {
            if is_workspace_entry(entry) && !workspace_entries.contains(entry) {
                workspace_entries.push(entry.clone());
            }
        }
        Resolver {
            paths,
            propaths,
            workspace_entries,
            listings: FxHashMap::default(),
            found: FxHashMap::default(),
            hits: FxHashMap::default(),
            globals: FxHashMap::default(),
            by_source: FxHashMap::default(),
            misread: FxHashMap::default(),
        }
    }

    fn listing(&mut self, dir: &Path) -> Option<Rc<Vec<String>>> {
        if let Some(listing) = self.listings.get(dir) {
            return listing.clone();
        }
        let listing = std::fs::read_dir(dir).ok().map(|entries| {
            Rc::new(
                entries
                    .filter_map(Result::ok)
                    .map(|entry| entry.file_name().to_string_lossy().into_owned())
                    .collect(),
            )
        });
        self.listings.insert(dir.to_path_buf(), listing.clone());
        listing
    }

    /// The files below `entry` whose path matches `rel` when case is ignored,
    /// as canonical paths; only the exact path with `exact_only`.
    fn find(&mut self, entry: &str, rel: &str, exact_only: bool) -> Rc<Vec<String>> {
        let key = (entry.to_string(), rel.to_string(), exact_only);
        if let Some(found) = self.found.get(&key) {
            return found.clone();
        }
        let mut found = Vec::new();
        if exact_only {
            let joined = normalize_lexically(&Path::new(entry).join(rel));
            let canonical = joined.to_string_lossy().into_owned();
            if self.paths.absolute(&canonical).is_file() {
                found.push(canonical);
            }
        } else {
            let mut level: Vec<(std::path::PathBuf, String)> =
                vec![(self.paths.absolute(entry), entry.to_string())];
            let parts: Vec<&str> = rel.split('/').collect();
            for (at, part) in parts.iter().enumerate() {
                let mut next = Vec::new();
                for (dir, canonical) in &level {
                    let Some(listing) = self.listing(dir) else {
                        continue;
                    };
                    for name in listing
                        .iter()
                        .filter(|name| name.eq_ignore_ascii_case(part))
                    {
                        next.push((dir.join(name), join(canonical, name)));
                    }
                }
                if at + 1 == parts.len() {
                    next.retain(|(path, _)| path.is_file());
                }
                level = next;
            }
            found.extend(level.into_iter().map(|(_, canonical)| canonical));
            found.sort();
        }
        let found = Rc::new(found);
        self.found.insert(key, found.clone());
        found
    }

    /// What PROPATH id `propath` finds for `rel`.
    fn hit(&mut self, propath: usize, rel: &str, exact_only: bool) -> Rc<Hit> {
        let key = (propath, rel.to_string(), exact_only);
        if let Some(hit) = self.hits.get(&key) {
            return hit.clone();
        }
        let mut hit = Hit::default();
        let entries: Vec<String> = self.propaths[propath]
            .iter()
            .filter(|entry| *entry != OUT)
            .cloned()
            .collect();
        for entry in &entries {
            if library_name(entry).is_some() {
                hit.library.get_or_insert_with(|| entry.clone());
                continue;
            }
            let exact = normalize_lexically(Path::new(&join(entry, rel)))
                .to_string_lossy()
                .into_owned();
            let found = self.find(entry, rel, exact_only);
            hit.variants
                .extend(found.iter().filter(|path| **path != exact).cloned());
            if found.contains(&exact) {
                hit.file = Some(exact);
                break;
            }
        }
        if hit.file.is_none() {
            hit.library = None;
        }
        let hit = Rc::new(hit);
        self.hits.insert(key, hit.clone());
        hit
    }

    /// U1, U2 and the case rule for a name whose analysed file is `target`.
    fn global(&mut self, class: bool, rel: &Rel, target: (u32, &str)) -> Rc<Global> {
        let key = (class, rel.clone(), target.0);
        if let Some(global) = self.globals.get(&key) {
            return global.clone();
        }
        let mut global = Global::default();
        match rel {
            Rel::Fixed => {}
            Rel::Unknown => {
                global.failed.insert(Failed::Name);
            }
            Rel::Path { rel, exact_only } => {
                for propath in 0..self.propaths.len() {
                    let hit = self.hit(propath, rel, *exact_only);
                    if let Some(file) = hit.file.as_ref().filter(|file| *file != target.1) {
                        global.failed.insert(Failed::U1);
                        global.files.insert(file.clone());
                    }
                    if let Some(library) = &hit.library {
                        global.failed.insert(Failed::U1);
                        global.files.insert(library.clone());
                    }
                    if !hit.variants.is_empty() {
                        global.failed.insert(Failed::Case);
                        global.files.extend(hit.variants.iter().cloned());
                    }
                }
                if class {
                    let stem = rel.rsplit_once('.').map_or(rel.as_str(), |(stem, _)| stem);
                    let rcode = format!("{stem}.r");
                    for entry in self.workspace_entries.clone() {
                        let found = self.find(&entry, &rcode, *exact_only);
                        if !found.is_empty() {
                            global.failed.insert(Failed::U2);
                            global.files.extend(found.iter().cloned());
                        }
                    }
                }
            }
        }
        if !global.failed.is_empty() {
            global.files.insert(target.1.to_string());
        }
        let global = Rc::new(global);
        self.globals.insert(key, global.clone());
        global
    }

    /// The verdict on one named edge, for a unit on PROPATH id `propath`.
    fn edge(
        &mut self,
        outcome: &Outcome,
        propath: usize,
        (class, name, via): (bool, Option<&str>, u32),
        target: u32,
    ) -> Option<Differing> {
        let target_path = outcome.name(target);
        let (rel, shown) = named_rel(class, name, target_path);
        let global = self.global(class, &rel, (target, target_path));
        let mut failed = global.failed.clone();
        let mut files = global.files.clone();
        if let Rel::Path { rel, exact_only } = &rel {
            let hit = self.hit(propath, rel, *exact_only);
            if hit.file.as_deref() != Some(target_path) {
                failed.insert(Failed::U3);
                files.insert(target_path.to_string());
                files.extend(hit.file.iter().cloned());
            }
        }
        if failed.is_empty() {
            return None;
        }
        Some(Differing {
            kind: if class { "class" } else { "include" },
            name: shown,
            via: outcome.name(via).to_string(),
            files: files.into_iter().collect(),
            failed: failed.into_iter().collect(),
        })
    }

    /// The class and include edges of the class source `source` that PROPATH
    /// id `propath` resolves to another file than the analysis did: what a
    /// compile on that PROPATH reads instead (U3 alone).
    pub fn misread(&mut self, outcome: &Outcome, source: u32, propath: usize) -> Rc<Vec<Misread>> {
        if let Some(found) = self.misread.get(&(source, propath)) {
            return found.clone();
        }
        let mut misread = Vec::new();
        if let Some(analysis) = outcome.analyses.get(source) {
            for edge in &analysis.edges {
                let class = edge.kind.is_class();
                if !(class || edge.kind.is_include()) {
                    continue;
                }
                let analysed = outcome.name(edge.target);
                let (rel, shown) = named_rel(class, edge.name.as_deref(), analysed);
                let found = match &rel {
                    Rel::Fixed => continue,
                    Rel::Unknown => None,
                    Rel::Path { rel, exact_only } => {
                        let hit = self.hit(propath, rel, *exact_only);
                        if hit.file.as_deref() == Some(analysed) {
                            continue;
                        }
                        hit.file.clone()
                    }
                };
                misread.push(Misread {
                    name: shown,
                    via: outcome.name(source).to_string(),
                    analysed: analysed.to_string(),
                    found,
                });
            }
        }
        let misread = Rc::new(misread);
        self.misread.insert((source, propath), misread.clone());
        misread
    }

    /// The edges of the analysis of `source` that are not uniform for a unit
    /// on PROPATH id `propath`: its class edges, and its include edges when
    /// it is a class source.
    fn source(
        &mut self,
        outcome: &Outcome,
        source: u32,
        class_source: bool,
        propath: usize,
    ) -> Rc<Vec<Differing>> {
        if let Some(found) = self.by_source.get(&(source, propath)) {
            return found.clone();
        }
        let mut differing = Vec::new();
        if let Some(analysis) = outcome.analyses.get(source) {
            for edge in &analysis.edges {
                let class = edge.kind.is_class();
                if !(class || (class_source && edge.kind.is_include())) {
                    continue;
                }
                let named = (class, edge.name.as_deref(), source);
                differing.extend(self.edge(outcome, propath, named, edge.target));
            }
        }
        let differing = Rc::new(differing);
        self.by_source.insert((source, propath), differing.clone());
        differing
    }

    /// The resolution of `unit` on PROPATH id `propath`, whose closure is
    /// `closure`. A class unit counts its own name, `own_name`, as an edge.
    pub fn unit(
        &mut self,
        outcome: &Outcome,
        (unit, propath, own_name): (u32, usize, Option<&str>),
        closure: &[u32],
        class_sources: &FxHashSet<u32>,
    ) -> ClassResolution {
        let mut differing: BTreeMap<(String, &'static str, String), Differing> = BTreeMap::new();
        let mut add = |found: Differing| {
            differing
                .entry((found.via.clone(), found.kind, found.name.clone()))
                .or_insert(found);
        };
        if let Some(name) = own_name
            && let Some(found) = self.edge(outcome, propath, (true, Some(name), unit), unit)
        {
            add(found);
        }
        let unit_is_class = class_sources.contains(&unit);
        for found in self.source(outcome, unit, unit_is_class, propath).iter() {
            add(found.clone());
        }
        for source in closure.iter().filter(|node| class_sources.contains(node)) {
            for found in self.source(outcome, *source, true, propath).iter() {
                add(found.clone());
            }
        }
        let differing: Vec<Differing> = differing.into_values().collect();
        ClassResolution {
            uniform: differing.is_empty(),
            differing,
        }
    }
}
