//! How the class names a unit reaches, and the includes of the class sources
//! the compiler reads, resolve: on the unit's own PROPATH and under every
//! PROPATH id of the build.
//!
//! The search is the one measured on the compiler: `$OUT` comes first and
//! holds the r-code of every class unit that compiles in this build; after
//! it, the first entry that holds `<pkg>/<Name>.r` or `<pkg>/<Name>.cls`,
//! exact in case, r-code before source. An include is the first entry that
//! holds the name as written, exact in case.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Component, Path};
use std::rc::Rc;

use oxabl_index::search::normalize_lexically;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::paths::PathMap;
use crate::paths::propath::{OUT, library_name};
use crate::refresh::Outcome;

/// A condition a named edge fails.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Failed {
    /// Another PROPATH id resolves the name to another file, or a library
    /// entry comes before the file.
    U1,
    /// The unit's own PROPATH does not resolve the name to the analysed file.
    U3,
    /// The unit's own compile binds class r-code that is not in the key.
    Rcode,
    /// The written name is unknown, or does not match the analysed file.
    Name,
}

impl Failed {
    pub fn as_str(self) -> &'static str {
        match self {
            Failed::U1 => "U1",
            Failed::U3 => "U3",
            Failed::Rcode => "rcode",
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
    /// With [`Failed::Rcode`]: the class r-code the compile binds.
    pub rcode: Option<String>,
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

impl ClassResolution {
    /// The K11 reasons: the class r-code the unit's compile binds that the
    /// key does not hold.
    pub fn rcode_reasons(&self) -> Vec<String> {
        self.differing
            .iter()
            .filter_map(|edge| edge.rcode.as_ref())
            .map(|file| format!("class-rcode: {file}"))
            .collect()
    }

    /// The K9 reasons: one per name that does not resolve the same under
    /// every PROPATH id, or not to the analysed file on the unit's own.
    pub fn propath_reasons(&self) -> Vec<String> {
        self.differing
            .iter()
            .filter(|edge| edge.failed.iter().any(|f| *f != Failed::Rcode))
            .map(|edge| {
                let name = match edge.kind {
                    "include" => format!("{{{}}} in {}", edge.name, edge.via),
                    _ => edge.name.clone(),
                };
                format!(
                    "per-unit-propath-classes: {name} ({})",
                    edge.files.join(", ")
                )
            })
            .collect()
    }
}

/// The counts over the units that have a [`ClassResolution`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ResolutionSummary {
    pub units: usize,
    pub uniform: usize,
    /// Units that have a key.
    pub released: usize,
}

/// What the resolver knows about the units of the build.
pub struct Units<'a> {
    /// The PROPATH id of each unit that compiles.
    pub propath: FxHashMap<u32, usize>,
    /// The closure of each unit that compiles.
    pub closure: FxHashMap<u32, &'a [u32]>,
    /// The class units that compile, by the stem of their r-code below
    /// `$OUT` (`pkg/Foo`).
    pub out: FxHashMap<String, u32>,
    /// The ids of `out`.
    pub compiles: FxHashSet<u32>,
    /// For each class unit that compiles and is in a compile-time cycle, the
    /// nodes of its closure.
    pub cycles: FxHashMap<u32, FxHashSet<u32>>,
    pub class_sources: &'a FxHashSet<u32>,
}

impl Units<'_> {
    /// Whether `a` and the class unit `b` are in one compile-time cycle: one
    /// job compiles them, in no fixed order.
    fn one_cycle(&self, a: u32, b: u32) -> bool {
        self.cycles.get(&b).is_some_and(|nodes| nodes.contains(&a))
    }

    /// Whether the compile of `unit` reads the class `class` from `$OUT`:
    /// `class` is a class unit that compiles before `unit`.
    pub fn binds_out(&self, unit: u32, class: u32) -> bool {
        self.compiles.contains(&class) && !self.one_cycle(unit, class)
    }
}

/// What one PROPATH id finds for a name.
#[derive(Debug, Default)]
struct Hit {
    /// The first file that matches the name exactly.
    file: Option<String>,
    /// A library entry before `file`: it may hold the name.
    library: Option<String>,
}

/// The name an edge was written with, as a path below a PROPATH entry.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Rel {
    /// A relative path; `exact_only` when it leaves its entry.
    Path { rel: String, exact_only: bool },
    /// An absolute name: no PROPATH search.
    Fixed,
    /// The name cannot be turned into a path.
    Unknown,
}

/// Which edges of a source count, and how.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Read {
    /// The unit's own text: its class edges, and its include edges when it is
    /// a class, on its own PROPATH.
    Own,
    /// A class the unit's compile reads from source: its class edges, and its
    /// include edges on every PROPATH id.
    Source,
}

const NO_CYCLE: u32 = u32::MAX;

/// Emulates the PROPATH search over the working tree, with every directory
/// listing and every answer kept for the rest of the plan.
pub struct Resolver<'a> {
    paths: &'a PathMap,
    propaths: &'a [Vec<String>],
    units: Units<'a>,
    listings: FxHashMap<std::path::PathBuf, Option<Rc<FxHashSet<String>>>>,
    exists: FxHashMap<(String, String), Option<String>>,
    hits: FxHashMap<(usize, Vec<String>), Rc<Hit>>,
    by_source: FxHashMap<(u32, usize, Read, u32), Rc<Vec<Differing>>>,
    by_unit: FxHashMap<u32, Rc<ClassResolution>>,
    misread: FxHashMap<(u32, usize, u32), Rc<Vec<Misread>>>,
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

/// The path stem of the class `name` below a package root, spelled as the
/// analysed file `target` spells it (`pkg/Foo`), and the name with that
/// spelling. `None` when the file does not end with the name's package path.
fn class_stem(name: &str, target: &str) -> Option<(String, String)> {
    let (stem, _) = target.rsplit_once('.')?;
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
    same.then(|| (tail.join("/"), tail.join(".")))
}

fn sorted(files: BTreeSet<String>) -> Vec<String> {
    files.into_iter().collect()
}

impl<'a> Resolver<'a> {
    pub fn new(paths: &'a PathMap, propaths: &'a [Vec<String>], units: Units<'a>) -> Self {
        Resolver {
            paths,
            propaths,
            units,
            listings: FxHashMap::default(),
            exists: FxHashMap::default(),
            hits: FxHashMap::default(),
            by_source: FxHashMap::default(),
            by_unit: FxHashMap::default(),
            misread: FxHashMap::default(),
        }
    }

    pub fn units(&self) -> &Units<'a> {
        &self.units
    }

    fn several(&self) -> bool {
        self.propaths.len() > 1
    }

    fn listing(&mut self, dir: &Path) -> Option<Rc<FxHashSet<String>>> {
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

    /// The canonical path of the file `rel` below `entry`, when the entry
    /// holds it with exactly that spelling.
    fn exists(&mut self, entry: &str, rel: &str, exact_only: bool) -> Option<String> {
        let key = (entry.to_string(), rel.to_string());
        if let Some(found) = self.exists.get(&key) {
            return found.clone();
        }
        let found = if exact_only {
            let joined = normalize_lexically(&Path::new(entry).join(rel));
            let canonical = joined.to_string_lossy().into_owned();
            self.paths
                .absolute(&canonical)
                .is_file()
                .then_some(canonical)
        } else {
            let mut dir = self.paths.absolute(entry);
            let mut whole = true;
            for part in rel.split('/') {
                let held = self
                    .listing(&dir)
                    .is_some_and(|listing| listing.contains(part));
                if !held {
                    whole = false;
                    break;
                }
                dir = dir.join(part);
            }
            (whole && dir.is_file()).then(|| join(entry, rel))
        };
        self.exists.insert(key, found.clone());
        found
    }

    /// What PROPATH id `propath` finds first, after `$OUT`, for any of
    /// `names`: the first entry that holds one, the earlier name first.
    fn hit(&mut self, propath: usize, names: &[String], exact_only: bool) -> Rc<Hit> {
        let key = (propath, names.to_vec());
        if let Some(hit) = self.hits.get(&key) {
            return hit.clone();
        }
        let mut hit = Hit::default();
        let entries: Vec<String> = self.propaths[propath]
            .iter()
            .filter(|entry| *entry != OUT)
            .cloned()
            .collect();
        'entries: for entry in &entries {
            if library_name(entry).is_some() {
                hit.library.get_or_insert_with(|| entry.clone());
                continue;
            }
            for name in names {
                if let Some(file) = self.exists(entry, name, exact_only) {
                    hit.file = Some(file);
                    break 'entries;
                }
            }
        }
        if hit.file.is_none() {
            hit.library = None;
        }
        let hit = Rc::new(hit);
        self.hits.insert(key, hit.clone());
        hit
    }

    fn class_hit(&mut self, propath: usize, stem: &str) -> Rc<Hit> {
        self.hit(
            propath,
            &[format!("{stem}.r"), format!("{stem}.cls")],
            false,
        )
    }

    /// The class r-code that PROPATH id `propath` finds before the class
    /// source `class`, under any class name the path can have (`Foo`,
    /// `pkg.Foo`, ...): the package is not known for a class only listed.
    pub fn rcode_before(&mut self, propath: usize, class: &str) -> Option<String> {
        let stem = class.strip_suffix(".cls")?;
        let parts: Vec<&str> = stem.split('/').collect();
        (0..parts.len()).rev().find_map(|from| {
            let hit = self.class_hit(propath, &parts[from..].join("/"));
            hit.file.clone().filter(|file| file.ends_with(".r"))
        })
    }

    /// The verdict on a class edge to `target` that the compile of `unit`
    /// binds, on PROPATH id `propath`.
    fn class_edge(
        &mut self,
        outcome: &Outcome,
        (unit, propath): (u32, usize),
        (name, via): (Option<&str>, u32),
        target: u32,
    ) -> Option<Differing> {
        let target_path = outcome.name(target);
        let Some((stem, shown)) = name.and_then(|name| class_stem(name, target_path)) else {
            return self.several().then(|| Differing {
                kind: "class",
                name: name.unwrap_or_default().to_string(),
                via: outcome.name(via).to_string(),
                files: vec![target_path.to_string()],
                failed: vec![Failed::Name],
                rcode: None,
            });
        };
        let mut failed = BTreeSet::new();
        let mut files = BTreeSet::new();
        let mut rcode = None;
        match self.units.out.get(&stem).copied() {
            Some(class) if !self.units.one_cycle(unit, class) => {
                if class != target {
                    failed.insert(Failed::U1);
                    files.insert(outcome.name(class).to_string());
                }
            }
            _ => {
                let own = self.class_hit(propath, &stem);
                match own.file.as_deref() {
                    Some(file) if file.ends_with(".r") => {
                        failed.insert(Failed::Rcode);
                        files.insert(file.to_string());
                        rcode = Some(file.to_string());
                    }
                    found if self.several() && found != Some(target_path) => {
                        failed.insert(Failed::U3);
                        files.extend(found.map(str::to_string));
                    }
                    _ => {}
                }
                if self.several() {
                    for other in 0..self.propaths.len() {
                        let hit = self.class_hit(other, &stem);
                        if let Some(file) = hit.file.as_deref()
                            && file != target_path
                            && Some(file) != rcode.as_deref()
                        {
                            failed.insert(Failed::U1);
                            files.insert(file.to_string());
                        }
                        if let Some(library) = &hit.library {
                            failed.insert(Failed::U1);
                            files.insert(library.clone());
                        }
                    }
                }
            }
        }
        if failed.is_empty() {
            return None;
        }
        files.insert(target_path.to_string());
        Some(Differing {
            kind: "class",
            name: shown,
            via: outcome.name(via).to_string(),
            files: sorted(files),
            failed: failed.into_iter().collect(),
            rcode,
        })
    }

    /// The verdict on an include edge of a class source, read on PROPATH id
    /// `propath`, and with `every` on every PROPATH id too.
    fn include_edge(
        &mut self,
        outcome: &Outcome,
        propath: usize,
        (name, via): (Option<&str>, u32),
        target: u32,
        every: bool,
    ) -> Option<Differing> {
        let target_path = outcome.name(target);
        let rel = name.map_or(Rel::Unknown, include_rel);
        let mut failed = BTreeSet::new();
        let mut files = BTreeSet::new();
        match &rel {
            Rel::Fixed => return None,
            Rel::Unknown => {
                failed.insert(Failed::Name);
            }
            Rel::Path { rel, exact_only } => {
                let names = [rel.clone()];
                let own = self.hit(propath, &names, *exact_only);
                if own.file.as_deref() != Some(target_path) {
                    failed.insert(Failed::U3);
                    files.extend(own.file.clone());
                }
                for other in (0..self.propaths.len()).filter(|_| every) {
                    let hit = self.hit(other, &names, *exact_only);
                    if let Some(file) = hit.file.as_ref().filter(|file| *file != target_path) {
                        failed.insert(Failed::U1);
                        files.insert(file.clone());
                    }
                    if let Some(library) = &hit.library {
                        failed.insert(Failed::U1);
                        files.insert(library.clone());
                    }
                }
            }
        }
        if failed.is_empty() {
            return None;
        }
        files.insert(target_path.to_string());
        Some(Differing {
            kind: "include",
            name: name.unwrap_or_default().to_string(),
            via: outcome.name(via).to_string(),
            files: sorted(files),
            failed: failed.into_iter().collect(),
            rcode: None,
        })
    }

    /// The class and include edges of the class source `source` that PROPATH
    /// id `propath` resolves to another file than the analysis did, for the
    /// compile of `unit`: what that compile reads instead (U3 alone). A class
    /// that `$OUT` gives, or that resolves to r-code (K11), is not one.
    /// `unit` is `None` for a unit that compiles after every class unit.
    pub fn misread(
        &mut self,
        outcome: &Outcome,
        source: u32,
        (unit, propath): (Option<u32>, usize),
    ) -> Rc<Vec<Misread>> {
        let context = unit.map_or(NO_CYCLE, |unit| self.context(unit));
        if let Some(found) = self.misread.get(&(source, propath, context)) {
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
                let name = edge.name.as_deref();
                let (shown, found) = if class {
                    let Some((stem, shown)) = name.and_then(|name| class_stem(name, analysed))
                    else {
                        misread.push(Misread {
                            name: name.unwrap_or_default().to_string(),
                            via: outcome.name(source).to_string(),
                            analysed: analysed.to_string(),
                            found: None,
                        });
                        continue;
                    };
                    if let Some(class) = self.units.out.get(&stem).copied()
                        && !unit.is_some_and(|unit| self.units.one_cycle(unit, class))
                    {
                        continue;
                    }
                    let hit = self.class_hit(propath, &stem);
                    match hit.file.as_deref() {
                        Some(file) if file == analysed || file.ends_with(".r") => continue,
                        found => (shown, found.map(str::to_string)),
                    }
                } else {
                    let found = match name.map_or(Rel::Unknown, include_rel) {
                        Rel::Fixed => continue,
                        Rel::Unknown => None,
                        Rel::Path { rel, exact_only } => {
                            let hit = self.hit(propath, &[rel], exact_only);
                            if hit.file.as_deref() == Some(analysed) {
                                continue;
                            }
                            hit.file.clone()
                        }
                    };
                    (name.unwrap_or_default().to_string(), found)
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
        self.misread
            .insert((source, propath, context), misread.clone());
        misread
    }

    /// The part of `unit` that decides how its compile resolves a class: the
    /// unit itself when it is in a compile-time cycle.
    fn context(&self, unit: u32) -> u32 {
        match self.units.cycles.contains_key(&unit) {
            true => unit,
            false => NO_CYCLE,
        }
    }

    /// The edges of the analysis of `source` that are not uniform for the
    /// compile of `unit` on PROPATH id `propath`.
    fn source(
        &mut self,
        outcome: &Outcome,
        source: u32,
        read: Read,
        (unit, propath): (u32, usize),
    ) -> Rc<Vec<Differing>> {
        let key = (source, propath, read, self.context(unit));
        if let Some(found) = self.by_source.get(&key) {
            return found.clone();
        }
        let is_class = self.units.class_sources.contains(&source);
        let mut differing = Vec::new();
        if let Some(analysis) = outcome.analyses.get(source) {
            for edge in &analysis.edges {
                let named = (edge.name.as_deref(), source);
                if edge.kind.is_class() {
                    differing.extend(self.class_edge(outcome, (unit, propath), named, edge.target));
                } else if edge.kind.is_include() && is_class && self.several() {
                    let every = read == Read::Source;
                    differing.extend(self.include_edge(
                        outcome,
                        propath,
                        named,
                        edge.target,
                        every,
                    ));
                }
            }
        }
        let differing = Rc::new(differing);
        self.by_source.insert(key, differing.clone());
        differing
    }

    /// The resolution of the unit `unit`, which compiles: the class edges of
    /// the unit and of every class source in its closure, and the include
    /// edges of each class whose source its compile reads. A class unit that
    /// it reads from `$OUT` adds its own resolution instead: the compile of
    /// that class read its names.
    pub fn unit(&mut self, outcome: &Outcome, unit: u32) -> Rc<ClassResolution> {
        if let Some(found) = self.by_unit.get(&unit) {
            return found.clone();
        }
        let Some(propath) = self.units.propath.get(&unit).copied() else {
            return Rc::new(ClassResolution {
                uniform: true,
                differing: Vec::new(),
            });
        };
        let closure: Vec<u32> = self
            .units
            .closure
            .get(&unit)
            .map(|closure| closure.to_vec())
            .unwrap_or_default();
        let mut differing: BTreeMap<(String, &'static str, String), Differing> = BTreeMap::new();
        let mut add = |found: &Differing| {
            differing
                .entry((found.via.clone(), found.kind, found.name.clone()))
                .or_insert_with(|| found.clone());
        };
        for found in self
            .source(outcome, unit, Read::Own, (unit, propath))
            .iter()
        {
            add(found);
        }
        for source in closure
            .iter()
            .copied()
            .filter(|node| self.units.class_sources.contains(node))
        {
            if self.units.binds_out(unit, source) {
                for found in &self.unit(outcome, source).differing {
                    add(found);
                }
            } else {
                for found in self
                    .source(outcome, source, Read::Source, (unit, propath))
                    .iter()
                {
                    add(found);
                }
            }
        }
        let differing: Vec<Differing> = differing.into_values().collect();
        let resolution = Rc::new(ClassResolution {
            uniform: differing.is_empty(),
            differing,
        });
        self.by_unit.insert(unit, resolution.clone());
        resolution
    }
}
