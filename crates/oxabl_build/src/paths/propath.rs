//! The unit PROPATH, the compile name and the places a unit cannot compile at.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use oxabl_index::search::normalize_lexically;

use super::PathMap;

/// The output directory, as a PROPATH entry.
pub const OUT: &str = "$OUT";
const DLC: &str = "$DLC";
const LIBRARY_PREFIX: &str = "$LIB:";

/// The `$LIB:<name>` of a PROPATH entry, if it is one.
pub fn library_name(entry: &str) -> Option<&str> {
    entry.strip_prefix(LIBRARY_PREFIX)
}

/// Whether the canonical `entry` is a directory under the DLC source
/// directory or the DLC directory itself.
pub fn is_dlc(entry: &str) -> bool {
    entry == DLC || entry.starts_with("$DLC/")
}

/// The canonical form of one `include_paths` entry: root-relative, `$DLC/...`
/// or `$LIB:<name>`. An entry with no portable form is an error.
pub fn canonical_entry(
    paths: &PathMap,
    libraries: &BTreeMap<String, String>,
    entry: &Path,
) -> Result<String, String> {
    let text = entry.to_string_lossy();
    if let Some(name) = text.strip_prefix(LIBRARY_PREFIX) {
        return match libraries.contains_key(name) {
            true => Ok(text.into_owned()),
            false => Err(format!("`{text}` names no [build.libraries] entry")),
        };
    }
    if let Some(rest) = text.strip_prefix(DLC)
        && (rest.is_empty() || rest.starts_with('/'))
    {
        let below = normalize_lexically(Path::new(rest.trim_start_matches('/')));
        if below.starts_with("..") {
            return Err(format!("`{text}` leaves $DLC"));
        }
        let below = below.to_string_lossy();
        return Ok(match below.is_empty() {
            true => DLC.to_string(),
            false => format!("{DLC}/{below}"),
        });
    }
    let canonical = paths.canonical(&paths.root().join(entry));
    match canonical.starts_with('/') {
        true => Err(format!(
            "`{text}` is not under the workspace root or the DLC sources, so it has no portable form"
        )),
        false => Ok(canonical),
    }
}

/// The PROPATH entries that do not depend on the unit, in search order.
pub struct Search {
    root_relative: Vec<PathBuf>,
    shared: Vec<String>,
}

impl Search {
    /// `include_paths` and `defaults` are canonical already; `root_relative`
    /// are the `root_relative_include_paths` as written.
    pub fn new(
        root_relative: Vec<PathBuf>,
        include_paths: Vec<String>,
        defaults: Vec<String>,
    ) -> Self {
        Search {
            root_relative,
            shared: include_paths.into_iter().chain(defaults).collect(),
        }
    }

    /// The unit PROPATH of a unit whose base directory is `base`: `$OUT`, the
    /// root-relative entries joined to `base`, the include paths and the DLC
    /// default entries, with later duplicates removed. An entry that leaves
    /// the root keeps its absolute spelling.
    pub fn propath(&self, paths: &PathMap, base: &str) -> Vec<String> {
        let base = paths.absolute(base);
        let joined = self
            .root_relative
            .iter()
            .map(|entry| paths.canonical(&base.join(entry)));
        let mut propath = vec![OUT.to_string()];
        for entry in joined.chain(self.shared.iter().cloned()) {
            if !propath.contains(&entry) {
                propath.push(entry);
            }
        }
        propath
    }
}

fn directory_of(unit: &str) -> &str {
    unit.rsplit_once('/')
        .map_or(".", |(directory, _)| directory)
}

/// The base directory of a unit: its own directory, or for a class unit its
/// package root. A class that declares a package its directory does not end
/// with cannot compile.
pub fn base_directory(unit: &str, declared: Option<&str>) -> Result<String, String> {
    let directory = directory_of(unit);
    let Some(name) = declared.filter(|_| is_class_unit(unit)) else {
        return Ok(directory.to_string());
    };
    let packages: Vec<&str> = name.split('.').collect();
    let packages = &packages[..packages.len() - 1];
    let components: Vec<&str> = match directory {
        "." => Vec::new(),
        directory => directory.split('/').collect(),
    };
    let ends_with_package = components.len() >= packages.len()
        && components[components.len() - packages.len()..]
            .iter()
            .zip(packages)
            .all(|(component, package)| component.eq_ignore_ascii_case(package));
    if !ends_with_package {
        return Err(format!("package-mismatch: {name}"));
    }
    let root = &components[..components.len() - packages.len()];
    Ok(match root.is_empty() {
        true => ".".to_string(),
        false => root.join("/"),
    })
}

/// Whether the unit is a class source.
pub fn is_class_unit(unit: &str) -> bool {
    Path::new(unit)
        .extension()
        .is_some_and(|extension| extension.eq_ignore_ascii_case("cls"))
}

/// The part of `unit` below the directory `entry`, when the entry holds it.
fn below<'a>(entry: &str, unit: &'a str) -> Option<&'a str> {
    if entry == "." {
        return (!unit.starts_with(['$', '/'])).then_some(unit);
    }
    unit.strip_prefix(entry)?.strip_prefix('/')
}

/// Where a unit compiles from, or why it cannot.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Placement {
    Compiles { name: String },
    Skipped(String),
}

/// The compile name of `unit` on `propath`: its path below the first entry
/// after `$OUT` that holds it. `base` is the unit's base directory and
/// `class` says whether its name must be the package path. `is_file` tells
/// whether a canonical path names a file, for the entries before the holder.
pub fn place(
    unit: &str,
    base: &str,
    class: bool,
    propath: &[String],
    is_file: impl Fn(&str) -> bool,
) -> Placement {
    let entries = propath.iter().skip(1);
    let holder = entries
        .clone()
        .enumerate()
        .find_map(|(at, entry)| below(entry, unit).map(|name| (at, entry, name)));
    let Some((at, entry, name)) = holder else {
        return Placement::Skipped(match class {
            true => format!("package-root-not-on-propath: {base}"),
            false => "not-on-propath".to_string(),
        });
    };
    if class && entry != base {
        return Placement::Skipped(format!("package-root-not-on-propath: {base}"));
    }
    for earlier in entries
        .take(at)
        .filter(|e| *e == "." || !e.starts_with('$') || is_dlc(e))
    {
        let candidate = match earlier.as_str() {
            "." => name.to_string(),
            earlier => format!("{earlier}/{name}"),
        };
        if candidate != unit && is_file(&candidate) {
            return Placement::Skipped(format!("shadowed-by: {candidate}"));
        }
    }
    Placement::Compiles {
        name: name.to_string(),
    }
}
