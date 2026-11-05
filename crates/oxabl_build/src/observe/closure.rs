//! The observed closure of one compile, from its `XREF-XML`.

use super::xref::Xref;
use crate::paths::PathMap;
use crate::paths::propath::{OUT, is_dlc, library_name};
use crate::store::ObservedClosure;
use crate::verify::reads::{canonical_name, normalise};

/// Where the build looks for the files a compile names.
pub struct Places<'a> {
    pub paths: &'a PathMap,
    /// `$DLC` as the worker sees it, without a trailing `/`.
    pub worker_dlc: &'a str,
    /// The workspace root as the worker sees it, without a trailing `/`.
    pub worker_root: &'a str,
    /// The r-code directory, root-relative: `$OUT` on a PROPATH.
    pub rcode: &'a str,
}

impl Places<'_> {
    /// The canonical path of `name` below the PROPATH entry `entry`; `None`
    /// for an entry the host cannot search.
    fn below(&self, entry: &str, name: &str) -> Option<Result<String, String>> {
        let directory = match entry {
            OUT => self.rcode,
            _ if library_name(entry).is_some() => return None,
            _ if entry.starts_with('/') => {
                return Some(Err(format!(
                    "the PROPATH entry {entry} is outside the root"
                )));
            }
            _ if is_dlc(entry) && self.paths.dlc().is_none() => {
                return Some(Err(format!(
                    "the PROPATH entry {entry} needs [workspace.sources].dlc"
                )));
            }
            entry => entry,
        };
        let joined = match directory {
            "." => name.to_string(),
            directory => format!("{directory}/{name}"),
        };
        let canonical = match joined.strip_prefix("$DLC") {
            Some(rest) => normalise(rest).map(|rest| format!("$DLC/{rest}")),
            None => normalise(&joined),
        };
        Some(canonical.ok_or_else(|| format!("the include {name} leaves its PROPATH entry")))
    }

    /// The file the include `name` resolves to on `propath`, and the paths
    /// before it that must stay absent.
    fn resolve(
        &self,
        name: &str,
        propath: &[String],
        closure: &mut ObservedClosure,
    ) -> Result<String, String> {
        if name.starts_with('/') {
            return canonical_name(name, self.worker_dlc, self.worker_root)
                .ok_or_else(|| format!("the include {name} has no canonical path"));
        }
        for entry in propath {
            let Some(candidate) = self.below(entry, name) else {
                continue;
            };
            let candidate = candidate?;
            if self.paths.absolute(&candidate).is_file() {
                return Ok(candidate);
            }
            closure.absent.push(candidate);
        }
        Err(format!(
            "the include {name} is not on the PROPATH of the host"
        ))
    }
}

/// The closure that one compile of `unit` read, by its `XREF-XML`. Each
/// include is resolved on the unit PROPATH on the host, and each file the
/// compiler names must be where an include resolves to: else the host and the
/// compiler disagree, and there is no closure. A use of a class gives none
/// either, because no `XREF-XML` names the file of a class.
pub fn observe(
    xref: &Xref,
    unit: &str,
    propath: &[String],
    places: &Places<'_>,
) -> Result<ObservedClosure, String> {
    if let Some(class) = &xref.class {
        return Err(format!("the compile used a class ({class})"));
    }
    let mut closure = ObservedClosure::default();
    for name in &xref.includes {
        let file = places.resolve(name, propath, &mut closure)?;
        closure.files.push(file);
    }
    for name in &xref.sources {
        let source = canonical_name(name, places.worker_dlc, places.worker_root)
            .filter(|_| !name.contains("<<"))
            .ok_or_else(|| format!("the compiler read {name}, which has no canonical path"))?;
        if source != unit && !closure.files.contains(&source) {
            return Err(format!(
                "the compiler read {source}, which no include resolves to on the host"
            ));
        }
    }
    closure.files.retain(|file| file != unit);
    closure.files.sort();
    closure.files.dedup();
    closure.absent.sort();
    closure.absent.dedup();
    closure
        .absent
        .retain(|path| closure.files.binary_search(path).is_err());
    Ok(closure)
}
