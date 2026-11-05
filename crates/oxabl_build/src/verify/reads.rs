//! V3: the compiler opened no file outside the closure of the unit.

use rustc_hash::{FxHashMap, FxHashSet};

/// Maps the names the compiler reports to canonical paths and checks them
/// against a closure.
pub struct Reads<'a> {
    names: &'a [String],
    nodes: FxHashMap<&'a str, u32>,
    /// `$DLC` as the worker sees it.
    worker_dlc: String,
    /// The workspace root as the worker sees it.
    worker_root: String,
    /// The r-code directory, root-relative.
    rcode: String,
}

/// The canonical path of a file name as the compiler gave it: relative to the
/// worker root, or absolute under the worker root or the worker's `$DLC`
/// (both without a trailing `/`). `None` when it maps to neither.
pub fn canonical_name(name: &str, worker_dlc: &str, worker_root: &str) -> Option<String> {
    let below = |prefix: &str| -> Option<String> {
        let rest = name.strip_prefix(prefix)?;
        rest.strip_prefix('/').map(str::to_string)
    };
    let relative = if name.starts_with('/') {
        if !worker_dlc.is_empty()
            && let Some(rest) = below(worker_dlc)
        {
            return normalise(&rest).map(|rest| format!("$DLC/{rest}"));
        }
        below(worker_root)?
    } else {
        name.to_string()
    };
    match relative.strip_prefix("$DLC/") {
        Some(rest) => normalise(rest).map(|rest| format!("$DLC/{rest}")),
        None => normalise(&relative),
    }
}

/// `text` without trailing `/`, unless that leaves nothing.
pub fn trim_slash(text: &str) -> &str {
    let trimmed = text.trim_end_matches('/');
    if trimmed.is_empty() { text } else { trimmed }
}

impl<'a> Reads<'a> {
    pub fn new(names: &'a [String], worker_dlc: &str, worker_root: &str, rcode: &str) -> Reads<'a> {
        Reads {
            names,
            nodes: names
                .iter()
                .enumerate()
                .map(|(id, name)| (name.as_str(), id as u32))
                .collect(),
            worker_dlc: trim_slash(worker_dlc).to_string(),
            worker_root: trim_slash(worker_root).to_string(),
            rcode: rcode.to_string(),
        }
    }

    fn canonical(&self, name: &str) -> Option<String> {
        canonical_name(name, &self.worker_dlc, &self.worker_root)
    }

    /// `Ok` when every name is the unit or in its closure. R-code under the
    /// output directory and members of a library are skipped when the closure
    /// holds a class: class resolution is protected by the plan rules. `Err`
    /// names the first file that is neither.
    pub fn check(&self, unit: u32, closure: &[u32], listed: &[String]) -> Result<(), String> {
        let allowed: FxHashSet<u32> = closure.iter().copied().chain([unit]).collect();
        let class_in_closure = allowed
            .iter()
            .any(|id| self.names[*id as usize].ends_with(".cls"));
        for name in listed {
            let Some(canonical) = self.canonical(name) else {
                return Err(name.clone());
            };
            if let Some(id) = self.nodes.get(canonical.as_str())
                && allowed.contains(id)
            {
                continue;
            }
            let rcode = canonical.ends_with(".r")
                && canonical.starts_with(&self.rcode)
                && canonical[self.rcode.len()..].starts_with('/');
            let library = name.contains(".pl<<");
            if class_in_closure && (rcode || library) {
                continue;
            }
            return Err(canonical);
        }
        Ok(())
    }
}

/// `path` without `.` components and doubled slashes; `None` when it has a
/// `..` component or is empty.
pub fn normalise(path: &str) -> Option<String> {
    let mut parts = Vec::new();
    for part in path.split('/') {
        match part {
            "" | "." => {}
            ".." => return None,
            other => parts.push(other),
        }
    }
    (!parts.is_empty()).then(|| parts.join("/"))
}
