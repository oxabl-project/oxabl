//! The class sources a compile of a unit that oxabl cannot read could use.

use crate::paths::PathMap;
use crate::paths::propath::{OUT, is_class_unit};

/// Every class source (`.cls`) below the workspace entries of `propath`, and
/// every class unit of the workspace, whose r-code lands in `$OUT`: sorted
/// canonical paths. Entries under `$DLC` and `$LIB:` entries are not listed;
/// their classes have the trust of the PROPATH digests, as in a parsed
/// closure. A directory that cannot be read, or a name that is not UTF-8,
/// is an error: nothing is guessed.
pub fn class_listing<'a>(
    paths: &PathMap,
    propath: &[String],
    class_units: impl Iterator<Item = &'a str>,
) -> Result<Vec<String>, String> {
    let mut found: Vec<String> = class_units.map(str::to_string).collect();
    let mut entries: Vec<&str> = propath
        .iter()
        .map(String::as_str)
        .filter(|entry| *entry != OUT && !entry.starts_with('$') && !entry.starts_with('/'))
        .collect();
    entries.sort_unstable();
    entries.dedup();
    for entry in entries {
        let directory = paths.absolute(entry);
        if !directory.is_dir() {
            continue;
        }
        for item in walkdir::WalkDir::new(&directory).follow_links(true) {
            let item = item.map_err(|error| format!("cannot list {entry}: {error}"))?;
            if !item.file_type().is_file() {
                continue;
            }
            let Some(name) = item.path().to_str() else {
                return Err(format!("a file name below {entry} is not UTF-8"));
            };
            if !is_class_unit(name) {
                continue;
            }
            let canonical = paths.canonical(item.path());
            if canonical.starts_with('/') || canonical.starts_with('$') {
                return Err(format!("{name} has no canonical path"));
            }
            found.push(canonical);
        }
    }
    found.sort_unstable();
    found.dedup();
    Ok(found)
}
