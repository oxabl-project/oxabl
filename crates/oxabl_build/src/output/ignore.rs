//! Keeps version control off oxabl's own trees: a `.gitignore` holding `*` in
//! the top directory that holds oxabl's state and output directories.

use std::collections::BTreeSet;
use std::ffi::OsString;
use std::fs;
use std::io::{ErrorKind, Write};
use std::path::{Component, Path, PathBuf};

const FILE: &str = ".gitignore";
const CONTENT: &str = "# Written by oxabl: this directory holds its build state and output.\n*\n";

/// What oxabl keeps below one top directory of the root.
#[derive(Default)]
struct Top {
    /// The second components of its directories.
    children: BTreeSet<OsString>,
    /// Whether a state directory is the top directory itself.
    state_is_top: bool,
    /// Whether an output directory is the top directory itself.
    output_is_top: bool,
}

/// Writes `<root>/<top>/.gitignore` for the first path component `top` of each
/// of `state` and `outputs` (absolute, under `root`). Skips a `top` that is an
/// output directory itself, that is not a real directory, that already has a
/// `.gitignore` or that holds anything else than oxabl's directories. Returns
/// the warnings of the writes that failed.
pub fn write(root: &Path, state: &[PathBuf], outputs: &[PathBuf]) -> Vec<String> {
    let mut tops: Vec<(OsString, Top)> = Vec::new();
    let marked = state
        .iter()
        .map(|directory| (directory, false))
        .chain(outputs.iter().map(|directory| (directory, true)));
    for (directory, is_output) in marked {
        let mut parts = directory
            .strip_prefix(root)
            .into_iter()
            .flat_map(|below| below.components())
            .filter_map(|part| match part {
                Component::Normal(name) => Some(name.to_os_string()),
                _ => None,
            });
        let Some(name) = parts.next() else { continue };
        let child = parts.next();
        let at = match tops.iter().position(|(top, _)| *top == name) {
            Some(at) => at,
            None => {
                tops.push((name, Top::default()));
                tops.len() - 1
            }
        };
        let top = &mut tops[at].1;
        match child {
            Some(child) => {
                top.children.insert(child);
            }
            None if is_output => top.output_is_top = true,
            None => top.state_is_top = true,
        }
    }
    let mut warnings = Vec::new();
    for (name, top) in tops {
        let path = root.join(name);
        if top.output_is_top || !writable(&path, &top) {
            continue;
        }
        if let Err(error) = create(&path) {
            warnings.push(format!(
                "cannot write {}: {error}",
                path.join(FILE).display()
            ));
        }
    }
    warnings
}

/// Whether `path` is a real directory without a `.gitignore`. Unless a state
/// directory is `path` itself, it holds nothing but oxabl's directories: a
/// file of the user is never hidden.
fn writable(path: &Path, top: &Top) -> bool {
    if !fs::symlink_metadata(path).is_ok_and(|meta| meta.is_dir()) {
        return false;
    }
    let Ok(entries) = fs::read_dir(path) else {
        return false;
    };
    for entry in entries {
        let Ok(entry) = entry else { return false };
        let name = entry.file_name();
        if name == FILE {
            return false;
        }
        let leftover = name.to_string_lossy().starts_with(".gitignore.");
        if !(top.state_is_top || top.children.contains(&name) || leftover) {
            return false;
        }
    }
    true
}

/// Writes the file through a temporary one, so a reader never sees half of
/// it and a file that appears meanwhile is never replaced.
fn create(directory: &Path) -> std::io::Result<()> {
    let target = directory.join(FILE);
    let temporary = directory.join(format!("{FILE}.{}.tmp", std::process::id()));
    let mut file = fs::File::create(&temporary)?;
    let written = file
        .write_all(CONTENT.as_bytes())
        .and_then(|()| file.sync_all());
    drop(file);
    let linked = written.and_then(|()| fs::hard_link(&temporary, &target));
    let _ = fs::remove_file(&temporary);
    match linked {
        Err(error) if error.kind() == ErrorKind::AlreadyExists => Ok(()),
        other => other,
    }
}
