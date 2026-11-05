use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::path::{Path, PathBuf};

use oxabl_workspace::is_root_file_in;

use super::probe::{Probe, Stat};
use crate::config::BuildSettings;
use crate::error::BuildError;
use crate::paths::PathMap;

/// A compile unit found by the walk.
#[derive(Debug, Clone)]
pub struct Unit {
    pub canonical: String,
    pub path: PathBuf,
    pub probe: Probe,
}

fn is_hidden_directory(entry: &walkdir::DirEntry) -> bool {
    entry.file_type().is_dir()
        && entry
            .file_name()
            .to_str()
            .is_some_and(|name| name.starts_with('.'))
}

/// Walk `[workspace.sources].directories` for the files with a root extension,
/// minus `[build].exclude`, sorted by canonical path. Each file is stat'ed once
/// here. A directory is walked through links, a hidden one below the first
/// level is not entered, and a file reached twice counts once.
pub fn walk_units(
    settings: &BuildSettings,
    paths: &PathMap,
    root_extensions: &[String],
) -> Result<Vec<Unit>, BuildError> {
    let mut units: Vec<Unit> = Vec::new();
    for directory in &settings.unit_directories {
        if !directory.is_dir() {
            return Err(BuildError::Usage(format!(
                "[workspace.sources].directories: {} is not a directory",
                directory.display()
            )));
        }
        let walk = walkdir::WalkDir::new(directory)
            .follow_links(true)
            .into_iter()
            .filter_entry(|entry| entry.depth() == 0 || !is_hidden_directory(entry))
            .filter_map(Result::ok);
        for entry in walk {
            if !entry.file_type().is_file() || !is_root_file_in(entry.path(), root_extensions) {
                continue;
            }
            let canonical = paths.canonical(entry.path());
            if settings.excludes.excludes(&canonical) {
                continue;
            }
            let Ok(metadata) = entry.metadata() else {
                continue;
            };
            units.push(Unit {
                canonical,
                path: entry.into_path(),
                probe: Probe::Present {
                    stat: Stat::of(&metadata),
                    is_dir: false,
                },
            });
        }
    }
    units.sort_by(|a, b| a.canonical.cmp(&b.canonical));
    units.dedup_by(|a, b| a.canonical == b.canonical);
    drop_second_spellings(&mut units);
    Ok(units)
}

/// Remove a unit that is the same real file as an earlier one, reached through
/// a linked directory. Two hard links are two files and stay.
fn drop_second_spellings(units: &mut Vec<Unit>) {
    let mut first: HashMap<(u64, u64), (usize, Option<PathBuf>)> = HashMap::new();
    let mut drop = vec![false; units.len()];
    for (i, unit) in units.iter().enumerate() {
        let Probe::Present { stat, .. } = unit.probe else {
            continue;
        };
        match first.entry((stat.dev, stat.ino)) {
            Entry::Vacant(slot) => {
                slot.insert((i, None));
            }
            Entry::Occupied(mut slot) => {
                let (earlier, real) = slot.get_mut();
                let real = real.get_or_insert_with(|| real_path(&units[*earlier].path));
                drop[i] = *real == real_path(&unit.path);
            }
        }
    }
    let mut index = 0;
    units.retain(|_| {
        index += 1;
        !drop[index - 1]
    });
}

fn real_path(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}
