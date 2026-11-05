//! How a file path is spelled in reports.

use std::path::{Component, Path, PathBuf};

/// The spelling of `path` for a report whose root files print as `root`: a
/// relative root means the run was typed with relative paths, so `path` is
/// shown relative to the working directory, lexically (`..` where it sits
/// outside). An absolute root keeps `path` absolute.
pub fn spell(path: &Path, root: &str) -> String {
    let path = normalize(path);
    if !Path::new(root).is_relative() {
        return path.display().to_string();
    }
    let Ok(cwd) = std::env::current_dir() else {
        return path.display().to_string();
    };
    relative_to(&normalize(&cwd), &path)
        .unwrap_or(path)
        .display()
        .to_string()
}

fn normalize(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                let climbed = out
                    .components()
                    .next_back()
                    .is_some_and(|last| matches!(last, Component::Normal(_)));
                if climbed {
                    out.pop();
                } else if !out.has_root() {
                    out.push("..");
                }
            }
            other => out.push(other.as_os_str()),
        }
    }
    out
}

fn relative_to(base: &Path, path: &Path) -> Option<PathBuf> {
    let base: Vec<_> = base.components().collect();
    let path_parts: Vec<_> = path.components().collect();
    let common = base
        .iter()
        .zip(&path_parts)
        .take_while(|(a, b)| a == b)
        .count();
    if common == 0 {
        return None;
    }
    let mut out = PathBuf::new();
    for _ in common..base.len() {
        out.push("..");
    }
    for part in &path_parts[common..] {
        out.push(part.as_os_str());
    }
    Some(out)
}
