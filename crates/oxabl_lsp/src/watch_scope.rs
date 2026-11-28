//! Which `workspace/didChangeWatchedFiles` events a session needs.
//!
//! The server registers its watchers with plain globs, and a client such as VS Code
//! applies a glob to every workspace folder. The server of one folder then hears of
//! every file that changes in the others, thousands of them after a branch switch.
//! An event for a file the session never reads must cost nothing: no
//! re-resolution of the configuration, no pass, no invalidation.

use std::collections::HashMap;
use std::path::{Component, Path, PathBuf};

use oxabl_pipeline::ConfiguredInputs;

/// A path in its two spellings: normalized without the disk, and resolved through
/// the symbolic links that exist.
pub(crate) struct Place {
    lexical: PathBuf,
    real: PathBuf,
}

impl Place {
    fn new(path: &Path) -> Place {
        let lexical = normalize(path);
        let real = std::fs::canonicalize(&lexical).unwrap_or_else(|_| lexical.clone());
        Place { lexical, real }
    }
}

/// The places a session reads: a file under one of them, a file it has read, or an
/// `oxabl.toml` above its root is an event it needs.
pub(crate) enum WatchScope {
    /// No filter: the client declared no root, a path is not absolute, or the
    /// configuration reads directories that no list names.
    All,
    Places {
        root: Place,
        /// The workspace root, the include paths, and what the configuration
        /// names.
        places: Vec<Place>,
    },
}

impl WatchScope {
    /// Whether the scope keeps every event.
    pub(crate) fn keeps_all(&self) -> bool {
        matches!(self, WatchScope::All)
    }

    /// The scope of a session with `root`, the resolved `include_paths` and the
    /// other `inputs` of its configuration. A path that is not absolute (a client
    /// that sent a malformed root) gives a scope that keeps every event.
    pub(crate) fn new(
        root: Option<&Path>,
        include_paths: &[PathBuf],
        inputs: &ConfiguredInputs,
    ) -> WatchScope {
        let Some(root) = root else {
            return WatchScope::All;
        };
        let all = std::iter::once(root)
            .chain(include_paths.iter().map(PathBuf::as_path))
            .chain(inputs.paths.iter().map(PathBuf::as_path));
        if inputs.unbounded || all.clone().any(|path| !path.is_absolute()) {
            return WatchScope::All;
        }
        WatchScope::Places {
            root: Place::new(root),
            places: all.map(Place::new).collect(),
        }
    }

    /// Whether the session needs the event for `path`. `read` says whether the
    /// session has read a path: it is asked about the spelling of the event, and
    /// again with the symbolic links resolved. `directories` keeps the resolved
    /// directories of one batch of events, so a directory that many events share
    /// is read from the disk once.
    pub(crate) fn keeps(
        &self,
        path: &Path,
        directories: &mut HashMap<PathBuf, PathBuf>,
        read: &mut dyn FnMut(&Path) -> bool,
    ) -> bool {
        // An event path that is not absolute cannot be placed: keep it.
        let WatchScope::Places { root, places } = self else {
            return true;
        };
        if !path.is_absolute() {
            return true;
        }
        let lexical = normalize(path);
        if is_config_above_root(root, &lexical) || holds(places, &lexical, false) || read(&lexical)
        {
            return true;
        }
        // The disk is read only for an event that no spelling of the path
        // accepted: the common case of an event for a sibling folder.
        let real = match (lexical.parent(), lexical.file_name()) {
            (Some(dir), Some(name)) => {
                let dir = directories.entry(dir.to_path_buf()).or_insert_with(|| {
                    std::fs::canonicalize(dir).unwrap_or_else(|_| dir.to_path_buf())
                });
                dir.join(name)
            }
            _ => return false,
        };
        is_config_above_root(root, &real)
            || holds(places, &real, true)
            || (real != lexical && read(&real))
    }
}

/// Whether `path` is under one of `places`, in the spelling `real` selects.
fn holds(places: &[Place], path: &Path, real: bool) -> bool {
    places
        .iter()
        .any(|place| path.starts_with(if real { &place.real } else { &place.lexical }))
}

/// An `oxabl.toml` in the root or in a directory above it: the one that governs
/// the root may be any of them, and one may appear.
fn is_config_above_root(root: &Place, path: &Path) -> bool {
    path.file_name().is_some_and(|name| name == "oxabl.toml")
        && path
            .parent()
            .is_some_and(|dir| root.lexical.starts_with(dir) || root.real.starts_with(dir))
}

/// `path` with its `.` and `..` components resolved without the disk. A `..` at
/// the root stays at the root.
fn normalize(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    let mut rooted = false;
    for part in path.components() {
        match part {
            Component::CurDir => {}
            Component::ParentDir => {
                if !out.pop() && !rooted {
                    out.push("..");
                }
            }
            Component::RootDir => {
                rooted = true;
                out.push(part.as_os_str());
            }
            other => out.push(other.as_os_str()),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scope(include_paths: &[&str], inputs: &[&str]) -> WatchScope {
        let inputs = ConfiguredInputs {
            paths: inputs.iter().map(PathBuf::from).collect(),
            unbounded: false,
        };
        let include_paths: Vec<PathBuf> = include_paths.iter().map(PathBuf::from).collect();
        WatchScope::new(Some(Path::new("/w/alpha")), &include_paths, &inputs)
    }

    fn keeps(scope: &WatchScope, path: &str) -> bool {
        scope.keeps(Path::new(path), &mut HashMap::new(), &mut |_| false)
    }

    #[test]
    fn keeps_what_the_session_reads_and_drops_the_rest() {
        let scope = scope(&["/w/alpha/../shared", "/opt/dlc/src"], &["/w/db"]);
        for kept in [
            "/w/alpha/a.p",
            "/w/alpha/sub/x.i",
            "/w/shared/ext.i",
            "/opt/dlc/src/std.i",
            "/w/db/a.df",
            "/w/oxabl.toml",
            "/oxabl.toml",
            "/w/alpha/oxabl.toml",
        ] {
            assert!(keeps(&scope, kept), "{kept}");
        }
        for dropped in [
            "/w/beta/x.p",
            "/w/beta/inc.i",
            "/w/beta/db/b.df",
            "/w/beta/oxabl.toml",
            "/w/alpha2/a.p",
            "/w/alpha/../beta/x.p",
        ] {
            assert!(!keeps(&scope, dropped), "{dropped}");
        }
    }

    #[test]
    fn keeps_a_file_the_session_has_read() {
        let scope = scope(&[], &[]);
        assert!(!keeps(&scope, "/w/beta/inc.i"));
        assert!(
            scope.keeps(Path::new("/w/beta/inc.i"), &mut HashMap::new(), &mut |p| p
                == Path::new("/w/beta/inc.i"))
        );
    }

    #[test]
    fn keeps_everything_without_a_root_or_with_a_search_path_that_leaves_its_file() {
        let none = WatchScope::new(None, &[], &ConfiguredInputs::default());
        assert!(keeps(&none, "/w/beta/x.p"));
        let unbounded = WatchScope::new(
            Some(Path::new("/w/alpha")),
            &[],
            &ConfiguredInputs {
                paths: Vec::new(),
                unbounded: true,
            },
        );
        assert!(keeps(&unbounded, "/w/beta/x.p"));
    }

    #[cfg(unix)]
    #[test]
    fn follows_a_symbolic_link_to_a_kept_place() {
        let dir = tempfile::tempdir().unwrap();
        let real = dir.path().canonicalize().unwrap();
        std::fs::create_dir_all(real.join("alpha")).unwrap();
        std::fs::create_dir_all(real.join("shared")).unwrap();
        std::os::unix::fs::symlink(real.join("shared"), real.join("alpha/link")).unwrap();
        let scope = WatchScope::new(
            Some(&real.join("alpha")),
            &[real.join("alpha/link")],
            &ConfiguredInputs::default(),
        );
        assert!(keeps(&scope, real.join("shared/ext.i").to_str().unwrap()));
        assert!(!keeps(&scope, real.join("other/ext.i").to_str().unwrap()));
    }

    #[test]
    fn keeps_every_event_for_a_path_that_cannot_be_placed() {
        let relative = WatchScope::new(
            Some(Path::new("w/alpha")),
            &[],
            &ConfiguredInputs::default(),
        );
        assert!(relative.keeps_all());
        let relative_include = WatchScope::new(
            Some(Path::new("/w/alpha")),
            &[PathBuf::from("inc")],
            &ConfiguredInputs::default(),
        );
        assert!(relative_include.keeps_all());
        let scope = scope(&[], &[]);
        assert!(!scope.keeps_all());
        assert!(keeps(&scope, "x.p"), "an event path that is not absolute");
    }

    #[test]
    fn a_parent_component_at_the_root_stays_at_the_root() {
        assert_eq!(normalize(Path::new("/../w/./a/../b")), Path::new("/w/b"));
        assert_eq!(normalize(Path::new("../a")), Path::new("../a"));
        assert!(!keeps(&scope(&[], &[]), "/../w/beta/x.p"));
        assert!(keeps(&scope(&[], &[]), "/../w/alpha/x.p"));
    }

    #[test]
    fn resolves_the_directory_of_a_batch_once() {
        let scope = scope(&[], &[]);
        let mut directories = HashMap::new();
        for name in ["a.p", "b.p", "c.p"] {
            let path = Path::new("/w/beta").join(name);
            assert!(!scope.keeps(&path, &mut directories, &mut |_| false));
        }
        assert_eq!(directories.len(), 1);
    }

    #[cfg(unix)]
    #[test]
    fn keeps_the_config_above_the_root_through_a_symbolic_link() {
        let dir = tempfile::tempdir().unwrap();
        let real = dir.path().canonicalize().unwrap();
        std::fs::create_dir_all(real.join("parent/alpha")).unwrap();
        std::os::unix::fs::symlink(real.join("parent"), real.join("link")).unwrap();
        let scope = WatchScope::new(
            Some(&real.join("parent/alpha")),
            &[],
            &ConfiguredInputs::default(),
        );
        let through_link = real.join("link/oxabl.toml");
        assert!(keeps(&scope, through_link.to_str().unwrap()));
        let other = real.join("link/beta/oxabl.toml");
        assert!(!keeps(&scope, other.to_str().unwrap()));
    }
}
