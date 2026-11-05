//! The worker program that ships inside oxabl.

use std::io;
use std::path::{Path, PathBuf};

/// The ABL source of the worker program.
pub const SOURCE: &str = include_str!("../../resources/oxabl_worker.p");

/// The BLAKE3 hash of the program's bytes.
pub fn hash() -> blake3::Hash {
    blake3::hash(SOURCE.as_bytes())
}

/// Where the program sits, on the host and as the worker sees it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub host_path: PathBuf,
    pub worker_path: String,
}

/// Writes the program to `<state>/worker/<hash>/oxabl_worker.p` unless it is
/// there already with the right bytes. The write is temp-and-rename.
pub fn materialize(state_dir: &Path, root: &Path, worker_root: &str) -> io::Result<Program> {
    let hex = hash().to_hex().to_string();
    let directory = state_dir.join("worker").join(&hex);
    let host_path = directory.join("oxabl_worker.p");
    if std::fs::read(&host_path).is_ok_and(|bytes| bytes == SOURCE.as_bytes()) {
        return Ok(program(state_dir, root, worker_root, &hex, host_path));
    }
    std::fs::create_dir_all(&directory)?;
    let temp = directory.join(format!("oxabl_worker.p.tmp-{}", std::process::id()));
    write_then_rename(&temp, &host_path)?;
    Ok(program(state_dir, root, worker_root, &hex, host_path))
}

fn write_then_rename(temp: &Path, target: &Path) -> io::Result<()> {
    std::fs::write(temp, SOURCE.as_bytes())?;
    std::fs::rename(temp, target)
}

fn program(
    state_dir: &Path,
    root: &Path,
    worker_root: &str,
    hex: &str,
    host_path: PathBuf,
) -> Program {
    let relative = state_dir.strip_prefix(root).unwrap_or(state_dir);
    Program {
        host_path,
        worker_path: format!(
            "{}/{}/worker/{hex}/oxabl_worker.p",
            worker_root.trim_end_matches('/'),
            relative.to_string_lossy()
        ),
    }
}
