//! Moving the outputs of a compiled unit from the stage to their final paths.

use std::fs::{self, File};
use std::io::{self, Write};
use std::os::unix::fs::MetadataExt;
use std::path::Path;

use super::layout::Expected;
use crate::store::OutputRow;

/// The stat and the hash of the file at `path`, as an output row for `dest`.
pub fn describe(path: &Path, dest: &str) -> io::Result<OutputRow> {
    let meta = fs::metadata(path)?;
    let bytes = fs::read(path)?;
    Ok(OutputRow {
        path: dest.to_string(),
        size: meta.len(),
        mtime_ns: meta.mtime() * 1_000_000_000 + meta.mtime_nsec(),
        ino: meta.ino(),
        hash: *blake3::hash(&bytes).as_bytes(),
    })
}

/// Moves `from` to `to` with `rename`; across file systems it copies to a
/// temporary name beside `to`, calls `fsync` and renames that.
fn move_file(from: &Path, to: &Path) -> io::Result<()> {
    match fs::rename(from, to) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == io::ErrorKind::CrossesDevices => {
            let temp = to.with_file_name(format!(
                "{}.oxabl-tmp-{}",
                to.file_name().unwrap_or_default().to_string_lossy(),
                std::process::id()
            ));
            let result = (|| {
                let mut source = File::open(from)?;
                let mut target = File::create(&temp)?;
                io::copy(&mut source, &mut target)?;
                target.flush()?;
                target.sync_all()?;
                fs::rename(&temp, to)
            })();
            if result.is_err() {
                let _ = fs::remove_file(&temp);
            }
            result?;
            fs::remove_file(from)
        }
        Err(error) => Err(error),
    }
}

/// Moves every expected output of a unit from `stage` to its place below
/// `root`. Nothing moves unless every output is in the stage as a regular
/// file. Returns one row for each output.
pub fn promote(root: &Path, stage: &Path, expected: &[Expected]) -> Result<Vec<OutputRow>, String> {
    for file in expected {
        let source = stage.join(&file.staged);
        match fs::symlink_metadata(&source) {
            Ok(meta) if meta.is_file() => {}
            _ => {
                return Err(format!(
                    "the compile produced no regular file {}",
                    file.staged
                ));
            }
        }
    }
    let mut rows = Vec::with_capacity(expected.len());
    for file in expected {
        let target = root.join(&file.dest);
        if let Some(parent) = target.parent() {
            fs::create_dir_all(parent).map_err(|e| format!("cannot create {}: {e}", file.dest))?;
        }
        move_file(&stage.join(&file.staged), &target)
            .map_err(|e| format!("cannot install {}: {e}", file.dest))?;
        rows.push(
            describe(&target, &file.dest).map_err(|e| format!("cannot read {}: {e}", file.dest))?,
        );
    }
    Ok(rows)
}

/// Deletes the outputs of a unit, whatever state they are in.
pub fn remove(root: &Path, expected: &[Expected]) {
    for file in expected {
        let _ = fs::remove_file(root.join(&file.dest));
    }
}
