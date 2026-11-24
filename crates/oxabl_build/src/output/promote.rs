//! Moving the outputs of a compiled unit from the stage to their final paths.

use std::fs::{self, File};
use std::io::{self, Write};
use std::os::unix::fs::MetadataExt;
use std::path::Path;

use super::layout::Expected;
use crate::store::OutputRow;

/// The stat and the hash of the file at `path`, as an output row for `dest`.
pub fn describe(path: &Path, dest: &str) -> io::Result<OutputRow> {
    let observed_ns = crate::refresh::now_ns();
    let meta = fs::metadata(path)?;
    let bytes = fs::read(path)?;
    Ok(OutputRow {
        path: dest.to_string(),
        size: meta.len(),
        mtime_ns: meta.mtime() * 1_000_000_000 + meta.mtime_nsec(),
        ctime_ns: meta.ctime() * 1_000_000_000 + meta.ctime_nsec(),
        ino: meta.ino(),
        hash: *blake3::hash(&bytes).as_bytes(),
        observed_ns,
    })
}

/// What the name of a temporary file beside an output holds.
pub const TEMP_INFIX: &str = ".oxabl-tmp-";

/// Moves `from` to `to` with `rename`; across file systems it copies to a
/// temporary name beside `to`, calls `fsync` and renames that.
fn move_file(from: &Path, to: &Path) -> io::Result<()> {
    match fs::rename(from, to) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == io::ErrorKind::CrossesDevices => {
            let temp = to.with_file_name(format!(
                "{}{TEMP_INFIX}{}",
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

/// Moves every expected output of a unit from `stage`, where `at` gives its
/// path, to its place below `root`. Nothing moves unless every output is in
/// the stage as a regular file. Returns one row for each output.
pub fn promote(
    root: &Path,
    stage: &Path,
    expected: &[Expected],
    at: impl Fn(&Expected) -> &str,
) -> Result<Vec<OutputRow>, String> {
    for file in expected {
        let source = stage.join(at(file));
        match fs::symlink_metadata(&source) {
            Ok(meta) if meta.is_file() => {}
            _ => {
                return Err(format!("the compile produced no regular file {}", at(file)));
            }
        }
    }
    let mut rows = Vec::with_capacity(expected.len());
    for file in expected {
        let target = root.join(&file.dest);
        if let Some(parent) = target.parent().filter(|_| !file.in_place) {
            fs::create_dir_all(parent).map_err(|e| format!("cannot create {}: {e}", file.dest))?;
        }
        move_file(&stage.join(at(file)), &target)
            .map_err(|e| format!("cannot install {}: {e}", file.dest))?;
        rows.push(
            describe(&target, &file.dest).map_err(|e| format!("cannot read {}: {e}", file.dest))?,
        );
    }
    Ok(rows)
}

/// Deletes the outputs of a unit, whatever state they are in. An output that
/// is not there is deleted; any other failure names the file, because an
/// output left in place can run.
pub fn remove(root: &Path, expected: &[Expected]) -> Result<(), String> {
    remove_where(root, expected, |_| true)
}

/// Like [`remove`], for the outputs beside the source alone.
pub fn remove_in_place(root: &Path, expected: &[Expected]) -> Result<(), String> {
    remove_where(root, expected, |file| file.in_place)
}

fn remove_where(
    root: &Path,
    expected: &[Expected],
    which: impl Fn(&Expected) -> bool,
) -> Result<(), String> {
    let mut failed = None;
    for file in expected.iter().filter(|file| which(file)) {
        match fs::remove_file(root.join(&file.dest)) {
            Err(error) if error.kind() != io::ErrorKind::NotFound => {
                failed.get_or_insert_with(|| format!("cannot delete {}: {error}", file.dest));
            }
            _ => {}
        }
    }
    failed.map_or(Ok(()), Err)
}
