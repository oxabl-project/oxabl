//! Putting a file into a cache entry: a reflink when the file system has
//! them, else a hard link, else a copy.

use std::fs::{self, File, OpenOptions};
use std::io;
use std::os::unix::fs::OpenOptionsExt;
use std::path::Path;

/// Opens `path` for reading and refuses a symbolic link in the final component.
pub fn open_no_follow(path: &Path) -> io::Result<File> {
    OpenOptions::new()
        .read(true)
        .custom_flags(rustix::fs::OFlags::NOFOLLOW.bits() as i32)
        .open(path)
}

#[cfg(any(target_os = "linux", target_os = "android"))]
fn reflink(from: &Path, to: &Path) -> io::Result<()> {
    let source = open_no_follow(from)?;
    let target = OpenOptions::new().write(true).create_new(true).open(to)?;
    match rustix::fs::ioctl_ficlone(&target, &source) {
        Ok(()) => Ok(()),
        Err(error) => {
            drop(target);
            let _ = fs::remove_file(to);
            Err(error.into())
        }
    }
}

#[cfg(not(any(target_os = "linux", target_os = "android")))]
fn reflink(_: &Path, _: &Path) -> io::Result<()> {
    Err(io::ErrorKind::Unsupported.into())
}

/// Makes `to` hold the bytes of `from`. `to` must not exist and its directory
/// must.
pub fn place(from: &Path, to: &Path) -> io::Result<()> {
    if reflink(from, to).is_ok() {
        return Ok(());
    }
    if fs::hard_link(from, to).is_ok() {
        return Ok(());
    }
    let mut source = open_no_follow(from)?;
    let mut target = OpenOptions::new().write(true).create_new(true).open(to)?;
    let result = io::copy(&mut source, &mut target).map(|_| ());
    if result.is_err() {
        drop(target);
        let _ = fs::remove_file(to);
    }
    result
}
