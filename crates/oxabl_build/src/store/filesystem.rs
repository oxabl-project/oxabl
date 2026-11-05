use std::path::Path;

use rustix::fs::statfs;

use super::StoreError;

const NFS: u64 = 0x6969;
const SMB: u64 = 0x517B;
const CIFS: u64 = 0xFF53_4D42;
const SMB2: u64 = 0xFE53_4D42;
const FUSE: u64 = 0x6573_5546;

/// Refuse a directory on a network or FUSE file system: SQLite's WAL mode needs
/// shared memory that they do not give.
pub fn require_local(directory: &Path) -> Result<(), StoreError> {
    let Ok(stat) = statfs(directory) else {
        return Ok(());
    };
    let kind = stat.f_type as u64 & 0xFFFF_FFFF;
    if matches!(kind, NFS | SMB | CIFS | SMB2 | FUSE) {
        return Err(StoreError::Unsupported(
            "the build store needs a local filesystem (WAL); set [build].store to a local directory"
                .to_string(),
        ));
    }
    Ok(())
}
