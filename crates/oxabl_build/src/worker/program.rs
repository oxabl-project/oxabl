//! The worker program that ships inside oxabl.
//!
//! It has two parts. The compile part holds what decides how a unit compiles
//! and is the only part the compile key hashes. The driver part holds the
//! protocol, the schema image, the hashing and the reports. A test lexes the
//! driver part and refuses the statements that can change a compile.

use std::io;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

/// The first line of the program: the protocol version.
const HEADER: &str = "/* oxabl-worker protocol 1 */\n";

/// The compile part of the program.
pub const COMPILE_PART: &str = include_str!("../../resources/oxabl_worker_compile.p");

/// The driver part of the program.
pub const DRIVER_PART: &str = include_str!("../../resources/oxabl_worker_driver.p");

/// The digest of the compile part that the split of the worker program made,
/// in hex.
const SPLIT_COMPILE_DIGEST: &str =
    "aaff9bcafe00f377f5b7610f549adfe348fac37d1cf6b0eec1009ce10e49c999";

/// The digest of the whole program of release 5.3.2, in hex. The compile part
/// of that program is the compile part above.
const RELEASED_PROGRAM_DIGEST: &str =
    "cdd13b1ffca241532b7ab473551f2f6d3b31a9680baf30b7dbd363603bc9f019";

/// The ABL source of the worker program: the header, the compile part and the
/// driver part.
pub fn source() -> &'static str {
    static SOURCE: OnceLock<String> = OnceLock::new();
    SOURCE.get_or_init(|| format!("{HEADER}{COMPILE_PART}{DRIVER_PART}"))
}

/// The BLAKE3 hash of the program's bytes. It names the program's directory
/// and binds a stored schema image.
pub fn hash() -> blake3::Hash {
    blake3::hash(source().as_bytes())
}

/// The BLAKE3 hash of the compile part.
pub fn compile_digest() -> blake3::Hash {
    blake3::hash(COMPILE_PART.as_bytes())
}

/// The digest the worker section of the compile key holds for a compile part
/// with digest `compile`. The compile part of the split is the compile part
/// of release 5.3.2, so for exactly that digest the key keeps the whole-program
/// digest of that release and no key moves. Any other compile part stands for
/// itself.
pub fn key_digest_of(compile: blake3::Hash) -> blake3::Hash {
    if compile.to_hex().as_str() == SPLIT_COMPILE_DIGEST {
        return blake3::Hash::from_hex(RELEASED_PROGRAM_DIGEST)
            .expect("the released digest is 64 hex digits");
    }
    compile
}

/// The digest of the worker section of the compile key.
pub fn key_digest() -> blake3::Hash {
    key_digest_of(compile_digest())
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
    if std::fs::read(&host_path).is_ok_and(|bytes| bytes == source().as_bytes()) {
        return Ok(program(state_dir, root, worker_root, &hex, host_path));
    }
    std::fs::create_dir_all(&directory)?;
    let temp = directory.join(format!("oxabl_worker.p.tmp-{}", std::process::id()));
    write_then_rename(&temp, &host_path)?;
    Ok(program(state_dir, root, worker_root, &hex, host_path))
}

fn write_then_rename(temp: &Path, target: &Path) -> io::Result<()> {
    std::fs::write(temp, source().as_bytes())?;
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
