//! The install procedure: download beside the target, verify, try, rename.

use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use sha2::{Digest, Sha256};

use super::github::{ASSET, Api, Release};

const MAX_BINARY: u64 = 512 * 1024 * 1024;
const VERSION_TIMEOUT: Duration = Duration::from_secs(15);

/// A staged file, deleted unless it is renamed away.
struct Staged(PathBuf);

impl Drop for Staged {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.0);
    }
}

fn random() -> String {
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |d| d.subsec_nanos());
    format!("{:x}{:x}", std::process::id(), nanos)
}

/// The digest in a `sha256sum` line; its file name, when it has one, must be
/// the asset.
fn expected_digest(text: &str) -> Result<String, String> {
    let mut fields = text.split_whitespace();
    let digest = fields.next().unwrap_or_default().to_ascii_lowercase();
    if digest.len() != 64 || !digest.bytes().all(|b| b.is_ascii_hexdigit()) {
        return Err("the checksum file holds no SHA-256 digest".into());
    }
    match fields.next().map(|name| name.trim_start_matches('*')) {
        Some(name) if name != ASSET => Err(format!("the checksum file names {name}")),
        _ => Ok(digest),
    }
}

/// Runs `<binary> --version` for at most 15 s.
fn version_of(binary: &Path) -> Result<String, String> {
    let mut child = Command::new(binary)
        .arg("--version")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|e| format!("cannot start the downloaded binary: {e}"))?;
    let deadline = Instant::now() + VERSION_TIMEOUT;
    loop {
        match child.try_wait() {
            Ok(Some(_)) => break,
            Ok(None) if Instant::now() < deadline => {
                std::thread::sleep(Duration::from_millis(20));
            }
            Ok(None) => {
                let _ = child.kill();
                let _ = child.wait();
                return Err("the downloaded binary did not answer --version in 15 s".into());
            }
            Err(e) => return Err(format!("cannot wait for the downloaded binary: {e}")),
        }
    }
    let output = child.wait_with_output().map_err(|e| e.to_string())?;
    if !output.status.success() {
        return Err("the downloaded binary failed on --version".into());
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

/// Replaces `target` with the release binary, or leaves it as it is.
pub async fn install(api: &Api, release: &Release, target: &Path) -> Result<(), String> {
    let directory = target.parent().ok_or("the binary has no directory")?;
    let text = api.bytes(&release.checksum, 4096).await?;
    let expected = expected_digest(&String::from_utf8_lossy(&text))?;

    let staged = Staged(directory.join(format!(".oxabl-install-{}", random())));
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&staged.0)
        .map_err(|e| format!("cannot write in {}: {e}", directory.display()))?;
    let mut response = api.get(&release.binary).await?;
    let mut hasher = Sha256::new();
    let mut size = 0u64;
    while let Some(chunk) = response
        .chunk()
        .await
        .map_err(|e| format!("cannot download {}: {e}", release.binary))?
    {
        size += chunk.len() as u64;
        if size > MAX_BINARY {
            return Err("the download is too large".into());
        }
        hasher.update(&chunk);
        file.write_all(&chunk)
            .map_err(|e| format!("cannot write {}: {e}", staged.0.display()))?;
    }
    file.sync_all().map_err(|e| e.to_string())?;
    drop(file);
    let digest: String = hasher
        .finalize()
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect();
    if digest != expected {
        return Err("the SHA-256 of the download does not match the release".into());
    }
    std::fs::set_permissions(&staged.0, std::fs::Permissions::from_mode(0o755))
        .map_err(|e| e.to_string())?;
    let said = version_of(&staged.0)?;
    let wanted = format!("oxabl {}", release.version);
    if said != wanted {
        return Err(format!(
            "the downloaded binary says {said:?}, not {wanted:?}"
        ));
    }
    std::fs::rename(&staged.0, target)
        .map_err(|e| format!("cannot replace {}: {e}", target.display()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_checksum_line_must_hold_a_digest_for_the_asset() {
        let digest = "ab".repeat(32);
        assert_eq!(
            expected_digest(&format!("{digest}  {ASSET}\n")),
            Ok(digest.clone())
        );
        assert_eq!(expected_digest(&digest), Ok(digest.clone()));
        assert!(expected_digest(&format!("{digest}  other")).is_err());
        assert!(expected_digest("abc").is_err());
        assert!(expected_digest("").is_err());
    }
}
