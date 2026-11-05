//! Identities and the roster of a cache, set up with the real `oxabl-cache`
//! member command line the way a cache owner does it.

use std::ffi::OsStr;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::Command;

use serde_json::Value;
use tempfile::TempDir;

use super::binaries;

fn write_secret(path: &Path, content: &str) {
    fs::write(path, content).expect("a secret file");
    fs::set_permissions(path, fs::Permissions::from_mode(0o600)).expect("mode");
}

fn run(arguments: &[&OsStr]) -> Value {
    let output = Command::new(binaries::member_cli())
        .env_remove("OXABL_CACHE_TOKEN")
        .env_remove("OXABL_CACHE_IDENTITY")
        .env_remove("OXABL_CACHE_IDENTITY_PASSPHRASE")
        .arg("--json")
        .args(arguments)
        .output()
        .expect("run the member command line");
    assert!(
        output.status.success(),
        "oxabl-cache {arguments:?}: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).unwrap_or_else(|e| {
        panic!(
            "oxabl-cache {arguments:?} printed no JSON ({e}): {}",
            String::from_utf8_lossy(&output.stdout)
        )
    })
}

fn text<'a>(value: &'a Value, key: &str) -> &'a str {
    value[key]
        .as_str()
        .unwrap_or_else(|| panic!("no `{key}` in {value}"))
}

/// A member of a cache: its identity string and its public signing key.
pub struct Member {
    pub label: &'static str,
    pub role: &'static str,
    pub identity_string: String,
    descriptor: String,
    key: String,
}

impl Member {
    /// A member whose identity string lives in a CI secret store.
    fn new(dir: &Path, label: &'static str, role: &'static str) -> Member {
        let secret = dir.join(format!("{label}.secret"));
        let created = run(&[
            OsStr::new("identity"),
            OsStr::new("new"),
            OsStr::new("--secret-out"),
            secret.as_os_str(),
        ]);
        Member {
            label,
            role,
            identity_string: fs::read_to_string(&secret)
                .expect("an identity string")
                .trim()
                .to_string(),
            descriptor: text(&created, "descriptor").to_string(),
            key: text(&created, "signing_key").to_string(),
        }
    }

    /// `ed25519:<base64>`: how `.oxabl/cache.toml` and the signer of an entry
    /// spell a public key.
    pub fn key_text(&self) -> &str {
        &self.key
    }
}

/// The owner of a cache and the files its command line reads.
pub struct Owner {
    dir: TempDir,
    pub key: String,
    pub key_hex: String,
}

impl Owner {
    pub fn new() -> Owner {
        let dir = TempDir::new().expect("an owner directory");
        let passphrase = dir.path().join("passphrase");
        let identity = dir.path().join("identity");
        write_secret(&passphrase, "owner passphrase\n");
        let created = run(&[
            OsStr::new("--passphrase-file"),
            passphrase.as_os_str(),
            OsStr::new("identity"),
            OsStr::new("new"),
            OsStr::new("--file"),
            identity.as_os_str(),
        ]);
        Owner {
            key: text(&created, "signing_key").to_string(),
            key_hex: text(&created, "signing_key_hex").to_string(),
            dir,
        }
    }

    fn path(&self, name: &str) -> PathBuf {
        self.dir.path().join(name)
    }

    fn command(&self, arguments: &[&str]) -> Value {
        let files = [
            ("--identity-file", self.path("identity")),
            ("--passphrase-file", self.path("passphrase")),
            ("--token-file", self.path("token")),
            ("--state-dir", self.path("state")),
            ("--repo", self.path("repo")),
        ];
        let mut all: Vec<&OsStr> = Vec::new();
        for (flag, path) in &files {
            all.push(OsStr::new(flag));
            all.push(path.as_os_str());
        }
        all.extend(arguments.iter().map(OsStr::new));
        run(&all)
    }

    /// Publishes the first roster of `cache` with this owner alone, using
    /// `token` (a `cache:admin` token), and keeps the repository that pins it.
    pub fn init(&self, server: &str, tenant: &str, cache: &str, token: &str) {
        write_secret(&self.path("token"), &format!("{token}\n"));
        let initial = self.command(&[
            "init", "--server", server, "--tenant", tenant, "--cache", cache, "--label", "owner",
        ]);
        let toml = self.path("repo/.oxabl/cache.toml");
        fs::create_dir_all(toml.parent().expect("a directory")).expect("a repository");
        fs::write(&toml, text(&initial, "cache_toml")).expect("cache.toml");
    }

    /// A new member, added to the roster in the current epoch.
    pub fn add_member(&self, label: &'static str, role: &'static str) -> Member {
        let member = Member::new(self.dir.path(), label, role);
        self.command(&[
            "member",
            "add",
            &member.descriptor,
            "--label",
            label,
            "--role",
            role,
        ]);
        member
    }
}
