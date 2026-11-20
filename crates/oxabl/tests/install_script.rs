//! `scripts/install.sh` against a fake GitHub on a loopback port. The "binary"
//! is a shell script that prints its version. `OXABL_TEST_SH` names the shell
//! (CI runs it with dash).

mod common;

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use common::fake_github::{ASSET, Server};
use tempfile::TempDir;

fn script() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../scripts/install.sh")
        .canonicalize()
        .unwrap()
}

fn binary(version: &str) -> Vec<u8> {
    format!("#!/bin/sh\necho 'oxabl {version}'\n").into_bytes()
}

struct Fixture {
    dir: TempDir,
    server: Server,
}

impl Fixture {
    fn new() -> Fixture {
        Fixture {
            dir: TempDir::new().unwrap(),
            server: Server::start(),
        }
    }

    fn bin_dir(&self) -> PathBuf {
        self.dir.path().join("bin")
    }

    fn target(&self) -> PathBuf {
        self.bin_dir().join("oxabl")
    }

    fn record(&self) -> PathBuf {
        self.dir.path().join("data/oxabl/install.json")
    }

    fn install(&self, extra: &[(&str, &str)]) -> Output {
        let shell = std::env::var("OXABL_TEST_SH").unwrap_or_else(|_| "sh".into());
        let mut command = Command::new(shell);
        command
            .arg(script())
            .env("HOME", self.dir.path().join("home"))
            .env("OXABL_INSTALL_DIR", self.bin_dir())
            .env("XDG_DATA_HOME", self.dir.path().join("data"))
            .env("OXABL_UPDATE_API_URL", self.server.url())
            .env("TMPDIR", self.dir.path())
            .env_remove("OXABL_VERSION");
        for (name, value) in extra {
            command.env(name, value);
        }
        command.output().unwrap()
    }

    /// Files in the install directory, the temporary files included.
    fn installed_files(&self) -> Vec<String> {
        std::fs::read_dir(self.bin_dir())
            .map(|entries| {
                entries
                    .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
                    .collect()
            })
            .unwrap_or_default()
    }

    fn version_of_installed(&self) -> String {
        let output = Command::new(self.target())
            .arg("--version")
            .output()
            .unwrap();
        String::from_utf8_lossy(&output.stdout).trim().to_string()
    }
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

#[test]
fn it_installs_the_newest_release_and_writes_the_record() {
    let fixture = Fixture::new();
    fixture.server.release("99.1.0", &binary("99.1.0"), None);

    let output = fixture.install(&[]);

    assert!(output.status.success(), "{}", text(&output.stderr));
    assert_eq!(fixture.version_of_installed(), "oxabl 99.1.0");
    assert_eq!(fixture.installed_files(), ["oxabl"]);
    let mode = std::os::unix::fs::PermissionsExt::mode(
        &std::fs::metadata(fixture.target()).unwrap().permissions(),
    );
    assert_eq!(mode & 0o777, 0o755);
    assert_eq!(
        std::fs::read_to_string(fixture.record()).unwrap(),
        format!(
            "{{\"format\":1,\"installer\":\"install.sh\",\"path\":\"{}\",\"version\":\"99.1.0\"}}\n",
            fixture.target().canonicalize().unwrap().display()
        )
    );
    assert!(
        text(&output.stdout).contains("not on your PATH"),
        "{}",
        text(&output.stdout)
    );
}

#[test]
fn a_bad_checksum_installs_nothing() {
    let fixture = Fixture::new();
    fixture
        .server
        .release("99.1.0", &binary("99.1.0"), Some(&"0".repeat(64)));

    let output = fixture.install(&[]);

    assert!(!output.status.success());
    assert!(
        text(&output.stderr).contains("SHA-256"),
        "{}",
        text(&output.stderr)
    );
    assert!(
        fixture.installed_files().is_empty(),
        "{:?}",
        fixture.installed_files()
    );
    assert!(!fixture.record().exists());
}

#[test]
fn a_download_that_names_another_version_installs_nothing() {
    let fixture = Fixture::new();
    fixture.server.release("99.1.0", &binary("99.9.9"), None);

    let output = fixture.install(&[]);

    assert!(!output.status.success());
    assert!(fixture.installed_files().is_empty());
    assert!(!fixture.record().exists());
}

#[test]
fn an_incomplete_release_installs_nothing() {
    let fixture = Fixture::new();
    fixture.server.release("99.1.0", &binary("99.1.0"), None);
    fixture
        .server
        .release_json("99.1.0", false, false, &[ASSET]);

    let output = fixture.install(&[]);

    assert!(!output.status.success());
    assert!(
        text(&output.stderr).contains("incomplete"),
        "{}",
        text(&output.stderr)
    );
    assert!(fixture.installed_files().is_empty());
}

#[test]
fn a_second_run_updates_the_binary_in_place() {
    let fixture = Fixture::new();
    fixture.server.release("99.1.0", &binary("99.1.0"), None);
    assert!(fixture.install(&[]).status.success());
    fixture.server.release("99.2.0", &binary("99.2.0"), None);

    let output = fixture.install(&[]);

    assert!(output.status.success(), "{}", text(&output.stderr));
    assert_eq!(fixture.version_of_installed(), "oxabl 99.2.0");
    assert_eq!(fixture.installed_files(), ["oxabl"]);
    assert!(
        std::fs::read_to_string(fixture.record())
            .unwrap()
            .contains("\"version\":\"99.2.0\"")
    );
}

#[test]
fn a_failed_second_run_keeps_the_first_binary() {
    let fixture = Fixture::new();
    fixture.server.release("99.1.0", &binary("99.1.0"), None);
    assert!(fixture.install(&[]).status.success());
    fixture
        .server
        .release("99.2.0", &binary("99.2.0"), Some(&"f".repeat(64)));

    let output = fixture.install(&[]);

    assert!(!output.status.success());
    assert_eq!(fixture.version_of_installed(), "oxabl 99.1.0");
    assert_eq!(fixture.installed_files(), ["oxabl"]);
    assert!(
        std::fs::read_to_string(fixture.record())
            .unwrap()
            .contains("\"version\":\"99.1.0\"")
    );
}

#[test]
fn a_pinned_version_wins_over_the_newest() {
    let fixture = Fixture::new();
    fixture.server.release("99.2.0", &binary("99.2.0"), None);
    // The pinned release is complete; the newest tag is not looked at.
    fixture.server.release("99.1.0", &binary("99.1.0"), None);
    fixture
        .server
        .tags(&["refs/tags/oxabl-v99.1.0", "refs/tags/oxabl-v99.2.0"]);

    let output = fixture.install(&[("OXABL_VERSION", "99.1.0")]);

    assert!(output.status.success(), "{}", text(&output.stderr));
    assert_eq!(fixture.version_of_installed(), "oxabl 99.1.0");
}

#[test]
fn the_newest_plain_version_is_found_across_pages_and_pre_release_tags() {
    let fixture = Fixture::new();
    fixture.server.release("99.10.0", &binary("99.10.0"), None);
    let second = format!("{}/page2", fixture.server.url());
    fixture.server.route(
        "/repos/oxabl-project/oxabl/git/matching-refs/tags/oxabl-v",
        common::fake_github::Reply::Linked(
            b"[{\"ref\":\"refs/tags/oxabl-v99.9.0\"},{\"ref\":\"refs/tags/oxabl-v100.0.0-rc1\"}]"
                .to_vec(),
            format!("<{second}>; rel=\"next\""),
        ),
    );
    fixture.server.route(
        "/page2",
        common::fake_github::Reply::Ok(b"[{\"ref\":\"refs/tags/oxabl-v99.10.0\"}]".to_vec()),
    );

    let output = fixture.install(&[]);

    assert!(output.status.success(), "{}", text(&output.stderr));
    assert_eq!(fixture.version_of_installed(), "oxabl 99.10.0");
}
