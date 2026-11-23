//! `oxabl self-update` and the background check, run as a binary against a
//! fake GitHub on a loopback port. The "new binary" is a shell script that
//! prints its version.

mod common;

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::time::{Duration, Instant};

use common::fake_github::{ASSET, INSTALLED, Reply, Server};
use tempfile::TempDir;

const NEWER: &str = "99.1.0";

/// A copy of the binary in a directory of its own, with its own XDG
/// directories, so no test touches the binary cargo built.
struct Fixture {
    dir: TempDir,
    server: Server,
}

impl Fixture {
    fn new() -> Fixture {
        let dir = TempDir::new().unwrap();
        std::fs::create_dir_all(dir.path().join("bin")).unwrap();
        let target = dir.path().join("bin/oxabl");
        let built = env!("CARGO_BIN_EXE_oxabl");
        if std::fs::hard_link(built, &target).is_err() {
            std::fs::copy(built, &target).unwrap();
        }
        Fixture {
            dir,
            server: Server::start(),
        }
    }

    fn binary(&self) -> PathBuf {
        self.dir.path().join("bin/oxabl").canonicalize().unwrap()
    }

    fn data(&self) -> PathBuf {
        self.dir.path().join("data/oxabl")
    }

    fn state(&self) -> PathBuf {
        self.dir.path().join("state/oxabl")
    }

    /// The install record as the installer script writes it.
    fn manage(&self) {
        std::fs::create_dir_all(self.data()).unwrap();
        std::fs::write(
            self.data().join("install.json"),
            format!(
                "{{\"format\":1,\"installer\":\"install.sh\",\"path\":{:?},\"version\":\"{INSTALLED}\"}}",
                self.binary().display().to_string()
            ),
        )
        .unwrap();
    }

    fn record(&self) -> serde_json::Value {
        let text = std::fs::read_to_string(self.data().join("install.json")).unwrap();
        serde_json::from_str(&text).unwrap()
    }

    fn command(&self, args: &[&str]) -> Command {
        let mut command = Command::new(self.dir.path().join("bin/oxabl"));
        command
            .args(args)
            .env("XDG_DATA_HOME", self.dir.path().join("data"))
            .env("XDG_STATE_HOME", self.dir.path().join("state"))
            .env("OXABL_UPDATE_API_URL", self.server.url())
            .env_remove("CI")
            .env_remove("OXABL_NO_SELF_UPDATE");
        command
    }

    fn run(&self, args: &[&str]) -> Output {
        self.command(args).output().unwrap()
    }

    /// The installed bytes.
    fn installed_bytes(&self) -> Vec<u8> {
        std::fs::read(self.dir.path().join("bin/oxabl")).unwrap()
    }

    fn leftovers(&self) -> Vec<String> {
        std::fs::read_dir(self.dir.path().join("bin"))
            .unwrap()
            .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
            .filter(|name| name != "oxabl")
            .collect()
    }
}

fn script(version: &str) -> Vec<u8> {
    format!("#!/bin/sh\necho 'oxabl {version}'\n").into_bytes()
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

fn wait_until(what: &str, condition: impl Fn() -> bool) {
    let deadline = Instant::now() + Duration::from_secs(30);
    while !condition() {
        assert!(Instant::now() < deadline, "timed out waiting for {what}");
        std::thread::sleep(Duration::from_millis(50));
    }
}

fn unchanged(fixture: &Fixture, before: &[u8]) {
    assert_eq!(fixture.installed_bytes(), before, "the binary changed");
    assert!(fixture.leftovers().is_empty(), "{:?}", fixture.leftovers());
}

#[test]
fn an_update_installs_the_release_and_rewrites_the_record() {
    let fixture = Fixture::new();
    fixture.manage();
    fixture.server.release(NEWER, &script(NEWER), None);

    let output = fixture.run(&["self-update"]);

    assert_eq!(output.status.code(), Some(0), "{}", text(&output.stderr));
    assert_eq!(
        text(&output.stdout).trim(),
        format!("oxabl was updated from {INSTALLED} to {NEWER}")
    );
    assert_eq!(fixture.installed_bytes(), script(NEWER));
    assert!(fixture.leftovers().is_empty());
    let version = fixture.run(&["--version"]);
    assert_eq!(text(&version.stdout).trim(), format!("oxabl {NEWER}"));
    assert_eq!(
        fixture.record(),
        serde_json::json!({
            "format": 1,
            "installer": "self-update",
            "path": fixture.binary().display().to_string(),
            "version": NEWER,
        })
    );
}

#[test]
fn the_newest_plain_version_wins_over_older_and_pre_release_tags() {
    let fixture = Fixture::new();
    fixture.manage();
    fixture.server.release("99.2.0", &script("99.2.0"), None);
    fixture.server.tags(&[
        "refs/tags/oxabl-v99.2.0",
        "refs/tags/oxabl-v99.10.0-rc1",
        "refs/tags/oxabl-v99.3",
        "refs/tags/oxabl-v7.0.0",
    ]);

    let output = fixture.run(&["self-update"]);

    assert_eq!(output.status.code(), Some(0), "{}", text(&output.stderr));
    assert_eq!(fixture.installed_bytes(), script("99.2.0"));
}

#[test]
fn the_tag_list_is_followed_through_its_next_pages() {
    let fixture = Fixture::new();
    fixture.manage();
    fixture.server.release("99.2.0", &script("99.2.0"), None);
    let second = format!("{}/page2", fixture.server.url());
    fixture.server.route(
        "/repos/oxabl-project/oxabl/git/matching-refs/tags/oxabl-v",
        Reply::Linked(
            b"[{\"ref\":\"refs/tags/oxabl-v1.0.0\"}]".to_vec(),
            format!("<{second}>; rel=\"next\""),
        ),
    );
    fixture.server.route(
        "/page2",
        Reply::Ok(b"[{\"ref\":\"refs/tags/oxabl-v99.2.0\"}]".to_vec()),
    );

    let output = fixture.run(&["self-update"]);

    assert_eq!(output.status.code(), Some(0), "{}", text(&output.stderr));
    assert_eq!(fixture.installed_bytes(), script("99.2.0"));
}

#[test]
fn a_release_that_is_not_newer_is_up_to_date() {
    let fixture = Fixture::new();
    fixture.manage();
    fixture.server.release(INSTALLED, &script(INSTALLED), None);
    let before = fixture.installed_bytes();

    let output = fixture.run(&["self-update"]);

    assert_eq!(output.status.code(), Some(0));
    assert_eq!(
        text(&output.stdout).trim(),
        format!("oxabl is up to date ({INSTALLED})")
    );
    unchanged(&fixture, &before);
}

#[test]
fn a_checksum_mismatch_keeps_the_old_binary() {
    let fixture = Fixture::new();
    fixture.manage();
    fixture
        .server
        .release(NEWER, &script(NEWER), Some(&"0".repeat(64)));
    let before = fixture.installed_bytes();

    let output = fixture.run(&["self-update"]);

    assert_eq!(output.status.code(), Some(3), "{}", text(&output.stdout));
    assert!(
        text(&output.stderr).contains("SHA-256"),
        "{}",
        text(&output.stderr)
    );
    unchanged(&fixture, &before);
    assert_eq!(fixture.record()["version"], INSTALLED);
}

#[test]
fn a_download_whose_version_check_fails_keeps_the_old_binary() {
    let fixture = Fixture::new();
    fixture.manage();
    fixture.server.release(NEWER, b"#!/bin/sh\nexit 1\n", None);
    let before = fixture.installed_bytes();

    let output = fixture.run(&["self-update"]);

    assert_eq!(output.status.code(), Some(3), "{}", text(&output.stdout));
    unchanged(&fixture, &before);
}

#[test]
fn a_download_that_names_another_version_keeps_the_old_binary() {
    let fixture = Fixture::new();
    fixture.manage();
    fixture.server.release(NEWER, &script("99.9.9"), None);
    let before = fixture.installed_bytes();

    let output = fixture.run(&["self-update"]);

    assert_eq!(output.status.code(), Some(3), "{}", text(&output.stdout));
    assert!(
        text(&output.stderr).contains("99.9.9"),
        "{}",
        text(&output.stderr)
    );
    unchanged(&fixture, &before);
}

#[test]
fn an_incomplete_release_is_not_established_and_not_up_to_date() {
    for (draft, prerelease, assets) in [
        (false, false, vec![ASSET]),
        (
            true,
            false,
            vec![ASSET, "oxabl-x86_64-unknown-linux-musl.sha256"],
        ),
        (
            false,
            true,
            vec![ASSET, "oxabl-x86_64-unknown-linux-musl.sha256"],
        ),
    ] {
        let fixture = Fixture::new();
        fixture.manage();
        fixture.server.release(NEWER, &script(NEWER), None);
        fixture
            .server
            .release_json(NEWER, draft, prerelease, &assets);
        let before = fixture.installed_bytes();

        let output = fixture.run(&["self-update"]);

        assert_eq!(
            output.status.code(),
            Some(3),
            "{draft} {prerelease} {assets:?}"
        );
        assert!(!text(&output.stdout).contains("up to date"));
        unchanged(&fixture, &before);

        let checked = fixture.run(&["self-update", "--check", "--json"]);
        assert_eq!(checked.status.code(), Some(3));
        assert!(checked.stdout.is_empty());
    }
}

#[test]
fn an_unreachable_api_is_not_established() {
    let fixture = Fixture::new();
    fixture.manage();

    let output = fixture.run(&["self-update"]);

    assert_eq!(output.status.code(), Some(3));
    assert!(!text(&output.stdout).contains("up to date"));
}

#[test]
fn an_unmanaged_binary_exits_2_and_downloads_nothing() {
    let fixture = Fixture::new();
    fixture.server.release(NEWER, &script(NEWER), None);
    let before = fixture.installed_bytes();

    let without_record = fixture.run(&["self-update"]);
    // A record that names another binary does not manage this one.
    std::fs::create_dir_all(fixture.data()).unwrap();
    std::fs::write(
        fixture.data().join("install.json"),
        "{\"format\":1,\"installer\":\"vscode\",\"path\":\"/nonexistent/oxabl\",\"version\":\"1.0.0\"}",
    )
    .unwrap();
    let other_record = fixture.run(&["self-update"]);

    for output in [without_record, other_record] {
        assert_eq!(output.status.code(), Some(2), "{}", text(&output.stderr));
        let stderr = text(&output.stderr);
        assert!(stderr.contains("install.json"), "{stderr}");
        assert!(
            stderr.contains(&fixture.binary().display().to_string()),
            "{stderr}"
        );
    }
    assert!(
        fixture.server.requests().is_empty(),
        "{:?}",
        fixture.server.requests()
    );
    unchanged(&fixture, &before);
}

#[test]
fn check_json_has_the_documented_shape_and_installs_nothing() {
    let fixture = Fixture::new();
    fixture.manage();
    fixture.server.release(NEWER, &script(NEWER), None);
    let before = fixture.installed_bytes();

    let output = fixture.run(&["self-update", "--check", "--json"]);

    assert_eq!(output.status.code(), Some(0), "{}", text(&output.stderr));
    assert_eq!(
        text(&output.stdout).trim(),
        format!(
            "{{\"installed\":\"{INSTALLED}\",\"latest\":\"{NEWER}\",\"update\":true,\"managed\":true}}"
        )
    );
    unchanged(&fixture, &before);
    assert!(
        !fixture
            .server
            .requests()
            .iter()
            .any(|r| r.starts_with("/dl/"))
    );

    fixture.server.release(INSTALLED, &script(INSTALLED), None);
    let same = fixture.run(&["self-update", "--check", "--json"]);
    assert_eq!(
        text(&same.stdout).trim(),
        format!(
            "{{\"installed\":\"{INSTALLED}\",\"latest\":\"{INSTALLED}\",\"update\":false,\"managed\":true}}"
        )
    );
}

/// A source file for `oxabl check`: any command that schedules a check.
fn source(directory: &Path) -> PathBuf {
    let file = directory.join("a.p");
    std::fs::write(&file, "MESSAGE \"a\".\n").unwrap();
    file
}

fn quiet_for_a_moment(fixture: &Fixture) {
    std::thread::sleep(Duration::from_millis(1500));
    assert!(
        fixture.server.requests().is_empty(),
        "{:?}",
        fixture.server.requests()
    );
    assert!(!fixture.state().join("update.log").exists());
}

#[test]
fn a_command_of_a_managed_binary_updates_it_in_the_background() {
    let fixture = Fixture::new();
    fixture.manage();
    fixture.server.release(NEWER, &script(NEWER), None);
    let file = source(fixture.dir.path());

    fixture.run(&["check", &file.display().to_string()]);

    wait_until("the update", || fixture.record()["version"] == NEWER);
    assert_eq!(fixture.record()["installer"], "self-update");
    assert_eq!(fixture.installed_bytes(), script(NEWER));
    // The schedule is written after the install record.
    let path = fixture.state().join("update-state.json");
    wait_until("the update state", || path.exists());
    let state: serde_json::Value = serde_json::from_slice(&std::fs::read(&path).unwrap()).unwrap();
    assert_eq!(state["failures"], 0);
    assert!(state["next_due"].as_u64().unwrap() > 23 * 3600);
}

#[test]
fn the_foreground_command_does_not_wait_for_the_network() {
    let fixture = Fixture::new();
    fixture.manage();
    fixture.server.route(
        "/repos/oxabl-project/oxabl/git/matching-refs/tags/oxabl-v",
        Reply::Stall(Duration::from_secs(8)),
    );
    let file = source(fixture.dir.path());

    let started = Instant::now();
    fixture.run(&["check", &file.display().to_string()]);

    assert!(
        started.elapsed() < Duration::from_secs(5),
        "{:?}",
        started.elapsed()
    );
    wait_until("the background request", || {
        !fixture.server.requests().is_empty()
    });
}

#[test]
fn ci_and_the_opt_out_and_an_unmanaged_binary_schedule_nothing() {
    for variable in ["CI", "OXABL_NO_SELF_UPDATE"] {
        let fixture = Fixture::new();
        fixture.manage();
        fixture.server.release(NEWER, &script(NEWER), None);
        let file = source(fixture.dir.path());

        fixture
            .command(&["check", &file.display().to_string()])
            .env(variable, "1")
            .output()
            .unwrap();

        quiet_for_a_moment(&fixture);
    }
    let fixture = Fixture::new();
    fixture.server.release(NEWER, &script(NEWER), None);
    let file = source(fixture.dir.path());

    fixture.run(&["check", &file.display().to_string()]);

    quiet_for_a_moment(&fixture);
}

fn unix_now() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

#[test]
fn a_check_that_is_not_due_is_not_made() {
    let fixture = Fixture::new();
    fixture.manage();
    fixture.server.release(NEWER, &script(NEWER), None);
    std::fs::create_dir_all(fixture.state()).unwrap();
    std::fs::write(
        fixture.state().join("update-state.json"),
        format!("{{\"failures\":0,\"next_due\":{}}}", unix_now() + 3600),
    )
    .unwrap();
    let file = source(fixture.dir.path());

    fixture.run(&["check", &file.display().to_string()]);

    quiet_for_a_moment(&fixture);
}

#[test]
fn a_failed_background_check_backs_off() {
    let fixture = Fixture::new();
    fixture.manage();
    fixture.server.route(
        "/repos/oxabl-project/oxabl/git/matching-refs/tags/oxabl-v",
        Reply::Status(500),
    );
    let file = source(fixture.dir.path());
    let state = fixture.state().join("update-state.json");

    fixture.run(&["check", &file.display().to_string()]);

    wait_until("the failure to be recorded", || state.exists());
    let state: serde_json::Value = serde_json::from_slice(&std::fs::read(state).unwrap()).unwrap();
    assert_eq!(state["failures"], 1);
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    let wait = state["next_due"].as_u64().unwrap().saturating_sub(now);
    assert!((800..=900).contains(&wait), "{wait}");
    assert_eq!(fixture.record()["version"], INSTALLED);
}
