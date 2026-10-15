//! End-to-end tests for `oxabl check --watch`: edit a file in a temp tree and
//! wait for the new finding on the running process's stdout.

use std::fs;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::time::{Duration, Instant};

use tempfile::TempDir;

const TIMEOUT: Duration = Duration::from_secs(30);

struct Watch {
    child: Child,
    lines: Receiver<String>,
}

impl Watch {
    fn start(dir: &Path, args: &[&str]) -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_oxabl"))
            .args(["check", "--watch", "--no-format"])
            .args(args)
            .arg(dir)
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .unwrap();
        let stdout = child.stdout.take().unwrap();
        let (sender, lines) = mpsc::channel();
        std::thread::spawn(move || {
            for line in BufReader::new(stdout).lines().map_while(Result::ok) {
                if sender.send(line).is_err() {
                    break;
                }
            }
        });
        Watch { child, lines }
    }

    /// Read lines until one contains `needle`, returning every line read.
    fn wait_for(&self, needle: &str) -> Vec<String> {
        let deadline = Instant::now() + TIMEOUT;
        let mut seen = Vec::new();
        loop {
            let left = deadline.saturating_duration_since(Instant::now());
            match self.lines.recv_timeout(left) {
                Ok(line) => {
                    let hit = line.contains(needle);
                    seen.push(line);
                    if hit {
                        return seen;
                    }
                }
                Err(_) => panic!("timed out waiting for `{needle}`; saw {seen:#?}"),
            }
        }
    }
}

impl Drop for Watch {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

#[test]
fn edit_shows_the_new_finding_only() {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("clean.p"), "MESSAGE \"hi\".\n").unwrap();
    let edited = tmp.path().join("edited.p");
    fs::write(&edited, "MESSAGE \"hi\".\n").unwrap();

    let watch = Watch::start(tmp.path(), &[]);
    let text = watch.wait_for("watching").join("\n");
    assert!(text.contains("checked 2 files: 0 with findings"), "{text}");

    fs::write(&edited, "DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n").unwrap();
    let lines = watch.wait_for("rechecked");
    let text = lines.join("\n");
    assert!(text.contains("v-total"), "{text}");
    assert!(!text.contains("clean.p"), "{text}");
    assert!(text.contains("rechecked 1 file: 1 changed"), "{text}");

    fs::write(&edited, "MESSAGE \"hi\".\n").unwrap();
    let text = watch.wait_for("rechecked").join("\n");
    assert!(text.contains("edited.p: no findings"), "{text}");
}

#[test]
fn include_edit_rechecks_its_includer() {
    let tmp = TempDir::new().unwrap();
    let include = tmp.path().join("vars.i");
    fs::write(&include, "DEFINE VARIABLE v-shared AS INTEGER NO-UNDO.\n").unwrap();
    fs::write(tmp.path().join("user.p"), "{vars.i}\nMESSAGE v-shared.\n").unwrap();
    fs::write(tmp.path().join("other.p"), "MESSAGE \"hi\".\n").unwrap();

    let include_path = tmp.path().to_string_lossy().into_owned();
    let watch = Watch::start(tmp.path(), &["-I", &include_path]);
    watch.wait_for("watching");

    fs::write(&include, "DEFINE VARIABLE v-other AS INTEGER NO-UNDO.\n").unwrap();
    let text = watch.wait_for("rechecked").join("\n");
    assert!(text.contains("v-shared"), "{text}");
    assert!(text.contains("user.p"), "{text}");
    assert!(!text.contains("other.p"), "{text}");
}

#[test]
fn callee_summaries_are_shared_and_rebuilt() {
    let tmp = TempDir::new().unwrap();
    fs::write(
        tmp.path().join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n\n[workspace.lint]\nenable = [\"strict.transactions\"]\n",
    )
    .unwrap();
    // One worker, and the caller sorted after its callee, so a worker that
    // built a summary from its first file would judge the second with it.
    let callee = tmp.path().join("a.p");
    fs::write(&callee, "MESSAGE \"hi\".\n").unwrap();
    let caller = tmp.path().join("z.p");
    fs::write(&caller, "DO TRANSACTION:\nRUN a.p.\nEND.\n").unwrap();

    let include_path = tmp.path().to_string_lossy().into_owned();
    let watch = Watch::start(tmp.path(), &["-j", "1", "-I", &include_path]);
    let text = watch.wait_for("watching").join("\n");
    assert!(text.contains("checked 2 files: 0 with findings"), "{text}");

    // A callee that now updates the database keeps the finding, so the next
    // pass must rebuild the shared summaries, not reuse the old ones.
    fs::write(&callee, "CREATE Customer.\n").unwrap();
    let text = watch.wait_for("rechecked").join("\n");
    assert!(text.contains("z.p") && text.contains("TX004"), "{text}");
}

#[cfg(unix)]
#[test]
fn ctrl_c_exits_cleanly() {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("a.p"), "MESSAGE \"hi\".\n").unwrap();
    let mut watch = Watch::start(tmp.path(), &[]);
    watch.wait_for("watching");

    let pid = watch.child.id().to_string();
    assert!(
        Command::new("kill")
            .args(["-INT", &pid])
            .status()
            .unwrap()
            .success()
    );
    let deadline = Instant::now() + TIMEOUT;
    loop {
        if let Some(status) = watch.child.try_wait().unwrap() {
            assert!(status.success(), "{status:?}");
            break;
        }
        assert!(Instant::now() < deadline, "did not exit on SIGINT");
        std::thread::sleep(Duration::from_millis(50));
    }
}
