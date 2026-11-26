//! Black-box tests of `cargo xtask lost-content` on synthetic repositories.

use std::path::PathBuf;
use std::process::{Command, Output};

struct Repo(PathBuf);

impl Repo {
    fn new(name: &str) -> Repo {
        let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(name);
        std::fs::remove_dir_all(&dir).ok();
        std::fs::create_dir_all(&dir).expect("create repo dir");
        let repo = Repo(dir);
        repo.git(&["init", "-q", "-b", "master"]);
        repo.git(&["config", "user.email", "t@example.com"]);
        repo.git(&["config", "user.name", "t"]);
        repo.git(&["config", "commit.gpgsign", "false"]);
        repo
    }

    fn git(&self, arguments: &[&str]) -> String {
        let output = Command::new("git")
            .args(arguments)
            .current_dir(&self.0)
            .output()
            .expect("run git");
        assert!(
            output.status.success(),
            "git {arguments:?}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        String::from_utf8_lossy(&output.stdout).trim().to_string()
    }

    fn commit(&self, file: &str, text: &str) {
        std::fs::write(self.0.join(file), text).expect("write file");
        self.git(&["add", file]);
        self.git(&["commit", "-q", "-m", "change"]);
    }

    fn lost_content(&self, arguments: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_xtask"))
            .arg("lost-content")
            .args(arguments)
            .current_dir(&self.0)
            .output()
            .expect("run xtask")
    }
}

impl Drop for Repo {
    fn drop(&mut self) {
        std::fs::remove_dir_all(&self.0).ok();
    }
}

fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

const START: &str = "one\ntwo\nthree\nfour\nfive\n";

#[test]
fn a_clean_rebase_passes() {
    let repo = Repo::new("lost-clean");
    repo.commit("doc.md", START);
    repo.git(&["checkout", "-q", "-b", "topic"]);
    repo.commit("doc.md", "one\ntwo\nthree\nfour\nfive\nsix\n");
    repo.git(&["checkout", "-q", "master"]);
    repo.commit("other.md", "other\n");
    repo.git(&["checkout", "-q", "topic"]);
    let old = repo.git(&["rev-parse", "HEAD"]);
    repo.git(&["rebase", "-q", "master"]);
    let output = repo.lost_content(&[&old, "HEAD", "--base", "master"]);
    assert!(output.status.success(), "{}", stdout(&output));
}

#[test]
fn a_conflict_resolved_by_dropping_the_other_side_fails() {
    let repo = Repo::new("lost-dropped");
    repo.commit("doc.md", START);
    repo.git(&["checkout", "-q", "-b", "topic"]);
    repo.commit("doc.md", "one\ntwo\nthree changed by topic\nfour\nfive\n");
    repo.git(&["checkout", "-q", "master"]);
    repo.commit(
        "doc.md",
        "one\ntwo\nthree\nfour with an added sentence\nfive\n",
    );
    repo.git(&["checkout", "-q", "topic"]);
    let old = repo.git(&["rev-parse", "HEAD"]);
    // Resolve as if the other side's line were never there: keep only the topic's text.
    repo.git(&["checkout", "-q", "-B", "rebased", "master"]);
    repo.commit("doc.md", "one\ntwo\nthree changed by topic\nfour\nfive\n");
    let output = repo.lost_content(&[&old, "HEAD", "--base", "master"]);
    assert!(!output.status.success());
    let text = stdout(&output);
    assert!(text.contains("doc.md:4:"), "{text}");
    assert!(text.contains("sentence"), "{text}");
}

#[test]
fn a_deletion_the_branch_made_itself_passes() {
    let repo = Repo::new("lost-own-deletion");
    repo.commit("doc.md", START);
    repo.git(&["checkout", "-q", "-b", "topic"]);
    repo.commit("doc.md", "one\ntwo\nfour\nfive\n");
    repo.git(&["checkout", "-q", "master"]);
    repo.commit("other.md", "other\n");
    repo.git(&["checkout", "-q", "topic"]);
    let old = repo.git(&["rev-parse", "HEAD"]);
    repo.git(&["rebase", "-q", "master"]);
    let output = repo.lost_content(&[&old, "HEAD", "--base", "master"]);
    assert!(output.status.success(), "{}", stdout(&output));
}

#[test]
fn a_bad_ref_fails_closed() {
    let repo = Repo::new("lost-bad-ref");
    repo.commit("doc.md", START);
    let output = repo.lost_content(&["nope", "HEAD", "--base", "master"]);
    assert_eq!(output.status.code(), Some(2));
}
