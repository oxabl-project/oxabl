//! `oxabl skill show` and `oxabl skill install`, run as the built binary. Every
//! install runs with its own `HOME` and `XDG_DATA_HOME`.

use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use tempfile::TempDir;

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn run(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .args(args)
        .env_remove("HOME")
        .env_remove("USERPROFILE")
        .env_remove("XDG_DATA_HOME")
        .output()
        .unwrap()
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

fn code(output: &Output) -> i32 {
    output.status.code().unwrap()
}

fn source_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("skill")
}

fn source_files(dir: &Path, prefix: &str, out: &mut Vec<String>) {
    for entry in fs::read_dir(dir).unwrap() {
        let entry = entry.unwrap();
        let name = format!("{prefix}{}", entry.file_name().to_string_lossy());
        if entry.file_type().unwrap().is_dir() {
            source_files(&entry.path(), &format!("{name}/"), out);
        } else {
            out.push(name);
        }
    }
}

fn skill_files() -> Vec<String> {
    let mut files = Vec::new();
    source_files(&source_dir(), "", &mut files);
    files.sort();
    files
}

/// The files under `dir`, relative, sorted; none when it does not exist.
fn tree(dir: &Path) -> Vec<String> {
    let mut files = Vec::new();
    if dir.exists() {
        source_files(dir, "", &mut files);
    }
    files.sort();
    files
}

/// A home directory and a data directory of their own.
struct Env {
    home: TempDir,
    data: TempDir,
}

impl Env {
    fn new() -> Env {
        Env {
            home: TempDir::new().unwrap(),
            data: TempDir::new().unwrap(),
        }
    }

    fn command(&self, args: &[&str]) -> Command {
        let mut command = Command::new(env!("CARGO_BIN_EXE_oxabl"));
        command
            .args(["skill", "install"])
            .args(args)
            .env("HOME", self.home.path())
            .env("XDG_DATA_HOME", self.data.path())
            .env_remove("USERPROFILE");
        command
    }

    fn install(&self, args: &[&str]) -> Output {
        self.command(args).output().unwrap()
    }

    fn store(&self) -> PathBuf {
        self.data.path().join("oxabl/skills/oxabl")
    }

    fn agents(&self) -> PathBuf {
        self.home.path().join(".agents/skills/oxabl")
    }

    fn claude(&self) -> PathBuf {
        self.home.path().join(".claude/skills/oxabl")
    }

    /// Every path under the home and data directories, links not followed.
    fn everything(&self) -> Vec<String> {
        fn walk(dir: &Path, prefix: &str, out: &mut Vec<String>) {
            for entry in fs::read_dir(dir).unwrap() {
                let entry = entry.unwrap();
                let name = format!("{prefix}{}", entry.file_name().to_string_lossy());
                out.push(name.clone());
                if entry.file_type().unwrap().is_dir() {
                    walk(&entry.path(), &format!("{name}/"), out);
                }
            }
        }
        let mut out = Vec::new();
        walk(self.home.path(), "home/", &mut out);
        walk(self.data.path(), "data/", &mut out);
        out.sort();
        out
    }
}

fn assert_installed(env: &Env) {
    assert_eq!(tree(&env.store()), skill_files());
    for link in [env.agents(), env.claude()] {
        assert!(
            fs::symlink_metadata(&link)
                .unwrap()
                .file_type()
                .is_symlink()
        );
        assert_eq!(fs::read_link(&link).unwrap(), env.store());
    }
}

fn backups(dir: &Path) -> Vec<PathBuf> {
    let mut found: Vec<PathBuf> = fs::read_dir(dir)
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| {
            path.file_name()
                .unwrap()
                .to_string_lossy()
                .starts_with("oxabl.bak-")
        })
        .collect();
    found.sort();
    found
}

/// Makes a directory that holds the oxabl skill as a copy, as an install on a
/// system without symlinks leaves it.
fn make_copy(dir: &Path) {
    for name in skill_files() {
        let path = dir.join(&name);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        let body = fs::read_to_string(source_dir().join(&name)).unwrap();
        let body = if name == "SKILL.md" {
            body.replacen(
                "oxabl-version: dev",
                &format!("oxabl-version: {VERSION}"),
                1,
            )
        } else {
            body
        };
        fs::write(path, body).unwrap();
    }
}

#[test]
fn a_fresh_install_makes_the_store_and_two_links_and_check_agrees() {
    let env = Env::new();

    let output = env.install(&[]);

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert_installed(&env);
    let out = text(&output.stdout);
    assert_eq!(out.lines().count(), 3, "{out}");
    for path in [env.store(), env.agents(), env.claude()] {
        assert!(out.contains(&path.display().to_string()), "{out}");
    }
    let skill = fs::read_to_string(env.store().join("SKILL.md")).unwrap();
    assert!(
        skill.contains(&format!("oxabl-version: {VERSION}")),
        "{skill}"
    );
    let check = env.install(&["--check"]);
    assert_eq!(code(&check), 0, "{}", text(&check.stdout));
    assert_eq!(text(&check.stdout).lines().count(), 3);
    let again = env.install(&[]);
    assert_eq!(code(&again), 0);
    assert_eq!(text(&again.stdout).matches("current").count(), 3);
}

#[test]
fn the_store_is_under_xdg_data_home_or_the_default_data_dir() {
    let home = TempDir::new().unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .args(["skill", "install"])
        .env("HOME", home.path())
        .env_remove("XDG_DATA_HOME")
        .env_remove("USERPROFILE")
        .output()
        .unwrap();

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    let store = home.path().join(".local/share/oxabl/skills/oxabl");
    assert_eq!(tree(&store), skill_files());
    assert_eq!(
        fs::read_link(home.path().join(".claude/skills/oxabl")).unwrap(),
        store
    );

    let env = Env::new();
    env.install(&[]);
    assert_eq!(tree(&env.store()), skill_files());
    assert!(!env.home.path().join(".local").exists());
}

#[test]
fn check_with_nothing_installed_fails_and_writes_nothing() {
    let env = Env::new();

    let check = env.install(&["--check"]);

    assert_eq!(code(&check), 1);
    assert_eq!(text(&check.stdout).matches("missing").count(), 3);
    assert!(env.everything().is_empty(), "{:?}", env.everything());
}

#[test]
fn check_names_an_edited_store_file_and_install_fixes_it() {
    let env = Env::new();
    env.install(&[]);
    fs::write(env.store().join("references/rules.md"), "edited\n").unwrap();

    let check = env.install(&["--check"]);

    assert_eq!(code(&check), 1);
    let out = text(&check.stdout);
    assert!(
        out.contains("stale") && out.contains("references/rules.md differs"),
        "{out}"
    );
    assert_eq!(
        fs::read_to_string(env.store().join("references/rules.md")).unwrap(),
        "edited\n",
        "--check wrote"
    );
    let fixed = env.install(&[]);
    assert_eq!(code(&fixed), 0, "{}", text(&fixed.stderr));
    assert!(text(&fixed.stdout).contains("updated"));
    assert_installed(&env);
    assert_eq!(code(&env.install(&["--check"])), 0);
}

#[test]
fn a_file_the_skill_does_not_have_is_stale_and_goes_with_the_next_install() {
    let env = Env::new();
    env.install(&[]);
    fs::write(env.store().join("references/old.md"), "old\n").unwrap();

    let check = env.install(&["--check"]);

    assert_eq!(code(&check), 1);
    assert!(text(&check.stdout).contains("references/old.md is not part of the skill"));
    env.install(&[]);
    assert_installed(&env);
}

/// Puts something foreign at `link` (the entry is made by `make`) and checks
/// that an install refuses it, exit 2, with nothing written anywhere.
fn refused(name: &str, make: impl Fn(&Env, &Path)) {
    let env = Env::new();
    let link = env.claude();
    fs::create_dir_all(link.parent().unwrap()).unwrap();
    make(&env, &link);
    let before = env.everything();

    let output = env.install(&[]);

    assert_eq!(code(&output), 2, "{name}: {}", text(&output.stdout));
    let err = text(&output.stderr);
    assert!(err.contains(&link.display().to_string()), "{name}: {err}");
    assert!(err.contains("nothing was written"), "{name}: {err}");
    assert_eq!(
        env.everything(),
        before,
        "{name}: the refused install wrote"
    );
    assert!(!env.store().exists(), "{name}: the store was written");
    assert!(!env.agents().exists(), "{name}: the other link was written");
    let check = env.install(&["--check"]);
    assert_eq!(code(&check), 1, "{name}");
    assert!(text(&check.stdout).contains("foreign"), "{name}");
}

#[test]
fn a_foreign_directory_file_or_symlink_is_refused_and_nothing_is_written() {
    refused("a directory without SKILL.md", |_, link| {
        fs::create_dir(link).unwrap();
        fs::write(link.join("notes.md"), "mine\n").unwrap();
    });
    refused("a SKILL.md of another skill", |_, link| {
        fs::create_dir(link).unwrap();
        fs::write(
            link.join("SKILL.md"),
            "---\nname: other\ndescription: x\nmetadata:\n  oxabl-version: 1.0.0\n---\n",
        )
        .unwrap();
    });
    refused("a SKILL.md without an oxabl-version", |_, link| {
        fs::create_dir(link).unwrap();
        fs::write(
            link.join("SKILL.md"),
            "---\nname: oxabl\ndescription: mine\n---\n",
        )
        .unwrap();
    });
    refused("a file", |_, link| fs::write(link, "mine\n").unwrap());
    refused("a symlink to another place", |env, link| {
        let elsewhere = env.home.path().join("elsewhere");
        fs::create_dir(&elsewhere).unwrap();
        std::os::unix::fs::symlink(&elsewhere, link).unwrap();
    });
    refused("a dangling symlink", |env, link| {
        std::os::unix::fs::symlink(env.home.path().join("gone"), link).unwrap();
    });
}

#[test]
fn check_fails_for_a_link_that_points_elsewhere_while_the_store_is_current() {
    let env = Env::new();
    env.install(&[]);
    fs::remove_file(env.claude()).unwrap();
    let elsewhere = env.home.path().join("elsewhere");
    fs::create_dir(&elsewhere).unwrap();
    std::os::unix::fs::symlink(&elsewhere, env.claude()).unwrap();

    let check = env.install(&["--check"]);

    assert_eq!(code(&check), 1);
    let out = text(&check.stdout);
    assert!(
        out.lines()
            .any(|line| line.contains(".claude") && line.contains("foreign")),
        "{out}"
    );
}

#[test]
fn check_fails_for_a_missing_link_while_the_store_is_current() {
    let env = Env::new();
    env.install(&[]);
    fs::remove_file(env.agents()).unwrap();

    let check = env.install(&["--check"]);

    assert_eq!(code(&check), 1);
    assert!(text(&check.stdout).contains("missing"));
}

#[test]
fn a_foreign_store_is_refused_too() {
    let env = Env::new();
    fs::create_dir_all(env.store()).unwrap();
    fs::write(env.store().join("notes.md"), "mine\n").unwrap();

    let output = env.install(&[]);

    assert_eq!(code(&output), 2);
    assert!(text(&output.stderr).contains(&env.store().display().to_string()));
    assert_eq!(tree(&env.store()), ["notes.md"]);
    assert!(!env.claude().exists() && !env.agents().exists());
}

#[test]
fn force_renames_the_foreign_entry_beside_it_and_links() {
    let env = Env::new();
    let link = env.claude();
    fs::create_dir_all(&link).unwrap();
    fs::write(link.join("notes.md"), "mine\n").unwrap();
    let elsewhere = env.home.path().join("elsewhere");
    fs::create_dir(&elsewhere).unwrap();
    fs::remove_dir_all(env.agents()).ok();
    fs::create_dir_all(env.agents().parent().unwrap()).unwrap();
    std::os::unix::fs::symlink(&elsewhere, env.agents()).unwrap();

    let output = env.install(&["--force"]);

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert_installed(&env);
    let kept = backups(link.parent().unwrap());
    assert_eq!(kept.len(), 1, "{kept:?}");
    assert_eq!(
        fs::read_to_string(kept[0].join("notes.md")).unwrap(),
        "mine\n"
    );
    let stamp = kept[0].file_name().unwrap().to_string_lossy().into_owned();
    let stamp = stamp.strip_prefix("oxabl.bak-").unwrap();
    assert!(
        stamp.len() == 16
            && stamp.as_bytes()[8] == b'T'
            && stamp.ends_with('Z')
            && stamp[..8].bytes().all(|b| b.is_ascii_digit())
            && stamp[9..15].bytes().all(|b| b.is_ascii_digit()),
        "{stamp}"
    );
    let moved = backups(env.agents().parent().unwrap());
    assert_eq!(moved.len(), 1);
    assert_eq!(fs::read_link(&moved[0]).unwrap(), elsewhere);
    assert!(text(&output.stdout).contains("oxabl.bak-"));
    assert_eq!(code(&env.install(&["--check"])), 0);
}

#[test]
fn force_keeps_a_foreign_store_as_a_backup() {
    let env = Env::new();
    fs::create_dir_all(env.store()).unwrap();
    fs::write(env.store().join("notes.md"), "mine\n").unwrap();

    let output = env.install(&["--force"]);

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert_installed(&env);
    let kept = backups(env.store().parent().unwrap());
    assert_eq!(kept.len(), 1, "{kept:?}");
    assert_eq!(tree(&kept[0]), ["notes.md"]);
}

#[test]
fn refresh_creates_nothing_where_nothing_was() {
    let env = Env::new();

    let output = env.install(&["--refresh"]);

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert_eq!(text(&output.stdout).matches("skipped").count(), 3);
    assert!(env.everything().is_empty(), "{:?}", env.everything());
}

#[test]
fn refresh_updates_an_existing_store_and_leaves_a_removed_link_removed() {
    let env = Env::new();
    env.install(&[]);
    fs::remove_file(env.agents()).unwrap();
    fs::write(env.store().join("references/rules.md"), "stale\n").unwrap();

    let output = env.install(&["--refresh"]);

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert_eq!(tree(&env.store()), skill_files());
    assert_eq!(
        fs::read_to_string(env.store().join("references/rules.md")).unwrap(),
        fs::read_to_string(source_dir().join("references/rules.md")).unwrap()
    );
    assert!(
        fs::symlink_metadata(env.agents()).is_err(),
        "the link came back"
    );
    assert!(
        fs::symlink_metadata(env.claude())
            .unwrap()
            .file_type()
            .is_symlink()
    );
}

#[test]
fn refresh_leaves_a_foreign_entry_alone() {
    let env = Env::new();
    env.install(&[]);
    fs::remove_file(env.claude()).unwrap();
    fs::create_dir(env.claude()).unwrap();
    fs::write(env.claude().join("notes.md"), "mine\n").unwrap();

    let output = env.install(&["--refresh"]);

    assert_eq!(code(&output), 0);
    assert_eq!(tree(&env.claude()), ["notes.md"]);
}

#[test]
fn agent_claude_touches_only_its_link() {
    let env = Env::new();

    let output = env.install(&["--agent", "claude"]);

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert_eq!(text(&output.stdout).lines().count(), 2);
    assert_eq!(tree(&env.store()), skill_files());
    assert!(
        fs::symlink_metadata(env.claude())
            .unwrap()
            .file_type()
            .is_symlink()
    );
    assert!(!env.home.path().join(".agents").exists());
    assert_eq!(code(&env.install(&["--agent", "claude", "--check"])), 0);
    assert_eq!(code(&env.install(&["--agent", "agents", "--check"])), 1);
    let agents = env.install(&["--agent", "agents"]);
    assert_eq!(code(&agents), 0);
    assert_installed(&env);
}

#[test]
fn a_foreign_entry_of_an_unselected_agent_does_not_block() {
    let env = Env::new();
    fs::create_dir_all(env.agents()).unwrap();

    let output = env.install(&["--agent", "claude"]);

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert!(tree(&env.agents()).is_empty());
}

#[test]
fn without_a_home_directory_every_mode_fails_with_two() {
    let data = TempDir::new().unwrap();
    for extra in [&[][..], &["--check"], &["--refresh"]] {
        let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
            .args(["skill", "install"])
            .args(extra)
            .env_remove("HOME")
            .env_remove("USERPROFILE")
            .env("XDG_DATA_HOME", data.path())
            .output()
            .unwrap();

        assert_eq!(code(&output), 2, "{extra:?}");
        assert!(text(&output.stderr).contains("HOME"), "{extra:?}");
    }
    assert_eq!(fs::read_dir(data.path()).unwrap().count(), 0);
}

#[test]
fn dir_and_user_are_gone() {
    let env = Env::new();
    for flag in [&["--user"][..], &["--dir", "."]] {
        let output = env.install(flag);

        assert_eq!(code(&output), 2, "{flag:?}");
        assert!(
            text(&output.stderr).contains("unexpected argument"),
            "{flag:?}"
        );
    }
    assert!(env.everything().is_empty());
}

#[test]
fn a_failed_store_write_keeps_the_old_store_whole() {
    let env = Env::new();
    env.install(&[]);
    let skill = env.store().join("references/rules.md");
    fs::write(&skill, "old store\n").unwrap();
    let parent = env.store().parent().unwrap().to_path_buf();
    fs::set_permissions(&parent, fs::Permissions::from_mode(0o500)).unwrap();
    if fs::create_dir(parent.join("probe")).is_ok() {
        // Running as a user that ignores permissions (root).
        fs::remove_dir(parent.join("probe")).unwrap();
        return;
    }

    let output = env.install(&[]);

    fs::set_permissions(&parent, fs::Permissions::from_mode(0o700)).unwrap();
    assert_eq!(code(&output), 2);
    let err = text(&output.stderr);
    assert!(
        err.starts_with("error:") && !err.contains("not installed by oxabl"),
        "{err}"
    );
    assert_eq!(fs::read_to_string(&skill).unwrap(), "old store\n");
    assert_eq!(tree(&env.store()), skill_files());
    assert_eq!(
        tree(&parent).len(),
        skill_files().len(),
        "a staging directory is left"
    );
}

#[test]
fn an_unwritable_link_directory_fails_with_two_before_the_store_is_written() {
    let env = Env::new();
    let blocked = env.home.path().join(".claude");
    fs::create_dir(&blocked).unwrap();
    fs::set_permissions(&blocked, fs::Permissions::from_mode(0o500)).unwrap();
    if fs::create_dir(blocked.join("probe")).is_ok() {
        // Running as a user that ignores permissions (root).
        return;
    }

    let output = env.install(&[]);

    fs::set_permissions(&blocked, fs::Permissions::from_mode(0o700)).unwrap();
    assert_eq!(code(&output), 2);
    assert!(text(&output.stderr).starts_with("error:"));
    assert!(!env.store().exists(), "the store was written");
    assert!(!env.agents().exists(), "the other link was written");
}

#[test]
fn a_file_where_a_directory_belongs_fails_with_two_and_writes_nothing() {
    let env = Env::new();
    fs::write(env.home.path().join(".claude"), "mine\n").unwrap();
    let before = env.everything();

    let output = env.install(&[]);

    assert_eq!(code(&output), 2);
    assert!(
        text(&output.stderr)
            .to_lowercase()
            .contains("not a directory"),
        "{}",
        text(&output.stderr)
    );
    assert_eq!(env.everything(), before);
}

#[test]
fn a_copy_at_a_link_location_is_oxabls_and_check_compares_it() {
    let env = Env::new();
    env.install(&["--agent", "claude"]);
    make_copy(&env.agents());

    let check = env.install(&["--check"]);

    assert_eq!(code(&check), 0, "{}", text(&check.stdout));
    fs::write(env.agents().join("references/build.md"), "edited\n").unwrap();
    let stale = env.install(&["--check"]);
    assert_eq!(code(&stale), 1);
    assert!(text(&stale.stdout).contains("references/build.md differs"));
}

#[test]
fn refresh_and_install_replace_a_stale_copy_whole() {
    for mode in [&["--refresh"][..], &[][..]] {
        let env = Env::new();
        env.install(&["--agent", "claude"]);
        make_copy(&env.agents());
        fs::write(env.agents().join("references/build.md"), "edited\n").unwrap();
        fs::write(env.agents().join("references/extra.md"), "extra\n").unwrap();

        let output = env.install(mode);

        assert_eq!(code(&output), 0, "{mode:?}: {}", text(&output.stderr));
        assert_eq!(tree(&env.agents()), skill_files(), "{mode:?}");
        assert!(
            fs::symlink_metadata(env.agents()).unwrap().is_dir(),
            "{mode:?}: the copy became something else"
        );
        assert_eq!(
            fs::read_to_string(env.agents().join("references/build.md")).unwrap(),
            fs::read_to_string(source_dir().join("references/build.md")).unwrap(),
            "{mode:?}"
        );
        assert!(
            fs::read_dir(env.agents().parent().unwrap())
                .unwrap()
                .count()
                == 1,
            "{mode:?}: a staging directory is left"
        );
    }
}

#[test]
fn a_relative_symlink_to_the_store_counts_as_linked() {
    let env = Env::new();
    env.install(&["--agent", "claude"]);
    let link = env.agents();
    fs::create_dir_all(link.parent().unwrap()).unwrap();
    let relative = pathdiff(&env.store(), link.parent().unwrap());
    std::os::unix::fs::symlink(relative, &link).unwrap();

    let check = env.install(&["--check"]);

    assert_eq!(code(&check), 0, "{}", text(&check.stdout));
}

/// `target` relative to the directory `from`; both absolute and without `..`.
fn pathdiff(target: &Path, from: &Path) -> PathBuf {
    let target: Vec<_> = target.components().collect();
    let from: Vec<_> = from.components().collect();
    let common = target.iter().zip(&from).take_while(|(a, b)| a == b).count();
    let mut out = PathBuf::new();
    for _ in common..from.len() {
        out.push("..");
    }
    for part in &target[common..] {
        out.push(part);
    }
    out
}

#[test]
fn show_prints_the_source_with_the_binary_version() {
    for name in skill_files() {
        let source = fs::read_to_string(source_dir().join(&name)).unwrap();
        let expected = if name == "SKILL.md" {
            assert!(source.contains("oxabl-version: dev"));
            source.replacen(
                "oxabl-version: dev",
                &format!("oxabl-version: {VERSION}"),
                1,
            )
        } else {
            source
        };

        let output = if name == "SKILL.md" {
            run(&["skill", "show"])
        } else {
            run(&["skill", "show", &name])
        };

        assert_eq!(code(&output), 0, "{name}: {}", text(&output.stderr));
        assert_eq!(text(&output.stdout), expected, "{name}");
    }
    let named = run(&["skill", "show", "SKILL.md"]);
    assert_eq!(text(&named.stdout), text(&run(&["skill", "show"]).stdout));
}

#[test]
fn show_list_names_every_file_of_the_skill_directory() {
    let output = run(&["skill", "show", "--list"]);

    assert_eq!(code(&output), 0);
    let mut listed: Vec<String> = text(&output.stdout).lines().map(String::from).collect();
    listed.sort();
    assert_eq!(
        listed,
        skill_files(),
        "crates/oxabl/skill/ and the FILES list in src/skill.rs differ"
    );
}

#[test]
fn show_of_an_unknown_file_is_an_error_that_lists_the_files() {
    let output = run(&["skill", "show", "nope.md"]);

    assert_eq!(code(&output), 2);
    let err = text(&output.stderr);
    assert!(err.contains("nope.md") && err.contains("SKILL.md"), "{err}");
}

#[test]
fn the_front_matter_follows_the_contract() {
    let output = run(&["skill", "show"]);

    let out = text(&output.stdout);
    assert_eq!(out.lines().next(), Some("---"));
    let front: Vec<&str> = out
        .lines()
        .skip(1)
        .take_while(|line| *line != "---")
        .collect();
    let keys: Vec<&str> = front
        .iter()
        .filter(|line| !line.starts_with([' ', '\t']))
        .filter_map(|line| line.split(':').next())
        .collect();
    assert_eq!(keys, ["name", "description", "metadata"], "{front:?}");
    assert!(front.contains(&"name: oxabl"), "{front:?}");
    assert!(
        front.contains(&format!("  oxabl-version: {VERSION}").as_str()),
        "{front:?}"
    );
}
