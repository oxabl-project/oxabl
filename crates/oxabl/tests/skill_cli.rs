//! `oxabl skill show` and `oxabl skill install`, run as the built binary.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use tempfile::TempDir;

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn oxabl(args: &[&str]) -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_oxabl"));
    command.args(args).env_remove("HOME");
    command
}

fn run(args: &[&str]) -> Output {
    oxabl(args).output().unwrap()
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

/// What an install leaves in a target: the skill and its manifest.
fn installed_files() -> Vec<String> {
    let mut files = skill_files();
    files.push(".oxabl-skill.json".to_string());
    files.sort();
    files
}

/// The files under `dir`, relative, sorted.
fn tree(dir: &Path) -> Vec<String> {
    let mut files = Vec::new();
    if dir.exists() {
        source_files(dir, "", &mut files);
    }
    files.sort();
    files
}

/// Lists `name` in the manifest of the copy in `target`, as an older release
/// that shipped that file would have.
fn add_to_manifest(target: &Path, name: &str) {
    let path = target.join(".oxabl-skill.json");
    let mut manifest: serde_json::Value =
        serde_json::from_str(&fs::read_to_string(&path).unwrap()).unwrap();
    manifest["files"].as_array_mut().unwrap().push(name.into());
    fs::write(&path, manifest.to_string()).unwrap();
}

fn agents(base: &Path) -> PathBuf {
    base.join(".agents/skills/oxabl")
}

fn claude(base: &Path) -> PathBuf {
    base.join(".claude/skills/oxabl")
}

fn install(base: &Path, extra: &[&str]) -> Output {
    let mut args = vec!["skill", "install", "--dir", base.to_str().unwrap()];
    args.extend_from_slice(extra);
    run(&args)
}

#[test]
fn install_creates_both_targets_and_check_agrees() {
    let dir = TempDir::new().unwrap();

    let output = install(dir.path(), &[]);

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    let out = text(&output.stdout);
    assert_eq!(out.lines().count(), 2, "{out}");
    for target in [agents(dir.path()), claude(dir.path())] {
        assert!(out.contains(&target.display().to_string()), "{out}");
        assert_eq!(tree(&target), installed_files());
        let skill = fs::read_to_string(target.join("SKILL.md")).unwrap();
        assert!(
            skill.contains(&format!("oxabl-version: {VERSION}")),
            "{skill}"
        );
    }
    let check = install(dir.path(), &["--check"]);
    assert_eq!(code(&check), 0, "{}", text(&check.stdout));
}

#[test]
fn check_on_an_empty_directory_fails_and_writes_nothing() {
    let dir = TempDir::new().unwrap();

    let check = install(dir.path(), &["--check"]);

    assert_eq!(code(&check), 1);
    assert!(text(&check.stdout).contains("missing"));
    assert_eq!(fs::read_dir(dir.path()).unwrap().count(), 0);
}

#[test]
fn check_names_an_edited_file() {
    let dir = TempDir::new().unwrap();
    install(dir.path(), &[]);
    let edited = agents(dir.path()).join("references/check.md");
    let mut changed = fs::read_to_string(&edited).unwrap();
    changed.push_str("edited\n");
    fs::write(&edited, changed).unwrap();

    let check = install(dir.path(), &["--check"]);

    assert_eq!(code(&check), 1);
    let out = text(&check.stdout);
    assert!(
        out.contains(&agents(dir.path()).display().to_string())
            && out.contains("stale")
            && out.contains("references/check.md differs"),
        "{out}"
    );
    assert!(
        out.contains("current"),
        "the other target is still current: {out}"
    );
}

#[test]
fn check_lists_a_file_oxabl_did_not_write_without_failing() {
    let dir = TempDir::new().unwrap();
    install(dir.path(), &[]);
    fs::write(claude(dir.path()).join("notes.md"), "x").unwrap();

    let check = install(dir.path(), &["--check"]);

    assert_eq!(code(&check), 0, "{}", text(&check.stdout));
    let out = text(&check.stdout);
    assert!(out.contains("other files left alone: notes.md"), "{out}");
    assert!(!out.contains("stale"), "{out}");
}

#[test]
fn check_names_a_file_an_earlier_install_wrote_and_the_skill_dropped() {
    let dir = TempDir::new().unwrap();
    install(dir.path(), &[]);
    let old = claude(dir.path()).join("references/old.md");
    fs::write(&old, "old").unwrap();
    add_to_manifest(&claude(dir.path()), "references/old.md");

    let check = install(dir.path(), &["--check"]);

    assert_eq!(code(&check), 1);
    assert!(
        text(&check.stdout).contains("references/old.md is no longer part of the skill"),
        "{}",
        text(&check.stdout)
    );
    assert!(old.exists(), "--check writes nothing");
}

#[test]
fn a_foreign_skill_is_refused_without_force() {
    let dir = TempDir::new().unwrap();
    let foreign = "---\nname: oxabl\ndescription: mine\n---\nmy own notes\n";
    for target in [agents(dir.path()), claude(dir.path())] {
        fs::create_dir_all(&target).unwrap();
    }
    fs::write(claude(dir.path()).join("SKILL.md"), foreign).unwrap();

    let refused = install(dir.path(), &[]);

    assert_eq!(code(&refused), 2);
    assert!(text(&refused.stderr).contains("--force"));
    assert_eq!(
        fs::read_to_string(claude(dir.path()).join("SKILL.md")).unwrap(),
        foreign
    );
    assert!(
        tree(&agents(dir.path())).is_empty(),
        "a refusal writes no target"
    );
    let check = install(dir.path(), &["--check"]);
    assert_eq!(code(&check), 1);
    assert!(text(&check.stdout).contains("foreign"));

    let forced = install(dir.path(), &["--force"]);

    assert_eq!(code(&forced), 0, "{}", text(&forced.stderr));
    assert_eq!(tree(&claude(dir.path())), installed_files());
    assert_eq!(code(&install(dir.path(), &["--check"])), 0);
}

#[test]
fn a_skill_of_another_name_is_foreign() {
    let dir = TempDir::new().unwrap();
    let other = "---\nname: other\nmetadata:\n  oxabl-version: 1.0.0\n---\n";
    fs::create_dir_all(agents(dir.path())).unwrap();
    fs::write(agents(dir.path()).join("SKILL.md"), other).unwrap();

    let refused = install(dir.path(), &["--agent", "agents"]);

    assert_eq!(code(&refused), 2);
}

#[test]
fn force_keeps_the_other_files_of_a_foreign_directory_for_good() {
    let dir = TempDir::new().unwrap();
    fs::create_dir_all(agents(dir.path())).unwrap();
    fs::write(agents(dir.path()).join("SKILL.md"), "no front matter").unwrap();
    fs::write(agents(dir.path()).join("mine.txt"), "keep").unwrap();

    let forced = install(dir.path(), &["--agent", "agents", "--force"]);

    assert_eq!(code(&forced), 0, "{}", text(&forced.stderr));
    let mine = agents(dir.path()).join("mine.txt");
    assert_eq!(fs::read_to_string(&mine).unwrap(), "keep");
    assert!(tree(&agents(dir.path())).contains(&"SKILL.md".to_string()));
    let manifest = fs::read_to_string(agents(dir.path()).join(".oxabl-skill.json")).unwrap();
    assert!(!manifest.contains("mine.txt"), "{manifest}");
    let check = install(dir.path(), &["--agent", "agents", "--check"]);
    assert_eq!(code(&check), 0, "{}", text(&check.stdout));

    for extra in [&["--refresh"][..], &[][..]] {
        let mut args = vec!["--agent", "agents"];
        args.extend_from_slice(extra);
        let output = install(dir.path(), &args);
        assert_eq!(code(&output), 0, "{}", text(&output.stderr));
        assert_eq!(fs::read_to_string(&mine).unwrap(), "keep", "{extra:?}");
    }
    stale_the_copy(&agents(dir.path()));
    let output = install(dir.path(), &["--agent", "agents", "--refresh"]);
    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert!(text(&output.stdout).contains("updated"));
    assert_eq!(fs::read_to_string(&mine).unwrap(), "keep");
}

/// Makes the copy in `target` older than the binary.
fn stale_the_copy(target: &Path) {
    let skill = target.join("SKILL.md");
    let text = fs::read_to_string(&skill)
        .unwrap()
        .replace(VERSION, "0.0.1");
    fs::write(&skill, text).unwrap();
}

#[test]
fn a_user_file_survives_install_refresh_and_a_second_install() {
    let dir = TempDir::new().unwrap();
    install(dir.path(), &[]);
    let notes = claude(dir.path()).join("notes.md");
    let nested = claude(dir.path()).join("references/mine/extra.md");
    fs::create_dir_all(nested.parent().unwrap()).unwrap();
    fs::write(&notes, "mine").unwrap();
    fs::write(&nested, "mine too").unwrap();
    stale_the_copy(&claude(dir.path()));

    let refreshed = install(dir.path(), &["--refresh"]);
    assert_eq!(code(&refreshed), 0, "{}", text(&refreshed.stderr));
    let again = install(dir.path(), &[]);
    assert_eq!(code(&again), 0, "{}", text(&again.stderr));

    assert_eq!(fs::read_to_string(&notes).unwrap(), "mine");
    assert_eq!(fs::read_to_string(&nested).unwrap(), "mine too");
    assert_eq!(code(&install(dir.path(), &["--check"])), 0);
}

#[test]
fn a_current_copy_without_a_manifest_is_current_and_an_install_writes_the_manifest() {
    let dir = TempDir::new().unwrap();
    install(dir.path(), &[]);
    let target = agents(dir.path());
    fs::remove_file(target.join(".oxabl-skill.json")).unwrap();
    fs::write(target.join("notes.md"), "mine").unwrap();

    let check = install(dir.path(), &["--agent", "agents", "--check"]);
    assert_eq!(code(&check), 0, "{}", text(&check.stdout));
    assert!(text(&check.stdout).contains("current"));
    assert!(text(&check.stdout).contains(".oxabl-skill.json is missing"));
    assert!(!target.join(".oxabl-skill.json").exists());

    let refresh = install(dir.path(), &["--agent", "agents", "--refresh"]);

    assert_eq!(code(&refresh), 0, "{}", text(&refresh.stderr));
    assert!(target.join(".oxabl-skill.json").exists());
    assert_eq!(fs::read_to_string(target.join("notes.md")).unwrap(), "mine");
    let check = install(dir.path(), &["--agent", "agents", "--check"]);
    assert_eq!(code(&check), 0);
    assert!(!text(&check.stdout).contains("is missing"));

    fs::remove_file(target.join(".oxabl-skill.json")).unwrap();
    let output = install(dir.path(), &["--agent", "agents"]);
    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert!(target.join(".oxabl-skill.json").exists());
}

#[test]
fn a_stale_copy_without_a_manifest_is_stale_and_upgrades_keeping_every_other_file() {
    let dir = TempDir::new().unwrap();
    install(dir.path(), &[]);
    let target = agents(dir.path());
    fs::remove_file(target.join(".oxabl-skill.json")).unwrap();
    stale_the_copy(&target);
    fs::write(target.join("notes.md"), "mine").unwrap();
    fs::write(target.join("references/old.md"), "from an older oxabl").unwrap();

    let check = install(dir.path(), &["--agent", "agents", "--check"]);
    assert_eq!(code(&check), 1);
    assert!(text(&check.stdout).contains("SKILL.md differs"));

    let output = install(dir.path(), &["--agent", "agents"]);

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert!(text(&output.stdout).contains("updated"));
    assert_eq!(fs::read_to_string(target.join("notes.md")).unwrap(), "mine");
    assert!(
        target.join("references/old.md").exists(),
        "a file with no skill name is not oxabl's without a manifest"
    );
    assert!(target.join(".oxabl-skill.json").exists());
    let check = install(dir.path(), &["--agent", "agents", "--check"]);
    assert_eq!(code(&check), 0, "{}", text(&check.stdout));
}

#[test]
fn a_manifest_cannot_name_a_file_outside_the_copy() {
    let dir = TempDir::new().unwrap();
    install(dir.path(), &["--agent", "agents"]);
    let target = agents(dir.path());
    let outside = dir.path().join("outside.txt");
    fs::write(&outside, "keep").unwrap();
    add_to_manifest(&target, "../../../outside.txt");
    add_to_manifest(&target, outside.to_str().unwrap());
    stale_the_copy(&target);

    let output = install(dir.path(), &["--agent", "agents"]);

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert_eq!(fs::read_to_string(&outside).unwrap(), "keep");
}

#[test]
fn a_failed_install_leaves_the_old_skill_md_whole() {
    use std::os::unix::fs::PermissionsExt;
    let dir = TempDir::new().unwrap();
    install(dir.path(), &["--agent", "agents"]);
    let target = agents(dir.path());
    stale_the_copy(&target);
    let old = fs::read_to_string(target.join("SKILL.md")).unwrap();
    fs::write(target.join("references/check.md"), "edited").unwrap();
    let references = target.join("references");
    fs::set_permissions(&references, fs::Permissions::from_mode(0o500)).unwrap();
    if fs::File::create(references.join("probe")).is_ok() {
        // Running as a user that ignores permissions (root).
        return;
    }

    let output = install(dir.path(), &["--agent", "agents"]);

    fs::set_permissions(&references, fs::Permissions::from_mode(0o700)).unwrap();
    assert_eq!(code(&output), 2);
    assert!(text(&output.stderr).starts_with("error:"));
    assert_eq!(fs::read_to_string(target.join("SKILL.md")).unwrap(), old);
}

#[test]
fn a_symlinked_target_is_written_through_and_becomes_current() {
    let dir = TempDir::new().unwrap();
    let real = dir.path().join("shared/oxabl");
    fs::create_dir_all(&real).unwrap();
    let link = agents(dir.path());
    fs::create_dir_all(link.parent().unwrap()).unwrap();
    std::os::unix::fs::symlink(&real, &link).unwrap();

    let first = install(dir.path(), &["--agent", "agents"]);
    assert_eq!(code(&first), 0, "{}", text(&first.stderr));
    assert!(text(&first.stdout).contains("installed"));
    assert!(
        fs::symlink_metadata(&link)
            .unwrap()
            .file_type()
            .is_symlink()
    );
    assert_eq!(tree(&real), installed_files());
    fs::write(real.join("notes.md"), "mine").unwrap();
    stale_the_copy(&real);
    assert_eq!(
        code(&install(dir.path(), &["--agent", "agents", "--check"])),
        1
    );

    let second = install(dir.path(), &["--agent", "agents"]);

    assert_eq!(code(&second), 0, "{}", text(&second.stderr));
    assert!(text(&second.stdout).contains("updated"));
    let check = install(dir.path(), &["--agent", "agents", "--check"]);
    assert_eq!(code(&check), 0, "{}", text(&check.stdout));
    assert_eq!(fs::read_to_string(real.join("notes.md")).unwrap(), "mine");
}

#[test]
fn a_symlink_inside_a_target_fails_with_two_and_writes_nothing_anywhere() {
    use std::os::unix::fs::symlink;
    for extra in [&[][..], &["--refresh"][..], &["--force"][..]] {
        let dir = TempDir::new().unwrap();
        let out = dir.path().join("out");
        fs::create_dir(&out).unwrap();
        install(dir.path(), &["--agent", "agents"]);
        let target = agents(dir.path());
        stale_the_copy(&target);
        fs::remove_dir_all(target.join("references")).unwrap();
        symlink(&out, target.join("references")).unwrap();
        let before = fs::read_to_string(target.join("SKILL.md")).unwrap();

        let output = install(dir.path(), extra);

        assert_eq!(code(&output), 2, "{extra:?}");
        assert!(text(&output.stderr).contains("references"), "{extra:?}");
        assert!(tree(&out).is_empty(), "{extra:?}: {:?}", tree(&out));
        assert!(!claude(dir.path()).exists(), "{extra:?}");
        assert_eq!(
            fs::read_to_string(target.join("SKILL.md")).unwrap(),
            before,
            "{extra:?}"
        );
    }
}

#[test]
fn a_dangling_symlink_or_a_file_as_references_fails_with_two() {
    use std::os::unix::fs::symlink;
    for dangling in [true, false] {
        let dir = TempDir::new().unwrap();
        install(dir.path(), &["--agent", "claude"]);
        let target = claude(dir.path());
        stale_the_copy(&target);
        fs::remove_dir_all(target.join("references")).unwrap();
        if dangling {
            symlink(dir.path().join("gone"), target.join("references")).unwrap();
        } else {
            fs::write(target.join("references"), "a file").unwrap();
        }

        let output = install(dir.path(), &[]);

        assert_eq!(code(&output), 2, "dangling {dangling}");
        assert!(!dir.path().join("gone").exists());
        assert!(!agents(dir.path()).exists());
    }
}

#[test]
fn a_dangling_symlink_fails_with_two_and_writes_nothing_anywhere() {
    let dir = TempDir::new().unwrap();
    let link = claude(dir.path());
    fs::create_dir_all(link.parent().unwrap()).unwrap();
    std::os::unix::fs::symlink(dir.path().join("gone"), &link).unwrap();

    for extra in [
        &[][..],
        &["--refresh"][..],
        &["--force"][..],
        &["--check"][..],
    ] {
        let output = install(dir.path(), extra);

        assert_eq!(code(&output), 2, "{extra:?}");
        assert!(text(&output.stderr).starts_with("error:"), "{extra:?}");
        assert!(
            text(&output.stderr).contains(&link.display().to_string()),
            "{}",
            text(&output.stderr)
        );
        assert!(!dir.path().join(".agents").exists(), "{extra:?}");
        assert!(!dir.path().join("gone").exists(), "{extra:?}");
    }
}

#[test]
fn a_dangling_symlink_above_a_target_fails_with_two() {
    let dir = TempDir::new().unwrap();
    std::os::unix::fs::symlink(dir.path().join("gone"), dir.path().join(".claude")).unwrap();

    let output = install(dir.path(), &[]);

    assert_eq!(code(&output), 2);
    assert!(!dir.path().join(".agents").exists());
}

#[test]
fn a_target_that_is_a_file_fails_with_two_and_writes_nothing_anywhere() {
    let dir = TempDir::new().unwrap();
    fs::create_dir_all(claude(dir.path()).parent().unwrap()).unwrap();
    fs::write(claude(dir.path()), "a file").unwrap();

    let output = install(dir.path(), &[]);

    assert_eq!(code(&output), 2);
    assert!(text(&output.stderr).contains("not a directory"));
    assert!(!dir.path().join(".agents").exists());
}

#[test]
fn an_unwritable_second_target_leaves_the_first_untouched() {
    use std::os::unix::fs::PermissionsExt;
    let dir = TempDir::new().unwrap();
    let blocked = dir.path().join(".claude");
    fs::create_dir(&blocked).unwrap();
    fs::set_permissions(&blocked, fs::Permissions::from_mode(0o500)).unwrap();
    if fs::create_dir(blocked.join("probe")).is_ok() {
        // Running as a user that ignores permissions (root).
        return;
    }

    let output = install(dir.path(), &[]);

    fs::set_permissions(&blocked, fs::Permissions::from_mode(0o700)).unwrap();
    assert_eq!(code(&output), 2);
    assert!(!dir.path().join(".agents").exists());
}

#[test]
fn refresh_creates_nothing_where_nothing_was() {
    let dir = TempDir::new().unwrap();

    let output = install(dir.path(), &["--refresh"]);

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert!(text(&output.stdout).contains("skipped"));
    assert_eq!(fs::read_dir(dir.path()).unwrap().count(), 0);
}

#[test]
fn refresh_updates_an_existing_copy_only() {
    let dir = TempDir::new().unwrap();
    install(dir.path(), &["--agent", "agents"]);
    let skill = agents(dir.path()).join("SKILL.md");
    let stale = fs::read_to_string(&skill)
        .unwrap()
        .replace(VERSION, "0.0.1");
    fs::write(&skill, stale).unwrap();
    assert_eq!(code(&install(dir.path(), &["--check"])), 1);

    let output = install(dir.path(), &["--refresh"]);

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert_eq!(
        code(&install(dir.path(), &["--agent", "agents", "--check"])),
        0
    );
    assert!(
        !claude(dir.path()).exists(),
        "refresh must not create the claude target"
    );
}

#[test]
fn replacing_a_copy_removes_a_file_that_an_earlier_install_wrote_and_the_set_lacks() {
    let dir = TempDir::new().unwrap();
    install(dir.path(), &[]);
    let extra = agents(dir.path()).join("references/old/deep.md");
    fs::create_dir_all(extra.parent().unwrap()).unwrap();
    fs::write(&extra, "old").unwrap();
    add_to_manifest(&agents(dir.path()), "references/old/deep.md");
    let mine = agents(dir.path()).join("references/mine.md");
    fs::write(&mine, "mine").unwrap();
    let sibling = dir.path().join(".agents/skills/other/SKILL.md");
    fs::create_dir_all(sibling.parent().unwrap()).unwrap();
    fs::write(&sibling, "another skill").unwrap();

    let output = install(dir.path(), &[]);

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert!(!extra.exists());
    assert!(
        !extra.parent().unwrap().exists(),
        "the directory that the removal emptied goes too"
    );
    assert_eq!(fs::read_to_string(&mine).unwrap(), "mine");
    let mut expected = installed_files();
    expected.push("references/mine.md".to_string());
    expected.sort();
    assert_eq!(tree(&agents(dir.path())), expected);
    assert_eq!(fs::read_to_string(&sibling).unwrap(), "another skill");
    assert!(
        fs::read_dir(dir.path().join(".agents/skills"))
            .unwrap()
            .all(|e| !e
                .unwrap()
                .file_name()
                .to_string_lossy()
                .starts_with(".oxabl-skill")),
        "no staging file is left behind"
    );
}

#[test]
fn a_current_copy_is_left_alone() {
    let dir = TempDir::new().unwrap();
    install(dir.path(), &[]);

    let output = install(dir.path(), &[]);

    assert_eq!(code(&output), 0);
    assert_eq!(text(&output.stdout).matches("current").count(), 2);
}

#[test]
fn agent_selects_one_target() {
    let dir = TempDir::new().unwrap();

    install(dir.path(), &["--agent", "claude"]);

    assert!(claude(dir.path()).join("SKILL.md").exists());
    assert!(!agents(dir.path()).exists());
}

#[test]
fn user_installs_under_home() {
    let home = TempDir::new().unwrap();

    let output = oxabl(&["skill", "install", "--user"])
        .env("HOME", home.path())
        .output()
        .unwrap();

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert_eq!(tree(&agents(home.path())), installed_files());
    assert_eq!(tree(&claude(home.path())), installed_files());
}

#[test]
fn user_without_home_is_a_usage_error() {
    let output = run(&["skill", "install", "--user"]);

    assert_eq!(code(&output), 2);
    assert!(text(&output.stderr).contains("HOME"));
}

#[test]
fn the_default_base_is_the_current_directory() {
    let dir = TempDir::new().unwrap();

    let output = oxabl(&["skill", "install"])
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert_eq!(code(&output), 0, "{}", text(&output.stderr));
    assert_eq!(tree(&agents(dir.path())), installed_files());
}

#[test]
fn user_and_dir_conflict() {
    let output = run(&["skill", "install", "--user", "--dir", "."]);

    assert_eq!(code(&output), 2);
}

#[test]
fn an_unwritable_target_fails_with_two_and_leaves_no_skill_file() {
    use std::os::unix::fs::PermissionsExt;
    let dir = TempDir::new().unwrap();
    let blocked = dir.path().join(".agents");
    fs::create_dir(&blocked).unwrap();
    fs::set_permissions(&blocked, fs::Permissions::from_mode(0o500)).unwrap();
    if fs::create_dir(blocked.join("probe")).is_ok() {
        // Running as a user that ignores permissions (root).
        return;
    }

    let output = install(dir.path(), &["--agent", "agents"]);

    fs::set_permissions(&blocked, fs::Permissions::from_mode(0o700)).unwrap();
    assert_eq!(code(&output), 2);
    assert!(text(&output.stderr).starts_with("error:"));
    assert!(tree(&blocked).is_empty(), "{:?}", tree(&blocked));
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
