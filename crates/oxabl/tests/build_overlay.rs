//! The overlay of `oxabl build`: a second TOML file with the schema of
//! `oxabl.toml`, merged over it key by key, and `--print-config`, which says
//! where each value came from. The worker is the fake AVM.

mod common;

use std::collections::BTreeMap;

use common::built::{Built, Run, of_kind, unit};
use serde_json::Value;

/// `key = value  # source` lines of `--print-config`, by key.
fn settings(run: &Run) -> BTreeMap<String, (String, String)> {
    run.succeeded();
    run.stdout
        .lines()
        .filter(|line| !line.starts_with('#'))
        .map(|line| {
            let (setting, source) = line.rsplit_once("  # ").expect("a source comment");
            let (key, value) = setting.split_once(" = ").expect("key = value");
            (
                key.to_string(),
                (value.trim_end().to_string(), source.to_string()),
            )
        })
        .collect()
}

fn setting(run: &Run, key: &str) -> (String, String) {
    settings(run)
        .remove(key)
        .unwrap_or_else(|| panic!("no {key} in\n{}", run.stdout))
}

fn with_overlay(workspace: &Built, overlay: &str, args: &[&str]) -> Run {
    workspace.write("overlay.toml", overlay);
    let path = workspace.path("overlay.toml");
    let mut all = vec!["--config-overlay", path.to_str().unwrap()];
    all.extend_from_slice(args);
    workspace.build_with(&all)
}

fn assert_refused(workspace: &Built, run: &Run, named: &str) {
    assert_eq!(
        run.code, 2,
        "stdout:\n{}\nstderr:\n{}",
        run.stdout, run.stderr
    );
    assert!(run.stderr.contains(named), "{}", run.stderr);
    assert!(
        workspace.compiled().is_empty(),
        "{:?}",
        workspace.compiled()
    );
    assert!(!workspace.exists("build"));
    assert!(!workspace.exists(".oxabl"));
}

fn sources() -> Built {
    let workspace = Built::new();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.write("src/b.p", "MESSAGE \"b\".\n");
    workspace.write("src/c.p", "MESSAGE \"c\".\n");
    workspace
}

#[test]
fn an_overlay_wins_key_by_key_and_an_array_replaces() {
    let workspace = Built::new();
    workspace.workers("count = 1");

    let run = with_overlay(
        &workspace,
        "[workspace.sources]\ninclude_paths = [\"lib\", \"src\"]\n\n\
         [build.workers]\ncount = 2\nenv = { TERM = \"dumb\" }\n",
        &["--print-config"],
    );

    let all = settings(&run);
    assert_eq!(
        all["workspace.sources.include_paths"],
        ("[\"lib\",\"src\"]".to_string(), "overlay".to_string())
    );
    assert_eq!(
        all["workspace.sources.directories"],
        ("[\"src\"]".to_string(), "oxabl.toml".to_string())
    );
    assert_eq!(
        all["build.workers.count"],
        ("2".to_string(), "overlay".to_string())
    );
    assert_eq!(all["build.workers.env.TERM"].1, "overlay");
    assert_eq!(all["build.workers.env.FAKE_AVM_LOG"].1, "oxabl.toml");
    assert_eq!(all["build.workers.command"].1, "oxabl.toml");
    assert_eq!(all["build.workers.unit_timeout"].1, "default");
    assert_eq!(all["build.output"].1, "oxabl.toml");
    assert_eq!(all["workspace.name"].1, "oxabl.toml");
    assert_eq!(
        all["build.external_packages.\"OpenEdge.\""],
        ("[\"$DLC\"]".to_string(), "default".to_string())
    );
}

#[test]
fn the_overlay_reaches_the_build_and_the_analysis() {
    let workspace = sources();
    workspace.write("src/main.p", "{inc.i}\n");
    workspace.write("lib/inc.i", "MESSAGE \"i\".\n");

    let without = workspace.build_with(&["--plan", "--probe", "--json", "--all"]);
    let plan: Value = serde_json::from_str(&without.stdout).unwrap();
    assert!(
        !unit(&plan, "src/main.p")["unkeyable"]
            .as_array()
            .unwrap()
            .is_empty(),
        "{plan}"
    );

    let run = with_overlay(
        &workspace,
        "[workspace.sources]\ninclude_paths = [\"src\", \"lib\"]\n\n\
         [build]\noutput = \"out/r\"\n\n[build.workers]\ncount = 3\n",
        &["--events"],
    );

    run.succeeded();
    let events = run.events();
    assert_eq!(of_kind(&events, "build-started")[0]["workers"], 3);
    assert!(
        workspace.exists("out/r/a.r"),
        "{}{}",
        run.stdout,
        run.stderr
    );
    assert!(!workspace.exists("build"));
    let plan: Value = serde_json::from_str(
        &with_overlay(
            &workspace,
            "[workspace.sources]\ninclude_paths = [\"src\", \"lib\"]\n\n[build]\noutput = \"out/r\"\n",
            &["--plan", "--probe", "--json", "--all"],
        )
        .stdout,
    )
    .unwrap();
    assert!(
        unit(&plan, "src/main.p")["unkeyable"]
            .as_array()
            .unwrap()
            .is_empty(),
        "{plan}"
    );
}

#[test]
fn an_overlay_array_replaces_the_worker_command() {
    let workspace = sources();

    let run = with_overlay(
        &workspace,
        "[build.workers]\ncommand = [\"/nonexistent/oxabl-test-worker\", \"{program}\"]\n",
        &[],
    );

    assert_eq!(
        run.code, 3,
        "stdout:\n{}\nstderr:\n{}",
        run.stdout, run.stderr
    );
    assert!(
        run.stderr.contains("/nonexistent/oxabl-test-worker"),
        "{}",
        run.stderr
    );
    assert!(workspace.log_lines().is_empty());
}

#[test]
fn a_flag_beats_the_environment_beats_the_overlay_beats_oxabl_toml_beats_the_default() {
    let workspace = Built::with("include_paths = [\"src\"]\ndlc = \"/from-toml\"", "");
    let dlc = "workspace.sources.dlc";
    let overlay = "[workspace.sources]\ndlc = \"/from-overlay\"\n";

    let run = workspace.build_with(&["--print-config"]);
    assert_eq!(
        setting(&run, dlc),
        ("\"/from-toml\"".into(), "oxabl.toml".into())
    );
    let run = with_overlay(&workspace, overlay, &["--print-config"]);
    assert_eq!(
        setting(&run, dlc),
        ("\"/from-overlay\"".into(), "overlay".into())
    );
    workspace.env("OXABL_DLC_SOURCES", Some("/from-env"));
    let run = with_overlay(&workspace, overlay, &["--print-config"]);
    assert_eq!(
        setting(&run, dlc),
        (
            "\"/from-env\"".into(),
            "environment OXABL_DLC_SOURCES".into()
        )
    );
    workspace.env("OXABL_DLC_SOURCES", None);

    let count = "build.workers.count";
    let run = with_overlay(
        &workspace,
        "[build.workers]\ncount = 2\n",
        &["--print-config"],
    );
    assert_eq!(setting(&run, count), ("2".into(), "overlay".into()));
    let run = with_overlay(
        &workspace,
        "[build.workers]\ncount = 2\n",
        &["--print-config", "--workers", "4"],
    );
    assert_eq!(setting(&run, count), ("4".into(), "flag --workers".into()));
    let run = workspace.build_with(&["--print-config"]);
    assert_eq!(setting(&run, count), ("1".into(), "oxabl.toml".into()));
    let run = Built::new().build_with(&["--print-config", "--workers", "6"]);
    assert_eq!(setting(&run, count), ("6".into(), "flag --workers".into()));
}

#[test]
fn the_cache_flags_and_the_cache_directory_variable_are_sources_too() {
    let workspace = Built::new();
    let local = "build.cache.local";
    let overlay = "[build.cache]\nlocal = \"/from-overlay\"\n";

    let run = with_overlay(&workspace, overlay, &["--print-config"]);
    assert_eq!(
        setting(&run, local),
        ("\"/from-overlay\"".into(), "overlay".into())
    );
    workspace.env("OXABL_BUILD_CACHE_DIR", Some("/from-env"));
    let run = with_overlay(&workspace, overlay, &["--print-config"]);
    assert_eq!(
        setting(&run, local),
        (
            "\"/from-env\"".into(),
            "environment OXABL_BUILD_CACHE_DIR".into()
        )
    );

    let run = workspace.build_with(&["--print-config", "--no-cache", "--no-remote-cache"]);
    assert_eq!(
        setting(&run, "build.cache.enabled"),
        ("false".into(), "flag --no-cache".into())
    );
    assert_eq!(
        setting(&run, "build.cache.remote"),
        ("false".into(), "flag --no-remote-cache".into())
    );
    let run = workspace.build_with(&["--print-config"]);
    assert_eq!(
        setting(&run, "build.cache.enabled"),
        ("true".into(), "default".into())
    );
}

#[test]
fn print_config_stops_before_any_work() {
    let workspace = sources();
    workspace.workers("count = 1\nunit_timeout = \"3m\"");

    let run = workspace.build_with(&["--print-config"]);

    run.succeeded();
    assert!(
        run.stdout
            .contains(&format!("# root: {}", workspace.root().display())),
        "{}",
        run.stdout
    );
    assert_eq!(setting(&run, "build.workers.unit_timeout").0, "\"3m\"");
    assert!(workspace.log_lines().is_empty());
    assert!(!workspace.exists("build"));
    assert!(!workspace.exists(".oxabl"));
}

#[test]
fn print_config_as_json_gives_each_setting_its_value_and_source() {
    let workspace = Built::new();

    let run = with_overlay(
        &workspace,
        "[build.workers]\ncount = 2\n",
        &["--print-config", "--json"],
    );

    run.succeeded();
    let document: Value = serde_json::from_str(&run.stdout).unwrap();
    assert_eq!(document["format"], 1);
    assert_eq!(document["oxabl_toml"], true);
    assert_eq!(
        document["overlay"],
        workspace.path("overlay.toml").to_str().unwrap()
    );
    assert_eq!(document["settings"]["build.workers.count"]["value"], 2);
    assert_eq!(
        document["settings"]["build.workers.count"]["source"],
        "overlay"
    );
    assert_eq!(
        document["settings"]["workspace.sources.dlc"]["value"],
        Value::Null
    );
    assert_eq!(
        document["settings"]["workspace.sources.dlc"]["source"],
        "default"
    );
}

#[test]
fn print_config_does_not_combine_with_a_plan_or_a_build_only_flag() {
    let workspace = Built::new();
    for flag in ["--plan", "--probe", "--events", "--fail-fast"] {
        let run = workspace.build_with(&["--print-config", flag]);
        assert_eq!(run.code, 2, "{flag}: {}", run.stderr);
        assert!(run.stderr.contains("--print-config"), "{}", run.stderr);
    }
}

#[test]
fn a_key_that_an_overlay_may_not_set_is_exit_code_2() {
    for (overlay, named) in [
        ("[workspace.lint]\nenable = [\"strict.calls\"]\n", "lint"),
        ("[workspace.style]\nindent_size = 2\n", "style"),
        ("[biuld]\noutput = \"r\"\n", "biuld"),
        ("[lint]\nx = 1\n", "lint"),
        ("[workspace]\nlocation = \"x\"\n", "location"),
    ] {
        let workspace = sources();

        let run = with_overlay(&workspace, overlay, &[]);

        assert_refused(&workspace, &run, named);
        assert!(run.stderr.contains("overlay.toml"), "{}", run.stderr);
    }
}

#[test]
fn an_unknown_key_in_an_overlay_table_is_exit_code_2() {
    for (overlay, named) in [
        ("[build.workers]\ncuont = 2\n", "cuont"),
        ("[build]\noutptu = \"r\"\n", "outptu"),
        (
            "[workspace.sources]\ninclude_pahts = [\"x\"]\n",
            "include_pahts",
        ),
        ("[workspace.schema]\nfile = [\"x.df\"]\n", "file"),
        (
            "[workspace.preprocessor]\nproversoin = \"12.8\"\n",
            "proversoin",
        ),
    ] {
        let workspace = sources();

        let run = with_overlay(&workspace, overlay, &[]);

        assert_refused(&workspace, &run, named);
        assert!(run.stderr.contains("overlay.toml"), "{}", run.stderr);
    }
}

#[test]
fn an_overlay_that_does_not_parse_or_does_not_exist_is_exit_code_2() {
    let workspace = sources();
    let run = with_overlay(&workspace, "[build\noutput = ", &[]);
    assert_refused(&workspace, &run, "overlay.toml");

    let run = workspace.build_with(&["--config-overlay", "missing.toml"]);
    assert_refused(&workspace, &run, "missing.toml");

    let run = with_overlay(&workspace, "[build.workers]\ncount = \"two\"\n", &[]);
    assert_refused(&workspace, &run, "count");
}

#[test]
fn a_key_that_looks_like_a_secret_is_exit_code_2_and_its_value_is_never_printed() {
    for key in [
        "password",
        "DB_PASSWORD",
        "api-key",
        "Access_Token",
        "client_secret",
    ] {
        let workspace = sources();
        let overlay = format!("[build.workers]\nenv = {{ {key:?} = \"hunter2-value\" }}\n");

        let run = with_overlay(&workspace, &overlay, &[]);

        assert_refused(&workspace, &run, key);
        assert!(!run.stderr.contains("hunter2-value"), "{}", run.stderr);
        assert!(!run.stdout.contains("hunter2-value"), "{}", run.stdout);
    }
}

#[test]
fn a_key_that_looks_like_a_secret_in_oxabl_toml_is_exit_code_2() {
    let workspace = sources();
    workspace.worker_env("DB_PASSWORD", "hunter2-value");

    let run = workspace.build_with(&["--print-config"]);

    assert_refused(&workspace, &run, "DB_PASSWORD");
    assert!(!run.stderr.contains("hunter2-value"), "{}", run.stderr);
}

#[test]
fn a_trust_key_of_the_remote_cache_in_an_overlay_is_exit_code_2() {
    for key in ["tenant", "cache", "owner_keys", "min_roster_version"] {
        let workspace = sources();
        let overlay = format!("[build.cache]\n{key} = \"x\"\n");

        let run = with_overlay(&workspace, &overlay, &[]);

        assert_refused(&workspace, &run, key);
        assert!(run.stderr.contains("cache.toml"), "{}", run.stderr);
    }
    let workspace = sources();
    let run = with_overlay(
        &workspace,
        "[build.cache.remote]\nowner_keys = [\"x\"]\n",
        &[],
    );
    assert_refused(&workspace, &run, "remote");
}

#[test]
fn a_trust_key_of_the_remote_cache_in_oxabl_toml_is_exit_code_2_with_or_without_an_overlay() {
    for key in ["tenant", "cache", "owner_keys", "min_roster_version"] {
        let workspace = sources();
        workspace.reconfigure(
            "include_paths = [\"src\"]",
            &format!("[build.cache]\n{key} = \"x\"\n"),
        );

        let run = workspace.build();
        assert_refused(&workspace, &run, key);
        assert!(run.stderr.contains("cache.toml"), "{}", run.stderr);

        let run = with_overlay(&workspace, "[build.workers]\ncount = 1\n", &[]);
        assert_refused(&workspace, &run, key);
        assert!(run.stderr.contains("cache.toml"), "{}", run.stderr);
    }
}

#[test]
fn a_complete_overlay_works_without_oxabl_toml() {
    let workspace = sources();
    let complete = workspace.read("oxabl.toml");
    workspace.remove("oxabl.toml");
    workspace.write("complete.toml", &complete);
    let overlay = workspace.path("complete.toml");
    let overlay = overlay.to_str().unwrap();
    let root = workspace.root().to_str().unwrap();

    let run = workspace.build_with(&["--root", root, "--config-overlay", overlay, "--events"]);

    run.succeeded();
    let events = run.events();
    assert_eq!(of_kind(&events, "summary")[0]["compiled"], 3);
    assert!(workspace.exists("build/r/a.r"));
    let run = workspace.build_with(&[
        "--root",
        root,
        "--config-overlay",
        overlay,
        "--print-config",
    ]);
    let all = settings(&run);
    assert!(run.stdout.contains("# oxabl.toml: none"), "{}", run.stdout);
    assert!(
        all.values().all(|(_, source)| source != "oxabl.toml"),
        "{}",
        run.stdout
    );
    assert_eq!(all["workspace.name"].1, "overlay");
}

#[test]
fn an_overlay_without_oxabl_toml_must_name_the_workspace() {
    let workspace = sources();
    workspace.remove("oxabl.toml");
    workspace.write("overlay.toml", "[build]\noutput = \"build/r\"\n");
    let root = workspace.root().to_str().unwrap();

    let run = workspace.build_with(&["--root", root, "--config-overlay", "overlay.toml"]);

    assert_refused(&workspace, &run, "name");
}

#[test]
fn a_workspace_with_no_oxabl_toml_and_no_overlay_is_exit_code_2() {
    let workspace = sources();
    workspace.remove("oxabl.toml");
    let root = workspace.root().to_str().unwrap();

    let run = workspace.build_with(&["--root", root]);

    assert_refused(&workspace, &run, "oxabl.toml");
}

#[test]
fn the_overlay_variable_names_an_overlay_and_the_flag_wins() {
    let workspace = Built::new();
    workspace.write("from-env.toml", "[build.workers]\ncount = 2\n");
    workspace.write("from-flag.toml", "[build.workers]\ncount = 3\n");
    let env_path = workspace.path("from-env.toml");
    workspace.env("OXABL_BUILD_OVERLAY", Some(env_path.to_str().unwrap()));

    let run = workspace.build_with(&["--print-config"]);
    assert_eq!(
        setting(&run, "build.workers.count"),
        ("2".into(), "overlay".into())
    );
    assert!(
        run.stdout.contains("(OXABL_BUILD_OVERLAY)"),
        "{}",
        run.stdout
    );

    let run = workspace.build_with(&["--print-config", "--config-overlay", "from-flag.toml"]);
    assert_eq!(
        setting(&run, "build.workers.count"),
        ("3".into(), "overlay".into())
    );
    assert!(run.stdout.contains("(--config-overlay)"), "{}", run.stdout);
    assert!(!run.stdout.contains("from-env.toml"), "{}", run.stdout);
}
