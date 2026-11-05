//! `oxabl build --probe` against the fake AVM: the handshake, the environment
//! identity and the schema image, as the worker protocol specifies them.

mod common;

use std::fs;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use common::{
    Workspace, env_with_schema, identity, probe_json, process_exists, run, schema_identity,
    started_pids, stderr, stdout,
};
use serde_json::{Value, json};

/// The values of the worked example in the compile key specification
/// (`docs/design/build-tool/compile-key.md`), taken from the document and not
/// from this code. `example_environment` holds the document's inputs.
const EXAMPLE_ENVIRONMENT_IDENTITY: &str =
    "88379412ea5b9c91d75b53fed5c30c3ed18048293664ed0ec0deff2ec441a079";
const EXAMPLE_SCHEMA_IDENTITY: &str =
    "8a495ef6b5c1d99915758a8773441ab7adf1b6abe407bf19c1a4f097ac62a199";

fn example_schema_identity() -> String {
    EXAMPLE_SCHEMA_IDENTITY.to_string()
}

fn example_environment() -> Value {
    json!({
        "env": { "dlc_version": "OpenEdge Release 12.8.9", "cpcoll": "ICU-UCA" },
        "parameters": ["-cpinternal UTF-8", "-cpstream UTF-8", "-d mdy", "-inp 32000"],
        "databases": [{
            "ldb": "ledger",
            "aliases": ["dictdb"],
            "schema": [
                ["db", "_db-coll-name=ICU-UCA", "_db-type=PROGRESS", "_db-xl-name=UTF-8"],
                ["table", "customer", "_can-read=*", "_crc=12345", "_dump-name=customer",
                 "_prime-index=index/customer/custnum"],
                ["field", "customer", "custnum", "_data-type=integer", "_decimals=?",
                 "_format=>>>>9", "_label=Cust Num", "_view-as=?"],
                ["field", "customer", "name", "_data-type=character", "_decimals=?",
                 "_format=x(30)", "_label=Name", "_view-as=?"],
                ["index", "customer", "custnum", "_active=yes", "_unique=yes"],
                ["index-field", "customer", "custnum", "1", "custnum", "_ascending=yes"],
                ["sequence", "next-cust", "_cycle-ok=no", "_seq-incr=1", "_seq-init=1"]
            ]
        }]
    })
}

fn workspace_with(env: &Value, workers: &str) -> Workspace {
    let workspace = Workspace::new(workers, &["--env", "{root}/env.json"]);
    workspace.env_file("env.json", env);
    let names: Vec<&str> = env["databases"]
        .as_array()
        .into_iter()
        .flatten()
        .filter_map(|database| database["ldb"].as_str())
        .collect();
    workspace.declare_databases(&names);
    workspace
}

#[test]
fn the_probe_prints_the_environment_and_schema_identities() {
    let workspace = workspace_with(&example_environment(), "count = 2\n");

    let report = probe_json(&workspace, &[]);

    assert_eq!(report["environment"]["source"], "probe");
    assert_eq!(identity(&report), EXAMPLE_ENVIRONMENT_IDENTITY);
    let database = &report["environment"]["databases"][0];
    assert_eq!(database["name"], "ledger");
    assert_eq!(
        database["schema_identity"],
        example_schema_identity().as_str()
    );
    assert_eq!(report["workers"], json!({ "started": 2, "requested": 2 }));

    let text = run(workspace.probe(&[]));
    assert_eq!(text.status.code(), Some(0), "{}", stderr(&text));
    let text = stdout(&text);
    assert!(text.contains(EXAMPLE_ENVIRONMENT_IDENTITY), "{text}");
    assert!(text.contains(&example_schema_identity()), "{text}");
}

#[test]
fn noise_lines_do_not_break_the_handshake_and_reach_the_log() {
    let workspace = workspace_with(&example_environment(), "count = 1\n");

    let report = probe_json(&workspace, &[("FAKE_AVM_NOISE", "1")]);

    assert_eq!(identity(&report), EXAMPLE_ENVIRONMENT_IDENTITY);
    assert!(
        workspace
            .worker_logs()
            .contains("** noise: session starting")
    );
}

#[test]
fn a_worker_that_exits_during_the_handshake_is_started_again_twice_then_exit_3() {
    let workspace = workspace_with(
        &example_environment(),
        "count = 2\nstart_timeout = \"5s\"\n",
    );
    let log = workspace.fake_log();

    let mut command = workspace.probe(&[]);
    command
        .env("FAKE_AVM_EXIT_IN_HANDSHAKE", "1")
        .env("FAKE_AVM_LOG", &log);
    let output = run(command);

    assert_eq!(output.status.code(), Some(3), "{}", stderr(&output));
    let message = stderr(&output);
    assert!(message.contains("no worker could start"), "{message}");
    assert!(message.contains("last 20 lines of"), "{message}");
    assert_eq!(
        started_pids(&log).len(),
        6,
        "two slots start three times each"
    );
}

#[test]
fn a_worker_that_exits_once_is_replaced_and_the_probe_succeeds() {
    let workspace = workspace_with(
        &example_environment(),
        "count = 1\nstart_timeout = \"5s\"\n",
    );
    let state = workspace.path("fake-state");
    fs::create_dir_all(&state).unwrap();
    let log = workspace.fake_log();

    let mut command = workspace.probe(&["--json"]);
    command
        .env("FAKE_AVM_EXIT_IN_HANDSHAKE_ONCE", "1")
        .env("FAKE_AVM_STATE", &state)
        .env("FAKE_AVM_LOG", &log);
    let output = run(command);

    assert_eq!(output.status.code(), Some(0), "{}", stderr(&output));
    let report: Value = serde_json::from_str(&stdout(&output)).unwrap();
    assert_eq!(identity(&report), EXAMPLE_ENVIRONMENT_IDENTITY);
    assert_eq!(started_pids(&log).len(), 2);
}

#[test]
fn a_slow_start_past_the_start_timeout_is_retried_killed_and_exit_3() {
    let workspace = workspace_with(
        &example_environment(),
        "count = 1\nstart_timeout = \"1s\"\n",
    );
    let log = workspace.fake_log();

    let mut command = workspace.probe(&[]);
    command
        .env("FAKE_AVM_START_DELAY", "20000")
        .env("FAKE_AVM_LOG", &log);
    let started = Instant::now();
    let output = run(command);

    assert_eq!(output.status.code(), Some(3), "{}", stderr(&output));
    assert!(
        started.elapsed() < Duration::from_secs(15),
        "the timeout did not cut the start"
    );
    let pids = started_pids(&log);
    assert_eq!(pids.len(), 3, "one start and two restarts");
    for pid in pids {
        assert!(!process_exists(pid), "worker {pid} is still alive");
    }
}

#[test]
fn a_wrong_program_token_is_a_protocol_error() {
    let workspace = workspace_with(
        &example_environment(),
        "count = 1\nstart_timeout = \"5s\"\n",
    );

    let mut command = workspace.probe(&[]);
    command.env("FAKE_AVM_BAD_TOKEN", "1");
    let output = run(command);

    assert_eq!(output.status.code(), Some(3), "{}", stderr(&output));
    assert!(
        stderr(&output).contains("session token"),
        "{}",
        stderr(&output)
    );
}

#[test]
fn workers_with_different_environments_stop_the_build_with_exit_2() {
    let workspace = Workspace::new("count = 2\n", &["--env", "{root}/env-{worker}.json"]);
    workspace.env_file(
        "env-1.json",
        &env_with_schema(vec![vec!["db".into(), "_db-type=PROGRESS".into()]]),
    );
    workspace.env_file(
        "env-2.json",
        &json!({
            "env": { "date_format": "dmy" },
            "databases": [{ "ldb": "ledger", "stamp": "other", "schema": [["db", "_db-type=OTHER"]] }]
        }),
    );
    workspace.declare_databases(&["ledger"]);
    let output = run(workspace.probe(&[]));

    assert_eq!(output.status.code(), Some(2), "{}", stderr(&output));
    let message = stderr(&output);
    assert!(
        message.contains("workers see different environments"),
        "{message}"
    );
    assert!(message.contains("date_format: mdy vs dmy"), "{message}");
    assert!(message.contains("fingerprint ledger"), "{message}");
}

#[test]
fn a_proversion_that_differs_from_the_worker_names_both_values() {
    let workspace = workspace_with(&example_environment(), "count = 1\n");
    let config = fs::read_to_string(workspace.path("oxabl.toml")).unwrap();
    workspace.write("oxabl.toml", &config.replace("\"12.8\"", "\"11.7\""));

    let output = run(workspace.probe(&[]));

    assert_eq!(output.status.code(), Some(2), "{}", stderr(&output));
    let message = stderr(&output);
    assert!(
        message.contains("11.7") && message.contains("12.8"),
        "{message}"
    );
    assert!(message.contains("proversion"), "{message}");
}

#[test]
fn an_unset_proversion_is_a_configuration_error() {
    let workspace = workspace_with(&example_environment(), "count = 1\n");
    let config = fs::read_to_string(workspace.path("oxabl.toml")).unwrap();
    workspace.write("oxabl.toml", &config.replace("proversion = \"12.8\"", ""));

    let output = run(workspace.probe(&[]));

    assert_eq!(output.status.code(), Some(2), "{}", stderr(&output));
    assert!(
        stderr(&output).contains("proversion is not set"),
        "{}",
        stderr(&output)
    );
}

#[test]
fn every_predefined_name_must_agree_with_the_worker() {
    let cases = [
        ("opsys = \"WIN32\"", "opsys"),
        ("window-system = \"MS-WINDOWS\"", "window-system"),
        ("batch-mode = false", "batch-mode"),
        ("process-architecture = 32", "process-architecture"),
        (
            "database-types = { ledger = \"ORACLE\" }",
            "database-types.ledger",
        ),
    ];
    for (line, name) in cases {
        let workspace = workspace_with(&example_environment(), "count = 1\n");
        workspace.preprocessor_line(line);

        let output = run(workspace.probe(&[]));

        assert_eq!(output.status.code(), Some(2), "{line}: {}", stderr(&output));
        assert!(
            stderr(&output).contains(name),
            "{line}: {}",
            stderr(&output)
        );
    }
    let workspace = Workspace::new("count = 1\n", &["--env", "{root}/env.json"]);
    workspace.env_file("env.json", &example_environment());
    workspace.declare_databases(&["other", "ledger"]);
    let output = run(workspace.probe(&[]));
    assert_eq!(output.status.code(), Some(2), "{}", stderr(&output));
    assert!(stderr(&output).contains("databases"), "{}", stderr(&output));
}

#[test]
fn databases_that_the_analysis_does_not_know_are_a_disagreement() {
    let workspace = Workspace::new("count = 1\n", &["--env", "{root}/env.json"]);
    workspace.env_file("env.json", &example_environment());

    // The worker connects `ledger`; neither `databases` nor a schema file names it.
    let output = run(workspace.probe(&[]));

    assert_eq!(output.status.code(), Some(2), "{}", stderr(&output));
    assert!(stderr(&output).contains("databases"), "{}", stderr(&output));
}

#[test]
fn no_worker_command_is_exit_2_with_the_message_of_the_design() {
    let workspace = Workspace::bare();
    workspace.write("oxabl.toml", "[workspace]\nname = \"probe\"\n");

    let output = run(workspace.probe(&[]));

    assert_eq!(output.status.code(), Some(2));
    assert!(
        stderr(&output).contains("oxabl build needs a worker command; set [build.workers].command"),
        "{}",
        stderr(&output)
    );
    let without_probe = run({
        let mut command = Command::new(env!("CARGO_BIN_EXE_oxabl"));
        command.current_dir(workspace.root()).arg("build");
        command
    });
    assert_eq!(without_probe.status.code(), Some(2));
    assert!(stderr(&without_probe).contains("needs a worker command"));
}

#[test]
fn an_unknown_placeholder_in_the_command_is_a_configuration_error() {
    let workspace = Workspace::new("count = 1\n", &["{nope}"]);

    let output = run(workspace.probe(&[]));

    assert_eq!(output.status.code(), Some(2), "{}", stderr(&output));
    assert!(stderr(&output).contains("{nope}"), "{}", stderr(&output));
}

#[test]
fn a_worker_outside_the_worker_root_is_a_configuration_error() {
    let env = json!({ "env": { "cwd": "/elsewhere" } });
    let workspace = workspace_with(&env, "count = 1\n");

    let output = run(workspace.probe(&[]));

    assert_eq!(output.status.code(), Some(2), "{}", stderr(&output));
    assert!(
        stderr(&output).contains("/elsewhere"),
        "{}",
        stderr(&output)
    );
}

fn parameters_environment(parameters: Value) -> Value {
    json!({ "parameters": parameters, "databases": [] })
}

fn parameter_pairs(report: &Value) -> Vec<(String, String)> {
    report["environment"]["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|p| {
            (
                p["name"].as_str().unwrap().into(),
                p["value"].as_str().unwrap().into(),
            )
        })
        .collect()
}

#[test]
fn parameters_from_a_pf_file_equal_the_same_parameters_on_the_command_line() {
    let on_command_line = workspace_with(
        &parameters_environment(json!(["-d mdy", "-inp 32000"])),
        "count = 1\n",
    );
    let from_pf = workspace_with(
        &parameters_environment(
            json!([{ "pf": "/work/build.pf", "contents": ["-d mdy", "-inp 32000"] }]),
        ),
        "count = 1\n",
    );

    let first = probe_json(&on_command_line, &[]);
    let second = probe_json(&from_pf, &[]);

    assert_eq!(identity(&first), identity(&second));
    assert_eq!(parameter_pairs(&first), parameter_pairs(&second));
    assert_eq!(
        parameter_pairs(&second),
        vec![
            ("-d".to_string(), "mdy".to_string()),
            ("-inp".to_string(), "32000".to_string())
        ]
    );
    let text = serde_json::to_string(&second).unwrap();
    assert!(
        !text.contains("-pf") && !text.contains("(end .pf)"),
        "{text}"
    );
}

#[test]
fn connection_parameters_and_secrets_never_reach_the_output_or_the_identity() {
    let plain = workspace_with(&parameters_environment(json!(["-d mdy"])), "count = 1\n");
    let secret = workspace_with(
        &parameters_environment(json!([
            "-U admin",
            "-P hunter2",
            "-db /data/ledger",
            "-d mdy",
            "-H dbhost"
        ])),
        "count = 1\n",
    );

    let with_secrets = run({
        let mut command = secret.probe(&["--json"]);
        command.stdout(Stdio::piped());
        command
    });
    let report: Value = serde_json::from_str(&stdout(&with_secrets)).unwrap();

    assert_eq!(identity(&report), identity(&probe_json(&plain, &[])));
    let everything = format!(
        "{}{}{}",
        stdout(&with_secrets),
        stderr(&with_secrets),
        secret.worker_logs()
    );
    for secret_value in ["hunter2", "admin", "/data/ledger", "dbhost"] {
        assert!(
            !everything.contains(secret_value),
            "{secret_value} leaked: {everything}"
        );
    }
}

#[test]
fn a_file_valued_parameter_counts_by_content_and_a_directory_makes_the_environment_unknown() {
    let env = parameters_environment(json!([
        "-compileroptionsfile opts.txt",
        "-assemblies assemblies"
    ]));
    let workspace = workspace_with(&env, "count = 1\n");
    workspace.write("opts.txt", "first");
    fs::create_dir_all(workspace.path("assemblies")).unwrap();

    let first = probe_json(&workspace, &[]);
    workspace.write("opts.txt", "second");
    let second = probe_json(&workspace, &[]);

    assert_ne!(identity(&first), identity(&second));
    let value = |report: &Value, name: &str| {
        parameter_pairs(report)
            .into_iter()
            .find(|(n, _)| n == name)
            .unwrap()
            .1
    };
    assert!(value(&first, "-compileroptionsfile").starts_with("sha256:"));
    assert_ne!(
        value(&first, "-compileroptionsfile"),
        value(&second, "-compileroptionsfile")
    );
    assert_eq!(value(&first, "-assemblies"), "assemblies");
    assert_eq!(
        first["environment"]["unknown"],
        json!(["unhashable-parameter: -assemblies"])
    );
}

#[test]
fn a_schema_field_of_20_kb_with_non_ascii_characters_is_hashed_whole() {
    let long: String = "é日x%".repeat(5_000);
    assert!(long.len() > 20_000);
    let records = vec![
        vec!["db".to_string(), "_db-type=PROGRESS".to_string()],
        vec![
            "table".to_string(),
            "customer".to_string(),
            format!("_desc={long}"),
            String::new(),
        ],
        vec![
            "field".to_string(),
            "customer".to_string(),
            "name".to_string(),
            "_label=Name".to_string(),
        ],
        vec![
            "sequence".to_string(),
            "s1".to_string(),
            "_incr=1".to_string(),
        ],
    ];
    let workspace = workspace_with(&env_with_schema(records.clone()), "count = 1\n");

    let report = probe_json(&workspace, &[]);

    let database = &report["environment"]["databases"][0];
    assert_eq!(
        database["schema_identity"],
        schema_identity(&records).as_str()
    );
    assert_eq!(database["records"], 4);
    assert_eq!(database["entries"], 3);
}

#[test]
fn a_bad_schema_entry_or_a_wrong_schema_end_loses_the_worker() {
    let records = vec![vec!["db".to_string(), "_db-type=PROGRESS".to_string()]];
    for (fault, expected) in [
        ("duplicate-entry", "appears twice"),
        ("bad-hash", "bad hash"),
        ("bad-count", "SCHEMA-END"),
    ] {
        let workspace = workspace_with(
            &env_with_schema(records.clone()),
            "count = 1\nstart_timeout = \"5s\"\n",
        );

        let mut command = workspace.probe(&[]);
        command.env("FAKE_AVM_SCHEMA_FAULT", fault);
        let output = run(command);

        assert_eq!(
            output.status.code(),
            Some(3),
            "{fault}: {}",
            stderr(&output)
        );
        assert!(
            stderr(&output).contains(expected),
            "{fault}: {}",
            stderr(&output)
        );
    }
}

#[test]
fn libraries_under_dlc_are_hashed_and_the_others_are_named_in_a_warning() {
    let workspace = Workspace::new("count = 1\n", &["--env", "{root}/env.json"]);
    workspace.write("dlc/tty/OpenEdge.Core.pl", "library bytes");
    workspace.env_file(
        "env.json",
        &json!({
            "env": { "dlc": "/usr/dlc", "propath": ["/usr/dlc/tty/OpenEdge.Core.pl", "/elsewhere/x.pl"] },
            "dlc_dir": workspace.path("dlc"),
            "databases": []
        }),
    );

    let output = run(workspace.probe(&["--json"]));

    assert_eq!(output.status.code(), Some(0), "{}", stderr(&output));
    let report: Value = serde_json::from_str(&stdout(&output)).unwrap();
    assert_eq!(
        report["environment"]["libraries"],
        json!([{
            "entry": "$DLC/tty/OpenEdge.Core.pl",
            "digest": "8c45ce4e5a5cb0c03670ca09e153a24b1c7b7f94141c4da68f31ce8e91bc1bf8"
        }])
    );
    assert!(
        stderr(&output).contains("/elsewhere/x.pl"),
        "{}",
        stderr(&output)
    );
}

#[test]
fn workers_that_see_different_dlc_libraries_disagree() {
    let workspace = Workspace::new("count = 2\n", &["--env", "{root}/env-{worker}.json"]);
    for worker in [1, 2] {
        workspace.write(
            &format!("dlc-{worker}/tty/OpenEdge.Core.pl"),
            &format!("library {worker}"),
        );
        workspace.env_file(
            &format!("env-{worker}.json"),
            &json!({
                "env": { "dlc": "/usr/dlc", "propath": ["/usr/dlc/tty/OpenEdge.Core.pl"] },
                "dlc_dir": workspace.path(&format!("dlc-{worker}")),
                "databases": []
            }),
        );
    }

    let output = run(workspace.probe(&[]));

    assert_eq!(output.status.code(), Some(2), "{}", stderr(&output));
    let message = stderr(&output);
    assert!(
        message.contains("workers see different environments"),
        "{message}"
    );
    assert!(
        message.contains("library $DLC/tty/OpenEdge.Core.pl"),
        "{message}"
    );
}

#[test]
fn a_cancel_during_the_start_stops_every_worker_and_exits_130() {
    let workspace = workspace_with(
        &example_environment(),
        "count = 2\nstart_timeout = \"60s\"\n",
    );
    let log = workspace.fake_log();
    let mut command = workspace.probe(&[]);
    command
        .env("FAKE_AVM_START_DELAY", "30000")
        .env("FAKE_AVM_LOG", &log)
        .stdout(Stdio::null())
        .stderr(Stdio::null());
    let mut child = command.spawn().unwrap();
    let deadline = Instant::now() + Duration::from_secs(20);
    while started_pids(&log).len() < 2 && Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(50));
    }
    assert_eq!(started_pids(&log).len(), 2, "both workers started");

    let status = Command::new("kill")
        .args(["-INT", &child.id().to_string()])
        .status()
        .unwrap();
    assert!(status.success());
    let signalled = Instant::now();
    let exit = child.wait().unwrap();

    assert_eq!(exit.code(), Some(130));
    assert!(
        signalled.elapsed() < Duration::from_secs(10),
        "the cancel did not stop the start"
    );
    for pid in started_pids(&log) {
        assert!(!process_exists(pid), "worker {pid} is still alive");
    }
}

#[test]
fn build_stays_out_of_the_help() {
    let output = run({
        let mut command = Command::new(env!("CARGO_BIN_EXE_oxabl"));
        command.arg("--help");
        command
    });
    assert!(!stdout(&output).contains("build"), "{}", stdout(&output));
}

#[test]
fn the_order_of_the_schema_entries_does_not_change_the_identity() {
    let records = example_environment()["databases"][0]["schema"].clone();
    let records = records.as_array().unwrap();
    // The entries are the database record, the table block and the sequence.
    let reordered: Vec<Value> = [&records[6..], &records[1..6], &records[..1]].concat();
    let mut env = example_environment();
    env["databases"][0]["schema"] = json!(reordered);
    let workspace = workspace_with(&env, "count = 1\n");

    let report = probe_json(&workspace, &[]);

    assert_eq!(identity(&report), EXAMPLE_ENVIRONMENT_IDENTITY);
}

#[test]
fn names_aliases_and_trailing_white_space_are_canonical() {
    let mut env = example_environment();
    env["env"]["dlc_version"] = json!("OpenEdge Release 12.8.9   ");
    env["databases"][0]["ldb"] = json!("LEDGER");
    env["databases"][0]["aliases"] = json!(["DICTDB"]);
    let workspace = workspace_with(&env, "count = 1\n");

    let report = probe_json(&workspace, &[]);

    assert_eq!(identity(&report), EXAMPLE_ENVIRONMENT_IDENTITY);
}

fn many_tables_environment() -> Value {
    let mut records = vec![vec!["db".to_string(), "_db-type=PROGRESS".to_string()]];
    for table in ["zeta", "alpha", "mid", "beta", "omega", "gamma", "delta"] {
        records.push(vec![
            "table".to_string(),
            table.to_string(),
            format!("_desc=table {table}"),
        ]);
        records.push(vec![
            "field".to_string(),
            table.to_string(),
            "id".to_string(),
            "_label=Id".to_string(),
        ]);
    }
    records.push(vec![
        "sequence".to_string(),
        "s1".to_string(),
        "_incr=1".to_string(),
    ]);
    env_with_schema(records)
}

fn schema_records(env: &Value) -> Vec<Vec<String>> {
    serde_json::from_value(env["databases"][0]["schema"].clone()).unwrap()
}

#[test]
fn every_worker_images_a_share_of_every_database_and_the_identity_ignores_the_split() {
    let env = many_tables_environment();
    let expected = schema_identity(&schema_records(&env));
    for workers in [1, 2, 3, 8] {
        let workspace = workspace_with(&env, &format!("count = {workers}\n"));
        let log = workspace.fake_log();

        let report = probe_json(&workspace, &[("FAKE_AVM_LOG", log.to_str().unwrap())]);

        let database = &report["environment"]["databases"][0];
        assert_eq!(
            database["schema_identity"],
            expected.as_str(),
            "{workers} workers"
        );
        assert_eq!(database["entries"], 9);
        let text = fs::read_to_string(&log).unwrap();
        for part in 0..workers {
            let wanted = format!(" schema ledger {part}/{workers}");
            let asked = text.lines().filter(|line| line.ends_with(&wanted)).count();
            assert_eq!(asked, 1, "share {part} of {workers}:\n{text}");
        }
    }
}

#[test]
fn a_worker_that_skips_its_share_fails_the_build() {
    let workspace = workspace_with(&many_tables_environment(), "count = 3\n");

    let mut command = workspace.probe(&[]);
    command.env("FAKE_AVM_SKIP_SHARE", "1");
    let output = run(command);

    assert_eq!(output.status.code(), Some(3), "{}", stderr(&output));
    let message = stderr(&output);
    assert!(
        message.contains("has 7 tables but the workers imaged"),
        "{message}"
    );
}

#[test]
fn workers_that_count_different_tables_fail_the_build() {
    let workspace = workspace_with(&many_tables_environment(), "count = 3\n");

    let mut command = workspace.probe(&[]);
    command.env("FAKE_AVM_COUNT_SKEW", "2");
    let output = run(command);

    assert_eq!(output.status.code(), Some(3), "{}", stderr(&output));
    assert!(
        stderr(&output).contains("workers count 7 and 8 tables"),
        "{}",
        stderr(&output)
    );
}

#[test]
fn a_cancel_by_sigterm_stops_every_worker_and_exits_130() {
    let workspace = workspace_with(
        &example_environment(),
        "count = 2\nstart_timeout = \"60s\"\n",
    );
    let log = workspace.fake_log();
    let mut command = workspace.probe(&[]);
    command
        .env("FAKE_AVM_START_DELAY", "30000")
        .env("FAKE_AVM_LOG", &log)
        .stdout(Stdio::null())
        .stderr(Stdio::null());
    let mut child = command.spawn().unwrap();
    let deadline = Instant::now() + Duration::from_secs(20);
    while started_pids(&log).len() < 2 && Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(50));
    }
    assert_eq!(started_pids(&log).len(), 2, "both workers started");

    let status = Command::new("kill")
        .args(["-TERM", &child.id().to_string()])
        .status()
        .unwrap();
    assert!(status.success());
    let signalled = Instant::now();
    let exit = child.wait().unwrap();

    assert_eq!(exit.code(), Some(130));
    assert!(signalled.elapsed() < Duration::from_secs(10));
    for pid in started_pids(&log) {
        assert!(!process_exists(pid), "worker {pid} is still alive");
    }
}
