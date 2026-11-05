//! What the environment of a real AVM holds: startup parameters, libraries and
//! the `$DLC` search path.

use sha2::{Digest, Sha256};

use crate::common::openedge::compile::{
    Compile, Options, compile_workspace, compile_workspace_with,
};
use crate::common::openedge::worker::library_program;
use crate::common::openedge::{Target, probe};
use crate::setup;

fn parameters(report: &serde_json::Value) -> Vec<(String, String)> {
    report["environment"]["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|parameter| {
            (
                parameter["name"].as_str().unwrap().to_string(),
                parameter["value"].as_str().unwrap().to_string(),
            )
        })
        .collect()
}

fn with_arguments(target: &Target, arguments: &[&str]) -> Target {
    let mut changed = target.clone();
    changed
        .command
        .extend(arguments.iter().map(|argument| argument.to_string()));
    changed
}

/// A parameter that only a `.pf` sets is in the probe, and its canonical form
/// equals the one of the same parameter on the command line.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn startup_pf() {
    let target = setup();
    let workspace = compile_workspace(&target);
    workspace.write("extra.pf", "-Bt 350\n");
    let pf = format!("{}/extra.pf", workspace.worker);

    let (from_pf, _pf_guard) = probe(&with_arguments(&target, &["-pf", &pf]), 1);
    let (from_line, _line_guard) = probe(&with_arguments(&target, &["-Bt", "350"]), 1);

    let named = parameters(&from_pf);
    assert!(
        named.contains(&("-Bt".to_string(), "350".to_string())),
        "the .pf parameter is missing: {named:?}"
    );
    assert_eq!(named, parameters(&from_line));
    for (name, value) in &named {
        assert!(
            name != "-pf" && !value.contains("(end .pf)"),
            "{name} {value}"
        );
    }
    assert_eq!(
        from_pf["environment"]["identity"],
        from_line["environment"]["identity"]
    );
}

/// A file-valued parameter counts by the bytes of its file. The AVM reads the
/// file at its start, and accepts a comment line.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn param_file_hash() {
    let target = setup();
    let workspace = compile_workspace(&target);
    let file = format!("{}/options.txt", workspace.worker);
    let with_file = with_arguments(&target, &["-compileroptionsfile", &file]);
    let value = |report: &serde_json::Value| {
        parameters(report)
            .into_iter()
            .find(|(name, _)| name == "-compileroptionsfile")
            .unwrap_or_else(|| panic!("-compileroptionsfile is not in the probe"))
            .1
    };

    workspace.write("options.txt", "# first\n");
    let (first, _first_guard) = probe(&with_file, 1);
    workspace.write("options.txt", "# second\n");
    let (second, _second_guard) = probe(&with_file, 1);

    assert_eq!(
        value(&first),
        format!("sha256:{}", hex(&Sha256::digest(b"# first\n")))
    );
    assert_eq!(
        value(&second),
        format!("sha256:{}", hex(&Sha256::digest(b"# second\n")))
    );
    assert_ne!(
        first["environment"]["identity"],
        second["environment"]["identity"]
    );
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}

/// `HASH` gives the SHA-256 of a `.pl`, and a changed `.pl` changes the keys of
/// the units that have it on their PROPATH.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn library_hash() {
    let target = setup();
    let workspace = compile_workspace_with(
        &target,
        Options {
            sources: "directories = [\"src\"]\ninclude_paths = [\"src\", \"$LIB:acme\"]".into(),
            extra: "[build.libraries]\nacme = \"libs/acme.pl\"".into(),
            ..Options::default()
        },
    );
    workspace.write("libsrc/hello.p", "PUT UNFORMATTED \"hello\" SKIP.\n");
    workspace.write("libsrc/second.p", "PUT UNFORMATTED \"second\" SKIP.\n");
    workspace.write("src/user.p", "RUN hello.p.\n");
    let printed = workspace.run_program(
        &target,
        &library_program(&workspace.worker, &[("hello.p", "hello.r")], true),
    );
    assert!(printed.contains("LIBRARY DONE"), "{printed}");
    let digest = |workspace: &Compile| hex(&Sha256::digest(workspace.bytes("libs/acme.pl")));
    let entry_digest = |plan: &serde_json::Value| {
        plan["propaths"][0]
            .as_array()
            .unwrap()
            .iter()
            .find(|entry| entry["entry"] == "$LIB:acme")
            .unwrap_or_else(|| panic!("$LIB:acme is not on the PROPATH: {}", plan["propaths"]))["digest"]
            .as_str()
            .unwrap()
            .to_string()
    };
    let key = |plan: &serde_json::Value| {
        plan["units"]
            .as_array()
            .unwrap()
            .iter()
            .find(|unit| unit["path"] == "src/user.p")
            .unwrap()["key"]
            .as_str()
            .unwrap_or_else(|| panic!("src/user.p has no key: {plan}"))
            .to_string()
    };

    let before = workspace.plan();
    let first_digest = digest(&workspace);
    let printed = workspace.run_program(
        &target,
        &library_program(&workspace.worker, &[("second.p", "second.r")], false),
    );
    assert!(printed.contains("LIBRARY DONE"), "{printed}");
    let after = workspace.plan();

    assert_eq!(entry_digest(&before), format!("sha256:{first_digest}"));
    assert_eq!(
        entry_digest(&after),
        format!("sha256:{}", digest(&workspace))
    );
    assert_ne!(
        first_digest,
        digest(&workspace),
        "the library did not change"
    );
    assert_ne!(key(&before), key(&after));
}

/// A unit that uses an `OpenEdge.*` class compiles with no `$DLC` entry in
/// `include_paths`: the worker's own default entries are on every PROPATH.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn dlc_default_entries() {
    let target = setup();
    let workspace = compile_workspace(&target);
    workspace.write(
        "src/usecore.p",
        "USING OpenEdge.Core.String FROM PROPATH.\nDEFINE VARIABLE core AS CLASS OpenEdge.Core.String NO-UNDO.\ncore = NEW OpenEdge.Core.String(\"abc\").\nPUT UNFORMATTED core:ToString() SKIP.\n",
    );

    let report = workspace.build_report();

    assert_eq!(report["summary"]["compiled"], 1, "{report}");
    assert_eq!(report["summary"]["failed"], 0, "{report}");
    let plan = workspace.plan();
    let entries: Vec<&str> = plan["propaths"][0]
        .as_array()
        .unwrap()
        .iter()
        .map(|entry| entry["entry"].as_str().unwrap())
        .collect();
    assert!(
        entries
            .iter()
            .any(|entry| entry.starts_with("$DLC/") && entry.contains("OpenEdge.Core")),
        "no $DLC default entry on the PROPATH: {entries:?}"
    );
}
