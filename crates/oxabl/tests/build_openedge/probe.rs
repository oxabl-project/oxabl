//! The worker transport and the probe of a real AVM.

use std::time::Duration;

use crate::common::openedge::compile::compile_workspace;
use crate::common::openedge::worker::Session;
use crate::common::openedge::{is_hex64, probe};
use crate::{list, setup};

/// Echoes every line of stdin, after a `READY` line, until `QUIT`.
const ECHO: &str = "DEFINE VARIABLE line AS CHARACTER NO-UNDO.
PUT UNFORMATTED \"READY\" SKIP.
INPUT FROM VALUE(\"/dev/stdin\").
REPEAT:
    IMPORT UNFORMATTED line.
    IF line = \"QUIT\" THEN LEAVE.
    PUT UNFORMATTED \"ECHO \" + STRING(LENGTH(line)) + \" \" + line SKIP.
END.
PUT UNFORMATTED \"BYE\" SKIP.
QUIT.
";

/// A batch worker reads stdin line by line, and each `PUT UNFORMATTED ... SKIP`
/// reaches stdout while stdin is still open, a 30,000-byte line included.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn transport() {
    let target = setup();
    let workspace = compile_workspace(&target);
    let mut session = Session::start(&target, &workspace.host, &workspace.worker, ECHO);

    assert_eq!(
        session.line(Duration::from_secs(120)).as_deref(),
        Some("READY"),
        "the worker did not start"
    );
    let long = "x".repeat(30_000);
    for (length, line) in [(5, "hello"), (30_000, long.as_str()), (11, "last of all")] {
        session.send(line);
        let reply = session
            .line(Duration::from_secs(10))
            .unwrap_or_else(|| panic!("no reply to a line of {length} bytes while stdin is open"));
        assert_eq!(reply, format!("ECHO {length} {line}"));
    }
    session.send("QUIT");
    assert_eq!(
        session.line(Duration::from_secs(10)).as_deref(),
        Some("BYE")
    );
    assert_eq!(session.finish(), Some(0));
}

/// Every field of the handshake is there, in its form.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn probe_fields() {
    let target = setup();

    let (report, _guard) = probe(&target, 1);

    let environment = &report["environment"];
    assert_eq!(environment["source"], "probe");
    assert_eq!(environment["unknown"].as_array().unwrap().len(), 0);
    let fields = environment["fields"].as_object().unwrap();
    let mut names: Vec<&str> = fields.keys().map(String::as_str).collect();
    names.sort_unstable();
    assert_eq!(
        names,
        [
            "batch_mode",
            "cpcase",
            "cpcoll",
            "cpinternal",
            "cpstream",
            "cwd",
            "date_format",
            "dlc",
            "dlc_version",
            "numeric_decimal_point",
            "numeric_separator",
            "opsys",
            "process_architecture",
            "program",
            "propath",
            "proversion",
            "window_system",
            "year_offset"
        ]
    );
    let text = |name: &str| fields[name].as_str().unwrap();
    for (name, value) in fields {
        assert!(!value.as_str().unwrap().is_empty(), "{name} is empty");
    }
    assert_eq!(text("batch_mode"), "yes");
    assert!(text("proversion").starts_with(&target.proversion));
    assert!(text("dlc").starts_with('/') && !text("dlc").ends_with('/'));
    assert!(text("dlc_version").starts_with("OpenEdge Release"));
    assert!(["UNIX", "WIN32"].contains(&text("opsys")));
    assert!(["32", "64"].contains(&text("process_architecture")));
    assert!(
        text("date_format").len() == 3 && text("date_format").chars().all(|c| "dmy".contains(c)),
        "{}",
        text("date_format")
    );
    assert!(text("year_offset").chars().all(|c| c.is_ascii_digit()));
    assert_eq!(text("numeric_decimal_point").chars().count(), 1);
    assert_eq!(text("numeric_separator").chars().count(), 1);
    assert!(text("propath").split(',').count() >= 1);
    assert!(text("cwd").starts_with('/'), "cwd is a path");

    let databases = environment["databases"].as_array().unwrap();
    assert_eq!(databases.len(), target.databases.len());
    for (at, database) in databases.iter().enumerate() {
        assert_eq!(database["name"], target.databases[at].as_str());
        assert_eq!(database["number"], at as u64 + 1);
        assert!(!database["type"].as_str().unwrap().is_empty());
        assert!(database["aliases"].is_array());
        assert!(is_hex64(&database["schema_identity"]), "{database}");
        assert!(database["entries"].as_u64().unwrap() >= 1, "{database}");
        assert!(database["records"].as_u64().unwrap() >= 1, "{database}");
        let fingerprint = &database["fingerprint"];
        assert!(!fingerprint["guid"].as_str().unwrap().is_empty());
        assert!(!fingerprint["stamp"].as_str().unwrap().is_empty());
    }

    let libraries = environment["libraries"].as_array().unwrap();
    assert!(
        libraries.iter().any(
            |library| library["entry"].as_str().unwrap().starts_with("$DLC")
                && is_hex64(&library["digest"])
        ),
        "no hashed library below $DLC: {libraries:?}"
    );
    for library in libraries {
        let digest = &library["digest"];
        assert!(
            is_hex64(digest) || digest == "dir" || digest == "none",
            "{library}"
        );
    }
    for parameter in environment["parameters"].as_array().unwrap() {
        assert!(parameter["name"].as_str().unwrap().starts_with('-'));
        assert!(parameter["value"].is_string());
    }
    assert!(is_hex64(&environment["identity"]));
}

#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn probe_a_real_avm() {
    let target = setup();

    let (report, _guard) = probe(&target, 1);
    let (again, _again_guard) = probe(&target, 1);

    let environment = &report["environment"];
    assert!(is_hex64(&environment["identity"]));
    assert_eq!(
        environment["identity"], again["environment"]["identity"],
        "the identity is stable"
    );
    let fields = &environment["fields"];
    assert!(
        fields["proversion"]
            .as_str()
            .unwrap()
            .starts_with(&target.proversion)
    );
    for name in [
        "dlc",
        "dlc_version",
        "opsys",
        "window_system",
        "cpinternal",
        "cpstream",
        "date_format",
        "year_offset",
        "cwd",
    ] {
        assert!(
            !fields[name].as_str().unwrap_or_default().is_empty(),
            "field {name} is empty: {fields}"
        );
    }
    assert_eq!(fields["batch_mode"], "yes");
    let reported: Vec<&str> = environment["databases"]
        .as_array()
        .unwrap()
        .iter()
        .map(|database| database["name"].as_str().unwrap())
        .collect();
    assert_eq!(
        reported,
        target
            .databases
            .iter()
            .map(String::as_str)
            .collect::<Vec<_>>()
    );
    for database in environment["databases"].as_array().unwrap() {
        assert!(is_hex64(&database["schema_identity"]), "{database}");
        assert!(database["records"].as_u64().unwrap() >= 1, "{database}");
    }
    assert_eq!(environment["unknown"].as_array().unwrap().len(), 0);
}

/// The schema image is split across the workers, so the identity must be the
/// same for every worker count. Prints the wall time of each probe.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn probe_time_by_worker_count() {
    let target = setup();
    let counts: Vec<usize> = match list("OXABL_OPENEDGE_WORKERS") {
        given if !given.is_empty() => given.iter().map(|n| n.parse().unwrap()).collect(),
        _ => vec![1, 4, 8],
    };

    let mut identities = Vec::new();
    for workers in counts {
        let started = std::time::Instant::now();
        let (report, _guard) = probe(&target, workers);
        eprintln!(
            "TIMING workers={workers} wall={:.1}s",
            started.elapsed().as_secs_f64()
        );
        identities.push((workers, report["environment"]["identity"].clone()));
    }

    assert!(
        identities.windows(2).all(|pair| pair[0].1 == pair[1].1),
        "the identity depends on the worker count: {identities:?}"
    );
}

#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn two_workers_agree_on_one_environment() {
    let target = setup();

    let (single, _single_guard) = probe(&target, 1);
    let (pair, _pair_guard) = probe(&target, 2);

    assert_eq!(pair["workers"]["started"], 2);
    assert_eq!(
        single["environment"]["identity"],
        pair["environment"]["identity"]
    );
}
