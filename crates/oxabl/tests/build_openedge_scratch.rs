//! Schema identity and the schema stamp on real scratch databases. Opt-in:
//! every test is `#[ignore]`, and a test fails when its variables are missing.
//!
//! Each test creates databases from the empty database of the installation
//! (`prodb`), loads `.df` files into them and removes them again. It never
//! touches another database. The worker runs single-user (`-1`) on the scratch
//! database, so a probe uses one worker.
//!
//! | Variable | Meaning |
//! | --- | --- |
//! | `OXABL_OPENEDGE_EXEC` | The command that runs a program where OpenEdge is, as a JSON array that ends with the container, with `{root}` for the working directory, for example `["docker","exec","-i","-w","{root}","abl"]`. |
//! | `OXABL_OPENEDGE_SCRATCH` | A directory there, where the tests make their databases. |
//! | `OXABL_OPENEDGE_DLC` | The installation there. Default `/psc/dlc`. |
//! | `OXABL_OPENEDGE_WORKSPACE`, `OXABL_OPENEDGE_WORKER_ROOT`, `OXABL_OPENEDGE_PROVERSION` | As in `build_openedge.rs`. |
//!
//! Run: `cargo test -p oxabl --test build_openedge_scratch -- --ignored --nocapture`.

mod common;

use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::Duration;

use common::openedge::compile::{Options, compile_workspace_with};
use common::openedge::{Target, probe};
use serde_json::Value;

struct Scratch {
    exec: Vec<String>,
    dir: String,
    dlc: String,
    workspace: PathBuf,
    worker_root: String,
    proversion: String,
    made: Vec<String>,
}

impl Drop for Scratch {
    fn drop(&mut self) {
        for db in &self.made {
            let _ = self.run(&format!("rm -rf {}", self.db_dir(db)));
        }
    }
}

fn var(name: &str) -> String {
    std::env::var(name).unwrap_or_else(|_| panic!("{name} must be set to run this test"))
}

impl Scratch {
    fn new() -> Scratch {
        let exec: Vec<String> = serde_json::from_str(&var("OXABL_OPENEDGE_EXEC"))
            .expect("OXABL_OPENEDGE_EXEC is a JSON array");
        let workspace = PathBuf::from(var("OXABL_OPENEDGE_WORKSPACE"));
        let worker_root = std::env::var("OXABL_OPENEDGE_WORKER_ROOT")
            .unwrap_or_else(|_| workspace.to_string_lossy().into_owned());
        Scratch {
            exec,
            dir: var("OXABL_OPENEDGE_SCRATCH")
                .trim_end_matches('/')
                .to_string(),
            dlc: std::env::var("OXABL_OPENEDGE_DLC").unwrap_or_else(|_| "/psc/dlc".into()),
            workspace,
            worker_root,
            proversion: std::env::var("OXABL_OPENEDGE_PROVERSION")
                .unwrap_or_else(|_| "12.8".into()),
            made: Vec::new(),
        }
    }

    fn db_dir(&self, db: &str) -> String {
        format!("{}/{db}", self.dir)
    }

    /// Runs a shell script where OpenEdge is; returns its output.
    fn run(&self, script: &str) -> String {
        let argv: Vec<String> = self
            .exec
            .iter()
            .map(|arg| arg.replace("{root}", &self.dir))
            .collect();
        let mut child = Command::new(&argv[0])
            .args(&argv[1..])
            .args(["bash", "-s"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("the exec command starts");
        let script = format!(
            "set -e\nexport DLC={}\ncd {}\n{script}\n",
            self.dlc, self.dir
        );
        child
            .stdin
            .take()
            .unwrap()
            .write_all(script.as_bytes())
            .unwrap();
        let output = child.wait_with_output().unwrap();
        let text = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(output.status.success(), "script failed:\n{script}\n{text}");
        text
    }

    /// A new database from the empty one, named `db`.
    fn create(&mut self, db: &str) {
        let dir = self.db_dir(db);
        self.run(&format!(
            "rm -rf {dir}; mkdir -p {dir}; cd {dir}\n{dlc}/bin/prodb {db} {dlc}/empty8 > /dev/null\n\
             cat > load.p <<'ABL'\nRUN prodict/load_df.p (INPUT SESSION:PARAMETER).\nQUIT.\nABL",
            dlc = self.dlc
        ));
        self.made.push(db.to_string());
    }

    /// Loads a `.df` body, with the trailer that the dictionary expects.
    fn load(&self, db: &str, body: &str) {
        let dir = self.db_dir(db);
        let trailer = format!(".\nPSC\ncpstream=ISO8859-1\n.\n{:010}\n", body.len());
        let output = self.run(&format!(
            "cd {dir}\ncat > edit.df <<'DF'\n{body}{trailer}DF\n\
             {dlc}/bin/_progres -b -q -db {db} -1 -s 100000 -stsh 100000 -p {dir}/load.p -param {dir}/edit.df 2>&1 || true",
            dlc = self.dlc
        ));
        assert!(
            !output.to_ascii_lowercase().contains("error") && !output.contains("**"),
            "load failed:\n{output}"
        );
    }

    /// Runs ABL statements single-user on the database, in one transaction.
    fn abl(&self, db: &str, statements: &str) {
        let dir = self.db_dir(db);
        let output = self.run(&format!(
            "cd {dir}\ncat > change.p <<'ABL'\nDO TRANSACTION ON ERROR UNDO, LEAVE:\n{statements}\nEND.\nQUIT.\nABL\n\
             {dlc}/bin/_progres -b -q -db {db} -1 -p {dir}/change.p 2>&1 || true",
            dlc = self.dlc
        ));
        assert!(!output.contains("**"), "the change failed:\n{output}");
    }

    /// A single-user worker on the database, connected as `ldb`.
    fn target(&self, db: &str, ldb: &str) -> Target {
        let dir = self.db_dir(db);
        let mut command: Vec<String> = self.exec.clone();
        command.extend(
            [
                &format!("{}/bin/_progres", self.dlc),
                "-b",
                "-q",
                "-db",
                &format!("{dir}/{db}"),
                "-ld",
                ldb,
                "-1",
                "-p",
                "{program}",
            ]
            .map(String::from),
        );
        Target {
            command,
            workspace: self.workspace.clone(),
            worker_root: self.worker_root.clone(),
            proversion: self.proversion.clone(),
            databases: vec![ldb.to_string()],
        }
    }

    /// Probes the database with one single-user worker.
    fn probe(&self, db: &str) -> Value {
        probe(&self.target(db, db), 1).0
    }
}

fn schema_identity(report: &Value) -> String {
    report["environment"]["databases"][0]["schema_identity"]
        .as_str()
        .unwrap()
        .to_string()
}

fn stamp(report: &Value) -> String {
    report["environment"]["databases"][0]["fingerprint"]["stamp"]
        .as_str()
        .unwrap()
        .to_string()
}

const BASE: &str = r#"ADD TABLE "t1"
  AREA "Schema Area"
  DUMP-NAME "t1"

ADD FIELD "f1" OF "t1" AS character
  FORMAT "x(8)"
  INITIAL ""
  POSITION 2
  MAX-WIDTH 16
  ORDER 10

ADD FIELD "f2" OF "t1" AS integer
  FORMAT "->,>>>,>>9"
  INITIAL "0"
  POSITION 3
  MAX-WIDTH 4
  ORDER 20

ADD INDEX "i1" ON "t1"
  AREA "Schema Area"
  PRIMARY
  INDEX-FIELD "f1" ASCENDING

ADD TABLE "t2"
  AREA "Schema Area"
  DUMP-NAME "t2"

ADD FIELD "g1" OF "t2" AS character
  FORMAT "x(8)"
  INITIAL ""
  POSITION 2
  MAX-WIDTH 16
  ORDER 10

"#;

#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn databases_loaded_from_one_df_give_one_schema_identity() {
    let mut scratch = Scratch::new();
    let suffix = std::process::id();
    let (first, second) = (format!("oxsa{suffix}"), format!("oxsb{suffix}"));
    scratch.create(&first);
    scratch.load(&first, BASE);
    std::thread::sleep(Duration::from_millis(2100));
    scratch.create(&second);
    scratch.load(&second, BASE);

    let a = scratch.probe(&first);
    let b = scratch.probe(&second);

    assert_ne!(
        stamp(&a),
        stamp(&b),
        "the databases were loaded at different times"
    );
    assert_eq!(schema_identity(&a), schema_identity(&b));
}

/// Each edit: a name, the `.df` body and whether the stamp must move for it.
const EDITS: &[(&str, &str, bool)] = &[
    (
        "label",
        "UPDATE FIELD \"f1\" OF \"t1\"\n  LABEL \"First field\"\n\n",
        true,
    ),
    (
        "format",
        "UPDATE FIELD \"f1\" OF \"t1\"\n  FORMAT \"x(12)\"\n\n",
        true,
    ),
    (
        "add index",
        "ADD INDEX \"i2\" ON \"t1\"\n  AREA \"Schema Area\"\n  INDEX-FIELD \"f2\" ASCENDING\n\n",
        true,
    ),
    (
        "primary index",
        "UPDATE PRIMARY INDEX \"i2\" ON \"t1\"\n\n",
        false,
    ),
    (
        "validation",
        "UPDATE FIELD \"f2\" OF \"t1\"\n  VALEXP \"f2 > 0\"\n  VALMSG \"positive\"\n\n",
        false,
    ),
    (
        "help",
        "UPDATE FIELD \"g1\" OF \"t2\"\n  HELP \"some help\"\n\n",
        false,
    ),
    (
        "add field",
        "ADD FIELD \"g2\" OF \"t2\" AS integer\n  FORMAT \"->,>>>,>>9\"\n  INITIAL \"0\"\n  POSITION 3\n  MAX-WIDTH 4\n  ORDER 20\n\n",
        false,
    ),
    (
        "add table",
        "ADD TABLE \"t3\"\n  AREA \"Schema Area\"\n  DUMP-NAME \"t3\"\n\nADD FIELD \"h1\" OF \"t3\" AS character\n  FORMAT \"x(8)\"\n  INITIAL \"\"\n  POSITION 2\n  MAX-WIDTH 16\n  ORDER 10\n\n",
        false,
    ),
    (
        "add sequence",
        "ADD SEQUENCE \"s1\"\n  INITIAL 0\n  INCREMENT 1\n  CYCLE-ON-LIMIT no\n\n",
        false,
    ),
    ("drop index", "DROP INDEX \"i2\" ON \"t1\"\n\n", false),
    ("drop field", "DROP FIELD \"g2\" OF \"t2\"\n\n", false),
];

/// The schema identity changes for every edit. The stamp has to move for the
/// edits that `EDITS` marks; the rest are recorded. An edit that changes the
/// identity and leaves the stamp alone is why the stamp is never trusted to
/// skip imaging.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn schema_edit_table() {
    let mut scratch = Scratch::new();
    let db = format!("oxst{}", std::process::id());
    scratch.create(&db);
    scratch.load(&db, BASE);
    let mut before = scratch.probe(&db);
    let mut untrusted = Vec::new();
    let mut table = String::new();

    for (name, body, must_move) in EDITS {
        std::thread::sleep(Duration::from_millis(1300));
        scratch.load(&db, body);
        let after = scratch.probe(&db);
        let identity_moved = schema_identity(&before) != schema_identity(&after);
        let stamp_moved = stamp(&before) != stamp(&after);
        table.push_str(&format!(
            "{name:14} identity moved: {identity_moved:5}  stamp moved: {stamp_moved}\n"
        ));
        assert!(
            identity_moved,
            "the schema identity did not change for `{name}`"
        );
        assert!(
            !must_move || stamp_moved,
            "the stamp did not move for `{name}`"
        );
        if !stamp_moved {
            untrusted.push(*name);
        }
        before = after;
    }

    eprintln!("{table}");
    eprintln!("edits the stamp does not see: {untrusted:?}");
}

/// A field description of 30,600 bytes, close to the longest value that a
/// schema record holds (the dictionary refuses a record over 32,000 bytes), hashes without error, and the schema identity follows it.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn schema_long_field() {
    let mut scratch = Scratch::new();
    let db = format!("oxlf{}", std::process::id());
    scratch.create(&db);
    scratch.load(&db, BASE);
    let before = scratch.probe(&db);
    let description = "long description ".repeat(1_800);
    assert_eq!(description.len(), 30_600);

    scratch.load(
        &db,
        &format!("UPDATE FIELD \"f1\" OF \"t1\"\n  DESCRIPTION \"{description}\"\n\n"),
    );
    let after = scratch.probe(&db);
    let again = scratch.probe(&db);

    assert_ne!(schema_identity(&before), schema_identity(&after));
    assert_eq!(schema_identity(&after), schema_identity(&again));
}

/// R-code records the CRC of a metaschema table that a unit reads. An edit of
/// such a table leaves every user table as it was, and must still change the
/// schema identity and the key of a unit that reads the table.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn a_system_table_edit_changes_the_key_of_a_unit_that_reads_it() {
    let mut scratch = Scratch::new();
    let suffix = std::process::id();
    let (plain, edited) = (format!("oxya{suffix}"), format!("oxyb{suffix}"));
    for db in [&plain, &edited] {
        scratch.create(db);
        scratch.load(db, BASE);
    }
    scratch.abl(
        &edited,
        "FIND _File WHERE _File._File-Name = \"_User\".\n_File._Frozen = FALSE.\n\
         FIND _Field OF _File WHERE _Field._Field-Name = \"_User-Name\".\n\
         _Field._Order = _Field._Order + 5000.\n_File._Frozen = TRUE.",
    );
    let keyed = |db: &str| {
        let target = scratch.target(db, "oxmeta");
        let identity = schema_identity(&probe(&target, 1).0);
        let workspace = compile_workspace_with(
            &target,
            Options {
                workers: 1,
                ..Options::default()
            },
        );
        workspace.write(
            "src/users.p",
            "FOR EACH _User NO-LOCK:\n    DISPLAY _User._Userid _User._User-Name.\nEND.\n",
        );
        let plan = workspace.plan();
        let unit = plan["units"]
            .as_array()
            .unwrap()
            .iter()
            .find(|unit| unit["path"] == "src/users.p")
            .unwrap_or_else(|| panic!("no unit src/users.p in {plan}"));
        let key = unit["key"]
            .as_str()
            .unwrap_or_else(|| panic!("src/users.p has no key: {unit}"))
            .to_string();
        (identity, key)
    };

    let (plain_identity, plain_key) = keyed(&plain);
    let (edited_identity, edited_key) = keyed(&edited);

    assert_ne!(plain_identity, edited_identity);
    assert_ne!(plain_key, edited_key);
}
