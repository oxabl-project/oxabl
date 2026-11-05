//! What the compiler's cross-reference output holds, read by the readers of
//! `oxabl_effects`.

use oxabl_effects::{xref, xref_xml};

use crate::common::openedge::compile::{Options, compile_workspace, compile_workspace_with};
use crate::common::openedge::worker::library_program;
use crate::compile::FIXTURE_UNITS;
use crate::setup;

/// The files that an `XREF-XML` document lists, in order.
fn sources(xml: &str) -> Vec<String> {
    xref_xml::source_files(xml).unwrap_or_else(|e| panic!("the XREF-XML is not read: {e}\n{xml}"))
}

/// What the `XREF-XML` of a unit lists for a class that comes from source,
/// from r-code in the output directory and from a procedure library: the unit
/// alone, so the read check sees no class file. The names printed are the
/// evidence.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn xref_xml_classes() {
    let target = setup();
    let workspace = compile_workspace_with(
        &target,
        Options {
            sources: "directories = [\"src\"]\ninclude_paths = [\"src\", \"lib\", \"$LIB:acme\"]"
                .into(),
            extra: "[build.libraries]\nacme = \"libs/acme.pl\"\n[build.external_packages]\n\"acme.\" = [\"$LIB:acme\"]"
                .into(),
            workers: 1,
            ..Options::default()
        },
    );
    workspace.write(
        "libsrc/acme/Lib.cls",
        "CLASS acme.Lib:\n    METHOD PUBLIC CHARACTER Name():\n        RETURN \"from a library\".\n    END METHOD.\nEND CLASS.\n",
    );
    let printed = workspace.run_program(
        &target,
        &library_program(&workspace.worker, &[("acme/Lib.cls", "acme/Lib.r")], true),
    );
    assert!(printed.contains("LIBRARY DONE"), "{printed}");
    workspace.write(
        "src/own/Own.cls",
        "CLASS own.Own:\n    METHOD PUBLIC CHARACTER Name():\n        RETURN \"from the output directory\".\n    END METHOD.\nEND CLASS.\n",
    );
    workspace.write(
        "lib/ext/Ext.cls",
        "CLASS ext.Ext:\n    METHOD PUBLIC CHARACTER Name():\n        RETURN \"from source\".\n    END METHOD.\nEND CLASS.\n",
    );
    let users = [
        ("u_out", "own.Own"),
        ("u_src", "ext.Ext"),
        ("u_lib", "acme.Lib"),
    ];
    for (name, class) in users {
        workspace.write(
            &format!("src/{name}.p"),
            &format!(
                "DEFINE VARIABLE o AS {class} NO-UNDO.\no = NEW {class}().\nPUT UNFORMATTED o:Name() SKIP.\n"
            ),
        );
    }

    let report = workspace.build_report();

    assert_eq!(report["summary"]["failed"], 0, "{report}");
    for (name, class) in users {
        let xml = workspace.read(&format!("build/xref/{name}.p.xref.xml"));
        let listed = sources(&xml);
        eprintln!("EVIDENCE xref_xml_classes {name} uses {class}: {listed:?}");
        assert_eq!(
            listed,
            [format!("src/{name}.p")],
            "the XREF-XML lists a file for the class {class}: the read check sees classes now"
        );
    }
}

/// The `Source` elements of an `XREF-XML` name each include that holds a
/// reference as the compiler resolved it, and none for an include that holds
/// only preprocessor definitions. The read check accepts every unit of the
/// fixture.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn xref_xml_resolution() {
    let target = setup();
    let workspace = compile_workspace(&target);
    workspace.copy_fixture();
    let cache = tempfile::TempDir::new().unwrap();

    let output = workspace.build_with_cache(&["--json"], 0, "developer", Some(cache.path()));

    let report: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    let listed = sources(&workspace.read("build/xref/main.p.xref.xml"));
    eprintln!("EVIDENCE xref_xml_resolution main.p: {listed:?}");
    assert_eq!(listed, ["src/main.p", "src/defs.i", "src/ttrows.i"]);
    assert!(
        !listed.iter().any(|file| file.ends_with("consts.i")),
        "the include with only preprocessor definitions is listed now: the read check sees it"
    );
    assert_eq!(report["cache"]["stored"], FIXTURE_UNITS.len(), "{report}");
    assert!(
        !report["notes"].to_string().contains("outside the closure"),
        "the compiler opened a file that the analysis did not see: {}",
        report["notes"]
    );
}

/// The text reader finds every include of the fixture by the name written in
/// the source; the XML reader finds the includes that hold a reference.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn xref_readers() {
    let target = setup();
    let mut workspace = compile_workspace(&target);
    workspace.copy_fixture();
    workspace.build(&["--json"], 0);
    let xml = sources(&workspace.read("build/xref/main.p.xref.xml"));
    workspace.set_outputs("xref = \"build/xref\"\nxref_format = \"text\"");
    workspace.drop_rcode();
    workspace.build(&["--json"], 0);
    let text = workspace.read("build/xref/main.p.xref");
    eprintln!("EVIDENCE xref_readers text XREF of main.p:\n{text}");

    let rows = xref::parse(&text);

    for include in ["consts.i", "defs.i", "ttrows.i"] {
        assert!(
            rows.iter()
                .any(|row| row.kind == "INCLUDE" && row.details == [include]),
            "the text reader does not find {include}: {rows:?}"
        );
    }
    for include in ["defs.i", "ttrows.i"] {
        assert!(
            xml.iter()
                .any(|file| file.ends_with(&format!("src/{include}"))),
            "the XML reader does not find {include}: {xml:?}"
        );
    }
}
