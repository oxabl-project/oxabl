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

/// The files in the second column of a text `XREF`, in order, as the build
/// reads them.
fn text_sources(text: &str) -> Vec<String> {
    oxabl::build::observe::xref::parse_text(text)
        .unwrap_or_else(|e| panic!("the XREF is not read: {e}\n{text}"))
        .sources
}

/// What the cross-reference of a unit lists for a class that comes from
/// source, from r-code in the output directory and from a procedure library:
/// the unit alone, so the read check sees no class file. The names printed are
/// the evidence. `outputs` is the `[build.outputs]` line that asks for the
/// form, `read` reads the file of a unit.
fn classes_listed(outputs: &str, extension: &str, read: fn(&str) -> Vec<String>) {
    let target = setup();
    let workspace = compile_workspace_with(
        &target,
        Options {
            sources: "directories = [\"src\"]\ninclude_paths = [\"src\", \"lib\", \"$LIB:acme\"]"
                .into(),
            outputs: outputs.into(),
            extra: "[build.libraries]\nacme = \"libs/acme.pl\"\n[build.external_packages]\n\"acme.\" = [\"$LIB:acme\"]"
                .into(),
            workers: 1,
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
        let listed = read(&workspace.read(&format!("build/xref/{name}.p.{extension}")));
        eprintln!("EVIDENCE classes {extension} {name} uses {class}: {listed:?}");
        assert_eq!(
            listed,
            [format!("src/{name}.p")],
            "the {extension} lists a file for the class {class}: the read check sees classes now"
        );
    }
}

/// What the `XREF-XML` of a unit lists for a class: the unit alone.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn xref_xml_classes() {
    classes_listed("xref = \"build/xref\"", "xref.xml", sources);
}

/// The same for the text `XREF`: the second column names the unit alone.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn xref_text_classes() {
    classes_listed(
        "xref = \"build/xref\"\nxref_format = \"text\"",
        "xref",
        text_sources,
    );
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

/// The text `XREF` names, in its second column, the files that hold a
/// reference as the compiler resolved them, and none for an include with only
/// preprocessor definitions (that include has an `INCLUDE` row, with its name
/// as written). The read check accepts every unit of the fixture, with a
/// requested text `XREF` and with the private one.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn xref_text_resolution() {
    let target = setup();
    for outputs in ["xref = \"build/xref\"\nxref_format = \"text\"", ""] {
        let mut workspace = compile_workspace(&target);
        workspace.set_outputs(outputs);
        workspace.copy_fixture();
        let cache = tempfile::TempDir::new().unwrap();

        let output = workspace.build_with_cache(&["--json"], 0, "developer", Some(cache.path()));

        let report: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
        if !outputs.is_empty() {
            let text = workspace.read("build/xref/main.p.xref");
            eprintln!("EVIDENCE xref_text_resolution main.p:\n{text}");
            let listed = text_sources(&text);
            assert_eq!(listed, ["src/main.p", "src/defs.i", "src/ttrows.i"]);
            assert_eq!(listed, sources_of_xml(&target), "text and XML differ");
        }
        assert_eq!(
            report["cache"]["stored"],
            FIXTURE_UNITS.len(),
            "outputs {outputs:?}: {report}"
        );
        assert!(
            !report["notes"].to_string().contains("outside the closure"),
            "the compiler opened a file that the analysis did not see: {}",
            report["notes"]
        );
    }
}

/// `main.p` of the fixture compiled with `XREF-XML`: the files it lists.
fn sources_of_xml(target: &crate::common::openedge::Target) -> Vec<String> {
    let workspace = compile_workspace(target);
    workspace.copy_fixture();
    workspace.build(&["--json"], 0);
    sources(&workspace.read("build/xref/main.p.xref.xml"))
}

/// Names that the text format could break: a skipped include (the unit is then
/// keyed from the text `XREF`, as no include of it has a file), a space in a
/// unit name, and an include path of about 200 characters (the compiler
/// refuses a longer include name). The build is cached with the private text
/// `XREF`, so the read check read each file back.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn xref_text_unusual_names() {
    let target = setup();
    let mut workspace = compile_workspace(&target);
    workspace.set_outputs("xref = \"build/xref\"\nxref_format = \"text\"");
    let deep = format!("{0}/{0}/{0}", "d".repeat(65));
    workspace.write(
        &format!("src/{deep}/deep.i"),
        "DEFINE VARIABLE deepvar AS INTEGER NO-UNDO.\nASSIGN deepvar = 1.\n",
    );
    workspace.write(
        "src/skip.p",
        "&IF 1 = 2 &THEN\n{never.i}\n&ENDIF\nMESSAGE \"skip\".\n",
    );
    workspace.write(
        "src/two words.p",
        &format!("{{{deep}/deep.i}}\nMESSAGE \"two\" deepvar.\n"),
    );
    let cache = tempfile::TempDir::new().unwrap();

    let output = workspace.build_with_cache(&["--json"], 0, "developer", Some(cache.path()));

    let report: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    for unit in ["skip.p", "two words.p"] {
        let text = workspace.read(&format!("build/xref/{unit}.xref"));
        eprintln!("EVIDENCE xref_text_unusual_names {unit}:\n{text}");
        text_sources(&text);
    }
    assert_eq!(report["summary"]["failed"], 0, "{report}");
    assert_eq!(
        report["cache"]["stored"], 1,
        "the unit with a space in its name is stored: {report}"
    );
    let plan = workspace.plan();
    let skip = plan["units"]
        .as_array()
        .unwrap()
        .iter()
        .find(|unit| unit["path"] == "src/skip.p")
        .unwrap();
    assert_eq!(
        skip["closure"], "observed",
        "an include in a branch that is not taken has no file, so the unit is keyed from the text XREF: {plan}"
    );
    assert!(skip["key"].is_string(), "{plan}");
    let unit = text_sources(&workspace.read("build/xref/two words.p.xref"));
    assert!(
        unit.iter().any(|file| file.ends_with("/deep.i")),
        "{unit:?}"
    );
}

/// What the observed closure reads from an `XREF-XML`: one `INCLUDE`
/// reference for each include, the one with only preprocessor definitions
/// too. The observed closure is sound only while this holds.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn xref_xml_observed_closure() {
    let target = setup();
    let workspace = compile_workspace(&target);
    workspace.copy_fixture();
    workspace.build(&["--json"], 0);

    let main = workspace.read("build/xref/main.p.xref.xml");
    let main = oxabl::build::observe::xref::parse(&main)
        .unwrap_or_else(|e| panic!("the XREF-XML is not read: {e}\n{main}"));
    eprintln!("EVIDENCE xref_xml_observed_closure main.p: {main:?}");
    for include in ["defs.i", "ttrows.i", "consts.i"] {
        assert!(
            main.includes.iter().any(|name| name.ends_with(include)),
            "no INCLUDE reference names {include}: the observed closure would miss it"
        );
    }
}

/// What the observed closure reads from a text `XREF`: one `INCLUDE` row for
/// each include, the one with only preprocessor definitions too, with the
/// name as written.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn xref_text_observed_closure() {
    let target = setup();
    let mut workspace = compile_workspace(&target);
    workspace.set_outputs("xref = \"build/xref\"\nxref_format = \"text\"");
    workspace.copy_fixture();
    workspace.build(&["--json"], 0);

    let main = workspace.read("build/xref/main.p.xref");
    let main = oxabl::build::observe::xref::parse_text(&main)
        .unwrap_or_else(|e| panic!("the XREF is not read: {e}\n{main}"));
    eprintln!("EVIDENCE xref_text_observed_closure main.p: {main:?}");
    for include in ["defs.i", "ttrows.i", "consts.i"] {
        assert!(
            main.includes.iter().any(|name| name.ends_with(include)),
            "no INCLUDE row names {include}: the observed closure would miss it"
        );
    }
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
