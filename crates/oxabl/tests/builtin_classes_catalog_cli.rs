//! Built-in classes through `oxabl check`, where the catalog scraped from the
//! documentation is not sure: a scalar return or property that may be an
//! `EXTENT`, a `MEMPTR` that the documentation links to the class
//! `OpenEdge.Core.Memptr`, a `SUPER:` receiver, and a project on an OpenEdge
//! version newer than the catalog.

#![cfg(unix)]

use std::process::Command;

use serde_json::Value;

const STRICT: &str = "[workspace]\nname = \"builtins\"\n\
    [workspace.sources]\ninclude_paths = [\".\"]\n\
    [workspace.lint]\nenable = [\"strict.calls\"]\n";

/// `(trimmed line, message)` of each finding of the `CL006` and `CL007` rules
/// for `source` checked with `config`, in source order.
fn findings(config: &str, files: &[(&str, &str)], source: &str) -> Vec<(String, String)> {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("oxabl.toml"), config).unwrap();
    for (name, text) in files {
        let path = dir.path().join(name);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, text).unwrap();
    }
    std::fs::write(dir.path().join("main.p"), source).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .args(["check", "--json", "--no-format", "main.p"])
        .current_dir(dir.path())
        .output()
        .unwrap();
    let json: Value = serde_json::from_slice(&out.stdout)
        .unwrap_or_else(|e| panic!("not JSON ({e}): {}", String::from_utf8_lossy(&out.stderr)));
    assert_eq!(json["failures"], serde_json::json!([]), "{json}");
    let mut found: Vec<(u64, String, String)> = json["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|d| d["code"] == "CL006" || d["code"] == "CL007" || d["code"] == "LINT0001")
        .map(|d| {
            let line = d["start"]["line"].as_u64().unwrap();
            let text = source.lines().nth(line as usize - 1).unwrap().trim();
            (
                line,
                text.to_string(),
                d["message"].as_str().unwrap().to_string(),
            )
        })
        .collect();
    found.sort();
    found.into_iter().map(|(_, l, m)| (l, m)).collect()
}

fn row(line: &str, message: &str) -> (String, String) {
    (line.to_string(), message.to_string())
}

#[test]
fn a_scalar_that_a_built_in_member_returns_may_be_an_extent() {
    let source = "DEFINE VARIABLE oJson AS Progress.Json.ObjectModel.JsonObject NO-UNDO.\n\
        DEFINE VARIABLE oHdr AS OpenEdge.Net.HTTP.HttpHeader NO-UNDO.\n\
        DEFINE VARIABLE lc AS LONGCHAR NO-UNDO.\n\
        lc = OpenEdge.Core.String:Join(oJson:GetNames(), \",\").\n\
        lc = OpenEdge.Core.String:Join(oHdr:GetParameterValues(), \",\").\n\
        lc = OpenEdge.Core.String:Join(\"a,b\", \",\").\n\
        MESSAGE lc.\n";
    // `GetNames` returns an EXTENT (an override fixes the row). The catalog
    // gives `GetParameterValues` a scalar return, which may have lost its
    // EXTENT: no finding. A literal is a scalar for sure.
    assert_eq!(
        findings(STRICT, &[], source),
        [row(
            "lc = OpenEdge.Core.String:Join(\"a,b\", \",\").",
            "no overload of `OpenEdge.Core.String:Join` takes arguments of these types: `CHARACTER`, `CHARACTER`"
        )]
    );
}

#[test]
fn a_super_receiver_is_not_judged_by_the_member_rules() {
    let class = "CLASS app.MyErr INHERITS Progress.Lang.AppError:\n\
        METHOD PUBLIC VOID Go():\n\
        SUPER:NoSuchThing().\n\
        MESSAGE SUPER:GetMessage(1, 2).\n\
        MESSAGE SUPER:GetMessage(1).\n\
        END METHOD.\n\
        END CLASS.\n";
    let source = "DEFINE VARIABLE o AS app.MyErr NO-UNDO.\nMESSAGE o.\n";
    // The rules judge a receiver whose type is a built-in type; `SUPER` in a
    // user class is that class. Its own file has no finding either.
    assert_eq!(findings(STRICT, &[("app/MyErr.cls", class)], source), []);
    assert_eq!(findings(STRICT, &[("app/Other.cls", class)], class), []);
}

const VERSIONED: &str = "DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.\n\
    MESSAGE oErr:GetMesage(1).\n\
    MESSAGE oErr:GetMessage(1, 2).\n\
    MESSAGE oErr:GetMessage(\"1\").\n";

fn with_version(version: &str) -> String {
    format!("{STRICT}[workspace.preprocessor]\nproversion = \"{version}\"\n")
}

#[test]
fn a_newer_openedge_version_silences_the_absence_findings() {
    let types = row(
        "MESSAGE oErr:GetMessage(\"1\").",
        "no overload of `Progress.Lang.AppError:GetMessage` takes arguments of these types: `CHARACTER`",
    );
    let every = vec![
        row(
            "MESSAGE oErr:GetMesage(1).",
            "built-in type `Progress.Lang.AppError` has no member `GetMesage`",
        ),
        row(
            "MESSAGE oErr:GetMessage(1, 2).",
            "no overload of `Progress.Lang.AppError:GetMessage` takes 2 arguments",
        ),
        types.clone(),
    ];
    // The catalog's version, an older one, and none: every finding.
    for config in [
        with_version("12.8"),
        with_version("12.2.4"),
        STRICT.to_string(),
    ] {
        assert_eq!(findings(&config, &[], VERSIONED), every, "{config}");
    }
    // A newer version may have the member or the overload: only a type that
    // never converts is still reported.
    for version in ["12.9", "13.0"] {
        assert_eq!(
            findings(&with_version(version), &[], VERSIONED),
            std::slice::from_ref(&types),
            "{version}"
        );
    }
}

/// `(trimmed line, code, message)` of each `LINT0004` and `CL006` finding.
fn typed(source: &str) -> Vec<(String, String, String)> {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("oxabl.toml"), STRICT).unwrap();
    std::fs::write(dir.path().join("main.p"), source).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .args(["check", "--json", "--no-format", "main.p"])
        .current_dir(dir.path())
        .output()
        .unwrap();
    let json: Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(json["failures"], serde_json::json!([]), "{json}");
    let mut found: Vec<(u64, String, String, String)> = json["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|d| d["code"] == "LINT0004" || d["code"] == "CL006")
        .map(|d| {
            let line = d["start"]["line"].as_u64().unwrap();
            let text = source.lines().nth(line as usize - 1).unwrap().trim();
            (
                line,
                text.to_string(),
                d["code"].as_str().unwrap().to_string(),
                d["message"].as_str().unwrap().to_string(),
            )
        })
        .collect();
    found.sort();
    found.into_iter().map(|(_, l, c, m)| (l, c, m)).collect()
}

fn typed_row(line: &str, code: &str, message: &str) -> (String, String, String) {
    (line.to_string(), code.to_string(), message.to_string())
}

#[test]
fn a_built_in_property_may_hold_an_extent() {
    let source = "DEFINE VARIABLE oH AS Ccs.Common.Support.ICharacterArrayHolder NO-UNDO.\n\
        DEFINE VARIABLE oD AS OpenEdge.Core.DateArrayHolder NO-UNDO.\n\
        DEFINE VARIABLE oOH AS OpenEdge.Core.ObjectArrayHolder NO-UNDO.\n\
        DEFINE VARIABLE oS AS OpenEdge.Net.ServerConnection.ClientSocketConnectionParameters NO-UNDO.\n\
        DEFINE VARIABLE oB AS OpenEdge.Net.HTTP.Trace.Rule.BodyRule NO-UNDO.\n\
        DEFINE VARIABLE oKV AS OpenEdge.Core.KeyValuePair NO-UNDO.\n\
        DEFINE VARIABLE cx AS CHARACTER EXTENT 2 NO-UNDO.\n\
        DEFINE VARIABLE dx AS DATE EXTENT 2 NO-UNDO.\n\
        DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
        oH:Value = cx.\n\
        oD:Value = dx.\n\
        oS:SslProtocols = cx.\n\
        oB:MimeTypes = cx.\n\
        oB:MimeTypes = dx.\n\
        c = oH:Value.\n\
        MESSAGE NEW OpenEdge.Core.ObjectArrayHolder(oOH:Value).\n\
        MESSAGE NEW OpenEdge.Core.Collections.Array(oOH:Value).\n\
        MESSAGE NEW OpenEdge.Core.Collections.Array(oKV:Value).\n\
        MESSAGE NEW OpenEdge.Core.Collections.Array(oKV).\n\
        MESSAGE oH oD oOH oS oB oKV cx[1] dx[1] c.\n";
    // The holders and `SslProtocols` are EXTENT properties (overrides).
    // `BodyRule:MimeTypes` and `KeyValuePair:Value` have none: an EXTENT of a
    // type that converts to the documented scalar may be correct, so it gives
    // no finding, and neither does a class scalar of a built-in property for an
    // `Object[]` parameter. An EXTENT of DATE into CHARACTER, a holder value
    // into a scalar and a plain object into `Object[]` do not convert.
    assert_eq!(
        typed(source),
        [
            typed_row(
                "oB:MimeTypes = dx.",
                "LINT0004",
                "type mismatch: cannot assign `DATE EXTENT 2` to `CHARACTER`"
            ),
            typed_row(
                "c = oH:Value.",
                "LINT0004",
                "type mismatch: cannot assign `CHARACTER EXTENT` to `CHARACTER`"
            ),
            typed_row(
                "MESSAGE NEW OpenEdge.Core.Collections.Array(oKV).",
                "CL006",
                "no constructor of `OpenEdge.Core.Collections.Array` takes arguments of these types: `OpenEdge.Core.KeyValuePair`"
            ),
        ]
    );
}

#[test]
fn a_memptr_is_not_taken_for_the_class_memptr() {
    let source = "DEFINE VARIABLE mp AS MEMPTR NO-UNDO.\n\
        DEFINE VARIABLE oMp AS OpenEdge.Core.Memptr NO-UNDO.\n\
        DEFINE VARIABLE oBB AS OpenEdge.Core.ByteBucket NO-UNDO.\n\
        DEFINE VARIABLE oResp AS OpenEdge.Net.HTTP.IHttpResponse NO-UNDO.\n\
        DEFINE VARIABLE oReq AS OpenEdge.Web.IWebRequest NO-UNDO.\n\
        DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
        mp = CAST(oResp:Entity, OpenEdge.Core.Memptr):Value.\n\
        mp = oMp:Value.\n\
        mp = oBB:GetBytes():Value.\n\
        mp = oReq:GetRawEntity():Value.\n\
        oBB:PutBytes(mp).\n\
        oBB:PutBytes(oMp).\n\
        oBB:ReadBytes(1, mp).\n\
        MESSAGE NEW OpenEdge.Core.Memptr(mp).\n\
        i = oMp:Value.\n\
        oBB:PutBytes(mp, 1, 2).\n\
        MESSAGE mp oMp oBB oResp oReq i.\n";
    // `Value` is MEMPTR (the `IMemptrHolder` contract); the `PutBytes`,
    // `ReadBytes` and constructor parameters follow the short signatures of
    // the page. A return the page links to the class page is not known
    // (`GetBytes`, `GetRawEntity`), so nothing is said about it.
    assert_eq!(
        typed(source),
        [
            typed_row(
                "i = oMp:Value.",
                "LINT0004",
                "type mismatch: cannot assign `MEMPTR` to `INTEGER`"
            ),
            typed_row(
                "oBB:PutBytes(mp, 1, 2).",
                "CL006",
                "no overload of `OpenEdge.Core.ByteBucket:PutBytes` takes 3 arguments"
            ),
        ]
    );
}
