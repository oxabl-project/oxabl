//! Built-in Progress classes through the real `oxabl lsp` and its daemon: the
//! diagnostics agree with `oxabl check`, the hover of a built-in type and of
//! its members shows the catalog, definition and references have no location
//! for a built-in name, the highlights and the semantic tokens treat a
//! built-in member like a workspace one, and every position of a program that
//! uses built-in classes gets an answer.

#![cfg(unix)]

#[path = "builtin_classes_ws/mod.rs"]
mod ws;

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;
use std::time::Duration;

use serde_json::{Value, json};
use session::{Session, file_uri};
use ws::{
    CLOSED, CONFIG, CONFIG_NO_SOURCES, CONFIG_STRICT, Finding, SURE, Workspace, calls_findings,
    calls_program, errors_lint0004, errors_program, lint0004_of, sure_lint0004, types_program,
    types_rows,
};

/// The byte of the `nth` (0-based) occurrence of `needle` in `text`.
#[track_caller]
fn nth(text: &str, needle: &str, nth: usize) -> usize {
    text.match_indices(needle)
        .nth(nth)
        .unwrap_or_else(|| panic!("no occurrence {nth} of `{needle}`"))
        .0
}

/// The byte one character into `word`, as it stands in the first `context` of `text`.
#[track_caller]
fn in_context(text: &str, context: &str, word: &str) -> usize {
    nth(text, context, 0) + context.rfind(word).unwrap() + 1
}

/// The UTF-16 `(line, character)` of byte `at` in `text`.
fn utf16_at(text: &str, at: usize) -> (u64, u64) {
    let line_start = text[..at].rfind('\n').map_or(0, |n| n + 1);
    let line = text[..at].matches('\n').count();
    let character = text[line_start..at].encode_utf16().count();
    (line as u64, character as u64)
}

fn params_at(file: &Path, text: &str, at: usize) -> Value {
    let (line, character) = utf16_at(text, at);
    json!({
        "textDocument": {"uri": file_uri(file)},
        "position": {"line": line, "character": character}
    })
}

/// The `result` of `method` with `params`; fails on an error.
#[track_caller]
fn ask(session: &mut Session, method: &str, params: Value) -> Value {
    let answer = session.request(method, params);
    assert!(
        answer.get("error").is_none(),
        "`{method}` gave an error: {}",
        answer["error"]
    );
    answer["result"].clone()
}

/// The text of a single-line `range` of `text` (UTF-16 columns).
fn slice(text: &str, range: &Value) -> String {
    let line = range["start"]["line"].as_u64().unwrap() as usize;
    assert_eq!(range["end"]["line"].as_u64().unwrap() as usize, line);
    let units: Vec<u16> = text.split('\n').nth(line).unwrap().encode_utf16().collect();
    let from = range["start"]["character"].as_u64().unwrap() as usize;
    let to = range["end"]["character"].as_u64().unwrap() as usize;
    String::from_utf16(&units[from..to]).unwrap()
}

/// The findings of `code` among the diagnostics of a publish: the trimmed text
/// of the line, the message and the text of the range, in source order.
fn findings(diagnostics: &[Value], text: &str, code: &str) -> Vec<Finding> {
    let mut found: Vec<(u64, u64, Finding)> = diagnostics
        .iter()
        .filter(|d| d["code"] == code)
        .map(|d| {
            let line = d["range"]["start"]["line"].as_u64().unwrap();
            let column = d["range"]["start"]["character"].as_u64().unwrap();
            (
                line,
                column,
                (
                    text.split('\n')
                        .nth(line as usize)
                        .unwrap()
                        .trim()
                        .to_string(),
                    d["message"].as_str().unwrap().to_string(),
                    slice(text, &d["range"]),
                ),
            )
        })
        .collect();
    found.sort_by_key(|(line, column, _)| (*line, *column));
    found.into_iter().map(|(.., finding)| finding).collect()
}

fn show(findings: &[Finding]) -> String {
    if findings.is_empty() {
        return "  (none)".to_string();
    }
    findings
        .iter()
        .map(|(line, message, span)| format!("  {line}  |  {message}  |  range `{span}`"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// The diagnostics of the last publish for `path` that arrives while the
/// server is quiet for `quiet` after the first one.
#[track_caller]
fn settled_diagnostics(session: &Session, path: &Path, quiet: Duration) -> Vec<Value> {
    let uri = json!(file_uri(path));
    let is_publish = |m: &Value| {
        m["method"] == json!("textDocument/publishDiagnostics") && m["params"]["uri"] == uri
    };
    let mut last = session.wait_for(|m| is_publish(m));
    while let Some(next) = session.try_next(quiet) {
        if is_publish(&next) {
            last = next;
        }
    }
    last["params"]["diagnostics"].as_array().unwrap().clone()
}

/// Write `text` as `name`, open it and compare the findings of each code of
/// its settled diagnostics with the list wanted for that code.
#[track_caller]
fn assert_published(
    session: &mut Session,
    ws: &Workspace,
    name: &str,
    text: &str,
    wanted: Vec<(&str, Vec<Finding>)>,
) {
    ws.write(name, text);
    let file = ws.path(name);
    session.open(&file, text);
    let published = settled_diagnostics(session, &file, Duration::from_secs(2));
    for (code, expected) in wanted {
        let actual = findings(&published, text, code);
        assert!(
            actual == expected,
            "{code} diagnostics of `{name}` differ.\nexpected:\n{}\nactual:\n{}",
            show(&expected),
            show(&actual)
        );
    }
}

// ---------------------------------------------------------------------------
// L1 and L2. The diagnostics match the CLI
// ---------------------------------------------------------------------------

/// One program in a session on `config`; its diagnostics equal `wanted`.
#[track_caller]
fn published_with(config: &str, name: &str, text: &str, wanted: Vec<(&str, Vec<Finding>)>) {
    let ws = Workspace::new(Some(config));
    let mut session = Session::start(&ws.root);
    assert_published(&mut session, &ws, name, text, wanted);
}

#[test]
fn the_types_of_built_in_members_are_published() {
    published_with(
        CONFIG,
        "types.p",
        &types_program(),
        vec![("LINT0004", lint0004_of(&types_rows(), true))],
    );
}

#[test]
fn the_types_through_a_class_that_inherits_a_built_in_class_are_published() {
    published_with(
        CONFIG,
        "errors.p",
        &errors_program(),
        vec![("LINT0004", errors_lint0004())],
    );
}

#[test]
fn a_program_with_no_known_built_in_facts_publishes_no_type_finding() {
    published_with(CONFIG, "closed.p", CLOSED, vec![("LINT0004", vec![])]);
}

#[test]
fn the_member_rules_are_published() {
    let (cl006, cl007) = calls_findings(true);
    published_with(
        CONFIG_STRICT,
        "calls.p",
        &calls_program(true),
        vec![("CL006", cl006), ("CL007", cl007)],
    );
}

#[test]
fn diagnostics_with_no_search_path() {
    published_with(
        CONFIG_NO_SOURCES,
        "sure.p",
        SURE,
        vec![("LINT0004", sure_lint0004())],
    );
}

// ---------------------------------------------------------------------------
// L3 to L5. Hover, definition and references
// ---------------------------------------------------------------------------

const HOVER: &str = r#"DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.
DEFINE VARIABLE oStr AS OpenEdge.Core.String NO-UNDO.
DEFINE VARIABLE oJson AS Progress.Json.ObjectModel.JsonObject NO-UNDO.
DEFINE VARIABLE oThing AS app.Thing NO-UNDO.
DEFINE VARIABLE oNamed AS app.Named NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE lc AS LONGCHAR NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
c = oErr:GetMessage(1).
i = oErr:NumMessages.
lc = oJson:GetLongchar("a").
MESSAGE oJson:Add("k", "v").
MESSAGE OpenEdge.Core.String:IsQuoted(lc).
i = Progress.Json.ObjectModel.JsonDataType:NUMBER.
MESSAGE Progress.ApplicationServer.AdapterTypes:SOAP.
c = oThing:ToString().
c = oNamed:ToString().
DO ON ERROR UNDO, LEAVE:
    i = 1.
    CATCH e AS Progress.Lang.Error:
        c = e:GetMessage(1).
    END CATCH.
END.
MESSAGE oErr oStr oJson oThing oNamed c lc i.
"#;

fn code(text: &str) -> String {
    format!("```abl\n{text}\n```")
}

const DOCS: &str = "https://docs-be.progress.com/bundle/abl-reference/page";
const API_DOCS: &str = "https://docs-be.progress.com/bundle/openedge-abl-api-reference-128/page";

fn open_hover_program() -> (Workspace, Session) {
    let ws = Workspace::new(Some(CONFIG));
    ws.write("hover.p", HOVER);
    let mut session = Session::start(&ws.root);
    session.open(&ws.path("hover.p"), HOVER);
    (ws, session)
}

/// The markdown of the hover with the cursor one character into `word` as it
/// stands in the first `context`; fails when there is none.
#[track_caller]
fn hover_markdown(session: &mut Session, ws: &Workspace, context: &str, word: &str) -> String {
    let file = ws.path("hover.p");
    let at = in_context(HOVER, context, word);
    let result = ask(session, "textDocument/hover", params_at(&file, HOVER, at));
    assert!(!result.is_null(), "no hover on `{word}` in `{context}`");
    assert_eq!(
        result["contents"]["kind"],
        json!("markdown"),
        "hover on `{word}` in `{context}`: {result}"
    );
    result["contents"]["value"]
        .as_str()
        .unwrap_or_else(|| panic!("hover on `{word}` has no text: {result}"))
        .to_string()
}

#[track_caller]
fn assert_hover(session: &mut Session, ws: &Workspace, context: &str, word: &str, want: String) {
    let got = hover_markdown(session, ws, context, word);
    assert_eq!(got, want, "hover on `{word}` in `{context}`");
    assert!(
        !got.contains("Database effects"),
        "the hover on `{word}` names database effects:\n{got}"
    );
}

#[test]
fn hover_of_a_built_in_type() {
    let (ws, mut session) = open_hover_program();
    assert_hover(
        &mut session,
        &ws,
        "oErr AS Progress.Lang.AppError",
        "Progress.Lang.AppError",
        code("CLASS Progress.Lang.AppError INHERITS Progress.Lang.ProError")
            + &format!(
                "\n\nBuilt-in class of OpenEdge 12.8. [Progress documentation]({DOCS}/Progress.Lang.AppError-class.html)"
            ),
    );
    assert_hover(
        &mut session,
        &ws,
        "oStr AS OpenEdge.Core.String",
        "OpenEdge.Core.String",
        code(
            "CLASS OpenEdge.Core.String INHERITS Progress.Lang.Object IMPLEMENTS Ccs.Common.Support.ILongcharHolder, OpenEdge.Core.ISupportEncoding, Progress.Collections.IHashable",
        ) + &format!(
            "\n\nBuilt-in class of OpenEdge 12.8. [Progress documentation]({API_DOCS}/OpenEdge.Core.String.html)"
        ),
    );
    assert_hover(
        &mut session,
        &ws,
        "CATCH e AS Progress.Lang.Error",
        "Progress.Lang.Error",
        code("INTERFACE Progress.Lang.Error")
            + &format!(
                "\n\nBuilt-in interface of OpenEdge 12.8. [Progress documentation]({DOCS}/Progress.Lang.Error-interface.html)"
            ),
    );
}

#[test]
fn hover_of_a_built_in_member() {
    let (ws, mut session) = open_hover_program();
    let session = &mut session;
    let ws = &ws;

    assert_hover(
        session,
        ws,
        "c = oErr:GetMessage(1)",
        "GetMessage",
        code("METHOD PUBLIC CHARACTER GetMessage (MessageIndex AS INTEGER)")
            + &format!(
                "\n\nDeclared in built-in class `Progress.Lang.ProError` (OpenEdge 12.8). Parameter modes are not known. [Progress documentation]({DOCS}/Progress.Lang.ProError-class.html)"
            ),
    );
    assert_hover(
        session,
        ws,
        "i = oErr:NumMessages",
        "NumMessages",
        code("DEFINE PUBLIC PROPERTY NumMessages AS INTEGER")
            + &format!(
                "\n\nDeclared in built-in class `Progress.Lang.ProError` (OpenEdge 12.8). [Progress documentation]({DOCS}/Progress.Lang.ProError-class.html)"
            ),
    );
    assert_hover(
        session,
        ws,
        "lc = oJson:GetLongchar",
        "GetLongchar",
        code("METHOD PUBLIC LONGCHAR GetLongchar (property-name AS CHARACTER)")
            + "\n\n"
            + &code(
                "METHOD PUBLIC LONGCHAR GetLongchar (property-name AS CHARACTER, code-page AS CHARACTER)",
            )
            + &format!(
                "\n\nDeclared in built-in class `Progress.Json.ObjectModel.JsonObject` (OpenEdge 12.8). Parameter modes are not known. [Progress documentation]({DOCS}/Progress.Json.ObjectModel.JsonObject-class.html)"
            ),
    );
    assert_hover(
        session,
        ws,
        "OpenEdge.Core.String:IsQuoted",
        "IsQuoted",
        code("METHOD PUBLIC STATIC LOGICAL IsQuoted (pcValue AS LONGCHAR)")
            + "\n\n"
            + &code(
                "METHOD PUBLIC STATIC LOGICAL IsQuoted (pcValue AS LONGCHAR, pcQuote AS CHARACTER)",
            )
            + &format!(
                "\n\nDeclared in built-in class `OpenEdge.Core.String` (OpenEdge 12.8). Parameter modes are not known. [Progress documentation]({API_DOCS}/OpenEdge.Core.String.html)"
            ),
    );
    assert_hover(
        session,
        ws,
        "JsonDataType:NUMBER",
        "NUMBER",
        code("DEFINE PUBLIC STATIC PROPERTY Number AS INTEGER")
            + &format!(
                "\n\nDeclared in built-in class `Progress.Json.ObjectModel.JsonDataType` (OpenEdge 12.8). [Progress documentation]({DOCS}/Progress.Json.ObjectModel.JsonDataType-class.html)"
            ),
    );
    assert_hover(
        session,
        ws,
        "AdapterTypes:SOAP",
        "SOAP",
        code("Progress.ApplicationServer.AdapterTypes:SOAP")
            + &format!(
                "\n\nMember of built-in enum `Progress.ApplicationServer.AdapterTypes` (OpenEdge 12.8). [Progress documentation]({DOCS}/Progress.ApplicationServer.AdapterTypes-enumeration.html)"
            ),
    );
    assert_hover(
        session,
        ws,
        "c = oThing:ToString()",
        "ToString",
        code("METHOD PUBLIC CHARACTER ToString ()")
            + &format!(
                "\n\nDeclared in built-in class `Progress.Lang.Object` (OpenEdge 12.8). [Progress documentation]({DOCS}/Progress.Lang.Object-class.html)"
            ),
    );
    assert_hover(
        session,
        ws,
        "c = e:GetMessage(1)",
        "GetMessage",
        code("METHOD PUBLIC CHARACTER GetMessage (MessageIndex AS INTEGER)")
            + &format!(
                "\n\nDeclared in built-in interface `Progress.Lang.Error` (OpenEdge 12.8). Parameter modes are not known. [Progress documentation]({DOCS}/Progress.Lang.Error-interface.html)"
            ),
    );
}

#[test]
fn hover_of_an_overload_set_shows_ten_signatures_and_the_count_of_the_rest() {
    let (ws, mut session) = open_hover_program();
    let got = hover_markdown(&mut session, &ws, "MESSAGE oJson:Add(", "Add");
    let first = code("METHOD PUBLIC LOGICAL Add (property-name AS CHARACTER, value AS CHARACTER)");
    let tail = format!(
        "\n\n7 more overloads.\n\nDeclared in built-in class `Progress.Json.ObjectModel.JsonObject` (OpenEdge 12.8). Parameter modes are not known. [Progress documentation]({DOCS}/Progress.Json.ObjectModel.JsonObject-class.html)"
    );
    assert_eq!(got.matches("```abl").count(), 10, "{got}");
    assert!(got.starts_with(&first), "{got}");
    assert!(got.ends_with(&tail), "{got}");
    assert!(
        got[..got.len() - tail.len()].ends_with("\n```"),
        "the 10th code block must end the signatures:\n{got}"
    );
    assert!(!got.contains("Database effects"), "{got}");
}

#[test]
fn hover_of_a_workspace_override_names_the_workspace_class() {
    let (ws, mut session) = open_hover_program();
    let got = hover_markdown(&mut session, &ws, "c = oNamed:ToString()", "ToString");
    let first = code("METHOD PUBLIC OVERRIDE CHARACTER ToString ()");
    assert!(got.starts_with(&first), "{got}");
    assert!(got.contains("Declared in `app.Named`."), "{got}");
    assert!(!got.contains("Progress.Lang.Object"), "{got}");
}

#[test]
fn definition_and_references_of_built_in_names_have_no_location() {
    let (ws, mut session) = open_hover_program();
    let file = ws.path("hover.p");
    let spots = [
        ("oErr AS Progress.Lang.AppError", "Progress.Lang.AppError"),
        ("c = oErr:GetMessage(1)", "GetMessage"),
        ("i = oErr:NumMessages", "NumMessages"),
        ("AdapterTypes:SOAP", "SOAP"),
    ];
    for (context, word) in spots {
        let at = in_context(HOVER, context, word);
        let result = ask(
            &mut session,
            "textDocument/definition",
            params_at(&file, HOVER, at),
        );
        assert!(
            result.is_null() || result == json!([]),
            "definition of `{word}` gave {result}"
        );
    }
    for (context, word) in [spots[0], spots[1]] {
        for include in [true, false] {
            let at = in_context(HOVER, context, word);
            let mut params = params_at(&file, HOVER, at);
            params["context"] = json!({"includeDeclaration": include});
            let result = ask(&mut session, "textDocument/references", params);
            assert!(
                result.is_null() || result == json!([]),
                "references of `{word}` (includeDeclaration {include}) gave {result}"
            );
        }
    }
}

// ---------------------------------------------------------------------------
// L6. Highlights
// ---------------------------------------------------------------------------

const HL: &str = "DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.\n\
    DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
    c = oErr:GetMessage(1).\n\
    c = oErr:GetMessage(2).\n\
    oErr:ReturnValue = \"x\".\n\
    c = oErr:ReturnValue.\n\
    MESSAGE c.\n";

/// What the highlights of the name under the cursor show: line (0-based),
/// text and the kind (2 Read, 3 Write).
#[track_caller]
fn highlights(session: &mut Session, file: &Path, at: usize) -> Vec<(u64, String, u64)> {
    let result = ask(
        session,
        "textDocument/documentHighlight",
        params_at(file, HL, at),
    );
    let mut found: Vec<_> = result
        .as_array()
        .unwrap_or_else(|| panic!("no highlights: {result}"))
        .iter()
        .map(|h| {
            (
                h["range"]["start"]["line"].as_u64().unwrap(),
                slice(HL, &h["range"]),
                h["kind"].as_u64().unwrap(),
            )
        })
        .collect();
    found.sort();
    found
}

#[test]
fn highlights_of_a_built_in_member() {
    let ws = Workspace::new(Some(CONFIG));
    ws.write("hl.p", HL);
    let file = ws.path("hl.p");
    let mut session = Session::start(&ws.root);
    session.open(&file, HL);

    assert_eq!(
        highlights(&mut session, &file, nth(HL, ":GetMessage", 0) + 2),
        vec![
            (2, "GetMessage".to_string(), 2),
            (3, "GetMessage".to_string(), 2)
        ]
    );
    assert_eq!(
        highlights(&mut session, &file, nth(HL, ":ReturnValue", 0) + 2),
        vec![
            (4, "ReturnValue".to_string(), 3),
            (5, "ReturnValue".to_string(), 2)
        ]
    );
}

// ---------------------------------------------------------------------------
// L7. Semantic tokens
// ---------------------------------------------------------------------------

const TOKENS: &str = "DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.\n\
    DEFINE VARIABLE oOE AS app.OrderError NO-UNDO.\n\
    DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
    DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
    c = oErr:GetMessage(1).\n\
    c = oOE:Describe().\n\
    i = oErr:NumMessages.\n\
    i = oOE:OrderNum.\n\
    MESSAGE Progress.ApplicationServer.AdapterTypes:SOAP.\n\
    DO ON ERROR UNDO, LEAVE:\n\
    \x20   i = 1.\n\
    \x20   CATCH e AS Progress.Lang.Error:\n\
    \x20       c = e:GetMessage(1).\n\
    \x20   END CATCH.\n\
    END.\n\
    MESSAGE oErr oOE c i.\n";

fn legend(session: &Session, key: &str) -> Vec<String> {
    session.initialized["result"]["capabilities"]["semanticTokensProvider"]["legend"][key]
        .as_array()
        .unwrap_or_else(|| {
            panic!(
                "the server has no semantic token legend: {}",
                session.initialized
            )
        })
        .iter()
        .map(|name| name.as_str().unwrap().to_string())
        .collect()
}

/// The `(line, text, type)` of each token that the relative `data` holds.
fn decode(session: &Session, text: &str, data: &Value) -> Vec<(u64, String, String)> {
    let types = legend(session, "tokenTypes");
    let numbers: Vec<u64> = data
        .as_array()
        .expect("the data is an array")
        .iter()
        .map(|n| n.as_u64().expect("the data holds integers"))
        .collect();
    assert_eq!(numbers.len() % 5, 0, "five integers for each token");
    let lines: Vec<&str> = text.split('\n').collect();
    let (mut line, mut column) = (0, 0);
    numbers
        .chunks(5)
        .map(|token| {
            line += token[0];
            column = if token[0] == 0 {
                column + token[1]
            } else {
                token[1]
            };
            let units: Vec<u16> = lines[line as usize].encode_utf16().collect();
            let word =
                String::from_utf16(&units[column as usize..(column + token[2]) as usize]).unwrap();
            (line, word, types[token[3] as usize].clone())
        })
        .collect()
}

#[test]
fn semantic_tokens_of_built_in_names() {
    let ws = Workspace::new(Some(CONFIG));
    ws.write("tokens.p", TOKENS);
    let file = ws.path("tokens.p");
    let capabilities = json!({
        "textDocument": {"semanticTokens": {
            "requests": {"range": true, "full": {"delta": true}},
            "tokenTypes": [], "tokenModifiers": [], "formats": ["relative"]
        }},
        "workspace": {"semanticTokens": {"refreshSupport": true}}
    });
    let mut session = Session::start_with(&ws.root, capabilities);
    session.open(&file, TOKENS);
    let result = ask(
        &mut session,
        "textDocument/semanticTokens/full",
        json!({"textDocument": {"uri": file_uri(&file)}}),
    );
    let tokens = decode(&session, TOKENS, &result["data"]);
    let kind = |line: u64, text: &str| -> String {
        tokens
            .iter()
            .find(|(l, t, _)| *l == line && t == text)
            .unwrap_or_else(|| panic!("no token `{text}` on line {line}; tokens: {tokens:?}"))
            .2
            .clone()
    };

    assert_eq!(kind(1, "app.OrderError"), "class");
    assert_eq!(kind(0, "Progress.Lang.AppError"), "class");
    assert_eq!(kind(11, "Progress.Lang.Error"), "interface");
    assert_eq!(kind(5, "Describe"), "method");
    assert_eq!(kind(4, "GetMessage"), "method");
    assert_eq!(kind(7, "OrderNum"), "property");
    assert_eq!(kind(6, "NumMessages"), "property");
    assert_eq!(kind(8, "SOAP"), "property");
}

// ---------------------------------------------------------------------------
// L8. Every position answers
// ---------------------------------------------------------------------------

/// Each `(line, UTF-16 character)` of `text` with a character there, plus the
/// end of each line.
fn positions(text: &str) -> Vec<(usize, usize)> {
    let mut all = Vec::new();
    for (line, content) in text.split('\n').enumerate() {
        let width = content.encode_utf16().count();
        all.extend((0..=width).map(|character| (line, character)));
    }
    all
}

#[track_caller]
fn sweep(session: &mut Session, file: &Path, text: &str) {
    let params: Vec<Value> = positions(text)
        .into_iter()
        .map(|(line, character)| {
            json!({
                "textDocument": {"uri": file_uri(file)},
                "position": {"line": line, "character": character}
            })
        })
        .collect();
    // The server answers one hover and one highlight at a time and cancels
    // an older request.
    for method in ["textDocument/hover", "textDocument/documentHighlight"] {
        for params in &params {
            let answer = session.request(method, params.clone());
            assert!(
                answer.get("error").is_none(),
                "`{method}` at {} gave an error: {}",
                params["position"],
                answer["error"]
            );
        }
    }
    for chunk in params.chunks(200) {
        let answers = session.request_all("textDocument/definition", chunk.to_vec());
        for (answer, params) in answers.iter().zip(chunk) {
            assert!(
                answer.get("error").is_none(),
                "`textDocument/definition` at {} gave an error: {}",
                params["position"],
                answer["error"]
            );
        }
    }
}

#[test]
fn every_position_of_a_built_in_program_answers() {
    let ws = Workspace::new(Some(CONFIG_STRICT));
    let mut session = Session::start(&ws.root);
    for (name, text) in [
        ("types.p", types_program()),
        ("calls.p", calls_program(true)),
    ] {
        ws.write(name, &text);
        let file = ws.path(name);
        session.open(&file, &text);
        sweep(&mut session, &file, &text);
    }
}
