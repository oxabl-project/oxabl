//! The call fill through the real `oxabl lsp`: an item of a procedure, a
//! function or a program writes the call with one snippet placeholder for
//! each parameter, as signature help reads the callee
//! (`docs/design/call-fill.md`).
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

#[path = "lsp/completion.rs"]
mod completion;

use completion::{Case, FUNCTION, Files, List, Ws, capabilities, new_text};
use serde_json::{Value, json};
use session::file_uri;

/// `InsertTextFormat.Snippet`.
const SNIPPET_FORMAT: u64 = 2;
const HINTS: &str = "editor.action.triggerParameterHints";
const FILL: &str = "oxabl.fillCall";
const FILE_KIND: u64 = 17;

const DO_WORK: &str = "PROCEDURE DoWork:\n    \
    DEFINE INPUT PARAMETER piCount AS INTEGER NO-UNDO.\n    \
    DEFINE OUTPUT PARAMETER pcName AS CHARACTER NO-UNDO.\n    \
    DEFINE INPUT-OUTPUT PARAMETER plOk AS LOGICAL NO-UNDO.\n\
    END PROCEDURE.\n";

const FUNCTIONS: &str = "FUNCTION Twice RETURNS INTEGER (INPUT piValue AS INTEGER, OUTPUT pcNote AS CHARACTER):\n    \
    RETURN piValue * 2.\n\
    END FUNCTION.\n\
    FUNCTION Stamp RETURNS CHARACTER ():\n    \
    RETURN \"now\".\n\
    END FUNCTION.\n\
    DEFINE VARIABLE iTotal AS INTEGER NO-UNDO.\n";

/// The program `prog.p`, with two parameters, and `blind.p`, whose
/// parameters start with an include.
fn programs(files: Files) -> Files {
    files
        .file(
            "prog.p",
            "DEFINE INPUT PARAMETER pcIn AS CHARACTER NO-UNDO.\n\
             DEFINE OUTPUT PARAMETER piOut AS INTEGER NO-UNDO.\n\
             piOut = LENGTH(pcIn).\n",
        )
        .file(
            "blind.p",
            "{params.i}\nDEFINE INPUT PARAMETER pcIn AS CHARACTER NO-UNDO.\n",
        )
        .file(
            "params.i",
            "DEFINE INPUT PARAMETER piFirst AS INTEGER NO-UNDO.\n",
        )
}

/// The item `label` of `kind`, with its inserted text, its format and its
/// command.
#[track_caller]
fn fill(list: &List, kind: u64, label: &str) -> (String, Option<u64>, Option<String>) {
    let item = list.item(kind, label);
    (
        new_text(item).to_string(),
        item["insertTextFormat"].as_u64(),
        item["command"]["command"].as_str().map(str::to_string),
    )
}

#[track_caller]
fn assert_fill(list: &List, kind: u64, label: &str, text: &str, command: Option<&str>) {
    let (got, format, got_command) = fill(list, kind, label);
    assert_eq!(got, text, "the text of `{label}` in {}", list.summary());
    assert_eq!(format, Some(SNIPPET_FORMAT), "`{label}` is a snippet");
    assert_eq!(got_command.as_deref(), command, "the command of `{label}`");
}

/// The item writes its name only: no snippet, no command.
#[track_caller]
fn assert_name_only(list: &List, kind: u64, label: &str) {
    let (got, format, command) = fill(list, kind, label);
    assert_eq!(
        got,
        label,
        "`{label}` writes its name only in {}",
        list.summary()
    );
    assert_ne!(format, Some(SNIPPET_FORMAT), "`{label}` is no snippet");
    assert_eq!(command, None, "`{label}` has no command");
}

fn options(call_arguments: &str) -> Value {
    json!({"completion": {"callArguments": call_arguments, "fillCommand": true}})
}

#[test]
fn run_of_an_internal_procedure_writes_each_parameter_with_its_mode() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("main.p", &format!("{DO_WORK}RUN Do|"));
    assert_fill(
        answer.list(),
        FUNCTION,
        "DoWork",
        "DoWork (${1:piCount}, OUTPUT ${2:pcName}, INPUT-OUTPUT ${3:plOk}).$0",
        Some(HINTS),
    );
}

#[test]
fn the_modes_follow_the_keyword_case_of_the_project() {
    let mut ws = Files::config_n(Case::Lower).start();
    let answer = ws.complete("main.p", &format!("{DO_WORK}run Do|"));
    assert_fill(
        answer.list(),
        FUNCTION,
        "DoWork",
        "DoWork (${1:piCount}, output ${2:pcName}, input-output ${3:plOk}).$0",
        Some(HINTS),
    );
}

#[test]
fn a_table_a_buffer_and_a_dataset_get_their_keyword() {
    let mut ws = Files::config_n(Case::Upper).start();
    let text = "DEFINE TEMP-TABLE ttOrder NO-UNDO FIELD iNum AS INTEGER.\n\
                DEFINE DATASET dsOrder FOR ttOrder.\n\
                PROCEDURE Ship:\n    \
                DEFINE OUTPUT PARAMETER TABLE FOR ttOrder.\n    \
                DEFINE PARAMETER BUFFER bOrder FOR ttOrder.\n    \
                DEFINE INPUT PARAMETER DATASET FOR dsOrder.\n    \
                DEFINE INPUT PARAMETER phTable AS HANDLE NO-UNDO.\n\
                END PROCEDURE.\n\
                RUN Sh|";
    let answer = ws.complete("main.p", text);
    assert_fill(
        answer.list(),
        FUNCTION,
        "Ship",
        "Ship (OUTPUT TABLE ${1:ttOrder}, BUFFER ${2:bOrder}, DATASET ${3:dsOrder}, ${4:phTable}).$0",
        Some(HINTS),
    );
    // The keywords follow the keyword case of the project too.
    let mut ws = Files::config_n(Case::Lower).start();
    let answer = ws.complete("main.p", text);
    assert_fill(
        answer.list(),
        FUNCTION,
        "Ship",
        "Ship (output table ${1:ttOrder}, buffer ${2:bOrder}, dataset ${3:dsOrder}, ${4:phTable}).$0",
        Some(HINTS),
    );
}

#[test]
fn a_run_in_the_middle_of_a_line_gets_no_period() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete(
        "main.p",
        &format!("{DO_WORK}IF TRUE THEN RUN Do| NO-ERROR.\n"),
    );
    assert_fill(
        answer.list(),
        FUNCTION,
        "DoWork",
        "DoWork (${1:piCount}, OUTPUT ${2:pcName}, INPUT-OUTPUT ${3:plOk})$0",
        Some(HINTS),
    );
}

#[test]
fn a_call_that_has_its_parentheses_gets_no_fill() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("main.p", &format!("{DO_WORK}RUN Do| (1, OUTPUT c, l).\n"));
    assert_name_only(answer.list(), FUNCTION, "DoWork");
}

#[test]
fn a_user_function_writes_its_arguments_in_parentheses() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("main.p", &format!("{FUNCTIONS}iTotal = Tw|"));
    assert_fill(
        answer.list(),
        FUNCTION,
        "Twice",
        "Twice(${1:piValue}, OUTPUT ${2:pcNote})$0",
        Some(HINTS),
    );
    let answer = ws.complete("other.p", &format!("{FUNCTIONS}iTotal = St|"));
    assert_fill(answer.list(), FUNCTION, "Stamp", "Stamp()$0", None);
}

#[test]
fn a_built_in_function_fills_the_arguments_its_syntax_needs() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("main.p", &format!("{FUNCTIONS}iTotal = ENT|"));
    // The delimiter is in brackets in the syntax: it is left out.
    assert_fill(
        answer.list(),
        FUNCTION,
        "ENTRY",
        "ENTRY(${1:element}, ${2:list})$0",
        Some(HINTS),
    );
}

#[test]
fn a_callee_with_several_declarations_gets_its_parentheses_and_signature_help() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("main.p", &format!("{FUNCTIONS}iTotal = DAT|"));
    // `DATE` has two syntaxes: the arguments pick one.
    assert_fill(answer.list(), FUNCTION, "DATE", "DATE($0)", Some(HINTS));
}

#[test]
fn a_built_in_function_that_signature_help_does_not_know_gets_no_fill() {
    let mut ws = Files::config_n(Case::Upper).start();
    // `TODAY` is not in the table of syntaxes.
    let answer = ws.complete("main.p", &format!("{FUNCTIONS}iTotal = TOD|"));
    assert_name_only(answer.list(), FUNCTION, "TODAY");
}

#[test]
fn the_setting_gives_the_parentheses_or_the_name_only() {
    let mut ws =
        Files::config_n(Case::Upper).start_with_options(capabilities(), options("parentheses"));
    let answer = ws.complete("main.p", &format!("{DO_WORK}RUN Do|"));
    assert_fill(
        answer.list(),
        FUNCTION,
        "DoWork",
        "DoWork ($0).",
        Some(HINTS),
    );
    let answer = ws.complete("other.p", &format!("{FUNCTIONS}iTotal = Tw|"));
    assert_fill(answer.list(), FUNCTION, "Twice", "Twice($0)", Some(HINTS));

    ws.session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeConfiguration",
        "params": {"settings": {"oxabl": {"completion": {"callArguments": "none"}}}}
    }));
    let answer = ws.complete("main.p", &format!("{DO_WORK}RUN Do|"));
    assert_name_only(answer.list(), FUNCTION, "DoWork");

    ws.session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeConfiguration",
        "params": {"settings": {"oxabl": {"completion": {"callArguments": "fill"}}}}
    }));
    let answer = ws.complete("main.p", &format!("{DO_WORK}RUN Do|"));
    assert_fill(
        answer.list(),
        FUNCTION,
        "DoWork",
        "DoWork (${1:piCount}, OUTPUT ${2:pcName}, INPUT-OUTPUT ${3:plOk}).$0",
        Some(HINTS),
    );
}

#[test]
fn a_client_without_snippets_gets_the_name_only() {
    // The client has the command, so only the missing snippet support stops
    // the fill and the command.
    let mut ws = Files::config_n(Case::Upper).start_with_options(
        completion::capabilities_with_snippets(false),
        options("fill"),
    );
    let answer = ws.complete("main.p", &format!("{DO_WORK}RUN Do|"));
    assert_name_only(answer.list(), FUNCTION, "DoWork");
}

/// Insert `name` at `cursor` of `rel` as the client does when it accepts an
/// item, as version 2, and ask for the fill at the end of the name.
#[track_caller]
fn ask_fill(ws: &mut Ws, rel: &str, cursor: completion::Cursor, name: &str, version: i32) -> Value {
    ws.change(
        rel,
        2,
        (cursor.line, cursor.character),
        (cursor.line, cursor.character),
        name,
    );
    let response = ws.session.request(
        "oxabl/callArguments",
        json!({
            "textDocument": {"uri": file_uri(&ws.path(rel)), "version": version},
            "position": {"line": cursor.line, "character": cursor.character + name.len() as u32}
        }),
    );
    assert!(response.get("error").is_none(), "{response}");
    response["result"].clone()
}

#[test]
fn a_program_is_filled_after_the_insert_through_the_command() {
    let mut ws =
        programs(Files::config_a(Case::Upper)).start_with_options(capabilities(), options("fill"));
    let cursor = ws.open_marked("main.p", "RUN |");
    let answer = ws.complete_at("main.p", cursor, None);
    let list = answer.list();
    // The base cannot give the parameters of a program: the item asks later.
    let (text, _, command) = fill(list, FILE_KIND, "prog.p");
    assert_eq!(text, "prog.p");
    assert_eq!(command.as_deref(), Some(FILL));
    let result = ask_fill(&mut ws, "main.p", cursor, "prog.p", 2);
    assert_eq!(
        result,
        json!({"snippet": " (${1:pcIn}, OUTPUT ${2:piOut}).$0", "parameterHints": true}),
    );
}

#[test]
fn a_program_with_a_parameter_include_gets_no_fill_yet() {
    let mut ws =
        programs(Files::config_a(Case::Upper)).start_with_options(capabilities(), options("fill"));
    let cursor = ws.open_marked("main.p", "RUN |");
    let result = ask_fill(&mut ws, "main.p", cursor, "blind.p", 2);
    assert_eq!(result, Value::Null);
}

#[test]
fn a_fill_asked_for_an_older_text_answers_nothing() {
    let mut ws =
        programs(Files::config_a(Case::Upper)).start_with_options(capabilities(), options("fill"));
    let cursor = ws.open_marked("main.p", "RUN |");
    // The request names version 1; the server holds version 2.
    let result = ask_fill(&mut ws, "main.p", cursor, "prog.p", 1);
    assert_eq!(result, Value::Null);
}

#[test]
fn a_client_without_the_command_gets_no_command() {
    let mut ws = programs(Files::config_a(Case::Upper)).start();
    let answer = ws.complete("main.p", "RUN |");
    let (text, _, command) = fill(answer.list(), FILE_KIND, "prog.p");
    assert_eq!(text, "prog.p");
    assert_eq!(command, None);
}

#[test]
fn an_edit_of_the_declaration_never_gives_the_old_parameters() {
    let mut ws = Files::config_n(Case::Upper).start_with_options(capabilities(), options("fill"));
    let one = "PROCEDURE Grow:\n    DEFINE INPUT PARAMETER piOld AS INTEGER NO-UNDO.\nEND PROCEDURE.\nRUN Gr|";
    let answer = ws.complete("main.p", one);
    assert_fill(
        answer.list(),
        FUNCTION,
        "Grow",
        "Grow (${1:piOld}).$0",
        Some(HINTS),
    );
    // Rename the parameter: the base the next request reads may be the old one.
    ws.change("main.p", 2, (1, 27), (1, 32), "piNew");
    let (_, cursor) = completion::split_cursor(one);
    let answer = ws.complete_at("main.p", cursor, None);
    let (got, _, command) = fill(answer.list(), FUNCTION, "Grow");
    assert!(
        got == "Grow (${1:piNew}).$0" || (got == "Grow" && command.as_deref() == Some(FILL)),
        "the old parameters must not be written: {got} {command:?}"
    );
}
