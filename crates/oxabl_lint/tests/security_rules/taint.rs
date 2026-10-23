//! Stage 2: the taint mode of the sink rules.
//!
//! Each test runs one rule through the registry with
//! `[workspace.lint.security] mode = "taint"`, so a sink is reported only when
//! a value from an untrusted source reaches it without a sanitiser for that
//! sink.

use oxabl_common::{
    Diagnostic, FileId, LintSeverityMap, ParameterSources, SecurityMode, SecurityOptions,
    SecurityValidator,
};
use oxabl_lexer::tokenize;
use oxabl_lint::Registry;
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

pub fn taint() -> SecurityOptions {
    SecurityOptions {
        mode: SecurityMode::Taint,
        ..SecurityOptions::default()
    }
}

/// Run the rule `code` over `src` with the security group on and `options`.
pub fn run_with(code: &str, src: &str, options: SecurityOptions) -> Vec<Diagnostic> {
    let tokens = tokenize(src);
    let program = Parser::new(&tokens, src).parse_program();
    assert!(
        program.errors.is_empty(),
        "must parse cleanly: {:?}",
        program.errors
    );
    let schema = Schema::empty();
    let mut severities = LintSeverityMap::new();
    severities.base_mut().enable_group("security");
    severities.set_security(options);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, src, &schema).with_lint_severities(severities);
    let sem = analyze_file(&program.statements, &ctx);
    Registry::builtin().run_where(&program.statements, &sem, &ctx, |m| m.code == code)
}

fn check_taint(code: &str, src: &str) -> Vec<Diagnostic> {
    run_with(code, src, taint())
}

fn check_sink(code: &str, src: &str) -> Vec<Diagnostic> {
    run_with(code, src, SecurityOptions::default())
}

/// The text of each label, in order.
fn steps<'a>(src: &'a str, d: &Diagnostic) -> Vec<&'a str> {
    d.labels
        .iter()
        .map(|l| &src[l.span.span.start as usize..l.span.span.end as usize])
        .collect()
}

const PREPARE_FROM_PARAM: &str = "\
DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWhere AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
cWhere = \"FOR EACH Customer WHERE Customer.Name = '\" + pcName + \"'\".
hQ:QUERY-PREPARE(cWhere).
";

#[test]
fn an_input_parameter_reaches_a_query_through_an_assignment() {
    let diags = check_taint("ABLSEC001", PREPARE_FROM_PARAM);
    assert_eq!(diags.len(), 1, "{diags:?}");
    let d = &diags[0];
    assert!(d.message.contains("untrusted input"), "{}", d.message);
    assert!(d.message.contains("`pcName`"), "{}", d.message);
    assert!(d.message.contains("`cWhere`"), "{}", d.message);
    assert_eq!(
        steps(PREPARE_FROM_PARAM, d),
        [
            "DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.",
            "cWhere = \"FOR EACH Customer WHERE Customer.Name = '\" + pcName + \"'\"."
        ]
    );
    assert!(
        d.labels[0].message.contains("input parameter"),
        "{:?}",
        d.labels
    );
}

#[test]
fn the_sink_mode_keeps_the_stage_one_message() {
    let diags = check_sink("ABLSEC001", PREPARE_FROM_PARAM);
    assert_eq!(diags.len(), 1);
    assert!(
        diags[0].message.contains("non-literal"),
        "{}",
        diags[0].message
    );
    assert!(diags[0].labels.is_empty());
}

#[test]
fn a_variable_that_no_source_reaches_is_not_reported() {
    let src = "\
DEFINE VARIABLE cWhere AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
cWhere = \"FOR EACH Customer\".
hQ:QUERY-PREPARE(cWhere).
hQ:QUERY-PREPARE(cOther).
";
    assert_eq!(check_sink("ABLSEC001", src).len(), 2);
    assert_eq!(check_taint("ABLSEC001", src).len(), 0);
}

#[test]
fn a_constant_assignment_replaces_the_taint() {
    let src = "\
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
c = pc.
c = \"ls -l\".
OS-COMMAND SILENT VALUE(c).
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 0);
}

#[test]
fn quoter_clears_a_query_and_not_a_command() {
    let src = "\
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE Customer.Name = \" + QUOTER(pc)).
OS-COMMAND SILENT VALUE(\"echo \" + QUOTER(pc)).
";
    assert_eq!(check_taint("ABLSEC001", src).len(), 0);
    assert_eq!(check_taint("ABLSEC003", src).len(), 1);
}

#[test]
fn a_numeric_conversion_clears_injection_but_not_a_size() {
    let src = "\
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
i = INTEGER(pc).
hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE Customer.CustNum = \" + STRING(i)).
SET-SIZE(m) = i.
";
    assert_eq!(check_taint("ABLSEC001", src).len(), 0);
    assert_eq!(check_taint("ABLSEC016", src).len(), 1);
}

#[test]
fn a_lookup_guard_that_returns_clears_the_value_after_it() {
    let src = "\
DEFINE INPUT PARAMETER pcMode AS CHARACTER NO-UNDO.
IF LOOKUP(pcMode, \"daily,weekly\") = 0 THEN RETURN.
OS-COMMAND SILENT VALUE(\"backup-\" + pcMode + \".sh\").
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 0);
}

#[test]
fn a_guard_clears_only_its_own_branch() {
    let src = "\
DEFINE INPUT PARAMETER pcMode AS CHARACTER NO-UNDO.
IF LOOKUP(pcMode, \"daily,weekly\") > 0 THEN
    OS-COMMAND SILENT VALUE(\"backup-\" + pcMode).
OS-COMMAND SILENT VALUE(\"restore-\" + pcMode).
";
    let diags = check_taint("ABLSEC003", src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert!(src[diags[0].span.span.start as usize..].starts_with("VALUE(\"restore-"));
}

#[test]
fn an_equality_with_literals_is_a_membership_test() {
    let src = "\
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
IF pc = \"a.p\" OR pc = \"b.p\" THEN RUN VALUE(pc).
IF NOT (pc = \"a.p\") THEN RUN VALUE(pc).
";
    assert_eq!(check_taint("ABLSEC004", src).len(), 1);
}

#[test]
fn a_case_with_literal_branches_clears_those_branches() {
    let src = "\
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
CASE pc:
    WHEN \"a.p\" OR WHEN \"b.p\" THEN RUN VALUE(pc).
    OTHERWISE RUN VALUE(pc).
END CASE.
";
    let diags = check_taint("ABLSEC004", src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert!(src[..diags[0].span.span.start as usize].contains("OTHERWISE"));
}

#[test]
fn a_temp_table_field_carries_taint() {
    let src = "\
DEFINE TEMP-TABLE ttFilter NO-UNDO FIELD cWhere AS CHARACTER.
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
CREATE ttFilter.
ttFilter.cWhere = pc.
FOR EACH ttFilter:
    hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE \" + ttFilter.cWhere).
END.
";
    let diags = check_taint("ABLSEC001", src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert!(
        diags[0].message.contains("ttFilter.cWhere"),
        "{}",
        diags[0].message
    );
}

#[test]
fn an_unknown_call_is_tainted_when_an_argument_is() {
    let src = "\
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE d AS CHARACTER NO-UNDO.
c = build-command(pc).
d = build-command(\"fixed\").
OS-COMMAND SILENT VALUE(c).
OS-COMMAND SILENT VALUE(d).
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 1);
}

#[test]
fn run_output_arguments_take_the_taint_of_the_inputs() {
    let src = "\
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE d AS CHARACTER NO-UNDO.
RUN make-path (INPUT pc, OUTPUT c).
RUN make-path (INPUT \"x\", OUTPUT d).
OUTPUT TO VALUE(c).
OUTPUT TO VALUE(d).
";
    assert_eq!(check_taint("ABLSEC005", src).len(), 1);
}

#[test]
fn a_private_procedure_has_no_untrusted_parameters_by_default() {
    let src = "\
PROCEDURE run-it PRIVATE:
    DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
    OS-COMMAND SILENT VALUE(pc).
END PROCEDURE.
PROCEDURE run-that:
    DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
    OS-COMMAND SILENT VALUE(pc).
END PROCEDURE.
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 1);
    let all = SecurityOptions {
        parameters: ParameterSources::All,
        ..taint()
    };
    assert_eq!(run_with("ABLSEC003", src, all).len(), 2);
    let none = SecurityOptions {
        parameters: ParameterSources::None,
        ..taint()
    };
    assert_eq!(run_with("ABLSEC003", src, none).len(), 0);
}

#[test]
fn an_output_parameter_is_not_a_source() {
    let src = "\
DEFINE OUTPUT PARAMETER pc AS CHARACTER NO-UNDO.
OS-COMMAND SILENT VALUE(pc).
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 0);
}

#[test]
fn method_parameters_follow_the_access_modifier() {
    let src = "\
CLASS Svc:
    METHOD PUBLIC VOID Open(INPUT pcFile AS CHARACTER):
        INPUT FROM VALUE(pcFile).
    END METHOD.
    METHOD PRIVATE VOID OpenOwn(INPUT pcFile AS CHARACTER):
        INPUT FROM VALUE(pcFile).
    END METHOD.
END CLASS.
";
    assert_eq!(check_taint("ABLSEC005", src).len(), 1);
}

#[test]
fn a_function_parameter_is_a_source_unless_private() {
    let src = "\
FUNCTION f1 RETURNS LOGICAL (INPUT pc AS CHARACTER):
    RUN VALUE(pc).
    RETURN TRUE.
END FUNCTION.
FUNCTION f2 RETURNS LOGICAL PRIVATE (INPUT pc AS CHARACTER):
    RUN VALUE(pc).
    RETURN TRUE.
END FUNCTION.
";
    assert_eq!(check_taint("ABLSEC004", src).len(), 1);
}

#[test]
fn a_class_member_set_from_a_parameter_is_tainted_in_other_methods() {
    let src = "\
CLASS Finder:
    DEFINE PRIVATE VARIABLE cFilter AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE hQ AS HANDLE NO-UNDO.
    METHOD PUBLIC VOID SetFilter(INPUT pc AS CHARACTER):
        cFilter = pc.
    END METHOD.
    METHOD PUBLIC VOID Open():
        hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE \" + cFilter).
    END METHOD.
END CLASS.
";
    let diags = check_taint("ABLSEC001", src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert!(
        diags[0].message.contains("`cFilter`"),
        "{}",
        diags[0].message
    );
}

#[test]
fn web_inputs_environment_and_session_are_sources() {
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
c = get-value(\"file\").
INPUT FROM VALUE(c).
OUTPUT TO VALUE(OS-GETENV(\"LOGDIR\") + \"/x.log\").
OS-COPY VALUE(SESSION:PARAMETER) VALUE(\"/tmp/x\").
COPY-LOB FROM FILE WEB-CONTEXT:GET-CGI-VALUE(\"GET\", \"f\") TO m.
";
    assert_eq!(check_taint("ABLSEC005", src).len(), 4);
}

#[test]
fn a_web_request_member_is_a_source() {
    let src = "\
CLASS Handler INHERITS WebHandler:
    METHOD PRIVATE VOID Serve(INPUT poRequest AS IWebRequest):
        DEFINE VARIABLE cId AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
        cId = poRequest:GetPathParameter(\"id\").
        hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE Customer.Id = \" + cId).
    END METHOD.
END CLASS.
";
    assert_eq!(check_taint("ABLSEC001", src).len(), 1);
}

#[test]
fn file_and_socket_reads_are_sources() {
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE d AS CHARACTER NO-UNDO.
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
DEFINE VARIABLE hSock AS HANDLE NO-UNDO.
IMPORT UNFORMATTED c.
OS-COMMAND SILENT VALUE(c).
hSock:READ(m, 1, 100).
d = GET-STRING(m, 1).
OS-COMMAND SILENT VALUE(d).
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 2);
}

#[test]
fn html_encoding_clears_xss_and_header_taint() {
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
c = get-value(\"name\").
PUT STREAM WebStream UNFORMATTED html-encode(c).
PUT STREAM WebStream UNFORMATTED c.
oResponse:SetHeader(\"X-Name\", url-encode(c)).
oResponse:SetHeader(\"X-Other\", c).
";
    assert_eq!(check_taint("ABLSEC006", src).len(), 1);
    assert_eq!(check_taint("ABLSEC007", src).len(), 1);
}

#[test]
fn a_validator_protects_only_its_sink_kinds() {
    let src = "\
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
IF NOT is-valid-name(pc) THEN RETURN.
hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE Customer.Name = '\" + pc + \"'\").
OS-COMMAND SILENT VALUE(\"echo \" + pc).
";
    let options = SecurityOptions {
        validators: vec![SecurityValidator {
            name: "is-valid-name".into(),
            protects: vec!["query".into()],
        }],
        ..taint()
    };
    assert_eq!(run_with("ABLSEC001", src, options.clone()).len(), 0);
    assert_eq!(run_with("ABLSEC003", src, options).len(), 1);
    assert_eq!(check_taint("ABLSEC001", src).len(), 1);
}

#[test]
fn a_validator_used_as_a_wrapper_cleans_its_result() {
    let src = "\
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
RUN VALUE(safe-program(pc)).
";
    let options = SecurityOptions {
        validators: vec![SecurityValidator {
            name: "safe-program".into(),
            protects: vec!["all".into()],
        }],
        ..taint()
    };
    assert_eq!(run_with("ABLSEC004", src, options).len(), 0);
    assert_eq!(check_taint("ABLSEC004", src).len(), 1);
}

#[test]
fn project_sources_are_configurable() {
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
c = read-body().
RUN VALUE(c).
";
    assert_eq!(check_taint("ABLSEC004", src).len(), 0);
    let options = SecurityOptions {
        sources: vec!["read-body".into()],
        ..taint()
    };
    assert_eq!(run_with("ABLSEC004", src, options).len(), 1);
}

#[test]
fn database_fields_are_sources_only_when_configured() {
    let src = "\
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
FIND FIRST Customer NO-LOCK.
hQ:QUERY-PREPARE(\"FOR EACH Order WHERE Order.Note = '\" + Customer.Comments + \"'\").
";
    assert_eq!(check_taint("ABLSEC001", src).len(), 0);
    let options = SecurityOptions {
        database_fields: true,
        ..taint()
    };
    assert_eq!(run_with("ABLSEC001", src, options).len(), 1);
}

#[test]
fn an_earlier_pair_of_the_same_assign_is_seen() {
    let src = "\
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE hSrc AS HANDLE NO-UNDO.
ASSIGN c = \"Name = \" + pc
       hSrc:FILL-WHERE-STRING = c.
";
    assert_eq!(check_taint("ABLSEC001", src).len(), 1);
}

#[test]
fn a_loop_carries_taint_around_its_back_edge() {
    let src = "\
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE d AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DO i = 1 TO 3:
    OS-COMMAND SILENT VALUE(d).
    d = c.
    c = pc.
END.
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 1);
}

#[test]
fn the_mode_can_be_set_per_rule() {
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
OS-COMMAND SILENT VALUE(c).
RUN VALUE(c).
";
    let options = SecurityOptions {
        modes: vec![("os-command".into(), SecurityMode::Sink)],
        ..taint()
    };
    assert_eq!(run_with("ABLSEC003", src, options.clone()).len(), 1);
    assert_eq!(run_with("ABLSEC004", src, options).len(), 0);
}

#[test]
fn rules_without_a_taint_mode_are_unchanged() {
    let src = "\
DEFINE VARIABLE cPassword AS CHARACTER NO-UNDO.
cPassword = \"hunter2-secret\".
DISABLE TRIGGERS FOR LOAD OF Customer.
";
    assert_eq!(
        check_taint("ABLSEC008", src).len(),
        check_sink("ABLSEC008", src).len()
    );
    assert_eq!(check_taint("ABLSEC017", src).len(), 1);
}

#[test]
fn a_value_that_the_analysis_cannot_judge_is_still_reported() {
    // `{&UNDEFINED}` stays in the text when nothing defines it.
    let src = "\
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
hQ:QUERY-PREPARE({&UNDEFINED}).
";
    assert_eq!(check_taint("ABLSEC001", src).len(), 1);
}

#[test]
fn a_main_block_sees_what_an_internal_procedure_writes_after_running_it() {
    let src = "\
DEFINE VARIABLE cCmd AS CHARACTER NO-UNDO.
cCmd = \"ls\".
OS-COMMAND SILENT VALUE(cCmd).
RUN read-command.
OS-COMMAND SILENT VALUE(cCmd).
PROCEDURE read-command:
    cCmd = get-value(\"cmd\").
END PROCEDURE.
";
    let diags = check_taint("ABLSEC003", src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert!(src[diags[0].span.span.start as usize..].starts_with("VALUE(cCmd).\nPROCEDURE"));
    assert!(diags[0].message.contains("`cCmd`"), "{}", diags[0].message);
    assert!(
        diags[0].message.contains("web input `get-value`"),
        "{}",
        diags[0].message
    );
}
