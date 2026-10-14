//! Unreserved keywords (`image`, `menu`, `server`, ...) are recased only where
//! they are used as keywords, never where they name something.

use oxabl_formatter::format;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_style::{KeywordCase, StyleGuide};

fn fmt(src: &str, case: KeywordCase) -> String {
    let tokens = tokenize(src);
    let program = Parser::new(&tokens, src).parse_program();
    assert!(program.is_ok(), "{src}: {:?}", program.errors);
    let mut style = StyleGuide::default_base();
    style.keyword_case = case;
    style.indent_size = 2;
    format(src, &program, &style).unwrap_or_else(|e| panic!("unexpected bail: {e}"))
}

#[track_caller]
fn assert_formats(src: &str, upper: &str, lower: &str) {
    assert_eq!(fmt(src, KeywordCase::Uppercase), upper, "uppercase");
    assert_eq!(fmt(src, KeywordCase::Lowercase), lower, "lowercase");
}

#[test]
fn declared_names_keep_their_spelling() {
    let src = "\
Define Variable image As Character No-Undo.
Define Variable Menu As Character No-Undo.
Define Variable server As Integer No-Undo.
Define Temp-Table ttx No-Undo
  Field Image As Character
  Field socket As Character.
Image = \"a\".
Menu = image + \"b\".
Create ttx.
ttx.Image = image.
Message image Menu server ttx.socket.
";
    let upper = "\
DEFINE VARIABLE image AS CHARACTER NO-UNDO.
DEFINE VARIABLE Menu AS CHARACTER NO-UNDO.
DEFINE VARIABLE server AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE ttx NO-UNDO
  FIELD Image AS CHARACTER
  FIELD socket AS CHARACTER.
Image = \"a\".
Menu = image + \"b\".
CREATE ttx.
ttx.Image = image.
MESSAGE image Menu server ttx.socket.
";
    let lower = upper
        .replace("DEFINE VARIABLE", "define variable")
        .replace("AS CHARACTER", "as character")
        .replace("AS INTEGER", "as integer")
        .replace("NO-UNDO", "no-undo")
        .replace("DEFINE TEMP-TABLE", "define temp-table")
        .replace("FIELD", "field")
        .replace("CREATE", "create")
        .replace("MESSAGE", "message");
    assert_formats(src, upper, &lower);
}

#[test]
fn procedure_function_and_parameter_names_keep_their_spelling() {
    let src = "\
Procedure Menu:
  Define Input Parameter Socket As Integer No-Undo.
  Message socket.
End Procedure.
Function image Returns Character (Input server As Character):
  Return server.
End Function.
Run Menu (Input 1).
Message image(\"a\").
";
    let upper = "\
PROCEDURE Menu:
  DEFINE INPUT PARAMETER Socket AS INTEGER NO-UNDO.
  MESSAGE socket.
END PROCEDURE.
FUNCTION image RETURNS CHARACTER (INPUT server AS CHARACTER):
  RETURN server.
END FUNCTION.
RUN Menu (INPUT 1).
MESSAGE image(\"a\").
";
    let lower = "\
procedure Menu:
  define input parameter Socket as integer no-undo.
  message socket.
end procedure.
function image returns character (input server as character):
  return server.
end function.
run Menu (input 1).
message image(\"a\").
";
    assert_formats(src, upper, lower);
}

#[test]
fn class_member_and_qualified_names_keep_their_spelling() {
    let src = "\
Using Progress.Lang.*.
Class pkg.Server:
  Define Public Property Image As Character No-Undo Get. Set.
  Define Private Variable editor As Character No-Undo.
  Method Public Void Socket(Input server As Character):
    Define Variable o As pkg.Server No-Undo.
    Define Variable e As Progress.Lang.Object No-Undo.
    o = New pkg.Server().
    o:Image = server.
    This-Object:Menu = o:Image.
    o:Socket(editor).
  End Method.
End Class.
";
    let upper = "\
USING Progress.Lang.*.
CLASS pkg.Server:
  DEFINE PUBLIC PROPERTY Image AS CHARACTER NO-UNDO GET. SET.
  DEFINE PRIVATE VARIABLE editor AS CHARACTER NO-UNDO.
  METHOD PUBLIC VOID Socket(INPUT server AS CHARACTER):
    DEFINE VARIABLE o AS pkg.Server NO-UNDO.
    DEFINE VARIABLE e AS Progress.Lang.Object NO-UNDO.
    o = NEW pkg.Server().
    o:Image = server.
    THIS-OBJECT:Menu = o:Image.
    o:Socket(editor).
  END METHOD.
END CLASS.
";
    let lower = "\
using Progress.Lang.*.
class pkg.Server:
  define public property Image as character no-undo get. set.
  define private variable editor as character no-undo.
  method public void Socket(input server as character):
    define variable o as pkg.Server no-undo.
    define variable e as Progress.Lang.Object no-undo.
    o = new pkg.Server().
    o:Image = server.
    this-object:Menu = o:Image.
    o:Socket(editor).
  end method.
end class.
";
    assert_formats(src, upper, lower);
}

#[test]
fn a_keyword_used_as_a_type_is_still_recased() {
    let src = "\
Define Variable ed As Character View-As Editor Size 10 By 2.
Create Button hb In Widget-Pool \"p\".
Create Image hi.
Create Menu hm.
";
    let upper = "\
DEFINE VARIABLE ed AS CHARACTER VIEW-AS EDITOR SIZE 10 BY 2.
CREATE BUTTON hb IN WIDGET-POOL \"p\".
CREATE IMAGE hi.
CREATE MENU hm.
";
    let lower = "\
define variable ed as character view-as editor size 10 by 2.
create button hb in widget-pool \"p\".
create image hi.
create menu hm.
";
    assert_formats(src, upper, lower);
}

#[test]
fn delete_object_and_widget_keep_their_keyword_recasing() {
    let src = "Delete Object hQ.\nDelete Widget hW.\nRelease Object hC.\nDelete Procedure This-Procedure.\n";
    let upper = "DELETE OBJECT hQ.\nDELETE WIDGET hW.\nRELEASE OBJECT hC.\nDELETE PROCEDURE THIS-PROCEDURE.\n";
    let lower = "delete object hQ.\ndelete widget hW.\nrelease object hC.\ndelete procedure this-procedure.\n";
    assert_formats(src, upper, lower);
}

#[test]
fn class_names_in_type_positions_keep_their_spelling() {
    let src = "\
Using pkg.*.
Class pkg2.Client Inherits Server Implements pkg.IFoo, Socket, Image:
  Method Public Void Go(Input o As Progress.Lang.Object):
    Define Variable s As Server No-Undo.
    s = Cast(o, Server).
    If Type-Of(o, Socket) Then Message \"x\".
    Message Get-Class(Image):TypeName.
    s = Dynamic-Cast(o, Menu).
    s = New Server().
    Menu:StaticCall().
  End Method.
End Class.
";
    let upper = "\
USING pkg.*.
CLASS pkg2.Client INHERITS Server IMPLEMENTS pkg.IFoo, Socket, Image:
  METHOD PUBLIC VOID Go(INPUT o AS Progress.Lang.Object):
    DEFINE VARIABLE s AS Server NO-UNDO.
    s = CAST(o, Server).
    IF Type-Of(o, Socket) THEN MESSAGE \"x\".
    MESSAGE Get-Class(Image):TypeName.
    s = Dynamic-Cast(o, Menu).
    s = NEW Server().
    Menu:StaticCall().
  END METHOD.
END CLASS.
";
    let lower = "\
using pkg.*.
class pkg2.Client inherits Server implements pkg.IFoo, Socket, Image:
  method public void Go(input o as Progress.Lang.Object):
    define variable s as Server no-undo.
    s = cast(o, Server).
    if Type-Of(o, Socket) then message \"x\".
    message Get-Class(Image):TypeName.
    s = Dynamic-Cast(o, Menu).
    s = new Server().
    Menu:StaticCall().
  end method.
end class.
";
    assert_formats(src, upper, lower);
}

#[test]
fn widget_operand_keywords_are_recased_after_create_define_and_view_as() {
    let src = "\
Define Button bOk Label \"Ok\".
Define Image iLogo File \"a.bmp\".
Define Variable cTxt As Character View-As Editor Size 10 By 2.
Define Variable lTgl As Logical View-As Toggle-Box.
Create Server hServer.
Create Socket hSock.
Delete Object hSock.
";
    let upper = "\
DEFINE BUTTON bOk LABEL \"Ok\".
DEFINE IMAGE iLogo File \"a.bmp\".
DEFINE VARIABLE cTxt AS CHARACTER VIEW-AS EDITOR SIZE 10 BY 2.
DEFINE VARIABLE lTgl AS LOGICAL VIEW-AS TOGGLE-BOX.
CREATE SERVER hServer.
CREATE SOCKET hSock.
DELETE OBJECT hSock.
";
    let lower = "\
define button bOk label \"Ok\".
define image iLogo File \"a.bmp\".
define variable cTxt as character view-as editor size 10 by 2.
define variable lTgl as logical view-as toggle-box.
create server hServer.
create socket hSock.
delete object hSock.
";
    assert_formats(src, upper, lower);
}

#[test]
fn a_new_keyword_that_names_a_table_keeps_its_spelling() {
    let src = "\
Define Temp-Table Image No-Undo Field id As Integer.
Create Image.
Delete Image.
Create Button hb.
";
    let upper = "\
DEFINE TEMP-TABLE Image NO-UNDO FIELD id AS INTEGER.
CREATE Image.
DELETE Image.
CREATE BUTTON hb.
";
    assert_eq!(fmt(src, KeywordCase::Uppercase), upper);
}

#[test]
fn system_handle_receivers_are_recased() {
    let src = "\
Message log-manager:logfile-name security-policy:xml-schema profiler:enabled.
Message audit-control:appl-context-id rcode-info:file-name error-status:error.
";
    let upper = "\
MESSAGE LOG-MANAGER:logfile-name SECURITY-POLICY:xml-schema PROFILER:enabled.
MESSAGE AUDIT-CONTROL:appl-context-id RCODE-INFO:file-name ERROR-STATUS:error.
";
    assert_eq!(fmt(src, KeywordCase::Uppercase), upper);
}

#[test]
fn built_in_members_after_a_colon_are_recased() {
    let src = "\
Define Variable hq As Handle No-Undo.
Define Variable hb As Handle No-Undo.
hq:Num-Results = hb:Label + hb:Title + hb:Num-Entries.
hb:Screen-Value = String(hq:Handle).
";
    let upper = "\
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
DEFINE VARIABLE hb AS HANDLE NO-UNDO.
hq:Num-Results = hb:LABEL + hb:TITLE + hb:NUM-ENTRIES.
hb:Screen-Value = String(hq:HANDLE).
";
    assert_eq!(fmt(src, KeywordCase::Uppercase), upper);
}

#[test]
fn keywords_are_recased_even_when_a_field_has_their_spelling() {
    let src = "\
Define Temp-Table tt No-Undo Field name As Character Field label As Character.
Display tt.name Label \"Name\".
Message label.
";
    let upper = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD name AS CHARACTER FIELD LABEL AS CHARACTER.
DISPLAY tt.name LABEL \"Name\".
MESSAGE LABEL.
";
    assert_eq!(fmt(src, KeywordCase::Uppercase), upper);
}

#[test]
fn a_field_named_like_an_introducer_does_not_protect_the_next_word() {
    let src = "If b.Class Eq \"x\" Then Message \"y\".\nRun fill.p.\nRun Fill.p (Input 1).\n";
    let upper = "IF b.CLASS EQ \"x\" THEN MESSAGE \"y\".\nRUN fill.p.\nRUN Fill.p (INPUT 1).\n";
    assert_eq!(fmt(src, KeywordCase::Uppercase), upper);
}
