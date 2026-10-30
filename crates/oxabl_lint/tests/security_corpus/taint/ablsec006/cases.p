/* xss-output (ABLSEC006): stage 2 cases. */
DEFINE TEMP-TABLE ttRow NO-UNDO FIELD cName AS CHARACTER.
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHtml AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

cName = get-value("name").

/* Tainted, directly. */
PUT STREAM WebStream UNFORMATTED get-field("comment"). /* taint: real */

/* Tainted through an assignment chain. */
cHtml = "<p>" + cName + "</p>".
PUT STREAM WebStream UNFORMATTED cHtml. /* taint: real */

/* Tainted through a temp-table field. */
CREATE ttRow.
ttRow.cName = cName.
FOR EACH ttRow:
    oWriter:Write("<td>" + ttRow.cName + "</td>"). /* taint: real */
END.

/* Tainted through an unknown call. */
cHtml = render-row(cName).
PUT STREAM WebStream UNFORMATTED cHtml. /* taint: real */

/* Sanitised for this sink. */
PUT STREAM WebStream UNFORMATTED "<p>" + html-encode(cName) + "</p>". /* taint: safe */

/* Sanitised for the wrong sink. */
PUT STREAM WebStream UNFORMATTED "<p>" + QUOTER(cName) + "</p>". /* taint: real */

/* A number. */
iCount = NUM-ENTRIES(cName).
PUT STREAM WebStream UNFORMATTED "<p>" + STRING(iCount) + "</p>". /* taint: safe */

/* A constant held in a variable. */
cHtml = "<p>Welcome</p>".
PUT STREAM WebStream UNFORMATTED cHtml. /* taint: safe */

/* An unknown call with constant arguments. */
cHtml = render-row("header").
oWriter:Write(cHtml). /* taint: safe */

/* Tainted through a JSON object written to text. */
DEFINE VARIABLE oJson AS Progress.Json.ObjectModel.JsonObject NO-UNDO.
DEFINE VARIABLE oList AS Progress.Json.ObjectModel.JsonArray NO-UNDO.
DEFINE VARIABLE lcJson AS LONGCHAR NO-UNDO.
oJson = NEW Progress.Json.ObjectModel.JsonObject().
oJson:Add("name", get-value("name")).
oJson:Write(lcJson).
PUT STREAM WebStream UNFORMATTED lcJson. /* taint: real */

/* Through an array that holds the object. */
oList = NEW Progress.Json.ObjectModel.JsonArray().
oList:Add(oJson).
lcJson = oList:GetJsonText().
oWriter:Write(lcJson). /* taint: real */

/* A JSON object of constants. */
oJson = NEW Progress.Json.ObjectModel.JsonObject().
oJson:Set("status", "ok").
oJson:Write(lcJson, TRUE).
PUT STREAM WebStream UNFORMATTED lcJson. /* taint: safe */
