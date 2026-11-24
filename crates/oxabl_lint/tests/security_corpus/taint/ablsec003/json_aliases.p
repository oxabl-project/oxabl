/* Several references may point to one JSON object. What is added through
   one of them is in what the others read, so a JSON variable that is not
   the only reference to its object reads Unknown. */
USING Progress.Json.ObjectModel.*.

DEFINE VARIABLE lcText AS LONGCHAR NO-UNDO.

/* A copy, also in an ASSIGN statement. */
DEFINE VARIABLE oCopy1 AS JsonObject NO-UNDO.
DEFINE VARIABLE oCopy2 AS JsonObject NO-UNDO.
oCopy1 = NEW JsonObject().
oCopy2 = oCopy1.
oCopy1:Add("k", get-value("cmd")).
OS-COMMAND VALUE(oCopy2:GetCharacter("k")). /* taint: real */
DEFINE VARIABLE aCopy1 AS JsonArray NO-UNDO.
DEFINE VARIABLE aCopy2 AS JsonArray NO-UNDO.
aCopy1 = NEW JsonArray().
ASSIGN aCopy2 = aCopy1.
aCopy1:Add(get-value("cmd")).
OS-COMMAND VALUE(aCopy2:GetCharacter(1)). /* taint: real */

/* An object added to another, and changed after it. */
DEFINE VARIABLE oOuter AS JsonObject NO-UNDO.
DEFINE VARIABLE oInner AS JsonObject NO-UNDO.
oOuter = NEW JsonObject().
oInner = NEW JsonObject().
oOuter:Add("in", oInner).
oInner:Add("k", get-value("cmd")).
OS-COMMAND VALUE(oOuter:GetJsonObject("in"):GetCharacter("k")). /* taint: real */
oOuter:Write(lcText).
OS-COMMAND VALUE(STRING(lcText)). /* taint: real */

/* A reference got from another object. */
DEFINE VARIABLE oHolder AS JsonObject NO-UNDO.
DEFINE VARIABLE oGot AS JsonObject NO-UNDO.
oHolder = NEW JsonObject().
oHolder:Add("in", NEW JsonObject()).
oGot = oHolder:GetJsonObject("in").
oGot:Add("k", get-value("cmd")).
OS-COMMAND VALUE(oHolder:GetJsonObject("in"):GetCharacter("k")). /* taint: real */

/* An object passed to a routine of the file, which adds to it. */
DEFINE VARIABLE oPassed AS JsonObject NO-UNDO.
PROCEDURE fill-object:
    DEFINE INPUT PARAMETER poTarget AS JsonObject NO-UNDO.
    poTarget:Add("k", get-value("cmd")).
END PROCEDURE.
oPassed = NEW JsonObject().
RUN fill-object (oPassed).
OS-COMMAND VALUE(oPassed:GetCharacter("k")). /* taint: real */

/* The only reference to an object of constants. */
DEFINE VARIABLE oOwn AS JsonObject NO-UNDO.
oOwn = NEW JsonObject().
oOwn:Add("k", "ls").
OS-COMMAND VALUE(oOwn:GetCharacter("k")). /* taint: safe */
