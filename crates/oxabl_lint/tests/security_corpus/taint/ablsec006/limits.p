/* xss-output (ABLSEC006): what stage 2 gets wrong. */
DEFINE VARIABLE oJson AS Progress.Json.ObjectModel.JsonObject NO-UNDO.
DEFINE VARIABLE oList AS Progress.Json.ObjectModel.JsonArray NO-UNDO.
DEFINE VARIABLE lcJson AS LONGCHAR NO-UNDO.

/* Reported: a JSON variable that another object holds is not the only
   reference to its object, also after it takes a new one: the analysis does
   not order the places the file names it. */
oJson = NEW Progress.Json.ObjectModel.JsonObject().
oList = NEW Progress.Json.ObjectModel.JsonArray().
oList:Add(oJson).
oJson = NEW Progress.Json.ObjectModel.JsonObject().
oJson:Set("status", "ok").
oJson:Write(lcJson, TRUE).
PUT STREAM WebStream UNFORMATTED lcJson. /* taint: safe */
