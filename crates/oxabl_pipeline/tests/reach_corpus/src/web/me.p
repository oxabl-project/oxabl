/* GET /api/me: echoes the name as JSON. */
USING Progress.Json.ObjectModel.*.
DEFINE VARIABLE oJson AS JsonObject NO-UNDO.
DEFINE VARIABLE lcOut AS LONGCHAR NO-UNDO.
oJson = NEW JsonObject().
oJson:Add("name", get-value("name")).
oJson:Write(lcOut).
PUT STREAM WebStream UNFORMATTED lcOut.
