DEFINE VARIABLE oJson AS Progress.Json.ObjectModel.JsonObject NO-UNDO.

oJson = NEW Progress.Json.ObjectModel.JsonObject().
oJson:Add("name", "value").
