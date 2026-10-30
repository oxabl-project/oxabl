/* expect: 1 */
/* The owner named in a JSON body decides whether the update is allowed. */
USING Progress.Json.ObjectModel.*.
DEFINE INPUT PARAMETER poRequest AS OpenEdge.Web.IWebRequest NO-UNDO.
DEFINE VARIABLE oBody AS JsonObject NO-UNDO.
oBody = CAST(poRequest:Entity, JsonObject).
IF oBody:GetCharacter("ownerId") <> "" THEN
    RUN update-order.
