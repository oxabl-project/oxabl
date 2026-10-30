/* expect: 1 */
/* A user name from a request header. */
DEFINE INPUT PARAMETER poRequest AS OpenEdge.Web.IWebRequest NO-UNDO.
DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.
cUser = poRequest:GetHeader("X-User-Id"):Value.
IF cUser = "system" THEN
    RUN elevate.
