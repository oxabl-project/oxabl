/* /report.w: an internal procedure checks the role. */
{web/common.i}
RUN require-role (INPUT "report").
CREATE ReportRun. /* reach: safe ABLSEC011 */
RUN lib/cache.p.

PROCEDURE require-role:
    DEFINE INPUT PARAMETER pcRole AS CHARACTER NO-UNDO.
    IF NOT CAN-DO(SESSION:CURRENT-REQUEST-INFO:GetClientPrincipal():ROLES, pcRole) THEN
        RETURN ERROR "denied".
END PROCEDURE.
