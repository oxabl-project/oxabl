/* Stops the request unless the session may do `pcRight`. */
DEFINE INPUT PARAMETER pcRight AS CHARACTER NO-UNDO.
IF NOT CAN-DO(SESSION:CURRENT-REQUEST-INFO:GetClientPrincipal():ROLES, pcRight) THEN
    RETURN ERROR "denied".
