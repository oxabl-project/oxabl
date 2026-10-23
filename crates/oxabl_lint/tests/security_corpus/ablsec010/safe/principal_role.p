/* The role comes from the client principal of the request. */
DEFINE VARIABLE hPrincipal AS HANDLE NO-UNDO.
hPrincipal = SESSION:CURRENT-REQUEST-INFO:GetClientPrincipal().
IF CAN-DO(hPrincipal:ROLES, "admin") THEN
    RUN purge-orders.
