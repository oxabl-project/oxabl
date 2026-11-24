/* Authorises the request: the session must hold the admin role. */
IF NOT CAN-DO(SESSION:CURRENT-REQUEST-INFO:GetClientPrincipal():ROLES, "admin") THEN
    RETURN ERROR "denied".
