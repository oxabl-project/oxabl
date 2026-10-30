/* The user of the session, not of the request. */
IF USERID("sports") = "admin" THEN
    RUN purge-orders.
