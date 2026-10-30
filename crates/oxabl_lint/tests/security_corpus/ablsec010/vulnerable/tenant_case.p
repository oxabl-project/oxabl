/* expect: 1 */
/* A function of the file returns the tenant the caller names, and a CASE reads it. */
FUNCTION request-tenant RETURNS CHARACTER PRIVATE ():
    RETURN get-value("tenant").
END FUNCTION.
CASE request-tenant():
    WHEN "north" THEN RUN north-orders.
    OTHERWISE RUN all-orders.
END CASE.
