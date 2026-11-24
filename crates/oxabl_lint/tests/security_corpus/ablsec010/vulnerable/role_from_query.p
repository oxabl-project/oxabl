/* expect: 1 */
/* A role from the query string decides who may purge. */
IF get-value("role") = "admin" THEN
    RUN purge-orders.
