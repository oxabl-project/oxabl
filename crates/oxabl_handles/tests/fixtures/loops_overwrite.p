DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE hOther AS HANDLE NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

/* Every iteration but the last loses its query. */
DO i = 1 TO 10:
    CREATE QUERY hQuery.
END.
DELETE OBJECT hQuery.

/* Freed in the loop: fine. */
DO i = 1 TO 10:
    CREATE QUERY hQuery.
    DELETE OBJECT hQuery.
END.

/* Overwritten by a second create and by the unknown value. */
CREATE BUFFER hOther FOR TABLE "customer".
CREATE BUFFER hOther FOR TABLE "order".
hOther = ?.
