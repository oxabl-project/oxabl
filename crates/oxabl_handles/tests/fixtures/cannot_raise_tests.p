/* A test of variables and literals cannot raise, so an error does not skip
   the delete. */
PROCEDURE plainTests:
    DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
    DEFINE VARIABLE iA AS INTEGER NO-UNDO.
    DEFINE VARIABLE lOk AS LOGICAL NO-UNDO.
    CREATE QUERY hQ.
    IF iA > 5 AND NOT lOk THEN iA = 1.
    ELSE IF iA = 2 OR lOk THEN iA = 3.
    CASE iA:
        WHEN 1 THEN iA = 2.
        OTHERWISE iA = 3.
    END CASE.
    DO WHILE iA < 10 AND lOk:
        iA = 11.
    END.
    DELETE OBJECT hQ.
END PROCEDURE.

/* A call, an attribute, arithmetic or a field in the test can raise. */
PROCEDURE raisingTests:
    DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
    DEFINE VARIABLE iA AS INTEGER NO-UNDO.
    CREATE QUERY hQ.
    IF iA + 1 > 5 THEN iA = 1.
    DELETE OBJECT hQ.
END PROCEDURE.
