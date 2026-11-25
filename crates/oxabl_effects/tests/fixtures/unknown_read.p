/* A read of a table the schema does not have. */
FIND FIRST Customer NO-LOCK.

PROCEDURE gone:
    FOR EACH Nosuch NO-LOCK:
    END.
END PROCEDURE.
