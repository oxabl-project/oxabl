DEFINE VARIABLE i AS INTEGER NO-UNDO.
REPEAT:
    DO ON ERROR UNDO, LEAVE:
        i = 1.
        CATCH e AS Progress.Lang.Error:
            UNDO, LEAVE.
        END CATCH.
    END.
    MESSAGE "after the DO, still in the REPEAT".
END.
FOR EACH Customer NO-LOCK:
    i = 2.
    CATCH e2 AS Progress.Lang.Error:
        IF i = 2 THEN UNDO, NEXT.
        IF i = 3 THEN UNDO, RETRY.
        IF i = 4 THEN UNDO.
        UNDO, LEAVE.
    END CATCH.
END.
REPEAT:
    DO ON ERROR UNDO, LEAVE:
        i = 3.
        CATCH e3 AS Progress.Lang.Error:
            IF i = 3 THEN DO:
                UNDO, LEAVE.
            END.
            UNDO, NEXT.
        END CATCH.
    END.
END.
