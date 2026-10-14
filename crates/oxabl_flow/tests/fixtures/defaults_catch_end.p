DEFINE VARIABLE i AS INTEGER NO-UNDO.
REPEAT:
    i = 1.
    CATCH e AS Progress.Lang.Error:
        MESSAGE "the REPEAT goes on with its next iteration".
    END CATCH.
END.
DO:
    i = 2.
    CATCH e2 AS Progress.Lang.Error:
        MESSAGE "the DO is left".
    END CATCH.
END.
PROCEDURE p1:
    DO i = 1 TO 3:
        IF i = 2 THEN NEXT.
        MESSAGE i.
        FINALLY:
            MESSAGE "fin".
        END FINALLY.
    END.
    RETURN ERROR "x".
    CATCH e3 AS Progress.Lang.Error:
        MESSAGE "RETURN ERROR does not reach this CATCH".
    END CATCH.
    FINALLY:
        MESSAGE "but the FINALLY runs".
    END FINALLY.
END PROCEDURE.
