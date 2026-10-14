DEFINE VARIABLE i AS INTEGER NO-UNDO.
DO ON ERROR UNDO, LEAVE:
    FOR EACH Customer:
        IF i = 0 THEN UNDO, THROW NEW Progress.Lang.AppError("x", 1).
        MESSAGE "body".
    END.
    CATCH e AS Progress.Lang.Error:
        MESSAGE "outer catch".
    END CATCH.
END.
outer:
DO ON ERROR UNDO, LEAVE:
    i = 5.
    DO ON ERROR UNDO, LEAVE:
        IF i = 1 THEN UNDO outer, THROW NEW Progress.Lang.AppError("y", 1).
    END.
    CATCH e2 AS Progress.Lang.Error:
        MESSAGE "never reached by the throw above".
    END CATCH.
END.
DO ON ERROR UNDO, LEAVE:
    DO ON ERROR UNDO, LEAVE:
        i = 2.
        CATCH e3 AS Progress.Lang.Error:
            UNDO, THROW e3.
        END CATCH.
    END.
    CATCH e4 AS Progress.Lang.Error:
        MESSAGE "rethrown".
    END CATCH.
END.
DO ON ERROR UNDO, LEAVE:
    THROW NEW Progress.Lang.AppError("z", 1).
    CATCH e5 AS Progress.Lang.Error:
        MESSAGE "the bare THROW stays in its own block".
    END CATCH.
END.
