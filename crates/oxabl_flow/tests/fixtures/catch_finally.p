DEFINE VARIABLE h AS HANDLE NO-UNDO.
outer:
DO ON ERROR UNDO, LEAVE:
    inner:
    DO ON ERROR UNDO, THROW:
        CREATE QUERY h.
        IF VALID-HANDLE(h) THEN LEAVE outer.
        CATCH e AS Progress.Lang.AppError:
            MESSAGE e:GetMessage(1).
        END CATCH.
        FINALLY:
            DELETE OBJECT h NO-ERROR.
        END FINALLY.
    END.
    CATCH e2 AS Progress.Lang.Error:
        MESSAGE e2:GetMessage(1).
    END CATCH.
    FINALLY:
        h = ?.
    END FINALLY.
END.
