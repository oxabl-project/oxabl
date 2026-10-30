DEFINE VARIABLE cMsg AS CHARACTER NO-UNDO.

DO ON ERROR UNDO, LEAVE:
    RUN work.p.
    CATCH e AS Progress.Lang.Error:
        cMsg = e:GetMessage(1).
        MESSAGE cMsg.
    END CATCH.
END.
