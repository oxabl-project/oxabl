DEFINE OUTPUT PARAMETER pcError AS CHARACTER NO-UNDO.

DO ON ERROR UNDO, LEAVE:
    RUN work.p.
    CATCH e AS Progress.Lang.Error:
        LOG-MANAGER:WRITE-MESSAGE(e:GetMessage(1)).
        pcError = "The request failed".
    END CATCH.
END.
