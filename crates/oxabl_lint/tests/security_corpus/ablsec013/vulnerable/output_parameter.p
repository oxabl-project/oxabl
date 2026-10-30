/* expect: 3 */
DEFINE OUTPUT PARAMETER pcError AS CHARACTER NO-UNDO.

DO ON ERROR UNDO, LEAVE:
    RUN work.p.
    CATCH e AS Progress.Lang.Error:
        pcError = e:GetMessage(1).
        pcError = pcError + e:CallStack.
        ASSIGN pcError = ERROR-STATUS:GET-MESSAGE(1).
    END CATCH.
END.
