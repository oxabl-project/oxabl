/* expect: 1 */
CLASS Acme.Service:
    METHOD PUBLIC VOID Run(OUTPUT pcError AS CHARACTER):
        DO ON ERROR UNDO, LEAVE:
            RUN work.p.
            CATCH e AS Progress.Lang.Error:
                pcError = SUBSTITUTE("Failed: &1", e:GetMessage(1)).
            END CATCH.
        END.
    END METHOD.
END CLASS.
