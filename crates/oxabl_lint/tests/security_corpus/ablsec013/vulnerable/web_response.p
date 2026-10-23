/* expect: 3 */
&GLOBAL-DEFINE OUT PUT STREAM WebStream UNFORMATTED
DO ON ERROR UNDO, LEAVE:
    RUN work.p.
    CATCH e AS Progress.Lang.Error:
        {&OUT} "<p>" e:GetMessage(1) "</p>".
        oResp:Write(e:CallStack).
        oResponse:Entity = NEW OpenEdge.Core.String(ERROR-STATUS:GET-MESSAGE(1)).
    END CATCH.
END.
