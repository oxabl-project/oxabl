/* Values the runtime carries between statements, outside any variable:
   RETURN-VALUE, the text of RETURN ERROR, a CATCH variable, and the
   members of a handle that the runtime fills. */
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS CHARACTER.

PROCEDURE get-cmd PRIVATE:
    RETURN get-value("cmd").
END PROCEDURE.

PROCEDURE echo PRIVATE:
    DEFINE INPUT PARAMETER p AS CHARACTER NO-UNDO.
    RETURN p.
END PROCEDURE.

PROCEDURE fail-with PRIVATE:
    RETURN ERROR get-value("cmd").
END PROCEDURE.

PROCEDURE fixed PRIVATE:
    RETURN "ls".
END PROCEDURE.

RUN get-cmd.
OS-COMMAND VALUE(RETURN-VALUE). /* taint: real */
RUN echo (get-value("cmd")).
c = RETURN-VALUE.
OS-COMMAND VALUE(c). /* taint: real */
RUN fail-with NO-ERROR.
OS-COMMAND VALUE(RETURN-VALUE). /* taint: real */
RUN other.p (get-value("cmd")).
OS-COMMAND VALUE(RETURN-VALUE). /* taint: real */
RUN fixed.
OS-COMMAND VALUE(RETURN-VALUE). /* taint: safe */
RUN echo ("ls").
OS-COMMAND VALUE(RETURN-VALUE). /* taint: safe */

DO ON ERROR UNDO, THROW:
    UNDO, THROW NEW Progress.Lang.AppError(get-value("cmd"), 1).
    CATCH e AS Progress.Lang.Error:
        OS-COMMAND VALUE(e:GetMessage(1)). /* taint: real */
    END CATCH.
END.

/* The message names the character that is not a digit. */
i = INTEGER(get-value("n")) NO-ERROR.
OS-COMMAND VALUE(ERROR-STATUS:GET-MESSAGE(1)). /* taint: real */

CREATE tt.
tt.f = get-value("cmd").
hBuf = BUFFER tt:HANDLE.
OS-COMMAND VALUE(hBuf:BUFFER-FIELD("f"):BUFFER-VALUE). /* taint: real */
OS-COMMAND VALUE(SESSION:TEMP-DIRECTORY + "purge.sh"). /* taint: safe */

/* The RETURN action of a block's ON phrase sets RETURN-VALUE when the
   condition comes, also when nothing in the block is seen to raise it. */
DEFINE VARIABLE cMsg AS CHARACTER NO-UNDO.

PROCEDURE on-error-return PRIVATE:
    RUN fixed.
    DO ON ERROR UNDO, RETURN cMsg:
        i = 1.
    END.
END PROCEDURE.

PROCEDURE on-error-return-run PRIVATE:
    RUN fixed.
    DO ON ERROR UNDO, RETURN cMsg:
        RUN fixed.
    END.
END PROCEDURE.

PROCEDURE on-error-return-error PRIVATE:
    RUN fixed.
    DO ON ERROR UNDO, RETURN ERROR cMsg:
        i = INTEGER(cMsg).
    END.
END PROCEDURE.

PROCEDURE on-endkey-return PRIVATE:
    RUN fixed.
    REPEAT ON ENDKEY UNDO, RETURN cMsg:
        i = 1.
    END.
END PROCEDURE.

PROCEDURE on-error-return-fixed PRIVATE:
    RUN fixed.
    DO ON ERROR UNDO, RETURN "ls":
        i = INTEGER(cMsg).
    END.
END PROCEDURE.

cMsg = get-value("cmd").
RUN on-error-return.
OS-COMMAND VALUE(RETURN-VALUE). /* taint: real */
RUN on-error-return-run.
OS-COMMAND VALUE(RETURN-VALUE). /* taint: real */
RUN on-error-return-error NO-ERROR.
OS-COMMAND VALUE(RETURN-VALUE). /* taint: real */
RUN on-endkey-return.
OS-COMMAND VALUE(RETURN-VALUE). /* taint: real */
RUN on-error-return-fixed.
OS-COMMAND VALUE(RETURN-VALUE). /* taint: safe */

/* A built-in method that writes its argument: a node of a parsed document,
   and what a method copies out of it, hold what the document holds. */
DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
DEFINE VARIABLE hRoot AS HANDLE NO-UNDO.
DEFINE VARIABLE lcXml AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcOut AS LONGCHAR NO-UNDO.
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
CREATE X-DOCUMENT hDoc.
CREATE X-NODEREF hRoot.
lcXml = get-value("xml").
hDoc:LOAD("longchar", lcXml, FALSE).
hDoc:GET-DOCUMENT-ELEMENT(hRoot).
OS-COMMAND VALUE(hRoot:NAME). /* taint: real */
hRoot:NODE-VALUE-TO-LONGCHAR(lcOut).
OS-COMMAND VALUE(STRING(lcOut)). /* taint: real */
lcOut = "".
hDoc:SAVE("longchar", lcOut).
OS-COMMAND VALUE(STRING(lcOut)). /* taint: real */
cFile = "out.xml".
hDoc:SAVE("file", cFile).
OS-COMMAND VALUE("rm " + cFile). /* taint: safe */
lcOut = "".
TEMP-TABLE tt:WRITE-JSON("longchar", lcOut).
OS-COMMAND VALUE(STRING(lcOut)). /* taint: real */

/* A handle that another program fills, also through a variable, and the
   PRIVATE-DATA of the session and of this procedure, which any program of
   the session or the caller may set. */
DEFINE VARIABLE hOther AS HANDLE NO-UNDO.
hOther = SOURCE-PROCEDURE.
OS-COMMAND VALUE(hOther:PRIVATE-DATA). /* taint: real */
hOther = SELF.
OS-COMMAND VALUE(hOther:PRIVATE-DATA). /* taint: real */
OS-COMMAND VALUE(THIS-PROCEDURE:PRIVATE-DATA). /* taint: real */
OS-COMMAND VALUE(SESSION:PRIVATE-DATA). /* taint: real */
hOther = THIS-PROCEDURE.
OS-COMMAND VALUE(hOther:PRIVATE-DATA). /* taint: real */
