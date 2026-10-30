/* DYNAMIC-FUNCTION of a literal name runs the function of the file by that
   name; IN a handle, it may run a function of another program. */
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.

FUNCTION get-cmd RETURNS CHARACTER PRIVATE ():
    RETURN get-value("cmd").
END FUNCTION.

FUNCTION fixed-cmd RETURNS CHARACTER PRIVATE ():
    RETURN "ls".
END FUNCTION.

OS-COMMAND VALUE(DYNAMIC-FUNCTION("get-cmd")). /* taint: real */
c = DYNAMIC-FUNCTION("get-cmd").
OS-COMMAND VALUE(c). /* taint: real */
c = DYNAMIC-FUNCTION("get-cmd" IN THIS-PROCEDURE).
OS-COMMAND VALUE(c). /* taint: real */
RUN lib.p PERSISTENT SET hLib.
c = DYNAMIC-FUNCTION("get-cmd" IN hLib).
OS-COMMAND VALUE(c). /* taint: real */
OS-COMMAND VALUE(DYNAMIC-FUNCTION("fixed-cmd")). /* taint: safe */
c = DYNAMIC-FUNCTION("fixed-cmd").
OS-COMMAND VALUE(c). /* taint: safe */
