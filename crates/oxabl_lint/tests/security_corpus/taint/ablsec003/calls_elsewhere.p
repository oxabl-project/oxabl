/* What code the file does not hold gives back, in an OUTPUT argument or as
   a result, is Unknown, as its RETURN-VALUE is: another program, a function
   or procedure of another one, a method of an object. */
DEFINE VARIABLE cOut AS CHARACTER NO-UNDO.
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
DEFINE VARIABLE oLib AS Progress.Lang.Object NO-UNDO.

FUNCTION lib-cmd RETURNS CHARACTER IN hLib.

FUNCTION fixed-cmd RETURNS CHARACTER PRIVATE ():
    RETURN "ls".
END FUNCTION.

RUN lib.p (OUTPUT cOut).
OS-COMMAND VALUE(cOut). /* taint: real */
RUN lib.p PERSISTENT SET hLib.
cOut = "".
DYNAMIC-FUNCTION("get-cmd" IN hLib, OUTPUT cOut).
OS-COMMAND VALUE(cOut). /* taint: real */
cOut = "".
DYNAMIC-INVOKE(hLib, "get-cmd", OUTPUT cOut).
OS-COMMAND VALUE(cOut). /* taint: real */
OS-COMMAND VALUE(lib-cmd()). /* taint: real */
oLib = NEW pkg.Lib().
cOut = oLib:GetCmd().
OS-COMMAND VALUE(cOut). /* taint: real */
cOut = "".
oLib:GetCmd(OUTPUT cOut).
OS-COMMAND VALUE(cOut). /* taint: real */

/* A function of the file that returns a literal. */
OS-COMMAND VALUE(fixed-cmd()). /* taint: safe */

/* A function the file does not declare. ABL compiles no such call in a file
   that holds all its code, so the call is read as its arguments. */
OS-COMMAND VALUE(build-cmd("ls")). /* taint: safe */
