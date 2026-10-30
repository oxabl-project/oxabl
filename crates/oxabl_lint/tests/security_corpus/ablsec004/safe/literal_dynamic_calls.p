DEFINE INPUT PARAMETER pcValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE hObj AS HANDLE NO-UNDO.
DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
DEFINE VARIABLE oObj AS Progress.Lang.Object NO-UNDO.
DEFINE VARIABLE oClass AS Progress.Lang.Class NO-UNDO.

cResult = DYNAMIC-FUNCTION("calc-tax", pcValue).
DYNAMIC-INVOKE(hObj, "Handle", pcValue).
oObj = DYNAMIC-NEW "Acme.Handler" (pcValue).
oClass = Progress.Lang.Class:GetClass("Acme.Handler").
