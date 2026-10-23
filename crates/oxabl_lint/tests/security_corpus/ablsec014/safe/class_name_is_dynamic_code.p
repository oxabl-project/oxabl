DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE VARIABLE oClass AS Progress.Lang.Class NO-UNDO.

oClass = Progress.Lang.Class:GetClass("Acme.Fixed").
