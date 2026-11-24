DEFINE INPUT PARAMETER pcMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE oError AS Progress.Lang.AppError NO-UNDO.

oError = NEW Progress.Lang.AppError(pcMessage, 1).
