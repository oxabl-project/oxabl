DEFINE INPUT PARAMETER pcKind AS CHARACTER NO-UNDO.

CASE pcKind:
    WHEN "invoice" THEN RUN post-invoice.p.
    WHEN "credit" THEN RUN post-credit.p.
    OTHERWISE UNDO, THROW NEW Progress.Lang.AppError("unknown kind", 1).
END CASE.
