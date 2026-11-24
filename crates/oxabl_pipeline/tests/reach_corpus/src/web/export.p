/* /export.w: no check before it posts the ledger. */
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
cFile = get-value("file").
OUTPUT TO VALUE("/data/export/" + cFile).
RUN lib/ledger.p.
OUTPUT CLOSE.
