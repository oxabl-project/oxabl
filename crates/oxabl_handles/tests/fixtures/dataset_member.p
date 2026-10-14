DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.
DEFINE VARIABLE hTable AS HANDLE NO-UNDO.
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

/* The dataset is taken as the owner of the table's buffer. */
CREATE DATASET hDataset.
CREATE TEMP-TABLE hTable.
hTable:TEMP-TABLE-PREPARE("work").
hDataset:ADD-BUFFER(hTable:DEFAULT-BUFFER-HANDLE).
hTable = ?.
DELETE OBJECT hDataset.

/* A query does not own its buffers. */
CREATE QUERY hQuery.
CREATE BUFFER hBuffer FOR TABLE "customer".
hQuery:ADD-BUFFER(hBuffer).
DELETE OBJECT hQuery.
