/* A temp-table field is not a field of the schema. */
DEFINE TEMP-TABLE tt NO-UNDO FIELD d AS DATE INITIAL ?.
FIND FIRST tt NO-ERROR.
IF tt.d < TODAY THEN MESSAGE "temp-table".
FIND FIRST Nothing NO-LOCK NO-ERROR.
IF Nothing.d < TODAY THEN MESSAGE "unknown table".
