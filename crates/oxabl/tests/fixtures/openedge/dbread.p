DEFINE VARIABLE found AS INTEGER NO-UNDO.

FOR EACH _file NO-LOCK WHERE _file._file-name BEGINS "_f":
    found = found + 1.
END.
PUT UNFORMATTED "tables " found SKIP.
