DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO.
DEFINE VARIABLE x AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER b AS LOGICAL NO-UNDO.
IF b THEN n = 1.
MESSAGE n + 1.
IF n = ? THEN RETURN.
MESSAGE n + 1.
ASSIGN n = ? x = n + 1 n = 2.
MESSAGE n + 1.
MESSAGE x + 1.
n = INTEGER(?).
MESSAGE n + 1.
n = INTEGER("1").
MESSAGE n + 1.
n = IF ? THEN ? ELSE 1.
MESSAGE n + 1.
n = IF b THEN ? ELSE 1.
MESSAGE n + 1.
b = (x > 1).
IF b THEN MESSAGE "ordering result".
b = NOT ?.
IF b THEN MESSAGE "logical result".
n = ?.
IF n > 1 OR n = ? THEN MESSAGE "handled".
IF n > 1 AND n <> ? THEN MESSAGE "known".
