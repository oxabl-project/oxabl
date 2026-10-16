DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO.
DEFINE VARIABLE c AS CHARACTER INITIAL ? NO-UNDO.
DEFINE VARIABLE x AS INTEGER NO-UNDO.
DEFINE VARIABLE b AS LOGICAL INITIAL ? NO-UNDO.
MESSAGE -n n - 1 n * 2 n / 2 n MODULO 2.
IF c BEGINS "a" THEN MESSAGE "begins".
IF c MATCHES "a*" THEN MESSAGE "matches".
x = IF n <> ? THEN n ELSE 0.
MESSAGE x + 1.
x = IF b THEN ? ELSE 0.
MESSAGE x + 1.
x = LENGTH(c).
MESSAGE x + 1.
x = INTEGER(?).
MESSAGE x + 1.
x = 1 / 0.
MESSAGE x + 1.
c = STRING(n).
MESSAGE c + "text".
b = VALID-HANDLE(?).
IF b THEN MESSAGE "valid".
b = VALID-OBJECT(?).
IF b THEN MESSAGE "valid".
b = n > 0.
IF b THEN MESSAGE "ordering is low confidence".
b = NOT ?.
IF b THEN MESSAGE "logic is low confidence".
b = ? AND YES.
IF b THEN MESSAGE "logic is low confidence".
b = ? OR NO.
IF b THEN MESSAGE "logic is low confidence".
x = unseen().
MESSAGE x + 1.
x = 1 / 0.0.
MESSAGE x + 1.
