DEFINE VARIABLE a AS INTEGER INITIAL ? NO-UNDO.
DEFINE VARIABLE b AS INTEGER INITIAL ? NO-UNDO.
DEFINE INPUT PARAMETER choice AS LOGICAL NO-UNDO.
REPEAT:
  IF a = ? AND b = ? THEN ASSIGN a = unseen() b = unseen().
  ELSE IF b > a THEN MESSAGE "known source is gone".
  IF choice THEN LEAVE.
END.
a = ?.
b = a.
IF a <> ? THEN MESSAGE b + 1.
a = unseen().
IF a <> ? THEN MESSAGE b + 1.
