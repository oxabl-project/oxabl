&IF DEFINED(new-api) &THEN
CLASS app.v2:
&ELSE
CLASS app.v1:
&ENDIF
  {members.i}
END CLASS.
