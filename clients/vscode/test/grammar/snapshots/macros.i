/* macros.i - original sample include for the grammar snapshot tests.
   Arguments: {1} table name, {2} field name, {&flags} option list. */

&IF DEFINED(MACROS-I) = 0 &THEN
&GLOBAL-DEFINE MACROS-I YES

&SCOPED-DEFINE SEP ", "
&SCOP LIMIT 10
&GLOB LONG-VALUE first part ~
    second part ~
    third part
&UNDEFINE LIMIT

&MESSAGE Building macros.i for {&opsys}

&IF "{&flags}" = "debug" &THEN
    &SCOPED-DEFINE LOG-LEVEL 3
&ELSEIF "{&flags}" = "quiet" &THEN
    &SCOPED-DEFINE LOG-LEVEL 0
&ELSE
    &SCOPED-DEFINE LOG-LEVEL 1
&ENDIF

DEFINE VARIABLE cList AS CHARACTER NO-UNDO.

FOR EACH {1} NO-LOCK:
    cList = cList + {&SEP} + STRING({1}.{2}).
    IF LENGTH(cList) > {&LOG-LEVEL} THEN LEAVE.
END.

{common/trace.i &level={&LOG-LEVEL} &text="loaded {1}" extra}
{ common/footer.i }
{*}
{&*}

&ENDIF
