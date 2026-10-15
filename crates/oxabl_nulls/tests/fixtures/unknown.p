/* K5, K6, section 9.5: what the model forgets. */
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE h AS HANDLE NO-UNDO.
DEFINE TEMP-TABLE tt NO-UNDO FIELD k AS INTEGER.

FIND FIRST Customer NO-LOCK NO-ERROR.
RUN fill-customer (BUFFER Customer).            /* passed to a call */
c = Customer.Name.

FIND FIRST Customer NO-LOCK NO-ERROR.
h = BUFFER Customer:HANDLE.
h:FIND-FIRST("", NO-LOCK).                      /* a dynamic method */
c = Customer.Name.

FIND FIRST Customer NO-LOCK NO-ERROR.
RUN other-routine.                              /* Customer is named elsewhere */
c = Customer.Name.

FIND FIRST tt NO-ERROR.
RUN other-routine.                              /* tt is not */
c = STRING(tt.k).                               /* missing */

PROCEDURE fill-customer:
    DEFINE PARAMETER BUFFER b FOR Customer.
    FIND FIRST b NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE other-routine:
    FIND FIRST Customer NO-LOCK NO-ERROR.
END PROCEDURE.
