/* expect: 1 */
&GLOBAL-DEFINE PW "hunter2"
DEFINE VARIABLE cPassword AS CHARACTER NO-UNDO.

cPassword = {&PW}.
