/* expect: 4 */
DEFINE VARIABLE cPassword AS CHARACTER NO-UNDO.
DEFINE VARIABLE cApiSecret AS CHARACTER NO-UNDO INITIAL "abc123".

cPassword = "hunter2".
ASSIGN cDbPwd = "x1y2".
oCfg:Password = "hunter2".
