/* expect: 3 */
DEFINE INPUT PARAMETER cPassword AS CHARACTER NO-UNDO.

MESSAGE "login with " + cPassword.
LOG-MANAGER:WRITE-MESSAGE("pw=" + cPassword).
MESSAGE SUBSTITUTE("token &1", cApiSecret).
