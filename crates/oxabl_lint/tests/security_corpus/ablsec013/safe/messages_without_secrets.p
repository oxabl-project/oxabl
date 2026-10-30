DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.

MESSAGE "Password changed".
MESSAGE cUser.
MESSAGE cPasswordPrompt.
LOG-MANAGER:WRITE-MESSAGE("login ok for " + cUser).
