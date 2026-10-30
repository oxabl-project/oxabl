/* A password-named variable that holds the markup of a password field, not a password. */
DEFINE VARIABLE vPassword AS CHARACTER NO-UNDO INITIAL '<input type="password" name="pw">'.
DEFINE VARIABLE cPwdRow AS CHARACTER NO-UNDO.
cPwdRow = "<tr><td>" + vPassword + "</td></tr>".
vPassword = '<label for="pw">&lt;required&gt;</label>'.
