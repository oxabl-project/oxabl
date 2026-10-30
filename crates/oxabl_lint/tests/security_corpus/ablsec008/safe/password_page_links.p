/* Links and display values beside a password field, held in password-named variables. */
DEFINE VARIABLE cForgotPasswordHref AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHideChngPwd AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResetPassword AS CHARACTER NO-UNDO.
cForgotPasswordHref = "javascript:showReset();".
cHideChngPwd = "none".
cResetPassword = "https://example.com/account/reset".
