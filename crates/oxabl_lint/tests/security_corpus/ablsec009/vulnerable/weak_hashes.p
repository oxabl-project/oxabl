/* expect: 3 */
DEFINE INPUT PARAMETER pcPassword AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHash AS CHARACTER NO-UNDO.

cHash = ENCODE(pcPassword).
cHash = MD5-DIGEST(pcPassword + cSalt).
cHash = SHA1-DIGEST(cResetToken).
