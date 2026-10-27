/* expect: 3 */
/* Real secrets that happen to hold one markup or template character. */
DEFINE VARIABLE cPassword AS CHARACTER NO-UNDO.
cPassword = "a<3b!".
cPassword = "x&y1".
cPassword = "password".
