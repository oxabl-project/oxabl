/* expect: 4 */
DEFINE VARIABLE cOut AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDbSecret AS CHARACTER NO-UNDO INITIAL "oech1::6a7b8c9d0e1f".

cOut = ENCRYPT(cData, "k3y-k3y-k3y").
cOut = DECRYPT(cData, "k3y-k3y-k3y", rIv).
rKey = GENERATE-PBE-KEY("passphrase", rSalt).
