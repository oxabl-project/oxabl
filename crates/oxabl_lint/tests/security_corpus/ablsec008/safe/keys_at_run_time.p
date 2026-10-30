DEFINE VARIABLE cKey AS CHARACTER NO-UNDO.

cOut = ENCRYPT(cData, cKey).
cOut = DECRYPT(cData, cKey, rIv).
rKey = GENERATE-PBE-KEY(cPass, rSalt).
