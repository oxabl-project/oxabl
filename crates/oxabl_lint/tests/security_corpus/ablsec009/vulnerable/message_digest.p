/* expect: 2 */
DEFINE VARIABLE cHash AS CHARACTER NO-UNDO.

cHash = MESSAGE-DIGEST("MD5", cPassword).
cHash = MESSAGE-DIGEST("SHA-1", cApiKey).
