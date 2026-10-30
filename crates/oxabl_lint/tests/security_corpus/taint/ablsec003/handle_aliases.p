/* Several handles may point to one object. A write through one of them
   changes what the others read, so a document that a node was put into,
   and a handle that was copied, hold Unknown. */
DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
DEFINE VARIABLE hRoot AS HANDLE NO-UNDO.
DEFINE VARIABLE hText AS HANDLE NO-UNDO.
DEFINE VARIABLE lcValue AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcOut AS LONGCHAR NO-UNDO.
CREATE X-DOCUMENT hDoc.
CREATE X-NODEREF hRoot.
CREATE X-NODEREF hText.
hDoc:CREATE-NODE(hRoot, "r", "ELEMENT").
hDoc:APPEND-CHILD(hRoot).
hDoc:CREATE-NODE(hText, ?, "TEXT").
hText:NODE-VALUE = get-value("cmd").
hRoot:APPEND-CHILD(hText).
hDoc:SAVE("longchar", lcOut).
OS-COMMAND VALUE(STRING(lcOut)). /* taint: real */

DEFINE VARIABLE hDoc2 AS HANDLE NO-UNDO.
DEFINE VARIABLE hRoot2 AS HANDLE NO-UNDO.
DEFINE VARIABLE hText2 AS HANDLE NO-UNDO.
CREATE X-DOCUMENT hDoc2.
CREATE X-NODEREF hRoot2.
CREATE X-NODEREF hText2.
lcValue = get-value("cmd").
hDoc2:CREATE-NODE(hRoot2, "r", "ELEMENT").
hDoc2:APPEND-CHILD(hRoot2).
hDoc2:CREATE-NODE(hText2, ?, "TEXT").
hText2:LONGCHAR-TO-NODE-VALUE(lcValue).
hRoot2:APPEND-CHILD(hText2).
lcOut = "".
hDoc2:SAVE("longchar", lcOut).
OS-COMMAND VALUE(STRING(lcOut)). /* taint: real */

DEFINE VARIABLE hDoc3 AS HANDLE NO-UNDO.
DEFINE VARIABLE hRoot3 AS HANDLE NO-UNDO.
CREATE X-DOCUMENT hDoc3.
CREATE X-NODEREF hRoot3.
hDoc3:CREATE-NODE(hRoot3, "r", "ELEMENT").
hDoc3:APPEND-CHILD(hRoot3).
hRoot3:SET-ATTRIBUTE("a", get-value("cmd")).
lcOut = "".
hDoc3:SAVE("longchar", lcOut).
OS-COMMAND VALUE(STRING(lcOut)). /* taint: real */

/* A copy of a handle: the object behind both. */
DEFINE VARIABLE h1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h2 AS HANDLE NO-UNDO.
CREATE X-NODEREF h1.
h2 = h1.
h1:PRIVATE-DATA = get-value("cmd").
OS-COMMAND VALUE(h2:PRIVATE-DATA). /* taint: real */

/* A method that puts its argument into its receiver. */
DEFINE VARIABLE hList AS HANDLE NO-UNDO.
CREATE COMBO-BOX hList.
hList:ADD-LAST(get-value("cmd")).
OS-COMMAND VALUE(hList:LIST-ITEMS). /* taint: real */

/* A handle of its own, never copied, with what the program set on it. */
DEFINE VARIABLE hOwn AS HANDLE NO-UNDO.
CREATE COMBO-BOX hOwn.
hOwn:ADD-LAST("ls").
OS-COMMAND VALUE(hOwn:LIST-ITEMS). /* taint: safe */
