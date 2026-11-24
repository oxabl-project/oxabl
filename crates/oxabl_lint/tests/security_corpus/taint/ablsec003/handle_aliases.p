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

/* The same copy in an ASSIGN statement. */
DEFINE VARIABLE hAssign1 AS HANDLE NO-UNDO.
DEFINE VARIABLE hAssign2 AS HANDLE NO-UNDO.
CREATE BUTTON hAssign1.
ASSIGN hAssign2 = hAssign1
       hAssign1:PRIVATE-DATA = get-value("cmd").
OS-COMMAND VALUE(hAssign2:PRIVATE-DATA). /* taint: real */

/* A WIDGET-HANDLE is a handle: its copy, and the nodes a document loaded
   from a file gives. */
DEFINE VARIABLE hWidget1 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hWidget2 AS WIDGET-HANDLE NO-UNDO.
CREATE BUTTON hWidget1.
hWidget2 = hWidget1.
hWidget1:PRIVATE-DATA = get-value("cmd").
OS-COMMAND VALUE(hWidget2:PRIVATE-DATA). /* taint: real */
DEFINE VARIABLE hXmlDoc AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hXmlRoot AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hXmlText AS WIDGET-HANDLE NO-UNDO.
CREATE X-DOCUMENT hXmlDoc.
CREATE X-NODEREF hXmlRoot.
CREATE X-NODEREF hXmlText.
hXmlDoc:LOAD("file", "in.xml", FALSE).
hXmlDoc:GET-DOCUMENT-ELEMENT(hXmlRoot).
hXmlRoot:GET-CHILD(hXmlText, 1).
OS-COMMAND VALUE(hXmlText:NODE-VALUE). /* taint: real */

/* Other ways to a second handle of an object: a handle-valued attribute,
   a field of a temp-table, the buffer of a table and its table, a frame. */
DEFINE VARIABLE hAttr1 AS HANDLE NO-UNDO.
DEFINE VARIABLE hAttr2 AS HANDLE NO-UNDO.
CREATE BUTTON hAttr1.
hAttr2 = hAttr1:HANDLE.
hAttr1:PRIVATE-DATA = get-value("cmd").
OS-COMMAND VALUE(hAttr2:PRIVATE-DATA). /* taint: real */
DEFINE TEMP-TABLE ttHandle NO-UNDO FIELD hf AS HANDLE.
DEFINE VARIABLE hField1 AS HANDLE NO-UNDO.
DEFINE VARIABLE hField2 AS HANDLE NO-UNDO.
CREATE BUTTON hField1.
CREATE ttHandle.
ttHandle.hf = hField1.
hField2 = ttHandle.hf.
hField1:PRIVATE-DATA = get-value("cmd").
OS-COMMAND VALUE(hField2:PRIVATE-DATA). /* taint: real */
DEFINE VARIABLE hTable AS HANDLE NO-UNDO.
DEFINE VARIABLE hTableBuf AS HANDLE NO-UNDO.
DEFINE VARIABLE hTableAgain AS HANDLE NO-UNDO.
CREATE TEMP-TABLE hTable.
hTable:ADD-NEW-FIELD("f", "character").
hTable:TEMP-TABLE-PREPARE("t").
hTableBuf = hTable:DEFAULT-BUFFER-HANDLE.
hTable:PRIVATE-DATA = get-value("cmd").
hTableAgain = hTableBuf:TABLE-HANDLE.
OS-COMMAND VALUE(hTableAgain:PRIVATE-DATA). /* taint: real */
DEFINE FRAME fAlias WITH SIZE 10 BY 10.
DEFINE VARIABLE hFrame AS HANDLE NO-UNDO.
DEFINE VARIABLE hFrameAgain AS HANDLE NO-UNDO.
hFrame = FRAME fAlias:HANDLE.
hFrameAgain = hFrame:HANDLE.
hFrame:PRIVATE-DATA = get-value("cmd").
OS-COMMAND VALUE(hFrameAgain:PRIVATE-DATA). /* taint: real */

/* A handle passed to a routine of the file, which sets its object. */
DEFINE VARIABLE hPassed AS HANDLE NO-UNDO.
DEFINE VARIABLE hPassedF AS HANDLE NO-UNDO.
PROCEDURE set-data:
    DEFINE INPUT PARAMETER phTarget AS HANDLE NO-UNDO.
    phTarget:PRIVATE-DATA = get-value("cmd").
END PROCEDURE.
FUNCTION set-data-f RETURNS LOGICAL (INPUT phTarget AS HANDLE):
    phTarget:PRIVATE-DATA = get-value("cmd").
    RETURN TRUE.
END FUNCTION.
CREATE BUTTON hPassed.
RUN set-data (hPassed).
OS-COMMAND VALUE(hPassed:PRIVATE-DATA). /* taint: real */
CREATE BUTTON hPassedF.
set-data-f(hPassedF).
OS-COMMAND VALUE(hPassedF:PRIVATE-DATA). /* taint: real */

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

/* A WIDGET-HANDLE of its own reads what the program set on it, as a HANDLE
   does: it is no object of a class. */
DEFINE VARIABLE hOwnWidget AS WIDGET-HANDLE NO-UNDO.
CREATE COMBO-BOX hOwnWidget.
hOwnWidget:ADD-LAST("ls").
OS-COMMAND VALUE(hOwnWidget:LIST-ITEMS). /* taint: safe */
