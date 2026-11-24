/* expect: 3 */
DEFINE VARIABLE cPath AS CHARACTER NO-UNDO.

hDoc:LOAD("file", cPath, FALSE).
hDoc:LOAD("longchar", lcXml, FALSE).
hDoc:LOAD("memptr", mXml, FALSE).
