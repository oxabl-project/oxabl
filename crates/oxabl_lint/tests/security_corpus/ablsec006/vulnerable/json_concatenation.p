/* expect: 1 */
DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.

oWriter:Write('~{"name":"' + pcName + '"}').
