/* expect: 3 */
DEFINE INPUT PARAMETER pmData AS MEMPTR NO-UNDO.

oObj = oSer:Deserialize(pmData).
oSer:Deserialize(cText).
oObj = oSer:Deserialize(pmData, "x" + cType).
