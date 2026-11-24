/* expect: 2 */
DEFINE VARIABLE lcJson AS LONGCHAR NO-UNDO.

oObj = NEW Progress.IO.BinarySerializer():Deserialize(mData).
oObj = Progress.IO.JsonSerializer:Deserialize(lcJson).
