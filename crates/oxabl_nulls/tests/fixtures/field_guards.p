/* Guards remove the source, as for a local. */
DEFINE VARIABLE x AS DATE NO-UNDO.
FIND FIRST Shipment NO-LOCK NO-ERROR.
IF NOT AVAILABLE Shipment THEN RETURN.
IF Shipment.DateA <> ? AND Shipment.DateA < TODAY THEN MESSAGE "and".
IF Shipment.DateB NE ? THEN MESSAGE Shipment.DateB - 1.
IF Shipment.DateC = ? OR Shipment.DateC < TODAY THEN MESSAGE "or".
IF Shipment.DateD = ? THEN RETURN.
IF Shipment.DateD < TODAY THEN MESSAGE "returned".
IF Shipment.DateE = ? THEN MESSAGE Shipment.DateE - 1.
IF Shipment.DateF = ? THEN Shipment.DateF = TODAY.
IF Shipment.DateF < TODAY THEN MESSAGE "set or known".
IF Shipment.DateG > TODAY OR Shipment.DateG = ? THEN MESSAGE "ordering proves".
IF Shipment.DateH > TODAY THEN MESSAGE "then".
ELSE MESSAGE Shipment.DateH - 1.
IF NOT (Shipment.DateI = ?) THEN MESSAGE Shipment.DateI - 1.
IF Shipment.DateJ = Shipment.DateD THEN MESSAGE Shipment.DateJ - 1.
IF Shipment.DateK = x THEN MESSAGE Shipment.DateK - 1.
/* A WHEN guards one ASSIGN pair or one DISPLAY item. */
ASSIGN x = Shipment.DateL - 1 WHEN Shipment.DateL <> ?.
DISPLAY Shipment.Weight * 2 WHEN Shipment.Weight <> ?.
ASSIGN Shipment.Memo = "a" + Shipment.Memo WHEN Shipment.Memo <> ?.
MESSAGE "a" + Shipment.Memo.
