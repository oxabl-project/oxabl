/* Only a non-mandatory field that starts at ? is a source. */
FIND FIRST Shipment NO-LOCK NO-ERROR.
IF NOT AVAILABLE Shipment THEN RETURN.
IF Shipment.ShipDate < TODAY THEN MESSAGE "late".
IF Shipment.DueDate < TODAY THEN MESSAGE "mandatory".
IF Shipment.Qty < 5 THEN MESSAGE "initial 0".
IF Shipment.Plain < TODAY THEN MESSAGE "no initial".
IF Shipment.Tag < "a" THEN MESSAGE "string question mark".
IF Shipment.Slot[1] < 5 THEN MESSAGE "extent".
IF Shipment.Weight > 1 THEN MESSAGE "heavy".
IF Shipment.Memo BEGINS "a" THEN MESSAGE "memo".
IF Shipment.Flag THEN MESSAGE "flag".
MESSAGE Shipment.DateA * 2.
MESSAGE Shipment.Qty * 2.
DISPLAY Shipment.DateB.
IF Shipment.DateC = ? THEN MESSAGE "test".
/* an unqualified name */
IF DateD < TODAY THEN MESSAGE "unqualified".
