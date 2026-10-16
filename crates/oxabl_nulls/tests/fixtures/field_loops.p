/* A FOR EACH loads a record on each pass; its WHERE holds of the record. */
FOR EACH Shipment NO-LOCK:
  IF Shipment.DateA < TODAY THEN MESSAGE "each".
END.
FOR EACH Shipment NO-LOCK WHERE Shipment.DateB < TODAY:
  MESSAGE Shipment.DateB - 1.
  MESSAGE Shipment.DateC - 1.
END.
FOR EACH Shipment NO-LOCK WHERE Shipment.Memo = "a" AND Shipment.DateD <> ?:
  MESSAGE Shipment.Memo + "x".
  MESSAGE Shipment.DateD + 1.
END.
FOR EACH Shipment NO-LOCK:
  IF Shipment.DateE = ? THEN NEXT.
  MESSAGE Shipment.DateE - 1.
END.
FOR EACH Shipment NO-LOCK:
END.
IF Shipment.DateF > TODAY THEN MESSAGE "after the loop".
FIND FIRST Shipment WHERE Shipment.DateG > TODAY NO-LOCK NO-ERROR.
MESSAGE Shipment.DateG - 1.
MESSAGE Shipment.DateH - 1.
FOR FIRST Shipment NO-LOCK:
  MESSAGE Shipment.DateI - 1.
END.
