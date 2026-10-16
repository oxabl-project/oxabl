/* no-schema */
FIND FIRST Shipment NO-LOCK NO-ERROR.
IF Shipment.ShipDate < TODAY THEN MESSAGE "unknown".
