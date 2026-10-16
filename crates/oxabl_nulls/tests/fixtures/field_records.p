/* A record that is read or created may hold ? again; a write or a call that
   takes the buffer leaves its fields Unknown. */
DEFINE BUFFER other FOR Shipment.
FIND FIRST Shipment NO-LOCK NO-ERROR.
IF Shipment.DateA = ? THEN RETURN.
MESSAGE Shipment.DateA - 1.
FIND NEXT Shipment NO-LOCK NO-ERROR.
MESSAGE Shipment.DateA - 2.
IF Shipment.DateA = ? THEN RETURN.
CREATE Shipment.
MESSAGE Shipment.DateA - 3.
Shipment.DateB = TODAY.
MESSAGE Shipment.DateB - 4.
Shipment.DateC = ?.
MESSAGE Shipment.DateC - 5.
IF Shipment.DateG = ? THEN RETURN.
ASSIGN Shipment.DateG = Shipment.DateH.
MESSAGE Shipment.DateG - 9.
IF Shipment.DateD = ? THEN RETURN.
BUFFER-COPY other TO Shipment.
MESSAGE Shipment.DateD - 6.
FIND FIRST other NO-LOCK NO-ERROR.
IF other.DateE = ? THEN RETURN.
RUN fill (BUFFER other).
MESSAGE other.DateE - 7.
IF Shipment.DateF = ? THEN RETURN.
UPDATE Shipment.DateF.
MESSAGE Shipment.DateF - 8.
