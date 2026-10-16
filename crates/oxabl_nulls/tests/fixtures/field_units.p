/* A shared buffer may be changed by a call; a unit has its own facts. */
DEFINE SHARED BUFFER Shipment FOR Shipment.
PROCEDURE one:
  IF Shipment.DateA = ? THEN RETURN.
  MESSAGE Shipment.DateA - 1.
  RUN two.
  MESSAGE Shipment.DateA - 2.
END PROCEDURE.
PROCEDURE two:
  MESSAGE Shipment.DateB - 1.
END PROCEDURE.
FUNCTION f RETURNS INTEGER ():
  DEFINE BUFFER b FOR Shipment.
  FIND FIRST b NO-LOCK NO-ERROR.
  IF b.DateC = ? THEN RETURN 0.
  MESSAGE b.DateC - 1.
  MESSAGE unseen() + b.DateC.
  IF b.DateD = ? THEN RETURN 0.
  MESSAGE unseen() + b.DateD.
  RETURN 1.
END FUNCTION.
