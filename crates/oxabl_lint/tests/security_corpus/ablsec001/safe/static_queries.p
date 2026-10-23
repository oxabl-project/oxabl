DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE QUERY q FOR Customer.

FOR EACH Customer NO-LOCK WHERE Customer.Name = pcName:
    DISPLAY Customer.Name.
END.
FIND FIRST Customer NO-LOCK WHERE Customer.Name = pcName NO-ERROR.
OPEN QUERY q FOR EACH Customer NO-LOCK WHERE Customer.Name = pcName.
