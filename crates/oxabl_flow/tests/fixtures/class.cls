USING Progress.Lang.*.
CLASS Sample:
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO
        GET:
            RETURN Name.
        END GET.
        SET(INPUT pv AS CHARACTER):
            Name = pv.
        END SET.
    CONSTRUCTOR PUBLIC Sample():
        Name = "x".
    END CONSTRUCTOR.
    METHOD PUBLIC VOID Run():
        DO ON ERROR UNDO, THROW:
            Name = "y".
        END.
    END METHOD.
    DESTRUCTOR PUBLIC Sample():
        Name = "".
    END DESTRUCTOR.
END CLASS.
