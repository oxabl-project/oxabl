/* Account.cls - original sample class for the grammar snapshot tests. */
USING Progress.Lang.*.
USING bank.model.Owner.

CLASS bank.model.Account INHERITS bank.model.Base IMPLEMENTS bank.model.IAuditable, bank.model.IPrintable FINAL:

    DEFINE PRIVATE VARIABLE dBalance AS DECIMAL NO-UNDO.
    DEFINE PRIVATE STATIC VARIABLE iNext AS INTEGER INITIAL 1 NO-UNDO.

    DEFINE PUBLIC PROPERTY Owner AS bank.model.Owner NO-UNDO
        GET.
        PRIVATE SET.

    DEFINE PUBLIC PROPERTY Number AS INTEGER NO-UNDO GET. SET.

    DEFINE PUBLIC EVENT Overdrawn SIGNATURE VOID (INPUT dAmount AS DECIMAL).

    CONSTRUCTOR PUBLIC Account(INPUT poOwner AS bank.model.Owner):
        SUPER().
        THIS-OBJECT:Owner = poOwner.
        Number = iNext.
        iNext = iNext + 1.
    END CONSTRUCTOR.

    METHOD PUBLIC VOID Deposit(INPUT dAmount AS DECIMAL):
        IF dAmount <= 0 THEN
            UNDO, THROW NEW AppError("Amount must be positive", 1).
        dBalance += dAmount.
    END METHOD.

    METHOD PUBLIC LOGICAL Withdraw(INPUT dAmount AS DECIMAL):
        IF dAmount > dBalance THEN DO:
            Overdrawn:Publish(dAmount - dBalance).
            RETURN FALSE.
        END.
        dBalance -= dAmount.
        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC STATIC bank.model.Account Open(INPUT poOwner AS CLASS bank.model.Owner):
        DEFINE VARIABLE oAccount AS bank.model.Account NO-UNDO.
        oAccount = NEW bank.model.Account(poOwner).
        RETURN oAccount.
    END METHOD.

    METHOD OVERRIDE PUBLIC CHARACTER ToString():
        RETURN SUBSTITUTE("Account &1 of &2: &3", Number, Owner:Name, STRING(dBalance)).
    END METHOD.

    DESTRUCTOR PUBLIC Account():
        dBalance = 0.
    END DESTRUCTOR.

END CLASS.
