// SYNTAX TEST "source.abl" "type references"

    DEFINE VARIABLE o AS CLASS a.B NO-UNDO.
//                             ^^^ entity.name.type.abl
//                       ^^^^^ keyword.other.abl
    DEFINE VARIABLE o AS a.B NO-UNDO.
//                       ^^^ entity.name.type.abl
    DEFINE VARIABLE o AS Simple NO-UNDO.
//                       ^^^^^^ entity.name.type.abl
    DEFINE VARIABLE o AS Progress.Lang.Object NO-UNDO.
//                       ^^^^^^^^^^^^^^^^^^^^ entity.name.type.abl
    DEFINE VARIABLE n AS INTEGER NO-UNDO.
//                       ^^^^^^^ storage.type.abl
//                       ^^^^^^^ - entity.name.type.abl
    DEFINE VARIABLE c AS CHARACTER EXTENT 3 NO-UNDO.
//                       ^^^^^^^^^ storage.type.abl
    DEFINE VARIABLE c AS char NO-UNDO.
//                       ^^^^ storage.type.abl
//                       ^^^^ - entity.name.type.abl
    FUNCTION f RETURNS a.B ():
//                     ^^^ entity.name.type.abl
    FUNCTION f RETURNS INTEGER ():
//                     ^^^^^^^ storage.type.abl
    METHOD PUBLIC VOID m(INPUT p AS a.B):
//                                  ^^^ entity.name.type.abl
    o = NEW a.B(1).
//          ^^^ entity.name.type.abl
    o = NEW Simple().
//          ^^^^^^ entity.name.type.abl
    CATCH e AS Progress.Lang.Error:
//             ^^^^^^^^^^^^^^^^^^^ entity.name.type.abl
    DEFINE VARIABLE h AS HANDLE NO-UNDO.
//                       ^^^^^^ storage.type.abl

// VIEW-AS is not an AS type reference
    DEFINE VARIABLE x AS INTEGER VIEW-AS FILL-IN NO-UNDO.
//                               ^^^^^^^ keyword.other.abl
//                                       ^^^^^^^ - entity.name.type.abl
    DEFINE FRAME f x VIEW-AS EDITOR.
//                           ^^^^^^ - entity.name.type.abl
//                   ^^^^^^^ keyword.other.abl
    DEFINE VARIABLE x AS INTEGER VIEW-AS COMBO-BOX LIST-ITEMS "a", "b" NO-UNDO.
//                                       ^^^^^^^^^ - entity.name.type.abl
    DISPLAY x VIEW-AS TOGGLE-BOX.
//                    ^^^^^^^^^^ - entity.name.type.abl

