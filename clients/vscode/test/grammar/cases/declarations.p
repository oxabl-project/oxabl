// SYNTAX TEST "source.abl" "declarations: DEFINE forms, parameters, VAR, FIELD/INDEX, procedures, functions, classes, USING"

// DEFINE forms name the variable
    DEFINE VARIABLE a AS INTEGER NO-UNDO.
//  ^^^^^^ keyword.other.define.abl
//                  ^ entity.name.variable.abl
    DEFINE PRIVATE STATIC VARIABLE a AS INTEGER NO-UNDO.
//  ^^^^^^ keyword.other.define.abl
//         ^^^^^^^ storage.modifier.abl
//                 ^^^^^^ storage.modifier.abl
//                                 ^ entity.name.variable.abl
    DEFINE NEW SHARED VARIABLE sv AS CHARACTER NO-UNDO.
//                             ^^ entity.name.variable.abl
    DEFINE TEMP-TABLE tt NO-UNDO
//  ^^^^^^ keyword.other.define.abl
//                    ^^ entity.name.variable.abl
    DEFINE BUFFER b FOR tt.
//  ^^^^^^ keyword.other.define.abl
//                ^ entity.name.variable.abl
    DEFINE QUERY q FOR tt.
//               ^ entity.name.variable.abl
    DEFINE DATASET ds FOR tt.
//                 ^^ entity.name.variable.abl
    DEFINE STREAM s.
//                ^ entity.name.variable.abl
    DEFINE FRAME fr x.
//               ^^ entity.name.variable.abl
    DEFINE PUBLIC PROPERTY p AS CHARACTER GET. SET.
//                         ^ entity.name.variable.abl
    DEFINE PUBLIC EVENT ev SIGNATURE VOID (INPUT a AS INTEGER).
//                      ^^ entity.name.variable.abl
    define variable lower as integer no-undo.
//  ^^^^^^ keyword.other.define.abl
//                  ^^^^^ entity.name.variable.abl
    DEF VAR short AS INT NO-UNDO.
//  ^^^ keyword.other.define.abl
//          ^^^^^ entity.name.variable.abl

// parameters
    DEFINE INPUT PARAMETER p1 AS CHARACTER NO-UNDO.
//                         ^^ variable.parameter.abl
//         ^^^^^ storage.modifier.abl
    DEFINE OUTPUT PARAMETER p2 AS CHARACTER NO-UNDO.
//                          ^^ variable.parameter.abl
    DEFINE INPUT-OUTPUT PARAMETER p3 AS CHARACTER NO-UNDO.
//                                ^^ variable.parameter.abl
    DEF INPUT PARAM p4 LIKE customer.name.
//                  ^^ variable.parameter.abl
    DEFINE INPUT PARAMETER TABLE FOR tt.
//                         ^^^^^ keyword.other.abl
//                                   ^^ - variable.parameter.abl entity.name.variable.abl entity.name.variable.field.abl
//                                   ^^ variable.other.abl
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt.
//                                          ^^ - variable.parameter.abl entity.name.variable.abl entity.name.variable.field.abl
    FUNCTION f RETURNS INTEGER (INPUT a AS INTEGER, OUTPUT b AS CHARACTER):
//                                    ^ variable.parameter.abl
//                                                         ^ variable.parameter.abl
    METHOD PUBLIC VOID m(INPUT p AS INTEGER):
//                             ^ variable.parameter.abl
    METHOD PUBLIC VOID m(INPUT p AS INTEGER, OUTPUT q AS CHARACTER):
//                             ^ variable.parameter.abl
//                                                  ^ variable.parameter.abl
    CONSTRUCTOR PUBLIC Ctor(INPUT p AS INTEGER):
//                                ^ variable.parameter.abl
    RUN p (INPUT 1, OUTPUT x).
//                         ^ variable.other.abl
//                         ^ - variable.parameter.abl

// temp-table fields and indexes
    DEFINE TEMP-TABLE tt NO-UNDO FIELD f1 AS CHARACTER INDEX i1 IS PRIMARY UNIQUE f1.
//                               ^^^^^ keyword.other.abl
//                                     ^^ entity.name.variable.field.abl
//                                                     ^^^^^ keyword.other.abl
//                                                           ^^ entity.name.variable.field.abl
    DEFINE TEMP-TABLE tt NO-UNDO
    FIELD a AS INTEGER
//  ^^^^^ keyword.other.abl
//        ^ entity.name.variable.field.abl
    INDEX ix a.
//        ^^ entity.name.variable.field.abl

// VAR statement
    VAR INTEGER i.
//  ^^^ keyword.other.var.abl
//      ^^^^^^^ storage.type.abl
//              ^ entity.name.variable.abl
    VAR CHARACTER c = "x".
//                ^ entity.name.variable.abl
    var int64 num = 1.
//            ^^^ entity.name.variable.abl

// procedures and functions
    PROCEDURE p:
//  ^^^^^^^^^ storage.type.procedure.abl
//            ^ entity.name.function.procedure.abl
    PROCEDURE do-work PRIVATE:
//  ^^^^^^^^^ storage.type.procedure.abl
//            ^^^^^^^ entity.name.function.procedure.abl
    procedure lower-proc:
//  ^^^^^^^^^ storage.type.procedure.abl
//            ^^^^^^^^^^ entity.name.function.procedure.abl
    FUNCTION f RETURNS INTEGER (INPUT a AS INTEGER):
//  ^^^^^^^^ storage.type.function.abl
//           ^ entity.name.function.abl
//                     ^^^^^^^ storage.type.abl
    FUNCTION g RETURNS CHARACTER FORWARD (INPUT a AS INTEGER).
//           ^ entity.name.function.abl
    DELETE PROCEDURE h.
//         ^^^^^^^^^ - storage.type.procedure.abl
//                   ^ - entity.name.function.procedure.abl
    DELETE PROCEDURE h NO-ERROR.
//                   ^ - entity.name.function.procedure.abl
    x = THIS-PROCEDURE:FILE-NAME.
//      ^^^^^^^^^^^^^^ - storage.type.procedure.abl

// methods and constructors
    METHOD PUBLIC VOID m():
//  ^^^^^^ storage.type.method.abl
//                     ^ entity.name.function.method.abl
    METHOD PUBLIC STATIC CHARACTER Name(INPUT p AS INTEGER):
//  ^^^^^^ storage.type.method.abl
//                                 ^^^^ entity.name.function.method.abl
    METHOD PUBLIC a.B Make():
//                    ^^^^ entity.name.function.method.abl
    CONSTRUCTOR PUBLIC Ctor(INPUT p AS INTEGER):
//  ^^^^^^^^^^^ storage.type.method.abl
//                     ^^^^ entity.name.function.method.abl
    DESTRUCTOR PUBLIC Ctor():
//                    ^^^^ entity.name.function.method.abl

// classes, interfaces, enums
    CLASS a.B:
//  ^^^^^ storage.type.class.abl
//        ^^^ entity.name.type.class.abl
    CLASS a.B INHERITS c.D IMPLEMENTS e.F, g.H:
//        ^^^ entity.name.type.class.abl
//                     ^^^ entity.other.inherited-class.abl
//                                    ^^^ entity.other.inherited-class.abl
//                                         ^^^ entity.other.inherited-class.abl
    CLASS a.B FINAL:
//        ^^^ entity.name.type.class.abl
//            ^^^^^ storage.modifier.abl
    CLASS Simple:
//        ^^^^^^ entity.name.type.class.abl
    INTERFACE a.I:
//  ^^^^^^^^^ storage.type.interface.abl
//            ^^^ entity.name.type.interface.abl
    INTERFACE a.I INHERITS b.J:
//                         ^^^ entity.other.inherited-class.abl
    ENUM a.E:
//  ^^^^ storage.type.enum.abl
//       ^^^ entity.name.type.enum.abl
    class lower.Name inherits other.Base:
//  ^^^^^ storage.type.class.abl
//        ^^^^^^^^^^ entity.name.type.class.abl
//                            ^^^^^^^^^^ entity.other.inherited-class.abl

// USING
    USING Progress.Lang.*.
//  ^^^^^ keyword.other.using.abl
//        ^^^^^^^^^^^^^^^ entity.name.namespace.abl
    USING Progress.Lang.Object.
//        ^^^^^^^^^^^^^^^^^^^^ entity.name.namespace.abl
    using Progress.Json.ObjectModel.JsonObject FROM PROPATH.
//        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ entity.name.namespace.abl
//                                             ^^^^ keyword.other.abl
    x = using.
//      ^^^^^ - entity.name.namespace.abl

