// SYNTAX TEST "source.abl" "keywords: categories, abbreviations, case, and where a word is not a keyword"

// control keywords
    IF a THEN b ELSE c.
//  ^^ keyword.control.abl
//       ^^^^ keyword.control.abl
//              ^^^^ keyword.control.abl
    DO WHILE a:
//  ^^ keyword.control.abl
//     ^^^^^ keyword.control.abl
    FOR EACH t NO-LOCK:
//  ^^^ keyword.control.abl
//      ^^^^ keyword.control.abl
    REPEAT:
//  ^^^^^^ keyword.control.abl
    CASE a:
//  ^^^^ keyword.control.abl
    WHEN 1 THEN x = 1.
//  ^^^^ keyword.control.abl
    OTHERWISE x = 2.
//  ^^^^^^^^^ keyword.control.abl
    LEAVE. NEXT. RETURN. UNDO. RETRY.
//  ^^^^^ keyword.control.abl
//         ^^^^ keyword.control.abl
//               ^^^^^^ keyword.control.abl
//                       ^^^^ keyword.control.abl
//                             ^^^^^ keyword.control.abl
    CATCH e AS Progress.Lang.Error:
//  ^^^^^ keyword.control.abl
    FINALLY:
//  ^^^^^^^ keyword.control.abl
    THROW e.
//  ^^^^^ keyword.control.abl
    STOP. QUIT.
//  ^^^^ keyword.control.abl
//        ^^^^ keyword.control.abl
    END.
//  ^^^ keyword.control.abl
//     ^ punctuation.terminator.statement.abl

// word operators
    IF a AND b OR NOT c THEN.
//       ^^^ keyword.operator.expression.logical.abl
//             ^^ keyword.operator.expression.logical.abl
//                ^^^ keyword.operator.expression.logical.abl
    IF a EQ b NE c LT d LE e GT f GE g THEN.
//       ^^ keyword.operator.expression.comparison.abl
//            ^^ keyword.operator.expression.comparison.abl
//                 ^^ keyword.operator.expression.comparison.abl
//                      ^^ keyword.operator.expression.comparison.abl
//                           ^^ keyword.operator.expression.comparison.abl
//                                ^^ keyword.operator.expression.comparison.abl
    IF a BEGINS "x" OR b MATCHES "y" OR c CONTAINS d THEN.
//       ^^^^^^ keyword.operator.expression.pattern.abl
//                       ^^^^^^^ keyword.operator.expression.pattern.abl
//                                        ^^^^^^^^ keyword.operator.expression.pattern.abl
    x = a MODULO 2.
//        ^^^^^^ keyword.operator.expression.arithmetic.abl

// data types
    DEFINE VARIABLE a AS CHARACTER NO-UNDO.
//                       ^^^^^^^^^ storage.type.abl
    DEFINE VARIABLE a AS INTEGER NO-UNDO.
//                       ^^^^^^^ storage.type.abl
    DEFINE VARIABLE a AS INT64 NO-UNDO.
//                       ^^^^^ storage.type.abl
    DEFINE VARIABLE a AS HANDLE NO-UNDO.
//                       ^^^^^^ storage.type.abl
    DEFINE VARIABLE a AS DECIMAL NO-UNDO.
//                       ^^^^^^^ storage.type.abl
    DEFINE VARIABLE a AS LOGICAL NO-UNDO.
//                       ^^^^^^^ storage.type.abl
    DEFINE VARIABLE a AS DATE NO-UNDO.
//                       ^^^^ storage.type.abl
    METHOD PUBLIC VOID m().
//                ^^^^ storage.type.abl

// modifiers
    METHOD PUBLIC STATIC FINAL OVERRIDE VOID m().
//         ^^^^^^ storage.modifier.abl
//                ^^^^^^ storage.modifier.abl
//                       ^^^^^ storage.modifier.abl
//                             ^^^^^^^^ storage.modifier.abl
    METHOD PRIVATE ABSTRACT VOID m().
//         ^^^^^^^ storage.modifier.abl
//                 ^^^^^^^^ storage.modifier.abl
    METHOD PROTECTED VOID m().
//         ^^^^^^^^^ storage.modifier.abl
    DEFINE VARIABLE a AS INTEGER NO-UNDO.
//                               ^^^^^^^ storage.modifier.abl
    RUN p (INPUT 1, OUTPUT x, INPUT-OUTPUT y).
//         ^^^^^ storage.modifier.abl
//                  ^^^^^^ storage.modifier.abl
//                            ^^^^^^^^^^^^ storage.modifier.abl
    DEFINE NEW GLOBAL SHARED VARIABLE a AS INTEGER NO-UNDO.
//             ^^^^^^ storage.modifier.abl
//                    ^^^^^^ storage.modifier.abl
    CLASS a.B SERIALIZABLE:
//            ^^^^^^^^^^^^ storage.modifier.abl

// system handles and references
    x = SESSION:NAME.
//      ^^^^^^^ variable.language.abl
    x = THIS-PROCEDURE:FILE-NAME.
//      ^^^^^^^^^^^^^^ variable.language.abl
    x = ERROR-STATUS:ERROR.
//      ^^^^^^^^^^^^ variable.language.abl
    x = SELF:NAME.
//      ^^^^ variable.language.abl
    x = THIS-OBJECT:Name.
//      ^^^^^^^^^^^ variable.language.abl
    SUPER:Run().
//  ^^^^^ variable.language.abl

// other keywords
    DISPLAY x.
//  ^^^^^^^ keyword.other.abl
    ASSIGN x = 1.
//  ^^^^^^ keyword.other.abl
    FIND FIRST t NO-LOCK.
//               ^^^^^^^ keyword.other.abl
    FIND FIRST t EXCLUSIVE-LOCK.
//               ^^^^^^^^^^^^^^ keyword.other.abl
    DEFINE FRAME f x VIEW-AS FILL-IN.
//                   ^^^^^^^ keyword.other.abl
    FIND FIRST t NO LOCK.
//               ^^^^^^^ keyword.other.abl
//               ^^ - constant.language.boolean.abl
    FIND FIRST t SHARE LOCK.
//               ^^^^^^^^^^ keyword.other.abl
    FIND FIRST t EXCLUSIVE LOCK.
//               ^^^^^^^^^^^^^^ keyword.other.abl

// abbreviations and case
    DEF VAR a AS INT.
//  ^^^ keyword.other.define.abl
//               ^^^ storage.type.abl
    DEFI VAR a AS INT.
//  ^^^^ keyword.other.define.abl
    DEFIN VAR a AS INT.
//  ^^^^^ keyword.other.define.abl
    DEFINE VAR a AS INT.
//  ^^^^^^ keyword.other.define.abl
    DEFINE VARIABLE a AS INTEGER.
//  ^^^^^^ keyword.other.define.abl
//         ^^^^^^^^ keyword.other.abl
    DEFINE VAR a AS CHAR.
//         ^^^ keyword.other.abl
//                  ^^^^ storage.type.abl
    DEFINE VAR a AS CHARACTER.
//                  ^^^^^^^^^ storage.type.abl
    DEFINE VAR a AS LOG.
//                  ^^^ storage.type.abl
    DEFINE VAR a AS LOGICAL.
//                  ^^^^^^^ storage.type.abl
    dEfInE vAr a aS iNtEgEr.
//  ^^^^^^ keyword.other.define.abl
//         ^^^ keyword.other.abl
//                  ^^^^^^^ storage.type.abl
    define variable a as integer.
//  ^^^^^^ keyword.other.define.abl
//                       ^^^^^^^ storage.type.abl
    If a ThEn b eLsE c.
//  ^^ keyword.control.abl
//       ^^^^ keyword.control.abl
//              ^^^^ keyword.control.abl
    fOr EaCh t nO-lOcK:
//  ^^^ keyword.control.abl
//             ^^^^^^^ keyword.other.abl
    DE x.
//  ^^ variable.other.abl
//  ^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = DE.
//      ^^ variable.other.abl
//      ^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl

// a keyword is not a keyword inside a longer name
    x = my-define.
//      ^^^^^^^^^ variable.other.abl
//      ^^^^^^^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = define-this.
//      ^^^^^^^^^^^ variable.other.abl
//      ^^^^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = end-of-file.
//      ^^^^^^^^^^^ variable.other.abl
//      ^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = iffy.
//      ^^^^ variable.other.abl
//      ^^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = foreach.
//      ^^^^^^^ variable.other.abl
//      ^^^^^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = a_if.
//        ^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = a#if.
//        ^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = if$.
//      ^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl

// a keyword is not a keyword in a qualified name
    x = a.end.
//        ^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
//       ^ punctuation.accessor.abl
    x = a.if.
//        ^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = customer.display.
//               ^^^^^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = if.a.
//      ^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = user.name.
//      ^^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
//      ^^^^ variable.other.abl
//          ^ punctuation.accessor.abl
//           ^^^^ variable.other.abl
    x = end.y.
//      ^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = session.name.
//      ^^^^^^^ - variable.language.abl
    x = a.b.c.
//       ^ punctuation.accessor.abl
//         ^ punctuation.accessor.abl
//           ^ punctuation.terminator.statement.abl
    END.
//  ^^^ keyword.control.abl
    x = 1. END.
//         ^^^ keyword.control.abl

// words only meaningful as attributes are plain identifiers
    x = name.
//      ^^^^ variable.other.abl
//      ^^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = key + line + y.
//      ^^^ variable.other.abl
//      ^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
//            ^^^^ variable.other.abl
//            ^^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = x + y.
//      ^ variable.other.abl
//  ^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = h:NAME.
//        ^^^^ variable.other.property.abl
    x = h:KEY + h:X + h:LINE.
//        ^^^ variable.other.property.abl
//                ^ variable.other.property.abl
//                      ^^^^ variable.other.property.abl

