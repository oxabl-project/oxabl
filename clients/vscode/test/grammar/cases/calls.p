// SYNTAX TEST "source.abl" "calls, member access and RUN targets"

// built-in functions
    x = SUBSTRING(a, 1, 2).
//      ^^^^^^^^^ support.function.abl
    x = SUBSTR (a, 1, 2).
//      ^^^^^^ support.function.abl
    IF CAN-FIND(FIRST t) THEN.
//     ^^^^^^^^ support.function.abl
    x = substring(a, 1).
//      ^^^^^^^^^ support.function.abl
    x = LENGTH(a) + TRIM(b).
//      ^^^^^^ support.function.abl
//                  ^^^^ support.function.abl
    x = substring.
//      ^^^^^^^^^ - support.function.abl
    x = SUBSTRING.
//      ^^^^^^^^^ - support.function.abl

// other calls
    x = myFunc(a).
//      ^^^^^^ entity.name.function.abl
    myFunc(a).
//  ^^^^^^ entity.name.function.abl
    x = my-func(a, b).
//      ^^^^^^^ entity.name.function.abl
    x = myFunc (a).
//      ^^^^^^ variable.other.abl
//      ^^^^^^ - entity.name.function.abl
    x = myFunc.
//      ^^^^^^ variable.other.abl
//      ^^^^^^ - entity.name.function.abl
    x = (a + b).
//      ^ punctuation.section.parens.begin.abl
//      ^ - entity.name.function.abl

// member access
    x = h:NAME.
//       ^ punctuation.accessor.abl
//        ^^^^ variable.other.property.abl
    x = h:QUERY-OPEN().
//        ^^^^^^^^^^ entity.name.function.member.abl
    x = b::f.
//       ^^ punctuation.accessor.abl
//         ^ variable.other.property.abl
    x = obj:Method(1).
//         ^ punctuation.accessor.abl
//          ^^^^^^ entity.name.function.member.abl
    x = obj:Prop.
//          ^^^^ variable.other.property.abl
    x = a:b:c.
//       ^ punctuation.accessor.abl
//        ^ variable.other.property.abl
//         ^ punctuation.accessor.abl
//          ^ variable.other.property.abl
    x = h:KEY + h:LINE + h:X.
//        ^^^ variable.other.property.abl
//                ^^^^ variable.other.property.abl
//                         ^ variable.other.property.abl
    h:VISIBLE = TRUE.
//    ^^^^^^^ variable.other.property.abl
    x = obj:Method(1):Other.
//          ^^^^^^ entity.name.function.member.abl
//                    ^^^^^ variable.other.property.abl
    x = SESSION:TEMP-DIRECTORY.
//      ^^^^^^^ variable.language.abl
//              ^^^^^^^^^^^^^^ variable.other.property.abl
    x = THIS-OBJECT:Name.
//                  ^^^^ variable.other.property.abl

// identifiers and qualified names
    x = cName.
//      ^^^^^ variable.other.abl
    x = customer.name.
//      ^^^^^^^^ variable.other.abl
//              ^ punctuation.accessor.abl
//               ^^^^ variable.other.abl
    x = my-var-name.
//      ^^^^^^^^^^^ variable.other.abl
    x = _under.
//      ^^^^^^ variable.other.abl
    x = a#b$c%d&e.
//      ^^^^^^^^^ variable.other.abl

// RUN targets
    RUN src/a.p.
//  ^^^ keyword.other.abl
//      ^^^^^^^ entity.name.function.abl
    RUN local-proc.
//      ^^^^^^^^^^ entity.name.function.abl
    RUN local-proc (INPUT 1).
//      ^^^^^^^^^^ entity.name.function.abl
    RUN a.p PERSISTENT SET h.
//      ^^^ entity.name.function.abl
    run lower-proc in h.
//  ^^^ keyword.other.abl
//      ^^^^^^^^^^ entity.name.function.abl
    RUN VALUE(x).
//      ^^^^^ support.function.abl
//      ^^^^^ - entity.name.function.abl
    RUN VALUE(x) (INPUT 1).
//      ^^^^^ - entity.name.function.abl
    RUN VALUE (x).
//      ^^^^^ - entity.name.function.abl
    RUN SUPER.
//      ^^^^^ - entity.name.function.abl
    RUN SUPER (INPUT 1).
//      ^^^^^ - entity.name.function.abl
    x = RUN.
//      ^^^ - entity.name.function.abl

