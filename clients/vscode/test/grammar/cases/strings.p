// SYNTAX TEST "source.abl" "strings: quotes, escapes, attributes, references"

    x = "plain text".
//      ^^^^^^^^^^^^ string.quoted.double.abl
    x = 'plain text'.
//      ^^^^^^^^^^^^ string.quoted.single.abl

    x = "it's".
//      ^^^^^^ string.quoted.double.abl
    x = 'say "hi"'.
//      ^^^^^^^^^^ string.quoted.single.abl

    x = "a ~" b".
//         ^^ string.quoted.double.abl constant.character.escape.abl
//          ^^^^ string.quoted.double.abl
    x = "tab~tend~nnl".
//          ^^ string.quoted.double.abl constant.character.escape.abl
//               ^^ string.quoted.double.abl constant.character.escape.abl
    x = "octal ~012 end".
//             ^^^^ string.quoted.double.abl constant.character.escape.abl
//                  ^^^ string.quoted.double.abl
    x = "tilde ~~ end".
//             ^^ constant.character.escape.abl
    x = "doubled "" quote".
//               ^^ string.quoted.double.abl constant.character.escape.abl
//                  ^^^^^ string.quoted.double.abl
    x = 'doubled '' quote'.
//               ^^ string.quoted.single.abl constant.character.escape.abl
//                  ^^^^^ string.quoted.single.abl

    x = "text":U.
//      ^^^^^^ string.quoted.double.abl
//             ^ storage.modifier.string-attribute.abl
    x = "text":R10.
//             ^^^ storage.modifier.string-attribute.abl
    x = "text":U + "b".
//             ^ storage.modifier.string-attribute.abl
//               ^ keyword.operator.arithmetic.abl
    x = "text":u.
//             ^ storage.modifier.string-attribute.abl
    x = 'text':L.
//             ^ storage.modifier.string-attribute.abl
    x = "text" : y.
//               ^ - storage.modifier.string-attribute.abl

    MESSAGE "value {&name} here" "{1}".
//                   ^^^^ string.quoted.double.abl variable.other.constant.preprocessor.abl
//                                 ^ string.quoted.double.abl variable.other.constant.preprocessor.abl

    x = "a
    b".
//  ^^ string.quoted.double.abl

    MESSAGE "keywords DEFINE END inside" y.
//                    ^^^^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
//                           ^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
//                                       ^ variable.other.abl

