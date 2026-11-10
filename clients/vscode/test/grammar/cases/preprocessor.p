// SYNTAX TEST "source.abl" "preprocessor: directives, continuation, references, includes"

// conditional directives
    &IF DEFINED(x) &THEN
//  ^^^ keyword.control.directive.conditional.abl
//                 ^^^^^ keyword.control.directive.conditional.abl
    &ELSEIF {&y} = 1 &THEN
//  ^^^^^^^ keyword.control.directive.conditional.abl
    &ELSE
//  ^^^^^ keyword.control.directive.conditional.abl
    &ENDIF
//  ^^^^^^ keyword.control.directive.conditional.abl
    &if defined(x) &then
//  ^^^ keyword.control.directive.conditional.abl
//                 ^^^^^ keyword.control.directive.conditional.abl
    &IF {&opt} &THEN x = 1. &ENDIF
//  ^^^ keyword.control.directive.conditional.abl
//                          ^^^^^^ keyword.control.directive.conditional.abl

// define directives
    &GLOBAL-DEFINE name some value
//  ^^^^^^^^^^^^^^ meta.preprocessor.define.abl keyword.control.directive.define.abl
//                 ^^^^ meta.preprocessor.define.abl entity.name.function.preprocessor.abl
    &SCOPED-DEFINE name some value
//  ^^^^^^^^^^^^^^ meta.preprocessor.define.abl keyword.control.directive.define.abl
//                 ^^^^ meta.preprocessor.define.abl entity.name.function.preprocessor.abl
    &GLOB name 1
//  ^^^^^ keyword.control.directive.define.abl
//        ^^^^ entity.name.function.preprocessor.abl
    &SCOP name 1
//  ^^^^^ keyword.control.directive.define.abl
//        ^^^^ entity.name.function.preprocessor.abl
    &scoped-define lower 1
//  ^^^^^^^^^^^^^^ keyword.control.directive.define.abl
//                 ^^^^^ entity.name.function.preprocessor.abl
    &GLOBAL-DEFINE long-name 1 + ~
//  ^^^^^^^^^^^^^^ keyword.control.directive.define.abl
//                 ^^^^^^^^^ entity.name.function.preprocessor.abl
//                               ^ meta.preprocessor.define.abl
    2 + ~
//  ^ meta.preprocessor.define.abl
    3
//  ^ meta.preprocessor.define.abl
    x = 4.
//  ^ variable.other.abl - meta.preprocessor.define.abl
    &SCOPED-DEFINE one 1
    x = 2.
//  ^ variable.other.abl - meta.preprocessor.define.abl
    &SCOPED-DEFINE two "a" {&one}
//                         ^^ meta.preprocessor.define.abl
//                           ^^^ variable.other.constant.preprocessor.abl

// undefine and message
    &UNDEFINE name
//  ^^^^^^^^^ keyword.control.directive.undefine.abl
//            ^^^^ entity.name.function.preprocessor.abl
    &MESSAGE some text DEFINE here
//  ^^^^^^^^ keyword.control.directive.message.abl
//           ^^^^^^^^^^^^^^^^^^^^^ string.unquoted.preprocessor.message.abl
//                     ^^^^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    x = 1.
//  ^ variable.other.abl - string.unquoted.preprocessor.message.abl

// AppBuilder markers
    &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS
//  ^^^^^^^^^^^^^^^^ keyword.control.directive.analyze.abl
//                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ comment.line.preprocessor.analyze.abl
    DEFINE VARIABLE a AS INTEGER.
//  ^^^^^^ keyword.other.define.abl - comment.line.preprocessor.analyze.abl
    &ANALYZE-RESUME
//  ^^^^^^^^^^^^^^^ keyword.control.directive.analyze.abl
    &analyze-suspend _UIB-PREPROCESSOR-BLOCK
//  ^^^^^^^^^^^^^^^^ keyword.control.directive.analyze.abl

// references
    x = {&name}.
//        ^^^^ variable.other.constant.preprocessor.abl
    x = {1} + {2}.
//       ^ variable.other.constant.preprocessor.abl
//             ^ variable.other.constant.preprocessor.abl
    RUN {*}.
//       ^ variable.other.constant.preprocessor.abl
    x = {&*}.
//        ^ variable.other.constant.preprocessor.abl
    x = {&OPSYS}.
//        ^^^^^ variable.other.constant.preprocessor.abl
    x = "a{&name}b".
//          ^^^^ string.quoted.double.abl variable.other.constant.preprocessor.abl
    x = {&name}suffix.
//        ^^^^ variable.other.constant.preprocessor.abl
//             ^^^^^^ variable.other.abl

// includes
    {a/b.i &x=1 "y" z}
//  ^ meta.preprocessor.include.abl
//   ^^^^^ meta.preprocessor.include.abl string.unquoted.include.abl
//         ^^ meta.preprocessor.include.abl variable.parameter.preprocessor.abl
//              ^^^ meta.preprocessor.include.abl string.quoted.double.abl
//                  ^ meta.preprocessor.include.abl string.unquoted.argument.abl
    { inc.i }
//    ^^^^^ string.unquoted.include.abl
    {src/common.i}
//   ^^^^^^^^^^^^ meta.preprocessor.include.abl string.unquoted.include.abl
    {inc.i &a="b c" &d=e}
//   ^^^^^ string.unquoted.include.abl
//         ^^ variable.parameter.preprocessor.abl
//                  ^^ variable.parameter.preprocessor.abl
//                     ^ string.unquoted.argument.abl
    {inc.i &p={&q}}
//   ^^^^^ string.unquoted.include.abl
//         ^^ variable.parameter.preprocessor.abl
//              ^ variable.other.constant.preprocessor.abl
    {{&file}}
//     ^^^^ variable.other.constant.preprocessor.abl
    {&name}
//  ^^^^^^^ - meta.preprocessor.include.abl
    {1}
//   ^ - meta.preprocessor.include.abl
    MESSAGE "{inc.i}".
//            ^^^^^ - string.unquoted.include.abl

