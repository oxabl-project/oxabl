//! Parameter lists, as the caller of a `RUN` sees them.
//!
//! [`ProgramSignatures::of`] reads the `DEFINE ... PARAMETER` statements of an
//! external program's main block and of each internal procedure. A list is
//! only given when the statements show all of it: an `{include}` or an `&IF`
//! that holds a parameter definition makes the list unknown.
//!
//! The semantics, with confidence marks, are in
//! `docs/design/strict-analysis-open-questions.md`, section 10.

use oxabl_ast::{
    HandleParamKind, ParameterDirection, ParameterExtent, ParameterType, PreprocIf, Statement,
    StatementKind, TypeSource,
};

use crate::index::{IndexName, MemberType};
use crate::types::{PrimitiveTy, ResolvedType};

/// How a parameter passes its value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamMode {
    Input,
    Output,
    InputOutput,
}

/// What a parameter is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamShape {
    /// A variable. The type is `None` when it is not a primitive the
    /// declaration states: `LIKE`, a class, a conditional type or an array
    /// (`EXTENT`).
    Scalar(Option<PrimitiveTy>),
    /// `BUFFER b FOR table`.
    Buffer,
    /// `TABLE FOR tt`.
    Table,
    /// `TABLE-HANDLE h`.
    TableHandle,
    /// `DATASET FOR ds`.
    Dataset,
    /// `DATASET-HANDLE h`.
    DatasetHandle,
}

/// One parameter of a procedure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Param {
    pub mode: ParamMode,
    pub shape: ParamShape,
}

/// The parameters of a procedure, in order.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Signature {
    pub params: Vec<Param>,
    /// An `{include}` or an `&IF` supplies some of the parameters: the list
    /// comes from the expanded text, and another compile environment (another
    /// search path, another `&GLOBAL-DEFINE`) may expand it to another list.
    pub from_include: bool,
}

/// The parameter lists of one file: its main block and its internal
/// procedures. A list is `None` when the file's statements do not show it
/// whole.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ProgramSignatures {
    /// The parameters of the program itself (`RUN file.p (...)`). `None` for a
    /// class, interface or a main block with a parameter the statements hide.
    pub main: Option<Signature>,
    /// Internal procedures with a body, by folded name. A name defined twice
    /// has no list.
    pub procedures: Vec<(IndexName, Option<Signature>)>,
    /// How many parameter statements the main block's own text holds.
    main_written: usize,
    /// The same for each internal procedure.
    procedures_written: Vec<(IndexName, usize)>,
}

impl ProgramSignatures {
    /// The parameters `statements` define.
    pub fn of(statements: &[Statement]) -> Self {
        let mut out = ProgramSignatures::default();
        let class = statements.iter().any(|s| {
            matches!(
                s.kind,
                StatementKind::Class { .. } | StatementKind::Interface { .. }
            )
        });
        // A file with no statement (empty, or all comment) is a placeholder: it
        // gives no list.
        if class || statements.is_empty() {
            return out;
        }
        let (main, written) = collect(statements);
        out.main = main;
        out.main_written = written;
        let mut found = Vec::new();
        procedures(statements, &mut found);
        for (name, list, written) in found {
            out.procedures.push((name.clone(), list));
            out.procedures_written.push((name, written));
        }
        out
    }

    /// Mark each list that `written` (the same file's statements before its
    /// includes were expanded) did not show whole and that an include added
    /// parameters to.
    pub fn marking_includes(mut self, written: &ProgramSignatures) -> Self {
        if written.main.is_none()
            && let Some(main) = &mut self.main
            && main.params.len() > written.main_written
        {
            main.from_include = true;
        }
        for (name, list) in &mut self.procedures {
            let shown = written
                .procedures
                .iter()
                .any(|(n, l)| n == name && l.is_some());
            let own = written
                .procedures_written
                .iter()
                .find(|(n, _)| n == name)
                .map_or(0, |(_, n)| *n);
            if let (false, Some(list)) = (shown, list)
                && list.params.len() > own
            {
                list.from_include = true;
            }
        }
        self
    }

    /// The list of the internal procedure `name`.
    pub fn procedure(&self, name: &IndexName) -> Option<&Signature> {
        self.procedures
            .iter()
            .find(|(n, _)| n == name)
            .and_then(|(_, s)| s.as_ref())
    }
}

/// The parameters one block defines, or `None` when something hides part of
/// the list, and how many parameter statements the block's own text holds.
fn collect(statements: &[Statement]) -> (Option<Signature>, usize) {
    let mut params = Vec::new();
    let known = collect_into(statements, &mut params);
    let written = params.len();
    let signature = known.then_some(Signature {
        params,
        from_include: false,
    });
    (signature, written)
}

/// Add the parameters of `statements` to `params`, looking inside a `DO` block
/// and a label. Returns `false` when something hides part of the list: an
/// include, a conditional or a loop body that holds a parameter.
fn collect_into(statements: &[Statement], params: &mut Vec<Param>) -> bool {
    let mut known = true;
    for s in statements {
        match &s.kind {
            StatementKind::DefineParameter {
                direction,
                param_type,
            } => {
                if let Some(p) = param(direction, param_type) {
                    params.push(p);
                }
            }
            StatementKind::IncludeReference { .. } | StatementKind::IncludeArgReference { .. } => {
                known = false;
            }
            StatementKind::PreprocIf(branches) if hides_parameters(branches) => known = false,
            StatementKind::Block(body) => known &= collect_into(body, params),
            StatementKind::Do {
                from: None,
                to: None,
                by: None,
                while_condition: None,
                on_actions,
                body,
                ..
            } if on_actions.is_empty() => known &= collect_into(body, params),
            StatementKind::Label { body, .. } => {
                known &= collect_into(std::slice::from_ref(&**body), params);
            }
            // A loop or a condition that defines a parameter: how many
            // times it runs is a run-time fact.
            _ if holds_parameter(s) => known = false,
            _ => {}
        }
    }
    known
}

/// Whether a statement other than a routine holds a parameter definition in
/// its body.
fn holds_parameter(s: &Statement) -> bool {
    let any = |body: &[Statement]| body.iter().any(holds_parameter);
    match &s.kind {
        StatementKind::DefineParameter { .. } => true,
        StatementKind::Do { body, .. }
        | StatementKind::Repeat { body, .. }
        | StatementKind::ForEach { body, .. }
        | StatementKind::Block(body) => any(body),
        StatementKind::If {
            then_branch,
            else_branch,
            ..
        } => holds_parameter(then_branch) || else_branch.as_deref().is_some_and(holds_parameter),
        StatementKind::Case {
            when_branches,
            otherwise,
            ..
        } => when_branches.iter().any(|w| any(&w.body)) || otherwise.as_deref().is_some_and(any),
        StatementKind::Label { body, .. } => holds_parameter(body),
        _ => false,
    }
}

/// Whether a conditional block holds a parameter definition or an include.
fn hides_parameters(branches: &PreprocIf<Vec<Statement>>) -> bool {
    let hides = |body: &Vec<Statement>| {
        body.iter().any(|s| match &s.kind {
            StatementKind::DefineParameter { .. }
            | StatementKind::IncludeReference { .. }
            | StatementKind::IncludeArgReference { .. } => true,
            StatementKind::PreprocIf(inner) => hides_parameters(inner),
            _ => false,
        })
    };
    hides(&branches.then_branch)
        || branches.elseif_branches.iter().any(|(_, b)| hides(b))
        || branches.else_branch.as_ref().is_some_and(hides)
}

/// The internal procedures of `statements`, nested `&IF` branches included.
fn procedures(statements: &[Statement], out: &mut Vec<(IndexName, Option<Signature>, usize)>) {
    for s in statements {
        match &s.kind {
            StatementKind::Procedure {
                name,
                body,
                is_prototype: false,
                ..
            } => {
                let key = IndexName::new(&name.name);
                if let Some(entry) = out.iter_mut().find(|(n, _, _)| *n == key) {
                    entry.1 = None;
                } else {
                    let (list, written) = collect(body);
                    out.push((key, list, written));
                }
            }
            StatementKind::PreprocIf(branches) => {
                procedures(&branches.then_branch, out);
                for (_, b) in &branches.elseif_branches {
                    procedures(b, out);
                }
                if let Some(b) = &branches.else_branch {
                    procedures(b, out);
                }
            }
            _ => {}
        }
    }
}

/// One `DEFINE ... PARAMETER` statement as a parameter, or `None` for a
/// `RETURN` parameter, which no argument fills.
fn param(direction: &ParameterDirection, param_type: &ParameterType) -> Option<Param> {
    let mode = match direction {
        ParameterDirection::Input => ParamMode::Input,
        ParameterDirection::Output => ParamMode::Output,
        ParameterDirection::InputOutput => ParamMode::InputOutput,
        ParameterDirection::Return => return None,
    };
    let shape = match param_type {
        ParameterType::Variable {
            type_source,
            extent,
            ..
        } => {
            let array = extent.is_some();
            ParamShape::Scalar(match type_source {
                TypeSource::Explicit(t) if !array => match ResolvedType::from_data_type(t) {
                    ResolvedType::Primitive(p) => Some(p),
                    _ => None,
                },
                _ => None,
            })
        }
        ParameterType::Handle { kind, .. } => match kind {
            HandleParamKind::Table => ParamShape::Table,
            HandleParamKind::TableHandle => ParamShape::TableHandle,
            HandleParamKind::Dataset => ParamShape::Dataset,
            HandleParamKind::DatasetHandle => ParamShape::DatasetHandle,
        },
        ParameterType::Buffer { .. } => ParamShape::Buffer,
    };
    Some(Param { mode, shape })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn signatures(source: &str) -> ProgramSignatures {
        let tokens = oxabl_lexer::tokenize(source);
        let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
        ProgramSignatures::of(&program.statements)
    }

    fn scalar(mode: ParamMode, ty: Option<PrimitiveTy>) -> Param {
        Param {
            mode,
            shape: ParamShape::Scalar(ty),
        }
    }

    #[test]
    fn a_program_lists_its_main_block_parameters_in_order() {
        let sigs = signatures(
            "DEFINE INPUT PARAMETER a AS INTEGER NO-UNDO.\n\
             DEFINE OUTPUT PARAMETER b AS CHARACTER NO-UNDO.\n\
             DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt.\n\
             DEFINE INPUT PARAMETER pl LIKE Customer.Name.\n\
             DEFINE INPUT PARAMETER arr AS INTEGER EXTENT 3 NO-UNDO.\n",
        );
        let main = sigs.main.expect("the main block shows its parameters");
        assert_eq!(
            main.params,
            [
                scalar(ParamMode::Input, Some(PrimitiveTy::Integer)),
                scalar(ParamMode::Output, Some(PrimitiveTy::Character)),
                Param {
                    mode: ParamMode::InputOutput,
                    shape: ParamShape::Table
                },
                scalar(ParamMode::Input, None),
                scalar(ParamMode::Input, None),
            ]
        );
    }

    #[test]
    fn an_internal_procedure_has_its_own_list() {
        let sigs = signatures(
            "PROCEDURE calc:\n  DEFINE INPUT PARAMETER x AS DECIMAL NO-UNDO.\nEND PROCEDURE.\n",
        );
        assert_eq!(sigs.main, Some(Signature::default()));
        let calc = sigs.procedure(&IndexName::new("CALC")).unwrap();
        assert_eq!(
            calc.params,
            [scalar(ParamMode::Input, Some(PrimitiveTy::Decimal))]
        );
    }

    #[test]
    fn an_include_hides_the_list() {
        let sigs = signatures(
            "{params.i}\nPROCEDURE calc:\n  {more.i}\nEND PROCEDURE.\n\
             PROCEDURE plain:\n  DEFINE INPUT PARAMETER x AS INTEGER NO-UNDO.\nEND PROCEDURE.\n",
        );
        assert_eq!(sigs.main, None);
        assert_eq!(sigs.procedure(&IndexName::new("calc")), None);
        assert!(sigs.procedure(&IndexName::new("plain")).is_some());
    }

    #[test]
    fn a_parameter_in_a_do_block_counts_and_one_in_a_loop_or_condition_hides_the_list() {
        let sigs = signatures(
            "PROCEDURE p:\n  DO:\n    DEFINE INPUT PARAMETER a AS INTEGER NO-UNDO.\n  END.\n\
             END PROCEDURE.\n\
             PROCEDURE q:\n  IF TRUE THEN DO:\n    DEFINE INPUT PARAMETER a AS INTEGER NO-UNDO.\n  END.\n\
             END PROCEDURE.\n\
             PROCEDURE r:\n  REPEAT:\n    DEFINE INPUT PARAMETER a AS INTEGER NO-UNDO.\n  END.\n\
             END PROCEDURE.\n",
        );
        assert_eq!(
            sigs.procedure(&IndexName::new("p")).unwrap().params.len(),
            1
        );
        assert_eq!(sigs.procedure(&IndexName::new("q")), None);
        assert_eq!(sigs.procedure(&IndexName::new("r")), None);
    }

    #[test]
    fn a_conditional_parameter_hides_the_list_but_other_conditions_do_not() {
        let hidden = signatures(
            "&IF DEFINED(x) &THEN\nDEFINE INPUT PARAMETER a AS INTEGER NO-UNDO.\n&ENDIF\n",
        );
        assert_eq!(hidden.main, None);
        let shown =
            signatures("&IF DEFINED(x) &THEN\nDEFINE VARIABLE v AS INTEGER NO-UNDO.\n&ENDIF\n");
        assert_eq!(shown.main, Some(Signature::default()));
    }

    #[test]
    fn a_procedure_defined_twice_has_no_list() {
        let sigs = signatures(
            "PROCEDURE p:\n  DEFINE INPUT PARAMETER x AS INTEGER NO-UNDO.\nEND PROCEDURE.\n\
             PROCEDURE p:\nEND PROCEDURE.\n",
        );
        assert_eq!(sigs.procedure(&IndexName::new("p")), None);
        assert_eq!(sigs.procedures.len(), 1);
    }

    #[test]
    fn a_file_with_no_statement_has_no_list() {
        assert_eq!(signatures("").main, None);
        assert_eq!(signatures("/* all of it is a comment */").main, None);
        assert_eq!(signatures("MESSAGE 1.").main, Some(Signature::default()));
    }

    #[test]
    fn a_class_has_no_program_signature() {
        let sigs = signatures("CLASS a.b:\nEND CLASS.\n");
        assert_eq!(sigs.main, None);
    }
}

/// One parameter of a class method, in a form that survives a file boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberParam {
    pub mode: ParamMode,
    pub shape: MemberParamShape,
}

/// What a method parameter is. A class type keeps its name as written, for
/// the consumer to resolve through the declaring file's imports.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemberParamShape {
    /// A variable with a type of its own. `EXTENT` gives an array.
    Value(MemberType),
    /// A variable `LIKE` a field or a variable: its name as written, and the
    /// `EXTENT` clause of the parameter. Its type is not known here.
    Like {
        source: IndexName,
        extent: Option<ParameterExtent>,
    },
    /// `BUFFER b FOR table`.
    Buffer,
    /// `TABLE FOR tt`.
    Table,
    /// `TABLE-HANDLE h`.
    TableHandle,
    /// `DATASET FOR ds`.
    Dataset,
    /// `DATASET-HANDLE h`.
    DatasetHandle,
}

/// The parameter list of a method, from the statements between its
/// parentheses. `None` when a statement is not a plain parameter definition
/// (an `&IF`, an include reference, a `RETURN` parameter): then the list is
/// not known.
pub fn member_params(parameters: &[Statement]) -> Option<Vec<MemberParam>> {
    parameters.iter().map(member_param_of).collect()
}

fn member_param_of(statement: &Statement) -> Option<MemberParam> {
    match &statement.kind {
        StatementKind::DefineParameter {
            direction,
            param_type,
        } => member_param(direction, param_type),
        _ => None,
    }
}

/// The parameter a `DEFINE PARAMETER` statement declares, as a call sees it.
/// A class name, `LIKE` and an unknown type give `Unknown`: the statement
/// alone cannot resolve them. `None` for a statement that is not a plain
/// parameter definition.
pub fn call_param_of(statement: &Statement) -> Option<CallParam> {
    let declared = [member_param_of(statement)?];
    call_params(&declared, |_| ResolvedType::Unknown)
        .into_iter()
        .next()
}

fn member_param(direction: &ParameterDirection, param_type: &ParameterType) -> Option<MemberParam> {
    let mode = match direction {
        ParameterDirection::Input => ParamMode::Input,
        ParameterDirection::Output => ParamMode::Output,
        ParameterDirection::InputOutput => ParamMode::InputOutput,
        ParameterDirection::Return => return None,
    };
    let shape = match param_type {
        ParameterType::Variable {
            type_source: TypeSource::Explicit(t),
            extent: None,
            ..
        } if t.class_name().is_some() => {
            MemberParamShape::Value(MemberType::Named(IndexName::new(t.class_name()?)))
        }
        ParameterType::Variable {
            type_source: TypeSource::Explicit(t),
            extent,
            ..
        } => MemberParamShape::Value(
            MemberType::portable(ResolvedType::from_declared(t, *extent))
                .unwrap_or(MemberType::Untyped),
        ),
        ParameterType::Variable {
            type_source: TypeSource::Like { source },
            extent,
            ..
        } => MemberParamShape::Like {
            source: IndexName::new(&source.name),
            extent: *extent,
        },
        ParameterType::Handle { kind, .. } => match kind {
            HandleParamKind::Table => MemberParamShape::Table,
            HandleParamKind::TableHandle => MemberParamShape::TableHandle,
            HandleParamKind::Dataset => MemberParamShape::Dataset,
            HandleParamKind::DatasetHandle => MemberParamShape::DatasetHandle,
        },
        ParameterType::Buffer { .. } => MemberParamShape::Buffer,
    };
    Some(MemberParam { mode, shape })
}

/// One parameter of a method, with its class names resolved in the analysed
/// file. The type-check pass fits the arguments of a call against these.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallParam {
    pub mode: ParamMode,
    pub kind: CallParamKind,
}

/// What a method parameter is, in the analysed file's symbol space.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CallParamKind {
    /// A variable. `Unknown` when oxabl does not know the type.
    Value(ResolvedType),
    Buffer,
    Table,
    TableHandle,
    Dataset,
    DatasetHandle,
}

/// Resolve a portable parameter list. `class` gives the type of a class name
/// as written; a type that does not survive the boundary gives `Unknown`.
pub(crate) fn call_params(
    decls: &[MemberParam],
    mut class: impl FnMut(&IndexName) -> ResolvedType,
) -> Vec<CallParam> {
    decls
        .iter()
        .map(|p| CallParam {
            mode: p.mode,
            kind: match &p.shape {
                MemberParamShape::Value(MemberType::Portable(ty)) => {
                    CallParamKind::Value(ty.as_resolved().clone())
                }
                MemberParamShape::Value(MemberType::Named(name)) => {
                    CallParamKind::Value(class(name))
                }
                MemberParamShape::Value(MemberType::Untyped) | MemberParamShape::Like { .. } => {
                    CallParamKind::Value(ResolvedType::Unknown)
                }
                MemberParamShape::Buffer => CallParamKind::Buffer,
                MemberParamShape::Table => CallParamKind::Table,
                MemberParamShape::TableHandle => CallParamKind::TableHandle,
                MemberParamShape::Dataset => CallParamKind::Dataset,
                MemberParamShape::DatasetHandle => CallParamKind::DatasetHandle,
            },
        })
        .collect()
}
