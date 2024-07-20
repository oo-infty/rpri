use std::fmt::{Display, Formatter, Result as FmtResult};
use std::ops::BitXor;

use enum_dispatch::enum_dispatch;
use snafu::prelude::*;

use crate::entity::atom::AtomHandle;
use crate::entity::base::VariableIdentifier;
use crate::entity::predicate::{Predicate, PredicateHandle, Signature};
use crate::utils::cursor::{
    Cursor, CursorError, CursorIter, Direction, IndexCursor, SingleCursor, SliceCursor,
};

/// Argument of predicates, which can be a variable or constant. Only when this
/// structure is associated with a predicate or its negation does the order of
/// arguments make sense.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Argument {
    Variable(VariableIdentifier),
    Constant(AtomHandle),
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Variable(s) => s.fmt(f),
            Self::Constant(s) => s.fmt(f),
        }
    }
}

/// An abstract form of expressions in Prolog, base of facts, rules and queries.
#[enum_dispatch]
pub trait Expression: Clone + Eq + PartialEq + Display {
    fn arguments(&self) -> &Vec<Argument>;

    fn kind(&self) -> ExpressionKind;

    fn view(&self, negated: bool) -> ExpressionView;

    fn cursor(&self) -> ExpressionCursor;

    fn iter(&self) -> CursorIter<ExpressionCursor> {
        self.cursor().into_iter(Direction::Forward)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionElement {
    Predicate(PredicateHandle),
    SubExpressions(Vec<ExpressionNode>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpressionKind {
    Conjunctive,
    Disjunctive,
}

impl ExpressionKind {
    fn negate(self) -> Self {
        match self {
            Self::Conjunctive => Self::Disjunctive,
            Self::Disjunctive => Self::Conjunctive,
        }
    }
}

/// Basic node of expression trees.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionNode {
    arguments: Vec<Argument>,
    elements: ExpressionElement,
    kind: ExpressionKind,
    negated: bool,
}

impl ExpressionNode {
    pub fn try_new_predicate(
        predicate: PredicateHandle,
        arguments: Vec<Argument>,
        negated: bool,
    ) -> Result<Self, TryNewExpressionError> {
        ensure!(
            arguments.len() == predicate.signature().arity(),
            MismatchedAritySnafu {
                signature: predicate.signature().clone(),
                actual: arguments.len()
            }
        );

        Ok(ExpressionNode {
            arguments,
            elements: ExpressionElement::Predicate(predicate),
            kind: ExpressionKind::Conjunctive,
            negated,
        })
    }

    pub fn try_new_conjunction(
        elements: Vec<ExpressionNode>,
        negated: bool,
    ) -> Result<Self, TryNewExpressionError> {
        Self::try_new_compound_expression(elements, ExpressionKind::Conjunctive, negated)
    }

    pub fn try_new_disjunction(
        elements: Vec<ExpressionNode>,
        negated: bool,
    ) -> Result<Self, TryNewExpressionError> {
        Self::try_new_compound_expression(elements, ExpressionKind::Disjunctive, negated)
    }

    fn try_new_compound_expression(
        elements: Vec<ExpressionNode>,
        kind: ExpressionKind,
        negated: bool,
    ) -> Result<Self, TryNewExpressionError> {
        ensure!(!elements.is_empty(), EmptyElementSnafu);

        // Get unique arguments.
        let mut arguments = Vec::new();
        let all_arguments = elements
            .iter()
            .map(|expr| expr.arguments().clone())
            .collect::<Vec<_>>()
            .concat();

        for arg in all_arguments {
            if !arguments.contains(&arg) {
                arguments.push(arg);
            }
        }

        Ok(Self {
            arguments,
            elements: ExpressionElement::SubExpressions(elements),
            kind,
            negated,
        })
    }
}

impl Expression for ExpressionNode {
    fn arguments(&self) -> &Vec<Argument> {
        &self.arguments
    }

    fn kind(&self) -> ExpressionKind {
        if self.negated {
            self.kind.negate()
        } else {
            self.kind
        }
    }

    fn view(&self, negated: bool) -> ExpressionView {
        let ExpressionNode { elements, kind, .. } = self;
        let negate_each = self.negated.bitxor(negated);
        let kind = if negate_each { kind.negate() } else { *kind };

        match elements {
            ExpressionElement::Predicate(_) => PredicateView {
                target: self,
                negated: negate_each,
            }
            .into(),
            ExpressionElement::SubExpressions(_) => match kind {
                ExpressionKind::Conjunctive => ConjunctionView {
                    target: self,
                    negate_each,
                }
                .into(),
                ExpressionKind::Disjunctive => DisjunctionView {
                    target: self,
                    negate_each,
                }
                .into(),
            },
        }
    }

    fn cursor(&self) -> ExpressionCursor {
        match self.view(false) {
            ExpressionView::Predicate(view) => ExpressionCursor::Predicate {
                cursor: SingleCursor::new(self),
                negated: view.negated,
            },
            ExpressionView::Conjunction(view) => match &self.elements {
                ExpressionElement::SubExpressions(expr) => ExpressionCursor::SubExpressions {
                    cursor: SliceCursor::new(expr),
                    negate_each: view.negate_each,
                },
                _ => unreachable!(),
            },
            ExpressionView::Disjunction(view) => match &self.elements {
                ExpressionElement::SubExpressions(expr) => ExpressionCursor::SubExpressions {
                    cursor: SliceCursor::new(expr),
                    negate_each: view.negate_each,
                },
                _ => unreachable!(),
            },
        }
    }
}

impl Display for ExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self.view(false) {
            ExpressionView::Predicate(s) => s.fmt(f),
            ExpressionView::Conjunction(s) => s.fmt(f),
            ExpressionView::Disjunction(s) => s.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Snafu, PartialEq, Eq)]
pub enum TryNewExpressionError {
    #[snafu(display("{signature} should accepts exact {} argument(s). Got: {actual}", signature.arity()))]
    MismatchedArity { signature: Signature, actual: usize },
    #[snafu(display("Expects at least one expression."))]
    EmptyElement,
}

/// Upper-level representation of an expression node, providing classification
/// of expressions depending on the inner node.
#[derive(Debug, Clone, PartialEq, Eq)]
#[enum_dispatch(Expression)]
pub enum ExpressionView<'a> {
    Predicate(PredicateView<'a>),
    Conjunction(ConjunctionView<'a>),
    Disjunction(DisjunctionView<'a>),
}

impl ExpressionView<'_> {
    pub fn as_predicate(&self) -> Option<(PredicateHandle, bool)> {
        match self {
            Self::Predicate(s) => match &s.target.elements {
                ExpressionElement::Predicate(p) => Some((p.clone(), s.negated)),
                _ => None,
            },
            _ => None,
        }
    }
}

impl Display for ExpressionView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Predicate(s) => s.fmt(f),
            Self::Conjunction(s) => s.fmt(f),
            Self::Disjunction(s) => s.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PredicateView<'a> {
    target: &'a ExpressionNode,
    negated: bool,
}

impl PredicateView<'_> {
    pub fn negated(&self) -> bool {
        self.negated
    }
}

impl Expression for PredicateView<'_> {
    fn arguments(&self) -> &Vec<Argument> {
        self.target.arguments()
    }

    fn kind(&self) -> ExpressionKind {
        ExpressionKind::Conjunctive
    }

    fn view(&self, negated: bool) -> ExpressionView {
        Self {
            target: self.target,
            negated: self.negated.bitxor(negated),
        }
        .into()
    }

    fn cursor(&self) -> ExpressionCursor {
        ExpressionCursor::Predicate {
            cursor: SingleCursor::new(self.target),
            negated: self.negated,
        }
    }
}

impl Display for PredicateView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let ExpressionNode {
            arguments,
            elements: ExpressionElement::Predicate(predicate),
            ..
        } = self.target
        else {
            unreachable!()
        };

        let prefix = if self.negated { "\\+" } else { "" };

        if let Some(first) = arguments.first() {
            let args = arguments
                .iter()
                .skip(1)
                .map(|arg| arg.to_string())
                .fold(first.to_string(), |acc, arg| acc + ", " + &arg);
            write!(f, "{prefix}{}({})", predicate.identifier(), args)
        } else {
            write!(f, "{prefix}{}", predicate.identifier())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConjunctionView<'a> {
    target: &'a ExpressionNode,
    negate_each: bool,
}

impl Expression for ConjunctionView<'_> {
    fn arguments(&self) -> &Vec<Argument> {
        self.target.arguments()
    }

    fn kind(&self) -> ExpressionKind {
        ExpressionKind::Conjunctive
    }

    fn view(&self, negated: bool) -> ExpressionView {
        if !negated {
            self.clone().into()
        } else {
            DisjunctionView {
                target: self.target,
                negate_each: !self.negate_each,
            }
            .into()
        }
    }

    fn cursor(&self) -> ExpressionCursor {
        match &self.target.elements {
            ExpressionElement::SubExpressions(expr) => ExpressionCursor::SubExpressions {
                cursor: SliceCursor::new(expr),
                negate_each: self.negate_each,
            },
            _ => unreachable!(),
        }
    }
}

impl Display for ConjunctionView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let Some(first) = self.iter().next() else {
            unreachable!()
        };

        let to_string = |expr| {
            if matches!(expr, ExpressionView::Predicate(_)) {
                expr.to_string()
            } else {
                format!("({})", expr)
            }
        };

        let res = self
            .iter()
            .skip(1)
            .map(to_string)
            .fold(to_string(first), |acc, expr| acc + ", " + &expr);

        write!(f, "{res}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DisjunctionView<'a> {
    target: &'a ExpressionNode,
    negate_each: bool,
}

impl Expression for DisjunctionView<'_> {
    fn arguments(&self) -> &Vec<Argument> {
        self.target.arguments()
    }

    fn kind(&self) -> ExpressionKind {
        ExpressionKind::Disjunctive
    }

    fn view(&self, negated: bool) -> ExpressionView {
        if !negated {
            self.clone().into()
        } else {
            ConjunctionView {
                target: self.target,
                negate_each: !self.negate_each,
            }
            .into()
        }
    }

    fn cursor(&self) -> ExpressionCursor {
        match &self.target.elements {
            ExpressionElement::SubExpressions(expr) => ExpressionCursor::SubExpressions {
                cursor: SliceCursor::new(expr),
                negate_each: self.negate_each,
            },
            _ => unreachable!(),
        }
    }
}

impl Display for DisjunctionView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let Some(first) = self.iter().next() else {
            unreachable!()
        };

        let to_string = |expr| {
            if matches!(expr, ExpressionView::Predicate(_)) {
                expr.to_string()
            } else {
                format!("({})", expr)
            }
        };

        let res = self
            .iter()
            .skip(1)
            .map(to_string)
            .fold(to_string(first), |acc, expr| acc + "; " + &expr);

        write!(f, "{res}")
    }
}

pub enum ExpressionCursor<'a> {
    Predicate {
        cursor: SingleCursor<'a, ExpressionNode>,
        negated: bool,
    },
    SubExpressions {
        cursor: SliceCursor<'a, ExpressionNode>,
        negate_each: bool,
    },
}

impl<'a> Cursor for ExpressionCursor<'a> {
    type Item = ExpressionView<'a>;
    type Position = usize;

    fn current(&self) -> Option<Self::Item> {
        match self {
            Self::Predicate { cursor, negated } => cursor
                .current()
                .map(|expr| expr.view(expr.negated.bitxor(negated))),
            Self::SubExpressions {
                cursor,
                negate_each,
            } => cursor.current().map(|expr| expr.view(*negate_each)),
        }
    }

    fn position(&self) -> Result<Self::Position, CursorError> {
        match self {
            Self::Predicate { cursor, .. } => cursor.position(),
            Self::SubExpressions { cursor, .. } => cursor.position(),
        }
    }

    fn move_toward(&mut self, direction: Direction) -> bool {
        match self {
            Self::Predicate { cursor, .. } => cursor.move_toward(direction),
            Self::SubExpressions { cursor, .. } => cursor.move_toward(direction),
        }
    }
}

impl<'a> IndexCursor for ExpressionCursor<'a> {
    fn len(&self) -> Self::Position {
        match self {
            Self::Predicate { cursor, .. } => cursor.len(),
            Self::SubExpressions { cursor, .. } => cursor.len(),
        }
    }

    fn set(&mut self, index: Self::Position) -> bool {
        match self {
            Self::Predicate { cursor, .. } => cursor.set(index),
            Self::SubExpressions { cursor, .. } => cursor.set(index),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::entity::atom::{AtomDefinition, AtomHandle};
    use crate::entity::predicate::{PredicateDefinition, PredicateHandle, Signature};

    #[test]
    fn argument_display() {
        let atom = AtomHandle::from(AtomDefinition::new("atom".parse().unwrap(), 42));
        let constant = Argument::Constant(atom);
        assert_eq!(constant.to_string(), "atom");

        let variable = Argument::Variable("Variable".parse().unwrap());
        assert_eq!(variable.to_string(), "Variable");
    }

    #[test]
    fn predicate_try_new() {
        let signature = Signature::new("pred".parse().unwrap(), 1);
        let predicate = PredicateDefinition::new(signature, 0);
        let predicate = PredicateHandle::from(predicate);
        let expr = ExpressionNode::try_new_predicate(
            predicate,
            vec![Argument::Variable("X".parse().unwrap())],
            false,
        );
        assert!(expr.is_ok());

        let signature = Signature::new("pred".parse().unwrap(), 2);
        let predicate = PredicateDefinition::new(signature, 0);
        let predicate = PredicateHandle::from(predicate);
        let expr = ExpressionNode::try_new_predicate(
            predicate,
            vec![Argument::Variable("X".parse().unwrap())],
            false,
        );
        assert!(matches!(
            expr,
            Err(TryNewExpressionError::MismatchedArity {
                signature: _,
                actual: 1
            })
        ));
    }

    #[test]
    fn conjunction_try_new() {
        let expr = ExpressionNode::try_new_conjunction(
            vec![make_predicate_expr(true), make_predicate_expr(true)],
            false,
        )
        .unwrap();
        assert_eq!(
            expr.arguments,
            vec![Argument::Variable("X".parse().unwrap())]
        );
        assert_eq!(expr.negated, false);

        let expr = ExpressionNode::try_new_conjunction(vec![], false);
        assert_eq!(expr, Err(TryNewExpressionError::EmptyElement));
    }

    #[test]
    fn predicate_view_display() {
        let expr = make_predicate_expr(true);
        let view = expr.view(false);
        assert_eq!(view.to_string(), "pred(X)");
        let view = view.view(true);
        assert_eq!(view.to_string(), r"\+pred(X)");

        let expr = make_predicate_expr(false);
        let view = expr.view(false);
        assert_eq!(view.to_string(), "pred");
        let view = view.view(true);
        assert_eq!(view.to_string(), r"\+pred");
    }

    #[test]
    fn predicate_view_iteration() {
        let expr = make_predicate_expr(true);
        let view = expr.view(false);
        let mut iter = view.iter();
        assert_eq!(iter.next(), Some(make_predicate_expr(true).view(false)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn conjunctive_view_iteration() {
        let expr = make_conjunction_expr();

        let view = expr.view(false);
        let mut iter = view.iter();
        assert_eq!(iter.next(), Some(make_predicate_expr(false).view(false)));
        assert_eq!(iter.next(), Some(make_predicate_expr(true).view(false)));
        assert_eq!(iter.next(), None);

        let view = expr.view(true);
        assert!(view.iter().all(|view| matches!(
            view,
            ExpressionView::Predicate(PredicateView {
                target: _,
                negated: true
            })
        )));
    }

    #[test]
    fn expression_view_display() {
        let expr = make_combined_expr();
        let view = expr.view(true);
        assert_eq!(view.to_string(), r"\+pred, (\+pred; \+pred(X)), \+pred(X)");
    }

    fn make_predicate_expr(has_arg: bool) -> ExpressionNode {
        let arity = if has_arg { 1 } else { 0 };
        let signature = Signature::new("pred".parse().unwrap(), arity);
        let predicate = PredicateDefinition::new(signature, 0);
        let predicate = PredicateHandle::from(predicate);

        ExpressionNode::try_new_predicate(
            predicate,
            if has_arg {
                vec![Argument::Variable("X".parse().unwrap())]
            } else {
                vec![]
            },
            false,
        )
        .unwrap()
    }

    fn make_conjunction_expr() -> ExpressionNode {
        ExpressionNode::try_new_conjunction(
            vec![make_predicate_expr(false), make_predicate_expr(true)],
            false,
        )
        .unwrap()
    }

    fn make_combined_expr() -> ExpressionNode {
        ExpressionNode::try_new_disjunction(
            vec![
                make_predicate_expr(false),
                make_conjunction_expr(),
                make_predicate_expr(true),
            ],
            false,
        )
        .unwrap()
    }
}
